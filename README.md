OpenAPI 3.1 handler for Cowboy (Erlang/OTP)
=================

# What is it?
`openapi_handler` is a library translating [OpenAPI](https://spec.openapis.org/oas/v3.1.0) requests
into native Erlang function calls. It takes schema (compiled single file JSON or YAML), extracts, validates
and converts request parameters to Erlang types.

# How to use

## Load schema
First, you need to compile cowboy routes for your schema.  
```erlang
  PetstoreRoutes = openapi_handler:routes(#{
    schema => PetstorePath,         % path/to/your/schema.{json,yaml}
    prefix => <<"/api/prefix">>,    % HTTP path prefix for your API
    name => petstore_server_api,    % API identifier, must be unique
    module => petstore_impl         % A module with functions to call
  }),

  cowboy:start_clear(petstore_api_server, [{port, 8000}],
    #{env => #{dispatch => cowboy_router:compile([{'_', PetstoreRoutes}])}}),
```

## Implement callback module
Your callback module needs authorize function:
```erlang
authorize(#{authorization := AuthorizationHeader, operationId := OperationId, args := OperationParams,
    ip := _, name := ApiName, accept := _}) ->
  % Any map for success (will appear as auth_context),
  % {error, _} on bad auth
  #{user_id => 42}.
```

Then, for each `operationId` you want to handle, create a function with that name in the callback module.  
For example, assume part of your schema is:
```yaml
paths:
  '/user/{username}':
    get:
      operationId: getUserByName
      parameters:
        - name: username
          in: path
          required: true
          schema:
            type: string
```

When a client performs `GET /api/prefix/user/Jack`, the corresponding function is called
with a map of parameters as a single argument:
```erlang
petstore_impl:getUserByName(#{username => <<"Jack">>} = _OperationArgs) ->
  #{id => 3418, email => <<"jack@example.com">>}.
```

`OperationArgs` is OperationParams with some added fields:
  * `auth_context` -- a term returned by `authorize/1` callback


Valid callback return values are:
  * `{json, StatusCode, #{} = RespObject}` -- the server will respond with given status and RespObject encoded as JSON in body
  * `{json, StatusCode, undefined}` -- the server will respond with given status and no body
  * `{error, badrequest | enoent | unavailable}` -- shortcuts for statuses 400, 404, 503 accordingly with minimal status description in body
  * `ok` -- status 204 with no body
  * `#{} = RespObject` -- shortcut to `{json, 200, RespObject}`
  * `<<_/binary>>` (valid only for `text` and `csv` content) -- status 200 and exactly this body

## Make sure return values conform to your schema
If your schema describes `responses`, the callback return value is validated against response schema.  
E.g., if you return an atom or a binary where schema requires integer, it will be an error, and response code will be 500.  
Also see validation quirks below.

# Type conversions
| Schema | Erlang |
| ----   | ----   |
| string | binary |
| integer| integer|
| number | number |
| enum   | atom   |
| oneOf(const) | atom |
| boolean | boolean|

# Validation quirks
## `null` vs non-`nullable`
`openapi_handler` allows `undefined` value for non-`nullable` fields and drops these fields.  
This behaviour allows writing simple code like `Response = #{key1 => key1_getter(State)}` without
further complex fillering of each key/value pair.
