-module(openapi_collection_SUITE).
-compile([nowarn_export_all, export_all]).

all() ->
  [
    {group, filter},
    {group,query}
  ].

groups() ->
  [
    {filter, [filter_enum_types]},
    {query, [], [ % parallel
      read_default,

      param_no_value,
      filter_eq,
      filter_like,
      filter_gt,
      filter_gte,
      filter_lt,
      filter_lte,
      filter_gt_lt,
      filter_is_not_null,
      next_cursor,
      prev_cursor,
      encode_filter
  ]}
  ].


init_per_suite(Config) ->
  SchemaPath = code:lib_dir(openapi_handler, test) ++ "/flussonic-230127.json",
  openapi_handler:load_schema(SchemaPath, test_openapi),
  Config.

end_per_suite(Config) ->
  Config.


qs(Proplist) ->
  openapi_collection:parse_qs(cow_qs:qs(Proplist), #{limit => 100, collection_type => stream_config, schema_name => test_openapi}).

q(Proplist) ->
  openapi_collection:list(dataset(), qs(Proplist)).

encode_qs(#{} = Query) ->
  openapi_collection:qs(Query).


filter_enum_types(_) ->
  Dataset = [
    #{id => 1, key1 => testvalue},
    #{id => 2, key1 => <<"testvalue">>},
    #{id => 3, key1 => othervalue},
    #{id => 3, key1 => <<"othervalue">>},
    #{id => 4, key2 => testvalue}
  ],
  #{estimated_count := 2, items := [#{id := 1}, #{id := 2}]} = openapi_collection:list(Dataset, #{filter => #{key1 => [testvalue]}}),
  ok.

read_default(_) ->
  Json = #{<<"name">> => <<"read_default">>},
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  #{static := true, name := <<"read_default">>} = 
    openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true}),

  Stream2 = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema}),
  Stream2 = #{name => <<"read_default">>},
  ok.


param_no_value(_) ->
  openapi_collection:parse_qs(<<"named_by&stats.media_info.title">>, #{limit => 100, collection_type => stream_config, schema_name => test_openapi}),
  ok.

filter_eq(_) ->
  % #{estimated_count := 7} = q([{<<"named_by">>,<<"config">>}]),
  #{estimated_count := 1} = q([{<<"stats.media_info.title">>,<<"C03">>}]),
  ok.

filter_like(_) ->
  % #{items := [#{name := <<"c/08">>},#{name := <<"c/09">>}]} = q([{<<"named_by_like">>,<<"remote">>}]),
  #{items := [#{name := <<"c/03">>}]} = q([{<<"stats.media_info.title_like">>,<<"C03">>}]),
  ok.



filter_gt(_) ->
  #{items := [#{name := <<"c/09">>}]} = q([{<<"name_gt">>,<<"c/08">>}]),
  #{items := [#{name := <<"c/09">>}]} = q([{<<"stats.opened_at_gt">>,<<"1631102874142">>}]),
  ok.



filter_gte(_) ->
  #{items := [#{name := <<"c/08">>},#{name := <<"c/09">>}]} = q([{<<"name_gte">>,<<"c/08">>}]),
  #{items := [#{name := <<"c/08">>},#{name := <<"c/09">>}]} = q([{<<"stats.opened_at_gte">>,<<"1631102874142">>}]),
  ok.


filter_lt(_) ->
  #{items := [#{name := <<"c/01">>}]} = q([{<<"name_lt">>,<<"c/02">>}]),
  #{items := [#{name := <<"c/01">>}]} = q([{<<"stats.opened_at_lt">>,<<"1631102868070">>}]),
  ok.



filter_lte(_) ->
  #{items := [#{name := <<"c/01">>},#{name := <<"c/02">>}]} = q([{<<"name_lte">>,<<"c/02">>}]),
  #{items := [#{name := <<"c/01">>},#{name := <<"c/02">>}]} = q([{<<"stats.opened_at_lte">>,<<"1631102868070">>}]),
  ok.



filter_gt_lt(_) ->
  #{items := [#{name := <<"c/05">>}]} = q([{<<"name_gt">>,<<"c/04">>},{<<"name_lt">>,<<"c/06">>}]),
  #{items := [#{name := <<"c/05">>}]} = q([{<<"stats.opened_at_gt">>,<<"1631102868079">>},{<<"stats.opened_at_lt">>,<<"1631102868089">>}]),
  ok.

filter_is_not_null(_) ->
  #{items := [#{name := <<"c/02">>}]} = q([{<<"dvr_is_not">>,<<"null">>}]),
  ok.


next_cursor(_) ->
  #{next := <<Next1/binary>>, items := [#{name := <<"c/01">>}|_]} = q([{<<"limit">>,<<"3">>},{<<"sort">>,<<"name">>}]),
  #{next := <<Next2/binary>>, items := [#{name := <<"c/04">>}|_]} = q([{<<"limit">>,<<"3">>},{<<"sort">>,<<"name">>},{<<"cursor">>,Next1}]),
  #{next := undefined, items := [#{name := <<"c/07">>}|_]} = q([{<<"limit">>,<<"3">>},{<<"sort">>,<<"name">>},{<<"cursor">>,Next2}]),
  ok.

prev_cursor(_) ->
  #{prev := undefined, next := <<Next1/binary>>, items := [#{name := <<"c/01">>}|_]} = q([{<<"limit">>,<<"3">>},{<<"sort">>,<<"name">>}]),
  #{prev := <<Prev1/binary>>, items := [#{name := <<"c/04">>}|_]} = q([{<<"limit">>,<<"3">>},{<<"sort">>,<<"name">>},{<<"cursor">>,Next1}]),
  #{prev := undefined, items := [#{name := <<"c/01">>}|_]} = q([{<<"limit">>,<<"3">>},{<<"sort">>,<<"name">>},{<<"cursor">>,Prev1}]),
  ok.


encode_filter(_) ->
  <<"drv_is_not=null">> = encode_qs(#{filter => #{<<"drv">> => not_null}}),
  <<"transcoder_is=null">> = encode_qs(#{filter => #{<<"transcoder">> => null}}),

  <<"name_ne=abc">> = encode_qs(#{filter => #{<<"name">> => #{'$ne' => <<"abc">>}}}),
  <<"name_gt=abc">> = encode_qs(#{filter => #{<<"name">> => #{'$gt' => <<"abc">>}}}),
  <<"name_lt=abc">> = encode_qs(#{filter => #{<<"name">> => #{'$lt' => <<"abc">>}}}),
  <<"name_gte=abc">> = encode_qs(#{filter => #{<<"name">> => #{'$gte' => <<"abc">>}}}),
  <<"name_lte=abc">> = encode_qs(#{filter => #{<<"name">> => #{'$lte' => <<"abc">>}}}),
  <<"name_like=abc">> = encode_qs(#{filter => #{<<"name">> => #{'$like' => <<"abc">>}}}),

  <<"parameter=abc%2Cdef">> = encode_qs(#{filter => #{<<"parameter">> => [<<"abc">>,<<"def">>]}}),

  <<"parameter.drv_is_not=null&parameter.transcoder_is=null">> = encode_qs(#{filter => #{<<"parameter">> => #{<<"drv">> => not_null, <<"transcoder">> => null}}}),

  ok.






dataset() ->
[
  #{name => <<"c/01">>,named_by => config,position => 0,
            segment_duration => 1000,static => true,
            stats =>
                #{alive => true,bitrate => 137,bufferings => 0,
                  bytes_in => 174858,bytes_out => 179,client_count => 0,
                  dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a794-0ea4-485b-8ec5-fef6e6e0abf9">>,
                  input_bitrate => 137,input_error_rate => 0,
                  last_access_at => 1631102868058,
                  last_dts => 1631102874487.0117,last_dts_at => 1631102874487,
                  lifetime => 6400.01171875,
                  media_info =>
                      #{tracks =>
                            [#{bitrate => 83,codec => h264,content => video,
                               fps => 24.0,height => 160,language => <<"eng">>,
                               last_gop => 48,level => <<"3.0">>,
                               pix_fmt => yuv420p,pixel_height => 160,
                               pixel_width => 240,profile => <<"Baseline">>,
                               sar_height => 1,sar_width => 1,
                               track_id => <<"v1">>,width => 240},
                             #{bitrate => 54,channels => 2,codec => aac,
                               content => audio,language => <<"eng">>,
                               sample_rate => 48000,track_id => <<"a1">>}]},
                  opened_at => 1631102868058,out_bandwidth => 1,
                  output_bitrate => 137,publish_enabled => false,
                  remote => false,retry_count => 0,running => true,
                  running_transcoder => false,
                  source_id => <<"6138a794-0ed8-4630-8e6a-5cdb2d942746">>,
                  start_running_at => 1631102868058,
                  transcoder_overloaded => false,ts_delay => 665,
                  url => <<"file://vod/bunny.mp4">>},
            urls => [#{url => <<"file://vod/bunny.mp4">>}]},
          #{name => <<"c/02">>,named_by => config,position => 1,
            segment_duration => 1000,static => true,
            dvr => #{root => <<"/storage">>},
            stats =>
                #{alive => true,bitrate => 137,bufferings => 0,
                  bytes_in => 174858,bytes_out => 179,client_count => 0,
                  dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a794-11ba-4f69-bbb0-57f79f9d9edb">>,
                  input_bitrate => 137,input_error_rate => 0,
                  last_access_at => 1631102874135,
                  last_dts => 1631102874492.0117,last_dts_at => 1631102874493,
                  lifetime => 6400.01171875,
                  media_info =>
                      #{tracks =>
                            [#{bitrate => 83,codec => h264,content => video,
                               fps => 24.0,height => 160,language => <<"eng">>,
                               last_gop => 48,level => <<"3.0">>,
                               pix_fmt => yuv420p,pixel_height => 160,
                               pixel_width => 240,profile => <<"Baseline">>,
                               sar_height => 1,sar_width => 1,
                               track_id => <<"v1">>,width => 240},
                             #{bitrate => 54,channels => 2,codec => aac,
                               content => audio,language => <<"eng">>,
                               sample_rate => 48000,track_id => <<"a1">>}]},
                  opened_at => 1631102868070,out_bandwidth => 1,
                  output_bitrate => 137,publish_enabled => false,
                  remote => false,retry_count => 0,running => true,
                  running_transcoder => false,
                  source_id => <<"6138a794-11f0-4e63-a146-33fb8750259a">>,
                  start_running_at => 1631102868070,
                  transcoder_overloaded => false,ts_delay => 659,
                  url => <<"file://vod/bunny.mp4">>},
            urls => [#{url => <<"file://vod/bunny.mp4">>}]},
          #{name => <<"c/03">>,named_by => config,position => 2,
            segment_duration => 1000,static => true,
            stats =>
                #{alive => true,bitrate => 137,bufferings => 0,
                  bytes_in => 174858,bytes_out => 179,client_count => 0,
                  dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a794-12a2-460f-98ca-b815f2bca329">>,
                  input_bitrate => 137,input_error_rate => 0,
                  last_access_at => 1631102874138,
                  last_dts => 1631102874498.0117,last_dts_at => 1631102874498,
                  lifetime => 6400.01171875,
                  media_info =>
                      #{title => <<"C03">>,
                        tracks =>
                            [#{bitrate => 83,codec => h264,content => video,
                               fps => 24.0,height => 160,language => <<"eng">>,
                               last_gop => 48,level => <<"3.0">>,
                               pix_fmt => yuv420p,pixel_height => 160,
                               pixel_width => 240,profile => <<"Baseline">>,
                               sar_height => 1,sar_width => 1,
                               track_id => <<"v1">>,width => 240},
                             #{bitrate => 54,channels => 2,codec => aac,
                               content => audio,language => <<"eng">>,
                               sample_rate => 48000,track_id => <<"a1">>}]},
                  opened_at => 1631102868074,out_bandwidth => 1,
                  output_bitrate => 137,publish_enabled => false,
                  remote => false,retry_count => 0,running => true,
                  running_transcoder => false,
                  source_id => <<"6138a794-133a-4721-a518-bafd670bcc46">>,
                  start_running_at => 1631102868074,
                  transcoder_overloaded => false,ts_delay => 654,
                  url => <<"file://vod/bunny.mp4">>},
            title => <<"C03">>,
            urls => [#{url => <<"file://vod/bunny.mp4">>}]},
          #{name => <<"c/04">>,named_by => config,position => 3,
            static => true,
            stats =>
                #{alive => false,bufferings => 0,bytes_in => 0,bytes_out => 0,
                  client_count => 0,dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a794-13da-4fce-afab-195ff156f6c5">>,
                  input_error_rate => 0,last_access_at => 1631102868079,
                  lifetime => 0,opened_at => 1631102868079,out_bandwidth => 0,
                  publish_enabled => false,remote => false,running => true,
                  running_transcoder => false,
                  start_running_at => 1631102868079,
                  transcoder_overloaded => false},
            title => <<"C04">>},
          #{name => <<"c/05">>,named_by => config,position => 4,
            static => true,
            stats =>
                #{alive => false,bufferings => 0,bytes_in => 0,bytes_out => 0,
                  client_count => 0,dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a794-151e-4c0a-bc3e-bc0ec2bbecc5">>,
                  input_error_rate => 0,last_access_at => 1631102868084,
                  lifetime => 0,opened_at => 1631102868084,out_bandwidth => 0,
                  publish_enabled => false,remote => false,running => true,
                  running_transcoder => false,
                  start_running_at => 1631102868084,
                  transcoder_overloaded => false},
            title => <<"C05">>},
          #{name => <<"c/06">>,named_by => config,position => 5,
            static => true,
            stats =>
                #{alive => false,bufferings => 0,bytes_in => 0,bytes_out => 0,
                  client_count => 0,dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a794-1666-4e22-879a-5e44a4811a2a">>,
                  input_error_rate => 0,last_access_at => 1631102868089,
                  lifetime => 0,opened_at => 1631102868089,out_bandwidth => 0,
                  publish_enabled => false,remote => false,running => true,
                  running_transcoder => false,
                  start_running_at => 1631102868089,
                  transcoder_overloaded => false},
            title => <<"C06">>},
          #{name => <<"c/07">>,named_by => config,position => 6,
            static => true,
            stats =>
                #{alive => false,bufferings => 0,bytes_in => 0,bytes_out => 0,
                  client_count => 0,dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a794-1814-49b2-86b4-2abb9e169cd1">>,
                  input_error_rate => 0,last_access_at => 1631102868096,
                  lifetime => 0,opened_at => 1631102868096,out_bandwidth => 0,
                  publish_enabled => false,remote => false,running => true,
                  running_transcoder => false,
                  start_running_at => 1631102868096,
                  transcoder_overloaded => false},
            title => <<"C07">>},
          #{cluster_key => <<"key1">>,groups => [],name => <<"c/08">>,
            named_by => remote,remote => true,section => stream,
            segment_duration => 1000,source_hostname => <<"src1.local">>,
            static => true,
            stats =>
                #{alive => true,bitrate => 137,bufferings => 0,
                  bytes_in => 98691,bytes_out => 179,client_count => 0,
                  dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a79a-239b-4902-a2dd-d8d10417bd69">>,
                  input_bitrate => 137,input_error_rate => 0,
                  last_access_at => 1631102874142,
                  last_dts => 1631102874133.6877,last_dts_at => 1631102874153,
                  lifetime => 5994.6875,
                  media_info =>
                      #{duration => 2.0e3,title => <<"C08">>,
                        tracks =>
                            [#{bitrate => 83,codec => h264,content => video,
                               fps => 24.0,height => 160,language => <<"eng">>,
                               last_gop => 48,level => <<"3.0">>,
                               pix_fmt => yuv420p,pixel_height => 160,
                               pixel_width => 240,profile => <<"Baseline">>,
                               sar_height => 1,sar_width => 1,
                               track_id => <<"v1">>,width => 240},
                             #{bitrate => 54,channels => 2,codec => aac,
                               content => audio,language => <<"eng">>,
                               sample_rate => 48000,track_id => <<"a1">>}]},
                  opened_at => 1631102874142,out_bandwidth => 0,
                  output_bitrate => 137,publish_enabled => false,
                  remote => true,retry_count => 0,running => true,
                  running_transcoder => false,
                  source_hostname => <<"http://src1.local:6020/c/08">>,
                  source_id => <<"6138a79a-2439-4d9e-9ec6-58baf4bbcc14">>,
                  start_running_at => 1631102874142,
                  transcoder_overloaded => false,ts_delay => 999,
                  url => <<"m4f://src1.local:6020/c/08">>},
            title => <<"C08">>,
            urls =>
                [#{cluster_key => <<"key1">>,
                   url => <<"m4f://src1.local:6020/c/08">>}]},
          #{cluster_key => <<"key1">>,groups => [],name => <<"c/09">>,
            named_by => remote,remote => true,section => stream,
            segment_duration => 1000,source_hostname => <<"src1.local">>,
            static => true,
            stats =>
                #{alive => true,bitrate => 137,bufferings => 0,
                  bytes_in => 98691,bytes_out => 179,client_count => 0,
                  dvr_enabled => false,dvr_only => false,
                  dvr_replication_running => false,
                  id => <<"6138a79a-a254-4742-a6bc-477cb557ece8">>,
                  input_bitrate => 137,input_error_rate => 0,
                  last_access_at => 1631102874649,
                  last_dts => 1631102874164.6875,last_dts_at => 1631102874661,
                  lifetime => 5994.6875,
                  media_info =>
                      #{duration => 2.0e3,title => <<"C09">>,
                        tracks =>
                            [#{bitrate => 83,codec => h264,content => video,
                               fps => 24.0,height => 160,language => <<"eng">>,
                               last_gop => 48,level => <<"3.0">>,
                               pix_fmt => yuv420p,pixel_height => 160,
                               pixel_width => 240,profile => <<"Baseline">>,
                               sar_height => 1,sar_width => 1,
                               track_id => <<"v1">>,width => 240},
                             #{bitrate => 54,channels => 2,codec => aac,
                               content => audio,language => <<"eng">>,
                               sample_rate => 48000,track_id => <<"a1">>}]},
                  opened_at => 1631102874649,out_bandwidth => 0,
                  output_bitrate => 137,publish_enabled => false,
                  remote => true,retry_count => 0,running => true,
                  running_transcoder => false,
                  source_hostname => <<"http://src1.local:6020/c/09">>,
                  source_id => <<"6138a79a-a2d8-4ac8-978b-fc180e2f1970">>,
                  start_running_at => 1631102874649,
                  transcoder_overloaded => false,ts_delay => 491,
                  url => <<"m4f://src1.local:6020/c/09">>},
            title => <<"C09">>,
            urls =>
                [#{cluster_key => <<"key1">>,
                   url => <<"m4f://src1.local:6020/c/09">>}]}
  ].
                   
