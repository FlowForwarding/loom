-module(tap_yaws).

-compile([export_all]).

-include("../include/tapestry.hrl").

start()->
    start(?TAP_DEFAULT_HOST,?TAP_DEFAULT_ADDRESS,?TAP_DEFAULT_PORT).

start(Servername,IPAddress,Port) ->
    start(Servername,IPAddress,Port,[]).
    
start(_Servername,_IPAddress,Port,ConfOptions) when is_list(ConfOptions) ->
    crypto:start(),
    code:add_pathz(?YAWS_EBIN_DIR),
    file:make_dir(?YAWS_LOG_DIR),
    error_logger:info_msg("Starting Embedded Yaws!~n"),
    GL = [
	  {logdir,?YAWS_LOG_DIR},
	  {ebin_dir, [?YAWS_EBIN_DIR]}],
    DocRoot = "./www",
    SL = [
	  {port,Port},
	  {listen,?YAWS_LOCAL_PORT}], 
    yaws:start_embedded(DocRoot,SL,GL),
    self().

stop(_Servername,IPAddress,Port,Docroot) -> 
    error_logger:info_msg("Stopping Embedded Yaws!~n"),
    GL = [{logdir,?YAWS_LOG_DIR},
	  {ebin_dir, [?YAWS_EBIN_DIR]},
	  {id, ?YAWS_ID}],
    SL = [{doc_root,?YAWS_DOC_ROOT},
	  {port,Port},
	  {listen,IPAddress}],
    yaws:stop_embedded(Docroot,SL,GL).
