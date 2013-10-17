-module(tap_yaws).

-compile([export_all]).

-include("../include/tapestry.hrl").

start()->
    start(?TAP_DEFAULT_HOST,?YAWS_DEFAULT_ADDRESS,?TAP_DEFAULT_PORT).

start(Servername,IPAddress,Port) ->
    start(Servername,IPAddress,Port,[]).
    
start(_Servername,IPAddress,Port,ConfOptions) when is_list(ConfOptions) ->
    crypto:start(),
    {ok,ParsedAddress} = inet_parse:ipv4_address(IPAddress),
    code:add_pathz(?YAWS_EBIN_DIR),
    file:make_dir(?YAWS_LOG_DIR),
    error_logger:info_msg("Starting Embedded Yaws!~n"),
    GL = [
	  {logdir,?YAWS_LOG_DIR},
	  {ebin_dir, [?YAWS_EBIN_DIR]}],
    SL = [
	  {port,Port},
	  {listen,ParsedAddress}], 
    yaws:start_embedded(?YAWS_DOC_ROOT,SL,GL),
    self().

restart()->
    yaws:stop(),
    start().
    
