-module(ofs_util).

-export([conf_default/3]).

conf_default(Entry, GuardFun, Default) ->
    case application:get_env(of_driver, Entry) of
        {ok, Value} ->
            case GuardFun(Value) of
                true -> Value;
                false -> Default
            end;
        _ ->
            Default
    end.
