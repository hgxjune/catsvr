%%%-------------------------------------------------------------------
%% @doc web_echo public API
%% @end
%%%-------------------------------------------------------------------

-module(web_echo).
-include("debug.hrl").


-export([init/2]).


init(Req0, Opts) ->
    try
        case maps:get(method, Req0) of
            <<"GET">> -> request_get(Req0, Opts);
            <<"POST">> -> request_post(Req0, Opts)
        end
    catch
        Class:Reason:Stack ->
            ?trace("Req0(~p) ~nweb critical error, Class:~p, Reason:~p, Stack:~p", [Req0, Class, Reason, Stack]),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"404">>, Req0),
            {ok, Req, Opts}
    end.


%%%-------------------------------------------------------------------
%% internal functions
request_get(Req0, Opts) ->
    Path = maps:get(path, Req0),
    Qs = maps:get(qs, Req0),
    request_get(Req0, Opts, Path, Qs).

request_get(Req0, Opts, <<"/who">>, Body) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"master: ", Body/binary>>, Req0),
    {ok, Req, Opts};
request_get(Req0, Opts, _, _) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"404">>, Req0),
    {ok, Req, Opts}.


%%%-------------------------------------------------------------------
request_post(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"404">>, Req0),
    {ok, Req, Opts}.
