%%%-------------------------------------------------------------------
%% @doc db public API
%% @end
%%%-------------------------------------------------------------------

-module(db).
-include("debug.hrl").


-export([connect/0]).
-export([disconnect/1]).
-export([insert/3]).
-export([delete_one/3]).
-export([delete/3]).
-export([find_one/3]).
-export([find/3]).
-export([count/3]).
-export([update/4]).

-export([to_binstring/1]).
-export([binstring_to_string/1]).
-export([binstring_to_integer/1]).
-export([binstring_to_atom/1]).

-export([term_to_binstring/1]).
-export([binstring_to_term/1]).

-export([keystore/4]).


%%% mongodb 两种使用方法：
%% 1. 使用 mc_worker_api 接口，直连数据库
%% 2. 使用 mongo_api 建立 mongoc 池
%% 20211111 采用 1 方式，组织好事务，事务结束后释放掉
%% 增删查改数据类型见 mongo_protocol.hrl

%%% 时序问题
%% 1. 属于系统的数据，只能由对应的系统来操作
%% 2. 用户的数据，只能由本用户操作
%% 3. 所有操作数据的进程，统一 handle_call stop 接口，停服时调用，数据落地


%%% mysql
%% 使用 https://gitee.com/tercero/mysqlboy


%% -----------------------------------------------------------------------------
%% 连接管理
connect() ->
    Config = db_app:get_config(),
    {ok, Connection} = mc_worker_api:connect(Config),
    Connection.

disconnect(Connection) ->
    mc_worker_api:disconnect(Connection).


%% 增：只使用 map 数据
%-spec insert(pid(), collection(), map()) -> {{boolean(), map()}, map()}.
insert(Connection, Collection, Doc) ->
    mc_worker_api:insert(Connection, Collection, Doc).


%% 删
%-spec delete_one(pid(), collection(), selector()) -> {boolean(), map()}.
delete_one(Connection, Collection, Selector) ->
    mc_worker_api:delete_one(Connection, Collection, Selector).

%-spec delete(pid(), collection(), selector()) -> {boolean(), map()}.
delete(Connection, Collection, Selector) ->
    mc_worker_api:delete(Connection, Collection, Selector).


%% 查
%-spec find_one(pid(), collection(), selector()) -> map() | undefined.
find_one(Connection, Collection, Selector) ->
    mc_worker_api:find_one(Connection, Collection, Selector).

%-spec find(pid(), collection(), selector()) -> {ok, cursor()} | [].
find(Connection, Collection, Selector) ->
    mc_worker_api:find(Connection, Collection, Selector).

%-spec count(pid(), collection(), selector()) -> integer().
count(Connection, Collection, Selector) ->
    mc_worker_api:count(Connection, Collection, Selector).


%% 改：只使用 map 数据
%-spec update(pid(), collection(), selector(), map()) -> {boolean(), map()}.
update(Connection, Collection, Selector, Doc) ->
    mc_worker_api:update(Connection, Collection, Selector, Doc).


%% -----------------------------------------------------------------------------
%% 入库：转换成 bin 字符串
to_binstring(V) when is_list(V)    -> list_to_binary(V);
to_binstring(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_binstring(V) when is_integer(V) -> integer_to_binary(V);
to_binstring(V) when is_binary(V)  -> V;
to_binstring(V) ->
    ?ERROR("db data badarg:~p", [V]),
    error(badarg).

%% 出库：从 bin 字符串转换
binstring_to_string(V)  when is_binary(V) -> binary_to_list(V);
binstring_to_string(V)  -> V.

binstring_to_integer(V) when is_binary(V) -> binary_to_integer(V);
binstring_to_integer(V) -> V.

binstring_to_atom(V)    when is_binary(V) -> binary_to_atom(V, utf8);
binstring_to_atom(V)    -> V.


%% -----------------------------------------------------------------------------
%% 任意数据互转字符串
term_to_binstring(Term) ->
    list_to_binary( lists:flatten( io_lib:write( Term ) ) ).

binstring_to_term(BinString) ->
    String     = db:binstring_to_string(BinString),
    {ok, T, _} = erl_scan:string(String++"."),
    {ok, Term} = erl_parse:parse_term(T),
    Term.


%% -----------------------------------------------------------------------------
%% 同 lists:keystore 列表元素为 map
keystore(Key, Value, List, Map) when is_atom(Key) ->
    keystore(to_binstring(Key), Value, List, Map);
keystore(Key, Value, List, Map) ->
    keystore2(Key, Value, List, Map).

keystore2(Key, Value, [H|T], New) ->
    case maps:get(Key, H) of
        Value -> [New | T];
        _ -> [H|keystore2(Key, Value, T, New)]
    end;
keystore2(_Key, _Value, [], New) ->
    [New].





%% internal functions








