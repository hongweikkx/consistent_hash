%%%-------------------------------------------------------------------
%%% @author hongweigaokkx@163.com
%%% @doc
%%%         一致性hash
%%% @end
%%% Created : 21. 二月 2019 15:17
%%%-------------------------------------------------------------------
-module(consistent_hash).
-author("kk").

-define(MAX_32, 4294967296).   %% trunc(math:pow(2, 32))

-type orddict() :: [{Key :: term(), Value :: term()}].
%% API
-export([
    new/0,
    add_node/2,
    add_vistual_node/3,
    select_node/2,
    del_node/2,
    del_node_by_key/2
]).

-record(node, {
    hash_k,
    str,
    is_virtual
}).

-spec new() -> orddict().
new() ->
    orddict:new().

%% 增加node
-spec add_node(NodeS::string(), OrdDict::orddict()) -> orddict() | {error, atom()}.
add_node(NodeS, OrdDict) ->
    case is_node_conflict(NodeS, OrdDict) of
        false ->
            Node = new_node(NodeS),
            orddict:store(Node#node.hash_k, Node, OrdDict);
        true ->
            {error, conflict_key}
    end.

%% 增加虚拟节点
-spec add_vistual_node(NodeS::string(), Key::integer(), OrdDict::orddict()) -> orddict() | {error, atom()}.
add_vistual_node(NodeS, Key, OrdDict) when is_integer(Key) ->
    NKey = Key rem ?MAX_32,
    case is_key_conflict(NKey, OrdDict) of
        false ->
            Node = new_vistual_node(Key, NodeS),
            orddict:store(Node#node.hash_k, Node, OrdDict);
        true ->
            {error, conflict_key}
    end.

%% 通过key选择对应的node 地址
%% must add_node() first
-spec select_node(Keys::string(), OrdDict::orddict()) -> string().
select_node(KeyS, OrdDict) ->
    Key = hash(KeyS),
    SortL = [N || {_, N} <- orddict:to_list(OrdDict)],
    SelectNode =
    case select_node_util(Key, SortL) of
        not_find ->
            hd(SortL);
        Node ->
            Node
    end,
    SelectNode#node.str.

%% 删除node
-spec del_node(NodeS::string(), OrdDict :: orddict()) -> orddict().
del_node(NodeS, OrdDict) ->
    %% 删除node
    NOrdDict = orddict:erase(hash(NodeS), OrdDict),
    %% 删除虚拟节点
    FDel = fun({_, N}, AccDict) ->
        case N#node.str == NodeS of
            true ->
                orddict:erase(N#node.hash_k, AccDict);
            false ->
                AccDict
        end
        end,
    lists:foldl(FDel, NOrdDict, orddict:to_list(NOrdDict)).

%% 通过key删除node
-spec del_node_by_key(NodeKey::integer(), OrdDict :: orddict()) -> {ok, orddict()} | {error, atom()}.
del_node_by_key(NodeKey, OrdDict) ->
    case orddict:find(NodeKey, OrdDict) of
        false ->
            {error, not_find};
        {ok, N} ->
            case N#node.is_virtual of
                true ->
                    {ok, orddict:erase(N#node.hash_k, OrdDict)};
                false ->
                    {ok, del_node(N#node.str, OrdDict)}
            end
    end.

%% ================ intel func ===============
new_node(NodeS) ->
    NodeK = hash(NodeS),
    #node{hash_k = NodeK, str = NodeS, is_virtual = false}.

new_vistual_node(Key, NodeS) ->
    #node{hash_k = Key, str = NodeS, is_virtual = true}.


%% 检测是否加入的node有冲突
-spec is_node_conflict(NodeS::string(), OrdDict::orddict()) -> boolean().
is_node_conflict(NodeS, OrdDict) ->
    is_key_conflict(hash(NodeS),OrdDict).

%% 检测hash key是否有冲突
-spec is_key_conflict(NodeKey::integer(), OrdDict::orddict()) -> boolean().
is_key_conflict(NodeKey, OrdDict) ->
    orddict:find(NodeKey, OrdDict) =/= error.


-spec hash(S::string()) -> integer().
hash(S) ->
    md5:md5_int(S) rem ?MAX_32.

select_node_util(_Key, []) ->
    not_find;
select_node_util(Key, [Node | Tail]) ->
    case Key =< Node#node.hash_k of
        true ->
            Node;
        false ->
            select_node_util(Key, Tail)
    end.



