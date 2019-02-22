%%%-------------------------------------------------------------------
%%% @author kk
%%% @copyright (C) 2019, hongweigaokkx@163.com
%%% @doc
%%%         一致性hash
%%%         todo 增加虚拟节点（其实就是增加一个映射）
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
    is_node_conflict/2,
    add_node/2,
    select_node/2,
    del_node/2
]).

-record(node, {
    hash_k,
    str
}).

-spec hash(S::string()) -> integer().
hash(S) ->
    md5:md5_int(S) rem ?MAX_32.

-spec new() -> orddict().
new() ->
    orddict:new().

%% 检测是否加入的node有冲突
-spec is_node_conflict(NodeS::string(), OrdDict::orddict()) -> boolean().
is_node_conflict(NodeS, OrdDict) ->
    orddict:find(hash(NodeS), OrdDict) =/= false.

%% 增加node
%% 增加时需要满足 is_node_conflict() == true
-spec add_node(NodeS::string(), OrdDict::orddict()) -> ok.
add_node(NodeS, OrdDict) ->
    Node = new_node(NodeS),
    orddict:store(Node#node.hash_k, Node, OrdDict).

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
    orddict:erase(hash(NodeS), OrdDict).

%% ================ intel func ===============
new_node(NodeS) ->
    NodeK = hash(NodeS),
    #node{hash_k = NodeK, str = NodeS}.

select_node_util(_Key, []) ->
    not_find;
select_node_util(Key, [Node | Tail]) ->
    case Key =< Node#node.hash_k of
        true ->
            Node;
        false ->
            select_node_util(Key, Tail)
    end.



