%%%-------------------------------------------------------------------
%%% @author hongweigaokkx@163.com
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%        md5:md5 and md5:md5_other 有相同的转化效果
%%% @end
%%% Created : 21. 二月 2019 15:14
%%%-------------------------------------------------------------------
-module(md5).
-export([md5/1, md5_other/1, md5_int/1]).



md5(S) ->
    Md5Bin = erlang:md5(S),
    binary_to_list(iolist_to_binary([io_lib:format("~2.16.0b", [Sb]) || Sb <- binary_to_list(Md5Bin)])).

md5_other(S) ->
    Md5Bin = erlang:md5(S),
    Md5List = binary_to_list(Md5Bin),
    lists:flatten(list_to_hex(Md5List)).

-spec md5_int(S::string()) -> integer().
md5_int(S) ->
    Md5List = md5:md5(S),
    list_to_integer(Md5List, 16).


%% ========================== intel func ===========================
list_to_hex(L) ->
    lists:map(fun(X) -> ini_to_hex(X) end, L).

ini_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).
