-module(week4).
-export([server/1,client/2,reverse_proxy/2]).
-import(week2,[palindrome/1]).


server(OwnerPid) ->
    receive
        {Cid,check,Str} ->
            handle_request(Cid,Str),
            server(OwnerPid);
        {Cid,_msg} when Cid == OwnerPid ->
            OwnerPid ! "Shutting down"
    end.

handle_request(Cid,Str) ->
    case week2:palindrome(Str) of
        true ->
            Cid ! {result,"'" ++ Str ++ "' is a palindrome",self()};
        false ->
            Cid ! {result,"'" ++ Str ++ "' is not a palindrome",self()}
    end.

client(ServerPid,CallerPid) ->
    receive
        {check,Str} ->
            ServerPid ! {self(),check,Str},
            client(ServerPid,CallerPid);
        {result,Str} ->
            CallerPid ! {result,Str},
            client(ServerPid,CallerPid);
        stop ->
            CallerPid ! "Here to serve you"
    end.

reverse_proxy(OwnerPid,ServerPids) ->
    receive
        Msg={_Cid,check,_Str} ->
            pick_server(ServerPids) ! Msg,
            reverse_proxy(OwnerPid,ServerPids);
        {Cid,_msg} when Cid == OwnerPid ->
            OwnerPid ! "Shutting down"
    end.

-spec [integer()] -> integer()
pick_server(Pids) ->
    Index=random:uniform(length(Pids)),
    lists:nth(Index,Pids).
