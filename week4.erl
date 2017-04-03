-module(week4).
-export([server/1,client/2,reverse_proxy/2]).
-import(week2,[palindrome/1]).


server(Owner) ->
    receive
        {From,check,Str} ->
            handle_request(From,Str),
            server(Owner);
        {From,_msg} when From == Owner ->
            Owner ! "Shutting down"
    end.

handle_request(Cid,Str) ->
    case week2:palindrome(Str) of
        true ->
            Cid ! {result,"'" ++ Str ++ "' is a palindrome",self()};
        false ->
            Cid ! {result,"'" ++ Str ++ "' is not a palindrome",self()}
    end.

client(Server,Caller) ->
    receive
        {check,Str} ->
            Server ! {self(),check,Str},
            client(Server,Caller);
        {result,Str,From} ->
            Caller ! {result,Str,From},
            client(Server,From);
        stop ->
            Caller ! "Here to serve you"
    end.

reverse_proxy(Owner,Servers) ->
    receive
        Msg={_Cid,check,_Str} ->
            pick_server(Servers) ! Msg,
            reverse_proxy(Owner,Servers);
        {Cid,_msg} when Cid == Owner ->
            Owner ! "Shutting down"
    end.

-spec pick_server([integer()]) -> integer().
pick_server(Servers) ->
    Index=random:uniform(length(Servers)),
    lists:nth(Index,Servers).
