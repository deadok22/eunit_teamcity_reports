%% Prints all raw messages it gets.
%%
-module(base).
-behaviour(eunit_listener).

%% API
-include_lib("eunit/include/eunit.hrl").

-export([start/0, start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
         terminate/2]).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(_Options) ->
    receive
        {start, _Reference} ->
            ok
    end.




terminate({ok, Data}, _St) ->
    io:format("~nterminate ok ~p~n", [Data]),
    sync_end(ok);
terminate({error, Reason}, _St) ->
    io:format("~nterminate error ~p~n", [Reason]),
    sync_end(error).

sync_end(Result) ->
    receive
        {stop, Reference, ReplyTo} ->
            ReplyTo ! {result, Reference, Result},
            ok
    end.

handle_begin(group, Data, _St) ->
    io:format("~nhandle_begin group ~p~n", [Data]);
handle_begin(test, Data, _St) ->
    io:format("~nhandle_begin test ~p~n", [Data]).

handle_end(group, Data, _St) ->
    io:format("~nhandle_end group ~p~n", [Data]);
handle_end(test, Data, _St) ->
    io:format("~nhandle_end test ~p~n", [Data]).

handle_cancel(group, Data, _St) ->
    io:format("~nhandle_cancel group ~p~n", [Data]);
handle_cancel(test, Data, _St) ->
    io:format("~nhandle_cancel test ~p~n", [Data]).