%% Copyright
-module(eunit_teamcity).
-behaviour(eunit_listener).

%% API
-include_lib("eunit/include/eunit.hrl").

-export([start/0, start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
         terminate/2]).

-include("eunit_teamcity.hrl").

%%
%% Behaviour functions
%%

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(_Options) ->
    receive
        {start, _Reference} ->
            #state{}
    end.

terminate({ok, _Data}, _St) ->
    sync_end(ok);
terminate({error, Reason}, _St) ->
    io:format("terminate error ~p~n", [Reason]),
    sync_end(error).

sync_end(Result) ->
    receive
        {stop, Reference, ReplyTo} ->
            ReplyTo ! {result, Reference, Result},
            ok
    end.

handle_begin(group, Data, St) ->
    enter_group(Data, St);
handle_begin(test, Data, St) ->
    enter_test(Data, St).

handle_end(group, Data, St) ->
    leave_group(Data, St);
handle_end(test, Data, St) ->
    leave_test(Data, St).

handle_cancel(group, Data, St) ->
    fail_group(Data, St);
handle_cancel(test, Data, St) ->
    fail_test(Data, St).

%%
%% Logic impl
%%
%% these functions should modify state

enter_group(Data, State) ->
    case (get_location(Data)) of
        undefined ->
            State; %%TODO we're at root node - do something
        _ ->
            Group = create_group(Data),
            case should_enter_group(Group, State) of
                true ->
                    print_teamcity_message(enter_group, Data, State),
                    add_group(Group, State);
                _ -> State
            end
    end.

leave_group(Data, State) ->
    Location = get_location(Data),
    case Location of
        undefined ->
            State; %%TODO we're at root node - do something
        Location ->
            Group = create_group(Data),
            case should_leave_group(Group, State) of
                true ->
                    print_teamcity_message(leave_group, Data, State),
                    remove_group(State);
                _ -> State
            end
    end.

enter_test(Data, State) ->
    print_teamcity_message(enter_test, Data, State),
    State.

leave_test(Data, State) ->
    print_teamcity_message(leave_test, Data, State),
    State.

fail_test(Data, State) ->
    print_teamcity_message(fail_test, Data, State),
    State.

fail_group(Data, State) ->
    Location = get_location(Data),
    case Location of
        undefined ->
            print_teamcity_message(fail_group, Data, State),
            State;
        Location ->
            Group = create_group(Data),
            case should_leave_group(Group, State) of
                true ->
                    print_teamcity_message(fail_group, Data, State),
                    remove_group(State);
                _ -> State
            end
    end.

get_depth(Data) ->
    case proplists:get_value(id, Data) of
        List when is_list(List) -> length(List);
        _ -> 0
    end.

create_group(Data) ->
    Name = get_group_name(Data),
    Location = get_location(Data),
    Depth = get_depth(Data),
    #group{name = Name, location = Location, depth = Depth}.

should_leave_group(_Group, #state{groups_stack = []}) ->
    true;
should_leave_group(Group, #state{groups_stack = [LastGroup|_]}) ->
    LastGroup =:= Group.

should_enter_group(_NewGroup, #state{groups_stack = []}) ->
    true;
should_enter_group(NewGroup, #state{groups_stack = [Group|_]}) ->
    case (Group#group.depth + 1) == NewGroup#group.depth of
        true ->
            (Group#group.name ++ "_test")
            ==
            NewGroup#group.name;
        _ -> true
    end.

add_group(Group, State) ->
    GroupsStack = State#state.groups_stack,
    State#state{groups_stack = [Group|GroupsStack]}.

remove_group(State) ->
    case State#state.groups_stack of
        [_Group|Groups] -> State#state{groups_stack = Groups};
        _ -> State
    end.

print_teamcity_message(enter_group, Data, _State) ->
    print_group_teamcity_message("testSuiteStarted", Data);
print_teamcity_message(leave_group, Data, _State) ->
    print_group_teamcity_message("testSuiteFinished", Data);
print_teamcity_message(enter_test, Data, State) ->
    print_test_teamcity_message("testStarted", Data, State);
print_teamcity_message(leave_test, Data, State) ->
    print_test_failed_message(Data, State),
    print_test_teamcity_message("testFinished", Data, State);
print_teamcity_message(fail_group, Data, _State) ->
    print_group_failed_message(Data),
    print_group_teamcity_message("testSuiteFinished", Data);
print_teamcity_message(fail_test, _Data, _State) ->
    whaaaat; %TODO handle test cancellation. See message example below
%% handle_cancel test [{id,[1]},
%% {reason,timeout},
%% {desc,<<"module 'eunit_teamcity_tests'">>},
%% {source,{eunit_teamcity_tests,handle_begin_output_test,0}},
%% {line,0}]

%%     print_test_failed_message(Data),
%%     print_test_teamcity_message("testFinished", Data);
print_teamcity_message(Type, Details, _State) ->
    io:format("Unknown message type: ~p. Details: ~p~n", [Type, Details]).

print_group_failed_message(Data) ->
    case proplists:get_value(reason, Data) of
        undefined -> ok;
        {abort, GroupAbortDetails} ->
            print_group_failed_abort_message(GroupAbortDetails);
        Reason ->
            io:format("~s~w~n", ["Test suite failed. Reason:", Reason])
    end.

print_group_failed_abort_message({bad_generator, {Location, _}}) ->
    Source = {source, Location},
    Data = [Source],
    State = #state{},
    print_teamcity_message(enter_test, Data, State),
    QualifiedGeneratorName = atom_to_list(get_module(Location)) ++ ":" ++
                             atom_to_list(get_function(Location)),
    NameProperty = name_property(QualifiedGeneratorName),
    LocationProperty = location_property(Location),
    Message = io_lib:format("Bad generator function: ~s", [QualifiedGeneratorName]),
    MessageProperty = message_property(Message),
    join_and_print_teamcity_message(["testFailed", NameProperty,
                                     LocationProperty, MessageProperty]);
print_group_failed_abort_message(GroupAbortDetails) ->
    io:format("~w~n", GroupAbortDetails).



print_test_failed_message(Data, State) ->
    case proplists:get_value(status, Data) of
        {error, _} ->
            Location = get_location(Data),
            LocationProperty = location_property(Location),
            NameProperty = name_property(get_test_name(Data, State)),
            MessageProperty = message_property(get_test_failed_message(Data)),
            DetailsProperty = details_property(get_test_failed_details(Data)),
            join_and_print_teamcity_message(["testFailed", NameProperty,
                                             LocationProperty, MessageProperty,
                                             DetailsProperty
                                            ]);
        _ -> ok
    end.

print_test_teamcity_message(MessageName, Data, State) ->
    Location = get_location(Data),
    LocationProperty = location_property(Location),
    NameProperty = name_property(get_test_name(Data, State)),
    join_and_print_teamcity_message([MessageName, NameProperty, LocationProperty]).

print_group_teamcity_message(MessageName, Data) ->
    Location = get_location(Data),
    if
        Location =:= undefined -> ok; %root node
        true ->
            LocationProperty = location_property(Location),
            NameProperty = name_property(get_group_name(Data)),
            join_and_print_teamcity_message([MessageName, NameProperty, LocationProperty])
    end.

join_and_print_teamcity_message(Attributes) ->
    Message = string:join(Attributes, " "),
    io:format("~s~n", ["##teamcity[" ++ Message ++ "]"]).

get_location(Data) ->
    case proplists:get_value(source, Data) of
        {Module, Function, _Arity} ->
            case proplists:get_value(line, Data) of
                Line when is_integer(Line) ->
                    {Module, Function, Line};
                _ -> {Module, Function, -1}
            end;
        {Module} ->
            {Module};
        undefined ->
            parse_module_name(proplists:get_value(desc, Data))
    end.

get_test_failed_details(Data) ->
    case proplists:get_value(output, Data) of
        undefined ->
            undefined;
        <<>> ->
            undefined;
        Output ->
            binary_to_list(Output)
    end.

get_test_failed_message(Data) ->
    case proplists:get_value(status, Data) of
        {error, Exception} ->
            lists:flatten(eunit_lib:format_exception(Exception));
        _ -> "Test failed"
    end.

get_group_name(Data) ->
    Desc = proplists:get_value(desc, Data),
    DescModuleName = parse_module_name(Desc),
    if
        DescModuleName =/= undefined ->
            case DescModuleName of
                {ModuleName} -> atom_to_list(ModuleName)
            end;
        Desc =/= undefined ->
            Desc;
        true ->
            Module = get_module(get_location(Data)),
            atom_to_list(Module)
    end.

get_test_name(Data, State) ->
    Desc = proplists:get_value(desc, Data),
    DescModuleName = parse_module_name(Desc),
    if
        DescModuleName =:= undefined andalso Desc =/= undefined ->
            binary_to_list(Desc);
        true ->
            Location = get_location(Data),
            FunctionName = atom_to_list(get_function(Location)),
            case State#state.groups_stack of
                [] ->
                    ModuleName = atom_to_list(get_module(Location)),
                    ModuleName ++ ":" ++ FunctionName;
                _ ->
                    FunctionName
            end
    end.

get_function({_Module, Function, _Line}) ->
    Function;
get_function(_Location) ->
    undefined.

get_module({Module}) ->
    Module;
get_module({Module, _Function, _Line}) ->
    Module;
get_module(_Location) ->
    undefined.

parse_module_name(undefined) -> undefined;
parse_module_name(DescBinaryString) ->
    DescString = binary_to_list(DescBinaryString),
    case re:run(DescString, "^module '([^']+)'$") of
        nomatch ->
            undefined;
        {match, [_, {FirstIdx, LastIdx}]} ->
            ModuleName = string:sub_string(DescString, FirstIdx + 1, FirstIdx + LastIdx),
            {list_to_atom(ModuleName)}
    end.

message_property(Message) ->
    attribute_str("message", Message).

details_property(Details) ->
    attribute_str("details", Details).

name_property(Name) ->
    attribute_str("name", Name).

location_property(undefined) -> "";
location_property(Location) ->
    attribute_str("locationHint", "eunit://" ++ location_str(Location)).
location_str({Module}) ->
    atom_to_list(Module);
location_str({Module, Function, Line}) ->
    M = atom_to_list(Module),
    F = atom_to_list(Function),
    L = lists:flatten(io_lib:format("~p", [Line])),
    M ++ ":" ++ F ++ ":" ++ L.

attribute_str(_AttributeName, undefined) -> "";
attribute_str(AttributeName, AttributeValue) ->
    AttributeName ++ "=" ++ "'" ++ escape_chars(AttributeValue) ++ "'".

escape_chars(Str) ->
    lists:flatten(lists:map(fun escape_map/1, Str)).

escape_map($')  -> "|'";
escape_map($\n) -> "|n";
escape_map($\r) -> "|r";
escape_map($|)  -> "||";
escape_map($])  -> "|]";
escape_map(X)   -> X.