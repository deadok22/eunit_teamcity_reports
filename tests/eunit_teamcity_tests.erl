-module(eunit_teamcity_tests).

%% API
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_teamcity.hrl").

get_run_function_command(Function, Arguments) ->
    ErlPrefix = "erl -pa \"out/production/eunit_teamcity\" -run -eval \"",
    ErlSuffix = "\" -s init stop -noshell",
    FunctionName = "eunit_teamcity:" ++ atom_to_list(Function),
    ArgumentsString = get_arguments_string(Arguments),
    ErlPrefix ++ escape_quotes(FunctionName ++ "(" ++ ArgumentsString ++ ").") ++ ErlSuffix.

get_arguments_string(Arguments) ->
    ArgumentTexts = [lists:flatten(io_lib:format("~p", [Arg])) || Arg <- Arguments],
    string:join(ArgumentTexts, ", ").

escape_quotes(String) ->
    lists:flatten(lists:map(fun
                                ($\") -> "\\\"";
                                (X) -> X
                            end, String)).

handle_begin_output_test() ->
    Data = [{id, [1]},
            {source, {test_module}},
            {desc, <<"module 'test_module'">>}],
    State = #state{},
    Command = get_run_function_command(handle_begin, [group, Data, State]),
    ?assertCmdOutput("##teamcity[testSuiteStarted name='test_module' locationHint='eunit://test_module']\n", Command).