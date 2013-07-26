-module(eunit_teamcity_tests).

%% API
-export([wrap_test_in_root_node_code/2, two_functions_outside_of_suite_code/1]).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_teamcity.hrl").

-record(fun_call, {module = undefined, function = undefined, args = []}).

get_run_functions_command(FunCalls) ->
    get_run_text_command(create_function_call_sequence_string(FunCalls)).

get_run_text_command(Text) ->
    ErlPrefix = "erl -pa .eunit -run -eval \"",
    ErlSuffix = "\" -s init stop -noshell",
    Command = ErlPrefix ++ escape_quotes(Text) ++ ErlSuffix,
    io:format("~s~n", [Command]),
    Command.

create_function_call_sequence_string(FunCalls) ->
    string:join(lists:map(fun create_function_call_string/1, FunCalls), ", ") ++ ".".

create_function_call_string(#fun_call{module = Module, function = Function, args = Arguments}) ->
    QualifiedFunctionName = atom_to_list(Module) ++ ":" ++ atom_to_list(Function),
    ArgumentsString = get_arguments_string(Arguments),
    QualifiedFunctionName ++ "(" ++ ArgumentsString ++ ")".

get_arguments_string(Arguments) ->
    ArgumentTexts = [lists:flatten(io_lib:format("~p", [Arg])) || Arg <- Arguments],
    string:join(ArgumentTexts, ", ").

escape_quotes(String) ->
    lists:flatten(lists:map(fun
                                ($\") -> "\\\"";
                                (X) -> X
                            end, String)).

group_output_test_() ->
    {"Check reporter produces valid output when handling group events",
     setup,
     fun() ->
         Data = [{id, [1]},
                 {desc, <<"module 'test_module'">>}],
         State = #state{},
         [group, Data, State]
     end,
     fun(_) -> ok end,
     fun(GroupEventArgs) ->
         [
             {
                 "begin_group is handled correctly",
                 ?_assertCmdOutput("##teamcity[testSuiteStarted name='test_module' locationHint='eunit://test_module']\n",
                                   get_run_functions_command([#fun_call{module = eunit_teamcity, function = handle_begin, args = GroupEventArgs}]))
             },
             {
                 "end_group is handled correctly",
                 ?_assertCmdOutput("##teamcity[testSuiteFinished name='test_module' locationHint='eunit://test_module']\n",
                                   get_run_functions_command([#fun_call{module = eunit_teamcity, function = handle_end, args = GroupEventArgs}]))
             }
         ]
     end
    }.

single_function_outside_of_suite_output_test() ->
    StartTestData = [{id, [1]},
                     {source, {test_suite, test_test, 4}},
                     {line, 81}],
    State = #state{},
    Args = [test, StartTestData, State],
    ?assertCmdOutput("##teamcity[testSuiteStarted name='test_suite' locationHint='eunit://test_suite']\n"
                     "##teamcity[testStarted name='test_test' locationHint='eunit://test_suite:test_test:81']\n",
                     get_run_functions_command([#fun_call{module = eunit_teamcity, function = handle_begin, args = Args}])).


two_functions_from_one_module_outside_of_suite_test() ->
    ?assertCmdOutput("##teamcity[testSuiteStarted name='test_suite' locationHint='eunit://test_suite']\n"
                     "##teamcity[testStarted name='test1_test' locationHint='eunit://test_suite:test1_test:81']\n"
                     "##teamcity[testFinished name='test1_test' locationHint='eunit://test_suite:test1_test:81']\n"
                     "##teamcity[testStarted name='test2_test' locationHint='eunit://test_suite:test2_test:88']\n"
                     "##teamcity[testFinished name='test2_test' locationHint='eunit://test_suite:test2_test:88']\n"
                     "##teamcity[testSuiteFinished name='test_suite' locationHint='eunit://test_suite']\n",
                     get_run_functions_command([#fun_call{module = eunit_teamcity_tests,
                                                          function = wrap_test_in_root_node_code,
                                                          args = [eunit_teamcity_tests, two_functions_outside_of_suite_code]}])).


%%
%% functions ending with _code are executed in tests.
%%

wrap_test_in_root_node_code(Module, Function) ->
    RootData = [{id, []}],
    State = eunit_teamcity:handle_begin(group, RootData, #state{}),
    State1 = Module:Function(State),
    eunit_teamcity:handle_end(group, RootData, State1).

two_functions_outside_of_suite_code(InitialState) ->
    StartFirstTestData = [{id, [1]}, {source, {test_suite, test1_test, 0}}, {line, 81}],
    State = eunit_teamcity:handle_begin(test, StartFirstTestData, InitialState),
    State1 = eunit_teamcity:handle_end(test, [{status, ok} | StartFirstTestData], State),
    StartSecondTestData = [{id, [2]}, {source, {test_suite, test2_test, 0}}, {line, 88}],
    State2 = eunit_teamcity:handle_begin(test, StartSecondTestData, State1),
    eunit_teamcity:handle_end(test, [{status, ok} | StartSecondTestData], State2).