-module(eunit_teamcity_tests).

%% API
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_teamcity.hrl").

get_run_function_command(Function, Arguments) ->
    ErlPrefix = "erl -pa ebin -run -eval \"",
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
                                   get_run_function_command(handle_begin, GroupEventArgs))
             },
             {
                 "end_group is handled correctly",
                 ?_assertCmdOutput("##teamcity[testSuiteFinished name='test_module' locationHint='eunit://test_module']\n",
                                   get_run_function_command(handle_end, GroupEventArgs))
             }
         ]
     end
    }.
