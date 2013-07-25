%% represents a tests group (suite)
-record(group, {name = "", location, depth = 0}).

-record(state, {groups_stack = []}).