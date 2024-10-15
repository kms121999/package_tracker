





%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
_test_()->
	[% happy thoughts
   ?_assertEqual({expected}, update(actual)),
	 % nasty thoughts start here
	 ?_assertEqual(1,default_compare(bob,-100))

	].

-endif.