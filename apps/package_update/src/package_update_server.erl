





%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
 create_test_()->
	[ %happy path
   ?_assertEqual(,default_compare(10,2)),
	 %nasty thoughts start here
	 ?_assertEqual(1,default_compare({bob,sue},{alice,jane})),
	].
-endif.