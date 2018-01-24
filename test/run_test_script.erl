#!/usr/bin/env escript
%
% Run a test script against a Petri Net, checking its behaviour.
%
% Test scripts interchange two kinds of element:
%  - Mapping non-empty place names to token counts
%  - Listing available actions
%
% The first element is the initial marking, the last is a lists of actions.
% Places with 0 tokens can be silently dropped from the mappings.
%
% To test, we take the first action off the list, and evolve to the next two.
%
% From: Rick van Rein <rick@openfortress.nl>


check_marking( _Expected,[],AccuOK ) ->
	io:format( "check_marking() -> ~p~n", [AccuOK] ),
	if AccuOK ->
		ok;
	true ->
		{error,check_marking}
	end;
check_marking( Expected,[{Place,TokenCount}|Marking],AccuOK ) ->
	% Absent entries in Expected are assumed 0
	TokensExpected = maps:get( Place,Expected,0 ),
	NewOK = TokenCount == TokensExpected,
	if not NewOK ->
		io:format( "Place ~p is ~p, should be ~p~n",[ Place,TokenCount,TokensExpected ]);
	true ->
		ok
	end,
	check_marking( Expected,Marking,AccuOK and NewOK).
%
check_marking( Instance,Goal ) ->
	Marking = gen_perpetuum:marking( Instance ),
	check_marking( Goal,Marking,true ).

check_canfire( Instance,Goal ) ->
	Firing = gen_perpetuum:canfire( Instance ),
	%DEBUG% io:format( "Firing power is ~p~n",[Firing] ),
	TooMuch   = lists:subtract( Firing,Goal ),
	TooLittle = lists:subtract( Goal,Firing ),
	case {TooMuch,TooLittle} of
	{[],[]} ->
		io:format( "Firing power is just right~n" ),
		ok;
	{_,[]} ->
		io:format( "Too much to fire: ~p~n",[TooMuch] ),
		{ error,check_canfire_toomuch };
	{[],_} ->
		io:format( "Too little to fire: ~p~n",[TooLittle] ),
		{ error,check_canfire_toolittle };
	{_,_} ->
		io:format( "Too much   to fire: ~p~n",[TooMuch  ] ),
		io:format( "Too little to fire: ~p~n",[TooLittle] ),
		{ error,{check_canfire_toomuch,check_canfire_toolittle} }
	end.

test_testrun( _Module,_Instance,[] ) ->
	ok;
test_testrun( Module,Instance,[State,Ops|Rest] ) ->
	%DEBUG% io:format( "test_testrun() was called~n"),
	% Compare State with Instance:
	Check_State = check_marking( Instance,State ),
	% Compare Ops with Instance:
	Check_Firing = check_canfire( Instance,Ops ),
	% Send Ops#0 event to Instance:
	[ TransName|_ ] = Ops,
	%DEBUG% io:format( "Sending event( ~p )~n",[TransName] ),
	noreply = gen_perpetuum:event( Instance,TransName,[] ),
	case {Check_State,Check_Firing} of
	{ok,ok} ->
		test_testrun( Module,Instance,Rest );
	{{error,A},ok} ->
		{ error,A };
	{ok,{error,B}} ->
		{ error,B };
	{{error,A},{error,B}} ->
		{ error,{A,B} }
	end.

run_tests( [],Module,_Test,AccuOK ) ->
	case AccuOK of
	true ->
		Result = 'SUCCESS';
	false ->
		Result = 'FAILED'
	end,
	io:format( "All tests of ~p completed.~n~n", [Module] ),
	io:format( "~p~n", [Result] ),
	AccuOK;
run_tests( [Option|Rest],Module,Test,AccuOK ) ->
	io:format( "Running test ~p against ~p... ", [Option,Module] ),
	TestOutput = case Option of
	testrun ->
		% { ok,Instance } = Module:start_link( gen_perpetuum,trans_noreply,[] ),
		{ ok,Instance } = Module:start( gen_perpetuum,trans_noreply,[] ),
		%DEBG% io:format( "STARTED: ~p~n", [Instance] ),
		Ref = monitor( process,Instance ),
		%DEBUG% Module:stop( Instance ),
		receive
		%TODO% Move breakdown handling away from here
		{ 'DOWN',Ref,process,Name1,normal } ->
			io:format( "Stopped ~p~n",[Name1] );
		{ 'DOWN',Ref,process,_Name1,_Reason1 }=Failure1 ->
			io:format( "CRASH: ~p~n",[Failure1] )
		after 1000 ->
			demonitor( Ref,[flush] )
		end,
		Retval = test_testrun( Module,Instance,Test ),
		receive
		%TODO% Move breakdown handling away from here
		{ 'DOWN',Ref,process,Name2,normal } ->
			io:format( "Stopped ~p~n",[Name2] );
		{ 'DOWN',Ref,process,_Name2,_Reason2 }=Failure2 ->
			io:format( "CRASH: ~p~n",[Failure2] )
		after 1000 ->
			demonitor( Ref,[flush] )
		end,
		Retval
	end,
	case TestOutput of
	ok ->
		io:format( "SUCCESS~n" ),
		NextOK = AccuOK;
	{ error,Reason }  ->
		io:format( "FAILURE ~p~n", [Reason] ),
		NextOK = false
	end,
	run_tests( Rest,Module,Test,NextOK ).
%
run_tests( OptNames,ModName,Test ) ->
	Module = list_to_atom( ModName ),
	Options = lists:map( fun(X)->list_to_atom(X)end,OptNames ),
	run_tests( Options,Module,Test,true ).

load_code( ModName ) ->
	code:add_patha( "../erlang" ),
	code:add_pathz( "../compiler/demo" ),
	{ module,GEN } = code:load_file( gen_perpetuum ),
	{ module,MUT } = code:load_file( list_to_atom( ModName )),
	io:format( "Loaded \"gen_perpetuum\" into module ~p~n\n", [GEN] ),
	io:format( "Loaded ~p into module ~p~n\n", [ModName,MUT] ).

load_test( ModName ) ->
	TestFile = unicode:characters_to_list( [ "./",ModName,".test" ]),
	%DEBUG% io:format( "Loading from ~p~n", [TestFile] ),
	{ ok,TestExpr } = file:script( TestFile ),
	%DEBUG% io:format( "Loaded test expression ~p~n", [TestExpr] ),
	TestExpr.

main( [ModName|OptStrings] ) ->
	load_code( ModName ),
	Test = load_test( ModName ),
	%DEBUG% io:format( "Test options are ~p~n", [AtomicOpts] );
	run_tests( OptStrings,ModName,Test );
main( _ ) ->
	io:format( "Usage: run_test_script.erl Module Option...~n"),
	io:format( "Options are:~n"),
	io:format( " - testrun:~n"),
	halt( 1 ).

