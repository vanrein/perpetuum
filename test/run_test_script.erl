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


-define(LambdaLift(Atom), fun(X) -> Atom( X ) end).


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

check_canfire_script( Instance,Goal ) ->
	Firing = gen_perpetuum:canfire( Instance ),
	%DEBUG% io:format( "Firing power is ~p~n",[Firing] ),
	TooMuch   = lists:subtract( Firing,Goal ),
	TooLittle = lists:subtract( Goal,Firing ),
	case {TooMuch,TooLittle} of
	{[],[]} ->
		io:format( "check_canfire_script: Firing power is just right~n" ),
		ok;
	{_,[]} ->
		io:format( "check_canfire_script: Too much to fire: ~p~n",[TooMuch] ),
		{ error,check_canfire_script_toomuch };
	{[],_} ->
		io:format( "check_canfire_script: Too little to fire: ~p~n",[TooLittle] ),
		{ error,check_canfire_script_toolittle };
	{_,_} ->
		io:format( "check_canfire_script: Too much   to fire: ~p~n",[TooMuch  ] ),
		io:format( "check_canfire_script: Too little to fire: ~p~n",[TooLittle] ),
		{ error,{check_canfire_script_toomuch,check_canfire_script_toolittle} }
	end.

check_canfire_others( Module,Instance,Goal ) ->
	AllAmmo = Module:transit(),
	TestAmmo = fun( TransName ) ->
		gen_perpetuum:canfire( Instance,TransName )
	end,
	Firing = lists:filter( TestAmmo,AllAmmo ),
	TooMuch   = lists:subtract( Firing,Goal ),
	TooLittle = lists:subtract( Goal,Firing ),
	case {TooMuch,TooLittle} of
	{[],[]} ->
		io:format( "check_canfire_others: Firing power is just right~n" ),
		ok;
	{_,[]} ->
		io:format( "check_canfire_others: Too much to fire: ~p~n",[TooMuch] ),
		{ error,check_canfire_others_toomuch };
	{[],_} ->
		io:format( "check_canfire_others: Too little to fire: ~p~n",[TooLittle] ),
		{ error,check_canfire_others_toolittle };
	{_,_} ->
		io:format( "check_canfire_others: Too much   to fire: ~p~n",[TooMuch  ] ),
		io:format( "check_canfire_others: Too little to fire: ~p~n",[TooLittle] ),
		{ error,{check_canfire_others_toomuch,check_canfire_others_toolittle} }
	end.

check_canfire_probed( Module,Instance,Goal ) ->
	Firing = gen_perpetuum:canfire( Instance ),
	BadAmmo = lists:subtract( Module:transit(),Firing ),
	TestAmmo = fun( TransName ) ->
		case gen_perpetuum:event( Instance,TransName,[] ) of
		{ retry,marking } ->
			false;
		_ ->
			true
		end
	end,
	TooMuch = lists:filter( TestAmmo,BadAmmo ),
	if TooMuch == [] ->
		io:format( "check_canfire_probed: Firing power is just right~n" ),
		ok;
	true ->
		io:format( "check_canfire_others: Too much to fire: ~p~n",[TooMuch] ),
		{ error,check_canfire_probed_toomuch }
	end.

test_testrun( _Module,_Instance,[] ) ->
	ok;
test_testrun( Module,Instance,[State,Ops|Rest] ) ->
	%DEBUG% io:format( "test_testrun() was called~n"),
	% Compare State with Instance:
	Check_State = check_marking( Instance,State ),
	% Compare Ops with Instance:
	Check_Firing_Script = check_canfire_script(        Instance,Ops ),
	Check_Firing_Others = check_canfire_others( Module,Instance,Ops ),
	Check_Firing_Probed = check_canfire_probed( Module,Instance,Ops ),
	% Send Ops#0 event to Instance:
	[ TransName|_ ] = Ops,
	io:format( "Sending event( ~p )~n",[TransName] ),
	%DEBUG% Ref = monitor( process,Instance ),
	%DEBUG% Instance ! { event,TransName,[],0,Ref,self() },
	%DEBUG% receive
	%DEBUG% { reply,Ref,EventReply } ->
	%DEBUG% 	demonitor( Ref,[flush] ),
	%DEBUG% 	io:format( "Received ~p~n",[EventReply] );
	%DEBUG% noreply ->
	%DEBUG% 	demonitor( Ref,[flush] ),
	%DEBUG% 	io:format( "No reply!~n" );
	%DEBUG% { 'DOWN',Ref,process,_,_}=Crash -> io:format( "Crashed ~p~n",[Crash] );
	%DEBUG% Rubbish ->
	%DEBUG% 	io:format( "Rubbish! ~p~n",[Rubbish] )
	%DEBUG% end,
	noreply = gen_perpetuum:event( Instance,TransName,[] ),
	%TODO% io:format( "NoReply is ~p~n",[NoReply] ),
	case {Check_State,Check_Firing_Script} of
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
	io:format( "Running test ~p against ~p...~n", [Option,Module] ),
	TestOutput = case Option of
	testrun ->
		% { ok,Instance } = Module:start_link( gen_perpetuum,trans_noreply,[] ),
		%DEBUG% io:format( "STARTING ~p~n",[Module] ),
		{ ok,Instance } = Module:start( gen_perpetuum,trans_noreply,[] ),
		%DEBUG% io:format( "STARTED: ~p~n",[Instance] ),
		%DEBUG% Ref = monitor( process,Instance ),
		%DEBUG% Module:stop( Instance ),
		%DEBUG% receive
		%DEBUG% %TODO% Move breakdown handling away from here
		%DEBUG% { 'DOWN',Ref,process,Name1,normal } ->
		%DEBUG% 	io:format( "Stopped ~p~n",[Name1] );
		%DEBUG% { 'DOWN',Ref,process,_Name1,_Reason1 }=Failure1 ->
		%DEBUG% 	io:format( "CRASH: ~p~n",[Failure1] )
		%DEBUG% after 1000 ->
		%DEBUG% 	demonitor( Ref,[flush] )
		%DEBUG% end,
		test_testrun( Module,Instance,Test )
	end,
	case TestOutput of
	ok ->
		io:format( "...SUCCESS~n" ),
		NextOK = AccuOK;
	{ error,Reason }  ->
		io:format( "...FAILURE ~p~n", [Reason] ),
		NextOK = false
	end,
	run_tests( Rest,Module,Test,NextOK ).
%
run_tests( OptNames,ModName,Test ) ->
	Module = list_to_atom( ModName ),
	Options = lists:map( ?LambdaLift(list_to_atom),OptNames ),
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

