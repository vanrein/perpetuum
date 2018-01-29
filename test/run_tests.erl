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
-define(LambdaLift3(Atom), fun(X,Y,Z) -> Atom( X,Y,Z ) end).


check_marking( _Expected,[],AccuOK ) ->
	io:format( "check_marking() -> ~p~n", [AccuOK] ),
	if AccuOK ->
		ok;
	true ->
		{error,check_marking}
	end;
check_marking( Expected,[{Place,TokenCount}|Marking],AccuOK ) ->
	% Absent entries in Expected are assumed 0
	%DEBUG% io:format( "Expecting ~p~n",[Expected] ),
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

check_canfire_probed( Module,Instance ) ->
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

% To compare the data trace to a test, we just need to match patterns.
% Event data is set to {886,TransName} by the invoke_traced operation used.
check_datatrace( CBArgs,[{'$init',CBArgs},{'$enquire',CBArgs,[]}],[] ) ->
	ok;
check_datatrace( CBArgs,[Init,{TrNm,CBArgs,{886,TrNm}}|MoreDataTrace],[_State,[TrNm|_Ops]|MoreTest] ) ->
	check_datatrace( CBArgs,[Init|MoreDataTrace],MoreTest );
check_datatrace( CBArgs,DataTrace,Test ) ->
	{error,{check_datatrace,CBArgs,DataTrace,Test}}.

testrun( _Module,_Instance,[],_Invoke,[] ) ->
	ok;
testrun( Module,Instance,[State,Ops|Rest],Invoke,EvDatas ) ->
	%DEBUG% io:format( "testrun() was called~n"),
	% Compare State with Instance:
	Check_State = check_marking( Instance,State ),
	% Compare Ops with Instance:
	Check_Firing_Script = check_canfire_script(        Instance,Ops ),
	Check_Firing_Others = check_canfire_others( Module,Instance,Ops ),
	Check_Firing_Probed = check_canfire_probed( Module,Instance     ),
	% Send Ops#0 event to Instance:
	[ TransName|_ ] = Ops,
	case EvDatas of
	[EvData|EvMore] ->
		ok;
	[]=EvData=EvMore ->
		ok
	end,
	NoReply = Invoke( Instance,TransName,EvData ),
	io:format( "NoReply is ~p~n",[NoReply] ),
	io:format( "Check_xxx results are ~p, ~p, ~p, ~p~n",[Check_State,Check_Firing_Script,Check_Firing_Others,Check_Firing_Probed] ),
	Errors = [ E || {error,E} <- [ Check_State,Check_Firing_Script,Check_Firing_Others,Check_Firing_Probed ] ],
	case Errors of
	[] ->
		testrun( Module,Instance,Rest,Invoke,EvMore );
	_ ->
		{ error,Errors }
	end.

% Three helper functions for submitting an attempted transition.
%
invoke_sync( Instance,TransName,EventData ) ->
	io:format( "Sending event( ~p )~n",[TransName] ),
	noreply = gen_perpetuum:event( Instance,TransName,EventData ).
%
invoke_async( Instance,TransName,EventData ) ->
	io:format( "Sending signal( ~p )~n",[TransName] ),
	gen_perpetuum:signal( Instance,TransName,EventData ).
%
invoke_traced( Instance,TransName,_EventData ) ->
	io:format( "Sending event( ~p )~n",[TransName] ),
	noreply = gen_perpetuum:event( Instance,TransName,{886,TransName} ).
%
invoke_manual( Instance,TransName,EventData ) ->
	io:format( "Manually sending event( ~p )~n",[TransName] ),
	Ref = monitor( process,Instance ),
	Instance ! { event,TransName,EventData,0,Ref,self() },
	receive
	{ reply,Ref,EventReply } ->
		demonitor( Ref,[flush] ),
		io:format( "Received ~p~n",[EventReply] );
	noreply ->
		demonitor( Ref,[flush] ),
		io:format( "No reply!~n" );
	{ 'DOWN',Ref,process,_,_}=Crash ->
		io:format( "Crashed ~p~n",[Crash] ),
		throw( Crash );
	Rubbish ->
		io:format( "Rubbish! ~p~n",[Rubbish] ),
		throw( {rubbish,Rubbish} )
	end.
%
invoke_wildnum( Instance,TransName,EventData ) ->
	WildNum = erlang:adler32( term_to_binary( EventData )),
	io:format( "WildNum ~p for EventData ~p~n",[WildNum,EventData] ),
	invoke_manual( Instance,TransName,WildNum ).

% Split a test into lists with its markings and event lists
%
split_test( [] ) ->
	{ [],[] };
split_test( [Markings,Events|More] ) ->
	{ MoreMarkings,MoreEvents } = split_test( More ),
	{ [Markings|MoreMarkings],[Events|MoreEvents] }.

% Iterate over the options to initiate its requested tests
%
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
	startstop ->
		io:format( "STARTING ~p~n",[Module] ),
		{ ok,Instance } = Module:start( gen_perpetuum,trans_noreply,[] ),
		io:format( "STARTED: ~p~n",[Instance] ),
		Ref = monitor( process,Instance ),
		Module:stop( Instance ),
		io:format( "STOPPED: ~p~n",[Instance] ),
		receive
		{ 'DOWN',Ref,process,_Name1,normal } ->
			%DEBUG% io:format( "Stopped ~p~n",[Name1] );
			ok;
		{ 'DOWN',Ref,process,_Name1,_Reason1 }=Failure1 ->
			%DEBUG% io:format( "CRASH: ~p~n",[Failure1] )
			{ error,{ crashed,Failure1 }}
		after 5000 ->
			demonitor( Ref,[flush] ),
			{ error,timeout }
		end;
	syncrun ->
		{ ok,Instance } = Module:start_link( gen_perpetuum,trans_noreply,[] ),
		testrun( Module,Instance,Test,?LambdaLift3(invoke_sync),[] );
	asyncrun ->
		{ ok,Instance } = Module:start( gen_perpetuum,trans_noreply,[] ),
		testrun( Module,Instance,Test,?LambdaLift3(invoke_async),[] );
	applogic ->
		CBArgs={486,[7,3]},
		{ ok,Instance } = Module:start( stub_applogic,trans_datatrace,CBArgs ),
		testrun( Module,Instance,Test,?LambdaLift3(invoke_traced),[] ),
		DataTrace  = gen_perpetuum:enquire( Instance,[] ),
		check_datatrace( CBArgs,DataTrace,Test );
	wildsum ->
		% We are blunt, in only deriving wild numbers from the input
		{ TestMarkings,TestEvents } = split_test( Test ),
		TestZip = lists:zip( TestMarkings,TestEvents ),
		TestNumbers = [ erlang:adler32( term_to_binary( T ))
				|| T <- TestZip ],
		io:format( "TestNumbers: ~p~n",[TestNumbers] ),
		{ ok,Instance } = Module:start( gen_perpetuum,trans_switch,
					stub_applogic:trans_sum_switch( 0 )),
		testrun( Module,Instance,Test,?LambdaLift3(invoke_wildnum),TestZip ),
		Outcome = gen_perpetuum:enquire( Instance,8967 ),
		Testsum = lists:sum( TestNumbers ),
		if Outcome /= Testsum ->
			{error, { wildsum,Outcome,Testsum }};
		true ->
			ok
		end
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
	if OptNames == [] ->
		io:format( "DESTINED TO FAIL: Please specify tests to run!~n" ),
		false;
	true ->
		Module = list_to_atom( ModName ),
		Options = lists:map( ?LambdaLift(list_to_atom),OptNames ),
		run_tests( Options,Module,Test,true )
	end.

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
	case run_tests( OptStrings,ModName,Test ) of
	true ->
		halt( 0 );
	false ->
		halt( 1 )
	end;
main( _ ) ->
	io:format( "Usage: run_test.erl Module Option...~n"),
	io:format( "Options are:~n"),
	io:format( " - startstop~n"),
	io:format( " - syncrun~n"),
	io:format( " - asyncrun~n"),
	io:format( " - applogic~n"),
	io:format( " - wildsum~n"),
	halt( 1 ).

