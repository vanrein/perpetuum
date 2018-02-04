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
%
invoke_delayed_signal( Instance,TransName,_EventData ) ->
	Now = erlang:monotonic_time( millisecond ),
	io:format( "Invoking ~p in 5 ms~n",[TransName] ),
	gen_perpetuum:signal( Instance,TransName,Now+5,25 ),
	timer:sleep( 10 ),
	noreply.
%
invoke_delayed_event( Instance,TransName,_EventData ) ->
	Now = erlang:monotonic_time( millisecond ),
	io:format( "Invoking ~p in 5 ms~n",[TransName] ),
	gen_perpetuum:event( Instance,TransName,Now+5,25 ),
	timer:sleep( 10 ),
	noreply.
%
invoke_and_linger( Instance,TransName,EventData ) ->
	gen_perpetuum:event( Instance,TransName,EventData ),
	timer:sleep( 50 ),
	noreply.

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
		end;

	syncrun_delayed ->
		StartTime = erlang:monotonic_time(millisecond),
		CBArgs = { StartTime,[ gen_perpetuum,trans_noreply,[] ]},
		{ ok,Instance } = Module:start( stub_applogic,trans_delayed,CBArgs ),
		testrun( Module,Instance,Test,?LambdaLift3(invoke_delayed_event),[] );

	asyncrun_delayed ->
		StartTime = erlang:monotonic_time(millisecond),
		CBArgs = { StartTime,[ gen_perpetuum,trans_noreply,[] ]},
		{ ok,Instance } = Module:start( stub_applogic,trans_delayed,CBArgs ),
		testrun( Module,Instance,Test,?LambdaLift3(invoke_delayed_signal),[] );

	backgrounded ->
		TestButLast = lists:droplast( lists:droplast( Test )),
		[Marking0,Events0|TestMiddle] = TestButLast,
		MarkingLast = lists:last( lists:droplast( Test )),
		EventsLast = lists:last( Test ),
		CBArgs = { gen_perpetuum,trans_noreply,[] },
		{ ok,Instance } = Module:start( stub_applogic,trans_backgrounded,CBArgs ),
		{ _MarkingMiddle,EventsMiddle } = split_test( TestMiddle ),
		BgTest = [ Marking0,Events0,MarkingLast,EventsLast ],
		testrun( Module,Instance,BgTest,?LambdaLift3(invoke_and_linger),[EventsMiddle,[]] );

	reflow_sentinel ->
		% Blunt once more; we simply ignore the .test file in this test
		Places = Module:places (),
		NumPlaces = length( Places ),
		InitialPlaceBits = Module:initial_placebits (),
		InitialSentinel = Module:sentinel( InitialPlaceBits ),
		%
		% Hope to produce the same Sentinel sequence as the compiler,
		% but use a different computation and produce more entries.
		%
		TenIntSizes = lists:seq( 3*64,12*64,64 ),
		TenPlaceBits = [ ((IntSz-1) div NumPlaces)-1 || IntSz <- TenIntSizes ],
		TestedPlaceBits = lists:filter( fun( Canidate ) -> Canidate>InitialPlaceBits end, TenPlaceBits ),
		io:format( "Reflowing Sentinels from ~p PlaceBits to ~p~n",[InitialPlaceBits,TestedPlaceBits] ),
		%
		% Produce the TransMap by asking the module.  The result starts absolute,
		% but proceeds by deriving from the smallest TransMap.
		%
		ReflowByModule = fun( PlaceBits ) ->
			Module:sentinel( PlaceBits )
		end,
		ModuleSequence = [ InitialSentinel | lists:map( ReflowByModule,TestedPlaceBits ) ],
		io:format( "Module produces ~p~n",[ModuleSequence] ),
		%
		% Produce the Sentinel by gradual enlargement of the previous one.
		% This is always done with gen_perpetuum:reflow_transmap/4, except
		% determining the initial Sentinel, which must come from the module.
		% Note: reflow_sentinel( Sentinel,NumPlaces,OldPlacebits,NewPlacebits ).
		%
		ReflowGenInc = fun( NewPlaceBits,OldSentinels ) ->
			OldPlaceBits = lists:nth( length( OldSentinels ),[ InitialPlaceBits | TestedPlaceBits ] ),
			io:format( "Incremental reflow_sentinel from ~p to ~p~n",[OldPlaceBits,NewPlaceBits] ),
			OldSentinel = lists:last( OldSentinels ),
			NewSentinel = gen_perpetuum:reflow_sentinel(
				OldSentinel,NumPlaces,OldPlaceBits,NewPlaceBits ),
			OldSentinels ++ [NewSentinel]
		end,
		io:format( "Folding with Function ~p, Accu ~p, list ~p~n",[ReflowGenInc,[InitialSentinel],TestedPlaceBits] ),
		GenIncSequence = lists:foldl(
					ReflowGenInc,
					[ InitialSentinel ],
					TestedPlaceBits ),
		io:format( "Generic produces result ~p~n",[GenIncSequence] ),
		%
		% Now compare ModuleSequence to GenIncSequence
		%
		if length( ModuleSequence ) /= length( GenIncSequence ) ->
			ErrorLengths = [ 'different-sentinel-sequences' ],
			io:format( "ModuleSequence and GenIncSequence are of different lengths: ~p and ~p~n",[ length( ModuleSequence ),length( GenIncSequence ) ] ),
			ZipTwo = { [],[] };
		true ->
			ErrorLengths = [],
			ZipTwo = lists:zip( ModuleSequence,GenIncSequence )
		end,
		ErrorVectors = [ {'vectors-differ',S1,S2} || { S1,S2 } <- ZipTwo, S1 /= S2 ],
		%
		% Harvest results and report on them
		%
		case ErrorLengths ++ ErrorVectors of
		[] ->
			ok;
		AllErrors ->
			{error,{'reflow_sentinel',AllErrors}}
		end;

	reflow_transmap ->
		% Blunt once more; we simply ignore the .test file in this test
		Places = Module:places (),
		NumPlaces = length( Places ),
		InitialPlaceBits = Module:initial_placebits (),
		InitialTransMap = Module:transmap( InitialPlaceBits ),
		%
		% Hope to produce the same PlaceBits sequence as the compiler,
		% but use a different computation and produce more entries.
		%
		TenIntSizes = lists:seq( 3*64,12*64,64 ),
		TenPlaceBits = [ ((IntSz-1) div NumPlaces)-1 || IntSz <- TenIntSizes ],
		TestedPlaceBits = lists:filter( fun( Canidate ) -> Canidate>InitialPlaceBits end, TenPlaceBits ),
		io:format( "Reflowing Transition Maps from ~p PlaceBits to ~p~n",[InitialPlaceBits,TestedPlaceBits] ),
		%
		% Produce the TransMap by asking the module.  The result starts absolute,
		% but proceeds by deriving from the smallest TransMap.
		%
		ReflowByModule = fun( PlaceBits ) ->
			Module:transmap( PlaceBits )
		end,
		ModuleSequence = [ InitialTransMap | lists:map( ReflowByModule,TestedPlaceBits ) ],
		io:format( "Module produces ~p~n",[ModuleSequence] ),
		%
		% Produce the TransMap by gradual enlargement of the previous one.
		% This is always done with gen_perpetuum:reflow_transmap/4, except
		% determining the initial TransMap, which must come from the module.
		% Note: reflow_transmap( TransMap,NumPlaces,OldPlacebits,NewPlacebits ).
		%
		ReflowGenInc = fun( NewPlaceBits,OldTransMaps ) ->
			OldPlaceBits = lists:nth( length( OldTransMaps ),[ InitialPlaceBits | TestedPlaceBits ] ),
			io:format( "Incremental reflow_transmap from ~p to ~p~n",[OldPlaceBits,NewPlaceBits] ),
			OldTransMap = lists:last( OldTransMaps ),
			NewTransMap = gen_perpetuum:reflow_transmap(
				OldTransMap,NumPlaces,OldPlaceBits,NewPlaceBits ),
			OldTransMaps ++ [NewTransMap]
		end,
		io:format( "Folding with Function ~p, Accu ~p, list ~p~n",[ReflowGenInc,[InitialTransMap],TestedPlaceBits] ),
		GenIncSequence = lists:foldl(
					ReflowGenInc,
					[ InitialTransMap ],
					TestedPlaceBits ),
		io:format( "Generic produces result ~p~n",[GenIncSequence] ),
		%
		% Now compare ModuleSequence to GenIncSequence
		%
		if length( ModuleSequence ) /= length( GenIncSequence ) ->
			ErrorLengths = [ 'different-transmap-sequences' ],
			io:format( "ModuleSequence and GenIncSequence are of different lengths: ~p and ~p~n",[ length( ModuleSequence ),length( GenIncSequence ) ] ),
			ZipTwo = { [],[] };
		true ->
			ErrorLengths = [],
			ZipTwo = lists:zip( ModuleSequence,GenIncSequence )
		end,
		ErrorKeys = [ {'keysets-differ',K1,K2} ||
				{ M1,M2 } <- ZipTwo,
				K1 <- [ maps:keys( M1 ) ],
				K2 <- [ maps:keys( M2 ) ],
				(length( K1 -- K2 ) /= 0) or (length( K2 -- K1 ) /= 0) ],
		ErrorMapping = [ {'mappings-differ',K,maps:get( K,M1 ),maps:get( K,M2 )} ||
				{ M1,M2} <- ZipTwo,
				K <- maps:keys( M1 ),
				maps:get( K,M1 ) /= maps:get( K,M2 ) ],
		%
		% Harvest results and report on them
		%
		case ErrorLengths ++ ErrorKeys ++ ErrorMapping of
		[] ->
			ok;
		AllErrors ->
			{error,{'reflow_transmap',AllErrors}}
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
	code:add_patha( "../erlang/perpetuum/ebin" ),
	%DEBUG% io:format( "Loading gen_perpetuum from ~p~n",[code:get_path()] ),
	io:format( "Loading gen_perpetuum~n" ),
	{ module,GEN } = code:load_file( gen_perpetuum ),
	io:format( "Loading ~p~n",[ModName] ),
	{ module,MUT } = code:load_file( list_to_atom( ModName )),
	io:format( "Loaded \"gen_perpetuum\" into module ~p~n\n", [GEN] ),
	io:format( "Loaded ~p into module ~p~n\n", [ModName,MUT] ).

load_test( ModName ) ->
	TestFile = unicode:characters_to_list( [ "./",ModName,".test" ]),
	Path = code:get_path(),
	io:format( "Loading from ~p~n", [TestFile] ),
	{ ok,TestExpr,_FileName } = file:path_script( Path,TestFile ),
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
	io:format( " - syncrun_delayed~n"),
	io:format( " - asyncrun_delayed~n"),
	io:format( " - backgrounded~n"),
	io:format( " - reflow_sentinel~n"),
	io:format( " - reflow_transmap~n"),
	halt( 1 ).

