% Stub application logic, used for certain runs of test scripts.
%
% This module holds various functions used to test the transition handling
% callbacks (these tests are targets for the 0.9 release):
%
%  - proper '$init' and '$finish' behaviour
%  - proper '$enquire' behaviour
%  - proper reception and passing of AppData and EventData
%  - proper program logic for the various forms of transreply()
%  - proper implementation of timer logic, namely through {delay,...}
%  - proper handling of backgrounded signals (followup transitions)
%
% From: Rick van Rein <rick@openfortress.nl>


-module( stub_applogic ).


-export([
	trans_datatrace/4,
	trans_replyforms/4,
	trans_sum_init/4,trans_sum_stop/4,trans_sum_enquire/4,trans_sum_any/4,
	trans_sum_switch/1,
	trans_delayed/4
]).


% Stub transition logic, effectively keeping track of transition history.
% Transition history holds OldAppState and EventData and will be returned
% as {noreply,HistorySoFar} values.  The eventual state would be reported
% by '$finish', which is not supposed to be part of a test script.
% A special test run however, will check for cleanup via monitoring.
%
trans_datatrace( CBArgs,TransName,EventData,AppState ) ->
	case TransName of
	'$init' ->
		% Ignore AppState, but take notice of Callback Args
		NewAppState = [ { '$init', CBArgs } ];
	_ ->
		% The CBArgs are repeatedly stored so we may validate them
		NewAppState = AppState ++ [ { TransName, CBArgs, EventData } ]
	end,
	%DEBUG% io:format( "trans_datatrace/4 -> NewAppState = ~p~n",[NewAppState] ),
	if TransName == '$enquire' ->
		{ reply,NewAppState,[] };
	true ->
		{ noreply,NewAppState }
	end.


% Stub transition logic, reporting EventData as the result of a
% transition, with a few exceptions to get it going.  This is used
% for testing that reply values are reported as expected.
%
trans_replyforms( _CBArgs,_TransName,EventData,_AppState ) ->
	EventData.


% Stub transition logic, meant for testing '$init', '$finish' and '$equire',
% by summing up integer values passed in.  All of CBArgs, EventData and
% AppState are assumed to be integer() typed values.
%
trans_sum_init( CBArgs,_TransName,_EventData,_AppState ) ->
	%DEBUG% io:format( "trans_sum_init~n" ),
	{ noreply,CBArgs }.
%
trans_sum_stop( _CBArgs,_TransName,_EventData,AppState ) ->
	%DEBUG% io:format( "trans_sum_stop~n" ),
	{ reply,AppState,[] }.
%
trans_sum_enquire( _CBArgs,_TransName,EventData,AppState ) ->
	%DEBUG% io:format( "trans_sum_enquire( ~p,~p )~n",[EventData,AppState] ),
	{ reply,AppState,AppState+EventData }.
%
trans_sum_any( _CBArgs,_TransName,EventData,AppState ) ->
	%DEBUG% io:format( "trans_sum_any( ~p,~p )~n",[EventData,AppState] ),
	{ noreply,AppState+EventData }.
%
trans_sum_switch(CBArgs) -> #{
	'$init'    => { ?MODULE,trans_sum_init,   CBArgs },
	'$stop'    => { ?MODULE,trans_sum_stop,   CBArgs },
	'$enquire' => { ?MODULE,trans_sum_enquire,CBArgs },
	'$default' => { ?MODULE,trans_sum_any,    CBArgs } }.


% Start a transaction by deferring it.  The suggested timestamp for running
% is in the EventData.  Beyond this time, it will be run, but before it, it
% will be delayed for the desired number of milliseconds.
%
% When the event is finally run, it ivokes {M,F,A} from the CBArgs, where
% it is setup together with the absolute timestamp() at the start:
%
%   CBArgs :: { monotonic_time(),[ module(),function(),[ term() ] ]}
%
trans_delayed( CBArgs,TransName,EventData,AppState ) ->
	{ StartTime,[ M2,F2,A2 ]} = CBArgs,
	Now = erlang:monotonic_time(millisecond),
	DelaySoFar = Now - StartTime,
	io:format( "Event ~p delay so far is ~p ms~n",[TransName,DelaySoFar] ),
	if TransName == '$init' ->
		io:format ( "Invoking '$init' transition immediately~n" ),
		M2:F2( A2,TransName,EventData,AppState );
	EventData > Now ->
		io:format( "Delaying event ~p for ~p ms~n",[TransName,EventData-Now] ),
		{delay,EventData-Now};
	true ->
		io:format( "Starting delayed event ~p after ~p ms~n",[TransName,DelaySoFar] ),
		M2:F2( A2,TransName,EventData,AppState )
	end.

%TODO% Start background events in a testable manner
% Idea: pass a sequence in as eventdata, and observe how the marking evolves


