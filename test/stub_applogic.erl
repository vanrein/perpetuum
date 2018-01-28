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
	trans_sum/4
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
trans_sum( CBArgs,TransName,EventData,AppState ) ->
	case TransName of
	'$init' ->
		{ noreply,CBArgs };
	'$finish' ->
		{ reply,AppState,[] };
	'$enquire' ->
		{ reply,AppState,AppState+EventData };
	_ ->
		{ noreply,AppState+EventData }
	end.


%TODO% Return {delay,...} in a testable manner


%TODO% Start background events in a testable manner


