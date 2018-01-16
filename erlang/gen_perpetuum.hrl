% This module, gen_perpetuum, is the generic portion of code for
% handling the Perpetuum model of Petri Nets, where each of the
% instances represents a single "colour".  Note that there is no
% support (yet?) for colours in individual tokens, we simply use
% different Petri Nets for each Colour.
%
% From: Rick van Rein <rick@openfortress.nl>


% PetriNet describes the structural aspects of a Petri Net.
% These structures may be reflowed if needed, to reflect a
% scaling-up of a definition for a given Colour that has it
% included.
%
-record( petrinet, {
	numplaces :: integer(),
	placebits :: integer(),
	intlen    :: integer()
} ).

% Colour describes the type of a Petri Net of one Colour.
% It includes the structure, so that it may modify that as
% needed through procedures such as reflowing.
%
% InitialColour is a special value for Colour, representing
% the initial values for a newly created colour instance:
% initial petrinet structure, initial marking, initial
% sentinel, initial dictionary of transitions.
%
%TODO% It is most efficient when a cache of TransDict
% structures for various IntLen exists, so computations
% from previous models can be shared.  This is possible
% because TransDict is an immutable structure.
%
-record( colour, {
	petrinet  :: #petrinet{},
	marking   :: integer(),
	sentinel  :: integer(),	%TODO% Never used...?
	transdict :: dict:dict(
		atom(),			% TransName
		{			% TransInfo
			integer(),	% MarkingAddend
			integer(),	% MarkingSubber
			integer()	% TransSentinel
		} )
} ).


% transreply() is the type for transition replies.
% Successes are reported as {noreply,...} and {reply,...}
% and the rest is some form of failure.  Depending on the
% function at hand, these failures may be processed further,
% or considered complete rejections.
%
% The forms are:
%  - {noreply,PerpetuumState,ApplicationState}
%  - {reply,ReplyCode,PerpetuumState,ApplicationState}
%  - {error,Reason}
%  -  retry
%  - {retry,ApplicationEventInfo}
%  - {delay,FiniteNonNegativeMilliSecondDelay}
%
-type transreply() :: {noreply,colour,term()} |
                      {reply,term(),colour,term()} |
                      {error,term()} |
                      retry |
                      {retry,term()} |
                      {delay,integer()}.


% Perform a transition and return any findings.  This routine
% will not schedule timer-related issues, but rather reply
% accordingly.
%
% The basic transition applies a Subber to the current
% Marking, and checks if  the TransSentinel approves.  This check
% will take both underflow and inhibitor arcs into account in one
% comparison!  See ERLANG.MD for details.
%
% When not agreeable, the transition receives {error,badstate} as
% a failure indication.  When agreeable, callbacks are tried and
% their result is decisive.  Callbacks may use EventData and
% InternalState to construct new InternalState.  It is however not
% advisable to take this data into account when deciding about the
% acceptability of the transition, as that would extend the
% synchronisation semantics beyond those of the Petri Net, in a way
% not perceived by static analysis.
%
% A successful transition ends by adding Addend to the
% current Marking, whereas a failed transition does not return a
% new state for the caller to store.
%
-spec handle_trans(
	Prior::colour,
	TransName::atom(),
	_EventData::term(),
	InternalState::term() ).


