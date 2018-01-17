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
	placebits :: integer()
} ).

% Colour describes the type of a Petri Net of one Colour.
% It includes the structure, so that it may modify that as
% needed through procedures such as reflowing.
%
% InitialColour is a special value for Colour, representing
% the initial values for a newly created colour instance:
% initial petrinet structure, initial marking, initial
% sentinel, initial mapping of transitions.
%
% Usually, the transmap is a literal so it can be shared.
% only for very-very-large Petri Nets, where storage space
% is not likely to be the limiting resource, will a
% dynamic expansion be made towards (pretty much) boundless
% sizes.  By then, counters have been incremented very
% often, and we are not primarily concerned with space.
%
-record( colour, {
	petrinet  :: #petrinet{},
	marking   :: integer(),
	sentinel  :: integer(),	%TODO% Never used...?
	transmap :: #{
		atom(),			% TransName
		{			% TransInfo
			integer(),	% MarkingAddend
			integer(),	% MarkingSubber
			integer()	% TransSentinel
		}
	}
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

