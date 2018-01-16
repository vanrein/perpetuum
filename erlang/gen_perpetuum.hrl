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

