% This module, gen_perpetuum, is the generic portion of code for
% handling the Perpetuum model of Petri Nets, where each of the
% instances represents a single "colour".  Note that there is no
% support (yet?) for colours in individual tokens, we simply use
% different Petri Nets for each Colour.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( gen_perpetuum ).

-include( "gen_perpetuum.hrl" ).

-export([
	init/3,
	system_continue/3,
	system_terminate/4
]).

-spec handle_trans(
		TransName::atom(),
		EventData::term(),
		Prior::colour
	) -> transreply() .

-spec marking( Prior::colour ) -> [ { PlaceName::atom(),TokenCount::integer() } ].


% Callback function "transit" lists the transitions as
% atoms.  The order may at some day be used to replace
% the map with a list, if this is more efficient.
%
% This is generated as a literal, so its storage is
% shared among any number of instances.
%
-callback transit() -> [ atom() ].

% Callback function "places" lists the places as atoms.
% The order matches the numbering scheme (from 0 onward)
% used in the bitfield-vector encoding (where 0 is the
% least valued bitfield, so with multiplier 1).
%
% This is generated as a literal, so its storage is
% shared among any number of instances.
%
% The number of places can be derived as the length of
% this list.  This is simple, and avoids inconsistencies.
%
-callback places() -> [ atom() ].

% Callback function "initial_placebits" should return the
% initial number of bits reserved for a place.
%
-callback initial_placebits() -> integer().

% Callback function "initial_marking" should return the
% initial marking for the smallest possible integer size.
%
-callback initial_marking( integer() ) -> integer().

% Callback function "sentinel" should return the sentinel bits
% for a given number of places, which is useful because it lets
% us share literals in most use cases, thus off-loading the
% heaps of instances.
%
-callback sentinel( integer() ) -> integer().

% Callback function "transmap" constructs a transition
% infomap from transition names to the changes it commands.
% The function is indexed by the number of place bits,
% because its values grow along with it.  The result of the
% call is ideally a literal so it can be shared by many
% processes.  Only the very largest ones need to expand
% dynamically, to hold very large sizes.
%
-callback transmap( integer() ) -> #{}.


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
handle_trans( TransName, EventData, #colour{
					petrinet=#petrinet{
						callback={CallbackMod,CallbackFun,CallbackArgs},
						transmap=TransMap
					},
					marking=Marking
				}=Colour) ->
		{ Addend,Subber,TransSentinel } = maps:get ( TransName,TransMap ),
		PreMarking = Marking - Subber,
		RetVal = if
		( PreMarking band TransSentinel ) /= 0 ->
			{retry,marking};
		true ->
			CallbackMod:CallbackFun (CallbackArgs, TransName, EventData)
		end,
		case RetVal of
		{ noreply,_,NewInternalState } ->
			NewColour = reflow( Colour#colour{
				marking=( PreMarking+Addend )
				} ),
			{noreply,NewColour,NewInternalState};
		{ reply,Reply,_,NewInternalState } ->
			NewColour = reflow( Colour#colour{
				marking=( PreMarking+Addend )
				} ),
			{reply,Reply,NewColour,NewInternalState};
		_ ->
			RetVal
		end.


% The reflow procedure inserts extra place bits to make an
% outcome of a transition fit.  It only has impact when
% there is actually a need for more bits.
%
% The assumption is that intlen in the contained petrinet
% is a size occurring in the Erlang virtual machine, so
% 60 or N*64 with N>=3.  It will step up the number of
% bits so that every place receives at least one more bit,
% and possibly more.  It will 
%
% To reflow a Sentinel, we will double the bitfields for
% all the places, even the first, and we will keep the
% low bit because it might be set to cover the entire
% place for the implementation of inhibitor arcs.
%
% To reflow a Marking or Addend or Subber, we will extend
% all bitfields by inserting a zero bit on top as the new
% Carry or Sentinel bit; the previous Sentinel bit may be
% set to hold a Carry from a preceding addition, which
% would be an overflow that caused the need for the reflow.
%
reflow( #colour{ petrinet=PetriNet, marking=Marking, sentinel=Sentinel }=Colour ) ->
	io:fwrite( "Marking: ~p~n",[Marking] ),
	io:fwrite( "Sentinel: ~p~n",[Sentinel] ),
	if
	( Marking band Sentinel ) == 0 ->
		Colour;
	true ->
		case PetriNet of
		#petrinet{ instance=InstanceMod, callback=Callback, numplaces=NumPlaces, placebits=PlaceBits, transmap=TransMap } ->
			io:fwrite( "NumPlaces: ~p~n",[NumPlaces] ),
			io:fwrite( "PlaceBits: ~p~n",[PlaceBits] ),
			%
			% Determine the new integer length and bits per place
			% assuming that one bit extra per place is enough to
			% handle overflow
			%
			NeedIntLen = 1 + (PlaceBits+2) * NumPlaces,
			io:fwrite( "NeedIntLen: ~p~n",[NeedIntLen] ),
			NewIntLen = if
				NeedIntLen < 60 -> 60;
				NeedIntLen < 3*64 -> 3*64;
				true -> (NeedIntLen + 63) band -64
			end,
			io:fwrite( "NewIntLen: ~p~n",[NewIntLen] ),
			NewPlaceBits = (NewIntLen - 1) div NumPlaces - 1,
			io:fwrite( "NewPlaceBits: ~p~n",[NewPlaceBits] ),
			ExtraPlaceBits = NewPlaceBits - PlaceBits,
			io:fwrite( "ExtraPlaceBits: ~p~n",[ExtraPlaceBits] ),
			%UNDEFINED% assert:assert( NewIntLen    >= NeedIntLen ),
			%UNDEFINED% assert:assert( NewPlaceBits >  PlaceBits  ),
			%
			% Update Sentinels and Addend in the petrinet and colour
			% by inserting the extra NewPlaceBits-PlaceBits everywhere
			%
			%TODO% Would be good to retrieve reflown structures from a cache
			%
			ExtendBitfields_rec = fun( YF,Vec,BitPos,PlacesTogo ) ->
				if PlacesTogo == 0 ->
					Vec;
				true ->
					%
					% Split Vec into top and bottom parts, and add
					% ExtraPlaceBits bits valued 0 on top
					% of place bits _and_ carry bit by shifting
					% the rest up
					%
					TopVec = Vec band ( -1 bsl (BitPos+PlaceBits+1) ),
					BotVec = Vec bxor TopVec,
					UpdVec = BotVec bor ( TopVec bsl ExtraPlaceBits ),
					io:fwrite( "Extending with Vec ~p/~p, TopVec ~p, BotVec ~p, UpdVec ~p~n", [Vec,BitPos,TopVec,BotVec,UpdVec] ),
					YF( YF,UpdVec,BitPos+NewPlaceBits+1,PlacesTogo-1 )
				end
			end,
			ExtendBitfields = fun( Vec ) ->
				%TODO% Move this where InstanceMod:transmap() can call it
				RETVAL =
				ExtendBitfields_rec( ExtendBitfields_rec,Vec,0,NumPlaces )
				, io:fwrite( "Extended Bitfields ~p to ~p~n", [Vec,RETVAL] ), RETVAL
			end,
			ReguardExtraBits = ( -1 bsl ExtraPlaceBits ) bxor -1,
			ReguardBitfields_rec = fun( YF,Vec,BitPos,PlacesTogo ) ->
				if PlacesTogo == 0 ->
					Vec;
				true ->
					TopVec = Vec band ( -1 bsl BitPos ),
					BotVec = Vec bxor TopVec,
					NewVec = if
						(Vec band (1 bsl BitPos)) /= 0 -> ReguardExtraBits bsl BitPos;
						true -> 0
					end,
					UpdVec = BotVec bor (TopVec bsl ExtraPlaceBits) bor NewVec,
					io:fwrite( "Reguarding with Vec ~p/~p, TopVec ~p, BotVec ~p, NewVec ~p, UpdVec ~p~n", [Vec,BitPos,TopVec,BotVec,NewVec,UpdVec] ),
					YF( YF,UpdVec,BitPos+NewPlaceBits+1,PlacesTogo-1 )
				end
			end,
			ReguardBitfields = fun( Vec ) ->
				%TODO% Move this where InstanceMod:transmap() can call it
				RETVAL =
				ReguardBitfields_rec( ReguardBitfields_rec,Vec,0,NumPlaces )
				, io:fwrite( "Reguarded Bitfields ~p to ~p~n", [Vec,RETVAL] ), RETVAL
			end,
			ExpandTransMapKV = fun( _TransName,{ Addend,Subber,TransSentinel } ) ->
				%TODO% Move this where InstanceMod:transmap() can call it
				{
					ExtendBitfields( Addend ),
					ExtendBitfields( Subber ),
					ReguardBitfields( TransSentinel )
				}
			end,
			#colour {
				petrinet  = #petrinet {
					instance  = InstanceMod,
					callback  = Callback,
					numplaces = NumPlaces,
					placebits = NewPlaceBits,
					transmap  = InstanceMod:transmap( NewPlaceBits )
					%TODO:OLD% transmap = maps:map( ExpandTransMapKV, TransMap )
				},
				marking   = ExtendBitfields( Marking ),
				sentinel  = ReguardBitfields( Sentinel )
			}
		end
	end.


% Deliver the current marking for a given Petri Net colouring.
% This yields a list of { PlaceName,TokenCount } pairs.
%
% This function uses information from the places() callback.
%
marking( #colour{ petrinet=#petrinet { instance=InstanceMod, placebits=PlaceBits }, marking=Marking } ) ->
	PlaceMask = (1 bsl PlaceBits) - 1,
	ListPlaces = fun( YF,ShiftMarking,Places ) ->
		case Places of
		[] ->
			[];
		[Plc|NextPlaces] ->
			NextMarking = ShiftMarking bsr (PlaceBits + 1),
			[ {Plc,ShiftMarking band PlaceMask} | YF( YF,NextMarking,NextPlaces ) ]
		end
	end,
	ListPlaces( ListPlaces,Marking,InstanceMod:places() ).


% Determine transitions that can fire or, when one is given, whether
% the requested one can fire.  In all cases, return a list of transitions
% that is potentially empty.
%
-spec canfire( colour,TransName::atom() ) -> [ TransName::atom() ].
canfire( #colour{ petrinet=#petrinet { transmap=TransMap }, marking=Marking }, TransName ) ->
	{ _Addend,Subber,TransSentinel } = maps:get ( TransName,TransMap ),
	if (Marking - Subber) band TransSentinel == 0 ->
		[];
	true ->
		[TransName]
	end.
%
-spec canfire( colour                   ) -> [ TransName::atom() ].
canfire( #colour{ petrinet=#petrinet { transmap=TransMap }, marking=Marking } ) ->
	SelFun = fun( _TransName,{_Addend,Subber,TransSentinel} ) ->
		canfire_helper( Marking,Subber,TransSentinel )
	end,
	maps:keys( maps:filter( SelFun,TransMap )).
%
canfire_helper( Marking,Subber,TransSentinel ) ->
	(Marking - Subber) band TransSentinel == 0.


% The init routine is usually invoked from an instance of this behaviour.
% It uses proc_lib to register "properly" and then starts the process loop.
% The parent process will be referred to as the AppLogicPid.
%
%TODO% Do we need to pass in state?
%
init( AppLogicPid,InstanceMod,{_CallbackMod,_CallbackFun,_CallbackArgs}=Callback ) ->
	NumPlaces = length( InstanceMod:places() ),
	PlaceBits = InstanceMod:initial_placebits(),
	NewPetriNet = #petrinet{
		instance  = InstanceMod,
		callback  = Callback,
		numplaces = NumPlaces,
		placebits = PlaceBits,
		transmap = InstanceMod:transmap(        PlaceBits )
	},
	NewColour = #colour{
		petrinet = NewPetriNet,
		marking  = InstanceMod:initial_marking( PlaceBits ),
		sentinel = InstanceMod:sentinel(        PlaceBits )
	},
	proc_lib:init_ack( AppLogicPid, {ok,self()} ),
	loop( AppLogicPid,NewColour ).

% A main loop that may be used if Perpetuum is meant as a service
% to many processes, rather than as a process-internal structuring
% agent.
%
loop( AppLogicPid,Colour )  ->
	%
	% Receive a request and process it; setup a Response
	%
	case receive
	{ event,TransName,EventData,Pid } ->
		send_response (Pid,
			handle_trans( TransName,EventData,Colour ));
	{ event,TransName,EventData,Pid,Timeout } ->
		send_response (Pid,
			handle_delay (Timeout,TransName,EventData,Pid,
				handle_trans( TransName,EventData,Colour )));
	{ signal,TransName,EventData } ->
		handle_async_infodrop (
			handle_delay (0,TransName,EventData,-1,
				handle_trans( TransName,EventData,Colour )));
	{ signal,TransName,EventData,Timeout } ->
		handle_async_infodrop (
			handle_delay ( Timeout,TransName,EventData,-1,
				handle_trans( TransName,EventData,Colour )));
	{ marking,Pid } ->
		send_response( Pid, marking( Colour )),
		noreply;
	{ canfire,Pid } ->
		send_response( Pid, canfire( Colour )),
		noreply;
	{ canfire,Pid,TransName } ->
		send_response( Pid, canfire( Colour,TransName )),
		noreply;
	%
	% Go on to receive system / management messages
	stop ->
		exit (normal);
	{ system,From,Msg } ->
		DebugData = [],
		sys:handle_system_message( Msg,From,AppLogicPid,?MODULE,DebugData,Colour );
	{ 'EXIT', _Pid, Reason } ->
		exit (Reason)
	%
	% Now harvest whatever result we ran into, and loop back.
	% We accept the extra word noreply from a number of sources
	% as a way to cycle back without change to data.
	%
	end of
		{ noreply,NewColour } ->
			loop( AppLogicPid,NewColour );
		{ reply,_,NewColour } ->
			loop( AppLogicPid,NewColour );
		_ ->
			% includes the noreply response
			loop( AppLogicPid,Colour )
	end.


% Synchronous loop actions desire a response to be sent to the
% requesting Pid.  Then, the response is returned, though it may
% be processed while in transit; this might even revert to a
% prior state when the Pid did not receive the message.  This is
% probably going to be lardered with panic recovery mode code...
%
% The special value noreply is suppressed, as it is used for
% internal loop recycling, and would have come from a handled
% delay that will be retried in some time.
%
send_response( _,noreply ) ->
	noreply;
send_response( Pid,Response ) ->
	Pid ! Response,
	% yeah, I know that ! returns Response, but I'll forget it when updating this function, I'm sure
	Response.


% Handle any timeouts in an intermediate response, by resending the
% message type as indicated.  If the timeout exceeds the tolerable
% limit, change the timeout into an error code to return (but do
% not raise an exception for it).
%
% Timer-resent messages are events when PidOpt fits is_pid/1, or
% otherwise as signal; you can set PidOpt = -1 to ensure the latter.
%
% The function returns noreply to requesting loop to cycle around
% with the same state.
%
handle_delay( MaxDelay,TransName,EventData,PidOpt,PreResponse ) ->
	case PreResponse of
	{ delay,DeferMS } ->
		if DeferMS =< 0 ->
			erlang:error( 'illegal-defer' );
		true ->
			NewTimeout = MaxDelay - DeferMS,
			if NewTimeout > 0 ->
				% Try repeated delivery after DeferMS have passed
				if is_pid (PidOpt) ->
					NewMsg = {  event,TransName,EventData,PidOpt,NewTimeout };
				true ->
					NewMsg = { signal,TransName,EventData,       NewTimeout }
				end,
				timer:send_after( DeferMS, NewMsg  ),
				noreply;
			true ->
				{ error,timeout }
			end
		end;
	_ ->
		PreResponse
	end.

% Raise an error when dropping information attachments in a response.
% This constrains transition callbacks to only send information that
% will get processed.  Descriptive information about the result are
% exempted, as an asynchronous caller clearly doesn't care; this is
% the case for {retry,Reason}.
%
% The function returns noreply to requesting loop to cycle around
% with the same state.
%
handle_async_infodrop( PreResponse ) ->
	case PreResponse of
	{retry,_Reason} ->
		% We can safely forget about retrying
		%
		noreply;
	{ reply,_Info,_PerpState,_AppState } ->
		% This is unacceptable for async comms,
		% as we would be discarding state
		%
		erlang:error( 'reply-info-discarded' );
	_ ->
		PreResponse
	end.

% System messages for continuation or termination.
% These can be expanded into much more potent things.
%
system_continue( AppLogicPid,_Debug,Colour ) ->
	loop( AppLogicPid,Colour ).
system_terminate( Reason,_AppLogicPid,_Debug,_Colour ) ->
	exit( Reason ).

