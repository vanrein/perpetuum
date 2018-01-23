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
	marking/1,
	canfire/1, canfire/2,
	event/3,  event/4,
	signal/3, signal/4,
	system_continue/3, system_terminate/4,
	trans_switch/3,
	trans_noreply/3,
	trans_error_arg/3, trans_error_trans/3
]).


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
-spec handle_trans( TransName::atom(),EventData::term(),colour,AppState::term() ) -> internreply().
handle_trans( TransName,EventData,AppState,
			#colour{
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
			{ retry,marking };
		true ->
			CallbackMod:CallbackFun( CallbackArgs,TransName,EventData,AppState )
		end,
		case RetVal of
		{ noreply,NewInternalState } ->
			NewColour = reflow( Colour#colour{
				marking=( PreMarking+Addend )
				} ),
			{ noreply,NewColour,NewInternalState };
		{ reply,Reply,NewInternalState } ->
			NewColour = reflow( Colour#colour{
				marking=( PreMarking+Addend )
				} ),
			{ reply,Reply,NewColour,NewInternalState };
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


% Switch the transitions according to a transition map, where the
% keys are transition names and the values are {M,F,A} values to be
% re-triggered.
%
% To activate, use the following arguments to start_link/3:
%  1. This module's name, gen_perpetuum
%  2. This function's name, trans_switch
%  3. A switch map from transition name to {M,F,A}
%
% When it is a literal map, it will occupy no memory per instance.
%
-spec trans_switch( #{ TransName::atom() => { Module::atom(),Function::atom(),Args::term() } }, TransName::atom(), EventData::term() ) -> transreply().
trans_switch( SwitchMap,TransName,EventData ) ->
	{Mswi,Fswi,Aswi} = maps:get( TransName,SwitchMap ),
	Mswi:Fswi( Aswi,TransName,EventData ).

% Accept any transition immediately, returning a simple noreply.
%
-spec trans_noreply( CBArg::term(),TransName::atom(),EventData::term() ) -> noreply.
trans_noreply( _CBArg,_TransName,_EventData ) -> noreply.

% Reject the transition with an error.
%
-spec trans_error_arg( CBArg::term(),TransName::atom(),EventData::term() ) -> {error,term()}.
trans_error_arg( CBArg,_TransName,_EventData ) -> { error,CBArg }.
%
-spec trans_error_trans( CBArg::term(),TransName::atom(),EventData::term() ) -> {error,term()}.
trans_error_trans( _CBArg,TransName,_EventData ) -> { error,TransName }.


% Ask a Petri Net for its current marking.
%
% Since the Petri Net is a process on its own, its state may vary
% if you are not the only one with access to its Pid.
%
% Note that locking a Petri Net would render any proofs of liveness
% or deadlock freedom useless, so locking is not supported.  You are
% of course free to add extra places to your Petri Net to do just
% these things, in the places where you need them, and rerun any
% validations on those modified processes.
%
-spec marking( PetriNet::pid() ) -> [ { Place::atom(),TokenCount::integer() } ].
marking( PetriNet ) ->
	Ref = monitor( process,PetriNet ),
	catch PetriNet ! { marking,Ref,self() },
	receive
	{ reply,Ref,Reply } ->
		demonitor( Ref,[flush] ),
		Reply;
	{ 'DOWN',Ref,process,_Name,Reason } ->
		{ error,Reason }
	end.


% Ask a Petri Net for transitions that may currently fire, or ask it
% if a named transition may currently fire.
%
% Since the Petri Net is a process on its own, its state may vary
% if you are not the only one with access to its Pid.
%
% Note that locking a Petri Net would render any proofs of liveness
% or deadlock freedom useless, so locking is not supported.  You are
% of course free to add extra places to your Petri Net to do just
% these things, in the places where you need them, and rerun any
% validations on those modified processes.
%
-spec canfire( PetriNet::pid() ) -> [ TransName::atom() ] | {error,term()}.
canfire( PetriNet ) ->
	Ref = monitor( process,PetriNet ),
	catch PetriNet ! { canfire,Ref,self() },
	receive
	{ reply,Ref,Reply } ->
		demonitor( Ref,[flush] ),
		Reply;
	{ 'DOWN',Ref,process,_Name,Reason } ->
		{ error,Reason }
	end.
%
-spec canfire( PetriNet::pid(),TransName::atom() ) -> boolean() | {error,term()}.
canfire( PetriNet,TransName ) ->
	Ref = monitor( process,PetriNet ),
	catch PetriNet ! { canfire,TransName,Ref,self() },
	receive
	{ reply,Ref,Reply } ->
		demonitor( Ref,[flush] ),
		Reply;
	{ 'DOWN',Ref,process,_Name,Reason } ->
		{ error,Reason }
	end.

% Use signal to asynchronously send a transition request to a Petri Net.
% This is pretty much the same as cast() elsewhere.
%
% When a Timeout in milli-seconds is specified, the process may try
% the transition for that long before it gives up.  Any such situation
% will be backgrounded, and reappear for another try at a moment
% indicated by the underlying application logic.
% 
% When no Timeout is specified, any attempt to background the request
% will be ignored and the signal will be lost.
%
-spec signal( PetriNet::pid(),TransName::atom(),EventData::term() ) -> ok.
signal( PetriNet,TransName,EventData ) ->
	signal( PetriNet,TransName,EventData,0 ).
%
-spec signal( PetriNet::pid(),TransName::atom(),EventData::term(),Timeout::integer() ) -> ok.
signal( PetriNet,TransName,EventData,Timeout ) ->
	catch PetriNet ! { signal,TransName,EventData,Timeout },
	ok.


% Use event to synchronously send a transition request to a Petri Net and
% await its response.  This is pretty much the same as send() elsewhere.
%
% When a Timeout in milli-seconds is specified, it indicates the total
% time that application logic may defer handling.  Longer times would
% not even be tried, but the remaining waiting time is returned.
%
% When no Timeout is specified, it behaves just like 0 ms; meaning,
% any timeouts will be skipped.
%
% In both cases, a remaining timeout is reported as an error, namely
% {error,{timeout,TimeLeft}}.  Note that this is not a real-time logic;
% the time measured is only the total deferral by application logic,
% not the time that messages are pending in queues.  Maybe we will
% change that in the future, these functions will allow us to.
%
% This code was taken with modifications from this excellent book:
%        Designing for Scalability with Erlang/OTP,
%        Francesco Cesarini & Steve Vinoski,
%        ISBN/EAN 9781449320737.
% 
-spec event( PetriNet::pid(),TransName::atom(),EventData::term() ) -> eventreply().
event( PetriNet,TransName,EventData ) ->
	event( PetriNet,TransName,EventData,0 ).
%
-spec event( PetriNet::pid(),TransName::atom(),EventData::term(),Timeout::integer() ) -> eventreply().
event( PetriNet,TransName,EventData,Timeout ) ->
	Ref = monitor( process,PetriNet ),
	catch PetriNet ! { event,TransName,EventData,Timeout,Ref,self() },
	receive
	{ reply,Reply } ->
		demonitor( Ref, [flush] ),
		Reply;
	{ 'DOWN', Ref, process, _Name, Reason } ->
		{ error,Reason }
	end.


% Synchronous loop actions desire a response to be sent to the
% requesting Pid.  Then, the response is returned, though it may
% be processed while in transit; this might even revert to a
% prior state when the Pid did not receive the message.  This is
% probably going to be lardered with panic recovery mode code...
%
% The special value noreply is suppressed, as it is used for
% internal loop recycling, and would have come from a handled
% delay that will be retried in some time.  This is distinct
% from the {noreply,PerpetuumState} return, which is considered
% an indication to send ok.
%
% Unlike event() and signal(), this function is for internal use.
%
-spec send_response( reference(),pid(),internreply() | noreply ) -> internreply() | noreply.
send_response( _Ref,_Pid,noreply ) ->
	noreply;
send_response( Ref,Pid,Response ) ->
	case Response of
	{ reply,Reply,_NC,_PS} -> Pid ! { reply,Ref,{ reply,Reply  } };
	{ error,Reason       } -> Pid ! { reply,Ref,{ error,Reason } };
	{ retry,Reason       } -> Pid ! { reply,Ref,{ retry,Reason } };
	{ delay,DelayT       } -> Pid ! { reply,Ref,{ delay,DelayT } };
	{ noreply,    _NC,_PS} -> Pid ! { reply,Ref,  noreply        }
	end,
	Response.


% The init routine is usually invoked from an instance of this behaviour.
% It uses proc_lib to register "properly" and then starts the process loop.
%
%TODO% Do we need to pass in state?
%
init( Parent,InstanceMod,{CallbackMod,CallbackFun,CallbackArgs}=Callback ) ->
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
	%TODO% Future extensions might accept delay, replay and more...
	{noreply,AppState} = CallbackMod:CallbackFun( CallbackArgs,'$init',NewPetriNet,{} ),
	proc_lib:init_ack( Parent, {ok,self()} ),
	loop( Parent,NewColour,AppState ).


% A main loop that may be used if Perpetuum is meant as a service
% to many processes, rather than as a process-internal structuring
% agent.
%
loop( Parent,Colour,AppState )  ->
	case receive
	%
	% Receive a request and process it; setup a Response
	%
	{ event,TransName,EventData,Timeout,Ref,Pid } ->
		send_response(Ref,Pid,
			handle_delay(Timeout,TransName,EventData,Pid,
				handle_trans( TransName,EventData,Colour,AppState )));
	{ signal,TransName,EventData,Timeout } ->
		handle_async_infodrop(
			handle_delay( Timeout,TransName,EventData,-1,
				handle_trans( TransName,EventData,Colour,AppState )));
	%
	% Utility functions to query the Petri Net
	%
	{ marking,Ref,Pid } ->
		send_response( Ref,Pid,handle_marking( Colour,AppState           )),
		noreply;
	{ canfire,Ref,Pid } ->
		send_response( Ref,Pid,handle_canfire( Colour,AppState           )),
		noreply;
	{ canfire,TransName,Ref,Pid } ->
		send_response( Ref,Pid,handle_canfire( Colour,AppState,TransName )),
		noreply;
	%
	% Go on to receive system / management messages
	%
	{ stop,Reason } ->
		handle_stop( Reason,Colour,AppState );
	{ system,From,Msg } ->
		DebugData = [],
		sys:handle_system_message( Msg,From,Parent,?MODULE,DebugData,{Colour,AppState} );
	{ 'EXIT', _Pid, Reason } ->
		exit (Reason)
	%
	% Now harvest whatever result we ran into, and loop back.
	% We accept the extra word noreply from a number of sources
	% as a way to cycle back without change to data.
	%
	end of
		{ noreply,NewColour,NewAppState } ->
			loop( Parent,NewColour,NewAppState );
		{ reply,_,NewColour,NewAppState } ->
			loop( Parent,NewColour,NewAppState );
		_ ->
			% includes the noreply response
			loop( Parent,Colour,AppState )
	end.


% Deliver the current marking for a given Petri Net colouring.
% This yields a list of { PlaceName,TokenCount } pairs.
%
% This function uses information from the places() callback.
%
-spec handle_marking( Prior::colour,AppState::term() ) -> { reply,[ { PlaceName::atom(),TokenCount::integer() } ],colour,term() }.
handle_marking( #colour{ petrinet=#petrinet { instance=InstanceMod, placebits=PlaceBits }, marking=Marking }=Colour,AppState ) ->
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
	{ reply,ListPlaces( ListPlaces,Marking,InstanceMod:places() ),Colour,AppState }.


% Determine transitions that can fire or, when one is given, whether
% the requested one can fire.  In all cases, return a list of transitions
% that is potentially empty.
%
-spec handle_canfire( colour,AppState::term(),TransName::atom() ) -> { reply,boolean() }.
handle_canfire( #colour{ petrinet=#petrinet { transmap=TransMap }, marking=Marking }=Colour,AppState,TransName ) ->
	{ _Addend,Subber,TransSentinel } = maps:get ( TransName,TransMap ),
	{ reply,check_canfire( Marking,Subber,TransSentinel ),Colour,AppState }.
%
-spec handle_canfire( colour,AppState::term()                   ) -> { reply,[ TransName::atom() ] }.
handle_canfire( #colour{ petrinet=#petrinet { transmap=TransMap }, marking=Marking }=Colour,AppState ) ->
	CheckCanFire = fun( _TransName,{_Addend,Subber,TransSentinel} ) ->
		check_canfire( Marking,Subber,TransSentinel )
	end,
	{ reply,maps:keys( maps:filter( CheckCanFire,TransMap )),Colour,AppState }.
%
check_canfire( Marking,Subber,TransSentinel ) ->
	(Marking - Subber) band TransSentinel == 0.


% internreply() is the internal form of transition replies.
% Compared to transreply() it adds Colour to successes as
% they are reported as {noreply,...} and {reply,...} but
% the rest remains the same form of failure.  In addition,
% there is the internal form consisting of just the word
% noreply, representing an invalid and to-be-ignored reply.
%
% The forms are:
%  - { noreply,            Colour, AppState      }
%  - { reply,   ReplyCode, Colour, AppState      }
%  - { error,   Reason                           }
%  - { retry,   Reason                           }
%  - { delay,   PositiveMilliSecondDelay         }
%  -   noreply
%
-type internreply() :: { noreply,         colour, term() } |
                       { reply,   term(), colour, term() } |
                       { error,   term()                 } |
                       { retry,   term()                 } |
                       { delay,   integer()              } |
                         noreply.

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
			error( 'illegal-defer' );
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
				{ error,{timeout,NewTimeout} }
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
	{ retry,_Reason } ->
		% We can safely forget about retrying
		%
		noreply;
	{ reply,_Info,_Colour,_AppState } ->
		% This is unacceptable for async comms,
		% as we would be discarding state
		%
		error( 'reply-info-discarded' );
	_ ->
		PreResponse
	end.


% Make an attempt to stop the system, in response to a stop request.
% The response is ignored, but it may be influenced negatively by
% the underlying transition handler '$stop' of the application's
% callback module.
%
-spec handle_stop( Reason::term(),colour,AppState::term() ) -> internreply().
handle_stop( Reason,
			#colour{
				petrinet=#petrinet{
					callback={CallbackMod,CallbackFun,CallbackArgs}
				}
			}=Colour, AppState) ->
	{ reply,UserMarking,Colour,AppState } = handle_marking( Colour,AppState ),
	case CallbackMod:CallbackFun( CallbackArgs,'$stop',{Reason,UserMarking},AppState ) of
	%
	% Downright refusal to cooperate with a stop request.
	% (This could be used to protect the Marking from poor death.)
	%
	{ error,_ErrorReason  } -> noreply;
	{ retry,_RetryReason  } -> noreply;
	%
	% Delayed death by setting an alarm for another attempt.
	% (This can be continued for a long time, and might pile up.)
	% 
	{ delay,Timeout       } -> timer:send_after( Timeout,{ stop,Reason } ), noreply;
	%
	% Acceptance of the requested termination.
	% (But do spoil a normal exit if a reply state was lost.)
	%
	{ noreply,     _AppSt } -> exit( Reason );
	{ reply,_Reply,_AppSt } -> exit( 'reply-info-discarded-at-exit' )
end.

% System messages for continuation or termination.
% These can be expanded into much more potent things.
%
system_continue( Parent,_Debug,{Colour,AppState} ) ->
	loop( Parent,Colour,AppState ).
%
system_terminate( Reason,_Parent,_Debug,{ Colour,AppState } ) ->
	handle_stop( Reason,Colour,AppState ),
	% and, it case it would fall through:
	exit( 'perpetuum-refusal-in-system_terminate' ).

