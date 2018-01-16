% This module, gen_perpetuum, is the generic portion of code for
% handling the Perpetuum model of Petri Nets, where each of the
% instances represents a single "colour".  Note that there is no
% support (yet?) for colours in individual tokens, we simply use
% different Petri Nets for each Colour.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( gen_perpetuum ).

-include( "gen_perpetuum.hrl" ).

-export([ reflow/1 ]).


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
reflow( #colour{ petrinet=PetriNet, marking=Marking, sentinel=Sentinel, transdict=TransDict }=Colour ) ->
	io:fwrite( "Marking: ~p~n",[Marking] ),
	io:fwrite( "Sentinel: ~p~n",[Sentinel] ),
	if
	( Marking band Sentinel ) == 0 ->
		Colour;
	true ->
		case PetriNet of
		#petrinet{ numplaces=NumPlaces, placebits=PlaceBits, intlen=IntLen } ->
			io:fwrite( "NumPlaces: ~p~n",[NumPlaces] ),
			io:fwrite( "PlaceBits: ~p~n",[PlaceBits] ),
			io:fwrite( "IntLen:    ~p~n",[IntLen   ] ),
			%
			% Determine the new integer length and bits per place
			% assuming that one bit extra per place is enough to
			% handle overflow
			%
			NeedIntLen = 1 + (PlaceBits+2) * NumPlaces,
			io:fwrite( "NeedIntLen: ~p~n",[NeedIntLen] ),
			StepupIntLen = fun( YF,CurIntLen ) ->
				if CurIntLen < 60 ->
					YF( YF,60 );
				CurIntLen >= NeedIntLen ->
					CurIntLen;
				CurIntLen < 64 ->
					YF( YF,3*64 );
				CurIntLen rem 64 == 0 ->
					YF( YF,CurIntLen+64 )
				end
			end,
			NewIntLen = StepupIntLen( StepupIntLen,IntLen ),
			io:fwrite( "NewIntLen: ~p~n",[NewIntLen] ),
			NewPlaceBits = (NewIntLen - 1) div NumPlaces - 1,
			io:fwrite( "NewPlaceBits: ~p~n",[NewPlaceBits] ),
			ExtraPlaceBits = NewPlaceBits - PlaceBits,
			io:fwrite( "ExtraPlaceBits: ~p~n",[ExtraPlaceBits] ),
			%UNDEFINED% assert:assert( NewIntLen    > IntLen    ),
			%UNDEFINED% assert:assert( NewPlaceBits > PlaceBits ),
			%
			% Update Sentinels and MarkingAddends in the petrinet and colour
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
				RETVAL =
				ReguardBitfields_rec( ReguardBitfields_rec,Vec,0,NumPlaces )
				, io:fwrite( "Reguarded Bitfields ~p to ~p~n", [Vec,RETVAL] ), RETVAL
			end,
			ExpandTransDictKV = fun( _TransName,{ MarkingAddend,MarkingSubber,TransSentinel } ) ->
				{
					ExtendBitfields( MarkingAddend ),
					ExtendBitfields( MarkingSubber ),
					ReguardBitfields( TransSentinel )
				}
			end,
			#colour {
				petrinet  = #petrinet {
					numplaces = NumPlaces,
					placebits = NewPlaceBits,
					intlen    = NewIntLen
				},
				marking   = ExtendBitfields( Marking ),
				sentinel  = ReguardBitfields( Sentinel ),
				transdict = dict:map( ExpandTransDictKV, TransDict )
			}
		end
	end.

