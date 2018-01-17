

% Determine the minimum, and then the next up number of
% bits in a (signed) integer.  This takes into account
% knowledge of the internals of the BEAM virtual machine.
% We crash on silly input (meaning, not from us).
%
bits() -> 60.
bits(60) -> 3*64;
bits(N) where N rem 64 == 0 -> N + 64.

% Given an integer, find the number of bits to store it.
% This is used to determine the minimum amount of storage
% for a Place, given its maximum number of transitions in
% and out; below this threshold, overflow or underflow
% might go undetected, so it serves to find the smallest
% possible integer vector size.
%
log2up( N ) ->
	log2up( N,0 ).
log2up( N,Bits ) ->
	if 1 bsl Bits <= N ->
		log2up( N,(Bits+1) );
	true ->
		Bits.

% Given a number of places and a maximum change due to
% incoming and outgoing transitions for any place, find
% the sizing information for the storage vector.  Given
% the smallest enclosing integer in the BEAM virtual
% machine, make the places as large as possible.
%
size_places( NumPlaces,MaxDelta ) ->
	size_places( NumPlaces,log2up( MaxDelta ),bits() ).
size_places( NumPlaces,MaxDeltaBits,BitAttempt ) ->
	if BitAttempt < 1 + NumPlaces * (1+MaxDeltaBits) ->
		size_places( NumPlaces,MaxDeltaBits,bits( BitAttempt ));
	true ->
		% Determine the larges possible size for a place.
		% We subtract the sign bit which is used for timers,
		% and we allocate one bit per place as a sentinel.
		%
		ActualDeltaBits = (BitAttempt-1) div (NumPlaces) - 1,
		Sizing = {
			% Position of the timer bit
			BitAttempt-1,
			% Size of the places in bits
			ActualDeltaBits,
			% Number of places
			NumPlaces },
		Sizing.

% Given the current sizing of places in a vector, allocate the
% next step up.  This is a method of dealing with overflow.
% (Underflow is different; it merely blocks transitions.)
%
sizeup_places( Sizing={TimerBitPos,DeltaBits,NumPlaces} ) ->
	size_places( NumPlaces,DeltaBits+1,TimerBItPos+1 ).

% Given a sizing for places in a vector, derive masks and
% bit fields that allow the habitual computations for this
% Petri net.
%
set_vectors( Sizing={TimerBitPos,DeltaBits,NumPlaces} ) ->
	{
		% Mask for the timer bit
		-1 bsl TimerBitPos,
		% Mask for the sentinel bits
		lists:foldl( fun(X,Y) -> X bor Y end,0,
			[ 1 bsl (DeltaBits + (DeltaBits+1)*Ctr)
			|| Ctr <- lists:seq( 0,NumPlaces-1 ) ] )
	}.

