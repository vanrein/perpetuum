-module( test ).

-include( "gen_perpetuum.hrl" ).

-export([
	traffic_lights/0,
	disco_lights/0
]).

traffic_lights () ->
	#colour {
		petrinet = #petrinet {
			numplaces = 3,
			placebits = 1,
			intlen    = 7
		},
		marking = 1,	% 0000001
		sentinel = 42,	% 0101010
		transdict = dict:new()
	}.

disco_lights () ->
	#colour {
		petrinet = #petrinet {
			numplaces = 3,
			placebits = 1,
			intlen    = 7
		},
		marking = 2,	% 0000010
		% sentinel = 42,	% 0101010
		sentinel = 43,	% 0101011
		transdict = dict:new()
	}.

