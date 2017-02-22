/* traffic_light.c
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include "traffic_light.h"


#ifdef PETRINET_WITHOUT_NAMES
#define NAME_COMMA(x)
#else
#define NAME_COMMA(x) x,
#endif


static const transref_t yellow_trans_out [] = { 1, 2 };
static const transref_t yellow_trans_out_inh [] = { 0 };

static const transref_t red_trans_out [] = { 1, 3 };
static const transref_t red_trans_out_inh [] = { 0 };

static const transref_t green_trans_out [] = { 1, 1 };
static const transref_t green_trans_out_inh [] = { 0 };

static const placeref_t caution_place_in [] = { 1, 3 };
static const placeref_t caution_place_out [] = { 1, 1 };

static const placeref_t stop_place_in [] = { 1, 1 };
static const placeref_t stop_place_out [] = { 1, 2 };

static const placeref_t go_place_in [] = { 1, 2 };
static const placeref_t go_place_out [] = { 1, 3 };

static const place_topo_t traffic_light_places [] = {
	{ NAME_COMMA ("yellow") yellow_trans_out, yellow_trans_out_inh },
	{ NAME_COMMA ("red") red_trans_out, red_trans_out_inh },
	{ NAME_COMMA ("green") green_trans_out, green_trans_out_inh },
};

static const trans_topo_t traffic_light_transitions [] = {
	{ NAME_COMMA ("caution") caution_place_in, caution_place_out, trans_action_caution },
	{ NAME_COMMA ("stop") stop_place_in, stop_place_out, trans_action_stop },
	{ NAME_COMMA ("go") go_place_in, go_place_out, trans_action_go },
};

#ifdef PETRINET_SINGLETONS
static place_t the_traffic_light_places [] = {
	PLACE_INIT_yellow,
	PLACE_INIT_red,
	PLACE_INIT_green,
};
#endif

#ifdef PETRINET_SINGLETONS
static trans_t the_traffic_light_transitions [] = {
	TRANS_INIT_caution,
	TRANS_INIT_stop,
	TRANS_INIT_go,
};
#endif

#ifdef PETRINET_SINGLETONS
#ifdef PETRINET_GLOBAL_NAME
petrinet_t PETRINET_GLOBAL_NAME = {
#else
petrinet_t the_traffic_light = {
#endif
	NAME_COMMA (.colour = "the_traffic_light")
	.topology = {
		/* Topology is inlined due to PETRINET_SINGLETONS */
		NAME_COMMA (.name = "traffic_light")
		.place_num = 3,
		.trans_num = 3,
		.place_ary = &traffic_light_places [-1],
		.trans_ary = &traffic_light_transitions [-1],
		/* TODO: Support for inital USRDEF_PETRINET_FIELDS */
	},
	.place_ary = &the_traffic_light_places [-1],
	.trans_ary = &the_traffic_light_transitions [-1],
	/* TODO: Support for initial PLACE_HASH_CTX_FIELDS */
	/* TODO: Support for initial TRANS_HASH_CTX_FIELDS */
	/* TODO: Support for initial USRDEF_PETRINET_COLOUR_FIELDS */
#ifndef PETRINET_GLOBAL_NAME
};
#else
};
#endif
#endif

#ifndef PETRINET_SINGLETONS
const petrinet_topo_t traffic_light = {
	NAME_COMMA (.name = "traffic-light")
	.place_num = 3,
	.trans_num = 3,
	.place_ary = &traffic_light_places [-1],
	.trans_ary = &traffic_light_transitions [-1],
};
#endif



/* End of generated file traffic_light.c */
