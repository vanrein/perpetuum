/* traffic_light_nightly.c
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include "traffic_light_nightly.h"


#ifdef PETRINET_WITHOUT_NAMES
#define NAME_COMMA(x)
#else
#define NAME_COMMA(x) x,
#endif


static const transref_t yellow_trans_out [] = { 1, 3 };
static const transref_t yellow_trans_out_inh [] = { 0 };

static const transref_t red_trans_out [] = { 1, 5 };
static const transref_t red_trans_out_inh [] = { 0 };

static const transref_t green_trans_out [] = { 1, 1 };
static const transref_t green_trans_out_inh [] = { 0 };

static const transref_t night_service_trans_out [] = { 1, 4 };
static const transref_t night_service_trans_out_inh [] = { 2, 2, 3 };

static const placeref_t caution_place_in [] = { 1, 3 };
static const placeref_t caution_place_out [] = { 1, 1 };

static const placeref_t dawn_place_in [] = { 0 };
static const placeref_t dawn_place_out [] = { 1, 4 };

static const placeref_t stop_place_in [] = { 1, 1 };
static const placeref_t stop_place_out [] = { 1, 2 };

static const placeref_t sunset_place_in [] = { 1, 4 };
static const placeref_t sunset_place_out [] = { 0 };

static const placeref_t go_place_in [] = { 1, 2 };
static const placeref_t go_place_out [] = { 1, 3 };

static const place_topo_t traffic_light_nightly_places [] = {
	{ NAME_COMMA ("yellow") yellow_trans_out, yellow_trans_out_inh },
	{ NAME_COMMA ("red") red_trans_out, red_trans_out_inh },
	{ NAME_COMMA ("green") green_trans_out, green_trans_out_inh },
	{ NAME_COMMA ("night_service") night_service_trans_out, night_service_trans_out_inh },
};

/* TODO: Demo mode only, this action prints transition name and timing */

#include <stdio.h>

trans_retcode_t test_action_print_trans (
				PARMDEF_COMMA (pnc)
				transref_t tr,
				time_t *nowp) {
	printf ("Firing %s -- now=%ld, notbefore=%ld, firstfail=%ld\n",
			TRANS_NAME (pnc, tr),
			(long) *nowp,
			(long) REF2TRANS (pnc, tr).notbefore,
			(long) REF2TRANS (pnc, tr).firstfail);
	return TRANS_SUCCESS;
}

static const trans_topo_t traffic_light_nightly_transitions [] = {
	{ NAME_COMMA ("caution") caution_place_in, caution_place_out, test_action_print_trans, },
	{ NAME_COMMA ("dawn") dawn_place_in, dawn_place_out, test_action_print_trans, },
	{ NAME_COMMA ("stop") stop_place_in, stop_place_out, test_action_print_trans, },
	{ NAME_COMMA ("sunset") sunset_place_in, sunset_place_out, test_action_print_trans, },
	{ NAME_COMMA ("go") go_place_in, go_place_out, test_action_print_trans, },
};

#ifdef PETRINET_SINGLETONS
static place_t the_traffic_light_nightly_places [] = {
	PLACE_INIT_yellow,
	PLACE_INIT_red,
	PLACE_INIT_green,
	PLACE_INIT_night_service,
};
#endif

#ifdef PETRINET_SINGLETONS
static trans_t the_traffic_light_nightly_transitions [] = {
	TRANS_INIT_caution,
	TRANS_INIT_dawn,
	TRANS_INIT_stop,
	TRANS_INIT_sunset,
	TRANS_INIT_go,
};
#endif

#ifdef PETRINET_SINGLETONS
#ifdef PETRINET_GLOBAL_NAME
petrinet_t PETRINET_GLOBAL_NAME = {
#else
petrinet_t the_traffic_light_nightly = {
#endif
	NAME_COMMA (.colour = "the_traffic_light_nightly")
	.topology = {
		/* Topology is inlined due to PETRINET_SINGLETONS */
		NAME_COMMA (.name = "traffic_light_nightly")
		.place_num = 4,
		.trans_num = 5,
		.place_ary = &traffic_light_nightly_places [-1],
		.trans_ary = &traffic_light_nightly_transitions [-1],
		/* TODO: Support for inital USRDEF_PETRINET_FIELDS */
#ifndef PETRINET_GLOBAL_NAME
	},
#else
	},
#endif
	.place_ary = &the_traffic_light_nightly_places [-1],
	.trans_ary = &the_traffic_light_nightly_transitions [-1],
	/* TODO: Support for initial PLACE_HASH_CTX_FIELDS */
	/* TODO: Support for initial TRANS_HASH_CTX_FIELDS */
	/* TODO: Support for initial USRDEF_PETRINET_COLOUR_FIELDS */
};
#endif

#ifndef PETRINET_SINGLETONS
const petrinet_topo_t traffic_light_nightly = {
	NAME_COMMA (.name = "traffic_light_nightly")
	.place_num = 4,
	.trans_num = 5,
	.place_ary = &traffic_light_nightly_places [-1],
	.trans_ary = &traffic_light_nightly_transitions [-1],
};
#endif



/* End of generated file traffic_light_nightly.c */
