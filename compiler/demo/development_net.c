/* development_net.c
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include "development_net.h"


static const transref_t place00_trans_out [] = { 5, 3, 10, 26, 34, 40 };
static const transref_t place00_trans_out_inh [] = { 0 };

static const transref_t place29_trans_out [] = { 6, 3, 8, 12, 18, 26, 31 };
static const transref_t place29_trans_out_inh [] = { 0 };

static const transref_t place07_trans_out [] = { 4, 15, 29, 30, 34 };
static const transref_t place07_trans_out_inh [] = { 0 };

static const transref_t place39_trans_out [] = { 3, 2, 9, 17 };
static const transref_t place39_trans_out_inh [] = { 0 };

static const transref_t place09_trans_out [] = { 5, 10, 13, 15, 28, 36 };
static const transref_t place09_trans_out_inh [] = { 0 };

static const transref_t place43_trans_out [] = { 3, 3, 37, 38 };
static const transref_t place43_trans_out_inh [] = { 0 };

static const transref_t place40_trans_out [] = { 4, 4, 9, 22, 28 };
static const transref_t place40_trans_out_inh [] = { 0 };

static const transref_t place18_trans_out [] = { 1, 31 };
static const transref_t place18_trans_out_inh [] = { 0 };

static const transref_t place20_trans_out [] = { 0 };
static const transref_t place20_trans_out_inh [] = { 0 };

static const transref_t place15_trans_out [] = { 3, 13, 17, 28 };
static const transref_t place15_trans_out_inh [] = { 0 };

static const transref_t place38_trans_out [] = { 9, 1, 7, 12, 14, 15, 17, 28, 34, 38 };
static const transref_t place38_trans_out_inh [] = { 0 };

static const transref_t place48_trans_out [] = { 6, 1, 5, 14, 24, 25, 26 };
static const transref_t place48_trans_out_inh [] = { 0 };

static const transref_t place49_trans_out [] = { 5, 3, 9, 16, 18, 27 };
static const transref_t place49_trans_out_inh [] = { 0 };

static const transref_t place10_trans_out [] = { 1, 21 };
static const transref_t place10_trans_out_inh [] = { 0 };

static const transref_t place27_trans_out [] = { 2, 2, 4 };
static const transref_t place27_trans_out_inh [] = { 0 };

static const transref_t place17_trans_out [] = { 3, 1, 32, 35 };
static const transref_t place17_trans_out_inh [] = { 0 };

static const transref_t place42_trans_out [] = { 6, 5, 15, 17, 18, 25, 40 };
static const transref_t place42_trans_out_inh [] = { 0 };

static const transref_t place41_trans_out [] = { 3, 14, 17, 22 };
static const transref_t place41_trans_out_inh [] = { 0 };

static const transref_t place44_trans_out [] = { 2, 38, 39 };
static const transref_t place44_trans_out_inh [] = { 0 };

static const transref_t place47_trans_out [] = { 7, 1, 7, 14, 18, 21, 30, 31 };
static const transref_t place47_trans_out_inh [] = { 0 };

static const transref_t place37_trans_out [] = { 7, 9, 15, 16, 17, 27, 31, 39 };
static const transref_t place37_trans_out_inh [] = { 0 };

static const transref_t place32_trans_out [] = { 6, 1, 8, 13, 20, 24, 32 };
static const transref_t place32_trans_out_inh [] = { 0 };

static const transref_t place33_trans_out [] = { 4, 18, 19, 34, 35 };
static const transref_t place33_trans_out_inh [] = { 0 };

static const transref_t place46_trans_out [] = { 2, 11, 28 };
static const transref_t place46_trans_out_inh [] = { 0 };

static const transref_t place45_trans_out [] = { 5, 9, 12, 23, 34, 36 };
static const transref_t place45_trans_out_inh [] = { 0 };

static const transref_t place02_trans_out [] = { 2, 9, 29 };
static const transref_t place02_trans_out_inh [] = { 0 };

static const transref_t place22_trans_out [] = { 3, 25, 37, 38 };
static const transref_t place22_trans_out_inh [] = { 0 };

static const transref_t place23_trans_out [] = { 1, 5 };
static const transref_t place23_trans_out_inh [] = { 0 };

static const transref_t place16_trans_out [] = { 8, 15, 17, 18, 22, 30, 32, 38, 39 };
static const transref_t place16_trans_out_inh [] = { 0 };

static const transref_t place25_trans_out [] = { 3, 21, 23, 39 };
static const transref_t place25_trans_out_inh [] = { 0 };

static const transref_t place13_trans_out [] = { 6, 3, 12, 19, 24, 26, 32 };
static const transref_t place13_trans_out_inh [] = { 0 };

static const transref_t place03_trans_out [] = { 3, 14, 22, 35 };
static const transref_t place03_trans_out_inh [] = { 0 };

static const transref_t place34_trans_out [] = { 7, 3, 7, 9, 11, 21, 29, 37 };
static const transref_t place34_trans_out_inh [] = { 0 };

static const transref_t place05_trans_out [] = { 3, 1, 19, 39 };
static const transref_t place05_trans_out_inh [] = { 0 };

static const transref_t place21_trans_out [] = { 4, 1, 3, 5, 13 };
static const transref_t place21_trans_out_inh [] = { 0 };

static const transref_t place26_trans_out [] = { 5, 2, 7, 16, 20, 30 };
static const transref_t place26_trans_out_inh [] = { 0 };

static const transref_t place06_trans_out [] = { 5, 2, 5, 9, 10, 11 };
static const transref_t place06_trans_out_inh [] = { 0 };

static const transref_t place24_trans_out [] = { 4, 1, 20, 24, 32 };
static const transref_t place24_trans_out_inh [] = { 0 };

static const transref_t place14_trans_out [] = { 1, 24 };
static const transref_t place14_trans_out_inh [] = { 0 };

static const transref_t place31_trans_out [] = { 7, 2, 6, 10, 15, 20, 24, 32 };
static const transref_t place31_trans_out_inh [] = { 0 };

static const transref_t place35_trans_out [] = { 5, 3, 11, 17, 26, 37 };
static const transref_t place35_trans_out_inh [] = { 0 };

static const transref_t place11_trans_out [] = { 3, 1, 17, 38 };
static const transref_t place11_trans_out_inh [] = { 0 };

static const transref_t place36_trans_out [] = { 1, 30 };
static const transref_t place36_trans_out_inh [] = { 0 };

static const transref_t place08_trans_out [] = { 4, 2, 16, 24, 37 };
static const transref_t place08_trans_out_inh [] = { 0 };

static const transref_t place28_trans_out [] = { 7, 18, 24, 26, 33, 36, 38, 39 };
static const transref_t place28_trans_out_inh [] = { 0 };

static const transref_t place12_trans_out [] = { 1, 26 };
static const transref_t place12_trans_out_inh [] = { 0 };

static const transref_t place04_trans_out [] = { 2, 2, 38 };
static const transref_t place04_trans_out_inh [] = { 0 };

static const transref_t place30_trans_out [] = { 2, 14, 21 };
static const transref_t place30_trans_out_inh [] = { 0 };

static const transref_t place01_trans_out [] = { 4, 10, 28, 37, 38 };
static const transref_t place01_trans_out_inh [] = { 0 };

static const transref_t place19_trans_out [] = { 3, 14, 30, 32 };
static const transref_t place19_trans_out_inh [] = { 0 };

static const placeref_t trans24_place_in [] = { 9, 11, 12, 16, 20, 22, 34, 35, 38, 42 };
static const placeref_t trans24_place_out [] = { 4, 1, 5, 16, 19 };

static const placeref_t trans15_place_in [] = { 7, 4, 15, 36, 37, 40, 44, 47 };
static const placeref_t trans15_place_out [] = { 4, 4, 9, 10, 28 };

static const placeref_t trans13_place_in [] = { 8, 1, 2, 6, 13, 31, 33, 35, 41 };
static const placeref_t trans13_place_out [] = { 5, 7, 25, 35, 41, 42 };

static const placeref_t trans03_place_in [] = { 2, 7, 15 };
static const placeref_t trans03_place_out [] = { 7, 1, 4, 8, 13, 17, 21, 25 };

static const placeref_t trans16_place_in [] = { 5, 12, 17, 28, 35, 37 };
static const placeref_t trans16_place_out [] = { 7, 2, 14, 17, 18, 21, 38, 50 };

static const placeref_t trans18_place_in [] = { 1, 40 };
static const placeref_t trans18_place_out [] = { 6, 2, 6, 8, 24, 40, 46 };

static const placeref_t trans06_place_in [] = { 4, 11, 20, 33, 36 };
static const placeref_t trans06_place_out [] = { 5, 2, 14, 18, 39, 47 };

static const placeref_t trans34_place_in [] = { 2, 2, 22 };
static const placeref_t trans34_place_out [] = { 7, 5, 6, 8, 10, 26, 33, 36 };

static const placeref_t trans25_place_in [] = { 8, 4, 7, 13, 21, 25, 26, 33, 37 };
static const placeref_t trans25_place_out [] = { 4, 6, 11, 31, 36 };

static const placeref_t trans05_place_in [] = { 5, 1, 5, 37, 40, 49 };
static const placeref_t trans05_place_out [] = { 2, 13, 19 };

static const placeref_t trans21_place_in [] = { 4, 24, 33, 37, 41 };
static const placeref_t trans21_place_out [] = { 7, 1, 2, 5, 12, 24, 36, 43 };

static const placeref_t trans04_place_in [] = { 4, 2, 11, 25, 31 };
static const placeref_t trans04_place_out [] = { 4, 9, 23, 34, 47 };

static const placeref_t trans20_place_in [] = { 4, 5, 10, 22, 35 };
static const placeref_t trans20_place_out [] = { 3, 17, 18, 25 };

static const placeref_t trans27_place_in [] = { 7, 11, 12, 18, 20, 32, 48, 50 };
static const placeref_t trans27_place_out [] = { 3, 4, 34, 36 };

static const placeref_t trans38_place_in [] = { 7, 3, 5, 11, 17, 21, 29, 40 };
static const placeref_t trans38_place_out [] = { 4, 4, 14, 38, 49 };

static const placeref_t trans39_place_in [] = { 4, 13, 21, 36, 44 };
static const placeref_t trans39_place_out [] = { 7, 8, 20, 23, 26, 34, 39, 46 };

static const placeref_t trans01_place_in [] = { 9, 4, 10, 11, 17, 18, 21, 29, 41, 42 };
static const placeref_t trans01_place_out [] = { 6, 3, 4, 8, 29, 35, 47 };

static const placeref_t trans32_place_in [] = { 7, 2, 13, 17, 20, 23, 29, 45 };
static const placeref_t trans32_place_out [] = { 2, 27, 42 };

static const placeref_t trans35_place_in [] = { 3, 23, 31, 34 };
static const placeref_t trans35_place_out [] = { 5, 5, 19, 22, 26, 34 };

static const placeref_t trans26_place_in [] = { 4, 22, 36, 38, 40 };
static const placeref_t trans26_place_out [] = { 4, 3, 19, 29, 39 };

static const placeref_t trans02_place_in [] = { 5, 14, 20, 30, 33, 48 };
static const placeref_t trans02_place_out [] = { 3, 14, 28, 33 };

static const placeref_t trans23_place_in [] = { 4, 7, 18, 29, 32 };
static const placeref_t trans23_place_out [] = { 4, 10, 35, 43, 48 };

static const placeref_t trans28_place_in [] = { 2, 25, 30 };
static const placeref_t trans28_place_out [] = { 11, 1, 4, 7, 12, 14, 15, 16, 17, 20, 23, 31 };

static const placeref_t trans07_place_in [] = { 8, 12, 22, 31, 38, 39, 40, 44, 45 };
static const placeref_t trans07_place_out [] = { 13, 2, 6, 15, 20, 24, 29, 30, 33, 35, 38, 40, 41, 44 };

static const placeref_t trans00_place_in [] = { 3, 12, 17, 27 };
static const placeref_t trans00_place_out [] = { 5, 11, 27, 31, 37, 49 };

static const placeref_t trans08_place_in [] = { 7, 1, 2, 12, 31, 41, 45, 46 };
static const placeref_t trans08_place_out [] = { 4, 7, 26, 27, 33 };

static const placeref_t trans31_place_in [] = { 2, 13, 21 };
static const placeref_t trans31_place_out [] = { 7, 7, 14, 16, 17, 18, 24, 32 };

static const placeref_t trans11_place_in [] = { 6, 5, 7, 10, 11, 24, 49 };
static const placeref_t trans11_place_out [] = { 4, 3, 10, 24, 41 };

static const placeref_t trans19_place_in [] = { 3, 3, 26, 33 };
static const placeref_t trans19_place_out [] = { 9, 18, 22, 30, 31, 37, 42, 43, 45, 46 };

static const placeref_t trans22_place_in [] = { 6, 3, 20, 29, 36, 43, 50 };
static const placeref_t trans22_place_out [] = { 5, 10, 20, 28, 31, 39 };

static const placeref_t trans12_place_in [] = { 4, 2, 8, 20, 21 };
static const placeref_t trans12_place_out [] = { 4, 9, 13, 28, 43 };

static const placeref_t trans09_place_in [] = { 7, 16, 22, 29, 31, 38, 40, 50 };
static const placeref_t trans09_place_out [] = { 5, 4, 6, 7, 17, 22 };

static const placeref_t trans30_place_in [] = { 1, 45 };
static const placeref_t trans30_place_out [] = { 10, 5, 9, 11, 18, 33, 39, 41, 46, 49, 50 };

static const placeref_t trans29_place_in [] = { 5, 1, 3, 11, 23, 25 };
static const placeref_t trans29_place_out [] = { 1, 18 };

static const placeref_t trans10_place_in [] = { 3, 16, 23, 32 };
static const placeref_t trans10_place_out [] = { 4, 24, 39, 40, 41 };

static const placeref_t trans36_place_in [] = { 3, 5, 25, 45 };
static const placeref_t trans36_place_out [] = { 10, 2, 4, 11, 16, 27, 39, 40, 42, 46, 49 };

static const placeref_t trans17_place_in [] = { 6, 6, 27, 33, 41, 44, 49 };
static const placeref_t trans17_place_out [] = { 6, 19, 20, 21, 23, 25, 34 };

static const placeref_t trans14_place_in [] = { 9, 6, 11, 19, 27, 29, 42, 45, 47, 49 };
static const placeref_t trans14_place_out [] = { 9, 2, 6, 15, 17, 21, 24, 25, 41, 50 };

static const placeref_t trans33_place_in [] = { 6, 19, 21, 29, 30, 34, 45 };
static const placeref_t trans33_place_out [] = { 10, 2, 9, 10, 13, 19, 23, 27, 28, 34, 38 };

static const placeref_t trans37_place_in [] = { 2, 1, 17 };
static const placeref_t trans37_place_out [] = { 10, 2, 3, 14, 19, 25, 37, 39, 40, 47, 50 };

static const place_topo_t development_net_places [] = {
	{ "place00", place00_trans_out, place00_trans_out_inh },
	{ "place29", place29_trans_out, place29_trans_out_inh },
	{ "place07", place07_trans_out, place07_trans_out_inh },
	{ "place39", place39_trans_out, place39_trans_out_inh },
	{ "place09", place09_trans_out, place09_trans_out_inh },
	{ "place43", place43_trans_out, place43_trans_out_inh },
	{ "place40", place40_trans_out, place40_trans_out_inh },
	{ "place18", place18_trans_out, place18_trans_out_inh },
	{ "place20", place20_trans_out, place20_trans_out_inh },
	{ "place15", place15_trans_out, place15_trans_out_inh },
	{ "place38", place38_trans_out, place38_trans_out_inh },
	{ "place48", place48_trans_out, place48_trans_out_inh },
	{ "place49", place49_trans_out, place49_trans_out_inh },
	{ "place10", place10_trans_out, place10_trans_out_inh },
	{ "place27", place27_trans_out, place27_trans_out_inh },
	{ "place17", place17_trans_out, place17_trans_out_inh },
	{ "place42", place42_trans_out, place42_trans_out_inh },
	{ "place41", place41_trans_out, place41_trans_out_inh },
	{ "place44", place44_trans_out, place44_trans_out_inh },
	{ "place47", place47_trans_out, place47_trans_out_inh },
	{ "place37", place37_trans_out, place37_trans_out_inh },
	{ "place32", place32_trans_out, place32_trans_out_inh },
	{ "place33", place33_trans_out, place33_trans_out_inh },
	{ "place46", place46_trans_out, place46_trans_out_inh },
	{ "place45", place45_trans_out, place45_trans_out_inh },
	{ "place02", place02_trans_out, place02_trans_out_inh },
	{ "place22", place22_trans_out, place22_trans_out_inh },
	{ "place23", place23_trans_out, place23_trans_out_inh },
	{ "place16", place16_trans_out, place16_trans_out_inh },
	{ "place25", place25_trans_out, place25_trans_out_inh },
	{ "place13", place13_trans_out, place13_trans_out_inh },
	{ "place03", place03_trans_out, place03_trans_out_inh },
	{ "place34", place34_trans_out, place34_trans_out_inh },
	{ "place05", place05_trans_out, place05_trans_out_inh },
	{ "place21", place21_trans_out, place21_trans_out_inh },
	{ "place26", place26_trans_out, place26_trans_out_inh },
	{ "place06", place06_trans_out, place06_trans_out_inh },
	{ "place24", place24_trans_out, place24_trans_out_inh },
	{ "place14", place14_trans_out, place14_trans_out_inh },
	{ "place31", place31_trans_out, place31_trans_out_inh },
	{ "place35", place35_trans_out, place35_trans_out_inh },
	{ "place11", place11_trans_out, place11_trans_out_inh },
	{ "place36", place36_trans_out, place36_trans_out_inh },
	{ "place08", place08_trans_out, place08_trans_out_inh },
	{ "place28", place28_trans_out, place28_trans_out_inh },
	{ "place12", place12_trans_out, place12_trans_out_inh },
	{ "place04", place04_trans_out, place04_trans_out_inh },
	{ "place30", place30_trans_out, place30_trans_out_inh },
	{ "place01", place01_trans_out, place01_trans_out_inh },
	{ "place19", place19_trans_out, place19_trans_out_inh },
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

static const trans_topo_t development_net_transitions [] = {
	{ "trans24", trans24_place_in, trans24_place_out, test_action_print_trans, },
	{ "trans15", trans15_place_in, trans15_place_out, test_action_print_trans, },
	{ "trans13", trans13_place_in, trans13_place_out, test_action_print_trans, },
	{ "trans03", trans03_place_in, trans03_place_out, test_action_print_trans, },
	{ "trans16", trans16_place_in, trans16_place_out, test_action_print_trans, },
	{ "trans18", trans18_place_in, trans18_place_out, test_action_print_trans, },
	{ "trans06", trans06_place_in, trans06_place_out, test_action_print_trans, },
	{ "trans34", trans34_place_in, trans34_place_out, test_action_print_trans, },
	{ "trans25", trans25_place_in, trans25_place_out, test_action_print_trans, },
	{ "trans05", trans05_place_in, trans05_place_out, test_action_print_trans, },
	{ "trans21", trans21_place_in, trans21_place_out, test_action_print_trans, },
	{ "trans04", trans04_place_in, trans04_place_out, test_action_print_trans, },
	{ "trans20", trans20_place_in, trans20_place_out, test_action_print_trans, },
	{ "trans27", trans27_place_in, trans27_place_out, test_action_print_trans, },
	{ "trans38", trans38_place_in, trans38_place_out, test_action_print_trans, },
	{ "trans39", trans39_place_in, trans39_place_out, test_action_print_trans, },
	{ "trans01", trans01_place_in, trans01_place_out, test_action_print_trans, },
	{ "trans32", trans32_place_in, trans32_place_out, test_action_print_trans, },
	{ "trans35", trans35_place_in, trans35_place_out, test_action_print_trans, },
	{ "trans26", trans26_place_in, trans26_place_out, test_action_print_trans, },
	{ "trans02", trans02_place_in, trans02_place_out, test_action_print_trans, },
	{ "trans23", trans23_place_in, trans23_place_out, test_action_print_trans, },
	{ "trans28", trans28_place_in, trans28_place_out, test_action_print_trans, },
	{ "trans07", trans07_place_in, trans07_place_out, test_action_print_trans, },
	{ "trans00", trans00_place_in, trans00_place_out, test_action_print_trans, },
	{ "trans08", trans08_place_in, trans08_place_out, test_action_print_trans, },
	{ "trans31", trans31_place_in, trans31_place_out, test_action_print_trans, },
	{ "trans11", trans11_place_in, trans11_place_out, test_action_print_trans, },
	{ "trans19", trans19_place_in, trans19_place_out, test_action_print_trans, },
	{ "trans22", trans22_place_in, trans22_place_out, test_action_print_trans, },
	{ "trans12", trans12_place_in, trans12_place_out, test_action_print_trans, },
	{ "trans09", trans09_place_in, trans09_place_out, test_action_print_trans, },
	{ "trans30", trans30_place_in, trans30_place_out, test_action_print_trans, },
	{ "trans29", trans29_place_in, trans29_place_out, test_action_print_trans, },
	{ "trans10", trans10_place_in, trans10_place_out, test_action_print_trans, },
	{ "trans36", trans36_place_in, trans36_place_out, test_action_print_trans, },
	{ "trans17", trans17_place_in, trans17_place_out, test_action_print_trans, },
	{ "trans14", trans14_place_in, trans14_place_out, test_action_print_trans, },
	{ "trans33", trans33_place_in, trans33_place_out, test_action_print_trans, },
	{ "trans37", trans37_place_in, trans37_place_out, test_action_print_trans, },
};

#ifdef PETRINET_SINGLETONS
static place_t the_development_net_places [] = {
	PLACE_INIT_place00,
	PLACE_INIT_place29,
	PLACE_INIT_place07,
	PLACE_INIT_place39,
	PLACE_INIT_place09,
	PLACE_INIT_place43,
	PLACE_INIT_place40,
	PLACE_INIT_place18,
	PLACE_INIT_place20,
	PLACE_INIT_place15,
	PLACE_INIT_place38,
	PLACE_INIT_place48,
	PLACE_INIT_place49,
	PLACE_INIT_place10,
	PLACE_INIT_place27,
	PLACE_INIT_place17,
	PLACE_INIT_place42,
	PLACE_INIT_place41,
	PLACE_INIT_place44,
	PLACE_INIT_place47,
	PLACE_INIT_place37,
	PLACE_INIT_place32,
	PLACE_INIT_place33,
	PLACE_INIT_place46,
	PLACE_INIT_place45,
	PLACE_INIT_place02,
	PLACE_INIT_place22,
	PLACE_INIT_place23,
	PLACE_INIT_place16,
	PLACE_INIT_place25,
	PLACE_INIT_place13,
	PLACE_INIT_place03,
	PLACE_INIT_place34,
	PLACE_INIT_place05,
	PLACE_INIT_place21,
	PLACE_INIT_place26,
	PLACE_INIT_place06,
	PLACE_INIT_place24,
	PLACE_INIT_place14,
	PLACE_INIT_place31,
	PLACE_INIT_place35,
	PLACE_INIT_place11,
	PLACE_INIT_place36,
	PLACE_INIT_place08,
	PLACE_INIT_place28,
	PLACE_INIT_place12,
	PLACE_INIT_place04,
	PLACE_INIT_place30,
	PLACE_INIT_place01,
	PLACE_INIT_place19,
};
#endif

#ifdef PETRINET_SINGLETONS
static trans_t the_development_net_transitions [] = {
	TRANS_INIT_trans24,
	TRANS_INIT_trans15,
	TRANS_INIT_trans13,
	TRANS_INIT_trans03,
	TRANS_INIT_trans16,
	TRANS_INIT_trans18,
	TRANS_INIT_trans06,
	TRANS_INIT_trans34,
	TRANS_INIT_trans25,
	TRANS_INIT_trans05,
	TRANS_INIT_trans21,
	TRANS_INIT_trans04,
	TRANS_INIT_trans20,
	TRANS_INIT_trans27,
	TRANS_INIT_trans38,
	TRANS_INIT_trans39,
	TRANS_INIT_trans01,
	TRANS_INIT_trans32,
	TRANS_INIT_trans35,
	TRANS_INIT_trans26,
	TRANS_INIT_trans02,
	TRANS_INIT_trans23,
	TRANS_INIT_trans28,
	TRANS_INIT_trans07,
	TRANS_INIT_trans00,
	TRANS_INIT_trans08,
	TRANS_INIT_trans31,
	TRANS_INIT_trans11,
	TRANS_INIT_trans19,
	TRANS_INIT_trans22,
	TRANS_INIT_trans12,
	TRANS_INIT_trans09,
	TRANS_INIT_trans30,
	TRANS_INIT_trans29,
	TRANS_INIT_trans10,
	TRANS_INIT_trans36,
	TRANS_INIT_trans17,
	TRANS_INIT_trans14,
	TRANS_INIT_trans33,
	TRANS_INIT_trans37,
};
#endif

#ifdef PETRINET_SINGLETONS
petrinet_t the_development_net = {
	"the_development_net",
	{ /* PETRINET_SINGLETONS => inlined topology */
		"development_net",
		50,
		40,
		&development_net_places [-1],
		&development_net_transitions [-1],
		/* TODO: Support for inital USRDEF_PETRINET_FIELDS */
	},
	&the_development_net_places [-1],
	&the_development_net_transitions [-1],
	/* TODO: Support for initial PLACE_HASH_CTX_FIELDS */
	/* TODO: Support for initial TRANS_HASH_CTX_FIELDS */
	/* TODO: Support for initial USRDEF_PETRINET_COLOUR_FIELDS */
};
#endif

#ifndef PETRINET_SINGLETONS
const petrinet_topo_t development_net = {
	"development_net",
	50, 40,
	&development_net_places [-1], &development_net_transitions [-1]
};
#endif



/* End of generated file development_net.c */
