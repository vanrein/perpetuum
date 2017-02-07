/* development_net.h
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include <stdint.h>


typedef uint8_t transref_t;
typedef uint8_t placeref_t;

#include <perpetuum/model.h>


/* Place initialisation */
#define PLACE_INIT_place00 { 0 }
#define PLACE_INIT_place29 { 0 }
#define PLACE_INIT_place07 { 0 }
#define PLACE_INIT_place39 { 0 }
#define PLACE_INIT_place09 { 0 }
#define PLACE_INIT_place43 { 0 }
#define PLACE_INIT_place40 { 0 }
#define PLACE_INIT_place18 { 0 }
#define PLACE_INIT_place20 { 0 }
#define PLACE_INIT_place15 { 0 }
#define PLACE_INIT_place38 { 0 }
#define PLACE_INIT_place48 { 0 }
#define PLACE_INIT_place49 { 0 }
#define PLACE_INIT_place10 { 0 }
#define PLACE_INIT_place27 { 0 }
#define PLACE_INIT_place17 { 0 }
#define PLACE_INIT_place42 { 0 }
#define PLACE_INIT_place41 { 0 }
#define PLACE_INIT_place44 { 0 }
#define PLACE_INIT_place47 { 0 }
#define PLACE_INIT_place37 { 0 }
#define PLACE_INIT_place32 { 0 }
#define PLACE_INIT_place33 { 0 }
#define PLACE_INIT_place46 { 0 }
#define PLACE_INIT_place45 { 0 }
#define PLACE_INIT_place02 { 0 }
#define PLACE_INIT_place22 { 0 }
#define PLACE_INIT_place23 { 0 }
#define PLACE_INIT_place16 { 0 }
#define PLACE_INIT_place25 { 0 }
#define PLACE_INIT_place13 { 0 }
#define PLACE_INIT_place03 { 0 }
#define PLACE_INIT_place34 { 0 }
#define PLACE_INIT_place05 { 0 }
#define PLACE_INIT_place21 { 0 }
#define PLACE_INIT_place26 { 0 }
#define PLACE_INIT_place06 { 0 }
#define PLACE_INIT_place24 { 0 }
#define PLACE_INIT_place14 { 0 }
#define PLACE_INIT_place31 { 0 }
#define PLACE_INIT_place35 { 0 }
#define PLACE_INIT_place11 { 0 }
#define PLACE_INIT_place36 { 0 }
#define PLACE_INIT_place08 { 0 }
#define PLACE_INIT_place28 { 0 }
#define PLACE_INIT_place12 { 0 }
#define PLACE_INIT_place04 { 0 }
#define PLACE_INIT_place30 { 0 }
#define PLACE_INIT_place01 { 0 }
#define PLACE_INIT_place19 { 0 }

/* Place initialisation with countdown; set to inputs + non-empty inhibitors*/
#define TRANS_INIT_trans24 { 9, 0, 0 }
#define TRANS_INIT_trans15 { 7, 0, 0 }
#define TRANS_INIT_trans13 { 8, 0, 0 }
#define TRANS_INIT_trans03 { 2, 0, 0 }
#define TRANS_INIT_trans16 { 5, 0, 0 }
#define TRANS_INIT_trans18 { 1, 0, 0 }
#define TRANS_INIT_trans06 { 4, 0, 0 }
#define TRANS_INIT_trans34 { 2, 0, 0 }
#define TRANS_INIT_trans25 { 8, 0, 0 }
#define TRANS_INIT_trans05 { 5, 0, 0 }
#define TRANS_INIT_trans21 { 4, 0, 0 }
#define TRANS_INIT_trans04 { 4, 0, 0 }
#define TRANS_INIT_trans20 { 4, 0, 0 }
#define TRANS_INIT_trans27 { 7, 0, 0 }
#define TRANS_INIT_trans38 { 7, 0, 0 }
#define TRANS_INIT_trans39 { 4, 0, 0 }
#define TRANS_INIT_trans01 { 9, 0, 0 }
#define TRANS_INIT_trans32 { 7, 0, 0 }
#define TRANS_INIT_trans35 { 3, 0, 0 }
#define TRANS_INIT_trans26 { 4, 0, 0 }
#define TRANS_INIT_trans02 { 5, 0, 0 }
#define TRANS_INIT_trans23 { 4, 0, 0 }
#define TRANS_INIT_trans28 { 2, 0, 0 }
#define TRANS_INIT_trans07 { 8, 0, 0 }
#define TRANS_INIT_trans00 { 3, 0, 0 }
#define TRANS_INIT_trans08 { 7, 0, 0 }
#define TRANS_INIT_trans31 { 2, 0, 0 }
#define TRANS_INIT_trans11 { 6, 0, 0 }
#define TRANS_INIT_trans19 { 3, 0, 0 }
#define TRANS_INIT_trans22 { 6, 0, 0 }
#define TRANS_INIT_trans12 { 4, 0, 0 }
#define TRANS_INIT_trans09 { 7, 0, 0 }
#define TRANS_INIT_trans30 { 1, 0, 0 }
#define TRANS_INIT_trans29 { 5, 0, 0 }
#define TRANS_INIT_trans10 { 3, 0, 0 }
#define TRANS_INIT_trans36 { 3, 0, 0 }
#define TRANS_INIT_trans17 { 6, 0, 0 }
#define TRANS_INIT_trans14 { 9, 0, 0 }
#define TRANS_INIT_trans33 { 6, 0, 0 }
#define TRANS_INIT_trans37 { 2, 0, 0 }

#ifdef PETRINET_SINGLETONS
extern petrinet_colour_t the_development_net;
#endif

#ifndef PETRINET_SINGLETONS
extern const petrinet_t development_net;
#else
#ifdef PETRINET_GLOBAL_NAME
#define development_net (&PETRINET_GLOBAL_NAME.topology)
#else
#define development_net (&the_development_net->topology)
#endif
#endif



/* End of generated file development_net.h */
