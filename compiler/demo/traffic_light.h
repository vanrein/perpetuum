/* traffic_light.h
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include <stdint.h>

#include <perpetuum/model.h>


/* Place initialisation */
#define PLACE_INIT_yellow { 0 }
#define PLACE_INIT_red { 1 }
#define PLACE_INIT_green { 0 }

/* Place initialisation; countdown := empty inputs + non-empty inhibitors */
#define TRANS_INIT_stop { 1 + 0, 0, 0 }
#define TRANS_INIT_caution { 1 + 0, 0, 0 }
#define TRANS_INIT_go { 0 + 0, 0, 0 }

#ifdef PETRINET_SINGLETONS
#ifdef PETRINET_GLOBAL_NAME
extern petrinet_t PETRINET_GLOBAL_NAME;
#else
extern petrinet_t the_traffic_light;
#endif
#endif

#ifndef PETRINET_SINGLETONS
extern const petrinet_topo_t traffic_light;
#else
#ifdef PETRINET_GLOBAL_NAME
#define traffic_light (&PETRINET_GLOBAL_NAME.topology)
#else
#define traffic_light (&the_traffic_light->topology)
#endif
#endif



/* End of generated file traffic_light.h */
