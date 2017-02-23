/* Perpetuum's model of Petri Nets, for a single Colour.
 *
 * This is meant as a runtime model.  Transitions are modelled as quick
 * actions that may succeed or fail (or lead to a deferred retry).  Places
 * are merely counters for acquired tokens.
 *
 * The purpose of this code is to achieve straightforward integration with
 * event loop mechanisms, and to making those coordinate external actions.
 * It should be relatively straightforward to redesign the flow using Petri
 * nets, and analysing the model with the many tools that exist for them.
 * The integration of this new overview with the transition logic should
 * be trivial because it is generated.  One area where this would be useful
 * is the management of identity information comprising of public and
 * private keys, perhaps a Kerberos principal, DNS entries, DANE records,
 * DNSSEC signing, LDAP repositories, and so on.  This is a bit like an
 * "operational Makefile".
 *
 * Generated logic can be extended in a generic manner.  For example,
 * a lot of useful monitoring information should be easy to derive from
 * progress in the Petri nets.
 *
 * From: Rick van Rein <rick@openfortress.nl>
 */


#ifndef PERPETUUM_MODEL_H
#define PERPETUUM_MODEL_H


#include <stdint.h>

#include <time.h>


/* Types for place and transition are subjects for compaction when their
 * numbers are known.  The model will consistently refer to these types
 * to accommodate that.  They must be just large enough to hold the number
 * of elements of the given type.
 *
 * Note: These should be defined by applications, so they are commented out.
 */
//APPDEF// typedef unsigned int placeref_t;
//APPDEF// typedef unsigned int transref_t;
#include "petritypes.h"
typedef placeref_t placectr_t;
typedef transref_t transctr_t;


/* The values NIL_PLACE and NIL_TRANS represent invalid, unlisted, unavailable
 * values for places and transitions.  They are set to 0 so they can be easily
 * tested by an optimising compiler.  This means that counting of places and
 * transitions starts from 1, not from 0.
 */
#define NIL_PLACE 0
#define NIL_TRANS 0


/* Lists of transitions and places are used to reference back and forth
 * between the elements.  The convention is this:
 *  - transition 0 and place 0 are NIL_TRANS and NIL_PLACE
 *  - counting starts from 1, so list entry [1] represents number 1
 *  - the entry [0] in the list gives the number of entries
 *  - the total array size is 1 (for entry [0]) plus the number of entries
 *  - the highest entry matches the number of entries
 *  - these lists must not be set to NULL pointers; empty lists offer 0 entries
 * This is not the default convention in C, but it seems to make sense here.
 */
typedef placeref_t placeref_list_t [1];
typedef transref_t transref_list_t [1];
typedef placeref_t *placeref_listref_t;
typedef transref_t *transref_listref_t;


#define FOR_PLACEREF(idx,var,ary) \
		for ((idx) = *(ary); (var) = (ary) [idx], (idx) > 0; (idx)--)
#define FOR_TRANSREF(idx,var,ary) \
		for ((idx) = *(ary); (var) = (ary) [idx], (idx) > 0; (idx)--)


/* Token counters are integers up to a certain value.  By analysing a model
 * and proving properties over it, you should be able to determine the highest
 * count of tokens that can even occur -- you should never design models with
 * boundless tokens.  Since work to do may acrue, the values may turn out
 * higher in some runs than in others.  In general, given the highest number
 * that could ever acrue anywhere, this type might be persuaded to modest
 * sizes.
 */
typedef unsigned int tokenctr_t;


/* The place type represents place internal administration, as well as the
 * references back and forth through lists of references.  In fact, the
 * back references to trans_in does not seem to be used anywhere.
 *
 * The available token count includes locked tokens, which are removed from
 * the unlocked count.  When attempting a transition, its inputs will lower
 * the unlocked count; upon success, their available will also be lowered
 * and upon failure, their count of unlocked tokens will increase.
 * The trans_out_inh represents inhibitor output arcs.
 */

typedef struct {
#ifndef PETRINET_WITHOUT_NAMES
	const char *name;
#endif
	const transref_listref_t trans_out;		/* never NULL */
	const transref_listref_t trans_out_inh;		/* never NULL */
} place_topo_t;

typedef struct {
	tokenctr_t available;
#ifndef BUILD_SINGLE_THREADED
	tokenctr_t unlocked;
#endif
} place_t;


/* Return codes for transitions make clear what should be done.  They can
 * be set to a number of values:
 *  -  0 for success
 *  - >0 for delays of so many seconds
 *  - ~0 for "infinite delays", meaning "forget it" or "failed"
 * The type is an unsigned integer that may be scaled down as long as the
 * highest timeout to expect it less than ~0.
 *
 * The macros help to symbolically return these values.  Note how the
 * TRANS_DELAY_SECURE(t) macro can be used to avoid external tools to
 * smuggle failure or success into the system by way of a delay.  You
 * should also be caustious about signed/unsigned mixup warnings.  Use
 * TRANS_MAXDELAY to avoid retries anytime soon; this is helpful when
 * a transition is waiting for an outside event to cause it to happen.
 */

typedef unsigned int trans_retcode_t;
#define TRANS_SUCCESS 0
#define TRANS_FAILURE (~(trans_retcode_t)0)
#define TRANS_DELAY(t) ((trans_retcode)(t))
#define TRANS_DELAY_SECURE(t) \
		(((t)==TRANS_SUCCESS)? \
			1: \
			(((t)>=TRANS_FAILURE)? \
				(TRANS_FAILURE-1): \
				(t)))
#define TRANS_MAXDELAY ((TRANS_FAILURE)-1)


/* The following macros are used to allow for compilation variants between
 * the most flexible and the most compact representations.  They do not make
 * the code much more appealing, but are hoped to keep it readable by
 * concealing the horrors caused by the variations.
 */
#ifdef PETRINET_GLOBAL_NAME
#  define PARMDEF(pnc)
#  define PARMDEF_COMMA(pnc)
#  define COMMA_PARMDEF(pnc)
#  define PARMARG(pnc)
#  define PARMARG_COMMA(pnc)
#  define COMMA_PARMARG(pnc)
#  define PARM_USE(pnc) (&PETRINET_GLOBAL_NAME)
#else
#  define PARMDEF(pnc) petrinet_t *pnc
#  define PARMDEF_COMMA(pnc)  PARMDEF(pnc),
#  define COMMA_PARMDEF(pnc) ,PARMDEF(pnc)
#  define PARM_USE(pnc) (pnc)
#  define PARMARG(pnc) (pnc)
#  define PARMARG_COMMA(pnc)  PARMARG(pnc),
#  define COMMA_PARMARG(pnc) ,PARMARG(pnc)
#endif

#ifdef PETRINET_SINGLETONS
#  define TOPO(pnc) (&PARM_USE(pnc)->topology)
#else
#  define TOPO(pnc) ( PARM_USE(pnc)->topology)
#endif

#define REF2PLACE(pnc,pr) (PARM_USE(pnc)->place_ary[pr])
#define REF2TRANS(pnc,pr) (PARM_USE(pnc)->trans_ary[pr])

#define REF2PLACE_TOPO(pnc,pr) (TOPO(pnc)->place_ary[pr])
#define REF2TRANS_TOPO(pnc,pr) (TOPO(pnc)->trans_ary[pr])

#ifdef PETRINET_WITHOUT_NAMES
#define PLACE_NAME(pnc,pr) "Place_X"
#define TRANS_NAME(pnc,tr) "Trans_X" 
#define PETRI_NAME(pnc)    "Petri_X"
#else
#define PLACE_NAME(pnc,pr) (REF2PLACE_TOPO((pnc),(pr)).name)
#define TRANS_NAME(pnc,tr) (REF2TRANS_TOPO((pnc),(tr)).name)
#define PETRI_NAME(pnc)    (TOPO(pnc).name)
#endif


/* The trans type represents transition internal administration, as well as
 * the references back and forth through lists of references.  These lists
 * are actively used while trying, committing or rolling back a transitions'
 * associated action, or while sending out tokens after success.
 * The place_in_inh represents inhibitor's input arcs.
 */

typedef struct trans_topo_st trans_topo_t;
typedef struct trans_st trans_t;

typedef struct petrinet_topo_st petrinet_topo_t;
typedef struct petrinet_st petrinet_t;

typedef struct trans_topo_st {
#ifndef PETRINET_WITHOUT_NAMES
	const char *name;
#endif
	const placeref_listref_t place_in;		/* never NULL */
	const placeref_listref_t place_out;		/* never NULL */
	trans_retcode_t (*const action) (		/* never NULL */
			PARMDEF_COMMA (pnc)
			transref_t tr,
			time_t *nowp,
			void *opt_evdata);		/* event data or NULL */
} trans_topo_t;
 
struct trans_st {
	tokenctr_t countdown;
	time_t firstfail;
	time_t notbefore;
};


/* A Petri net is a collection of places and transitions.  Wow, that's quite
 * a revelation :)
 *
 * Clever generators will probably use a minimal perfect hash function to
 * quickly find places and transitions by name; note that we do not support
 * perfect hash functions that are not minimal -- since there is no need.
 * We allow for some type insertions if the right #define is present.
 * Similarly, we leave some room for user-defined references.
 */

typedef struct petrinet_topo_st {
#ifndef PETRINET_WITHOUT_NAMES
	const char *name;
#endif
	placeref_t place_num;
	transref_t trans_num;
	place_topo_t *place_ary;	// Start accessing from [1]
	trans_topo_t *trans_ary;	// Start accessing from [1]
#	ifdef USRDEF_PETRINET_TOPO_FIELDS
	USRDEF_PETRINET_TOPO_FIELDS
#	endif
} petrinet_topo_t;

typedef struct petrinet_st {
	const char *colour;
#ifndef PETRINET_SINGLETONS
	const petrinet_topo_t *topology;
#else
	const petrinet_topo_t topology;
#endif
	place_t *place_ary;		// Start accessing from [1]
	trans_t *trans_ary;		// Start accessing from [1]
#ifndef PETRINET_WITHOUT_NAMES
#	ifdef PLACE_HASH_CTX_FIELDS
	PLACE_HASH_CTX_FIELDS
#	endif
#	ifdef TRANS_HASH_CTX_FIELDS
	TRANS_HASH_CTX_FIELDS
#	endif
#endif
#	ifdef USRDEF_PETRINET_FIELDS
	USRDEF_PETRINET_FIELDS
#	endif
} petrinet_t;


/* Given a place or transition name, find the corresponding entry in the
 * given Petri net.  Note that a clever generator will employ minimal
 * perfect hashing to find this in O(1) time; simpler implementations
 * might resort to linear search in O(n) time.  In general, you should
 * probably choose these functions over looping yourself.
 */

#ifndef PETRINET_WITHOUT_NAMES
placeref_t find_place (PARMDEF_COMMA (pnc) const char *name);
transref_t find_trans (PARMDEF_COMMA (pnc) const char *name);
#endif


/* Callback functions often provide access to a (void *) and that should hold
 * just the information to be usable.  We define a type that can be voided as
 * a representation of a transition or a place.
 *
 * The list versions incorporate a list of place/trans references with one
 * entry; just enough to store an empty list, but you can allocate more data
 * to add more references, as you please.
 */

typedef struct {
	petrinet_t network;
	placeref_t place;
} place_opaque_t;

typedef struct {
	petrinet_t network;
	transref_t trans;
} trans_opaque_t;

typedef struct {
	petrinet_t network;
	placeref_list_t placelist;
} placelist_opaque_t;

typedef struct {
	petrinet_t network;
	transref_list_t translist;
} translist_opaque_t;


#endif /* PERPETUUM_MODEL_H */

