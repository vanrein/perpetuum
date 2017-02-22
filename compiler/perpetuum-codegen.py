#!/usr/bin/env python
#
# perpetuum-codegen.py -- Given a Petri net, generate driver code for it.
#
# This is a code generator from a set of Petri nets with suitable labels.
# The output consists of:
#  - gen tables of places and transitions, modelled after <perpetuum/model.h>
#  - use a minimum perfect hash to offer searches by key
#  - gen initialisation and, for singletons, instance information w/ marking
#  - setup initial countdown for places (TODO: incorporate inhibiting arcs)
#  - process multiplicity inscriptions on arcs by generating sequences of arcs
#  - invocations of actions as declared by transition labels
#  - processing of events as declared by transition labels
#  - dropped: starting/ending of activities as declared by place labels
#  - dropped: processing of activity output as declared by place labels
#
# From: Rick van Rein <rick@openfortress.nl>


import os
import sys
import string
import random

import pntools.petrinet
import cmph


#
# Load the Petri net to work on
#

if len (sys.argv) != 2:
	sys.stderr.write ('Usage: %s infile.pnml...\n' % sys.argv [0])
	sys.stderr.write ('TODO: We currently process only one PNML file at a time\n')
	sys.exit (1)

netin_fn  = os.path.splitext (sys.argv [1]) [0] + os.extsep + 'pnml'
netout_fn = os.path.splitext (sys.argv [1]) [0] + os.extsep + 'out'
net = pntools.petrinet.parse_pnml_file (netin_fn)
if len (net) != 1:
	sys.stderr.write ('Parsing failed, found %d Petri nets instead of 1\n' % len (net))
	sys.exit (1)
net = net [0]  #TODO# Some day, support multiple Petri nets
pntools.petrinet.write_pnml_file (net, netout_fn)
place_num = len (net.places     )
trans_num = len (net.transitions)
print 'Network name is', net.name
print 'Number of places is', place_num
print 'Number of transitions is', trans_num

#TODO# Combine Petri nets [0..] with matching labels of places and transitions


#
# Map network name to file name and structure name; produce file names
#
neat_net_name = ''.join ( [ c
			for c in net.name.replace ('-', '_').replace (' ', '_')
			if c in  '_' + string.ascii_letters + string.digits
			] )
if not neat_net_name [:1] in string.ascii_letters:
	neat_net_name = 'x' + neat_net_name
if neat_net_name == '':
	neat_net_name = 'perpetuum'
outdir = os.path.dirname (sys.argv [0]) + os.sep + 'demo' + os.sep
c_fn    = outdir + neat_net_name + '.c'
h_fn    = outdir + neat_net_name + '.h'
pkey_fn = outdir + neat_net_name + '.pkey'
tkey_fn = outdir + neat_net_name + '.tkey'
pidx_fn = outdir + neat_net_name + '.pidx'
tidx_fn = outdir + neat_net_name + '.tidx'
ptyp_fn = outdir + 'petritypes.h'
# dot_fn  = outdir + neat_net_name + '.dot'
# png_fn  = outdir + neat_net_name + '.dot'
pnml_fn = outdir + neat_net_name + '.pnml'

#
# Write out the PNML file -- the composed result of the set of Petri nets
#
#TODO:COMPOSED#pnml = snakes.pnml.dumps (net)
#TODO:COMPOSED#print 'Produced PNML, size', len (pnml)
#TODO:COMPOSED#print 'Writing PNML to', pnml_fn
#TODO:COMPOSED#pout = open (pnml_fn, 'w')
#TODO:COMPOSED#pout.write (pnml)
#TODO:COMPOSED#pout.close ()
#TODO:COMPOSED#print 'Written', pnml_fn


#
# Map places and transitions each to their own indices
#

places = net.places     .keys ()
transs = net.transitions.keys ()

range2type = [
	( 1<< 8, 'uint8_t'  ),
	( 1<<16, 'uint16_t' ),
	( 1<<32, 'uint32_t' ),
	( 1<<64, 'uint64_t' )
]

placeref_t = None
for (r,t) in range2type:
	if len (places) < r:
		placeref_t = t
		break

transref_t = None
for (r,t) in range2type:
	if len (transs) < r:
		transref_t = t
		break

if placeref_t is None or transref_t is None:
	sys.stderr.write ('FATAL: Unable to fit places and transitions into 64 bit integers\n')
	sys.exit (1)


place_mph = cmph.generate_hash (places, algorithm='bdz')
trans_mph = cmph.generate_hash (transs, algorithm='bdz')

place_mph.save (pidx_fn)
trans_mph.save (tidx_fn)

place_idx = { }
trans_idx = { }
place_list = [''] * place_num
trans_list = [''] * trans_num

for p in places:
	place_idx [p] = place_mph.lookup (p)
	place_list [place_idx [p]] = p
for t in transs:
	trans_idx [t] = trans_mph.lookup (t)
	trans_list [trans_idx [t]] = t

open (pkey_fn, 'w').write ('\n'.join (place_list) + '\n')
open (tkey_fn, 'w').write ('\n'.join (trans_list) + '\n')

#DEBUG# print 'Places:', place_idx
#DEBUG# print 'Transitions:', trans_idx

#UNUSED# rp,wp = os.pipe ()	#PROBLEM# Limited pipe buffer
#UNUSED# place_mph.save (rp)
#UNUSED# place_hashtable = wp.read (65537)
#UNUSED# ...generate code for insertion...
#UNUSED# 
#UNUSED# rp,wp = os.pipe ()	#PROBLEM# Limited pipe buffer
#UNUSED# trans_mph.save (rp)
#UNUSED# trans_hashtable = wp.read (65537)
#UNUSED# ...generate code for insertion...


#
# Construct tables
#

# Generate typedefs for transref_t and placeref_t
tout = open (ptyp_fn, 'w')
tout.write ('''/* Petri Net storage types for ''' + neat_net_name + ''' */

typedef ''' + transref_t + ''' transref_t;
typedef ''' + placeref_t + ''' placeref_t;

''')
tout.close ()

# Generate headers for the .h and .c files
cout = open (c_fn, 'w')
hout = open (h_fn, 'w')
hout.write ('/* ' + neat_net_name + '''.h
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


''')


cout.write ('/* ' + neat_net_name + '''.c
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


#include "''' + neat_net_name + '''.h"


#ifdef PETRINET_WITHOUT_NAMES
#define NAME_COMMA(x)
#else
#define NAME_COMMA(x) x,
#endif


''')


# Generate the lists of places and transitions from each of their neighbours
def genlist (kind, dict, name, reflist):
	cout.write ('static const ' + kind + 'ref_t ' + name + ' [] = { ' + str (len (reflist)))
	for ref in reflist:
		i = 1 + dict [ref]
		cout.write (', ' + str (i))
	cout.write (' };\n')
p2t = [ (e.source,e.target) for e in net.edges if net.places.has_key (e.source) and e.type != 'inhibitor' for multi in range (int (e.inscription)) ]
p2i = [ (e.source,e.target) for e in net.edges if net.places.has_key (e.source) and e.type == 'inhibitor' ]
t2p = [ (e.source,e.target) for e in net.edges if net.places.has_key (e.target) for multi in range (int (e.inscription)) ]
for p in place_list:
	genlist ('trans', trans_idx, p + '_trans_out',     [ t for (p2,t) in p2t if p==p2 ] )
	genlist ('trans', trans_idx, p + '_trans_out_inh', [ t for t in trans_list if (p,t) in p2i ] )
	cout.write ('\n')
for t in trans_list:
	genlist ('place', place_idx, t + '_place_in',      [ p for (p,t2) in p2t if t==t2 ] )
	genlist ('place', place_idx, t + '_place_out',     [ p for (t2,p) in t2p if t==t2 ] )
	cout.write ('\n')

# Generate the place_topo_t[] from the array of each place's inputs and outputs
cout.write ('static const place_topo_t ' + neat_net_name + '_places [] = {\n')
for p in place_list:
	cout.write ('\t{ NAME_COMMA ("' + p + '") ')
	cout.write (p + '_trans_out, ')
	cout.write (p + '_trans_out_inh },\n')
cout.write ('};\n\n')


# Generate function prototypes for all the transitions' actions

hout.write ('/* Function prototypes for transition actions */\n')
for t in trans_list:
	hout.write ('trans_retcode_t trans_action_' + t + ' (PARMDEF_COMMA (pnc) transref_t tr, time_t *nowp, void *opt_evdata);\n')
hout.write ('\n')

# Generate the trans_topo_t[] from the array of each place's inputs and outputs
cout.write ('static const trans_topo_t ' + neat_net_name + '_transitions [] = {\n')
for t in trans_list:
	cout.write ('\t{ NAME_COMMA ("' + t + '") ')
	cout.write (t + '_place_in, ')
	cout.write (t + '_place_out, ')
	cout.write ('trans_action_' + t + ' ')
	cout.write ('},\n')
cout.write ('};\n\n')

# Generate init vectors for places, and optional singleton array
hout.write ('/* Place initialisation */\n')
for plc in place_list:
	ini_mark = net.places [plc].marking
	hout.write ('#define PLACE_INIT_' + plc + ' { ' + str (ini_mark) + ' }\n')
hout.write ('\n')
cout.write ('#ifdef PETRINET_SINGLETONS\n')
cout.write ('static place_t the_' + neat_net_name + '_places [] = {\n')
for plc in place_list:
	cout.write ('\tPLACE_INIT_' + plc + ',\n')
cout.write ('};\n')
cout.write ('#endif\n\n')

# Generate init vectors for transitions, and optional singleton array
hout.write ('/* Place initialisation; countdown := empty inputs + non-empty inhibitors */\n')
for tr in trans_list:
	# initial "countdown" is zero normal plus non-zero inhibitors trans
	ini_countdown = ( str (len ( [ src for (src,tgt) in p2t
	                               if tgt == tr
	                               and net.places [src].marking == 0 ] ) )
	                + ' + '
			+ str (len ( [ src for (src,tgt) in p2i
	                               if tgt == tr
	                               and net.places [src].marking >  0 ] ) ) )
	hout.write ('#define TRANS_INIT_' + tr + ' { ' + str (ini_countdown) + ', 0, 0 }\n')
hout.write ('\n')
cout.write ('#ifdef PETRINET_SINGLETONS\n')
cout.write ('static trans_t the_' + neat_net_name + '_transitions [] = {\n')
for tr in trans_list:
	cout.write ('\tTRANS_INIT_' + tr + ',\n')
cout.write ('};\n')
cout.write ('#endif\n\n')

# Generate optional code for global variables, which simplifies embedded code
#TODO# Gen topology, gen transitions, gen places
hout.write ('#ifdef PETRINET_SINGLETONS\n')
hout.write ('#ifdef PETRINET_GLOBAL_NAME\n')
hout.write ('extern petrinet_t PETRINET_GLOBAL_NAME;\n')
hout.write ('#else\n')
hout.write ('extern petrinet_t the_' + neat_net_name + ';\n')
hout.write ('#endif\n')
hout.write ('#endif\n\n')
cout.write ('''#ifdef PETRINET_SINGLETONS
#ifdef PETRINET_GLOBAL_NAME
petrinet_t PETRINET_GLOBAL_NAME = {
#else
petrinet_t the_''' + neat_net_name + ''' = {
#endif
	NAME_COMMA (.colour = "the_''' + neat_net_name + '''")
	.topology = {
		/* Topology is inlined due to PETRINET_SINGLETONS */
		NAME_COMMA (.name = "''' + neat_net_name + '''")
		.place_num = ''' + str (place_num) + ''',
		.trans_num = ''' + str (trans_num) + ''',
		.place_ary = &''' + neat_net_name + '''_places [-1],
		.trans_ary = &''' + neat_net_name + '''_transitions [-1],
		/* TODO: Support for inital USRDEF_PETRINET_FIELDS */
	},
	.place_ary = &the_''' + neat_net_name + '''_places [-1],
	.trans_ary = &the_''' + neat_net_name + '''_transitions [-1],
	/* TODO: Support for initial PLACE_HASH_CTX_FIELDS */
	/* TODO: Support for initial TRANS_HASH_CTX_FIELDS */
	/* TODO: Support for initial USRDEF_PETRINET_COLOUR_FIELDS */
#ifndef PETRINET_GLOBAL_NAME
};
#else
};
#endif
#endif

''')

# Generate the petrinet_topo_t with the static structure, based on the arrays
hout.write ('''#ifndef PETRINET_SINGLETONS
extern const petrinet_topo_t ''' + neat_net_name + ''';
#else
#ifdef PETRINET_GLOBAL_NAME
#define ''' + neat_net_name + ''' (&PETRINET_GLOBAL_NAME.topology)
#else
#define ''' + neat_net_name + ''' (&the_''' + neat_net_name + '''->topology)
#endif
#endif

''')

cout.write ('''#ifndef PETRINET_SINGLETONS
const petrinet_topo_t ''' + neat_net_name + ''' = {
	NAME_COMMA (.name = "''' + net.name + '''")
	.place_num = ''' + str (place_num) + ''',
	.trans_num = ''' + str (trans_num) + ''',
	.place_ary = &''' + neat_net_name + '''_places [-1],
	.trans_ary = &''' + neat_net_name + '''_transitions [-1],
};
#endif

''')

# Print an end remark
hout.write ('\n\n/* End of generated file ' + neat_net_name + '.h */\n')
cout.write ('\n\n/* End of generated file ' + neat_net_name + '.c */\n')

# Close output files
hout.close ()
cout.close ()
