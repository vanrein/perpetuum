#!/usr/bin/env python
#
# perpetuum-codegen.py -- Given a Petri net, generate driver code for it.
#
# This is a code generator from a set of Petri nets with suitable labels.
# The output consists of:
#  - tables of places and transitions, modelled after <perpetuum/model.h>
#  - uses a minimum perfect hash to offer searches by key
#  - TODO: invocations of actions as declared by transition labels
#  - TODO: processing of events as declared by transition labels
#  - TODO: starting/ending of activities as declared by place labels
#  - TODO: processing of activity output as declared by place labels
#
# From: Rick van Rein <rick@openfortress.nl>


import os
import sys
import random

from snakes.nets import PetriNet, Place, Transition, StateGraph, Variable
import snakes.plugins

import cmph


#TODO#
#TODO# Start of Petri net generation for development purposes, semi-random
#TODO#

prng = random.Random ()
prng.seed (1234567890)
one_in_ = 10
place_num = 50
trans_num = 40
place_out = 'place%02d'
trans_out = 'trans%02d'

net = PetriNet ('development_net')
for p in range (place_num):
	net.add_place (Place (place_out % p))
for t in range (trans_num):
	net.add_transition (Transition (trans_out % t))
for p in range (place_num):
	for t in range (trans_num):
		if prng.uniform (0, one_in_) < 1:
			net.add_input  (place_out % p, trans_out % t, Variable('TODO'))
		if prng.uniform (0, one_in_) < 1:
			net.add_output (place_out % p, trans_out % t, Variable('TODO'))


#TODO# End of Petri net generation for development purposes, semi-random


#TODO# Combine Petri nets based on matching labels of places and transitions

#
# Map network name to file name and structure name
#
neat_net_name = net.name.replace (' ', '_').replace ('-', '_')  #TODO#  Escape/subst etc
net_name = 'petrinet_' + neat_net_name


#
# Output a GraphViz .dot file with the graph format
#
snakes.plugins.load ('gv', 'snakes.nets', 'nets')
#TODO:NOTFOUND# net.draw (',' + neat_net_name + '.png')
s = StateGraph (net)
s.build ()
#TODO:NOTFOUND# s.draw (',' + neat_net_name + '.png')

#
# Map places and transitions each to their own indices
#

places = [ p.name for p in net.place      () ]
transs = [ t.name for t in net.transition () ]

place_mph = cmph.generate_hash (places, algorithm='bdz')
trans_mph = cmph.generate_hash (transs, algorithm='bdz')

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

#DEBUG# print 'Places:', place_idx
#DEBUG# print 'Transitions:', trans_idx

#TODO# rp,wp = os.pipe ()	#TODO# Limited pipe buffer
#TODO# place_mph.save (rp)
#TODO# place_hashtable = wp.read (65537)
#TODO# 
#TODO# rp,wp = os.pipe ()	#TODO# Limited pipe buffer
#TODO# trans_mph.save (rp)
#TODO# trans_hashtable = wp.read (65537)


#
# Construct tables
#

cout = open ('demo/' + neat_net_name + '.c', 'w')
hout = open ('demo/' + neat_net_name + '.h', 'w')

# Generate headers for the .h and .c files
hout.write ('/* ' + neat_net_name + '''.h
 *
 * This is a generated file.  Do not edit it, but rather its source and
 * run perpetuum to produce a new version of this file.
 *
 * Please file issues with https://github.com/vanrein/perpetuum/issues
 *
 * With compliments from the ARPA2.net / InternetWide.org project!
 */


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


''')


# Generate the lists of places and transitions from each of their neighbours
def genlist (kind, dict, name, reflist):
	cout.write ('static const ' + kind + 'ref_t ' + name + ' [] = { ' + str (len (reflist)))
	for ref in reflist:
		i = 1 + dict [ref]
		cout.write (', ' + str (i))
	cout.write (' };\n')
p2t = [ ]
t2p = [ ]
for t in trans_list:
	for p in net.transition (t).input ():
		p2t.append ( (p[0].name,t) )
	for p in net.transition (t).output ():
		t2p.append ( (t,p[0].name) )
for p in place_list:
	genlist ('trans', trans_idx, p + '_trans_out',     [ t for t in trans_list if (p,t) in p2t ] )
	genlist ('trans', trans_idx, p + '_trans_out_inh', []) #TODO#
	cout.write ('\n')
for t in trans_list:
	genlist ('place', place_idx, t + '_place_in',      [ p for p in place_list if (p,t) in p2t ] )
	#NONEED# genlist ('place', place_idx, t + '_place_in_inh',  []) #TODO#
	genlist ('place', place_idx, t + '_place_out',     [ p for p in place_list if (t,p) in t2p ] )
	cout.write ('\n')

# Generate the place_t[] based on the array of each place's inputs and outputs
cout.write ('static const place_t ' + neat_net_name + '_places [] = {\n')
for p in place_list:
	cout.write ('\t{ "' + p + '", ')
	cout.write (p + '_trans_out, ')
	cout.write (p + '_trans_out_inh },\n')
cout.write ('};\n\n')

#TODO# Demo only, print transition name as a default action
cout.write ('''/* TODO: Demo mode only, this standard action prints the transition name */

#include <stdio.h>

trans_retcode_t TODO_action_print_trans (time_t *nowp,
				transref_t tr,
				trans_t *tt,
				trans_colour_t *tc) {
	printf ("Firing %s -- now=%ld, notbefore=%ld, firstfail=%ld\\n",
			tt->name,
			(long) *nowp,
			(long) tc->notbefore,
			(long) tc->firstfail);
	return TRANS_SUCCESS;
}

''')

# Generate the trans_t[] based on the array of each place's inputs and outputs
cout.write ('static const trans_t ' + neat_net_name + '_transitions [] = {\n')
for t in trans_list:
	cout.write ('\t{ "' + t + '", ')
	cout.write (t + '_place_in, ')
	cout.write ('/*NONEED* ' + t + '_place_in_inh, */ ')
	cout.write (t + '_place_out, ')
	cout.write ('TODO_action_print_trans, ')
	cout.write ('},\n')
cout.write ('};\n\n')

# Generate init vectors for places, and optional singleton array
hout.write ('/* Place initialisation TODO:without tokens */\n')
for plc in place_list:
	hout.write ('#define PLACE_INIT_' + plc + ' { 0 }\n')
hout.write ('\n')
cout.write ('#ifdef PETRINET_SINGLETONS\n')
cout.write ('static place_colour_t the_' + neat_net_name + '_places [] = {\n')
for plc in place_list:
	cout.write ('\tPLACE_INIT_' + plc + ',\n')
cout.write ('};\n')
cout.write ('#endif\n\n')

# Generate init vectors for transitions, and optional singleton array
hout.write ('/* Place initialisation with countdown; set to inputs + non-empty inhibitors*/\n')
for tr in trans_list:
	ini_countdown = len (net.transition (tr).input ()) #TODO:INH#
	hout.write ('#define TRANS_INIT_' + tr + ' { ' + str (ini_countdown) + ', 0, 0 }\n')
hout.write ('\n')
cout.write ('#ifdef PETRINET_SINGLETONS\n')
cout.write ('static trans_colour_t the_' + neat_net_name + '_transitions [] = {\n')
for tr in trans_list:
	cout.write ('\tTRANS_INIT_' + tr + ',\n')
cout.write ('};\n')
cout.write ('#endif\n\n')

# Generate optional code for global variables, which simplifies embedded code
#TODO# Gen topology, gen transitions, gen places
hout.write ('#ifdef PETRINET_SINGLETONS\n')
hout.write ('extern petrinet_colour_t the_' + neat_net_name + ';\n')
hout.write ('#endif\n\n')
cout.write ('#ifdef PETRINET_SINGLETONS\n')
cout.write ('petrinet_colour_t the_' + neat_net_name + ' = {\n')
cout.write ('\t\"the_' + neat_net_name + '",\n')
cout.write ('\t{ /* PETRINET_SINGLETONS => inlined topology */\n')
cout.write ('\t\t\"' + neat_net_name + '\",\n')
cout.write ('\t\t' + str (len (place_list)) + ',\n')
cout.write ('\t\t' + str (len (trans_list)) + ',\n')
cout.write ('\t\t&' + neat_net_name + '_places [-1],\n')
cout.write ('\t\t&' + neat_net_name + '_transitions [-1],\n')
cout.write ('\t\t/* TODO: Support for inital USRDEF_PETRINET_FIELDS */\n')
cout.write ('\t},\n')
cout.write ('\t&the_' + neat_net_name + '_places [-1],\n')
cout.write ('\t&the_' + neat_net_name + '_transitions [-1],\n')
cout.write ('\t/* TODO: Support for initial PLACE_HASH_CTX_FIELDS */\n')
cout.write ('\t/* TODO: Support for initial TRANS_HASH_CTX_FIELDS */\n')
cout.write ('\t/* TODO: Support for initial USRDEF_PETRINET_COLOUR_FIELDS */\n')
cout.write ('};\n')
cout.write ('#endif\n\n')

# Generate the petrinet_t with the static structure, based on the arrays
hout.write ('#ifndef PETRINET_SINGLETONS\n')
hout.write ('extern const petrinet_t ' + neat_net_name + ';\n')
hout.write ('#else\n')
hout.write ('#ifdef PETRINET_CODED_FOR_ONE\n')
hout.write ('#define ' + neat_net_name + ' (&the_' + neat_net_name +  '.topology)\n')
hout.write ('#else\n')
hout.write ('#define ' + neat_net_name + ' (&the_' + neat_net_name + '->topology)\n')
hout.write ('#endif\n')
hout.write ('#endif\n\n')
cout.write ('''#ifndef PETRINET_SINGLETONS
const petrinet_t ''' + neat_net_name + ''' = {
	"''' + net.name + '''",
	''' + str (place_num) + ', ' + str (trans_num) + ''',
	&''' + neat_net_name + '''_places [-1], &''' + neat_net_name + '''_transitions [-1]
};
#endif

''')

# Print an end remark
hout.write ('\n\n/* End of generated file ' + neat_net_name + '.h */\n')
cout.write ('\n\n/* End of generated file ' + neat_net_name + '.c */\n')

#TODO-TMP# print 'Net ::', dir (net)
#TODO-TMP# print 'Transition ::', dir (net.transition (trans_list [0]))
#TODO-TMP# print 'Transition Input::', net.transition (trans_list [0]).input ()
#TODO-TMP# print 'Transition Output::', net.transition (trans_list [0]).output ()
#TODO-TMP# print 'Place ::', dir (net.place (place_list [0]))
