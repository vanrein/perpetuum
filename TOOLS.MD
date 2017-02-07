# Tools to use with Perpetuum

> *A plethora of Petri-net tools is available, at least in theory.  In
> practice, not all work well.  Usually, the subset of features implemented
> is a less-than-perfect match.*

The feature set required for Perpetuum is quite modest:

  * Standard Place/Transition Nets with only one extension: Inhibitor Arcs
  * We don't really use colour, though we can have many orthogonal instances
    of one Petri net
  * We read Petri nets in the standard
    [PNML notation](http://pnml.org)
  * We process Petri nets with
    [SNAKES](https://www.ibisc.univ-evry.fr/~fpommereau/SNAKES/API/index.html)


## Editors

We tried several, and found most were unpractical.  It seems like
[TAPAAL](http://www.tapaal.net)
is the most usable of all.  We do not use the time annotations.

  * [PNeditor](http://www.pneditor.org) does not actually enable the use of
    inhibitor arcs; its output is encoded in a non-standard manner
  * [Wolfgang](https://sourceforge.net/projects/wolfgang-editor/)
    works quite nicely, but lacks inhibitor nodes
  * [GreatSPN Editor](http://www.di.unito.it/~amparore/mc4cslta/editor.html)
    is really pleasant to work with, but it does not output
    [PNML](http://www.pnml.org) -- though they seemed open to adding it.
    GreatSPN is free for research purposes, and paid for commercial use.
    Its API delivers the best images, mostly thanks to snap-to-grid.
  * [CPNtools](http://cpntools.org) is only kept alive for educational
    purposes and some research at the University; it barely runs on anything
    but Windows; it is written in a language that is hardly known by anyone.

If you tried other tools that we should add, please let us know.  If you
developed tools to surpass what is reported, we would like to hear about it.


## Validation

Various tests are available:

  * LTL and CTL are timed logic languages, of which CTL is the most expressive.
    These can be used to test random predicates that range over the time
    progression of a system.
  * Standard tests of use include deadlock, livelock, boundedness.

We have not tried any tools yet.  If you tried tools that we should add,
please let us know.  If you developed tools to surpass what is reported, we
would like to hear about it.

