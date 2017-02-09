# all: compilable development_net
# all: development_net
all: traffic_light

CFLAGS=-I . -I ../../include/ -ggdb3

TESTDEF1=
TESTDEF2=-DPETRINET_SINGLETONS
TESTDEF3=-DPETRINET_SINGLETONS -DPETRINET_GLOBAL_NAME=my1st

TESTSRC=development_net.c
TESTHDR=development_net.h

DEFINES=-DBUILD_SINGLE_THREADED -DPETRINET_SINGLETONS
SOURCES=testmain.c development_net.c ../../src/runtime.c ../../src/flatsched.c
HEADERS=development_net.h

TFICSRC=lightmain.c traffic_light.c ../../src/runtime.c ../../src/flatsched.c
TFICHDR=traffic_light.h

development_net.c: netin.pnml ../perpetuum-codegen.py
	../perpetuum-codegen.py ./netin.pnml

compilable: $(TESTSRC) $(TESTHDR) development_net.c
	gcc -Dconst= $(CFLAGS) $(TESTDEF1) -o "$@" -c "$<"
	gcc -Dconst= $(CFLAGS) $(TESTDEF2) -o "$@" -c "$<"
	gcc -Dconst= $(CFLAGS) $(TESTDEF3) -o "$@" -c "$<"

development_net: $(SOURCES) $(HEADERS) netin.pnml ../perpetuum-codegen.py
	../perpetuum-codegen.py netin.pnml
	gcc -Dconst= $(CFLAGS) $(DEFINES) -o "$@" $(SOURCES)

traffic_light.c: traffic_light.pnml ../perpetuum-codegen.py
	../perpetuum-codegen.py traffic_light.pnml

traffic_light: $(TFICSRC) $(TFICHDR)
	gcc -Dconst= $(CFLAGS) $(DEFINES) -o "$@" $(TFICSRC)

