include ../make.inc

# INFO: Turn on useful debug printouts
#override CFLAGS+= -DDEBUG_MODE

LIB_VER = 1.1.0

#Name of libraries to create
LIB_TAU_SPINNER_SO = libTauolaTauSpinner.so
LIB_TAU_SPINNER_A  = libTauolaTauSpinner.a

TAU_SPINNER_OBJECTS = src/tau_reweight_lib.o src/nonSM.o src/disth.o
TAU_SPINNER_EXAMPLES_OBJECTS = src/read_particles_from_TAUOLA.o

all:
ifeq ($(MAKE_TAU_SPINNER),yes)
	@make libTauSpinner
	@echo "Consider: make move "
else
	@make error_message
endif

move:
	cp -rf include/TauSpinner ../include
	cp -f lib/$(LIB_TAU_SPINNER_A) ../lib
	cp -f lib/$(LIB_TAU_SPINNER_SO).$(LIB_VER) ../lib
	ln -sf $(LIB_TAU_SPINNER_SO).$(LIB_VER) ../lib/$(LIB_TAU_SPINNER_SO)
	@echo "##################################################################"	
	@echo "       TauSpinner library created and moved to lib/ directory     "
	@echo "##################################################################"

%.o: %.cxx
	$(CC) $(CFLAGS) -c $< -o $@ -Iinclude \
	-I$(PREFIX)/include                   \
	-I$(LHAPDFLOCATION)/include

%.o: %.f
	$(F77) $(F77FLAGS) -c $< -o $@

libTauSpinner: $(TAU_SPINNER_OBJECTS)
	ar cr lib/$(LIB_TAU_SPINNER_A) $(TAU_SPINNER_OBJECTS)
	$(LD) $(LDFLAGS) $(SOFLAGS) $(TAU_SPINNER_OBJECTS) -o lib/$(LIB_TAU_SPINNER_SO).$(LIB_VER)
	ln -sf $(LIB_TAU_SPINNER_SO).$(LIB_VER) lib/$(LIB_TAU_SPINNER_SO)

clean:
	rm -f src/*.o lib/*

../make.inc:
error_message:
	@echo ""
	@echo "Before compiling TauSpinner, in main TAUOLA directory execute:"
	@echo "./configure --with-tau-spinner --with-lhapdf=<PATH>"
	@echo "(remember to include --with-hepmc=<PATH> or --without-hepmc)"
	@echo ""
	@false
