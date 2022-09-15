SHELL = /bin/sh
EXTRA  = -O3
FFLAGS =  $(EXTRA)
CFLAGS = -Ae $(EXTRA)
FC      = gfortran -std=legacy -ffixed-line-length-none -ffpe-summary=none $(FFLAGS)
CC      = cc $(CFLAGS)

COREPATH = ${HOME}/trading/src/core
STGYPATH = ${HOME}/trading/src/strats
APPPATH = ${HOME}/trading/app
NRPATH = ${HOME}/trading/src/nr

COREOBJ = load_asset.o print_error.o ema.o print_env.o \
	 ret.o rebase.o indcross.o retcond.o logret.o rollma.o unaries.o \
	 unary.o binary.o

NROBJ = gammln.o memcof.o evlmem.o

SRCOBJ = main.o 

%.o: $(COREPATH)/%.f
	$(FC) $(FFLAGS) -c $<

%.o: $(NRPATH)/%.f
	$(FC) $(FFLAGS) -c $<

main.o: $(STGYPATH)/main.f $(STGYPATH)/memory.inc
	$(FC) $(FFLAGS) -c $<

trend.o: $(STGYPATH)/trend.f $(STGYPATH)/trend.inc
	$(FC) $(FFLAGS) -c $<	

simul: $(COREOBJ) $(SRCOBJ) $(NROBJ)
	 $(FC)  $(COREOBJ)  $(NROBJ) $(SRCOBJ) -o $(APPPATH)/simul -lm

trend: $(COREOBJ) trend.o $(NROBJ)
	 $(FC)  $(COREOBJ)  $(NROBJ) trend.o -o $(APPPATH)/trend -lm

clean:
	rm -f *.o


