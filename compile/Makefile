SHELL = /bin/sh
EXTRA  = -O3
FFLAGS =  $(EXTRA)
CFLAGS = -Ae $(EXTRA)
FC      = gfortran -std=legacy -ffixed-line-length-none -ffpe-summary=none $(FFLAGS)
CC      = cc $(CFLAGS)

COREPATH = ${HOME}/trading/src/core
STGYPATH = ${HOME}/trading/src/strats
APPPATH = ${HOME}/trading/app

COREOBJ = load_asset.o print_error.o ema.o print_env.o \
	 ret.o rebase.o indcross.o retcond.o logret.o rollma.o unaries.o \
	 unary.o binary.o

SRCOBJ = main.o 

%.o: $(COREPATH)/%.f
	$(FC) $(FFLAGS) -c $<

main.o: $(STGYPATH)/main.f
	$(FC) $(FFLAGS) -c $<

simul: $(COREOBJ) $(SRCOBJ) 
	 $(FC)  $(COREOBJ) $(SRCOBJ) -o $(APPPATH)/simul -lm

clean:
	rm -f *.o


