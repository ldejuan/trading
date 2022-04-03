SHELL = /bin/sh
EXTRA  = -O3
FFLAGS =  $(EXTRA)
CFLAGS = -Ae $(EXTRA)
FC      = gfortran -std=legacy -ffixed-line-length-none -ffpe-summary=none $(FFLAGS)
CC      = cc $(CFLAGS)

OBJECTS0 = load_asset.o print_error.o ema.o print_env.o \
	 ret.o rebase.o indcross.o retcond.o logret.o rollma.o unaries.o \
	 unary.o binary.o

OBJECTS1 = main.o 

simul: $(OBJECTS0) $(OBJECTS1) 
	 $(FC)  $(OBJECTS1) $(OBJECTS0) -o simul -lm

clean:
	rm -f *.o


