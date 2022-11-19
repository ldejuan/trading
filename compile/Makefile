SHELL = /bin/sh
EXTRA  = -O3
FFLAGS =  $(EXTRA)
CFLAGS = -Ae $(EXTRA)
FC      = gfortran -std=legacy -ffixed-line-length-none -ffpe-summary=none $(FFLAGS)
CC      = cc $(CFLAGS)


COREPATH = ${HOME}/strategies/trading/src/core
STGYPATH = ${HOME}/strategies/trading/src/strats
APPPATH = ${HOME}/strategies/trading/app
NRPATH = ${HOME}/strategies/trading/src/nr

COREOBJ = load_asset.o logger.o ema.o print_env.o \
	 ret.o rebase.o indcross.o retcond.o logret.o ma.o unaries.o \
	 unary.o binary.o detrend.o aliasing.o vol.o indchannelbreak.o \
	 movmax.o movmin.o indmacd.o risks.o binaries.o

NROBJ = gammln.o memcof.o evlmem.o avevar.o datetime.o


%.o: $(COREPATH)/%.f
	$(FC) $(FFLAGS) -c $<

%.o: $(NRPATH)/%.f
	$(FC) $(FFLAGS) -c $<

main.o: $(STGYPATH)/main.f $(STGYPATH)/simulation.inc
	$(FC) $(FFLAGS) -c $<

test.o: $(STGYPATH)/test.f
	$(FC) $(FFLAGS) -c $<

trend.o: $(STGYPATH)/trend.f $(STGYPATH)/trend.inc
	$(FC) $(FFLAGS) -c $<	

simulchannel.o: $(STGYPATH)/simulchannel.f $(STGYPATH)/simulchannel.inc
	$(FC) $(FFLAGS) -c $<

simulmacd.o: $(STGYPATH)/simulmacd.f $(STGYPATH)/simulmacd.inc
	$(FC) $(FFLAGS) -c $<

blockchannel.o: $(STGYPATH)/blockchannel.f $(STGYPATH)/blockchannel.inc
	$(FC) $(FFLAGS) -c $<

simul: $(COREOBJ) $(NROBJ) simul.o
	 $(FC)  $(COREOBJ)  $(NROBJ) simul.o -o $(APPPATH)/simul -lm

trend: $(COREOBJ) $(NROBJ) trend.o
	 $(FC)  $(COREOBJ)  $(NROBJ) trend.o -o $(APPPATH)/trend -lm

simulchannel: $(COREOBJ) $(NROBJ) simulchannel.o
	 $(FC)  $(COREOBJ)  $(NROBJ) simulchannel.o -o $(APPPATH)/simulchannel -lm

blockchannel: $(COREOBJ) $(NROBJ) blockchannel.o
	 $(FC)  $(COREOBJ)  $(NROBJ) blockchannel.o -o $(APPPATH)/blockchannel -lm

simulmacd: $(COREOBJ) $(NROBJ) simulmacd.o
	 $(FC)  $(COREOBJ)  $(NROBJ) simulmacd.o -o $(APPPATH)/simulmacd -lm

test: $(COREOBJ) $(NROBJ) test.o
	 $(FC)  $(COREOBJ)  $(NROBJ) test.o -o $(APPPATH)/test -lm

clean:
	rm -f *.o

debug: 
	@echo ${HOME}
