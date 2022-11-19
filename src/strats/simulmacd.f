      program simulmacd
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c simulmacd.f
c date 20220923
c author Luis de Juan
c version 0.1
c 
c    Full simulation of the macd to perform optimizations 
c inputs:
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 assetfilename /'../data/cac40_index.csv'/
      integer i,j,IMAX,JMAX,in,jm,IPERIODS, ipds,id
      integer jdate, ishort, fshort, dshort, ilong, flong, dlong
      integer nvars
      character*15 headers(12)
      integer ird, irdx, iwr, iwrx
      logical ier

      data headers /'id', 'date', 'short','long', 'spread_return', 'spread_vol',
     & 'spread_sharpe', 'spread_calmar', 'return', 'vol', 'sharpe', 'calmar' /

      include "macdgenerate.inc"

      integer schedule(IPERIODS,3)
      double precision excessrisks(IPERIODS,4), vars(2), stratrisks(IPERIODS,4)

      double precision env(1:IMAX, 1:JMAX)
      common /coutput/ ier,iwr,ird
      ird = irdx
      ier = .FALSE.
      iwr = iwrx

      call load_asset(assetfilename, in, jm, env, IMAX, JMAX)
      call logger('info', 'success loading environment')
      
      call gen_schedule('A', ipds, schedule, IPERIODS, env(jdate,1), in)
      call logger('debug', 'success generating schedule')


      open(ird, file='simul_macd_4.csv') 
      write(ird,*) (headers(i), i=1,12)
      id = 1
      do i=ishort, fshort, dshort
        do j=ilong,flong, dlong
          vars(1) = real(i,8)
          vars(2) = real(j,8)
          call macd2(excessrisks, stratrisks, vars, nvars, schedule, IPERIODS, ipds, env, IMAX, JMAX, in)
          call print_simul_risks(ird, id, vars, nvars, schedule, excessrisks, stratrisks, IPERIODS, ipds)
          id = id + 1
        end do
      end do

      close(ird)

      call logger('debug', 'success simulating all strategies')

      end program

      subroutine macd(excessrisks, stratrisks, vars, nvars, schedule, IPERIODS, ipds, env, IMAX, JMAX, in)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c macd.f
c date 20220923
c author Luis de Juan
c version 0.1
c 
c    one single simulation of the macd strategy 
c inputs:
c     vars: double precision(nvars) : variables of the strategy
c           vars(isht) : number of day of short 
c           vars(ilng) : number of days of long
c     nvars: integer    :  number of variables of the strategy             
c     schedule: integer(nperiods, 3) : yearly schedule for the risk calculations
c               schedule(i,1) : start index of the risk period
c               schedule(i,2) : en index of the risk period
c               schedule(i,3) : end date of the risk period as YYYYMMDD (int)
c     nperiods :  number of periods of the schedule
c     env      : double precision(imax,jmax,kmax) : flux environment of the index strategy
c outputs:
c     risks    : double precision(nperiods,4) : excess risks over the index
c     stratrisk:  double precision(nperiods,4): strategy risks 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer in, ipds, nvars, isht, ilng,i, nstart
      integer jopen, jhigh, jlow, jclose,jpdiv, jplog
      integer jstema, jlgema, jtslpema
      double precision alphast, alphalg 
      integer jret, jnav
      integer jcrs, jrst, jnavs
      integer IMAX, JMAX, IPERIODS

      include "macd.inc"

      double precision env(IMAX,JMAX)
      integer schedule(IPERIODS,3) 
      double precision indexrisks(IPERIODS,4), stratrisks(IPERIODS,4),excessrisks(IPERIODS,4) 
      double precision vars(nvars)

      intrinsic dlog

      double precision chg,slip
      data chg /1.e-5/
      data slip /0.0/

 
      nstart = int(vars(ilng))

      alphast = 2./(1. + vars(isht))
      alphalg = 2./(1. + vars(ilng))

      do i=1,in
        call unary(dlog,i,env(1,jplog), env(1,jclose))
        call ema(alphast, i, env(1,jstema), env(1, jplog))
        call ema(alphalg, i, env(1,jlgema), env(1, jplog))
        call ema(5.0D-2, i, env(1,jtslpema), env(1, jplog))
        call indmacd(i, env(1,jcrs), env(1,jlgema), env(1,jstema), env(1,jtslpema))

        call ret(i, env(1,jret), env(1, jpdiv))
        call rebase(nstart, i, env(1,jnav), env(1, jret))
        
        call retcond(chg, slip, i, env(1,jrst), env(1,jret), env(1,jcrs))
        call rebase(nstart, i, env(1, jnavs), env(1, jrst))
      enddo

      call schedulerisks(stratrisks, schedule, IPERIODS, ipds,env(1,jnavs),in)
      call schedulerisks(indexrisks, schedule, IPERIODS ,ipds,env(1,jnav),in)   
      call diffmatrix(excessrisks,stratrisks,indexrisks, IPERIODS,4, ipds,4)

      end subroutine
 
       subroutine macd2(excessrisks, stratrisks, vars, nvars, schedule, IPERIODS, ipds, env, IMAX, JMAX, in)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c macd.f
c date 20220923
c author Luis de Juan
c version 0.1
c 
c    one single simulation of the macd strategy ove the log((H+L)/2) 
c inputs:
c     vars: double precision(nvars) : variables of the strategy
c           vars(isht) : number of day of short 
c           vars(ilng) : number of days of long
c     nvars: integer    :  number of variables of the strategy             
c     schedule: integer(nperiods, 3) : yearly schedule for the risk calculations
c               schedule(i,1) : start index of the risk period
c               schedule(i,2) : en index of the risk period
c               schedule(i,3) : end date of the risk period as YYYYMMDD (int)
c     nperiods :  number of periods of the schedule
c     env      : double precision(imax,jmax,kmax) : flux environment of the index strategy
c outputs:
c     risks    : double precision(nperiods,4) : excess risks over the index
c     stratrisk:  double precision(nperiods,4): strategy risks 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer in, ipds, nvars, isht, ilng,i, nstart
      integer jopen, jhigh, jlow, jclose,jpdiv, jplog,jave
      integer jstema, jlgema, jtslpema
      integer jret, jnav
      integer jcrs, jrst, jnavs
      integer IMAX, JMAX, IPERIODS
      integer nbbarsst, nbbarslg

      include "macd2.inc"

      double precision env(IMAX,JMAX)
      integer schedule(IPERIODS,3) 
      double precision indexrisks(IPERIODS,4), stratrisks(IPERIODS,4),excessrisks(IPERIODS,4) 
      double precision vars(nvars), half
      external dhalfvalue
      intrinsic dlog

      double precision chg,slip
      data chg /1.e-5/
      data slip /0.0/

 
      nstart = int(vars(ilng))

      nbbarsst = int(vars(isht))
      nbbarslg = int(vars(ilng))

      do i=1,in
        call binary(dhalfvalue,i,env(1,jave), env(1,jhigh),env(1,jlow))
        call ma(nbbarsst, i, env(1,jstema), env(1, jave))
        call ma(nbbarslg, i, env(1,jlgema), env(1, jave))
        call ema(5.0D-2, i, env(1,jtslpema), env(1, jave))
        call indmacd(i, env(1,jcrs), env(1,jlgema), env(1,jstema), env(1,jtslpema))

        call ret(i, env(1,jret), env(1, jpdiv))
        call rebase(nstart, i, env(1,jnav), env(1, jret))
        
        call retcond(chg, slip, i, env(1,jrst), env(1,jret), env(1,jcrs))
        call rebase(nstart, i, env(1, jnavs), env(1, jrst))
      enddo

      call schedulerisks(stratrisks, schedule, IPERIODS, ipds,env(1,jnavs),in)
      call schedulerisks(indexrisks, schedule, IPERIODS ,ipds,env(1,jnav),in)   
      call diffmatrix(excessrisks,stratrisks,indexrisks, IPERIODS,4, ipds,4)

      end subroutine
 
