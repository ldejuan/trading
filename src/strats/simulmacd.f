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
      integer i,j,IMAX,JMAX,in,jm, ipds
      integer ishort, fshort, dshort, ilong, flong, dlong
      integer IPERIODS, nvars
      character*15 headers(11)
      integer ird, irdx, iwr, iwrx

      data headers /'date', 'short','long', 'spread_return', 'spread_vol',
     & 'spread_sharpe', 'spread_calmar', 'return', 'vol', 'sharpe', 'calmar' /

      include "macdgenerate.inc"
      include "macdallocation.inc"

      common /allocation/ IPERIODS, IMAX, JMAX

      integer schedule(IPERIODS,3)
      real env(1:IMAX,1:JMAX), risks(IPERIODS,4), vars(2), stratrisks(IPERIODS,4)
      character dates(1:IMAX)*10

      call load_asset(assetfilename,env, dates, in,jm)
      call gen_schedule('A', schedule, dates, in, ipds)


      open(irdx, file='simul_macd.csv')
      write(irdx,*) (headers(i), i=1,11)

      do i=ishort, fshort, dshort
        do j=ilong,flong, dlong
          vars(1) = real(i)
          vars(2) = real(j)
          call macd(risks, stratrisks, vars, nvars, schedule, iprds, env, in, jn)
          call print_simul_risks(irdx, vars, nvars, schedule, risks, stratrisks, iprds)
        end do
      end do

      close(irdx)

      end program

      subroutine macd(risks, stratrisks, vars, nvars, schedule, iprds, env, in, jn)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c macd.f
c date 20220923
c author Luis de Juan
c version 0.1
c 
c    one single simulation of the macd strategy 
c inputs:
c     vars: real(nvars) : variables of the strategy
c           vars(isht) : number of day of short 
c           vars(ilng) : number of days of long
c     nvars: integer    :  number of variables of the strategy             
c     schedule: integer(nperiods, 3) : yearly schedule for the risk calculations
c               schedule(i,1) : start index of the risk period
c               schedule(i,2) : en index of the risk period
c               schedule(i,3) : end date of the risk period as YYYYMMDD (int)
c     nperiods :  number of periods of the schedule
c     env      : real(imax,jmax,kmax) : flux environment of the index strategy
c outputs:
c     risks    : real(nperiods,4) : excess risks over the index
c     stratrisk:  real(nperiods,4): strategy risks 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer in, jm, iprds, nvars, isht, ilng,i, nstart
      integer jopen, jhigh, jlow, jclose,jpdiv, jplog
      integer jstema, jlgema, jtslpema
      real alphast, alphalg 
      integer jret, jnav
      integer jcrs, jrst, jnavs
      integer IMAX, JMAX, IPERIODS

      integer ird, irdx, iwr, iwrx, ier

      include "macd.inc"
      include "macdallocation.inc"

      integer schedule(IPERIODS,3)
      real indexrisk(IPERIODS,4), stratrisks(IPERIODS,4), env(IMAX,JMAX)
      real risks(IPERIODS,4), vars(nvars)
      intrinsic alog
      common /cfile/ ird
      common /cerror/ iwr,ier
      real chg,slip
      data chg /1.e-5/
      data slip /0.0/

      
      ird = irdx
      iwr = iwrx
 
      nstart = int(vars(ilng))

      alphast = 2./(1. + vars(isht))
      alphalg = 2./(1. + vars(ilng))

      do i=1,in
        call unary(alog,i,env(1,jplog), env(1,jclose))
        call ema(alphast, i, env(1,jstema), env(1, jplog))
        call ema(alphalg, i, env(1,jlgema), env(1, jplog))
        call ema(0.05, i, env(1,jtslpema), env(1, jplog))
        call indmacd(i, env(1,jcrs), env(1,jlgema), env(1,jstema), env(1,jtslpema))

        call ret(i, env(1,jret), env(1, jpdiv))
        call rebase(nstart, i, env(1,jnav), env(1, jret))
        
        call retcond(chg, slip, i, env(1,jrst), env(1,jret), env(1,jcrs))
        call rebase(nstart, i, env(1, jnavs), env(1, jrst))
        enddo
      enddo

      call schedulerisks(schedule, stratrisks,ipdrs,env(1,jnavs),in)
      call schedulerisks(schedule, indexrisk,ipdrs,env(1,jnav),in)
      call diffmatrix(risks,stratrisks,indexrisk,ipdrs,4)

      end subroutine
 
