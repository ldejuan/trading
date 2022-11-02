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
          call macd(risks, stratrisks, vars, nvars, schedule, nperiods, env, imax, jmax, kmax)
          call print_simul_risks(irdx, vars, nvars, schedule, risks, stratrisks, nperiods)
        end do
      end do

      close(irdx)

      end program

      subroutine macd(risks, stratrisks, vars, nvars, schedule, nperiods, env, imax, jmax, kmax)
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
      integer imax, jmax, kmax, nperiods, nvars, isht, ilng,k,i, nstart
      integer jopen, jhigh, jlow, jclose,jpdiv, jplog
      integer jstema, jlgema, jtslpema
      real alphast, alphalg 
      integer jret, jnav
      integer jcrs, jrst, jnavs

      integer ird, irdx, iwr, iwrx, ier

      include "macd.inc"
      integer schedule(nperiods,3)
      real indexrisk(nperiods,4), stratrisks(nperiods,4), env(imax,jmax,kmax)
      real risks(nperiods,4), vars(nvars)
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

      do i=1,imax
        do k=1,kmax
          call unary(alog,i,env(1,jplog,k), env(1,jclose,k))
          call ema(alphast, i, env(1,jstema,k), env(1, jplog, k))
          call ema(alphalg, i, env(1,jlgema,k), env(1, jplog, k))
          call ema(0.05, i, env(1,jtslpema,k), env(1, jplog, k))
          call indmacd(i, env(1,jcrs,k), env(1,jlgema,k), env(1,jstema,k), env(1,jtslpema,k))

          call ret(i, env(1,jret,k), env(1, jpdiv,k))
          call rebase(nstart, i, env(1,jnav,k), env(1, jret,k))
          
          call retcond(chg, slip, i, env(1,jrst,k), env(1,jret, k), env(1,jcrs,k))
          call rebase(nstart, i, env(1, jnavs,k), env(1, jrst, k))
        enddo
      enddo

      call schedulerisks(schedule, stratrisks,nperiods,env(1,jnavs,1),imax)
      call schedulerisks(schedule, indexrisk,nperiods,env(1,jnav,1),imax)
      call diffmatrix(risks,stratrisks,indexrisk,nperiods,4)

      end subroutine
 
