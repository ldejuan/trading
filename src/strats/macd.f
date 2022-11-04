     subroutine macd(risks, stratrisks, vars, nvars, schedule, IPERIODS, iprds, env, IMAX, JMAX, in)
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
      integer in, iprds, nvars, isht, ilng,i, nstart
      integer jopen, jhigh, jlow, jclose,jpdiv, jplog
      integer jstema, jlgema, jtslpema
      double precision alphast, alphalg 
      integer jret, jnav
      integer jcrs, jrst, jnavs
      integer IMAX, JMAX, IPERIODS

      include "macd.inc"

      double precision env(IMAX,JMAX)
      integer schedule(IPERIODS,3) 
      double precision indexrisks(IPERIODS,4), stratrisks(IPERIODS,4),risks(IPERIODS,4) 
      double precision vars(nvars)

      intrinsic alog

      double precision chg,slip
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

      call schedulerisks(stratrisks, schedule, IPERIODS, ipdrs,env(1,jnavs),in)
      call schedulerisks(indexrisks, schedule, IPERIODS ,ipdrs,env(1,jnav),in)
      call diffmatrix(risks,stratrisks,indexrisks, IPERIODS, ipdrs,4)

      end subroutine
 