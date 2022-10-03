      program simulmacd
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c simulchannel.f
c date 20220923
c author Luis de Juan
c version 0.1
c 
c  File entry  point for a simulation of the a macd 
c  The common structure to env structure 
c  simulchannel.inc for parameters
c  assetfilename : name of the file to open
c  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 assetfilename /'../data/cac40_index.csv'/
      integer i,k, ird, iwr, iwrx, irdx, nn, mm, kk, imax, jmax, kmax,
     & nmax, jheaderx
      integer jopen, jhigh, jlow, jclose,jpdiv

      integer jstema, jlgema, jtslpema
      integer nshort, nlong
      real alphast, alphalg 

      integer jret, jnav
      integer jcrs, jrst, jnavs 
      integer nst,nlg,nyr, nbdays, nbyears, startday

      real chg,slip
      data chg /1.e-5/
      data slip /0.0/
      data nbdays /252/
      data startday /208/
      logical ier
      include "simulmacd.inc"

c
c    define headers
c      
      character*15 headers(jheaderx)
      data headers /
     & 'date',
     & 'open',
     & 'high',
     & 'low',
     & 'close',
     & 'divclose',
     & 'ema_short',
     & 'ema_long',
     & 'slope',
     & 'ret_underling',
     & 'nav_underling',
     & 'position',
     & 'ret_strategy',
     & 'nav_strategy'/
c
c env : global environnement with all the data 
c     env(i,j,k): i: bars, j: proprietes, k: asset
c
      
      real env(1:imax,1:jmax,1:kmax)
      character dates(1:imax,1:kmax)*10
      real indexrisk(1:nbyears,4), stratrisk(1:nbyears,4), excesrisk(1:nbyears,4)
      integer sched(1:nbyears, 3), nsched
      common /cfile/ird
      common /cerror/iwr,ier
      ird = irdx
      iwr = iwrx
c
c
c

      call load_asset(assetfilename,env, dates, nn,mm,kk, kmax)
c
c start of the simulation 
c
      alphast = 2./(1. + nshort)
      alphalg = 2./(1. + nlong)

      do i=1,imax
        do k=1,kmax
          call ema(alphast, i, env(1,jstema,k), env(1, jclose, k), imax)
          call ema(alphalg, i, env(1,jlgema,k), env(1, jclose, k), imax)
          call ema(0.05, i, env(1,jtslpema,k), env(1, jclose, k), imax)
          call indmacd(i, env(1,jcrs,k), env(1,jlgema,k), env(1,jstema,k), env(1,jtslpema,k), imax)

          call ret(i, env(1,jret,k), env(1, jpdiv,k),imax)
          call rebase(nlong, i, env(1,jnav,k), env(1, jret,k),imax)
          
          call retcond(chg, slip, i, env(1,jrst,k), env(1,jret, k), env(1,jcrs,k), imax)
          call rebase(nlong, i, env(1, jnavs,k), env(1, jrst, k), imax)
        enddo
      enddo
c
c Calculate the 1 year risk matrics from a price values
c
      call gen_schedule('A', nsched, sched, nbyears, dates(1,1), imax)

      nmax = imax-startday+1
c
c    strategy statistics
c
      call schedulerisks(sched, stratrisk,nbyears,env(1,jnavs,1),imax)
      open(10, file='stats_macd.csv')
      call print_schedule_risks(10, sched, stratrisk, nbyears, dates, imax)
      close(10)
c
c    index statisticis
c
      call schedulerisks(sched, indexrisk,nbyears,env(1,jnav,1),imax)
      open(10, file='stats_index.csv')
      call print_schedule_risks(10, sched, indexrisk, nbyears, dates, imax)
      close(10)
c
c    spread statistics
c
      call diffmatrix(excesrisk,stratrisk,indexrisk,nbyears,4)

      open(10, file='stats_spread_macd.csv')
      call print_schedule_risks(10, sched, excesrisk, nbyears, dates, imax)
      close(10)

      open(10, file='simul_macd.csv')
      call print_env_headers(10,env,dates,imax,jmax,kmax, headers, jheaderx)
      close(10)

      end program
