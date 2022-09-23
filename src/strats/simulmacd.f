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
      integer i,k, ird, iwr, iwrx, irdx, nn, mm, kk, imax, jmax, kmax
      integer jopen, jhigh, jlow, jclose,jpdiv

      integer jstema, jlgema, jtslpema
      integer nshort, nlong
      real alphast, alphalg 

      integer jret, jnav
      integer jcrs, jrst, jnavs 
      integer nst,nlg,nyr, nbdays

      real chg,slip
      data chg /1.e-5/
      data slip /0.0/
      data nbdays /252/
      logical ier
      include "simulmacd.inc"
c
c env : global environnement with all the data 
c     env(i,j,k): i: bars, j: proprietes, k: asset
c
      
      real env(1:imax,1:jmax,1:kmax)
      character dates(1:imax,1:kmax)*10
      common /cfile/ird
      common /cerror/iwr,ier
      ird = irdx
      iwr = iwrx

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
    
      call print_env(1,env,dates,imax,jmax,kmax)

      end program
