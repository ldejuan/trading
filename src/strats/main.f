      program simulation
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c main.f
c date 20210302
c author Luis de Juan
c version 0.1
c 
c  File entry  point for a simulation 
c  The common structure to env structure 
c  simulation.inc for parameters
c  assetfilename : name of the file to open
c  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 assetfilename /'../data/cac40_index.csv'/ 
      integer i,k, ird, iwr, iwrx, irdx, nn, mm, kk, imax, jmax, kmax
      integer jopen, jhigh, jlow, jclose,jpdiv
      integer jemast, jemalg 
      integer jret, jnav
      integer jcrs, jrst, jnavs 
      integer jvol 
      integer nst,nlg,nyr, nbdays
      logical long
      real ast,alg,chg,square,stdev, slip
      external square,stdev
      data chg /1.e-5/
      data slip /0.0/
      data nbdays /252/
      logical ier
      include "simulation.inc"
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
      ast = 2./(1. + nst)
      alg = 2./(1. + nlg) 
      do i=1,imax
        do k=1,kmax
          call ema(ast, i, env(1,jemast,k) , env(1, jclose, k), imax)
          call ema(alg, i, env(1,jemalg,k) , env(1, jclose, k), imax)
          call indcross(long, i, env(1,jcrs,k), env(1,jemalg,k), env(1,jemast,k), imax)
          call ret(i, env(1,jret,k), env(1, jpdiv,k),imax)
          call rebase(nlg, i, env(1,jnav,k), env(1, jret,k),imax)
          call retcond(chg, slip, i, env(1,jrst,k), env(1,jret, k), env(1,jcrs,k), imax)
          call rebase(nlg, i, env(1, jnavs,k), env(1, jrst, k), imax)
          call vol(nbdays,i, env(1,jvol,k), env(1,jret,k), imax)          
        enddo
      enddo
    
      call print_env(1,env,dates,imax,jmax,kmax)

      end program
