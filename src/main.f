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
      character*80 assetfilename /'../data/ldo_im.csv'/ 
      integer i,k, ird, iwr, iwrx, irdx, nn, mm, kk, imax, jmax, kmax
      integer jopen,jhigh,jlow,jclose,jpdiv,jemast,jemalg,jret,jrbs
      integer jlret,jcrs,jrst,jnav,jyret 
      integer nst,nlg,nyr
      logical long
      real ast,alg,chg
      data chg /1.e-5/
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
          call ema(ast, i, k, jclose, jemast, env, imax, jmax, kmax)
          call ema(alg, i, k, jclose, jemalg, env, imax, jmax, kmax)
          call indcross(i,k,jemast,jemalg,jcrs,long,env,imax,jmax,kmax)
          call ret(i,k,jpdiv,jret,env,imax,jmax,kmax)
          call retcond(i,k,jret,jcrs,jrst,chg,env,imax,jmax,kmax)
          call rebase(i,k,jret,jrbs,nlg,env,imax,jmax,kmax)
          call rebase(i,k,jrst,jnav,nlg,env,imax,jmax,kmax)
          call logret(i,k,jpdiv,jlret,env,imax,jmax,kmax)
          call rollma(nyr,i,k,jlret,jyret,env,imax,jmax,kmax) 
        enddo
      enddo
      call print_env(1,env,dates,imax,jmax,kmax)
      end program
