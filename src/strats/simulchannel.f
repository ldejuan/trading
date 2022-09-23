      program simulchannel
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c simulchannel.f
c date 20220923
c author Luis de Juan
c version 0.1
c 
c  File entry  point for a simulation of the a channel break through 
c  The common structure to env structure 
c  simulchannel.inc for parameters
c  assetfilename : name of the file to open
c  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 assetfilename /'../data/cac40_index.csv'/ 
      integer i,k, ird, iwr, iwrx, irdx, nn, mm, kk, imax, jmax, kmax
      integer jopen, jhigh, jlow, jclose,jpdiv

      integer jminhigh, jmaxlow
      integer nbbarshigh, nbbarslow

      integer jret, jnav
      integer jcrs, jrst, jnavs 
      integer nst,nlg,nyr, nbdays

      real chg,slip
      data chg /1.e-5/
      data slip /0.0/
      data nbdays /252/
      logical ier
      include "simulchannel.inc"
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
      do i=1,imax
        do k=1,kmax
          call movmax(nbbarslow, i, env(1,jmaxlow,k) , env(1, jlow, k), imax)
          call movmin(nbbarshigh, i, env(1,jminhigh,k) , env(1, jhigh, k), imax)
          call indchannelbreak(i, env(1,jcrs,k), env(1,jminhigh,k), env(1,jmaxlow,k), env(1,jhigh,k), env(1,jlow,k), imax)
          call ret(i, env(1,jret,k), env(1, jpdiv,k),imax)
          call rebase(1, i, env(1,jnav,k), env(1, jret,k),imax)
          call retcond(chg, slip, i, env(1,jrst,k), env(1,jret, k), env(1,jcrs,k), imax)
          call rebase(1, i, env(1, jnavs,k), env(1, jrst, k), imax)
        enddo
      enddo
    
      call print_env(1,env,dates,imax,jmax,kmax)

      end program
