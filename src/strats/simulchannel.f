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
      integer i, in, ird, iwr, iwrx, irdx,  jm, iMAX, jMAX
      integer jopen, jhigh, jlow, jclose,jpdiv, jplog

      integer jmaxhigh, jminlow
      integer nbbarshigh, nbbarslow,nstart

      integer jret, jnav
      integer jcrs, jrst, jnavs 
      integer nst,nlg,nyr, nbdays

      double precision chg,slip
      data chg /1.e-5/
      data slip /0.0/
      data nbdays /252/
      logical ier
      include "simulchannel.inc"
c
c env : global environnement with all the data 
c     env(i,j): i: bars, j: proprietes, k: asset
c
      
      double precision env(1:IMAX,1:JMAX)
      character dates(1:IMAX)*10
      common /cfile/ird
      common /cerror/iwr,ier
      ird = irdx
      iwr = iwrx
      nstart = max(nbbarshigh,nbbarslow)

      call load_asset(assetfilename, env, dates, in, jm)
c
c start of the simulation 
c
      do i=1,in
        call movmax(nbbarshigh, i, env(1,jmaxhigh,k) , env(1, jhigh, k), imax)
        call movmin(nbbarslow, i, env(1,jminlow,k) , env(1, jlow, k), imax)
        call indchannelbreak(i, env(1,jcrs,k), env(1,jmaxhigh,k), env(1,jminlow,k), env(1,jhigh,k), env(1,jlow,k), imax)
        call ret(i, env(1,jret,k), env(1, jpdiv,k),imax)
        call rebase(nstart, i, env(1,jnav,k), env(1, jret,k),imax)
        call retcond(chg, slip, i, env(1,jrst,k), env(1,jret, k), env(1,jcrs,k), imax)
        call rebase(nstart, i, env(1, jnavs,k), env(1, jrst, k), imax)
        enddo
      enddo
    
      call print_env(1,env,dates,imax,jmax,kmax)

      end program
