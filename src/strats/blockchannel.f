      program blockchannel
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c blockchannel.f
c date 20221004
c author Luis de Juan
c version 0.1
c 
c  File entry  point for a simulation of the a channel break through 
c  with a block environnement  
c  blockchannel.inc for parameters
c  assetfilename : name of the file to open
c  What should be variable are 
c      ix <= IMAX number of bars 
c      kx <= KMAX number of assets
c  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 assetfilename /'../data/assets.csv'/ 
      integer i,k, ird, iwr, iwrx, irdx, jm, IMAX, JMAX, KMAX
      integer kx
      integer jopen, jhigh, jlow, jclose,jpdiv

      integer jmaxhigh, jminlow
      integer nbbarshigh, nbbarslow,nstart

      integer jret, jnav
      integer jcrs, jrst, jnavs 
      integer nst,nlg,nyr, nbdays

      real chg,slip
      data chg /1.e-5/
      data slip /0.0/
      data nbdays /252/
      logical ier
      include "blockchannel.inc"
c
c env : global environnement with all the data 
c     env(i,j,k): i: bars, j: proprietes, k: asset
c
      
      real env(1:IMAX,1:JMAX,1:KMAX)
      integer nmaxs(1:KMAX)
      common /cfile/ird
      common /cerror/iwr,ier
      ird = irdx
      iwr = iwrx
      nstart = max(nbbarshigh,nbbarslow)

      call load_assets(assetsfilename, env, nmaxs, jm, kx, IMAX, JMAX, KMAX)

c
c start of the simulation 
c
      do k=1,kx
        do i=1,nmaxs(k)
          call movmax(nbbarshigh, i, env(1,jmaxhigh,k) , env(1, jhigh, k), IMAX)
          call movmin(nbbarslow, i, env(1,jminlow,k) , env(1, jlow, k), IMAX)
          call indchannelbreak(i, env(1,jcrs,k), env(1,jmaxhigh,k), env(1,jminlow,k), env(1,jhigh,k), env(1,jlow,k), IMAX)
          call ret(i, env(1,jret,k), env(1, jpdiv,k),IMAX)
          call rebase(nstart, i, env(1,jnav,k), env(1, jret,k),IMAX)
          call retcond(chg, slip, i, env(1,jrst,k), env(1,jret, k), env(1,jcrs,k), IMAX)
          call rebase(nstart, i, env(1, jnavs,k), env(1, jrst, k), IMAX)
        enddo
      enddo
      call print_env_date(ird,env,nmaxs,iMAX,JMAX,kx)

      end program
