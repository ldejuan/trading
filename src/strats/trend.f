      program simulation
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c trend.f
c date 20220915
c author Luis de Juan
c version 0.1
c 
c     Version of the instantaneos trend line of John Ehlers
c     Uses numerical recipices to calculate the power spectrum
c
c     ientps: integer : row index of the firs date to calculate the spectrum
c     ientpe: integer : row index of the last date to calculate the spectrum
c     npole : integer : number of poles
c  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 assetfilename /'../data/cac40_index.csv'/ 
      integer i,k, ird, iwr, iwrx, irdx, nn, mm, kk, imax, jmax, kmax
      integer jopen,jhigh,jlow,jclose,jpdiv,jret,jals,jtrd
      integer ientps, ientpe, npole, ndepth, nperiod, nbyears
      logical ier
      double precision xms,fq, evlmem
      external evlmem
      include "trend.inc"
c
c env : global environnement with all the data 
c     env(i,j,k): i: bars, j: proprietes, k: asset
c
      
      double precision env(1:imax,1:jmax,1:kmax), coefs(1:npole), spectrum(1:nperiod, 1:nperiod)
      character dates(1:imax,1:kmax)*10
      common /cfile/ird
      common /cerror/iwr,ier
      ird = irdx
      iwr = iwrx

      call load_asset(assetfilename,env, dates, nn,mm,kk, kmax)
c calculation of the returns  
      do i=1,imax
        call ret(i, env(1,jret,kk), env(1,jpdiv,kk), imax)
        call aliasing(i, env(1,jals,kk), env(1,jpdiv, kk), imax)
        call detrend(i, env(1,jtrd,kk), env(1,jals, kk), imax)        
      enddo

c calculation of the maximum entropy spectrum for one window
      ientpe = imax
      ientps = imax - ndepth + 1

      call memcof(env(ientps,jret,1), ndepth, npole, xms, coefs)

c calculate the spectrum

      do i=nperiod, 2, -1
        fq = 1./ double precision(i)
        spectrum(i,1) = fq
        spectrum(i,2) = evlmem(fq,coefs,npole,1.)
        write(*,*) i, spectrum(i,1), spectrum(i,2)
      enddo

c      call print_env(1,env,dates,imax,jmax,kmax)

c calculate the yearly risks

      end program
