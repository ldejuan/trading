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
      integer i,j,IMAX,JMAX,in,jm
      integer ishort, fshort, dshort, ilong, flong, dlong
      integer IPERIODS, nvars
c      character*15 headers(11)
      integer ird, irdx, iwr, iwrx

c      data headers /'date', 'short','long', 'spread_return', 'spread_vol',
c     & 'spread_sharpe', 'spread_calmar', 'return', 'vol', 'sharpe', 'calmar' /

      include "macdgenerate.inc"

c      integer schedule(IPERIODS,3)
c      real env(1:IMAX,1:JMAX), risks(IPERIODS,4), vars(2), stratrisks(IPERIODS,4)

      real env(1:IMAX, 1:JMAX)
      common /cfile/ ird

      write(*,*) 'loading environment'
      call load_asset(assetfilename, in,jm, env, IMAX, JMAX)
      write(*,*) 'end of loading environment'
c      call gen_schedule('A', schedule, dates, in, ipds)


c      open(irdx, file='simul_macd.csv')
c      write(irdx,*) (headers(i), i=1,11)

c      do i=ishort, fshort, dshort
c        do j=ilong,flong, dlong
c          vars(1) = real(i)
c          vars(2) = real(j)
c          call macd(risks, stratrisks, vars, nvars, schedule, iprds, env, in, jn)
c          call print_simul_risks(irdx, vars, nvars, schedule, risks, stratrisks, iprds)
c        end do
c      end do

c     close(irdx)

      end program

 
