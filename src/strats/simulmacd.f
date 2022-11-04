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
      integer i,j,IMAX,JMAX,in,jm,IPERIODS, ipds
      integer jdate, ishort, fshort, dshort, ilong, flong, dlong
      integer nvars
c      character*15 headers(11)
      integer ird, irdx, iwr, iwrx
      logical ier

c      data headers /'date', 'short','long', 'spread_return', 'spread_vol',
c     & 'spread_sharpe', 'spread_calmar', 'return', 'vol', 'sharpe', 'calmar' /

      include "macdgenerate.inc"

      integer schedule(IPERIODS,3)
      double precision risks(IPERIODS,4), vars(2), stratrisks(IPERIODS,4)

      double precision env(1:IMAX, 1:JMAX)
      common /coutput/ ier,iwr,ird
      ird = irdx
      ier = .FALSE.
      iwr = iwrx

      call load_asset(assetfilename, in, jm, env, IMAX, JMAX)
      call logger('info', 'end of loading environment')
      call gen_schedule('A', ipds, schedule, IPERIODS, env(jdate,1), in)

c      open(irdx, file='simul_macd.csv')
c      write(irdx,*) (headers(i), i=1,11)

      do i=ishort, fshort, dshort
        do j=ilong,flong, dlong
          vars(1) = double precision(i)
          vars(2) = double precision(j)
          call macd(risks, stratrisks, vars, nvars, schedule, IPERIODS, iprds, env, IMAX, JMAX, in)
c          call print_simul_risks(irdx, vars, nvars, schedule, risks, stratrisks, iprds)
        end do
      end do

c     close(irdx)

      end program

 
