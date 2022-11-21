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
      integer i,j,IMAX,JMAX,in,jm,IPERIODS, ipds,id
      integer jdate, ishort, fshort, dshort, ilong, flong, dlong
      integer nvars
      character*15 headers(12)
      integer ird, irdx, iwr, iwrx
      logical ier

      data headers /'id', 'date', 'short','long', 'spread_return', 'spread_vol',
     & 'spread_sharpe', 'spread_calmar', 'return', 'vol', 'sharpe', 'calmar' /

      include "macdgenerate.inc"

      integer schedule(IPERIODS,3)
      double precision excessrisks(IPERIODS,4), vars(2), stratrisks(IPERIODS,4)

      double precision env(1:IMAX, 1:JMAX)
      common /coutput/ ier,iwr,ird
      ird = irdx
      ier = .FALSE.
      iwr = iwrx

      call load_asset(assetfilename, in, jm, env, IMAX, JMAX)
      call logger('info', 'success loading environment')
      
      call gen_schedule('A', ipds, schedule, IPERIODS, env(jdate,1), in)
      call logger('debug', 'success generating schedule')


      open(ird, file='/outputdir/simul_macd_5.csv') 
      write(ird,*) (headers(i), i=1,12)
      id = 1
      do i=ishort, fshort, dshort
        do j=ilong,flong, dlong
          vars(1) = real(i,8)
          vars(2) = real(j,8)
          call macd3(excessrisks, stratrisks, vars, nvars, schedule, IPERIODS, ipds, env, IMAX, JMAX, in)
          call print_simul_risks(ird, id, vars, nvars, schedule, excessrisks, stratrisks, IPERIODS, ipds)
          id = id + 1
        end do
      end do

      close(ird)

      call logger('debug', 'success simulating all strategies')

      end program