      program onemacd
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c onechannel.f
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
      character*80 assetfilename /'../data/cac40_index.csv'/
      integer i,j,IMAX,JMAX,in,jm,IPERIODS, ipds,id
      integer jdate, ishort, fshort, dshort, ilong, flong, dlong
      integer nvars
      character*15 headers(16)
      integer ird, irdx, iwr, iwrx
      logical ier

      data headers / 'date',
     & 'open',
     & 'high',
     & 'low',
     & 'close',
     & 'divclose',
     & 'half',
     & 'close',
     & 'filter_short',
     & 'filter_long',
     & 'slope',
     & 'ret_underling',
     & 'nav_underling',
     & 'position',
     & 'ret_strategy',
     & 'nav_strategy'/

      include "onemacd.inc"

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

      vars(1) = real(short,8)
      vars(2) = real(long,8)
      call macd3(excessrisks, stratrisks, vars, nvars, schedule, IPERIODS, ipds, env, IMAX, JMAX, in)

      open(10, file='/outputdir/simul_onemacd.csv')
      call print_env_headers(10,env, IMAX,JMAX,in, headers, 16)
      close(10)


      call logger('debug', 'success generating strategy with short and long')

      end program

