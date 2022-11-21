      subroutine print_env(ird, in, jm, env, IMAX, JMAX)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_env.f
c
c  This function will print of the environement variabless to a 
c  open filename
c
c input 
c   ird      : integer      : index of the I/O to write to
c   in       : integer      : number of dates to print
c   jm       : integer      : number of propreties to print
c
c   in the environment variables
c
c   env      : double precision(iMAX, JMAX): the simulation outputd
c
c   IMAX     : integer      : MAX number of rows in env 
c   JMAX     : integer      : MAX number of proprietes in env:

c       / ird  : unit to read file
c
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird, IMAX, JMAX, in, jm, i,j
      double precision env(1:IMAX,1:JMAX)

      do i=1,in
        write(ird,*) (env(i,j),j=1,jm)
      enddo
      
      end subroutine

      subroutine print_risks(ird, freq, riskv, n, dates, m)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_risks.f
c
c  This function will print of the risk matrix over a period
c
c input 
c   ird      : integer          : index of the I/O to write to
c   freq     : integer          : frequency in bars of the risk measures (period)
c   riskv    : double precision(n)          : risk matrix
c   n        : integer          : total number of risk periods to print
c   dates    : character*10 (m) : date vector with the initial set to 1
c   m        : integer          : size of the date vector
c 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird, freq, n, m, nm, i, j
      double precision riskv(n,4)
      character*10 dates(m)
      character*10 fields(5)
      data fields /'date', 'return', 'volatility', 'sharpe', 'calmar'/
      write(ird,'(*(a15,","))') (fields(i), i=1,5)
      do i=1,n
        nm = min(m,i*freq)
        write(ird,*) dates(nm), (riskv(i,j), j=1,4)
      enddo

      end subroutine

      subroutine print_env_headers(ird,env, IMAX,JMAX,in, headers, jm)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_env.f
c
c  This function will print of the environement variabless to a 
c  open filename
c
c input 
c   ird      : integer      : index of the I/O to write to 
c   env      : double precision(iMAX,jMAX): the simulation outputd
c   in       : integer      : lenght of the output strategy (in<IMAX)
c   jm       : integer      : number of proprietes in env:
c   headers  : character*15 (jm) : headers to print
c
c  '(*(a15,","))' '(i4,i12,*(F8.3))'
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird,IMAX,JMAX,in,jm,j,i
      double precision env(1:IMAX,1:JMAX)
      character*15 headers(jm)

      write(ird,*) (headers(j), j=1, jm)

      do i=1,in
          write(ird,'(I10,*(F10.3))') int(env(i,1)), (env(i,j),j=2,jm)
        enddo

      end subroutine

      subroutine print_schedule(ird, shed, IPERIODS, ipds)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_env.f
c
c  This function will print of the environement variabless to a 
c  open filename
c
c input 
c   ird      : integer      : index of the I/O to write to 
c   shed      : integer(mmax,3): schedule
c   nnmax    : integer  : lenght of the schedule file
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird,i, j, ipds, IPERIODS, shed(IPERIODS,3)
      character*10 headers(3)
      data headers /'i_start', 'i_end', 'date_end'/ 

      write(ird,*) (headers(i),i=1,3)
      do i=1,ipds
        write(ird,*) (shed(i,j),j=1,3)
      enddo


      end subroutine

      subroutine print_simul_risks(ird, id, vars, nv, schedule, risks, stratrisks, IPERIODS, ipds)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_simul_risks.f
c
c  This function will print of the risk matrix over a period
c
c input
c   ird      : integer          : index of the I/O to write to
c   id       : integer          : id of the simulation
c   vars     : double precision(nv)      : parameters  of the simulation
c   nv       : integer          : size of the vars vector
c   schedule : integer          : schedule of the risks
c   riskv    : double precision(n)          : execess risk matrix
c   stratrisk: double precision(n)          : strategy risk
c   n        : n          : total number of risk periods to print
c 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird, nv, ipds, id, i, j, IPERIODS, schedule(IPERIODS,3)
      double precision risks(IPERIODS,4), vars(nv), stratrisks(IPERIODS,4)
      do i=1,ipds
        write(ird,'(i4,i12,*(F8.3))') id, schedule(i,3), (vars(j), j=1,nv), (risks(i,j), j=1,4),
     &  (stratrisks(i,j), j=1,4)  
      enddo 

      end subroutine   

      subroutine print_schedule_risks(ird, schedule, riskv, n)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_risks.f
c
c  This function will print of the risk matrix over a period
c
c input 
c   ird      : integer          : index of the I/O to write to
c   schedule : integer          : schedule of the risks
c   riskv    : double precision(n)          : risk matrix
c   n        : integer          : total number of risk periods to print
c 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird, n, i, j, schedule(n,3)
      double precision riskv(n,4) 
      character*10 fields(5)
      data fields /'date', 'return', 'volatility', 'sharpe', 'calmar'/
      write(ird,'(*(a15,","))') (fields(i), i=1,5)

      do i=1,n
        write(ird,*) schedule(i,3), (riskv(i,j), j=1,4)
      enddo

      end subroutine      

