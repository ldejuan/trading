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
c   env      : real(iMAX, JMAX): the simulation outputd
c
c   IMAX     : integer      : MAX number of rows in env 
c   JMAX     : integer      : MAX number of proprietes in env:

c       / ird  : unit to read file
c
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird, IMAX, JMAX, in, jm, i,j
      real env(1:IMAX,1:JMAX)

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
c   riskv    : real(n)          : risk matrix
c   n        : integer          : total number of risk periods to print
c   dates    : character*10 (m) : date vector with the initial set to 1
c   m        : integer          : size of the date vector
c 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird, freq, n, m, nm, i, j
      real riskv(n,4)
      character*10 dates(m)
      character*10 fields(5)
      data fields /'date', 'return', 'volatility', 'sharpe', 'calmar'/
      write(ird,'(*(a15,","))') (fields(i), i=1,5)
      do i=1,n
        nm = min(m,i*freq)
        write(ird,*) dates(nm), (riskv(i,j), j=1,4)
      enddo

      end subroutine

      subroutine print_env_headers(ird, env, dates,ix,jx,kx, headers, jheaderx)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_env.f
c
c  This function will print of the environement variabless to a 
c  open filename
c
c input 
c   ird      : integer      : index of the I/O to write to 
c   env      : real(ix,jx,kx): the simulation outputd
c   dates    : character*10 (ix,kx))  : dates for each assets/bars
c   ix       : integer      : number of rows in env 
c   jx       : integer      : number of proprietes in env:
c   kx       : integer      : number of assets to outputs
c   headers  : character*15 (jheaders) : headers to print
c   jheaders : integer      : total numbe of headers 

c       / ird  : unit to read file
c output :
c    env      : real(nn,mm,kx)         : env variable for the simulation
c    dates    : character(nn,kx)*10    : dates for the simulation
c                                as a function of the assets
c     The function will insert the values in the  
c       env(i,j,kk)
c       dates(i,kk)
c
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird,ix,jx,kx,i,j,k,jheaderx
      real env(ix,jx,kx)
      character dates(ix,kx)*10
      character*15 headers(jheaderx)

      write(ird,'(*(a15,","))') (headers(j), j=1, jheaderx)

      do k=1,kx
        do i=1,ix
          write(ird,*) i,dates(i,k),(env(i,j,k),j=1,jx)
        enddo
      enddo

      end subroutine

      subroutine print_schedule(ird, shed, mmax)
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
      integer ird,i, j, mmax, shed(mmax,3)
      character*10 headers(3)
      data headers /'i_start', 'i_end', 'date_end'/ 

      write(ird,*) (headers(i),i=1,3)
      do i=1,mmax
        write(ird,*) (shed(i,j),j=1,3)
      enddo


      end subroutine

      subroutine print_simul_risks(ird, vars, nv, schedule, risks, stratrisks, n)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_simul_risks.f
c
c  This function will print of the risk matrix over a period
c
c input 
c   ird      : integer          : index of the I/O to write to
c   vars     : real(nv)      : parameters  of the simulation
c   nv       : integer          : size of the vars vector
c   schedule : integer          : schedule of the risks
c   riskv    : real(n)          : execess risk matrix
c   stratrisk: real(n)          : strategy risk
c   n        : n          : total number of risk periods to print
c 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird, nv, n, i, j, schedule(n,3)
      real risks(n,4), vars(nv), stratrisks(n,4)
      do i=1,n
        write(ird,*) schedule(i,3), (vars(j), j=1,nv), (risks(i,j), j=1,4),
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
c   riskv    : real(n)          : risk matrix
c   n        : integer          : total number of risk periods to print
c 
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird, n, i, j, schedule(n,3)
      real riskv(n,4) 
      character*10 fields(5)
      data fields /'date', 'return', 'volatility', 'sharpe', 'calmar'/
      write(ird,'(*(a15,","))') (fields(i), i=1,5)

      do i=1,n
        write(ird,*) schedule(i,3), (riskv(i,j), j=1,4)
      enddo

      end subroutine      

