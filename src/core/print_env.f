      subroutine print_env(ird, env, dates,ix,jx,kx)
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
c   dates    : character(ix,kx)*10)  : dates for each assets/bars
c   ix       : integer      : number of rows in env 
c   jx       : integer      : number of proprietes in env:
c   kx       : integer      : number of assets to outputs 
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
      integer ird,ix,jx,kx,i,j,k
      real env(ix,jx,kx)
      character dates(ix,kx)*10

      do k=1,kx
        do i=1,ix
          write(*,*) i,dates(i,k),(env(i,j,k),j=1,jx)
        enddo
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

