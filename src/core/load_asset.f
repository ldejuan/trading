      subroutine load_asset(filename, env, dates, nn, mm, ka, kx)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to load_market.f
c
c To load a market bars as OLCHP are other values 
c
c The values should be describe as:
c YYYYMMDD XXXX.X XXXXX.XX XXXXX.X XXXXX.X 
c where YYYYMMDD at the initial date of the period
c XXXXX.XX are values of the given property : they will be transformed
c ton float 32
c input 
c   filename : character(80): full name with path of the asset file name
c   nn       : integer      : number of values to read
c   mm       : integer      : number of properties to read
c   ka       : integer      : index of the asset to store into
c   kx       : integer      : number of maximum assets 
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
      character*80 filename
      character*100 line
      integer i,j,nn,mm,ka, ird,kx
      real env(1:nn,1:mm,1:kx)
      character dates(1:nn,1:kx)*10
      common /cfile/ ird

      open(ird,file = filename, err = 10)
      do i=1,nn
c      
c read a line as a string
c
        read(ird,'(A)', err = 20) line
c
c parse the date
c
        dates(i,ka) = line(1:10)
c
c parse the values (OHLCD)
c
        read(line(11:),*, err = 30) (env(i,j,ka),j=1,mm)
      enddo
      close (ird)

      return
c 
c error section 
c 
  10  call print_error('load_asset : error opening file')
      close(ird)
      return

  20  call print_error('load_asset : Error reading bars')
      close(ird)
      return

  30  call print_error('load_asset : Error reading values')
      close(ird)
      return

      end subroutine

      subroutine load_assets(assetsfilename, env, ns, m, kx, imax, jmax, kmax)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to load_assets.f
c
c To load a list of assets from an input file of type
c     asset1: full path to the asset price values
c     asset2: full path to the asset price values 
c     .... (up to kmax)
c
c input 
c   assetsfilename : character(80): full name with path of the asset file name
c   imax     : integer      : maximun number of values to read
c   jmax     : integer      : maximum size of properties
c   kmax      : integer      : maximum number of asset 
c   m         : integer      :number of properties (OHLCD) + 1to read from 2 to mm
c       ird  : unit to read file
c output :
c    env      : real(imax,jmax,kmax)         : env variable for the simulation
c    ns        : integer (kmax)                : number of dates read as a function of the asset
c    kx       : number of assets read
c                                as a function of the assets
c     The function will insert the values in the  
c       env(i,j,k) and ns(k)
c
c
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 assetsfilename
      character*100 line
      integer i,j,m, k, imax, jmax, kmax, ird, kx
      real env(1:imax,1:jmax,1:kmax), ns(1:kmax)
      character*80 assets(kmax)
      common /cfile/ ird

      open(ird,file = assetsfilename, err = 10)
      do k=1, kmax
        read(ird,'(A)', err = 20, end=40) line
        assets(k)=line(9:80)
      end do
  40  close(ird)
      kx=k-1
      do k=1,kx
        call load_asset_date(assets(k),env, ns(k), m, k, imax, jmax, kmax)  
      end do

      return 


  10  call print_error('load_asset : error assetsfilename file')
      close(ird)
      return

  20  call print_error('load_asset : Error reading bars')
      close(ird)
      return

  30  call print_error('load_asset : Error reading values')
      close(ird)
      return

      end subroutine     


      subroutine load_asset_date(filename,env, n, m, k, imax, jmax, kmax)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to load_market.f
c
c To load a market bars as OLCHP are other values 
c
c The values should be describe as:
c YYYYMMDD XXXX.X XXXXX.XX XXXXX.X XXXXX.X 
c where YYYYMMDD at the initial date of the period
c XXXXX.XX are values of the given property : they will be transformed
c ton float 32
c input 
c   filename : character(80): full name with path of the asset file name
c   imax     : integer      : maximun number of values to read
c   jmax     : integer      : maximum size of properties
c   kmax      : integer      : maximim number of asset 
c   m         : integer      :number of properties (OHLCD) + 1to read from 2 to mm
c   k       : integer      : index of the asset to store into
c       / ird  : unit to read file
c output :
c    env      : real(imax,jmax,kmax)         : env variable for the simulation
c    n        : integer                  : number of dates read
c                                as a function of the assets
c     The function will insert the values in the  
c       env(i,j,kk)
c
c
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 filename
      character*100 line
      integer i,j,n,m, k, imax, jmax, kmax, ird, jdate, date2int
      real env(1:imax,1:jmax,1:kmax)

      parameter (jdate=1)
      external date2int
      common /cfile/ ird

      if (k .gt. kmax) then 
        call print_error('k is bigger than kmax')
        return
      end if

      if (m .gt. jmax) then 
        call print_error('m is bigger than jmax')
        return
      end if

      open(ird,file = filename, err = 10)
      do i=1,imax
c      
c read a line as a string
c
        read(ird,'(A)', err = 20, end=40) line
c
c parse the date
c
        env(i,jdate,k) = real(date2int(line(1:10)))
c
c parse the values (OHLCD)
c
        read(line(11:),*, err = 30) (env(i,j,k),j=2,m)
      enddo
      close (ird)

      call print_error('check number of dates read')

      return
c 
c error section 
c 
  10  call print_error('load_asset : error opening file')
      close(ird)
      return

  20  call print_error('load_asset : Error reading bars')
      close(ird)
      return

  30  call print_error('load_asset : Error reading values')
      close(ird)
      return

  40  call print_error('load_asset : Finishing reading values')
      close(ird)
      n = i-1
      return

      end subroutine

