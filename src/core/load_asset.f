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

