      subroutine load_asset(filename, in, jm, env, IMAX, JMAX)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subroutine to load_market.f
c
c To load a single market bars as OLCHP are other values and the dates
c
c The values should be describe as:
c YYYYMMDD XXXX.X XXXXX.XX XXXXX.X XXXXX.X 
c where YYYYMMDD at the initial date of the period
c XXXXX.XX are values of the given property : they will be transformed
c ton float 32
c input 
c   filename : character(80): full name with path of the asset file name  
c   jm        : integer      : number of properties to read
c   IMAX      : integer      : mMAX number of dates
c   JMAX      : integer      : MAX number of properties
c       / ird : unit to read file
c outputs :
c    Defined in the environment 
c    env       : double precision(IMAX, JMAX)         : env variable for the simulation
c    in        : actual number of dates read
c                                as a function of the assets
c     The function will insert the values in the  
c       env(i,j)

c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      character*80 filename
      character*100 line
      integer i,j,in,jm,ird, IMAX, JMAX, iwr
      logical ier
      double precision env(IMAX,JMAX)
      integer date2int
      external date2int
      common /coutput/ier,iwr, ird


      if (jm .gt. JMAX) then 
        call logger('error','number of propreties to read larger than JMAX')
        return 
      end if 

      open(ird,file = filename, err = 10)

      do i=1,IMAX
c      
c read a line as a string
c
        read(ird,'(A)', err = 20, end=1) line
c
c parse the date and save as double precision YYYYMMDD
c
        env(i,1) = date2int(line(1:10))
c
c parse the values (OHLCD)
c
        read(line(11:),*, err = 20, end=1) (env(i,j),j=2,jm)
      enddo
c     remove last index call
      in = i-1
      call logger('warning', 'data length greater than iMAX')
      close (ird)
      return
c     remove last index call
   1  call logger('info', 'end of file found')
      in = i-1 
      close(ird)
      return
c 
c error section 
c 

  10  call logger('error','load_asset : error opening file')
      close(ird)
      return

  20  call logger('error', 'load_asset : Error reading bars')
      close(ird)
      return
 

      end subroutine


