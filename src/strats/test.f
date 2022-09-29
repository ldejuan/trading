      program test
      implicit none
      character*(10) sdate
      data sdate /'01/01/2020'/
      integer date2int
      external date2int

      write(*,*) sdate

      write(*,*) sdate, date2int(sdate)

      end 