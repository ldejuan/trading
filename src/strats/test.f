      program test
      implicit none
      integer i,y,m,d
      character*(10) sdate(2)
      data sdate /'01/01/2020','02/01/2020'/
      integer date2int
      double precision date2jed,f,jed
      external date2int,date2jed

      do i=1,2
        write(*,*) sdate(i), date2int(sdate(i)),date2jed(sdate(i))
      end do

      jed = 2446065
      
      call ymdf_to_jed_common ( y, m, d, f, jed )
      write(*,*) y,m,d,f,jed




      end 