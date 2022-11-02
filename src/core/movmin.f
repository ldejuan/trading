      subroutine movmin(nbbars, i, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  movmin.f : implements and moving min over a window size of nbbars 
c     if i<nbbars : the window range is i
c
c inputs :
c     nbbars:integer    : windows size  
c     i     : integer   :row index of the bar  to calculate the filter values 
c     xs    : real(1:i) :vector of inputs (prices) of the timeseries 
c     ix    : integer   : row dimension of the inputs timeseries
c
c outputs :
c     ys    : real(1:i) :vector of outputs : moving min 

      implicit none
      integer i,ix, nbbars, n, j
      real xs(1:i), ys(1:i), rmin
      n = min(i,nbbars)
      rmin = 1000000.
      do j=1,n
        rmin = min(rmin,xs(i-j+1))
      end do
      ys(i) = rmin
      
      end subroutine
