      subroutine movmax(nbbars, i, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  movmax.f : implements and moving max over a window size of nbbars 
c     if i<nbbars : the window range is i
c
c inputs :
c     nbbars:integer    : windows size  
c     i     : integer   :row index of the bar  to calculate the filter values 
c     xs    : real(1:i) :vector of inputs (prices) of the timeseries 
c
c outputs :
c     ys    : real(1:i) :vector of outputs : moving max 

      implicit none
      integer i,ix, nbbars, n, j
      real xs(1:i), ys(1:i), rmax
      n = min(i,nbbars)
      rmax = -1000000.
      do j=1,n
        rmax = max(rmax,xs(i-j+1))
      end do
      ys(i) = rmax
      
      end subroutine