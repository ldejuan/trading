      subroutine detrend(i, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  detrend.f : implements a detrend filter from ehler high pass  filter
c              with vectors only  
c
c inputs :
c     i    : integer   :row index of the bar  to calculate the filter values 
c     ys  : real(1:i) :vector of inputs : to store the output values
c     xs  : real(1:i) :vector of outputs of the timeseries 
c
c outputs :
c    the value of the filter is stored in ovec(i)

      implicit none
      integer i,i1
      real xs(1:i), ys(1:i)
      i1=i-1
      if (i .eq. 1) then
        ys(i) = 0.
      else
        ys(i) = 0.95 * (xs(i) - xs(i1)) + 0.9 * ys(i1)
      endif   

      end subroutine
