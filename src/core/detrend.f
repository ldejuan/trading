      subroutine detrend(i, ys, xs , ix)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  detrend.f : implements a detrend filter from ehler high pass  filter
c              with vectors only  
c
c inputs :
c     i    : integer   :row index of the bar  to calculate the filter values 
c     ys  : real(ix) :vector of inputs : to store the output values
c     xs  : real(ix) :vector of outputs of the timeseries 
c     ix  : integer   : row dimension of the inputs timeseries
c
c outputs :
c    the value of the filter is stored in ovec(i)

      implicit none
      integer i,ix,i1
      real xs(ix), ys(ix)
      i1=i-1
      if (i .eq. 1) then
        ys(i) = 0.
      else
        ys(i) = 0.95 * (xs(i) - xs(i1)) + 0.9 * ys(i1)
      endif   

      end subroutine
