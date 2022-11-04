      subroutine ret(i, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ret.f : function to calculate  the period asset returns over a period²e
c           from a given asset time series
c
c equation : 
c     return(i) = price(i)/price(i-1) - 1. 
c
c inputs :   
c     i    : integer   :row index of the bar  to calculate the filter values 
c     ys  : double precision(1:i) :vector of inputs : to store the output values
c     xs  : double precision(1:i) :vector of outputs of the timeseries 
c outputs :
c
c outputs :
c    the value of the filter is stored in ys(i)
      implicit none
      integer i,ix,i1
      double precision xs(1:i), ys(1:i)
c calculate previous bar
      i1=i-1
      if (i .eq. 1) then
        ys(i) = 0.
      else
        ys(i) = xs(i) / xs(i1) -1.
      endif
      end subroutine


