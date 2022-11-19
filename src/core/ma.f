      subroutine ma(nbars, i, ys, xs)       
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ret.f : function to calculate rolling moving average of a time series 
c           over a period of length = nbars in a smart way
c
c equation : 
c     if ib > nb) : 
c     ma(ib,nd) = ma(ib-1,nd) + 1./nbarsn * (x(ib) - x(ib-nd+1)
c     if ib <= nb : 
c     rollma(ib,nd) = (1-1/ib)*rollma(ib,nb) + 1/ib * x(ib)
c
c inputs :
c     nbars :integer  :number of periods (nbars)of the rolling 
c inputs :
c     i    : integer   :row index of the bar  to calculate the filter values 
c     xs  : double precision(1:i) :vector of outputs of the timeseries 
c
c outputs :
c     ys  : double precision(1:i) :vector of inputs : to store the output values
c outputs :
c    the value of the filter is stored in ys(i)
      implicit none
      integer i,nbars, ix
      double precision ys(1:i), xs(1:i), res, alpha
c calculate previous bar
      if (i .eq. 1) then
        res = xs(i)
      else if (i .lt. nbars) then 
        res = (1. - 1./i) * ys(i-1) + 1./i * xs(i)
      else
        alpha = 1./nbars
        res =  ys(i-1) + alpha * (xs(i) - xs(i-nbars+1))          
      endif

      ys(i) = res
      end subroutine

