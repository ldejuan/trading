      subroutine ema(alpha, i, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ema.f : function to calculate an exponential moving average
c           from a given asset time series
c
c equation : 
c     ema(i) = ema(i-1)*(1.-alpha) + alpha * x(i)
c
c      alpha = 2/(N+1) for N terms in a moving average
c
c inputs :
c     alpha: real      : ema factors   
c     i    : integer   :row index of the bar  to calculate the filter values 
c     xs  : real(ix) :vector of outputs of the timeseries 
c     ix  : integer   : row dimension of the inputs timeseries
c outputs :
c     ys  : real(ix) :vector of inputs : to store the output values
      implicit none
      integer i,i1
      real alpha, xs(1:i), ys(1:i)
c calculate previous bar
      i1=i-1
      if (i .eq. 1) then
        ys(i) = xs(i)
      else
        ys(i) = (1.-alpha)*ys(i1) + alpha*xs(i)
      endif
      end subroutine
