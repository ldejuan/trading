      subroutine logret( i, ys, xs)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  logret.f : function to calculate  the asset log returns over a period²e
c           from a given asset time series
c
c equation : 
c     logreturn(i) = log(price(i)/price(i-1)) 
c
c
c inputs :   
c     i        : integer   : time bar to be calculated
c     ys       : real(1:i)  : vector of outputs : log returns 
c     xs       : real(1:i)  : vector of prices   
c     ix       : integer   : row dimension of the inputs timeseries
c outputs :
c    the value of the return at i is stored in the ys(i)
c outputs :
c    the value of the return at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer i
      integer ip
      real xs(1:i),ys(1:i)
c calculate previous bar
      ip=i-1
      if (i .eq. 1) then
        ys(i) = 0. 
      else
        ys(i) = log(xs(i)/xs(i-1))
      endif
      end subroutine
