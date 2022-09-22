      subroutine unary(func,  i, ys, xs, ix)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  unary.f : implements a call to a func of type unary f(x) 
c
c equation : 
c     ys(i) = func(xs(i)) : 
c inputs :
c     func   : real      :external unary function
c     i    : integer   :row index of the bar  to calculate the filter values 
c     xs  : real(ix) :vector of outputs of the timeseries 
c     ix  : integer   : row dimension of the inputs timeseries
c outputs :
c     ys  : real(ix) :vector of inputs : to store the output values
      implicit none
      integer i,ix
      real func,xs(ix),ys(ix)
      
      ys(i) = func(xs(i)) 
      
      end subroutine
