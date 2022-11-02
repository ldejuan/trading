      subroutine unary(func,  i, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  unary.f : implements a call to a func of type unary f(x) 
c
c equation : 
c     ys(i) = func(xs(i)) : 
c inputs :
c     func   : real      :external unary function
c     i    : integer   :row index of the bar  to calculate the filter values 
c     xs  : real(1:i) :vector of inputs of the timeseries 
c
c outputs :
c     ys  : real(1:i) :vector of outputs : to store the output values
      implicit none
      integer i,ix
      real func,xs(1:i),ys(1:i)
      
      ys(i) = func(xs(i)) 
      
      end subroutine
