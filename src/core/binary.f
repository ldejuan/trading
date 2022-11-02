      subroutine binary(func, i, zs, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  unary.f : implements a call to a func of type binray z=f(x,y) 
c
c inputs :
c     i    : integer   :row index of the bar  to calculate the filter values 
c     ys  : real(1:i) :vector of inputs : to store the  values
c     xs  : real(1:i) :vector of inputs of the timeseries
c     zs  : real(1:i) :  vector of ouputs
c     
c
c outputs :
c    the value of the filter is stored in ys(i)                      
      integer i,i1,i2,i3
      real xs(1:i), ys(1:i), zs(1:i)
      real func
      
      zs(i)= func(xs(i),ys(i)) 
      
      end subroutine

