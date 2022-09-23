      subroutine binary(func, i, zs, ys, xs, ix)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  unary.f : implements a call to a func of type binray z=f(x,y) 
c
c inputs :
c     i    : integer   :row index of the bar  to calculate the filter values 
c     ys  : real(ix) :vector of inputs : to store the  values
c     xs  : real(ix) :vector of inputs of the timeseries
c     zs  : real(ix) :  vector of ouputs
c     
c     ix  : integer   : row dimension of the inputs timeseries
c
c outputs :
c    the value of the filter is stored in ys(i)                      
      integer i,ix,i1,i2,i3
      real xs(ix), ys(ix), zs(ix)
      real func
      
      zs(i)= func(xs(i),ys(i)) 
      
      end subroutine

