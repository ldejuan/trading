      subroutine aliasing(i, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  aliasing.f : implements and anti-aliasing filter from ehler low pass filter  
c
c inputs :
c     i   : integer   :row index of the bar  to calculate the filter values 
c     ys  : double precision(1:i) :vector of inputs : to store the output values
c     xs  : double precision(1:i) :vector of outputs of the timeseries 
c
c outputs :
c    the value of the filter is stored in ys(i)

      implicit none
      integer i,i1,i2,i3
      double precision xs(1:i), ys(1:i)
      if (i .le. 3) then
        ys(i) = xs(i)
      else
        i1 = i-1
        i2 = i-2
        i3 = i-3
        ys(i) = 0.0774 * xs(i) + 0.0778 * xs(i1)
     &  + 0.0778 * xs(i2) + 0.0774 * xs(i3)
     &  + 1.4847 * ys(i1) -1.0668 * ys(i2) 
     &  + 0.2698 * ys(i3)
      endif   

      end subroutine
