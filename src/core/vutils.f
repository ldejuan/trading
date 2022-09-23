c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  v.utils.f : implements several vector like calculation 
c
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine cum(func, ys, n,xs)
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      map.f subroutine to calculate the running cummulative ever a vector
c
c inputs: 
c     func: real : binary function (max, min, sum)
c      xs  : real(n): input time series 
c      n   : integer: size of the timeseries
c outputs: 
c     ys : real(n): vector of outputs
c
      implicit none
      integer n, i
      real ys(n), xs(n), func

      ys(1) = xs(1)
      do i=2,n
         ys(i) = func(xs(i),ys(i-1)) 
      end do

      end subroutine

      subroutine map(func, ys, n, xs)
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      map.f subroutine to apply func to xs : vectorize a function
c
c inputs: 
c     func: real : unary function 
c      xs  : real(n): input time series 
c      n   : integer: size of the timeseries
c outputs: 
c     ys : real(n): vector of outputs
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer n, i
      real ys(n), xs(n), func
      do i=1,n
         ys(i) = func(xs(i)) 
      end do
      end subroutine
