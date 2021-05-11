ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c file : unaries.f
c
c Files that contains unaries functions of types
c y=f(x) or y=f(x,c) with c constants
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c square fonction
c      input : x real
c      output: x*x 
c
      function square(x)
      implicit none
      real x,square

      square = x**2
      return
      end

      function stdev(x,y)
c
c     calculates the volatility
c      input : x second order moment
c              y mean value
c
      implicit none
      real x,y,factor,stdev

c     factor = 1/sqrt(252)*(252-1)/252
      parameter (factor=0.06274410233155821) 
      stdev = sqrt(x-y*y)*factor
      return
      end
      
      
