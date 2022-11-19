ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c file : binaries.f
c
c Files that contains binaries functions of types
c             z=f(x,y) 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function dhalfvalue(x,y)
c
c     calculates half price (x+y)/2
c      input : x second order moment
c              y mean value
c
      implicit none
      double precision x,y,dhalfvalue
      dhalfvalue = 0.5*(x+y)

      return
      end function
      
