      subroutine vol(depth, i, yvols, xs,ix)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  vol.f : function to calculate the mean and volatility  
c           of the return over a period of depth ds
c     
c equation : 
c       calls the numeral recipices function avevar
c       The initialisation is made for i>1 until n<depth
c       if i <= depth then n=i 
c
cc
c inputs :   
c     depth  : integer   :depth : number of bars to calculate the vol
c     i      : integer   :time bar to be calculated
c     xs     : real(ix)  : time series 
c     ix     : integer   :index of the asset
c outputs :
c     ymeans : real(ix) : time series of the output means at i
c     yvols  : real(ix) : time series of the output variance at i
      implicit none
      integer depth,i, ix, n
      real  yvols(ix), xs(ix), yrvolfc, var, mean
      parameter(yrvolfc = 15.8745078664)
c calculate previous bar
      n = min(i, depth)
      if (n .eq. 1) then
         yvols(i) = 0. 
      else
        call avevar(xs(i-n+1), n, mean, var)
        yvols(i) = yrvolfc * sqrt(var)
      endif
      end subroutine
