      subroutine detrend1(ji, jo, ib, ka, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  detrend.f : implements a detrend filter from ehler high pass  filter  
c
c inputs :
c     ji    : integer   : column index of the asset price 
c     jo    : integer    :column index of the output (where the filter value will be
c                         stored)
c     ib     : integer   :time bar to be calculated 
c     ka     : integer   :index of the asset
c     env   :real(ix,jx,kx)
c                       : environnement of the simulation
c     ix  : integer   : row dimension of the env variables
c                         (total number of bars)
c     jx  : integer   : colunm dimension of the env variable
c                         (total number of properties)
c     kx  : integer   : depth dimenstion of the env variable
c                         (total number of assets)
c outputs :
c    the value of the return at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer ib,ka,ji,jo,ix,jx,kx
      real env(ix,jx,kx)
      if (ib .eq. 1) then
        env(ib,jo,ka) = 0.
      else
        env(ib,jo,ka) = 0.95 * env(ib,ji,ka) - 0.95 * env(ib-1, ji,ka) 
     &  + 0.9 * env(ib-1,jo,ka)
      endif      
      end subroutine

       subroutine detrend(i, ys, xs , ix)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  detrend.f : implements a detrend filter from ehler high pass  filter
c              with vectors only  
c
c inputs :
c     i    : integer   :row index of the bar  to calculate the filter values 
c     ys  : real(ix) :vector of inputs : to store the output values
c     xs  : real(ix) :vector of outputs of the timeseries 
c     ix  : integer   : row dimension of the inputs timeseries
c
c outputs :
c    the value of the filter is stored in ovec(i)

      implicit none
      integer i,ix,i1
      real xs(ix), ys(ix)
      i1=i-1
      if (i .eq. 1) then
        ys(i) = 0.
      else
        ys(i) = 0.95 * (xs(i) - xs(i1)) + 0.9 * ys(i1)
      endif   

      end subroutine
