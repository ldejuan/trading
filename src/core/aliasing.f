      subroutine aliasing1(ji, jo, ib, ka, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  aliasing.f : implements and anti-aliasing filter from ehler low pass filter  
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
      if (ib .le. 3) then
        env(ib,jo,ka) = env(ib,ji,ka)
      else
        env(ib,jo,ka) = 0.0774 + env(ib,ji,ka) + 0.0778 * env(ib-1,ji,ka)
     &  + 0.0778 * env(ib-2, ji,ka) + 0.0774 * env(ib-3,ji,ka)
     &  + 1.487 * env(ib-1,jo,ka) -1.0668 * env(ib-2,jo,ka) 
     &  + 0.2698 * env(ib-3,jo,ka)
      endif      
      end subroutine


       subroutine aliasing(i, ys, xs,ix)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  aliasing.f : implements and anti-aliasing filter from ehler low pass filter  
c
c inputs :
c     i    : integer   :row index of the bar  to calculate the filter values 
c     ys  : real(ix) :vector of inputs : to store the output values
c     xs  : real(ix) :vector of outputs of the timeseries 
c     ix  : integer   : row dimension of the inputs timeseries
c
c outputs :
c    the value of the filter is stored in ys(i)

      implicit none
      integer i,ix,i1,i2,i3
      real xs(ix), ys(ix)
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
