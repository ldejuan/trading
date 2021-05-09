      subroutine vol(depth, ib, ka, jret, jvol, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  vol.f : function to calculate the volatility  
c           of the return over a period of depth ds
c
c equation : 
c     vol(i) = 1/depth (asum(ret(i)*ret(i) - ret_m(i)*ret_m(i)) 
c
cc
c inputs :   
c     depth  : integer   :depth : number of bars to calculate the vol
c     ib     : integer   :time bar to be calculated
c     ka     : integer   :index of the asset
c     jret   : integer   :index of the log ret 
c     jvol   : integer   :index of the properties where the vol will be e
c                         stored
c     env   :real(ix,jx,kx)
c                       : environnement of the simulation
c     ix  : integer   : row dimension of the env variables
c                         (total number of bars)
c     jx  : integer   : colunm dimension of the env variable
c                         (total number of properties)
c     kx  : integer   : depth dimenstion of the env variable
c                         (total number of assets)
c outputs :
c    the value of the vol at ib is stored in the jvol, ka 
c    position of the env variable
      implicit none
      integer depth,ib,ka,ji,jo,ix,jx,kx
      integer ip
      real vol, env(ix,jx,kx)
c calculate previous bar
      ip=ib-1
      if (ib .lt. depth) then
        env(ib,jo,ka) = 0. 
      else
        env(ib,jo,ka) = (1.-alpha)*env(ip,jo,ka)
     &      + alpha*env(ib,ji,ka)
      endif
      end subroutine
