      subroutine logret( ji, jo, ib, ka, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  logret.f : function to calculate  the asset log returns over a period²e
c           from a given asset time series
c
c equation : 
c     return(i) = log(price(i)/price(i-1)) 
c
c
c inputs :   
c     ib     : integer   :time bar to be calculated
c     ka     : integer   :index of the asset
c     ji     : integer   :index of the price 
c     jo     :integer    :index of the output (where the return will be
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
c    the value of the return at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer ib,ka,ji,jo,ix,jx,kx
      integer ip
      real env(ix,jx,kx)
c calculate previous bar
      ip=ib-1
      if (ib .eq. 1) then
        env(ib,jo,ka) = 0. 
      else
        env(ib,jo,ka) = log(env(ib,ji,ka)/env(ip,ji,ka))
      endif
      end subroutine
