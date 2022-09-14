      subroutine ema(alpha, ji, jo, ib, ka, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ema.f : function to calculate an exponential moving average
c           from a given asset time series
c
c equation : 
c     ema(i) = ema(i-1)*(1.-alpha) + alpha * x(i)
c
c      alpha = 2/(N+1) for N terms in a moving average
c
c inputs :   
c     alpha  : real      :parameter of ema (from 0 to 1)
c     ji     : integer   :index of the price 
c     jo     :integer    :index of the output (where the ema will be
c                         stored
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
c    the value of the ema at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer ib,ka,ji,jo,ix,jx,kx
      integer ip
      real alpha, env(ix,jx,kx)
c calculate previous bar
      ip=ib-1
      if (ib .eq. 1) then
        env(ib,jo,ka) = env(ib,ji,ka)
      else
        env(ib,jo,ka) = (1.-alpha)*env(ip,jo,ka)
     &      + alpha*env(ib,ji,ka)
      endif
      end subroutine
