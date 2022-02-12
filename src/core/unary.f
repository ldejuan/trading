      subroutine unary(func, ib, ka, ji, jo, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  unary.f : implements a call to a func of type unary f(x) 
c
c equation : 
c     func(env(ib,ka,ja) : 
c inputs :
c     func   : real      :external unary function
c     ib     : integer   :time bar to be calculated
c     nbars  : integer   :number of periods (nbars)of the rolling 
c     ka     : integer   :index of the asset
c     ji     : integer   :index of the timeseriese 
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
      real func,env(ix,jx,kx)
      
      env(ib,jo,ka) = func(env(ib,ji,ka)) 
      
      end subroutine
