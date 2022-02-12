      subroutine logrebase(ib, ka, ji, jo, ist, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  rebase.f : function to calculate  the total return of an asset 
c   in a 100 initial price from a given date
c           from a given asset log return timeseries
c
c equation : 
c      if i > ist
c          logrebase(i) = logrebase(i-1) * exp(logret(i)) 
c     else logrebase(i) = 100.
      
c
c inputs :   
c     ib     : integer   :time bar to be calculated
c     ka     : integer   :index of the asset
c     ji     : integer   :index of the log return over the period i-1,i  
c     jo     : integer   :index of the output (where the reabase
c                         price will be stored
c     ist    : integer    : start date to calculate the rebase at 100:
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
      integer ib,ka,ji,jo,ix,jx,kx,ist
      integer ip
      real env(ix,jx,kx)
c
c calculate previous bar
c
      ip=ib-1
      if (ib .le. ist) then
        env(ib,jo,ka) = 100. 
      else
        env(ib,jo,ka) = env(ip,jo,ka) * exp(env(ib,ji,ka))
      endif
      end subroutine
