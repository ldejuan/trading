      subroutine ret1(ji, jo, ib, ka,env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ret.f : function to calculate  the period asset returns over a period²e
c           from a given asset time series
c
c equation : 
c     return(i) = price(i)/price(i-1) - 1. 
c
c
c inputs :   
c     ji  :integer  :colunm index of the price 
c     jo  :integer  :colunm index of the output (where the return will be
c                         stored
c     ib  :integer  :row time bar to be calculated
c     ka  :integer  :depth index of the asset
c     env :real(ix,jx,kx)
c                   :environnement of the simulation
c     ix  :integer  :row dimension of the env variables
c                         (total number of bars)
c     jx  :integer  :colunm dimension of the env variable
c                         (total number of properties)
c     kx  :integer  :depth dimenstion of the env variable
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
        env(ib,jo,ka) = env(ib,ji,ka)/env(ip,ji,ka) - 1.
      endif
      end subroutine

      subroutine ret(i, ys, xs, ix)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ret.f : function to calculate  the period asset returns over a period²e
c           from a given asset time series
c
c equation : 
c     return(i) = price(i)/price(i-1) - 1. 
c
c inputs :   
c     i    : integer   :row index of the bar  to calculate the filter values 
c     ys  : real(ix) :vector of inputs : to store the output values
c     xs  : real(ix) :vector of outputs of the timeseries 
c     ix  : integer   : row dimension of the inputs timeseries
c outputs :
c
c outputs :
c    the value of the filter is stored in ys(i)
      implicit none
      integer i,ix,i1
      real alpha, xs(ix), ys(ix)
c calculate previous bar
      i1=i-1
      if (i .eq. 1) then
        ys(i) = 0.
      else
        ys(i) = xs(i) / xs(i1) -1.
      endif
      end subroutine
