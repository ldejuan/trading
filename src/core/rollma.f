      subroutine rollma(nbars, ji, jo, ib, ka, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ret.f : function to calculate rolling moving average of a time series 
c           over a period of length = nbars
c
c equation : 
c     if ib > nb) : 
c     rollma(ib,nd) = rollma(ib-1,nd) + 1./nbarsn * (x(ib) - x(ib-nd+1)
c     if ib <= nb : 
c     rollma(ib,nd) = (1-1/ib)*rollma(ib,nb) + 1/ib * x(ib)
c
c inputs :
c     nbars :integer  :number of periods (nbars)of the rolling 
c     ji    :integer  :column index of the timeseries (prices) 
c     jo    :integer  :colmun index of the output (where the return will be
c                         stored
c     ib    :integer  :row time bar to be calculated
c     ka    :integer  :depth index of the asset
c     env   :real(ix,jx,kx)
c                       : environnement of the simulation
c     ix    :integer  :row dimension of the env variables
c                         (total number of bars)
c     jx    :integer  :colunm dimension of the env variable
c                         (total number of properties)
c     kx    :integer  :depth dimenstion of the env variable
c                         (total number of assets)
c outputs :
c    the value of the return at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer ib,nbars,ka,ji,jo,ix,jx,kx
      integer ip
      real env(ix,jx,kx),alpha,res
c calculate previous bar
      if (ib .eq. 1) then
        res = env(ib,ji,ka)
      else if (ib .lt. nbars) then 
        res = (1. - 1./ib) * env(ib-1,jo,ka) + 1./ib * env(ib,ji,ka)
      else
        alpha = 1./nbars
        res =  env(ib-1,jo,ka) + alpha * (env(ib,ji,ka) - env(ib-nbars+1,ji,ka))          
      endif

      env(ib,jo,ka) = res
      end subroutine
