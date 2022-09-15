      subroutine retcond(chg, jid,  jrt, jo, ib, ka, env, ix, jx, kx)
                        

c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ema.f : function to calculate the return over a conditional indicator
c        it assumes that the entry-exit dates are at the close dateg
c
c equation : 
c     condret(i) = ret(i)*ind(i-1) 
c                  ENSURE CAUSALITY : return from i-1 to i, with
c                  position(ind) at i-1 ! 
c
c inputs :
c     chg   :real    :cost of a entry - sold position in unit of returns
c     jid   :integer :column index of the indicator position
c     jrt   :integer :column index of the asset return 
c     jo    :integer :column index of the output (where the conditional
c                          return will be stored )
c     ib    :integer :time bar to be calculated
c     ka    :integer :depth index of the asset
c     env   :real(ix,jx,kx)
c                       : environnement of the simulation
c     ix  : integer   :row dimension of the env variables
c                         (total number of bars)
c     jx  : integer   :colunm dimension of the env variable
c                         (total number of properties)
c     kx  : integer   :depth dimenstion of the env variable
c                         (total number of assets)
c outputs :
c    the value of the ema at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer ib,ka,jrt,jid,jo,ix,jx,kx
      real chg, env(ix,jx,kx), cost
      integer ip
      ip = ib-1
      if (ib .eq. 1) then
        env(ib,jo,ka) = 0.
      else
        if (env(ip,jid,ka) .eq. env(ib,jid,ka)) then 
          cost = 0.
        else
          cost = -chg
        endif
        env(ib,jo,ka) = env(ip,jid,ka) * env(ib,jrt,ka)+cost
      endif
      end subroutine
