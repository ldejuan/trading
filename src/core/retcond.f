      subroutine retcond(chg, slip,  i, ys, xrets, xinds,  ix)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  retcond.f : function to calculate the return over a conditional indicator
c        it assumes that the entry-exit dates are at the close dateg
c        with cost and split hedge 
c
c equation : 
c     condret(i) = ret(i)*ind(i-1) 
c                  ENSURE CAUSALITY : return from i-1 to i, with
c                  position(ind) at i-1 ! 
c
c inputs :
c     chg   :real     :cost of a entry - sold position in unit of returns
c     split :real     :splithedge in % of the return
c     i     :integer  :time bar to be calculated
c     xrets :read(ix) :input asset return time series of the 
c     xinds :real(ix) :input indicator time series
c     ix    : integer :row dimension of the inputs
c outputs :
c     ys    :real(ix) : output time series where the condret must be stored

      implicit none
      integer i, ix
      real chg, slip, cost 
      real ys(ix), xrets(ix), xinds(ix)
      integer i1
      i1 = i-1
      if (i .eq. 1) then
        ys(i) = 0.
      else
        if (xinds(i) .eq. xinds(i1)) then 
          cost = 0.
        else
          cost = - chg - slip * abs(xrets(i))
        endif
        ys(i) = xrets(i) * xinds(i1) + cost
      endif
      end subroutine
