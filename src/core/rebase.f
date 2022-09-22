      subroutine rebase(ist, i, ys, xrets, ix)   
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  rebase.f : function to calculate  the total return of an asset 
c   in a 100 initial price from a given date
c           from a given asset log return timeseries
c
c equation : 
c      if i > ist
c          ys(i) = ys(i-1) * (1.+xrets(i)) 
c     else ys(i) = 100.      
c
c inputs :   
c     i        : integer   : time bar to be calculated
c     ist      : integer   : start date to calculate the rebase at 100:
c     ys       : real(ix)  : vector of outputs : to store the output values
c     xrets    : real(ix)    : vector of inputs  asset returns  
c     ix       : integer   : row dimension of the inputs timeseries
c outputs :
c    the value of the return at i is stored in the ys(i) 
      implicit none
      integer i,ist,ix, i1
      real ys(ix),xrets(ix)
c
c calculate previous bar
c
      if (i .le. ist) then
        ys(i) = 100. 
      else
        i1=i-1
        ys(i) = ys(i1) * (1. + xrets(i))
      endif
      end subroutine
