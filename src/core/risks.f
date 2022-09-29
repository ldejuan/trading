      subroutine risks(retrn, vol, sharpe, calmar, prcs, n)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  risks.f : function to calculate risks of a strategy of prices from 1 to n 
c          . Total Return
c          . Volatility 
c          . Sharpe Ratio
c          . calmar
c inputs :
c     prcs: real(n)      : price time series   
c     n  :  integer :size of the input price vector
c outputs :
c     retrn  : total return over the period: 
c     vol  : volatility over the period
c     sharpe  : sharpe ratio (ret/vol)
c     calmar  : max ret / max drawdown 


      implicit none
      integer i, n, rets(n)
      real prcs(n), retrn, vol, sharpe, calmar, rollmaxs(n), dds(n), maxdds(n)
      real ave, var, yrvolfc, drawdown
      external drawdown

      parameter(yrvolfc = 15.8745078664)

c calculate retrn
      retrn = prcs(n)/prcs(1) - 1.
      do i=1,n
        call ret(i, rets, prcs, n)
      end do

c calculate vol
      call avevar(rets, n, ave, var)
      vol = yrvolfc * sqrt(var)
      
c calculate sharpe
      sharpe = retrn / vol

c calculate the daily drawdown
      calmar = drawdown(prcs,n) / vol

      end subroutine

      function drawdown(data, n)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  drawdown.f : function to calculate the max drawdown over a period 
c   inputs :
c     data: real(n)      : price time series   
c     n  :  integer :size of the input price vector
c outputs :
c     drawdown  : real : maximum drawdonw over the period

      implicit none
      integer n, i
      real data(n), rollmax, dd, drawdown
      rollmax = 0.
      drawdown = 0. 
      do i=1,n
        rollmax = max(rollmax, data(i))
        dd = data(i)/rollmax - 1.
        drawdown = min(dd, drawdown)
      enddo
      return

      end function

      subroutine yearlyrisks(riskv, prcs, n, m)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    yearlyrisks.f : subroutine to calculate the yearly risks over a period.
c           The period is 252 trading days (1 year by default)
c    inputs:
c     prcs: real(n) : price time series (or strategy nav)
c     n : real : size of the time series
c    outpout: 
c     riskv: real(m,4) : risks matrix : 
c            rows : for each year : ret, vol, sharpe, calmar
c    
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
      implicit none
      integer n, m, jret, jvol, jsharpe, jcalmar,i, nbar, nn, j
      real riskv(m,4), prcs(n)
      parameter(jret=1, jvol=2, jsharpe=3, jcalmar=4, nbar=252)
      j=1
      do i=1,n,nbar
        nn = min(i+nbar,n)
        call risks(riskv(j,jret), riskv(j,jvol), riskv(j,jsharpe), riskv(j,jcalmar), prcs(i:nn), nn-i+1)
        j = j+1
      enddo

      end subroutine
