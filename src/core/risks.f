      subroutine risks(ret, vol, sharpe, calmar, prcs, n)  
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
c     ret  : total return over the period: 
c     vol  : volatility over the period
c     sharpe  : sharpe ratio (ret/vol)
c     calmar  : max ret / max drawdown 


      implicit none
      integer i, n, rets(n)
      real prcs(n), ret, vol, sharpe, calmar, rollmaxs(n), dds(n), maxdds(n)
      real mean, vol, yrvolfc

      parameter(yrvolfc = 15.8745078664)

c calculate ret
      ret = prcs(n)/prcs(1) - 1.
      do i=1,n
        call ret(i, rets, prcs, n)
      end do

c calculate vol
      call avevar(rets, n, mean, var)
      vol = yrvolfc * sqrt(var))
      
c calculate sharpe
      sharpe = ret / yrvolfc

c calculate the daily drawdown
      call cum(max, rollmaxs, n,prcs)
      call cum(ret, dds, n,)

c    Daily_Drawdown = SPY_Dat['Adj Close']/Roll_Max - 1.0
c    Max_Daily_Drawdown = Daily_Drawdown.cummin() 
      end subroutine
