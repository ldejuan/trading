      subroutine indmacd(i, ys, xlngs, xshts, xflts)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  indmacd : function to calculate a the macd indicator for long strategies
c                   at time bar i macd > 0 && SLOPE (xflts(1:i) - xflts(ix-1)> 0 : BUY
c                    If the LowPrice < Moving Average of Max Low Price : SELL 
c
c inputs :
c     i         : integer       :row time bar to be calculated
c     xlngs     : real(1:i)    : time series of long period EMA
c     xshts     : real(1:i)    : time series of short period EMA
c     xflts     : real(1:i)      : time series of EMA for slope calculation   
c                      
c     ys     : real(1:i)    : output indicator position at the bar (1: long, 0: no position)  
c    the value of the ema at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer i, ix,i1
      real ys(1:i), xlngs(1:i), xshts(1:i), xflts(1:i), macd
      if (i .eq. 1) then 
          ys(i) = 0.
      else
        ys(i) = ys(i-1)
        macd = xshts(i) - xlngs(i) 
        if ((macd .ge. 0) .and. (xflts(i) .gt. xflts(i-1))) then
          ys(i) = 1.
        end if
        
        if (macd .lt. 0.) then
          ys(i) = 0.
        end if
      end if


c     write(*,*) 'i=',i,'ema short=',xshts(i),'ema long=',xlngs(i), 'macd=', macd, 'slope=',slope, 'position=', ys(i), 'xflts=', xflts(i)
      end subroutine

