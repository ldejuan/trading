      subroutine indchannelbreak(i, ys, xmaxs, xmins, xhighs, xlows)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  indchanelbreak : function to calculate a the channel break through indicator for long strategies
c                   at time bar i If the HighPrice is > Moving average of Min of High Price : BUY
c                    If the LowPrice < Moving Average of Max Low Price : SELL 
c
c inputs :
c     i         : integer       :row time bar to be calculated
c     xmaxs     : real(1:i)    : time series of moving max of high prices
c     xmins     : real(1:i)    : time series of moving min of low prices
c     xhighs    : real(1:i)      : time series of high price 
c     xlows     : real(1:i)      : time series of low price 
c                         (total number of bars)
c outputs :
c     ys     : real(1:i)    : output indicator position at the bar (1: long, 0: no position)  
c    the value of the ema at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer i,i1
      real ys(1:i), xmaxs(1:i), xmins(1:i), xhighs(1:i), xlows(1:i)
      i1 = i-1 
      if (i .eq. 1) then
        ys(i) = 0.
      else
        ys(i) = ys(i1)
c    entry long
        if ((ys(i) .eq. 0) .and. (xhighs(i) .gt. xmaxs(i1))) then 
          ys(i) = 1.
        else
c    exit long
          if ((ys(i) .eq. 1) .and. (xlows(i) .lt. xmins(i1)))  then 
            ys(i) = 0.
          endif
        endif
      endif
c    write(*,*) 'i=',i,'low Price =',xlows(i),'Mov Max Low Price=',xmaxs(i1), 'High Price=', xhighs(i), 'Mov Min High Price=',xmins(i1), 'position', ys(i)
      end subroutine

