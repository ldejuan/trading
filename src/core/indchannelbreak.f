      subroutine indchannelbreak(i, ys, xmins, xmaxs, xhighs, xlows, ix)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  indchanelbreak : function to calculate a the channel break through indicator for long strategies
c
c
c inputs :
c     i         : integer       :row time bar to be calculated
c     xhighs    : real(ix)      : time series of highs price 
c     xlows     : real(ix)      : time series of lows price 
c     xmins     : real(1:ix)    : time series of moving minimun of high prices
c     xmaxs     : real(1:ix)    : time series of moving maximun of low prices
c     ix        : integer         : row dimension of the env variables
c                         (total number of bars)
c outputs :
c     ys     : real(1:ix)    : output indicator position at the bar (1: long, 0: no position)  
c    the value of the ema at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer i, ix,i1
      real ys(ix), xs(ix), xmins(ix), xmaxs(ix), xhighs(ix), xlows(ix)
      i1 = i-1 
      if (i .eq. 1) then
        ys(i) = 0.
      else
        ys(i) = ys(i1)
        if ( (ys(i1) .eq. 0.) .and. (xhighs(i) .gt. xmins(i1)) )  then 
          ys(i) = 1.
        endif
        if ( (ys(i1) .eq. 1.) .and. (xlows(i) .lt. xmaxs(i1)) )  then 
          ys(i) = 0.
        endif
      endif
      end subroutine

