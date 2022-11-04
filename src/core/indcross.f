      subroutine indcross(long, i, ys, xlngs, xshts)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ema.f : function to calculate a single cross indicator of two moving
c        average : short and long from a given asset time series 
c
c equation : 
c     indcross(i) = 1 if xlngs(i) <= xshts(i)
c                   -1 if xshrts(i) > xlongs(i)
c     if long is set to TRUE long only strategies
c
c inputs :
c     long   : logical       :if .TRUE. long only strategy 
c     i      : integer       :row time bar to be calculated
c                            : environnement of the simulation
c     ys     : double precision(1:i)    : output indicator
c     xlgns  : double precision(1:i)    : time series of the long moving average
c     xshts  : double precision(1:i)    : time series of the short moving average
c     ix   : integer         : row dimension of the env variables
c                         (total number of bars)
c outputs :
c    the value of the ema at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer i
      logical long
      double precision alpha, ys(1:i), xlngs(1:i), xshts(1:i)
      if ( xlngs(i) .le. xshts(i) ) then 
        ys(i) = 1.
      else
        if (long .eqv. .true.) then
          ys(i) = 0.
        else
          ys(i) = -1.
        endif
      endif
      end subroutine
