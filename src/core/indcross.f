      subroutine indcross(long, i, ys, xlngs, xshts, ix)
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
c     ys     : real(1:ix)    : output indicator
c     xlgns  : real(1:ix)    : time series of the long moving average
c     xshts  : real(1:ix)    : time series of the short moving average
c     ix   : integer         : row dimension of the env variables
c                         (total number of bars)
c outputs :
c    the value of the ema at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer i, ix
      logical long
      real alpha, ys(ix), xlngs(ix), xshts(ix)
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

      subroutine indcross1(long, jst, jlg, jo, ib, ka, env, ix, jx, kx)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ema.f : function to calculate a single cross indicator of two moving
c        average : short and long
c           from a given asset time series
c
c equation : 
c     indcross(i) = 1 if ema_long(i) <= ema_short(i)
c                   -1 if ema_long(i) > ema_short(i)
c     if long is set to TRUE long only strategies
c
c inputs :
c     long : logical   :if .TRUE. long only strategy 
c     jst  : integer   :column index of the moving average shorte 
c     jlg  : integer   :column index of the moving average long 
c     jo   : integer   :column index of the output (where the index will be
c     ib   : integer   :row time bar to be calculated
c     ka   : integer   :depth index of the asset stored
c     env  : real(ix,jx,kx)
c                      : environnement of the simulation
c     ix   : integer   : row dimension of the env variables
c                         (total number of bars)
c     jx   : integer   : colunm dimension of the env variable
c                         (total number of properties)
c     kx   : integer   : depth dimenstion of the env variable
c                         (total number of assets)
c outputs :
c    the value of the ema at ib is stored in the jo, ka
c    position of the env variable
      implicit none
      integer ib,ka,jst,jlg,jo,ix,jx,kx
      logical long
      real alpha, env(ix,jx,kx)
      if ( env(ib,jlg,ka) .le. env(ib,jst,ka) ) then 
        env(ib,jo,ka) = 1.
      else
        if (long .eqv. .true.) then
          env(ib,jo,ka) = 0.
        else
          env(ib,jo,ka) = -1.
        endif
      endif
      end subroutine
