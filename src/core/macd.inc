      parameter(jopen=2,jhigh=3,jlow=4,jclose=5,jpdiv=6,jplog=7)
      parameter(jstema=8, jlgema=9,jtslpema=10)
      parameter(jret=11, jnav=12)
      parameter(jcrs=13, jrst=14,jnavs=15)
      parameter(isht=1, ilng=2)

  ! irdx  :  index of I/O file
  ! iwrx  :  index of I/O file for errors (0:stderr, 5:stdin, 6: stdout)
  ! jopen : column index of the open price in a bar
  ! jhigh : column index of the high price in a bar
  ! jclose: column index of the close price in a bar
  ! jlow  : column index of the low price in a bar
  ! jpdiv : column index of dividend reinvested in a bar
  ! jplog : column index of the log closing price
  ! jstema: column index of the short ema
  ! jlgema : column index of the long ema
  ! jtslpema : column index of the slope ema

  ! jret  : column index of the return of the dividend corrected price
  ! jnav  : column index of the nav (over 100) of the dividend corrected price

  ! jcrs  : column index of the channel break through indicator strategy
  ! jrst  : column index of the return of the channel break through (long)
  ! jnavs  : column index of the nav (over 100) of the strategy
  ! isht : index of the short parameter (1)
  ! ilng : index of the long parameter (2)
