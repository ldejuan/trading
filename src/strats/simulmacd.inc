      parameter(imax=8258,jmax=14, kmax=1)
      parameter(nn=8258,mm=5,kk=1,irdx=10,iwrx=0)
      parameter(jopen=1,jhigh=2,jlow=3,jclose=4,jpdiv=5)
      parameter(jstema=6, jlgema=7,jtslpema=8)
      parameter(jret=9, jnav=10)
      parameter(jcrs=11, jrst=12,jnavs=13)
      parameter(nshort=26, nlong=100)

  ! imax  : is the number of bars in the market data
  ! jmax  : is the total number of properties of the simulation
  ! kmax  : is the total number of assets in the simulation
  ! nn    : number of bars to read
  ! mm    : number of bars parameter to read
  ! irdx  :  index of I/O file
  ! iwrx  :  index of I/O file for errors (0:stderr, 5:stdin, 6: stdout)
  ! kk    : depth index of the asset to read in a bar
  ! jopen : column index of the open price in a bar
  ! jhigh : column index of the high price in a bar
  ! jclose: column index of the close price in a bar
  ! jlow  : column index of the low price in a bar
  ! jpdiv : column index of dividend reinvested in a bar
  ! jstema: column index of the short ema
  ! jlgema : column index of the long ema
  ! jtslpema : column index of the slope ema

  ! jret  : column index of the return of the dividend corrected price
  ! jnav  : column index of the nav (over 100) of the dividend corrected price

  ! jcrs  : column index of the channel break through indicator strategy
  ! jrst  : column index of the return of the channel break through (long)
  ! jnavs  : column index of the nav (over 100) of the strategy
  ! nshort : column index of the short ema period
  ! nlong : column index of the long ema period

