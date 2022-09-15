      parameter(imax=8258,jmax=17, kmax=1)
      parameter(nn=8258,mm=5,kk=1,irdx=10,iwrx=0)
      parameter(jopen=1,jhigh=2,jlow=3,jclose=4,jpdiv=5, jemast=6, jemalg=7)
      parameter(jcrs=8, jret=9,jrbs=10,jrst=11,jnav=12,jlret=13)
      parameter(jyret=14, jsqr=15,jmtw=16,jyvol=17)
      parameter(nst=20, nlg=150)
      parameter(long=.true.)
      parameter(nyr = 252)

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
  ! jret  : column index of the short moving average
  ! jemalg: column index of the long moving average
  ! jcrs  : column index of the single cross indicator strategy
  ! jret  : column index of the return of the dividend corrected price
  ! jrbs  : column index of the asset return on 100 initial price
  ! jrst  : column index of the return of the strategy (long/short)
  ! jnav  : column index of the nav (over 100) of the strategy
  ! jlret : column index of the log of returns 
  ! jyret : column index of the yearly returns
  ! jsqr  : column index of the square of the return (r*r)
  ! jmtw  : column index of the second order moment of the returns
  ! jyvol : column index of the yearly volatility as stdev
  ! nst   : number of days for the short moving average
  ! nlg   : number of days for the long moving average
  ! nyr   : number of bars in a year