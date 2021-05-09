      parameter(imax=2072,jmax=14, kmax=1)
      parameter(nn=2072,mm=5,kk=1,irdx=10,iwrx=0)
      parameter(jopen=1,jhigh=2,jlow=3,jclose=4,jpdiv=5, jemast=6, jemalg=7)
      parameter(jcrs=8, jret=9,jrbs=10,jrst=11,jnav=12,jlret=13)
      parameter(jyret=14)
      parameter(nst=20, nlg=100)
      parameter(long=.true.)
      parameter(nyr = 252)

  ! imax  : is the number of bars in the market data
  ! jmax  : is the total number of properties of the simulation
  ! kmax  : is the total number of assets in the simulation
  ! nn    : number of bars to read
  ! mm    : number of bars parameter to read
  ! irdx  : index of I/O file
  ! iwrx  : index of I/O file for errors (0:stderr, 5:stdin, 6: stdout)
  ! kk    : index of the asset to read in a bar
  ! jopen : index of the open price in a bar
  ! jhigh : index of the high price in a bar
  ! jclose: index of the close price in a bar
  ! jlow  : index of the low price in a bar
  ! jpdiv : index of dividend reinvested in a bar
  ! jemast: index of the short moving average
  ! jemalg: index of the long moving average
  ! jcrs  : iundex of the single cross strategy
  ! jret  : index of the price with dividend return
  ! jrbs  : index of the asset return on 100 initial price
  ! jrst  : index of the return of the strategy (long/short)
  ! jnav  : index of the nav (over 100) of the strategy
  ! jlret : index of the log of returns 
  ! jyret : index of the yearly returns 
  ! nst   : number of days for the short moving average
  ! nlg   : number of days for the long moving average
  ! nyr   : number of bars in a year
