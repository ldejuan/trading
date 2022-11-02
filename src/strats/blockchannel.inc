      parameter(IMAX=10000,JMAX=13, KMAX=2)
      parameter(irdx=10,iwrx=0)
      parameter(jm=6, jdate=1, jopen=2,jhigh=3,jlow=4,jclose=5,jpdiv=6)
      parameter(jmaxhigh=7, jminlow=8)
      parameter(jret=9, jnav=10)
      parameter(jcrs=11, jrst=12,jnavs=13)
      parameter(nbbarshigh=26, nbbarslow=28)

  ! IMAX  : is the number of bars in the market data
  ! JMAX  : is the total number of properties of the simulation
  ! KMAX  : is the total number of assets in the simulation
  ! nn    : number of bars to read
  ! jm    : number of bars parameter to read (OHLCD+1 (date))
  ! irdx  :  index of I/O file
  ! iwrx  :  index of I/O file for errors (0:stderr, 5:stdin, 6: stdout)
  ! jdate : column index of the date as double : YYYYMMDD
  ! jopen : column index of the open price in a bar
  ! jhigh : column index of the high price in a bar
  ! jclose: column index of the close price in a bar
  ! jlow  : column index of the low price in a bar
  ! jpdiv : column index of dividend reinvested in a bar
  ! jminhigh: column index of the running minimum high prices
  ! jmaxlow : column index of the running maximum low prices

  ! jret  : column index of the return of the dividend corrected price
  ! jnav  : column index of the nav (over 100) of the dividend corrected price

  ! jcrs  : column index of the channel break through indicator strategy
  ! jrst  : column index of the return of the channel break through (long)
  ! jnavs  : column index of the nav (over 100) of the strategy
  ! nbbarshigh : column index of the look back window for moving min of high price
  ! nbbarslow : column index of the look back window for moving max of low price

