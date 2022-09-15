      parameter(imax=8258,jmax=6, kmax=1)
      parameter(nn=8258,mm=5,kk=1,irdx=10,iwrx=0)
      parameter(jopen=1,jhigh=2,jlow=3,jclose=4,jpdiv=5, jret=6)
      parameter(npole=16,ndepth=40, nperiod=200)


  ! imax  : is the number of bars in the market data rows
  ! jmax  : is the total number of properties of the simulation columns
  ! kmax  : is the total number of assets in the simulation depth
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
  ! jret  : column index of the pdiv asset return
  ! npole  : number of poles for the calculation of entropy spectrum
  ! ndepth : depth of the data to compute the spectrum
  ! nperiod : max period (in bars) for the calculation of the spectrum (from nperiods to 2 Nyquist Frequency)
