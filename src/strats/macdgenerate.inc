      parameter(imax=8258,jmax=13, kmax=1)
      parameter(nn=8258,mm=5,kk=1,irdx=10,iwrx=0)
      parameter(ishort=10, fshort=50, dshort=5)
      parameter(ilong=60, flong=260, dlong=10)
      parameter(nperiods=40, nvars=2)


  ! imax  : is the number of bars in the market data
  ! jmax  : is the total number of properties of the simulation
  ! kmax  : is the total number of assets in the simulation
  ! nn    : number of bars to read
  ! mm    : number of fields to read (OHLCD)
  ! irdx  :  index of I/O file
  ! iwrx  :  index of I/O file for errors (0:stderr, 5:stdin, 6: stdout)
  ! ishort: initial  number of bars of the short ema
  ! fshort: final  number of bars of the short ema
  ! dshort: step size of the short ema
  ! ilong: initial  number of bars of the long ema
  ! flong: final  number of bars of the long ema
  ! dlong: step size of the long ema
  ! nperiods : number of periods of the yearly schedule for risk optimisation
  ! nvars : number of parameters to vary
