      parameter(IMAX=10000,JMAX=20, IPERIODS=40)
      parameter(jm=5,irdx=10,iwrx=0)
      parameter(ishort=10, fshort=50, dshort=5)
      parameter(ilong=60, flong=260, dlong=10)
      parameter(nvars=2)


  ! IMAX  : is the number of bars in the market data
  ! JMAX  : is the total number of properties of the simulation
  ! jm    : number of fields to read (OHLCD)
  ! irdx  :  index of I/O file
  ! iwrx  :  index of I/O file for errors (0:stderr, 5:stdin, 6: stdout)
  ! ishort: initial  number of bars of the short ema
  ! fshort: final  number of bars of the short ema
  ! dshort: step size of the short ema
  ! ilong: initial  number of bars of the long ema
  ! flong: final  number of bars of the long ema
  ! dlong: step size of the long ema
  ! IPERIODS : number of maximum periods of the yearly schedule for risk optimisation
  ! nvars : number of parameters to vary
