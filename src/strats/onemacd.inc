      parameter(IMAX=10000, JMAX=20, IPERIODS=40, jm=6,irdx=10,iwrx=6)
      parameter(jdate=1)
      parameter(short=15, long=70)
      parameter(nvars=2)

  ! IMAX        : is the number of bars in the market data
  ! JMAX        : is the total number of properties of the simulation
  ! IPERIODS    : is the total number of the schedule in the simulation
  ! jm          : number of fields to read (OHLCD)
  ! irdx        :  index of I/O file
  ! iwrx        :  index of I/O file for errors (0:stderr, 5:stdin, 6: stdout)
  ! short       :  short cutoff frequency
  ! long        :  long cutoff frequency
  ! nvars : number of parameters to vary
