      subroutine risks(retrn, vol, sharpe, calmar, prcs, n)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  risks.f : function to calculate risks of a strategy of prices from 1 to n 
c          . Total Return
c          . Volatility 
c          . Sharpe Ratio
c          . calmar
c inputs :
c     prcs: real(n)      : price time series   
c     n  :  integer :size of the input price vector
c outputs :
c     return  : total return over the period: 
c     vol  : volatility over the period
c     sharpe  : sharpe ratio (ret/vol)
c     calmar  : max ret / max drawdown 


      implicit none
      integer i, n, rets(n)
      real prcs(n), retrn, vol, sharpe, calmar, rollmaxs(n), dds(n), maxdds(n)
      real ave, var, yrvolfc, drawdown, dd
      external drawdown

      parameter(yrvolfc = 15.8745078664)

c calculate return
      retrn = prcs(n)/prcs(1) - 1.
      do i=1,n
        call ret(i, rets, prcs, n)
      end do

c calculate vol
      call avevar(rets, n, ave, var)
      vol = yrvolfc * sqrt(var)
      
c calculate sharpe
      if (vol .eq. 0) then
        sharpe = 0.
      else
        sharpe = retrn / vol
      end if      

c calculate the daily drawdown
      dd = drawdown(prcs,n)
      if (dd .eq. 0) then
        calmar = 0.
      else
         calmar = abs(retrn / dd)   
      end if
      end subroutine

      function drawdown(data, n)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  drawdown.f : function to calculate the max drawdown over a period 
c   inputs :
c     data: real(n)      : price time series   
c     n  :  integer :size of the input price vector
c outputs :
c     drawdown  : real : maximum drawdonw over the period

      implicit none
      integer n, i
      real data(n), rollmax, dd, drawdown
      rollmax = 0.
      drawdown = 0. 
      do i=1,n
        rollmax = max(rollmax, data(i))
        dd = data(i)/rollmax - 1.
        drawdown = min(dd, drawdown)
      enddo
      return

      end function

      subroutine schedulerisks(sched, riskv, m, prcs, n)
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    yearlyrisks.f : subroutine to calculate the risks over a defined schedule 

c    inputs:
c     sched : integer(m,3) : schedule for the risk calculations. It is the 
c                            result of the gen_schedule functions
c     m     : integer     : number of periods
c     prcs: real(n) : price time series (or strategy nav)
c     n : real : size of the time series
c    outpout: 
c     riskv: real(m,4) : risks matrix : 
c            rows : for each year : ret, vol, sharpe, calmar
c    
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer n, m, jret, jvol, jsharpe,jcalmar,ii,il,i,j
      integer IPERIODS, IMAX, JMAX
      common /allocation/ IPERIODS, IMAX, JMAX
      integer sched(IPERIODS,3)
      real riskv(IPERIODS,4), prcs(n)
      parameter(jret=1, jvol=2, jsharpe=3, jcalmar=4)
      j=1
      do i=1,m
        ii = sched(i,1)
        il = sched(i,2)
        call risks(riskv(j,jret), riskv(j,jvol), riskv(j,jsharpe), riskv(j,jcalmar), prcs(ii:il), il-ii+1)
        j = j+1
      enddo

      end subroutine    

      subroutine periodrisks(freq, riskv, m, prcs, n)
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    yearlyrisks.f : subroutine to calculate the risks over a period defined as a 
c           constant step 
c           The period is 252 trading days (1 year by default)
c    inputs:
c     freq : frequency in number of bars (generally days) of the risk calculation
c     prcs: real(n) : price time series (or strategy nav)
c     n : real : size of the time series
c    outpout: 
c     riskv: real(m,4) : risks matrix : 
c            rows : for each year : ret, vol, sharpe, calmar
c    
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
      implicit none
      integer n, m, jret, jvol, jsharpe,jcalmar,i,freq,nn, j
      real riskv(m,4), prcs(n)
      parameter(jret=1, jvol=2, jsharpe=3, jcalmar=4)
      j=1
      do i=1,n,freq
        nn = min(i+freq,n)
        call risks(riskv(j,jret), riskv(j,jvol), riskv(j,jsharpe), riskv(j,jcalmar), prcs(i:nn), nn-i+1)
        j = j+1
      enddo

      end subroutine

      subroutine gen_schedule(freq, schedule, sdates, ipds, in)
c  ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c     gen_schedule.f : subroutine to generate a schedule over a list of dates
c      at different frequencies
c
c     input :
c        freq : char :A,S,Q,M : annual, semi annual, quarter, monthly
c        sdates : character*10(in) : dates over which the schedule is generated
c        in  : integer    : length of the dates vector
c     output:
c        schedule : integer(iPERIODS,3) : where 
c            schedule(i,1) date index of the start of the period
c            schedule(i,2) date index of the end of the period
c            schedule(i,3) last date in format YYYYMMDD as int of the period
c        ipds : integer : number of maximum periods to consider
c        
c
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer i,y,m,d, yp,ip,j,k,ipds,IPERIODS,in
      character*10 sdates(1:in)
      integer date2int
      character freq
      external date2int
      parameter(IPERIODS = 20)
      integer schedule(IPERIODS, 3)


      if (freq .ne. 'A') then
        write (*,*) 'Error only Annual'
        return
      end if
      call date2ymd(yp,m,d, sdates(1))
      ip = 1
      j = 1
      do i=2, in
        call date2ymd(y,m,d,sdates(i))
        if (y .ne. yp) then
c    do not forget the start of the period is at endofday of the previous date
          schedule(j,1)=max(ip-1,1)
          schedule(j,2)=i
          schedule(j,3)=date2int(sdates(i))
          ip=i
          j=j+1
          yp=y
        end if
      end do

c ensure last

      if (y .eq. yp) then
        schedule(j,1)=ip-1
        schedule(j,2)=in
        schedule(j,3)=date2int(sdates(in))
      end if
      ipds = j

      end subroutine


      
      subroutine diffvector(vs, v1s, v2s, n)
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
c   
c     cmprisks.f Function to take the difference between vectors
c   
c     inputs:
c     v1s: real(n) : vector of inputs  
c     v2s: real(n) : vector of inputs
c     output
c     vs : real(n) : v1s-v2s
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer n,i
      real vs(n), v1s(n), v2s(n)
      do i=1,n
        vs(i) = v1s(i)-v2s(i)
      end do

      end subroutine

      subroutine diffmatrix(ms, m1s, m2s, n, m)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
c   
c     diffmatrix.f Function the element wise different
c   
c     inputs:
c     m1s: real(n,m) : matrix of inputs  
c     m2s: real(n,m) : matrix of inputs
c     output
c     ms : real(n,m) : m1s-m2s
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
      implicit none
      integer n,m,i,j
      real ms(n,m), m1s(n,m), m2s(n,m)
      do i=1,n
        do j=1,m
          ms(i,j) = m1s(i,j)-m2s(i,j)
        end do  
      end do

      end subroutine