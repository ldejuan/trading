      subroutine risks(retrn, vol, sharpe, calmar, prcs, n)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  risks.f : function to calculate risks of a strategy of prices from 1 to n 
c          . Total Return
c          . Volatility 
c          . Sharpe Ratio
c          . calmar
c inputs :
c     prcs: double precision(n)      : price time series   
c     n  :  integer :size of the input price vector
c outputs :
c     return  : total return over the period: 
c     vol  : volatility over the period
c     sharpe  : sharpe ratio (ret/vol)
c     calmar  : max ret / max drawdown 


      implicit none
      integer i, n
      double precision prcs(n), retrn, vol, sharpe, calmar, rets(n), rollmaxs(n), dds(n), maxdds(n)
      double precision ave, var, yrvolfc, drawdown, dd
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
c     data: double precision(n)      : price time series   
c     n  :  integer :size of the input price vector
c outputs :
c     drawdown  : double precision : maximum drawdonw over the period

      implicit none
      integer n, i
      double precision data(n), rollmax, dd, drawdown
      rollmax = 0.
      drawdown = 0. 
      do i=1,n
        rollmax = max(rollmax, data(i))
        dd = data(i)/rollmax - 1.
        drawdown = min(dd, drawdown)
      enddo
      return

      end function

      subroutine schedulerisks(riskv, schedule, IPERIODS, ipdrs, prcs, n)
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    yearlyrisks.f : subroutine to calculate the risks over a defined schedule 

c    inputs:
c     sched : integer(m,3) : schedule for the risk calculations. It is the 
c                            result of the gen_schedule functions
c     m     : integer     : number of periods
c     prcs: double precision(n) : price time series (or strategy nav)
c     n : double precision : size of the time series
c    outpout: 
c     riskv: double precision(m,4) : risks matrix : 
c            rows : for each year : ret, vol, sharpe, calmar
c    
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer n, ipdrs, jret, jvol, jsharpe,jcalmar,ii,il,i
      integer IPERIODS, schedule(IPERIODS,3)
      double precision riskv(IPERIODS,4), prcs(1:n)
      double precision dret, dvol,dsharpe,dcalmar

      parameter(jret=1, jvol=2, jsharpe=3, jcalmar=4)
      do i=1,ipdrs
        ii = schedule(i,1)
        il = schedule(i,2)
        call risks(riskv(i,jret), riskv(i,jvol), riskv(i,jsharpe), riskv(i,jcalmar), prcs(ii:il), il-ii+1)
      enddo

      end subroutine    

      subroutine gen_schedule(freq, ipds, schedule, IPERIODS, dates, in)

c  ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c     gen_schedule.f : subroutine to generate a schedule over a list of dates
c      at different frequencies
c
c     input :
c        freq : char :A,S,Q,M : annual, semi annual, quarter, monthly
c        IPERIODS : number of maxium periods in the schedule
c        dates : double precision : dates over which the schedule is generated
c        in  : integer    : length of the dates vector to consider
c     output:
c        schedule : integer(iPERIODS,3) : where IPERIODS 
c            schedule(i,1) date index of the start of the period
c            schedule(i,2) date index of the end of the period
c            schedule(i,3) last date in format YYYYMMDD as int of the period
c        ipds : integer : number of maximum periods to consider
c        
c
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer i,y,m,d, yp,ip,j,k,ipds,IPERIODS,in
      double precision dates(1:in)
      integer schedule(IPERIODS, 3)
      character freq

      if (freq .ne. 'A') then
        call logger ('error','Error only Annual')
        return
      end if
      call ddate2ymd(yp,m,d, dates(1))
      ip = 1
      j = 1
      do i=2, in
        call ddate2ymd(y,m,d,dates(i))
        if (y .ne. yp) then
c    do not forget the start of the period is at endofday of the previous date
          schedule(j,1)=max(ip-1,1)
          schedule(j,2)=i
          schedule(j,3)=int(dates(i))
          ip=i
          j=j+1
          yp=y
        end if
      end do

c ensure last

      if (y .eq. yp) then
        schedule(j,1)=ip-1
        schedule(j,2)=in
        schedule(j,3)=int(dates(in))
      end if
      ipds = j

      end subroutine
      
      subroutine diffvector(vs, v1s, v2s, n)
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
c   
c     cmprisks.f Function to take the difference between vectors
c   
c     inputs:
c     v1s: double precision(n) : vector of inputs  
c     v2s: double precision(n) : vector of inputs
c     output
c     vs : double precision(n) : v1s-v2s
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer n,i
      double precision vs(n), v1s(n), v2s(n)
      do i=1,n
        vs(i) = v1s(i)-v2s(i)
      end do

      end subroutine

      subroutine diffmatrix(ms, m1s, m2s, imax, jmax, n, m)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
c   
c     diffmatrix.f Function the element wise different of a matrix of size imax,jmax
c      over a submatrix of size (1:n, 1:m)
c   
c     inputs:
c     m1s: double precision(imax,jmax) : matrix of inputs  
c     m2s: double precision(imax,jmax) : matrix of inputs
c     output
c     ms : double precision(imax,jmax) : m1s-m2s
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
      implicit none
      integer n,m,i,j, imax, jmax
      double precision ms(imax,jmax), m1s(imax,jmax), m2s(imax,jmax)
      do i=1,n
        do j=1,m
          ms(i,j) = m1s(i,j)-m2s(i,j)
        end do  
      end do

      end subroutine