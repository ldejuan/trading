c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   file firstorder.f 
c               File contains two subroutines
c               dfirstorder : calculation of first order discrete filter
c     from continous filter and the bilinar transformation on z space as
c
c                     z-1
c         F(p) = F(k -----)
c                     z+1     
c
c      where k is the frequency wrapping from continuous to discrete
c      if w_a is the continous 3db cut off pulsation and 
c         w_d is the district 3db cut off pulsation,
c         DT  is the sampling rate 
c 
c                    w_a
c         k =  ------------
c                   w_d DT 
c              tan( ------)
c                      2
c
c      The continous first order filter is  with cutoff rate t_a=1/w_a: 
c
c                             1
c              F(p) =  -------------
c                       1+ t_a * p
c                                              
c
c      in order to have THE SAME discret and continous cut of pulsation
c
c       w_a = w_d = 1/t_a then: 
c                     1
c           k=  --------------
c                           DT
c                t_a tan( ------)
c                           2 t_a
c
c      the filter is: 
c               1-u            u
c        s(n) = --- s(n-1) +  --- (e(n)+e(n-1))
c               1+u           1+u
c
c                DT
c        u = tan(--)
c               2 t_a
c
c     Implements the subroutines
c              dlowpassfo
c              dlowpassfocoefs
c

      subroutine dlowpassfo(a,b, i, ys, xs)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ema.f : function to calculate the first order discrete filter 
c           from the continous firstorder low pass pass
c           from a given asset time series
c
c equation : 
c         ys(i) = b * ys(i-1) + a *(xs(i)+xs(i-1))  
c
c
c inputs :
c     a : double precision  : coefficient numerator  
c     b : double precision  : coefficient of denominator of the transfer fonction
c     i : index of the time bar 
c     xs  : double precision   : row dimension of the inputs timeseries
c outputs :
c     ys  : double precision(ix) :vector of inputs : to store the output values
      implicit none
      integer i,i1
      double precision a,b, xs(1:i), ys(1:i)
c calculate previous bar
      i1=i-1
      if (i .eq. 1) then
        ys(i) = xs(i)
      else
        ys(i) = b*ys(i1) + a*(xs(i)+xs(i-1))
      endif
      end subroutine

      subroutine dlowpassfocoefs(a,b, dt, cutoff)  
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  dlowpassfocoef : function to calculate the coefficients of a 
c                    discrete first order low pass filter from
c                    continuous first order and bilinear transformation
c
c inoputs:
c     dt : double precision : sample rate in bars (generally =1 ) 
c     cutoff  : double precision :  cut off rate in bars
c outputs :
c     a : double precision  : coefficient numerator  
c     b : double precision  : coefficient of denominator of the transfer fonction

c
      implicit none
      double precision a,b, dt, cutoff, u

      u = dtan(dt/(2.*cutoff))
      a = u/(1.+u)
      b= (1-u)/(1+u)

      end subroutine
