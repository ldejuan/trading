      subroutine logger(level, text)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     logger subroutine
c     Will print in the iwr an logger message and set the
c     logical variable ier to .TRUE.
c     subroutine from s6 code
c     inputs: 
c       text : type character of variable lenght 
c     common /cerror/ iwr, ier 
c       iwr : index of the I/O File output
c       ier : logical : error message
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none

      character*(*) text
      character*(5) level 
      character*10 stime
      character*8 sdate
      integer h,n,s,mm

      logical ier
      integer iwr, ird

      common /coutput/ ier,iwr,ird
      
      ier = .TRUE.
      call date_and_time ( sdate, stime )

      write(iwr,'(*(a))') sdate,' ', stime(1:2),'h', stime(3:4),'m',stime(5:),' [', trim(level),'] ', trim(text)
      return

      end subroutine
