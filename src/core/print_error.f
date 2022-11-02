ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     print_error subroutine
c     Will print in the iwr an error message and set the
c     logical variable ier to .TRUE.
c     subroutine from s6 code
c     inputs: 
c       text : type character of variable lenght 
c     common /cerror/ iwr, ier 
c       iwr : index of the I/O File output
c       ier : logical : error message
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine print_error(text)
      implicit none

      character *(*) text
      logical ier
      integer iwr
      common /cerror/iwr,ier
      ier = .TRUE.
      write(iwr,'(a)') text
      return

      end subroutine

      subroutine logger(level, text)
      implicit none

      character*(*) text
      character*10 level

      logical ier
      integer iwr
      common /cerror/iwr,ier
      ier = .TRUE.
      write(iwr,'*(a)') level, text
      return
