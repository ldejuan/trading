      program test
      implicit none

      character date*10
      character line*100
      double precision values(6)
      integer i

      open(1,file = '../data/values.csv')
      read(1,'(A)') line
      date = line(1:10)
      read(line(11:),*) (values(i), i=1,4)
      write(*,*) 'reading from values.csv'
      write(*,*) line
      write(*,*) date
      write(*,*) values
      close(1)
      end

