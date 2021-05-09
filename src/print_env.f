      subroutine print_env(ird, env, dates,ix,jx,kx)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  subrouutine to print_env.f
c
c  This function will print of the environement variabless to a 
c  open filename
c
c input 
c   ird      : integer      : index of the I/O to write to 
c   env      : real(ix,jx,kx): the simulation outputd
c   dates    : character(ix,kx)*10)  : dates for each assets/bars
c   ix       : integer      : number of rows in env 
c   jx       : integer      : number of proprietes in env:
c   kx       : integer      : number of assets to outputs 
c       / ird  : unit to read file
c output :
c    env      : real(nn,mm,kx)         : env variable for the simulation
c    dates    : character(nn,kx)*10    : dates for the simulation
c                                as a function of the assets
c     The function will insert the values in the  
c       env(i,j,kk)
c       dates(i,kk)
c
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      integer ird,ix,jx,kx,i,j,k
      real env(ix,jx,kx)
      character dates(ix,kx)*10

      do k=1,kx
        do i=1,ix
          write(*,*) i,dates(i,k),(env(i,j,k),j=1,jx)
        enddo
      enddo

      end subroutine
