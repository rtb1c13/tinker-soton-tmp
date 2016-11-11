c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1990  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ##############################################################
c     ##                                                          ##
c     ##  subroutine prtuxyz  --  output of induced dipoles        ##
c     ##                                                          ##
c     ##############################################################
c
c
c     "prtuxyz" writes out a set of induced dipoles, along with 
c     Cartesian coordinates of those induced dipoles,
c     to an external disk file
c
c
      subroutine prtuxyz (iind,iindxyz)
      use sizes
      use atomid
      use atoms
      use couple
      use files
      use inform
      use mpole
      use polar
      use titles
      use units
      use usage
      implicit none
      integer i,j,k,l,iind,iindxyz
      integer size,crdsiz
      real*8 crdmin,crdmax
      logical opened
      character*2 atmc
      character*2 crdc
      character*2 digc
      character*25 fstr
      character*120 indxyzf
      character*120 indfile
c
c
c     open the output units if not already done
c     (Shouldn't be necessary, currently only called from inside mdsave)
c
      inquire (unit=iindxyz,opened=opened)
      if (.not. opened) then
         indxyzf = filename(1:leng)//'.uindxyz'
         call version (indxyzf,'new')
         open (unit=iindxyz,file=indxyzf,status='new')
      end if
      inquire (unit=iind,opened=opened)
      if (.not. opened) then
         indfile = filename(1:leng)//'.uind'
         call version (indfile,'new')
         open (unit=iind,file=indfile,status='new')
      end if
c
c     check for large systems needing extended formatting
c
      atmc = 'i6'
      if (nprt .ge. 100000)  atmc = 'i7'
      if (nprt .ge. 1000000)  atmc = 'i8'
      crdmin = 0.0d0
      crdmax = 0.0d0
      do i = 1, nprt
         j=iprt(i)
         if (polarity(j) .ne. 0.0d0) then
            k = ipole(j)
            crdmin = min(crdmin,x(k),y(k),z(k))
            crdmax = max(crdmax,x(k),y(k),z(k))
         end if
      end do
      crdsiz = 6
      if (crdmin .le. -1000.0d0)  crdsiz = 7
      if (crdmax .ge. 10000.0d0)  crdsiz = 7
      if (crdmin .le. -10000.0d0)  crdsiz = 8
      if (crdmax .ge. 100000.0d0)  crdsiz = 8
      crdsiz = crdsiz + max(6,digits)
      size = 0
      call numeral (crdsiz,crdc,size)
      if (digits .le. 6) then
         digc = '6 '
      else if (digits .le. 8) then
         digc = '8'
      else
         digc = '10'
      end if
c
c     write out the number of atoms and the title
c
      if (ltitle .eq. 0) then
         fstr = '('//atmc//')'
         write (iindxyz,fstr(1:4))  nprt
      else
         fstr = '('//atmc//',2x,a)'
         write (iindxyz,fstr(1:9))  nprt,title(1:ltitle)
      end if
c
c     write out the coordinate line for each atom to be printed
c
      fstr = '('//atmc//',2x,a3,3f'//crdc//
     &          '.'//digc//',i6,8'//atmc//')'
      do i = 1, nprt
         j = iprt(i)
         if (polarity(j) .ne. 0.0d0) then
            k = ipole(j)
            write (iindxyz,fstr)  k,name(k),x(k),y(k),z(k),type(k),
     &                      (i12(l,k),l=1,n12(k))
         end if
      end do
c
c     do the same for the induced dipole printing
c
      write (iind,10)  nprt,title(1:ltitle)
   10 format (i6,2x,a)
      do i = 1, nprt
         j = iprt(i)
         if (polarity(j) .ne. 0.0d0) then
            k = ipole(j)
            write (iind,20)  k,name(k),(debye*uind(l,j),l=1,3)
   20       format (i6,2x,a3,3f12.6)
         end if
      end do

c
c     close the output unit if opened by this routine
c
      if (.not. opened)  close (unit=iind)
      if (.not. opened)  close (unit=iindxyz)
      return
      end
