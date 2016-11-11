c
c
c     ###################################################
c     ##  COPYRIGHT (C)  1992  by  Jay William Ponder  ##
c     ##              All Rights Reserved              ##
c     ###################################################
c
c     ################################################################
c     ##                                                            ##
c     ##  module usage  --  atoms active during energy computation  ##
c     ##                                                            ##
c     ################################################################
c
c
c     nuse   total number of active atoms in energy calculation
c     nprt   total number of atoms for printing of induced dipoles
c     iuse   numbers of the atoms active in energy calculation
c     iprt   numbers of the atoms with printed induced dipoles
c     use    true if an atom is active, false if inactive
c     prtind    true if an atom has its dipoles printed, false if not
c
c
      module usage
      implicit none
      integer nuse
      integer nprt
      integer, allocatable :: iuse(:)
      integer, allocatable :: iprt(:)
      logical, allocatable :: use(:)
      logical, allocatable :: prtind(:)
      save
      end
