/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */

/* We do support the IEC 559 math functionality, real and complex.  */

/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */

/* We do not support C11 <threads.h>.  */

C.----------------------------------------------------------------------
C.
C.    PHOTOS:   PHOton radiation in decays TeST program
C.
C.    Purpose:  Example of application of PHOTOS.
C.
C.    Input Parameters:   None
C.
C.    Output Parameters:  None
C.
C.    Author(s):  B. van Eijk, E. Barberio        Created at:  31/05/90
C.                                                Last Update: 05/06/90
C.
C.----------------------------------------------------------------------
      PROGRAM PHOTST
C      IMPLICIT NONE
      INTEGER EVENT,NHEP0
C this is the hepevt class in old style. No d_h_ class pre-name
      INTEGER NMXHEP
      PARAMETER (NMXHEP=10000)
      REAL*8  phep,  vhep ! to be real*4/ *8  depending on host
      INTEGER nevhep,nhep,isthep,idhep,jmohep,
     $        jdahep
      COMMON /hepevt/
     $      nevhep,               ! serial number
     $      nhep,                 ! number of particles
     $      isthep(nmxhep),   ! status code
     $      idhep(nmxhep),    ! particle ident KF
     $      jmohep(2,nmxhep), ! parent particles
     $      jdahep(2,nmxhep), ! childreen particles
     $      phep(5,nmxhep),   ! four-momentum, mass [GeV]
     $      vhep(4,nmxhep)    ! vertex [mm]
* ----------------------------------------------------------------------
      LOGICAL qedrad
      COMMON /phoqed/ 
     $     qedrad(nmxhep)    ! Photos flag
* ----------------------------------------------------------------------
      SAVE hepevt,phoqed
      INTEGER PHLUN
      COMMON/PHOLUN/PHLUN
C--
C--   Initialise PHOTOS
      CALL PHOINI
C--
C--   Loop over JETSET event until PHOTOS has generated one or more pho-
C--   tons.  Do this  for 10 JETSET events.  The event record is printed
C--   before and after photon emission.
      DO 20 EVENT=1,1 
        CALL LUEEVT(4,91.)
C--
C--   Conversion to /HEPEVT/ standard
        CALL LUHEPC(1)
C--
C--   Write event record before emission...
        NEVHEP=EVENT
        CALL PHODMP
        NHEP0=NHEP
C--
C--   Generate photon(s)... Arbitrary enforced generation.
C--   Normally line: IF (NHEP.EQ.NHEP0) GOTO 10 must be absent!
   10   CALL PHOTOS(1)
        IF (NHEP.EQ.NHEP0) GOTO 10
C--
C--   Write event record...
      WRITE(PHLUN,9050)
      WRITE(PHLUN,9040)
        CALL PHODMP
   20 CONTINUE
      WRITE(PHLUN,9000)
      WRITE(PHLUN,9010)
      WRITE(PHLUN,9020)
      WRITE(PHLUN,9030)
      WRITE(PHLUN,9020)
      WRITE(PHLUN,9010)
      STOP
 9000 FORMAT(1H1)
 9010 FORMAT(1H ,80('*'))
 9020 FORMAT(1H ,'*',78X,'*')
 9030 FORMAT(1H ,'**** PHOTOS Test Run has successfully ended',32X,
     &' ****')
 9040 FORMAT(1H ,26X,'=== after PHOTOS: ===')
 9050 FORMAT(1H0,80('='))
      END
