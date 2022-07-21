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

      PROGRAM TAUDEM
C     **************
C NOTE THAT THE ROUTINES ARE NOT LIKE IN CPC DECK THIS IS HISTORICAL !!
C=======================================================================
C====================== DECTES    : TEST OF TAU DECAY LIBRARY===========
C====================== KTORY = 1 : INTERFACE OF KORAL-Z TYPE ==========
C====================== KTORY = 2 : INTERFACE OF KORAL-B TYPE =========
C=======================================================================
C     COMMON  /PAWC/ BLAN(10000)
      COMMON  / / BLAN(10000)
      CHARACTER*7 DNAME
      COMMON / INOUT / INUT,IOUT
      DNAME='KKPI'
!      CALL GLIMIT(20000)
!      CALL GOUTPU(16)
      INUT=5
      IOUT=6
      OPEN(IOUT,FILE="./tauola.output")
       OPEN(INUT,FILE="./dane.dat")
      KTORY=1
      CALL DECTES(KTORY)
      KTORY=2
      CALL DECTES(KTORY)
      END
      SUBROUTINE DECTES(KTORY)
C     ************************
      REAL POL(4)
      DOUBLE PRECISION HH(4)
C SWITCHES FOR TAUOLA;
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / IDFC  / IDFF
C I/O UNITS  NUMBERS
      COMMON / INOUT /  INUT,IOUT
C LUND TYPE IDENTIFIER FOR A1
      COMMON / IDPART / IA1
C /PTAU/ IS USED IN ROUTINE TRALO4
      COMMON /PTAU/ PTAU
      COMMON / TAURAD / XK0DEC,ITDKRC
      REAL*8            XK0DEC
      COMMON /TESTA1/ KEYA1
C special switch for tests of dGamma/dQ**2 in a1 decay
C KEYA1=1 constant width of a1 and rho
C KEYA1=2 free choice of rho propagator (defined in function FPIK)
C         and free choice of a1 mass and width. function g(Q**2)
C         (see formula 3.48 in Comp. Phys. Comm. 64 (1991) 275)
C         hard coded both in Monte Carlo and in testing distribution.
C KEYA1=3 function g(Q**2) hardcoded in the Monte Carlo
C         (it is timy to calculate!), but appropriately adjusted in
C         testing distribution.
C-----------------------------------------------------------------------
C          INITIALIZATION
C-----------------------------------------------------------------------
C======================================
      NINP=INUT
      NOUT=IOUT
 3000 FORMAT(A80)
 3001 FORMAT(8I2)
 3002 FORMAT(I10)
 3003 FORMAT(F10.0)
      IF (KTORY.EQ.1) THEN
      READ( NINP,3000) TESTIT
      WRITE(NOUT,3000) TESTIT
      READ( NINP,3001) KAT1,KAT2,KAT3,KAT4,KAT5,KAT6
      READ( NINP,3002) NEVT,JAK1,JAK2,ITDKRC
      READ( NINP,3003) PTAU,XK0DEC
      ENDIF
C======================================
C control output
      WRITE(NOUT,'(6A6/6I6)')
     $ 'KAT1','KAT2','KAT3','KAT4','KAT5','KAT6',
     $  KAT1 , KAT2 , KAT3 , KAT4 , KAT5 , KAT6
      WRITE(NOUT,'(4A12/4I12)')
     $  'NEVT','JAK1','JAK2','ITDKRC',
     $   NEVT,  JAK1 , JAK2 , ITDKRC
      WRITE(NOUT,'(2A12/2F12.6)')
     $ 'PTAU','XK0DEC',
     $  PTAU , XK0DEC
C======================================
      JAK=0
C      JAK1=5
C      JAK2=5
C LUND IDENTIFIER (FOR TAU+) -15
      IF (KTORY.EQ.1) THEN
        IDFF=-15
      ELSE
        IDFF= 15
      ENDIF
C KTO=1 DENOTES TAU DEFINED BY IDFF (I.E. TAU+)
C KTO=2 DENOTES THE OPPOSITE        (I.E. TAU-)
      KTO=2
      IF (KTO.NE.2) THEN
        PRINT *, 'for the sake of these tests KTO has to be 2'
        PRINT *, 'to change tau- to tau+ change IDFF from -15 to 15'
        STOP
      ENDIF
C TAU POLARIZATION IN ITS RESTFRAME;
      POL(1)=0.
      POL(2)=0.
      POL(3)=.9
C TAU MOMENTUM IN GEV;
C      PTAU=CMSENE/2.D0
C NUMBER OF EVENTS TO BE GENERATED;
      NEVTES=10
      NEVTES=NEVT
      PRINT *, 'NEVTES= ',NEVTES
      WRITE(IOUT,7011) KEYA1
C
      IF (KTORY.EQ.1) THEN
         WRITE(IOUT,7001) JAK,IDFF,POL(3),PTAU
      ELSE
         WRITE(IOUT,7004) JAK,IDFF,POL(3),PTAU
      ENDIF
C INITIALISATION OF TAU DECAY PACKAGE TAUOLA
C ******************************************
        CALL INIMAS
        CALL INITDK


        CALL INIPHY(0.1D0)
      IF (KTORY.EQ.1) THEN
         CALL DEXAY(-1,POL)
      ELSE
         CALL DEKAY(-1,HH)
      ENDIF
C-----------------------------------------------------------------------
C          GENERATION
C-----------------------------------------------------------------------
      NEV=0
      DO 300 IEV=1,NEVTES
      NEV=NEV+1
C RESLU INITIALISE THE LUND RECORD
      CALL TAUFIL
C DECAY....
      IF (KTORY.EQ.1) THEN
         CALL DEXAY(KTO,POL)
      ELSE
         CALL DEKAY(KTO,HH)
         CALL DEKAY(KTO+10,HH)
      ENDIF
      CALL LUHEPC(2)
      IF(IEV.LE.44) THEN
       WRITE(IOUT,7002) IEV
       IF (KTORY.NE.1) THEN
         WRITE(IOUT,7003) HH
       ENDIF
C      CALL LULIST(11)
      CALL LULIST(2)
      ENDIF
      IPRI=MOD(NEV,1000)
      IF(IPRI.EQ.1) PRINT *, ' event no: ',NEV,' NEVTES: ',NEVTES
  300 CONTINUE
  301 CONTINUE
C-----------------------------------------------------------------------
C                     POSTGENERATION
C-----------------------------------------------------------------------
      IF (KTORY.EQ.1) THEN
         CALL DEXAY(100,POL)
      ELSE
         CALL DEKAY(100,HH)
      ENDIF
      RETURN
 7001 FORMAT(//4(/1X,15(5H=====))
     $ /,' ',     19X,'  TEST OF RAD. CORR IN ELECTRON DECAY   ',9X,1H ,
     $ /,' ',     19X,'    TESTS OF TAU DECAY ROUTINES         ',9X,1H ,
     $ /,' ',     19X,'    INTERFACE OF THE KORAL-Z TYPE       ',9X,1H ,
     $  2(/,1X,15(5H=====)),
     $ /,5X ,'JAK   =',I7  ,'  KEY DEFINING DECAY TYPE         ',9X,1H ,
     $ /,5X ,'IDFF  =',I7  ,'  LUND IDENTIFIER FOR FIRST TAU   ',9X,1H ,
     $ /,5X ,'POL(3)=',F7.2,'  THIRD COMPONENT OF TAU POLARIZ. ',9X,1H ,
     $ /,5X ,'PTAU  =',F7.2,'  THIRD COMPONENT OF TAU MOM. GEV ',9X,1H ,
     $  2(/,1X,15(5H=====))/)
 7002 FORMAT(///1X, '===== EVENT NO.',I4,1X,5H=====)
 7003 FORMAT(5X,'POLARIMETRIC VECTOR: ',
     $       7X,'HH(1)',7X,'HH(2)',7X,'HH(3)',7X,'HH(4)',
     $ /,    5X,'                     ', 4(1X,F11.8)   )
 7004 FORMAT(//4(/1X,15(5H=====))
     $ /,'  ',     19X,'  TEST OF RAD. CORR IN ELECTRON DECAY  ',9X,1H ,
     $ /,'  ',     19X,'    TESTS OF TAU DECAY ROUTINES        ',9X,1H ,
     $ /,'  ',     19X,'    INTERFACE OF THE KORAL-B TYPE      ',9X,1H ,
     $  2(/,1X,15(5H=====)),
     $ /,5X ,'JAK   =',I7  ,'  KEY DEFINING DECAY TYPE         ',9X,1H ,
     $ /,5X ,'IDFF  =',I7  ,'  LUND IDENTIFIER FOR FIRST TAU   ',9X,1H ,
     $ /,5X ,'POL(3)=',F7.2,'  THIRD COMPONENT OF TAU POLARIZ. ',9X,1H ,
     $ /,5X ,'PTAU  =',F7.2,'  THIRD COMPONENT OF TAU MOM. GEV ',9X,1H ,
     $  2(/,1X,15(5H=====))/)
 7011 FORMAT(///1X, '===== TYPE OF CURRENT',I4,1X,5H=====)
      END
      SUBROUTINE CHOICE(MNUM,RR,ICHAN,PROB1,PROB2,PROB3,
     $            AMRX,GAMRX,AMRA,GAMRA,AMRB,GAMRB)
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      AMROP=1.1
      GAMROP=0.36
      AMOM=.782
      GAMOM=0.0084
C     XXXXA CORRESPOND TO S2 CHANNEL !
      IF(MNUM.EQ.0) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =AMA1
       GAMRX=GAMA1
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMRO
       GAMRB=GAMRO
      ELSEIF(MNUM.EQ.1) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.57
       GAMRX=0.9
       AMRB =AMKST
       GAMRB=GAMKST
       AMRA =AMRO
       GAMRA=GAMRO
      ELSEIF(MNUM.EQ.2) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.57
       GAMRX=0.9
       AMRB =AMKST
       GAMRB=GAMKST
       AMRA =AMRO
       GAMRA=GAMRO
      ELSEIF(MNUM.EQ.3) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.27
       GAMRX=0.3
       AMRA =AMKST
       GAMRA=GAMKST
       AMRB =AMKST
       GAMRB=GAMKST
      ELSEIF(MNUM.EQ.4) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.27
       GAMRX=0.3
       AMRA =AMKST
       GAMRA=GAMKST
       AMRB =AMKST
       GAMRB=GAMKST
      ELSEIF(MNUM.EQ.5) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =1.27
       GAMRX=0.3
       AMRA =AMKST
       GAMRA=GAMKST
       AMRB =AMRO
       GAMRB=GAMRO
      ELSEIF(MNUM.EQ.6) THEN
       PROB1=0.4
       PROB2=0.4
       AMRX =1.27
       GAMRX=0.3
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMKST
       GAMRB=GAMKST
      ELSEIF(MNUM.EQ.7) THEN
       PROB1=0.0
       PROB2=1.0
       AMRX =1.27
       GAMRX=0.9
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMRO
       GAMRB=GAMRO
      ELSEIF(MNUM.EQ.8) THEN
       PROB1=0.0
       PROB2=1.0
       AMRX =AMROP
       GAMRX=GAMROP
       AMRB =AMOM
       GAMRB=GAMOM
       AMRA =AMRO
       GAMRA=GAMRO
      ELSEIF(MNUM.EQ.101) THEN
       PROB1=.35
       PROB2=.35
       AMRX =1.2
       GAMRX=.46
       AMRB =AMOM
       GAMRB=GAMOM
       AMRA =AMOM
       GAMRA=GAMOM
      ELSEIF(MNUM.EQ.102) THEN
       PROB1=0.0
       PROB2=0.0
       AMRX =1.4
       GAMRX=.6
       AMRB =AMOM
       GAMRB=GAMOM
       AMRA =AMOM
       GAMRA=GAMOM
      ELSE
       PROB1=0.0
       PROB2=0.0
       AMRX =AMA1
       GAMRX=GAMA1
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMRO
       GAMRB=GAMRO
      ENDIF
C
      IF    (RR.LE.PROB1) THEN
       ICHAN=1
      ELSEIF(RR.LE.(PROB1+PROB2)) THEN
       ICHAN=2
        AX   =AMRA
        GX   =GAMRA
        AMRA =AMRB
        GAMRA=GAMRB
        AMRB =AX
        GAMRB=GX
        PX   =PROB1
        PROB1=PROB2
        PROB2=PX
      ELSE
       ICHAN=3
      ENDIF
C
      PROB3=1.0-PROB1-PROB2
      END
      SUBROUTINE INITDK
* ----------------------------------------------------------------------
*     INITIALISATION OF TAU DECAY PARAMETERS  and routines
*
*     called by : KORALZ
* ----------------------------------------------------------------------

      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      PARAMETER (NMODE=15,NM1=0,NM2=1,NM3=8,NM4=2,NM5=1,NM6=3)
      COMMON / TAUDCD /IDFFIN(9,NMODE),MULPIK(NMODE)
     &                ,NAMES
      CHARACTER NAMES(NMODE)*31
      CHARACTER OLDNAMES(7)*31
      CHARACTER*80 bxINIT
      PARAMETER (
     $  bxINIT ='(1x,1h*,g17.8,            16x, a31,a4,a4, 1x,1h*)'
     $ )
      REAL*4 PI,POL1(4)
*
*
* LIST OF BRANCHING RATIOS
CAM normalised to e nu nutau channel
CAM                  enu   munu   pinu  rhonu   A1nu   Knu    K*nu   pi
CAM   DATA JLIST  /    1,     2,     3,     4,     5,     6,     7,
*AM   DATA GAMPRT /1.000,0.9730,0.6054,1.2432,0.8432,0.0432,O.O811,0.616
*AM
*AM  multipion decays
*
*    conventions of particles names
*                 K-,P-,K+,  K0,P-,KB,  K-,P0,K0
*                  3, 1,-3  , 4, 1,-4  , 3, 2, 4  ,
*                 P0,P0,K-,  K-,P-,P+,  P-,KB,P0
*                  2, 2, 3  , 3, 1,-1  , 1,-4, 2  ,
*                 ET,P-,P0   P-,P0,GM
*                  9, 1, 2  , 1, 2, 8
*
C
      DIMENSION NOPIK(6,NMODE),NPIK(NMODE)
*AM   outgoing multiplicity and flavors of multi-pion /multi-K modes    
      DATA   NPIK  /                4,                    4,  
     1                              5,                    5,
     2                              6,                    6,
     3                              3,                    3,            
     4                              3,                    3,            
     5                              3,                    3,            
     6                              3,                    3,  
     7                              2                         /         
      DATA  NOPIK / -1,-1, 1, 2, 0, 0,     2, 2, 2,-1, 0, 0,  
     1              -1,-1, 1, 2, 2, 0,    -1,-1,-1, 1, 1, 0,  
     2              -1,-1,-1, 1, 1, 2,    -1,-1, 1, 2, 2, 2, 
     3              -3,-1, 3, 0, 0, 0,    -4,-1, 4, 0, 0, 0,  
     4              -3, 2,-4, 0, 0, 0,     2, 2,-3, 0, 0, 0,  
     5              -3,-1, 1, 0, 0, 0,    -1, 4, 2, 0, 0, 0,  
     6               9,-1, 2, 0, 0, 0,    -1, 2, 8, 0, 0, 0,
C AJWMOD fix sign bug, 2/22/99
     7              -3,-4, 0, 0, 0, 0                         /
* LIST OF BRANCHING RATIOS
      NCHAN = NMODE + 7
      DO 1 I = 1,30
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I
        IF(I.EQ. 1) GAMPRT(I) =0.1800 
        IF(I.EQ. 2) GAMPRT(I) =0.1751 
        IF(I.EQ. 3) GAMPRT(I) =0.1110 
        IF(I.EQ. 4) GAMPRT(I) =0.2515 
        IF(I.EQ. 5) GAMPRT(I) =0.1790 
        IF(I.EQ. 6) GAMPRT(I) =0.0071 
        IF(I.EQ. 7) GAMPRT(I) =0.0134
        IF(I.EQ. 8) GAMPRT(I) =0.0450
        IF(I.EQ. 9) GAMPRT(I) =0.0100
        IF(I.EQ.10) GAMPRT(I) =0.0009
        IF(I.EQ.11) GAMPRT(I) =0.0004 
        IF(I.EQ.12) GAMPRT(I) =0.0003 
        IF(I.EQ.13) GAMPRT(I) =0.0005 
        IF(I.EQ.14) GAMPRT(I) =0.0015 
        IF(I.EQ.15) GAMPRT(I) =0.0015 
        IF(I.EQ.16) GAMPRT(I) =0.0015 
        IF(I.EQ.17) GAMPRT(I) =0.0005
        IF(I.EQ.18) GAMPRT(I) =0.0050
        IF(I.EQ.19) GAMPRT(I) =0.0055
        IF(I.EQ.20) GAMPRT(I) =0.0017 
        IF(I.EQ.21) GAMPRT(I) =0.0013 
        IF(I.EQ.22) GAMPRT(I) =0.0010 
        IF(I.EQ. 1) OLDNAMES(I)='  TAU-  -->   E-               '
        IF(I.EQ. 2) OLDNAMES(I)='  TAU-  -->  MU-               '
        IF(I.EQ. 3) OLDNAMES(I)='  TAU-  -->  PI-               '
        IF(I.EQ. 4) OLDNAMES(I)='  TAU-  -->  PI-, PI0          '
        IF(I.EQ. 5) OLDNAMES(I)='  TAU-  -->  A1- (two subch)   '
        IF(I.EQ. 6) OLDNAMES(I)='  TAU-  -->   K-               '
        IF(I.EQ. 7) OLDNAMES(I)='  TAU-  -->  K*- (two subch)   '
        IF(I.EQ. 8) NAMES(I-7)='  TAU-  --> 2PI-,  PI0,  PI+   '
        IF(I.EQ. 9) NAMES(I-7)='  TAU-  --> 3PI0,        PI-   '
        IF(I.EQ.10) NAMES(I-7)='  TAU-  --> 2PI-,  PI+, 2PI0   '
        IF(I.EQ.11) NAMES(I-7)='  TAU-  --> 3PI-, 2PI+,        '
        IF(I.EQ.12) NAMES(I-7)='  TAU-  --> 3PI-, 2PI+,  PI0   '
        IF(I.EQ.13) NAMES(I-7)='  TAU-  --> 2PI-,  PI+, 3PI0   '
        IF(I.EQ.14) NAMES(I-7)='  TAU-  -->  K-, PI-,  K+      '
        IF(I.EQ.15) NAMES(I-7)='  TAU-  -->  K0, PI-, K0B      '
        IF(I.EQ.16) NAMES(I-7)='  TAU-  -->  K-,  K0, PI0      '
        IF(I.EQ.17) NAMES(I-7)='  TAU-  --> PI0  PI0   K-      '
        IF(I.EQ.18) NAMES(I-7)='  TAU-  -->  K-  PI-  PI+      '
        IF(I.EQ.19) NAMES(I-7)='  TAU-  --> PI-  K0B  PI0      '
        IF(I.EQ.20) NAMES(I-7)='  TAU-  --> ETA  PI-  PI0      '
        IF(I.EQ.21) NAMES(I-7)='  TAU-  --> PI-  PI0  GAM      '
        IF(I.EQ.22) NAMES(I-7)='  TAU-  -->  K-  K0            '
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   1  CONTINUE
      DO I=1,NMODE
        MULPIK(I)=NPIK(I)
        DO J=1,MULPIK(I)
         IDFFIN(J,I)=NOPIK(J,I)
        ENDDO
      ENDDO
*
*
* --- COEFFICIENTS TO FIX RATIO OF:
* --- A1 3CHARGED/ A1 1CHARGED 2 NEUTRALS MATRIX ELEMENTS (MASLESS LIM.)
* --- PROBABILITY OF K0 TO BE KS
* --- PROBABILITY OF K0B TO BE KS
* --- RATIO OF COEFFICIENTS FOR K*--> K0 PI-
* --- ALL COEFFICENTS SHOULD BE IN THE RANGE (0.0,1.0)
* --- THEY MEANING IS PROBABILITY OF THE FIRST CHOICE ONLY IF ONE
* --- NEGLECTS MASS-PHASE SPACE EFFECTS
      BRA1=0.5
      BRK0=0.5
      BRK0B=0.5
      BRKS=0.6667
*

      GFERMI = 1.16637E-5
      CCABIB = 0.975
      GV     = 1.0
      GA     =-1.0



* ZW 13.04.89 HERE WAS AN ERROR
      SCABIB = SQRT(1.-CCABIB**2)
      PI =4.*ATAN(1.)
      GAMEL  = GFERMI**2*AMTAU**5/(192*PI**3)
*
*      CALL DEXAY(-1,pol1)
*
      RETURN
      END
      FUNCTION DCDMAS(IDENT)
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      IF      (IDENT.EQ. 1) THEN
        APKMAS=AMPI
      ELSEIF  (IDENT.EQ.-1) THEN
        APKMAS=AMPI
      ELSEIF  (IDENT.EQ. 2) THEN
        APKMAS=AMPIZ
      ELSEIF  (IDENT.EQ.-2) THEN
        APKMAS=AMPIZ
      ELSEIF  (IDENT.EQ. 3) THEN
        APKMAS=AMK
      ELSEIF  (IDENT.EQ.-3) THEN
        APKMAS=AMK
      ELSEIF  (IDENT.EQ. 4) THEN
        APKMAS=AMKZ
      ELSEIF  (IDENT.EQ.-4) THEN
        APKMAS=AMKZ
      ELSEIF  (IDENT.EQ. 8) THEN
        APKMAS=0.0001
      ELSEIF  (IDENT.EQ.-8) THEN
        APKMAS=0.0001
      ELSEIF  (IDENT.EQ. 9) THEN
        APKMAS=0.5488
      ELSEIF  (IDENT.EQ.-9) THEN
        APKMAS=0.5488
      ELSE
        PRINT *, 'STOP IN APKMAS, WRONG IDENT=',IDENT
        STOP
      ENDIF
      DCDMAS=APKMAS
      END
      FUNCTION LUNPIK(ID,ISGN)
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      REAL*4 XIO(1)
      IDENT=ID*ISGN
      IF      (IDENT.EQ. 1) THEN
        IPKDEF=-211
      ELSEIF  (IDENT.EQ.-1) THEN
        IPKDEF= 211
      ELSEIF  (IDENT.EQ. 2) THEN
        IPKDEF=111
      ELSEIF  (IDENT.EQ.-2) THEN
        IPKDEF=111
      ELSEIF  (IDENT.EQ. 3) THEN
        IPKDEF=-321
      ELSEIF  (IDENT.EQ.-3) THEN
        IPKDEF= 321
      ELSEIF  (IDENT.EQ. 4) THEN
*
* K0 --> K0_LONG (IS 130) / K0_SHORT (IS 310) = 1/1
        CALL RANMAR(XIO,1)
        IF (XIO(1).GT.BRK0) THEN
          IPKDEF= 130
        ELSE
          IPKDEF= 310
        ENDIF
      ELSEIF  (IDENT.EQ.-4) THEN
*
* K0B--> K0_LONG (IS 130) / K0_SHORT (IS 310) = 1/1
        CALL RANMAR(XIO,1)
        IF (XIO(1).GT.BRK0B) THEN
          IPKDEF= 130
        ELSE
          IPKDEF= 310
        ENDIF
      ELSEIF  (IDENT.EQ. 8) THEN
        IPKDEF= 22
      ELSEIF  (IDENT.EQ.-8) THEN
        IPKDEF= 22
      ELSEIF  (IDENT.EQ. 9) THEN
        IPKDEF= 221
      ELSEIF  (IDENT.EQ.-9) THEN
        IPKDEF= 221
      ELSE
        PRINT *, 'STOP IN IPKDEF, WRONG IDENT=',IDENT
        STOP
      ENDIF
      LUNPIK=IPKDEF
      END



      SUBROUTINE TAURDF(KTO)
C THIS ROUTINE CAN BE CALLED BEFORE ANY TAU+ OR TAU- EVENT IS GENERATED
C IT CAN BE USED TO GENERATE TAU+ AND TAU- SAMPLES OF DIFFERENT
C CONTENTS
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      IF (KTO.EQ.1) THEN
C     ==================
C AJWMOD: Set the BRs for (A1+ -> rho+ pi0) and (K*+ -> K0 pi+)
      BRA1 = PKORB(4,1)
      BRKS = PKORB(4,3)
      BRK0  = PKORB(4,5)
      BRK0B  = PKORB(4,6)
      ELSE
C     ====
C AJWMOD: Set the BRs for (A1+ -> rho+ pi0) and (K*+ -> K0 pi+)
      BRA1 = PKORB(4,2)
      BRKS = PKORB(4,4)
      BRK0  = PKORB(4,5)
      BRK0B  = PKORB(4,6)
      ENDIF
C     =====
      END

      SUBROUTINE INIPHY(XK00)
* ----------------------------------------------------------------------
*     INITIALISATION OF PARAMETERS
*     USED IN QED and/or GSW ROUTINES
* ----------------------------------------------------------------------
      COMMON / QEDPRM /ALFINV,ALFPI,XK0
      REAL*8           ALFINV,ALFPI,XK0
      REAL*8 PI8,XK00
*
      PI8    = 4.D0*DATAN(1.D0)
      ALFINV = 137.03604D0
      ALFPI  = 1D0/(ALFINV*PI8)
      XK0=XK00
      END

      SUBROUTINE INIMAS
C ----------------------------------------------------------------------
C     INITIALISATION OF MASSES
C
C     called by : KORALZ
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
C IN-COMING / OUT-GOING  FERMION MASSES
      AMTAU  = 1.7842
C --- let us update tau mass ...
      AMTAU  = 1.777
      AMNUTA = 0.010
      AMEL   = 0.0005111
      AMNUE  = 0.0
      AMMU   = 0.105659 
      AMNUMU = 0.0
*
* MASSES USED IN TAU DECAYS
      AMPIZ  = 0.134964
      AMPI   = 0.139568
      AMRO   = 0.773
      GAMRO  = 0.145
*C    GAMRO  = 0.666
      AMA1   = 1.251
      GAMA1  = 0.599
      AMK    = 0.493667
      AMKZ   = 0.49772
      AMKST  = 0.8921
      GAMKST = 0.0513
C
C
C IN-COMING / OUT-GOING  FERMION MASSES
!!      AMNUTA = PKORB(1,2)
!!      AMNUE  = PKORB(1,4)
!!      AMNUMU = PKORB(1,6)
C
C MASSES USED IN TAU DECAYS  Cleo settings
!!      AMPIZ  = PKORB(1,7)
!!      AMPI   = PKORB(1,8)
!!      AMRO   = PKORB(1,9)
!!      GAMRO  = PKORB(2,9)
      AMA1   = 1.275   !! PKORB(1,10)
      GAMA1  = 0.615   !! PKORB(2,10)
!!      AMK    = PKORB(1,11)
!!      AMKZ   = PKORB(1,12)
!!      AMKST  = PKORB(1,13)
!!      GAMKST = PKORB(2,13)
C

      RETURN
      END
      SUBROUTINE TAUFIL
C     *****************
C SUBSITUTE OF tau PRODUCTION GENERATOR
C
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / IDFC  / IDFF
C positions of taus in the LUND common block
C it will be used by TAUOLA output routines.
      COMMON /TAUPOS / NPA,NPB
      DIMENSION XPB1(4),XPB2(4),AQF1(4),AQF2(4)
C
C --- DEFINING DUMMY EVENTS MOMENTA
      DO 4 K=1,3
        XPB1(K)=0.0
        XPB2(K)=0.0
        AQF1(K)=0.0
        AQF2(K)=0.0
  4   CONTINUE
        AQF1(4)=AMTAU
        AQF2(4)=AMTAU
C --- TAU MOMENTA
      CALL TRALO4(1,AQF1,AQF1,AM)
      CALL TRALO4(2,AQF2,AQF2,AM)
C --- BEAMS MOMENTA AND IDENTIFIERS
        KFB1= 11*IDFF/IABS(IDFF)
        KFB2=-11*IDFF/IABS(IDFF)
        XPB1(4)= AQF1(4)
        XPB1(3)= AQF1(4)
        IF(AQF1(3).NE.0.0)
     $  XPB1(3)= AQF1(4)*AQF1(3)/ABS(AQF1(3))
        XPB2(4)= AQF2(4)
        XPB2(3)=-AQF2(4)
        IF(AQF2(3).NE.0.0)
     $  XPB2(3)= AQF2(4)*AQF2(3)/ABS(AQF2(3))
C --- Position of first and second tau in LUND common
      NPA=3
      NPB=4
C --- FILL TO LUND COMMON
      CALL FILHEP(  1,3, KFB1,0,0,0,0,XPB1, AMEL,.TRUE.)
      CALL FILHEP(  2,3, KFB2,0,0,0,0,XPB2, AMEL,.TRUE.)
      CALL FILHEP(NPA,1, IDFF,1,2,0,0,AQF1,AMTAU,.TRUE.)
      CALL FILHEP(NPB,1,-IDFF,1,2,0,0,AQF2,AMTAU,.TRUE.)
      END
      SUBROUTINE TRALO4(KTO,P,Q,AM)
C     **************************
C SUBSITUTE OF TRALO4
      REAL  P(4),Q(4)
C
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON /PTAU/ PTAU
      AM=AMAS4(P)
      ETAU=SQRT(PTAU**2+AMTAU**2)
      EXE=(ETAU+PTAU)/AMTAU
      IF(KTO.EQ.2) EXE=(ETAU-PTAU)/AMTAU
      CALL BOSTR3(EXE,P,Q)
C ======================================================================
C         END OF THE TEST JOB
C ======================================================================
      END
      SUBROUTINE FILHEP(N,IST,ID,JMO1,JMO2,JDA1,JDA2,P4,PINV,PHFLAG)
C ----------------------------------------------------------------------
C this subroutine fills one entry into the HEPEVT common
C and updates the information for affected mother entries
C
C written by Martin W. Gruenewald (91/01/28)
C
C     called by : ZTOHEP,BTOHEP,DWLUxy
C ----------------------------------------------------------------------
C
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
C      PARAMETER (NMXHEP=2000)
C      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
C     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C      SAVE  /HEPEVT/
C      COMMON/PHOQED/QEDRAD(NMXHEP)
C      LOGICAL QEDRAD
C      SAVE /PHOQED/
      LOGICAL PHFLAG
C
      REAL*4  P4(4)
C
C check address mode
      IF (N.EQ.0) THEN
C
C append mode
        IHEP=NHEP+1
      ELSE IF (N.GT.0) THEN
C
C absolute position
        IHEP=N
      ELSE
C
C relative position
        IHEP=NHEP+N
      END IF
C
C check on IHEP
      IF ((IHEP.LE.0).OR.(IHEP.GT.NMXHEP)) RETURN
C
C add entry
      NHEP=IHEP
      ISTHEP(IHEP)=IST
      IDHEP(IHEP)=ID
      JMOHEP(1,IHEP)=JMO1
      IF(JMO1.LT.0)JMOHEP(1,IHEP)=JMOHEP(1,IHEP)+IHEP
      JMOHEP(2,IHEP)=JMO2
      IF(JMO2.LT.0)JMOHEP(2,IHEP)=JMOHEP(2,IHEP)+IHEP
      JDAHEP(1,IHEP)=JDA1
      JDAHEP(2,IHEP)=JDA2
C
      DO I=1,4
        PHEP(I,IHEP)=P4(I)
C
C KORAL-B and KORAL-Z do not provide vertex and/or lifetime informations
        VHEP(I,IHEP)=0.0
      END DO
      PHEP(5,IHEP)=PINV
C FLAG FOR PHOTOS...
      QEDRAD(IHEP)=PHFLAG
C
C update process:
      DO IP=JMOHEP(1,IHEP),JMOHEP(2,IHEP)
        IF(IP.GT.0)THEN
C
C if there is a daughter at IHEP, mother entry at IP has decayed
          IF(ISTHEP(IP).EQ.1)ISTHEP(IP)=2
C
C and daughter pointers of mother entry must be updated
          IF(JDAHEP(1,IP).EQ.0)THEN
            JDAHEP(1,IP)=IHEP
            JDAHEP(2,IP)=IHEP
          ELSE
            JDAHEP(2,IP)=MAX(IHEP,JDAHEP(2,IP))
          END IF
        END IF
      END DO
C
      RETURN
      END
