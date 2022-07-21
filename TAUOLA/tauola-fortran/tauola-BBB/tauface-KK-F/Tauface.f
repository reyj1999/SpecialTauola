*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  !!!!!!! WARNING!!!!!   This source is agressive !!!!                           //
*//                                                                                 //
*//  Due to short common block names it  owerwrites variables in other parts        //
*//  of the code.                                                                   //
*//                                                                                 //
*//  One should add suffix c_Taul_ to names of all commons as soon as possible!!!!  //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////

*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Standard Tauola interface/initialization routines of functionality exactly    //
*//   as in Tauola CPC  but input is partially from xpar(*) matrix                  //
*//   ITAUXPAR is for indirect adressing                                            //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////


      SUBROUTINE INIETC(ITAUXPAR,xpar)
      INCLUDE "BXformat.h"
      REAL*8 xpar(*)
      INTEGER   INUT,IOUT
      COMMON /INOUT/  
     $     INUT,         ! Input  unit  number (not used)
     $     IOUT          ! Ounput unit  number
      COMMON / IDFC  / IDFF
      COMMON / TAURAD / XK0DEC,ITDKRC
      DOUBLE PRECISION            XK0DEC
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
* Note: I dont see KeyA1=2,3 realy implemented in the code SJ. ??????
      INTEGER  KeyA1
      COMMON /TESTA1/
     $     KeyA1           ! Special switch for tests of dGamma/dQ**2 in a1 decay
* KeyA1=1 constant width of a1 and rho
* KeyA1=2 free choice of rho propagator (defined in function FPIK)
*         and free choice of a1 mass and width. function g(Q**2)
*         (see formula 3.48 in Comp. Phys. Comm. 64 (1991) 275)
*         hard coded both in Monte Carlo and in testing distribution.
* KeyA1=3 function g(Q**2) hardcoded in the Monte Carlo
*         (it is timy to calculate!), but appropriately adjusted in testing distribution.
      SAVE
       idff    = xpar(ITAUXPAR+3)        ! Lund identifier for first tau (15 for  tau-)
C XK0 for tau decays.
       xk0dec  = xpar(ITAUXPAR+5)        ! IR-cut for QED rad. in leptonic decays
C radiative correction switch in tau --> e (mu) decays !
       itdkRC  = xpar(ITAUXPAR+4)        ! QED rad. in leptonic decays
C switches of tau+ tau- decay modes !!
       Jak1            = xpar(ITAUXPAR+1)   ! Decay Mask for first tau
       Jak2            = xpar(ITAUXPAR+2)   ! Decay Mask for second tau
C output file number for TAUOLA
       IOUT    = xpar(4)
C  KeyA1 is used for formfactors actually not in use
      KeyA1   = xpar(ITAUXPAR+6)        ! Type of a1 current

        WRITE(iout,bxope)
        WRITE(iout,bxtxt) ' Parameters passed from KK  to Tauola:   '
        WRITE(iout,bxl1i) Jak1,      'dec. type 1-st tau  ','Jak1  ','t01'
        WRITE(iout,bxl1i) Jak2,      'dec. type 2-nd tau  ','Jak2  ','t02'
        WRITE(iout,bxl1i) KeyA1,     'current type a1 dec.','KeyA1 ','t03'
        WRITE(iout,bxl1i) idff,      'PDG id 1-st tau     ','idff  ','t04'
        WRITE(iout,bxl1i) itdkRC,    'R.c. switch lept dec','itdkRC','t05'
        WRITE(iout,bxl1g) xk0dec,    'IR-cut for lept r.c.','xk0dec','t06'
        WRITE(iout,bxclo)

      end

      SUBROUTINE INITDK(ITAUXPAR,xpar)
* ----------------------------------------------------------------------
*     INITIALISATION OF TAU DECAY PARAMETERS  and routines
*
*     called by : KORALZ
* ----------------------------------------------------------------------
      INCLUDE "BXformat.h"
      INTEGER   INUT,IOUT
      COMMON /INOUT/  
     $     INUT,         ! Input  unit  number (not used)
     $     IOUT          ! Ounput unit  number
      REAL*8 xpar(*)

      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / TAUBRA / GAMPRT(500),JLIST(500),NCHAN
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS

      PARAMETER (NMODE=86,NM1=0,NM2=11,NM3=19,NM4=22,NM5=21,NM6=13)
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
      DIMENSION NOPIK(9,NMODE),NPIK(NMODE)
*AM   outgoing multiplicity and flavors of multi-pion /multi-K modes    
      DATA   NPIK  /                4,                    4,    ! old 4scalar
     a                              4,                    4,    ! new (may 2004)
     b                              4,                    4,
     c                              4,                    4,
     d                              4,                    4,
     e                              4,                    4,    ! new (may 2004)
     e                              4,                    4,    ! new (sep 2004)
     e                              4,                    4,    
     e                              4,                    4,    
     e                              4,                    4,    
     e                              4,                    4,    ! new (sep 2004)
     1                              5,       
     a                              5,                    5,    ! new (may 2004)
     b                              5,                    5,
     c                              5,                    5,
     d                              5,                    5,
     e                              5,                    5,    ! new (may 2004)
     a                              5,                    5,    ! new (sep 2004)
     b                              5,                    5,
     c                              5,                    5,
     d                              5,                    5,
     e                              5,                    5,    ! new (sep 2004)
     x                                                    5,    ! old npi starts here
     2                              6,                    6,
     a                              6,                    6,    ! new (may 2004)
     b                              6,                    6,    ! new (may 2004)
     c                              6,                    6,    ! new (may 2004)
     d                              6,                    6,    ! new (may 2004)
     e                              6,                    6,    ! new (may 2004)
     3                              3,                    3,            
     4                              3,                    3,            
     5                              3,                    3,            
     6                              3,                    3,  
     7                              3,                          ! new (may 2004) and useful
     a                              3,                    3,    ! new (may 2004)
     a                              3,                    3,    ! new (may 2004)
     a                              3,                    3,    ! new (may 2004)
     a                              3,                    3,    ! new (may 2004)
     a                              3,                    3,    ! new (may 2004)
     8                                                    2, 
     9                              2,                    2,    ! new (may 2004)
     9                              2,                    2,    ! new (may 2004)
     9                              2,                    2,    ! new (may 2004)
     9                              2,                    2,    ! new (may 2004)
     9                              2,                    2/    ! new (may 2004)          

      DATA  NOPIK / -1,-1, 1, 2, 0, 0,3*0,     2, 2, 2,-1, 0, 0,3*0,  
     a               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (may 2004)
     b               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (may 2004)
     c               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (may 2004)
     d               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (may 2004)
     e               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (may 2004)
     a               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (sep 2004)
     b               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (sep 2004)
     c               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (sep 2004)
     d               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (sep 2004)
     e               4, 2, 2,-1, 0, 0,3*0,     4, 2, 2,-1, 0, 0,3*0,     ! new (sep 2004)
     1              -1,-1, 1, 2, 2, 0,3*0,  
     a              -1,-1, 1, 2, 4, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (may 2004)
     a               1,-1,-1, 2, 2, 0,3*0,    -1, 2, 2, 2, 2, 0,3*0,     ! new (may 2004)
     a              -1, 1, 1,-1,-1, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (may 2004)
     a              -1,-1, 1, 2, 4, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (may 2004)
     a              -1,-1, 1, 2, 4, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (may 2004)
     a              -1,-1, 1, 2, 4, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (sep 2004)
     a              -1,-1, 1, 2, 4, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (sep 2004)
     a              -1,-1, 1, 2, 4, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (sep 2004)
     a              -1,-1, 1, 2, 4, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (sep 2004)
     a              -1,-1, 1, 2, 4, 0,3*0,    -1,-1, 1, 2, 4, 0,3*0,     ! new (sep 2004)
     x                                        -1,-1,-1, 1, 1, 0,3*0,     ! old npi starts here
     2              -1,-1,-1, 1, 1, 2,3*0,    -1,-1, 1, 2, 2, 2,3*0, 
     a              -1,-1,-1, 1, 1, 1,3*0,    -1,-1, 1, 2, 2, 1,3*0,     ! new (may 2004)
     b              -1,-1,-1, 1, 1, 1,3*0,    -1,-1, 1, 2, 2, 1,3*0,     ! new (may 2004)
     c              -1,-1,-1, 1, 1, 1,3*0,    -1,-1, 1, 2, 2, 1,3*0,     ! new (may 2004)
     d              -1,-1,-1, 1, 1, 1,3*0,    -1,-1, 1, 2, 2, 1,3*0,     ! new (may 2004)
     e              -1,-1,-1, 1, 1, 1,3*0,    -1,-1, 1, 2, 2, 1,3*0,     ! new (may 2004)
     3              -3,-1, 3, 0, 0, 0,3*0,    -4,-1, 4, 0, 0, 0,3*0,  
     4              -3, 2,-4, 0, 0, 0,3*0,     2, 2,-3, 0, 0, 0,3*0,  
     5              -3,-1, 1, 0, 0, 0,3*0,    -1, 4, 2, 0, 0, 0,3*0,  
     6               9,-1, 2, 0, 0, 0,3*0,    -1, 2, 8, 0, 0, 0,3*0,


C AJWMOD fix sign bug, 2/22/99
     7               2, 2,-1, 0, 0, 0,3*0,                           ! new (may 2004) but useful
     7               2, 2, 2, 0, 0, 0,3*0,     2, 2, 2, 0, 0, 0,3*0, ! new (may 2004)
     7               2, 2, 2, 0, 0, 0,3*0,     2, 2, 2, 0, 0, 0,3*0, ! new (may 2004)
     7               2, 2, 2, 0, 0, 0,3*0,     2, 2, 2, 0, 0, 0,3*0, ! new (may 2004)
     7               2, 2, 2, 0, 0, 0,3*0,     2, 2, 2, 0, 0, 0,3*0, ! new (may 2004)
     7               2, 2, 2, 0, 0, 0,3*0,     2, 2, 2, 0, 0, 0,3*0, ! new (may 2004)

     8                                        -3,-4, 0, 0, 0, 0,3*0,
     8               -3,-3, 0, 0, 0, 0,3*0,   -3,-3, 0, 0, 0, 0,3*0, ! new (may 2004)
     8               -3,-3, 0, 0, 0, 0,3*0,   -3,-3, 0, 0, 0, 0,3*0, ! new (may 2004)
     8               -3,-3, 0, 0, 0, 0,3*0,   -3,-3, 0, 0, 0, 0,3*0, ! new (may 2004)
     8               -3,-3, 0, 0, 0, 0,3*0,   -3,-3, 0, 0, 0, 0,3*0, ! new (may 2004)
     8               -3,-3, 0, 0, 0, 0,3*0,   -3,-3, 0, 0, 0, 0,3*0 /! new (may 2004)


* LIST OF BRANCHING RATIOS
      NCHAN = NMODE + 7
      DO 1 I = 1,500
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I

        IF(I.EQ. 1) GAMPRT(I) = 1.0000
        IF(I.EQ. 2) GAMPRT(I) = 1.0000
        IF(I.EQ. 3) GAMPRT(I) = 1.0000
        IF(I.EQ. 4) GAMPRT(I) = 1.0000
        IF(I.EQ. 5) GAMPRT(I) = 1.0000
        IF(I.EQ. 6) GAMPRT(I) = 1.0000
        IF(I.EQ. 7) GAMPRT(I) = 1.0000
        IF(I.EQ. 8) GAMPRT(I) = 1.0000
        IF(I.EQ. 9) GAMPRT(I) = 1.0000
        IF(I.EQ.10) GAMPRT(I) = 1.0000
        IF(I.EQ.11) GAMPRT(I) = 1.0000
        IF(I.EQ.12) GAMPRT(I) = 1.0000
        IF(I.EQ.13) GAMPRT(I) = 1.0000
        IF(I.EQ.14) GAMPRT(I) = 1.0000
        IF(I.EQ.15) GAMPRT(I) = 1.0000
        IF(I.EQ.16) GAMPRT(I) = 1.0000
        IF(I.EQ.17) GAMPRT(I) = 1.0000
        IF(I.EQ.18) GAMPRT(I) = 1.0000
        IF(I.EQ.19) GAMPRT(I) = 1.0000
        IF(I.EQ.20) GAMPRT(I) = 1.0000
        IF(I.EQ.21) GAMPRT(I) = 1.0000
        IF(I.EQ.22) GAMPRT(I) = 1.0000
        IF(I.GT.22.AND.I.LE.93)  GAMPRT(I) = 1.0000
C second default
        IF(I.GT.0.AND.I.LE.93)  GAMPRT(I) = 0.0000
        IF(I.EQ. 1) GAMPRT(I) =0.1800 
        IF(I.EQ. 2) GAMPRT(I) =0.1751 
        IF(I.EQ. 3) GAMPRT(I) =0.1110 
        IF(I.EQ. 4) GAMPRT(I) =0.2515 
        IF(I.EQ. 5) GAMPRT(I) =0.1790 /2
        IF(I.EQ. 6) GAMPRT(I) =0.0071 
        IF(I.EQ. 7) GAMPRT(I) =0.0134
        IF(I.EQ. 8) GAMPRT(I) =0.0450
        IF(I.EQ. 9) GAMPRT(I) =0.0100

        IF(I.EQ.30) GAMPRT(I) =0.0009
        IF(I.EQ.33) GAMPRT(I) =0.004
        IF(I.EQ.34) GAMPRT(I) =0.002
        IF(I.EQ.35) GAMPRT(I) =0.001

        IF(I.EQ.51) GAMPRT(I) =0.0004 
        IF(I.EQ.52) GAMPRT(I) =0.0003 
        IF(I.EQ.53) GAMPRT(I) =0.0005 

        IF(I.EQ.64) GAMPRT(I) =0.0015 
        IF(I.EQ.65) GAMPRT(I) =0.0015 
        IF(I.EQ.66) GAMPRT(I) =0.0015 
        IF(I.EQ.67) GAMPRT(I) =0.0005
        IF(I.EQ.68) GAMPRT(I) =0.0050
        IF(I.EQ.69) GAMPRT(I) =0.0055
        IF(I.EQ.70) GAMPRT(I) =0.0017 
        IF(I.EQ.71) GAMPRT(I) =0.0013
        IF(I.EQ.72) GAMPRT(I) =0.1790 /2  

        IF(I.EQ.83) GAMPRT(I) =0.0010 

        IF(I.EQ. 1) OLDNAMES(I)='  TAU-  -->   E-               '
        IF(I.EQ. 2) OLDNAMES(I)='  TAU-  -->  MU-               '
        IF(I.EQ. 3) OLDNAMES(I)='  TAU-  -->  PI-               '
        IF(I.EQ. 4) OLDNAMES(I)='  TAU-  -->  PI-, PI0          '
        IF(I.EQ. 5) OLDNAMES(I)='  TAU-  -->  PI-, PI-,  PI+    '
        IF(I.EQ. 6) OLDNAMES(I)='  TAU-  -->   K-               '
        IF(I.EQ. 7) OLDNAMES(I)='  TAU-  -->  K*- (two subch)   '
        IF(I.EQ. 8) NAMES(I-7)='  TAU-  --> 2PI-,  PI0,  PI+   '
        IF(I.EQ. 9) NAMES(I-7)='  TAU-  --> 3PI0,        PI-   '

        IF(I.EQ.10) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.11) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.12) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.13) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.14) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.15) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.16) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.17) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.18) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)  
        IF(I.EQ.19) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (may 2004)
        IF(I.EQ.20) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)
        IF(I.EQ.21) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)
        IF(I.EQ.22) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)
        IF(I.EQ.23) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)
        IF(I.EQ.24) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)
        IF(I.EQ.25) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)
        IF(I.EQ.26) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)
        IF(I.EQ.27) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)
        IF(I.EQ.28) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)  
        IF(I.EQ.29) NAMES(I-7)='  TAU-  --> xxxxxxx4xxxxxxxx   '  !  (sep 2004)


        IF(I.EQ.30) NAMES(I-7)='  TAU-  --> 2PI-,  PI+, 2PI0   '

        IF(I.EQ.31) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (may 2004)
        IF(I.EQ.32) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (may 2004)
        IF(I.EQ.33) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (may 2004)
        IF(I.EQ.34) NAMES(I-7)='  TAU-  --> PI- 4PI0           '  !  (may 2004)
        IF(I.EQ.35) NAMES(I-7)='  TAU-  --> 3PI- 2PI+          '  !  (may 2004)
        IF(I.EQ.36) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (may 2004)
        IF(I.EQ.37) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (may 2004)
        IF(I.EQ.38) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (may 2004)
        IF(I.EQ.39) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (may 2004)
        IF(I.EQ.40) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (may 2004)

        IF(I.EQ.41) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.42) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.43) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.44) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.45) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.46) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.47) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.48) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.49) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)
        IF(I.EQ.50) NAMES(I-7)='  TAU-  --> xxxxxxxxx5xxxxxx   '  !  (sep 2004)

        IF(I.EQ.51) NAMES(I-7)='  TAU-  --> 3PI-, 2PI+,        '
        IF(I.EQ.52) NAMES(I-7)='  TAU-  --> 3PI-, 2PI+,  PI0   '
        IF(I.EQ.53) NAMES(I-7)='  TAU-  --> 2PI-,  PI+, 3PI0   '
        IF(I.EQ.54) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.55) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.56) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.57) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.58) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.59) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.60) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.61) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.62) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)
        IF(I.EQ.63) NAMES(I-7)='  TAU-  --> xxxxxxxxxnxxxxxx   '  !  (may 2004)

        IF(I.EQ.64) NAMES(I-7)='  TAU-  -->  K-, PI-,  K+      '
        IF(I.EQ.65) NAMES(I-7)='  TAU-  -->  K0, PI-, K0B      '

        IF(I.EQ.66) NAMES(I-7)='  TAU-  -->  K-,  K0, PI0      '

        IF(I.EQ.67) NAMES(I-7)='  TAU-  --> PI0  PI0   K-      '
        IF(I.EQ.68) NAMES(I-7)='  TAU-  -->  K-  PI-  PI+      '
        IF(I.EQ.69) NAMES(I-7)='  TAU-  --> PI-  K0B  PI0      '
        IF(I.EQ.70) NAMES(I-7)='  TAU-  --> ETA  PI-  PI0      '
        IF(I.EQ.71) NAMES(I-7)='  TAU-  --> PI-  PI0  GAM      '
        IF(I.EQ.72) NAMES(I-7)='  TAU-  --> PI-  PI0  PI0      '
        IF(I.EQ.73) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.74) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.75) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.76) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.77) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.78) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.79) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.80) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.81) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
        IF(I.EQ.82) NAMES(I-7)='  TAU-  --> xxxxxxxxx3xxxxxx   '  !  (may 2004)
 

        IF(I.EQ.83) NAMES(I-7)='  TAU-  -->  K-  K0            '
        IF(I.EQ.84) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.85) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.86) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.87) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.88) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.89) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.90) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.91) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.92) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
        IF(I.EQ.93) NAMES(I-7)='  TAU-  --> xxxxxxxxx2xxxxxx   '  !  (may 2004)
 
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
      BRA1=1d0   ! 0.5
      BRK0=0.5
      BRK0B=0.5
      BRKS=0.6667
*

      GFERMI = 1.16637E-5
      CCABIB = 0.975
      GV     = 1.0
      GA     =-1.0



      GFERMI = xpar(32)
      IF (XPAR(ITAUXPAR+100+1).GT.-1D0) THEN
C initialization form KK
        CCABIB = XPAR(ITAUXPAR+7)
        GV     = XPAR(ITAUXPAR+8)
        GA     = XPAR(ITAUXPAR+9)

        BRA1   = 1D0 ! XPAR(ITAUXPAR+10)   ! input is overruled; must be 1d0 now
        BRKS   = XPAR(ITAUXPAR+11)
        BRK0   = XPAR(ITAUXPAR+12)
        BRK0B  = XPAR(ITAUXPAR+13)
        DO K=1,NCHAN
C         GAMPRT(K)=XPAR(ITAUXPAR+100+K)

        IF(K.EQ. 1) GAMPRT(K) =XPAR(ITAUXPAR+100+K)
        IF(K.EQ. 2) GAMPRT(K) =XPAR(ITAUXPAR+100+K)
        IF(K.EQ. 3) GAMPRT(K) =XPAR(ITAUXPAR+100+K)
        IF(K.EQ. 4) GAMPRT(K) =XPAR(ITAUXPAR+100+K)
        IF(K.EQ. 5) GAMPRT(K) =XPAR(ITAUXPAR+100+K) /2
        IF(K.EQ. 6) GAMPRT(K) =XPAR(ITAUXPAR+100+K)
        IF(K.EQ. 7) GAMPRT(K) =XPAR(ITAUXPAR+100+K)
        IF(K.EQ. 8) GAMPRT(K) =XPAR(ITAUXPAR+100+K)
        IF(K.EQ. 9) GAMPRT(K) =XPAR(ITAUXPAR+100+K)

        IF(K.EQ.30) GAMPRT(K) =XPAR(ITAUXPAR+100+10)

        IF(K.EQ.51) GAMPRT(K) =XPAR(ITAUXPAR+100+11)
        IF(K.EQ.52) GAMPRT(K) =XPAR(ITAUXPAR+100+12)
        IF(K.EQ.53) GAMPRT(K) =XPAR(ITAUXPAR+100+13)

        IF(K.EQ.64) GAMPRT(K) =XPAR(ITAUXPAR+100+14)
        IF(K.EQ.65) GAMPRT(K) =XPAR(ITAUXPAR+100+15)
        IF(K.EQ.66) GAMPRT(K) =XPAR(ITAUXPAR+100+16)
        IF(K.EQ.67) GAMPRT(K) =XPAR(ITAUXPAR+100+17)
        IF(K.EQ.68) GAMPRT(K) =XPAR(ITAUXPAR+100+18)
        IF(K.EQ.69) GAMPRT(K) =XPAR(ITAUXPAR+100+19)
        IF(K.EQ.70) GAMPRT(K) =XPAR(ITAUXPAR+100+20)
        IF(K.EQ.71) GAMPRT(K) =XPAR(ITAUXPAR+100+21)
        IF(K.EQ.72) GAMPRT(K) =XPAR(ITAUXPAR+100+5) /2  

        IF(K.EQ.83) GAMPRT(K) =XPAR(ITAUXPAR+100+22)

        IF(K.EQ.33) GAMPRT(I) =XPAR(ITAUXPAR+100+23)
        IF(K.EQ.34) GAMPRT(I) =XPAR(ITAUXPAR+100+24)
        IF(K.EQ.35) GAMPRT(I) =XPAR(ITAUXPAR+100+25)

        ENDDO
      ENDIF
* ZW 13.04.89 HERE WAS AN ERROR
      SCABIB = SQRT(1.-CCABIB**2)
      PI =4.*ATAN(1.)
      GAMEL  = GFERMI**2*AMTAU**5/(192*PI**3)
*
*      CALL DEXAY(-1,pol1)
*
* PRINTOUTS FOR KK version

      SUM=0
      DO K=1,NCHAN
       SUM=SUM+GAMPRT(K)
      ENDDO

      
      WRITE(iout,bxope)
      WRITE(iout,bxtxt) ' TAUOLA Initialization SUBROUTINE INITDK:    '
      WRITE(iout,bxtxt) ' Adopted to read from KK                     '
      WRITE(iout,bxtxt) '                      '
      WRITE(iout,bxtxt) ' Choice Probability      --     Decay Channel'
      DO K=1,7      
      WRITE(iout,bxINIT) GAMPRT(K)/SUM,    OLDNAMES(K),'****','***'
      ENDDO
      DO K=8,7+NMODE      
      WRITE(iout,bxINIT) GAMPRT(K)/SUM,     NAMES(K-7),'****','***'
      ENDDO
      WRITE(iout,bxtxt) ' In addition:'
      WRITE(iout,bxINIT) GV,    'Vector W-tau-nu coupl.     ','****','***'
      WRITE(iout,bxINIT) GA,    'Axial  W-tau-nu coupl.     ','****','***'
      WRITE(iout,bxINIT) GFERMI,'Fermi Coupling             ','****','***'
      WRITE(iout,bxINIT) CCABIB,'cabibo angle               ','****','***'
      WRITE(iout,bxINIT) BRA1,  'a1 br ratio (massless)     ','****','***'
      WRITE(iout,bxINIT) BRKS,  'K* br ratio (massless)     ','****','***'
      WRITE(iout,bxclo)
            
      RETURN
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

      SUBROUTINE INIMAS(ITAUXPAR,xpar)
* ----------------------------------------------------------------------
*     INITIALISATION OF MASSES
*
*     called by : KORALZ
* ----------------------------------------------------------------------
      INCLUDE "BXformat.h"
      INTEGER   INUT,IOUT
      COMMON /INOUT/  
     $     INUT,         ! Input  unit  number (not used)
     $     IOUT          ! Ounput unit  number
      REAL*8 xpar(*)
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      CHARACTER*80 bxINIT
      PARAMETER (
     $  bxINIT ='(1x,1h*,g17.8,            16x, a31,a4,a4, 1x,1h*)'
     $ )
*
* IN-COMING / OUT-GOING  FERMION MASSES
      AMTAU  = xpar(656)
      AMNUTA = 0.010
      AMEL   = xpar(616)
      AMNUE  = 0.0
      AMMU   = xpar(636)
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


      WRITE(iout,bxope)
      WRITE(iout,bxtxt) ' TAUOLA Initialization SUBROUTINE INIMAS:    '
      WRITE(iout,bxtxt) ' Adopted to read from KK                     '
      WRITE(iout,bxINIT) amtau, 'AMTAU tau-mass             ','****','***'
      WRITE(iout,bxINIT) amel , 'AMEL  electron-mass        ','****','***'
      WRITE(iout,bxINIT) ammu , 'AMMU  muon-mass            ','****','***'
      WRITE(iout,bxclo)

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
      ELSEIF(MNUM.EQ.9) THEN
       PROB1=0.5
       PROB2=0.5
       AMRX =AMA1
       GAMRX=GAMA1
       AMRA =AMRO
       GAMRA=GAMRO
       AMRB =AMRO
       GAMRB=GAMRO
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
      ELSEIF(MNUM.GE.103.AND.MNUM.LE.112) THEN
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
* THIS ROUTINE CAN BE CALLED BEFORE ANY TAU+ OR TAU- EVENT IS GENERATED
* IT CAN BE USED TO GENERATE TAU+ AND TAU- SAMPLES OF DIFFERENT
* CONTENTS
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      COMMON / TAUBRA / GAMPRT(500),JLIST(500),NCHAN
      RETURN  ! this routine is called somewhere and is now deactivated, one has to fill it in properly  for  use.
      IF (KTO.EQ.1) THEN
*     ==================
* LIST OF BRANCHING RATIOS
      NCHAN = 19
      DO 1 I = 1,500
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I
        IF(I.EQ. 1) GAMPRT(I) = .0000
        IF(I.EQ. 2) GAMPRT(I) = .0000
        IF(I.EQ. 3) GAMPRT(I) = .0000
        IF(I.EQ. 4) GAMPRT(I) = .0000
        IF(I.EQ. 5) GAMPRT(I) = .0000
        IF(I.EQ. 6) GAMPRT(I) = .0000
        IF(I.EQ. 7) GAMPRT(I) = .0000
        IF(I.EQ. 8) GAMPRT(I) = 1.0000
        IF(I.EQ. 9) GAMPRT(I) = 1.0000
        IF(I.EQ.10) GAMPRT(I) = 1.0000
        IF(I.EQ.11) GAMPRT(I) = 1.0000
        IF(I.EQ.12) GAMPRT(I) = 1.0000
        IF(I.EQ.13) GAMPRT(I) = 1.0000
        IF(I.EQ.14) GAMPRT(I) = 1.0000
        IF(I.EQ.15) GAMPRT(I) = 1.0000
        IF(I.EQ.16) GAMPRT(I) = 1.0000
        IF(I.EQ.17) GAMPRT(I) = 1.0000
        IF(I.EQ.18) GAMPRT(I) = 1.0000
        IF(I.EQ.19) GAMPRT(I) = 1.0000
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   1  CONTINUE
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
      ELSE
*     ====
* LIST OF BRANCHING RATIOS
      NCHAN = 19
      DO 2 I = 1,500
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I
        IF(I.EQ. 1) GAMPRT(I) = .0000
        IF(I.EQ. 2) GAMPRT(I) = .0000
        IF(I.EQ. 3) GAMPRT(I) = .0000
        IF(I.EQ. 4) GAMPRT(I) = .0000
        IF(I.EQ. 5) GAMPRT(I) = .0000
        IF(I.EQ. 6) GAMPRT(I) = .0000
        IF(I.EQ. 7) GAMPRT(I) = .0000
        IF(I.EQ. 8) GAMPRT(I) = 1.0000
        IF(I.EQ. 9) GAMPRT(I) = 1.0000
        IF(I.EQ.10) GAMPRT(I) = 1.0000
        IF(I.EQ.11) GAMPRT(I) = 1.0000
        IF(I.EQ.12) GAMPRT(I) = 1.0000
        IF(I.EQ.13) GAMPRT(I) = 1.0000
        IF(I.EQ.14) GAMPRT(I) = 1.0000
        IF(I.EQ.15) GAMPRT(I) = 1.0000
        IF(I.EQ.16) GAMPRT(I) = 1.0000
        IF(I.EQ.17) GAMPRT(I) = 1.0000
        IF(I.EQ.18) GAMPRT(I) = 1.0000
        IF(I.EQ.19) GAMPRT(I) = 1.0000
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   2  CONTINUE
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
      ENDIF
*     =====
      END

