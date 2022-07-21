     
      REAL*8 FUNCTION VBFDISTR(I1,I2,I3,I4,H1,H2,P,KEY)
C***************************************************************************
C* CALCULATES (matrix element)^2 for 
C        I1 I2 -> I3 I4 TAU+TAU- with given tau polarizations H1 H2
C        for a given set of momenta P
C        I1,...,I4 are PDG particle codes
C        if I3 or I4=0, then summed over final jet flavors 
C        H1 and H2 are tau+ and tau- helicities R: 1, L: -1, R+L: 0 
C        Key=0: no Higgs
C        Key=1: SM Higgs
C        Key=2: non-standard state 
C***************************************************************************

C***************************************************************************
C*         so far only for initial ud state                   *
C***************************************************************************
      IMPLICIT NONE

      INTEGER I1,I2,I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'only for ud initial state'
 
      IF(KEY.EQ.0) THEN  
           WRITE(*,*) 'no Higgs' 
      ELSE IF(KEY.EQ.1) THEN 
           WRITE(*,*) 'SM Higgs '
      ELSE IF(KEY.EQ.2) THEN
           WRITE(*,*) 'non-standard state -- implementation not finished'
           stop
      ELSE     
      WRITE(*,*) 'WRONG KEY'
      STOP
      ENDIF


      SELECT CASE(I1+I2)
         CASE(:-5)
           WRITE(*,*) 'WRONG INITIAL STATE'
           STOP
         CASE(-4)    !   UXUX INITIAL STATE
           CALL UXUX(P,I3,I4,H1,H2,KEY,ANS) 
         CASE(-3)    !   DXUX INITIAL STATE
           CALL DXUX(P,I3,I4,H1,H2,KEY,ANS)
         CASE(-2)    !   DXDX INITIAL STATE
           CALL DXDX(P,I3,I4,H1,H2,KEY,ANS)
         CASE(-1)    !   UXD INITIAL STATE
           CALL UXD(P,I3,I4,H1,H2,KEY,ANS) 
         CASE(0)     !    UUX OR DDX INITIAL STATE
             IF(ABS(I1).EQ.1)  CALL DDX(P,I3,I4,H1,H2,KEY,ANS)
             IF(ABS(I1).EQ.2)  CALL UUX(P,I3,I4,H1,H2,KEY,ANS)
         CASE(1)     !    UDX INITIAL STATE
           CALL UDX(P,I3,I4,H1,H2,KEY,ANS)
         CASE(2)     !   DD INITIAL STATE
           CALL DD(P,I3,I4,H1,H2,KEY,ANS)
         CASE(3)     !   UD INITIAL STATE
           CALL UD(P,I3,I4,H1,H2,KEY,ANS)
         CASE(4)     !   UU INITIAL STATE
           CALL UU(P,I3,I4,H1,H2,KEY,ANS)
         CASE(5:)
           WRITE(*,*) 'WRONG INITIAL STATE'
           STOP
      END SELECT

      VBFDISTR=ANS
      END FUNCTION VBFDISTR
C *******************************************

      SUBROUTINE  UXUX(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'UXUX NOT FINISHED YET'
      STOP
      END SUBROUTINE UXUX
C *******************************************

      SUBROUTINE  DXUX(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'DXUX NOT FINISHED YET'
      STOP
      END SUBROUTINE DXUX
C *******************************************

      SUBROUTINE  DXDX(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'DXDX NOT FINISHED YET'
      STOP
      END SUBROUTINE DXDX
C *******************************************

      SUBROUTINE  UXD(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'UXD NOT FINISHED YET'
      STOP
      END SUBROUTINE UXD
C *******************************************

      SUBROUTINE  DDX(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'DDX NOT FINISHED YET'
      STOP
      END SUBROUTINE DDX
C *******************************************

      SUBROUTINE  UUX(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'UUX NOT FINISHED YET'
      STOP
      END SUBROUTINE UUX
C *******************************************

      SUBROUTINE  UDX(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'UDX NOT FINISHED YET'
      STOP
      END SUBROUTINE UDX
C *******************************************

      SUBROUTINE  DD(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'DD NOT FINISHED YET'
      STOP
      END SUBROUTINE DD
C *******************************************

      SUBROUTINE  UU(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS

      WRITE(*,*) 'UU NOT FINISHED YET'
      STOP
      END SUBROUTINE UU
C *******************************************


 
      SUBROUTINE  UD(P,I3,I4,H1,H2,KEY,ANS)
      IMPLICIT NONE
 
      INTEGER I3,I4,   H1,H2,   KEY
      REAL*8 P(0:3,6), ANS, ANS1,ANS2,ANS3,ANS4

C      I3=0,2 OR 4             I4=0,1 OR 3

      IF(KEY.EQ.1) THEN
         IF(I3.EQ.2 .AND. I4.EQ.1) CALL UD_UD_H(P,H1,H2,ANS)
         IF(I3.EQ.2 .AND. I4.EQ.3) CALL UD_US_H(P,H1,H2,ANS)
         IF(I3.EQ.4 .AND. I4.EQ.1) CAll UD_CD_H(P,H1,H2,ANS)
         IF(I3.EQ.4 .AND. I4.EQ.3) CALL UD_CS_H(P,H1,H2,ANS)
            IF(I3.EQ.0 .AND. I4.EQ.0) THEN 
                CALL UD_UD_H(P,H1,H2,ANS1)
                CALL UD_US_H(P,H1,H2,ANS2)
                CALL UD_CD_H(P,H1,H2,ANS3)
                CALL UD_CS_H(P,H1,H2,ANS4)
                ANS=ANS1+ANS2+ANS3+ANS4
            ENDIF
      ELSE IF(KEY.EQ.0) THEN 
         IF(I3.EQ.2 .AND. I4.EQ.1) CALL UD_UD_NOH(P,H1,H2,ANS)
         IF(I3.EQ.2 .AND. I4.EQ.3) CALL UD_US_NOH(P,H1,H2,ANS)
         IF(I3.EQ.4 .AND. I4.EQ.1) CAll UD_CD_NOH(P,H1,H2,ANS)
         IF(I3.EQ.4 .AND. I4.EQ.3) CALL UD_CS_NOH(P,H1,H2,ANS)
            IF(I3.EQ.0 .AND. I4.EQ.0) THEN 
                CALL UD_UD_NOH(P,H1,H2,ANS1)
                CALL UD_US_NOH(P,H1,H2,ANS2)
                CALL UD_CD_NOH(P,H1,H2,ANS3)
                CALL UD_CS_NOH(P,H1,H2,ANS4)
                ANS=ANS1+ANS2+ANS3+ANS4
            ENDIF   
      ELSE
           WRITE(*,*) 'NOT FINISHED'
           STOP
      ENDIF
      END SUBROUTINE UD



C ----- begin subprocesses  UD->jjH  with Higgs->tautau

      SUBROUTINE UD_CD_H(P,H1,H2,ANS)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     MadGraph StandAlone Version
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: u d > c d h WEIGHTED=6
C     *   Decay: h > ta+ ta- WEIGHTED=2
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=64)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      INTEGER H1,H2
C     
C     LOCAL VARIABLES 
C     
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 MATRIX_UD_CD_H
      INTEGER IHEL,IDEN, I
      INTEGER JC(NEXTERNAL)
      LOGICAL GOODHEL(NCOMB)
      DATA NTRY/0/
      DATA GOODHEL/NCOMB*.FALSE./
      DATA (NHEL(I,   1),I=1,6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   6),I=1,6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   7),I=1,6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   8),I=1,6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   9),I=1,6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  10),I=1,6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  11),I=1,6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  12),I=1,6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  13),I=1,6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  14),I=1,6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  15),I=1,6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  16),I=1,6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  17),I=1,6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  18),I=1,6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  19),I=1,6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  20),I=1,6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  21),I=1,6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  30),I=1,6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  31),I=1,6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  32),I=1,6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  33),I=1,6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  34),I=1,6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  35),I=1,6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  36),I=1,6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  37),I=1,6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  38),I=1,6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  39),I=1,6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  40),I=1,6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  41),I=1,6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  42),I=1,6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  43),I=1,6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  44),I=1,6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  45),I=1,6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  54),I=1,6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  55),I=1,6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  56),I=1,6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  57),I=1,6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  58),I=1,6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  59),I=1,6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  60),I=1,6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  61),I=1,6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  62),I=1,6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  63),I=1,6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  64),I=1,6) / 1, 1, 1, 1, 1, 1/
      DATA IDEN/36/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      DO IHEL=1,NEXTERNAL
        JC(IHEL) = +1
      ENDDO
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          T=MATRIX_UD_CD_H(P ,H1,H2,NHEL(1,IHEL),JC(1))
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END


      REAL*8 FUNCTION MATRIX_UD_CD_H(P,H1,H2,NHEL,IC)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: u d > c d h WEIGHTED=6
C     *   Decay: h > ta+ ta- WEIGHTED=2
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=1)
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=7, NCOLOR=1)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      COMPLEX*16 DUM0,DUM1
      DATA DUM0, DUM1/(0D0, 0D0), (1D0, 0D0)/
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
C     
C     COLOR DATA
C     
      DATA DENOM(1)/1/
      DATA (CF(I,  1),I=  1,  1) /    9/
C     1 T(3,2) T(4,1)

      INTEGER H1,H2
      REAL*8 MATRIX
      MATRIX_UD_CD_H=0.D0
      IF(H1.EQ.0.  OR  .H1.EQ.IHEL(5))  THEN      
      IF(H2.EQ.0.  OR  .H2.EQ.IHEL(6))  THEN   

C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL IXXXXX(P(0,5),MTA,NHEL(5),-1*IC(5),W(1,5))
      CALL OXXXXX(P(0,6),MTA,NHEL(6),+1*IC(6),W(1,6))
      CALL FFS4_3(W(1,5),W(1,6),GC_99,MH,WH,W(1,7))
      CALL FFV2_3(W(1,1),W(1,4),GC_100,MW,WW,W(1,6))
      CALL FFV2_3(W(1,2),W(1,3),GC_44,MW,WW,W(1,4))
C     Amplitude(s) for diagram number 1
      CALL VVS1_0(W(1,6),W(1,4),W(1,7),GC_72,AMP(1))
      JAMP(1)=+AMP(1)

      MATRIX = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX = MATRIX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
      ENDDO
      MATRIX_UD_CD_H=MATRIX
      ENDIF                ! CLOSES H1 IF 
      ENDIF                ! CLOSES H2 IF
      END


      SUBROUTINE UD_CS_H(P,H1,H2,ANS)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     MadGraph StandAlone Version
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: u d > c s h WEIGHTED=6
C     *   Decay: h > ta+ ta- WEIGHTED=2
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=64)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      INTEGER H1,H2
C     
C     LOCAL VARIABLES 
C     
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 MATRIX_UD_CS_H
      INTEGER IHEL,IDEN, I
      INTEGER JC(NEXTERNAL)
      LOGICAL GOODHEL(NCOMB)
      DATA NTRY/0/
      DATA GOODHEL/NCOMB*.FALSE./
      DATA (NHEL(I,   1),I=1,6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   6),I=1,6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   7),I=1,6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   8),I=1,6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   9),I=1,6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  10),I=1,6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  11),I=1,6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  12),I=1,6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  13),I=1,6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  14),I=1,6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  15),I=1,6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  16),I=1,6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  17),I=1,6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  18),I=1,6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  19),I=1,6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  20),I=1,6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  21),I=1,6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  30),I=1,6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  31),I=1,6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  32),I=1,6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  33),I=1,6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  34),I=1,6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  35),I=1,6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  36),I=1,6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  37),I=1,6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  38),I=1,6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  39),I=1,6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  40),I=1,6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  41),I=1,6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  42),I=1,6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  43),I=1,6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  44),I=1,6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  45),I=1,6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  54),I=1,6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  55),I=1,6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  56),I=1,6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  57),I=1,6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  58),I=1,6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  59),I=1,6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  60),I=1,6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  61),I=1,6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  62),I=1,6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  63),I=1,6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  64),I=1,6) / 1, 1, 1, 1, 1, 1/
      DATA IDEN/36/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      DO IHEL=1,NEXTERNAL
        JC(IHEL) = +1
      ENDDO
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          T=MATRIX_UD_CS_H(P ,H1,H2,NHEL(1,IHEL),JC(1))
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END


      REAL*8 FUNCTION MATRIXUD_CS_H(P,H1,H2,NHEL,IC)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: u d > c s h WEIGHTED=6
C     *   Decay: h > ta+ ta- WEIGHTED=2
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=1)
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=7, NCOLOR=1)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      COMPLEX*16 DUM0,DUM1
      DATA DUM0, DUM1/(0D0, 0D0), (1D0, 0D0)/
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
C     
C     COLOR DATA
C     
      DATA DENOM(1)/1/
      DATA (CF(I,  1),I=  1,  1) /    9/
C     1 T(3,2) T(4,1)

      INTEGER H1,H2
      MATRIX_UD_CS_H=0.D0
      IF(H1.EQ.0.  OR  .H1.EQ.IHEL(5))  THEN      
      IF(H2.EQ.0.  OR  .H2.EQ.IHEL(6))  THEN   

C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL IXXXXX(P(0,5),MTA,NHEL(5),-1*IC(5),W(1,5))
      CALL OXXXXX(P(0,6),MTA,NHEL(6),+1*IC(6),W(1,6))
      CALL FFS4_3(W(1,5),W(1,6),GC_99,MH,WH,W(1,7))
      CALL FFV2_3(W(1,1),W(1,4),GC_101,MW,WW,W(1,6))
      CALL FFV2_3(W(1,2),W(1,3),GC_44,MW,WW,W(1,4))
C     Amplitude(s) for diagram number 1
      CALL VVS1_0(W(1,6),W(1,4),W(1,7),GC_72,AMP(1))
      JAMP(1)=+AMP(1)

      MATRIX = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX = MATRIX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
      ENDDO
      MATRIX_UD_CS_H=MATRIX
      ENDIF                ! CLOSES H1 IF 
      ENDIF                ! CLOSES H2 IF
      END


      SUBROUTINE UD_UD_H(P,H1,H2,ANS)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     MadGraph StandAlone Version
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: u d > u d h WEIGHTED=6
C     *   Decay: h > ta+ ta- WEIGHTED=2
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=64)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      INTEGER H1,H2                               ! DODALEM
C     
C     LOCAL VARIABLES 
C     
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 MATRIX_UD_UD_H
      INTEGER IHEL,IDEN, I
      INTEGER JC(NEXTERNAL)
      LOGICAL GOODHEL(NCOMB)
      DATA NTRY/0/
      DATA GOODHEL/NCOMB*.FALSE./
      DATA (NHEL(I,   1),I=1,6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   6),I=1,6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   7),I=1,6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   8),I=1,6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   9),I=1,6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  10),I=1,6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  11),I=1,6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  12),I=1,6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  13),I=1,6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  14),I=1,6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  15),I=1,6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  16),I=1,6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  17),I=1,6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  18),I=1,6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  19),I=1,6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  20),I=1,6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  21),I=1,6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  30),I=1,6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  31),I=1,6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  32),I=1,6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  33),I=1,6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  34),I=1,6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  35),I=1,6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  36),I=1,6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  37),I=1,6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  38),I=1,6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  39),I=1,6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  40),I=1,6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  41),I=1,6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  42),I=1,6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  43),I=1,6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  44),I=1,6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  45),I=1,6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  54),I=1,6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  55),I=1,6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  56),I=1,6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  57),I=1,6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  58),I=1,6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  59),I=1,6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  60),I=1,6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  61),I=1,6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  62),I=1,6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  63),I=1,6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  64),I=1,6) / 1, 1, 1, 1, 1, 1/
      DATA IDEN/36/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      DO IHEL=1,NEXTERNAL
        JC(IHEL) = +1
      ENDDO
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          T=MATRIX_UD_UD_H(P ,H1,H2,NHEL(1,IHEL),JC(1))
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END 


      REAL*8 FUNCTION MATRIX_UD_UD_H(P,H1,H2,NHEL,IC)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: u d > u d h WEIGHTED=6
C     *   Decay: h > ta+ ta- WEIGHTED=2
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=2)
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=7, NCOLOR=2)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)

C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      COMPLEX*16 DUM0,DUM1
      DATA DUM0, DUM1/(0D0, 0D0), (1D0, 0D0)/
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
C     
C     COLOR DATA
C     
      DATA DENOM(1)/1/
      DATA (CF(I,  1),I=  1,  2) /    9,    3/
C     1 T(3,1) T(4,2)
      DATA DENOM(2)/1/
      DATA (CF(I,  2),I=  1,  2) /    3,    9/
C     1 T(3,2) T(4,1)

      INTEGER H1,H2
      REAL*8 MATRIX
      MATRIX_UD_UD_H=0.D0
      IF(H1.EQ.0.  OR  .H1.EQ.IHEL(5))  THEN      
      IF(H2.EQ.0.  OR  .H2.EQ.IHEL(6))  THEN   

C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL IXXXXX(P(0,5),MTA,NHEL(5),-1*IC(5),W(1,5))
      CALL OXXXXX(P(0,6),MTA,NHEL(6),+1*IC(6),W(1,6))
      CALL FFS4_3(W(1,5),W(1,6),GC_99,MH,WH,W(1,7))
      CALL FFV2_5_3(W(1,1),W(1,3),GC_51,GC_58,MZ,WZ,W(1,6))
      CALL FFV2_3_3(W(1,2),W(1,4),GC_50,GC_58,MZ,WZ,W(1,5))
C     Amplitude(s) for diagram number 1
      CALL VVS1_0(W(1,6),W(1,5),W(1,7),GC_81,AMP(1))
      CALL FFV2_3(W(1,1),W(1,4),GC_100,MW,WW,W(1,5))
      CALL FFV2_3(W(1,2),W(1,3),GC_100,MW,WW,W(1,4))
C     Amplitude(s) for diagram number 2
      CALL VVS1_0(W(1,5),W(1,4),W(1,7),GC_72,AMP(2))
      JAMP(1)=-AMP(1)
      JAMP(2)=+AMP(2)

      MATRIX = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX = MATRIX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
      ENDDO
      MATRIX_UD_UD_H=MATRIX
      ENDIF                ! CLOSES H1 IF 
      ENDIF                ! CLOSES H2 IF
      END

      SUBROUTINE UD_US_H(P,H1,H2,ANS)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     MadGraph StandAlone Version
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: u d > u s h WEIGHTED=6
C     *   Decay: h > ta+ ta- WEIGHTED=2
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=64)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      INTEGER H1,H2
C     
C     LOCAL VARIABLES 
C     
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 MATRIX_UD_US_H                            
      INTEGER IHEL,IDEN, I
      INTEGER JC(NEXTERNAL)
      LOGICAL GOODHEL(NCOMB)
      DATA NTRY/0/
      DATA GOODHEL/NCOMB*.FALSE./
      DATA (NHEL(I,   1),I=1,6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   6),I=1,6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   7),I=1,6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   8),I=1,6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   9),I=1,6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  10),I=1,6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  11),I=1,6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  12),I=1,6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  13),I=1,6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  14),I=1,6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  15),I=1,6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  16),I=1,6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  17),I=1,6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  18),I=1,6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  19),I=1,6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  20),I=1,6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  21),I=1,6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  30),I=1,6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  31),I=1,6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  32),I=1,6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  33),I=1,6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  34),I=1,6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  35),I=1,6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  36),I=1,6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  37),I=1,6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  38),I=1,6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  39),I=1,6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  40),I=1,6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  41),I=1,6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  42),I=1,6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  43),I=1,6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  44),I=1,6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  45),I=1,6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  54),I=1,6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  55),I=1,6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  56),I=1,6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  57),I=1,6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  58),I=1,6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  59),I=1,6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  60),I=1,6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  61),I=1,6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  62),I=1,6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  63),I=1,6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  64),I=1,6) / 1, 1, 1, 1, 1, 1/
      DATA IDEN/36/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      DO IHEL=1,NEXTERNAL
        JC(IHEL) = +1
      ENDDO
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          T=MATRIX_UD_US_H(P ,H1,H2,NHEL(1,IHEL),JC(1))
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END


      REAL*8 FUNCTION MATRIX_UD_US_H(P,H1,H2,NHEL,IC)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: u d > u s h WEIGHTED=6
C     *   Decay: h > ta+ ta- WEIGHTED=2
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=1)
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=7, NCOLOR=1)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      COMPLEX*16 DUM0,DUM1
      DATA DUM0, DUM1/(0D0, 0D0), (1D0, 0D0)/
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
C     
C     COLOR DATA
C     
      DATA DENOM(1)/1/
      DATA (CF(I,  1),I=  1,  1) /    9/
C     1 T(3,2) T(4,1)

      INTEGER H1,H2
      REAL*8 MATRIX
      MATRIX_UD_US_H=0.D0
      IF(H1.EQ.0.  OR  .H1.EQ.IHEL(5))  THEN      
      IF(H2.EQ.0.  OR  .H2.EQ.IHEL(6))  THEN   

C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL IXXXXX(P(0,5),MTA,NHEL(5),-1*IC(5),W(1,5))
      CALL OXXXXX(P(0,6),MTA,NHEL(6),+1*IC(6),W(1,6))
      CALL FFS4_3(W(1,5),W(1,6),GC_99,MH,WH,W(1,7))
      CALL FFV2_3(W(1,1),W(1,4),GC_101,MW,WW,W(1,6))
      CALL FFV2_3(W(1,2),W(1,3),GC_100,MW,WW,W(1,4))
C     Amplitude(s) for diagram number 1
      CALL VVS1_0(W(1,6),W(1,4),W(1,7),GC_72,AMP(1))
      JAMP(1)=+AMP(1)

      MATRIX = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX = MATRIX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
      ENDDO
      MATRIX_UD_US_H=MATRIX
      ENDIF                ! CLOSES H1 IF 
      ENDIF                ! CLOSES H2 IF
      END


C  ---  end of subprocess ud->jjH, H->tautau

C ----- begin subprocesses ud->jjtautau, no Higgs

      SUBROUTINE SMATRIX_UD_CD_NOH(P,H1,H2,ANS)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     MadGraph StandAlone Version
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: u d > c d ta+ ta- / h QED=4
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=64)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      INTEGER H1,H2
C     
C     LOCAL VARIABLES 
C     
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 MATRIX_UD_CD_NOH
      INTEGER IHEL,IDEN, I
      INTEGER JC(NEXTERNAL)
      LOGICAL GOODHEL(NCOMB)
      DATA NTRY/0/
      DATA GOODHEL/NCOMB*.FALSE./
      DATA (NHEL(I,   1),I=1,6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   6),I=1,6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   7),I=1,6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   8),I=1,6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   9),I=1,6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  10),I=1,6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  11),I=1,6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  12),I=1,6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  13),I=1,6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  14),I=1,6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  15),I=1,6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  16),I=1,6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  17),I=1,6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  18),I=1,6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  19),I=1,6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  20),I=1,6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  21),I=1,6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  30),I=1,6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  31),I=1,6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  32),I=1,6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  33),I=1,6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  34),I=1,6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  35),I=1,6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  36),I=1,6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  37),I=1,6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  38),I=1,6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  39),I=1,6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  40),I=1,6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  41),I=1,6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  42),I=1,6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  43),I=1,6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  44),I=1,6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  45),I=1,6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  54),I=1,6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  55),I=1,6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  56),I=1,6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  57),I=1,6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  58),I=1,6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  59),I=1,6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  60),I=1,6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  61),I=1,6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  62),I=1,6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  63),I=1,6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  64),I=1,6) / 1, 1, 1, 1, 1, 1/
      DATA IDEN/36/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      DO IHEL=1,NEXTERNAL
        JC(IHEL) = +1
      ENDDO
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          T=MATRIX_UD_CD_NOH(P ,H1,H2,NHEL(1,IHEL),JC(1))
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END


      REAL*8 FUNCTION MATRIX_UD_CD_NOH(P,H1,H2,NHEL,IC)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: u d > c d ta+ ta- / h QED=4
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=11)
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=10, NCOLOR=1)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      COMPLEX*16 DUM0,DUM1
      DATA DUM0, DUM1/(0D0, 0D0), (1D0, 0D0)/
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
C     
C     COLOR DATA
C     
      DATA DENOM(1)/1/
      DATA (CF(I,  1),I=  1,  1) /    9/
C     1 T(3,2) T(4,1)


      INTEGER H1,H2
      REAL*8 MATRIX
      MATRIX_UD_CD_NOH=0.D0
      IF(H1.EQ.0.  OR  .H1.EQ.IHEL(5))  THEN      
      IF(H2.EQ.0.  OR  .H2.EQ.IHEL(6))  THEN   
C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL IXXXXX(P(0,5),MTA,NHEL(5),-1*IC(5),W(1,5))
      CALL OXXXXX(P(0,6),MTA,NHEL(6),+1*IC(6),W(1,6))
      CALL FFV2_3(W(1,1),W(1,4),GC_100,MW,WW,W(1,7))
      CALL FFV2_3(W(1,2),W(1,3),GC_44,MW,WW,W(1,8))
      CALL FFV2_2(W(1,5),W(1,7),GC_108,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 1
      CALL FFV2_0(W(1,9),W(1,6),W(1,8),GC_108,AMP(1))
      CALL FFV1P0_3(W(1,5),W(1,6),GC_3,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 2
      CALL VVV1_0(W(1,9),W(1,7),W(1,8),GC_4,AMP(2))
      CALL FFV2_4_3(W(1,5),W(1,6),GC_50,GC_59,MZ,WZ,W(1,10))
C     Amplitude(s) for diagram number 3
      CALL VVV1_0(W(1,7),W(1,8),W(1,10),GC_53,AMP(3))
      CALL FFV2_2(W(1,2),W(1,7),GC_44,ZERO,ZERO,W(1,6))
C     Amplitude(s) for diagram number 4
      CALL FFV1_0(W(1,6),W(1,3),W(1,9),GC_2,AMP(4))
      CALL FFV2_1(W(1,3),W(1,7),GC_44,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 5
      CALL FFV1_0(W(1,2),W(1,5),W(1,9),GC_1,AMP(5))
C     Amplitude(s) for diagram number 6
      CALL FFV2_5_0(W(1,6),W(1,3),W(1,10),GC_51,GC_58,AMP(6))
C     Amplitude(s) for diagram number 7
      CALL FFV2_3_0(W(1,2),W(1,5),W(1,10),GC_50,GC_58,AMP(7))
      CALL FFV2_2(W(1,1),W(1,8),GC_100,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 8
      CALL FFV1_0(W(1,5),W(1,4),W(1,9),GC_1,AMP(8))
      CALL FFV1_2(W(1,1),W(1,9),GC_2,ZERO,ZERO,W(1,2))
C     Amplitude(s) for diagram number 9
      CALL FFV2_0(W(1,2),W(1,4),W(1,8),GC_100,AMP(9))
C     Amplitude(s) for diagram number 10
      CALL FFV2_3_0(W(1,5),W(1,4),W(1,10),GC_50,GC_58,AMP(10))
      CALL FFV2_5_2(W(1,1),W(1,10),GC_51,GC_58,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 11
      CALL FFV2_0(W(1,5),W(1,4),W(1,8),GC_100,AMP(11))
      JAMP(1)=+AMP(1)+AMP(2)+AMP(3)+AMP(4)+AMP(5)+AMP(6)+AMP(7)+AMP(8)
     $ +AMP(9)+AMP(10)+AMP(11)

      MATRIX = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX = MATRIX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
      ENDDO
      MATRIX_UD_CD_NOH=MATRIX
      ENDIF                ! CLOSES H1 IF 
      ENDIF                ! CLOSES H2 IF
      END


      SUBROUTINE SMATRIX_UD_CS_NOH(P,H1,H2,ANS)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     MadGraph StandAlone Version
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: u d > c s ta+ ta- / h QED=4
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=64)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      INTEGER H1,H2
C     
C     LOCAL VARIABLES 
C     
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 MATRIX_UD_CS_NOH
      INTEGER IHEL,IDEN, I
      INTEGER JC(NEXTERNAL)
      LOGICAL GOODHEL(NCOMB)
      DATA NTRY/0/
      DATA GOODHEL/NCOMB*.FALSE./
      DATA (NHEL(I,   1),I=1,6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   6),I=1,6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   7),I=1,6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   8),I=1,6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   9),I=1,6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  10),I=1,6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  11),I=1,6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  12),I=1,6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  13),I=1,6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  14),I=1,6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  15),I=1,6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  16),I=1,6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  17),I=1,6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  18),I=1,6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  19),I=1,6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  20),I=1,6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  21),I=1,6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  30),I=1,6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  31),I=1,6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  32),I=1,6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  33),I=1,6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  34),I=1,6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  35),I=1,6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  36),I=1,6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  37),I=1,6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  38),I=1,6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  39),I=1,6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  40),I=1,6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  41),I=1,6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  42),I=1,6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  43),I=1,6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  44),I=1,6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  45),I=1,6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  54),I=1,6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  55),I=1,6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  56),I=1,6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  57),I=1,6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  58),I=1,6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  59),I=1,6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  60),I=1,6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  61),I=1,6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  62),I=1,6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  63),I=1,6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  64),I=1,6) / 1, 1, 1, 1, 1, 1/
      DATA IDEN/36/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      DO IHEL=1,NEXTERNAL
        JC(IHEL) = +1
      ENDDO
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          T=MATRIX_UD_CS_NOH(P ,H1,H2,NHEL(1,IHEL),JC(1))
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END


      REAL*8 FUNCTION MATRIX_UD_CS_NOH(P,H1,H2,NHEL,IC)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: u d > c s ta+ ta- / h QED=4
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=11)
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=10, NCOLOR=1)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      COMPLEX*16 DUM0,DUM1
      DATA DUM0, DUM1/(0D0, 0D0), (1D0, 0D0)/
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
C     
C     COLOR DATA
C     
      DATA DENOM(1)/1/
      DATA (CF(I,  1),I=  1,  1) /    9/
C     1 T(3,2) T(4,1)

      INTEGER H1,H2
      REAL*8 MATRIX
      MATRIX_UD_CS_NOH=0.D0
      IF(H1.EQ.0.  OR  .H1.EQ.IHEL(5))  THEN      
      IF(H2.EQ.0.  OR  .H2.EQ.IHEL(6))  THEN   
C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL IXXXXX(P(0,5),MTA,NHEL(5),-1*IC(5),W(1,5))
      CALL OXXXXX(P(0,6),MTA,NHEL(6),+1*IC(6),W(1,6))
      CALL FFV2_3(W(1,1),W(1,4),GC_101,MW,WW,W(1,7))
      CALL FFV2_3(W(1,2),W(1,3),GC_44,MW,WW,W(1,8))
      CALL FFV2_2(W(1,5),W(1,7),GC_108,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 1
      CALL FFV2_0(W(1,9),W(1,6),W(1,8),GC_108,AMP(1))
      CALL FFV1P0_3(W(1,5),W(1,6),GC_3,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 2
      CALL VVV1_0(W(1,9),W(1,7),W(1,8),GC_4,AMP(2))
      CALL FFV2_4_3(W(1,5),W(1,6),GC_50,GC_59,MZ,WZ,W(1,10))
C     Amplitude(s) for diagram number 3
      CALL VVV1_0(W(1,7),W(1,8),W(1,10),GC_53,AMP(3))
      CALL FFV2_2(W(1,2),W(1,7),GC_44,ZERO,ZERO,W(1,6))
C     Amplitude(s) for diagram number 4
      CALL FFV1_0(W(1,6),W(1,3),W(1,9),GC_2,AMP(4))
      CALL FFV2_1(W(1,3),W(1,7),GC_44,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 5
      CALL FFV1_0(W(1,2),W(1,5),W(1,9),GC_1,AMP(5))
C     Amplitude(s) for diagram number 6
      CALL FFV2_5_0(W(1,6),W(1,3),W(1,10),GC_51,GC_58,AMP(6))
C     Amplitude(s) for diagram number 7
      CALL FFV2_3_0(W(1,2),W(1,5),W(1,10),GC_50,GC_58,AMP(7))
      CALL FFV2_2(W(1,1),W(1,8),GC_101,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 8
      CALL FFV1_0(W(1,5),W(1,4),W(1,9),GC_1,AMP(8))
      CALL FFV1_2(W(1,1),W(1,9),GC_2,ZERO,ZERO,W(1,2))
C     Amplitude(s) for diagram number 9
      CALL FFV2_0(W(1,2),W(1,4),W(1,8),GC_101,AMP(9))
C     Amplitude(s) for diagram number 10
      CALL FFV2_3_0(W(1,5),W(1,4),W(1,10),GC_50,GC_58,AMP(10))
      CALL FFV2_5_2(W(1,1),W(1,10),GC_51,GC_58,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 11
      CALL FFV2_0(W(1,5),W(1,4),W(1,8),GC_101,AMP(11))
      JAMP(1)=+AMP(1)+AMP(2)+AMP(3)+AMP(4)+AMP(5)+AMP(6)+AMP(7)+AMP(8)
     $ +AMP(9)+AMP(10)+AMP(11)

      MATRIX = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX = MATRIX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
      ENDDO
      MATRIX_UD_CS_NOH=MATRIX
      ENDIF                ! CLOSES H1 IF 
      ENDIF                ! CLOSES H2 IF
      END

      SUBROUTINE SMATRIX_UD_UD_NOH(P,H1,H2,ANS)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     MadGraph StandAlone Version
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: u d > u d ta+ ta- / h QED=4
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=64)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      INTEGER H1,H2
C     
C     LOCAL VARIABLES 
C     
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 MATRIX_UD_UD_NOH
      INTEGER IHEL,IDEN, I
      INTEGER JC(NEXTERNAL)
      LOGICAL GOODHEL(NCOMB)
      DATA NTRY/0/
      DATA GOODHEL/NCOMB*.FALSE./
      DATA (NHEL(I,   1),I=1,6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   6),I=1,6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   7),I=1,6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   8),I=1,6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   9),I=1,6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  10),I=1,6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  11),I=1,6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  12),I=1,6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  13),I=1,6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  14),I=1,6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  15),I=1,6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  16),I=1,6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  17),I=1,6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  18),I=1,6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  19),I=1,6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  20),I=1,6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  21),I=1,6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  30),I=1,6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  31),I=1,6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  32),I=1,6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  33),I=1,6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  34),I=1,6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  35),I=1,6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  36),I=1,6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  37),I=1,6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  38),I=1,6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  39),I=1,6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  40),I=1,6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  41),I=1,6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  42),I=1,6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  43),I=1,6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  44),I=1,6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  45),I=1,6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  54),I=1,6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  55),I=1,6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  56),I=1,6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  57),I=1,6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  58),I=1,6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  59),I=1,6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  60),I=1,6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  61),I=1,6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  62),I=1,6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  63),I=1,6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  64),I=1,6) / 1, 1, 1, 1, 1, 1/
      DATA IDEN/36/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      DO IHEL=1,NEXTERNAL
        JC(IHEL) = +1
      ENDDO
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          T=MATRIX_UD_UD_NOH(P ,H1,H2,NHEL(1,IHEL),JC(1))
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END


      REAL*8 FUNCTION MATRIX_UD_UD_NOH(P,H1,H2,NHEL,IC)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: u d > u d ta+ ta- / h QED=4
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=43)
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=14, NCOLOR=2)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      COMPLEX*16 DUM0,DUM1
      DATA DUM0, DUM1/(0D0, 0D0), (1D0, 0D0)/
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
C     
C     COLOR DATA
C     
      DATA DENOM(1)/1/
      DATA (CF(I,  1),I=  1,  2) /    9,    3/
C     1 T(3,1) T(4,2)
      DATA DENOM(2)/1/
      DATA (CF(I,  2),I=  1,  2) /    3,    9/
C     1 T(3,2) T(4,1)

      INTEGER H1,H2
      REAL*8 MATRIX
      MATRIX_UD_UD_NOH=0.D0
      IF(H1.EQ.0.  OR  .H1.EQ.IHEL(5))  THEN      
      IF(H2.EQ.0.  OR  .H2.EQ.IHEL(6))  THEN   


C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL IXXXXX(P(0,5),MTA,NHEL(5),-1*IC(5),W(1,5))
      CALL OXXXXX(P(0,6),MTA,NHEL(6),+1*IC(6),W(1,6))
      CALL FFV1P0_3(W(1,1),W(1,3),GC_2,ZERO,ZERO,W(1,7))
      CALL FFV1P0_3(W(1,2),W(1,4),GC_1,ZERO,ZERO,W(1,8))
      CALL FFV1_2(W(1,5),W(1,7),GC_3,MTA,ZERO,W(1,9))
C     Amplitude(s) for diagram number 1
      CALL FFV1_0(W(1,9),W(1,6),W(1,8),GC_3,AMP(1))
      CALL FFV1_1(W(1,6),W(1,7),GC_3,MTA,ZERO,W(1,10))
C     Amplitude(s) for diagram number 2
      CALL FFV1_0(W(1,5),W(1,10),W(1,8),GC_3,AMP(2))
      CALL FFV2_3_3(W(1,2),W(1,4),GC_50,GC_58,MZ,WZ,W(1,11))
C     Amplitude(s) for diagram number 3
      CALL FFV2_4_0(W(1,9),W(1,6),W(1,11),GC_50,GC_59,AMP(3))
C     Amplitude(s) for diagram number 4
      CALL FFV2_4_0(W(1,5),W(1,10),W(1,11),GC_50,GC_59,AMP(4))
      CALL FFV2_5_3(W(1,1),W(1,3),GC_51,GC_58,MZ,WZ,W(1,10))
      CALL FFV2_4_2(W(1,5),W(1,10),GC_50,GC_59,MTA,ZERO,W(1,9))
C     Amplitude(s) for diagram number 5
      CALL FFV1_0(W(1,9),W(1,6),W(1,8),GC_3,AMP(5))
      CALL FFV2_4_1(W(1,6),W(1,10),GC_50,GC_59,MTA,ZERO,W(1,12))
C     Amplitude(s) for diagram number 6
      CALL FFV1_0(W(1,5),W(1,12),W(1,8),GC_3,AMP(6))
C     Amplitude(s) for diagram number 7
      CALL FFV2_4_0(W(1,9),W(1,6),W(1,11),GC_50,GC_59,AMP(7))
C     Amplitude(s) for diagram number 8
      CALL FFV2_4_0(W(1,5),W(1,12),W(1,11),GC_50,GC_59,AMP(8))
      CALL FFV1P0_3(W(1,5),W(1,6),GC_3,ZERO,ZERO,W(1,12))
      CALL FFV1_2(W(1,2),W(1,7),GC_1,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 9
      CALL FFV1_0(W(1,9),W(1,4),W(1,12),GC_1,AMP(9))
      CALL FFV1_1(W(1,4),W(1,7),GC_1,ZERO,ZERO,W(1,13))
C     Amplitude(s) for diagram number 10
      CALL FFV1_0(W(1,2),W(1,13),W(1,12),GC_1,AMP(10))
      CALL FFV2_4_3(W(1,5),W(1,6),GC_50,GC_59,MZ,WZ,W(1,7))
C     Amplitude(s) for diagram number 11
      CALL FFV2_3_0(W(1,9),W(1,4),W(1,7),GC_50,GC_58,AMP(11))
C     Amplitude(s) for diagram number 12
      CALL FFV2_3_0(W(1,2),W(1,13),W(1,7),GC_50,GC_58,AMP(12))
      CALL FFV1P0_3(W(1,1),W(1,3),GC_11,ZERO,ZERO,W(1,13))
      CALL FFV1_2(W(1,2),W(1,13),GC_11,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 13
      CALL FFV1_0(W(1,9),W(1,4),W(1,12),GC_1,AMP(13))
      CALL FFV1_1(W(1,4),W(1,13),GC_11,ZERO,ZERO,W(1,14))
C     Amplitude(s) for diagram number 14
      CALL FFV1_0(W(1,2),W(1,14),W(1,12),GC_1,AMP(14))
C     Amplitude(s) for diagram number 15
      CALL FFV2_3_0(W(1,9),W(1,4),W(1,7),GC_50,GC_58,AMP(15))
C     Amplitude(s) for diagram number 16
      CALL FFV2_3_0(W(1,2),W(1,14),W(1,7),GC_50,GC_58,AMP(16))
      CALL FFV2_3_2(W(1,2),W(1,10),GC_50,GC_58,ZERO,ZERO,W(1,14))
C     Amplitude(s) for diagram number 17
      CALL FFV1_0(W(1,14),W(1,4),W(1,12),GC_1,AMP(17))
      CALL FFV2_3_1(W(1,4),W(1,10),GC_50,GC_58,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 18
      CALL FFV1_0(W(1,2),W(1,9),W(1,12),GC_1,AMP(18))
C     Amplitude(s) for diagram number 19
      CALL FFV2_3_0(W(1,14),W(1,4),W(1,7),GC_50,GC_58,AMP(19))
C     Amplitude(s) for diagram number 20
      CALL FFV2_3_0(W(1,2),W(1,9),W(1,7),GC_50,GC_58,AMP(20))
      CALL FFV2_3(W(1,1),W(1,4),GC_100,MW,WW,W(1,9))
      CALL FFV2_3(W(1,2),W(1,3),GC_100,MW,WW,W(1,14))
      CALL FFV2_2(W(1,5),W(1,9),GC_108,ZERO,ZERO,W(1,10))
C     Amplitude(s) for diagram number 21
      CALL FFV2_0(W(1,10),W(1,6),W(1,14),GC_108,AMP(21))
C     Amplitude(s) for diagram number 22
      CALL VVV1_0(W(1,12),W(1,9),W(1,14),GC_4,AMP(22))
C     Amplitude(s) for diagram number 23
      CALL VVV1_0(W(1,9),W(1,14),W(1,7),GC_53,AMP(23))
      CALL FFV2_2(W(1,2),W(1,9),GC_100,ZERO,ZERO,W(1,10))
C     Amplitude(s) for diagram number 24
      CALL FFV1_0(W(1,10),W(1,3),W(1,12),GC_2,AMP(24))
      CALL FFV2_1(W(1,3),W(1,9),GC_100,ZERO,ZERO,W(1,6))
C     Amplitude(s) for diagram number 25
      CALL FFV1_0(W(1,2),W(1,6),W(1,12),GC_1,AMP(25))
C     Amplitude(s) for diagram number 26
      CALL FFV2_5_0(W(1,10),W(1,3),W(1,7),GC_51,GC_58,AMP(26))
C     Amplitude(s) for diagram number 27
      CALL FFV2_3_0(W(1,2),W(1,6),W(1,7),GC_50,GC_58,AMP(27))
      CALL FFV2_2(W(1,1),W(1,14),GC_100,ZERO,ZERO,W(1,6))
C     Amplitude(s) for diagram number 28
      CALL FFV1_0(W(1,6),W(1,4),W(1,12),GC_1,AMP(28))
      CALL FFV1_2(W(1,1),W(1,12),GC_2,ZERO,ZERO,W(1,10))
C     Amplitude(s) for diagram number 29
      CALL FFV2_0(W(1,10),W(1,4),W(1,14),GC_100,AMP(29))
C     Amplitude(s) for diagram number 30
      CALL FFV2_3_0(W(1,6),W(1,4),W(1,7),GC_50,GC_58,AMP(30))
      CALL FFV2_5_2(W(1,1),W(1,7),GC_51,GC_58,ZERO,ZERO,W(1,6))
C     Amplitude(s) for diagram number 31
      CALL FFV2_0(W(1,6),W(1,4),W(1,14),GC_100,AMP(31))
      CALL FFV1_2(W(1,1),W(1,8),GC_2,ZERO,ZERO,W(1,14))
C     Amplitude(s) for diagram number 32
      CALL FFV1_0(W(1,14),W(1,3),W(1,12),GC_2,AMP(32))
C     Amplitude(s) for diagram number 33
      CALL FFV1_0(W(1,10),W(1,3),W(1,8),GC_2,AMP(33))
C     Amplitude(s) for diagram number 34
      CALL FFV2_5_0(W(1,14),W(1,3),W(1,7),GC_51,GC_58,AMP(34))
C     Amplitude(s) for diagram number 35
      CALL FFV1_0(W(1,6),W(1,3),W(1,8),GC_2,AMP(35))
      CALL FFV1P0_3(W(1,2),W(1,4),GC_11,ZERO,ZERO,W(1,8))
      CALL FFV1_2(W(1,1),W(1,8),GC_11,ZERO,ZERO,W(1,4))
C     Amplitude(s) for diagram number 36
      CALL FFV1_0(W(1,4),W(1,3),W(1,12),GC_2,AMP(36))
C     Amplitude(s) for diagram number 37
      CALL FFV1_0(W(1,10),W(1,3),W(1,8),GC_11,AMP(37))
C     Amplitude(s) for diagram number 38
      CALL FFV2_5_0(W(1,4),W(1,3),W(1,7),GC_51,GC_58,AMP(38))
C     Amplitude(s) for diagram number 39
      CALL FFV1_0(W(1,6),W(1,3),W(1,8),GC_11,AMP(39))
      CALL FFV2_5_2(W(1,1),W(1,11),GC_51,GC_58,ZERO,ZERO,W(1,8))
C     Amplitude(s) for diagram number 40
      CALL FFV1_0(W(1,8),W(1,3),W(1,12),GC_2,AMP(40))
C     Amplitude(s) for diagram number 41
      CALL FFV2_5_0(W(1,10),W(1,3),W(1,11),GC_51,GC_58,AMP(41))
C     Amplitude(s) for diagram number 42
      CALL FFV2_5_0(W(1,8),W(1,3),W(1,7),GC_51,GC_58,AMP(42))
C     Amplitude(s) for diagram number 43
      CALL FFV2_5_0(W(1,6),W(1,3),W(1,11),GC_51,GC_58,AMP(43))
      JAMP(1)=-AMP(1)-AMP(2)-AMP(3)-AMP(4)-AMP(5)-AMP(6)-AMP(7)-AMP(8)
     $ -AMP(9)-AMP(10)-AMP(11)-AMP(12)+1./6.*AMP(13)+1./6.*AMP(14)
     $ +1./6.*AMP(15)+1./6.*AMP(16)-AMP(17)-AMP(18)-AMP(19)-AMP(20)
     $ -AMP(32)-AMP(33)-AMP(34)-AMP(35)+1./6.*AMP(36)+1./6.*AMP(37)
     $ +1./6.*AMP(38)+1./6.*AMP(39)-AMP(40)-AMP(41)-AMP(42)-AMP(43)
      JAMP(2)=-1./2.*AMP(13)-1./2.*AMP(14)-1./2.*AMP(15)-1./2.*AMP(16)
     $ +AMP(21)+AMP(22)+AMP(23)+AMP(24)+AMP(25)+AMP(26)+AMP(27)
     $ +AMP(28)+AMP(29)+AMP(30)+AMP(31)-1./2.*AMP(36)-1./2.*AMP(37)
     $ -1./2.*AMP(38)-1./2.*AMP(39)

      MATRIX = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX = MATRIX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
      ENDDO
      MATRIX_UD_UD_NOH=MATRIX
      ENDIF                ! CLOSES H1 IF 
      ENDIF                ! CLOSES H2 IF
      END


      SUBROUTINE SMATRIX_UD_US_NOH(P,H1,H2,ANS)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     MadGraph StandAlone Version
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: u d > u s ta+ ta- / h QED=4
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=64)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      INTEGER H1,H2
C     
C     LOCAL VARIABLES 
C     
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 MATRIX_UD_UD_NOH
      INTEGER IHEL,IDEN, I
      INTEGER JC(NEXTERNAL)
      LOGICAL GOODHEL(NCOMB)
      DATA NTRY/0/
      DATA GOODHEL/NCOMB*.FALSE./
      DATA (NHEL(I,   1),I=1,6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   6),I=1,6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   7),I=1,6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   8),I=1,6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   9),I=1,6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  10),I=1,6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  11),I=1,6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  12),I=1,6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  13),I=1,6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  14),I=1,6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  15),I=1,6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  16),I=1,6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  17),I=1,6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  18),I=1,6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  19),I=1,6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  20),I=1,6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  21),I=1,6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  30),I=1,6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  31),I=1,6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  32),I=1,6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  33),I=1,6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  34),I=1,6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  35),I=1,6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  36),I=1,6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  37),I=1,6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  38),I=1,6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  39),I=1,6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  40),I=1,6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  41),I=1,6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  42),I=1,6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  43),I=1,6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  44),I=1,6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  45),I=1,6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  54),I=1,6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  55),I=1,6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  56),I=1,6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  57),I=1,6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  58),I=1,6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  59),I=1,6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  60),I=1,6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  61),I=1,6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  62),I=1,6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  63),I=1,6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  64),I=1,6) / 1, 1, 1, 1, 1, 1/
      DATA IDEN/36/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      DO IHEL=1,NEXTERNAL
        JC(IHEL) = +1
      ENDDO
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          T=MATRIX_UD_UD_NOH(P ,H1,H2,NHEL(1,IHEL),JC(1))
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END


      REAL*8 FUNCTION MATRIX_UD_US_NOH(P,H1,H2,NHEL,IC)
C     
C     Generated by MadGraph 5 v. 1.5.12, 2013-08-21
C     By the MadGraph Development Team
C     Please visit us at https://launchpad.net/madgraph5
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: u d > u s ta+ ta- / h QED=4
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=11)
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=6)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=10, NCOLOR=1)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      COMPLEX*16 DUM0,DUM1
      DATA DUM0, DUM1/(0D0, 0D0), (1D0, 0D0)/
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
C     
C     COLOR DATA
C     
      DATA DENOM(1)/1/
      DATA (CF(I,  1),I=  1,  1) /    9/
C     1 T(3,2) T(4,1)

      INTEGER H1,H2
      REAL*8 MATRIX
      MATRIX_UD_US_NOH=0.D0
      IF(H1.EQ.0.  OR  .H1.EQ.IHEL(5))  THEN      
      IF(H2.EQ.0.  OR  .H2.EQ.IHEL(6))  THEN   

C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL IXXXXX(P(0,5),MTA,NHEL(5),-1*IC(5),W(1,5))
      CALL OXXXXX(P(0,6),MTA,NHEL(6),+1*IC(6),W(1,6))
      CALL FFV2_3(W(1,1),W(1,4),GC_101,MW,WW,W(1,7))
      CALL FFV2_3(W(1,2),W(1,3),GC_100,MW,WW,W(1,8))
      CALL FFV2_2(W(1,5),W(1,7),GC_108,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 1
      CALL FFV2_0(W(1,9),W(1,6),W(1,8),GC_108,AMP(1))
      CALL FFV1P0_3(W(1,5),W(1,6),GC_3,ZERO,ZERO,W(1,9))
C     Amplitude(s) for diagram number 2
      CALL VVV1_0(W(1,9),W(1,7),W(1,8),GC_4,AMP(2))
      CALL FFV2_4_3(W(1,5),W(1,6),GC_50,GC_59,MZ,WZ,W(1,10))
C     Amplitude(s) for diagram number 3
      CALL VVV1_0(W(1,7),W(1,8),W(1,10),GC_53,AMP(3))
      CALL FFV2_2(W(1,2),W(1,7),GC_100,ZERO,ZERO,W(1,6))
C     Amplitude(s) for diagram number 4
      CALL FFV1_0(W(1,6),W(1,3),W(1,9),GC_2,AMP(4))
      CALL FFV2_1(W(1,3),W(1,7),GC_100,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 5
      CALL FFV1_0(W(1,2),W(1,5),W(1,9),GC_1,AMP(5))
C     Amplitude(s) for diagram number 6
      CALL FFV2_5_0(W(1,6),W(1,3),W(1,10),GC_51,GC_58,AMP(6))
C     Amplitude(s) for diagram number 7
      CALL FFV2_3_0(W(1,2),W(1,5),W(1,10),GC_50,GC_58,AMP(7))
      CALL FFV2_2(W(1,1),W(1,8),GC_101,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 8
      CALL FFV1_0(W(1,5),W(1,4),W(1,9),GC_1,AMP(8))
      CALL FFV1_2(W(1,1),W(1,9),GC_2,ZERO,ZERO,W(1,2))
C     Amplitude(s) for diagram number 9
      CALL FFV2_0(W(1,2),W(1,4),W(1,8),GC_101,AMP(9))
C     Amplitude(s) for diagram number 10
      CALL FFV2_3_0(W(1,5),W(1,4),W(1,10),GC_50,GC_58,AMP(10))
      CALL FFV2_5_2(W(1,1),W(1,10),GC_51,GC_58,ZERO,ZERO,W(1,5))
C     Amplitude(s) for diagram number 11
      CALL FFV2_0(W(1,5),W(1,4),W(1,8),GC_101,AMP(11))
      JAMP(1)=+AMP(1)+AMP(2)+AMP(3)+AMP(4)+AMP(5)+AMP(6)+AMP(7)+AMP(8)
     $ +AMP(9)+AMP(10)+AMP(11)

      MATRIX = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX = MATRIX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
      ENDDO
      MATRIX_UD_US_NOH=MATRIX
      ENDIF                ! CLOSES H1 IF 
      ENDIF                ! CLOSES H2 IF
      END


C  ------- end subprocesses ud->jjtautau, no Higgs




C###############################################################################
C
C Copyright (c) 2010 The ALOHA Development team and Contributors
C
C This file is a part of the MadGraph 5 project, an application which
C automatically generates Feynman diagrams and matrix elements for arbitrary
C high-energy processes in the Standard Model and beyond.
C
C It is subject to the ALOHA license which should accompany this
C distribution.
C
C###############################################################################
      subroutine ixxxxx(p, fmass, nhel, nsf ,fi)
c
c This subroutine computes a fermion wavefunction with the flowing-IN
c fermion number.
c
c input:
c       real    p(0:3)         : four-momentum of fermion
c       real    fmass          : mass          of fermion
c       integer nhel = -1 or 1 : helicity      of fermion
c       integer nsf  = -1 or 1 : +1 for particle, -1 for anti-particle
c
c output:
c       complex fi(6)          : fermion wavefunction               |fi>
c
      implicit none
      double complex fi(6),chi(2)
      double precision p(0:3),sf(2),sfomeg(2),omega(2),fmass,
     &     pp,pp3,sqp0p3,sqm(0:1)
      integer nhel,nsf,ip,im,nh

      double precision rZero, rHalf, rTwo
      parameter( rZero = 0.0d0, rHalf = 0.5d0, rTwo = 2.0d0 )

c#ifdef HELAS_CHECK
c      double precision p2
c      double precision epsi
c      parameter( epsi = 2.0d-5 )
c      integer stdo
c      parameter( stdo = 6 )
c#endif
c
c#ifdef HELAS_CHECK
c      pp = sqrt(p(1)**2+p(2)**2+p(3)**2)
c      if ( abs(p(0))+pp.eq.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in ixxxxx is zero momentum'
c      endif
c      if ( p(0).le.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in ixxxxx has non-positive energy'
c         write(stdo,*)
c     &        '             : p(0) = ',p(0)
c      endif
c      p2 = (p(0)-pp)*(p(0)+pp)
c      if ( abs(p2-fmass**2).gt.p(0)**2*epsi ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in ixxxxx has inappropriate mass'
c         write(stdo,*)
c     &        '             : p**2 = ',p2,' : fmass**2 = ',fmass**2
c      endif
c      if (abs(nhel).ne.1) then
c         write(stdo,*) ' helas-error : nhel in ixxxxx is not -1,1'
c         write(stdo,*) '             : nhel = ',nhel
c      endif
c      if (abs(nsf).ne.1) then
c         write(stdo,*) ' helas-error : nsf in ixxxxx is not -1,1'
c         write(stdo,*) '             : nsf = ',nsf
c      endif
c#endif

      fi(1) = dcmplx(p(0),p(3))*nsf*-1
      fi(2) = dcmplx(p(1),p(2))*nsf*-1

      nh = nhel*nsf

      if ( fmass.ne.rZero ) then

         pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))

         if ( pp.eq.rZero ) then

            sqm(0) = dsqrt(abs(fmass)) ! possibility of negative fermion masses
            sqm(1) = sign(sqm(0),fmass) ! possibility of negative fermion masses
            ip = (1+nh)/2
            im = (1-nh)/2

            fi(3) = ip     * sqm(ip)
            fi(4) = im*nsf * sqm(ip)
            fi(5) = ip*nsf * sqm(im)
            fi(6) = im     * sqm(im)

         else

            sf(1) = dble(1+nsf+(1-nsf)*nh)*rHalf
            sf(2) = dble(1+nsf-(1-nsf)*nh)*rHalf
            omega(1) = dsqrt(p(0)+pp)
            omega(2) = fmass/omega(1)
            ip = (3+nh)/2
            im = (3-nh)/2
            sfomeg(1) = sf(1)*omega(ip)
            sfomeg(2) = sf(2)*omega(im)
            pp3 = max(pp+p(3),rZero)
            chi(1) = dcmplx( dsqrt(pp3*rHalf/pp) )
            if ( pp3.eq.rZero ) then
               chi(2) = dcmplx(-nh )
            else
               chi(2) = dcmplx( nh*p(1) , p(2) )/dsqrt(rTwo*pp*pp3)
            endif

            fi(3) = sfomeg(1)*chi(im)
            fi(4) = sfomeg(1)*chi(ip)
            fi(5) = sfomeg(2)*chi(im)
            fi(6) = sfomeg(2)*chi(ip)

         endif

      else

         if(p(1).eq.0d0.and.p(2).eq.0d0.and.p(3).lt.0d0) then
            sqp0p3 = 0d0
         else
            sqp0p3 = dsqrt(max(p(0)+p(3),rZero))*nsf
         end if
         chi(1) = dcmplx( sqp0p3 )
         if ( sqp0p3.eq.rZero ) then
            chi(2) = dcmplx(-nhel )*dsqrt(rTwo*p(0))
         else
            chi(2) = dcmplx( nh*p(1), p(2) )/sqp0p3
         endif
         if ( nh.eq.1 ) then
            fi(3) = dcmplx( rZero )
            fi(4) = dcmplx( rZero )
            fi(5) = chi(1)
            fi(6) = chi(2)
         else
            fi(3) = chi(2)
            fi(4) = chi(1)
            fi(5) = dcmplx( rZero )
            fi(6) = dcmplx( rZero )
         endif
      endif
c
      return
      end


      subroutine oxxxxx(p,fmass,nhel,nsf , fo)
c
c This subroutine computes a fermion wavefunction with the flowing-OUT
c fermion number.
c
c input:
c       real    p(0:3)         : four-momentum of fermion
c       real    fmass          : mass          of fermion
c       integer nhel = -1 or 1 : helicity      of fermion
c       integer nsf  = -1 or 1 : +1 for particle, -1 for anti-particle
c
c output:
c       complex fo(6)          : fermion wavefunction               <fo|
c
      implicit none
      double complex fo(6),chi(2)
      double precision p(0:3),sf(2),sfomeg(2),omega(2),fmass,
     &     pp,pp3,sqp0p3,sqm(0:1)
      integer nhel,nsf,nh,ip,im

      double precision rZero, rHalf, rTwo
      parameter( rZero = 0.0d0, rHalf = 0.5d0, rTwo = 2.0d0 )

c#ifdef HELAS_CHECK
c      double precision p2
c      double precision epsi
c      parameter( epsi = 2.0d-5 )
c      integer stdo
c      parameter( stdo = 6 )
c#endif
c
c#ifdef HELAS_CHECK
c      pp = sqrt(p(1)**2+p(2)**2+p(3)**2)
c      if ( abs(p(0))+pp.eq.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in oxxxxx is zero momentum'
c      endif
c      if ( p(0).le.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in oxxxxx has non-positive energy'
c         write(stdo,*)
c     &        '         : p(0) = ',p(0)
c      endif
c      p2 = (p(0)-pp)*(p(0)+pp)
c      if ( abs(p2-fmass**2).gt.p(0)**2*epsi ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in oxxxxx has inappropriate mass'
c         write(stdo,*)
c     &        '             : p**2 = ',p2,' : fmass**2 = ',fmass**2
c      endif
c      if ( abs(nhel).ne.1 ) then
c         write(stdo,*) ' helas-error : nhel in oxxxxx is not -1,1'
c         write(stdo,*) '             : nhel = ',nhel
c      endif
c      if ( abs(nsf).ne.1 ) then
c         write(stdo,*) ' helas-error : nsf in oxxxxx is not -1,1'
c         write(stdo,*) '             : nsf = ',nsf
c      endif
c#endif

      fo(1) = dcmplx(p(0),p(3))*nsf
      fo(2) = dcmplx(p(1),p(2))*nsf

      nh = nhel*nsf

      if ( fmass.ne.rZero ) then

         pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))

         if ( pp.eq.rZero ) then

            sqm(0) = dsqrt(abs(fmass)) ! possibility of negative fermion masses
            sqm(1) = sign(sqm(0),fmass) ! possibility of negative fermion masses
            im = nhel * (1+nh)/2
            ip = nhel * -1 * ((1-nh)/2)
            fo(3) = im     * sqm(abs(ip))
            fo(4) = ip*nsf * sqm(abs(ip))
            fo(5) = im*nsf * sqm(abs(im))
            fo(6) = ip     * sqm(abs(im))
         else

            pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))
            sf(1) = dble(1+nsf+(1-nsf)*nh)*rHalf
            sf(2) = dble(1+nsf-(1-nsf)*nh)*rHalf
            omega(1) = dsqrt(p(0)+pp)
            omega(2) = fmass/omega(1)
            ip = (3+nh)/2
            im = (3-nh)/2
            sfomeg(1) = sf(1)*omega(ip)
            sfomeg(2) = sf(2)*omega(im)
            pp3 = max(pp+p(3),rZero)
            chi(1) = dcmplx( dsqrt(pp3*rHalf/pp) )
            if ( pp3.eq.rZero ) then
               chi(2) = dcmplx(-nh )
            else
               chi(2) = dcmplx( nh*p(1) , -p(2) )/dsqrt(rTwo*pp*pp3)
            endif

            fo(3) = sfomeg(2)*chi(im)
            fo(4) = sfomeg(2)*chi(ip)
            fo(5) = sfomeg(1)*chi(im)
            fo(6) = sfomeg(1)*chi(ip)

         endif

      else

         if(p(1).eq.0d0.and.p(2).eq.0d0.and.p(3).lt.0d0) then
            sqp0p3 = 0d0
         else
            sqp0p3 = dsqrt(max(p(0)+p(3),rZero))*nsf
         end if
         chi(1) = dcmplx( sqp0p3 )
         if ( sqp0p3.eq.rZero ) then
            chi(2) = dcmplx(-nhel )*dsqrt(rTwo*p(0))
         else
            chi(2) = dcmplx( nh*p(1), -p(2) )/sqp0p3
         endif
         if ( nh.eq.1 ) then
            fo(3) = chi(1)
            fo(4) = chi(2)
            fo(5) = dcmplx( rZero )
            fo(6) = dcmplx( rZero )
         else
            fo(3) = dcmplx( rZero )
            fo(4) = dcmplx( rZero )
            fo(5) = chi(2)
            fo(6) = chi(1)
         endif

      endif
c
      return
      end

      subroutine pxxxxx(p,tmass,nhel,nst , tc)

c    CP3 2009.NOV

c This subroutine computes a PSEUDOR wavefunction.
c
c input:
c       real    p(0:3)         : four-momentum of tensor boson
c       real    tmass          : mass          of tensor boson
c       integer nhel           : helicity      of tensor boson
c                = -2,-1,0,1,2 : (0 is forbidden if tmass=0.0)
c       integer nst  = -1 or 1 : +1 for final, -1 for initial
c
c output:
c       complex tc(18)         : PSEUDOR  wavefunction    epsilon^mu^nu(t)
c
      implicit none
      double precision p(0:3), tmass
      integer nhel, nst
      double complex tc(18)

      double complex ft(6,4), ep(4), em(4), e0(4)
      double precision pt, pt2, pp, pzpt, emp, sqh, sqs
      integer i, j

      double precision rZero, rHalf, rOne, rTwo
      parameter( rZero = 0.0d0, rHalf = 0.5d0 )
      parameter( rOne = 1.0d0, rTwo = 2.0d0 )


      tc(3)=NHEL
      tc(1) = dcmplx(p(0),p(3))*nst
      tc(2) = dcmplx(p(1),p(2))*nst

      return
      end

      subroutine sxxxxx(p,nss , sc)
c
c This subroutine computes a complex SCALAR wavefunction.
c
c input:
c       real    p(0:3)         : four-momentum of scalar boson
c       integer nss  = -1 or 1 : +1 for final, -1 for initial
c
c output:
c       complex sc(3)          : scalar wavefunction                   s
c
      implicit none
      double complex sc(3)
      double precision p(0:3)
      integer nss

      double precision rOne
      parameter( rOne = 1.0d0 )

c#ifdef HELAS_CHECK
c      double precision p2
c      double precision epsi
c      parameter( epsi = 2.0d-5 )
c      double precision rZero
c      parameter( rZero = 0.0d0 )
c      integer stdo
c      parameter( stdo = 6 )
c#endif
c
c#ifdef HELAS_CHECK
c      if ( abs(p(0))+abs(p(1))+abs(p(2))+abs(p(3)).eq.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in sxxxxx is zero momentum'
c      endif
c      if ( p(0).le.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in sxxxxx has non-positive energy'
c         write(stdo,*)
c     &        '             : p(0) = ',p(0)
c      endif
c      p2 = p(0)**2-(p(1)**2+p(2)**2+p(3)**2)
c      if ( p2.lt.-p(0)**2*epsi ) then
c         write(stdo,*) ' helas-error : p(0:3) in sxxxxx is spacelike'
c         write(stdo,*) '             : p**2 = ',p2
c      endif
c      if ( abs(nss).ne.1 ) then
c         write(stdo,*) ' helas-error : nss in sxxxxx is not -1,1'
c         write(stdo,*) '             : nss = ',nss
c      endif
c#endif

      sc(3) = dcmplx( rOne )
      sc(1) = dcmplx(p(0),p(3))*nss
      sc(2) = dcmplx(p(1),p(2))*nss
c
      return
      end

      subroutine txxxxx(p,tmass,nhel,nst , tc)
c
c This subroutine computes a TENSOR wavefunction.
c
c input:
c       real    p(0:3)         : four-momentum of tensor boson
c       real    tmass          : mass          of tensor boson
c       integer nhel           : helicity      of tensor boson
c                = -2,-1,0,1,2 : (0 is forbidden if tmass=0.0)
c       integer nst  = -1 or 1 : +1 for final, -1 for initial
c
c output:
c       complex tc(18)         : tensor wavefunction    epsilon^mu^nu(t)
c
      implicit none
      double precision p(0:3), tmass
      integer nhel, nst
      double complex tc(18)

      double complex ft(6,4), ep(4), em(4), e0(4)
      double precision pt, pt2, pp, pzpt, emp, sqh, sqs
      integer i, j

      double precision rZero, rHalf, rOne, rTwo
      parameter( rZero = 0.0d0, rHalf = 0.5d0 )
      parameter( rOne = 1.0d0, rTwo = 2.0d0 )

      integer stdo
      parameter( stdo = 6 )

      sqh = sqrt(rHalf)
      sqs = sqrt(rHalf/3.d0)

      pt2 = p(1)**2 + p(2)**2
      pp = min(p(0),sqrt(pt2+p(3)**2))
      pt = min(pp,sqrt(pt2))

      ft(5,1) = dcmplx(p(0),p(3))*nst
      ft(6,1) = dcmplx(p(1),p(2))*nst

      if ( nhel.ge.0 ) then
c construct eps+
         if ( pp.eq.rZero ) then
            ep(1) = dcmplx( rZero )
            ep(2) = dcmplx( -sqh )
            ep(3) = dcmplx( rZero , nst*sqh )
            ep(4) = dcmplx( rZero )
         else
            ep(1) = dcmplx( rZero )
            ep(4) = dcmplx( pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = p(3)/(pp*pt)*sqh
               ep(2) = dcmplx( -p(1)*pzpt , -nst*p(2)/pt*sqh )
               ep(3) = dcmplx( -p(2)*pzpt ,  nst*p(1)/pt*sqh )
            else
               ep(2) = dcmplx( -sqh )
               ep(3) = dcmplx( rZero , nst*sign(sqh,p(3)) )
            endif
         endif
      end if

      if ( nhel.le.0 ) then
c construct eps-
         if ( pp.eq.rZero ) then
            em(1) = dcmplx( rZero )
            em(2) = dcmplx( sqh )
            em(3) = dcmplx( rZero , nst*sqh )
            em(4) = dcmplx( rZero )
         else
            em(1) = dcmplx( rZero )
            em(4) = dcmplx( -pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = -p(3)/(pp*pt)*sqh
               em(2) = dcmplx( -p(1)*pzpt , -nst*p(2)/pt*sqh )
               em(3) = dcmplx( -p(2)*pzpt ,  nst*p(1)/pt*sqh )
            else
               em(2) = dcmplx( sqh )
               em(3) = dcmplx( rZero , nst*sign(sqh,p(3)) )
            endif
         endif
      end if

      if ( abs(nhel).le.1 ) then
c construct eps0
         if ( pp.eq.rZero ) then
            e0(1) = dcmplx( rZero )
            e0(2) = dcmplx( rZero )
            e0(3) = dcmplx( rZero )
            e0(4) = dcmplx( rOne )
         else
            emp = p(0)/(tmass*pp)
            e0(1) = dcmplx( pp/tmass )
            e0(4) = dcmplx( p(3)*emp )
            if ( pt.ne.rZero ) then
               e0(2) = dcmplx( p(1)*emp )
               e0(3) = dcmplx( p(2)*emp )
            else
               e0(2) = dcmplx( rZero )
               e0(3) = dcmplx( rZero )
            endif
         end if
      end if

      if ( nhel.eq.2 ) then
         do j = 1,4
            do i = 1,4
               ft(i,j) = ep(i)*ep(j)
            end do
         end do
      else if ( nhel.eq.-2 ) then
         do j = 1,4
            do i = 1,4
               ft(i,j) = em(i)*em(j)
            end do
         end do
      else if (tmass.eq.0) then
         do j = 1,4
            do i = 1,4
               ft(i,j) = 0
            end do
         end do
      else if (tmass.ne.0) then
        if  ( nhel.eq.1 ) then
           do j = 1,4
              do i = 1,4
                 ft(i,j) = sqh*( ep(i)*e0(j) + e0(i)*ep(j) )
              end do
           end do
        else if ( nhel.eq.0 ) then
           do j = 1,4
              do i = 1,4
                 ft(i,j) = sqs*( ep(i)*em(j) + em(i)*ep(j)
     &                                + rTwo*e0(i)*e0(j) )
              end do
           end do
        else if ( nhel.eq.-1 ) then
           do j = 1,4
              do i = 1,4
                 ft(i,j) = sqh*( em(i)*e0(j) + e0(i)*em(j) )
              end do
           end do
        else
           write(stdo,*) 'invalid helicity in TXXXXX'
           stop
        end if
      end if

      tc(3) = ft(1,1)
      tc(4) = ft(1,2)
      tc(5) = ft(1,3)
      tc(6) = ft(1,4)
      tc(7) = ft(2,1)
      tc(8) = ft(2,2)
      tc(9) = ft(2,3)
      tc(10) = ft(2,4)
      tc(11) = ft(3,1)
      tc(12) = ft(3,2)
      tc(13) = ft(3,3)
      tc(14) = ft(3,4)
      tc(15) = ft(4,1)
      tc(16) = ft(4,2)
      tc(17) = ft(4,3)
      tc(18) = ft(4,4)
      tc(1) = ft(5,1)
      tc(2) = ft(6,1)

      return
      end


      subroutine vxxxxx(p,vmass,nhel,nsv , vc)
c
c This subroutine computes a VECTOR wavefunction.
c
c input:
c       real    p(0:3)         : four-momentum of vector boson
c       real    vmass          : mass          of vector boson
c       integer nhel = -1, 0, 1: helicity      of vector boson
c                                (0 is forbidden if vmass=0.0)
c       integer nsv  = -1 or 1 : +1 for final, -1 for initial
c
c output:
c       complex vc(6)          : vector wavefunction       epsilon^mu(v)
c
      implicit none
      double complex vc(6)
      double precision p(0:3),vmass,hel,hel0,pt,pt2,pp,pzpt,emp,sqh
      integer nhel,nsv,nsvahl

      double precision rZero, rHalf, rOne, rTwo
      parameter( rZero = 0.0d0, rHalf = 0.5d0 )
      parameter( rOne = 1.0d0, rTwo = 2.0d0 )

c#ifdef HELAS_CHECK
c      double precision p2
c      double precision epsi
c      parameter( epsi = 2.0d-5 )
c      integer stdo
c      parameter( stdo = 6 )
c#endif
c
c#ifdef HELAS_CHECK
c      pp = sqrt(p(1)**2+p(2)**2+p(3)**2)
c      if ( abs(p(0))+pp.eq.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in vxxxxx is zero momentum'
c      endif
c      if ( p(0).le.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in vxxxxx has non-positive energy'
c         write(stdo,*)
c     &        '             : p(0) = ',p(0)
c      endif
c      p2 = (p(0)+pp)*(p(0)-pp)
c      if ( abs(p2-vmass**2).gt.p(0)**2*2.e-5 ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in vxxxxx has inappropriate mass'
c         write(stdo,*)
c     &        '             : p**2 = ',p2,' : vmass**2 = ',vmass**2
c      endif
c      if ( vmass.ne.rZero ) then
c         if ( abs(nhel).gt.1 ) then
c            write(stdo,*) ' helas-error : nhel in vxxxxx is not -1,0,1'
c            write(stdo,*) '             : nhel = ',nhel
c         endif
c      else
c         if ( abs(nhel).ne.1 ) then
c            write(stdo,*) ' helas-error : nhel in vxxxxx is not -1,1'
c            write(stdo,*) '             : nhel = ',nhel
c         endif
c      endif
c      if ( abs(nsv).ne.1 ) then
c         write(stdo,*) ' helas-error : nsv in vmxxxx is not -1,1'
c         write(stdo,*) '             : nsv = ',nsv
c      endif
c#endif

      sqh = dsqrt(rHalf)
      hel = dble(nhel)
      nsvahl = nsv*dabs(hel)
      pt2 = p(1)**2+p(2)**2
      pp = min(p(0),dsqrt(pt2+p(3)**2))
      pt = min(pp,dsqrt(pt2))

      vc(1) = dcmplx(p(0),p(3))*nsv
      vc(2) = dcmplx(p(1),p(2))*nsv

c#ifdef HELAS_CHECK
c nhel=4 option for scalar polarization
c      if( nhel.eq.4 ) then
c         if( vmass.eq.rZero ) then
c            vc(1) = rOne
c            vc(2) = p(1)/p(0)
c            vc(3) = p(2)/p(0)
c            vc(4) = p(3)/p(0)
c         else
c            vc(1) = p(0)/vmass
c            vc(2) = p(1)/vmass
c            vc(3) = p(2)/vmass
c            vc(4) = p(3)/vmass
c         endif
c         return
c      endif
c#endif

      if ( vmass.ne.rZero ) then

         hel0 = rOne-dabs(hel)

         if ( pp.eq.rZero ) then

            vc(3) = dcmplx( rZero )
            vc(4) = dcmplx(-hel*sqh )
            vc(5) = dcmplx( rZero , nsvahl*sqh )
            vc(6) = dcmplx( hel0 )

         else

            emp = p(0)/(vmass*pp)
            vc(3) = dcmplx( hel0*pp/vmass )
            vc(6) = dcmplx( hel0*p(3)*emp+hel*pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = p(3)/(pp*pt)*sqh*hel
               vc(4) = dcmplx( hel0*p(1)*emp-p(1)*pzpt ,
     &                         -nsvahl*p(2)/pt*sqh       )
               vc(5) = dcmplx( hel0*p(2)*emp-p(2)*pzpt ,
     &                          nsvahl*p(1)/pt*sqh       )
            else
               vc(4) = dcmplx( -hel*sqh )
               vc(5) = dcmplx( rZero , nsvahl*sign(sqh,p(3)) )
            endif

         endif

      else

         pp = p(0)
         pt = sqrt(p(1)**2+p(2)**2)
         vc(3) = dcmplx( rZero )
         vc(6) = dcmplx( hel*pt/pp*sqh )
         if ( pt.ne.rZero ) then
            pzpt = p(3)/(pp*pt)*sqh*hel
            vc(4) = dcmplx( -p(1)*pzpt , -nsv*p(2)/pt*sqh )
            vc(5) = dcmplx( -p(2)*pzpt ,  nsv*p(1)/pt*sqh )
         else
            vc(4) = dcmplx( -hel*sqh )
            vc(5) = dcmplx( rZero , nsv*sign(sqh,p(3)) )
         endif

      endif
c
      return
      end

      subroutine boostx(p,q , pboost)
c
c This subroutine performs the Lorentz boost of a four-momentum.  The
c momentum p is assumed to be given in the rest frame of q.  pboost is
c the momentum p boosted to the frame in which q is given.  q must be a
c timelike momentum.
c
c input:
c       real    p(0:3)         : four-momentum p in the q rest  frame
c       real    q(0:3)         : four-momentum q in the boosted frame
c
c output:
c       real    pboost(0:3)    : four-momentum p in the boosted frame
c
      implicit none
      double precision p(0:3),q(0:3),pboost(0:3),pq,qq,m,lf

      double precision rZero
      parameter( rZero = 0.0d0 )

c#ifdef HELAS_CHECK
c      integer stdo
c      parameter( stdo = 6 )
c      double precision pp
c#endif
c
      qq = q(1)**2+q(2)**2+q(3)**2

c#ifdef HELAS_CHECK
c      if (abs(p(0))+abs(p(1))+abs(p(2))+abs(p(3)).eq.rZero) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in boostx is zero momentum'
c      endif
c      if (abs(q(0))+qq.eq.rZero) then
c         write(stdo,*)
c     &        ' helas-error : q(0:3) in boostx is zero momentum'
c      endif
c      if (p(0).le.rZero) then
c         write(stdo,*)
c     &        ' helas-warn  : p(0:3) in boostx has not positive energy'
c         write(stdo,*)
c     &        '             : p(0) = ',p(0)
c      endif
c      if (q(0).le.rZero) then
c         write(stdo,*)
c     &        ' helas-error : q(0:3) in boostx has not positive energy'
c         write(stdo,*)
c     &        '             : q(0) = ',q(0)
c      endif
c      pp=p(0)**2-p(1)**2-p(2)**2-p(3)**2
c      if (pp.lt.rZero) then
c         write(stdo,*)
c     &        ' helas-warn  : p(0:3) in boostx is spacelike'
c         write(stdo,*)
c     &        '             : p**2 = ',pp
c      endif
c      if (q(0)**2-qq.le.rZero) then
c         write(stdo,*)
c     &        ' helas-error : q(0:3) in boostx is not timelike'
c         write(stdo,*)
c     &        '             : q**2 = ',q(0)**2-qq
c      endif
c      if (qq.eq.rZero) then
c         write(stdo,*)
c     &   ' helas-warn  : q(0:3) in boostx has zero spacial components'
c      endif
c#endif

      if ( qq.ne.rZero ) then
         pq = p(1)*q(1)+p(2)*q(2)+p(3)*q(3)
         m = sqrt(max(q(0)**2-qq,1d-99))
         lf = ((q(0)-m)*pq/qq+p(0))/m
         pboost(0) = (p(0)*q(0)+pq)/m
         pboost(1) =  p(1)+q(1)*lf
         pboost(2) =  p(2)+q(2)*lf
         pboost(3) =  p(3)+q(3)*lf
      else
         pboost(0) = p(0)
         pboost(1) = p(1)
         pboost(2) = p(2)
         pboost(3) = p(3)
      endif
c
      return
      end

      subroutine momntx(energy,mass,costh,phi , p)
c
c This subroutine sets up a four-momentum from the four inputs.
c
c input:
c       real    energy         : energy
c       real    mass           : mass
c       real    costh          : cos(theta)
c       real    phi            : azimuthal angle
c
c output:
c       real    p(0:3)         : four-momentum
c
      implicit none
      double precision p(0:3),energy,mass,costh,phi,pp,sinth

      double precision rZero, rOne
      parameter( rZero = 0.0d0, rOne = 1.0d0 )

c#ifdef HELAS_CHECK
c      double precision rPi, rTwo
c      parameter( rPi = 3.14159265358979323846d0, rTwo = 2.d0 )
c      integer stdo
c      parameter( stdo = 6 )
c#endif
c
c#ifdef HELAS_CHECK
c      if (energy.lt.mass) then
c         write(stdo,*)
c     &        ' helas-error : energy in momntx is less than mass'
c         write(stdo,*)
c     &        '             : energy = ',energy,' : mass = ',mass
c      endif
c      if (mass.lt.rZero) then
c         write(stdo,*) ' helas-error : mass in momntx is negative'
c         write(stdo,*) '             : mass = ',mass
c      endif
c      if (abs(costh).gt.rOne) then
c         write(stdo,*) ' helas-error : costh in momntx is improper'
c         write(stdo,*) '             : costh = ',costh
c      endif
c      if (phi.lt.rZero .or. phi.gt.rTwo*rPi) then
c         write(stdo,*)
c     &   ' helas-warn  : phi in momntx does not lie on 0.0 thru 2.0*pi'
c         write(stdo,*)
c     &   '             : phi = ',phi
c      endif
c#endif

      p(0) = energy

      if ( energy.eq.mass ) then

         p(1) = rZero
         p(2) = rZero
         p(3) = rZero

      else

         pp = sqrt((energy-mass)*(energy+mass))
         sinth = sqrt((rOne-costh)*(rOne+costh))
         p(3) = pp*costh
         if ( phi.eq.rZero ) then
            p(1) = pp*sinth
            p(2) = rZero
         else
            p(1) = pp*sinth*cos(phi)
            p(2) = pp*sinth*sin(phi)
         endif

      endif
c
      return
      end
      subroutine rotxxx(p,q , prot)
c
c This subroutine performs the spacial rotation of a four-momentum.
c the momentum p is assumed to be given in the frame where the spacial
c component of q points the positive z-axis.  prot is the momentum p
c rotated to the frame where q is given.
c
c input:
c       real    p(0:3)         : four-momentum p in q(1)=q(2)=0 frame
c       real    q(0:3)         : four-momentum q in the rotated frame
c
c output:
c       real    prot(0:3)      : four-momentum p in the rotated frame
c
      implicit none
      double precision p(0:3),q(0:3),prot(0:3),qt2,qt,psgn,qq,p1

      double precision rZero, rOne
      parameter( rZero = 0.0d0, rOne = 1.0d0 )

c#ifdef HELAS_CHECK
c      integer stdo
c      parameter( stdo = 6 )
c#endif
c
      prot(0) = p(0)

      qt2 = q(1)**2 + q(2)**2

c#ifdef HELAS_CHECK
c      if (abs(p(0))+abs(p(1))+abs(p(2))+abs(p(3)).eq.rZero) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in rotxxx is zero momentum'
c      endif
c      if (abs(q(0))+abs(q(3))+qt2.eq.rZero) then
c         write(stdo,*)
c     &        ' helas-error : q(0:3) in rotxxx is zero momentum'
c      endif
c      if (qt2+abs(q(3)).eq.rZero) then
c         write(stdo,*)
c     &   ' helas-warn  : q(0:3) in rotxxx has zero spacial momentum'
c      endif
c#endif

      if ( qt2.eq.rZero ) then
          if ( q(3).eq.rZero ) then
             prot(1) = p(1)
             prot(2) = p(2)
             prot(3) = p(3)
          else
             psgn = dsign(rOne,q(3))
             prot(1) = p(1)*psgn
             prot(2) = p(2)*psgn
             prot(3) = p(3)*psgn
          endif
      else
          qq = sqrt(qt2+q(3)**2)
          qt = sqrt(qt2)
          p1 = p(1)
          prot(1) = q(1)*q(3)/qq/qt*p1 -q(2)/qt*p(2) +q(1)/qq*p(3)
          prot(2) = q(2)*q(3)/qq/qt*p1 +q(1)/qt*p(2) +q(2)/qq*p(3)
          prot(3) =          -qt/qq*p1               +q(3)/qq*p(3)
      endif
c
      return
      end

      subroutine mom2cx(esum,mass1,mass2,costh1,phi1 , p1,p2)
c
c This subroutine sets up two four-momenta in the two particle rest
c frame.
c
c input:
c       real    esum           : energy sum of particle 1 and 2
c       real    mass1          : mass            of particle 1
c       real    mass2          : mass            of particle 2
c       real    costh1         : cos(theta)      of particle 1
c       real    phi1           : azimuthal angle of particle 1
c
c output:
c       real    p1(0:3)        : four-momentum of particle 1
c       real    p2(0:3)        : four-momentum of particle 2
c     
      implicit none
      double precision p1(0:3),p2(0:3),
     &     esum,mass1,mass2,costh1,phi1,md2,ed,pp,sinth1

      double precision rZero, rHalf, rOne, rTwo
      parameter( rZero = 0.0d0, rHalf = 0.5d0 )
      parameter( rOne = 1.0d0, rTwo = 2.0d0 )

c#ifdef HELAS_CHECK
c      double precision rPi
c      parameter( rPi = 3.14159265358979323846d0 )
c      integer stdo
c      parameter( stdo = 6 )
c#endif
cc
c#ifdef HELAS_CHECK
c      if (esum.lt.mass1+mass2) then
c         write(stdo,*)
c     &        ' helas-error : esum in mom2cx is less than mass1+mass2'
c         write(stdo,*)
c     &        '             : esum = ',esum,
c     &        ' : mass1+mass2 = ',mass1,mass2
c      endif
c      if (mass1.lt.rZero) then
c         write(stdo,*) ' helas-error : mass1 in mom2cx is negative'
c         write(stdo,*) '             : mass1 = ',mass1
c      endif
c      if (mass2.lt.rZero) then
c         write(stdo,*) ' helas-error : mass2 in mom2cx is negative'
c         write(stdo,*) '             : mass2 = ',mass2
c      endif
c      if (abs(costh1).gt.1.) then
c         write(stdo,*) ' helas-error : costh1 in mom2cx is improper'
c         write(stdo,*) '             : costh1 = ',costh1
c      endif
c      if (phi1.lt.rZero .or. phi1.gt.rTwo*rPi) then
c         write(stdo,*)
c     &   ' helas-warn  : phi1 in mom2cx does not lie on 0.0 thru 2.0*pi'
c         write(stdo,*) 
c     &   '             : phi1 = ',phi1
c      endif
c#endif

      md2 = (mass1-mass2)*(mass1+mass2)
      ed = md2/esum
      if ( mass1*mass2.eq.rZero ) then
         pp = (esum-abs(ed))*rHalf
      else
         pp = sqrt(max((md2/esum)**2-rTwo*(mass1**2+mass2**2)+esum**2,1d-99))*rHalf
      endif
      sinth1 = sqrt((rOne-costh1)*(rOne+costh1))

      p1(0) = max((esum+ed)*rHalf,rZero)
      p1(1) = pp*sinth1*cos(phi1)
      p1(2) = pp*sinth1*sin(phi1)
      p1(3) = pp*costh1

      p2(0) = max((esum-ed)*rHalf,rZero)
      p2(1) = -p1(1)
      p2(2) = -p1(2)
      p2(3) = -p1(3)
c
      return
      end
      subroutine irxxxx(p,rmass,nhel,nsr , ri)
c
c This subroutine computes a Rarita-Schwinger wavefunction of spin-3/2
c fermion with the flowing-IN fermion number.
c
c input:
c       real    p(0:3)           : four-momentum of RS fermion
c       real    rmass            : mass          of RS fermion
c       integer nhel = -3,-1,1,3 : helicity      of RS fermion
c                                  (1- and 1 is forbidden if rmass = 0)
c       integer nsr  = -1 or 1   : +1 for particle, -1 for anti-particle
c
c output:
c       complex ri(18)           : RS fermion wavefunction         |ri>v   
c     
c- by K.Mawatari - 2008/02/26
c
      implicit none
      double precision p(0:3),rmass
      integer nhel,nsr
      double complex ri(18)

      double complex rc(6,4),ep(4),em(4),e0(4),fip(4),fim(4),chi(2)
      double precision pp,pt2,pt,pzpt,emp, sf(2),sfomeg(2),omega(2),pp3,
     &                 sqp0p3,sqm      
      integer i,j,nsv,ip,im,nh

      double precision rZero, rHalf, rOne, rTwo, rThree, sqh,sq2,sq3
      parameter( rZero = 0.0d0, rHalf = 0.5d0 )
      parameter( rOne = 1.0d0, rTwo = 2.0d0, rThree = 3.0d0 )

c#ifdef HELAS_CHECK
c      double precision p2
c      double precision epsi
c      parameter( epsi = 2.0d-5 )
c      integer stdo
c      parameter( stdo = 6 )
c#endif
c
c#ifdef HELAS_CHECK
c      pp = sqrt(p(1)**2+p(2)**2+p(3)**2)
c      if ( abs(p(0))+pp.eq.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in irxxxx is zero momentum'
c      endif
c      if ( p(0).le.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in irxxxx has non-positive energy'
c         write(stdo,*)
c     &        '             : p(0) = ',p(0)
c      endif
c      p2 = (p(0)-pp)*(p(0)+pp)
c      if ( abs(p2-rmass**2).gt.p(0)**2*epsi ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in irxxxx has inappropriate mass'
c         write(stdo,*)
c     &        '             : p**2 = ',p2,' : rmass**2 = ',rmass**2
c      endif
c      if (abs(nhel).gt.3 .or. abs(nhel).eq.2 .or. abs(nhel).eq.0 ) then
c         write(stdo,*) ' helas-error : nhel in irxxxx is not -3,-1,1,3'
c         write(stdo,*) '             : nhel = ',nhel
c      endif
c      if (abs(nsr).ne.1) then
c         write(stdo,*) ' helas-error : nsr in irxxxx is not -1,1'
c         write(stdo,*) '             : nsr = ',nsr
c      endif
c#endif

      sqh = sqrt(rHalf)
      sq2 = sqrt(rTwo)
      sq3 = sqrt(rThree)

      pt2 = p(1)**2 + p(2)**2
      pp = min(p(0),sqrt(pt2+p(3)**2))
      pt = min(pp,sqrt(pt2))

      rc(5,1) = -1*dcmplx(p(0),p(3))*nsr
      rc(6,1) = -1*dcmplx(p(1),p(2))*nsr

      nsv = -nsr ! nsv=+1 for final, -1 for initial

      if ( nhel.ge.1 ) then 
c construct eps+
         if ( pp.eq.rZero ) then
            ep(1) = dcmplx( rZero )
            ep(2) = dcmplx( -sqh )
            ep(3) = dcmplx( rZero , nsv*sqh )
            ep(4) = dcmplx( rZero )
         else
            ep(1) = dcmplx( rZero )
            ep(4) = dcmplx( pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = p(3)/(pp*pt)*sqh
               ep(2) = dcmplx( -p(1)*pzpt , -nsv*p(2)/pt*sqh )
               ep(3) = dcmplx( -p(2)*pzpt ,  nsv*p(1)/pt*sqh )
            else
               ep(2) = dcmplx( -sqh )
               ep(3) = dcmplx( rZero , nsv*sign(sqh,p(3)) )
            endif
         endif
      end if

      if ( nhel.le.-1 ) then 
c construct eps-
         if ( pp.eq.rZero ) then
            em(1) = dcmplx( rZero )
            em(2) = dcmplx( sqh )
            em(3) = dcmplx( rZero , nsv*sqh )
            em(4) = dcmplx( rZero )
         else
            em(1) = dcmplx( rZero )
            em(4) = dcmplx( -pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = -p(3)/(pp*pt)*sqh
               em(2) = dcmplx( -p(1)*pzpt , -nsv*p(2)/pt*sqh )
               em(3) = dcmplx( -p(2)*pzpt ,  nsv*p(1)/pt*sqh )
            else
               em(2) = dcmplx( sqh )
               em(3) = dcmplx( rZero , nsv*sign(sqh,p(3)) )
            endif
         endif
      end if

      if ( abs(nhel).le.1 ) then  
c construct eps0
         if ( pp.eq.rZero ) then
            e0(1) = dcmplx( rZero )
            e0(2) = dcmplx( rZero )
            e0(3) = dcmplx( rZero )
            e0(4) = dcmplx( rOne )
         else
            emp = p(0)/(rmass*pp)
            e0(1) = dcmplx( pp/rmass )
            e0(4) = dcmplx( p(3)*emp )
            if ( pt.ne.rZero ) then
               e0(2) = dcmplx( p(1)*emp )
               e0(3) = dcmplx( p(2)*emp )
            else
               e0(2) = dcmplx( rZero )
               e0(3) = dcmplx( rZero )
            endif
         end if
      end if

      if ( nhel.ge.-1 ) then
c constract spinor+ 
       nh = nsr
       if ( rmass.ne.rZero ) then
         pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))
         if ( pp.eq.rZero ) then
            sqm = dsqrt(rmass)
            ip = (1+nh)/2
            im = (1-nh)/2
              fip(1) = ip     * sqm
              fip(2) = im*nsr * sqm
              fip(3) = ip*nsr * sqm
              fip(4) = im     * sqm
         else
            sf(1) = dble(1+nsr+(1-nsr)*nh)*rHalf
            sf(2) = dble(1+nsr-(1-nsr)*nh)*rHalf
            omega(1) = dsqrt(p(0)+pp)
            omega(2) = rmass/omega(1)
            ip = (3+nh)/2
            im = (3-nh)/2
            sfomeg(1) = sf(1)*omega(ip)
            sfomeg(2) = sf(2)*omega(im)
            pp3 = max(pp+p(3),rZero)
            chi(1) = dcmplx( dsqrt(pp3*rHalf/pp) )
            if ( pp3.eq.rZero ) then
               chi(2) = dcmplx(-nh )
            else
               chi(2) = dcmplx( nh*p(1) , p(2) )/dsqrt(rTwo*pp*pp3)
            endif
              fip(1) = sfomeg(1)*chi(im)
              fip(2) = sfomeg(1)*chi(ip)
              fip(3) = sfomeg(2)*chi(im)
              fip(4) = sfomeg(2)*chi(ip)
         endif
       else
         sqp0p3 = dsqrt(max(p(0)+p(3),rZero))*nsr
         chi(1) = dcmplx( sqp0p3 )
         if ( sqp0p3.eq.rZero ) then
            chi(2) = dcmplx(-nhel )*dsqrt(rTwo*p(0))
         else
            chi(2) = dcmplx( nh*p(1), p(2) )/sqp0p3
         endif
         if ( nh.eq.1 ) then
            fip(1) = dcmplx( rZero )
            fip(2) = dcmplx( rZero )
            fip(3) = chi(1)
            fip(4) = chi(2)
         else
            fip(1) = chi(2)
            fip(2) = chi(1)
            fip(3) = dcmplx( rZero )
            fip(4) = dcmplx( rZero )
         endif
       endif
      end if

      if ( nhel.le.1 ) then
c constract spinor-
       nh = -nsr
       if ( rmass.ne.rZero ) then
         pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))
         if ( pp.eq.rZero ) then
            sqm = dsqrt(rmass)
            ip = (1+nh)/2
            im = (1-nh)/2
              fim(1) = ip     * sqm
              fim(2) = im*nsr * sqm
              fim(3) = ip*nsr * sqm
              fim(4) = im     * sqm
         else
            sf(1) = dble(1+nsr+(1-nsr)*nh)*rHalf
            sf(2) = dble(1+nsr-(1-nsr)*nh)*rHalf
            omega(1) = dsqrt(p(0)+pp)
            omega(2) = rmass/omega(1)
            ip = (3+nh)/2
            im = (3-nh)/2
            sfomeg(1) = sf(1)*omega(ip)
            sfomeg(2) = sf(2)*omega(im)
            pp3 = max(pp+p(3),rZero)
            chi(1) = dcmplx( dsqrt(pp3*rHalf/pp) )
            if ( pp3.eq.rZero ) then
               chi(2) = dcmplx(-nh )
            else
               chi(2) = dcmplx( nh*p(1) , p(2) )/dsqrt(rTwo*pp*pp3)
            endif
              fim(1) = sfomeg(1)*chi(im)
              fim(2) = sfomeg(1)*chi(ip)
              fim(3) = sfomeg(2)*chi(im)
              fim(4) = sfomeg(2)*chi(ip)
         endif
       else
         sqp0p3 = dsqrt(max(p(0)+p(3),rZero))*nsr
         chi(1) = dcmplx( sqp0p3 )
         if ( sqp0p3.eq.rZero ) then
            chi(2) = dcmplx(-nhel )*dsqrt(rTwo*p(0))
         else
            chi(2) = dcmplx( nh*p(1), p(2) )/sqp0p3
         endif
         if ( nh.eq.1 ) then
            fim(1) = dcmplx( rZero )
            fim(2) = dcmplx( rZero )
            fim(3) = chi(1)
            fim(4) = chi(2)
         else
            fim(1) = chi(2)
            fim(2) = chi(1)
            fim(3) = dcmplx( rZero )
            fim(4) = dcmplx( rZero )
         endif
       endif
      end if

c spin-3/2 fermion wavefunction
      if ( nhel.eq.3 ) then
         do j = 1,4
            do i = 1,4
               rc(i,j) = ep(i)*fip(j)
            end do
         end do
      else if ( nhel.eq.1 ) then
         do j = 1,4
            do i = 1,4
              if     ( pt.eq.rZero .and. p(3).ge.0d0 ) then 
               rc(i,j) =  sq2/sq3*e0(i)*fip(j) +rOne/sq3*ep(i)*fim(j)
              elseif ( pt.eq.rZero .and. p(3).lt.0d0 ) then 
               rc(i,j) =  sq2/sq3*e0(i)*fip(j) -rOne/sq3*ep(i)*fim(j)
              else
               rc(i,j) =  sq2/sq3*e0(i)*fip(j) 
     &                  +rOne/sq3*ep(i)*fim(j) *dcmplx(P(1),nsr*P(2))/pt  
              endif
            end do
         end do
      else if ( nhel.eq.-1 ) then
         do j = 1,4
            do i = 1,4
              if     ( pt.eq.rZero .and.p(3).ge.0d0 ) then 
               rc(i,j) = rOne/sq3*em(i)*fip(j) +sq2/sq3*e0(i)*fim(j)
              elseif ( pt.eq.rZero .and.p(3).lt.0d0 ) then 
               rc(i,j) = rOne/sq3*em(i)*fip(j) -sq2/sq3*e0(i)*fim(j)
              else
               rc(i,j) = rOne/sq3*em(i)*fip(j) 
     &                  + sq2/sq3*e0(i)*fim(j) *dcmplx(P(1),nsr*P(2))/pt  
              endif
            end do
         end do
      else
         do j = 1,4
            do i = 1,4
              if     ( pt.eq.rZero .and. p(3).ge.0d0 ) then 
               rc(i,j) =  em(i)*fim(j)
              elseif ( pt.eq.rZero .and. p(3).lt.0d0 ) then 
               rc(i,j) = -em(i)*fim(j)
              else
               rc(i,j) =  em(i)*fim(j) *dcmplx(P(1),nsr*P(2))/pt  
              endif
            end do
         end do
      end if

      ri(3) = rc(1,1)
      ri(4) = rc(1,2)
      ri(5) = rc(1,3)
      ri(6) = rc(1,4)
      ri(7) = rc(2,1)
      ri(8) = rc(2,2)
      ri(9) = rc(2,3)
      ri(10) = rc(2,4)
      ri(11) = rc(3,1)
      ri(12) = rc(3,2)
      ri(13) = rc(3,3)
      ri(14) = rc(3,4)
      ri(15) = rc(4,1)
      ri(16) = rc(4,2)
      ri(17) = rc(4,3)
      ri(18) = rc(4,4)
      ri(1) = rc(5,1)
      ri(2) = rc(6,1)

      return
      end
      subroutine orxxxx(p,rmass,nhel,nsr , ro)
c
c This subroutine computes a Rarita-Schwinger wavefunction of spin-3/2
c fermion with the flowing-IN fermion number.
c
c input:
c       real    p(0:3)           : four-momentum of RS fermion
c       real    rmass            : mass          of RS fermion
c       integer nhel = -3,-1,1,3 : helicity      of RS fermion
c                                  (1- and 1 is forbidden if rmass = 0)
c       integer nsr  = -1 or 1   : +1 for particle, -1 for anti-particle
c
c output:
c       complex ro(18)           : RS fermion wavefunction         |ro>v   
c     
c- by Y.Takaesu - 2011/01/11
c
      implicit none
      double precision p(0:3),rmass
      integer nhel,nsr
      double complex ro(18),fipp(4),fimm(4)

      double complex rc(6,4),ep(4),em(4),e0(4),fop(4),fom(4),chi(2)
      double precision pp,pt2,pt,pzpt,emp, sf(2),sfomeg(2),omega(2),pp3,
     &                 sqp0p3,sqm(0:1)      
      integer i,j,nsv,ip,im,nh

      double precision rZero, rHalf, rOne, rTwo, rThree, sqh,sq2,sq3
      parameter( rZero = 0.0d0, rHalf = 0.5d0 )
      parameter( rOne = 1.0d0, rTwo = 2.0d0, rThree = 3.0d0 )

c#ifdef HELAS_CHECK
c      double precision p2
c      double precision epsi
c      parameter( epsi = 2.0d-5 )
c      integer stdo
c      parameter( stdo = 6 )
c#endif
c
c#ifdef HELAS_CHECK
c      pp = sqrt(p(1)**2+p(2)**2+p(3)**2)
c      if ( abs(p(0))+pp.eq.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in orxxxx is zero momentum'
c      endif
c      if ( p(0).le.rZero ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in orxxxx has non-positive energy'
c         write(stdo,*)
c     &        '             : p(0) = ',p(0)
c      endif
c      p2 = (p(0)-pp)*(p(0)+pp)
c      if ( abs(p2-rmass**2).gt.p(0)**2*epsi ) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in orxxxx has inappropriate mass'
c         write(stdo,*)
c     &        '             : p**2 = ',p2,' : rmass**2 = ',rmass**2
c      endif
c      if (abs(nhel).gt.3 .or. abs(nhel).eq.2 .or. abs(nhel).eq.0 ) then
c         write(stdo,*) ' helas-error : nhel in orxxxx is not -3,-1,1,3'
c         write(stdo,*) '             : nhel = ',nhel
c      endif
c      if (abs(nsr).ne.1) then
c         write(stdo,*) ' helas-error : nsr in orxxxx is not -1,1'
c         write(stdo,*) '             : nsr = ',nsr
c      endif
c#endif

      sqh = sqrt(rHalf)
      sq2 = sqrt(rTwo)
      sq3 = sqrt(rThree)

      pt2 = p(1)**2 + p(2)**2
      pp = min(p(0),sqrt(pt2+p(3)**2))
      pt = min(pp,sqrt(pt2))

      rc(5,1) = dcmplx(p(0),p(3))*nsr
      rc(6,1) = dcmplx(p(1),p(2))*nsr

      nsv = nsr ! nsv=+1 for final, -1 for initial

      if ( nhel.ge.1 ) then 
c construct eps+
         if ( pp.eq.rZero ) then
            ep(1) = dcmplx( rZero )
            ep(2) = dcmplx( -sqh )
            ep(3) = dcmplx( rZero , nsv*sqh )
            ep(4) = dcmplx( rZero )
         else
            ep(1) = dcmplx( rZero )
            ep(4) = dcmplx( pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = p(3)/(pp*pt)*sqh
               ep(2) = dcmplx( -p(1)*pzpt , -nsv*p(2)/pt*sqh )
               ep(3) = dcmplx( -p(2)*pzpt ,  nsv*p(1)/pt*sqh )
            else
               ep(2) = dcmplx( -sqh )
               ep(3) = dcmplx( rZero , nsv*sign(sqh,p(3)) )
            endif
         endif
      end if

      if ( nhel.le.-1 ) then 
c construct eps-
         if ( pp.eq.rZero ) then
            em(1) = dcmplx( rZero )
            em(2) = dcmplx( sqh )
            em(3) = dcmplx( rZero , nsv*sqh )
            em(4) = dcmplx( rZero )
         else
            em(1) = dcmplx( rZero )
            em(4) = dcmplx( -pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = -p(3)/(pp*pt)*sqh
               em(2) = dcmplx( -p(1)*pzpt , -nsv*p(2)/pt*sqh )
               em(3) = dcmplx( -p(2)*pzpt ,  nsv*p(1)/pt*sqh )
            else
               em(2) = dcmplx( sqh )
               em(3) = dcmplx( rZero , nsv*sign(sqh,p(3)) )
            endif
         endif
      end if

      if ( abs(nhel).le.1 ) then  
c construct eps0
         if ( pp.eq.rZero ) then
            e0(1) = dcmplx( rZero )
            e0(2) = dcmplx( rZero )
            e0(3) = dcmplx( rZero )
            e0(4) = dcmplx( rOne )
         else
            emp = p(0)/(rmass*pp)
            e0(1) = dcmplx( pp/rmass )
            e0(4) = dcmplx( p(3)*emp )
            if ( pt.ne.rZero ) then
               e0(2) = dcmplx( p(1)*emp )
               e0(3) = dcmplx( p(2)*emp )
            else
               e0(2) = dcmplx( rZero )
               e0(3) = dcmplx( rZero )
            endif
         end if
      end if

      if ( nhel.ge.-1 ) then
c constract spinor+ 
       nh = nsr

       if ( rmass.ne.rZero ) then

         pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))

         if ( pp.eq.rZero ) then
            
            sqm(0) = dsqrt(abs(rmass)) ! possibility of negative fermion masses
            sqm(1) = sign(sqm(0),rmass) ! possibility of negative fermion masses
            ip = -((1+nh)/2)
            im =  (1-nh)/2
            
            fop(1) = im     * sqm(im)
            fop(2) = ip*nsr * sqm(im)
            fop(3) = im*nsr * sqm(-ip)
            fop(4) = ip     * sqm(-ip)
            
         else
            
            pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))
            sf(1) = dble(1+nsr+(1-nsr)*nh)*rHalf
            sf(2) = dble(1+nsr-(1-nsr)*nh)*rHalf
            omega(1) = dsqrt(p(0)+pp)
            omega(2) = rmass/omega(1)
            ip = (3+nh)/2
            im = (3-nh)/2
            sfomeg(1) = sf(1)*omega(ip)
            sfomeg(2) = sf(2)*omega(im)
            pp3 = max(pp+p(3),rZero)
            chi(1) = dcmplx( dsqrt(pp3*rHalf/pp) )
            if ( pp3.eq.rZero ) then
               chi(2) = dcmplx(-nh )
            else
               chi(2) = dcmplx( nh*p(1) , -p(2) )/dsqrt(rTwo*pp*pp3)
            endif
            
            fop(1) = sfomeg(2)*chi(im)
            fop(2) = sfomeg(2)*chi(ip)
            fop(3) = sfomeg(1)*chi(im)
            fop(4) = sfomeg(1)*chi(ip)

         endif
         
      else
         
         if(p(1).eq.0d0.and.p(2).eq.0d0.and.p(3).lt.0d0) then
            sqp0p3 = 0d0
         else
            sqp0p3 = dsqrt(max(p(0)+p(3),rZero))*nsr
         end if
         chi(1) = dcmplx( sqp0p3 )
         if ( sqp0p3.eq.rZero ) then
            chi(2) = dcmplx(-nhel )*dsqrt(rTwo*p(0))
         else
            chi(2) = dcmplx( nh*p(1), -p(2) )/sqp0p3
         endif
         if ( nh.eq.1 ) then
            fop(1) = chi(1)
            fop(2) = chi(2)
            fop(3) = dcmplx( rZero )
            fop(4) = dcmplx( rZero )
         else
            fop(1) = dcmplx( rZero )
            fop(2) = dcmplx( rZero )
            fop(3) = chi(2)
            fop(4) = chi(1)
         endif
       endif
      endif

      if ( nhel.le.1 ) then
c constract spinor+ 
       nh = -nsr

      if ( rmass.ne.rZero ) then

         pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))

         if ( pp.eq.rZero ) then
            
            sqm(0) = dsqrt(abs(rmass)) ! possibility of negative fermion masses
            sqm(1) = sign(sqm(0),rmass) ! possibility of negative fermion masses
            ip = -((1+nh)/2)
            im =  (1-nh)/2
            
            fom(1) = im     * sqm(im)
            fom(2) = ip*nsr * sqm(im)
            fom(3) = im*nsr * sqm(-ip)
            fom(4) = ip     * sqm(-ip)
            
         else
            
            pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))
            sf(1) = dble(1+nsr+(1-nsr)*nh)*rHalf
            sf(2) = dble(1+nsr-(1-nsr)*nh)*rHalf
            omega(1) = dsqrt(p(0)+pp)
            omega(2) = rmass/omega(1)
            ip = (3+nh)/2
            im = (3-nh)/2
            sfomeg(1) = sf(1)*omega(ip)
            sfomeg(2) = sf(2)*omega(im)
            pp3 = max(pp+p(3),rZero)
            chi(1) = dcmplx( dsqrt(pp3*rHalf/pp) )
            if ( pp3.eq.rZero ) then
               chi(2) = dcmplx(-nh )
            else
               chi(2) = dcmplx( nh*p(1) , -p(2) )/dsqrt(rTwo*pp*pp3)
            endif
            
            fom(1) = sfomeg(2)*chi(im)
            fom(2) = sfomeg(2)*chi(ip)
            fom(3) = sfomeg(1)*chi(im)
            fom(4) = sfomeg(1)*chi(ip)

         endif
         
      else
         
         if(p(1).eq.0d0.and.p(2).eq.0d0.and.p(3).lt.0d0) then
            sqp0p3 = 0d0
         else
            sqp0p3 = dsqrt(max(p(0)+p(3),rZero))*nsr
         end if
         chi(1) = dcmplx( sqp0p3 )
         if ( sqp0p3.eq.rZero ) then
            chi(2) = dcmplx(-nhel )*dsqrt(rTwo*p(0))
         else
            chi(2) = dcmplx( nh*p(1), -p(2) )/sqp0p3
         endif
         if ( nh.eq.1 ) then
            fom(1) = chi(1)
            fom(2) = chi(2)
            fom(3) = dcmplx( rZero )
            fom(4) = dcmplx( rZero )
         else
            fom(1) = dcmplx( rZero )
            fom(2) = dcmplx( rZero )
            fom(3) = chi(2)
            fom(4) = chi(1)
         endif
       endif 
      endif
      
c spin-3/2 fermion wavefunction
      if ( nhel.eq.3 ) then
         do j = 1,4
            do i = 1,4
               rc(i,j) = ep(i)*fop(j)
            end do
         end do
      else if ( nhel.eq.1 ) then
         do j = 1,4
            do i = 1,4
              if     ( pt.eq.rZero .and. p(3).ge.0d0 ) then 
               rc(i,j) =  sq2/sq3*e0(i)*fop(j)
     &                    +rOne/sq3*ep(i)*fom(j)
              elseif ( pt.eq.rZero .and. p(3).lt.0d0 ) then 
               rc(i,j) =  sq2/sq3*e0(i)*fop(j)
     &                    -rOne/sq3*ep(i)*fom(j)
              else
               rc(i,j) =  sq2/sq3*e0(i)*fop(j) 
     &                  +rOne/sq3*ep(i)*fom(j)
     &                   *dcmplx(P(1),-nsr*P(2))/pt  
              endif
            end do
         end do
      else if ( nhel.eq.-1 ) then
         do j = 1,4
            do i = 1,4
              if     ( pt.eq.rZero .and.p(3).ge.0d0 ) then 
               rc(i,j) = rOne/sq3*em(i)*fop(j)
     &                   +sq2/sq3*e0(i)*fom(j)
              elseif ( pt.eq.rZero .and.p(3).lt.0d0 ) then 
               rc(i,j) = rOne/sq3*em(i)*fop(j)
     &                   -sq2/sq3*e0(i)*fom(j)
              else
               rc(i,j) = rOne/sq3*em(i)*fop(j) 
     &                  + sq2/sq3*e0(i)*fom(j)
     &                   *dcmplx(P(1),-nsr*P(2))/pt  
              endif
            end do
         end do
      else
         do j = 1,4
            do i = 1,4
              if     ( pt.eq.rZero .and. p(3).ge.0d0 ) then 
               rc(i,j) =  em(i)*fom(j)
              elseif ( pt.eq.rZero .and. p(3).lt.0d0 ) then 
               rc(i,j) = -em(i)*fom(j)
              else
               rc(i,j) =  em(i)*fom(j)*dcmplx(P(1),-nsr*P(2))/pt  
              endif
            end do
         end do
      end if

      ro(3) = rc(1,1)
      ro(4) = rc(1,2)
      ro(5) = rc(1,3)
      ro(6) = rc(1,4)
      ro(7) = rc(2,1)
      ro(8) = rc(2,2)
      ro(9) = rc(2,3)
      ro(10) = rc(2,4)
      ro(11) = rc(3,1)
      ro(12) = rc(3,2)
      ro(13) = rc(3,3)
      ro(14) = rc(3,4)
      ro(15) = rc(4,1)
      ro(16) = rc(4,2)
      ro(17) = rc(4,3)
      ro(18) = rc(4,4)
      ro(1) = rc(5,1)
      ro(2) = rc(6,1)

      return
      end



C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,1)
C     
      SUBROUTINE FFV1_0(F1, F2, V3, COUP,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 TMP15
      COMPLEX*16 V3(*)
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      COMPLEX*16 VERTEX
      COMPLEX*16 COUP
      TMP15 = (F1(3)*(F2(5)*(V3(3)+V3(6))+F2(6)*(V3(4)+CI*(V3(5))))
     $ +(F1(4)*(F2(5)*(V3(4)-CI*(V3(5)))+F2(6)*(V3(3)-V3(6)))
     $ +(F1(5)*(F2(3)*(V3(3)-V3(6))-F2(4)*(V3(4)+CI*(V3(5))))
     $ +F1(6)*(F2(3)*(+CI*(V3(5))-V3(4))+F2(4)*(V3(3)+V3(6))))))
      VERTEX = COUP*-CI * TMP15
      END

C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,1)
C     
      SUBROUTINE FFV1_1(F2, V3, COUP, M1, W1,F1)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(*)
      COMPLEX*16 V3(*)
      REAL*8 P1(0:3)
      REAL*8 M1
      REAL*8 W1
      COMPLEX*16 F1(6)
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F1(1) = +F2(1)+V3(1)
      F1(2) = +F2(2)+V3(2)
      P1(0) = -DBLE(F1(1))
      P1(1) = -DBLE(F1(2))
      P1(2) = -DIMAG(F1(2))
      P1(3) = -DIMAG(F1(1))
      DENOM = COUP/(P1(0)**2-P1(1)**2-P1(2)**2-P1(3)**2 - M1 * (M1 
     $ -CI* W1))
      F1(3)= DENOM*CI*(F2(3)*(P1(0)*(V3(6)-V3(3))+(P1(1)*(V3(4)
     $ -CI*(V3(5)))+(P1(2)*(V3(5)+CI*(V3(4)))+P1(3)*(V3(6)-V3(3)))))
     $ +(F2(4)*(P1(0)*(V3(4)+CI*(V3(5)))+(P1(1)*-1D0*(V3(3)+V3(6))
     $ +(P1(2)*-1D0*(+CI*(V3(3)+V3(6)))+P1(3)*(V3(4)+CI*(V3(5))))))
     $ +M1*(F2(5)*(V3(3)+V3(6))+F2(6)*(V3(4)+CI*(V3(5))))))
      F1(4)= DENOM*-CI*(F2(3)*(P1(0)*(+CI*(V3(5))-V3(4))+(P1(1)*(V3(3)
     $ -V3(6))+(P1(2)*(-CI*(V3(3))+CI*(V3(6)))+P1(3)*(V3(4)-CI
     $ *(V3(5))))))+(F2(4)*(P1(0)*(V3(3)+V3(6))+(P1(1)*-1D0*(V3(4)
     $ +CI*(V3(5)))+(P1(2)*(+CI*(V3(4))-V3(5))-P1(3)*(V3(3)+V3(6)))))
     $ +M1*(F2(5)*(+CI*(V3(5))-V3(4))+F2(6)*(V3(6)-V3(3)))))
      F1(5)= DENOM*-CI*(F2(5)*(P1(0)*(V3(3)+V3(6))+(P1(1)*(+CI*(V3(5))
     $ -V3(4))+(P1(2)*-1D0*(V3(5)+CI*(V3(4)))-P1(3)*(V3(3)+V3(6)))))
     $ +(F2(6)*(P1(0)*(V3(4)+CI*(V3(5)))+(P1(1)*(V3(6)-V3(3))
     $ +(P1(2)*(-CI*(V3(3))+CI*(V3(6)))-P1(3)*(V3(4)+CI*(V3(5))))))
     $ +M1*(F2(3)*(V3(6)-V3(3))+F2(4)*(V3(4)+CI*(V3(5))))))
      F1(6)= DENOM*CI*(F2(5)*(P1(0)*(+CI*(V3(5))-V3(4))+(P1(1)*(V3(3)
     $ +V3(6))+(P1(2)*-1D0*(+CI*(V3(3)+V3(6)))+P1(3)*(+CI*(V3(5))
     $ -V3(4)))))+(F2(6)*(P1(0)*(V3(6)-V3(3))+(P1(1)*(V3(4)+CI
     $ *(V3(5)))+(P1(2)*(V3(5)-CI*(V3(4)))+P1(3)*(V3(6)-V3(3)))))
     $ +M1*(F2(3)*(+CI*(V3(5))-V3(4))+F2(4)*(V3(3)+V3(6)))))
      END



C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,1)
C     
      SUBROUTINE FFV1_2(F1, V3, COUP, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(6)
      COMPLEX*16 V3(*)
      REAL*8 P2(0:3)
      REAL*8 W2
      COMPLEX*16 F1(*)
      REAL*8 M2
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F2(1) = +F1(1)+V3(1)
      F2(2) = +F1(2)+V3(2)
      P2(0) = -DBLE(F2(1))
      P2(1) = -DBLE(F2(2))
      P2(2) = -DIMAG(F2(2))
      P2(3) = -DIMAG(F2(1))
      DENOM = COUP/(P2(0)**2-P2(1)**2-P2(2)**2-P2(3)**2 - M2 * (M2 
     $ -CI* W2))
      F2(3)= DENOM*CI*(F1(3)*(P2(0)*(V3(3)+V3(6))+(P2(1)*-1D0*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(+CI*(V3(4))-V3(5))-P2(3)*(V3(3)+V3(6)))))
     $ +(F1(4)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*(V3(6)-V3(3))
     $ +(P2(2)*(-CI*(V3(6))+CI*(V3(3)))+P2(3)*(+CI*(V3(5))-V3(4)))))
     $ +M2*(F1(5)*(V3(3)-V3(6))+F1(6)*(+CI*(V3(5))-V3(4)))))
      F2(4)= DENOM*-CI*(F1(3)*(P2(0)*-1D0*(V3(4)+CI*(V3(5)))+(P2(1)
     $ *(V3(3)+V3(6))+(P2(2)*(+CI*(V3(3)+V3(6)))-P2(3)*(V3(4)
     $ +CI*(V3(5))))))+(F1(4)*(P2(0)*(V3(6)-V3(3))+(P2(1)*(V3(4)
     $ -CI*(V3(5)))+(P2(2)*(V3(5)+CI*(V3(4)))+P2(3)*(V3(6)-V3(3)))))
     $ +M2*(F1(5)*(V3(4)+CI*(V3(5)))-F1(6)*(V3(3)+V3(6)))))
      F2(5)= DENOM*-CI*(F1(5)*(P2(0)*(V3(6)-V3(3))+(P2(1)*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(V3(5)-CI*(V3(4)))+P2(3)*(V3(6)-V3(3)))))
     $ +(F1(6)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*-1D0*(V3(3)+V3(6))
     $ +(P2(2)*(+CI*(V3(3)+V3(6)))+P2(3)*(V3(4)-CI*(V3(5))))))
     $ +M2*(F1(3)*-1D0*(V3(3)+V3(6))+F1(4)*(+CI*(V3(5))-V3(4)))))
      F2(6)= DENOM*CI*(F1(5)*(P2(0)*-1D0*(V3(4)+CI*(V3(5)))+(P2(1)
     $ *(V3(3)-V3(6))+(P2(2)*(-CI*(V3(6))+CI*(V3(3)))+P2(3)*(V3(4)
     $ +CI*(V3(5))))))+(F1(6)*(P2(0)*(V3(3)+V3(6))+(P2(1)*(+CI*(V3(5))
     $ -V3(4))+(P2(2)*-1D0*(V3(5)+CI*(V3(4)))-P2(3)*(V3(3)+V3(6)))))
     $ +M2*(F1(3)*(V3(4)+CI*(V3(5)))+F1(4)*(V3(3)-V3(6)))))
      END

C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,1)
C     
      SUBROUTINE FFV1P0_3(F1, F2, COUP, M3, W3,V3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(*)
      COMPLEX*16 V3(6)
      REAL*8 W3
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      V3(1) = +F1(1)+F2(1)
      V3(2) = +F1(2)+F2(2)
      P3(0) = -DBLE(V3(1))
      P3(1) = -DBLE(V3(2))
      P3(2) = -DIMAG(V3(2))
      P3(3) = -DIMAG(V3(1))
      DENOM = COUP/(P3(0)**2-P3(1)**2-P3(2)**2-P3(3)**2 - M3 * (M3 
     $ -CI* W3))
      V3(3)= DENOM*-CI*(F2(5)*F1(3)+F2(6)*F1(4)+F2(3)*F1(5)+F2(4)
     $ *F1(6))
      V3(4)= DENOM*-CI*(F2(4)*F1(5)+F2(3)*F1(6)-F2(6)*F1(3)-F2(5)
     $ *F1(4))
      V3(5)= DENOM*-CI*(-CI*(F2(6)*F1(3)+F2(3)*F1(6))+CI*(F2(5)*F1(4)
     $ +F2(4)*F1(5)))
      V3(6)= DENOM*-CI*(F2(6)*F1(4)+F2(3)*F1(5)-F2(5)*F1(3)-F2(4)
     $ *F1(6))
      END

C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_0(F1, F2, V3, COUP,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 V3(*)
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      COMPLEX*16 VERTEX
      COMPLEX*16 COUP
      COMPLEX*16 TMP13
      TMP13 = (F1(3)*(F2(5)*(V3(3)+V3(6))+F2(6)*(V3(4)+CI*(V3(5))))
     $ +F1(4)*(F2(5)*(V3(4)-CI*(V3(5)))+F2(6)*(V3(3)-V3(6))))
      VERTEX = COUP*-CI * TMP13
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_5_0(F1, F2, V3, COUP1, COUP2,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 COUP2
      COMPLEX*16 V3(*)
      COMPLEX*16 F1(*)
      COMPLEX*16 COUP1
      COMPLEX*16 F2(*)
      COMPLEX*16 VERTEX
      COMPLEX*16 TMP
      CALL FFV2_0(F1,F2,V3,COUP1,VERTEX)
      CALL FFV5_0(F1,F2,V3,COUP2,TMP)
      VERTEX = VERTEX + TMP
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_3_0(F1, F2, V3, COUP1, COUP2,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 COUP2
      COMPLEX*16 V3(*)
      COMPLEX*16 F1(*)
      COMPLEX*16 COUP1
      COMPLEX*16 F2(*)
      COMPLEX*16 VERTEX
      COMPLEX*16 TMP
      CALL FFV2_0(F1,F2,V3,COUP1,VERTEX)
      CALL FFV3_0(F1,F2,V3,COUP2,TMP)
      VERTEX = VERTEX + TMP
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_4_0(F1, F2, V3, COUP1, COUP2,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 COUP2
      COMPLEX*16 V3(*)
      COMPLEX*16 F1(*)
      COMPLEX*16 COUP1
      COMPLEX*16 F2(*)
      COMPLEX*16 VERTEX
      COMPLEX*16 TMP
      CALL FFV2_0(F1,F2,V3,COUP1,VERTEX)
      CALL FFV4_0(F1,F2,V3,COUP2,TMP)
      VERTEX = VERTEX + TMP
      END

C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_1(F2, V3, COUP, M1, W1,F1)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(*)
      COMPLEX*16 V3(*)
      REAL*8 P1(0:3)
      REAL*8 M1
      REAL*8 W1
      COMPLEX*16 F1(6)
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F1(1) = +F2(1)+V3(1)
      F1(2) = +F2(2)+V3(2)
      P1(0) = -DBLE(F1(1))
      P1(1) = -DBLE(F1(2))
      P1(2) = -DIMAG(F1(2))
      P1(3) = -DIMAG(F1(1))
      DENOM = COUP/(P1(0)**2-P1(1)**2-P1(2)**2-P1(3)**2 - M1 * (M1 
     $ -CI* W1))
      F1(3)= DENOM*CI * M1*(F2(5)*(V3(3)+V3(6))+F2(6)*(V3(4)+CI
     $ *(V3(5))))
      F1(4)= DENOM*-CI * M1*(F2(5)*(+CI*(V3(5))-V3(4))+F2(6)*(V3(6)
     $ -V3(3)))
      F1(5)= DENOM*-CI*(F2(5)*(P1(0)*(V3(3)+V3(6))+(P1(1)*(+CI*(V3(5))
     $ -V3(4))+(P1(2)*-1D0*(V3(5)+CI*(V3(4)))-P1(3)*(V3(3)+V3(6)))))
     $ +F2(6)*(P1(0)*(V3(4)+CI*(V3(5)))+(P1(1)*(V3(6)-V3(3))+(P1(2)*(
     $ -CI*(V3(3))+CI*(V3(6)))-P1(3)*(V3(4)+CI*(V3(5)))))))
      F1(6)= DENOM*-CI*(F2(5)*(P1(0)*(V3(4)-CI*(V3(5)))+(P1(1)*
     $ -1D0*(V3(3)+V3(6))+(P1(2)*(+CI*(V3(3)+V3(6)))+P1(3)*(V3(4)
     $ -CI*(V3(5))))))+F2(6)*(P1(0)*(V3(3)-V3(6))+(P1(1)*-1D0*(V3(4)
     $ +CI*(V3(5)))+(P1(2)*(+CI*(V3(4))-V3(5))+P1(3)*(V3(3)-V3(6))))))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_3_1(F2, V3, COUP1, COUP2, M1, W1,F1)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(*)
      COMPLEX*16 V3(*)
      REAL*8 P1(0:3)
      REAL*8 M1
      REAL*8 W1
      COMPLEX*16 F1(6)
      COMPLEX*16 COUP1
      COMPLEX*16 DENOM
      COMPLEX*16 COUP2
      INTEGER*4 I
      COMPLEX*16 FTMP(6)
      CALL FFV2_1(F2,V3,COUP1,M1,W1,F1)
      CALL FFV3_1(F2,V3,COUP2,M1,W1,FTMP)
      DO I = 3, 6
        F1(I) = F1(I) + FTMP(I)
      ENDDO
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_4_1(F2, V3, COUP1, COUP2, M1, W1,F1)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(*)
      COMPLEX*16 V3(*)
      REAL*8 P1(0:3)
      REAL*8 M1
      REAL*8 W1
      COMPLEX*16 F1(6)
      COMPLEX*16 COUP1
      COMPLEX*16 DENOM
      COMPLEX*16 COUP2
      INTEGER*4 I
      COMPLEX*16 FTMP(6)
      CALL FFV2_1(F2,V3,COUP1,M1,W1,F1)
      CALL FFV4_1(F2,V3,COUP2,M1,W1,FTMP)
      DO I = 3, 6
        F1(I) = F1(I) + FTMP(I)
      ENDDO
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_2(F1, V3, COUP, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(6)
      COMPLEX*16 V3(*)
      REAL*8 P2(0:3)
      REAL*8 W2
      COMPLEX*16 F1(*)
      REAL*8 M2
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F2(1) = +F1(1)+V3(1)
      F2(2) = +F1(2)+V3(2)
      P2(0) = -DBLE(F2(1))
      P2(1) = -DBLE(F2(2))
      P2(2) = -DIMAG(F2(2))
      P2(3) = -DIMAG(F2(1))
      DENOM = COUP/(P2(0)**2-P2(1)**2-P2(2)**2-P2(3)**2 - M2 * (M2 
     $ -CI* W2))
      F2(3)= DENOM*CI*(F1(3)*(P2(0)*(V3(3)+V3(6))+(P2(1)*-1D0*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(+CI*(V3(4))-V3(5))-P2(3)*(V3(3)+V3(6)))))
     $ +F1(4)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*(V3(6)-V3(3))+(P2(2)*(
     $ -CI*(V3(6))+CI*(V3(3)))+P2(3)*(+CI*(V3(5))-V3(4))))))
      F2(4)= DENOM*CI*(F1(3)*(P2(0)*(V3(4)+CI*(V3(5)))+(P2(1)*
     $ -1D0*(V3(3)+V3(6))+(P2(2)*-1D0*(+CI*(V3(3)+V3(6)))+P2(3)*(V3(4)
     $ +CI*(V3(5))))))+F1(4)*(P2(0)*(V3(3)-V3(6))+(P2(1)*(+CI*(V3(5))
     $ -V3(4))+(P2(2)*-1D0*(V3(5)+CI*(V3(4)))+P2(3)*(V3(3)-V3(6))))))
      F2(5)= DENOM*-CI * M2*(F1(3)*-1D0*(V3(3)+V3(6))+F1(4)*(
     $ +CI*(V3(5))-V3(4)))
      F2(6)= DENOM*CI * M2*(F1(3)*(V3(4)+CI*(V3(5)))+F1(4)*(V3(3)
     $ -V3(6)))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_5_2(F1, V3, COUP1, COUP2, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(6)
      COMPLEX*16 V3(*)
      COMPLEX*16 FTMP(6)
      COMPLEX*16 COUP2
      REAL*8 P2(0:3)
      REAL*8 W2
      COMPLEX*16 F1(*)
      REAL*8 M2
      COMPLEX*16 DENOM
      COMPLEX*16 COUP1
      INTEGER*4 I
      CALL FFV2_2(F1,V3,COUP1,M2,W2,F2)
      CALL FFV5_2(F1,V3,COUP2,M2,W2,FTMP)
      DO I = 3, 6
        F2(I) = F2(I) + FTMP(I)
      ENDDO
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_4_2(F1, V3, COUP1, COUP2, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(6)
      COMPLEX*16 V3(*)
      COMPLEX*16 FTMP(6)
      COMPLEX*16 COUP2
      REAL*8 P2(0:3)
      REAL*8 W2
      COMPLEX*16 F1(*)
      REAL*8 M2
      COMPLEX*16 DENOM
      COMPLEX*16 COUP1
      INTEGER*4 I
      CALL FFV2_2(F1,V3,COUP1,M2,W2,F2)
      CALL FFV4_2(F1,V3,COUP2,M2,W2,FTMP)
      DO I = 3, 6
        F2(I) = F2(I) + FTMP(I)
      ENDDO
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_3_2(F1, V3, COUP1, COUP2, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(6)
      COMPLEX*16 V3(*)
      COMPLEX*16 FTMP(6)
      COMPLEX*16 COUP2
      REAL*8 P2(0:3)
      REAL*8 W2
      COMPLEX*16 F1(*)
      REAL*8 M2
      COMPLEX*16 DENOM
      COMPLEX*16 COUP1
      INTEGER*4 I
      CALL FFV2_2(F1,V3,COUP1,M2,W2,F2)
      CALL FFV3_2(F1,V3,COUP2,M2,W2,FTMP)
      DO I = 3, 6
        F2(I) = F2(I) + FTMP(I)
      ENDDO
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_3(F1, F2, COUP, M3, W3,V3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 DENOM
      COMPLEX*16 V3(6)
      REAL*8 W3
      COMPLEX*16 TMP0
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      REAL*8 OM3
      COMPLEX*16 COUP
      OM3 = 0D0
      IF (M3.NE.0D0) OM3=1D0/M3**2
      V3(1) = +F1(1)+F2(1)
      V3(2) = +F1(2)+F2(2)
      P3(0) = -DBLE(V3(1))
      P3(1) = -DBLE(V3(2))
      P3(2) = -DIMAG(V3(2))
      P3(3) = -DIMAG(V3(1))
      TMP0 = (F1(3)*(F2(5)*(P3(0)+P3(3))+F2(6)*(P3(1)+CI*(P3(2))))
     $ +F1(4)*(F2(5)*(P3(1)-CI*(P3(2)))+F2(6)*(P3(0)-P3(3))))
      DENOM = COUP/(P3(0)**2-P3(1)**2-P3(2)**2-P3(3)**2 - M3 * (M3 
     $ -CI* W3))
      V3(3)= DENOM*-CI*(F2(5)*F1(3)+F2(6)*F1(4)-P3(0)*OM3*TMP0)
      V3(4)= DENOM*-CI*(-F2(6)*F1(3)-F2(5)*F1(4)-P3(1)*OM3*TMP0)
      V3(5)= DENOM*-CI*(-CI*(F2(6)*F1(3))+CI*(F2(5)*F1(4))-P3(2)*OM3
     $ *TMP0)
      V3(6)= DENOM*-CI*(F2(6)*F1(4)-F2(5)*F1(3)-P3(3)*OM3*TMP0)
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_3_3(F1, F2, COUP1, COUP2, M3, W3,V3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 DENOM
      COMPLEX*16 V3(6)
      REAL*8 W3
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 COUP1
      COMPLEX*16 F2(*)
      COMPLEX*16 COUP2
      REAL*8 OM3
      INTEGER*4 I
      COMPLEX*16 VTMP(6)
      CALL FFV2_3(F1,F2,COUP1,M3,W3,V3)
      CALL FFV3_3(F1,F2,COUP2,M3,W3,VTMP)
      DO I = 3, 6
        V3(I) = V3(I) + VTMP(I)
      ENDDO
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_5_3(F1, F2, COUP1, COUP2, M3, W3,V3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 DENOM
      COMPLEX*16 V3(6)
      REAL*8 W3
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 COUP1
      COMPLEX*16 F2(*)
      COMPLEX*16 COUP2
      REAL*8 OM3
      INTEGER*4 I
      COMPLEX*16 VTMP(6)
      CALL FFV2_3(F1,F2,COUP1,M3,W3,V3)
      CALL FFV5_3(F1,F2,COUP2,M3,W3,VTMP)
      DO I = 3, 6
        V3(I) = V3(I) + VTMP(I)
      ENDDO
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE FFV2_4_3(F1, F2, COUP1, COUP2, M3, W3,V3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 DENOM
      COMPLEX*16 V3(6)
      REAL*8 W3
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 COUP1
      COMPLEX*16 F2(*)
      COMPLEX*16 COUP2
      REAL*8 OM3
      INTEGER*4 I
      COMPLEX*16 VTMP(6)
      CALL FFV2_3(F1,F2,COUP1,M3,W3,V3)
      CALL FFV4_3(F1,F2,COUP2,M3,W3,VTMP)
      DO I = 3, 6
        V3(I) = V3(I) + VTMP(I)
      ENDDO
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) - 2*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV3_0(F1, F2, V3, COUP,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 V3(*)
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      COMPLEX*16 TMP14
      COMPLEX*16 VERTEX
      COMPLEX*16 COUP
      COMPLEX*16 TMP13
      TMP14 = (F1(5)*(F2(3)*(V3(3)-V3(6))-F2(4)*(V3(4)+CI*(V3(5))))
     $ +F1(6)*(F2(3)*(+CI*(V3(5))-V3(4))+F2(4)*(V3(3)+V3(6))))
      TMP13 = (F1(3)*(F2(5)*(V3(3)+V3(6))+F2(6)*(V3(4)+CI*(V3(5))))
     $ +F1(4)*(F2(5)*(V3(4)-CI*(V3(5)))+F2(6)*(V3(3)-V3(6))))
      VERTEX = COUP*(-CI*(TMP13)+2D0 * CI*(TMP14))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) - 2*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV3_1(F2, V3, COUP, M1, W1,F1)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(*)
      COMPLEX*16 V3(*)
      REAL*8 P1(0:3)
      REAL*8 M1
      REAL*8 W1
      COMPLEX*16 F1(6)
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F1(1) = +F2(1)+V3(1)
      F1(2) = +F2(2)+V3(2)
      P1(0) = -DBLE(F1(1))
      P1(1) = -DBLE(F1(2))
      P1(2) = -DIMAG(F1(2))
      P1(3) = -DIMAG(F1(1))
      DENOM = COUP/(P1(0)**2-P1(1)**2-P1(2)**2-P1(3)**2 - M1 * (M1 
     $ -CI* W1))
      F1(3)= DENOM*-2D0 * CI*(F2(3)*(P1(0)*(V3(6)-V3(3))+(P1(1)*(V3(4)
     $ -CI*(V3(5)))+(P1(2)*(V3(5)+CI*(V3(4)))+P1(3)*(V3(6)-V3(3)))))+(
     $ +1D0/2D0*(M1*(+2D0*(F2(5)*-1D0/2D0*(V3(3)+V3(6)))-F2(6)*(V3(4)
     $ +CI*(V3(5)))))+F2(4)*(P1(0)*(V3(4)+CI*(V3(5)))+(P1(1)*
     $ -1D0*(V3(3)+V3(6))+(P1(2)*-1D0*(+CI*(V3(3)+V3(6)))+P1(3)*(V3(4)
     $ +CI*(V3(5))))))))
      F1(4)= DENOM*-2D0 * CI*(F2(3)*(P1(0)*(V3(4)-CI*(V3(5)))
     $ +(P1(1)*(V3(6)-V3(3))+(P1(2)*(-CI*(V3(6))+CI*(V3(3)))+P1(3)*(
     $ +CI*(V3(5))-V3(4)))))+(+1D0/2D0*(M1*(F2(6)*(V3(6)-V3(3))
     $ +2D0*(F2(5)*1D0/2D0*(+CI*(V3(5))-V3(4)))))+F2(4)*(P1(0)*
     $ -1D0*(V3(3)+V3(6))+(P1(1)*(V3(4)+CI*(V3(5)))+(P1(2)*(V3(5)
     $ -CI*(V3(4)))+P1(3)*(V3(3)+V3(6)))))))
      F1(5)= DENOM*CI*(F2(5)*(P1(0)*-1D0*(V3(3)+V3(6))+(P1(1)*(V3(4)
     $ -CI*(V3(5)))+(P1(2)*(V3(5)+CI*(V3(4)))+P1(3)*(V3(3)+V3(6)))))
     $ +(F2(6)*(P1(0)*-1D0*(V3(4)+CI*(V3(5)))+(P1(1)*(V3(3)-V3(6))
     $ +(P1(2)*(-CI*(V3(6))+CI*(V3(3)))+P1(3)*(V3(4)+CI*(V3(5))))))
     $ +M1*(F2(3)*2D0*(V3(6)-V3(3))+2D0*(F2(4)*(V3(4)+CI*(V3(5)))))))
      F1(6)= DENOM*-CI*(F2(5)*(P1(0)*(V3(4)-CI*(V3(5)))+(P1(1)*
     $ -1D0*(V3(3)+V3(6))+(P1(2)*(+CI*(V3(3)+V3(6)))+P1(3)*(V3(4)
     $ -CI*(V3(5))))))+(F2(6)*(P1(0)*(V3(3)-V3(6))+(P1(1)*-1D0*(V3(4)
     $ +CI*(V3(5)))+(P1(2)*(+CI*(V3(4))-V3(5))+P1(3)*(V3(3)-V3(6)))))
     $ +M1*(F2(3)*2D0*(+CI*(V3(5))-V3(4))+2D0*(F2(4)*(V3(3)+V3(6))))))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) - 2*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV3_2(F1, V3, COUP, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(6)
      COMPLEX*16 V3(*)
      REAL*8 P2(0:3)
      REAL*8 W2
      COMPLEX*16 F1(*)
      REAL*8 M2
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F2(1) = +F1(1)+V3(1)
      F2(2) = +F1(2)+V3(2)
      P2(0) = -DBLE(F2(1))
      P2(1) = -DBLE(F2(2))
      P2(2) = -DIMAG(F2(2))
      P2(3) = -DIMAG(F2(1))
      DENOM = COUP/(P2(0)**2-P2(1)**2-P2(2)**2-P2(3)**2 - M2 * (M2 
     $ -CI* W2))
      F2(3)= DENOM*CI*(F1(3)*(P2(0)*(V3(3)+V3(6))+(P2(1)*-1D0*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(+CI*(V3(4))-V3(5))-P2(3)*(V3(3)+V3(6)))))
     $ +(F1(4)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*(V3(6)-V3(3))
     $ +(P2(2)*(-CI*(V3(6))+CI*(V3(3)))+P2(3)*(+CI*(V3(5))-V3(4)))))
     $ +M2*(F1(5)*2D0*(V3(6)-V3(3))+2D0*(F1(6)*(V3(4)-CI*(V3(5)))))))
      F2(4)= DENOM*CI*(F1(3)*(P2(0)*(V3(4)+CI*(V3(5)))+(P2(1)*
     $ -1D0*(V3(3)+V3(6))+(P2(2)*-1D0*(+CI*(V3(3)+V3(6)))+P2(3)*(V3(4)
     $ +CI*(V3(5))))))+(F1(4)*(P2(0)*(V3(3)-V3(6))+(P2(1)*(+CI*(V3(5))
     $ -V3(4))+(P2(2)*-1D0*(V3(5)+CI*(V3(4)))+P2(3)*(V3(3)-V3(6)))))
     $ +M2*(F1(5)*2D0*(V3(4)+CI*(V3(5)))-2D0*(F1(6)*(V3(3)+V3(6))))))
      F2(5)= DENOM*2D0 * CI*(F1(5)*(P2(0)*(V3(6)-V3(3))+(P2(1)*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(V3(5)-CI*(V3(4)))+P2(3)*(V3(6)-V3(3)))))+(
     $ +1D0/2D0*(M2*(F1(4)*(V3(4)-CI*(V3(5)))+2D0*(F1(3)*1D0/2D0
     $ *(V3(3)+V3(6)))))+F1(6)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*
     $ -1D0*(V3(3)+V3(6))+(P2(2)*(+CI*(V3(3)+V3(6)))+P2(3)*(V3(4)
     $ -CI*(V3(5))))))))
      F2(6)= DENOM*2D0 * CI*(F1(5)*(P2(0)*(V3(4)+CI*(V3(5)))+(P2(1)
     $ *(V3(6)-V3(3))+(P2(2)*(-CI*(V3(3))+CI*(V3(6)))-P2(3)*(V3(4)
     $ +CI*(V3(5))))))+(+1D0/2D0*(M2*(F1(4)*(V3(3)-V3(6))+2D0*(F1(3)
     $ *1D0/2D0*(V3(4)+CI*(V3(5))))))+F1(6)*(P2(0)*-1D0*(V3(3)+V3(6))
     $ +(P2(1)*(V3(4)-CI*(V3(5)))+(P2(2)*(V3(5)+CI*(V3(4)))+P2(3)
     $ *(V3(3)+V3(6)))))))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) - 2*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV3_3(F1, F2, COUP, M3, W3,V3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 DENOM
      COMPLEX*16 V3(6)
      COMPLEX*16 TMP1
      REAL*8 W3
      COMPLEX*16 TMP0
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      REAL*8 OM3
      COMPLEX*16 COUP
      OM3 = 0D0
      IF (M3.NE.0D0) OM3=1D0/M3**2
      V3(1) = +F1(1)+F2(1)
      V3(2) = +F1(2)+F2(2)
      P3(0) = -DBLE(V3(1))
      P3(1) = -DBLE(V3(2))
      P3(2) = -DIMAG(V3(2))
      P3(3) = -DIMAG(V3(1))
      TMP1 = (F1(5)*(F2(3)*(P3(0)-P3(3))-F2(4)*(P3(1)+CI*(P3(2))))
     $ +F1(6)*(F2(3)*(+CI*(P3(2))-P3(1))+F2(4)*(P3(0)+P3(3))))
      TMP0 = (F1(3)*(F2(5)*(P3(0)+P3(3))+F2(6)*(P3(1)+CI*(P3(2))))
     $ +F1(4)*(F2(5)*(P3(1)-CI*(P3(2)))+F2(6)*(P3(0)-P3(3))))
      DENOM = COUP/(P3(0)**2-P3(1)**2-P3(2)**2-P3(3)**2 - M3 * (M3 
     $ -CI* W3))
      V3(3)= DENOM*2D0 * CI*(OM3*1D0/2D0 * P3(0)*(TMP0-2D0*(TMP1))
     $ +(-1D0/2D0*(F2(5)*F1(3)+F2(6)*F1(4))+F2(3)*F1(5)+F2(4)*F1(6)))
      V3(4)= DENOM*2D0 * CI*(OM3*1D0/2D0 * P3(1)*(TMP0-2D0*(TMP1))+(
     $ +1D0/2D0*(F2(6)*F1(3)+F2(5)*F1(4))+F2(4)*F1(5)+F2(3)*F1(6)))
      V3(5)= DENOM*-2D0 * CI*(OM3*1D0/2D0 * P3(2)*(+2D0*(TMP1)-TMP0)
     $ +(-1D0/2D0 * CI*(F2(6)*F1(3))+1D0/2D0 * CI*(F2(5)*F1(4))
     $ -CI*(F2(4)*F1(5))+CI*(F2(3)*F1(6))))
      V3(6)= DENOM*-2D0 * CI*(OM3*1D0/2D0 * P3(3)*(+2D0*(TMP1)-TMP0)
     $ +(-1D0/2D0*(F2(5)*F1(3))+1D0/2D0*(F2(6)*F1(4))-F2(3)*F1(5)
     $ +F2(4)*F1(6)))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) + 2*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV4_0(F1, F2, V3, COUP,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 V3(*)
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      COMPLEX*16 TMP14
      COMPLEX*16 VERTEX
      COMPLEX*16 COUP
      COMPLEX*16 TMP13
      TMP14 = (F1(5)*(F2(3)*(V3(3)-V3(6))-F2(4)*(V3(4)+CI*(V3(5))))
     $ +F1(6)*(F2(3)*(+CI*(V3(5))-V3(4))+F2(4)*(V3(3)+V3(6))))
      TMP13 = (F1(3)*(F2(5)*(V3(3)+V3(6))+F2(6)*(V3(4)+CI*(V3(5))))
     $ +F1(4)*(F2(5)*(V3(4)-CI*(V3(5)))+F2(6)*(V3(3)-V3(6))))
      VERTEX = COUP*-1D0*(+CI*(TMP13)+2D0 * CI*(TMP14))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) + 2*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV4_1(F2, V3, COUP, M1, W1,F1)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(*)
      COMPLEX*16 V3(*)
      REAL*8 P1(0:3)
      REAL*8 M1
      REAL*8 W1
      COMPLEX*16 F1(6)
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F1(1) = +F2(1)+V3(1)
      F1(2) = +F2(2)+V3(2)
      P1(0) = -DBLE(F1(1))
      P1(1) = -DBLE(F1(2))
      P1(2) = -DIMAG(F1(2))
      P1(3) = -DIMAG(F1(1))
      DENOM = COUP/(P1(0)**2-P1(1)**2-P1(2)**2-P1(3)**2 - M1 * (M1 
     $ -CI* W1))
      F1(3)= DENOM*2D0 * CI*(F2(3)*(P1(0)*(V3(6)-V3(3))+(P1(1)*(V3(4)
     $ -CI*(V3(5)))+(P1(2)*(V3(5)+CI*(V3(4)))+P1(3)*(V3(6)-V3(3)))))+(
     $ +1D0/2D0*(M1*(F2(6)*(V3(4)+CI*(V3(5)))+2D0*(F2(5)*1D0/2D0
     $ *(V3(3)+V3(6)))))+F2(4)*(P1(0)*(V3(4)+CI*(V3(5)))+(P1(1)*
     $ -1D0*(V3(3)+V3(6))+(P1(2)*-1D0*(+CI*(V3(3)+V3(6)))+P1(3)*(V3(4)
     $ +CI*(V3(5))))))))
      F1(4)= DENOM*2D0 * CI*(F2(3)*(P1(0)*(V3(4)-CI*(V3(5)))+(P1(1)
     $ *(V3(6)-V3(3))+(P1(2)*(-CI*(V3(6))+CI*(V3(3)))+P1(3)*(
     $ +CI*(V3(5))-V3(4)))))+(+1D0/2D0*(M1*(F2(6)*(V3(3)-V3(6))
     $ +2D0*(F2(5)*1D0/2D0*(V3(4)-CI*(V3(5))))))+F2(4)*(P1(0)*
     $ -1D0*(V3(3)+V3(6))+(P1(1)*(V3(4)+CI*(V3(5)))+(P1(2)*(V3(5)
     $ -CI*(V3(4)))+P1(3)*(V3(3)+V3(6)))))))
      F1(5)= DENOM*-CI*(F2(5)*(P1(0)*(V3(3)+V3(6))+(P1(1)*(+CI*(V3(5))
     $ -V3(4))+(P1(2)*-1D0*(V3(5)+CI*(V3(4)))-P1(3)*(V3(3)+V3(6)))))
     $ +(F2(6)*(P1(0)*(V3(4)+CI*(V3(5)))+(P1(1)*(V3(6)-V3(3))
     $ +(P1(2)*(-CI*(V3(3))+CI*(V3(6)))-P1(3)*(V3(4)+CI*(V3(5))))))
     $ +M1*(F2(3)*2D0*(V3(6)-V3(3))+2D0*(F2(4)*(V3(4)+CI*(V3(5)))))))
      F1(6)= DENOM*CI*(F2(5)*(P1(0)*(+CI*(V3(5))-V3(4))+(P1(1)*(V3(3)
     $ +V3(6))+(P1(2)*-1D0*(+CI*(V3(3)+V3(6)))+P1(3)*(+CI*(V3(5))
     $ -V3(4)))))+(F2(6)*(P1(0)*(V3(6)-V3(3))+(P1(1)*(V3(4)+CI
     $ *(V3(5)))+(P1(2)*(V3(5)-CI*(V3(4)))+P1(3)*(V3(6)-V3(3)))))
     $ +M1*(F2(3)*2D0*(+CI*(V3(5))-V3(4))+2D0*(F2(4)*(V3(3)+V3(6))))))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) + 2*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV4_2(F1, V3, COUP, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(6)
      COMPLEX*16 V3(*)
      REAL*8 P2(0:3)
      REAL*8 W2
      COMPLEX*16 F1(*)
      REAL*8 M2
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F2(1) = +F1(1)+V3(1)
      F2(2) = +F1(2)+V3(2)
      P2(0) = -DBLE(F2(1))
      P2(1) = -DBLE(F2(2))
      P2(2) = -DIMAG(F2(2))
      P2(3) = -DIMAG(F2(1))
      DENOM = COUP/(P2(0)**2-P2(1)**2-P2(2)**2-P2(3)**2 - M2 * (M2 
     $ -CI* W2))
      F2(3)= DENOM*CI*(F1(3)*(P2(0)*(V3(3)+V3(6))+(P2(1)*-1D0*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(+CI*(V3(4))-V3(5))-P2(3)*(V3(3)+V3(6)))))
     $ +(F1(4)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*(V3(6)-V3(3))
     $ +(P2(2)*(-CI*(V3(6))+CI*(V3(3)))+P2(3)*(+CI*(V3(5))-V3(4)))))
     $ +M2*(F1(5)*2D0*(V3(3)-V3(6))+2D0*(F1(6)*(+CI*(V3(5))-V3(4))))))
      F2(4)= DENOM*CI*(F1(3)*(P2(0)*(V3(4)+CI*(V3(5)))+(P2(1)*
     $ -1D0*(V3(3)+V3(6))+(P2(2)*-1D0*(+CI*(V3(3)+V3(6)))+P2(3)*(V3(4)
     $ +CI*(V3(5))))))+(F1(4)*(P2(0)*(V3(3)-V3(6))+(P2(1)*(+CI*(V3(5))
     $ -V3(4))+(P2(2)*-1D0*(V3(5)+CI*(V3(4)))+P2(3)*(V3(3)-V3(6)))))
     $ +M2*(F1(5)*-2D0*(V3(4)+CI*(V3(5)))+2D0*(F1(6)*(V3(3)+V3(6))))))
      F2(5)= DENOM*-2D0 * CI*(F1(5)*(P2(0)*(V3(6)-V3(3))+(P2(1)*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(V3(5)-CI*(V3(4)))+P2(3)*(V3(6)-V3(3)))))+(
     $ +1D0/2D0*(M2*(F1(4)*(+CI*(V3(5))-V3(4))+2D0*(F1(3)*-1D0/2D0
     $ *(V3(3)+V3(6)))))+F1(6)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*
     $ -1D0*(V3(3)+V3(6))+(P2(2)*(+CI*(V3(3)+V3(6)))+P2(3)*(V3(4)
     $ -CI*(V3(5))))))))
      F2(6)= DENOM*-2D0 * CI*(F1(5)*(P2(0)*(V3(4)+CI*(V3(5)))
     $ +(P2(1)*(V3(6)-V3(3))+(P2(2)*(-CI*(V3(3))+CI*(V3(6)))-P2(3)
     $ *(V3(4)+CI*(V3(5))))))+(+1D0/2D0*(M2*(F1(4)*(V3(6)-V3(3))
     $ +2D0*(F1(3)*-1D0/2D0*(V3(4)+CI*(V3(5))))))+F1(6)*(P2(0)*
     $ -1D0*(V3(3)+V3(6))+(P2(1)*(V3(4)-CI*(V3(5)))+(P2(2)*(V3(5)
     $ +CI*(V3(4)))+P2(3)*(V3(3)+V3(6)))))))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) + 2*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV4_3(F1, F2, COUP, M3, W3,V3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 DENOM
      COMPLEX*16 V3(6)
      COMPLEX*16 TMP1
      REAL*8 W3
      COMPLEX*16 TMP0
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      REAL*8 OM3
      COMPLEX*16 COUP
      OM3 = 0D0
      IF (M3.NE.0D0) OM3=1D0/M3**2
      V3(1) = +F1(1)+F2(1)
      V3(2) = +F1(2)+F2(2)
      P3(0) = -DBLE(V3(1))
      P3(1) = -DBLE(V3(2))
      P3(2) = -DIMAG(V3(2))
      P3(3) = -DIMAG(V3(1))
      TMP1 = (F1(5)*(F2(3)*(P3(0)-P3(3))-F2(4)*(P3(1)+CI*(P3(2))))
     $ +F1(6)*(F2(3)*(+CI*(P3(2))-P3(1))+F2(4)*(P3(0)+P3(3))))
      TMP0 = (F1(3)*(F2(5)*(P3(0)+P3(3))+F2(6)*(P3(1)+CI*(P3(2))))
     $ +F1(4)*(F2(5)*(P3(1)-CI*(P3(2)))+F2(6)*(P3(0)-P3(3))))
      DENOM = COUP/(P3(0)**2-P3(1)**2-P3(2)**2-P3(3)**2 - M3 * (M3 
     $ -CI* W3))
      V3(3)= DENOM*-2D0 * CI*(OM3*-1D0/2D0 * P3(0)*(TMP0+2D0*(TMP1))+(
     $ +1D0/2D0*(F2(5)*F1(3)+F2(6)*F1(4))+F2(3)*F1(5)+F2(4)*F1(6)))
      V3(4)= DENOM*-2D0 * CI*(OM3*-1D0/2D0 * P3(1)*(TMP0+2D0*(TMP1))
     $ +(-1D0/2D0*(F2(6)*F1(3)+F2(5)*F1(4))+F2(4)*F1(5)+F2(3)*F1(6)))
      V3(5)= DENOM*2D0 * CI*(OM3*1D0/2D0 * P3(2)*(TMP0+2D0*(TMP1))+(
     $ +1D0/2D0 * CI*(F2(6)*F1(3))-1D0/2D0 * CI*(F2(5)*F1(4))
     $ -CI*(F2(4)*F1(5))+CI*(F2(3)*F1(6))))
      V3(6)= DENOM*2D0 * CI*(OM3*1D0/2D0 * P3(3)*(TMP0+2D0*(TMP1))+(
     $ +1D0/2D0*(F2(5)*F1(3))-1D0/2D0*(F2(6)*F1(4))-F2(3)*F1(5)
     $ +F2(4)*F1(6)))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) + 4*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV5_0(F1, F2, V3, COUP,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 V3(*)
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      COMPLEX*16 TMP14
      COMPLEX*16 VERTEX
      COMPLEX*16 COUP
      COMPLEX*16 TMP13
      TMP14 = (F1(5)*(F2(3)*(V3(3)-V3(6))-F2(4)*(V3(4)+CI*(V3(5))))
     $ +F1(6)*(F2(3)*(+CI*(V3(5))-V3(4))+F2(4)*(V3(3)+V3(6))))
      TMP13 = (F1(3)*(F2(5)*(V3(3)+V3(6))+F2(6)*(V3(4)+CI*(V3(5))))
     $ +F1(4)*(F2(5)*(V3(4)-CI*(V3(5)))+F2(6)*(V3(3)-V3(6))))
      VERTEX = COUP*-1D0*(+CI*(TMP13)+4D0 * CI*(TMP14))
      END

C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) + 4*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV5_2(F1, V3, COUP, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 F2(6)
      COMPLEX*16 V3(*)
      REAL*8 P2(0:3)
      REAL*8 W2
      COMPLEX*16 F1(*)
      REAL*8 M2
      COMPLEX*16 DENOM
      COMPLEX*16 COUP
      F2(1) = +F1(1)+V3(1)
      F2(2) = +F1(2)+V3(2)
      P2(0) = -DBLE(F2(1))
      P2(1) = -DBLE(F2(2))
      P2(2) = -DIMAG(F2(2))
      P2(3) = -DIMAG(F2(1))
      DENOM = COUP/(P2(0)**2-P2(1)**2-P2(2)**2-P2(3)**2 - M2 * (M2 
     $ -CI* W2))
      F2(3)= DENOM*CI*(F1(3)*(P2(0)*(V3(3)+V3(6))+(P2(1)*-1D0*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(+CI*(V3(4))-V3(5))-P2(3)*(V3(3)+V3(6)))))
     $ +(F1(4)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*(V3(6)-V3(3))
     $ +(P2(2)*(-CI*(V3(6))+CI*(V3(3)))+P2(3)*(+CI*(V3(5))-V3(4)))))
     $ +M2*(F1(5)*4D0*(V3(3)-V3(6))+4D0*(F1(6)*(+CI*(V3(5))-V3(4))))))
      F2(4)= DENOM*CI*(F1(3)*(P2(0)*(V3(4)+CI*(V3(5)))+(P2(1)*
     $ -1D0*(V3(3)+V3(6))+(P2(2)*-1D0*(+CI*(V3(3)+V3(6)))+P2(3)*(V3(4)
     $ +CI*(V3(5))))))+(F1(4)*(P2(0)*(V3(3)-V3(6))+(P2(1)*(+CI*(V3(5))
     $ -V3(4))+(P2(2)*-1D0*(V3(5)+CI*(V3(4)))+P2(3)*(V3(3)-V3(6)))))
     $ +M2*(F1(5)*-4D0*(V3(4)+CI*(V3(5)))+4D0*(F1(6)*(V3(3)+V3(6))))))
      F2(5)= DENOM*-4D0 * CI*(F1(5)*(P2(0)*(V3(6)-V3(3))+(P2(1)*(V3(4)
     $ +CI*(V3(5)))+(P2(2)*(V3(5)-CI*(V3(4)))+P2(3)*(V3(6)-V3(3)))))+(
     $ +1D0/4D0*(M2*(F1(4)*(+CI*(V3(5))-V3(4))+4D0*(F1(3)*-1D0/4D0
     $ *(V3(3)+V3(6)))))+F1(6)*(P2(0)*(V3(4)-CI*(V3(5)))+(P2(1)*
     $ -1D0*(V3(3)+V3(6))+(P2(2)*(+CI*(V3(3)+V3(6)))+P2(3)*(V3(4)
     $ -CI*(V3(5))))))))
      F2(6)= DENOM*-4D0 * CI*(F1(5)*(P2(0)*(V3(4)+CI*(V3(5)))
     $ +(P2(1)*(V3(6)-V3(3))+(P2(2)*(-CI*(V3(3))+CI*(V3(6)))-P2(3)
     $ *(V3(4)+CI*(V3(5))))))+(+1D0/4D0*(M2*(F1(4)*(V3(6)-V3(3))
     $ +4D0*(F1(3)*-1D0/4D0*(V3(4)+CI*(V3(5))))))+F1(6)*(P2(0)*
     $ -1D0*(V3(3)+V3(6))+(P2(1)*(V3(4)-CI*(V3(5)))+(P2(2)*(V3(5)
     $ +CI*(V3(4)))+P2(3)*(V3(3)+V3(6)))))))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1) + 4*Gamma(3,2,-1)*ProjP(-1,1)
C     
      SUBROUTINE FFV5_3(F1, F2, COUP, M3, W3,V3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 DENOM
      COMPLEX*16 V3(6)
      COMPLEX*16 TMP1
      REAL*8 W3
      COMPLEX*16 TMP0
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      REAL*8 OM3
      COMPLEX*16 COUP
      OM3 = 0D0
      IF (M3.NE.0D0) OM3=1D0/M3**2
      V3(1) = +F1(1)+F2(1)
      V3(2) = +F1(2)+F2(2)
      P3(0) = -DBLE(V3(1))
      P3(1) = -DBLE(V3(2))
      P3(2) = -DIMAG(V3(2))
      P3(3) = -DIMAG(V3(1))
      TMP1 = (F1(5)*(F2(3)*(P3(0)-P3(3))-F2(4)*(P3(1)+CI*(P3(2))))
     $ +F1(6)*(F2(3)*(+CI*(P3(2))-P3(1))+F2(4)*(P3(0)+P3(3))))
      TMP0 = (F1(3)*(F2(5)*(P3(0)+P3(3))+F2(6)*(P3(1)+CI*(P3(2))))
     $ +F1(4)*(F2(5)*(P3(1)-CI*(P3(2)))+F2(6)*(P3(0)-P3(3))))
      DENOM = COUP/(P3(0)**2-P3(1)**2-P3(2)**2-P3(3)**2 - M3 * (M3 
     $ -CI* W3))
      V3(3)= DENOM*-4D0 * CI*(OM3*-1D0/4D0 * P3(0)*(TMP0+4D0*(TMP1))+(
     $ +1D0/4D0*(F2(5)*F1(3)+F2(6)*F1(4))+F2(3)*F1(5)+F2(4)*F1(6)))
      V3(4)= DENOM*-4D0 * CI*(OM3*-1D0/4D0 * P3(1)*(TMP0+4D0*(TMP1))
     $ +(-1D0/4D0*(F2(6)*F1(3)+F2(5)*F1(4))+F2(4)*F1(5)+F2(3)*F1(6)))
      V3(5)= DENOM*4D0 * CI*(OM3*1D0/4D0 * P3(2)*(TMP0+4D0*(TMP1))+(
     $ +1D0/4D0 * CI*(F2(6)*F1(3))-1D0/4D0 * CI*(F2(5)*F1(4))
     $ -CI*(F2(4)*F1(5))+CI*(F2(3)*F1(6))))
      V3(6)= DENOM*4D0 * CI*(OM3*1D0/4D0 * P3(3)*(TMP0+4D0*(TMP1))+(
     $ +1D0/4D0*(F2(5)*F1(3))-1D0/4D0*(F2(6)*F1(4))-F2(3)*F1(5)
     $ +F2(4)*F1(6)))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     P(3,1)*Metric(1,2) - P(3,2)*Metric(1,2) - P(2,1)*Metric(1,3) +
C      P(2,3)*Metric(1,3) + P(1,2)*Metric(2,3) - P(1,3)*Metric(2,3)
C     
      SUBROUTINE VVV1_0(V1, V2, V3, COUP,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 V2(*)
      COMPLEX*16 TMP2
      COMPLEX*16 TMP12
      COMPLEX*16 V3(*)
      COMPLEX*16 TMP11
      REAL*8 P1(0:3)
      COMPLEX*16 TMP10
      REAL*8 P2(0:3)
      COMPLEX*16 TMP7
      REAL*8 P3(0:3)
      COMPLEX*16 TMP6
      COMPLEX*16 TMP5
      COMPLEX*16 VERTEX
      COMPLEX*16 COUP
      COMPLEX*16 TMP9
      COMPLEX*16 V1(*)
      COMPLEX*16 TMP8
      P1(0) = DBLE(V1(1))
      P1(1) = DBLE(V1(2))
      P1(2) = DIMAG(V1(2))
      P1(3) = DIMAG(V1(1))
      P2(0) = DBLE(V2(1))
      P2(1) = DBLE(V2(2))
      P2(2) = DIMAG(V2(2))
      P2(3) = DIMAG(V2(1))
      P3(0) = DBLE(V3(1))
      P3(1) = DBLE(V3(2))
      P3(2) = DIMAG(V3(2))
      P3(3) = DIMAG(V3(1))
      TMP9 = (P3(0)*V2(3)-P3(1)*V2(4)-P3(2)*V2(5)-P3(3)*V2(6))
      TMP8 = (V2(3)*P1(0)-V2(4)*P1(1)-V2(5)*P1(2)-V2(6)*P1(3))
      TMP2 = (V2(3)*V1(3)-V2(4)*V1(4)-V2(5)*V1(5)-V2(6)*V1(6))
      TMP5 = (V3(3)*P1(0)-V3(4)*P1(1)-V3(5)*P1(2)-V3(6)*P1(3))
      TMP7 = (V1(3)*V3(3)-V1(4)*V3(4)-V1(5)*V3(5)-V1(6)*V3(6))
      TMP6 = (V3(3)*P2(0)-V3(4)*P2(1)-V3(5)*P2(2)-V3(6)*P2(3))
      TMP11 = (V1(3)*P2(0)-V1(4)*P2(1)-V1(5)*P2(2)-V1(6)*P2(3))
      TMP10 = (V2(3)*V3(3)-V2(4)*V3(4)-V2(5)*V3(5)-V2(6)*V3(6))
      TMP12 = (P3(0)*V1(3)-P3(1)*V1(4)-P3(2)*V1(5)-P3(3)*V1(6))
      VERTEX = COUP*(TMP10*(-CI*(TMP11)+CI*(TMP12))+(TMP2*(-CI*(TMP5)
     $ +CI*(TMP6))+TMP7*(-CI*(TMP9)+CI*(TMP8))))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     ProjM(2,1) + ProjP(2,1)
C     
      SUBROUTINE FFS4_3(F1, F2, COUP, M3, W3,S3)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 DENOM
      COMPLEX*16 S3(3)
      REAL*8 W3
      REAL*8 P3(0:3)
      REAL*8 M3
      COMPLEX*16 F1(*)
      COMPLEX*16 F2(*)
      COMPLEX*16 TMP4
      COMPLEX*16 COUP
      COMPLEX*16 TMP3
      S3(1) = +F1(1)+F2(1)
      S3(2) = +F1(2)+F2(2)
      P3(0) = -DBLE(S3(1))
      P3(1) = -DBLE(S3(2))
      P3(2) = -DIMAG(S3(2))
      P3(3) = -DIMAG(S3(1))
      TMP4 = (F2(5)*F1(5)+F2(6)*F1(6))
      TMP3 = (F2(3)*F1(3)+F2(4)*F1(4))
      DENOM = COUP/(P3(0)**2-P3(1)**2-P3(2)**2-P3(3)**2 - M3 * (M3 
     $ -CI* W3))
      S3(3)= DENOM*(+CI*(TMP3+TMP4))
      END


C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Metric(1,2)
C     
      SUBROUTINE VVS1_0(V1, V2, S3, COUP,VERTEX)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 V2(*)
      COMPLEX*16 TMP2
      COMPLEX*16 S3(*)
      COMPLEX*16 VERTEX
      COMPLEX*16 COUP
      COMPLEX*16 V1(*)
      TMP2 = (V2(3)*V1(3)-V2(4)*V1(4)-V2(5)*V1(5)-V2(6)*V1(6))
      VERTEX = COUP*-CI * TMP2*S3(3)
      END










