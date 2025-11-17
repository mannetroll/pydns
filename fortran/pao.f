      SUBROUTINE PAO(UR,ALFA,GAMMA,N,NE,RE,K0,VISC)
C********1*********2*********3*********4*********5*********6*********7**
C     Sets up a random isotropic field
      
      IMPLICIT NONE
      REAL PI
      COMPLEX IM,NOLL
      PARAMETER (PI=3.14159265358979)
      PARAMETER (IM=(0.00,1.00),NOLL=(0.00,0.00))
      
      INTEGER X,Z,I,N,ND2,NE,NED2
      INTEGER IMM,IT,ID,ISEED,IRPT,INSEED
      REAL ABSU2,ABSW2
      REAL ALFA(N/2),GAMMA(N),DALFA,DGAMMA
      REAL RANVEC(97),RAN
      REAL K0,NLAM,NORM,E1,E3,Q2,W2,A(7)
      REAL K,TETA1,EP,De,E110,U1U1,U3U3,RE,VISC
      REAL K2,KOL,DXZ
      COMPLEX UR(N*3/4+1,N*3/2,3)
      COMPLEX ARG
	SAVE ISEED
C      CHARACTER*32 NAMNUT

C***********************************************************************
      DXZ=2*PI/FLOAT(N)
      ND2=N/2
      NED2=NE/2
      NORM=PI*K0*K0
      ISEED=ISEED+1
      E1=1
      E3=1./E1
C***********************************************************************
      IMM=420029
      ID=5011
      IT=2017
      DO 100 I=1,97
         ISEED=MOD(ISEED*IMM+IT,ID)
 100  CONTINUE
      DO 200 I=1,97
         ISEED=MOD(ISEED*IMM+IT,ID)
         RANVEC(I)=FLOAT(ISEED)/FLOAT(ID)
 200  CONTINUE
C***********************************************************************
      DALFA=1./E1
      DGAMMA=1./E3
      DO 300 X=1,NED2
         ALFA(X)=FLOAT(X-1)*DALFA
 300  CONTINUE
      GAMMA(1)=0.
      DO 400 Z=2,NED2+1
         GAMMA(Z)=FLOAT(Z-1)*DGAMMA
         GAMMA(NE+2-Z)=-GAMMA(Z)
 400  CONTINUE
C     WRITE(*,*) 'N & NE',N,NE
C***********************************************************************
C     The average kinetic energy per mass, ie q2/2, is 1/2.
C     The power spectrum has the shape
C---------------------------------------------------------------------
C     
C     F(k) = C*exp(-(k/ko)^2)
C     
C---------------------------------------------------------------------
C     One angle TETA1 which is uniformly
C     disrtibuted on the interval (0,2pi) are generated.
C     At the end of the step UR contains (u0_th, w0_th)
C***********************************************************************
      DO 450 I=1,3
         DO 460 Z=1,3*N/2
            DO 470 X=1,3*N/4+1
               UR(X,Z,I)=NOLL
 470        CONTINUE
 460     CONTINUE
 450  CONTINUE

      DO 500 Z=1,NE
         DO 510 X=1,NED2
            ISEED=MOD(ISEED*IMM+IT,ID)
            RAN=FLOAT(ISEED)/FLOAT(ID)
            IRPT=1+INT(97.*RAN)
            TETA1=2.*PI*RANVEC(IRPT)
            RANVEC(IRPT)=RAN
            ARG=EXP(IM*TETA1)
            K=SQRT(ALFA(X)**2+GAMMA(Z)**2)

            IF(ALFA(X).EQ.0) THEN
               UR(X,Z,2)=0
               ABSU2=exp(-(K/K0)**2)/NORM
               UR(X,Z,1)=SQRT(ABSU2)*ARG
            ELSE
               ABSW2=exp(-(K/K0)**2)/(1+(GAMMA(Z)/ALFA(X))**2)/NORM
               UR(X,Z,2)=SQRT(ABSW2)*ARG
               UR(X,Z,1)=-GAMMA(Z)/ALFA(X)*UR(X,Z,2) !En slump bug?
            ENDIF
C     WRITE(*,98)ISEED,RAN,IRPT,TETA1,UR(X,Z,1),UR(X,Z,2)
 510     CONTINUE
C     PAUSE
 500  CONTINUE
 98   FORMAT(I5,F6.2,I4,F5.2,4(E10.2))
      DO 600 Z=2,NED2
         UR(1,NE+2-Z,1)=CONJG(UR(1,Z,1))
 600  CONTINUE

      DO 700 X=1,NED2
         UR(X,NED2+1,1)=NOLL
         UR(X,NED2+1,2)=NOLL
 700  CONTINUE 
      UR(1,1,1)=NOLL
      UR(1,1,2)=NOLL
      
C***********************************************************************
C     calculates some averages 
      DO 750 I=1,7
         A(I)=0
 750  CONTINUE 
      E110=0
      DO 800 X=1,ND2
         DO 810 Z=1,N
            U1U1=UR(X,Z,1)*CONJG(UR(X,Z,1))
            U3U3=UR(X,Z,2)*CONJG(UR(X,Z,2))
            K2=(ALFA(X)**2+GAMMA(Z)**2)
            IF(X.EQ.1) THEN
               A(1)=A(1)+U1U1
               A(2)=A(2)+U3U3
               A(3)=A(3)+U1U1*ALFA(X)**2
               A(4)=A(4)+U1U1*GAMMA(Z)**2
               A(5)=A(5)+U3U3*ALFA(X)**2
               A(6)=A(6)+U3U3*GAMMA(Z)**2
               A(7)=A(7)+(U1U1+U3U3)*K2*K2
               E110=E110+U1U1
            ELSE
               A(1)=A(1)+2*U1U1
               A(2)=A(2)+2*U3U3
               A(3)=A(3)+2*U1U1*ALFA(X)**2
               A(4)=A(4)+2*U1U1*GAMMA(Z)**2
               A(5)=A(5)+2*U3U3*ALFA(X)**2
               A(6)=A(6)+2*U3U3*GAMMA(Z)**2
               A(7)=A(7)+2*(U1U1+U3U3)*K2*K2
            ENDIF
 810     CONTINUE 
C     WRITE(*,99)X,(A(I),I=1,7)
 800  CONTINUE 
 99   FORMAT(I4,7(F9.3))
      Q2=A(1)+A(2)
      W2=A(3)+A(4)+A(5)+A(6)
      VISC=SQRT(Q2**2/RE/W2)
      EP=VISC*W2
      De=2*VISC**2*A(7)
      KOL=(VISC**3/EP)**0.25
      NLAM=0
      IF(E110.NE.0)NLAM=2*A(1)/E110
C************************************************************************
C      WRITE(*,11)'Reynolds n. =',RE
C      WRITE(*,11)'Energy      =',Q2
C      WRITE(*,11)'WiWi        =',W2
C      WRITE(*,11)'Epsilon     =',EP
C      WRITE(*,11)'a11         =',2*A(1)/Q2-1
C      WRITE(*,11)'e11         =',2*(A(3)+A(4))/W2-1
C      WRITE(*,11)'Time scale  =',0.5*Q2/EP
C      WRITE(*,11)'Kolmogorov  =',KOL
C      WRITE(*,11)'Viscosity   =',VISC
C      WRITE(*,11)'dx/Kol.     =',DXZ/KOL
C      WRITE(*,11)'2Pi/Nlamda  =',NLAM
C      WRITE(*,11)'2Pi/Lux     =',2*PI/SQRT(2*A(1)/A(3))
C      WRITE(*,11)'2Pi/Luz     =',2*PI/SQRT(2*A(1)/A(4))
C      WRITE(*,11)'2Pi/Lwx     =',2*PI/SQRT(2*A(2)/A(5))
C      WRITE(*,11)'2Pi/Lwz     =',2*PI/SQRT(2*A(2)/A(6))
C      WRITE(*,11)'Deps.       =',De
C      WRITE(*,11)'Ceps2       =',0.5*Q2*De/EP**2
C      WRITE(*,11)'E1          =',E1
C      WRITE(*,11)'E3          =',E3
 11   FORMAT(' ',A,F17.10)
C************************************************************************
C     reshuffle 
      DO 1000 I=1,2
         DO 1000 Z=NED2,1,-1
            DO 1000 X=1,NED2
               UR(X,N-NED2+Z,I)=UR(X,Z+NED2,I)
               IF(Z.LE.(N-NE))UR(X,Z+NED2,I)=NOLL
 1000       CONTINUE
C************************************************************************
C     OPEN(UNIT=4,FILE=NAMNUT,FORM='UNFORMATTED')
C     DO 10 I=1,2
C     DO 20 Z=1,N
C     WRITE(4)(UR(X,Z,I),X=1,ND2)
C     20	  CONTINUE
C     10    CONTINUE
C     CLOSE(UNIT=4)

            RETURN
            END 




