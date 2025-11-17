C	SUBROUTINE VISASUB
C********1*********2*********3*********4*********5*********6*********7**
C	IMPLICIT NONE
C	INCLUDE 'parameter3.f'
C	INCLUDE 'variabel3.f'
C	
C*********************************MAIN**********************************
C	IF(IT.EQ.0)THEN
C	   CALL READINPUT(T0,T,IT,CN,CNM1,CFLNUM,TMAX,
C    &                  ITMAX,IFN,K0,RE)
C	   CALL PAO(UC,ALFA,GAMMA,NX,NX,RE,K0,VISC)
C	   CALL INIT(NX,NZ,ALFA,GAMMA,FNM1,PREX,PREZ,WSAVE)
C	   CALL CALCOM(NX,NZ,ALFA,GAMMA,UC,OM2)
C	END IF
C
C	CALL STEP2A(NX,NZ,UC,UR,TFFTXZ,PREX,PREZ)		!Non-linear
C	IF(IFN.NE.0)CALL NEXTDT(NX,NZ,UR,CFLNUM,IT,IFN,DT,CN)
C	CALL STEP2B(NX,NZ,UC,UR,TFFTXZ,PREX,PREZ)		!Non-linear
C	CALL STEP3(NX,NZ,UC,UR,TFFTXZ,PREX,PREZ,
C    &           OM2,ALFA,GAMMA,FNM1,VISC,T,DT,CN,CNM1)		!One-diagonal
C	WRITE(*,'(I4,A,F12.9)')IT,'  T = ',T
C	
C	RETURN
C	END
C*********************************************************************
C*******************************SUBBS*********************************
C*********************************************************************
	SUBROUTINE READINPUT(T0,T,IT,CN,CNM1,CFLNUM,TMAX,ITMAX,IFN,K0,RE)
	IMPLICIT NONE
	INTEGER IT,ITMAX,IFN
	REAL    T0,T,TMAX,DT,CN,CNM1,CFLNUM,K0,RE
	T0=0
      T=T0
      IT=0
      CN=1
      CNM1=0
      CFLNUM=0.7
	TMAX=10
	DT=0
	ITMAX=10000
	IFN=1
	K0=10
	RE=500
	RETURN
	END			!READINPUT
C*********************************************************************
	SUBROUTINE INIT(NX,NZ,ALFA,GAMMA,FNM1,PREX,PREZ,WSAVE)
	IMPLICIT NONE
	INTEGER   NX,NZ
	COMPLEX   NOLL
	PARAMETER (NOLL=(0.00,0.00))
	COMPLEX   FNM1(NX/2,NZ)
	REAL      ALFA(NX/2),GAMMA(NZ) 
	REAL      PREX(3*NX/2+15),PREZ(3*NZ+15),WSAVE(NX+15)
	INTEGER   X,Z
	CALL VRFFTI(3*NX/2,PREX,0)
	CALL VRFFTI(NX,WSAVE,0)
	CALL VCFFTI(3*NZ/2,PREZ,0)
	DO 200 X=1,NX/2
	   ALFA(X)=FLOAT(X-1)
 200	CONTINUE
	GAMMA(1)=0.
	DO 300 Z=2,NZ/2+1
	   GAMMA(Z)=FLOAT(Z-1)
	   GAMMA(NZ+2-Z)=-GAMMA(Z)
 300	CONTINUE
	DO 400 X=1,NX/2
	   DO 500 Z=1,NZ
	      FNM1(X,Z)=NOLL
 500	   CONTINUE
 400	CONTINUE
	RETURN
	END			!INIT

C*************************************************************
	SUBROUTINE CALCOM(NX,NZ,ALFA,GAMMA,UC,OM2)
	IMPLICIT NONE
	INTEGER NX,NZ
      COMPLEX UC(3*NX/4+1,3*NZ/2,3),OM2(NX/2,NZ)
	REAL ALFA(NX/2),GAMMA(NZ)
	COMPLEX IM
	PARAMETER (IM=(0.00,1.00))
	INTEGER X,Z
C       calculate omega2_th spectrally
	DO 100 Z=1,NZ
	   DO 110 X=1,NX/2
	      OM2(X,Z)=IM*(GAMMA(Z)*UC(X,Z,1)-ALFA(X)*UC(X,Z,2))
 110	   CONTINUE
 100	CONTINUE

	RETURN
	END			!CALCOM

C***********************************************************************
	SUBROUTINE STEP2A(NX,NZ,UC,UR,TFFTXZ,PREX,PREZ)
	IMPLICIT NONE
	INTEGER NX,NZ
	COMPLEX UC(3*NX/4+1,3*NZ/2,3)
	REAL UR(3*NX/2+2,3*NZ/2,3)
      REAL TFFTXZ(3*NX/2+2,3*NZ/2),PREX(3*NX/2+15),PREZ(3*NZ+15)
	COMPLEX   NOLL
	PARAMETER (NOLL=(0.00,0.00))
	INTEGER I,X,Z
C     EQUIVALENCE (UC,UR)

C       reshuffle and insert zeros in a-g-plane
	DO 100 I=1,2
	   DO 110 Z=1,3*NZ/2
	      DO 120 X=NX/2+1,3*NX/4+1
		     UC(X,Z,I)=NOLL
 120	      CONTINUE
 110	   CONTINUE
 100	CONTINUE

	DO 200 I=1,2
	   DO 210 Z=1,NZ/2
	      DO 220 X=1,NX/2
		     UC(X,Z+NZ,I)=UC(X,Z+NZ/2,I)
		     UC(X,Z+NZ/2,I)=NOLL
 220	      CONTINUE
 210	   CONTINUE
 200	CONTINUE

C       transform ui_h to phys space
	DO 500 I=1,2
	   CALL VCFFTB(UR(1,1,I),UR(2,1,I),TFFTXZ,TFFTXZ(2,1),
     &                 3*NZ/2,NX/2,3*NX/2+2,2,PREZ)
	   CALL VRFFTB(UR(1,1,I),UR(2,1,I),TFFTXZ,TFFTXZ(2,1),
     &                 3*NX/2,3*NZ/2,2,3*NX/2+2,PREX)
 500	CONTINUE
C       ui now real and stored in UR(,,i)
	
	RETURN
	END

C***********************************************************************
	SUBROUTINE STEP2B(NX,NZ,UC,UR,TFFTXZ,PREX,PREZ)
	IMPLICIT NONE
	INTEGER NX,NZ
	COMPLEX UC(3*NX/4+1,3*NZ/2,3)
	REAL UR(3*NX/2+2,3*NZ/2,3)
      REAL TFFTXZ(3*NX/2+2,3*NZ/2),PREX(3*NX/2+15),PREZ(3*NZ+15)
	COMPLEX   NOLL
	PARAMETER (NOLL=(0.00,0.00))
	INTEGER I,X,Z
C     EQUIVALENCE (UC,UR)
C
C       calculate uiuj and put into UR(x,z,1-3)
	DO 700 Z=1,3*NZ/2
	   DO 710 X=1,3*NX/2
	      UR(X,Z,3)=UR(X,Z,1)*UR(X,Z,2)
	      UR(X,Z,1)=UR(X,Z,1)*UR(X,Z,1)
	      UR(X,Z,2)=UR(X,Z,2)*UR(X,Z,2)
 710	   CONTINUE
 700	CONTINUE

C       transform uiuj back to a-g space, i.e. to uiuj_th
	DO 800 I=1,3
	   CALL VRFFTF(UR(1,1,I),UR(2,1,I),TFFTXZ,TFFTXZ(2,1),
     &                 3*NX/2,3*NZ/2,2,3*NX/2+2,PREX)
	   CALL VCFFTF(UR(1,1,I),UR(2,1,I),TFFTXZ,TFFTXZ(2,1),
     &                 3*NZ/2,NX/2,3*NX/2+2,2,PREZ)
 800	CONTINUE

C       put the 'extra' fourier coeff. to zero (the middle one)
	DO 900 I=1,3
	   DO 910 X=1,NX/2
	      UC(X,NZ+1,I)=NOLL
 910	   CONTINUE
 900	CONTINUE
	
	RETURN
	END			!STEP2
C***********************************************************************
	SUBROUTINE STEP3(NX,NZ,UC,UR,TFFTXZ,PREX,PREZ,
     &                 OM2,ALFA,GAMMA,FNM1,VISC,T,DT,CN,CNM1)
	IMPLICIT NONE
	INTEGER NX,NZ
	COMPLEX UC(3*NX/4+1,3*NZ/2,3),OM2(NX/2,NZ),FNM1(NX/2,NZ)
	REAL UR(3*NX/2+2,3*NZ/2,3)
      REAL TFFTXZ(3*NX/2+2,3*NZ/2),PREX(3*NX/2+15),PREZ(3*NZ+15)
	REAL ALFA(NX/2),GAMMA(NZ)
	COMPLEX   NOLL,IM
	PARAMETER (NOLL=(0.00,0.00),IM=(0.00,1.00))
	COMPLEX FN
	REAL DIVXZ,K2,VT,VISC,T,DT,CN,CNM1
	INTEGER I,X,Z
C     EQUIVALENCE (UC,UR)
C
C       colve for omega2_th, u_th and w_th
C       in the first timestep FNM1=0 and the eulerstep is DT0

	DIVXZ=1./(FLOAT(3*NX/2)*FLOAT(3*NZ/2))
	VT=0.5*VISC*DT
	DO 100 Z=1,NZ/2
	   DO 110 X=1,NX/2
	      FN=(GAMMA(Z)*ALFA(X)*(UC(X,Z,1)-UC(X,Z,2))+
     &           UC(X,Z,3)*(GAMMA(Z)**2-ALFA(X)**2))*DIVXZ
	      K2=(ALFA(X)**2+GAMMA(Z)**2)
	      OM2(X,Z)=(OM2(X,Z)*(1-VT*K2)+
     &             DT*0.5*((2+CNM1)*FN-CNM1*FNM1(X,Z)))/(1+VT*K2)
	      FNM1(X,Z)=FN	!save fn for next time step
 110	   CONTINUE
 100	CONTINUE

	DO 200 Z=NZ/2+1,NZ
	   DO 210 X=1,NX/2
	      FN=(GAMMA(Z)*ALFA(X)*(UC(X,Z+NZ/2,1)-UC(X,Z+NZ/2,2))+
     &           UC(X,Z+NZ/2,3)*(GAMMA(Z)**2-ALFA(X)**2))*DIVXZ
	      K2=(ALFA(X)**2+GAMMA(Z)**2)
	      OM2(X,Z)=(OM2(X,Z)*(1-VT*K2)+
     &             DT*0.5*((2+CNM1)*FN-CNM1*FNM1(X,Z)))/(1+VT*K2)
	      FNM1(X,Z)=FN	!save fn for next time step
 210	   CONTINUE
 200	CONTINUE

      CNM1=CN                 !save cn for next time step

	DO 300 Z=1,NZ
	   DO 310 X=2,NX/2
	      K2=(ALFA(X)**2+GAMMA(Z)**2)
	      UC(X,Z,1)=-IM*GAMMA(Z)*OM2(X,Z)/K2
	      UC(X,Z,2)=IM*ALFA(X)*OM2(X,Z)/K2
 310	   CONTINUE
 300	CONTINUE

	DO 400 Z=2,NZ           !X=1
	   UC(1,Z,1)=-IM*OM2(1,Z)/GAMMA(Z)
	   UC(1,Z,2)=NOLL
 400	CONTINUE

      UC(1,1,1)=NOLL
	UC(1,1,2)=NOLL
      T=T+DT

	RETURN
	END			!STEP3

C***********************************************************************
	SUBROUTINE NEXTDT(NX,NZ,UR,CFLNUM,IT,IFN,DT,CN)
	IMPLICIT NONE
	INTEGER NX,NZ,IT,IFN
	REAL UR(3*NX/2+2,3*NZ/2,3)
	REAL PI,CFLM,DX,DZ,CFL,CFLNUM,DT,CN
	PARAMETER (PI=3.14159265358979)
	INTEGER X,Z
C       calculates the next time step
      CFLM=0

      IF((IT.EQ.0).AND.(DT.EQ.0))THEN
        DX=2*PI/FLOAT(NX)
        DZ=2*PI/FLOAT(NZ)
        DO 100 Z=1,3*NZ/2
		  DO 110 X=1,3*NX/2
			  CFLM=MAX(CFLM,ABS(UR(X,Z,1))/DX+ABS(UR(X,Z,2))/DZ)
 110		  CONTINUE
 100    CONTINUE
	  DT=CFLNUM/CFLM/PI
	  RETURN
	END IF

	IF(MOD(IT,IFN).EQ.0)THEN
        DX=2*PI/FLOAT(NX)
        DZ=2*PI/FLOAT(NZ)
        DO 200 Z=1,3*NZ/2
		  DO 210 X=1,3*NX/2
			  CFLM=MAX(CFLM,ABS(UR(X,Z,1))/DX+ABS(UR(X,Z,2))/DZ)
 210		  CONTINUE
 200    CONTINUE
        CFL=CFLM*DT*PI
	  CN=0.8+0.2*CFLNUM/CFL
        DT=DT*CN
	  RETURN
	END IF

	RETURN
	END			!NEXTDT
C***********************************************************************
	SUBROUTINE FIELD2PIX(PIXARR,PX,PY,UR,ENFAC,NX3D2,NZ3D2,AXIS)
	IMPLICIT NONE
	INTEGER  NX3D2, NZ3D2, PX, PY, SIZE, AXIS
	REAL	 UR(NX3D2+2,NZ3D2,3)
	INTEGER  PIXARR(PX,PY)
	INTEGER  GRAY, I, J, II, JJ, PIXEL, SCALE
	REAL     ENFAC
	!C
	!C	32-bit Grayscale
	!C
	GRAY  = (1+256+65536)
	SCALE = MIN(PX/NX3D2,PY/NZ3D2)
	DO I=1,NX3D2
		DO J=1,NZ3D2
			PIXEL = MAX(1,MIN(255,(INT(ENFAC*UR(I,J,AXIS))+128)))*GRAY
			DO II = SCALE*(I-1)+1,SCALE*(I-1)+SCALE
				DO JJ = SCALE*(J-1)+1,SCALE*(J-1)+SCALE
					PIXARR(II,JJ) = PIXEL
				END DO
			END DO
		END DO
	END DO

	RETURN
	END			!FIELD2PIX

C***********************************************************************
	SUBROUTINE FIELD2KIN(PIXARR,PX,PY,UR,KEFAC,NX3D2,NZ3D2)
	IMPLICIT NONE
	INTEGER  NX3D2, NZ3D2, PX, PY, SIZE
	REAL	 UR(NX3D2+2,NZ3D2,3)
	INTEGER  PIXARR(PX,PY)
	INTEGER  GRAY, I, J, II, JJ, PIXEL, SCALE
	REAL     KEFAC, U2, W2, K
	!C
	!C	32-bit Grayscale
	!C
	GRAY  = (1+256+65536)
	SCALE = MIN(PX/NX3D2,PY/NZ3D2)
	DO I=1,NX3D2
		DO J=1,NZ3D2
			   U2 = UR(I,J,1)**2
			   W2 = UR(I,J,2)**2
				K = SQRT(U2+W2)
			PIXEL = MAX(1,MIN(255,(INT(KEFAC*K))))*GRAY
			DO II = SCALE*(I-1)+1,SCALE*(I-1)+SCALE
				DO JJ = SCALE*(J-1)+1,SCALE*(J-1)+SCALE
					PIXARR(II,JJ) = PIXEL
				END DO
			END DO
		END DO
	END DO

	RETURN
	END			!FIELD2KIN

C***********************************************************************
	SUBROUTINE OM2PHYS(NX,NZ,UC,UR,OM2,TFFTXZ,PREX,PREZ)
	IMPLICIT NONE
	INTEGER NX,NZ
	COMPLEX OM2(NX/2,NZ)
	COMPLEX UC(3*NX/4+1,3*NZ/2,3)
	REAL UR(3*NX/2+2,3*NZ/2,3)
      REAL TFFTXZ(3*NX/2+2,3*NZ/2),PREX(3*NX/2+15),PREZ(3*NZ+15)
	COMPLEX   NOLL
	PARAMETER (NOLL=(0.00,0.00))
	INTEGER I,X,Z
C      EQUIVALENCE (UC,UR)

C	Copy omega
	DO 10 Z=1,NZ
	  DO 20 X=1,NX/2
	    UC(X,Z,3) = OM2(X,Z)
20	  CONTINUE
10	CONTINUE

C     reshuffle and insert zeros in a-g-plane
	DO 110 Z=1,3*NZ/2
	  DO 120 X=NX/2+1,3*NX/4+1
		UC(X,Z,3) = NOLL
 120	  CONTINUE
 110	CONTINUE

	DO 210 Z=1,NZ/2
	  DO 220 X=1,NX/2
		UC(X,Z+NZ,3)   = UC(X,Z+NZ/2,3)
		UC(X,Z+NZ/2,3) = NOLL
 220	  CONTINUE
 210	CONTINUE

	DO 910 X=1,NX/2
	  UC(X,NZ+1,3)=NOLL
 910	CONTINUE

C       transform omega to phys space
	CALL VCFFTB(UR(1,1,3),UR(2,1,3),TFFTXZ,TFFTXZ(2,1),
     &                 3*NZ/2,NX/2,3*NX/2+2,2,PREZ)
	CALL VRFFTB(UR(1,1,3),UR(2,1,3),TFFTXZ,TFFTXZ(2,1),
     &                 3*NX/2,3*NZ/2,2,3*NX/2+2,PREX)
	
	RETURN
	END			!OM2PHYS

C***********************************************************************
	SUBROUTINE FIELD2PHI(PIXARR,PX,PY,UR,OMFAC,NX3D2,NZ3D2,AXIS)
	IMPLICIT NONE
	INTEGER  NX3D2, NZ3D2, PX, PY, SIZE, AXIS
	REAL	 UR(NX3D2+2,NZ3D2,3)
	INTEGER  PIXARR(PX,PY)
	INTEGER  GRAY, I, J, II, JJ, PIXEL, SCALE
	REAL     OMFAC
	!C
	!C	32-bit Grayscale
	!C
	GRAY  = (1+256+65536)
	SCALE = MIN(PX/NX3D2,PY/NZ3D2)
	DO I=1,NX3D2
		DO J=1,NZ3D2
			PIXEL = MAX(1,MIN(255,(INT(OMFAC*UR(I,J,AXIS)))))*GRAY
			DO II = SCALE*(I-1)+1,SCALE*(I-1)+SCALE
				DO JJ = SCALE*(J-1)+1,SCALE*(J-1)+SCALE
					PIXARR(II,JJ) = PIXEL
				END DO
			END DO
		END DO
	END DO

	RETURN
	END			!FIELD2OMG

C***********************************************************************
	SUBROUTINE PHIPHYS(NX,NZ,ALFA,GAMMA,UC,UR,TFFTXZ,PREX,PREZ)
	IMPLICIT NONE
	INTEGER NX,NZ
	REAL ALFA(NX/2),GAMMA(NZ)
	COMPLEX UC(3*NX/4+1,3*NZ/2,3)
	REAL UR(3*NX/2+2,3*NZ/2,3)
      REAL TFFTXZ(3*NX/2+2,3*NZ/2),PREX(3*NX/2+15),PREZ(3*NZ+15)
	REAL K2
	COMPLEX   NOLL, IM
	PARAMETER (NOLL=(0.00,0.00))
	PARAMETER (IM=(0.00,1.00))
	INTEGER I,X,Z
C      EQUIVALENCE (UC,UR)

C	Calculate phi
	DO 10 Z=2,NZ
	  DO 20 X=2,NX/2
	    UC(X,Z,3) = -IM/(2*ALFA(X)*GAMMA(Z))*
     &                (GAMMA(Z)*UC(X,Z,1) + ALFA(X)*UC(X,Z,2))
20	  CONTINUE
10	CONTINUE
	UC(1,1,3)=NOLL

	DO 30 X=2,NX/2
	    UC(X,1,3) = -IM/ALFA(X)*UC(X,1,1)
30	CONTINUE

	DO 40 Z=2,NZ
	    UC(1,Z,3) = -IM/GAMMA(Z)*UC(1,Z,2)
40	CONTINUE



C     reshuffle and insert zeros in a-g-plane
	DO 110 Z=1,3*NZ/2
	  DO 120 X=NX/2+1,3*NX/4+1
		UC(X,Z,3) = NOLL
 120	  CONTINUE
 110	CONTINUE

	DO 210 Z=1,NZ/2
	  DO 220 X=1,NX/2
		UC(X,Z+NZ,3)   = UC(X,Z+NZ/2,3)
		UC(X,Z+NZ/2,3) = NOLL
 220	  CONTINUE
 210	CONTINUE

	DO 910 X=1,NX/2
	  UC(X,NZ+1,3)=NOLL
 910	CONTINUE

C       transform omega to phys space
	CALL VCFFTB(UR(1,1,3),UR(2,1,3),TFFTXZ,TFFTXZ(2,1),
     &                 3*NZ/2,NX/2,3*NX/2+2,2,PREZ)
	CALL VRFFTB(UR(1,1,3),UR(2,1,3),TFFTXZ,TFFTXZ(2,1),
     &                 3*NX/2,3*NZ/2,2,3*NX/2+2,PREX)
	
	RETURN
	END			!PHIPHYS
C***********************************************************************
	SUBROUTINE STREAMFUNC(NX,NZ,ALFA,GAMMA,UC,UR,OM2,TFFTXZ,PREX,PREZ)
	IMPLICIT NONE
	INTEGER NX,NZ
	REAL ALFA(NX/2),GAMMA(NZ)
	COMPLEX OM2(NX/2,NZ)
	COMPLEX UC(3*NX/4+1,3*NZ/2,4)
	REAL UR(3*NX/2+2,3*NZ/2,4)
      REAL TFFTXZ(3*NX/2+2,3*NZ/2),PREX(3*NX/2+15),PREZ(3*NZ+15)
	REAL K2
	COMPLEX   NOLL, IM
	PARAMETER (NOLL=(0.00,0.00))
	PARAMETER (IM=(0.00,1.00))
	INTEGER I,X,Z
C      EQUIVALENCE (UC,UR)

C	Calculate Streamfunction
	DO 10 Z=1,NZ
	  DO 20 X=1,NX/2
			   K2 = ALFA(X)**2 + GAMMA(Z)**2
		UC(X,Z,4) = OM2(X,Z)/(K2 + 1.0E-30)
20	  CONTINUE
10	CONTINUE
	UC(1,1,4)=NOLL


C     reshuffle and insert zeros in a-g-plane
	DO 110 Z=1,3*NZ/2
	  DO 120 X=NX/2+1,3*NX/4+1
		UC(X,Z,4) = NOLL
 120	  CONTINUE
 110	CONTINUE

	DO 210 Z=1,NZ/2
	  DO 220 X=1,NX/2
		UC(X,Z+NZ,4)   = UC(X,Z+NZ/2,4)
		UC(X,Z+NZ/2,4) = NOLL
 220	  CONTINUE
 210	CONTINUE

	DO 910 X=1,NX/2
	  UC(X,NZ+1,4)=NOLL
 910	CONTINUE

C       transform omega to phys space
	CALL VCFFTB(UR(1,1,4),UR(2,1,4),TFFTXZ,TFFTXZ(2,1),
     &                 3*NZ/2,NX/2,3*NX/2+2,2,PREZ)
	CALL VRFFTB(UR(1,1,4),UR(2,1,4),TFFTXZ,TFFTXZ(2,1),
     &                 3*NX/2,3*NZ/2,2,3*NX/2+2,PREX)
	
	RETURN
	END			!STREAMFUNC
