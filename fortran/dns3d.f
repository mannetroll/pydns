      PROGRAM RUN_DNS
C***********************************************************************
C  Simple diagnostic harness for the legacy DNS routines (visasub.f)
C  Updated: 100 iterations, log every 10th step, report FPS at the end.
C***********************************************************************

      IMPLICIT NONE
      INTEGER N, N3D2, PX, PY
      PARAMETER (N = 256)
      PARAMETER (N3D2 = 3*N/2)
      PARAMETER (PX = N3D2*2)
      PARAMETER (PY = N3D2*2)

C     Arrays
      COMPLEX   UC(1+3*N/4,3*N/2,3)
      COMPLEX   OM2(N/2,N)
      COMPLEX   FNM1(N/2,N)
      REAL      ALFA(N/2), GAMMA(N)
      REAL      TFFTXZ(2+3*N/2,3*N/2,4)
      REAL      PREX(15+3*N/2,4)
      REAL      PREZ(15+3*N,4)
      REAL      WSAVE(15+N)
      REAL      UR(2+3*N/2,3*N/2,3)
      INTEGER   PIXARR(PX,PY), STEPS

      EQUIVALENCE (UC,UR)

C     Scalars
      REAL      RE, K0, VISC, T, DT, CN, CNM1, CFLNUM
      INTEGER   IT, IFN, NE

C     Timing for FPS (using standard CPU_TIME)
      REAL      TBEGIN, TEND, ELAP, FPS, ELAP2
      INTEGER COUNT_RATE, START, FINISH
      INTEGER START2, FINISH2, COUNT_RATE2, COUNT_MAX, TICKS
      CALL SYSTEM_CLOCK(START2, COUNT_RATE2, COUNT_MAX)

      NE = N
      RE = 1000.0
      K0 = 10.0
      VISC = 1.0/RE
      T = 0.0
      DT = 0.0
      CN = 1.0
      CNM1 = 0.0
      CFLNUM = 0.75
      IT = 0
      IFN = 1

      WRITE(*,*) '--- INITIALIZING DNS3D ---'
      WRITE(*,*) 'N=',N

C===== Initialization sequence =========================================

C 1. PAO
      CALL PAO(UC,ALFA,GAMMA,N,NE,RE,K0,VISC)
      WRITE(*,*) 'After PAO: VISC=',VISC
      WRITE(*,*) '  Max |UC|=',MAXVAL(ABS(UC))

C 2. INIT
      CALL INIT(N,N,ALFA,GAMMA,FNM1,PREX,PREZ,WSAVE)
      WRITE(*,*) 'After INIT  Max(ALFA)=',MAXVAL(ALFA),
     &           ' Max(GAMMA)=',MAXVAL(GAMMA)

C 3. CALCOM
      CALL CALCOM(N,N,ALFA,GAMMA,UC,OM2)
      WRITE(*,*) 'After CALCOM  Max |OM2|=',MAXVAL(ABS(OM2))

C 4. STEP2A
      CALL STEP2A(N,N,UC,UR,TFFTXZ,PREX,PREZ)
      WRITE(*,*) 'Max |UC| before STEP2A =', MAXVAL(ABS(UC))
      WRITE(*,*) 'Max |UR| after STEP2A  =', MAXVAL(ABS(UR))

C===== Time stepping ===================================================

      CALL NEXTDT(N,N,UR,CFLNUM,IT,IFN,DT,CN)
      WRITE(*,*) 'Initial DT=',DT,' CN=',CN

C     Start timing before the main loop
      CALL CPU_TIME(TBEGIN)
      CALL SYSTEM_CLOCK(START2)

C     1000 time steps, log every 10th step
      STEPS = 1000
      DO 100 IT=1,STEPS
         CALL STEP2B(N,N,UC,UR,TFFTXZ,PREX,PREZ)
         CALL STEP3(N,N,UC,UR,TFFTXZ,PREX,PREZ,
     &              OM2,ALFA,GAMMA,FNM1,VISC,T,DT,CN,CNM1)
         CALL STEP2A(N,N,UC,UR,TFFTXZ,PREX,PREZ)
         CALL NEXTDT(N,N,UR,CFLNUM,IT,IFN,DT,CN)

         IF (MOD(IT,100) .EQ. 0) THEN
            WRITE(*,*) 'Step',IT,' T=',T,' DT=',DT,' CN=',CN
         END IF
  100 CONTINUE

C     Stop timing after the loop and compute FPS
      CALL CPU_TIME(TEND)
      ELAP = TEND - TBEGIN
      IF (ELAP .GT. 0.0) THEN
         FPS = REAL(STEPS) / ELAP
      ELSE
         FPS = 0.0
      END IF

      WRITE(*,*) 'Elapsed CPU time for 100 steps (s) =', ELAP
      WRITE(*,*) 'Frames per second (FPS)            =', FPS

      CALL SYSTEM_CLOCK(FINISH2)
      TICKS = FINISH2 - START2
      IF (TICKS .LT. 0) TICKS = TICKS + COUNT_MAX
      ELAP2= FLOAT(TICKS) / FLOAT(COUNT_RATE2)
      PRINT *, 'Elapsed time (s): ', ELAP2
      PRINT *, 'FPS: ', REAL(STEPS) / ELAP2

C===== Visualization / sanity checks ===================================

      CALL FIELD2PIX(PIXARR,PX,PY,UR,75.0,N3D2,N3D2,1)
      CALL FIELD2PIX(PIXARR,PX,PY,UR,75.0,N3D2,N3D2,2)
      CALL FIELD2PIX(PIXARR,PX,PY,UR,75.0,N3D2,N3D2,3)
      CALL FIELD2KIN(PIXARR,PX,PY,UR,125.0,N3D2,N3D2)
      CALL OM2PHYS(N,N,UC,UR,OM2,TFFTXZ,PREX,PREZ)

      WRITE(*,*) 'Pixels sample:',PIXARR(1,1),PIXARR(64,64)
      WRITE(*,*) 'Final T=',T,' CN=',CN,' DT=',DT,' VISC=',VISC
      WRITE(*,*) 'Final Max |UR|=',MAXVAL(ABS(UR))
      WRITE(*,*) 'Final Max |OM2|=',MAXVAL(ABS(OM2))
      WRITE(*,*) 'Final Max |PIXARR|=',MAXVAL(ABS(PIXARR))


      END