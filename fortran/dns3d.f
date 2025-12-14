      PROGRAM RUN_DNS
C***********************************************************************
C  Simple diagnostic harness for the legacy DNS routines (visasub3d.f)
C  Updated: 100 iterations, log every 10th step, report FPS at the end.
C
C  Change: N is read from command line:  ./dns3d.exe <N>
C          If not provided, defaults to 512.
C
C  NOTE: To keep Fortran-77 style + EQUIVALENCE(UC,UR), arrays are sized
C        with MAXN at compile-time. Runtime N must be <= MAXN.
C***********************************************************************

      IMPLICIT NONE

C----- Max compile-time size (adjust if you want) -----------------------
      INTEGER MAXN, MAXN3D2, MAXPX, MAXPY
      PARAMETER (MAXN   = 8192)
      PARAMETER (MAXN3D2 = 3*MAXN/2)
      PARAMETER (MAXPX  = MAXN3D2*2)
      PARAMETER (MAXPY  = MAXN3D2*2)

C----- Runtime sizes ----------------------------------------------------
      INTEGER N, N3D2, PX, PY

C----- Command line -----------------------------------------------------
      INTEGER NARG
      CHARACTER*32 ARG

C     Arrays (sized by MAXN, but routines use runtime N)
      COMPLEX   UC(1+3*MAXN/4,3*MAXN/2,3)
      COMPLEX   OM2(MAXN/2,MAXN)
      COMPLEX   FNM1(MAXN/2,MAXN)
      REAL      ALFA(MAXN/2), GAMMA(MAXN)
      REAL      TFFTXZ(2+3*MAXN/2,3*MAXN/2,4)
      REAL      PREX(15+3*MAXN/2,4)
      REAL      PREZ(15+3*MAXN,4)
      REAL      WSAVE(15+MAXN)
      REAL      UR(2+3*MAXN/2,3*MAXN/2,3)

      EQUIVALENCE (UC,UR)

C     Scalars
      REAL      RE, K0, VISC, T, DT, CN, CNM1, CFLNUM
      INTEGER   IT, IFN, NE, STEPS

C     Timing for FPS (using standard CPU_TIME)
      REAL      TBEGIN, TEND, ELAP, FPS, ELAP2
      INTEGER START2, FINISH2, COUNT_RATE2, COUNT_MAX, TICKS

C----- Read N from command line ----------------------------------------
      NARG = IARGC()
      IF (NARG .GE. 1) THEN
         CALL GETARG(1, ARG)
         READ(ARG,*) N
      ELSE
         N = 512
      END IF

      N3D2 = 3*N/2
      PX   = 2*N3D2
      PY   = 2*N3D2

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
      WRITE(*,*) 'N=',N,' (MAXN=',MAXN,')'

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

C     100 time steps, log every 10th step
      STEPS = 100
      DO 100 IT=1,STEPS
         CALL STEP2B(N,N,UC,UR,TFFTXZ,PREX,PREZ)
         CALL STEP3(N,N,UC,UR,TFFTXZ,PREX,PREZ,
     &              OM2,ALFA,GAMMA,FNM1,VISC,T,DT,CN,CNM1)
         CALL STEP2A(N,N,UC,UR,TFFTXZ,PREX,PREZ)
         CALL NEXTDT(N,N,UR,CFLNUM,IT,IFN,DT,CN)

         IF (MOD(IT,10) .EQ. 0) THEN
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

      END