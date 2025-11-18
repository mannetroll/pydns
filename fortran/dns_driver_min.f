C***********************************************************************
C  dns_driver_min.f  —  Workspace + callable DNS API for Python (f2py)
C  Works with: pao.f, vfft.f, visasub.f, dns_fortran.pyf
C***********************************************************************

      MODULE DNS_WORKSPACE
      IMPLICIT NONE
C--- Global limits -----------------------------------------------------
      INTEGER, PARAMETER :: NMAX = 128

C--- Global scalar state -----------------------------------------------
      INTEGER, SAVE :: N, N3D2, PX, PY, IT, IFN
      REAL,    SAVE :: RE, K0, VISC, T, DT, CN, CNM1, CFLNUM

C--- Global arrays (workspace) -----------------------------------------
      COMPLEX, SAVE :: UC(1+3*NMAX/4,3*NMAX/2,3)
      COMPLEX, SAVE :: OM2(NMAX/2,NMAX)
      COMPLEX, SAVE :: FNM1(NMAX/2,NMAX)
      REAL,    SAVE :: ALFA(NMAX/2), GAMMA(NMAX)
      REAL,    SAVE :: TFFTXZ(2+3*NMAX/2,3*NMAX/2)
      REAL,    SAVE :: PREX(15+3*NMAX/2)
      REAL,    SAVE :: PREZ(15+3*NMAX)
      REAL,    SAVE :: WSAVE(15+NMAX)
      REAL,    SAVE :: UR(2+3*NMAX/2,3*NMAX/2,3)

C--- Legacy data layout: UC and UR share storage -----------------------
      EQUIVALENCE (UC,UR)

      END MODULE DNS_WORKSPACE
C=======================================================================


C=======================================================================
C  DNS_INIT  --  initialize full DNS state in module workspace
C=======================================================================
      SUBROUTINE DNS_INIT(NIN,REIN,K0IN)
      USE DNS_WORKSPACE
      IMPLICIT NONE
      INTEGER NIN
      REAL    REIN,K0IN

C--- Set problem size & parameters -------------------------------------
      N     = NIN
      RE    = REIN
      K0    = K0IN
      VISC  = 1.0/RE
      T     = 0.0
      DT    = 0.0
      CN    = 1.0
      CNM1  = 0.0
      CFLNUM= 0.75
      IT    = 0
      IFN   = 1
      N3D2  = 3*N/2
      PX    = 2*N3D2
      PY    = 2*N3D2

C--- Zero work arrays just in case -------------------------------------
      UR      = 0.0
      UC      = (0.0,0.0)
      OM2     = (0.0,0.0)
      FNM1    = (0.0,0.0)
      ALFA    = 0.0
      GAMMA   = 0.0
      TFFTXZ  = 0.0
      PREX    = 0.0
      PREZ    = 0.0
      WSAVE   = 0.0

C--- Original initialization sequence ----------------------------------
      WRITE(*,*) '--- DNS_INIT called ---'

      CALL PAO(UC,ALFA,GAMMA,N,N,RE,K0,VISC)
      WRITE(*,*) 'After PAO: VISC=',VISC
      WRITE(*,*) '  Max |UC|=',MAXVAL(ABS(UC))

      CALL INIT(N,N,ALFA,GAMMA,FNM1,PREX,PREZ,WSAVE)
      WRITE(*,*) 'After INIT  Max(ALFA)=',MAXVAL(ALFA),
     &           ' Max(GAMMA)=',MAXVAL(GAMMA)

      CALL CALCOM(N,N,ALFA,GAMMA,UC,OM2)
      WRITE(*,*) 'After CALCOM  Max |OM2|=',MAXVAL(ABS(OM2))

      CALL STEP2A(N,N,UC,UR,TFFTXZ,PREX,PREZ)
      WRITE(*,*) 'Max |UC| before STEP2A =', MAXVAL(ABS(UC))
      WRITE(*,*) 'Max |UR| after STEP2A  =', MAXVAL(ABS(UR))

      CALL NEXTDT(N,N,UR,CFLNUM,IT,IFN,DT,CN)
      WRITE(*,*) 'Initial DT=',DT,' CN=',CN

      RETURN
      END SUBROUTINE DNS_INIT
C=======================================================================


C=======================================================================
C  DNS_STEP  --  advance one timestep using current workspace
C=======================================================================
      SUBROUTINE DNS_STEP(TOUT,DTOUT,CNOUT)
      USE DNS_WORKSPACE
      IMPLICIT NONE
      REAL TOUT,DTOUT,CNOUT

      IT = IT + 1

      CALL STEP2B(N,N,UC,UR,TFFTXZ,PREX,PREZ)
      CALL STEP3(N,N,UC,UR,TFFTXZ,PREX,PREZ,
     &           OM2,ALFA,GAMMA,FNM1,VISC,T,DT,CN,CNM1)
      CALL STEP2A(N,N,UC,UR,TFFTXZ,PREX,PREZ)
      CALL NEXTDT(N,N,UR,CFLNUM,IT,IFN,DT,CN)

      TOUT  = T
      DTOUT = DT
      CNOUT = CN

      RETURN
      END SUBROUTINE DNS_STEP
C=======================================================================


C=======================================================================
C  DNS_FRAME  --  Fill a pixarr(nx,ny) supplied from Python
C=======================================================================
      SUBROUTINE DNS_FRAME(PIXARR,NPX,NPY,COMP)
      USE DNS_WORKSPACE
      IMPLICIT NONE
      INTEGER NPX,NPY,COMP
      INTEGER PIXARR(NPX,NPY)

C  Simple visualization via FIELD2PIX
      CALL FIELD2PIX(PIXARR, PX, PY, UR, 75.0, N3D2, N3D2, COMP)

      RETURN
      END SUBROUTINE DNS_FRAME

C=======================================================================
C  DNS_KINETIC  --  Fill a pixarr(nx,ny) supplied from Python
C=======================================================================
      SUBROUTINE DNS_KINETIC(PIXARR,NPX,NPY)
      USE DNS_WORKSPACE
      IMPLICIT NONE
      INTEGER NPX,NPY
      INTEGER PIXARR(NPX,NPY)

C  Simple visualization via FIELD2KIN
      CALL FIELD2KIN(PIXARR, PX, PY, UR, 125.0, N3D2, N3D2)

      RETURN
      END SUBROUTINE DNS_KINETIC

C=======================================================================
C  DNS_OM2PHYS -- Fill a pixarr(nx,ny) supplied from Python
C=======================================================================
      SUBROUTINE DNS_OM2PHYS(PIXARR,NPX,NPY)
      USE DNS_WORKSPACE
      IMPLICIT NONE
      INTEGER NPX,NPY
      INTEGER PIXARR(NPX,NPY)

C  Simple visualization via OM2PHYS
      CALL OM2PHYS(N, N, UC, UC, OM2, TFFTXZ, PREX, PREZ)
      CALL FIELD2PIX(PIXARR, PX, PY, UR, 7.0, N3D2, N3D2, 3)

      RETURN
      END SUBROUTINE DNS_OM2PHYS

C=======================================================================
C  DNS_STREAMFUNC -- Fill a pixarr(nx,ny) supplied from Python
C=======================================================================
      SUBROUTINE DNS_STREAMFUNC(PIXARR,NPX,NPY)
      USE DNS_WORKSPACE
      IMPLICIT NONE
      INTEGER NPX,NPY
      INTEGER PIXARR(NPX,NPY)

C  Simple visualization via OM2PHYS
      CALL STREAMFUNC(N,N,ALFA,GAMMA,UC,UR,OM2,TFFTXZ,PREX,PREZ)
      CALL FIELD2PIX(PIXARR,PX,PY,UR,75.0,N3D2,N3D2,4)

      RETURN
      END SUBROUTINE DNS_STREAMFUNC


C=======================================================================


C=======================================================================
C  TEST_DNS  --  standalone diagnostic harness (unchanged behaviour)
C=======================================================================
      SUBROUTINE TEST_DNS()
      USE DNS_WORKSPACE
      IMPLICIT NONE
      INTEGER NLOCAL, N3D2L, PXL, PYL
      PARAMETER (NLOCAL = 64)
      PARAMETER (N3D2L = 3*NLOCAL/2)
      PARAMETER (PXL = N3D2L*2)
      PARAMETER (PYL = N3D2L*2)

C-- Local work arrays ---------------------------------------------------
      COMPLEX   OM2L(NLOCAL/2,NLOCAL)
      COMPLEX   FNM1L(NLOCAL/2,NLOCAL)
      REAL      ALFAL(NLOCAL/2), GAMMAL(NLOCAL)
      REAL      TFFTXZL(2+3*NLOCAL/2,3*NLOCAL/2)
      REAL      PREXL(15+3*NLOCAL/2)
      REAL      PREZL(15+3*NLOCAL)
      REAL      WSAVEL(15+NLOCAL)
      INTEGER   PIXARRL(PXL,PYL)

C-- Scalars -------------------------------------------------------------
      REAL      REL, K0L, VISCL, TL, DTL, CNL, CNM1L, CFLNUML
      INTEGER   ITL, IFNL, NEL

      NEL     = NLOCAL
      REL     = 1000.0
      K0L     = 10.0
      VISCL   = 1.0/REL
      TL      = 0.0
      DTL     = 0.0
      CNL     = 1.0
      CNM1L   = 0.0
      CFLNUML = 0.75
      ITL     = 0
      IFNL    = 1

      WRITE(*,*) '--- INITIALIZING DNS (TEST_DNS) ---'

      CALL PAO(UC,ALFAL,GAMMAL,NLOCAL,NEL,REL,K0L,VISCL)
      WRITE(*,*) 'After PAO: VISC=',VISCL
      WRITE(*,*) '  Max |UC|=',MAXVAL(ABS(UC))

      CALL INIT(NLOCAL,NLOCAL,ALFAL,GAMMAL,FNM1L,PREXL,PREZL,WSAVEL)
      WRITE(*,*) 'After INIT  Max(ALFA)=',MAXVAL(ALFAL),
     &           ' Max(GAMMA)=',MAXVAL(GAMMAL)

      CALL CALCOM(NLOCAL,NLOCAL,ALFAL,GAMMAL,UC,OM2L)
      WRITE(*,*) 'After CALCOM  Max |OM2|=',MAXVAL(ABS(OM2L))

      CALL STEP2A(NLOCAL,NLOCAL,UC,UR,TFFTXZL,PREXL,PREZL)
      WRITE(*,*) 'Max |UC| before STEP2A =', MAXVAL(ABS(UC))
      WRITE(*,*) 'Max |UR| after STEP2A  =', MAXVAL(ABS(UR))

      CALL NEXTDT(NLOCAL,NLOCAL,UR,CFLNUML,ITL,IFNL,DTL,CNL)
      WRITE(*,*) 'Initial DT=',DTL,' CN=',CNL

      DO ITL=1,10
         CALL STEP2B(NLOCAL,NLOCAL,UC,UR,TFFTXZL,PREXL,PREZL)
         CALL STEP3(NLOCAL,NLOCAL,UC,UR,TFFTXZL,PREXL,PREZL,
     &              OM2L,ALFAL,GAMMAL,FNM1L,VISCL,TL,DTL,CNL,CNM1L)
         CALL STEP2A(NLOCAL,NLOCAL,UC,UR,TFFTXZL,PREXL,PREZL)
         CALL NEXTDT(NLOCAL,NLOCAL,UR,CFLNUML,ITL,IFNL,DTL,CNL)
         WRITE(*,*) 'Step',ITL,' T=',TL,' DT=',DTL,' CN=',CNL
      END DO

      CALL FIELD2PIX(PIXARRL,PXL,PYL,UR,75.0,N3D2L,N3D2L,1)
      CALL FIELD2KIN(PIXARRL,PXL,PYL,UR,125.0,N3D2L,N3D2L)
      CALL OM2PHYS(NLOCAL,NLOCAL,UC,UR,OM2L,TFFTXZL,PREXL,PREZL)
      CALL STREAMFUNC(NLOCAL,NLOCAL,ALFAL,GAMMAL,UC,UR,OM2L,TFFTXZL,PREXL,PREZL)
      CALL FIELD2PIX(PIXARRL,PX,PY,UR,75.0,N3D2,N3D2,4)

      WRITE(*,*) 'Pixels sample:',PIXARRL(1,1),PIXARRL(64,64)
      WRITE(*,*) 'Final T=',TL,' CN=',CNL,' DT=',DTL,' VISC=',VISCL
      WRITE(*,*) 'Final Max |UR|=',MAXVAL(ABS(UR))
      WRITE(*,*) 'Final Max |OM2|=',MAXVAL(ABS(OM2L))
      WRITE(*,*) 'Final Max |PIXARR|=',MAXVAL(ABS(PIXARRL))
      RETURN
      END SUBROUTINE TEST_DNS
C=======================================================================


C=======================================================================
C  RUN_DNS  --  simple entry point kept for backward compatibility
C=======================================================================
      SUBROUTINE RUN_DNS(NIN,REIN,K0IN)
      IMPLICIT NONE
      INTEGER NIN
      REAL    REIN,K0IN

C  Current implementation just calls TEST_DNS and ignores arguments.
C  This preserves the behaviour you had when calling:
C      dns_fortran.run_dns(64,1000.0,10.0)
      CALL TEST_DNS()
      RETURN
      END SUBROUTINE RUN_DNS
C=======================================================================
C  DNS_SNAPSHOT  --  copy UR(:,:,COMP) into a 2D REAL plane for Python
C=======================================================================
      SUBROUTINE DNS_SNAPSHOT(PLANE,NX,NY,COMP)
      USE DNS_WORKSPACE
      IMPLICIT NONE
      INTEGER NX,NY,COMP
      REAL    PLANE(NX,NY)
      INTEGER I,J

C  No checks, assume NX,NY match 2+3*N/2 and 3*N/2
      DO J = 1, NY
         DO I = 1, NX
            PLANE(I,J) = UR(I,J,COMP)
         END DO
      END DO

      RETURN
      END SUBROUTINE DNS_SNAPSHOT