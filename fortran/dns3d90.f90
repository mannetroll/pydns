program run_dns
!***********************************************************************
!  Heap-allocated diagnostic harness for legacy DNS routines (visasub3d.f)
!
!  Change: arrays are ALLOCATABLE and allocated from runtime N.
!          N is read from command line:  ./dns3d.exe <N>
!          Default N = 512
!
!  Note: this version allocates UC and UR separately (no EQUIVALENCE).
!        That avoids static .bss and linker limits on macOS/arm64.
!***********************************************************************

  implicit none

  integer :: n, n3d2
  integer :: ne, it, ifn, steps
  real    :: re, k0, visc, t, dt, cn, cnm1, cflnum

  ! Timing
  real    :: tbegin, tend, elap, fps, elap2
  integer :: start2, finish2, count_rate2, count_max, ticks

  ! Command line
  integer :: narg
  character(len=32) :: arg

  ! Arrays (allocated from N)
  complex, allocatable :: uc(:,:,:)
  complex, allocatable :: om2(:,:)
  complex, allocatable :: fnm1(:,:)
  real,    allocatable :: alfa(:)
  real,    allocatable :: gamma(:)
  real,    allocatable :: tfftxz(:,:,:)
  real,    allocatable :: prex(:,:)
  real,    allocatable :: prez(:,:)
  real,    allocatable :: wsave(:)
  real,    allocatable :: ur(:,:,:)

  ! ---- Read N from argv ----
  narg = command_argument_count()
  if (narg >= 1) then
     call get_command_argument(1, arg)
     read(arg, *) n
  else
     n = 512
  end if

  if (n <= 0) then
     write(*,*) 'ERROR: N must be > 0'
     stop
  end if

  n3d2 = 3*n/2

  call system_clock(start2, count_rate2, count_max)

  ! ---- Allocate arrays ----
  allocate( uc(1 + 3*n/4, 3*n/2, 3) )
  allocate( om2(n/2, n) )
  allocate( fnm1(n/2, n) )
  allocate( alfa(n/2) )
  allocate( gamma(n) )
  allocate( tfftxz(2 + 3*n/2, 3*n/2, 4) )
  allocate( prex(15 + 3*n/2, 4) )
  allocate( prez(15 + 3*n,   4) )
  allocate( wsave(15 + n) )
  allocate( ur(2 + 3*n/2, 3*n/2, 3) )

  ! ---- Scalars ----
  ne = n
  re = 1000.0
  k0 = 10.0
  visc = 1.0/re
  t = 0.0
  dt = 0.0
  cn = 1.0
  cnm1 = 0.0
  cflnum = 0.75
  it = 0
  ifn = 1

  write(*,*) '--- INITIALIZING DNS3D ---'
  write(*,*) 'N=', n

  !===== Initialization sequence ======================================

  ! 1. PAO
  call pao(uc, alfa, gamma, n, ne, re, k0, visc)
  write(*,*) 'After PAO: VISC=', visc
  write(*,*) '  Max |UC|=', maxval(abs(uc))

  ! 2. INIT
  call init(n, n, alfa, gamma, fnm1, prex, prez, wsave)
  write(*,*) 'After INIT  Max(ALFA)=', maxval(alfa), ' Max(GAMMA)=', maxval(gamma)

  ! 3. CALCOM
  call calcom(n, n, alfa, gamma, uc, om2)
  write(*,*) 'After CALCOM  Max |OM2|=', maxval(abs(om2))

  ! 4. STEP2A
  call step2a(n, n, uc, ur, tfftxz, prex, prez)
  write(*,*) 'Max |UC| before STEP2A =', maxval(abs(uc))
  write(*,*) 'Max |UR| after STEP2A  =', maxval(abs(ur))

  !===== Time stepping =================================================

  call nextdt(n, n, ur, cflnum, it, ifn, dt, cn)
  write(*,*) 'Initial DT=', dt, ' CN=', cn

  call cpu_time(tbegin)
  call system_clock(start2)

  steps = 100
  do it = 1, steps
     call step2b(n, n, uc, ur, tfftxz, prex, prez)
     call step3 (n, n, uc, ur, tfftxz, prex, prez, om2, alfa, gamma, fnm1, visc, t, dt, cn, cnm1)
     call step2a(n, n, uc, ur, tfftxz, prex, prez)
     call nextdt(n, n, ur, cflnum, it, ifn, dt, cn)

     if (mod(it,10) == 0) then
        write(*,*) 'Step', it, ' T=', t, ' DT=', dt, ' CN=', cn
     end if
  end do

  call cpu_time(tend)
  elap = tend - tbegin
  if (elap > 0.0) then
     fps = real(steps) / elap
  else
     fps = 0.0
  end if

  write(*,*) 'Elapsed CPU time for 100 steps (s) =', elap
  write(*,*) 'Frames per second (FPS)            =', fps

  call system_clock(finish2)
  ticks = finish2 - start2
  if (ticks < 0) ticks = ticks + count_max
  elap2 = real(ticks) / real(count_rate2)

  print *, 'Elapsed time (s): ', elap2
  print *, 'FPS: ', real(steps) / elap2

end program run_dns