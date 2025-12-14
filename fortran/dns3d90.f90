program run_dns
!***********************************************************************
!  Heap-allocated harness for legacy DNS routines (visasub3d.f)
!
!  Key point: UC and UR MUST alias the same storage (old EQUIVALENCE).
!  We implement that via one REAL buffer (UR) + a COMPLEX pointer view (UC).
!
!  Usage:  ./dns3d90.exe <N>   (default 512)
!***********************************************************************

  use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_float, c_float_complex
  implicit none

  integer :: n, narg, n3d2
  integer :: ne, it, ifn, steps
  real(c_float) :: re, k0, visc, t, dt, cn, cnm1, cflnum
  character(len=32) :: arg

  ! Timing
  real(c_float) :: tbegin, tend, elap, fps, elap2
  integer :: start2, finish2, count_rate2, count_max, ticks

  ! Arrays (allocated from N)
  real(c_float), allocatable, target :: ur(:,:,:)
  complex(c_float_complex), pointer  :: uc(:,:,:)

  complex(c_float_complex), allocatable :: om2(:,:)
  complex(c_float_complex), allocatable :: fnm1(:,:)
  real(c_float),    allocatable :: alfa(:)
  real(c_float),    allocatable :: gamma(:)
  real(c_float),    allocatable :: tfftxz(:,:,:)
  real(c_float),    allocatable :: prex(:,:)
  real(c_float),    allocatable :: prez(:,:)
  real(c_float),    allocatable :: wsave(:)

  type(c_ptr) :: p

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

  ! ---- Allocate UR (real buffer) ----
  allocate( ur(2 + 3*n/2, 3*n/2, 3) )

  ! ---- Create UC complex view over UR storage (old EQUIVALENCE) ----
  ! UC dims: (1+3*N/4, 3*N/2, 3) complex
  ! UR dims: (2+3*N/2, 3*N/2, 3) real
  ! Because 2*(1+3N/4) = 2 + 3N/2, the storage matches exactly.
  p = c_loc(ur(1,1,1))
  call c_f_pointer(p, uc, [1 + 3*n/4, 3*n/2, 3])

  ! ---- Allocate the rest ----
  allocate( om2(n/2, n) )
  allocate( fnm1(n/2, n) )
  allocate( alfa(n/2) )
  allocate( gamma(n) )
  allocate( tfftxz(2 + 3*n/2, 3*n/2, 4) )
  allocate( prex(15 + 3*n/2, 4) )
  allocate( prez(15 + 3*n,   4) )
  allocate( wsave(15 + n) )

  ! ---- Scalars ----
  ne = n
  re = 1000.0_c_float
  k0 = 10.0_c_float
  visc = 1.0_c_float / re
  t = 0.0_c_float
  dt = 0.0_c_float
  cn = 1.0_c_float
  cnm1 = 0.0_c_float
  cflnum = 0.75_c_float
  it = 0
  ifn = 1

  write(*,*) '--- INITIALIZING DNS3D ---'
  write(*,*) 'N=', n

  !===== Initialization sequence ======================================

  call pao(uc, alfa, gamma, n, ne, re, k0, visc)
  write(*,*) 'After PAO: VISC=', visc
  write(*,*) '  Max |UC|=', maxval(abs(uc))

  call init(n, n, alfa, gamma, fnm1, prex, prez, wsave)
  write(*,*) 'After INIT  Max(ALFA)=', maxval(alfa), ' Max(GAMMA)=', maxval(gamma)

  call calcom(n, n, alfa, gamma, uc, om2)
  write(*,*) 'After CALCOM  Max |OM2|=', maxval(abs(om2))

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
  if (elap > 0.0_c_float) then
     fps = real(steps, c_float) / elap
  else
     fps = 0.0_c_float
  end if

  write(*,*) 'Elapsed CPU time for 100 steps (s) =', elap
  write(*,*) 'Frames per second (FPS)            =', fps

  call system_clock(finish2)
  ticks = finish2 - start2
  if (ticks < 0) ticks = ticks + count_max
  elap2 = real(ticks, c_float) / real(count_rate2, c_float)

  print *, 'Elapsed time (s): ', elap2
  print *, 'FPS: ', real(steps, c_float) / elap2

end program run_dns