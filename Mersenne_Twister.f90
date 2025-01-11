program mersenne_twister

  implicit none
  integer, parameter :: MT = 624
  integer, parameter :: M = 397
  integer, parameter :: MATRIX_A = 0x9908b0df
  integer, parameter :: UPPER_MASK = 0x80000000
  integer, parameter :: LOWER_MASK = 0x7fffffff

  integer :: mt(MT), mti, i

  real :: random_number

  ! Initialize the MT array with a seed
  call init_genrand(12345)

  ! Generate and print 10 random numbers
  do i = 1, 10
    random_number = genrand_real1()
    write(*,*) random_number
  end do

contains

  subroutine init_genrand(seed)
    implicit none
    integer, intent(in) :: seed
    integer :: i

    mt(1) = seed & 0xffffffff
    do i = 2, MT
      mt(i) = 1812433253 * (mt(i-1) ^ (mt(i-1) >> 30)) + i
      mt(i) = mt(i) & 0xffffffff
    end do
    mti = MT
  end subroutine init_genrand

  function genrand_int32() result(r)
    implicit none
    integer :: r, i, x

    if (mti >= MT) then
      call genrand_int32()
    end if

    x = mt(mti)
    mti = mti + 1

    x ^= x >> 11
    x ^= (x << 7) & MATRIX_A
    x ^= (x << 15) & UPPER_MASK
    x ^= x >> 18

    r = x
  end function genrand_int32

  function genrand_real1() result(r)
    implicit none
    integer :: a
    real :: r

    a = genrand_int32()
    r = a * (1.0 / 4294967296.0)
  end function genrand_real1

end program mersenne_twister
