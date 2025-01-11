program lcg_random_numbers

  implicit none
  integer :: seed, a, c, m, i, n
  real :: x, random_number

  ! Set the seed, multiplier, increment, and modulus
  seed = 12345
  a = 16807
  c = 0
  m = 2147483647

  ! Number of random numbers to generate
  n = 10

  ! Initialize the random number generator
  x = real(seed) / real(m)

  do i = 1, n
    ! LCG formula: x_(n+1) = (a * x_n + c) mod m
    x = (a * x + c) / real(m)
    random_number = x

    write(*, *) random_number
  end do

end program lcg_random_numbers
