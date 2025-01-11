program mid_square

  implicit none
  integer :: seed, n, i, temp
  real :: random_number

  ! Set the initial seed (a 4-digit number)
  seed = 1234

  ! Number of random numbers to generate
  n = 10000

  do i = 1, n
    ! Square the seed
    temp = seed**2

    ! Extract the middle 4 digits as the new seed
    seed = mod(temp, 10000)

    ! Convert the seed to a real number between 0 and 1
    random_number = real(seed) / 10000.0

    write(*,*) random_number
  end do

end program mid_square
