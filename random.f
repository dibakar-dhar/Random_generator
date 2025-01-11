		  integer seed, a, c, m, i, n
		  real(kind=8):: x, rndm

		  ! Set the seed, multiplier, increment, and modulus
		  seed = 12345
		  a = 16807
		  c = 0
		  m = 2147483647

		  ! Number of random numbers to generate
		  n = 10000

		  ! Initialize the random number generator
		  x = real(seed) / real(m)

		  ! Generate and print random numbers
		  do i = 1, n
			x = (a * x + c) / real(m)
			rndm = x

			write(*,*) i, rndm
		  end do
		stop
		end
