module utilities
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Classical prime finding algorithms
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    contains 

    subroutine eratostheneses_sieve(max, found, primes)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Classical O(log(log(n))) algorithm
        ! INPUT:
        ! max: an arbitrary number 
        ! OUTPUT:
        ! found: the number of primes less than max
        ! primes: an array of the found primes  
        ! INTERNAL: 
        ! mask: an array of logicals representing whether prime or not
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        implicit none

        integer :: i, j                 ! counters
        integer, intent(in) :: max      ! the upper number
        integer, intent(out) :: found   ! total fonud
        integer, intent(inout), allocatable, dimension(:) :: primes
                                        ! the array of primes
        integer :: msqr                 ! probably unnecessary
        logical, dimension(max) :: mask ! is prime boolan mask

        ! set the array of bools to the total number
        mask = .true.           ! and make it true!
        mask(1) = .false.       ! except of course

        ! get the floored square root
        msqr = INT(SQRT(REAL(max)))

        ! the seive, paralleled
        !!$OMP PARALLEL DO PRIVATE(j)
        do i = 2, msqr
            if (mask(i)) then
                j = i**2
                do while (j <= max)
                    mask(j) = .false.
                    j = j + i
                enddo
            endif
        enddo
        !!$OMP END PARALLEL DO

        ! total number of positives in the mask
        found = COUNT(mask)
        allocate(primes(found))             ! finally
        ! prime array is index of true mask elements:
        j = 0
        do i = 1, max
            if (mask(i)) then
                j = j + 1
                primes(j) = i
            end if
        end do

    end subroutine eratostheneses_sieve

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine is_prime(num, ans)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! a quick & dirty test for the above
        ! INPUT:
        ! num: we want to know if this is prime.
        ! OUTPUT: 
        ! ans: yes or no.
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        implicit none

        integer, intent(in) :: num      ! the question
        logical, intent(out) :: ans     ! the answer
        integer :: i                    ! a counter

        if (num < 2) then
            ans = .false.           ! all numbers less than two are not prime
        else if (num == 2) then
            ans = .true.
        else 
            do i = 2,(INT(SQRT(REAL(num)))) 
                if (MODULO(num,i).eq.0) then
                    ans = .false.
                    exit
                else 
                    ans = .true.
                end if
            end do
        end if

    end subroutine is_prime

end module utilities
