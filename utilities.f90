module utilities
 
    contains 

    subroutine eratostheneses_sieve(max, found, primes)
        ! INPUT:
        ! max: an arbitrary number 
        ! OUTPUT:
        ! found: the number of primes less than max
        ! primes: an array of the found primes  
        ! INTERNAL: 
        ! mask: an array of logicals representing whether prime or not 
        
        implicit none

        integer :: i, j
        integer, intent(in) :: max
        integer, intent(out) :: found
        integer, intent(inout), allocatable, dimension(:) :: primes
        integer :: msqr
        logical, dimension(max) :: mask

        ! set the array of bools to the total number
        mask = .true.
        ! and make it true!
        mask(1) = .false. ! except of course

        ! get the floored square root
        msqr = INT(SQRT(REAL(max)))

        !$OMP PARALLEL DO PRIVATE(j)
        do i = 2, msqr
            if (mask(i)) then
                j = i**2
                do while (j <= max)
                    mask(j) = .false.
                    j = j + i
                enddo
            endif
        enddo
        !$OMP END PARALLEL DO

        found = COUNT(mask)
        allocate(primes(found))

        j = 0
        do i = 1, max
            if (mask(i)) then
                j = j + 1
                primes(j) = i
            end if
        end do

    end subroutine eratostheneses_sieve

    subroutine is_prime(num, ans)

        implicit none

        integer, intent(in) :: num
        logical, intent(out) :: ans
        integer :: i

        if (num < 2) then
            ans = .false.
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
