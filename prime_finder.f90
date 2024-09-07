program prime_finder
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Find all primes less than a given number
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    use utilities ! import module for desired prime finder method & tests

    implicit none

    integer :: found                                ! found is how many
    integer :: max                                  ! max is upper limit
    integer, allocatable, dimension(:) :: primes    ! array of primes
    integer :: i                                    ! counter
    logical :: tests, answ                          ! do we test?

    !!!!!!!!!!!!!
    max = 1000000000
    !!!!!!!!!!!!!
    ! adapt so max is user input
    ! print *, "To what upper limit shall we count the primes?"
    ! read (*, *) max
    ! print *, max, "? OK."

    call eratostheneses_sieve(max, found)

    print *, "Found: ", found
    
    !!!!!!!!
    ! tests
    !!!!!!!!
    tests = .false.
    if (tests) then
        do i=1,found
        !    print *, primes(i)
            call is_prime(primes(i), answ)
        !    print *, answ
            if (.not.answ) then 
                print *, 'shit'
            end if
        end do
    end if

end program prime_finder
