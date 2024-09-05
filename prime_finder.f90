program prime_finder

    ! import module for desired prime finder method
    use utilities

    implicit none

    integer :: found ! found is how many
    integer :: max ! max is upper limit
    integer, allocatable, dimension(:) :: primes ! array of primes
    integer :: i
    logical :: tests, answ

    max = 1000000000
    ! adapt so max is user input
   ! print *, "To what upper limit shall we count the primes?"
   ! read (*, *) max
    print *, max, "? OK."

    call eratostheneses_sieve(max, found, primes)

    print *, "Found: ", found
    
    ! tests
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
