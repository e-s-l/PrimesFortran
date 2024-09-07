program prime_finder
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Find all primes less than a given number
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    use utilities ! import module for desired prime finder method & tests

    implicit none

    integer, allocatable, dimension(:) :: primes    ! array of primes
    integer :: i                                    ! counter
    logical :: tests, answ                          ! do we test?
    integer, parameter :: i32 = selected_int_kind(32)
    integer(kind = i32) :: max                      ! max is upper limit
    integer(kind = i32) :: found                    ! found is how many



    !!!!!!!!!!!!!
    max = 10000000000
    ! NOTE: compilier freaks out at 10**10
    !!!!!!!!!!!!!
    ! adapt so max is user input
    ! print *, "To what upper limit shall we count the primes?"
    ! read (*, *) max
    ! print *, max, "? OK."
    !!!!!!!!!!!!!

    call eratostheneses_sieve(max, found)
    ! NOTE: this version does NOT return an array of primes
    ! so the test below cannot be implemeneted.


    !!!
    ! NOTE: formatting assumes numbers less than 10**13
    write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    write(*, fmt="(1x,a,i12,a,i12,a)") "Found: ", found, " primes under ", max, '.'
    write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    !!!

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
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
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program prime_finder
