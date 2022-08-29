!Resolvemos la ecucion de Laplace 1D

program laplace
    implicit none

    integer, parameter :: N = 100
    real*8, parameter :: pi = 3.141592653589793D0
    real*8, dimension(N,N) :: phi, phi_aux
    integer :: i, j, k
    real*8 :: x, t1, t2

    phi(:,:) = 0.D0

    !Definimos las condiciones de frontera
    do i = 1, N 
        phi(i, 1) = sin((i-1)*pi/dble(N-1))
    enddo

    phi(:,N) = phi(:,1)
    phi(1,:) = phi(:,1)
    phi(N,:) = phi(:,1)

    x = 1.D0

    do i = 1, N
        do j = 1, N
            print *, i,j, phi(i,j)
        enddo
        print *
    enddo



!call cpu_time(t1) !tiempo del computador en segundos
!call cpu_time(t2)
!print *, t2-t1
!forall(i=2:N-1, j=2:N-1)
!phi(i,j)= (phi(i+1, j) + phi(i-1, j) + phi(i, j+1) + phi(i, j-1))/4d0
!endforall

end program laplace
