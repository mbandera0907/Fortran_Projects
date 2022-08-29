!
!En este codigo mostramos la convergencia de la ecuación de Laplace en tres dimensiones al solucionarla de manera numerica.
!
program laplace3d
    implicit none

    integer, parameter :: N = 20
    real*8, parameter :: pi = 3.141592653589793d0
    real*8, dimension(N,N,N) :: phi, phi_aux
    real*8 :: x
    integer :: i, j, k, ii

    phi(:,:,:) = 0.d0
    
    !Definimos nuestras condiciones de contorno.
    do i = 1,N
        do j = 1,N
            do k = 1, N
                phi(i,j,1) = sin((i-1)*pi/dble(N-1)) * sin((j-1)*pi/dble(N-1))
            enddo
        enddo
    enddo
    
    phi(:,:,N) = phi(:,:,1)
    phi(:,1,:) = phi(:,:,1)
    phi(:,N,:) = phi(:,:,1)
    phi(1,:,:) = phi(:,:,1)
    phi(N,:,:) = phi(:,:,1)

    !**********************!

    x = 1.d0; ii = 0
    do while(x > 1.d-8)

        phi_aux(:,:,:) = phi(:,:,:)

        do i = 2, N-1
            do j = 2, N-1
                do k=2, N-1
phi(i,j,k) = (phi_aux(i+1,j,k) + phi_aux(i-1,j,k) + phi_aux(i,j+1,k) + phi_aux(i,j-1,k) + phi_aux(i,j,k+1) + phi_aux(i,j,k-1))/6.D0
                enddo
            enddo
        enddo

        x = 0.d0

        x = sum(abs(phi(:,:,:) - phi_aux(:,:,:)))
        ii = ii + 1   ! ii es un contador
        print *,  ii, x
    enddo


end program laplace3d


!--------------------Presentado por: Mauricio Bandera------------------!
