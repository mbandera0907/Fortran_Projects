!En este codigo obtenemos las soluciones reales enteras de la ecuacion cuadratica para una terna de numeros,
!haciendo un barrido sobre los primeros 800 numeros primos.

program ec_cuadratica

    implicit none

    integer, parameter :: N = 800
    integer, dimension(N) :: primos
    integer :: i, j, k, N1, N2
    real :: a, b, c, x1, x2, det

    !Llamamos a la subrutina que nos generan los primeros 800 numeros primos.
    call numeros_primos(N, primos)

    !Generamos las permutaciones de la terna de numeros a, b, c.
    do i = 1, N
        a = 1.0*primos(i)
        do j = 1, N
            b = 1.0*primos(j)
            do k = 1, N
                c = 1.0*primos(k)
                
                !!-----------------------------------!!
                !!solucionamos la ecuacion cuadratica!!
                !!-----------------------------------!!

                if(a /= 0.0)then !Es decir, no se indetermina la solucion.
                    
                    det = sqrt(b**2 - 4.0*a*c)

                    if(det >= 0.)then !Las soluciones son reales.
                        
                        x1 = (-b + det)/(2.*a) !Solucion 1
                        x2 = (-b - det)/(2.*a) !Solucion 2
                        
                        !Solo vamos a imprimir las soluciones reales enteras.
                        N1 = x1; N2 = x2 !Donde N1 y N2 son variables enteras.
                        if(abs(x1 - N1) == 0 .and. abs(x2 - N2) == 0)then 
                            
                            print *, "-----------------------------------------------------------------------------------"
                            print *, "Terna de numeros primos    ", "a:", a, "b:", b, "c:", c
                            print *, "Sol_1:", x1
                            print *, "Sol_2:", x2
                            print *, "-----------------------------------------------------------------------------------"

                        endif
                    else
                        x1 = 0.0; x2 = 0.0
                    endif

                else
                    print *, "La variable de A es igual a 0"
                endif

            enddo
        enddo
    enddo

endprogram ec_cuadratica

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Subrutina que genera numeros primos!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine numeros_primos(N, primos)

    implicit none

    integer, intent(in) :: N 
    integer :: i, j, k, l
    integer, dimension(N), intent(out) :: primos

    j = 1; primos(j) = 1
    j = 2; primos(j) = 2
    k = 3

    do while(j <= N)
        l = 1
        do i = 2, j
            if(mod(k, primos(i)) == 0) then
                l = 0
                exit
            endif
        enddo

        if(l == 1) then
            j = j + 1
            primos(j) = k
        endif

        k = k + 1
    enddo

end subroutine


!--------------------Presentado por: Mauricio Bandera------------------!