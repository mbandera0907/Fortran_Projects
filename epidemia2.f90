!En este codigo mira la expansion de un virus, en una red NxN de individuos, donde primero observamos el nivel de afectacion
!que tienen los primeros y segundos vecidos a un individuo (i,j). 

program epidemia2
    implicit none

    integer, parameter :: N = 200 ! raiz cuadrada del numero total de individuos
    integer, parameter :: M = 250 ! numeros de dias
    integer :: i, j, k, nt, k2, nti, l, ntm, Total, Total_Pob, nti2, nte
    real :: x, nivel
    integer, dimension(N,N) :: pob, pob_aux, inm, inm_aux, enf, enf_aux, dias, muertos, muertos_aux !poblacion, inmunes, enfermos
    integer, dimension(0:N+1) :: front !frontera

    open(1, file = "expansion.dat")
    open(2, file = "enfermos.dat")
    open(3, file = "inmunes.dat")
    open(4, file = "muertos.dat")

    x = 0.
    pob(:,:) = 0
    inm(:,:) = 0 
    enf(:,:) = 0
    dias(:,:)= 0
    muertos(:,:) = 0
    
    i = N/2

    pob(i,i) = 1

    !Vamos a definir las  un elemento que ayuda con las condiciones de frontera periodicas
    front(0) = N 
    do i = 1, N+1
        front(i) = i 
    enddo
    
    front(N+1) = 1

    call random_seed

 !   print *, "     ", "    Dias ", " Contagiados ", "   Inmunes ", "    Muertos"!, "   Inmunes 2da etapa "

    do l = 1, M

     !do while (l < M)
        pob_aux(:,:) = pob(:,:) !poblacion enferma
        inm_aux(:,:) = inm(:,:) !inmunes
        muertos_aux(:,:) = muertos(:,:)
        !enf_aux(:,:) = enf(:,:) !enfermos

        do i = 1, N
            do j = 1, N

                if(pob_aux(i, j) == 0 .and. inm_aux(i,j) == 0 .and. muertos_aux(i,j) == 0) then
                    
                    !k es el numero de enfermos alrededor (primeros vecinos)  
                    k = pob_aux(front(i+1), front(j)) + pob_aux(front(i-1), front(j)) &
                    + pob_aux(front(i), front(j+1)) + pob_aux(front(i), front(j-1)) 


                    k2 = pob_aux(front(i+1), front(j-1)) + pob_aux(front(i-1), front(j-1)) &
                    + pob_aux(front(i+1), front(j+1)) + pob_aux(front(i-1), front(j+1))

                    call random_number(x)

                    nivel = 0.3*k + 0.15*k2

                    if(x < nivel) then

                        call random_number(x)

                        if(x < 0.1) then
                            inm(i,j) = 1
                        else
                            pob(i,j) = 1
                            dias(i,j) = dias(i,j) + 1
                        endif

                    endif

                !endif

                !******************************************************
                !*******************2da etapa**************************
                !******************************************************
                !cuenta los dias de enfermedad del individuo (i,j)
                else
                    dias(i,j) = dias(i,j) + 1
                  !  if(pob(i,j) == 1 .and. inm(i,j) == 0. .and. muertos(i,j) == 0) then
                   !     dias(i,j) = dias(i,j) + 1

                        if(pob_aux(i,j) == 1 .and. dias(i,j) == 5 .and. muertos_aux(i,j) == 0) then
                            call random_number(x)

                            if(x < 0.9) then
                                inm(i,j) = 1
                               ! muertos(i,j) = 1
                                pob(i,j) = 0
                            else
                                muertos(i,j) = 1
                                !inm(i,j) = 1
                                pob(i,j) = 0
                            endif
                        endif
                 !   endif
                endif

                !*******************************************************
                !*******************************************************
                !*******************************************************

            enddo
        enddo

        ntm = sum(muertos) !Total de muertos
        nt = sum(pob) !- ntm !Total de enfermos vivos
        nti = sum(inm) !- sum(muertos) !restamos el numero de muertos, para evitar que entren en los inmunes. 1ra etapa
        nte = sum(enf) !Total de enefermos
      !  nti2 = sum(inm2) - sum(muertos) !total de individuos inmunes despues de los 5 dias
!        Total_Pob = N**2
!        Total = nti + ntm

     !   l = l + 1

        write(1,*) l, nt, nti, ntm, nte
!        print *, "~~~~~~~Datos~~~~~~"
!        print *, "     ", "    Dias ", " Contagiados ", "   Inmunes ", "    Muertos "!, " Total de enfermos"!, "   Inmunes 2da etapa "
        print *, l, nt, nti, ntm!, nte!, nti2
!        print *, "::::::::::::::::::::::::::::::::"
!        print *, "************** Dias ************"
!        print *, dias
!        print *, "::::::::::::::::::::::::::::::::"
!        print *, "::::::::::::::::::::::::::::::::"
!        print *, "*************Inmunes************"
!        print *, inm
!        print *, "::::::::::::::::::::::::::::::::"
!        print *, "::::::::::::::::::::::::::::::::"
!        print *, "**************Muertos***********"
!        print *, muertos !dias
!        print *, "::::::::::::::::::::::::::::::::"
!        print *, ""
!        print *, pob
!        print *, "pob_aux"
!        print *, pob_aux
    enddo
!    print *, "Total = ", Total
!    print *, "Total_Pob = ", Total_Pob
  !  print *, "En ", l, " dias se han contaguiado ", nt, " individuos, ", "un total de ", nti + nti2, &
  !  " individuos inmunes y ", ntm, " muertos." 

    do i = 1, N
        do j = 1, N

            write(2, 999) i, j, pob(i,j)
            write(3, 999) i, j, inm(i,j)
            write(4, 999) i, j, muertos(i,j)
        enddo

        write(2,*); write(3,*); write(4,*)
        
    enddo


    close(1)
    close(2)
    close(3)
    close(4)

    999 format(3(I8,1x))
   
end program epidemia2


!--------------------Presentado por: Mauricio Bandera------------------!
