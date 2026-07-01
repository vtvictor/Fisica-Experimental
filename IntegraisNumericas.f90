!=======================================================================
!  IntegraisNumericas.f90
!  Calcula a integral numérica de um conjunto de pontos (x,y) usando a
!  regra do trapézio.
!  Os dados são fornecidos diretamente no código (exemplo de função
!  y = x^2 + 1 entre x=0 e x=2).
!=======================================================================
program IntegraisNumericas
    implicit none
    integer, parameter :: n = 11          ! número de pontos
    real*8 :: x(n), y(n), dx, integral
    integer :: i

    ! definindo os pontos x (de 0.0 a 2.0 com passo 0.2)
    x = (/ (0.0d0 + (i-1)*0.2d0, i=1,n) /)
    ! y = x^2 + 1
    y = x**2 + 1.0d0

    ! passo h (assumindo espaçamento uniforme)
    dx = x(2) - x(1)

    ! regra do trapézio
    integral = (y(1) + y(n)) / 2.0d0
    do i = 2, n-1
        integral = integral + y(i)
    end do
    integral = integral * dx

    ! saída
    write(*,*) 'Pontos (x, y):'
    do i = 1, n
        write(*,'(F6.2,2X,F6.2)') x(i), y(i)
    end do
    write(*,*)
    write(*,'(A,F8.4)') 'Integral aproximada (trapézio) = ', integral
end program IntegraisNumericas
