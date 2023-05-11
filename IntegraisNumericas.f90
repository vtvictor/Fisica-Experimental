program IntegraisNumericas
    implicit none
    integer, parameter :: n = 20 ! número de pontos
    real*8 :: x(n), y(n), dx, integral
    integer :: i
    
    ! valores medidos
    ! x com 0.2 aprox.
    ! y com 0.39 aprox.
    x = (/0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, &
         2.0, 2.2, 2.4, 2.6, 2.8, 3.0, 3.2, 3.4, 3.6, 3.8/)
    y = (/0.00, 0.39, 0.71, 1.03, 1.35, 1.67, 1.99, 2.31, 2.63, &
         2.95, 3.27, 3.59, 3.91, 4.23, 4.55, 4.87, 5.19, 5.51, &
         5.83, 6.15/)
    
    ! intervalo de variação de x
    dx = x(2) - x(1)
    
    ! cálculo da integral usando a regra do trapézio
    integral = 0.0
    do i = 2, n-1
        integral = integral + y(i)
    end do
    integral = integral + (y(1) + y(n)) / 2.0
    integral = integral * dx
    
    ! impressão do resultado
    write(*,*) 'Dados experimentais:'
    do i = 1, n
        write(*,'(F6.2,F8.2)') x(i), y(i)
    end do
    
    write(*,*)
    write(*,*) 'Integral:'
    write(*,'(F8.2)') integral
    
end program integrais
