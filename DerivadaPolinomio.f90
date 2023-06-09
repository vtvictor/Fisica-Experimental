program DerivadaPolinomio
    implicit none
    integer :: i, n
    real*8 :: polin(TAM), deriv(TAM)
    parameter(TAM=12)
    write(*,*) 'Grau do polinomio: '
    read(*,*) n
    if (n < TAM) then
        write(*,*) 'Coeficientes do polinomio: '
        do i = n, 0, -1
            read(*,*) polin(i)
            write(*,'(F7.3)',advance='no') polin(i)
        end do
        write(*,*)
        write(*,*) 'Coeficientes da derivada do polinomio original: '
        do i = n, 1, -1
            deriv(i - 1) = polin(i) * i
            write(*,'(F7.3)',advance='no') deriv(i - 1)
        end do
        write(*,*)
    else
        write(*,*) 'Grau do polinomio muito elevado!'
    end if
end program DerivadaPolinomio
