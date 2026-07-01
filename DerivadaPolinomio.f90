!=======================================================================
!  DerivadaPolinomio.f90
!  Calcula os coeficientes da derivada de um polinômio.
!  Dado um polinômio P(x) = a_n x^n + a_{n-1} x^{n-1} + ... + a_0,
!  seus coeficientes da derivada são:
!      P'(x) = n*a_n x^{n-1} + (n-1)*a_{n-1} x^{n-2} + ... + 1*a_1.
!  O programa lê o grau n e os coeficientes a_n ... a_0 e imprime
!  os coeficientes da derivada.
!=======================================================================
program DerivadaPolinomio
    implicit none
    integer :: i, n
    real*8 :: polin(0:11), deriv(0:11)   ! índices 0..11 (máximo grau 11)
    integer, parameter :: TAM = 12

    write(*,*) 'Grau do polinômio (máximo 11): '
    read(*,*) n
    if (n < 0 .or. n >= TAM) then
        write(*,*) 'Grau inválido!'
        stop
    end if

    write(*,*) 'Digite os coeficientes a_n ... a_0:'
    do i = n, 0, -1
        read(*,*) polin(i)
    end do

    ! derivada: d/dx (a_i x^i) = i * a_i x^{i-1}
    do i = n, 1, -1
        deriv(i-1) = real(i) * polin(i)
    end do
    ! o termo constante desaparece
    deriv(n) = 0.0d0   ! não usado

    write(*,*)
    write(*,*) 'Coeficientes da derivada (do maior ao menor grau):'
    do i = n-1, 0, -1
        write(*,'(F8.4)',advance='no') deriv(i)
    end do
    write(*,*)
end program DerivadaPolinomio
