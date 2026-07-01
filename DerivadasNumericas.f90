!=======================================================================
!  DerivadasNumericas.f90
!  Exemplo de cálculo numérico da derivada de uma função usando a
!  fórmula de diferenças centrais:
!      f'(x) ≈ [ f(x+h) - f(x-h) ] / (2h)
!  Neste programa usamos f(x) = x^3 - 2x^2 + 4x - 8 e avaliamos em x=2.
!=======================================================================
program DerivadasNumericas
    implicit none
    real*8 :: x, h, f_x_plus_h, f_x_minus_h, derivada
    real*8 :: f

    external f

    ! ponto e passo
    x = 2.0d0
    h = 1.0d-4

    f_x_plus_h = f(x + h)
    f_x_minus_h = f(x - h)
    derivada = (f_x_plus_h - f_x_minus_h) / (2.0d0 * h)

    write(*,'(A,F8.4)') 'Valor da derivada em x = ', x
    write(*,'(A,F10.6)') 'Derivada aproximada (centrais) = ', derivada
    write(*,'(A,F10.6)') 'Valor exato (3x^2 -4x +4) = ', 3.0d0*x**2 - 4.0d0*x + 4.0d0
end program DerivadasNumericas

! função f(x) = x^3 - 2x^2 + 4x - 8
real*8 function f(xx)
    implicit none
    real*8 :: xx
    f = xx**3 - 2.0d0*xx**2 + 4.0d0*xx - 8.0d0
end function f
