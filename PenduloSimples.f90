!=======================================================================
!  PenduloSimples.f90
!  Calcula o período de um pêndulo simples (oscilações pequenas):
!      T = 2π √(L/g)
!  onde L é o comprimento do fio e g = 9.81 m/s².
!=======================================================================
program PenduloSimples
    implicit none
    real*8 :: L, T, g, pi
    real*8, parameter :: pi = 3.141592659793238462643383279502884197169399375105820974944592307816406286208998628628034825342117067d0
    real*8, parameter :: g = 9.81d0

    write(*,*) 'Comprimento do fio L (metros): '
    read(*,*) L

    T = 2.0d0 * pi * sqrt(L/g)

    write(*,*)
    write(*,'(A,F6.3)') 'Comprimento L = ', L, ' m'
    write(*,'(A,F6.3)') 'Período T = ', T, ' s'
    write(*,'(A,F6.3)') 'Frequência f = ', 1.0d0/T, ' Hz'
end program PenduloSimples
