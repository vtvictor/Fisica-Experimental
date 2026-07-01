!=======================================================================
!  OsciladorHarmonico.f90
!  Calcula o período e a frequência de um oscilador harmônico simples
!  massa-mola: T = 2π√(m/k), f = 1/T.
!  Também pode calcular a posição como função do tempo para movimento
!  harmônico simples (opcional).
!=======================================================================
program OsciladorHarmonico
    implicit none
    real*8 :: m, k, T, f, pi, t, x, A, phi
    integer :: i, n
    real*8, parameter :: pi = 3.141592653589793d0

    write(*,*) 'Massa (kg): '
    read(*,*) m
    write(*,*) 'Constante elástica k (N/m): '
    read(*,*) k

    T = 2.0d0 * pi * sqrt(m/k)
    f = 1.0d0 / T

    write(*,*)
    write(*,'(A,F6.3)') 'Massa = ', m, ' kg'
    write(*,'(A,F6.3)') 'Constante elástica = ', k, ' N/m'
    write(*,*)
    write(*,'(A,F6.3)') 'Período T = ', T, ' s'
    write(*,'(A,F6.3)') 'Frequência f = ', f, ' Hz'

    ! opcional: mostrar posição em alguns instantes
    write(*,*)
    write(*,*) 'Posição x(t) = A*cos(2π f t + φ) (supondo A=1.0 m, φ=0)'
    write(*,*) 't (s)        x (m)'
    write(*,*) '--------------------'
    do i = 0, 5
        t = i*0.5
        x = cos(2.0d0*pi*f*t)  ;! A=1, phi=0
        write(*,'(F6.2,2X,F8.4)') t, x
    end do
end program OsciladorHarmonico
