!=======================================================================
!  Projetil.f90
!  Calcula o alcance máximo, altura máxima e tempo de voo de um
!  projétil lançado com velocidade v0 e ângulo theta (em graus)
!  em relação à horizontal, assumindo ausência de resistência do ar
!  e aceleração da gravidade g = 9.81 m/s².
!  test
!=======================================================================
program Projetil
    implicit none
    real*8 :: v0, theta, g, rad, t_voo, h_max, alcance
    real*8, parameter :: pi = 3.141592653589793d0
    real*8, parameter :: g = 9.81d0

    write(*,*) 'Digite a velocidade inicial (m/s): '
    read(*,*) v0
    write(*,*) 'Digite o ângulo de lançamento (graus): '
    read(*,*) theta

    ! converter graus para radianos
    rad = theta * pi / 180.0d0

    t_voo = 2.0d0 * v0 * sin(rad) / g
    h_max = (v0*sin(rad))**2 / (2.0d0 * g)
    alcance = v0**2 * sin(2.0d0*rad) / g

    write(*,*)
    write(*,'(A,F6.2)') 'Velocidade inicial = ', v0, ' m/s'
    write(*,'(A,F6.2)') 'Ângulo de lançamento = ', theta, '°'
    write(*,*)
    write(*,'(A,F6.2)') 'Tempo de voo = ', t_voo, ' s'
    write(*,'(A,F6.2)') 'Altura máxima = ', h_max, ' m'
    write(*,'(A,F6.2)') 'Alcance horizontal = ', alcance, ' m'
end program Projetil
