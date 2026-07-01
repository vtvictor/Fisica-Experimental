!=======================================================================
!  IncertezasRelativas.f90
!  Calcula a média, desvio padrão, incerteza e incerteza relativa
!  de um conjunto de medições.
!=======================================================================
program IncertezasRelativas
    implicit none
    integer, parameter :: n = 5          ! número de medições
    real*8 :: valores(n), media, desvio_padrao, incerteza, incerteza_relativa
    integer :: i

    ! valores medidos (exemplo)
    valores = (/1.1, 1.2, 1.3, 1.2, 1.1/)

    ! média
    media = sum(valores) / real(n)

    ! desvio padrão amostral
    desvio_padrao = sqrt( sum((valores - media)**2) / real(n-1) )

    ! incerteza da média (erro padrão)
    incerteza = desvio_padrao / sqrt(real(n))

    ! incerteza relativa em percentual
    incerteza_relativa = (incerteza / media) * 100.0d0

    ! saída formatada
    write(*,*) 'Valores medidos:'
    do i = 1, n
        write(*,'(F6.2)') valores(i)
    end do
    write(*,*)
    write(*,'(A,F8.4)') 'Média = ', media
    write(*,'(A,F8.4)') 'Desvio padrão = ', desvio_padrao
    write(*,'(A,F8.4)') 'Incerteza da média = ', incerteza
    write(*,'(A,F6.2)') 'Incerteza relativa (%) = ', incerteza_relativa
end program IncertezasRelativas
