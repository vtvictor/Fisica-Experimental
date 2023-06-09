program IncertezasRelativas
    implicit none
    integer, parameter :: n = 5 ! número de medidas
    real*8 :: valores(n), media, desvio_padrao, incerteza, incerteza_relativa
    integer :: i
    
    ! valores medidos
    valores = (/1.1, 1.2, 1.3, 1.2, 1.1/)
    
    ! cálculo da média
    media = sum(valores) / n
    
    ! cálculo do desvio padrão
    desvio_padrao = sqrt(sum((valores - media)**2) / (n-1))
    
    ! cálculo da incerteza
    incerteza = desvio_padrao / sqrt(real(n))
    
    ! cálculo da incerteza relativa
    incerteza_relativa = incerteza / media * 100
    
    write(*,*) 'Valores medidos:'
    do i = 1, n
        write(*,'(F8.2)') valores(i)
    end do
    
    write(*,*)
    write(*,*) 'Média = ', media
    write(*,*) 'Desvio padrão = ', desvio_padrao
    write(*,*) 'Incerteza = ', incerteza
    write(*,*) 'Incerteza relativa (%) = ', incerteza_relativa
    
end program IncertezasRelativas