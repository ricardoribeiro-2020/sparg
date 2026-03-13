MODULE prints

  USE comum

!  IMPLICIT NONE

  CONTAINS


    SUBROUTINE printt
!************************************************************************
!************************************************************************
!*                                                                     **
!*                       PRINTT.F90                                    **
!*              ===============================                        **
!*                                                                     **
!*        Subrotina para lidar com a impressao dos resultados          **
!*                                                                     **
!*                                                                     **
!*              Ricardo Mendes Ribeiro              Marco 2003         **
!*                                                                     **
!************************************************************************
!************************************************************************
!
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   %               Explicacao detalhada do programa             %
!   % Esta subrotina contem as instrucoes de gravacao dos re-    %
!   % sultados obtidos em ficheiros externos onde posteriormente %
!   % podem ser utilizados por outrous programas                 %
!   %                                                            %
!   %                                                            %
!   % Funciona com variaveis comuns, que sao as que imprime      %
!   %                                                            %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!

    USE comum

! variaveis mudas
    INTEGER(KIND=4) :: i, j
    INTEGER(KIND=4),DIMENSION(1:tam_max_cad) :: a
    CHARACTER(LEN=40) :: direcadeias, diremonomeros

    direcadeias = TRIM(director(contavezes))//'cadeias.txt'
    diremonomeros = TRIM(director(contavezes))//'monomeros.txt'

!!! Escrita do ficheiro das cadeias
    OPEN (UNIT=4, FILE=direcadeias)

    WRITE(UNIT=4,FMT=*) Ncad
    DO i=1,Ncad
      a = 0
      a(1)=cadeia(i,1)
      DO j=2,ncadeia(i)
        a(j) = a(j-1) + 1
      ENDDO
      WRITE(UNIT=4,FMT=*) a(:)
    ENDDO

    DO i=1,Ncad
      WRITE(UNIT=4,FMT=*)ncadeia(i)
    ENDDO

    CLOSE(UNIT=4)

!!! Escrita do ficheiro dos monomeros
    OPEN (UNIT=5, FILE=diremonomeros)

    WRITE(UNIT=5,FMT=*)Nmon
    DO i=1,Nmon
      WRITE(UNIT=5,FMT='(i7,6f12.5)') i, pos(i,1), pos(i,2), pos(i,3), alfa(i), beta(i), gama(i)
    ENDDO

    CLOSE(UNIT=5)

!!! Finalizacao da escrita no ficheiro de corrida

    densidade = Nmon/volume*massamon*1000
    WRITE(UNIT=2,FMT=*)
    WRITE(UNIT=2,FMT=*)' Amostra numero: ',contavezes
    WRITE(UNIT=2,FMT=*)' Densidade obtida em g/cm3 '
    WRITE(UNIT=2,FMT='(1f8.5)')densidade(1:ndom)

    RETURN

    END SUBROUTINE

!************************************************************************
!************************************************************************
!*                                                                     **
!*                       PRINTF.F90                                    **
!*              ===============================                        **
!*                                                                     **
!*        Subrotina para lidar com a impressao dos resultados finais   **
!*                                                                     **
!*                                                                     **
!*              Ricardo Mendes Ribeiro              Maio  2003         **
!*                                                                     **
!************************************************************************
!************************************************************************

    SUBROUTINE printf


    USE comum

! variaveis mudas
    INTEGER(KIND=4) :: i, j, k

!!! Escrita das estatisticas finais

    WRITE(UNIT=2,FMT=*)'amostra, dominio, tamanho cadeia, numero de cadeias'
    DO k=1, vezes
      DO i=1,ndom
        DO j=2,tam_max_cad
          WRITE(UNIT=2,FMT='(4i8)') k,i,j,con_tam_cad(j,i,k)
        ENDDO
      ENDDO
    ENDDO
    WRITE(UNIT=2,FMT=*)
    WRITE(UNIT=2,FMT=*)SUM(con_tam_cad)
    WRITE(UNIT=2,FMT=*)

    WRITE(UNIT=2,FMT=*)'amostra, dominio, angulo cadeia (>=), numero de cadeias'
    DO k=1, vezes
      DO i=1,ndom
        DO j=0,18
          WRITE(UNIT=2,FMT='(4i8)') k,i,j*5,con_ang_cad(j,i,k)
        ENDDO
      ENDDO
    ENDDO
    WRITE(UNIT=2,FMT=*)
    WRITE(UNIT=2,FMT=*)SUM(con_ang_cad)
    WRITE(UNIT=2,FMT=*)

    WRITE(UNIT=2,FMT=*)'amostra, dominio, posicao cadeia (>=), numero de cadeias'
    DO k=1, vezes
      DO i=1,ndom
        DO j=0,dimz_inteiro-1
          WRITE(UNIT=2,FMT='(4i8)') k,i,j,con_pos_cad(j,i,k)
        ENDDO
      ENDDO
    ENDDO
    WRITE(UNIT=2,FMT=*)
    WRITE(UNIT=2,FMT=*)SUM(con_pos_cad)
    WRITE(UNIT=2,FMT=*)

    WRITE(UNIT=2,FMT=*)'amostra, posicao monomero (>=), numero de monomeros'
     DO k=1, vezes
      DO j=0,dimz_inteiro-1
        WRITE(UNIT=2,FMT='(3i8)') k,j,con_pos_mon(j,k)
      ENDDO
    ENDDO
    WRITE(UNIT=2,FMT=*)
    WRITE(UNIT=2,FMT=*)SUM(con_pos_mon)

    RETURN

    END SUBROUTINE


END MODULE


