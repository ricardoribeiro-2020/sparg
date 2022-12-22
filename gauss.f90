MODULE gaussiana

  USE DFPORT    !Sera necessario comentar esta linha quando nao se estiver a usar 
                ! o fortran da DIGITAL/COMPAQ/HP
!  IMPLICIT NONE

  CONTAINS

    FUNCTION gauss(valor1, valor2, media, desvio, opcao, semente)
!************************************************************************
!************************************************************************
!*                                                                     **
!*                         GAUSS.F90                                   **
!*              ===============================                        **
!*                                                                     **
!*        Funcao de determinacao de uma distribuicao gaussiana         **
!*                                                                     **
!*                                                                     **
!*                                                                     **
!*              Ricardo Mendes Ribeiro              Marco 2003         **
!*                                                                     **
!************************************************************************
!************************************************************************
!
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   %               Explicacao detalhada do programa             %
!   % Este programa esta pensado para funcionar como uma funcao  %
!   % que retorna um valor ou conjunto de valores inteiros entre %
!   % 1 e _valor_ com uma frequencia que obedece a uma           %
!   % distribuicao estatistica gaussiana, centrada na _media_    %
!   % e com uma variancia _variancia_.                           %
!   %                                                            %
!   % f(x)=1/(vari.*sqrt(2pi)) * exp[-(x-media)**2 /2*vari.**2]  %
!   %                                                            %
!   % Funciona com variaveis proprias independentes do programa  %
!   % que o chama.                                               %
!   %                                                            %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! valor1, valor2 - valor maximo e minimo da distribuicao
!                  integer*4: podem tomar qualquer valor positivo ou negativo,
!                  mas valor1 < valor2.
! media, desvio - media e desvio padrão da distribuicao
!                  real*4: podem tomar qualquer valor mas desvio >0.
! opcao - opcoes a enviar para a funcao
!                  integer*1: nao e' usada para ja.
! semente - valor de inicializacao do random generator
!                  integer*4: se for 0 usa o valor seguinte do random number
!                  generator RAND; se for >0 da' sempre o mesmo resultado
!                  nao pode ser <0.
! gauss - valor que retorna a funcao
!                  integer*4: em caso de nao dar um valor dentro da distribuicao
!                  retorna valor1-1

! constantes fisicas e matematicas
    REAL(KIND=8), PARAMETER :: pi=3.14159266
! variaveis mudas
    INTEGER(KIND=4) :: i

! variaveis de input e output
    INTEGER(KIND=4) :: valor1, valor2
    REAL(KIND=4) :: media, desvio
    INTEGER(KIND=4) :: semente
    INTEGER(KIND=4) :: opcao
    INTENT(IN) :: valor1, valor2, media, desvio, opcao, semente
    INTEGER(KIND=4) :: gauss

! distribuicao e' colocada nestes arrays
    REAL(KIND=4) :: F(valor1-1:valor2)
    REAL(KIND=4) :: distrib(valor1-1:valor2)

! variaveis auxiliares
    REAL(KIND=4) :: vari, tiro

! validacao das variaveis de input
    IF (desvio <= 0) THEN
      WRITE(*,*)' Chamada invalida da funcao gauss.'
      WRITE(*,*)' Desvio padrao <= 0.'
      STOP
    ELSEIF (semente < 0) THEN
      WRITE(*,*)' Chamada invalida da funcao gauss.'
      WRITE(*,*)' Seed < 0.'
      STOP
    ELSEIF (valor1 > valor2) THEN
      WRITE(*,*)' Chamada invalida da funcao gauss.'
      WRITE(*,*)' valor1 > valor2.'
      STOP
    ENDIF


!!!!!!!!!!!!!! INICIO DO CALCULO DA FUNCAO !!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Distribuicao normal
    vari = 1/desvio/SQRT(2*pi)
    F(valor1-1) = 0

    DO i=valor1, valor2    !determinacao da distribuicao
      F(i) = F(i-1) + vari*EXP(-((REAL(i) - media)**2)/2/desvio**2)
      distrib(i) = F(i) - F(i-1)
    ENDDO

    tiro = RAND(semente)*F(valor2)    !tira a sorte um ponto da distribuicao

    DO i=valor1,valor2
      IF (tiro >= F(i-1) .AND. tiro < F(i)) THEN
        gauss=i
        EXIT
      ENDIF
      gauss = valor1 - 1    !em caso de nao acertar na distribuicao
    ENDDO                   !da este valor a assinalar o erro

    END FUNCTION

END MODULE



! MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
!
!program teste

!use gaussiana

!integer(kind=4) :: x, i
!real y

!open(file='gauss.txt',unit=1)

!call random_seed
!call random_number(y)
!write(*,*) y

!do i=1,50000
!x = gauss(-100,100,50.,20.,1,0)


!write(1,*)i,x
!enddo

!end
