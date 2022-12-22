MODULE encaixar

  IMPLICIT NONE

  CONTAINS

    FUNCTION encaixa(ponto,raio,array,tamanho_array)
!************************************************************************
!************************************************************************
!*                                                                     **
!*                       ENCAIXA.F90                                   **
!*              ===============================                        **
!*                                                                     **
!*        Funcao que verifica se um dado ponto 'encaixa' num           **
!*               conjunto de pontos pre-existentes                     **
!*                                                                     **
!*                                                                     **
!*              Ricardo Mendes Ribeiro              Marco 2003         **
!*                                                                     **
!************************************************************************
!************************************************************************
!
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   %               Explicacao detalhada do programa             %
!   % Esta funcao retorna verdadeiro ou falso conforme a distan  %
!   % cia entre o ponto dado e o conjunto de pontos do array     %
!   % estiver a uma distancia maior ou menor do que um dado valor%
!   % Determina se algum ponto do _array_ esta a uma distancia   %
!   % < _raio_. Se ha', _encaixa_ = .false.; se nao ha',         %
!   % _encaixa_ = .true.                                         %
!   %                                                            %
!   % Funciona com variaveis proprias independentes do programa  %
!   % que o chama.                                               %
!   %                                                            %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! array - array que contem os pontos pre-existentes
!         real*4: nao ha restricoes.
! ponto - coordenadas do ponto que se quer ver se encaixa
!         real*4: nao ha restricoes.
! raio - distancia minima permitida entre os pontos
!         real*4: nao ha restricoes (ate pode ser <0; usa o modulo).
! tamanho_array - numero de elementos do _array_
!         integer*4: tem de ser mesmo esse numero.

! variaveis mudas
    INTEGER(KIND=4) :: i

! variaveis de input e output
    INTEGER(KIND=4) :: tamanho_array
    REAL(KIND=4),DIMENSION(1:tamanho_array,1:3) :: array
    REAL(KIND=4),DIMENSION(3) :: ponto
    REAL(KIND=4) :: raio
    INTENT(IN) :: array, ponto, raio
    LOGICAL encaixa

! variaveis locais
    REAL(KIND=4),DIMENSION(1:tamanho_array) :: distancia2    !distancia ao quadrado dos 
                                                       !pontos do array ao ponto
    REAL(KIND=4) :: raio2    !distancia minima ao quadrado permitida entre os pontos

! validacao das variaveis de input
    IF (tamanho_array < 0) THEN
      WRITE(*,*)' Chamada invalida da funcao encaixa.'
      WRITE(*,*)' Tamanho do array < 0.'
      STOP
    ELSEIF (tamanho_array == 0) THEN
      encaixa = .TRUE.
      RETURN
    ENDIF

!!!!!!!!!!!!!! INICIO DO CALCULO DA FUNCAO !!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    raio2 = raio*raio

    distancia2(:) = (array(:,1) - ponto(1))*(array(:,1) - ponto(1)) + &
                 (array(:,2) - ponto(2))*(array(:,2) - ponto(2)) + &
                 (array(:,3) - ponto(3))*(array(:,3) - ponto(3))

    IF (MINVAL(distancia2) > raio2) THEN    !exclusivo
      encaixa = .TRUE.
    ELSE
      encaixa = .FALSE.
    ENDIF
    
    END FUNCTION

END MODULE


! MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
!
!program teste
!USE DFPORT
!use encaixar

!INTEGER,parameter :: tam=100, caixa=20
!real a(3,tam)
!real raio
!real ponto(3)
!logical testes

!do i=1,tam
!  a(1,i)=rand(0)*caixa
!  a(2,i)=rand(0)*caixa
!  a(3,i)=rand(0)*caixa
!enddo

!raio=1

!do i=1,100
!  ponto(1)=rand(0)*caixa
!  ponto(2)=rand(0)*caixa
!  ponto(3)=rand(0)*caixa
!write(*,*)i
!  testes=encaixa(ponto,raio,a,SIZE(a,DIM=2))

  !write(*,*)testes
!enddo

!end
