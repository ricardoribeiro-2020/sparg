
MODULE crescer

  USE DFPORT    !Sera necessario comentar esta linha quando nao se estiver a usar 
                ! o fortran da DIGITAL/COMPAQ/HP

  USE comum
  ! precisa das variaveis comuns:
  ! - comprimento: comprimento dos monomeros
  ! - raio: distancia minima entre monomeros
  ! - array,tamarray: array com os monomeros existentes e seu tamanho

  USE encaixar

!  IMPLICIT NONE

  CONTAINS

    FUNCTION cresce(x,y,z,alf,bet,gam,taman,sucesso)
!************************************************************************
!************************************************************************
!*                                                                     **
!*                       CRESCE.F90                                    **
!*              ===============================                        **
!*                                                                     **
!*        Funcao que acrescenta um monomero a uma cadeia               **
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
!   % Esta funcao tentar crescer uma cadeia a partir de um ponto %
!   % inicial e dada uma direccao, ate um tamanho pre-definido.  %
!   %                                                            %
!   % Usa os dados de um modulo de variaveis comuns que contem   %
!   % o array de pontos existentes, o tamanho dos monomeros, etc.%
!   %                                                            %
!   % Nao verifica se o ponto original encaixa.                  %
!   %                                                            %
!   %                                                            %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! x,y,z - coordenadas do ponto de partida da cadeia
!           real*4
! alf,bet,gam - angulos que dao a direccao da cadeia
!           real*4
! taman - numero de monomeros previstos para esta cadeia
!           integer*4: nao pode ser >100
! sucesso - flag que indica se conseguiu preencher a cadeia completamente
!           logical
!
! cresce(N,3) - array que contem as coordenadas dos pontos que encaixou
!           real*4: N e' igual a taman

! variaveis mudas
    INTEGER(KIND=4) :: i,j
    REAL(KIND=8), PARAMETER :: pi=3.141592654

! variaveis de input e output
    REAL(KIND=4) :: x,y,z,alf,bet,gam
    INTEGER(KIND=4) :: taman 
!    INTENT(IN) :: x,y,z,alf,bet,gam,taman
    LOGICAL ::sucesso

    REAL(KIND=4), DIMENSION(1:taman+1,3) :: cresce

! variaveis locais
    REAL(KIND=4),DIMENSION(-taman:taman, 3) :: ponto
    INTEGER(KIND=1) :: tam    !taman que tem a cadeia neste momento
    INTEGER(KIND=1) :: tami    !taman que tem a cadeia na direccao i
    INTEGER(KIND=1) :: tamj    !tamanho que tem a cadeia na direccao j
    INTEGER(KIND=1) :: lado    !determina para que lado vai começar a crescer (+1/-1)
!    REAL(KIND=4) :: al, ga

! validacao das variaveis de input
    IF (taman < 0 .OR. taman > 100) THEN
      WRITE(*,*)' Chamada invalida da funcao cresce.'
      WRITE(*,*)' tamanho < 0 ou tamanho > 100'
      WRITE(*,*)' x,y,z ',x,y,z
      WRITE(*,*)' alf,bet,gam ',alf,bet,gam
      WRITE(*,*)' taman,sucesso ',taman,sucesso
      WRITE(*,*)' Nmon, Ncad ',Nmon, Ncad
      WRITE(*,*)' numeromon, numerocad ',numeromon, numerocad

      STOP
    ENDIF

!!!!!!!!!!!!!! INICIO DO CALCULO DA FUNCAO !!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ponto = -100    !inicializacao do array para um valor impossivel
    cresce = -100
    sucesso = .TRUE.

    ponto(0,1) = x    !esta e' a posicao do inicio da cadeia, portanto a coordenada
    ponto(0,2) = y    ! zero do primeiro monomero; agora pode crescer para um lado
    ponto(0,3) = z    ! ou para o outro

    tam = 0
    tami = 0
    tamj = 0
    lado = 2*INT(RAND(0) + 0.5) - 1
    IF(lado == -1) THEN
      alf = alf + pi
      gam = gam + pi
    ELSE
      alf = alf
      gam = gam
    ENDIF

    DO i=1,taman    !tenta crescer para um lado; cada ponto a mais e' um monomero a mais
      ponto(i,1) = ponto(i-1,1) + comprimento*SIN(alf)*COS(gam)
      ponto(i,2) = ponto(i-1,2) + comprimento*SIN(alf)*SIN(gam)
      ponto(i,3) = ponto(i-1,3) + comprimento*COS(alf)
      IF (ponto(i,3) >= dimz .OR. ponto(i,3) <= 0 .OR. &
          .NOT.encaixa(ponto(i,:),raio,array,tamarray)) THEN
        EXIT
      ENDIF
      tam = tam +1
      tami = tami +1
    ENDDO

    DO j=1,taman-tam
      ponto(-j,1) = ponto(-j+1,1) - comprimento*SIN(alf)*COS(gam)
      ponto(-j,2) = ponto(-j+1,2) - comprimento*SIN(alf)*SIN(gam)
      ponto(-j,3) = ponto(-j+1,3) - comprimento*COS(alf)
      IF (ponto(-j,3) >= dimz .OR. ponto(-j,3) <= 0 .OR. &
          .NOT.encaixa(ponto(-j,:),raio,array,tamarray)) THEN
        sucesso = .FALSE.
        EXIT
      ENDIF
      tam = tam +1
      tamj = tamj +1
    ENDDO 

    IF (sucesso) THEN
      cresce(1:tam,:) = ponto(-tamj:tami-1,:)    !passa os valores para a funcao
    ELSE
      cresce(1:tam,:) = -100    !sinalizacao do erro (falha em encaixar)
    ENDIF

    END FUNCTION

END MODULE

