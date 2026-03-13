    MODULE global
!************************************************************************
!************************************************************************
!*                                                                     **
!*                      MODULO  GLOBAL                                 **
!*              ===============================                        **
!*                                                                     **
!*               Modulo que define constantes a ser usadas             **
!*                      em programas em geral                          **
!*                                                                     **
!*             Versao 1                                                **
!*               Ricardo Mendes Ribeiro           Abril 2003           **
!*                                                                     **
!************************************************************************
!************************************************************************
!
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   %               Explicacao detalhada do programa             %
!   % Este modulo e' suposto ser adicionado a todos os programas %
!   % em Fortran 90, com o fim de ajudar a normalizar a notacao. %
!   % Define uma serie de constantes muito utilizadas em fisica. %
!   %                                                            %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!----- DESCRICAO DAS VARIAVEIS ------------------------------------------
! 
!
!
! 
!   INDICES MUDOS
!
! i, j, k, l, m, n, it, a, b, c 
! ii, jj, kk, iii, jjj, kkk
!
!   CONSTANTES
! 
! pi, pix2, sqrpi, pi180 - 2pi, sqrt(pi), pi/180
! h, h2p - constante de Plank e dividido por 2pi
! me, e - massa e carga do electrao
! cc - velocidade da luz
! cc2 - velocidade da luz ao quadrado
! eps0 - constante dielectrica no vazio
! miu0 - permeabilidade magnetica no vazio
! kb - constante de Boltzmann
! Av - numero de Avogadro

    IMPLICIT NONE

    INTEGER(KIND=4) niter, iti, itf, it

    REAL(KIND=8), PARAMETER :: pi=3.141592654, pix2=6.283185307
    REAL(KIND=8), PARAMETER :: sqrpi=1.772453851, pi180=0.017453293
    REAL(KIND=8), PARAMETER :: h=6.6260755d-34, h2p=1.05457266d-34
    REAL(KIND=8), PARAMETER :: me=9.1093897d-31, e=1.60217733d-19
    REAL(KIND=8), PARAMETER :: cc=299792458, cc2=8.987551787368d+016
    REAL(KIND=8), PARAMETER :: eps0=8.854187817d-12, miu0=12.566370614d-7
    REAL(KIND=8), PARAMETER :: kb=1.380658d-23, Av=6.0221367d23
      

END MODULE global
!
!************************************************************************
    MODULE comum
!************************************************************************
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   %          Modulo que define as variaveis do programa        %
!   %                                                            %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!----- DESCRICAO DAS VARIAVEIS ------------------------------------------
! 
! Nmon - numero total de monomeros
! Ncad - numero total de cadeias
! Nlig - numero de cadeias ligadas ao substrato = revestimento
! ndom - numero de dominios com caracteristicas proprias

! numeromon - parametro que define o numero maximo de monomeros que cabem nos arrays
! numerocad - parametro que define o numero maximo de cadeias que cabem nos arrays
! numerodom - parametro que define o numero maximo de dominios que cabem nos arrays

! pos(Nmon,3) - coordenadas x,y,z da posicao do monomero (nm)

! alfa(Nmon) - angulo que o eixo do monomero faz com o eixo dos zz (rad)
! beta(Nmon) - angulo que o plano do monomero faz com o eixo dos xx (rad)
! gama(Nmon) - angulo que a projeccao do eixo do monomero no plano xy faz com o eixo xx (rad)

! comprimento - tamanho do eixo do momomero
! massamon - massa do monomero em kg

! raio - distancia minima entre monomeros (nm)

! array - array com os pontos todos existentes, incluindo as 8 copias 'a volta
! tamarray - tamanho do array com os pontos existentes
! fronteira - array auxiliar para o calculo das condicoes fronteira

! cadeia(Ncad, 0:1) - array com a listagem das cadeias e do 1� monomero
!                          na posicao 0 indica o dominio a que pertence a cadeia
! ncadeia(Ncad) - numero de monomeros em cada cadeia
! ncadeiafim(Ncad) - numero de monomeros final previsto para cada cadeia

! ncompleta -  numero de cadeias completas

! dimx - dimensoes da amostra segundo x (nm)
! dimy - dimensoes da amostra segundo y (nm)
! dimz - dimensoes da amostra segundo z (nm)
! volume - volume da amostra em cm3
! dimz_inteiro - dimensao da amostra segundo z (nm) mas em variavel inteira

!!!!!! definicao dos dominios:  !!!!!!!
! opcdom1 - opcao: crescimento aleatorio=1; comeca palas maiores=0
! opcdom2 - opcao: todos ao mesmo tempo=0; primeiro as fronteiras=1; pela ordem de input=2
! opcdom3 - opcao: se dominios pre-definidos=1; se crescem em torno de cadeias=0
! ndom - numero de dominios com caracteristicas proprias
! domx0(nvol) - primeira coordenada do volume, em x (nm)
! domx1(nvol) - segunda coordenada do volume, em x (nm)
! domy0(nvol) - primeira coordenada do volume, em y (nm)
! domy1(nvol) - segunda coordenada do volume, em y (nm)
! domz0(nvol) - primeira coordenada do volume, em z (nm)
! domz1(nvol) - segunda coordenada do volume, em z (nm)
! medias(nvol) - media da distribuicao no dominio
! desvios(nvol) - desvio padrao da distribuicao no dominio
! tetaz(nvol), deltaz(nvol) - Angulo com eixo e variacao (+/-) (rad: input em graus e depois convertido)
! tetax(nvol), deltax(nvol) - Angulo com xx e variacao (+/-) (rad: input em graus e depois convertido)
! tetaxy(nvol), deltaxy(nvol) - Angulo com xx em xy e variacao (+/-) (rad: input em graus e depois convertido)
! tetaz0(nvol),tetaz1(nvol) - Intervalo de angulos para cada dominio
! tetax0(nvol),tetax1(nvol) - Intervalo de angulos para cada dominio
! tetaxy0(nvol),tetaxy1(nvol) - Intervalo de angulos para cada dominio
! densidade(nvol) - densidade em g/cm3 do dominio

! vol(nvol) - volume do dominio em nm3
! ncade(nvol) - numero de cadeias previsto em cada dominio

! previsto(nvol) - numero de monomeros previstos para o dominio
! preenchido(nvol) - numero de monomeros ja no dominio
! npreenchido - numero de dominios preenchidos

! vezes - numero de amostras que cria
! contavezes - contagem das amostras que criou
! contagem - conta o numero de tentativas de encaixar uma cadeia num dominio
! tenta_encaix - numero de tentativas de encaixar ate dar um aviso
! director - array com as directorias em que vai criar os ficheiros

!!!!!!!! variaveis estatisticas
!  con_tam_cad - conta os tamanhos das cadeias
!  con_ang_cad - conta os angulos das cadeias
!  con_pos_cad - conta as posicoes das cadeias
!  con_pos_mon - conta as posicoes dos monomeros

!!!!!!!
!
! fronteira - array auxiliar para o calculo das condicoes fronteira
! tentax, tentay, tentaz - tentativa de posicao inicial de cadeia
! tentaalfa, tentabeta, tentagama - tentativa de angulos iniciais da cadeia
! tamanho - numero de monomeros previsto para a cadeia que se esta a tratar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! VARIAVEIS QUE NAO PODEM SER COMUNS
!
! i, j, k, a, b, c, x, y, z
! alf, bet, gam
! tam, tami, tamj, taman, temp
! sucesso
! ponto
!
!!!!!!!!!!!!!!!!!!!!!!
! TIPOS DE VARIAVEIS
!
! kind=1 -128 to 127 
! kind=2 -32,768 to 32,767 
! kind=4 -2,147,483,648 to 2,147,483,647 
!
! kind=4 1.17549435E-38  to 1.17549435E-38 7 decimal digits
! kind=8 2.2250738585072013D-308 to 1.7976931348623158D308 15 decimal digits

    IMPLICIT NONE

    INTEGER(KIND=4), PARAMETER :: numerodom =10

    INTEGER(KIND=4) :: Nmon, Ncad, Nlig
 
    REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:) :: pos
    REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: alfa,beta,gama
    REAL(KIND=4) :: comprimento, massamon

    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:) :: cadeia
    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:) :: ncadeia, ncadeiafim
    INTEGER(KIND=4) :: ncompleta

    REAL(KIND=4) :: raio
    REAL(KIND=4) :: dimx, dimy, dimz, volume
    INTEGER(KIND=4) ::dimz_inteiro

    INTEGER(KIND=1) :: opcdom1, opcdom2, opcdom3
    INTEGER(KIND=2) :: ndom
    REAL(KIND=4),DIMENSION(numerodom) :: domx0, domy0, domz0, domx1, domy1, domz1
    REAL(KIND=4),DIMENSION(numerodom) :: medias, desvios
    REAL(KIND=4),DIMENSION(numerodom) :: tetaz, deltaz, tetax, deltax, tetaxy, deltaxy 
    REAL(KIND=4),DIMENSION(numerodom) :: densidade
    REAL(KIND=4),DIMENSION(numerodom) :: vol
    REAL(KIND=4),DIMENSION(numerodom) :: tetaz0,tetaz1,tetax0,tetax1,tetaxy0,tetaxy1
    INTEGER(KIND=4),DIMENSION(numerodom) :: previsto, preenchido
    INTEGER(KIND=4),DIMENSION(numerodom) :: ncade
    INTEGER(KIND=4) :: npreenchido
    INTEGER(KIND=4),DIMENSION(numerodom) :: contagem
    
    REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:) :: array
    INTEGER(KIND=4) :: tamarray
    REAL(KIND=4), DIMENSION(1:8,1:3) :: fronteira


    REAL(KIND=4) :: tentax, tentay, tentaz, tentaalfa, tentabeta, tentagama
    INTEGER(KIND=4) :: tamanho

    INTEGER(KIND=4) :: vezes, tenta_encaix, contavezes
    CHARACTER(LEN=10),ALLOCATABLE, DIMENSION(:) :: director

    INTEGER(KIND=4) :: tam_max_cad

    INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:,:,:) :: con_tam_cad
    INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:,:,:) :: con_ang_cad
    INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:,:,:) :: con_pos_cad
    INTEGER(KIND=4),ALLOCATABLE,DIMENSION(:,:) :: con_pos_mon


  CONTAINS
!
!************************************************************************
     SUBROUTINE SETPAR()
!************************************************************************
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   % Subrotina de inicializacao das variaveis no principio do   %
!   %    programa                                                %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!

    Nmon = 0; Ncad = 0; Nlig = 0

    massamon = 1.6932e-25    !kg
    comprimento = 1.

    raio = 0.

    ncompleta = 0

    dimx = 1.; dimy = 1.; dimz = 1.; volume = 1.

    opcdom1 = 1; opcdom2 = 0; opcdom3 = 1
    ndom = 1

    domx0 = 0.; domy0 = 0.; domz0 = 0.
    domx1 = 1.; domy1 = 1.; domz1 = 1.

    medias = 0.; desvios = 1.
    densidade = 0.; vol = 1.

    tetaz = 0.; deltaz = 0.
    tetax = 0.; deltax = 0.
    tetaxy = 0.; deltaxy = 0.

    tetaz0 = 0.; tetaz1 = 0.
    tetax0 = 0.; tetax1 = 0.
    tetaxy0 = 0.; tetaxy1 = 0.

    tentax = 0.; tentay = 0.; tentaz = 0.
    tentaalfa = 0.; tentabeta = 0.; tentagama = 0.

    previsto = 0; preenchido = 0; npreenchido = 0; ncade = 0

    contagem = 0

    tamarray = 0
    tamanho = 0

    fronteira = 0.
    fronteira(1:3,1) = 1.
    fronteira(4:5,1) = 0.
    fronteira(6:8,1) = -1.
    fronteira(1,2) = 1.
    fronteira(2,2) = 0.
    fronteira(3,2) = -1.
    fronteira(4,2) = 1.
    fronteira(5,2) = -1.
    fronteira(6,2) = 1.
    fronteira(7,2) = 0.
    fronteira(8,2) = -1.

    vezes = 1
    contavezes = 1
    tam_max_cad = 20

  END SUBROUTINE


END MODULE
MODULE gaussiana


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
! media, desvio - media e desvio padr�o da distribuicao
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

MODULE crescer



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
    INTEGER(KIND=1) :: lado    !determina para que lado vai come�ar a crescer (+1/-1)
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

MODULE input

  USE comum



  CONTAINS

    SUBROUTINE inputs()
!************************************************************************
!************************************************************************
!*                                                                     **
!*                       INPUTS.F90                                    **
!*              ===============================                        **
!*                                                                     **
!*        Subrotina que le o ficheiro de dados do programa sparg       **
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
!   % Esta subrotina le do ficheiro sparg.ini os valores usados  %
!   % na corrida do programa sparg.                              %
!   %                                                            %
!   % Usa os dados de um modulo de variaveis comuns onde vai     %
!   % colocar os valores lidos no ficheiro de input, chamado    .%
!   % sparg.ini                                                  %
!   %                                                            %
!   %                                                            %
!   %                                                            %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!  < INPUT IS READ IN KEYWORD FORMAT.  KEYWORDS ARE:                <
!  <                                                                <
!  <  (1) TITLE                      (2) OPCOES                     <
!  <  (3) MASSA MONOMERO             (4) DIMENSOES (do volume)      <
!  <  (5) PASSOS (para cada impress) (6) DOMINIOS                   <
!  <  (7) COMPRIMENTO (monomeros)    (8) RAIO (dist. min. entre cad.<
!  <  (9) VEZES (que corre)          (10) SEED (random generator)   <
!  <  (11) TENTATIVAS DE ENCAIXE     (12)                           <
!  <  (13)                           (14)                           <
!  <
!  <  (29)                           (30) END                       <
!  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    REAL(KIND=8), PARAMETER :: pi=3.141592654, pi180=0.017453293
    INTEGER(KIND=4) :: numeromon, numerocad
    INTEGER :: error

    INTEGER(KIND=4) :: i, j, k, temp
    INTEGER(KIND=4) :: semente
    REAL(KIND=4) :: a
    LOGICAL :: existe

    CHARACTER(25) :: WDKEY
    CHARACTER(25) :: WKEY
    CHARACTER(80) :: TITLE
    CHARACTER(3) :: READIN

    numeromon = 300000
    numerocad = 50000

    INQUIRE(FILE="sparg.ini", EXIST=existe)
    IF (.NOT. existe) THEN
      WRITE(*,*) 'Erro: ficheiro de input "sparg.ini" nao encontrado.'
      STOP
    ENDIF

    OPEN (UNIT=1, FILE="sparg.ini", STATUS="old")
    OPEN (UNIT=2, FILE="sparg.txt")

500 READ(UNIT=1,FMT='(A25)')WDKEY
    READIN = WDKEY(1:3)     
!1---------------------------------------------------------------------
    IF( READIN == 'tit' .or. READIN == 'TIT')THEN
      READ(UNIT=1,FMT='(A80)') TITLE
      WRITE(UNIT=2,FMT='(A80,/)') TITLE
!2---------------------------------------------------------------------
    ELSE IF( READIN == 'opc' .or. READIN == 'OPC')THEN
      READ(UNIT=1,FMT=*) WKEY
      IF (WKEY(1:1) == 'A') THEN
        opcdom1 = 0
        WRITE(UNIT=2,FMT=*)' Comeca o preenchimento pelas cadeias maiores.'
      ELSEIF (WKEY(1:1) == 'B') THEN
        opcdom1 = 1
        WRITE(UNIT=2,FMT=*)' Faz o preenchimento aleatoriamente.'
      ENDIF

      IF (WKEY(2:2) == 'A') THEN
        opcdom2 = 0
        WRITE(UNIT=2,FMT=*)' Constroi os dominios todos ao mesmo tempo.'
      ELSEIF (WKEY(2:2) == 'B') THEN
        opcdom2 = 1
        WRITE(UNIT=2,FMT=*)' Constroi primeiro os dois primeiros dominios.'
      ELSEIF (WKEY(2:2) == 'C') THEN
        opcdom2 = 2
        WRITE(UNIT=2,FMT=*)' Constroi os dominios pela ordem de input.'
      ENDIF
!3---------------------------------------------------------------------
    ELSE IF( READIN == 'mas' .or. READIN == 'MAS')THEN
      READ(UNIT=1,FMT=*) massamon
      WRITE(UNIT=2,FMT='('' Massa de 1 monomero: '',e8.3,'' kg'')') massamon
!4---------------------------------------------------------------------
    ELSE IF( READIN == 'dim' .or. READIN == 'DIM')THEN
      READ(UNIT=1,FMT=*) dimx, dimy, dimz
      WRITE(UNIT=2,FMT='('' Dimensoes da amostra (nm): '',3f8.2)') dimx, dimy, dimz 
!5---------------------------------------------------------------------
!    ELSE IF( READIN == 'pas' .or. READIN == 'PAS')THEN
!      READ(UNIT=1,FMT=*) iprt
!      WRITE(UNIT=2,FMT='('' Numero de passos para imprimir: '',i8)') iprt 
!6---------------------------------------------------------------------
    ELSE IF( READIN == 'dom' .or. READIN == 'DOM')THEN
      READ(UNIT=1,FMT=*) opcdom3
      IF (opcdom3 == 0) THEN
        WRITE(UNIT=2,FMT=*)' Dominios crescem em torno de cadeias iniciais.'
      ELSE
        READ(UNIT=1,FMT=*) ndom
        WRITE(UNIT=2,FMT='('' Numero de dominios: '',i8)') ndom
        WRITE(UNIT=2,FMT=*)
        DO i=1,ndom
          READ(UNIT=1,FMT=*) domx0(i),domx1(i)
          READ(UNIT=1,FMT=*) domy0(i),domy1(i)
          READ(UNIT=1,FMT=*) domz0(i),domz1(i)
          READ(UNIT=1,FMT=*) medias(i), desvios(i)
          READ(UNIT=1,FMT=*) tetaz(i), deltaz(i)
          READ(UNIT=1,FMT=*) tetax(i), deltax(i)
          READ(UNIT=1,FMT=*) tetaxy(i), deltaxy(i)
          READ(UNIT=1,FMT=*) densidade(i)
          WRITE(UNIT=2,FMT='('' Numero do dominio: '',i8)') i
          WRITE(UNIT=2,FMT='('' Limites em x: '',2f8.1)') domx0(i),domx1(i)
          WRITE(UNIT=2,FMT='('' Limites em y: '',2f8.1)') domy0(i),domy1(i)
          WRITE(UNIT=2,FMT='('' Limites em z: '',2f8.1)') domz0(i),domz1(i)
          WRITE(UNIT=2,FMT='('' Media e Desvio padrao: '',2f8.2)') medias(i), desvios(i)
          WRITE(UNIT=2,FMT='('' Angulo com z (graus), tolerancia (+/-): '',2f8.2)') tetaz(i), deltaz(i)
          WRITE(UNIT=2,FMT='('' Angulo com x (graus), tolerancia (+/-): '',2f8.2)') tetax(i), deltax(i)
          WRITE(UNIT=2,FMT='('' Angulo com xy (graus), tolerancia (+/-): '',2f8.2)') tetaxy(i), deltaxy(i)
          WRITE(UNIT=2,FMT='('' Densidade em g/cm3 do dominio: '',f8.3)') densidade(i)
          WRITE(UNIT=2,FMT=*)
          ! angulos metidos em graus e logo a seguir convertidos em radianos
          ! distancias em nm
        ENDDO
      ENDIF
!7---------------------------------------------------------------------
    ELSE IF( READIN == 'com' .or. READIN == 'COM')THEN
      READ(UNIT=1,FMT=*) comprimento
      WRITE(UNIT=2,FMT='('' Comprimento do eixo do monomero (nm): '',f8.5)') comprimento
!8---------------------------------------------------------------------
    ELSE IF( READIN == 'rai' .or. READIN == 'RAI')THEN
      READ(UNIT=1,FMT=*) raio
      WRITE(UNIT=2,FMT='('' Distancia minima entre monomeros de cadeias diferentes (nm): '',f8.5)') raio
!9---------------------------------------------------------------------
    ELSE IF( READIN == 'vez' .or. READIN == 'VEZ')THEN
      READ(UNIT=1,FMT=*) vezes
      WRITE(UNIT=2,FMT='('' Cria este numero de amostras: '',i5)') vezes
!10--------------------------------------------------------------------
    ELSE IF( READIN == 'see' .or. READIN == 'SEE')THEN
      READ(UNIT=1,FMT=*) semente
      WRITE(UNIT=2,FMT='('' Valor de seed (random number generator:inteiro): '',i8)') semente
!11--------------------------------------------------------------------
    ELSE IF( READIN == 'ten' .or. READIN == 'TEN')THEN
      READ(UNIT=1,FMT=*) tenta_encaix
      WRITE(UNIT=2,FMT='('' Numero de tentativas para encaixar cadeias: '',i8)') tenta_encaix

!30--------------------------------------------------------------------
    ELSE IF( READIN == 'end' .or. READIN == 'END')THEN
      GOTO 400

    ENDIF
!----------------------------------------------------------------------

    GOTO 500

400 CLOSE (UNIT=1)

    WRITE(UNIT=2,FMT='(''Volume: '',f8.2,'' nm3'')')dimx*dimy*dimz
    volume = dimx*dimy*dimz*1e-21
    WRITE(UNIT=2,FMT='(''Volume: '',e8.2,'' cm3'')')volume
!label  FORMAT('f8.2')
    WRITE(UNIT=2,FMT=*)

! volume dos dominios e numero das cadeias estimado
    vol(1:ndom) = (domx1(1:ndom) - domx0(1:ndom))* &
                  (domy1(1:ndom) - domy0(1:ndom))* &
                  (domz1(1:ndom) - domz0(1:ndom))

    ncade(1:ndom) = vol(1:ndom)*densidade(1:ndom)*1e-21/1000. &
                    /massamon/medias(1:ndom)

    dimz_inteiro = INT(dimz)

    WRITE(UNIT=2,FMT=*)' volume dos dominios (nm3)'
    DO i=1,ndom
      WRITE(UNIT=2,FMT='(i5,f8.2)')i,vol(i)
    ENDDO
    WRITE(UNIT=2,FMT=*)' numero das cadeias estimado em cada dominio'
    DO i=1,ndom
      WRITE(UNIT=2,FMT='(2i8)')i,ncade(i)
    ENDDO

    numerocad = INT(SUM(ncade)*1.1)

    ALLOCATE (cadeia(1:numerocad,0:20), ncadeia(1:numerocad), ncadeiafim(1:numerocad), STAT=error)

    IF (error /= 0) THEN
      WRITE(*,*) 'Erro ao alocar espa�o para os arrays cadeia, ncadeia e ncadeiafim.'
      WRITE(*,*) 'numerocad = ',numerocad
      STOP
    ENDIF

    cadeia = 0
    ncadeia = 0
    ncadeiafim = 0

! numero de monomeros previstos por dominio (acerto de unidades)
    previsto(1:ndom) = INT(densidade(1:ndom)*vol(1:ndom)/massamon/1000.*1e-21)
    WRITE(UNIT=2,FMT=*)' Numero de monomeros previstos por dominio:'
    DO i=1,ndom
      WRITE(UNIT=2,FMT='(2i10)')i,previsto(i)
    ENDDO

    numeromon = INT(SUM(previsto)*1.1)

    ALLOCATE (pos(1:numeromon,1:3),alfa(1:numeromon),beta(1:numeromon),gama(1:numeromon), &
              array(1:numeromon*10,1:3), STAT=error)

    IF (error /= 0) THEN
      WRITE(*,*) 'Erro ao alocar espa�o para os arrays pos, alfa, beta, gama, array.'
      WRITE(*,*) 'numeromon = ',numeromon
      STOP
    ENDIF

    pos = 0.
    alfa = 0.; beta = 0.; gama = 0.
    array = 0.

! conversao dos angulos para radianos
    tetaz(1:ndom) = tetaz(1:ndom)*pi180
    deltaz(1:ndom) = deltaz(1:ndom)*pi180
    tetax(1:ndom) = tetax(1:ndom)*pi180
    deltax(1:ndom) = deltax(1:ndom)*pi180
    tetaxy(1:ndom) = tetaxy(1:ndom)*pi180
    deltaxy(1:ndom) = deltaxy(1:ndom)*pi180

! converte os angulos para um intervalo
    tetaz0(1:ndom) = tetaz(1:ndom) - deltaz(1:ndom)
    tetaz1(1:ndom) = tetaz(1:ndom) + deltaz(1:ndom)
    tetax0(1:ndom) = tetax(1:ndom) - deltax(1:ndom)
    tetax1(1:ndom) = tetax(1:ndom) + deltax(1:ndom)
    tetaxy0(1:ndom) = tetaxy(1:ndom) - deltaxy(1:ndom)
    tetaxy1(1:ndom) = tetaxy(1:ndom) + deltaxy(1:ndom)
    WRITE(UNIT=2,FMT=*)
    DO i=1,ndom
      WRITE(UNIT=2,FMT='('' Angulos com z (rad): '',i8,2f8.2)') i,tetaz0(i), tetaz1(i)
      WRITE(UNIT=2,FMT='('' Angulos com x (rad): '',i8,2f8.2)') i,tetax0(i), tetax1(i)
      WRITE(UNIT=2,FMT='('' Angulos com xy (rad): '',i8,2f8.2)') i,tetaxy0(i), tetaxy1(i)
    ENDDO
    WRITE(UNIT=2,FMT=*)

! utilizacao do seed
    CALL srand(semente)


! array auxiliar para estabelecer as condicoes fronteira
    fronteira = 0.
    fronteira(1:3,1) = dimx
    fronteira(4:5,1) = 0.
    fronteira(6:8,1) = -dimx
    fronteira(1,2) = dimy
    fronteira(2,2) = 0.
    fronteira(3,2) = -dimy
    fronteira(4,2) = dimy
    fronteira(5,2) = -dimy
    fronteira(6,2) = dimy
    fronteira(7,2) = 0.
    fronteira(8,2) = -dimy

!alocar espa�o para as variaveis estatisticas

    tam_max_cad = INT(MAXVAL(medias(1:ndom)) + 4.*MAXVAL(desvios(1:ndom))) + 1

    ALLOCATE (con_tam_cad(0:tam_max_cad,1:ndom,1:vezes))
    ALLOCATE (con_ang_cad(0:18,1:ndom,1:vezes))
    ALLOCATE (con_pos_cad(0:dimz_inteiro,1:ndom,1:vezes))
    ALLOCATE (con_pos_mon(0:dimz_inteiro,1:vezes))

    con_tam_cad = 0
    con_ang_cad = 0
    con_pos_cad = 0
    con_pos_mon = 0

! array de directorias
    ALLOCATE (director(1:vezes))

    IF (vezes == 1) THEN
      director(1) = './'
    ELSE
      DO i=1, vezes
        WRITE(director(i), '(A,I0,A)') './', i, '/'
        CALL EXECUTE_COMMAND_LINE('mkdir -p ' // TRIM(director(i)))
      ENDDO
    ENDIF

    RETURN
  END SUBROUTINE

END MODULE
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


MODULE rotinas


  USE comum

!  IMPLICIT NONE

  CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! tira 'a sorte a posicao inicial e a direccao
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE tentaponto(i)

    INTEGER(KIND=4) :: i

    tentax = domx0(i) + (domx1(i) - domx0(i))*RAND(0)
    tentay = domy0(i) + (domy1(i) - domy0(i))*RAND(0)
    tentaz = domz0(i) + (domz1(i) - domz0(i))*RAND(0)
    tentaalfa = tetaz0(i) + (tetaz1(i) - tetaz0(i))*RAND(0)
    tentabeta = tetax0(i) + (tetax1(i) - tetax0(i))*RAND(0)
    tentagama = tetaxy0(i) + (tetaxy1(i) - tetaxy0(i))*RAND(0)

    RETURN

    END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! se tem sucesso a preencher, atribui as variaveis os diversos valores
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE suces(i,jj)

    INTEGER(KIND=4) :: i, j, jj, k, nmtaman
    REAL(KIND=4) :: aaa, bbb, ccc

    nmtaman = Nmon + tamanho

    IF(jj == 0)THEN
      Ncad = Ncad +1    !incrementa o numero de cadeias
      jj = Ncad
    ENDIF
    cadeia(jj,0) = i    !indica qual o dominio a que pertence a cadeia
    cadeia(jj,1) = Nmon +1    !indica o numero do primeiro monomero da cadeia
    ncadeia(jj) = tamanho    !indica o tamanho da cadeia

    alfa(Nmon+1:nmtaman) = tentaalfa
    beta(Nmon+1:nmtaman) = tentabeta
    gama(Nmon+1:nmtaman) = tentagama

    !acrescenta os novos monomeros ao array usado para verificar se encaixa
    array(tamarray + 1:tamarray + tamanho,:) = pos(Nmon + 1:nmtaman,:)
    tamarray = tamarray + tamanho
    DO k=1,8
      DO j=1,3
        array(tamarray + 1:tamarray + tamanho,j) = pos(Nmon + 1:nmtaman,j) + fronteira(k,j)
      ENDDO
      tamarray = tamarray + tamanho
    ENDDO
    !acrescenta a ponta da cadeia ao array
    aaa = pos(nmtaman,1) + comprimento*SIN(alfa(nmtaman))*COS(gama(nmtaman))
    bbb = pos(nmtaman,2) + comprimento*SIN(alfa(nmtaman))*SIN(gama(nmtaman))
    ccc = pos(nmtaman,3) + comprimento*COS(alfa(nmtaman))

    tamarray = tamarray + 1
    array(tamarray,1) = aaa
    array(tamarray,2) = bbb
    array(tamarray,3) = ccc
    DO k=1,8
      array(tamarray + 1,1) = aaa + fronteira(k,1)
      array(tamarray + 1,2) = bbb + fronteira(k,2)
      array(tamarray + 1,3) = ccc + fronteira(k,3)
      tamarray = tamarray + 1
    ENDDO

    Nmon = nmtaman    !incrementa o numero total de monomeros

    preenchido(i) = preenchido(i) + tamanho    !incrementa o numero de monomeros 
    IF (preenchido(i) >= previsto(i)) THEN     !do dominio e verifica se ja esta
      npreenchido = npreenchido + 1            !preenchido
    ENDIF

    RETURN

    END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! se nao tem sucesso a preencher:
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE insuces(i,aviso)

    INTEGER(KIND=4) :: i, aviso
    INTENT(IN) :: i

    aviso = 0
    contagem(i) = contagem(i) + 1

    IF (contagem(i) > tenta_encaix) THEN
      WRITE(*,*) tenta_encaix,' tentativas encaixar cadeias dominio: ', i
      aviso = 1000
    ENDIF

    RETURN

    END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! estabelece as condicoes fronteira; reduz todos os pontos a um volume (em x e y)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE condfronteira

    WHERE (pos(:,1) > dimx) pos(:,1) = pos(:,1) - dimx
    WHERE (pos(:,1) < 0) pos(:,1) = pos(:,1) + dimx
    WHERE (pos(:,2) > dimx) pos(:,2) = pos(:,2) - dimy
    WHERE (pos(:,2) < 0) pos(:,2) = pos(:,2) + dimy

    RETURN

    END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! faz as estatisticas finais
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE estatisticas

    INTEGER(KIND=4) :: i,j,aaa,aaaa
    REAL(KIND=4) :: bbb

    DO i=1, Ncad
      !conta quantas cadeias ha em cada dominio com cada tamanho
      con_tam_cad(ncadeia(i),cadeia(i, 0),contavezes) = con_tam_cad(ncadeia(i),cadeia(i, 0),contavezes) + 1

      !conta quantas cadeias existem com cada angulo
      aaa = INT(ABS(alfa(cadeia(i,1)))*180./3.14159/5.)
      con_ang_cad(aaa,cadeia(i, 0),contavezes) = con_ang_cad(aaa,cadeia(i, 0),contavezes) + 1

      !conta quantas cadeias em cada posicao do eixo z
      aaaa = INT(pos(cadeia(i,1),3))
      con_pos_cad(aaaa,cadeia(i, 0),contavezes) = con_pos_cad(aaaa,cadeia(i, 0),contavezes) + 1
    ENDDO

    !conta quantos monomeros em cada posicao do eixo z
    DO i=1, Nmon
      aaa = INT(pos(i,3))
      con_pos_mon(aaa,contavezes) = con_pos_mon(aaa,contavezes) + 1
    ENDDO

    RETURN

    END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reinicia as variaveis para construir outra amostra
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE reiniciar


    Nmon = 0; Ncad = 0

    pos = 0
    alfa = 0.; beta = 0.; gama = 0.

    array = 0.
    tamarray = 0

    cadeia = 0
    ncadeia = 0
    ncadeiafim = 0

    ncompleta = 0
    preenchido = 0
    npreenchido = 0

    contagem = 0


    RETURN

    END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE

    PROGRAM sparg
!************************************************************************
!************************************************************************
!*                                                                     **
!*                         SPARG.F90                                   **
!*              ===============================                        **
!*                                                                     **
!*               Programa de construcao de uma estrutura               **
!*               de polimeros em cadeia em estilo esparguete           **
!*                                                                     **
!*                                                                     **
!*              Ricardo Mendes Ribeiro      Marco 2003                 **
!*                                                                     **
!************************************************************************
!************************************************************************
!
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   %               Explicacao detalhada do programa             %
!   % Este programa constroi uma base de dados com as caracteris %
!   % ticas de um conjunto de cadeias de polimeros com as        %
!   % restricoes que se pretender em relacao ao tamanho, direccao%
!   % torcao, inclinacao e outros parametros que possam ser rele %
!   % vantes para o estudo mesoscopico do polimero.              %
!   %                                                            %
!   %                                                            %
!   %                                                            %
!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!
    USE global
    USE comum
    USE gaussiana
    USE crescer
    USE input
    USE prints
    USE rotinas

    INTEGER(KIND=4) :: i, j, k, aviso
    INTEGER(KIND=4) :: tmp
    LOGICAL :: sucesso
    INTEGER(KIND=4), POINTER :: ordenacao(:)
    INTEGER(KIND=4), POINTER :: dom_ord(:)
    INTEGER(KIND=4), ALLOCATABLE :: ncaddom(:)    !numero de cadeias em
                                                  !cada dominio

    sucesso = .TRUE.

    CALL SETPAR     !Inicializacao das variaveis

    CALL INPUTS     !Leitura dos ficheiros de inicializacao e de dados

    ALLOCATE(ncaddom(1:ndom))

!************************************************************************
!**                  LOOP PRINCIPAL DO PROGRAMA                        **
!************************************************************************

    DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Opcao A: comeca a preeencher pelas cadeias maiores
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          implica definir primeiro as cadeias com os seus tamanhos
      IF (opcdom1==0) THEN
        j = SUM(Ncade(:))

        ALLOCATE(dom_ord(1:j))

        Ncad = 0    !inicializacao do numero total de cadeias
        ncaddom = 0    !inicializacao do numero de cadeias em cada dominio

        DO WHILE (Ncad < j)
          i = INT(RAND(0)*ndom + 1.)    !tira a sorte um dominio
          IF (ncaddom(i) < Ncade(i)) THEN
            ncaddom(i) = ncaddom(i) + 1
            Ncad = Ncad + 1   !estabelece um tamanho previo para cada cadeia
            ncadeiafim(Ncad)=gauss(2,tam_max_cad,medias(i),desvios(i),0,0)
            dom_ord(Ncad) = i
          ENDIF
        ENDDO

        ALLOCATE (ordenacao(1:Ncad))

! ordenar cadeias por tamanho (insertion sort, maior primeiro)

        ordenacao(1:Ncad) = (/(k,k=1,Ncad)/)

        DO j=2, Ncad
          tmp = ordenacao(j)
          k = j - 1
          DO WHILE (k >= 1 .AND. ncadeiafim(ordenacao(k)) < ncadeiafim(tmp))
            ordenacao(k+1) = ordenacao(k)
            k = k - 1
          ENDDO
          ordenacao(k+1) = tmp
        ENDDO

! tentar crescer cada cadeia
        DO j=1, Ncad
          i = dom_ord(ordenacao(j))    !indica a que dominio pertence a cadeia

          tamanho = ncadeiafim(ordenacao(j))    !ve qual o tamanho da cadeia

          DO    !loop de tentativas de posicao
            CALL tentaponto(i)

            pos(Nmon+1:Nmon+tamanho,:) = cresce(tentax,tentay,tentaz,tentaalfa, &
                                              tentabeta,tentagama,tamanho,sucesso)

            IF (pos(Nmon+1,1) /= -100) THEN
              CALL suces(i,ordenacao(j))
              EXIT
            ELSE
              CALL insuces(i,aviso)
              IF (aviso == 1000) THEN
                contagem(i) = INT(contagem(i)*0.9999)
                EXIT
              ENDIF
            ENDIF
          ENDDO

        ENDDO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Opcao B: comeca a preencher por uma cadeia qualquer
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ELSEIF (opcdom1==1) THEN

! -A: constroi os dominios todos ao mesmo tempo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (opcdom2==0) THEN
          DO
            i = INT(RAND(0)*ndom + 1)    !tira 'a sorte para qual dominio vai esta cadeia

            IF (preenchido(i) >= previsto(i)) THEN    !verifica se o dominio ja esta preenchido
              IF (npreenchido == ndom) THEN    !se todos os dominios estiverem preenchidos,
                EXIT                           !sai do loop
              ENDIF
              CYCLE
            ENDIF

        ! tira 'a sorte a posicao inicial e a direccao (no dominio i)
            CALL tentaponto(i)
        !tira 'a sorte um tamanho
           tamanho = gauss(2, tam_max_cad, medias(i), desvios(i), 0, 0)

        !tenta crescer nessa posicao e com esse tamanho
            pos(Nmon+1:Nmon+tamanho,:) = cresce(tentax,tentay,tentaz,tentaalfa, &
                                            tentabeta,tentagama,tamanho,sucesso)

            IF (pos(Nmon+1,1) /= -100) THEN
              CALL suces(i,0)
            ELSE
              CALL insuces(i,aviso)
              IF (aviso == 1000) THEN
                EXIT
              ENDIF
            ENDIF
          ENDDO


! -B constroi primeiro os dois primeiros da ordem de input
!    (poderao ser os ligados,por exemplo) e depois constroi
!     os outros todos ao mesmo tempo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF (opcdom2==1) THEN

 front1:  DO i=1,2
            DO WHILE (preenchido(i) < previsto(i))
          ! tira 'a sorte a posicao inicial e a direccao
              CALL tentaponto(i)

          !tira 'a sorte um tamanho
              tamanho = gauss(2, tam_max_cad, medias(i), desvios(i), 0, 0)

          !tenta crescer nessa posicao e com esse tamanho
              pos(Nmon+1:Nmon+tamanho,:) = cresce(tentax,tentay,tentaz,tentaalfa, &
                                              tentabeta,tentagama,tamanho,sucesso)
              IF (pos(Nmon+1,1) /= -100) THEN
                CALL suces(i,0)
              ELSE
                CALL insuces(i,aviso)
                IF (aviso == 1000) THEN
                  EXIT front1
                ENDIF
              ENDIF
            ENDDO
          ENDDO front1

          DO
            i = INT(RAND(0)*(ndom - 1)) + 2    !tira 'a sorte para qual dominio vai esta cadeia
            IF (preenchido(i) >= previsto(i)) THEN    !verifica se o dominio ja esta preenchido
              IF (npreenchido == ndom) THEN    ! se todos os dominios estiverem preenchidos,
                EXIT                           ! sai do loop
              ENDIF
              CYCLE
            ENDIF
        ! tira 'a sorte a posicao inicial e a direccao
            CALL tentaponto(i)

        !tira 'a sorte um tamanho
            tamanho = gauss(2, tam_max_cad, medias(i), desvios(i), 0, 0)

        !tenta crescer nessa posicao e com esse tamanho
            pos(Nmon+1:Nmon+tamanho,:) = cresce(tentax,tentay,tentaz,tentaalfa, &
                                            tentabeta,tentagama,tamanho,sucesso)
            IF (pos(Nmon+1,1) /= -100) THEN
              CALL suces(i,0)
            ELSE
              CALL insuces(i,aviso)
              IF (aviso == 1000) THEN
                EXIT
              ENDIF
            ENDIF
          ENDDO

! -C constroi os dominios pela ordem de input do ficheiro de dados
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF (opcdom2==2) THEN

 front2:  DO i=1,ndom
            DO WHILE (preenchido(i) < previsto(i))
          ! tira 'a sorte a posicao inicial e a direccao
              CALL tentaponto(i)

          !tira 'a sorte um tamanho
              tamanho = gauss(2, tam_max_cad, medias(i), desvios(i), 0, 0)

          !tenta crescer nessa posicao e com esse tamanho
              pos(Nmon+1:Nmon+tamanho,:) = cresce(tentax,tentay,tentaz,tentaalfa, &
                                              tentabeta,tentagama,tamanho,sucesso)
              IF (pos(Nmon+1,1) /= -100) THEN
                CALL suces(i,0)
              ELSE
                CALL insuces(i,aviso)
                IF (aviso == 1000) THEN
                  EXIT front2
                ENDIF
              ENDIF
            ENDDO
          ENDDO front2

        ENDIF
      ENDIF

! estabelece as condicoes fronteiras (reduz tudo a um volume)
      CALL condfronteira

! faz as estatisticas
      CALL estatisticas

! grava os resultados
      CALL printt

! para varias corridas
      contavezes = contavezes + 1

      IF (contavezes > vezes) EXIT

      CALL reiniciar
      aviso = 0
      IF (opcdom1 == 0) THEN
        DEALLOCATE(dom_ord)
        DEALLOCATE(ordenacao)
      ENDIF

    ENDDO

! grava os resultados finais
    CALL printf

    CLOSE(UNIT=2)


    STOP
    END
