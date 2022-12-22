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

! cadeia(Ncad, 0:1) - array com a listagem das cadeias e do 1º monomero
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

  END SUBROUTINE


END MODULE
