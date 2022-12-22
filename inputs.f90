MODULE input

  USE comum

  USE DFPORT    ! E' necessario comentar esta linha quando nao se usa o fortran
                ! da DIGITAL/COMPAQ/HP

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

    CHARACTER(25) :: WDKEY
    CHARACTER(25) :: WKEY
    CHARACTER(80) :: TITLE
    CHARACTER(3) :: READIN

    numeromon = 300000
    numerocad = 50000

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
      WRITE(*,*) 'Erro ao alocar espaço para os arrays cadeia, ncadeia e ncadeiafim.'
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
      WRITE(*,*) 'Erro ao alocar espaço para os arrays pos, alfa, beta, gama, array.'
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
    a = RAND(semente)


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

!alocar espaço para as variaveis estatisticas

    ALLOCATE (con_tam_cad(0:20,1:ndom,1:vezes))
    ALLOCATE (con_ang_cad(0:18,1:ndom,1:vezes))
    ALLOCATE (con_pos_cad(0:dimz_inteiro,1:ndom,1:vezes))
    ALLOCATE (con_pos_mon(0:dimz_inteiro,1:vezes))

    con_tam_cad = 0
    con_ang_cad = 0
    con_pos_cad = 0
    con_pos_mon = 0

! array de directorias
    ALLOCATE (director(1:vezes))

    IF (vezes <= 1) THEN
      director(1) = './'
    ELSEIF (vezes <= 10 .AND. vezes > 1) THEN
      DO i=1, vezes
        director(i) = './' // ACHAR(i+47) // '/'
      ENDDO
    ENDIF
    director = ADJUSTR(director)

    RETURN
  END SUBROUTINE

END MODULE
