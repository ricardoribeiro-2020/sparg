
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
              tmp = 0; CALL suces(i,tmp)
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
                tmp = 0; CALL suces(i,tmp)
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
              tmp = 0; CALL suces(i,tmp)
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
                tmp = 0; CALL suces(i,tmp)
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
    PRINT(UNIT=2,FMT=*)
    PRINT(UNIT=2,FMT=*)' Programa terminou'
    PRINT(UNIT=2,FMT=*)
    CLOSE(UNIT=2)

    STOP
    END
