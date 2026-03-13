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
