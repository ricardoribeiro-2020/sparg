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
