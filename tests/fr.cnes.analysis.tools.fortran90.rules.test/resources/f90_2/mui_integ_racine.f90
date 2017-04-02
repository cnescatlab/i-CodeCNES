subroutine mui_integ_racine ( integrateur, n_commut, A, gA, B, gB, err_abs, iter_max, &
     nb_racine, criter_arret, sol, retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Recherche d'une racine dans la fonction de commutation g(t,y(t))
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation a partir de mu_racine
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_mspro
use parametre_mspro
use valeur_code_retour_mspro

use int_util_internes_mspro, only : mui_integ_interp

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur), intent(in)                     ::  integrateur  ! integrateur
integer,       intent(in)                            ::  n_commut     ! indice designant la fonction g de commutation a etudier
real(pm_reel), intent(in)                            ::  A   ! borne inferieure de l'intervalle de recherche de solution
real(pm_reel), intent(in)                            ::  gA  ! valeur de g en A
real(pm_reel), intent(in)                            ::  B   ! borne superieure de l'intervalle de recherche de solution
real(pm_reel), intent(in)                            ::  gB  ! valeur de g en B
real(pm_reel), intent(in)                            ::  err_abs    ! erreur absolue autorisee par l'utilisateur
integer, intent(in)                                  ::  iter_max   ! nombre maximum d'iterations
integer, intent(out)                                 ::  nb_racine  ! indication sur le nombre de racines
integer, intent(out)                                 ::  criter_arret  ! critere d'arret utilise
real(pm_reel), intent(out)                           ::  sol    ! valeur de la racine si la routine en a trouve une
integer,       intent(out)                           ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: tols         ! critere de convergence
real(pm_reel) :: s1,s3        ! signes respectifs de gA et gB
real(pm_reel) :: presque_zero ! plus petit reel positif
real(pm_reel) :: eps100       ! constante eps pour les reels
real(pm_reel) :: c, fc        ! c et g(c)
real(pm_reel) :: d, e         ! reels d et e
real(pm_reel) :: xm, s, p, q, r ! variables intermediaires
real(pm_reel) :: AA, BB       ! copie des bornes
real(pm_reel) :: fa, fb       ! copies de gA et gB

real(pm_reel),dimension(integrateur%n) :: y       ! vecteur d'etat a t donne
integer                                :: retour_local

integer ::  iter_l       ! en local: nombre d'iterations effectuees
integer ::  retour_sub_l ! en local: code retour de la routine de commutation

intrinsic abs, tiny, epsilon, sign
external MsproAppelFonction4

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mui_integ_racine.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! autres initialisations

presque_zero = tiny(1._pm_reel)             ! plus petit nombre machine
eps100 = 100._pm_reel * epsilon(1._pm_reel) ! constante 100*eps machine

AA = A
BB = B

iter_l = 0

tols = 0._pm_reel

fa = gA
fb = gB

! ============================================
! si g(a) = 0 alors a est racine de l'equation
! ============================================
if (abs(fa) < presque_zero) then ! A est racine de l'equation

   nb_racine = pm_1racine

   ! ===========================================================
   ! si g(a) = 0 et g(b) = 0 alors a et b racines de l'equation
   ! ===========================================================

   if (abs(fb) < presque_zero) nb_racine = pm_A_B_racine  ! A et B sont racines de l'equation
   sol = AA
   criter_arret = pm_criter_arret_fx
   go to 6000

   ! ============================================
   ! si g(b) = 0 alors b est racine de l'equation
   ! ============================================

else if (abs(fb) < presque_zero) then ! B est racine de l'equation

   nb_racine = pm_1racine
   sol = BB
   criter_arret = pm_criter_arret_fx
   go to 6000

end if

! ============================================
! si g(a)*g(b) > 0 : 0 ou 2*n racines
! ============================================
! cas impossible

! ============================================
! Mise en oeuvre de l'algorithme de R.P. Brent
! ============================================

c = AA
fc = fa
d = BB - AA
e = d

if (abs(fc) < abs(fb)) then 
   AA = BB
   BB = c
   c = AA
   fa = fb
   fb = fc
   fc = fa
end if

tols=2._pm_reel * eps100 * abs(BB) + 0.5_pm_reel * err_abs
xm = 0.5_pm_reel * (c - BB)

do while ((abs(xm) > tols).and.(abs(fb) >= presque_zero))  ! la bissection est-elle necessaire?

   if ((abs(e) < tols).or.(abs(fa) <= abs(fb))) then

      d = xm
      e = d

   else

      s = fb / fa

      if (abs(AA - c) < presque_zero) then  ! Methode de la secante (= interpolation lineaire)

         p = 2.0_pm_reel * xm * s
         q = 1.0_pm_reel - s

      else             ! Methode de la secante (= interpolation quadratique inverse)

         q = fa / fc
         r = fb / fc
         p = s * (2.0_pm_reel * xm * q * (q - r) - (BB - AA) * (r - 1.0_pm_reel))
         q = (q - 1.0_pm_reel) * (r - 1.0_pm_reel) * (s - 1.0_pm_reel)

      end if

      if (p > 0._pm_reel) then

         q = -q

      else

         p = -p

      end if

      s = e
      e = d

      if (((2.0_pm_reel*p) < (3.0_pm_reel*xm*q - abs(tols*q))).and.(p < abs(0.5_pm_reel*s*q))) then

         d = p / q

      else

         d = xm
         e = d

      end if
   end if

   AA = BB
   fa = fb

   if (abs(d) > tols) then    ! Une etape en tols est-elle necessaire ?

      BB = BB + d

   else if (xm > 0.0_pm_reel) then

      BB = BB + tols

   else

      BB = BB - tols

   end if

   call mui_integ_interp(integrateur,BB,y,retour_local)
   if (retour_local /= pm_OK) then
      retour = retour_local
      if (retour_local < pm_OK) go to 6000
   end if
   call MsproAppelFonction4(integrateur%g_commut(n_commut)%adresse,BB,y,fb,retour_sub_l)
   if (retour_sub_l /= pm_OK) then
      retour = pm_err_dans_sub_commut
      go to 6000
   end if

   iter_l = iter_l + 1

   if (iter_l > iter_max) then  ! nombre d'iterations max atteint

      criter_arret = pm_criter_arret_iter_max
      nb_racine = pm_1racine
      sol = BB 
      go to 6000

   end if

   s1=sign(1._pm_reel, fb)
   s3=sign(1._pm_reel, fc)

   if (s1*s3 > 0._pm_reel) then
      c = AA
      fc = fa
      d = BB - AA
      e = d
   end if

   if (abs(fc) < abs(fb)) then 
      AA = BB
      BB = c
      c = AA
      fa = fb
      fb = fc
      fc = fa
   end if

   tols=2._pm_reel * eps100 * abs(BB) + 0.5_pm_reel * err_abs
   xm = 0.5_pm_reel * (c - BB)

end do

! =======================================
! le critere de convergence est satisfait
! =======================================

nb_racine = pm_1racine    
sol = BB
criter_arret = pm_criter_arret_x

6000 continue

end subroutine mui_integ_racine
