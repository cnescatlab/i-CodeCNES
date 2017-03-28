subroutine mv_equa_car ( mu, equa, pos_car, vit_car, code_retour, jacob )

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Passage des parametres orbitaux dits adaptes aux orbites EQUAtoriales non circulaires
! ===   aux parametres CARtesiens
!
! Note d'utilisation:  1) Applicable aux orbites elliptiques, hyperboliques, et paraboliques (equatoriales ou non).
! ==================   
!                      2) Pour les orbites equatoriales ET circulaires  voir la routine: mv_cir_equa_car 
!                      3) equa%a represente en sortie le demi grand axe dans le cas de l'ellipse et de l'hyperbole, 
!                         et le parametre p (2 x la distance au foyer) dans le cas de la parabole.
!                      4) Les unites en entree doivent etre coherentes entre elles. Ex.: 
!                         mu en m**3/s**2 et les parametres dits adaptes aux orbites equatoriales non circulaires
!                         en metres et radians -> les coordonees cartesiennes seront en m, m/s
!                      5) L'element (i,j) de la jacobienne correspond a la derivee partielle du parametre cartesien numero i
!                         par rapport a la derivee partielle du parametre dit adaptes aux orbites 
!                         equatoriales non circulaires numero j.
!                      6) La transformation inverse peut se faire par mv_car_equa.
!
!$Historique
! ==========
!   + Version 1.0 (SP 240 ed01 rev00): creation a partir de la routine MVOSRQ de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 361 ed01 rev00): ajout de commentaires sur le parametre p
!                                      introduction du parametre pm_err_i_equa_retro : h**2 > 4*(1 - 100*epsilon(1._pm_reel))
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use int_var_internes, only : mvi_equa_car_ellip
use int_var_internes, only : mvi_equa_car_hyperb
use int_var_internes, only : mvi_equa_car_parab

use test_mslib

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       

real(pm_reel), intent(in)                :: mu  ! constante de la gravitation universelle
type(tm_orb_equa),intent(in)             :: equa     ! parametres orbitaux de l'orbite equatoriale
real(pm_reel), dimension(3), intent(out) :: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out) :: vit_car  ! vecteur vitesse du satellite
type(tm_code_retour), intent(out)        ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)   :: aoup,exc           !     parametres a ou p, excentricite
real(pm_reel)   :: rnorm              !     carre norme vecteur inclinaison 
real(pm_reel)   :: eps100             !     variable epsilon*100
integer         :: retour

intrinsic epsilon, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_equa_car.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_equa_car.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

aoup   = equa%a
exc    = equa%e
rnorm  = equa%ix*equa%ix+equa%iy*equa%iy

!====================
! controle des donnees
!====================

if (mu <= eps100) then
   if (mu < 0._pm_reel) then                 ! constante de la gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else
      code_retour%valeur = pm_err_mu_nul     ! constante de la gravitation nulle
   end if
   go to 6000
end if

if (aoup <= eps100) then
   if (aoup < 0._pm_reel) then                       ! demi-grand axe ou parametre de la parabole < 0.
      code_retour%valeur= pm_err_a_negatif
      go to 6000
   else                                      ! demi-grand axe ou parametre de la parabole = 0.
      code_retour%valeur= pm_err_a_nul
      go to 6000
   end if
end if

if (exc < 0._pm_reel) then                   ! excentricite negative
   code_retour%valeur = pm_err_e_negatif
   go to 6000
end if

if (rnorm > 4._pm_reel) then                 ! ==> inclinaison i =  pi : impossible
   code_retour%valeur = pm_err_ix_iy_sup2
   go to 6000
end if
if (rnorm > (4._pm_reel*(1._pm_reel - eps100))) then    ! la definition du vecteur inclinaison (hx,hy) impose que l'inclinaison i soit
   code_retour%valeur = pm_err_i_equa_retro  ! inferieure a (pi - epsilon_retro) -> test si h**2 <= 4*(cos(epsilon_retro/2)**2)
   go to 6000                                ! soit h**2 <= 4*(1-100*epsilon) si epsilon_retro = 20*sqrt(epsilon) 
end if

!*********************************************************
! passage orbitaux ---> cartesiens selon le type d'orbite
!*********************************************************

if (exc <= (1._pm_reel- pm_eps_parab)) then

   !     --------------
   !     cas elliptique
   !     --------------
   if (present(jacob)) then
      call mvi_equa_car_ellip (equa,mu,pos_car, vit_car,retour,jacob=jacob)
   else
      call mvi_equa_car_ellip (equa,mu,pos_car, vit_car,retour)
   end if

   !  en retour de mvi_equa_car_ellip: seuls retour = 0, ou pm_err_conv_kepler_ellip issu de mv_kepler_std possible ici

else if (exc >= (1._pm_reel+ pm_eps_parab)) then

   !     ----------------
   !     cas hyperbolique
   !     ----------------

   if (present(jacob)) then
      call mvi_equa_car_hyperb (equa,mu,pos_car, vit_car,retour,jacob=jacob)
   else
      call mvi_equa_car_hyperb (equa,mu,pos_car, vit_car,retour)
   end if

   !  en retour de mvi_equa_car_hyperb: seuls retour = 0, ou pm_err_conv_kepler_hyperb issu de mvi_kepler_hyperb possible ici

else
   !             ((exc > (1._pm_reel-pm_eps_parab)).and.(exc < (1._pm_reel+ pm_eps_parab))

   !     ---------------
   !     cas parabolique
   !     ---------------
   if (present(jacob)) then
      call mvi_equa_car_parab (equa, mu, pos_car, vit_car, retour, jacob=jacob )
   else
      call mvi_equa_car_parab (equa, mu, pos_car, vit_car, retour)
   end if
   if (retour == 0) retour = pm_warn_e_parab

end if
code_retour%valeur = retour

6000 continue

code_retour%routine = pm_num_mv_equa_car
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_equa_car
