subroutine mv_car_equa (mu, pos_car, vit_car, equa, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres CARtesiens aux parametres orbitaux adaptes a l'orbite EQUAtoriale
! ===   (a ou p, e, pom+gom, 2sin(i/2)cos(gom), 2sin(i/2)sin(gom), M )
!
! Note d'utilisation: 0) Applicable aux cas elliptique, hyperbolique, et parabolique.
! ==================  1) Dans le cas elliptique, seulement applicable a une orbite NON circulaire. 
!                        Pour les orbites circulaires voir la routine: mv_car_cir_equa
!                     2) kep%a represente en sortie le demi grand axe dans le cas de l'ellipse et de l'hyperbole, 
!                        et le parametre p (2 x la distance au foyer) dans le cas de la parabole.
!                     3) Les unites en entree doivent etre coherentes entre elles. Ex.: pos_car en metres, vit_car en m/s,
!                        mu en m**3/s**2, et les parametres orbitaux adaptes a l'orbite equatoriale seront en metres et radians.
!                     4) L'element (i,j) de la jacobienne correspond a la derivee partielle du parametre adapte a l'orbite
!                         equatoriale numero i par rapport a la derivee partielle du parametre cartesien numero j.
!                     5) La transformation inverse se fait par mv_equa_car.
!$Historique
! ==========
!   + Version 1.0 (SP 236 ed01 rev00): creation a partir de la routine MVREEQ de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 359 ed01 rev00): ajout du test sur h**2 > 4*(1 - 100*epsilon(1._pm_reel))
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 4.1 (FA 488 ed01 rev00): Corrections pour excentricite faible
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
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
use test_mslib

use int_utilitaires, only : mu_norme
use int_utilitaires, only : mu_prod_vect
use int_var_internes, only : mvi_car_equa_ellip
use int_var_internes, only : mvi_car_equa_hyperb
use int_var_internes, only : mvi_car_equa_parab
use int_util_internes, only : mui_dot_product3

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)              :: mu  ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(in):: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(in):: vit_car  ! vecteur vitesse du satellite
type(tm_orb_equa),intent(out)          :: equa     ! parametres orbitaux de l'orbite equatoriale
type(tm_code_retour), intent(out)      ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), dimension(3) :: rrxv                  ! rrxv :produit vectoriel position x vitesse
real(pm_reel) :: un_sur_a                            ! 1/a
real(pm_reel) :: excentricite
real(pm_reel) :: parametre 
real(pm_reel) :: norme_vect_pos, norme_vect_vit, norme_moment_cinetique  !normes des vecteurs position,
                                                                         ! vitesse, et du produit vectoriel de ces derniers
real(pm_reel) :: rscal                               ! produit scalaire position*vitesse     
real(pm_reel) :: eps100                              ! variable epsilon machine * 100 
real(pm_reel) :: vn2, rrxvn2, e2, coefk              ! carre des normes calcules et de l'excentricite et coefficient
real(pm_reel) :: h2                                  ! carre de la norme du vecteur inclinaison
real(pm_reel) :: mu_retour

integer       :: retour

intrinsic epsilon, present, huge

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_car_equa.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_car_equa.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

! controle des donnees
!--------------------

if (mu <= eps100) then
   if (mu < 0._pm_reel) then                  ! constante de la gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else
      code_retour%valeur = pm_err_mu_nul      ! constante de la gravitation nulle
   end if
   go to 6000
end if

call mu_norme(pos_car, norme_vect_pos, code_retour)
if (norme_vect_pos < eps100) then             ! vecteur position nul
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if

call mu_norme(vit_car, norme_vect_vit, code_retour)
if (norme_vect_vit < eps100) then             ! vecteur vitesse nul
   code_retour%valeur = pm_err_vit_nul
   go to 6000
end if
vn2=norme_vect_vit*norme_vect_vit

call mu_prod_vect(pos_car, vit_car, rrxv, code_retour)
call mu_norme(rrxv, norme_moment_cinetique, code_retour)
if (norme_moment_cinetique < eps100) then     ! vecteurs position et vitesse colineaires
   code_retour%valeur = pm_err_pos_vit_colineaire
   go to 6000
end if
rrxvn2   = norme_moment_cinetique*norme_moment_cinetique

! test si l'orbite est equatoriale retrograde (i = pi)
! le vecteur inclinaison (hx,hy) n'est pas defini
h2 = 2._pm_reel * (1._pm_reel - rrxv(3))/norme_moment_cinetique

if (h2 > (4._pm_reel*(1._pm_reel - eps100))) then       ! la definition du vecteur inclinaison (hx,hy) impose que l'inclinaison i soit
   code_retour%valeur = pm_err_i_equa_retro  ! inferieure a (pi - epsilon_retro) -> test si h**2 <= 4*(cos(epsilon_retro/2)**2)
   go to 6000                                ! soit h**2 <= 4*(1-100*epsilon) si epsilon_retro = 20*sqrt(epsilon)
end if

! calcul de variables intermediaires
! ----------------------------------
! produit scalaire position*vitesse
call mui_dot_product3 ( pos_car , vit_car , rscal , mu_retour )

parametre = rrxvn2 / mu                         ! parametre p=||r x v||**2/mu = c**2/mu  ; p>0 par calcul

un_sur_a = 2._pm_reel/norme_vect_pos- vn2/mu    ! un_sur_a = 1._pm_reel/a

coefk = 0.5_pm_reel*vn2 - mu/norme_vect_pos     ! k = v**2/2 - mu/r

e2 = 1._pm_reel + 2._pm_reel*parametre*coefk/mu ! e**2 = 1 + 2.c**2.k/mu**2
                                                ! formulation valable pour tout type d'orbite

if (e2 < pm_eps_e_faible*pm_eps_e_faible) then

   code_retour%valeur = pm_err_e_faible
   go to 6000

else
   excentricite = sqrt( e2 )
end if

! ***********************************************
! calcul transformation cartesiens --> orbitaux
! selon le type d'orbite
! ***********************************************

if (excentricite <= (1._pm_reel- pm_eps_parab)) then

   ! orbite elliptique
   ! =================

   if (un_sur_a <= eps100) then   
      if (un_sur_a < 0._pm_reel) then           ! a est negatif
         code_retour%valeur = pm_err_a_negatif
      else                                      ! a est infini
         code_retour%valeur = pm_err_a_infini
      end if
      go to 6000
   else if (un_sur_a >= huge(1._pm_reel)) then  ! a est nul
         code_retour%valeur = pm_err_a_nul
         go to 6000
   end if

   if (present(jacob)) then
      call mvi_car_equa_ellip (pos_car, vit_car,rrxv,norme_vect_pos, norme_vect_vit, norme_moment_cinetique,un_sur_a,&
                               excentricite,rscal,mu,equa,retour,jacob=jacob)
   else  
      call mvi_car_equa_ellip (pos_car, vit_car,rrxv,norme_vect_pos, norme_vect_vit, norme_moment_cinetique,un_sur_a,&
                               excentricite,rscal,mu,equa,retour)
   end if

else if (excentricite >= (1._pm_reel+ pm_eps_parab)) then

   !  orbite hyperbolique
   !  ===================

   un_sur_a = abs(un_sur_a)                     ! on utilise la valeur absolue de 1/a dans le cas hyperbolique

   if (un_sur_a <= eps100) then                 ! a est infini 
      code_retour%valeur = pm_err_a_infini
      go to 6000
   else if (un_sur_a >= huge(1._pm_reel)) then  ! a est nul
      code_retour%valeur = pm_err_a_nul
      go to 6000
   end if

   if (present(jacob)) then
      call mvi_car_equa_hyperb (pos_car, vit_car,rrxv,norme_vect_pos, norme_vect_vit, norme_moment_cinetique,un_sur_a,&
                                excentricite, parametre,rscal,mu,equa,retour,jacob=jacob)
   else  
      call mvi_car_equa_hyperb (pos_car, vit_car,rrxv,norme_vect_pos, norme_vect_vit, norme_moment_cinetique,un_sur_a,&
                                excentricite, parametre,rscal,mu,equa,retour)
   end if

else           ! (excentricite > (1._pm_reel- pm_eps_parab).and.excentricite < (1._pm_reel+ pm_eps_parab))

   ! orbite parabolique
   ! ==================

   ! parametre p > 0 par calcul
   excentricite = 1._pm_reel
   if (present(jacob)) then
      call mvi_car_equa_parab (pos_car, vit_car,rrxv,norme_vect_pos,norme_moment_cinetique,&
                               excentricite, parametre,rscal,mu,equa,retour,jacob=jacob)
   else  
      call mvi_car_equa_parab (pos_car, vit_car,rrxv,norme_vect_pos,norme_moment_cinetique,&
                               excentricite, parametre,rscal,mu,equa,retour)
   end if
   if (retour == 0) retour = pm_warn_e_parab

end if
code_retour%valeur = retour

6000 continue

code_retour%routine = pm_num_mv_car_equa
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_car_equa
