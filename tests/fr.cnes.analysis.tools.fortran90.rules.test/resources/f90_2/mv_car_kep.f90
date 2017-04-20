subroutine mv_car_kep (mu, pos_car, vit_car, kep, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres cartesiens (x,y,z,vx,vy,vz) aux parametres kepleriens(a ou p,e,i,pomega,gomega,m)  
! ===  
!
! Note d'utilisation: 0) Applicable aux cas elliptique, hyperbolique, et parabolique.
! ==================  1) Seulement applicable a une orbite non circulaire et non equatoriale.
!                        Pour les orbites circulaires et/ou equatoriales, voir les routines: mv_car_cir_equa, mv_car_cir 
!                        et mv_car_equa.
!                     2) kep%a represente en sortie le demi grand axe dans le cas de l'ellipse et de l'hyperbole, 
!                        et le parametre p (2 x la distance au foyer) dans le cas de la parabole.
!                     3) Les unites en entree doivent etre coherentes entre elles. Ex.: pos_car en metres, vit_car en m/s,
!                        mu en m**3/s**2, et les parametres kepleriens seront en metres et radians.
!                     4) L'element (i,j) de la jacobienne correspond a la derivee partielle du parametre keplerien numero i
!                        par rapport a la derivee partielle du parametre cartesien numero j.
!                     5) La transformation inverse se fait par mv_kep_car.
!$Historique
! ==========
!   + Version 1.0 (SP 226 ed01 rev00): creation a partir de la routine MVREDO de la MSLIB f77
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 345 ed01 rev00): Ajout du test (e < pm_eps_cir) 
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 4.1 (FA 487 ed01 rev00): Corrections pour excentricite faible
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

use int_var_internes, only : mvi_car_kep_ellip
use int_var_internes, only : mvi_car_kep_hyperb
use int_var_internes, only : mvi_car_kep_parab
use int_utilitaires, only : mu_prod_vect
use int_utilitaires, only : mu_norme
use int_util_internes, only : mui_dot_product3

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
                                                       
real(pm_reel), intent(in)                            :: mu     ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(in)              :: pos_car! vecteur position du satellite
real(pm_reel), dimension(3), intent(in)              :: vit_car! vecteur vitesse du satellite
type(tm_orb_kep),intent(out)                         :: kep    ! parametres kepleriens
type(tm_code_retour),intent(out)                     :: code_retour
real(pm_reel), dimension(6,6),intent(out),optional   :: jacob  ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

!     declaration des variables locales
!     ---------------------------------

real(pm_reel)                   ::    norme_vect_pos, norme_vect_vit ! norme du rayon vecteur et norme de la vitesse
real(pm_reel)                   ::    norme_vect_vit_carre           ! carre de la vitesse
real(pm_reel)                   ::    norme_moment_cinetique         ! norme du moment cinetique
real(pm_reel), dimension(3)     ::    moment_cinetique               ! moment cinetique du satellite
real(pm_reel)                   ::    pos_fois_vit                   ! produit scalaire position*vitesse
real(pm_reel)                   ::    e2                             ! carre de l'excentricite
real(pm_reel)                   ::    parametre                      ! parametre p
real(pm_reel)                   ::    un_sur_a                       ! inverse du demi-grand-axe
real(pm_reel)                   ::    excentricite                   ! excentricite
real(pm_reel)                   ::    eps100                         ! variable epsilon machine * 100 
real(pm_reel)                   ::    coefk,a                        ! variables intermediaires
real(pm_reel)                   ::    mu_retour

integer                         ::    retour

intrinsic abs, sqrt, epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mv_car_kep.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_car_kep.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

! controle des donnees
! --------------------

! 1) constante de la gravitation

if (mu <= eps100) then
   if (mu < 0._pm_reel) then                                     ! constante de gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else                                                          ! constante de gravitation proche de 0
      code_retour%valeur = pm_err_mu_nul 
   end if
   go to 6000
end if

!) vecteurs positions-vitesse

call mu_norme(pos_car, norme_vect_pos, code_retour)
if (norme_vect_pos < eps100) then                                ! vecteur position  nul
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if

call mu_norme(vit_car, norme_vect_vit, code_retour)
if (norme_vect_vit < eps100) then                                ! vecteur vitesse  nul
   code_retour%valeur = pm_err_vit_nul
   go to 6000
end if

call mu_prod_vect(pos_car, vit_car, moment_cinetique, code_retour)
call mu_norme(moment_cinetique, norme_moment_cinetique, code_retour)
if (norme_moment_cinetique < eps100) then                        ! norme du moment cinetique (position  x vitesse) nulle
   code_retour%valeur = pm_err_pos_vit_colineaire
   go to 6000
end if

! calcul de variables intermediaires
! ----------------------------------
! produit scalaire position*vitesse
call mui_dot_product3 ( pos_car , vit_car , pos_fois_vit , mu_retour )

parametre = norme_moment_cinetique**2 / mu                       ! parametre p = (r x v)**2/mu = c**2/mu ; p > 0 car mu > 0
norme_vect_vit_carre=norme_vect_vit*norme_vect_vit
un_sur_a = 2._pm_reel/norme_vect_pos- norme_vect_vit_carre/mu    ! 1/a

coefk = 0.5_pm_reel*norme_vect_vit_carre - mu/norme_vect_pos     ! excentricite 
e2 = 1._pm_reel + 2._pm_reel*parametre*coefk/mu                  ! formulation valable pour tout type d'orbite: k = v**2/2 - |mu|/r

if (e2 < pm_eps_e_faible*pm_eps_e_faible) then
   code_retour%valeur = pm_err_e_faible
   go to 6000
else
   excentricite=sqrt(e2)                                         ! e**2 = 1 + 2.c**2.k/mu**2
end if

! ***********************************************
! calcul transformation cartesiens --> kepleriens
! selon le type d'orbite
! ***********************************************
 
if (excentricite <= (1._pm_reel- pm_eps_parab)) then             ! orbite elliptique

   if (un_sur_a < eps100) then
      if (un_sur_a < 0._pm_reel) then
         code_retour%valeur = pm_err_a_negatif                   ! 1/a , donc a, est negatif
         go to 6000
      else
         code_retour%valeur = pm_err_a_infini                    ! 1/a proche de 0, donc a infini
         go to 6000
      end if
   end if

   a = 1._pm_reel/un_sur_a                                       ! 1/a > 0 strictement a ce stade des calculs
   if (a <= eps100) then                                         ! a est proche de 0
      code_retour%valeur = pm_err_a_nul
      go to 6000
   end if

   if (present(jacob)) then
      call mvi_car_kep_ellip (pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_vect_vit,&
           un_sur_a,excentricite,pos_fois_vit,mu,kep,retour,jacob=jacob)
   else
      call mvi_car_kep_ellip (pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_vect_vit,&
           un_sur_a,excentricite,pos_fois_vit,mu,kep,retour)
   end if
   code_retour%valeur = retour

else if (excentricite >= (1._pm_reel+ pm_eps_parab)) then        ! orbite hyperbolique

   un_sur_a = abs(un_sur_a)                                      ! on utilise la valeur absolue de 1/a dans le cas hyperbolique

   if (un_sur_a < eps100) then                                   ! a infini
      code_retour%valeur = pm_err_a_infini
      go to 6000
   end if

   a = 1._pm_reel/un_sur_a

   if (a <= eps100) then                                         ! a est nul
      code_retour%valeur = pm_err_a_nul
      go to 6000
   end if

   if (present(jacob)) then
      call mvi_car_kep_hyperb(pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_vect_vit,norme_moment_cinetique,&
           un_sur_a,excentricite, parametre,pos_fois_vit,mu,kep,retour,jacob=jacob)
   else
      call mvi_car_kep_hyperb(pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_vect_vit,norme_moment_cinetique,&
           un_sur_a,excentricite, parametre,pos_fois_vit,mu,kep,retour)
   end if
   code_retour%valeur = retour

else   ! orbite parabolique (excentricite.gt.(1.- pm_eps_parab) .and. excentricite < (1.+ pm_eps_parab))

   excentricite = 1._pm_reel
   ! parametre > 0 par calcul

   if (present(jacob)) then
      call mvi_car_kep_parab(pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_moment_cinetique,&
           excentricite, parametre,pos_fois_vit,mu,kep,retour,jacob=jacob)
   else
      call mvi_car_kep_parab(pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_moment_cinetique,&
           excentricite, parametre,pos_fois_vit,mu,kep,retour)
   end if
   if (retour == 0 ) then
      code_retour%valeur = pm_warn_e_parab
   else
      code_retour%valeur = retour
   end if

end if

6000 continue

code_retour%routine = pm_num_mv_car_kep
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_car_kep
