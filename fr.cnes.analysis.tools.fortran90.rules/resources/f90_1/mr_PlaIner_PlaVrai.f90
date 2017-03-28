subroutine mr_PlaIner_PlaVrai (planete, modeleUAI, long, pos_PlaIner, pos_PlaVrai, code_retour, &
          vit_rot, jul1950, vit_PlaIner, vit_PlaVrai, jacob)

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Passage du repere planetocentrique inertiel "H0-n" pour n=0
! ===   au repere planetocentrique vrai
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!   + Version 5.0 (SP 612 ed01 rev00): creation
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 6.4 : corrections qualite
!                   (Date: 06/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use int_chgmnt_reperes, only : mt_iner_ref
use int_rep_internes, only : mri_def_rep_UAI

use type_mslib
use parametre_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                       intent(in)            ::  planete          ! planete
integer,                       intent(in)            ::  modeleUAI        ! modele UAI definissant la vitesse de rotation
real(pm_reel),                 intent(in)            ::  long             ! longitude du repere
real(pm_reel), dimension(3),   intent(in)            ::  pos_PlaIner      ! position dans le repere planetocentrique Inertiel
real(pm_reel), dimension(3),   intent(out)           ::  pos_PlaVrai      ! position dans le repere planetocentrique Vrai
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  vit_rot          ! vitesse de rotation de la planete
type(tm_jour_sec),             intent(in),  optional ::  jul1950          ! date pour la vit de rotation si planete = Neptune
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_PlaIner      ! vitesse dans le repere  planetocentrique Inertiel
real(pm_reel), dimension(3),   intent(out), optional ::  vit_PlaVrai      ! position dans le repere planetocentrique Vrai
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob            ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel)                :: dW                     ! derivee de la longitude du meridien origine 
real(pm_reel)                :: alpha0, delta0, W      ! pour l'appel a mri_def_rep_UAI
real(pm_reel)                :: sec                    ! pour l'appel a mt_iner_ref
type(tm_jour_sec)            :: date                   ! date en entree pour utilisation des modeles
type(tm_code_retour)         :: code_retour_local
integer                      :: retour_local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_PlaIner_PlaVrai.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_PlaIner_PlaVrai.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! Verifications
! ===============

! modeles
if ((modeleUAI < pm_UAI_autre_modele).OR.(modeleUAI > pm_UAI2000)) then
   code_retour%valeur = pm_err_ind_model
   go to 6000
end if

! date si planete = Neptune
if ((planete == pm_pla_neptune).AND.(modeleUAI /= pm_UAI_autre_modele).AND..NOT.(present(jul1950))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

! Calculs
! =======

! determination de la vitesse de rotation de l'astre
if ( modeleUAI == pm_UAI_autre_modele) then   ! modele fourni par l'utilisateur

   if (present(vit_rot))  then
      dW = vit_rot
   else
      code_retour%valeur = pm_err_para_option
      go to 6000
   end if

else    ! modele predefini. On recupere la vitesse de rotation

   if ((planete == pm_pla_neptune).AND.(modeleUAI /= pm_UAI_autre_modele)) then
   ! seul cas ou la date intervient dans le calcul de la derivee du temps sideral
      date%jour = jul1950%jour
      date%sec = jul1950%sec
   else ! la date n'a aucune importance dans les autres cas
      date%jour = 0_pm_entier
      date%sec = 0._pm_reel
   end if
      
   call  mri_def_rep_UAI ( planete, modeleUAI, date, alpha0, delta0, W, dW, retour_local )
   if (retour_local /= pm_OK) then
      code_retour%valeur = retour_local
      if (retour_local < pm_OK)  go to 6000
   end if

end if

! appel a mt_iner_ref
sec = 0._pm_reel   ! on n'introduit pas de decalage de la longitude de depart
if (present(vit_PlaIner).AND.present(vit_PlaVrai)) then ! avec les vitesses
   if (present(jacob)) then
      call mt_iner_ref (dW, long, sec, pos_PlaIner, pos_PlaVrai, code_retour_local, &
           vit_iner = vit_PlaIner, vit_ref = vit_PlaVrai, jacob = jacob)
   else
      call mt_iner_ref (dW, long, sec, pos_PlaIner, pos_PlaVrai, code_retour_local,  &
           vit_iner = vit_PlaIner, vit_ref = vit_PlaVrai)
   end if
else if (.NOT.present(vit_PlaIner).AND.present(vit_PlaVrai)) then
   code_retour%valeur = pm_err_para_option
   go to 6000
else   ! sans les vitesses
   if (present(vit_PlaIner).AND..NOT.present(vit_PlaVrai))  code_retour%valeur = pm_warn_para_option
   if (present(jacob)) then
      call mt_iner_ref (dW, long, sec, pos_PlaIner, pos_PlaVrai, code_retour_local, jacob = jacob)
   else
      call mt_iner_ref (dW, long, sec, pos_PlaIner, pos_PlaVrai, code_retour_local)
   end if
end if

if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

6000 continue

code_retour%routine = pm_num_mr_PlaIner_PlaVrai
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_PlaIner_PlaVrai
