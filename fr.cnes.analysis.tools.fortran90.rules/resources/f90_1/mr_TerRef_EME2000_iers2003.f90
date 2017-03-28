subroutine mr_TerRef_EME2000_iers2003(jul1950, Xp, Yp, delta_tu1, delta_te, dX, dY, pos_Ref, pos_EME2000, &
     code_retour, s_prime, vit_Ref, vit_EME2000, jacob)

! (C) Copyright CNES - MSLIB - 2008

!************************************************************************
!
! But:  Passage du terrestre de reference (IRTF) au repere equatorial moyen EME2000
!       d'après la définition de l'IERS2003. Seul le cas inertiel est traite,
!       le changement de repere TerRef->TerVrai ne permettant pas le calcul non inertiel. 
!
!
! $Remarques : ce code est couvert par la DV BIBMS n°18 (Code lié aux changements de repères de l'IERS 2003 (MSLIB90))
!              Plus d'informations sur ces routines est disponible dans la note algorithmique du thème R de la MSLIB90
!              -> BIBMS-SME-19-2025-ATOS
! 
! ===
!
!$Historique
! ==========
!   + Version 6.9 : DM-ID 1092 Création
!                   (Date: 07/2008 - Realisation: Atos origin)
!   + Version 6.10 : AQ : application de la DV 18
!                   (Date: 10/2008 - Realisation: Atos origin)
!                     FA-ID 1217: Correction du calcul de la jacobienne
!                        et suppression du parametre optionnel "inertiel".
!                        Seul le cas inertiel est traite.
!                   (Date: 02/2009 - Realisation: Atos origin)
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

use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use int_rep_fondamentaux, only : mr_TerRef_TerVrai_iers2003,&
                                 mr_TerVrai_CIP_iers2003,&
				 mr_CIP_EME2000_iers2003

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jours,sec)
real(pm_reel),                      intent(in)            :: Xp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 6 du bulletin A
real(pm_reel),                      intent(in)            :: Yp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 8 du bulletin A
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps de la
real(pm_reel),                      intent(in)            :: delta_te    ! ecart entre l'echelle de temps TE (ou TT) et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),                      intent(in)            :: dX          ! nutation  convertie en radian
                                                                         ! avant derniere colonne du bulletin A                                      
real(pm_reel),                      intent(in)            :: dY          ! nutation  convertie en radian
                                                                         ! derniere colonne du bulletin A									 									 
real(pm_reel),        dimension(3), intent(in)            :: pos_Ref     ! vecteur position dans le repere de référence
real(pm_reel),        dimension(3), intent(out)           :: pos_EME2000 ! vecteur position dans le repere equatorial moyen EME2000
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),                      intent(in),  optional :: s_prime     ! Parametre de correction s'
real(pm_reel),        dimension(3), intent(in) , optional :: vit_Ref     ! vecteur vitesse dans le repere de référence
real(pm_reel),        dimension(3), intent(out), optional :: vit_EME2000 ! vecteur vitesse dans le repere equatorial moyen EME2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel)                 :: s_prime_entree                 !correction s' intermediaire
real(pm_reel), dimension(6,6) :: jacob_ref_vrai,jacob_vrai_cip  ! Matrices jacobiennes intermediaires
real(pm_reel),  dimension(6,6):: jacob_cip_eme,jacob_tmp        !                 "
real(pm_reel), dimension(3)   :: pos_TerVrai,pos_CIP            ! vecteurs des positions intermediaires
real(pm_reel), dimension(3)   :: vit_entree,vit_TerVrai,vit_CIP ! vecteurs des vitesses intermediaires
real(pm_reel), dimension(3)   :: vit_EME2000_tmp                !                 "
logical                       :: inertiel                       ! indicateur vrai si calcul inertiel
type(tm_code_retour)          :: code_retour_local ! code retour local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mr_TerRef_EME2000_iers2003.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_TerRef_EME2000_iers2003.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK
code_retour_local%valeur = pm_OK

! Verifications
! ===============

! test sur la coherence des entrees/sorties optionnelles si precisees
if ((present(vit_EME2000)) .and. (.not. present(vit_Ref))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_Ref)) .and. (.not. present(vit_EME2000))) then
   code_retour%valeur = pm_warn_para_option
end if

! Calculs
! =======

! initialisation

! Cette boucle permet d'appeler les fonctions avec des vitesses meme si 
! l'utilisateur ne le souhaite pas
if (present(vit_EME2000)) then
  vit_entree(1)=vit_Ref(1) 
  vit_entree(2)=vit_Ref(2)
  vit_entree(3)=vit_Ref(3)
else
  vit_entree(1)=0._pm_reel 
  vit_entree(2)=0._pm_reel
  vit_entree(3)=0._pm_reel
end if

! On fait un calcul inertiel pour les changements de repère
! TerVrai->CIP et CIP->EME2000
inertiel=.true.

! Cette boucle permet d'appeler les fonctions avec s_prime meme si 
! l'utilisateur ne le souhaite pas
if (present(s_prime)) then
 s_prime_entree=s_prime
else
 s_prime_entree=0._pm_reel
end if

! Calcul de de toutes les sorties, demandées ou non

! TerRef -> TerVrai : calcul inertiel

call mr_TerRef_TerVrai_iers2003(Xp,Yp,pos_Ref,pos_TerVrai,code_retour_local,s_prime=s_prime_entree,&
                                vit_ref=vit_entree,vit_vrai=vit_TerVrai,jacob=jacob_ref_vrai)
if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = code_retour_local%valeur
    go to 6000
end if

! TerVrai -> CIP : calcul inertiel 

call mr_TerVrai_CIP_iers2003(jul1950,delta_tu1,pos_TerVrai,pos_CIP,code_retour_local,inertiel=inertiel,&
                             vit_TerVrai=vit_TerVrai,vit_CIP=vit_CIP,jacob=jacob_vrai_cip)
if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = code_retour_local%valeur
    go to 6000
end if

! CIP -> EME2000 : calcul inertiel

call mr_CIP_EME2000_iers2003(jul1950, delta_te, dX, dY, pos_CIP, pos_EME2000,code_retour_local,inertiel=inertiel,&
                             vit_CIP=vit_CIP, vit_EME2000=vit_EME2000_tmp, jacob=jacob_cip_eme)
if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = code_retour_local%valeur
    go to 6000
end if

! Mise en sortie des sorties optionelles

if (present(vit_EME2000)) then
  vit_EME2000(1)=vit_EME2000_tmp(1)
  vit_EME2000(2)=vit_EME2000_tmp(2)
  vit_EME2000(3)=vit_EME2000_tmp(3)
end if

if (present(jacob)) then

  ! JACOB_ref_eme = JACOB_cip_eme * JACOB_vrai_cip * JACOB_ref_vrai
  call mu_matmul6( jacob_cip_eme ,jacob_vrai_cip ,jacob_tmp ,code_retour_local)
  if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = pm_err_calc_mat
    go to 6000
  end if
  
  call mu_matmul6( jacob_tmp ,jacob_ref_vrai ,jacob ,code_retour_local)
  if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = pm_err_calc_mat
    go to 6000
  end if 
end if

6000 continue

code_retour%routine = pm_num_mr_TerRef_EME2000_iers
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_TerRef_EME2000_iers2003
