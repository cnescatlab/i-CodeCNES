subroutine mr_TerVrai_J2000 ( model, jul1950, delta_tu1, delta_tai, pos_TerVrai, pos_J2000, code_retour, &
                              vit_TerVrai, vit_J2000, jacob )

! (C) Copyright CNES - MSLIB - 2002

!************************************************************************
!
! But:  Passage du repere terrestre vrai a la date t au repere equtorial moyen J2000 
! ===
!
! Note d'utilisation:  [DR1] "Les systemes de reference utilises en astronomie"
! ===================        de M. Chapront-Touze, G. Francou et B. Morando
!                            Bureau Des Longitudes (BDL) novembre 1994
!                            ISSN 1243-4272
!                            ISBN 2-910015-05-X
!                            nomenclature MSLIB M-NT-0-160-CN
!                      Nota: Les epoques B1900, J1900 et J2000 sont explicitees en
!                            page 23 table 1.1
!
!                      Les vitesses declarees en sorties optionnelles ne peuvent etre calculees que si les vitesses declarees
!                      en entree optionnelles sont specifiees.
!
!$Historique
! ==========
!   + Version 4.0 (SP 459 ed01 rev00): creation a partir de rien
!                         (Date: 11/2002 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 484 ed01 rev00): calcul de la jacobienne 
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : utilisation de matmul3
!                   (Date: 03/2008 - Realisation: Atos origin)
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
use int_rep_fondamentaux, only : mr_TerVrai_EquaVrai
use int_rep_fondamentaux, only : mr_EquaVrai_EquaMoy
use int_rep_fondamentaux, only : mr_EquaMoy_J2000

use type_mslib
use parametre_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model       ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai   ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_TerVrai ! vecteur position dans le repere terrestre vrai a la date t 
real(pm_reel),        dimension(3), intent(out)           :: pos_J2000   ! vecteur position dans le repere equatorial moyen J2000
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_TerVrai ! vecteur vitesse dans le repere terrestre vrai a la date t 
real(pm_reel),        dimension(3), intent(out), optional :: vit_J2000   ! vecteur vitesse dans le repere equatorial moyen J2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel),        dimension(3) :: pos_EquaVrai, vit_EquaVrai ! vecteurs position/vitesse intermediaires
real(pm_reel),        dimension(3) :: pos_EquaMoy, vit_EquaMoy   ! vecteurs position/vitesse intermediaires
real(pm_reel),      dimension(6,6) :: jacob_1, jacob_2, jacob_3, jacob_int  ! jacobiennes intermediaires
type(tm_code_retour)               :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_TerVrai_J2000.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id = &
              ' $Id: mr_TerVrai_J2000.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! test sur la coherence des entrees/sorties optionnelles si precisees

if ((present(vit_J2000)) .and. (.not. present(vit_TerVrai))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_TerVrai)) .and. (.not. present(vit_J2000))) then
   code_retour%valeur = pm_warn_para_option
end if

! test sur le modele choisi 

if (model /= pm_lieske_wahr) then

   code_retour%valeur = pm_err_ind_model
   go to 6000

end if

if (present(jacob)) then               ! avec la jacobienne

   if (present(vit_J2000)) then        ! avec les vitesses

      !  calcul du passage du repere terrestre vrai au repere celeste vrai a la date t

      call mr_TerVrai_EquaVrai ( model, jul1950, delta_tu1, delta_tai, pos_TerVrai, pos_EquaVrai,                     &
           code_retour_local, vit_TerVrai=vit_TerVrai, vit_EquaVrai=vit_EquaVrai, jacob=jacob_1 )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

      !  calcul du passage du repere celeste vrai au repere celeste moyen a la date t

      call mr_EquaVrai_EquaMoy ( model, jul1950, delta_tai, pos_EquaVrai, pos_EquaMoy,                         &
           code_retour_local, vit_EquaVrai=vit_EquaVrai, vit_EquaMoy=vit_EquaMoy, jacob=jacob_2 )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

      !  calcul du passage du repere celeste moyen a la date t au repere J2000

      call mr_EquaMoy_J2000 (model, jul1950, delta_tai, pos_EquaMoy, pos_J2000,                                 &
           code_retour_local, vit_EquaMoy=vit_EquaMoy, vit_J2000=vit_J2000, jacob=jacob_3 )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

   else                                ! sans les vitesses

      !  calcul du passage du repere Terrestre Vrai au repere celeste vrai a la date t

      call mr_TerVrai_EquaVrai ( model, jul1950, delta_tu1, delta_tai, pos_TerVrai, pos_EquaVrai, code_retour_local, jacob=jacob_1 )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

      !  calcul du passage du repere celeste vrai au repere celeste moyen a la date t

      call mr_EquaVrai_EquaMoy ( model, jul1950, delta_tai, pos_EquaVrai, pos_EquaMoy, code_retour_local, jacob=jacob_2 )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

      !  calcul du passage du repere celeste moyen a la date t au repere J2000

      call mr_EquaMoy_J2000 (model, jul1950, delta_tai, pos_EquaMoy, pos_J2000, code_retour_local, jacob=jacob_3 )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

   end if

   call mu_matmul6(jacob_2,jacob_1,jacob_int,code_retour_local)
   call mu_matmul6(jacob_3,jacob_int,jacob,code_retour_local)

else                                   ! sans la jacobienne

   if (present(vit_J2000)) then        ! avec les vitesses

      !  calcul du passage du repere terrestre vrai au repere celeste vrai a la date t

      call mr_TerVrai_EquaVrai ( model, jul1950, delta_tu1, delta_tai, pos_TerVrai, pos_EquaVrai,                     &
           code_retour_local, vit_TerVrai=vit_TerVrai, vit_EquaVrai=vit_EquaVrai )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

      !  calcul du passage du repere celeste vrai au repere celeste moyen a la date t

      call mr_EquaVrai_EquaMoy ( model, jul1950, delta_tai, pos_EquaVrai, pos_EquaMoy,                         &
           code_retour_local, vit_EquaVrai=vit_EquaVrai, vit_EquaMoy=vit_EquaMoy )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

      !  calcul du passage du repere celeste moyen a la date t au repere J2000

      call mr_EquaMoy_J2000 (model, jul1950, delta_tai, pos_EquaMoy, pos_J2000,                                 &
           code_retour_local, vit_EquaMoy=vit_EquaMoy, vit_J2000=vit_J2000)

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

   else                                ! sans les vitesses

      !  calcul du passage du repere Terrestre Vrai au repere celeste vrai a la date t

      call mr_TerVrai_EquaVrai ( model, jul1950, delta_tu1, delta_tai, pos_TerVrai, pos_EquaVrai, code_retour_local )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

      !  calcul du passage du repere celeste vrai au repere celeste moyen a la date t

      call mr_EquaVrai_EquaMoy ( model, jul1950, delta_tai, pos_EquaVrai, pos_EquaMoy, code_retour_local )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

      !  calcul du passage du repere celeste moyen a la date t au repere J2000

      call mr_EquaMoy_J2000 (model, jul1950, delta_tai, pos_EquaMoy, pos_J2000, code_retour_local )

      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000  ! sortie si erreur
      end if

   end if

end if

! Fin de corps du programme
! =========================

6000 continue

code_retour%routine = pm_num_mr_TerVrai_J2000
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_TerVrai_J2000
