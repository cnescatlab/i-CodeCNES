subroutine mr_prec (model_prec, jul1950, prec, code_retour, delta_tai, deriv1_prec, deriv2_prec)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But: Calcul de la PRECession equatoriale : modele de precession de Lieske
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!   + Version 6.4 : DM-ID 426 : routine de calcul des angles de precession
!                 (Date: 04/2006 - Realisation: Claire Fabre - Atos Origin)
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
use parametres_internes_mslib
use type_mslib
use parametre_mslib



! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model_prec      ! indicateur du modele de precession 
type(tm_jour_sec), intent(in)       :: jul1950         ! date julienne
type(tm_prec), intent(out)          :: prec            ! precession equatoriale
type(tm_code_retour), intent(out)   :: code_retour
real(pm_reel), intent(in),  optional:: delta_tai       ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date jul1950
type(tm_prec), intent(out), optional:: deriv1_prec     ! derivee premiere de la precession
type(tm_prec), intent(out), optional:: deriv2_prec     ! derivee seconde de la precession

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
type(tm_jour_sec) :: jourTAI ! date en TAI
type(tm_jour_sec) :: jourTE  ! date en TE
real(pm_reel) :: tu, tu2, tu3 ! ecart entre le 01/01/2000 a 12h30 et la date courante (et ecart au carre, au cube
real(pm_reel) :: dzeta, z, teta ! parametres de la precession

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_prec.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_prec.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************
! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! controle des donnees
! ....................

if (model_prec /= pm_lieske) then          ! modele de precession inconnu
   code_retour%valeur = pm_err_ind_prec
   go to 6000
end if

! passage de la date t en date TE (inutile de normaliser la date ainsi obtenue)

! -> passage de T a TAI
jourTAI%jour = jul1950%jour
if (.not. present(delta_tai)) then
   jourTAI%sec  = jul1950%sec
else
   jourTAI%sec  = jul1950%sec + delta_tai
endif

! -> puis passage de TAI a TE
jourTE%jour = jourTAI%jour
jourTE%sec  = jourTAI%sec + pm_i_delta_TAI_TE

! nombre de milliers d'annees juliennes entre la date t et le 01-01-2000 a 12h
! (echelles TE)

tu  = ((real(jourTE%jour, pm_reel)) - pm_i_date_t2000 + jourTE%sec * pm_i_unsur86400) * pm_i_unsurKanJul
tu2 = tu * tu
tu3 = tu2* tu

! calcul des parametres de la precession (dzeta, z, teta)

dzeta = (pm_i_Lieske_PBD1*tu + pm_i_Lieske_PBD2*tu2 + pm_i_Lieske_PBD3*tu3) * pm_i_sec2rad 
z     = (pm_i_Lieske_PBZ1*tu + pm_i_Lieske_PBZ2*tu2 + pm_i_Lieske_PBZ3*tu3) * pm_i_sec2rad
teta  = (pm_i_Lieske_PBT1*tu + pm_i_Lieske_PBT2*tu2 + pm_i_Lieske_PBT3*tu3) * pm_i_sec2rad

prec%dzeta = dzeta
prec%z = z
prec%theta = teta

! calcul des derivees premieres

if (present(deriv1_prec)) then
   deriv1_prec%dzeta = (pm_i_Lieske_PBD1 + 2._pm_reel*pm_i_Lieske_PBD2*tu +   &
        3._pm_reel*pm_i_Lieske_PBD3*tu2)*pm_i_sec2rad*(pm_i_unsur86400*pm_i_unsurKanJul)
   deriv1_prec%z = (pm_i_Lieske_PBZ1 + 2._pm_reel*pm_i_Lieske_PBZ2*tu +       &
        3._pm_reel*pm_i_Lieske_PBZ3*tu2)*pm_i_sec2rad*(pm_i_unsur86400*pm_i_unsurKanJul)
   deriv1_prec%theta = (pm_i_Lieske_PBT1 + 2._pm_reel*pm_i_Lieske_PBT2*tu +   &
        3._pm_reel*pm_i_Lieske_PBT3*tu2)*pm_i_sec2rad*(pm_i_unsur86400*pm_i_unsurKanJul)
endif

! calcul des derivees secondes

if (present(deriv2_prec)) then
   deriv2_prec%dzeta = (2._pm_reel*pm_i_Lieske_PBD2 + 6._pm_reel*pm_i_Lieske_PBD3*tu)&
        *pm_i_sec2rad*((pm_i_unsur86400*pm_i_unsurKanJul)**2)
   deriv2_prec%z = (2._pm_reel*pm_i_Lieske_PBZ2 + 6._pm_reel*pm_i_Lieske_PBZ3*tu)&
        *pm_i_sec2rad*((pm_i_unsur86400*pm_i_unsurKanJul)**2)
   deriv2_prec%theta = (2._pm_reel*pm_i_Lieske_PBT2 + 6._pm_reel*pm_i_Lieske_PBT3*tu)&
        *pm_i_sec2rad*((pm_i_unsur86400*pm_i_unsurKanJul)**2)
endif

6000 continue

code_retour%routine = pm_num_mr_prec
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_prec
