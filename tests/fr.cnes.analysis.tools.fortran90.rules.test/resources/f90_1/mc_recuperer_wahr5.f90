subroutine mc_recuperer_wahr5 (code_retour,drapeau_wahr5)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Recuperer les flags mslib pour utilisation de modèles
! 
!$Historique
! ==========
!   + Version 6.4 : DM-ID 424 : Modification des arguments de la nutation luni-solaire
!                         (Date: 05/2006 - Realisation : Atos Origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
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
use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib
use flags_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_code_retour), intent(out)       :: code_retour
logical , optional, intent(out)         :: drapeau_wahr5

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mc_recuperer_wahr5.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: &
     rcs_id =' $Id: mc_recuperer_wahr5.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************
! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! recuperation du flag
if(present(drapeau_wahr5)) then
  drapeau_wahr5 = wahr_p5
endif

code_retour%routine = pm_num_mc_recuperer_wahr5
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mc_recuperer_wahr5
