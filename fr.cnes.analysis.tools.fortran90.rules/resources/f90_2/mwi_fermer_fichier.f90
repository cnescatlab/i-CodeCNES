subroutine mwi_fermer_fichier ( unific, retour )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Fermer l'unite logique associee a un fichier
! ===
!
! Note d'utilisation:  L'entree unific est un entier et correspond a
! ==================   l'unite logique a fermer
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 10/2002 - Realisation: Michel Lagreca
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)   :: unific  ! Unite logique associee au fichier saut de TUC
integer, intent(out)  :: retour  ! Code de retour du sous programme interne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: resultat       ! status de l'operation "close"
character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mwi_fermer_fichier.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! fermeture de l'unite associee au fichier
! ....................................................
close(UNIT=unific, IOSTAT=resultat)

if (resultat /= pm_OK) then
   retour = pm_err_fermer_fichier
end if

end subroutine mwi_fermer_fichier
