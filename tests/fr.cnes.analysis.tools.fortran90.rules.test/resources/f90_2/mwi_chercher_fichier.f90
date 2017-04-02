subroutine mwi_chercher_fichier ( nomfic, retour )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Rechercher l'existance d'un fichier avant ouverture
! ===
!
! Note d'utilisation:  L'entree nomfic est une chaine correspondant au
! ==================   nom complet (chemin+nom) du fichier a rechercher
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
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use parametre_interne_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

character(len=*), intent(in) :: nomfic  ! Nom du fichier saut de TUC (chemin+nom)
integer, intent(out) :: retour          ! Code de retour du sous programme interne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

logical :: fichier_existe       ! indicateur d'existence du fichier
logical :: fichier_ouvert       ! indicateur d'ouverture du fichier
character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mwi_chercher_fichier.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! initialisation des valeurs de retour de la fonction inquire
! ...........................................................
fichier_existe = pm_i_non
fichier_ouvert = pm_i_non

! Recherche du fichier nomfic non ouvert
! ....................................................
inquire(FILE=nomfic, EXIST=fichier_existe, OPENED=fichier_ouvert)

if(.not.fichier_existe) then
   retour = pm_err_fichier_absent
   go to 6000
else
   if(fichier_ouvert) then
      retour = pm_err_fichier_ouvert
      go to 6000
   end if
end if

6000 continue

end subroutine mwi_chercher_fichier
