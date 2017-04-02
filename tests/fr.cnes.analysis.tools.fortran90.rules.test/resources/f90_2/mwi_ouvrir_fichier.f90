subroutine mwi_ouvrir_fichier ( nomfic, unific, retour )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Ouvrir en lecture un fichier et positionner le pointeur
! ===  de lecture au debut de ce fichier
!
! Note d'utilisation:  L'entree nomfic est une chaine correspondant au
! ==================   nom complet (chemin+nom) du fichier a lire
!                      L'entree unific est un entier correspondant a
!                      l'unite logique reservee precedemment.
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

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

character(len=*), intent(in) :: nomfic  ! Nom du fichier saut de TUC (chemin+nom)
integer, intent(in) :: unific           ! unite logique allouee au fichier
integer, intent(out) :: retour          ! Code de retour du sous programme interne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

! attributs de la commande open Fortran pour ouvrir en lecture un
! fichier ASCII sequentiel existant en debut de fichier
character(len=*), parameter :: status_fichier="old"         ! attibut status
character(len=*), parameter :: acces_fichier="sequential"   ! attibut acces
character(len=*), parameter :: format_fichier="formatted"   ! attibut format
character(len=*), parameter :: position_fichier="rewind"    ! attibut position
character(len=*), parameter :: action_fichier="read"        ! attibut action

integer :: resultat    ! status de l'operation "open"
character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mwi_ouvrir_fichier.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! ouverture du fichier sequentiel ASCII 
! en acces lecture, le pointeur etant en debut de fichier
! .......................................................
open(UNIT=unific,&
     FILE=nomfic, &
     STATUS=status_fichier, &
     ACCESS=acces_fichier, &
     FORM=format_fichier, &
     POSITION=position_fichier, &
     ACTION=action_fichier, &
     IOSTAT=resultat) 

if (resultat /= pm_OK) then
   retour = pm_err_ouvrir_fichier
end if

end subroutine mwi_ouvrir_fichier
