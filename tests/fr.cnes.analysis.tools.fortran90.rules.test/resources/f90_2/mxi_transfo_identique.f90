subroutine mxi_transfo_identique ( taille, donnees_in, donnees_out, retour, jacob )

! (C) Copyright CNES - MSPRO - 2003

!************************************************************************
!
! But:  transfert des donnees d'entrees en sorties dans le cas ou 
! ===   il n'y a pas de transformation, avec initialisation de la jacobienne
!       en matrice identite (si demandee)
!
! Note d'utilisation:  Valable aussi bien pour les reperes que les variables
! ==================   La taille prevue est sensee etre petite (3 ou 6)
!                      (et les tableaux sont dimensionnes par l'appelant)
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation a partir de rien
!                         (Date: 01/2003 - Realisation: Guylaine Prat)
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

integer,  intent(in)                                 :: taille      ! du vecteur des donnees
real(pm_reel), dimension(:), intent(in)              :: donnees_in  ! donnees en entree
real(pm_reel), dimension(:), intent(out)             :: donnees_out ! donnees en sortie

integer, intent(out)                                 :: retour
real(pm_reel), dimension(:,:), intent(out), optional :: jacob       ! jacobienne de la transformation = identite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: diag_jacob     ! compteur de boucle sur la diagonale

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mxi_transfo_identique.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! Transfert des donnees en entree en donnees en sortie
! ====================================================

donnees_out(1:taille) = donnees_in(1:taille)

! Initialisation de la jacobienne par l'identite (si elle a ete demandee)
! =======================================================================

if (present(jacob)) then

   jacob(:,:) = 0._pm_reel

   do diag_jacob = 1 , taille
      jacob(diag_jacob,diag_jacob) = 1.0_pm_reel
   end do

end if

end subroutine mxi_transfo_identique
