subroutine mwi_alc_unite_logique ( unific, retour )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Allouer la premiere unite logique disponible pour ouvrir un fichier
! ===  Cette unite est determinee dans la plage des  valeurs comprises
!      entre l'unite 10 et l'unite 999.
!      Dans la pratique, la boucle s'arretera bien avant 999 car le nombre
!      maximum de fichiers simultanement ouverts est limite par le systeme.
!
! Note d'utilisation:  L'entree unific est un entier correspondant a
! ==================   l'unite logique reservee precedemment.
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

integer, intent(out)  :: unific  ! Unite logique associee au fichier saut de TUC
integer, intent(out)  :: retour  ! Code de retour du sous programme interne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: unite                       ! unite logique
integer, parameter :: unite_min=10     ! unite minimum
integer, parameter :: unite_max=999    ! unite maximum
logical :: unite_deja_prise            ! flag d'occupation d'unite

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mwi_alc_unite_logique.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! recherche de la premiere unite logique libre dans la plage
! des valeurs comprise entre unite_min et unite_max
! ..........................................................
do unite = unite_min, unite_max
   inquire(unite, OPENED=unite_deja_prise)
   if (.not. unite_deja_prise) then
      unific = unite
      exit
   end if
end do
 
if ((unite == unite_max) .and. unite_deja_prise) then
   retour = pm_err_alc_unite_logique
end if

end subroutine mwi_alc_unite_logique
