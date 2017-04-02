subroutine mu_creer_gest_pas (integrateur, gest_pas, code_retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But: Enregistrer un gestionnaire de pas dans l'integrateur  
! ===
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
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

use type_mspro
use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine gest_pas(integrateur,interpolateur,tdeb,tfin,dernier_pas,retour)     ! gestionnaire de pas

   use mslib
   use type_mspro

   interface
      subroutine interpolateur(integrateur,t,y,retour_interp)     ! routine d'interpolation
   
      use mslib
      use type_mspro
      
      type(tm_integrateur),        intent(in)           ::  integrateur   ! integrateur utilise
      real(pm_reel),               intent(in)           ::  t             ! abscisse
      real(pm_reel),dimension(integrateur%n),intent(out)::  y             ! vecteur d'etat interpole
      integer,                     intent(out)          ::  retour_interp
      
      end subroutine interpolateur
   end interface

   type(tm_integrateur), intent(in) ::  integrateur  ! integrateur utilise
   real(pm_reel),        intent(in) ::  tdeb  ! abscisse debut de l'interv. de validite de l'interpolateur
   real(pm_reel),        intent(in) ::  tfin  ! abscisse fin de l'interv. de validite de l'interpolateur
   logical,              intent(in) ::  dernier_pas  ! indique si l'on est au dernier pas
   integer,              intent(out)::  retour

   end subroutine gest_pas
end interface

type(tm_integrateur), intent(inout) ::  integrateur ! integrateur utilise
type(tm_code_retour), intent(out)   ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer :: MsproConvertitPointeur
external MsproConvertitPointeur

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_creer_gest_pas.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! conversion en entier de l'adresse de la subroutine gest_pas
! grace a une fonction C.

integrateur%adr_gest_pas = MsproConvertitPointeur(gest_pas)

code_retour%routine = pm_num_mu_creer_gest_pas
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_creer_gest_pas
