subroutine mu_liberer_integ (integrateur, code_retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Liberer la memoire allouee pour un integrateur donne
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
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 02/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 :  DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                     (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
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

type(tm_integrateur), intent(inout) ::  integrateur ! integrateur utilise
type(tm_code_retour), intent(out)   ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer :: alloc_ok     ! statut de la desallocation

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mu_liberer_integ.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

if (associated(integrateur%a)) then
   deallocate(integrateur%a,stat=alloc_ok)
endif

if (associated(integrateur%b)) then
   deallocate(integrateur%b,stat=alloc_ok)
endif

if (associated(integrateur%c)) then
   deallocate(integrateur%c,stat=alloc_ok)
endif

if (associated(integrateur%yDotK)) then
   deallocate(integrateur%yDotK,stat=alloc_ok)
endif

if (associated(integrateur%y_fin)) then
   deallocate(integrateur%y_fin,stat=alloc_ok)
endif

if (integrateur%nb_commut > 0) then
   deallocate(integrateur%g_commut,stat=alloc_ok)
end if

if (integrateur%pas_variable) then
   deallocate(integrateur%tol_rel,stat=alloc_ok)
   deallocate(integrateur%tol_abs,stat=alloc_ok)
   deallocate(integrateur%v,stat=alloc_ok)
end if

if (associated(integrateur%corr)) then
   deallocate(integrateur%corr,stat=alloc_ok)
endif

if (associated(integrateur%alpha)) then
   deallocate(integrateur%alpha,stat=alloc_ok)
endif

if (associated(integrateur%beta)) then
   deallocate(integrateur%beta,stat=alloc_ok)
endif

if (associated(integrateur%spys)) then
   deallocate(integrateur%spys,stat=alloc_ok)
endif

if (associated(integrateur%cys)) then
   deallocate(integrateur%cys,stat=alloc_ok)
endif

if (associated(integrateur%cyps)) then
   deallocate(integrateur%cyps,stat=alloc_ok)
endif

if (associated(integrateur%cypsi)) then
   deallocate(integrateur%cypsi,stat=alloc_ok)
endif

if (associated(integrateur%cysi)) then
   deallocate(integrateur%cysi,stat=alloc_ok)
endif

if (associated(integrateur%dels)) then
   deallocate(integrateur%dels,stat=alloc_ok)
endif

code_retour%routine = pm_num_mu_liberer_integ
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_liberer_integ
