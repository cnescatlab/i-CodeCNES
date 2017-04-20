subroutine mui_cowell_tempo(jdeb,secdeb,t,jfin,secfin,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol calcul d'une date finale (jfin,secf à partir d'une date initiale
!*rol d'une date initiale (jdeb,secdeb) et d'une duree (t)
!
! Note d'utilisation: 
! ================== 
!              parametres d'entree
!
!*par jdeb  : jours juliens date initiale
!*par secdeb : secondes date initiale
!*par t      : intervalle de temps (sec) 
!              parametres de sortie
!*par jfin  : jours juliens date finale   
!*par secfin : secondes date finale
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!     + Version 5.3 : creation 
!                     DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                     (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
!     + Version 5.4 : modification
!                     FA-ID 439 : remarques qualite
!                     (Date: 02/2006 - Realisation: Atos Origin)
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

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   integer, intent(in) :: jdeb                 ! jours juliens date initiale
   real (kind=pm_reel), intent(in) :: secdeb   ! secondes date initiale
   real (kind=pm_reel), intent(in) :: t        ! intervalle de temps (sec) 
   integer, intent(out) :: jfin                ! jours juliens date finale  
   real (kind=pm_reel), intent(out) :: secfin  ! secondes date finale
   integer, intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: jfinx
   real (kind=pm_reel) :: tt

   character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur=&
                     '@(#) Fichier MSPRO mui_cowell_tempo.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
   retour = pm_OK
   !
   tt=t+secdeb
   !
   if(tt.ge.0.) then
      jfinx=int(tt/86400.0_pm_reel)
   else
      jfinx=int(tt/86400.0_pm_reel)-1
   endif
   !
   jfin=jdeb+jfinx
   secfin=tt-86400.0_pm_reel*real(jfinx,kind=pm_reel)

 
end subroutine  mui_cowell_tempo
 
