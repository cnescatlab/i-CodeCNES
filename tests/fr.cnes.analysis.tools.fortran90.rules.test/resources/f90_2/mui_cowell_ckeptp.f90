subroutine mui_cowell_ckeptp(integrateur,s,ireg, tk,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol forme la partie keplerienne du temps pour ireg=3
!
! Note d'utilisation: 
! ================== 
!              parametre d'entrée/sortie
!
!*par integrateur : structure integrateur
!
!              parametres d'entree
!
!*par s      : variable independante ( anomalie vraie )
!*par ireg   : cle de regularisation
!
!              parametres de sortie
!
!*par tk     : partie keplerienne du temps
!*par retour : code retour
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!     + Version 5.3 : creation 
!                     DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                     (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.9 : FA-ID 961 : correction des cartouches
!                   (Date: 03/2008 - Realisation: Atos Origin)
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
use parametre_interne_mspro ! acces aux parametres internes de la librairie MSPRO

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), intent(in) :: s
   integer, intent(in) :: ireg
   real(pm_reel), intent(out) :: tk
   integer, intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
real(pm_reel) :: c1,ce,cv,depi,e,se,sv,v

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_ckeptp.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

   if (ireg /= 3) then
      tk=0.0_pm_reel
   else
!

      v = integrateur%anv0 + s*integrateur%xn
      cv = cos(v)
      sv = sin(v)
      c1 = 1.0_pm_reel + integrateur%exc*cv
      ce = (cv + integrateur%exc) / c1
      se = sv*integrateur%rume2 / c1
      e = atan2(se, ce)
      depi = 2.0_pm_reel*pm_pi
      v = mod(v, depi)
!
! tests pour retablir la valeur de e et de v entre 0 et 2*pi
      if (se < 0.0_pm_reel)  e = depi + e
      if (v < 0.0_pm_reel) v = depi + v
!
      tk = (e - (integrateur%exc*se) - v) / integrateur%xn
!
   endif

 end subroutine  mui_cowell_ckeptp

