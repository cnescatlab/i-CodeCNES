function mui_recale_angle ( alpha, angle_ref )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Recaler un angle alpha dans l'intervalle [angle_ref - PI, angle_ref + PI]
! ===
!
! Note d'utilisation:  pour plus de details se reporter a la note algorithmique
! ==================
!
! Utilisation: 
! ===========
!    Plusieurs usages sont possibles:
!      1) si angle_ref = 0 , permet de recadrer un angle entre [-pi, +pi]
!
!      2) si alpha doit etre comparer par rapport a un angle beta:
!            * on ramene alpha et beta dans un intervalle commun (en general [0,2pi]):
!                 avec creation de valeurs intermediaires: alpha_2pi et beta_2pi
!            * puis on examine la difference entre alpha_2pi et beta_2pi
!         Avec cette routine
!            * on ramene alpha dans [beta - PI, beta + PI] (ou beta = angle_ref)
!                 avec creation de la valeur intermediaire: alpha_recadre
!            * on examine alpha_recadre par rapport a beta
!               
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 04/2002 - Realisation: Guylaine Prat)
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

use parametre_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: alpha               ! angle initial 
real(pm_reel), intent(in) :: angle_ref           ! angle servant de centre au recalage
real(pm_reel)             :: mui_recale_angle    ! angle recale

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: partie_entiere

intrinsic floor

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_recale_angle.f90: derniere modification V5.15 >'

!************************************************************************

partie_entiere = floor ((alpha  + pm_pi - angle_ref) / pm_deux_pi)
mui_recale_angle = alpha - pm_deux_pi * real(partie_entiere,kind = pm_reel)

end function mui_recale_angle
