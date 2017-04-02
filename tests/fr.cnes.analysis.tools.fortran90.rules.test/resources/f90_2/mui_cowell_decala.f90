subroutine mui_cowell_decala (integrateur,cys,cyps,iordre,n4,cysi,cypsi,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol decale le tableau de cowell pour n'interpoler que sur des valeurs
!*rol totalement determinees
!
! Note d'utilisation: 
! ================== 
!
! paramètre d'entré/sortie
!
!*par integrateur : structure intégrateur
!
! parametres d entree
!
!*par cys      : tableau de cowell de la fonction a integrer
!*par cyps     : tableau de cowell des derivees de cette fonction
!*par iordre   : ordre de l'integrateur de cowell
!*par n4       : nombre total d equations differentielles
!
! parametres de sortie
!
!*par cysi     : tableau de cowell de la fonction a integrer decale
!                pour l interpolation (iord/2)
!*par cypsi    : tableau de cowell des derivees de cette fonction
!                pour l interpolation (iord/2)
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!     + Version 5.3 : creation 
!                     DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                     (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
!    + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!    + Version 5.9 : FA-ID 961 : maj des cartouches des routines internes du Cowell
!                   (Date: 03/2008 - Realisation: Y.TANGUY - Atos Origin)
!    + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
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
use parametre_interne_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(in) :: cys   ! tableau de cowell de la fonction a integrer                   
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(in) :: cyps  ! tableau de cowell des derivees de cette fonction    
   integer , intent(in):: iordre    ! ordre de l integrateur de cowell
   integer , intent(in):: n4      ! nombre total d equations differentielles
   real(pm_reel), dimension(integrateur%n, pm_i_dim17) , intent(inout) :: cysi  ! tableau de cowell de la fonction a integrer decale
   real(pm_reel), dimension(integrateur%n, pm_i_dim17) , intent(inout):: cypsi ! tableau de cowell des derivees de cette fonction
   integer , intent(out):: retour    ! code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: i,j  !indices de boucle

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_decala.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

!************************************************************************

   do  i=1,n4
      do  j=9-iordre/2,8
         cysi(i,j)=cysi(i,j+1)
         cypsi(i,j)=cypsi(i,j+1)
      end do
   end do
!
   do  i=1,n4
      do  j=9,9+iordre/2
         cysi(i,j)=cys(i,j-iordre/2)
         cypsi(i,j)=cyps(i,j-iordre/2)
      end do
   end do
!

 end subroutine  mui_cowell_decala
