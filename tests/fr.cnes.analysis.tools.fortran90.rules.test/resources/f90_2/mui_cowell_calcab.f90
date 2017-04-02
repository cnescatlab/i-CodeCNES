subroutine mui_cowell_calcab(iord,ideb,ifin,koeff,a,b,d,dp,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol calcul d'une colonne de chacune des 2 matrices alpha et beta
!*rol                                 pour j donne de ideb a ifin-1
!*rol                                     a(i) = alpha(i,j)
!*rol                                     b(i) = beta (i,j)
!
! Note d'utilisation: 
! ================== 
!              parametres d'entree
!
!*par iord   : ordre de cowell ( entier pair de 2 a 16 )
!*par ideb   : adresse extreme inferieure des alpha et beta
!*par ifin   : adresse extreme superieure des alpha et beta
!*par koeff  : tableau calcule par cigcag
!
!               parametres de sortie
!
!*par a      : jeme colonne du tableau alpha
!*par b      : jeme colonne du tableau beta
!*par d      : coefficient denormalisant
!*par dp     : ......................... 
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
!     + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!     + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
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

   integer , intent(in) :: iord    ! ordre de cowell
   integer , intent(in) :: ideb    ! adresse extreme inferieure des alpha et beta
   integer , intent(in) :: ifin    ! adresse extreme superieure des alpha et beta
   integer, dimension(pm_i_dim17,pm_i_dim15), intent(in) :: koeff  ! tableau calcule par cigcag

   real(pm_reel), dimension(pm_i_dim16), intent(out) :: a   ! jeme colonne du tableau alpha
   real(pm_reel), dimension(pm_i_dim16), intent(out) :: b   ! jeme colonne du tableau beta
   real(pm_reel), intent(out) :: d   ! coefficient denormalisant
   real(pm_reel), intent(out) :: dp 
   integer , intent(out) :: retour    ! code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: i,ksom,l,lc
   real(pm_reel) :: an,bn,coefa,coefb 
                      
   real(pm_reel),dimension(pm_i_dim16), parameter   :: den =(/        &
        1._pm_reel ,           12._pm_reel ,            6._pm_reel ,           60._pm_reel , &
        30._pm_reel ,         3780._pm_reel ,         1890._pm_reel ,        56700._pm_reel , &
        28350._pm_reel ,       623700._pm_reel ,       311850._pm_reel ,   2554051500._pm_reel, &
        1277025750._pm_reel ,   7662154500._pm_reel ,   3831077250._pm_reel , 130256626500._pm_reel /)
   
   real(pm_reel),dimension(pm_i_dim16), parameter    :: denp =(/       &      
        4._pm_reel ,            2._pm_reel ,           12._pm_reel ,            6._pm_reel , &
        180._pm_reel ,           90._pm_reel ,         3780._pm_reel ,         1890._pm_reel , &
        56700._pm_reel ,        28350._pm_reel ,      1871100._pm_reel ,       935550._pm_reel , &
        2554051500._pm_reel ,   1277025750._pm_reel ,   7662154500._pm_reel ,   3831077250._pm_reel /)
   
   real(pm_reel),dimension(pm_i_dim16), parameter    :: cpsn =(/       &         
        1._pm_reel ,            1._pm_reel ,           -1._pm_reel ,           -1._pm_reel , &
        11._pm_reel ,           31._pm_reel ,         -191._pm_reel ,         -289._pm_reel , &
        2497._pm_reel ,          317._pm_reel ,       -14797._pm_reel ,     -6803477._pm_reel , &
        92427157._pm_reel ,      3203699._pm_reel ,    -36740617._pm_reel ,    -73691749._pm_reel /)
   
   real(pm_reel),dimension(pm_i_dim16), parameter    :: cpsd =(/        &     
        2._pm_reel ,           12._pm_reel ,            6._pm_reel ,           60._pm_reel , &
        90._pm_reel ,         3780._pm_reel ,         1890._pm_reel ,        56700._pm_reel , &
        28350._pm_reel ,        89100._pm_reel ,       187110._pm_reel ,   2554051500._pm_reel , &
        1277025750._pm_reel ,   1532430900._pm_reel ,    547296750._pm_reel ,  43418875500._pm_reel /)

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_calcab.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! autres initialisations

   ksom=iord+1

!***********************************************************************
!*fon calcul des b(i) = beta (i,j)
!***********************************************************************
!
   dp = denp(iord) * 2.0_pm_reel**(iord-2)
!
   do  i=ideb,ifin
!
      bn=0.0_pm_reel
!
      do  l=3,iord,2
         lc=ksom-l
         coefb=koeff(i,lc)+koeff(i+1,lc)
         bn = bn + coefb * cpsn(l)*(dp/cpsd(l)/2.0_pm_reel**(l-1))
      end do
!
      coefb=koeff(i,ksom-2)
      bn = bn + coefb * cpsn(1)*(dp/cpsd(1))
      b(i)=bn
!
   end do
!
!***********************************************************************
!*fon calcul des a(i) = alpha(i,j)
!***********************************************************************
!
   d  = den (iord) * 2.0_pm_reel**(iord-2)
!
   do  i=ideb,ifin
!
      an=0.0_pm_reel
!
      do  l=2,iord,2
         lc=ksom-l
         coefa=koeff(i,lc)
         an = an + coefa * cpsn(l)*(d /cpsd(l)/2.0_pm_reel**(l-2))
      end do
!
      a(i)=an
!
   end do
      
   end subroutine  mui_cowell_calcab
