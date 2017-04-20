subroutine mui_cowell_cnicow(integrateur,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol calcul des coeff. alpha et beta en fonction de l'ordre iord
!*rol chargement du common coweab
!*rol valeurs autorisees pour iord : les entiers pairs de 2 a 16
!
! Note d'utilisation: 
! ================== 
!
!           paramètre d'entrée/sortie
!
!*par integrateur : structure intégrateur à initialiser
!
!           paramètre de sortie
!
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
!     + Version 5.4 : modification
!                     FA-ID 439 : remarques qualite
!                     (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 5.9 : FA-ID 961 : maj des cartouches des routines internes du Cowell
!                   (Date: 03/2008 - Realisation: Y.TANGUY - Atos Origin)
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

use int_util_internes_mspro, only : mui_cowell_calcoe
use int_util_internes_mspro, only : mui_cowell_calcab

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   integer , intent(out) :: retour    ! code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: ic,ideb
   integer :: ifin,j,jdeb,jfin,jordm,jordp,lc,lcf
   integer :: kini0
   integer, dimension(pm_i_dim15) :: kini
   integer, dimension(pm_i_dim17,pm_i_dim15) :: koeff
   real(pm_reel), dimension(pm_i_dim16) :: alpha0   ! 
   real(pm_reel), dimension(pm_i_dim16) :: beta0   ! 

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_cnicow.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! autres initialisations
! ..........................................
kini(:) = 0
koeff(:,:) = 0
alpha0(:) = 0._pm_reel 
beta0(:) = 0._pm_reel 

!***********************************************************************
!*fon adresses du point central  (ic,ic)
!***********************************************************************
!
   ic=8
!
!***********************************************************************
!*fon adresses extremes des alpha et beta
!***********************************************************************
!
   jordp=integrateur%ordre/2 - 1
   jordm=integrateur%ordre - jordp - 2
   jdeb=ic-jordm
   jfin=ic+jordp
   ideb=jdeb
   ifin=jfin+1
   lcf=integrateur%ordre-1
!
!***********************************************************************
!*fon initialisation du vecteur auxiliaire kini et de kini0
!***********************************************************************
!
   do  lc=1,lcf
      kini(lc)=0
   end do
!
   kini0=1
!
!***********************************************************************
!*fon calcul des coefficients alpha(i,j) et beta(i,j) a j donne
!***********************************************************************
!
   do  j=jdeb,jfin
      call mui_cowell_calcoe(integrateur,ideb,ifin,ic,lcf,kini0,kini,koeff,retour)
      if (retour /= pm_OK) go to 6000
      kini0=0

      call mui_cowell_calcab(integrateur%ordre,ideb,ifin,koeff,alpha0,beta0,integrateur%d,integrateur%dp,retour)
      if (retour /= pm_OK) go to 6000

      integrateur%alpha(:,j) = alpha0(:)
      integrateur%beta(:,j) = beta0(:)

   end do
!

6000 continue
   
 end subroutine mui_cowell_cnicow

