subroutine mui_cowell_normal(integrateur,yn,ypn,y,yp,u,indic,n1,n2,n4,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol normalisation ou denormalisation des tableaux
!*rol des conditions initiales
!
! Note d'utilisation: 
! ================== 
!              parametres d'entree
!
!*par indic  : indicateur du sens de la transformation
!*par            0 : y(nn1),yp(nn2),u  ---> yy(nn4),yyp(nn4)
!*par            1 : transformation inverse
!*par n1    : nombre d'equations a integrer par l'utilisateur
!*par n2    : .................. du 2eme ordre
!*par n4    : nombre total d'equations integrees par le ss-programme 
!           
!              parametres d'entree-sortie
!
!*par integrateur : structure intégrateur
!*par yn     : tableau des fonctions integrees
!*par ypn    : tableau des derivees 1eres
!*par y      : tableau des fonctions integrees (utilisateur)
!*par yp     : tableau des derivees des fonctions utilisateur
!*par u      : temps compte a partir de l'integration        
!
!              parametres de sortie
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
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.9 : FA-ID 961 : correction des cartouches du Cowell
!                   (Date: 03/2008 - Réalisation: Y.TANGUY - Atos Origin)
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

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n), intent(inout) :: yn   ! tableau des fonctions integrees               
   real(pm_reel), dimension(integrateur%n), intent(inout) :: ypn  ! tableau des derivees 1eres                    
   real(pm_reel), dimension(integrateur%n), intent(inout) :: y    ! tableau des fonctions integrees (utilisateur) 
   real(pm_reel), dimension(integrateur%n), intent(inout) :: yp   ! tableau des derivees des fonctions utilisateur
   real(pm_reel), intent(inout) :: u                              ! temps compte a partir de l'integration        

   integer, intent(in) :: indic  ! indicateur du sens de la transformation               
                                 !    0 : y(nn1),yp(nn2),u  ---> yy(nn4),yyp(nn4)         
                                 !    1 : transformation inverse                          

   integer, intent(in) :: n1     ! nombre d'equations a integrer par l'utilisateur (2nd ordre + 1er ordre)
   integer, intent(in) :: n2     ! ................. du 2eme ordre                       
   integer, intent(in) :: n4     ! nombre total d'equations integrees par le ss-programme (=n1 + variable d'intégration)
                                 
   integer, intent(out) :: retour! code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations

integer :: j
integer :: n2p

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_normal.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

!
!************************************************************************  
!
   if(indic == 0) then
!
!***********************************************************************
!*fon normalisation des tailles y,yp donne yn,ypn
!***********************************************************************
!
      do  j=1,n2
         yn(j)=y(j)
         ypn(j)=yp(j)
      end do
!
      if(n1 /= n2) then
!
         n2p=n2+1
!
         do  j=n2p,n1
            yn(j)=0._pm_reel
            ypn(j)=y(j)
         end do
!
      endif
         
      yn(n4)=0._pm_reel
      ypn(n4)=u
 
!
   else
!
!***********************************************************************
!*fon denormalisation des tailles yy,yyp donne y,yp
!***********************************************************************
!
      do  j=1,n2
         y(j)=yn(j)
         yp(j)=ypn(j)
      end do
!
      if(n1 /= n2) then
!
         n2p=n2+1
!
         do  j=n2p,n1
            y(j)=ypn(j)
         end do
!
      endif
      u=ypn(n4)

   endif
   
 end subroutine  mui_cowell_normal

