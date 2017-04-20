subroutine mui_cowell_chavar(integrateur,y,ys,yp,yps,s,indic,n1,n2,n4,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol sous-programme utilise en cas de regularisation
!*rol (utilisation d'une variable independante autre que le temps)
!*rol  il forme yys , derivees par rapport a ss si indic # 0
!*rol  il restaure yyp , derivees par rapport a t si indic = 0
!*rol  transformation identique si ireg = 0
!*rol  si ireg # 0 on suppose que yy(1 a 3) et yyp(1 a 3) sont des
!*rol coordonnees de position-vitesse en unites compatibles avec lecpot
!
! Note d'utilisation: 
! ================== 
!              parametres d'entree
!
!*par indic  : sens de la transformation
!*par              = 0 , variable independante donne temps
!*par              # 0 , temps donne variable independante
!*par n1    : nombre d'equations a integrer par l'utilisateur
!*par n2    : .................. du 2eme ordre
!*par n4    : nombre total d'equations integrees par le ss-programme
!
!              parametres d'entree-sortie
!
!*par integrateur : structure intégrateur
!*par y      : fonctions integrees
!*par ys     : fonctions integrees
!*par yp     : tableau des derivees des fonctions / ss si indic # 0
!*par yps    : tableau des derivees des fonctions / t  si indic = 0
!*par s      : variable independante
!
!              parametres de sortie
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
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.9 : FA-ID 961 : correction des cartouches
!                   (Date: 03/2008 - Realisation: Atos Origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

!
! Modules
! =======
   use mslib
   use type_mspro
use int_util_internes_mspro, only : mui_cowell_ckeptp

!
! Declarations
! ============
   implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n) , intent(inout):: y
   real(pm_reel), dimension(integrateur%n) , intent(inout) :: ys
   real(pm_reel), dimension(integrateur%n) , intent(inout):: yp
   real(pm_reel), dimension(integrateur%n) , intent(inout) :: yps
   real(pm_reel), intent(in) :: s
   integer , intent(in) :: indic  ! sens de la transformation
   integer , intent(in) :: n1   ! nombre d'equations a integrer par l'utilisateur
   integer , intent(in) :: n2   ! nombre d'equations du 2eme ordre
   integer , intent(in) :: n4   ! nombre total d'equations integrees par le ss-programme
   integer , intent(out):: retour  ! code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
! Autres declarations
   integer :: i  !indice de boucle
   integer :: jreg  ! cle de regularisation+1
   integer :: n2p
   real (kind=pm_reel) :: c1,c2,cv  ! constantes
   real (kind=pm_reel) :: dsdt,dtds,sv,tk,vv

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_chavar.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

!***********************************************************************
!*Initialisations
!************************************************************************

   n2p=n2+1
   jreg=integrateur%ireg+1
!
!***********************************************************************
!*fon pas de regularisation , restitutions identiques yys(nn4)=0.
!***********************************************************************
!
   if(jreg == 1) then
!
      if(indic /= 0) then
!
!  temps donne variable independante
!
         do  i=1,n1
            yps(i)=yp(i)
            ys(i)=y(i)
         end do
!
         yps(n4)=yp(n4) - integrateur%theta0 - s
         ys(n4)= y(n4)
      
!
      else
!
!   variable independante donne temps
!
         do  i=1,n1
            yp(i)=yps(i)
            y(i)=ys(i)
         end do
!
         yp(n4)=yps(n4) + integrateur%theta0 + s
         y(n4)= ys(n4)
      
      end if
   end if
!
!***********************************************************************
!*fon regularisation utilisant l'anomalie excentrique
!*fon yys(nn4) est la partie periodique du temps dans l'anomalie exc.
!***********************************************************************
!
   if(jreg == 2) then
!
      if(indic /= 0) then
!
!   temps donne anomalie excentrique
!
         dtds=sqrt(y(1)*y(1)+y(2)*y(2)+y(3)*y(3))/integrateur%a0
!
         do  i=1,n2
            yps(i)=yp(i)*dtds
            ys(i)=y(i)
         end do
!
         if(n1 /= n2) then
!
            do  i=n2p,n1
               yps(i)=yp(i)
               ys(i)=y(i)
            end do
!
         endif
!
! ** changement de variable au niveau de l'equation du temps
!
         if (integrateur%ihyp == 0) then
! ** cas elliptique
!
            yps(n4)=yp(n4) - integrateur%theta0 - s
            ys(n4)= y(n4)
         else
! ** cas hyperbolique
!
            yps(n4)=yp(n4)
            ys(n4)=y(n4)
         endif
         
!
!   anomalie excentrique donne temps
!
      else
!
         dsdt=integrateur%a0/sqrt(ys(1)*ys(1)+ys(2)*ys(2)+ys(3)*ys(3))
!
         do  i=1,n2
            yp(i)=yps(i)*dsdt
            y(i)=ys(i)
         end do
!
         if(n1 /= n2) then
            do  i=n2p,n1
               yp(i)=yps(i)
               y(i)=ys(i)
            end do
         endif
!
! ** changement de variable au niveau de l'equation du temps
!
         
         if (integrateur%ihyp == 0) then
! ** cas elliptique
!
            yp(n4)=yps(n4) + integrateur%theta0 + s
            y(n4)= ys(n4)
         else
! ** cas hyperbolique
!
            yp(n4)=yps(n4)
            y(n4)=ys(n4)
         endif
         
      endif
   endif
!
!***********************************************************************
!*fon regularisation utilisant l'anomalie vraie
!*fon sans changement des variables integrees (si icircl=0)
!*fon yys(nn4) est la partie periodique du temps par rapport a
!*fon l'anomalie vraie
!***********************************************************************
!
   if(jreg == 3) then
!
      if(indic /= 0) then
!
!   temps donne anomalie vraie
!
         dtds=(y(1)*y(1)+y(2)*y(2)+y(3)*y(3))/integrateur%c02
!
         do  i=1,n2
            yps(i)=yp(i)*dtds
            ys(i)=y(i)
         end do
!
         if(n1 /= n2) then
            do  i=n2p,n1
               yps(i)=yp(i)
               ys(i)=y(i)
            end do
         endif
!
! ** changement de variable au niveau de l'equation du temps
!
         if (integrateur%ihyp == 0) then
! ** cas elliptique
!
            yps(n4)=yp(n4) - integrateur%theta0 - s
            ys(n4)= y(n4)
         else
! ** cas hyperbolique
!
            yps(n4)=yp(n4)
            ys(n4)= y(n4)
         endif
!
!   anomalie vraie donne temps
!
      else
!
         dsdt=integrateur%c02/(ys(1)*ys(1)+ys(2)*ys(2)+ys(3)*ys(3))
!
         do  i=1,n2
            yp(i)=yps(i)*dsdt
            y(i)=ys(i)
         end do
!
         if(n1 /= n2) then
            do  i=n2p,n1
               yp(i)=yps(i)
               y(i)=ys(i)
            end do
         endif
! ** changement de variable au niveau de l'equation du temps
!
         if (integrateur%ihyp == 0) then
! ** cas elliptique
!
            yp(n4)=yps(n4) + integrateur%theta0 + s
            y(n4)= ys(n4)
         else
! ** cas hyperbolique
!
            yp(n4)=yps(n4)
            y(n4)= ys(n4)
         endif
         
      endif
   endif
!
!***********************************************************************
!*fon regularisation utilisant l'anomalie vraie et le changement des
!*fon variables integrees x=x/r (forcee si circularisation)
!*fon yys(nn4) est la difference entre le temps et l'expression
!*fon keplerienne du temps
!***********************************************************************
!
   if(jreg == 4) then
!
!   calcul des constantes
!
      vv=integrateur%anv0+s*integrateur%xn
      cv=cos(vv)
      sv=sin(vv)
      c1=1.+integrateur%exc*cv
      c1=c1*integrateur%a0/integrateur%param
      c2=integrateur%xn*integrateur%exc*sv
      c2=c2*integrateur%a0/integrateur%param
!
      if(indic /= 0) then
!
!   temps donne anomalie vraie
!
         dtds=(y(1)*y(1)+y(2)*y(2)+y(3)*y(3))/integrateur%c02
         dtds=dtds*c1
!
         do  i=1,n2
            yps(i)=yp(i)*dtds-y(i)*c2
            ys(i)=y(i)*c1
         end do
!
         if(n1 /= n2) then
!
            do  i=n2p,n1
               yps(i)=yp(i)*c1
               ys(i)=y(i)
            end do
!
         endif
!
         call mui_cowell_ckeptp(integrateur,s,integrateur%ireg,tk,retour)
         if (retour /= pm_OK) go to 6000
         
         yps(n4)=yp(n4) - integrateur%theta0 - s - tk
         ys(n4)= y(n4)
!
!   anomalie vraie donne temps
!
      else
!
         dsdt=integrateur%c02/(ys(1)*ys(1)+ys(2)*ys(2)+ys(3)*ys(3))
         c2=dsdt*c2
         dsdt=dsdt*c1
!
         do  i=1,n2
            yp(i)=yps(i)*dsdt+ys(i)*c2
            y(i)=ys(i)/c1
         end do
!
         if(n1 /= n2) then
!
            do  i=n2p,n1
               yp(i)=yps(i)/c1
               y(i)=ys(i)
            end do
         endif
        
!
         call mui_cowell_ckeptp(integrateur,s,integrateur%ireg,tk,retour)
         if (retour /= pm_OK) go to 6000

         yp(n4)=yps(n4) + integrateur%theta0 + s + tk
         y(n4)= ys(n4)

      end if
   end if

6000 continue

 end subroutine  mui_cowell_chavar

