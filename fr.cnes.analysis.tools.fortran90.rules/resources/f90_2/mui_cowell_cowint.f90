subroutine mui_cowell_cowint(integrateur,cys,cyps,s,tt, &
      hpas,n,n1,n2,n4,ireg,ihyp, yssig,ypssig,sigma,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol interpolation des tableaux de cowell a la date de l'evenement
!
! Note d'utilisation: 
! ================== 
!              paramètre d'entrée/sortie
!
!*par integrateur : structure intégrateur 
!
!              parametres d'entree
!
!*par cys    : tableau a interpoler centre sur s
!*par cyps   : ............ idem ...............
!*par s      : abscisse (variable independante)
!*par          pour cys(9) et cyps(9)
!*par tt     : date d'interpolation
!*par          les dates et les abscisses sont reliees par
!*par          tt=theta0+cyps(n1+1,9)+s
!*par n      : ordre de l'interpolation
!*par n1     : nombre total d'equations differentielles
!*par n2     : nombre d'equations du second ordre
!*par n4     : nombre d'equations differentielles
!*par hpas   : pas des tableaux (en abscisse)
!*par ireg   : cle de regularisation
!*par ihyp   : cle indiquant le cas hyperbolique
!
!              parametres de sortie
!
!*par yssig  : resultat de l'interpolation
!*par ypssig : ........ idem .............
!*par sigma  : solution de l'equation:tt=theta0+yyssig(n1+1)+sigma
!*par retour  : code retour
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
!   + Version 5.9 : FA-ID 961 : correction des cartouches
!                   (Date: 03/2008 - Realisation: Atos Origin)
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

use int_util_internes_mspro, only : mui_cowell_ckeptp
use int_util_internes_mspro, only : mui_cowell_everep

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(in) :: cys  ! tableau a interpoler centre sur s
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(in) :: cyps ! tableau a interpoler centre sur s
   real(pm_reel), intent(in) :: s   ! abscisse (variable indépendante)
   real(pm_reel), intent(in) :: tt  ! date d'interpolation
   real(pm_reel), intent(in) :: hpas   ! pas des tableaux (en abscisse)
   integer, intent(in) :: n  ! ordre de l'interpolation
   integer, intent(in) :: n1 ! nombre total d'equations differentielles
   integer, intent(in) :: n2 ! nombre d'equations du second ordre
   integer, intent(in) :: n4 ! nombre d'equations differentielles
   integer, intent(in) :: ireg ! cle de regularisation
   integer, intent(in) :: ihyp ! cle indiquant le cas hyperbolique

   real(pm_reel), dimension(integrateur%n), intent(out) :: yssig  ! resultat de l'interpolation
   real(pm_reel), dimension(integrateur%n), intent(out) :: ypssig  ! resultat de l'interpolation
   real(pm_reel), intent(out) :: sigma  ! solution de l'equation:tt=theta0+yyssig(n1+1)+sigma
   integer, intent(out) :: retour ! code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: i,j   ! indices de boucle
   integer :: n1p,n2p

   real (kind=pm_reel) :: epsil,fmax,fmin
   real (kind=pm_reel), dimension(pm_i_dim16) :: f
   real (kind=pm_reel) :: hpk,resul
   real (kind=pm_reel) :: sigmax,sigmin,t,tk

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_cowint.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! autres initialisations
! ..........................................
f(:) = 0._pm_reel 

   if(ireg == 0) then
!
!***********************************************************************
!*fon cas de la variable temps
!***********************************************************************
!
      sigma=tt-integrateur%theta0
!
   else
!
      if (ihyp == 0) then
         epsil=1.e-9_pm_reel
      else
         epsil=1.e-8_pm_reel
      endif
!
!***********************************************************************
!*fon si variable autre que temps
!*fon recherche de sigma par interpolation lineaire et decoupage
!*fon on resoud en fait l'equation tt-ss=theta0+cyys(n1+1,9)+tkep
!*fon ceci permet de ne pas perdre de precision quand ss et tt
!*fon prennent simultanement des valeurs trop importantes
!***********************************************************************
!
      do  i=1,n
!         hpk=dble(i-integrateur%nn-1)*hpas
         hpk=real((i-integrateur%nn-1),kind=pm_reel)*hpas
         call mui_cowell_ckeptp(integrateur,s+hpk,ireg,tk,retour)
         if (retour /= pm_OK) go to 6000

         if (ihyp == 0) then
! ** cas elliptique
!
            f(i)=integrateur%theta0 + cyps(n4,8-integrateur%nn+i) + hpk + tk
         else
! ** cas hyperbolique: on a directement les valeurs de ti dans cyps(n4,i)
!
            f(i)=cyps(n4,8-integrateur%nn+i) - s
         endif
      end do
!
      fmin=f(integrateur%nn)
      fmax=f(integrateur%nn+1)
      sigmin=-hpas
      sigmax=0.0_pm_reel
      t=tt-s
!
      sigma=sigmin+(t-fmin)*(sigmax-sigmin)/(fmax-fmin)
      call mui_cowell_everep(hpas,f,pm_i_zero,sigma,n,resul,retour)
      if (retour /= pm_OK) go to 6000

      do while (abs(resul-t).gt.epsil)
!
         if((resul-t)<0) then
            sigmin=sigma
            fmin=resul
            sigma=sigmin+(t-fmin)*(sigmax-sigmin)/(fmax-fmin)
         elseif((resul-t)>0) then
            sigmax=sigma
            fmax=resul
            sigma=sigmin+(t-fmin)*(sigmax-sigmin)/(fmax-fmin)
         endif
         call mui_cowell_everep(hpas,f,pm_i_zero,sigma,n,resul,retour)
         if (retour /= pm_OK) go to 6000
         
      enddo
      sigma=sigma+s
!
   endif
!
!***********************************************************************
!*fon on a trouve sigma ; on fait l'interpolation
!***********************************************************************
!
   do  i=1,n2
!
      do  j=1,n
         f(j)=cys(i,8-integrateur%nn+j)
      end do
!
      call mui_cowell_everep(hpas,f,s,sigma,n,yssig(i),retour)
      if (retour /= pm_OK) go to 6000
!
   end do
!
   n2p=n2+1
   n1p=n1+1
!
   do  i=n2p,n1p
      yssig(i)=0._pm_reel
   end do
!
   do  i=1,n1p
!
      do  j=1,n
         f(j)=cyps(i,8-integrateur%nn+j)
      end do
!
      call mui_cowell_everep(hpas,f,s,sigma,n,ypssig(i),retour)
      if (retour /= pm_OK) go to 6000
!
   end do
!
6000 continue
   
 end subroutine  mui_cowell_cowint
