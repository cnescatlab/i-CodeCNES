subroutine mui_cowell_cowcir(integrateur,icircl,per,x,xp,ier)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************   
! But:  
! ===     
!*rol etalonnage des coefficients correcteurs pour la circularisation
!*rol chargement du common correc
!
! Note d'utilisation: 
! ================== 
!              parametre d'entrée/sortie
!
!*par integrateur : structure intégrateur
!
!              parametres entree
!
!*par icircl : cle de circularisation
!*par per    : periode reelle du mouvement pseudo-circulaire (s)
!*par x      : fonctions integrees
!*par xp     : derivees des fonctions
!
!              parametre sortie
!
!*par ier    : code retour erreur (0: si ok)
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
!     + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!     + Version 5.9 : FA-ID 961 : maj des cartouches des routines du Cowell
!                     (Date: 03/2008 - Realisation: Y.TANGUY - Atos Origin)
!     + Version 5.10: FA-ID 961 : correction des cartouches
!                     DM-ID 1058 : Correction des warnings levés par g95
!                     (Date: 8/2008 - Realisation: Atos origin)
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

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   integer, intent(in) :: icircl    ! cle de circularisation
   real(pm_reel) :: per ! periode reelle du mouvement pseudo-circulaire (s)
   real(pm_reel), dimension(integrateur%n), intent(in) :: x  ! fonctions integrees
   real(pm_reel), dimension(integrateur%n), intent(in) :: xp ! derivees des fonctions
   integer, intent(out) :: ier

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: i,j,i1,i4,i5,i7,i9,ic,im,ip
   integer :: jord4,jordm,jordp
!
   real(pm_reel) :: a,ai
   real(pm_reel), dimension(pm_i_dim17) :: del,delp
   real(pm_reel) :: hsp,r,theta,v2
!
   real(pm_reel) :: ct,st,t2
!  epsilon pour évaluer la période
   real(pm_reel), parameter :: eps_periode = 1.e-15_pm_reel

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_cowcir.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
ier = pm_OK

! autres initialisations
! ..........................................
del(:) = 0._pm_reel
delp(:) = 0._pm_reel

!***********************************************************************
!*fon etalonnage des coefficients correcteurs integrateur%corr
!***********************************************************************
!
   jord4=integrateur%ordre/2
   i5=9
   i4=8
   jordp=jord4-1
   jordm=integrateur%ordre-jordp-2
   i1=i4-jordm
   i7=i4+jordp
   i9=i5+jord4
   integrateur%omeg=0.0_pm_reel
!
   do  i=1,4
      do  j=1,i9
         integrateur%corr(i,j)=0.0_pm_reel
      end do
   end do
!
!*********************************************************************
!       1)  pas de circularisation
!*********************************************************************
!
   if(icircl == 0) goto 6000
!
!********************************************************************
!       2)  circularisation demandee
!********************************************************************
!
   if(abs(per) > eps_periode ) then
!
!           2.1)  la periode est donnee
!                -----------------------
      hsp=integrateur%pas/per
      integrateur%omeg= 2.0_pm_reel*pm_pi/per
      theta=2.0_pm_reel*pm_pi*hsp
!
   else
!
!           2.2)  la periode est a calculer
!                ---------------------------
      r=sqrt(x(1)**2+x(2)**2+x(3)**2)
      v2=xp(1)**2+xp(2)**2+xp(3)**2
      a=2.0_pm_reel*integrateur%rxmu - v2*r
!
      if(a <= 0.) then
         ier = pm_err_non_ellip
         go to 6000
      endif
!
      a=r*integrateur%rxmu/a
!
      integrateur%omeg=sqrt(integrateur%rxmu/a**3)
      per=2.0_pm_reel*pm_pi/integrateur%omeg
      theta=integrateur%pas*integrateur%omeg
!
   endif
!
!***********************************************************************
!*fon on a theta et integrateur%ordre
!*fon on peut calculer les coefficients
!***********************************************************************
!
!   calcul des constantes
!
   t2=theta/2._pm_reel
   ct=cos(t2)
   st=sin(t2)
   ct=0.5_pm_reel*ct/st - 0.5_pm_reel/t2
   st=0.25_pm_reel/(st*st) - 0.25_pm_reel/(t2*t2)
!
!***********************************************************************
!*fon initialisation des coefficients integrateur%correcteurs
!***********************************************************************
!
   do  j=1,i9
      integrateur%corr(2,j)=0.5_pm_reel*integrateur%dp
      integrateur%corr(4,j)=  ct *integrateur%dp
      integrateur%corr(1,j)=  st *integrateur%d
      integrateur%corr(3,j)=0.0_pm_reel
   end do
!
!***********************************************************************
!*fon boucles de calcul
!***********************************************************************
!
   do  i=1,i9
      ai=i-i5
      del (i)= cos(theta*ai)
      delp(i)= sin(theta*ai)
   end do
!
   ct=del(i5+1)
   st=delp(i5+1)
!
   do  j=i5,i9
!
      do  i=i1,i7
         integrateur%corr(2,j)=integrateur%corr(2,j)- integrateur%beta(j-1,i)*del(i+1)
         integrateur%corr(4,j)=integrateur%corr(4,j)- integrateur%beta(j-1,i)*delp(i+1)
         integrateur%corr(1,j)=integrateur%corr(1,j)-integrateur%alpha(j-1,i)*del(i+1)
         integrateur%corr(3,j)=integrateur%corr(3,j)-integrateur%alpha(j-1,i)*delp(i+1)
      end do
!
      do  i=2,i9
         ic=i9+2-i
         del (ic)=del(ic-1)
         delp(ic)=delp(ic-1)
      end do
!
!***********************************************************************
!*fon test dernier point du segment et calcul du dernier point
!***********************************************************************
!
      if((j-i9) .eq. 0) then
         integrateur%corr(2,j)=integrateur%corr(2,j)/ct
         integrateur%corr(1,j)=integrateur%corr(1,j)/ct
         integrateur%corr(4,j)=integrateur%corr(4,j)+integrateur%corr(2,j)*st
         integrateur%corr(3,j)=integrateur%corr(3,j)+integrateur%corr(1,j)*st
      else 
         ai=j
         del(1)=  cos(theta*ai)
         delp(1)= -sin(theta*ai)
      endif
!
      integrateur%corr(4,j)=integrateur%corr(4,j)*theta  /integrateur%dp
      integrateur%corr(3,j)=integrateur%corr(3,j)*theta*integrateur%pas/integrateur%d
!
   end do
!
!***********************************************************************
!*fon utilisation des relations de symetrie
!***********************************************************************
!
   do  j=1,jord4
      ip=i5+j
      im=i5-j
      integrateur%corr(2,im)= -integrateur%corr(2,ip)
      integrateur%corr(4,im)= integrateur%corr(4,ip)
      integrateur%corr(1,im)= integrateur%corr(1,ip)
      integrateur%corr(3,im)= -integrateur%corr(3,ip)
   end do

6000 continue

 end subroutine  mui_cowell_cowcir
