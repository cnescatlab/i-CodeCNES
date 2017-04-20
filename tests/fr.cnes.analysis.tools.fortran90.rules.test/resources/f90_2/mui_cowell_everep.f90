subroutine mui_cowell_everep(hpas,y,tm,t,n,f,retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol interpolation par la methode d'everett d'ordre parametrable
!
! Note d'utilisation: 
! ================== 
!              parametres d'entree
!
!*par hpas   : pas des abscisses equidistantes
!*par y      : tableau contenant les ordonnees connues
!*par n      : entier pair <= 16 : dimension du tableau y
!*par tm     : abcisse du point milieu : (n/2+1)eme abscisse
!*par t      : abscisse a interpoler (comprise entre tm-hpas et tm )
!
!              parametres de sortie
!
!*par f      : resultat de l'interpolation
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
!     + Version 5.5 : modification
!                     DM-ID 476 : Matrices de transition
!                     (Date: 04/2006 - Realisation: Atos Origin)
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

   real(pm_reel), intent(in) :: hpas
   real(pm_reel), dimension(*), intent(in) :: y
   real(pm_reel), intent(in) :: tm
   real(pm_reel), intent(in) :: t
   integer, intent(in) :: n
   real(pm_reel), intent(out) :: f
   integer, intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
     integer :: i,ia,k,nm
!
   real(pm_reel) :: a,b
   real(pm_reel), dimension(8) :: d0,d1
   real(pm_reel), dimension(16) :: e
   real(pm_reel) ::r,r2,s,s2
   real(pm_reel), dimension(8) :: tr, ts
   real(pm_reel) :: xmin,xmax 

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_everep.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK
!
! autres initialisations
! ..........................................
   d0(:) = 0._pm_reel
   d1(:) = 0._pm_reel
   e(:) = 0._pm_reel
   tr(:) = 0._pm_reel
   ts(:) = 0._pm_reel

!************************************************************************

   r=(tm-t)/hpas
   r2=r*r
   s=1.-r
   s2=s*s
   nm=n/2
   ia=8-nm
!
   do  i=1,n
      e(ia+i)=y(i)
   end do
!
   ts(1)=s
   tr(1)=r
!
   if(nm /= 1) then
!
      do  i=2,nm
         k=i-1
         a=real(k*k,kind=pm_reel)
         b=real(4*k*k+2*k,kind=pm_reel)
         ts(i)=ts(i-1)*(s2-a)/b
         tr(i)=tr(i-1)*(r2-a)/b
      end do
!
   endif
!
!***********************************************************************
!*fon ordre superieur ou egal a 2
!***********************************************************************
!
   if (n >= 2) then 
      d0(1)=e(8) 
      d1(1)=e(9)
   endif

   if (n >= 4) then
!***********************************************************************
!*fon ordre superieur ou egal a 4
!***********************************************************************
      d0(2)=(e(9)-d0(1))+(e(7)-d0(1))
      d1(2)=(e(10)-d1(1))+(e(8)-d1(1))
   endif
!
   if(n >= 6) then
!***********************************************************************
!*fon ordre superieur ou egal a 6
!***********************************************************************
      d0(3)=(e(10)-d0(1))+(e(6)-d0(1))-4.*d0(2)
      d1(3)=(e(11)-d1(1))+(e(7)-d1(1))-4.*d1(2)
   endif
!
   if(n >= 8) then
!***********************************************************************
!*fon ordre superieur ou egal a 8
!***********************************************************************
      d0(4)=(e(11)-d0(1))+(e(5)-d0(1))-6.*d0(3)-9.*d0(2)
      d1(4)=(e(12)-d1(1))+(e(6)-d1(1))-6.*d1(3)-9.*d1(2)
   endif
!
   if(n >= 10) then
!***********************************************************************
!*fon ordre superieur ou egal a 10
!***********************************************************************
      d0(5)=(e(12)-d0(1))+(e(4)-d0(1))-8.*d0(4)-20.*d0(3)-16.*d0(2)
      d1(5)=(e(13)-d1(1))+(e(5)-d1(1))-8.*d1(4)-20.*d1(3)-16.*d1(2)
   endif

   if(n >= 12) then
!***********************************************************************
!*fon ordre superieur ou egal a 12
!***********************************************************************
      d0(6)=(e(13)-d0(1))+(e(3)-d0(1))-10.*d0(5)-35.*d0(4)-50.*d0(3)- &
           25.*d0(2)
      d1(6)=(e(14)-d1(1))+(e(4)-d1(1))-10.*d1(5)-35.*d1(4)-50.*d1(3)- &
           25.*d1(2)
   endif
!
   if(n >= 14) then
!***********************************************************************
!*fon ordre superieur ou egal a 14
!***********************************************************************
      d0(7)=(e(14)-d0(1))+(e(2)-d0(1))-12.*d0(6)-54.*d0(5)- &
           112.*d0(4)-105.*d0(3)-36.*d0(2)
      d1(7)=(e(15)-d1(1))+(e(3)-d1(1))-12.*d1(6)-54.*d1(5)- &
           112.*d1(4)-105.*d1(3)-36.*d1(2)
   endif
!
   if(n >= 16) then
!***********************************************************************
!*fon ordre=16
!***********************************************************************
      d0(8)=(e(15)-d0(1))+(e(1)-d0(1))-14.*d0(7)-77.*d0(6)- &
           210.*d0(5)-294.*d0(4)-196.*d0(3)-49.*d0(2)
      d1(8)=(e(16)-d1(1))+(e(2)-d1(1))-14.*d1(7)-77.*d1(6)- &
           210.*d1(5)-294.*d1(4)-196.*d1(3)-49.*d1(2)
   endif
!
   xmin=0._pm_reel
   xmax=0._pm_reel
!
   do  i=1,nm
      xmin=xmin+d0(i)*tr(i)
      xmax=xmax+d1(i)*ts(i)
   end do
!
   f=xmin+xmax
!
   
 end subroutine  mui_cowell_everep
