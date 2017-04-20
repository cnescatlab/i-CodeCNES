subroutine mui_cowell_inicow(fsub_cowell,integrateur,y,yp,icircl,n1,n2,n4, de, retour_fsub, pb_fsub, ier)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol initialisation des derivees secondes dans les tableaux de cowell
!*rol selon icircl a des valeurs dependantes ou non de k
!
! Note d'utilisation: 
! ================== 
!
!              parametres d'entrée/sortie
!*par integrateur : intégrateur utilisé
!
!              parametres d'entrée
!*par fsub_cowell : équation différentielle à intégrer 
!*par y      : fonctions integrees
!*par yp     : derivees des fonctions integrees / variable indep.
!*par icircl : cle de circularisation
!*par n1     : nombre total d'equations differentielles
!*par n2     : nombre d'equations du second ordre
!*par n4     : nombre total d'equations differentieeles
!
!              parametres de sortie
!
!*par de     : tableau des differences secondes de cowell
!*par retour_fsub    : code retour de fsub
!*par pb_fsub        : abscisse ayant causé le problème dans fsub
!*par ier    : code retour (0 si ok)
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
!                     (Date: 02/2006 - Realisation: Atos Origin)
!                     FA-ID 439 : remarques qualite
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + version 5.7 : DM-ID 738 : Evolution du cowell
!                   (Date: 06/2007 - Realisation: Sandrine Avril- Atos origin)
!   + Version 5.9 : FA-ID 961 : correction des cartouches
!                   (Date: 03/2008 - Realisation: Atos Origin)
!   + Version 5.10: FA-ID 961 : correction des cartouches
!                   DM-ID 1058: Correction des warnings levés par g95
!                   (Date: 08/2008 - Realisation: Atos Origin)
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

use int_util_internes_mspro, only : mui_cowell_cowsec

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   interface
      subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle
        
        use mslib
        
        type(tm_jour_sec),intent(in)                    ::  t     ! abscisse
        real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
        real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
        integer,                   intent(out)          ::  retour
   
      end subroutine fsub_cowell
   end interface
         
   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n) , intent(in):: y  ! fonctions integrees
   real(pm_reel), dimension(integrateur%n) , intent(in):: yp ! derivees des fonctions integrees / variable indep.
   integer , intent(in) :: icircl ! cle de circularisation
   integer , intent(in) :: n1 ! nombre total d'equations differentielles
   integer , intent(in) :: n2 ! nombre d'equations du second ordre
   integer , intent(in) :: n4 ! nombre total d'equations differentielles

   real(pm_reel), dimension(integrateur%n,pm_i_dim17) , intent(out) :: de ! tableau des differences secondes de cowell
   integer , intent(out) ::  retour_fsub  ! code retour de fsub
   type(tm_jour_sec),  intent(out) ::  pb_fsub      ! abscisse posant pb a fsub
   integer , intent(out) :: ier ! code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations

    integer :: i,j,k  ! indices de boucle

   real (kind=pm_reel) :: arg
   real(pm_reel), dimension(integrateur%n) :: y1
   real(pm_reel), dimension(integrateur%n) :: yp1
   integer :: retour_local  

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_inicow.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
   ier = pm_OK

! autres initialisations

   retour_fsub = pm_OK
   pb_fsub%jour = 0
   pb_fsub%sec = 0._pm_reel

   do  i=1,integrateur%n
      y1(i)=0._pm_reel
      yp1(i)=0._pm_reel
   end do
!
! pour assurer y et yp comme parametres entree
   do  i=1,n4
      y1(i)=y(i)
      yp1(i)=yp(i)
   end do
!
   call mui_cowell_cowsec(fsub_cowell,integrateur,y1,yp1,pm_i_zero,n1,n2,n4,pm_i_nne, &
        pm_i_nun,de(1:integrateur%n,9),retour_fsub, pb_fsub, retour_local,korrec = 0)
   if (retour_local /= pm_OK) then
      ier = retour_local
      if (retour_local < pm_OK) go to 6000
   end if

!
!***********************************************************************
!*fon init tableau "de"
!***********************************************************************
!
   if(icircl == 0) then
!
      do  j=1,n4
         do  k=integrateur%nd,integrateur%nfp
            de(j,k)=de(j,9)
         end do
      end do
!
   else
!
      do  j=1,n4
         do  k=integrateur%nd,integrateur%nfp
            arg=(k-9)*integrateur%pas*integrateur%omeg
            de(j,k)=de(j,9)*cos(arg)-integrateur%omeg*yp(j)*sin(arg)
         end do
      end do
!
   endif
!
 
6000 continue

 end subroutine mui_cowell_inicow
