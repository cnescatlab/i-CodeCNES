subroutine mui_cowell_cowreg(integrateur,y,yp,per,icircl,ier)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol calcul des constantes relatives aux changements de variables
!*rol chargement de coreg
!        ireg=0  :pas de regularisation
!        ireg=1  :variable independante s
!                 avec ds/dt=r/a , t=theta0+u+sigma
!                 si on prend t=sigma=0 a l'origine de l'integ
!                 on initialise theta0=-u(0)=rvect*vvect*a/rxmu
!        ireg=2,3,4,5...  :autres chgts de variable
!
! Note d'utilisation: 
! ================== 
!    paramètre d'entrée/sortie
!
!*par integrateur :   integrateur utilise                               
!
!    paramètres entrée
!
!*par y           :   fonctions integrees                               
!*par yp          :   derivees                                          
!*par per         :   periode reelle du mouvement pseudo-circulaire     
!*par icircl      :   cle de circularisation                            
!
!    paramètre de sortie
!*par ier         :   code retour                                       
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
!                   FA-ID 624 : initialisation de la variable u0
!     + Version 5.9 : FA-ID 961 : maj des cartouches des routines internes du Cowell
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

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                intent(inout)  :: integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n), intent(in) :: y            ! fonctions integrees
   real(pm_reel), dimension(integrateur%n), intent(in) :: yp           ! derivees
   real(pm_reel), intent(in) :: per                                    ! periode reelle du mouvement pseudo-circulaire
   integer, intent(in) :: icircl                                       ! cle de circularisation
   integer, intent(out) :: ier                                         ! code retour                                       

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   real(pm_reel) :: amoy,ce,cv,e2,ece,ee,en,ese
   real(pm_reel) :: p,r,se
   real(pm_reel) :: sv,u0,v2,vv,x1,x2,x3

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_cowreg.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
   ier = pm_OK

   ! Initialisation de u0
   u0=0._PM_REEL

   if(integrateur%ireg == 0) then
!
!***********************************************************************
!*fon variable independante : temps
!***********************************************************************
!
      integrateur%a0=1.0_pm_reel
      integrateur%theta0=0.0_pm_reel
      goto 6000
!
   endif

!******************************************************************
!*fon determination du type de trajectoire
!******************************************************************
!
   r=sqrt(y(1)*y(1) + y(2)*y(2) + y(3)*y(3) )
   v2=yp(1)*yp(1) + yp(2)*yp(2) + yp(3)*yp(3)
   en=(v2/2.0_pm_reel) - (integrateur%rxmu/r)
!
   if (en  > 0.0) then
      integrateur%ihyp=1
   else
      integrateur%ihyp=0
   endif

!***********************************************************************
!*fon variable independante : anomalie excentrique
!***********************************************************************
   if(integrateur%ireg == 1) then
!***********************************************************************
!*fon  1) cas de l'ellipse
!***********************************************************************

      if (en <=  0.0_pm_reel) then
         integrateur%a0=r*integrateur%rxmu / (2.0_pm_reel*integrateur%rxmu - r*v2)
         u0=y(1)*yp(1) + y(2)*yp(2) + y(3)*yp(3)
         u0=-u0*integrateur%a0/integrateur%rxmu
         integrateur%theta0=-u0
         goto 6000
      else
!*******************************************************************
!*fon cas impossible: circularisation dans le cas d'une hyperbole
!*******************************************************************
         if (icircl  ==  1) then
            ier = pm_err_integ_hyp
            goto 6000
         endif
!
!*******************************************************************
!* fon  2) cas de l'hyperbole
!*******************************************************************
!
         integrateur%a0=integrateur%rxmu/(2*en)
         u0=0.0_pm_reel
         integrateur%theta0=-u0
         goto 6000
         
      endif

!
!***********************************************************************
!*fon variable independante : anomalie vraie
!***********************************************************************
!
   elseif (integrateur%ireg == 2 .or. integrateur%ireg == 3) then 
      integrateur%a0=r*integrateur%rxmu / (2.0_pm_reel*integrateur%rxmu - r*v2)
!
      if (en <= 0.0_pm_reel) then
!
!******************************************************************
!*fon 1) cas de l'ellipse
!********************************************************************
!
         x1=(y(2)*yp(3)-y(3)*yp(2))**2
         x2=(y(1)*yp(3)-y(3)*yp(1))**2
         x3=(y(1)*yp(2)-y(2)*yp(1))**2
!
         integrateur%c02=(x1+x2+x3)*(integrateur%a0**3)/integrateur%rxmu
         integrateur%c02=sqrt(integrateur%c02)
!
         ese=y(1)*yp(1) + y(2)*yp(2) + y(3)*yp(3)
         ese=ese/(sqrt(integrateur%rxmu*integrateur%a0))
         ece=1.-(r/integrateur%a0)
!
         e2=ece**2 + ese**2
         integrateur%exc=sqrt(e2)
         integrateur%ume2=1.0_pm_reel - e2
         p=integrateur%a0*integrateur%ume2
         integrateur%rume2=sqrt(integrateur%ume2)
!
         se=ese/integrateur%exc
         ce=ece/integrateur%exc
!
         ee=atan2(se,ce)
!
         if(se <= 0._pm_reel) then
            ee=2.0_pm_reel*pm_pi+ee
         endif
  
         amoy=ee-ese
         sv=se*integrateur%rume2/(1.0_pm_reel-ece)
         cv=(ce-integrateur%exc)/(1.0_pm_reel-ece)
         vv=atan2(sv,cv)
!
         if(sv <= 0._pm_reel) then
            vv=2.0_pm_reel*pm_pi+vv
         endif
 
         u0=amoy-vv
         u0=u0*sqrt((integrateur%a0**3)/integrateur%rxmu)
         integrateur%param=p
         integrateur%anv0=vv
!
         if(integrateur%ireg == 3) then
!   si circularisation , per est calculee dans cowcir
!   ( periodicite de l'anomalie vraie )
!
            integrateur%xn=2.0_pm_reel*pm_pi/per
         else
!   formule de kepler si icircl=0
!
            integrateur%xn=sqrt(integrateur%rxmu/(integrateur%a0**3))
         endif
!
         integrateur%theta0=-u0
         goto 6000
      else

!*******************************************************************
!*fon 2) cas de l'hyperbole
!*******************************************************************
!
!*******************************************************************
!*fon cas impossible: circularisation dans le cas d'une hyperbole
!*******************************************************************
         if (icircl == 1) then
            ier = pm_err_integ_hyp
            goto 6000
         endif
!
         integrateur%a0=integrateur%rxmu/(2*en)
!
         x1=(y(2)*yp(3)-y(3)*yp(2))**2
         x2=(y(1)*yp(3)-y(3)*yp(1))**2
         x3=(y(1)*yp(2)-y(2)*yp(1))**2
!
         integrateur%c02=(x1+x2+x3)*(integrateur%a0**3)/integrateur%rxmu
         integrateur%c02=sqrt(integrateur%c02)
!
         u0=0.0_pm_reel

         integrateur%theta0=-u0
         goto 6000
      endif
!
!***********************************************************************
!*fon autres cas
!***********************************************************************
!

   elseif (integrateur%ireg == 4 .or. integrateur%ireg == 5 .or. integrateur%ireg == 6) then
      integrateur%theta0=-u0
   endif
!

6000 continue

 end subroutine  mui_cowell_cowreg
