subroutine mui_cowell_cowsec(fsub_cowell,integrateur,ys1,yps1,s,n1,n2,n4,nup, &
     isimp,y2s,retour_fsub, pb_fsub, retour,korrec)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  
! ===     
!*rol calcul du second membre de l'equation differentielle :
!*rol derivee seconde par rapport a la variable independante ss
!*rol   nb: les ecarts entre le modele de force complet
!*rol       et le modele simplifie sont stockes dans le buffer spys
!
! Note d'utilisation: 
! ================== 
!
!              paramètre d'entrée/sortie
!
!*par integrateur : structure intégrateur 
!
!              parametres d'entree
!
!*par fsub_cowell : fonction de calcul du second membre
!*par ys1    : tableau des fonctions integrees
!*par yps1   : tableau normalise des derivees 1eres par rapport a
!*par          la variable ss
!*par s      : variable independante
!*par n1     : nombre d'equations differentielles utilisateur
!*par n2     : nombre d'equations du second ordre
!*par n4     : nombre total d'equations differentielles
!*par nup    : indice dans le tableau spys
!*par isimp  : cle d'utilisation
!*par              = 0 , aucun calcul , spys progresse
!*par              # 0 , calcul de second membre
!
!              parametres de sortie
!
!*par y2s    : tableau normalise des derivees secondes
!*par          par rapport a s
!*par retour_fsub :  code retour de fsub      
!*par pb_fsub :      abscisse posant pb a fsub
!*par retour : code retour de la fonction ( < 0 si pb)
!*par korrec : paramètre optionnel indiquant le mode de calcul des dérivées secondes
!              lorsque un second membre simplifié est indiqué.
!              si korrec = 0 : calcul à partir du modèle complet
!              si korrec = 1 : calcul à partir du modèle simplifié
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
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   DM-ID 601 : suppression de l'option modele complet/simplifie
!                   (Date: 10/2006 - Realisation: Atos origin)
!                   FA-ID 624 : suppression de nbfomp (inutilisée)
!   + Version 5.7 : DM-ID 738 : Evolution du Cowell
!                   (Date: 06/2007 - Realisation: Sandrine Avril - Atos origin)
!   + Version 5.9 : FA-ID 961 : correction des cartouches
!                   (Date: 03/2008 - Realisation: Atos Origin)
!   + Version 5.10: FA-ID 961 : correction des cartouches
!                   (Date: 10/2008 - Realisation: Atos Origin)
!
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
   use parametre_mspro
   use type_mspro
   use valeur_code_retour_mspro

use int_util_internes_mspro, only : mui_cowell_chavar
use int_util_internes_mspro, only : mui_cowell_normal
use int_util_internes_mspro, only : mui_cowell_tempo

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   type(tm_jour_sec),intent(in)                  ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   end subroutine fsub_cowell
end interface

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n), intent(in) :: ys1
   real(pm_reel), dimension(integrateur%n), intent(in) :: yps1
   real(pm_reel), intent(in) :: s
   integer, intent(in) ::n1
   integer, intent(in) ::n2
   integer, intent(in) ::n4
   integer, intent(in) ::nup
   integer, intent(in) ::isimp
   real(pm_reel), dimension(integrateur%n), intent(out) :: y2s
   integer, intent(out)  ::  retour_fsub            ! code retour de fsub
   type(tm_jour_sec),  intent(out) ::  pb_fsub      ! abscisse posant pb a fsub
   integer, intent(out) ::retour
   integer, intent(in), optional   :: korrec   ! indicateur de la fonction à utiliser
                                               ! dans le cas d'un second memvre simplifié
                                               ! korrec = 0 : modèle complet
                                               ! korrec = 1 : modèle simplifié

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
   
   integer :: i,jjf, n
   integer :: n2p
   type(tm_jour_sec) :: t_courant          ! t courant
!
   real (kind=pm_reel) :: c1,c2,c3,c4,c5,c6,c7,cv
   real (kind=pm_reel) :: secf,sv,te,vv
   real(pm_reel), dimension(integrateur%n) :: y, yn, yp, ypn
   real(pm_reel), dimension(integrateur%n) :: yps, ys

   real(pm_reel), dimension(integrateur%n) :: y_work           ! vecteur d'etat de travail
   real(pm_reel), dimension(integrateur%n) :: deriv
   integer :: dim_mat, dim_matsur2      ! dimension de la matrice de transition

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_cowell_cowsec.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! autres initialisations

   retour_fsub = pm_OK
   pb_fsub%jour = 0
   pb_fsub%sec = 0._pm_reel

   y(:)=0._pm_reel
   yn(:)=0._pm_reel
   yp(:)=0._pm_reel
   ypn(:)=0._pm_reel
   yps(:)=0._pm_reel
   ys(:)=0._pm_reel
   y_work(:)=0._pm_reel
   deriv(:)=0._pm_reel
   dim_mat = 0
   dim_matsur2 = 0

! pour assurer ys1 et yps1 comme parametres entree
   do  i=1,n4
      ys(i)=ys1(i)
      yps(i)=yps1(i)
   end do
!

   if(isimp /= 0) then
!
!***********************************************************************
!*fon restitution et denormalisation de y,yp (grandeurs physiques)
!*fon et du temps pour appel a la fonction second membre utilisateur
!***********************************************************************
!
      call mui_cowell_chavar(integrateur,yn,ys,ypn,yps,s,pm_i_nze,n1,n2,n4,retour)
      if (retour /= pm_OK) go to 6000

      call mui_cowell_normal(integrateur,yn,ypn,y,yp,te,pm_i_nun,n1,n2,n4,retour)
      if (retour /= pm_OK) go to 6000

!
      call mui_cowell_tempo(integrateur%jj,integrateur%sec,te,jjf,secf,retour)
      if (retour /= pm_OK) go to 6000
!
!***********************************************************************
!*fon constitution de la derivee seconde 
!***********************************************************************
!
      y_work(1:3) = y(1:3) 
      y_work(4:6) = yp(1:3)
      if (integrateur%n > 7) then
         dim_mat = 6*(integrateur%nb_param_lib+4) 
         dim_matsur2 = dim_mat/2
         y_work(7:dim_matsur2 + 7 -1)=y(4:dim_matsur2 + 4 - 1)
         y_work(dim_matsur2+7 : dim_mat+7 -1) = yp(4:dim_matsur2 + 4 -1)
      endif
      if (integrateur%calcul_masse) then
         y_work(integrateur%n) = y(dim_matsur2 + 4)
      endif
      
      t_courant%jour = jjf
      t_courant%sec = secf

      if (integrateur%adr_fcowell_simp /=0) then ! un second membre simplifié est indiqué
         if (korrec == 0) then !! utilisation du modèle complet
            call fsub_cowell(t_courant,y_work(:),deriv(:),retour_fsub)
            if (retour_fsub /= pm_OK) then
               pb_fsub = t_courant
               if (retour_fsub > 0) then ! Avertissement
                  retour = pm_warn_sub
               else                      ! Erreur
                  retour = pm_err_sub
                  go to 6000
               end if
            end if
            !! appel de la fonction second membre simplifié
            call MsproAppelFonction4(integrateur%adr_fcowell_simp, &
                 t_courant,y_work(:),integrateur%spys(:,nup),retour_fsub)
            if (retour_fsub /= pm_OK) then
               pb_fsub = t_courant
               if (retour_fsub > 0) then ! Avertissement
                  retour = pm_warn_sub
               else                      ! Erreur
                  retour = pm_err_sub
                  go to 6000
               end if
            end if
            integrateur%spys(:,nup) = deriv(:) - integrateur%spys(:,nup)

         else !! utilisation du modèle simplifié
            call MsproAppelFonction4(integrateur%adr_fcowell_simp, &
                 t_courant,y_work(:),deriv(:),retour_fsub)
            if (retour_fsub /= pm_OK) then
               pb_fsub = t_courant
               if (retour_fsub > 0) then ! Avertissement
                  retour = pm_warn_sub
               else                      ! Erreur
                  retour = pm_err_sub
                  go to 6000
               end if
            end if
            deriv(:) = deriv(:) + integrateur%spys(:,nup)
          endif
       else !! modèle complet uniquement
          call fsub_cowell(t_courant,y_work(:),deriv(:),retour_fsub)
          if (retour_fsub /= pm_OK) then
             pb_fsub = t_courant
             if (retour_fsub > 0) then ! Avertissement
                retour = pm_warn_sub
             else                      ! Erreur
                retour = pm_err_sub
                go to 6000
             end if
          end if
          !
          do  i=1,n1
             deriv(i)=deriv(i)+integrateur%spys(i,nup)
          end do
       endif
!
!
!***********************************************************************
!*fon deriv contient le second membre au sens utilisateur
!*fon   on veut yyss : second membre de l'integration
!*fon changement de variable(s)
!***********************************************************************
!
      if(integrateur%ireg == 0) then
!
!***********************************************************************
!*fon   variable independante:temps
!***********************************************************************
!
         do  i=1,n1
            y2s(i)=deriv(i)
         end do
!
         y2s(n4)=0._pm_reel

      endif
!

!
!***********************************************************************
!*fon   variable independante : anomalie excentrique
!***********************************************************************
!
      if ( integrateur%ireg == 1 ) then
!
         c1=(y(1)*yp(1)+y(2)*yp(2)+y(3)*yp(3))/(integrateur%a0*integrateur%a0)
         c2=y(1)*y(1)+y(2)*y(2)+y(3)*y(3)
         c3=sqrt(c2)/integrateur%a0
         c2=c2/(integrateur%a0*integrateur%a0)
!
         do  i=1,n2
            y2s(i)=c1*yp(i)+c2*deriv(i)
         end do
!
         if(n1 /= n2) then
!
            n2p=n2+1
!
            do  i=n2p,n1
               y2s(i)=c3*deriv(i)
            end do
!
         endif
!
         if (integrateur%ihyp == 0) then
! ** cas elliptique: on integre la partie periodique du temps
!
            y2s(n4)=c3-1.
         else
! ** cas hyperbolique: on integre directement le temps
!
            y2s(n4)=c3
         endif
!
      endif
!
!***********************************************************************
!*fon   variable independante : anomalie vraie
!*fon     (sans changement des variables integrees)
!***********************************************************************
!
      if ( integrateur%ireg == 2 ) then
!
         c3=(y(1)*y(1)+y(2)*y(2)+y(3)*y(3))/integrateur%c02
         c2=c3**2
         c1=(y(1)*yp(1)+y(2)*yp(2)+y(3)*yp(3))*c3/integrateur%c02
         c1=2.*c1
! 
         do  i=1,n2
            y2s(i)=c1*yp(i)+c2*deriv(i)
         end do
!
         if(n1 /= n2) then
!
            n2p=n2+1
!
            do  i=n2p,n1
               y2s(i)=c3*deriv(i)
            end do
!
         endif
!
         if (integrateur%ihyp == 0) then
! ** cas elliptique: on integre la partie periodique du temps
!
            y2s(n4)=c3-1.
         else
! ** cas hyperbolique: on integre directement le temps
!
            y2s(n4)=c3
         endif
!
      endif
!
!***********************************************************************
!*fon   variable independante : anomalie vraie
!*fon     (avec changement des variables integrees x=x/r)
!***********************************************************************
!
      if ( integrateur%ireg == 3 ) then
!
         vv=integrateur%anv0+integrateur%xn*s
         sv=sin(vv)
         cv=cos(vv)
         c1=1.+integrateur%exc*cv
         c2=ys(1)*ys(1)+ys(2)*ys(2)+ys(3)*ys(3)
         c7=(c2-(integrateur%a0**2))/integrateur%c02
         c7=c7*((integrateur%param/(integrateur%a0*c1))**2)
         c3=ys(1)*yps(1)+ys(2)*yps(2)+ys(3)*yps(3)
         c3=2.*c3/c2
         c4=(c2/integrateur%c02)*(integrateur%param/(integrateur%a0*c1))
         c2=(c2/integrateur%c02)**2
         c2=c2*((integrateur%param/(integrateur%a0*c1))**3)
         c5=integrateur%xn*integrateur%exc*sv/c1
         c6=c3*sv-integrateur%xn*cv
         c6=c6*integrateur%xn*integrateur%exc/c1
!
         do  i=1,n2
            y2s(i)=deriv(i)*c2+yps(i)*c3+ys(i)*c6
         end do
!
         if(n1 /= n2) then
!
            n2p=n2+1
!
            do  i=n2p,n1
               y2s(i)=deriv(i)*c4-yps(i)*c5
            end do
!
         endif
!
         y2s(n4)=c7
      endif
!
!***********************************************************************
!*fon simplification, spys progresse d'un cran apres chaque iteration
!*fon c'est fait dans l'appel fictif de second avec isimp=0
!***********************************************************************
!
   else
!
      do  i=integrateur%nd,integrateur%nfp
         do  n=1,n1
            integrateur%spys(n,i)=integrateur%spys(n,i+1)
         end do
      end do
!
   endif
!
6000 continue

 end subroutine  mui_cowell_cowsec
