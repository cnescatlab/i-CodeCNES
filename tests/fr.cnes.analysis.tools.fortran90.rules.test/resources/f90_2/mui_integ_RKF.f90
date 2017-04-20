subroutine mui_integ_RKF ( fsub, integrateur, t0, y0, t, y, num_commut, nb_commut,&
     retour_fsub, pb_fsub, retour, evt_commut_tab )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Integration a pas variable de type Runge-Kutta-Fehlberg
! ===
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
!             routine definie a partir de la classe RungeKuttaFehlbergIntegrator
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 : FA-ID 385 : Anomalie mu_integrer 
!                         (Date: 12/09/2005 - Realisation: Claire Fabre)
!   + Version 5.3 : DM-ID 401 : Evolution mu_integrer 
!                         (Date: 23/09/2005 - Realisation: Claire Fabre)
!   + Version 5.4 : FA-ID 474 : Anomalie pas min 
!                         (Date: 02/02/2006 - Realisation: Atos Origin)
!                   DM-ID 472 : Amelioration de la detection d evenement
!                         (Date: 17/02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                         (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.6 : DM-ID 516 : détection d'évènements simultanés par les intégrateurs MSPRO
!                         (Date: 09/2006 - Réalisation : Atos Origin)
!   + VERSION:5.13:FA-ID:1381:05/03/2010: Sortie en erreur quand l'erreur testée est encore trop
!                         grande et que le pas min est déjà atteint 
!                                 (anomalie qui provoquait une boucle infinie)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use parametre_interne_mspro
use parametre_mspro
use type_mspro
use valeur_code_retour_mspro

use int_util_internes_mspro, only : mui_integ_commut
use int_util_internes_mspro, only : mui_integ_interp
use int_util_internes_mspro, only : mui_integ_erreur
use int_util_internes_mspro, only : mui_integ_init_pas
use int_util_internes_mspro, only : mui_interp_init

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   real(pm_reel),intent(in)                        ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   end subroutine fsub
end interface
   
type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
real(pm_reel),                         intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
real(pm_reel),                         intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
integer,                               intent(out)                    ::  num_commut   ! numero de la derniere routine ayant commute
integer,                               intent(out)                    ::  nb_commut    ! nombre total de commutations
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour
!!$ DM_ID 516
type(tm_evt_commut),dimension(:),      pointer,      optional         ::  evt_commut_tab   ! pointeurs sur les évènements de commutation 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), dimension(integrateur%n)    :: y_work           ! vecteur d'etat de travail
real(pm_reel), dimension(integrateur%n)    :: y_tmp            ! vecteur d'etat temporaire
real(pm_reel), dimension(integrateur%n)    :: yDotK            ! vecteur d'etat temporaire

real(pm_reel)  :: somme              ! pour calculs a chaque etape
real(pm_reel)  :: t_courant          ! t courant
real(pm_reel)  :: h,hnew,htest       ! pas
real(pm_reel)  :: erreur             ! erreur estimee du pas

logical        :: prem_fois          ! indique le 1er passage
logical        :: dernier_pas        ! indique l'occurrence du dernier pas
logical        :: boucle             ! test d'entree/sortie de boucle
logical        :: en_avant           ! indique le sens d'integration
logical        :: suivant_est_dernier! le prochain pas pourrait etre le dernier
logical        :: pas_min_atteint

! commutation
integer        :: prem_commut        ! numero de la 1ere commutation (0 si aucune)
integer        :: der_commut        ! numero de la derniere commutation (0 si aucune)
real(pm_reel)  :: t_commut           ! instant de la commutation
integer        :: action             ! action en cas de commutation

integer        :: i,j,k,l,m          ! compteur
integer        :: retour_local       ! retour des subroutines

external MsproAppelFonction6
intrinsic min, max, abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_integ_RKF.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! autres initialisations
num_commut = 0
nb_commut = 0
der_commut = 0

y_work(:) = y0(:)
t_courant = t0
hnew = 0._pm_reel
prem_fois = pm_i_oui
retour_fsub = pm_OK
pb_fsub = 0._pm_reel
en_avant = (t>t0)
action = pm_CONTINUE
pas_min_atteint = pm_i_non

dernier_pas = pm_i_non
do while (.not.dernier_pas)   ! boucle sur les pas
   
   integrateur%t_deb = integrateur%t_fin
   
   h = 0._pm_reel
   erreur = 0._pm_reel
   boucle = pm_i_oui

   do while (boucle)          ! tant que le pas n'est pas accepte
      integrateur%v_initialise = pm_i_non   ! l'interpolateur devra etre initialise
      if (prem_fois.or..not.integrateur%report_fin_deb) then   ! 1ere etape
         do m=1, integrateur%n
            yDotK(m) = integrateur%yDotK(1,m)
         enddo
         call fsub (t_courant, y_work, yDotK(:), retour_fsub)  ! appel a l'equation differentielle
         if (retour_fsub /= pm_OK) then
            pb_fsub = t_courant
            if (retour_fsub > 0) then ! Avertissement
               retour = pm_warn_sub
            else                      ! Erreur
               retour = pm_err_sub
               go to 6000
            end if
         end if
         do m=1, integrateur%n
            integrateur%yDotK(1,m) = yDotK(m)
         enddo
      end if
      
      if (prem_fois) then
         call mui_integ_init_pas ( fsub, integrateur, t0, y0, en_avant, hnew, &
           retour_fsub, pb_fsub, retour_local )
         if (retour_local /= pm_OK) then
            retour = retour_local
            if (retour < pm_OK) go to 6000
         end if
      end if
      
      ! verification du pas par rapport a pas_max
      if (hnew > integrateur%pas_max) then
         h = integrateur%pas_max
      else if (hnew < -integrateur%pas_max) then
         h = - integrateur%pas_max
      else 
         h = hnew
      end if

      ! ajustement du pas pres des bords
      if ((en_avant.and.(t_courant+h>t)).or.(.not.en_avant.and.(t_courant+h<t))) h = t-t_courant
         
      do k=2,integrateur%nb_etapes    ! etapes suivantes
         
         do j=1,integrateur%n
            somme = integrateur%a(k-1,1)*integrateur%yDotK(1,j)
            do l=2,k-1
               somme = somme + (integrateur%a(k-1,l) * integrateur%yDotK(l,j))
            end do
            y_tmp(j) = y_work(j) + h * somme
         end do
         do m=1, integrateur%n
            yDotK(m) = integrateur%yDotK(k,m)
         enddo
         call fsub (t_courant+(h*integrateur%c(k-1)), y_tmp, yDotK(:), &
              retour_fsub)  ! appel a l'equation differentielle
         if (retour_fsub /= pm_OK) then
            pb_fsub = t_courant
            if (retour_fsub > 0) then ! Avertissement
               retour = pm_warn_sub
            else                      ! Erreur
               retour = pm_err_sub
               go to 6000
            end if
         end if
         do m=1, integrateur%n
            integrateur%yDotK(k,m) = yDotK(m)
         enddo

      end do

      ! estimation de l'etat a la fin du pas
      do j=1,integrateur%n
         somme = integrateur%b(1)*integrateur%yDotK(1,j)
         do l=2,integrateur%nb_etapes
            somme = somme + (integrateur%b(l) * integrateur%yDotK(l,j))
         end do
         y_tmp(j) = y_work(j) + h * somme
      end do
      
      ! estimation de l'erreur a la fin du pas
      erreur = 0._pm_reel
      call mui_integ_erreur (integrateur,y_work,y_tmp,h,erreur,retour_local)
      if (retour_local /= pm_OK) then
         retour = retour_local
         if (retour_local < pm_OK) go to 6000
      end if

      if (erreur <= 1._pm_reel) then ! l'erreur est faible

         ! report pour l'interpolateur
         integrateur%y_fin(:) = y_tmp(:)
         integrateur%t_fin = t_courant+h
         
         ! gestion des subroutines de commutation
         if (integrateur%nb_commut > 0) then
            call mui_interp_init(fsub, integrateur, retour_fsub, pb_fsub, retour_local)
            if (retour_local /= pm_OK) then
               retour = retour_local
               if (retour_local < pm_OK) go to 6000
            end if
            call mui_integ_commut(integrateur, prem_fois, prem_commut, der_commut, t_commut, action, retour_local)
            if (retour_local /= pm_OK) then
               retour = retour_local
               if (retour_local < pm_OK) go to 6000
            end if
            if (prem_commut > 0) then        ! il y a eu commutation
               hnew = t_commut - t_courant
               if (abs(hnew)>integrateur%pas_max) then
                  prem_commut = 0
                  hnew = sign(integrateur%pas_max,hnew)
                  boucle = pm_i_non
	       end if
            else
               boucle = pm_i_non
            end if
         else      ! pas de subroutine de commutation
            boucle = pm_i_non
         end if
         
      else   ! erreur trop forte, on change le pas
 
         htest = h * min(pm_i_RKF_max_augm, &
              max(pm_i_RKF_min_reduc,pm_i_RKF_securite*(erreur**(-1._pm_reel/integrateur%ordre))))
         if (abs(htest) < integrateur%pas_min) then
            if (.not.pas_min_atteint) then
               retour = pm_warn_integ_pas_min
               hnew = integrateur%pas_min
               pas_min_atteint = pm_i_oui
            else
               retour = pm_err_integ_pas_min
               go to 6000	       
            endif
         else 
            hnew = htest
         end if

      end if
      prem_fois = pm_i_non
      
   end do
   ! le pas est accepte

   t_courant = t_courant + h
   y_work(:) = y_tmp(:)

   ! action a suivre
   if (action == pm_STOP) then
      dernier_pas = pm_i_oui
      retour = pm_warn_commutation
   else
      if (en_avant) then
         dernier_pas = (t_courant >= t)
      else
         dernier_pas = (t_courant <= t)
      end if
   end if

   if (integrateur%adr_gest_pas /= 0) then 
   ! un gestionnaire de pas est enregistre
      if (.not.integrateur%v_initialise) then
         call mui_interp_init(fsub, integrateur, retour_fsub, pb_fsub, retour_local)
         if (retour_local /= pm_OK) then
            retour = retour_local
            if (retour_local < pm_OK) go to 6000
         end if
      end if
      call MsproAppelFonction6(integrateur%adr_gest_pas,integrateur,mui_integ_interp, &
           integrateur%t_deb,t_courant,dernier_pas, retour_local)
      if (retour_local /= pm_OK) then
         retour = retour_local
         if (retour_local < pm_OK) go to 6000
      end if
   end if

   if ( integrateur%report_fin_deb ) then
      ! on sauvegarde la derniere evaluation pour le pas suivant
      integrateur%yDotK(1,:) = integrateur%yDotK(integrateur%nb_etapes,:)
   end if

   if (.not.dernier_pas) then
      ! preparation de la taille du pas pour le pas suivant
      htest = h * min(pm_i_RKF_max_augm, &
           max(pm_i_RKF_min_reduc,pm_i_RKF_securite*(erreur**(-1._pm_reel/integrateur%ordre))))
      if (en_avant) then
         suivant_est_dernier = (t_courant+htest >= t)
      else
         suivant_est_dernier = (t_courant+htest <= t)
      end if
      if (abs(htest) < integrateur%pas_min) then
         if (suivant_est_dernier) then
            hnew = sign(integrateur%pas_min,h)
         else
            if (.not.pas_min_atteint) then
               retour = pm_warn_integ_pas_min
               hnew = sign(integrateur%pas_min,h)
               pas_min_atteint = pm_i_oui
            else
	       retour = pm_err_integ_pas_min
               go to 6000	       
            endif
         end if
      else if (htest > integrateur%pas_max) then
         hnew = integrateur%pas_max
      else if (htest < -integrateur%pas_max) then
         hnew = - integrateur%pas_max
      else 
         hnew = htest
      end if
   end if

end do ! fin boucle sur les pas

! report de la valeur de travail
y(:) = y_work(:)

! Récupération du numero de la derniere routine ayant commuté et du 
! nombre total de commutations
if (der_commut .ne. 0) then
   num_commut = integrateur%g_commut(der_commut)%ident
endif
do i = 1, integrateur%nb_commut
   nb_commut = nb_commut + integrateur%g_commut(i)%nb_commut
enddo

!!$ DM_ID 516
!!$ on remplit le tableaux des tm_evt_commut
if (present(evt_commut_tab)) then
   allocate(evt_commut_tab(nb_commut))
   j = 1
   do i = 1, integrateur%nb_commut
      if (integrateur%g_commut(i)%nb_commut /= 0) then
         evt_commut_tab(j)%ident = integrateur%g_commut(i)%ident
         evt_commut_tab(j)%abscisse = integrateur%g_commut(i)%t_even_attendu
         j = j+1
      endif
   enddo
endif

6000 continue

end subroutine mui_integ_RKF
