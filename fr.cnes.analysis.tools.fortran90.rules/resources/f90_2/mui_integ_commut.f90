subroutine mui_integ_commut ( integrateur, prem_fois, prem_commut, der_commut, t_commut, action, retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Reperer l'occurrence d'un evenement dans l'intervalle de temps specifie
! ===   grace aux routines de commutation.
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
!             routine definie a partir de la classe SwitchState
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
!   + Version 5.4 : DM-ID 472 : Amelioration de la detection d evenement
!                         (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use int_util_internes_mspro, only : mui_integ_racine
use int_util_internes_mspro, only : mui_integ_interp

use type_mspro
use parametre_mspro
use parametre_interne_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur),      intent(inout)                  ::  integrateur   ! integrateur
logical,                   intent(in)                     ::  prem_fois     ! indique si on doit faire une initialisation
integer,                   intent(out)                    ::  prem_commut   ! numero de la 1ere commutation (0 si aucune)
integer,                   intent(out)                    ::  der_commut    ! numero de la derniere commutation (0 si aucune)
real(pm_reel),             intent(out)                    ::  t_commut      ! temps de la 1ere commutation
integer,                   intent(out)                    ::  action        ! action a effectuer
integer,                   intent(out)                    ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel),dimension(integrateur%n) :: y_deb         ! valeur de y a t_deb
real(pm_reel),dimension(integrateur%n) :: yb            ! valeur de y a tb
real(pm_reel)                          :: gval,ga,gb    ! valeurs de g
real(pm_reel)                          :: ta,tb,t_deb,t_fin, t_c         ! abscisses

! pour la recherche de racine
integer      :: nb_racine        ! indication sur le nombre de racines
integer      :: criter_arret     ! critere d'arret utilise
real(pm_reel):: sol              ! zero de g

integer       :: npas ! nombre de sous pas
real(pm_reel) :: h    ! valeur du pas

logical       :: commutation,evenement ! indique s'il y a commutation, et/ou evenement 
logical, dimension(integrateur%nb_commut) :: croissant     ! indique le sens de variation de chaque g

integer :: i,j       ! compteur
integer :: retour_local  ! code retour local

external MsproAppelFonction4
intrinsic abs,max,ceiling,int

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_integ_commut.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

t_deb = integrateur%t_deb
t_fin = integrateur%t_fin
t_c=0._pm_reel

if (prem_fois) then     
! il faut initialiser
   
   call mui_integ_interp(integrateur,t_deb,y_deb,retour_local)
   if (retour_local /= pm_OK) then
      retour = retour_local
      if (retour_local < pm_OK) go to 6000
   end if
   do i=1,integrateur%nb_commut
      integrateur%g_commut(i)%prem_even = pm_i_oui
      integrateur%g_commut(i)%even_attendu = pm_i_non
      integrateur%g_commut(i)%t_last_even = 0._pm_reel    ! le flag prem_even garantit que la valeur 0 ne gene pas
      integrateur%g_commut(i)%t_even_attendu = 0._pm_reel
      call MsproAppelFonction4(integrateur%g_commut(i)%adresse,t_deb,y_deb,gval,retour_local)
      if (retour_local /= pm_OK) then
         retour = pm_err_dans_sub_commut
         go to 6000
      end if
      integrateur%g_commut(i)%g0 = gval
      integrateur%g_commut(i)%g0positif = (gval >= 0._pm_reel)
      integrateur%g_commut(i)%nb_commut = 0
   end do

end if

prem_commut = 0
action  = pm_CONTINUE

do i=1,integrateur%nb_commut   ! boucle sur toutes les subroutines de commutation

   commutation = pm_i_non
   croissant(i) = pm_i_oui

   npas = max(int(1), ceiling(abs(t_fin-t_deb)/integrateur%g_commut(i)%max_deltat))
   h = (t_fin-t_deb)/real(npas,pm_reel)
   ta = t_deb
   ga = integrateur%g_commut(i)%g0
   tb = t_deb + sign(integrateur%g_commut(i)%eps_converg,(t_fin-t_deb))

   evenement = pm_i_non
   j = 1 
   do while ((j <= npas).and..not.evenement)
      ! valeur de g a la fin du sous-pas
      tb = tb + h
      call mui_integ_interp(integrateur,tb,yb,retour_local)
      if (retour_local /= pm_OK) then
         retour = retour_local
         if (retour_local < pm_OK) go to 6000
      end if
      call MsproAppelFonction4(integrateur%g_commut(i)%adresse,tb,yb,gb,retour_local)
      if (retour_local /= pm_OK) then
         retour = pm_err_dans_sub_commut
         go to 6000
      end if
      ! on verifie l'occurrence d'un evenement
      if ((integrateur%g_commut(i)%g0positif.and.(gb < 0._pm_reel)).or. &
           (.not.integrateur%g_commut(i)%g0positif.and.(gb >= 0._pm_reel))) then
         ! changement de signe
         croissant(i) = (gb >= ga)
         call mui_integ_racine (integrateur, i, ta, ga, tb, gb, integrateur%g_commut(i)%eps_converg, &
              pm_i_NB_MAX_ITER, nb_racine, criter_arret, sol, retour_local)
         if ((retour_local /= pm_OK).or.(criter_arret == pm_criter_arret_iter_max).or. &
              (nb_racine /= pm_1racine)) then
            retour = pm_err_cherche_racine
            go to 6000
         end if
         if ((integrateur%g_commut(i)%prem_even).or. &
              (abs(integrateur%g_commut(i)%t_last_even - sol) > integrateur%g_commut(i)%eps_converg)) then
            ! nouvel evenement trouve
            integrateur%g_commut(i)%t_even_attendu = sol
            if (integrateur%g_commut(i)%even_attendu.and.(abs(t_fin - sol) <=  integrateur%g_commut(i)%eps_converg)) then 
               ! il s'agit en fait d'un evenement deja trouve, mais qui a fait rejeter le pas precedent
               ! il faut donc accepter ce pas qui se termine exactement a l'evenement
               commutation = pm_i_non
            else
               ! il s'agit vraiment d'un evenement inattendu. Il y a commutation
               ! Le pas est donc rejete
               commutation = pm_i_oui
               integrateur%g_commut(i)%even_attendu = pm_i_oui
            end if
            evenement = pm_i_oui
         end if
      else
         ! pas de changement de signe pour l'instant
         ta = tb
         ga = gb
      end if
      
      j = j+1
   end do
   
   if (.not.evenement) then
      integrateur%g_commut(i)%even_attendu = pm_i_non
   else 
      if (commutation) then
         if (prem_commut == 0) then
            ! 1ere commutation
            prem_commut = i
            t_c = integrateur%g_commut(i)%t_even_attendu
         else
            ! determiner la 1ere commutation, suivant le sens de l'integration
            if (((t_fin >= t_deb).and.(integrateur%g_commut(i)%t_even_attendu < t_c)).or. &
                 ((t_fin < t_deb).and.(integrateur%g_commut(i)%t_even_attendu > t_c))) then
               prem_commut = i
               t_c = integrateur%g_commut(i)%t_even_attendu
            end if
         end if
         
         der_commut = i
         integrateur%g_commut(i)%nb_commut = integrateur%g_commut(i)%nb_commut + 1
      end if
   end if

end do  ! fin boucle sur toutes les subroutines de commutation

if (prem_commut == 0) then    
   ! pas de commutation, le pas est accepte. On prepare le prochain pas.
   do i=1,integrateur%nb_commut
      call MsproAppelFonction4(integrateur%g_commut(i)%adresse,t_fin,integrateur%y_fin,gval,retour_local)
      if (retour_local /= pm_OK) then
         retour = pm_err_dans_sub_commut
         go to 6000
      end if
      integrateur%g_commut(i)%g0 = gval
      if (integrateur%g_commut(i)%even_attendu) then
         ! l'evenement est a la fin de ce pas.
         ! On force le signe a la valeur juste apres l'evenement
         integrateur%g_commut(i)%t_last_even = t_fin
         integrateur%g_commut(i)%prem_even = pm_i_non
         integrateur%g_commut(i)%g0positif = croissant(i)
         action = integrateur%g_commut(i)%action
         integrateur%g_commut(i)%even_attendu = pm_i_non
      else
         integrateur%g_commut(i)%g0positif = (gval >= 0._pm_reel)
      end if
   end do
end if
   
t_commut = t_c

6000 continue

end subroutine mui_integ_commut
