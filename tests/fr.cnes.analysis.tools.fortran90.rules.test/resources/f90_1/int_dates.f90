module int_dates
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème D
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement par l'intermédiaire du module global
!   "mslib90"
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 (option) : inclusion des constantes du module 
!     indic_comp_joursec_mslib en vue de la suppression de ce module
!                   (Date: 05/2007 - Realisation: Atos origin) 
!
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************
use longueur_chaine_mslib
implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_dates.f90 362 2013-02-15 18:01:28Z bbjc $'


! Valeurs associees aux indicateurs de comparaison de 2 quantites jours, sec:
! ===========================================================================

! Convention adoptee : Q1 > Q2 -> pm_joursec1_sup_joursec2
!                      Q1 = Q2 -> pm_joursec1_egal_joursec2
!                      Q1 < Q2 -> pm_joursec1_inf_joursec2

integer, parameter :: pm_joursec1_sup_joursec2  =  7
integer, parameter :: pm_joursec1_egal_joursec2 =  8
integer, parameter :: pm_joursec1_inf_joursec2  =  9

public
interface
     subroutine md_calend_julien(an, mois, jour, heure, min, sec, jul1950, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
integer, intent(in)                      :: an              ! annee
integer, intent(in)                      :: mois            ! numero du mois dans l'annee
integer, intent(in)                      :: jour            ! numero du jour dans le mois
integer, intent(in)                      :: heure           ! heure du jour
integer, intent(in)                      :: min             ! minutes dans l'heure
real(pm_reel), intent(in)                :: sec             ! secondes decimales dans la minute

type(tm_jour_sec), intent(out)           :: jul1950         ! jours juliens CNES
type(tm_code_retour), intent(out)        :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine md_calend_julien
     subroutine md_comp_joursec(joursec1, joursec2, eps, ordre, code_retour )

       use type_mslib
       use precision_mslib


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),    intent(in)        :: joursec1         ! quantite #1 (en jours, sec) 
type(tm_jour_sec),    intent(in)        :: joursec2         ! quantite #2 (en jours, sec) 
real(pm_reel),        intent(in)        :: eps              ! epsilon de comparaison (en s)
integer,              intent(out)       :: ordre            ! resultat de la comparaison : pm_joursec1_sup_joursec2 si joursec1 > joursec2
type(tm_code_retour), intent(out)       :: code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine md_comp_joursec
     subroutine md_duree_jhms( duree, jour, heure, min, sec, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                 :: duree ! duree

integer, intent(out)                      :: jour  ! jour
integer, intent(out)                      :: heure ! heure
integer, intent(out)                      :: min   ! minute
real(pm_reel), intent(out)                :: sec   ! seconde
type(tm_code_retour), intent(out)         :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine md_duree_jhms
     subroutine md_jourfrac_joursec(jourfrac, joursec, code_retour )

       use type_mslib
       use precision_mslib


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),        intent(in)        :: jourfrac        ! quantite exprimee en jours fractionnaires
type(tm_jour_sec),    intent(out)       :: joursec         ! quantite exprimee en jours et secondes dans le jour
type(tm_code_retour), intent(out)       :: code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine md_jourfrac_joursec
     subroutine md_joursec_jourfrac(joursec, jourfrac, code_retour )

       use type_mslib
       use precision_mslib


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),    intent(in)        :: joursec         ! quantite exprime en jours et secondes dans le jour
real(pm_reel),        intent(out)       :: jourfrac        ! quantite exprime en jours fractionnaires
type(tm_code_retour), intent(out)       :: code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine md_joursec_jourfrac
     subroutine md_joursec_norme(joursec, joursec_norme, code_retour )

       use type_mslib
       use precision_mslib


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)           :: joursec         ! quantite exprime en jours et secondes
type(tm_jour_sec), intent(out)          :: joursec_norme   ! quantite exprime en jours et secondes dans le jour (secondes dans [0.;86400.[)
type(tm_code_retour), intent(out)       :: code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine md_joursec_norme
     subroutine md_julien_calend(jul1950, an, mois, jour, heure, min, sec, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)           :: jul1950         ! jours juliens CNES
                                                       
integer, intent(out)                    :: an              ! annee
integer, intent(out)                    :: mois            ! numero du mois dans l'annee
integer, intent(out)                    :: jour            ! numero du jour dans le mois
integer, intent(out)                    :: heure           ! heure du jour
integer, intent(out)                    :: min             ! minutes dans l'heure
real(pm_reel), intent(out)              :: sec             ! secondes decimales dans la minute

type(tm_code_retour), intent(out)       :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine md_julien_calend
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_dates.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_dates
