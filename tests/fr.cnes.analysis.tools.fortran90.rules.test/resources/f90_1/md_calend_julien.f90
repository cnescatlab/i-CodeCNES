subroutine md_calend_julien (an, mois, jour, heure, min, sec, jul1950, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Determination d'une date en jours juliens CNES (entiers) et secondes dans le jour 
! ===   a partir d'une date calendaire donnee (jour, mois, annee, heures, minutes, secondes).
!
!       
!        
!
! Note d'utilisation:  Les arguments d'entree doivent appartenir aux domaines de definition suivants:
! ==================   1950 <= an <= 2099
!                      1 <= mois <= 12
!                      1 <= jour <= 28, 29, 30 ou 31 suivant le mois de l'annee
!                      0 <= heure <= 23
!                      0 <= min <= 59
!                      0.<= sec < 60.
!                      Attention: cet algorithme n'est valable que pour des dates 
!                                 anterieures au 28 fevrier 2100, a cause du calcul
!                                 sur les annees bissextiles (avec un modulo). Du
!                                 fait de la marge prise, il n'accepte que des dates
!                                 anterieures au 01/01/2100 a 0 heures.
!
!$Historique
! ==========
!   + Version 1.0 (SP 202 ed01 rev00): creation a partir de la routine MUJJUL de la MSLIB f77 et 
!                                      evolution par la prise en compte dans la date calendaire
!                                      des heures minutes et secondes dans le jour
!                         (Date: 05/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 353 ed01 rev00): test et commentaire sur les annees > 2099 
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

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

! Autres declarations
! -------------------
logical ::  bissextil        ! indicateur d'annee bissextile

integer ::  nj2,iy,ia        ! variables intermediaire servant au calcul du nombre d'annees, et du nombre de jours dans le mois
integer(pm_entier) :: jours_juliens ! variable intermediaire pour le calcul du nombre de jour selon que l'annee courante est 
                                     !ou non bissextile
integer, dimension(12), parameter :: numois = (/-1,30,58,89,119,150,180,211,242,272,303,333/)
integer, dimension(12), parameter :: nbjour = (/31,28,31,30,31,30,31,31,30,31,30,31/)
 
intrinsic mod, real

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB md_calend_julien.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: md_calend_julien.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

 
! verification de la validite de la date d'entree test sur l'annee
! ================================================================
if (an < 1950) then
   code_retour%valeur = pm_err_an_inf1950
   go to 6000
end if

if (an > 2099) then
   code_retour%valeur = pm_err_an_sup2099      ! annee superieure a l'an 2099
   go to 6000
end if

! test sur le mois
! ================

if (mois < 1.or.mois > 12) then                ! numero de mois errone
   code_retour%valeur  = pm_err_mois_interval
   go to 6000
end if

    
! verification validite jour (avec mois et an)
! ===========================================    

if (jour < 1) then                             ! jour dans le mois negatif
   code_retour%valeur = pm_err_jour_interval
   go to 6000
end if

bissextil = (mod(an,4) == 0)                   ! calcul valable pour des annees < ou = a 2099

if (mois /= 2) then                            ! numero de jour > nombre de jour dans le mois considere
   if (jour > nbjour(mois)) then
      code_retour%valeur = pm_err_jour_interval
      go to 6000
   end if
else
   if (bissextil) then
      nj2=29
   else
      nj2=28
   end if
   if (jour > nj2) then                        ! verification pour le mois de fevrier 
      code_retour%valeur =pm_err_jour_interval
      go to 6000
   end if
end if
    
! verification validite heures, minutes et secondes 
! ================================================    

if (heure < 0 .or. heure >= 24) then           ! heure erronee
   code_retour%valeur = pm_err_heure_interval
   go to 6000
end if

if ( min < 0.or. min >= 60) then               ! minutes erronees
   code_retour%valeur = pm_err_min_interval
   go to 6000
end if

if (sec < 0._pm_reel .or. sec >= 60._pm_reel)    then  ! secondes erronees
   code_retour%valeur = pm_err_sec_interval_min
   go to 6000
end if

! si les donnees d'entree sont valides, calcul du nombre de jours juliens
!========================================================================

iy = an - 1950

! Calcul de la contribution (en nombre entier de jours) des annees bissextiles -> division entiere par 4
ia = (iy+1)/4
jours_juliens = 365*iy + ia + numois(mois) + jour
jul1950%jour  = jours_juliens

if(bissextil.and.mois > 2) jul1950%jour = jours_juliens + 1 !ajout eventuel de la journee bissextile de 
!                                                           !l'annee courante

! calcul des secondes dans le jour
! ================================
jul1950%sec= real(heure, kind=pm_reel) *3600._pm_reel + real(min, kind=pm_reel) *60._pm_reel + sec

6000  continue

code_retour%routine = pm_num_md_calend_julien
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine md_calend_julien
