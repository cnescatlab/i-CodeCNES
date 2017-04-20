subroutine md_calend_anjour (an, mois,jour_mois,jour_an, code_retour)

! (C) Copyright CNES - MSPRO - 2001-2004

!************************************************************************
!
! But: Conversion d'une date calendaire en une date exprimee sous la 
! ===  forme annee et numero du jour dans l'annee
!
!       
!   
!
! Note d'utilisation:  Les arguments d'entree doivent appartenir aux domaines de definition suivants:
! ==================   1950 <= an <= 2099
!                      1 <= mois <= 12
!                      1 <= jour <= 28, 29, 30 ou 31 suivant le mois de l'annee
!                     
!                      Attention: cet algorithme n'est valable que pour des dates 
!                                 anterieures au 28 fevrier 2100, a cause du calcul
!                                 sur les annees bissextiles (avec un modulo). Du
!                                 fait de la marge prise, il n'accepte que des dates
!                                 anterieures au 01/01/2100 a 0 heures.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 09/2001 - Realisation: Mickael Hazak)
!   + Version 5.2 (FA 1) : correction du nombre de jours en fevrier pour une annee non bissextile
!                         (Date: 11/2004 - Realisation: Veronique Lepine)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
integer, intent(in)                      :: an             ! annee
integer, intent(in)                      :: mois           ! numero du mois dans l'annee
integer, intent(in)                      :: jour_mois      ! numero du jour dans le mois

integer, intent(out)                     :: jour_an        ! numero du jour dans l'annee
type(tm_code_retour), intent(out)        :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
logical ::  bissextil        ! indicateur d'annee bissextile

integer ::  nj2, jour_anbis        ! variables intermediaire servant au calcul du nombre d'annees, et du nombre de jours dans le mois

! cumul des jours par mois (cas NON bissextile)
integer, dimension(12), parameter :: cumul = (/0,31,59,90,120,151,181,212,243,273,304,334/)
! nb de jour par mois pour annee NON bissextile
integer, dimension(12), parameter :: nbjour = (/31,28,31,30,31,30,31,31,30,31,30,31/) 
 
intrinsic mod
character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
         '@(#) Fichier MSPRO md_calend_anjour.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

 
! verification de la validite de la date d'entree 
! ===============================================

! test sur l'annee
if (an < 1950) then
   code_retour%valeur = pm_err_an_inf1950
   go to 6000
end if

if (an > 2099) then
   code_retour%valeur = pm_err_an_sup2099      ! annee superieure a l'an 2099
   go to 6000
end if

! test sur le mois
if ((mois < 1).or.(mois > 12)) then            ! numero de mois errone
   code_retour%valeur  = pm_err_mois_interval
   go to 6000
end if

    
! verification validite jour
if (jour_mois < 1) then                        ! jour dans le mois negatif
   code_retour%valeur = pm_err_jour_interval
   go to 6000
end if

bissextil = (mod(an,4) == 0)   ! calcul valable pour des annees < ou = a 2099

if (mois /= 2) then            ! numero de jour > nombre de jour dans le mois considere
   if (jour_mois > nbjour(mois)) then
      code_retour%valeur = pm_err_jour_interval
      go to 6000
   end if
else ! fevrier
   if (bissextil) then
      nj2=nbjour(mois) + 1 ! soit 29
   else
      nj2=nbjour(mois) ! soit 28
   end if
   if (jour_mois > nj2) then                        ! verification pour le mois de fevrier 
      code_retour%valeur = pm_err_jour_interval
      go to 6000
   end if

end if

    
! calcul du jour dans l'annee

jour_anbis = cumul(mois)+jour_mois

! recalage si annee bissextile
if ((bissextil).and.(mois > 2))  jour_anbis = jour_anbis + 1

jour_an = jour_anbis

6000 continue

code_retour%routine = pm_num_md_calend_anjour
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' ' 
end subroutine md_calend_anjour
