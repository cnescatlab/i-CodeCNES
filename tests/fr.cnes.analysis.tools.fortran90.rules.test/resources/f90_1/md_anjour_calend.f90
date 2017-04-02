subroutine md_anjour_calend (an, jour_an, mois, jour_mois, code_retour)

! (C) Copyright CNES - MSPRO - 2001

!************************************************************************
!
! But: Conversion d'une date exprimee sous la forme annee et numero du  
! ===  jour dans l'annee en une date calendaire.   
!
!       
!   
! Note d'utilisation:  Les arguments d'entree doivent appartenir aux domaines de definition suivants:
! ==================   1950 <= an <= 2099
!                      1 <= jour_an <= 365, 366 suivant l'annee
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
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
integer, intent(in)                      :: an              ! annee
integer, intent(in)                      :: jour_an         ! numero du jour dans l'annee
integer, intent(out)                     :: mois            ! numero du mois dans l'annee
integer, intent(out)                     :: jour_mois       ! numero du jour dans le mois
type(tm_code_retour), intent(out)        :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
logical ::  bissextil        ! indicateur d'annee bissextile

integer :: cumul ! cumul de jour selon le type d'annee

integer, dimension(13), parameter :: normal = (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
integer, dimension(13), parameter :: bissex = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
integer :: n,moisbis
intrinsic mod

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO md_anjour_calend.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
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

bissextil = (mod(an,4) == 0) ! determination si annee bissextile

! test sur le jour dans l'annee

if (bissextil) then
 n = 366
else
 n= 365
end if

if ((jour_an < 1).or.(jour_an > n)) then     ! jour dans l'annee  errone
   code_retour%valeur  = pm_err_jour_interval_an
   go to 6000
end if

! calculs du mois et du jour dans le mois
! =======================================

! calcul du mois dans l'annee

moisbis = 1

if (bissextil) then
  do while (jour_an > bissex (moisbis))
     moisbis = moisbis + 1
  end do
  moisbis = moisbis - 1
  cumul = bissex (moisbis)
else
  do while (jour_an > normal(moisbis))
     moisbis = moisbis + 1
  end do
  moisbis = moisbis - 1
  cumul = normal(moisbis)
end if

mois = moisbis

! calcul du jour dans le mois
jour_mois = jour_an - cumul

6000 continue

code_retour%routine = pm_num_md_anjour_calend
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' ' 

end subroutine md_anjour_calend
