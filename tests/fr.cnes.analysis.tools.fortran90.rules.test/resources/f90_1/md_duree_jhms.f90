subroutine md_duree_jhms (duree, jour, heure, min, sec, code_retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Conversion d'une DUREE exprimee en secondes en Jours, Heures, Minutes, Secondes
! ===
!
! Note d'utilisation:  La duree en entree doit etre positif
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 201 ed01 rev00): creation a partir de la routine MUCDAT de la MSLIB f77
!                         (Date: 05/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 335 ed01 rev00): ajout de test sur les heures et les minutes (cf. M-NT-0-92-CIS)
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
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
                                                       
real(pm_reel), intent(in)                 :: duree ! duree

integer, intent(out)                      :: jour  ! jour
integer, intent(out)                      :: heure ! heure
integer, intent(out)                      :: min   ! minute
real(pm_reel), intent(out)                :: sec   ! seconde
type(tm_code_retour), intent(out)         :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel)             :: eps100  ! valeur de test pour les reels
real(pm_reel)             :: reste   ! variable intermediaire de calcul
real(pm_reel)             :: nb_sec  ! variable intermediaire de calcul des secondes
real(pm_reel),parameter   :: r86 = 86400._pm_reel, r36 = 3600._pm_reel, r60 =60._pm_reel
integer,parameter         :: i60 = 60, i24 = 24
integer                   :: nb_jour, nb_min, nb_heure ! variables intermediaires de calcul des jours, heures et minutes

intrinsic epsilon, abs, real, int

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB md_duree_jhms.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: md_duree_jhms.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
!.................................
!
eps100= 100._pm_reel * epsilon(1._pm_reel)
!
! validation de la duree en entree
! =============================
!
if (duree < 0._pm_reel) then
   code_retour%valeur = pm_err_duree_negatif
   go to 6000
end if
!
! transformation de la duree en entre
! --------------------------------
!

nb_jour  = int (duree / r86) !Calcul du nombre de jours

!Calcul du nombre d'heures
reste  = duree - real(nb_jour, kind=pm_reel) * r86
nb_heure = int (reste / r36)

if (nb_heure==i24) then   ! La duree est un nombre entier de jours (ni heures ni minutes ni secondes)
   jour  = nb_jour + 1
   heure = 0
   min   = 0
   sec   = 0._pm_reel
   go to 6000
end if

! Calcul du nombres de minutes
reste  = reste - real(nb_heure, kind=pm_reel) * r36
nb_min   = int (reste / r60)

if (nb_min==i60) then    ! Le nombre d'heures est entier (pas de minutes ni de secondes)
   jour  = nb_jour
   heure = nb_heure + 1
   if (heure == i24) then           ! traite le cas ou le nombre d'heure serait passe a 24
      jour  = nb_jour + 1
      heure = 0
   end if
   min = 0
   sec = 0._pm_reel
   go to 6000
end if

!Calcul du nombre de secondes
nb_sec  = reste - real(nb_min, kind=pm_reel) * r60

if ( abs(nb_sec- 60._pm_reel)< eps100 ) then ! Le nombre de minutes est entier (pas de secondes)
   jour  = nb_jour
   heure = nb_heure
   min   = nb_min + 1
   if (min == i60) then             ! traite le cas ou le nombre de minutes serait passe a 60
      heure = nb_heure + 1
      if (heure == i24) then        ! traite le cas ou le nombre d'heure serait passe a 24
         jour  = nb_jour + 1
         heure = 0
      end if
      min = 0
   end if
   sec   = 0._pm_reel
   go to 6000
end if

! Affectation des parametres de sortie dans tous les autres cas
jour  = nb_jour
heure = nb_heure
min   = nb_min
sec   = nb_sec

6000 continue

code_retour%routine = pm_num_md_duree_jhms
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine md_duree_jhms
