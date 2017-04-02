subroutine md_julien_calend (jul1950, an, mois, jour, heure, min, sec, code_retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Determination d'une date calendaire (jour, mois, annee, heures, minutes, secondes)
! ===   a partir d'une date donnee en jours juliens CNES (entiers) et secondes dans le jour.
!
! Note d'utilisation:   Attention: cet algorithme n'est valable que pour des dates
! ==================               anterieures au 28 fevrier 2100, a cause du calcul
!                                  sur les annees bissextiles (avec un modulo). Du
!                                  fait de la marge prise, il n'accepte que des dates
!                                  anterieures au 01/01/2100 a 0 heures.
!
!$Historique
! ==========
!   + Version 1.0 (SP 200 ed01 rev00): creation a partir de la routine MUCALD de la MSLIB f77
!                         (Date: 05/1998 - Realisation: Veronique Lepine)
!   + Version 1.0.1 (FA 292 ed01 rev 00): erreur = utilisation de "jour" au lieu de "nb_jour"
!                         (Date: 01/1999 - Realisation: Guylaine Prat)
!   + Version 2.0 (FA 354 ed01 rev00): test et commentaire sur les annees > 2099 
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 2.0 (DE 398 ed01 rev00): - suppression du test sur les secondes < 86400
!                                      - appel a md_joursec_norme
!                                      - modification des commentaires sur la note d'utilisation
!                         (Date: 10/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.4 : FA-ID 527 : Calcul des secondes dans md_julien_calend
!                   (Date: 05/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use int_dates, only : md_joursec_norme

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

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

!declaration des variables locales
!---------------------------------
integer(pm_entier)   :: njul                         ! nombre de jours ecoules depuis le 01/01/1948
integer              :: na                           ! nombre d'annees ecoulees depuis le 01/01/1948
integer              :: nb                           ! nombre d'annees bisextiles ecoulees depuis le 01/01/1950
integer              :: oui_bissex                   ! indicateur d'annee bisextile (0:oui, 1 a 3:non)
integer              :: nm=0                         ! nombre de jour du mois precedent.
integer              :: num
integer              :: nb_jour, nb_mois=0           ! variables intermediaires du calcul des sorties jour et mois
type(tm_jour_sec)    :: joursec_norme                ! jour julien norme 
type(tm_code_retour) :: code_retour_local
integer              :: minute                      

!declaration des initialisations
!-------------------------------
                                                                                            ! indication du nombre de jours
integer,dimension(12), parameter:: normal = (/31,59,90,120,151,181,212,243,273,304,334,365/)! pour une annee normale
integer,dimension(12), parameter:: bissex = (/31,60,91,121,152,182,213,244,274,305,335,366/)! pour une annee bissextile

                                                                ! numeros de jour(fin mars, fin juin, fin septembre, fin decembre)
integer,parameter :: nn3=90, nn6=181, nn9=273, nn12=normal(12)  ! pour une annee non bissextile
integer,parameter :: nb3=91, nb6=182, nb9=274, nb12=bissex(12)  ! pour une annee bissextile    

intrinsic mod, int

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB md_julien_calend.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: md_julien_calend.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour 
! ..........................................
code_retour%valeur = pm_OK

! normalisation du jour julien en entree
! ======================================
call md_joursec_norme ( jul1950, joursec_norme, code_retour_local ) ! seul code retour possible = 0, non teste

! test valeur du jour julien
! ==========================
if (joursec_norme%jour < 0) then
   code_retour%valeur = pm_err_jul1950_negatif
   go to 6000
end if

if (joursec_norme%jour > 54787) then              ! 54787 = 01/01/2100 a 00:00:00
   code_retour%valeur = pm_err_jul1950_sup2099
   go to 6000
end if

! calcul du nombre d'annees bissextiles depuis le 01/01/1950
! =========================================================
njul = joursec_norme%jour + 731
na = njul / 365
nb = (na - 1) / 4

! calcul de la date calendaire
! ============================
! - annee
! =======
nb_jour = njul - 365 * na - nb
if (nb_jour>0) then
   an = 1948 + na
else
   an = 1948 + na - 1
end if

oui_bissex = mod (an,4)    ! calcul valable pour des annees < ou = a 2099
if (nb_jour<=0) then
   if (oui_bissex==0) then
      nb_jour = 366 + nb_jour
   else
      nb_jour = 365 + nb_jour
   end if
end if

! - mois et jour
! ==============

select case (oui_bissex)        ! traitement adapte selon que l'annee est ou non bissextile

!=========
case (1:3)                      ! annee non bissextile
!=========

   select case(nb_jour)            ! recherche du trimestre

   case (1:nn3)       ! du 1er janvier au 31 mars

      num = 0
      nb_mois = 0

100   continue

      nm = num
      nb_mois = nb_mois + 1
      num = normal (nb_mois)
      if (nb_jour>num) go to 100

   case (nn3+1:nn6)    ! du 1er avril au 30 juin

      num = nn3
      nb_mois = 3

101   continue
      nm = num
      nb_mois = nb_mois + 1
      num = normal (nb_mois)
      if (nb_jour>num) go to 101

   case (nn6+1:nn9)   ! du 1er juillet au 30 septembre

      num = nn6
      nb_mois = 6
200   continue
      nm = num
      nb_mois = nb_mois + 1
      num = normal (nb_mois)
      if (nb_jour>num) go to 200

   case (nn9+1:nn12)   ! au dela du 30 septembre

      num = nn9
      nb_mois = 9
201   continue
      nm = num
      nb_mois = nb_mois + 1
      num = normal (nb_mois)
      if (nb_jour>num) go to 201
 
   end select         ! fin annee non bissextile

!======
case(0)! annee  bissextile
!======

   select case (nb_jour)      
   case (1:nb3) !  du 1er janvier au 31 mars
      num = 0
      nb_mois = 0

300   continue

      nm = num
      nb_mois = nb_mois + 1
      num = bissex (nb_mois)
      if (nb_jour>num) go to 300

   case (nb3+1:nb6)        ! du 1er avril au 30 juin

      num = nb3
      nb_mois = 3

301   continue
      nm = num
      nb_mois = nb_mois + 1
      num = bissex (nb_mois)
      if (nb_jour>num) go to 301

   case (nb6+1:nb9)       ! du 1er juillet au 30 septembre

      num = nb6
      nb_mois = 6
400   continue
      nm = num
      nb_mois = nb_mois + 1
      num = bissex (nb_mois)
      if (nb_jour>num) go to 400

   case (nb9+1:nb12)   ! au dela du 30 septembre

      num = nb9
      nb_mois = 9
401   continue
      nm = num
      nb_mois = nb_mois + 1
      num = bissex (nb_mois)
      if (nb_jour>num) go to 401

   end select

end select                      ! fin annee bissextile

mois = nb_mois
jour = nb_jour - nm

!heure_decimale=joursec_norme%sec/3600._pm_reel
!heure=int(heure_decimale)
!min_decimale=(heure_decimale - real(heure, kind=pm_reel))*60._pm_reel
!min=int(min_decimale)
!sec=(min_decimale - real(min, kind=pm_reel))*60._pm_reel

! replacement de l'algorithme par celui de ZOOM
heure = int(joursec_norme%sec/3600._pm_reel)
minute = int(joursec_norme%sec/60._pm_reel)
min = mod(minute,int(60._pm_reel))
sec = mod(joursec_norme%sec,60._pm_reel)

6000 continue

code_retour%routine = pm_num_md_julien_calend
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine md_julien_calend
