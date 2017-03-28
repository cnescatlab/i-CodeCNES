subroutine mt_geod_car ( pos_geod, r_equa, apla, pos_car, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des coordonnees GEODesiques aux coordonnees CARtesiennes
! ===
!
! Note d'utilisation: 1) La transformation inverse peut s'effectuer par mt_car_geod.
! ==================  2) L'aplatissement f doit appartenir a [0.,1.[.
!                        Aucun test n'est effectue sur f < 0.
!                     3) Le rayon equatorial doit etre > 0. : aucun test n'est effectue ici.
!$Historique
! ==========
!   + Version 1.0 (SP 248 ed01 rev00): creation a partir de la routine MVELCA de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE 486 ed01 rev00) : calcul de la jacobienne
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : (Date : 02/05/2007 - Realisation: Sandrine Avril - Atos origin)
!                   DM-ID 636 : contrôle de la latitude pour la fonction mt_geoc_car
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

type(tm_geodesique), intent(in)                      ::  pos_geod     ! latitude, longitude,  hauteur geodesiques                                         
real(pm_reel), intent(in)                            ::  r_equa       ! rayon equatorial terrestre
real(pm_reel), intent(in)                            ::  apla         ! aplatissement terrestre
real(pm_reel), dimension(3), intent(out)             ::  pos_car      ! position en coordonnees cartesiennes
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(3,3), intent(out), optional ::  jacob        ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)  :: latitude,longitude     ! latitude et longitude exprimes en radians
real(pm_reel)  :: ex2,correc,Rphi        ! ex2 = terme permettant la correction du rayon en fonction de la latitude          
                                         ! correc = correction de rayon
                                         ! Rphi  = rayon terrestre a la latitude donnee

real(pm_reel):: K, KRphiplusH, RphiplusH ! variables de calcul pour la jacobienne

intrinsic sin, cos, sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mt_geod_car.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_geod_car.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! verification des donnees d'entree
!..................................
if (apla >= 1._pm_reel) then
   code_retour%valeur = pm_err_apla_sup1
   go to 6000
end if

if (r_equa <= 1._pm_reel) then ! rayon équatorial inférieur ou egal à 1
   code_retour%valeur = pm_err_r_equa_inf_egal1
   go to 6000
end if

latitude = pos_geod%lat
longitude = pos_geod%long

ex2 = apla * (2._pm_reel - apla)           

correc = 1._pm_reel - ex2 * (sin(latitude)**2) ! correc est > 0.
                                               ! correc peut etre nul si apla=1: impossible a ce stade des calculs
Rphi = r_equa / sqrt (correc)                  ! rayon a la latitude donnee

RphiplusH = Rphi + pos_geod%haut

! calcul des coordonnees x,y,z
! ============================
pos_car(1) = RphiplusH * cos (latitude) * cos (longitude)
pos_car(2) = RphiplusH * cos (latitude) * sin (longitude)
pos_car(3) = (Rphi * (1._pm_reel - ex2) + pos_geod%haut) * sin (latitude)

if (present(jacob)) then

   K = (1._pm_reel - ex2)/ correc  ! K dans [0,1]
  ! si apla = 0 : exc2 = 0 ; correc = 1 ; Rphi = r_equa; K = 1

   KRphiplusH = K*Rphi + pos_geod%haut

   jacob(1,1) = - KRphiplusH * sin(latitude)*cos(longitude)
   jacob(1,2) = - RphiplusH * cos(latitude)*sin(longitude)
   jacob(1,3) =   cos(latitude)*cos(longitude)

   jacob(2,1) = - KRphiplusH * sin(latitude)*sin(longitude)
   jacob(2,2) =   RphiplusH * cos(latitude)*cos(longitude)
   jacob(2,3) =   cos(latitude)*sin(longitude)

   jacob(3,1) =   KRphiplusH * cos(latitude)
   jacob(3,2) =   0._pm_reel
   jacob(3,3) =   sin(latitude)

end if

6000 continue

code_retour%routine = pm_num_mt_geod_car
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_geod_car
