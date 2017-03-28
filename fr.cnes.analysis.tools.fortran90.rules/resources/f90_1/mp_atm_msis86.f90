subroutine mp_atm_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
                          dens,code_retour,temp,pres,inv_haut_ech)
! (C) Copyright CNES - MSLIB - 2001-2003

!************************************************************************
!
! But:  Modele d'atmosphere MSIS86.
! ===
!
! Note d'utilisation:  Ce modele d'atmosphere est limite a des:
! ==================             * 90km  < altitudes 
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 08/2001 - Realisation: Guylaine Prat)
!   + Version 3.0 (FA 1 ed01 rev00) : renommage de commons au niveau des sources 77
!                         (Date: 02/2003 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
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

use parametre_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
real(pm_reel), intent(in)               :: flux_3rot    ! flux solaire moyen sur les 3 
real(pm_reel), dimension(7), intent(in) :: tab_ap       ! evolution de l'activite 
real(pm_reel), intent(in)               :: lat          ! latitude geodesique
real(pm_reel), intent(in)               :: long         ! longitude geodesique
real(pm_reel), intent(in)               :: alt          ! altitude geodesique
real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale
real(pm_reel), intent(out)              :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), intent(out), optional    :: temp         ! temperature
real(pm_reel), intent(out), optional    :: pres         ! pression
real(pm_reel), intent(out), optional    :: inv_haut_ech ! inverse de la hauteur d'echelle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

! Declarations pour l'appel a la routine IOLIB fortran 77 mpi_IO_e_msis86
integer                     :: JJ                ! date en JJ CNES
real(pm_reel)               :: SEC                  ! secondes dans le jour
integer                     :: retour            ! code retour
real(pm_reel)               :: F,FBAR            ! flux solaire du jour prec, et moyen sur trois mois
real(pm_reel)               :: ALTM,ALAT,ALONG   ! altitude geodesique, lat geodesique, longitude
real(pm_reel)               :: STL               ! heure solaire locale
real(pm_reel)               :: RHO               ! masse volumique a l'endroit considere
real(pm_reel)               :: RTEMP,RPRES,DM    ! temperature, pression, masse molaire moyenne
real(pm_reel)               :: ECH               ! hauteur d'echelle
real(pm_reel), dimension(7) :: AP                ! tableau de l'activite geomagnetique

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
         '@(#) Fichier MSPRO mp_atm_msis86.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

code_retour%valeur = pm_OK

! verification des arguments d'entree
! ===================================

if ((flux_veille < 0._pm_reel) .OR. (flux_3rot < 0._pm_reel)) then ! flux solaire negatif

   code_retour%valeur = pm_err_flux_sol_negatif
   go to 6000

end if

if (alt < pm_alt_min_MSIS86) then ! altitude inferieure a 90 km

   code_retour%valeur = pm_err_alt_inf90km
   go to 6000

end if

! pas de normalisation de date faites ici 
! compte tenu de l'utilisation jusqu'a present de mpi_IO_e_msis86
if (date%jour < 0) then

   code_retour%valeur = pm_err_jul1950_negatif
   go to 6000

end if

if (date%jour > 54787) then ! 54787 = 01/01/2100 a 00:00:00

   code_retour%valeur = pm_err_jul1950_sup2099
   go to 6000

end if

! calcul du modele atmospherique MSIS 86
! ======================================

! pour les unites des donnees en entre et en sortie: 
! utilisation des commentaires en debut de code de mpi_IO_e_msis86

JJ = date%jour
SEC = date%sec
F = flux_veille
FBAR = flux_3rot
AP (1:7) = tab_ap(1:7)
ALTM = alt
ALAT = lat
ALONG = long
STL = heure_sol

call mpi_IO_e_msis86 (JJ,SEC,F,FBAR,AP,ALTM,ALAT,ALONG,STL,RHO,RTEMP,RPRES,DM,ECH,retour)

if (retour /= pm_OK) then

! les valeurs non nulles possibles pour le code retour de mpi_IO_e_msis86 sont 
! normalement deja traitees avant l'appel

   if (retour > pm_OK) then ! warning
      code_retour%valeur = pm_warn_IOLIB
   else ! retour < pm_OK : erreur
      code_retour%valeur = pm_err_IOLIB
      go to 6000 ! pas d'affectation des sorties
   end if

end if

! affectation des sorties
! =======================

dens = RHO

if (present(temp)) temp = RTEMP
if (present(pres)) pres = RPRES
if (present(inv_haut_ech)) inv_haut_ech = ECH

6000 continue

code_retour%routine = pm_num_mp_atm_msis86
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mp_atm_msis86
