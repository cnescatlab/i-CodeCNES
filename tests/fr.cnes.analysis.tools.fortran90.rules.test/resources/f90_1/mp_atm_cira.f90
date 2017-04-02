subroutine mp_atm_cira (mois, pos_geod, tempe, pres, dens, code_retour)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But:  Modele d'atmosphere CIRA.
! ===
!
! Note d'utilisation:  Ce modele d'atmosphere est limite a des:
! ==================             * 0 < altitudes < 120 km
!                                * -80 < latitudes < +80 degres
!          Forcage des valeurs de la facon suivante:
!                    * si altitude < 0          ==> altitude = 0
!                    * si latitude > +80 degres ==> latitude = 80 deg
!                    * si latitude < -80 degres ==> latitude = -80 deg
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0 : creation a partir de la routine MCCIRA de la MSLIB f77
!                         (Date: 12/2000 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE globale 1) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 07/2001 - Realisation: Guylaine Prat)
!   + Version 3.1 (FA 1) : passage rad->deg de la latitude (cas hors bornes)
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!************************************************************************

  ! Modules
  ! =======
  use mslib

use int_geo_internes, only : mpi_atmi
use int_geo_internes, only : mpi_atmo

  use parametre_mspro

  use valeur_code_retour_mspro
  use numero_routine_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                            :: mois     ! mois de l'annee
type(tm_geodesique), intent(in)                :: pos_geod ! coordonnees geodesiques
real(pm_reel), intent(out)                     :: tempe    ! temperature
real(pm_reel), intent(out)                     :: pres     ! pression
real(pm_reel), intent(out)                     :: dens     ! densite
type(tm_code_retour), intent(out)              ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! ===================

  real(pm_reel) :: rlong1, rlat1, ralti1 ! variables intermediaires de calcul pour la longitude
                                         ! la latitude et l'altitude

  real(pm_reel), dimension(pm_nb_alt) :: rtarra, rzarra, rpress! sorties de mpi_atmi
  real(pm_reel)                       :: rpres, rtemp, rdens

  integer       :: retour                ! code retour des routines internes

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mp_atm_cira.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisations
  ! ===============

  code_retour%valeur = pm_OK

  ! verification des arguments d'entree
  ! ===================================

  if (mois < pm_janvier .or. mois > pm_decembre) then ! probleme sur le mois

     code_retour%valeur = pm_err_mois
     go to 6000

  end if

  if (pos_geod%haut > pm_alt_max) then ! altitude superieure a 120 km

     code_retour%valeur = pm_err_alt_sup120km
     go to 6000

  end if

  ! transformation des (rad, m) en (degres, km)
  ! avec forcage de valeurs si hors des bornes
  ! ===========================================

  rlong1 = pos_geod%long * pm_rad_deg

  if ( abs(pos_geod%lat) > pm_lat_borne) then  ! latitude non comprise dans  ]-80, +80[

     rlat1 = sign(1._pm_reel,pos_geod%lat)*pm_lat_borne ! on force la valeur de la latitude
                                                        !  a une valeur proche de +/- 80 
                                                        ! (suivant le signe de la latitude)
     code_retour%valeur = pm_warn_lat_cira

  else ! latitude dans ]-80, +80[

     rlat1  = pos_geod%lat

  end if

  rlat1 = rlat1 * pm_rad_deg

  if (pos_geod%haut < 0._pm_reel) then ! altitude negative

     ralti1 = 0._pm_reel  ! on force a zero l'altitude
     code_retour%valeur = pm_warn_alt_negatif
 
  else  ! altitude positive

     ralti1 = pos_geod%haut / pm_trans_m_km

  end if

  ! interpolation sur le fichier des donnees
  ! ========================================
  
  call mpi_atmi (rlat1,rlong1,mois,rtarra,rzarra,rpress,retour)

  if (retour /= pm_OK) then

     code_retour%valeur = retour
     if (retour < pm_OK) go to 6000

  end if 

  ! calcul du modele atmospherique cira
  ! ===================================
  
  call mpi_atmo (ralti1,rtarra,rzarra,rpress,rtemp,rpres,rdens,retour)

  if (retour /= pm_OK) then

     code_retour%valeur = retour
     if (retour < pm_OK) go to 6000

  end if 

  ! affectation des sorties
  ! =======================
  tempe = rtemp
  pres  = rpres * 100._pm_reel  ! calculs faits en mbar ==> conversion en Pascal
  dens  = rdens

6000 continue

  code_retour%routine = pm_num_mp_atm_cira
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mp_atm_cira
