module cps_atm_cira_msis86_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_cira_msis86_mod
!
!$Resume
!  Modele mixte d'atmosphere CIRA-MSIS86 
!
!$Description
!  Modele mixte d'atmosphere CIRA-MSIS86 
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Version
!  $Id: cps_atm_cira_msis86.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_atm_cira_msis86.F90,v $
!  Revision 1.3  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.2  2006/05/12 12:05:55  bouillaj
!  Amelioration qualite : complements sur les cartouches
!
!  Revision 1.1  2006/01/30 09:06:51  bouillaj
!  Creation a partir de la routine MSPRO mp_atm_cira_msis86
!
!
!$FinHistorique
!
!$Usage
!  use cps_atm_cira_msis86_mod
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_atm_cira_msis86
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- mslib
!- cps_atm_cira_mod
!- cps_atm_msis86_mod
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cps_atm_cira_msis86
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mslib
  
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_cira_msis86.F90 69 2012-09-11 08:33:34Z ffsm $'

  
contains
  
  subroutine cps_atm_cira_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
       dens,code_retour,temp,pres)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atm_cira_msis86
!
!$Resume
!   Pour des altitudes <= 110 km : utilisation de CIRA 
!                            >=120 km utilisation de MSIS86
!                    entre 110 km et 120 km utilisation du modele mixte CIRA-MSIS86
!
!$Description
!   Pour des altitudes <= 110 km : utilisation de CIRA 
!                            >=120 km utilisation de MSIS86
!                    entre 110 km et 120 km utilisation du modele mixte CIRA-MSIS86
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atm_cira_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
!.           dens,code_retour,[temp],[pres])
!.    type(tm_jour_sec) :: date 
!.    real(pm_reel) :: flux_veille 
!.    real(pm_reel) :: flux_3rot 
!.    real(pm_reel), dimension(7) :: tab_ap 
!.    real(pm_reel) :: lat 
!.    real(pm_reel) :: long 
!.    real(pm_reel) :: alt 
!.    real(pm_reel) :: heure_sol 
!.    real(pm_reel) :: dens 
!.    type(tm_code_retour) :: code_retour
!.    real(pm_reel) :: temp 
!.    real(pm_reel) :: pres 
!
!$Arguments
!>E     date         :<tm_jour_sec>       date julienne 1950 (JJ)
!>E     flux_veille  :<pm_reel>           flux solaire du jour précédent
!>E     flux_3rot    :<pm_reel>           flux solaire moyen sur les 3 dernières rotations solaires
!>E     tab_ap       :<pm_reel,DIM=(7)>   évolution de l'activité 
!                                         géomagnétique (60 h précédentes)
!>E     lat          :<pm_reel>           latitude géodésique (rad)
!>E     long         :<pm_reel>           longitude géodésique (rad)
!>E     alt          :<pm_reel>           altitude géodésique (m)
!>E     heure_sol    :<pm_reel>           heure solaire locale (rad)
!>S     dens         :<pm_reel>           densité atmosphérique (kg.m-3)
!>S     code_retour  :<tm_code_retour>    
!>[S]   temp         :<pm_reel>           température (Kelvin)
!>[S]   pres         :<pm_reel>           pression (Pa)
!
!$Common
!
!$Routines
!- md_julien_calend
!- cps_atm_cira
!- cps_atm_msis86
!
!$Include
!
!$Module
!#V
!- mslib
!- cps_atm_cira_mod
!- cps_atm_msis86_mod
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Modules
! =======
use mslib

use cps_atm_cira_mod
use cps_atm_msis86_mod

! Declarations
! ============
implicit none

type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
real(pm_reel), intent(in)               :: flux_3rot    ! flux solaire moyen sur les 3 
                                                        ! dernieres rotations solaires
real(pm_reel), dimension(7), intent(in) :: tab_ap       ! evolution de l'activite 
                                                        ! geomagnetique (60 h precedentes)
real(pm_reel), intent(in)               :: lat          ! latitude geodesique
real(pm_reel), intent(in)               :: long         ! longitude geodesique
real(pm_reel), intent(in)               :: alt          ! altitude geodesique
real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale
real(pm_reel), intent(out)              :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), intent(out), optional    :: temp         ! temperature
real(pm_reel), intent(out), optional    :: pres         ! pression

! Autres declarations
! ===================

real(pm_reel), parameter :: cste_mixte = pm_pi*1.e-4_pm_reel ! constante pour le modele mixte


! Declarations pour l'appel des module COMPAS cps_atm_cira et cps_atm_msis86
integer                           :: mois,an,heure,jour,minute          ! mois de l'annee (autres donnees inutiles)
type(tm_geodesique)               :: pos_geod                           ! coordonnees geodesiques
real(pm_reel)                     :: temp_cira,dens_cira,dens_msis86    ! temperature,densite du modele cira,densite du modele msis86
real(pm_reel)                     :: pres_cira, pres_msis86             ! pression du modele cira,pression du modele msis86
real(pm_reel)                     :: temp_msis86
real(pm_reel)                     :: az,temp1,pres1,sec
type(tm_code_retour)              :: code_local


intrinsic present


!************************************************************************

! initialisations
! ===============

code_retour%valeur = pm_OK

! verification des arguments d'entree
! ===================================

call md_julien_calend (date,an,mois,jour,heure,minute,sec,code_local)

if (code_local%valeur<pm_OK) then           ! pas de warning a tester
   code_retour%valeur = code_local%valeur 
   go to 6000 ! pas d'affectation des sorties 
end if


pos_geod%lat = lat
pos_geod%long = long
pos_geod%haut = alt

! calcul du modele atmospherique cira_msis86
! ==========================================

! pour les unites des donnees en entre et en sortie: 
! utilisation des commentaires en debut de code de mpi_IO_e_ciramsis86

if (alt <= 110.e3_pm_reel) then
   call cps_atm_cira (mois, pos_geod, temp_cira, pres_cira, dens_cira, code_local)
   if (code_local%valeur/=pm_OK) then 
      code_retour%valeur = code_local%valeur
      if (code_local%valeur<pm_OK) go to 6000 ! pas d'affectation des sorties 
   end if
   dens = dens_cira
   temp1 = temp_cira 
   pres1 = pres_cira  
else 
   if (alt >= 120.e3_pm_reel) then
      call  cps_atm_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
           dens_msis86,code_local,temp=temp_msis86,pres=pres_msis86)
      if (code_local%valeur/=pm_OK) then 
         code_retour%valeur = code_local%valeur 
         if (code_local%valeur<pm_OK) go to 6000 ! pas d'affectation des sorties 
      end if
      dens = dens_msis86
      temp1 = temp_msis86
      pres1 = pres_msis86 

   else ! modele mixte entre 110 et 120 km

      call cps_atm_cira (mois, pos_geod, temp_cira, pres_cira, dens_cira, code_local)
      if (code_local%valeur/=pm_OK) then 
         code_retour%valeur = code_local%valeur 
         if (code_local%valeur<pm_OK) go to 6000 ! pas d'affectation des sorties 
      end if
      
      call  cps_atm_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
           dens_msis86, code_local, temp=temp_msis86, pres=pres_msis86)
      if (code_local%valeur/=pm_OK) then 
         code_retour%valeur = code_local%valeur 
         if (code_local%valeur<pm_OK) go to 6000 ! pas d'affectation des sorties 
      end if
      
      az= 0.5_pm_reel*(1._pm_reel+cos ((alt-110.e3_pm_reel)*cste_mixte))
      dens= az*dens_cira+(1._pm_reel-az)*dens_msis86
      temp1= az*temp_cira+(1._pm_reel-az)*temp_msis86
      pres1=az*pres_cira+(1._pm_reel-az)*pres_msis86
      
   end if
end if


! affectation des sorties
! =======================

if (present(temp)) temp = temp1
if (present(pres)) pres = pres1


6000 continue

!code_retour%routine = pm_num_mp_atm_cira_msis86
!code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine cps_atm_cira_msis86

end module cps_atm_cira_msis86_mod
