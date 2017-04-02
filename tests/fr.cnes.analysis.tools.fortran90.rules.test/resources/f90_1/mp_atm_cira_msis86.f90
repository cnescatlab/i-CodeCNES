subroutine mp_atm_cira_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
                                dens,code_retour,temp,pres)
! (C) Copyright CNES - MSLIB - 2001

!************************************************************************
!
! But: Modele mixte d'atmosphere CIRA-MSIS86         
! ===
!
! Note d'utilisation:  :
! ==================            
!         Pour des altitudes <= 110 km : utilisation de CIRA 
!                            >=120 km utilisation de MSIS86
!                    entre 110 km et 120 km utilisation du modele mixte CIRA-MSIS86
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
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use int_geophysique_mspro, only : mp_atm_cira
use int_geophysique_mspro, only : mp_atm_msis86

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel), parameter :: cste_mixte = pm_pi*1.e-4_pm_reel ! constante pour le modele mixte

! Declarations pour l'appel des routines MSPRO mp_atm_cira et mp_atm_msis86
integer                           :: mois,an,heure,jour,minute          ! mois de l'annee (autres donnees inutiles)
type(tm_geodesique)               :: pos_geod                           ! coordonnees geodesiques
real(pm_reel)                     :: temp_cira,dens_cira,dens_msis86    ! temperature,densite du modele cira,densite du modele msis86
real(pm_reel)                     :: pres_cira, pres_msis86             ! pression du modele cira,pression du modele msis86
real(pm_reel)                     :: temp_msis86
real(pm_reel)                     :: az,temp1,pres1,sec
type(tm_code_retour)              :: code_local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
         '@(#) Fichier MSPRO mp_atm_cira_msis86.f90: derniere modification V5.15 >'

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
   call mp_atm_cira (mois, pos_geod, temp_cira, pres_cira, dens_cira, code_local)
   if (code_local%valeur/=pm_OK) then 
      code_retour%valeur = code_local%valeur
      if (code_local%valeur<pm_OK) go to 6000 ! pas d'affectation des sorties 
   end if
   dens = dens_cira
   temp1 = temp_cira 
   pres1 = pres_cira  
else 
   if (alt >= 120.e3_pm_reel) then
      call  mp_atm_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
           dens_msis86,code_local,temp=temp_msis86,pres=pres_msis86)
      if (code_local%valeur/=pm_OK) then 
         code_retour%valeur = code_local%valeur 
         if (code_local%valeur<pm_OK) go to 6000 ! pas d'affectation des sorties 
      end if
      dens = dens_msis86
      temp1 = temp_msis86
      pres1 = pres_msis86 

   else ! modele mixte entre 110 et 120 km

      call mp_atm_cira (mois, pos_geod, temp_cira, pres_cira, dens_cira, code_local)
      if (code_local%valeur/=pm_OK) then 
         code_retour%valeur = code_local%valeur 
         if (code_local%valeur<pm_OK) go to 6000 ! pas d'affectation des sorties 
      end if
      
      call  mp_atm_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
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

code_retour%routine = pm_num_mp_atm_cira_msis86
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mp_atm_cira_msis86
