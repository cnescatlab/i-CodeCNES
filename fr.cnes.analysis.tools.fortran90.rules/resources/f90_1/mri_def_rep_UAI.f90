subroutine mri_def_rep_UAI ( planete, modeleUAI, jul1950, alpha0, delta0, tsid, deriv_tsid, retour )

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  calculer les valeurs de alpha0, delta0, W et dW/dt pour une planete et un 
! ===   temps donnes, en fonction d'un modele choisi.
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!   + Version 5.0 (SP 492 ed01 rev00): creation
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
!   + Version 6.3 (DM-ID 394) : Qualite : augmentation du taux de commentaires
!                 (Date: 11/2005 - Realisation: Atos Origin)   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!       remplacement du module math_mslib (supprimé) par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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

use int_dates, only : md_joursec_jourfrac

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg
use type_mslib
use parametres_internes_mslib

use parametre_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                intent(in) ::  planete          ! planete
integer,                intent(in) ::  modeleUAI        ! modele UAI definissant les parametres du repere
type(tm_jour_sec),      intent(in) ::  jul1950          ! date julienne 1950 en jours secondes
real(pm_reel),          intent(out)::  alpha0           ! ascension droite du pole
real(pm_reel),          intent(out)::  delta0           ! declinaison du pole
real(pm_reel),          intent(out)::  tsid             ! longitude du meridien origine
real(pm_reel),          intent(out)::  deriv_tsid       ! derivee de cette longitude en fonction du temps
integer,                intent(out)::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)                 :: alpha0_d, delta0_d, tsid_d, deriv_tsid_d ! intermediaires en degres
real(pm_reel)                 :: d             ! date J2000 en jours fractionnaires
real(pm_reel)                 :: T             ! date J2000 en siecles juliens (36525 jours)
real(pm_reel)                 :: N             ! facteur specifique a Neptune
type(tm_code_retour)          :: code_retour_local

real(pm_reel) , parameter     :: delta_1950_2000 = 18262.5_pm_reel   ! nb de jours entre les origines 1950 et 2000
real(pm_reel),  parameter     :: unsur86400 = 1._pm_reel/86400._pm_reel ! inverse du nb de secondes dans un jour
real(pm_reel),  parameter     :: jsiecle = 36525._pm_reel          ! nb de jours dans un siecle

intrinsic cos,sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mri_def_rep_UAI.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mri_def_rep_UAI.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! Verification sur le modele
if ((modeleUAI <= pm_UAI_autre_modele).OR.(modeleUAI > pm_UAI2000)) then
   retour = pm_err_ind_model
   go to 6000
end if

!calcul des dates
call md_joursec_jourfrac(jul1950,d,code_retour_local)  ! code retour toujours OK
d = d - delta_1950_2000
T = d / jsiecle

! Calculs avec les valeurs du modele UAI 2000 ou du modele UAI 1994 suivant
! la valeur de modeleUAI
! Ces valeurs sont stockees dans le fichier parametres_internes_mslib.f90 :
! ascension droite (alpha0), declinaison (beta0) , temps sideral (w) des axes de rotation des astres

select case (planete)

   case(pm_pla_mercure)
      if (modeleUAI == pm_UAI1994) then
         alpha0_d = pm_i_uai1994_mercure_alpha0(1) + pm_i_uai1994_mercure_alpha0(2) * T
         delta0_d = pm_i_uai1994_mercure_delta0(1) + pm_i_uai1994_mercure_delta0(2) * T
         tsid_d = pm_i_uai1994_mercure_w(1) + pm_i_uai1994_mercure_w(2) * d
         deriv_tsid_d = pm_i_uai1994_mercure_w(2) * unsur86400
      else
         alpha0_d = pm_i_uai2000_mercure_alpha0(1) + pm_i_uai2000_mercure_alpha0(2) * T
         delta0_d = pm_i_uai2000_mercure_delta0(1) + pm_i_uai2000_mercure_delta0(2) * T
         tsid_d = pm_i_uai2000_mercure_w(1) + pm_i_uai2000_mercure_w(2) * d
         deriv_tsid_d = pm_i_uai2000_mercure_w(2) * unsur86400
      end if

   case(pm_pla_venus)
      if (modeleUAI == pm_UAI1994) then
         alpha0_d = pm_i_uai1994_venus_alpha0(1)
         delta0_d = pm_i_uai1994_venus_delta0(1)
         tsid_d = pm_i_uai1994_venus_w(1) + pm_i_uai1994_venus_w(2) * d
         deriv_tsid_d = pm_i_uai1994_venus_w(2) * unsur86400
      else
         alpha0_d = pm_i_uai2000_venus_alpha0(1)
         delta0_d = pm_i_uai2000_venus_delta0(1)
         tsid_d = pm_i_uai2000_venus_w(1) + pm_i_uai2000_venus_w(2) * d
         deriv_tsid_d = pm_i_uai2000_venus_w(2) * unsur86400
      end if

   case(pm_pla_terre)
      if (modeleUAI == pm_UAI1994) then
         alpha0_d = pm_i_uai1994_terre_alpha0(1) + pm_i_uai1994_terre_alpha0(2) * T
         delta0_d = pm_i_uai1994_terre_delta0(1) + pm_i_uai1994_terre_delta0(2) * T
         tsid_d = pm_i_uai1994_terre_w(1) + pm_i_uai1994_terre_w(2) * d
         deriv_tsid_d = pm_i_uai1994_terre_w(2) * unsur86400
      else
         alpha0_d = pm_i_uai2000_terre_alpha0(1) + pm_i_uai2000_terre_alpha0(2) * T
         delta0_d = pm_i_uai2000_terre_delta0(1) + pm_i_uai2000_terre_delta0(2) * T
         tsid_d = pm_i_uai2000_terre_w(1) + pm_i_uai2000_terre_w(2) * d
         deriv_tsid_d = pm_i_uai2000_terre_w(2) * unsur86400
      end if

   case(pm_pla_mars)
      if (modeleUAI == pm_UAI1994) then
         alpha0_d = pm_i_uai1994_mars_alpha0(1) + pm_i_uai1994_mars_alpha0(2) * T
         delta0_d = pm_i_uai1994_mars_delta0(1) + pm_i_uai1994_mars_delta0(2) * T
         tsid_d = pm_i_uai1994_mars_w(1) + pm_i_uai1994_mars_w(2) * d
         deriv_tsid_d = pm_i_uai1994_mars_w(2) * unsur86400
      else
         alpha0_d = pm_i_uai2000_mars_alpha0(1) + pm_i_uai2000_mars_alpha0(2) * T
         delta0_d = pm_i_uai2000_mars_delta0(1) + pm_i_uai2000_mars_delta0(2) * T
         tsid_d = pm_i_uai2000_mars_w(1) + pm_i_uai2000_mars_w(2) * d
         deriv_tsid_d = pm_i_uai2000_mars_w(2) * unsur86400
      end if

   case(pm_pla_jupiter)
      if (modeleUAI == pm_UAI1994) then
         alpha0_d = pm_i_uai1994_jupiter_alpha0(1) + pm_i_uai1994_jupiter_alpha0(2) * T
         delta0_d = pm_i_uai1994_jupiter_delta0(1) + pm_i_uai1994_jupiter_delta0(2) * T
         tsid_d = pm_i_uai1994_jupiter_w(1) + pm_i_uai1994_jupiter_w(2) * d
         deriv_tsid_d = pm_i_uai1994_jupiter_w(2) * unsur86400
      else
         alpha0_d = pm_i_uai2000_jupiter_alpha0(1) + pm_i_uai2000_jupiter_alpha0(2) * T
         delta0_d = pm_i_uai2000_jupiter_delta0(1) + pm_i_uai2000_jupiter_delta0(2) * T
         tsid_d = pm_i_uai2000_jupiter_w(1) + pm_i_uai2000_jupiter_w(2) * d
         deriv_tsid_d = pm_i_uai2000_jupiter_w(2) * unsur86400
      end if

   case(pm_pla_saturne)
      if (modeleUAI == pm_UAI1994) then
         alpha0_d = pm_i_uai1994_saturne_alpha0(1) + pm_i_uai1994_saturne_alpha0(2) * T
         delta0_d = pm_i_uai1994_saturne_delta0(1) + pm_i_uai1994_saturne_delta0(2) * T
         tsid_d = pm_i_uai1994_saturne_w(1) + pm_i_uai1994_saturne_w(2) * d
         deriv_tsid_d = pm_i_uai1994_saturne_w(2) * unsur86400
      else
         alpha0_d = pm_i_uai2000_saturne_alpha0(1) + pm_i_uai2000_saturne_alpha0(2) * T
         delta0_d = pm_i_uai2000_saturne_delta0(1) + pm_i_uai2000_saturne_delta0(2) * T
         tsid_d = pm_i_uai2000_saturne_w(1) + pm_i_uai2000_saturne_w(2) * d
         deriv_tsid_d = pm_i_uai2000_saturne_w(2) * unsur86400
      end if

   case(pm_pla_uranus)
      if (modeleUAI == pm_uAI1994) then
         alpha0_d = pm_i_uai1994_uranus_alpha0(1)
         delta0_d = pm_i_uai1994_uranus_delta0(1)
         tsid_d = pm_i_uai1994_uranus_w(1) + pm_i_uai1994_uranus_w(2) * d
         deriv_tsid_d = pm_i_uai1994_uranus_w(2) * unsur86400
      else
         alpha0_d = pm_i_uai2000_uranus_alpha0(1)
         delta0_d = pm_i_uai2000_uranus_delta0(1)
         tsid_d = pm_i_uai2000_uranus_w(1) + pm_i_uai2000_uranus_w(2) * d
         deriv_tsid_d = pm_i_uai2000_uranus_w(2) * unsur86400
      end if

   case(pm_pla_Neptune)
      if (modeleUAI == pm_UAI1994) then
         N = pm_i_uai1994_neptune_n(1) + pm_i_uai1994_neptune_n(2) * T
         N = N * pm_deg_rad
         alpha0_d = pm_i_uai1994_neptune_alpha0(1) + pm_i_uai1994_neptune_alpha0(2) * sin(N)
         delta0_d = pm_i_uai1994_neptune_delta0(1) + pm_i_uai1994_neptune_delta0(2) * cos(N)
         tsid_d = pm_i_uai1994_neptune_w(1) + pm_i_uai1994_neptune_w(2) * d &
              + pm_i_uai1994_neptune_w(3) * sin(N)
         deriv_tsid_d = pm_i_uai1994_neptune_w(2) * unsur86400 + pm_i_uai1994_neptune_w(3) * &
              (pm_i_uai1994_neptune_n(2) * unsur86400 / jsiecle) * cos(N)
      else
         N = pm_i_uai2000_neptune_n(1) + pm_i_uai2000_neptune_n(2) * T
         N = N * pm_deg_rad
         alpha0_d = pm_i_uai2000_neptune_alpha0(1) + pm_i_uai2000_neptune_alpha0(2) * sin(N)
         delta0_d = pm_i_uai2000_neptune_delta0(1) + pm_i_uai2000_neptune_delta0(2) * cos(N)
         tsid_d = pm_i_uai2000_neptune_w(1) + pm_i_uai2000_neptune_w(2) * d &
              + pm_i_uai2000_neptune_w(3) * sin(N)
         deriv_tsid_d = pm_i_uai2000_neptune_w(2) * unsur86400 + pm_i_uai2000_neptune_w(3) * &
              (pm_i_uai2000_neptune_n(2) * unsur86400 / jsiecle) * cos(N)
      end if

   case(pm_pla_pluton)
      if (modeleUAI == pm_UAI1994) then
         alpha0_d = pm_i_uai1994_pluton_alpha0(1)
         delta0_d = pm_i_uai1994_pluton_delta0(1)
         tsid_d = pm_i_uai1994_pluton_w(1) + pm_i_uai1994_pluton_w(2) * d
         deriv_tsid_d = pm_i_uai1994_pluton_w(2) * unsur86400
      else
         alpha0_d = pm_i_uai2000_pluton_alpha0(1)
         delta0_d = pm_i_uai2000_pluton_delta0(1)
         tsid_d = pm_i_uai2000_pluton_w(1) + pm_i_uai2000_pluton_w(2) * d
         deriv_tsid_d = pm_i_uai2000_pluton_w(2) * unsur86400
      end if

   case default
      retour = pm_err_planete
      go to 6000
end select

! passage en radians
alpha0 = alpha0_d * pm_deg_rad
delta0 = delta0_d * pm_deg_rad
tsid = tsid_d * pm_deg_rad
deriv_tsid = deriv_tsid_d * pm_deg_rad

6000 continue

end subroutine mri_def_rep_UAI
