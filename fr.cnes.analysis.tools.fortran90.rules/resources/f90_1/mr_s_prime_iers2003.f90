subroutine mr_s_prime_iers2003(jul1950,s_prime,code_retour)

! (C) Copyright CNES - MSLIB - 2008

!************************************************************************
!
! But:  Calcul du paramètre s' selon la définition de l'IERS
! ===
!
! $Remarques : ce code est couvert par la DV BIBMS n°18 (Code lié aux changements de repères de l'IERS 2003 (MSLIB90))
!              Plus d'informations sur ces routines est disponible dans la note algorithmique du thème R de la MSLIB90
!              -> BIBMS-SME-19-2025-ATOS
! 
!$Historique
! ==========
!   + Version 6.9 : DM-ID 1092 : Création
!                   (date : septembre 2008 - Réalisation : Atos Origin)
!   + Version 6.10 : AQ : application de la DV 18
!                   (Date: 10/2008 - Realisation: Atos origin)
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
use parametres_internes_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use int_constantes

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),             intent(in)            ::  jul1950       ! Date du calcul (J2000)
real(pm_reel),                 intent(out)           ::  s_prime       ! Paramètre s'
type(tm_code_retour),          intent(out)           ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(kind=pm_reel)   :: jul1950_frac
real(kind=pm_reel)   :: T
real(kind=pm_reel), parameter :: correction = -47.E-6_pm_reel
type(tm_code_retour) :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_s_prime_iers2003.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_s_prime_iers2003.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK
code_retour_local%valeur = pm_OK

! Conversion de la date en jour frac
call md_joursec_jourfrac(jul1950,jul1950_frac,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   ! Probleme lors de la conversion de la date
   code_retour%valeur = pm_err_conv_date
   goto 6000
end if

! Calcul de la quantité T a partir de la date jul2000=jul1950+18262.5
T = (jul1950_frac - pm_i_date_t2000) / pm_DJC

! Calcul de la quantité s'
s_prime = correction * T * pm_as_rad

6000 continue

code_retour%routine = pm_num_mr_s_prime_iers2003
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_s_prime_iers2003
