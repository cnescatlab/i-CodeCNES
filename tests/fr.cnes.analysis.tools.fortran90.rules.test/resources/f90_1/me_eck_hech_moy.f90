subroutine me_eck_hech_moy ( r_equa, cn0, ecart_admi, osc, moy, code_retour )

! (C) Copyright CNES - MSLIB - 1999

!************************************************************************
!
! But:  Calcul des parametres moyens du modele d'extrapolation d'orbite de ECKSTEIN-HECHLER.
! ===
!
! Note d'utilisation:  Ce modele est base sur l'integration en 2 iterations des equations de
! ==================   Lagrange pour le mouvement keplerien perturbe par les 6 premiers termes
!                      zonaux du potentiel terrestre et est specialise dans les orbites de faible
!                      excentricite.
!                      Pour la definition des domaines d'utilisation, de precision degradee et
!                      d'erreur de e et de i : se reporter a la routine me_eck_hech.f90.
!
!$Historique
! ==========
!   + Version 2.0 (SP 342 ed01 rev00): creation a partir de la routine MEEH6M de la MSLIB f77
!                         (Date: 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.0 (FA 431 ed01 rev00) : initialisation a 0 du code retour temporaire
!                         (Date 10/2000 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib par 
!     une sélection de int_constantes
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
use int_extrapolation, only : me_eck_hech

use precision_mslib

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg


use test_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                        intent(in)  :: r_equa      ! rayon equatorial terrestre
real(pm_reel),        dimension(2:6), intent(in)  :: cn0         ! coefficients C20 a C60 denormalises
type(tm_orb_cir),                     intent(in)  :: ecart_admi  ! ecarts admissibles sur a,ex,ey,i,gom,pom+M
type(tm_orb_cir),                     intent(in)  :: osc         ! parametres osculateurs
type(tm_orb_cir),                     intent(out) :: moy         ! parametres moyens
type(tm_code_retour),                 intent(out) :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel)               :: mu                           ! constante gravitationnelle             
integer                     :: n,nmax                       ! nombre d'iterations et nombre max d'iterations
type(tm_orb_cir)            :: oscn, moy_local , moy_sortie ! parametres osculateurs calcules
type(tm_orb_cir)            :: ecart                        ! ecarts entre les parametres adaptes
type(tm_jour_sec)           :: date                         ! date a laquelle on extrapole les parametres
logical                     :: convergence                  ! variable logique a vrai si tests de convergence verifies
type(tm_code_retour)        :: code_retour_local
integer                     :: code_retour_temporaire

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB me_eck_hech_moy.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: me_eck_hech_moy.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK
code_retour_temporaire = pm_OK

! autres initialisations
! ......................
nmax        = 51          ! nombre maximum d'iterations admises
n           = 1           ! initialisation du numero d'iterations
convergence = .false.     ! initialisation de l'indicateur de convergence
date%jour   = 1_pm_entier ! la date se revele inutile dans l'appel a me_eck_hech tout comme
date%sec    = 0._pm_reel  ! mu - artifice permettant de reutiliser me_eck_hech plutot que de
mu          = 1._pm_reel  ! developper une nouvelle routine - initialisation a 1

! initialisation des moyens aux osculateurs en entree et passage des angles dans l'intervalle [0,2*pi]
! ....................................................................................................
moy_local%a     = osc%a
moy_local%ex    = osc%ex
moy_local%ey    = osc%ey
moy_local%i     = modulo(osc%i,pm_deux_pi)
moy_local%gom   = modulo(osc%gom,pm_deux_pi)
moy_local%pso_M = modulo(osc%pso_M,pm_deux_pi)

! debut d'iteration
! .................
do while (n<nmax .and..not.convergence)  ! iterations tant que le nombre maximum d'iterations n'est pas atteint

!  calcul des osculateurs correspondant aux moyens
   call me_eck_hech( mu, r_equa, cn0, date, moy_local, date, moy_sortie, code_retour_local, osc_t2=oscn )
   if (code_retour_local%valeur < 0) then
      code_retour%valeur = code_retour_local%valeur
      go to 6000
   else if (code_retour_local%valeur > 0) then
      code_retour_temporaire = code_retour_local%valeur
   end if

!  calcul des ecarts entre parametres osculateurs calcules et ceux de depart. Ces ecarts sont ramenes sur [-pi,pi].
   ecart%a     = osc%a - oscn%a
   ecart%ex    = osc%ex - oscn%ex
   ecart%ey    = osc%ey - oscn%ey
   ecart%i     = osc%i - oscn%i
   ecart%gom   = osc%gom - oscn%gom
   ecart%pso_M = osc%pso_M - oscn%pso_M
   if (ecart%i < -(pm_pi)) then
      ecart%i = ecart%i + pm_deux_pi
   else if (ecart%i > pm_pi) then
      ecart%i = ecart%i - pm_deux_pi
   end if
   if (ecart%gom < -(pm_pi)) then
      ecart%gom = ecart%gom + pm_deux_pi
   else if (ecart%gom > pm_pi) then
      ecart%gom = ecart%gom - pm_deux_pi
   end if
   if (ecart%pso_M < -(pm_pi)) then
      ecart%pso_M = ecart%pso_M + pm_deux_pi
   else if (ecart%pso_M > pm_pi) then
      ecart%pso_M = ecart%pso_M - pm_deux_pi
   end if

!  test de convergence
   if (abs(ecart%a) <= ecart_admi%a .and. abs(ecart%ex) <= ecart_admi%ex           &
        .and. abs(ecart%ey) <= ecart_admi%ey .and. abs(ecart%i) <= ecart_admi%i    &
        .and. abs(ecart%gom) <= ecart_admi%gom .and. abs(ecart%pso_M) <= ecart_admi%pso_M ) then

      convergence = .true. ! convergence atteinte
      code_retour%valeur = code_retour_temporaire

   else
!  reinitialisation des elements moyens
      moy_local%a     = moy_local%a+ecart%a
      moy_local%ex    = moy_local%ex+ecart%ex
      moy_local%ey    = moy_local%ey+ecart%ey
      moy_local%i     = modulo(moy_local%i+ecart%i,pm_deux_pi)
      moy_local%gom   = modulo(moy_local%gom+ecart%gom,pm_deux_pi)
      moy_local%pso_M = modulo(moy_local%pso_M+ecart%pso_M, pm_deux_pi)

   end if

   n = n + 1

end do

if (.not.convergence) code_retour%valeur = pm_err_conv_eck_hech   ! pas de convergence dans le nombre imparti d'iterations
                                                                  ! compte tenu des ecarts admissibles demandes

! affectation des parametres de sortie
! ....................................
moy = moy_local

6000 continue

code_retour%routine = pm_num_me_eck_hech_moy
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine me_eck_hech_moy
