subroutine mr_tsid_veis (jul1950, delta_tu1, tsid, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Calcul du Temps SIDeral dans le repere de reference VEIS (Gamma50 CNES)
! ===
!
! Note d'utilisation:  delta_tu1 est donne par le BIH dans ses publications (circulaire D, rapid Service)
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 287 ed01 rev00): creation a partir de la routine MRTSID de la MSLIB f77
!                         (Date: 11/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 367 ed01 rev00): utilisation de md_joursec_jourfrac
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!       remplacement du module math_mslib (supprimé) par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg
use type_mslib
use parametre_mslib

use int_dates, only : md_joursec_jourfrac

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)             ::  jul1950   !  date julienne

real(pm_reel), intent(in)                 ::  delta_tu1 ! delta TU1

real(pm_reel), intent(out)                ::  tsid      !  temps sideral
type(tm_code_retour), intent(out)         ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)            :: jour_frac, secondes_en_jour    ! variables intermediaires de calcul 
real(pm_reel), parameter :: a1  =  .1746647708617871e+01_pm_reel ! radians, soit 100.075542 degres
real(pm_reel), parameter :: eps = 1.7202179573714597e-02_pm_reel ! radians/jour , soit 0.985612288 degres par jour 
                                                                 ! (a2 - 360, voir explications plus loin)
real(pm_reel), parameter :: a3  =  .7292115146705e-04_pm_reel    ! radians/seconde, soit (360+eps)/86400 degres/seconde
real(pm_reel)            :: tsid_int                             ! valeur intermediaire
type(tm_jour_sec)        :: jul1950_temp                         ! variable temporaire pour l appel a md_joursec_jourfrac
type(tm_code_retour)     :: code_retour_local

intrinsic modulo

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_tsid_veis.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_tsid_veis.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************
!              EXPLICATIONS SUR LES CONSTANTES UTILISEES  
!     .................................................................
!     par abus de langage, et pour plus de clarte dans les explications
!     on utilisera 2.pi avec des valeurs en degres dans les formules.
!     .................................................................
!     la reference Georges Veis - the system reference, 
!     in Sao special report 200, 1er volume, 1966
!     donne les constantes
!             a1=100.075542 degres
!             a2=360.985612288 degres par jour

!     pour un calcul du temps sideral:
!                        tsid = ( a1 + a2*jour_frac ) modulo 2.pi

!  or jour_frac est exprime dans une echelle de temps cnes (tuc) 
!     differente de l'echelle de temps tu1 utilisee pour la formulation
!     du temps sideral ci-dessus. La formule est donc adaptee en:
!                        tsid = ( a1 + a2*jour_frac + a3*rdtu1 ) modulo 2.pi

!        avec a3    = a2/86400 degres par seconde
!             rdtu1 = ecart en seconde entre tu1 et echelle de
!                     datation de jour_frac

!     pour des raisons de pertes de precision numerique l'algorithme
!     est adapte en: 
!         tsid = (a1 + 2.pi*frac(jour_frac) + eps*jour_frac + a3*rdtu1 ) mod 2.pi

!        avec frac(jour_frac) = partie fractionnaire de jour_frac
!             eps        = a2 - 360 (en degres par jour)

!      remarque1: on trouve souvent dans la litterature la valeur
!                 erronnee a3 = 0.004177 degres par seconde
!                 qui conduit a une
!                 valeur erronne = 0.729024d-04 radians par secondes
!      remarque2: les valeurs en radians des parameter ont ete verifiees
!                 sous maple

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

!     -----------------------
! calcul du temps sideral
!     -----------------------

jul1950_temp%jour = jul1950%jour
jul1950_temp%sec  = jul1950%sec
secondes_en_jour  = jul1950%sec/86400._pm_reel
call md_joursec_jourfrac ( jul1950_temp, jour_frac, code_retour_local )   ! code_retour_local%valeur = 0 est le seul retour possible => non teste
tsid_int = a3 * delta_tu1 + a1 + pm_deux_pi * secondes_en_jour + eps * jour_frac

tsid = modulo(tsid_int,pm_deux_pi)

code_retour%routine = pm_num_mr_tsid_veis
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_tsid_veis
