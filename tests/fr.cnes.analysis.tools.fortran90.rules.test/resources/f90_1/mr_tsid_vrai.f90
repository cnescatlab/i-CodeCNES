subroutine mr_tsid_vrai ( model, jul1950, delta_tu1, delta_tai, tsid_vrai, code_retour, offset_nuta, deriv_tsid_vrai)

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Calcul du temps sideral vrai
! ===
!
! Notes d'utlisation:  [DR1] "Les systemes de reference utilises en astronomie"
! ===================        de M. Chapront-Touze, G. Francou et B. Morando
!                            Bureau Des Longitudes (BDL) novembre 1994
!                            ISSN 1243-4272
!                            ISBN 2-910015-05-X
!                            nomenclature MSLIB M-NT-0-160-CN
!                      [DR2] "Algorithmes des routines du theme 
!                             Les Reperes Fondamentaux de la MSLIB" 
!                             nomenclature MSLIB M-NT-0-95-CN
!
!$Historique
! ==========
!   + Version 4.0 (SP 473 ed01 rev00): creation a partir de la routine interne mri_tsid_vrai (corrigee de m-de-1121-467-CIS et m-de-1000-480-CIS)
!                         (Date: 01/2003 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.3 (DM-ID  337 ed01 rev02): ajout de la prise en compte des corrections en longitude et obliquite
!                         (Date: 10/2005 - Realisation: Equipe Patrimoine Mécanique spatiale - ATOS ORIGIN)
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
use int_rep_fondamentaux, only : mr_tsid_aoki
use int_rep_fondamentaux, only : mr_nuta
use int_rep_fondamentaux, only : mr_obli_moy
use int_rep_internes, only : mri_arg_fon

use type_mslib
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg
use parametre_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,           intent(in)           :: model      ! indicateur pour les modeles de precession + nutation
type(tm_jour_sec), intent(in)           :: jul1950    ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),     intent(in)           :: delta_tu1  ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),     intent(in)           :: delta_tai  ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),     intent(out)          :: tsid_vrai  ! temps sideral vrai (radians)
type(tm_code_retour), intent(out)       :: code_retour
type(tm_nuta),     intent(in), optional :: offset_nuta     ! corrections en longitude et obliquite (radians)
real(pm_reel),     intent(out),optional :: deriv_tsid_vrai ! derivee du temps sideral vrai (radians/sec)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

! inverse du nombre de secondes par jour
real(pm_reel), parameter      :: unsur86400 = 1._pm_reel/86400._pm_reel

! inverse du nombre de jours en cent ans Juliens (C = cent = 100; Can = 100 ans) 
real(pm_reel), parameter      :: unsurCanJul = 1._pm_reel/36525._pm_reel 

! inverse du nombre de secondes pour cent ans Juliens
real(pm_reel), parameter      :: unsurSecCan = unsur86400*unsurCanJul

 ! t2000 = date julienne CNES au 01-01-2000 a 12h TDB~TE (confondu avec TE)
real(pm_reel), parameter      :: t2000 = 18262.5_pm_reel

real(pm_reel), parameter      :: p21 = 8640184.812866_pm_reel   ! parametres pour calculer la
real(pm_reel), parameter      :: p22 = 0.093104_pm_reel         ! derivee du temps sideral
real(pm_reel), parameter      :: p23 = 6.2e-6_pm_reel           ! moyen ( modele de Lieske ).

real(pm_reel)                 :: tu,tu2
real(pm_reel)                 :: tsid_moy, tsid_moyd=0._pm_reel ! calcul du temps sideral moyen et de sa derivee
type(tm_jour_sec)             :: joursec_ut1, joursec_tai       ! variables de calcul
type(tm_nuta)                 :: nuta                           ! nutation en longitude, nutation en obliquite
type(tm_nuta)                 :: offset                         ! corrections en longitude et en obliquite
type(tm_nuta)                 :: deriv1_nuta, deriv2_nuta       ! derivee 1ere et 2ieme de la nutation en longitude et en obliquite
real(pm_reel)                 :: obli_moy                       ! obliquite moyenne
real(pm_reel)                 :: deriv1_obli, deriv2_obli       ! derivee 1ere et 2ieme de l'obliquite

integer                       ::  modele_precession, modele_nutation  ! indicateurs des modeles de precession et de nutation
                                                                      ! associes a "model" en entree

real(pm_reel), dimension(5)   :: arg_fon                        ! arguments fondamentaux (rad)
real(pm_reel), dimension(5,2) :: arg_deriv                      ! derivees premieres (rad/s) et secondes (rad/s**2) des arguments fondamentaux
real(pm_reel)                 :: omega, omegad                  ! longitude du noeud ascendant de la Lune, derivee
integer                       :: retour_local
real(pm_reel), parameter      :: srad = pm_deg_rad/3600._pm_reel      ! conversion secondes d'arc en radians

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_tsid_vrai.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_tsid_vrai.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

if (present(offset_nuta)) then
   offset%long = offset_nuta%long
   offset%obli = offset_nuta%obli
else
   offset%long = 0.0_pm_reel
   offset%obli = 0.0_pm_reel
endif
  
! Debut de corps du programme
! ===========================

! test sur le modele choisi

if (model /= pm_lieske_wahr) then

   code_retour%valeur = pm_err_ind_model
   go to 6000

end if

modele_precession = pm_lieske
modele_nutation   = pm_wahr

! passage de la date t en date UT1

joursec_ut1%jour = jul1950%jour
joursec_ut1%sec  = jul1950%sec + delta_tu1

! passage de la date t en date TAI

joursec_tai%jour = jul1950%jour
joursec_tai%sec  = jul1950%sec + delta_tai

! calcul de omega et omegad
call mri_arg_fon( joursec_tai, arg_fon, retour_local, arg_deriv=arg_deriv ) ! code retour non teste car toujours = a 0

omega  = arg_fon(5)
omegad = arg_deriv(5,1)

! temps sideral moyen

call mr_tsid_aoki ( jul1950, delta_tu1, tsid_moy, code_retour )
if (code_retour%valeur < pm_OK)     go to 6000

! equation des equinoxes

! nutations en longitude (nuta%long) et en obliquite (nuta%obli) de la date

call mr_nuta ( modele_nutation, jul1950, nuta, code_retour, delta_tai=delta_tai, offset_nuta=offset,   &
               deriv1_nuta=deriv1_nuta, deriv2_nuta=deriv2_nuta )
if (code_retour%valeur < pm_OK)     go to 6000

! obliquite moyenne de la date

call mr_obli_moy ( modele_precession, jul1950, obli_moy, code_retour, delta_tai=delta_tai,      &
                   deriv1_obli=deriv1_obli, deriv2_obli=deriv2_obli )
if (code_retour%valeur < pm_OK)     go to 6000

! temps sideral vrai : voir formules donnees dans [DR2]

tsid_vrai = nuta%long * cos(obli_moy) + tsid_moy + 0.00264_pm_reel*srad*sin(omega) + 0.000063_pm_reel*srad*sin(2._pm_reel*omega)

if (present(deriv_tsid_vrai)) then

   ! derivee du temps sideral moyen

   if (model == pm_lieske_wahr) then
   
      ! voir formules 9.17 et 9.8 de [DR1]
      tu  = ((real(joursec_ut1%jour, pm_reel) - t2000) + joursec_ut1%sec * unsur86400) * unsurCanJul
      tu2 = tu * tu

      ! derivee de la formule 9.16 de [DR1]
      tsid_moyd = pm_deux_pi * unsur86400 * (1._pm_reel + unsurSecCan * (p21 + 2._pm_reel * p22 * tu - 3._pm_reel * p23 * tu2))

   end if

   ! derivee du temps sideral vrai : voir formules donnees dans [DR2]

   deriv_tsid_vrai = deriv1_nuta%long * cos(obli_moy) - nuta%long * deriv1_obli * sin(obli_moy) + tsid_moyd +        &
        0.00264_pm_reel * srad * omegad * cos(omega) + 0.000063_pm_reel * srad * 2._pm_reel * omegad * cos(2._pm_reel*omega)

end if

! Fin de corps du programme
! =========================

6000 continue

code_retour%routine = pm_num_mr_tsid_vrai
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_tsid_vrai
