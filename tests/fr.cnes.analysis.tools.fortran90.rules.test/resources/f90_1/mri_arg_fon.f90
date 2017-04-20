subroutine mri_arg_fon (jul1950, arg_fon, retour, arg_deriv)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Calcul des ARGuments FONdamentaux caracterisant a une date donnee les positions de la lune et du soleil, 
! ===   et leurs derivees, suivant le modele de Wahr 
!  
! Note d'utilisation: -  Routine interne.
! ==================  -  L'appelant doit avoir verifie que le code du modele correspond au modele de Wahr
!
!$Historique
! ==========
!   + Version 1.0 (SP 281 ed01 rev00): creation a partir de la routine MSARGF de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 365 ed01 rev00): suppression des commentaires sur la limitation sur les dates et des codes retour 
!                                      utilisation de md_joursec_jourfrac
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.4 : DM-ID 424 : Modification des arguments de la nutation luni-solaire
!                         (Date: 05/2006 - Realisation : Atos Origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use flags_mslib

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg


use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

use int_dates, only : md_joursec_jourfrac

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)               :: jul1950         ! date julienne 1950 (secondes en TAI)

real(pm_reel), intent(out),dimension(5)     :: arg_fon         ! arguments fondamentaux (rad)
integer, intent(out)                        :: retour
real(pm_reel), intent(out),dimension(5,2), optional :: arg_deriv ! derivees premieres (rad/s) et secondes (rad/s**2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
!     ... constantes utilisees dans les systemes de reference
real(pm_reel), parameter  :: cderp = 3.168808781403e-10_pm_reel ,cders = 1.00413490931e-19_pm_reel

integer,parameter              :: profil1=5, profil2=5  ! dimension du tableau de coefficients
!     ... Pour le modele de Wahr, coefficients utilises dans l'expression des arguments fondamentaux
real(pm_reel), dimension(profil1*profil2), parameter :: coef1b = (/    &
          .23555557434939e+1_pm_reel,          .62400601269133e+1_pm_reel,            &
               .16279050815375e+1_pm_reel,     .51984665886602e+1_pm_reel,            &
               .21824391966157e+1_pm_reel,                                            &
          .83286914257191e+4_pm_reel,          .62830195517140e+3_pm_reel,            &
               .84334661569164e+4_pm_reel,     .77713771455937e+4_pm_reel,            &
              -.33757044612636e+2_pm_reel,                                            &
          .1545547230000e-3_pm_reel,          -.26819893000000e-5_pm_reel,            &
              -.61819562100000e-4_pm_reel,     -.3088554040000e-4_pm_reel,            &
               .36226247900000e-4_pm_reel,                                            &
          .2503335000000e-6_pm_reel,          -.65930000000000e-9_pm_reel,            &
              .50275000000000e-8_pm_reel,      .31963800000000e-7_pm_reel,            &
              .373403e-7_pm_reel, -.11863e-8_pm_reel,                                 &
             -.557e-10_pm_reel,  -.202e-10_pm_reel,                                   &
             -.1536e-9_pm_reel,  -.2879e-9_pm_reel/)
real(pm_reel), dimension(profil1*profil2), parameter :: coef1 = (/    &
          .23555483935439e+1_pm_reel,          .62400359393259e+1_pm_reel,            &
               .16279019339719e+1_pm_reel,     .51984695135798e+1_pm_reel,            &
               .21824386243610e+1_pm_reel,                                            &
          .83286914228839e+4_pm_reel,          .62830195602418e+3_pm_reel,            &
               .84334661583185e+4_pm_reel,     .77713771461706e+4_pm_reel,            &
              -.33757045933754e+2_pm_reel,                                            &
          .1517951635554e-3_pm_reel,          -.27004122037801e-5_pm_reel,            &
              -.6427174970469e-4_pm_reel,     -.33408510765258e-4_pm_reel,            &
               .36142859926716e-4_pm_reel,                                            &
          .3102807559101e-6_pm_reel,          -.58177641733144e-7_pm_reel,            &
              .53329504922048e-7_pm_reel,      .92114599410811e-7_pm_reel,            &
              .38785094488762e-7_pm_reel, 0._pm_reel, 0._pm_reel,  0._pm_reel,        &
             0._pm_reel,  0._pm_reel/)
! Coefficients de l'expression de la position de la Lune et du Soleil pour le modele de Wahr
real(pm_reel),dimension(profil1,profil2)::coef2
real(pm_reel),dimension(profil1,profil2)::coef2b
real(pm_reel), parameter  :: delta_te_tai = 32.184_pm_reel!     ... difference entre le temps des ephemerides et le tai

real(pm_reel)                :: temps !     date en jour julien fractionnaire
real(pm_reel), dimension(5)  :: coeff !     coefficients pour un argument
integer                      :: i     !     indice de boucle
real(pm_reel), dimension(5)  :: rarfon!     tableau intermediaire des arguments fondamentaux

real(pm_reel)  :: rargf0,rargf1,rargf2,rargp0,rargp1,rargp2,rargs0,rargs1

type(tm_jour_sec)    :: jul1950_temp                   ! variable temporaire pour l appel a md_joursec_jourfrac
type(tm_code_retour) :: code_retour_local

intrinsic  modulo

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mri_arg_fon.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mri_arg_fon.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
 coef2 = reshape(coef1,(/profil1,profil2/))
 coef2b = reshape(coef1b,(/profil1,profil2/))
retour = pm_OK

! =========================================================
! date en siecles juliens
! conversion des secondes tai en temps dynamique terrestre
! =========================================================

jul1950_temp%jour = jul1950%jour
jul1950_temp%sec  = jul1950%sec + delta_te_tai
call md_joursec_jourfrac ( jul1950_temp, temps, code_retour_local )   ! code_retour_local%valeur = 0 est le seul retour possible => non teste
temps = temps / 36525._pm_reel
temps = temps - 0.5_pm_reel

! =======================
! arguments fondamentaux
! =======================

!     --------------------------
!     boucle sur les arguments
!     --------------------------

do  i = 1,5

   
   if (wahr_p5) then
     coeff(:) = coef2b(i,:)!     ... transfert dans variables de travail du modele de wahr ordre 5
   else
     coeff(:) = coef2(i,:)!     ... transfert dans variables de travail du modele de wahr ordre 4
   end if
   rargf0    = coeff(5) *temps + coeff(4)
   rargf1    = rargf0   *temps + coeff(3)
   rargf2    = rargf1   *temps + coeff(2)
   rarfon(i) = rargf2   *temps + coeff(1)

   arg_fon(i) = modulo ( rarfon(i),pm_deux_pi )

   if (present(arg_deriv)) then  ! calcul des derivees premieres et secondes

      !     -------------------
      !     derivee premiere
      !     -------------------
      rargp0      = 4._pm_reel*coeff(5)*temps + 3._pm_reel*coeff(4)
      rargp1      = rargp0*temps + 2._pm_reel*coeff(3)
      rargp2      = rargp1      *temps + coeff(2)
      arg_deriv (i,1) = rargp2 *cderp

      !     ------------------
      !     derivee seconde
      !     ------------------
      rargs0      = 12._pm_reel*coeff(5)*temps + 6._pm_reel*coeff(4)
      rargs1      = rargs0*temps + 2._pm_reel*coeff(3)
      arg_deriv(i,2) = rargs1*cders

   end if

end do

end subroutine mri_arg_fon
