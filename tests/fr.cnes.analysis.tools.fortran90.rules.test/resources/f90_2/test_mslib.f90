
module test_mslib

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Definition des epsilons de test sur les types d'orbite  
! ===
!
!$Historique
! ==========
!   + Version 1.0 (SP 212 ed01 rev00) : creation a partir du fichier include
!                                       mphys.h de la MSLIB77
!                        (Date 07/1998 ed01 rev00 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 348 ed01 rev00) : ajout de l'epsilon pour encadrer les inclinaisons critiques + commentaires pour i = pi
!                        (Date 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.0 (DE 433 ed01 rev00) : suppression de caracteres au dela de 254 colonnes
!                        (Date 10/2000 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE 489 ed01 rev00) : ajout de pm_eps_e_faible
!                        (Date 10/2003 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/03/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

  use precision_mslib           ! definition des precisions retenues
  use longueur_chaine_mslib     ! definition des longueurs de chaines de caracteres

  implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: test_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


  real(pm_reel),parameter :: pm_eps_cir            = 1.e-10_pm_reel  ! si e < eps_cir, orbite circulaire
  real(pm_reel),parameter :: pm_eps_parab          = 1.e-10_pm_reel  ! si e <= 1-eps_parab orbite elliptique,
                                                                     ! si e >= 1+eps_parab, orbite hyperbolique, parabole sinon
  real(pm_reel),parameter :: pm_eps_equa           = 1.e-10_pm_reel  ! si sin(i) < eps_equa, orbite equatoriale
!------------------------------------------------------------------------------------------------
! pour tester si une orbite a une inclinaison egale a pi (orbite equatoriale retrograde)
! deux tests possibles et equivalents:
!            si inclinaison > pi - epsiretro ===> inclinaison = pi
!               avec: epsiretro = 20._pm_reel*sqrt(epsilon(1._pm_reel))
!            si h**2 > 4._pm_reel*(1._pm_reel-100._pm_reel*epsilon(1._pm_reel)) ===> inclinaison = pi
!               avec: vecteur inclinaison h=(hx,hy)
!                     h**2 = norme au carre du vecteur inclinaison h
!                     hx = 2 sin(i/2)cos(omega)
!                     hy = 2 sin(i/2)sin(omega)
!------------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------------
  real(pm_reel),parameter :: pm_eps_i_critique     = 10.e-3_pm_reel

! si |i - pm_i_critique_non_retro| <= pm_eps_i_critique , le calcul n'est pas possible 
! car i est trop proche de l'inclinaison critique non retrograde
! (pm_i_critique_non_retro contient la valeur de l'inclinaison critique non retrograde).

! si |i - pm_i_critique_retro| <= pm_eps_i_critique , le calcul n'est pas possible 
! car i est trop proche de l'inclinaison critique retrograde
! (pm_i_critique_retro contient la valeur de l'inclinaison critique retrograde).
!------------------------------------------------------------------------------------------------

  real(pm_reel),parameter :: pm_eps_e_faible     = 10.e-7_pm_reel

! cette valeur correspond a la limite en-dessous de laquelle le calcul de e par mv_car_kep (ou mv_car_equa)
! n'est plus possible numeriquement, car e*e se trouve alors en-deca de l'epsilon machine (environ 2e-15).

!................................................................................................................

  character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
                    '@(#) Fichier MSLIB test_mslib.f90: derniere modification V6.13 >'

!................................................................................................................

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: test_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module test_mslib
