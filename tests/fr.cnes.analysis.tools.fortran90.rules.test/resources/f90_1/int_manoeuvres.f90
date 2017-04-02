module int_manoeuvres
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème M
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement par l'intermédiaire du module global
!   "mslib90"
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : DM-ID 616 (option) : inclusion des constantes du module 
!     indic_reperes_mslib en vue de la suppression de ce module
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
use longueur_chaine_mslib
implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_manoeuvres.f90 362 2013-02-15 18:01:28Z bbjc $'


! Valeurs associees aux indicateurs de repere:
! ============================================

! Convention adoptee : repere geocentrique inertiel -> pm_rep_geo
!                      repere qsw                   -> pm_rep_qsw
!                      repere tnw                   -> pm_rep_tnw

integer, parameter :: pm_rep_geo = 1001
integer, parameter :: pm_rep_qsw = 2002
integer, parameter :: pm_rep_tnw = 2003

public
interface
     subroutine mm_impul_car( pos_car, vit_car_avant, rep, impul, vit_car_apres, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)    :: pos_car        ! position du satellite
real(pm_reel), dimension(3), intent(in)    :: vit_car_avant  ! vitesse du satellite avant la manoeuvre
integer,                     intent(in)    :: rep            ! indicateur du repere dans lequel est 
real(pm_reel), dimension(3), intent(in)    :: impul          ! vecteur impulsion
real(pm_reel), dimension(3), intent(out)   :: vit_car_apres  ! vitesse du satellite apres la manoeuvre
type(tm_code_retour)       , intent(out)   :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mm_impul_car
     subroutine mm_impul_kep( mu, kep_avant, rep, impul, kep_apres, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                      intent(in)  :: mu           ! constante de la gravitation
type(tm_orb_kep),                   intent(in)  :: kep_avant    ! parametres kepleriens avant la manoeuvre
integer,                            intent(in)  :: rep          ! indicateur du repere dans lequel est 
real(pm_reel), dimension(3),        intent(in)  :: impul        ! vecteur impulsion
type(tm_orb_kep),                   intent(out) :: kep_apres    ! parametres kepleriens apres la manoeuvre
type(tm_code_retour),               intent(out) :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mm_impul_kep
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_manoeuvres.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_manoeuvres
