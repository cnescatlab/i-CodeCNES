module int_rep_internes
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des routines internes du thème R
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement en interne de la MSLIB90
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : rajout du cartouche
!                   (Date: 05/2007 - Realisation: Atos origin)
!
!   + Version 6.9 : DM-ID 1092 : Ajout des nouvelles routines internes
!                   (Date: 09/2008 - Realisation: Atos origin)
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_rep_internes.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine mri_arg_fon (jul1950, arg_fon, retour, arg_deriv)

       use type_mslib
       use precision_mslib


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)               :: jul1950         ! date julienne 1950 (secondes en TAI)

real(pm_reel), intent(out),dimension(5)     :: arg_fon         ! arguments fondamentaux (rad)
integer, intent(out)                        :: retour
real(pm_reel), intent(out),dimension(5,2), optional :: arg_deriv ! derivees premieres (rad/s) et secondes (rad/s**2)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_arg_fon
     subroutine mri_def_rep_UAI ( planete, modeleUAI, jul1950, alpha0, delta0, tsid, deriv_tsid, retour )

       use type_mslib
       use precision_mslib



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


     end subroutine mri_def_rep_UAI
     subroutine mri_eclip_M_eclip_V ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_eclip_M_eclip_V
     subroutine mri_eclip_M_equa_M ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske: LIESKE 
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_eclip_M_equa_M
     subroutine mri_eclip_M_equa_V ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_eclip_M_equa_V
     subroutine mri_eclip_V_eclip_M ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_eclip_V_eclip_M
     subroutine mri_eclip_V_equa_M ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele -->pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_eclip_V_equa_M
     subroutine mri_eclip_V_equa_V ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_eclip_V_equa_V
     subroutine mri_eclip_moy_t1_t2 ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske: LIESKE 
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_eclip_moy_t1_t2
     subroutine mri_eclip_vrai_t1_t2 ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_eclip_vrai_t1_t2
     subroutine mri_equa_M_eclip_M( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske: LIESKE 
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_equa_M_eclip_M
     subroutine mri_equa_M_eclip_V ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske: LIESKE 
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_equa_M_eclip_V
     subroutine mri_equa_M_equa_V( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske: LIESKE 
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_equa_M_equa_V
     subroutine mri_equa_V_eclip_M ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_equa_V_eclip_M
     subroutine mri_equa_V_eclip_V ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_equa_V_eclip_V
     subroutine mri_equa_V_equa_M ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_equa_V_equa_M
     subroutine mri_equa_moy_t1_t2( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_equa_moy_t1_t2
     subroutine mri_equa_vrai_t1_t2 ( model, jul1950_t1, jul1950_t2, mat_pass, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_equa_vrai_t1_t2
     subroutine mri_R1_iers2003 ( phi,R1,code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                      intent(in)            :: phi ! Angle de rotation
real(pm_reel), dimension(3,3),      intent(out)           :: R1  ! Matrice de rotation
type(tm_code_retour),               intent(out)           :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_R1_iers2003
     subroutine mri_R2_iers2003 ( phi,R2,code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                      intent(in)            :: phi ! Angle de rotation
real(pm_reel), dimension(3,3),      intent(out)           :: R2  ! Matrice de rotation
type(tm_code_retour),               intent(out)           :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_R2_iers2003
     subroutine mri_R3_iers2003 ( phi,R3,code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                      intent(in)            :: phi ! Angle de rotation
real(pm_reel), dimension(3,3),      intent(out)           :: R3  ! Matrice de rotation
type(tm_code_retour),               intent(out)           :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_R3_iers2003
     subroutine mri_XYS_iers2003(jul1950,X,Y,S,DX,DY,DS,code_retour)

       use type_mslib
       use precision_mslib
       use valeur_code_retour_mslib
       use numero_routine_mslib
       use int_constantes, only : pm_deux_pi, pm_as_rad, pm_pi
       use parametres_internes_mslib, only : pm_i_jj_par_cj, pm_i_date_t2000, pm_i_unsur86400



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jour,sec)                             
real(pm_reel),                      intent(out)           :: X           ! 1er vecteur unitaire du CIP dans le GCRS
real(pm_reel),                      intent(out)           :: Y           ! 2eme vecteur unitaire du CIP dans le GCRS
real(pm_reel),                      intent(out)           :: S           ! quantité positionnant le CEO
real(pm_reel),                      intent(out)           :: DX          ! derivé de X a la date t
real(pm_reel),                      intent(out)           :: DY          ! derivé de Y a la date t
real(pm_reel),                      intent(out)           :: DS          ! derivé de S a la date t
type(tm_code_retour),               intent(out)           :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mri_XYS_iers2003

end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_rep_internes.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_rep_internes
