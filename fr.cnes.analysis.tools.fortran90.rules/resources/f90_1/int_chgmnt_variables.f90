module int_chgmnt_variables

! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème V
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
!    code_anomalies_mslib  en vue de la suppression de ce module
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_chgmnt_variables.f90 362 2013-02-15 18:01:28Z bbjc $'


! Valeurs associees aux types d'anomalies:
! =======================================

integer, parameter :: pm_anom_E = 11 ! anomalie excentrique : E pour l'ellipse, 
                                     ! H pour l'hyperbole et D pour la parabole
integer, parameter :: pm_anom_v = 12 ! anomalie vraie
integer, parameter :: pm_anom_M = 13 ! anomalie moyenne

public
interface
     subroutine mv_car_cir(  mu, pos_car, vit_car, cir, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)              :: mu  ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(in):: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(in):: vit_car  ! vecteur vitesse du satellite
type(tm_orb_cir),intent(out)           :: cir      ! parametres orbitaux de l'orbite circulaire
type(tm_code_retour), intent(out)      ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_car_cir
     subroutine mv_car_cir_equa(  mu, pos_car, vit_car, cir_equa, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)              :: mu                 ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(in):: pos_car            ! vecteur position du satellite
real(pm_reel), dimension(3), intent(in):: vit_car            ! vecteur vitesse du satellite
type(tm_orb_cir_equa),intent(out)      :: cir_equa           ! parametres orbitaux de l'orbite circulaire equatoriale
type(tm_code_retour), intent(out)      ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_car_cir_equa
     subroutine mv_car_equa( mu, pos_car, vit_car, equa, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)              :: mu  ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(in):: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(in):: vit_car  ! vecteur vitesse du satellite
type(tm_orb_equa),intent(out)          :: equa     ! parametres orbitaux de l'orbite equatoriale
type(tm_code_retour), intent(out)      ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_car_equa
     subroutine mv_car_kep( mu, pos_car, vit_car, kep, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                            :: mu     ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(in)              :: pos_car! vecteur position du satellite
real(pm_reel), dimension(3), intent(in)              :: vit_car! vecteur vitesse du satellite
type(tm_orb_kep),intent(out)                         :: kep    ! parametres kepleriens
type(tm_code_retour),intent(out)                     :: code_retour
real(pm_reel), dimension(6,6),intent(out),optional   :: jacob  ! jacobien de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_car_kep
     subroutine mv_cir_car( mu, cir, pos_car, vit_car, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)               :: mu  ! constante de la gravitation universelle
type(tm_orb_cir),intent(in)             :: cir      ! parametres orbitaux de l'orbite circulaire
real(pm_reel), dimension(3), intent(out):: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out):: vit_car  ! vecteur vitesse du satellite
type(tm_code_retour), intent(out)       ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_cir_car
     subroutine mv_cir_equa_car(  mu, cir_equa, pos_car, vit_car, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)               :: mu                 ! constante de la gravitation universelle
type(tm_orb_cir_equa),intent(in)        :: cir_equa           ! parametres orbitaux de l'orbite circulaire equatoriale
real(pm_reel), dimension(3), intent(out):: pos_car            ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out):: vit_car            ! vecteur vitesse du satellite
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_cir_equa_car
     subroutine mv_cir_equa_kep ( cir_equa, kep, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_cir_equa), intent(in)     ::  cir_equa     ! parametres adaptes aux orbites circulaires equatoriales
type(tm_orb_kep), intent(out)         ::  kep          ! parametres kepleriens
type(tm_code_retour), intent(out)     ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob  ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_cir_equa_kep
     subroutine mv_cir_kep ( cir, kep, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_cir), intent(in)                         ::  cir ! parametres adaptes aux orbites circulaires
type(tm_orb_kep), intent(out)                        ::  kep ! parametres kepleriens
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_cir_kep
     subroutine mv_conv_anom (e, type_anom1, anom1, type_anom2, anom2, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)         :: e          ! excentricite
integer, intent(in)               :: type_anom1 ! type de l'anomalie a convertir
real(pm_reel), intent(in)         :: anom1      ! valeur de l'anomalie a convertir
integer, intent(in)               :: type_anom2 ! type de l'anomalie de sortie
real(pm_reel), intent(out)        :: anom2      ! valeur de l'anomalie de sortie
type(tm_code_retour), intent(out) :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_conv_anom
     subroutine mv_equa_car( mu, equa, pos_car, vit_car, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       

real(pm_reel), intent(in)                :: mu  ! constante de la gravitation universelle
type(tm_orb_equa),intent(in)             :: equa     ! parametres orbitaux de l'orbite equatoriale
real(pm_reel), dimension(3), intent(out) :: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out) :: vit_car  ! vecteur vitesse du satellite
type(tm_code_retour), intent(out)        ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_equa_car
     subroutine mv_equa_kep ( equa, kep, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_equa), intent(in)                        ::  equa  ! parametres adaptes aux orbites equatoriales
type(tm_orb_kep), intent(out)                        ::  kep   ! parametres kepleriens
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_equa_kep
     subroutine mv_kep_car( mu, kep, pos_car, vit_car, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                            ::  mu  ! constante de la gravitation
type(tm_orb_kep), intent(in)                         ::  kep ! parametres kepleriens
real(pm_reel), dimension(3), intent(out)             ::  pos_car ! position en coordonnees cartesiennes
real(pm_reel), dimension(3), intent(out)             ::  vit_car ! vitesse en coordonnees cartesiennes
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob  !  jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kep_car
     subroutine mv_kep_cir ( kep, cir, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_kep), intent(in)                         ::  kep ! parametres kepleriens
type(tm_orb_cir), intent(out)                        ::  cir ! parametres adaptes aux orbites circulaires
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kep_cir

   subroutine mv_kep_cir_equa ( kep, cir_equa, code_retour, jacob )
   use type_mslib
   use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_kep), intent(in)        :: kep      ! parametres kepleriens
type(tm_orb_cir_equa), intent(out)  :: cir_equa ! parametres adaptes aux orbites circulaires equatoriales
type(tm_code_retour), intent(out)   ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob  ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   end subroutine mv_kep_cir_equa
     subroutine mv_kep_equa ( kep, equa, code_retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_orb_kep), intent(in)                         ::  kep   ! parametres kepleriens
type(tm_orb_equa), intent(out)                       ::  equa  ! parametres adaptes aux orbites equatoriales
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob ! jacobienne de la transformation




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kep_equa
     subroutine mv_kepler_bar( anom_M, e, anom_E_D, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                 :: anom_M   ! anomalie moyenne (M)
real(pm_reel), intent(in)                 :: e        ! excentricite (e)
real(pm_reel), intent(out)                :: anom_E_D ! anomalie excentrique (E) ou D = tangente[anomalie vraie/2] selon 
type(tm_code_retour), intent(out)         :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kepler_bar
     subroutine mv_kepler_gene( pso_M, ex, ey, pso_E, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                            :: pso_M       !  argument du perigee + anomalie moyenne (+ longitude du noeud ascendant)
real(pm_reel), intent(in)                            :: ex          !  composante x du vecteur excentricite
real(pm_reel), intent(in)                            :: ey          !  composante y du vecteur excentricite

real(pm_reel), intent(out)                           :: pso_E       ! argument du perigee + anomalie excentrique (+ longitude du noeud ascendant)
type(tm_code_retour), intent(out)                    :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kepler_gene
     subroutine mv_kepler_std (anom_M, e, anom_E, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                :: anom_M      ! anomalie moyenne (M)                                              
real(pm_reel), intent(in)                :: e           ! excentricite (e)

real(pm_reel), intent(out)               :: anom_E      ! anomalie excentrique (E)  
type(tm_code_retour), intent(out)        :: code_retour !code retour






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mv_kepler_std
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_chgmnt_variables.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_chgmnt_variables
