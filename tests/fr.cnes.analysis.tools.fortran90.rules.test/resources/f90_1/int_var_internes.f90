module int_var_internes
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des routines internes du thème V
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_var_internes.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine mvi_barker( anom_M, D, retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                 :: anom_M   ! anomalie moyenne (M)
real(pm_reel), intent(out)                :: D        ! tangente[anomalie vraie/2]
integer, intent(out)                      :: retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_barker
     subroutine mvi_car_equa_ellip( pos_car, vit_car,rrxv,norme_vect_pos,norme_vect_vit,norme_moment_cinetique,&
                                    un_sur_a,excentricite ,rscal,mu,equa,retour,jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel),dimension(3), intent(in)               ::  pos_car ! position en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)               ::  vit_car ! vitesse en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)               ::  rrxv    ! produit vectoriel position x vitesse
real(pm_reel),intent(in)                             ::  norme_vect_pos   !  norme du vecteur position
real(pm_reel),intent(in)                             ::  norme_vect_vit   !  norme du vecteur vitesse 
real(pm_reel),intent(in)                             ::  norme_moment_cinetique   !  norme du produit vectoriel de ces derniers
real(pm_reel),intent(in)                             ::  un_sur_a         ! 1/a
real(pm_reel),intent(in)                             ::  excentricite     !  excentricite
real(pm_reel),intent(in)                             ::  rscal   ! produit scalaire position.vitesse
real(pm_reel),intent(in)                             ::  mu      ! constante de la gravitation

type(tm_orb_equa), intent(out)                       ::  equa    ! parametres orbitaux de l'orbite equatoriale
integer ,intent(out)                                 ::  retour
real(pm_reel),dimension(6,6), intent(out), optional  :: jacob    ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_car_equa_ellip
     subroutine mvi_car_equa_hyperb( pos_car, vit_car, rrxv, norme_vect_pos, norme_vect_vit, norme_moment_cinetique, &
                                     un_sur_a, excentricite, parametre, rscal, mu, equa, retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel),dimension(3), intent(in)               ::  pos_car ! position en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)               ::  vit_car ! vitesse en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)               ::  rrxv    ! produit vectoriel position x vitesse
real(pm_reel),intent(in)                             ::  norme_vect_pos   !  norme du vecteur position
real(pm_reel),intent(in)                             ::  norme_vect_vit   !  norme du vecteur vitesse 
real(pm_reel),intent(in)                             ::  norme_moment_cinetique   !  norme du produit vectoriel de ces derniers
real(pm_reel),intent(in)                             ::  un_sur_a         ! 1/a
real(pm_reel),intent(in)                             ::  excentricite     !  excentricite e
real(pm_reel),intent(in)                             ::  parametre        !  parametre p
real(pm_reel),intent(in)                             ::  rscal   ! produit scalaire position.vitesse
real(pm_reel),intent(in)                             ::  mu      ! constante de la gravitation

type(tm_orb_equa), intent(out)                       ::  equa    ! parametres orbitaux de l'orbite equatoriale
integer ,intent(out)                                 ::  retour
real(pm_reel),dimension(6,6), intent(out), optional  :: jacob    ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_car_equa_hyperb
     subroutine mvi_car_equa_parab( pos_car, vit_car, rrxv, norme_vect_pos, norme_moment_cinetique, excentricite ,&
                                    parametre, rscal, mu, equa, retour, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel),dimension(3), intent(in)               ::  pos_car ! position en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)               ::  vit_car ! vitesse en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)               ::  rrxv    ! produit vectoriel position x vitesse
real(pm_reel),intent(in)                             ::  norme_vect_pos   !  norme du vecteur position
real(pm_reel),intent(in)                             ::  norme_moment_cinetique   !  norme du produit vectoriel de ces derniers
real(pm_reel),intent(in)                             ::  excentricite     !  excentricite e
real(pm_reel),intent(in)                             ::  parametre        !  parametre p
real(pm_reel),intent(in)                             ::  rscal   ! produit scalaire position.vitesse
real(pm_reel),intent(in)                             ::  mu      ! constante de la gravitation

type(tm_orb_equa), intent(out)                       ::  equa    ! parametres orbitaux de l'orbite equatoriale
integer ,intent(out)                                 ::  retour
real(pm_reel),dimension(6,6), intent(out), optional  :: jacob    ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_car_equa_parab
     subroutine mvi_car_kep_ellip(pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_vect_vit,&
                              un_sur_a,e,pos_fois_vit,mu,kep,retour,jacob)
       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3),intent(in)     :: pos_car, vit_car ! positions vitesses en coordonnees cartesiennes
real(pm_reel), dimension(3),intent(in)     :: moment_cinetique ! moment cinetique du satellite
real(pm_reel),intent(in)                   :: norme_vect_pos   ! rayon vecteur
real(pm_reel),intent(in)                   :: norme_vect_vit   ! norme de la vitesse
real(pm_reel),intent(in)                   :: un_sur_a         ! inverse du demi grand axe
real(pm_reel),intent(in)                   :: e                ! excentricite
real(pm_reel),intent(in)                   :: pos_fois_vit     ! produit scalaire position x vitesse
real(pm_reel),intent(in)                   :: mu               ! constante de la gravitation

type(tm_orb_kep),intent(out)               :: kep              ! parametres osculateurs
integer ,intent(out)                       ::  retour
real(pm_reel), dimension(6,6), intent(out), optional  :: jacob ! jacobienne                  



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_car_kep_ellip
     subroutine mvi_car_kep_hyperb(pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_vect_vit,norme_moment_cinetique,&
                                   un_sur_a,excentricite, parametre,pos_fois_vit,mu,kep,retour,jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3),intent(in)     :: pos_car, vit_car ! positions vitesses en coordonnees cartesiennes
real(pm_reel), dimension(3),intent(in)     :: moment_cinetique ! moment cinetique du satellite
real(pm_reel),intent(in)                   :: norme_vect_vit   ! norme de la vitesse
real(pm_reel),intent(in)                   :: norme_moment_cinetique   ! norme du moment cinetique du satellite
real(pm_reel),intent(in)                   :: norme_vect_pos   ! rayon vecteur
real(pm_reel),intent(in)                   :: un_sur_a         ! inverse du demi grand axe
real(pm_reel),intent(in)                   :: parametre        ! parametre de l'hyperbole
real(pm_reel),intent(in)                   :: excentricite     ! excentricite
real(pm_reel),intent(in)                   :: pos_fois_vit     ! produit scalaire position x vitesse
real(pm_reel),intent(in)                   :: mu               ! constante de la gravitation

type(tm_orb_kep),intent(out)               :: kep              ! parametres osculateurs
integer ,intent(out)                       ::  retour
real(pm_reel), dimension(6,6), intent(out), optional  :: jacob ! jacobienne                  



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_car_kep_hyperb
     subroutine mvi_car_kep_parab(pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_moment_cinetique,&
                                  e, parametre,pos_fois_vit,mu,kep,retour,jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3),intent(in)     :: pos_car, vit_car ! positions vitesses en coordonnees cartesiennes
real(pm_reel), dimension(3),intent(in)     :: moment_cinetique ! moment cinetique du satellite
real(pm_reel),intent(in)                   :: norme_moment_cinetique   ! norme du moment cinetique du satellite
real(pm_reel),intent(in)                   :: norme_vect_pos   ! rayon vecteur
real(pm_reel),intent(in)                   :: parametre        ! parametre de la parabole
real(pm_reel),intent(in)                   :: e                ! excentricite
real(pm_reel),intent(in)                   :: pos_fois_vit     ! produit scalaire position x vitesse
real(pm_reel),intent(in)                   :: mu               ! constante de la gravitation

type(tm_orb_kep),intent(out)               :: kep              ! parametres kepleriens osculateurs
integer ,intent(out)                       :: retour
real(pm_reel), dimension(6,6), intent(out), optional  :: jacob ! jacobienne                  



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine mvi_car_kep_parab
     subroutine mvi_conv_anom_ellip (e, type_anom1, anom1, type_anom2, anom2, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)  :: e          ! excentricite
integer, intent(in)        :: type_anom1 ! type de l'anomalie d'entree
real(pm_reel), intent(in)  :: anom1      ! anomalie d'entree
integer, intent(in)        :: type_anom2 ! type d'anomalie demande
real(pm_reel), intent(out) :: anom2      ! anomalie demandee
integer, intent(out)       :: retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_conv_anom_ellip
     subroutine mvi_conv_anom_hyperb (e, type_anom1, anom1, type_anom2, anom2, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: e          ! excentricite
integer, intent(in)       :: type_anom1 ! type de l'anomalie d'entree
real(pm_reel), intent(in) :: anom1      ! anomalie d'entree
integer, intent(in)       :: type_anom2 ! type d'anomalie demande
real(pm_reel), intent(out):: anom2      ! anomalie demandee
integer, intent(out)      :: retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_conv_anom_hyperb
     subroutine mvi_conv_anom_parab (type_anom1, anom1, type_anom2, anom2, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)        :: type_anom1 ! type de l'anomalie d'entree
real(pm_reel), intent(in)  :: anom1      ! anomalie d'entree
integer, intent(in)        :: type_anom2 ! type d'anomalie demande
real(pm_reel), intent(out) :: anom2      ! anomalie demandee
integer, intent(out)       :: retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_conv_anom_parab
     subroutine mvi_equa_car_ellip(equa,mu,pos_car,vit_car,retour,jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       

type(tm_orb_equa),intent(in)             :: equa     ! parametres orbitaux de l'orbite equatoriale
real(pm_reel), intent(in)                :: mu       ! constante de la gravitation universelle

real(pm_reel), dimension(3), intent(out) :: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out) :: vit_car  ! vecteur vitesse du satellite
integer, intent(out)                     :: retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_equa_car_ellip
     subroutine mvi_equa_car_hyperb(equa,mu,pos_car,vit_car,retour,jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       

type(tm_orb_equa),intent(in)             :: equa     ! parametres orbitaux de l'orbite equatoriale
real(pm_reel), intent(in)                :: mu       ! constante de la gravitation universelle

real(pm_reel), dimension(3), intent(out) :: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out) :: vit_car  ! vecteur vitesse du satellite
integer, intent(out)                     :: retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_equa_car_hyperb
     subroutine mvi_equa_car_parab(equa,mu,pos_car,vit_car,retour,jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
type(tm_orb_equa),intent(in)             :: equa     ! parametres orbitaux de l'orbite equatoriale
real(pm_reel), intent(in)                :: mu       ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(out) :: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out) :: vit_car  ! vecteur vitesse du satellite
integer, intent(out)                     :: retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobien de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_equa_car_parab
     subroutine mvi_kep_car_ellip(  kep,mu,pos_car,vit_car,retour,jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                              
type(tm_orb_kep), intent(in)                         :: kep     ! parametres kepleriens
real(pm_reel), intent(in)                            :: mu      ! constante de la gravitation

real(pm_reel), dimension(3), intent(out)             :: pos_car ! position en coordonnees cartesiennes
real(pm_reel), dimension(3), intent(out)             :: vit_car ! vitesse ne coordonnees cartesiennes
integer ,intent(out)                                 ::  retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob   ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_kep_car_ellip
     subroutine mvi_kep_car_hyperb( kep,mu,pos_car,vit_car,retour,jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                              
type(tm_orb_kep), intent(in)                         :: kep     ! parametres kepleriens
real(pm_reel), intent(in)                            :: mu      ! constante de la gravitation

real(pm_reel), dimension(3), intent(out)             :: pos_car ! position en coordonnees cartesiennes
real(pm_reel), dimension(3), intent(out)             :: vit_car ! vitesse ne coordonnees cartesiennes
integer ,intent(out)                                 ::  retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob   ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_kep_car_hyperb
     subroutine mvi_kep_car_parab(  kep,mu,pos_car,vit_car,retour,jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                              
type(tm_orb_kep), intent(in)                         :: kep     ! parametres kepleriens
real(pm_reel), intent(in)                            :: mu      ! constante de la gravitation

real(pm_reel), dimension(3), intent(out)             :: pos_car ! position en coordonnees cartesiennes
real(pm_reel), dimension(3), intent(out)             :: vit_car ! vitesse ne coordonnees cartesiennes
integer ,intent(out)                                 ::  retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob   ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_kep_car_parab
     subroutine mvi_kepler_hyperb( anom_M, e, anom_E, retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                 :: anom_M   ! anomalie moyenne (M)
real(pm_reel), intent(in)                 :: e        ! excentricite (e)
real(pm_reel), intent(out)                :: anom_E   ! anomalie excentrique (E) 
integer, intent(out)                      :: retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mvi_kepler_hyperb
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_var_internes.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_var_internes
