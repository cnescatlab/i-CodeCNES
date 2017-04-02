module int_rep_orbitaux
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème O
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
!   + Version 6.6 : rajout du cartouche
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_rep_orbitaux.f90 362 2013-02-15 18:01:28Z bbjc $'


public
interface
     subroutine mo_def_qsw ( pos_car, vit_car, q, s, w, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent (in)  :: pos_car      ! position du satellite dans un repere inertiel
real(pm_reel), dimension(3), intent (in)  :: vit_car      ! vitesse du satellite dans un repere inertiel
real(pm_reel), dimension(3), intent (out) :: q            ! vecteur unitaire q du repere orbital local exprime
real(pm_reel), dimension(3), intent (out) :: s            ! vecteur unitaire s du repere orbital local exprime
real(pm_reel), dimension(3), intent (out) :: w            ! vecteur unitaire w du repere orbital local exprime
type(tm_code_retour), intent(out)         ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mo_def_qsw
     subroutine mo_def_tnw ( pos_car, vit_car, t, n, w, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent (in)  :: pos_car      ! position du satellite dans un repere inertiel
real(pm_reel), dimension(3), intent (in)  :: vit_car      ! vitesse du satellite dans un repere inertiel
real(pm_reel), dimension(3), intent (out) :: t            ! vecteur unitaire t du repere orbital local exprime
real(pm_reel), dimension(3), intent (out) :: n            ! vecteur unitaire n du repere orbital local exprime
real(pm_reel), dimension(3), intent (out) :: w            ! vecteur unitaire w du repere orbital local exprime
type(tm_code_retour), intent(out)         ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mo_def_tnw
     subroutine mo_geo_qsw( pos_car, vit_car, vect_geo, vect_qsw, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel),dimension(3), intent(in)    :: pos_car   ! position du satellite en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)    :: vit_car   ! idem pour la vitesse 
real(pm_reel),dimension(3), intent(in)    :: vect_geo  ! composantes du vecteur considere dans le repere geocentrique inertiel

real(pm_reel),dimension(3), intent(out)   :: vect_qsw  ! projections du vecteur considere suivant les axes du repere orbital local (q, s, w)
type(tm_code_retour), intent(out)         :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mo_geo_qsw
     subroutine mo_geo_tnw( pos_car, vit_car, vect_geo, vect_tnw, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in)              :: pos_car  ! vecteur position du satellite en geocentrique inertiel
real(pm_reel), dimension(3), intent(in)              :: vit_car  ! vecteur vitesse du satellite en geocentrique inertiel
real(pm_reel), dimension(3), intent(in)              :: vect_geo ! composantes du vecteur considere dans le repere geocentrique inertiel

real(pm_reel), dimension(3), intent(out)             :: vect_tnw ! projection du vecteur considere suivant les axes du repere (t,n,w)
type(tm_code_retour), intent(out)                    :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mo_geo_tnw
     subroutine mo_qsw_geo( pos_car, vit_car, vect_qsw, vect_geo, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel),        dimension(3), intent(in)  :: pos_car     ! position du satellite en coordonnees cartesiennes dans le repere geocentrique inertiel
real(pm_reel),        dimension(3), intent(in)  :: vit_car     ! idem pour la vitesse 
real(pm_reel),        dimension(3), intent(in)  :: vect_qsw    ! projections (Gq,Gs,Gw) du vecteur G suivant les axes du repere orbital local (q,s,w)
real(pm_reel),        dimension(3), intent(out) :: vect_geo    ! composantes (Gx,Gy,Gz) du vecteur G dans le repere geocentrique inertiel
type(tm_code_retour),               intent(out) :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mo_qsw_geo
     subroutine mo_tnw_geo( pos_car, vit_car, vect_tnw, vect_geo, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel),        dimension(3), intent(in)  :: pos_car     ! position du satellite en coordonnees cartesiennes dans le repere geocentrique inertiel
real(pm_reel),        dimension(3), intent(in)  :: vit_car     ! idem pour la vitesse 
real(pm_reel),        dimension(3), intent(in)  :: vect_tnw    ! projections (Gt,Gn,Gw) du vecteur G suivant les axes du repere orbital local (t,n,w)
real(pm_reel),        dimension(3), intent(out) :: vect_geo    ! composantes (Gx,Gy,Gz) du vecteur G dans le repere geocentrique inertiel
type(tm_code_retour),               intent(out) :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mo_tnw_geo
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_rep_orbitaux.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_rep_orbitaux
