module int_interpla_internes
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des routines internes du thème I
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_interpla_internes.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine mii_pb_equa_lambert(param_x, param_t, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)               ::  param_x  ! 
real(pm_reel), intent(out)              ::  param_t  ! 
type(tm_code_retour) ,intent(out)       ::  code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mii_pb_equa_lambert
     subroutine mii_pb_lambert_jac ( mu, pos_car, vit_car, duree, deriv, retour, encke)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                         :: mu        ! constante d'attraction du corps central
real(pm_reel), dimension(3), intent(in)           :: pos_car   ! vecteur position en coordonnées cartésiennes
real(pm_reel), dimension(3), intent(out)          :: vit_car   ! vecteur vitesse en coordonnées cartésiennes
real(pm_reel), intent(in)                         :: duree     ! duree de transfert
real(pm_reel), dimension(6,8), intent(out)        :: deriv     ! matrice (6,8) des derivees partielles de Lambert
integer , intent(out)                             :: retour
real(pm_reel), intent(out) , optional             :: encke     ! approximation de solution de l'equation de kepler generalise a t ou parametre de encke



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mii_pb_lambert_jac
     subroutine mii_pb_parg0(pos_car_t0, pos_car_t1, ind_sens, gom, inc, rga, rgb, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in) ::  pos_car_t0  ! COMPOSANTES CARTESIENNES VECT UNITAIRE JOIGNANT 1ER PT
real(pm_reel), dimension(3), intent(in) ::  pos_car_t1  ! COMPOSANTES CARTESIENNES VECT UNITAIRE JOIGNANT 2EM PT
integer ,intent(in )                    ::  ind_sens    ! INDICATEUR DE SENS DU PARCOURS (1:DIRECT -1:RETROGRADE)
real(pm_reel), intent(out)              ::  gom         ! LONGITUDE DU NOEUD ASCENDANT
real(pm_reel), intent(out)              ::  inc         ! INCLINAISON DU PLAN DE L'ORBITE
real(pm_reel), intent(out)              ::  rga         ! ANGLE ENTRE LE NOEUD ASCENDANT ET LE PREMIER POINT
real(pm_reel), intent(out)              ::  rgb         ! ANGLE ENTRE LE NOEUD ASCENDANT ET LE SECOND POINT
integer ,intent(out)                    ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mii_pb_parg0
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_interpla_internes.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_interpla_internes
