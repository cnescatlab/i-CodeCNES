module int_ephemerides

! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème S
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_ephemerides.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine ms_pos_soleil_lune (date, dir_sol, dist_sol, dir_lune, dist_lune, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)                        ::  date      ! date (JJCNES)
real(pm_reel), dimension(3), intent(out)             ::  dir_sol   ! cosinus directeur du Soleil dans Veis
real(pm_reel), intent(out)                           ::  dist_sol  ! distance Terre-Soleil
real(pm_reel), dimension(3), intent(out)             ::  dir_lune  ! cosinus directeur de la Lune dans Veis
real(pm_reel), intent(out)                           ::  dist_lune ! distance Terre-Lune 
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine ms_pos_soleil_lune
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_ephemerides.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_ephemerides
