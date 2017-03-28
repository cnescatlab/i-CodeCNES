module int_dates_internes
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des routines internes du thème D
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement en interne
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_dates_internes.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine mdi_som_diff_joursec(joursec1, joursec2, retour, joursec_somme, joursec_diff)

       use type_mslib
       use precision_mslib


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),    intent(in)            :: joursec1         ! quantite exprimee en jours et secondes
type(tm_jour_sec),    intent(in)            :: joursec2         ! quantite exprimee en jours et secondes
integer          ,    intent(out)           :: retour           ! code retour de la routine
type(tm_jour_sec),    intent(out), optional :: joursec_somme    ! quantite, somme des deux quantites en entree, exprimee en jours et secondes dans le jour
type(tm_jour_sec),    intent(out), optional :: joursec_diff     ! quantite, difference des deux quantites en entree (joursec1-joursec2), exprimee en jours et secondes dans le jour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mdi_som_diff_joursec
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_dates_internes.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_dates_internes
