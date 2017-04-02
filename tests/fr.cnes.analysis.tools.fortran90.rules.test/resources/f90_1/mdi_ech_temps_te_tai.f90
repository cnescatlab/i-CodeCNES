subroutine mdi_ech_temps_te_tai (sens, date_in, date_out, delta_t_te_tai, retour)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Transformation d'une date entre les echelles de temps
! ===  TE (Temps des Ephemerides) et TAI (Temps Atomique Internationnal)
!
! Note d'utilisation:  L'entree sens est un entier correspondant au
! ==================   sens de la transformation, qui doit etre initialise
!                      par la routine appelante (aucun test n'est effectue ici).
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 10/2002 - Realisation: Michel Lagreca
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use parametre_interne_mspro

! Declarations
! ============
implicit none

 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,            intent(in) :: sens           ! sens de la transformation (0: TE->TAI, 1: TAI->TE)
type(tm_jour_sec),  intent(in) :: date_in        ! date en entree
type(tm_jour_sec), intent(out) :: date_out       ! date en sortie
real(pm_reel),     intent(out) :: delta_t_te_tai ! ecart (TE-TAI)
integer,           intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), parameter :: delta_sec_TE_TAI = 32.184_pm_reel ! ecart en secondes entre les echelles TE et TAI
type(tm_jour_sec) :: date_inter       ! date intermediaire avant normalisation
type(tm_code_retour) :: code_retour_local   ! code de retour de md_joursec_norme

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mdi_ech_temps_te_tai.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! decalage des echelles de datation TE et TAI
! .................................................

date_inter%jour = date_in%jour
if(sens == pm_i_te_tai) then
   date_inter%sec  = date_in%sec - delta_sec_TE_TAI
   delta_t_te_tai = - delta_sec_TE_TAI
else
   date_inter%sec  = date_in%sec + delta_sec_TE_TAI
   delta_t_te_tai = + delta_sec_TE_TAI
end if

! normalisation de la date obtenue apres changement d'echelle
! (il n'y a pas de code de retour a tester ici)
! ...........................................................

call md_joursec_norme(date_inter, date_out, code_retour_local)

end subroutine mdi_ech_temps_te_tai
