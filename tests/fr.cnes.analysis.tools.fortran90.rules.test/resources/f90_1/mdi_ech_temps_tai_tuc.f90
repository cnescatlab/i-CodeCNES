subroutine mdi_ech_temps_tai_tuc (sens, date_in, date_out, delta_t_tai_tuc, retour, &
     nb_saut_tuc,date_saut_tuc, delta_saut_tuc)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Transformation d'une date entre les echelles de temps
! ===  TAI (Temps Atomique Internationnal) et TUC (Temps Universel Coordonne)
!
! Note d'utilisation:  L'entree sens est un entier correspondant au
! ==================   sens de la transformation, qui doit etre initialise
!                      par la routine appelante (aucun test n'est effectue ici)
!                      Les dates date_saut_tuc sont donnees dans l'echelle TUC
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 10/2002 - Realisation: Michel Lagreca/Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!                   FA-ID 624 : initialisation de la variable ecart à 0
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

use parametre_mspro
use parametre_interne_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,            intent(in) :: sens            ! sens de la transformation (0: TAI->TUC, 1: TUC->TAI)
type(tm_jour_sec),  intent(in) :: date_in         ! date en entree
type(tm_jour_sec), intent(out) :: date_out        ! date en sortie
real(pm_reel),     intent(out) :: delta_t_tai_tuc ! ecart (TAI-TUC) a l'origine
integer,           intent(out) :: retour
integer,                                          intent(in), optional :: nb_saut_tuc    ! nombre de sauts TUC
type(tm_jour_sec), dimension(pm_nb_max_saut_tuc), intent(in), optional :: date_saut_tuc  ! dates des sauts TUC
real(pm_reel),     dimension(pm_nb_max_saut_tuc), intent(in), optional :: delta_saut_tuc ! ecarts (TAI-TUC) pour ces dates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), parameter :: nb_sec_par_jour = 86400._pm_reel  ! nb de secondes par jour 
real(pm_reel), parameter :: jj_fin_arbitraire = 99999._pm_reel ! borne sup arbitraire de validite de delta_saut_tuc
! on condidere qu'entre date_saut_tuc(nb_saut_tuc) et cette date arbitraire, 
! la valeur du saut delta_saut_tuc(nb_saut_tuc) est toujours valable.

real(pm_reel) :: jjfrac_saut_deb  ! date Julienne de debut d'une plage de saut de TUC
real(pm_reel) :: jjfrac_saut_fin  ! date Julienne de fin d'une plage de saut de TUC
type(tm_jour_sec) :: date_inter   ! date intermediaire avant normalisation
real(pm_reel) :: jjfrac_in        ! date Julienne correspondant a date_in
real(pm_reel) :: ecart            ! ecart correspondant a une plage de dates
integer :: i,j                    ! indice de boucle
integer :: imax                   ! nombre d'iterations
logical :: trouve                 ! indicateur d'obtention d'une plage de saut correspondant a date_in
type(tm_code_retour) :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mdi_ech_temps_tai_tuc.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ...........................................
retour = pm_OK

! Initialisation de l'écart 
ecart = 0._PM_REEL

! recherche dans le tableau de la valeur du saut du TUC correspondant
! a la date date_in
! ....................................................................

call md_joursec_jourfrac (date_in, jjfrac_in, code_retour_local)

! boucle de parcours du tableau date_saut_tuc a partir de la fin afin de
! trouver la plage de dates encadrant la date date_in.
! On considere que la derniere plage couvre [date_in(nb_saut_tuc), 99999]
! Si TAI->TUC, on parcourt la boucle 2 fois.

if (sens == pm_i_tai_tuc) then
   imax = 2
else
   imax = 1
end if

do i=1,imax
   j = nb_saut_tuc
   trouve = pm_i_non

   do while ((j /= 0) .and. (.not. trouve))
      call md_joursec_jourfrac (date_saut_tuc(j), jjfrac_saut_deb, code_retour_local)
      if (j == nb_saut_tuc) then
         jjfrac_saut_fin = jj_fin_arbitraire
      else
         call md_joursec_jourfrac (date_saut_tuc(j+1), jjfrac_saut_fin, code_retour_local)
      end if
      if ((jjfrac_in >= jjfrac_saut_deb) .and. (jjfrac_in < jjfrac_saut_fin)) then
         trouve = pm_i_oui
         ecart = delta_saut_tuc(j)
      else
         j = j - 1
      end if
   end do
   jjfrac_in = jjfrac_in - ecart/nb_sec_par_jour
end do

! changement d'echelle de datation en fonction du sens demande
!                      sens = pm_i_tai_tuc : transformation TAI -> TUC 
!                      La relation est temp_TUC  = temps_TAI - ecart_TAI_TUC
!                      sens = pm_i_tuc_tai : transformation TUC -> TAI
!                      La relation est temp_TAI  = temps_TUC + ecart_TAI_TUC
! .................................................................

date_inter%jour = date_in%jour
if(sens == pm_i_tai_tuc) then
   delta_t_tai_tuc = - ecart
   date_inter%sec  = date_in%sec - ecart
else
   delta_t_tai_tuc = + ecart
   date_inter%sec  = date_in%sec + ecart
end if

! normalisation de la date obtenue apres changement d'echelle
! (il n'y a pas de code de retour a tester ici)
! ...........................................................

call md_joursec_norme(date_inter, date_out, code_retour_local)

end subroutine mdi_ech_temps_tai_tuc
