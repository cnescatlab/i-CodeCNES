subroutine md_ech_temps (ech_date_in, date_in, ech_date_out, delta_t, date_out, code_retour, &
                         nb_saut_tuc, date_saut_tuc, delta_saut_tuc)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Effectuer un changement d'echelle de temps entre les echelles TE, TAI, TUC 
! ===  Les transformations possibles sont : TE<->TAI, TE<->TUC, TUC<->TAI
!                                           TE<->TE, TAI<->TAI, TUC<->TUC
!      Echelle TE  : Temps des Ephemerides
!      Echelle TAI : Temps Atomique Internationnal
!      Echelle TUC : Temps Universel Coordonne
!
! Note d'utilisation:  Pour les dates posterieures a la derniere estimation du saut du TUC
! ==================   on considere que la valeur de ce saut est constante et egale a
!                      cette derniere (jusqu'a mise a jour du fichier sauts du TUC).
!                      
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 10/2002 - Realisation: Michel Lagreca/Bruno Revelin
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use valeur_code_retour_mspro
use numero_routine_mspro
use surcharge_egal_mspro
use int_dates_internes_mspro, only : mdi_ech_temps_te_tai
use int_dates_internes_mspro, only : mdi_ech_temps_tai_tuc

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,              intent(in)  :: ech_date_in    ! echelle de temps utilisee pour date_in
type(tm_jour_sec),    intent(in)  :: date_in        ! date en entree
integer,              intent(in)  :: ech_date_out   ! echelle de temps utilisee pour date_out
real(pm_reel),        intent(out) :: delta_t        ! ecart entre date_in et date_out
type(tm_jour_sec),    intent(out) :: date_out       ! date en sortie
type(tm_code_retour), intent(out) :: code_retour
integer,                                          intent(in), optional :: nb_saut_tuc    ! nombre de sauts TUC
type(tm_jour_sec), dimension(pm_nb_max_saut_tuc), intent(in), optional :: date_saut_tuc  ! dates des sauts TUC
real(pm_reel),     dimension(pm_nb_max_saut_tuc), intent(in), optional :: delta_saut_tuc ! ecarts (TAI-TUC) pour ces dates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: retour

type(tm_jour_sec) :: date_inter                   ! date intermediaire avant normalisation
real(pm_reel)     :: delta_t_te_tai               ! delta_t intermediaire entre echelle TE et TAI
real(pm_reel)     :: delta_t_tai_tuc              ! delta_t intermediaire entre echelle TAI et TUC
integer           :: sens_transfo                 ! indicateur de sens de transformation
real(pm_reel)     :: date_1saut_tuc_jjcnes        ! date julienne du 1er saut du TUC
real(pm_reel)     :: date_in_jjcnes               ! date julienne correspondant a date_in
real(pm_reel)     :: delta_t_tuc_tai              ! ecarts temporaires entre echelles de datation TUC-TAI
real(pm_reel)     :: delta_t_tai_te               ! ecarts temporaires entre echelles de datation TAI-TE
type(tm_code_retour):: code_retour_local          ! code retour local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO md_ech_temps.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! verification de la coherence des arguments obligatoires d'entree
! ................................................................

!   cas de ech_date_in : cette echelle doit appartenir a l'ensemble
!   [pm_TE, pm_TAI, pm_TUC]

if ((ech_date_in /= pm_TE) .and. (ech_date_in /= pm_TAI) .and. (ech_date_in /= pm_TUC)) then
   code_retour%valeur = pm_err_ind_ech_temps
   go to 6000
end if

!   cas de ech_date_out : cette echelle doit appartenir a l'ensemble
!   [pm_TE, pm_TAI, pm_TUC]

if ((ech_date_out /= pm_TE) .and. (ech_date_out /= pm_TAI) .and. (ech_date_out /= pm_TUC)) then
   code_retour%valeur = pm_err_ind_ech_temps
   go to 6000
end if

! verification de la coherence des arguments optionnels d'entree
! ..............................................................

!   dans le cas ou ech_date_in (idem pour ech_date_out) vaut pm_TUC, il est necessaire
!   d'avoir passe en arguments optionnels les quantites suivantes :
!   nb_saut_tuc, date_saut_tuc et delta_saut_tuc. Avec nb_saut_tuc >0

if((ech_date_in == pm_TUC) .or. (ech_date_out == pm_TUC)) then
   if ((.not. present(nb_saut_tuc)) .or. (.not. present(date_saut_tuc)) .or. &
      (.not. present(delta_saut_tuc))) then
      code_retour%valeur = pm_err_para_opt_abs
      go to 6000
   end if
   if (nb_saut_tuc == 0) then
      code_retour%valeur = pm_err_nb_saut_tuc_nul
      go to 6000
   end if
end if

!   dans le cas ou ech_date_in (idem pour ech_date_out) vaut pm_TUC, il est necessaire
!   de verifier que la date date_in soit posterieure a la date du premier saut de TUC

if((ech_date_in == pm_TUC) .or. (ech_date_out == pm_TUC)) then
   call md_joursec_jourfrac (date_in, date_in_jjcnes, code_retour_local)
   call md_joursec_jourfrac (date_saut_tuc(1), date_1saut_tuc_jjcnes, code_retour_local)
   if (date_in_jjcnes < date_1saut_tuc_jjcnes) then
     code_retour%valeur = pm_err_date_inf_1saut_tuc
     go to 6000
   end if 
end if

! traitement des cas de transformation a l'identique (TE <-> TE, TAI <-> TAI, TUC <-> TUC)
! dans ce cas, delta_t = 0. et date_out = date_in
! ........................................................................................

if ( ech_date_in == ech_date_out ) then
    delta_t  = 0._pm_reel
    date_out = date_in     ! affectation par surcharge du signe =
end if

! traitement des cas de transformation suivantes :
! ech_date_in = TE  : TE <-> TAI ou TE <-> TUC
! ech_date_in = TAI : TAI <-> TE ou TAI <-> TUC
! ech_date_in = TUC : TUC <-> TAI ou TUC <-> TE
!
! Les codes retour des transformations mdi_ech_temps_te_tai et
! mdi_ech_temps_tai_tuc etant toujours a 0 on ne fait pas de test
! ...............................................................

echelle_in: select case (ech_date_in)

    case (pm_TE) echelle_in

echelle_out_tai_tuc:  select case (ech_date_out)

         case (pm_TAI) echelle_out_tai_tuc

!           passage TE->TAI
            sens_transfo = pm_i_te_tai
            call mdi_ech_temps_te_tai(sens_transfo, date_in, date_out, delta_t, retour)
            
         case (pm_TUC) echelle_out_tai_tuc

!           passage TE-> TAI
            sens_transfo = pm_i_te_tai
            call mdi_ech_temps_te_tai(sens_transfo, date_in, date_inter, delta_t_te_tai, retour)
!           passage TAI->TUC
            sens_transfo = pm_i_tai_tuc
            call mdi_ech_temps_tai_tuc(sens_transfo, date_inter, date_out, delta_t_tai_tuc, retour,  &
                                       nb_saut_tuc = nb_saut_tuc, date_saut_tuc = date_saut_tuc, &
                                       delta_saut_tuc = delta_saut_tuc)  

            delta_t = delta_t_te_tai + delta_t_tai_tuc

      end select echelle_out_tai_tuc

    case (pm_TAI) echelle_in
  
echelle_out_te_tuc:  select case (ech_date_out)

         case (pm_TE) echelle_out_te_tuc

!           passage TAI->TE
            sens_transfo = pm_i_tai_te
            call mdi_ech_temps_te_tai(sens_transfo, date_in, date_out, delta_t, retour)

         case (pm_TUC) echelle_out_te_tuc

!           passage TAI->TUC
            sens_transfo = pm_i_tai_tuc
            call mdi_ech_temps_tai_tuc(sens_transfo, date_in, date_out, delta_t, retour,  &
                                       nb_saut_tuc = nb_saut_tuc, date_saut_tuc = date_saut_tuc, &
                                       delta_saut_tuc = delta_saut_tuc)  

      end select echelle_out_te_tuc

    case (pm_TUC) echelle_in

echelle_out_tai_te:  select case (ech_date_out)

         case (pm_TAI) echelle_out_tai_te

!           passage TUC->TAI
            sens_transfo = pm_i_tuc_tai
            call mdi_ech_temps_tai_tuc(sens_transfo, date_in, date_out, delta_t, retour,  &
                                       nb_saut_tuc = nb_saut_tuc, date_saut_tuc = date_saut_tuc, &
                                       delta_saut_tuc = delta_saut_tuc)  

         case (pm_TE) echelle_out_tai_te

!           passage TUC->TAI
            sens_transfo = pm_i_tuc_tai
            call mdi_ech_temps_tai_tuc(sens_transfo, date_in, date_inter, delta_t_tuc_tai, retour,  &
                                       nb_saut_tuc = nb_saut_tuc, date_saut_tuc = date_saut_tuc, &
                                       delta_saut_tuc = delta_saut_tuc)  
!           passage TAI->TE
            sens_transfo = pm_i_tai_te
            call mdi_ech_temps_te_tai(sens_transfo, date_inter, date_out, delta_t_tai_te, retour)

            delta_t = delta_t_tuc_tai + delta_t_tai_te

      end select echelle_out_tai_te

end select echelle_in

6000 continue

code_retour%routine = pm_num_md_ech_temps
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine md_ech_temps
