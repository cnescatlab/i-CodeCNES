module int_dates_internes_mspro
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_dates_internes_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mdi_ech_temps_tai_tuc ( sens, date_in, date_out, delta_t_tai_tuc, retour, &
          nb_saut_tuc, date_saut_tuc, delta_saut_tuc )

       use parametre_mspro
       use type_mspro
       use mslib



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


     end subroutine mdi_ech_temps_tai_tuc
     subroutine mdi_ech_temps_te_tai (sens, date_in, date_out, delta_t_te_tai, retour)

       use parametre_mspro
       use type_mspro
       use mslib

 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,            intent(in) :: sens           ! sens de la transformation (0: TE->TAI, 1: TAI->TE)
type(tm_jour_sec),  intent(in) :: date_in        ! date en entree
type(tm_jour_sec), intent(out) :: date_out       ! date en sortie
real(pm_reel),     intent(out) :: delta_t_te_tai ! ecart (TE-TAI)
integer,           intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mdi_ech_temps_te_tai
     subroutine mdi_lire_don_saut_tuc (unific, nb_saut_tuc, date_saut_tuc, delta_saut_tuc, retour )

       use parametre_mspro
       use type_mspro
       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)  :: unific          ! Unite logique associee au fichier sauts de TUC
integer, intent(out) :: nb_saut_tuc     ! Nombre de sauts TUC                       
type(tm_jour_sec), dimension(pm_nb_max_saut_tuc), intent(out) :: date_saut_tuc  ! dates des sauts TUC
real(pm_reel),     dimension(pm_nb_max_saut_tuc), intent(out) :: delta_saut_tuc ! ecarts (TAI-TUC) pour ces dates
integer, intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mdi_lire_don_saut_tuc
end interface

end module int_dates_internes_mspro
