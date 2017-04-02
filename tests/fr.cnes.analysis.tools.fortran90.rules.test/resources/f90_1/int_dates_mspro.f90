module int_dates_mspro
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_dates_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine md_anjour_calend (an,jour_an, mois,jour_mois, code_retour)
 
       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
integer, intent(in)                      :: an              ! annee
integer, intent(in)                      :: jour_an         ! numero du jour dans l'annee
integer, intent(out)                     :: mois            ! numero du mois dans l'annee
integer, intent(out)                     :: jour_mois       ! numero du jour dans le mois
type(tm_code_retour), intent(out)        :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine md_anjour_calend
     subroutine md_calend_anjour (an, mois,jour_mois,jour_an, code_retour)
 
       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
integer, intent(in)                      :: an             ! annee
integer, intent(in)                      :: mois           ! numero du mois dans l'annee
integer, intent(in)                      :: jour_mois      ! numero du jour dans le mois


integer, intent(out)                     :: jour_an        ! numero du jour dans l'annee
type(tm_code_retour), intent(out)        :: code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine md_calend_anjour
     subroutine md_ech_temps (ech_date_in, date_in, ech_date_out, delta_t, date_out, code_retour, &
                              nb_saut_tuc, date_saut_tuc, delta_saut_tuc)

       use parametre_mspro
       use type_mspro
       use mslib



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


     end subroutine md_ech_temps
     subroutine md_lire_saut_tuc (fichier, nb_saut_tuc, date_saut_tuc, delta_saut_tuc, code_retour )

       use parametre_mspro
       use type_mspro
       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

character(len=*), intent(in)  :: fichier         ! nom du fichier saut de TUC (chemin+nom)
integer,          intent(out) :: nb_saut_tuc     ! nombre de sauts TUC                       
type(tm_jour_sec), dimension(pm_nb_max_saut_tuc), intent(out) :: date_saut_tuc  ! dates des sauts TUC
real(pm_reel),     dimension(pm_nb_max_saut_tuc), intent(out) :: delta_saut_tuc ! (TAI-TUC) pour ces dates
type(tm_code_retour), intent(out) :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine md_lire_saut_tuc
end interface

end module int_dates_mspro
