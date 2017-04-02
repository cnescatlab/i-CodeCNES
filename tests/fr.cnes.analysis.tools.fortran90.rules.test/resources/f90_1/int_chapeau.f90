module int_chapeau
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_chapeau.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mx_rep ( tbv_rep_in, bv_pla_in, date_in, prec_nuta_tsid_in, pos_in, vit_in, &
                    tbv_rep_out, bv_pla_out, date_out, prec_nuta_tsid_out, pos_out, vit_out, code_retour, &
                    vit_rot_in, vit_rot_out, obliquite_in, obliquite_out, pole_in, pole_out, &
                    long_ref_in, long_ref_out, val_date_in, delta_tu1_in, delta_tai_in, &
                    val_date_out, delta_tu1_out, delta_tai_out, eps_date, def_topo_in, def_topo_out,&
                    pole_tsid_planete_in, pole_tsid_planete_out, jacob )

     use mslib
     use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                       intent(in)           ::  tbv_rep_in          ! type de base vectorielle pour le repere en entree
integer,                       intent(in)           ::  bv_pla_in           ! planete du repere en entree
integer,                       intent(in)           ::  date_in             ! date de definition du repere en entree
integer,                       intent(in)           ::  prec_nuta_tsid_in   ! Modele pour precession, nutation et tps sid en entree
real(pm_reel), dimension(3),   intent(in)           ::  pos_in              ! position cartesienne en entree (m)
real(pm_reel), dimension(3),   intent(in)           ::  vit_in              ! vitesse cartesienne en entree (m/s)
integer,                       intent(in)           ::  tbv_rep_out         ! type de base vectorielle pour le repere en sortie
integer,                       intent(in)           ::  bv_pla_out          ! planete du repere en sortie
integer,                       intent(in)           ::  date_out            ! date de definition du repere en sortie
integer,                       intent(in)           ::  prec_nuta_tsid_out  ! Modele pour precession, nutation et tps sid en sortie
real(pm_reel), dimension(3),   intent(out)          ::  pos_out             ! position cartesienne en sortie (m)
real(pm_reel), dimension(3),   intent(out)          ::  vit_out             ! vitesse cartesienne en sortie (m/s)
type(tm_code_retour),          intent(out)          ::  code_retour         ! code retour
real(pm_reel),                 intent(in), optional ::  vit_rot_in          ! vitesse de rotation de la planete en entree (rad/s)
real(pm_reel),                 intent(in), optional ::  vit_rot_out         ! vitesse de rotation de la planete en sortie (rad/s)
real(pm_reel),                 intent(in), optional ::  obliquite_in        ! obliquite en entree (rad)
real(pm_reel),                 intent(in), optional ::  obliquite_out       ! obliquite en sortie (rad)
type(tm_pole_uv),              intent(in), optional ::  pole_in             ! coord du pole vrai a la date "date_in" (rad)
type(tm_pole_uv),              intent(in), optional ::  pole_out            ! coord du pole vrai a la date "date_out" (rad)
real(pm_reel),                 intent(in), optional ::  long_ref_in         ! longitude de ref du repere en entree (rad)
real(pm_reel),                 intent(in), optional ::  long_ref_out        ! longitude de ref du repere en sortie (rad)
type(tm_jour_sec),             intent(in), optional ::  val_date_in         ! date de def du repere d'entree (JJCNES)
real(pm_reel),                 intent(in), optional ::  delta_tu1_in        ! ecart entre tps utilisateur et tps TU1 a "date_in"
real(pm_reel),                 intent(in), optional ::  delta_tai_in        ! ecart entre tps utilisateur et tps TAI a "date_in"
type(tm_jour_sec),             intent(in), optional ::  val_date_out        ! date de def du repere de sortie (JJCNES)
real(pm_reel),                 intent(in), optional ::  delta_tu1_out       ! ecart entre tps utilisateur et tps TU1 a "date_out"    
real(pm_reel),                 intent(in), optional ::  delta_tai_out       ! ecart entre tps utilisateur et tps TAI a "date_out"    
real(pm_reel),                 intent(in), optional ::  eps_date            ! epsilon pour comparaison de 2 dates
type(tm_def_topo),             intent(in), optional ::  def_topo_in         ! def du repere topocentrique en entree
type(tm_def_topo),             intent(in), optional ::  def_topo_out        ! def du repere topocentrique en sortie
type(tm_pole_tsid_planete),    intent(in), optional ::  pole_tsid_planete_in! def du pole de rot et du merid origine en entree
type(tm_pole_tsid_planete),    intent(in), optional ::  pole_tsid_planete_out! def du pole de rot et du merid origine en sortie
real(pm_reel),dimension(6,6),  intent(out),optional ::  jacob               ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mx_rep
     subroutine mx_var (typ_var_in, coord_in, typ_var_out, &
                        coord_out, code_retour, &
                        mu, ellips, &
                        jacob) 

        use mslib
        use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                     intent(in)  :: typ_var_in         ! Type de variables utilisees en entree
real(pm_reel), dimension(6), intent(in)  :: coord_in           ! coordonnees en entree
integer,                     intent(in)  :: typ_var_out        ! Type de variables utilisees en sortie

real(pm_reel), dimension(6), intent(out) :: coord_out          ! coordonnees en sortie
type(tm_code_retour),        intent(out) :: code_retour

real(pm_reel),                 intent(in), optional :: mu      ! Constante de la gravitation 
type(tm_ellipsoide),           intent(in), optional :: ellips  ! Rayon equatorial et aplatissement de l'ellipsoide
real(pm_reel), dimension(6,6), intent(out),optional :: jacob   ! Jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mx_var
end interface

end module int_chapeau
