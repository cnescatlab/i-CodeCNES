module int_chapeau_internes
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_chapeau_internes.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mxi_construit_fils ( nb_noeuds, arbre, retour )

       use mslib
       use type_themeX_interne_mspro     ! inclus use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                     ::  nb_noeuds         ! nb_noeuds
type(tm_i_noeud), dimension(:), intent(inout)      ::  arbre  ! arbre
integer, intent(out)                    ::  retour            ! retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_construit_fils
     subroutine mxi_def_parcours ( noeud_initial, noeud_final, arbre, liste_noeuds, nombre_transfo, retour )

       use mslib
       use type_themeX_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                        intent(in)      ::  noeud_initial   ! noeud_initial
integer,                        intent(in)      ::  noeud_final     ! noeud_final
type(tm_i_noeud), dimension(:), intent(in)      ::  arbre           ! arbre
integer,dimension(:),           intent(out)     ::  liste_noeuds    ! liste_noeuds
integer,                        intent(out)     ::  nombre_transfo  ! nombre_transfo
integer,                        intent(out)     ::  retour          ! retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_def_parcours
     subroutine mxi_rep_cons_arbre ( arbre, retour )

       use mslib
       use type_themeX_interne_mspro     ! inclus use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_i_noeud), dimension(:), intent(out)                       ::  arbre   ! arbre
integer, intent(out)                                              ::  retour  ! retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_rep_cons_arbre
     subroutine mxi_rep_ench_transfo (  liste_reperes, nombre_transfo, para_in, para_out,  planete_in, planete_out, &
        date_in, date_out, modele_in, modele_out, pos_in, vit_in, pos_out, vit_out, retour, message, jacob )

       use mslib
       use type_themeX_interne_mspro      ! inclus le use  type_mspro
       use parametre_themeX_interne_mspro 



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, dimension(pm_i_nombre_repere), intent(in)  :: liste_reperes      ! liste de reperes pour la transformation
integer,                                intent(in)  :: nombre_transfo     ! nb de changements de reperes consecutifs
type(tm_i_rep_para_opt),             intent(inout)  :: para_in            ! liste des parametres optionnels d'entree
type(tm_i_rep_para_opt),             intent(inout)  :: para_out           ! liste des parametres optionnels de sortie
integer,                                intent(in)  :: planete_in         ! planete en entree
integer,                                intent(in)  :: planete_out        ! planete en sortie
integer,                                intent(in)  :: date_in            ! type de date en entree
integer,                                intent(in)  :: date_out           ! type de date en sortie
integer,                                intent(in)  :: modele_in          ! modele de nutation en entree
integer,                                intent(in)  :: modele_out         ! modele de nutation en sortie
real(pm_reel), dimension(3),            intent(in)  :: pos_in             ! vecteur position en entree
real(pm_reel), dimension(3),            intent(in)  :: vit_in             ! vecteur vitesse en entree
real(pm_reel), dimension(3),            intent(out) :: pos_out            ! vecteur position en sortie
real(pm_reel), dimension(3),            intent(out) :: vit_out            ! vecteur vitesse en sortie
integer,                                intent(out) :: retour             ! code retour
character(len=pm_message),              intent(out) :: message            ! message retour
real(pm_reel), dimension(6,6),          intent(out), optional :: jacob    ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_rep_ench_transfo
     subroutine mxi_transfo_identique ( taille, donnees_in, donnees_out, retour, jacob )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,  intent(in)                                 :: taille      ! du vecteur des donnees
real(pm_reel), dimension(:), intent(in)              :: donnees_in  ! donnees en entree
real(pm_reel), dimension(:), intent(out)             :: donnees_out ! donnees en sortie

integer, intent(out)                                 :: retour
real(pm_reel), dimension(:,:), intent(out), optional :: jacob       ! jacobienne de la transformation = identite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_transfo_identique
     subroutine mxi_var_cons_arbre ( arbre, retour )

       use mslib
       use type_themeX_interne_mspro     ! inclus use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_i_noeud),dimension(:), intent(out) :: arbre ! arbre
integer, intent(out) :: retour 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_var_cons_arbre

        subroutine mxi_var_ench_transfo(liste_variables, nombre_transfo, para_opt, &
                   coord_in,                                                      &
                   coord_out, message, retour, jacob)
  
        use mslib
        use parametre_themeX_interne_mspro
        use type_themeX_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer, dimension(pm_i_nombre_variable),intent(in)   :: liste_variables   ! liste des transformations de variables 
integer,                                  intent(in)  :: nombre_transfo    ! nombre de transformations
type(tm_i_var_para_opt),                  intent(in)  :: para_opt          ! structure des parametres en entree
real(pm_reel), dimension(6),              intent(in)  :: coord_in          ! coordonnees en entree
real(pm_reel), dimension(6),              intent(out) :: coord_out         ! coordonnees en sortie
character(len=pm_message),                intent(out) :: message           ! Message associe au code de retour
integer,                                  intent(out) :: retour                                 

real(pm_reel), dimension(6,6), intent(out), optional :: jacob             ! jacobienne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_var_ench_transfo

     subroutine mxi_var_typ2noeud ( typ_var, var_int, retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer, intent(in)   :: typ_var   ! type de variable utilisateur
integer, intent(out)  :: var_int   ! type de variable interne
integer, intent(out)  :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_var_typ2noeud
     subroutine mxi_var_typ_atyp ( typ_var_in, typ_var_out,  &
                                   atypique, retour )
       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)   :: typ_var_in   ! type de variable utilisateur initiale 
integer, intent(in)   :: typ_var_out  ! type de variable utilisateur finale    
logical, intent(out)  :: atypique     ! transformation typique ou atypique
integer, intent(out)  :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_var_typ_atyp
     subroutine mxi_verinit_dates ( indic_date, complement_message, para, retour, message )

     use mslib
     use type_themeX_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                   intent(in)                       :: indic_date              ! indic_date
character(len=3),          intent(in)                       :: complement_message      ! complement_message
type(tm_i_rep_para_opt),       intent(inout)                    :: para                    ! para
integer,                   intent(out)                      :: retour                  ! retour
character(len=pm_message), intent(out)                      :: message                 ! message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mxi_verinit_dates
end interface

end module int_chapeau_internes
