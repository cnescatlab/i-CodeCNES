module type_themeX_interne_mspro

! (C) Copyright CNES - MSPRO - 2002-2003

!************************************************************************
!
! But:  Definition des types derives internes du theme X de la MSPRO. 
! ===
!
! Note d'utilisation: Ce module n'est pas accessible a l'utilisateur
! ==================  Il n'est accessible qu'en interne de la MSPRO
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 11/2002 - Realisation: Michel Lagreca/Bruno Revelin)
!   + Version 4.0 (DE globale 9) : ajout des reperes interplanetaires dans le theme X
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

use mslib

use parametre_themeX_interne_mspro

use type_mspro

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: type_themeX_interne_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


! types derives internes pour la surcharge de l'affectation
! ---------------------------------------------------------

type tm_i_car_pos_vit   ! type coordonnees cartesiennes en position et vitesse

   sequence
   real(pm_reel),dimension(3) :: pos  ! position en cartesien
   real(pm_reel),dimension(3) :: vit  ! vitesse en cartesien

end type tm_i_car_pos_vit

type tm_i_sgd_pos_vit   ! type coordonnees site/gisement/distance en position et vitesse

   sequence
   type(tm_sgd) :: pos  ! position en sgd
   type(tm_sgd) :: vit  ! vitesse en sgd

end type tm_i_sgd_pos_vit

type tm_i_gps_ard_pos_vit   ! type coordonnees GPS ARD en position et vitesse

   sequence
   type(tm_geodesique)  :: pos   ! position en coordonnees geodesiques
   type(tm_vit_gps_ard) :: vit   ! vitesse GPS ARD

end type tm_i_gps_ard_pos_vit

type tm_i_meca_vol_pos_vit  ! type coordonnees mecanique du vol en position et vitesse

   sequence
   type(tm_geodesique)   :: pos  ! position en coordonnees geodesiques
   type(tm_vit_meca_vol) :: vit  ! vitesse mecanique du vol

end type tm_i_meca_vol_pos_vit

type tm_i_geoc_pos_vit      ! type coordonnees geocentriques en position et vitesse

   sequence
   type(tm_geocentrique) :: pos  ! position en coordonnees geocentriques
   type(tm_geocentrique) :: vit  ! vitesse en coordonnees geocentriques

end type tm_i_geoc_pos_vit

! types relatifs aux parametres optionnels de mx_rep
! --------------------------------------------------

type tm_i_vit_rot   ! vitesse de rotation de la planete
   sequence
   logical          :: presence
   logical          :: superflu
   real(pm_reel)    :: valeur
end type tm_i_vit_rot

type tm_i_obliquite ! obliquite
   sequence
   logical          :: presence
   logical          :: superflu
   real(pm_reel)    :: valeur
end type tm_i_obliquite

type tm_i_pole      ! coordonnees (u,v) du pole vrai
   sequence
   logical          :: presence
   logical          :: superflu
   type(tm_pole_uv) :: valeur
end type tm_i_pole

type tm_i_long_ref  ! longitude de reference du repere planetocentrique de reference ou
                    ! du repere planetocentrique de reference quasi inertiel de la date
   sequence
   logical          :: presence
   logical          :: superflu
   real(pm_reel)    :: valeur
   logical          :: long_nul     ! definit la nature du repere (longitude nulle ou quelconque)
end type tm_i_long_ref

type tm_i_val_date  ! valeur de la date de definition du repere ou
                    ! du bulletin dans le cas d'un repere non inertiel
   sequence
   logical          :: presence
   logical          :: superflu
   type(tm_jour_sec):: valeur
end type tm_i_val_date

type tm_i_delta_tu1 ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisateur
   sequence
   logical          :: presence
   logical          :: superflu
   real(pm_reel)    :: valeur
end type tm_i_delta_tu1

type tm_i_delta_tai ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisateur
   sequence
   logical          :: presence
   logical          :: superflu
   real(pm_reel)    :: valeur
end type tm_i_delta_tai

type tm_i_eps_date  ! epsilon utilise pour la comparaison de deux dates
   sequence
   logical          :: presence
   logical          :: superflu
   real(pm_reel)    :: valeur
end type tm_i_eps_date

type tm_i_def_topo  ! definition du repere topocentrique
   sequence
   logical          :: presence
   logical          :: superflu
   type(tm_def_topo):: valeur
   logical          :: axe_nul     ! definit la nature du repere topocentrique (Nord ou quelconque)
end type tm_i_def_topo

type tm_i_pole_tsid_planete  ! definition du pole de rotation et du meridien origine d'une planete
   sequence
   logical                   :: presence
   logical                   :: superflu
   type(tm_pole_tsid_planete):: valeur
end type tm_i_pole_tsid_planete

! types relatifs aux parametres optionnels de mx_var
! --------------------------------------------------

type tm_i_mu        ! constante de la gravitation
   sequence
   logical          :: presence
   real(pm_reel)    :: valeur
end type tm_i_mu

type tm_i_ellips    ! rayon equatorial et aplatissement de l'ellipsoide
   sequence
   logical             :: presence
   type(tm_ellipsoide) :: valeur
end type tm_i_ellips

! ensemble des informations liees aux parametres optionnels d'entree de mx_rep
! ----------------------------------------------------------------------------

type tm_i_rep_para_opt ! contient toutes les informations liees aux parametres optionnels de mx_rep
                   ! utilisables aussi bien pour les valeurs en "in" et en "out"

   sequence
   type(tm_i_vit_rot)   ::  vit_rot
   type(tm_i_obliquite) ::  obliquite
   type(tm_i_pole)      ::  pole
   type(tm_i_long_ref)  ::  long_ref
   type(tm_i_val_date)  ::  val_date
   type(tm_i_delta_tu1) ::  delta_tu1
   type(tm_i_delta_tai) ::  delta_tai
   type(tm_i_eps_date)  ::  eps_date
   type(tm_i_def_topo)  ::  def_topo
   type(tm_i_pole_tsid_planete)  ::  pole_tsid_planete

end type tm_i_rep_para_opt

! ensemble des informations liees aux parametres optionnels d'entree de mx_var
! ----------------------------------------------------------------------------

type tm_i_var_para_opt ! contient toutes les informations liees aux parametres optionnels de mx_var

   sequence
   type(tm_i_mu)     :: mu
   type(tm_i_ellips) :: ellips

end type tm_i_var_para_opt

! definition d'un noeud (pour tout arbre)
! ---------------------------------------

type tm_i_noeud  ! contient toutes les informations liees a un noeud
                 ! Utilisable pour definir l'arbre des changements de reperes 
                 ! ou l'arbre des changements de variables
   
   sequence
   integer                                  :: pere    ! numero du pere
   integer, dimension(pm_i_taille_fils_max) :: fils    ! numeros des fils
   integer                                  :: nb_fils ! nb de fils < ou = a pm_i_taille_fils_max
   integer                                  :: min     ! numero du noeud le plus petit trouve dans la descendance 
                                                       ! OU numero du noeud si pas de descendance (noeud terminal)

end type tm_i_noeud

!................................................................................................................

character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
     '@(#) Fichier MSPRO type_themeX_interne_mspro.f90: derniere modification V5.15 >'

!................................................................................................................

end module type_themeX_interne_mspro
