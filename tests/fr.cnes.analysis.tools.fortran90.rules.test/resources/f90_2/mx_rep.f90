subroutine mx_rep ( tbv_rep_in, bv_pla_in, date_in, prec_nuta_tsid_in, pos_in, vit_in, &
                    tbv_rep_out, bv_pla_out, date_out, prec_nuta_tsid_out, pos_out, vit_out, code_retour, &
                    vit_rot_in, vit_rot_out, obliquite_in, obliquite_out, pole_in, pole_out, &
                    long_ref_in, long_ref_out, val_date_in, delta_tu1_in, delta_tai_in, &
                    val_date_out, delta_tu1_out, delta_tai_out, eps_date, def_topo_in, def_topo_out,&
                    pole_tsid_planete_in, pole_tsid_planete_out, jacob )

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  Routine chapeau de changement de reperes
! ===
!
! Note d'utilisation:  Pour plus de details, se reporter au document:
! ==================
!                      Dossier de conception du theme X de la MSPRO
!                      M-NT-0-450-CIS
!                      Auteur: G. Prat, avec la participation de L. Maisonobe, B. Revelin
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 11/2002 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE 1) : ajout de nouveaux calculs de jacobiennes
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 9) : ajout des reperes interplanetaires dans le theme X
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (DE 2) : redefinition du repere Terrestre Reference Inertiel
!                         (Date: 05/2004 - Realisation: Guylaine Prat)
!   + Version 5.1 (DE 3) : passage du repere Terrestre Reference Inertiel dans une autre branche
!                         (Date: 08/2004 - Realisation: Bruno Revelin)
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

use surcharge_egal_mspro
!use surcharge_moins_mspro
!use surcharge_plus_mspro

use type_themeX_interne_mspro      ! inclus le use type_mspro

use parametre_themeX_interne_mspro 
use parametre_interne_mspro
use parametre_mspro                ! inclus le use parametre_themeX_mspro

use int_chapeau_internes, only : mxi_rep_cons_arbre
use int_chapeau_internes, only : mxi_verinit_dates
use int_chapeau_internes, only : mxi_rep_ench_transfo
use int_chapeau_internes, only : mxi_def_parcours
use int_chapeau_internes, only : mxi_transfo_identique

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

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

! Autres declarations
! ===================

type(tm_i_rep_para_opt)                                   :: para_in        ! structure des parametres en entree
type(tm_i_rep_para_opt)                                   :: para_out       ! structure des parametres en sortie
type(tm_i_noeud), dimension(pm_i_nombre_repere), save     :: arbre          ! arbre des reperes

logical                                        , save :: flag_1er_appel = pm_i_oui  ! flag de 1er appel

! epsilon de comparaison
real(pm_reel)                  :: eps_ecart_sec           !  dates en secondes

! pour les tests de date
logical                        :: dates_egales            ! flag d'egalite des dates
real(pm_reel)                  :: ecart_calcul_sec        ! ecart entre 2 dates en secondes
type(tm_jour_sec)              :: val_date_in_tai         ! date avec TAI en jour-sec en entree
type(tm_jour_sec)              :: val_date_out_tai        ! date avec TAI en jour-sec en sortie
type(tm_jour_sec)              :: val_date_in_tu1         ! date avec TU1 en jour-sec en entree
type(tm_jour_sec)              :: val_date_out_tu1        ! date avec TU1 en jour-sec en sortie

! autres flags
logical                        :: planetes_egales         ! flag d'egalite des planetes
logical                        :: modeles_egaux           ! flag d'egalite des modeles
logical                        :: poles_egaux             ! flag d'egalite des poles
integer                        :: branche_entree          ! branche de l'arbre utilisee en entree
integer                        :: branche_sortie          ! branche de l'arbre utilisee en sortie

! pour le parcours de l'arbre des reperes
integer                        :: rep_initial             ! noeud initial de la transformation dans l'arbre des reperes
integer                        :: rep_final               ! noeud final de la transformation dans l'arbre des reperes
integer,dimension(pm_i_nombre_repere) :: liste_reperes    ! liste des reperes etapes de la transformation
integer                        :: nombre_transfo          ! nombre de changements de reperes
integer                        :: modele_in               ! valeur du modele de precession et nutation en entree
integer                        :: modele_out              ! valeur du modele de precession et nutation en sortie

! pour le test de validite sur les planetes
integer,dimension(pm_mx_pla_nb) :: planetes               ! tableau des planetes
logical                         :: pla_in_ok,pla_out_ok   ! flag de validite des planetes

! si aucune transformation
integer, parameter :: taille = 6                          ! taille des tableaux de donnees
real(pm_reel), dimension(taille) :: donnees_in, donnees_out  ! tableaux des donnees a transmettre

! autres
integer                        :: retour_local            ! code retour local interne
character(len=pm_message)      :: message_local           ! message a retourner 
integer                        :: i                       ! indicateur de boucle

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mx_rep.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur et du message du code retour 
! ........................................................
code_retour%valeur = pm_OK
code_retour%message = ' '

! autres initialisations

liste_reperes(:) = 0
planetes= (/pm_pla_mercure ,pm_pla_venus ,pm_pla_terre ,pm_pla_mars,  &
     pm_pla_jupiter ,pm_pla_saturne ,pm_pla_uranus ,pm_pla_neptune ,pm_pla_pluton /)
pla_in_ok = .false.
pla_out_ok = .false.

! Tests de coherence individuelle des parametres obligatoires
! ===========================================================

if ((tbv_rep_in < pm_mx_rep_min).OR.(tbv_rep_in > pm_mx_rep_max)) then
   code_retour%valeur = pm_err_val_para   
   code_retour%message = 'Le parametre incorrect est tbv_rep_in.'
   go to 6000
end if

if ((tbv_rep_out < pm_mx_rep_min).OR.(tbv_rep_out > pm_mx_rep_max)) then
   code_retour%valeur = pm_err_val_para   
   code_retour%message = 'Le parametre incorrect est tbv_rep_out.'
   go to 6000
end if
   
do i=1,pm_mx_pla_nb
   if (bv_pla_in == planetes(i)) pla_in_ok = .true.
   if (bv_pla_out == planetes(i)) pla_out_ok = .true.
end do

if (.not.pla_in_ok) then
   code_retour%valeur = pm_err_val_para   
   code_retour%message = 'Le parametre incorrect est bv_pla_in.'
   go to 6000
end if

if (.not.pla_out_ok) then
   code_retour%valeur = pm_err_val_para   
   code_retour%message = 'Le parametre incorrect est bv_pla_out.'
   go to 6000
end if

if (((prec_nuta_tsid_in < pm_mx_prec_nuta_tsid_min).OR.(prec_nuta_tsid_in > pm_mx_prec_nuta_tsid_max)).AND.&
     ((prec_nuta_tsid_in < pm_UAI_modeles_min).OR.(prec_nuta_tsid_in > pm_UAI_modeles_max))) then
   code_retour%valeur = pm_err_val_para   
   code_retour%message = 'Le parametre incorrect est prec_nuta_tsid_in.'
   go to 6000
end if

if (((prec_nuta_tsid_out < pm_mx_prec_nuta_tsid_min).OR.(prec_nuta_tsid_out > pm_mx_prec_nuta_tsid_max)).AND.&
     ((prec_nuta_tsid_out < pm_UAI_modeles_min).OR.(prec_nuta_tsid_out > pm_UAI_modeles_max))) then ! plages de la MSLIB90
   code_retour%valeur = pm_err_val_para   
   code_retour%message = 'Le parametre incorrect est prec_nuta_tsid_out.'
   go to 6000
end if

! Tests d'incoherences possibles entre parametres
! -----------------------------------------------

if ((bv_pla_in == pm_pla_terre) .AND. (tbv_rep_in == pm_equa_uai)) then
   code_retour%valeur =  pm_err_para_incoherents
   code_retour%message = 'Le repere pm_equa_uai (tbv_rep_in) est incompatible avec la planete pm_pla_terre (bv_pla_in)'
   go to 6000
end if
if ((bv_pla_out == pm_pla_terre) .AND. (tbv_rep_out == pm_equa_uai)) then
   code_retour%valeur =  pm_err_para_incoherents
   code_retour%message = 'Le repere pm_equa_uai (tbv_rep_out) est incompatible avec la planete pm_pla_terre (bv_pla_out)'
   go to 6000
end if

if ((bv_pla_in == pm_pla_terre) .AND. (tbv_rep_in /= pm_ecli_moy) .AND. &
     ((prec_nuta_tsid_in >= pm_UAI_modeles_min).AND.(prec_nuta_tsid_in <= pm_UAI_modeles_max))) then
   code_retour%valeur =  pm_err_para_incoherents
   code_retour%message = 'Le modele en entree (prec_nuta_tsid_in) est incompatible avec la planete pm_pla_terre (bv_pla_in)'
   go to 6000
end if
if ((bv_pla_out == pm_pla_terre) .AND. (tbv_rep_out /= pm_ecli_moy) .AND. &
     ((prec_nuta_tsid_out >= pm_UAI_modeles_min).AND.(prec_nuta_tsid_out <= pm_UAI_modeles_max))) then
   code_retour%valeur =  pm_err_para_incoherents
   code_retour%message = 'Le modele en sortie (prec_nuta_tsid_out) est incompatible avec la planete pm_pla_terre (bv_pla_out)'
   go to 6000
end if

if ((bv_pla_in /= pm_pla_terre) .AND. ((tbv_rep_in == pm_veis).OR.(tbv_rep_in == pm_equa_moy).OR.&
     (tbv_rep_in == pm_equa_vrai).OR.(tbv_rep_in == pm_ecli_moy))) then
   code_retour%valeur = pm_err_para_incoherents
   code_retour%message = 'Le repere en entree (tbv_rep_in) est incompatible avec une planete autre que la Terre'
   go to 6000
end if
if ((bv_pla_out /= pm_pla_terre) .AND. ((tbv_rep_out == pm_veis).OR.(tbv_rep_out == pm_equa_moy).OR.&
     (tbv_rep_out == pm_equa_vrai).OR.(tbv_rep_out == pm_ecli_moy))) then
   code_retour%valeur = pm_err_para_incoherents
   code_retour%message = 'Le repere en sortie (tbv_rep_out) est incompatible avec une planete autre que la Terre'
   go to 6000
end if

if ((date_in /= pm_1janvier2000_12h00) .AND. (tbv_rep_in == pm_ecli_moy)) then
   code_retour%valeur = pm_err_para_incoherents
   code_retour%message = 'Le repere ecli_moy en entree est obligatoirement a la date 1er janvier 2000'
   go to 6000
end if
if ((date_out /= pm_1janvier2000_12h00) .AND. (tbv_rep_out == pm_ecli_moy)) then
   code_retour%valeur = pm_err_para_incoherents
   code_retour%message = 'Le repere ecli_moy en sortie est obligatoirement a la date 1er janvier 2000'
   go to 6000
end if

! Tests de presence des parametres optionnels
! ===========================================
! Rque: certaines affectations necessitent la surcharge du signe egal

! traitement specifique pour l'epsilon de dates
if (present(eps_date)) then
   eps_ecart_sec = max(eps_date, pm_i_eps_ecart_sec_defaut)
else
   eps_ecart_sec = pm_i_eps_ecart_sec_defaut
end if

if (present(vit_rot_in)) then
   para_in%vit_rot%presence = pm_i_oui
   para_in%vit_rot%superflu = pm_i_oui
   para_in%vit_rot%valeur = vit_rot_in
else
   para_in%vit_rot%presence = pm_i_non
   para_in%vit_rot%superflu = pm_i_non
end if

if (present(vit_rot_out)) then
   para_out%vit_rot%presence = pm_i_oui
   para_out%vit_rot%superflu = pm_i_oui
   para_out%vit_rot%valeur = vit_rot_out
else
   para_out%vit_rot%presence = pm_i_non
   para_out%vit_rot%superflu = pm_i_non
end if

if (present(obliquite_in)) then
   para_in%obliquite%presence = pm_i_oui
   para_in%obliquite%superflu = pm_i_oui
   para_in%obliquite%valeur = obliquite_in
else
   para_in%obliquite%presence = pm_i_non
   para_in%obliquite%superflu = pm_i_non
end if

if (present(obliquite_out)) then
   para_out%obliquite%presence = pm_i_oui
   para_out%obliquite%superflu = pm_i_oui
   para_out%obliquite%valeur = obliquite_out
else
   para_out%obliquite%presence = pm_i_non
   para_out%obliquite%superflu = pm_i_non
end if

if (present(pole_in)) then
   para_in%pole%presence = pm_i_oui
   para_in%pole%superflu = pm_i_oui
   para_in%pole%valeur = pole_in                  ! surcharge du signe egal pour type tm_pole_uv
else
   para_in%pole%presence = pm_i_non
   para_in%pole%superflu = pm_i_non
end if

if (present(pole_out)) then
   para_out%pole%presence = pm_i_oui
   para_out%pole%superflu = pm_i_oui
   para_out%pole%valeur = pole_out                  ! surcharge du signe egal pour type tm_pole_uv    
else
   para_out%pole%presence = pm_i_non
   para_out%pole%superflu = pm_i_non
end if

if (present(long_ref_in)) then
   para_in%long_ref%presence = pm_i_oui
   para_in%long_ref%superflu = pm_i_oui
   para_in%long_ref%valeur = long_ref_in
   if (abs(long_ref_in)<pm_i_eps_angle_nul) then
      para_in%long_ref%long_nul = pm_i_oui          ! Greenwich
   else
      para_in%long_ref%long_nul = pm_i_non          ! Pseudo greenwich
   end if
else
   para_in%long_ref%presence = pm_i_non
   para_in%long_ref%superflu = pm_i_non
end if

if (present(long_ref_out)) then
   para_out%long_ref%presence = pm_i_oui
   para_out%long_ref%superflu = pm_i_oui
   para_out%long_ref%valeur = long_ref_out
   if (abs(long_ref_out)<pm_i_eps_angle_nul) then
      para_out%long_ref%long_nul = pm_i_oui          ! Greenwich
   else
      para_out%long_ref%long_nul = pm_i_non          ! Pseudo greenwich
   end if
else
   para_out%long_ref%presence = pm_i_non
   para_out%long_ref%superflu = pm_i_non
end if

if (present(val_date_in)) then
   para_in%val_date%presence = pm_i_oui
   para_in%val_date%superflu = pm_i_oui
   para_in%val_date%valeur = val_date_in                  ! surcharge du signe egal pour tm_jour_sec
else
   para_in%val_date%presence = pm_i_non
   para_in%val_date%superflu = pm_i_non
end if

if (present(val_date_out)) then
   para_out%val_date%presence = pm_i_oui
   para_out%val_date%superflu = pm_i_oui
   para_out%val_date%valeur = val_date_out                  ! surcharge du signe egal pour tm_jour_sec   
else
   para_out%val_date%presence = pm_i_non
   para_out%val_date%superflu = pm_i_non
end if

if (present(delta_tu1_in)) then
   para_in%delta_tu1%presence = pm_i_oui
   para_in%delta_tu1%superflu = pm_i_oui
   para_in%delta_tu1%valeur = delta_tu1_in
else
   para_in%delta_tu1%presence = pm_i_non
   para_in%delta_tu1%superflu = pm_i_non
end if

if (present(delta_tu1_out)) then
   para_out%delta_tu1%presence = pm_i_oui
   para_out%delta_tu1%superflu = pm_i_oui
   para_out%delta_tu1%valeur = delta_tu1_out
else
   para_out%delta_tu1%presence = pm_i_non
   para_out%delta_tu1%superflu = pm_i_non
end if

if (present(delta_tai_in)) then
   para_in%delta_tai%superflu = pm_i_oui
   para_in%delta_tai%presence = pm_i_oui
   para_in%delta_tai%valeur = delta_tai_in
else
   para_in%delta_tai%superflu = pm_i_non
   para_in%delta_tai%presence = pm_i_non
end if

if (present(delta_tai_out)) then
   para_out%delta_tai%presence = pm_i_oui
   para_out%delta_tai%superflu = pm_i_oui
   para_out%delta_tai%valeur = delta_tai_out
else
   para_out%delta_tai%presence = pm_i_non
   para_out%delta_tai%superflu = pm_i_non
end if

if (present(def_topo_in)) then
   para_in%def_topo%presence = pm_i_oui
   para_in%def_topo%superflu = pm_i_oui
   para_in%def_topo%valeur = def_topo_in                   ! surcharge du signe egal pour tm_def_topo
   if (abs(para_in%def_topo%valeur%axe_x)<pm_i_eps_angle_nul) then
      para_in%def_topo%axe_nul = pm_i_oui          ! topo Nord
   else
      para_in%def_topo%axe_nul = pm_i_non          ! topo axe X
   end if
else
   para_in%def_topo%presence = pm_i_non
   para_in%def_topo%superflu = pm_i_non
end if

if (present(def_topo_out)) then
   para_out%def_topo%presence = pm_i_oui
   para_out%def_topo%superflu = pm_i_oui
   para_out%def_topo%valeur = def_topo_out                   ! surcharge du signe egal pour tm_def_topo
   if (abs(para_out%def_topo%valeur%axe_x)<pm_i_eps_angle_nul) then
      para_out%def_topo%axe_nul = pm_i_oui          ! topo Nord
   else
      para_out%def_topo%axe_nul = pm_i_non          ! topo axe X
   end if
else
   para_out%def_topo%presence = pm_i_non
   para_out%def_topo%superflu = pm_i_non
end if

if (present(pole_tsid_planete_in)) then
   para_in%pole_tsid_planete%presence = pm_i_oui
   para_in%pole_tsid_planete%superflu = pm_i_oui
   para_in%pole_tsid_planete%valeur = pole_tsid_planete_in 
else
   para_in%pole_tsid_planete%presence = pm_i_non
   para_in%pole_tsid_planete%superflu = pm_i_non
end if

if (present(pole_tsid_planete_out)) then
   para_out%pole_tsid_planete%presence = pm_i_oui
   para_out%pole_tsid_planete%superflu = pm_i_oui
   para_out%pole_tsid_planete%valeur = pole_tsid_planete_out 
else
   para_out%pole_tsid_planete%presence = pm_i_non
   para_out%pole_tsid_planete%superflu = pm_i_non
end if

! Tests specifiques d'egalite des parametres
! ==========================================

! test sur les planetes
! ------------------
if (bv_pla_in == bv_pla_out) then
   planetes_egales = pm_i_oui
else
   planetes_egales = pm_i_non
end if

! test sur les dates
! ------------------

call mxi_verinit_dates( date_in, "in ", para_in, retour_local, message_local)
!La structure para_in est en inout
if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local   
   code_retour%message = message_local
   if (retour_local < pm_OK) go to 6000
end if
call mxi_verinit_dates( date_out, "out", para_out, retour_local, message_local)
! La structure para_out est en inout
if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local   
   code_retour%message = message_local
   if (retour_local < pm_OK) go to 6000
end if

if (date_in == date_out) then
! Memes indicateurs de dates
! un seul des indicateurs va suffire pour les tests

   if ((date_in == pm_1janvier1950_00h00) .OR. (date_in == pm_1janvier2000_12h00 )) then
   ! dates calibrees: les autres parametres de date sont connus

      dates_egales = pm_i_oui

   else   ! dates quelconques. On cherche la valeur de la date et du delta TAI (necessaires), et les delta TU1
     
      val_date_in_tai = para_in%val_date%valeur + para_in%delta_tai%valeur       ! surcharge de +
      val_date_out_tai = para_out%val_date%valeur + para_out%delta_tai%valeur    ! surcharge de +     
      ecart_calcul_sec = val_date_in_tai - val_date_out_tai                      ! surcharge de =
      if (abs(ecart_calcul_sec)<eps_ecart_sec) then  
      ! dates egales en TAI
         if (para_in%delta_tu1%presence.AND.para_out%delta_tu1%presence) then
         ! les deux delta TU1 sont presents
            val_date_in_tu1 = para_in%val_date%valeur + para_in%delta_tu1%valeur       ! surcharge de +
            val_date_out_tu1 = para_out%val_date%valeur + para_out%delta_tu1%valeur    ! surcharge de +     
            ecart_calcul_sec = val_date_in_tu1 - val_date_out_tu1                      ! surcharge de =
            if (abs(ecart_calcul_sec)<eps_ecart_sec) then  
            ! dates egales aussi en TU1
               dates_egales = pm_i_oui
            else
               code_retour%valeur = pm_err_para_incoherents
               code_retour%message = 'D''apres les delta_tu1 fournis: les dates sont egales en TAI, mais inegales en TU1'
               go to 6000
            end if
         else if ((para_in%delta_tu1%presence.AND.(.NOT.para_out%delta_tu1%presence)).OR. &
              ((.NOT.para_in%delta_tu1%presence).AND.para_out%delta_tu1%presence)) then
         ! un seul des TU1 est present

            dates_egales = pm_i_oui
            ! les dates sont egales, et on force le delta tu1 manquant a la meme valeur
            ! on recopie egalement val_date et TAI
            if (para_in%delta_tu1%presence) then
               para_out%delta_tu1%valeur = para_in%delta_tu1%valeur
               para_out%delta_tu1%presence = pm_i_oui
               para_out%delta_tai%valeur = para_in%delta_tai%valeur
               para_out%val_date%valeur = para_in%val_date%valeur
            else
               para_in%delta_tu1%valeur = para_out%delta_tu1%valeur
               para_in%delta_tu1%presence = pm_i_oui
               para_in%delta_tai%valeur = para_out%delta_tai%valeur
               para_in%val_date%valeur = para_out%val_date%valeur
            end if
            
         else
         ! pas de TU1 present

            dates_egales = pm_i_oui    ! car meme branche
 
         end if  ! fin test sur presence TU1
 
      else
         ! dates differentes en TAI

         dates_egales = pm_i_non
      end if
      
   end if  ! fin test sur type de date = 1950 ou 2000

else
! indicateurs de date differents, donc dates differentes
   
   dates_egales = pm_i_non   ! donc 2 branches differentes

end if  ! fin test date_in = date_out
         
! test sur les modeles de precession, nutation et rotation
! ---------------------------------------------------------
if ( (prec_nuta_tsid_in - prec_nuta_tsid_out) == 0) then ! modeles identiques

   if (prec_nuta_tsid_in == pm_UAI_autre_modele) then ! modele utilisateur
      
      if (para_in%pole_tsid_planete%presence.AND.para_out%pole_tsid_planete%presence) then
      ! les donnees de chacun des modeles sont presentes

         if ( abs(pole_tsid_planete_in%alpha0-pole_tsid_planete_out%alpha0)+ &
              abs(pole_tsid_planete_in%delta0-pole_tsid_planete_out%delta0)+ &
              abs(pole_tsid_planete_in%W-pole_tsid_planete_out%W)+ &
              abs(pole_tsid_planete_in%dW-pole_tsid_planete_out%dW) < pm_i_eps_angle_nul ) then

            modeles_egaux = pm_i_oui

         else ! une des valeurs est differente

            modeles_egaux = pm_i_non 

         end if

      else if (para_in%pole_tsid_planete%presence) then ! seul le "in" est present
      ! on suppose l'egalite des 2 modeles et on recopie les valeurs dans le parametre manquant
      ! Rq: cette affectation est utile dans le cas d'utilisation de la branche Planete 2 
      !     pour d'autres raisons que des modeles differents (ex: dates differentes)
         para_out%pole_tsid_planete%presence = pm_i_oui
         para_out%pole_tsid_planete%valeur = pole_tsid_planete_in   ! surcharge = tm_pole_tsid_planete
         modeles_egaux = pm_i_oui

      else if (para_out%pole_tsid_planete%presence) then ! seul le "out" est present
      ! idem
         para_in%pole_tsid_planete%presence = pm_i_oui
         para_in%pole_tsid_planete%valeur = pole_tsid_planete_out   ! surcharge = tm_pole_tsid_planete
         modeles_egaux = pm_i_oui

      else  ! pas de parametres optionnels
         code_retour%valeur = pm_err_para_opt_abs
         code_retour%message = 'Si le modele est pm_UAI_autre_modele, il faut fournir pole_tsid_planete.'
         go to 6000
      end if

   else !  prec_nuta_tsid_in /= pm_UAI_autre_modele
      modeles_egaux = pm_i_oui
   end if

else ! modeles differents

   modeles_egaux = pm_i_non

   ! verification de presence des donnees du modele si pm_UAI_autre_modele
   if (prec_nuta_tsid_in == pm_UAI_autre_modele) then

      if (.NOT.para_in%pole_tsid_planete%presence) then
         code_retour%valeur = pm_err_para_opt_abs
         code_retour%message = 'Si le modele in est pm_UAI_autre_modele, il faut fournir pole_tsid_planete_in.'
         go to 6000
      end if

   else if (prec_nuta_tsid_out == pm_UAI_autre_modele) then
      if (.NOT.para_out%pole_tsid_planete%presence) then
         code_retour%valeur = pm_err_para_opt_abs
         code_retour%message = 'Si le modele out est pm_UAI_autre_modele, il faut fournir pole_tsid_planete_out.'
         go to 6000
      end if
   end if

end if ! fin test (prec_nuta_tsid_in - prec_nuta_tsid_out)

! test sur les coordonnees du pole, avec affectation "intelligente" si besoin
! avec test si le repere initial ET final est sur la Terre
! ---------------------------------------------------------------------------
poles_egaux = pm_i_oui

if ((bv_pla_in == pm_pla_terre).AND.(bv_pla_out == pm_pla_terre).AND.&
     (tbv_rep_in /= pm_ecli_moy).AND.(tbv_rep_out /= pm_ecli_moy)) then

   if (present(pole_in) .AND. present(pole_out)) then 

      ! examen de l'egalite "physique" des coordonnees
      if ((abs(pole_in%u - pole_out%u) > pm_i_eps_coord_pole) .OR. &
           (abs(pole_in%v - pole_out%v) > pm_i_eps_coord_pole)) then
         
         poles_egaux = pm_i_non
         
         if (dates_egales) then 
            code_retour%valeur = pm_err_para_incoherents
            code_retour%message = 'Deux valeurs in et out du pole differentes -> les dates devraient etre differentes.'
            go to 6000
         end if
         
      end if
      
   else 
      
      if (dates_egales) then ! affectation "intelligente" de la donnee du pole manquante
         
         ! Rq: cette affectation est utile dans le cas d'utilisation de la branche Terre 2 
         !     pour d'autres raisons que des dates differentes (ex: modeles differents)
         
         if (present(pole_in)) then
            para_out%pole%presence = pm_i_oui  ! on ecrase la valeur initiale de l'utilisateur
            para_out%pole%valeur = pole_in     ! surcharge = du type tm_pole_uv
         end if
         
         if (present(pole_out)) then
            para_in%pole%presence = pm_i_oui   ! on ecrase la valeur initiale de l'utilisateur
            para_in%pole%valeur = pole_out     ! surcharge = du type tm_pole_uv
         end if
         
      end if
      
   end if
! le cas: dates differentes ET 1 seul pole present
! est sorti en erreur, si besoin, dans le traitement de la transformation.
! Impossible a determiner ici a ce stade des calculs.
end if

! Determination des branches utilisees
! ====================================

! entree
if (bv_pla_in == pm_pla_terre) then

   if (tbv_rep_in == pm_ecli_moy) then
      branche_entree = pm_i_branche_Ecli2000
   else if (tbv_rep_in == pm_planeto_ref_iner) then
      branche_entree = pm_i_branche_Terre_Ref_Iner
   else
      branche_entree = pm_i_branche_Terre1
   end if

else ! planete
   branche_entree = pm_i_branche_Planete1
end if

! sortie
if (bv_pla_out == pm_pla_terre) then

   if (tbv_rep_out == pm_ecli_moy) then
      branche_sortie = pm_i_branche_Ecli2000
   else if (tbv_rep_out == pm_planeto_ref_iner) then
      branche_sortie = pm_i_branche_Terre_Ref_Iner
   else if ( (dates_egales).AND.(modeles_egaux).AND.(poles_egaux).AND.(branche_entree == pm_i_branche_Terre1) ) then 
      branche_sortie = pm_i_branche_Terre1
   else
      branche_sortie = pm_i_branche_Terre2
   end if

else ! planete

   if ( (dates_egales).AND.(modeles_egaux).AND.(planetes_egales).AND.(branche_entree == pm_i_branche_Planete1) ) then 
      branche_sortie = pm_i_branche_Planete1
   else
      branche_sortie = pm_i_branche_Planete2
   end if

end if

! ===============================================================
! calcul des noeuds initiaux et finaux
! ===============================================================

! ***************************
! Reperes reserves a la Terre
! ***************************

! Cas pm_equa_moy
! --------------- 
if (tbv_rep_in == pm_equa_moy) then
   if (date_in == pm_1janvier2000_12h00) then 
      rep_initial = pm_i_EME2000 
   else 
      rep_initial = pm_i_equa_moy_Terre1
   end if
end if
if (tbv_rep_out == pm_equa_moy) then
   if (branche_sortie == pm_i_branche_Terre2) then 
      if (date_out == pm_1janvier2000_12h00) then
         rep_final = pm_i_EME2000
      else 
         rep_final = pm_i_equa_moy_Terre2
      end if
   else 
   ! on reste dans la branche Terre1 
      if (date_out == pm_1janvier2000_12h00) then 
         rep_final = pm_i_EME2000
      else
         rep_final = pm_i_equa_moy_Terre1
      end if
   end if
end if

! Cas pm_equa_vrai 
! --------------- 
if (tbv_rep_in == pm_equa_vrai) rep_initial = pm_i_equa_vrai_Terre1
if (tbv_rep_out == pm_equa_vrai) then
   if (branche_sortie == pm_i_branche_Terre2) then 
      rep_final = pm_i_equa_vrai_Terre2
   else
   ! on reste dans la branche Terre1
      rep_final = pm_i_equa_vrai_Terre1 
   end if
end if

! Cas pm_veis
! ---------------
if (tbv_rep_in == pm_veis) rep_initial = pm_i_veis_Terre1
if (tbv_rep_out == pm_veis) then
   if (branche_sortie == pm_i_branche_Terre2) then
      rep_final = pm_i_veis_Terre2
   else 
      rep_final = pm_i_veis_Terre1
   end if
end if

! Cas pm_ecli_moy
! ----------------
! Forcement a la date J2000 pour l'instant
if (tbv_rep_in == pm_ecli_moy) rep_initial = pm_i_ecli_2000
if (tbv_rep_out == pm_ecli_moy) rep_final = pm_i_ecli_2000

! ******************************************************************
! reperes pouvant etre soit sur la Terre, soit sur une autre planete
! ******************************************************************

! Cas pm_planeto_vrai 
! ------------------
! NB: en interplanetaire, planeto_vrai = planeto_ref

if (tbv_rep_in == pm_planeto_vrai) then
   if (bv_pla_in == pm_pla_terre) then
      rep_initial = pm_i_terre_vrai_Terre1
   else
      rep_initial = pm_i_planeto_ref_Planete1
   end if
end if
if (tbv_rep_out == pm_planeto_vrai) then
   select case (branche_sortie)
    case (pm_i_branche_Terre2)
      rep_final = pm_i_terre_vrai_Terre2
    case (pm_i_branche_Terre1)
      rep_final = pm_i_terre_vrai_Terre1
    case (pm_i_branche_Planete2)
      rep_final = pm_i_planeto_ref_Planete2
    case (pm_i_branche_Planete1)
      rep_final = pm_i_planeto_ref_Planete1
   end select
end if
   
! Cas pm_planeto_ref 
! --------------- 
if (tbv_rep_in == pm_planeto_ref) then 
   if (bv_pla_in == pm_pla_terre) then   ! cas Terre
      ! il faut determiner si la longitude est definie 
      if (.NOT.para_in%long_ref%presence) then
         code_retour%valeur = pm_err_para_opt_abs
         code_retour%message = 'Dans le cas ou tbv_rep_in = pm_planeto_ref, il est indispensable de definir long_ref_in'
         go to 6000
      end if
      ! il faut determiner s il s agit du repere de Greenwich classique ou non 
      if (para_in%long_ref%long_nul) then
         ! repere de Greenwich classique
         rep_initial = pm_i_terre_ref_long_nul_Terre1
         para_in%long_ref%superflu = pm_i_non
      else 
         ! pseudo Greenwich (longitude non nulle) 
         rep_initial = pm_i_terre_ref_long_Cin_Terre1
      end if
   else   ! cas autre planete
      rep_initial = pm_i_planeto_ref_Planete1     ! meme repere que planeto_vrai
   end if
end if

if ((tbv_rep_out == pm_planeto_ref)) then 
   if (bv_pla_out == pm_pla_terre) then   ! cas Terre
      ! il faut determiner si la longitude est definie 
      if (.NOT.para_out%long_ref%presence) then
         code_retour%valeur = pm_err_para_opt_abs
         code_retour%message = 'Dans le cas ou tbv_rep_out = pm_planeto_ref, il est indispensable de definir long_ref_out'
         go to 6000
      end if
      if (branche_sortie == pm_i_branche_Terre2) then 
         ! branche Terre2 
         ! il faut determiner s il s agit du repere de Greenwich classique 
         if (para_out%long_ref%long_nul) then
            ! repere de Greenwich classique 
            rep_final = pm_i_terre_ref_long_nul_Terre2
            para_out%long_ref%superflu = pm_i_non
         else
            ! pseudo Greenwich (longitude non nulle) 
            rep_final = pm_i_terre_ref_long_Cout_Terre2 
         end if
      else 
         ! on reste dans la branche Terre1 
         ! il faut determiner s il s agit du repere de Greenwich classique 
         if (para_out%long_ref%long_nul) then 
            ! repere de Greenwich classique 
            rep_final = pm_i_terre_ref_long_nul_Terre1 
            para_out%long_ref%superflu = pm_i_non
         else 
            ! pseudo Greenwich (longitude non nulle)
            rep_final = pm_i_terre_ref_long_Cout_Terre1
         end if
      end if
   else  ! cas autre planete
      if (branche_sortie == pm_i_branche_Planete2) then
         rep_final = pm_i_planeto_ref_Planete2     ! meme repere que planeto_vrai
      else 
         ! on reste dans la branche Planete1
         rep_final = pm_i_planeto_ref_Planete1     ! meme repere que planeto_vrai
      end if
   end if
end if

! Cas pm_planeto_ref_iner
! -----------------------    

if (tbv_rep_in == pm_planeto_ref_iner) then
   if (bv_pla_in == pm_pla_terre) then   ! cas Terre
      rep_initial = pm_i_terre_ref_iner_Cin
   else
      rep_initial = pm_i_planeto_iner_Cin_Planete1
   end if
end if
if (tbv_rep_out == pm_planeto_ref_iner) then
   select case (branche_sortie)
    case (pm_i_branche_Terre_Ref_Iner)
      rep_final = pm_i_terre_ref_iner_Cout
    case (pm_i_branche_Planete2)
      rep_final = pm_i_planeto_iner_Cout_Planete2
    case (pm_i_branche_Planete1)
      rep_final = pm_i_planeto_iner_Cout_Planete1
   end select
end if

! Cas pm_topo 
! ----------- 
! il faut determiner si le repere topocentrique est defini 
if ((tbv_rep_in == pm_topo).AND.(.NOT.para_in%def_topo%presence)) then
   code_retour%valeur = pm_err_para_opt_abs
   code_retour%message = ' Dans le cas ou tbv_rep_in = pm_topo, il est indispensable de definir def_topo_in'
   go to 6000
end if
if ((tbv_rep_out == pm_topo).AND.(.NOT.para_out%def_topo%presence)) then
   code_retour%valeur = pm_err_para_opt_abs
   code_retour%message = ' Dans le cas ou tbv_rep_out = pm_topo, il est indispensable de definir def_topo_out'
   go to 6000
end if

if (tbv_rep_in == pm_topo) then
   if (bv_pla_in == pm_pla_terre) then   ! cas Terre
      rep_initial = pm_i_topo_Cin_Terre1
   else
      rep_initial = pm_i_topo_Cin_Planete1
   end if
end if
if (tbv_rep_out == pm_topo) then
   select case (branche_sortie)
    case (pm_i_branche_Terre2)
      rep_final = pm_i_topo_Cout_Terre2
    case (pm_i_branche_Terre1)
      rep_final = pm_i_topo_Cout_Terre1
    case (pm_i_branche_Planete2)
      rep_final = pm_i_topo_Cout_Planete2
    case (pm_i_branche_Planete1)
      rep_final = pm_i_topo_Cout_Planete1
   end select
end if

! **********************************
! reperes inutilisables sur la Terre
! **********************************

! Cas pm_equa_uai
! --------------
if (tbv_rep_in == pm_equa_uai) rep_initial = pm_i_equa_uai_Planete1
if (tbv_rep_out == pm_equa_uai) then
   if (branche_sortie == pm_i_branche_Planete2) then 
      rep_final = pm_i_equa_uai_Planete2
   else 
      ! on reste dans la branche Planete1 
      rep_final = pm_i_equa_uai_Planete1
   end if
end if

!
! traitement des modeles
! ----------------------

select case (prec_nuta_tsid_in)
case (pm_lieske_wahr_aoki)
   modele_in = pm_lieske_wahr
case default
   modele_in = prec_nuta_tsid_in
end select

select case (prec_nuta_tsid_out)
case (pm_lieske_wahr_aoki)
   modele_out = pm_lieske_wahr
case default
   modele_out = prec_nuta_tsid_out
end select

! definition de l'arbre
! =====================
if (flag_1er_appel) then
   flag_1er_appel = pm_i_non
   call mxi_rep_cons_arbre(arbre,retour_local)
   if (retour_local /= pm_OK) then
      code_retour%valeur = retour_local
      if (retour_local < pm_OK) go to 6000
   end if
end if

! definition du parcours dans l'arbre
! ====================================

call mxi_def_parcours( rep_initial, rep_final, arbre, liste_reperes, nombre_transfo, retour_local )
if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local
   if (retour_local < pm_OK) go to 6000
end if

! Determination s'il y a quelque chose a faire
! ============================================
if (nombre_transfo == 0) then    ! on ne fait rien

   donnees_in(1:3) = pos_in(1:3)
   donnees_in(4:6) = vit_in(1:3)

   if (present(jacob)) then
      call mxi_transfo_identique (taille, donnees_in, donnees_out, retour_local, jacob = jacob)
   else
      call mxi_transfo_identique (taille, donnees_in, donnees_out, retour_local)
   end if
   if (retour_local /= pm_OK) then
      code_retour%valeur = retour_local
      if (retour_local < pm_OK) go to 6000
   end if

   pos_out(1:3) = donnees_out(1:3)
   vit_out(1:3) = donnees_out(4:6)

else    ! il y a des transformations a faire 

   if (present(jacob)) then
      call mxi_rep_ench_transfo ( liste_reperes, nombre_transfo, para_in, para_out, bv_pla_in, bv_pla_out,&
           date_in, date_out, modele_in, modele_out, pos_in, vit_in, &
           pos_out, vit_out, retour_local, message_local, jacob = jacob)
   else
      call mxi_rep_ench_transfo ( liste_reperes, nombre_transfo, para_in, para_out, bv_pla_in, bv_pla_out,&
           date_in, date_out, modele_in, modele_out, pos_in, vit_in, &
           pos_out, vit_out, retour_local, message_local)
   end if
   if (retour_local /= pm_OK) then
      code_retour%message = message_local
      code_retour%valeur = retour_local
      if (retour_local < pm_OK) go to 6000
   end if

end if ! fin du test sur nombre_transfo

6000 continue

code_retour%routine = pm_num_mx_rep
code_retour%biblio = pm_mspro

end subroutine mx_rep
