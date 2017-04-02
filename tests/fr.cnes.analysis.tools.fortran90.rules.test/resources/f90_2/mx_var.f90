subroutine mx_var (typ_var_in, coord_in, typ_var_out, coord_out, code_retour, &
                   mu, ellips, &
                   jacob)

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But: Routine chapeau de changement de variables
! ===
!
! Note d'utilisation:  La routine effectue les transformations demandees 
! ==================   sans changement de repere meme s'il peut paraitre implicite.
!                      La routine effectue TOUTES les transformations demandees par
!                      l'utilisateur meme si, a priori, elles n'ont pas un sens 
!                      physique classique. Dans ce cas, la routine n'interdit pas 
!                      la transformation, et effectue le calcul en renvoyant 
!                      un warning.
!
!                      Pour plus de details, se reporter au document:
!
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
!                         (Date: 11/2002 - Realisation: Michel Lagreca et Bruno Revelin)
!   + Version 3.1 (DE globale 7) : ajout des parametres perigee/apogee
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE globale 14) : ajout des parametres orbitaux hyperboliques 
!                         (Date: 11/2004 - Realisation: Bruno Revelin)
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

use type_themeX_interne_mspro   ! inclus le use type_mspro

use parametre_themeX_interne_mspro
use parametre_interne_mspro
use parametre_mspro             ! inclus le use parametre_themeX_mspro

use int_chapeau_internes, only : mxi_var_typ2noeud
use int_chapeau_internes, only : mxi_var_typ_atyp
use int_chapeau_internes, only : mxi_def_parcours
use int_chapeau_internes, only : mxi_transfo_identique
use int_chapeau_internes, only : mxi_var_ench_transfo
use int_chapeau_internes, only : mxi_var_cons_arbre

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

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

! Autres declarations
! -------------------

type(tm_i_var_para_opt) :: para_opt     ! structure des parametres en entree

logical :: atypique                     ! caractere atypique d'une transformation de coordonnees

integer :: var_initial                  ! type de variable interne initiale
integer :: var_final                    ! type de variable interne finale

integer, dimension(pm_i_nombre_variable) :: liste_var      ! liste des transformations de variables
integer                                  :: nombre_transfo ! nombre de transformations

type(tm_i_noeud), dimension(pm_i_nombre_variable), save :: arbre ! arbre des transformations

logical, save :: flag_1er_appel = pm_i_oui   ! flag de 1er appel

integer, parameter :: taille = 6             ! taille des entrees/sorties

integer :: retour_local                      ! code retour local
character(len=pm_message) :: message_local   ! message de retour local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mx_var.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur et du message du code retour
! ........................................................
code_retour%valeur  = pm_OK
code_retour%message = ' '

! autres initialisations
! ......................
liste_var(:) = 0

! verification de la coherence des arguments obligatoires d'entree
! ................................................................

! test de validite de typ_var_in :
if ((typ_var_in < pm_mx_type_var_min) .OR. (typ_var_in > pm_mx_type_var_max)) then
   code_retour%valeur = pm_err_val_para
   code_retour%message = 'Le parametre incorrect est typ_var_in.'
   go to 6000
end if

! test de validite de typ_var_out :
if ((typ_var_out < pm_mx_type_var_min) .OR. (typ_var_out > pm_mx_type_var_max)) then
   code_retour%valeur = pm_err_val_para
   code_retour%message = 'Le parametre incorrect est typ_var_out.'
   go to 6000
end if

! analyse des arguments optionnels d'entree
! .........................................

if (present(mu)) then
   para_opt%mu%presence = pm_i_oui
   para_opt%mu%valeur   = mu ! valeur relle (type pm_reel)
else
   para_opt%mu%presence = pm_i_non
end if

if (present(ellips)) then
   para_opt%ellips%presence = pm_i_oui
   para_opt%ellips%valeur   = ellips ! surcharge du signe '=' pour le type tm_ellipsoide
else
   para_opt%ellips%presence = pm_i_non
end if

! definition des deux types de parametres initial et final internes
! a partir des deux types de parametres in et out utilisateur
! .................................................................

call mxi_var_typ2noeud(typ_var_in, var_initial, retour_local)

if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local
   if (retour_local < pm_OK) go to 6000
end if

call mxi_var_typ2noeud(typ_var_out, var_final, retour_local)

if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local
   if (retour_local < pm_OK) go to 6000
end if

! verification du caractere atypique ou typique d'une transformation

call mxi_var_typ_atyp(typ_var_in, typ_var_out, atypique, retour_local)

if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local
   if (retour_local < pm_OK) go to 6000
end if

! definition de l'arbre
! =====================
if (flag_1er_appel) then
   flag_1er_appel = pm_i_non
   call mxi_var_cons_arbre(arbre,retour_local)
   if (retour_local /= pm_OK) then
      code_retour%valeur = retour_local
      if (retour_local < pm_OK) go to 6000
   end if
end if

! definition du parcours dans l'arbre
! ...................................

call mxi_def_parcours(var_initial, var_final, arbre, liste_var, nombre_transfo, retour_local)

if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local
   if (retour_local < pm_OK) go to 6000
end if

! Traitement de la transformation demandee : 

if (nombre_transfo == 0) then  ! on ne fait rien

   ! Traitement du cas ou la transformation demandee est egale a l'identite

   if (present(jacob)) then
      call mxi_transfo_identique (taille, coord_in, coord_out, retour_local, jacob = jacob)
   else
      call mxi_transfo_identique (taille, coord_in, coord_out, retour_local)
   end if

   if (retour_local /= pm_OK) then
      code_retour%valeur = retour_local
      if (retour_local < pm_OK) go to 6000
   end if

else  ! il y a des transformations a faire 
   
   ! Enchainement des transformations

   if (present(jacob)) then
      call mxi_var_ench_transfo(liste_var, nombre_transfo, para_opt, coord_in, &
           coord_out, message_local, retour_local, jacob=jacob)
   else
      call mxi_var_ench_transfo(liste_var, nombre_transfo, para_opt, coord_in, &
           coord_out, message_local, retour_local)
   end if

   ! Traitement du code de retour
   ! ............................

   if (retour_local == pm_OK) then

      if (atypique) then
         code_retour%valeur = pm_warn_trsf_atypique
      end if

   else

      if (retour_local > pm_OK) then ! cas de warning

         ! Si la transformation demandee est atypique, le champ valeur du
         ! code retour est positionne a pm_warn_tsf_atypique
         ! en priorite sur tout autre warning

         if (atypique) then
            code_retour%valeur = pm_warn_trsf_atypique
            code_retour%message = ' ' ! remise a zero puisque priorite du warning pm_warn_trsf_atypique
         else
            code_retour%valeur  = retour_local
            code_retour%message = message_local 
         end if

      else ! cas d'erreur

         code_retour%valeur  = retour_local
         code_retour%message = message_local 

      end if

   end if

end if

6000 continue

code_retour%routine = pm_num_mx_var
code_retour%biblio = pm_mspro

end subroutine mx_var
