subroutine mxi_rep_ench_transfo ( liste_reperes, nombre_transfo, para_in, para_out, planete_in, planete_out, & 
        date_in, date_out, modele_in, modele_out, pos_in, vit_in, pos_out, vit_out, retour, message, jacob)

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  realiser les calculs de changement de repere d'apres la liste de transformations successives fournie
! ===
!
! Note d'utilisation:  Calcul du jacobien:
! ==================   pour simplifier les traitements, si le jacobien est demande 
!                      a ce stade des calculs, cela signifie qu'il est calculable.
!                      La verification doit etre effectuee avant l'appel.

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
!   + Version 5.0 (DE 2 mx_rep) : redefinition du repere Terrestre Reference Inertiel
!                         (Date: 05/2004 - Realisation: Guylaine Prat)
!   + Version 5.1 (DE 3 mx_rep) : passage du repere Terrestre Reference Inertiel dans une autre branche
!                         (Date: 08/2004 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.9 : DM-ID 859 : utilisation de mu_matmul6
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 5.12 : FA-ID 1309 : appels à mr_EcliJ2000_J2000 et sa réciproque avec obliquite 
!                                 si ce parametre a été correctement initialisé
!                   (Date: 10/2009 - Realisation: Atos origin)
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
use type_themeX_interne_mspro      ! inclus le use type_mspro

use parametre_themeX_interne_mspro 
use parametre_interne_mspro 
use parametre_themeX_mspro 

use int_rep_internes_mspro, only : mti_rot_axeZ
use int_dates_internes_mspro, only : mdi_ech_temps_te_tai

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

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

! Autres declarations
! -------------------
real(pm_reel),dimension(3)          :: pos_entree        !  positions intermediaires en entree
real(pm_reel),dimension(3)          :: vit_entree        !  vitesses intermediaires en entree
real(pm_reel),dimension(3)          :: pos_sortie        !  positions intermediaires en sortie
real(pm_reel),dimension(3)          :: vit_sortie        !  vitesses intermediaires en sortie
integer                             :: nombre_reperes    !  nombre de reperes parcourus
integer                             :: indice_courant    !  indice courant de parcours de la liste de reperes
integer                             :: indice_precedent  !  indice precedent de parcours de la liste de reperes
integer                             :: numero_transfo    !  numero de la transformation courante
logical                             :: derniere_transfo  !  vrai si l'on a atteint la derniere transformation
integer                             :: fils              !  numero du repere fils

! parcours de l'arbre
integer                             :: sens           ! sens de parcours dans un couple pere/fils
integer,parameter                   :: up = 1         ! valeur possible pour le sens fils->pere
integer,parameter                   :: down = 0                                    ! pere->fils
logical                             :: branche_2      ! flag vrai si la transformation elementaire 
                                                      ! est dans la banche Terre2 ou Planete2 ou Ecli 2000(finale)

! parametres des transformations
integer                             :: planete        ! numero de la planete
type(tm_jour_sec)                   :: jul1950        ! date en Jours/secondes
real(pm_reel)                       :: delta_tu1      ! ecart avec l'echelle de temps TU1
real(pm_reel)                       :: delta_tai      ! ecart avec l'echelle de temps TAI
type(tm_geodesique)                 :: orig_topo      ! origine du repere topocentrique
real(pm_reel)                       :: r_equa         ! rayon equatorial
real(pm_reel)                       :: apla           ! aplatissement
real(pm_reel)                       :: axe_X          ! angle de l'axe X du repere topo par rapport au Nord
logical                             :: axe_nul        ! vrai si axe_X est nul
real(pm_reel)                       :: angle          ! angle pour la rotation d'axe Z
real(pm_reel)                       :: vit_rot        ! vitesse de rotation de Terre
real(pm_reel)                       :: long_ref       ! longitude de reference
type(tm_pole_uv)                    :: pole_uv        ! coordonnees du pole
integer                             :: modele         ! modele de precession/nutation
real(pm_reel)                       :: alpha0         ! ascension droite du pole de rotation
real(pm_reel)                       :: delta0         ! declinaison du pole de rotation
real(pm_reel)                       :: W              ! temps sideral
real(pm_reel)                       :: dW             ! derivee du temps sideral
real(pm_reel)                       :: obliquite      ! obliquite de la terre a J2000
real(pm_reel),dimension(6,6)        :: jacob_local    ! jacobienne issue d'une sous-routine 
real(pm_reel),dimension(6,6)        :: jacob_temp     ! jacobienne intermediaire

logical                             :: inertiel       ! pour calculs sans vitesse d'entrainement

! autres
logical                             :: date_J2000          ! vrai si la date testee est le 01/01/2000 a 12h
integer                             :: i                   ! indice de boucle
integer                             :: nb_para_opt_rest    ! nb de parametres restant
logical                             :: delta_tu1_in_su     ! copie du flag de presence/utilisation
logical                             :: delta_tu1_out_su    ! copie du flag de presence/utilisation
logical                             :: pole_in_su          ! copie du flag de presence/utilisation
logical                             :: pole_out_su         ! copie du flag de presence/utilisation
logical                             :: pole_tsid_planete_in_su   ! copie du flag de presence/utilisation
logical                             :: pole_tsid_planete_out_su  ! copie du flag de presence/utilisation
character(len=pm_message)           :: lst_para_utilises   ! liste des parametres utilises
real(pm_reel)                       :: delta_t          ! ecart TE TAI (uniquement pour appel a mdi_ech_temps_te_tai)
type(tm_jour_sec)                   :: date             ! date intermediare en Jours/secondes

! code retour
type(tm_code_retour)                :: code_retour_local
integer                             :: retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mxi_rep_ench_transfo.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ...............
retour = pm_OK
message = ''

lst_para_utilises = ''

indice_courant   = 2
nombre_reperes   = nombre_transfo + 1
pos_entree(1:3)  = pos_in(1:3)
vit_entree(1:3)  = vit_in(1:3)
nb_para_opt_rest = 0
branche_2   = pm_i_non

if (present(jacob)) then       ! initialisation par la matrice identite
   jacob(:,:) = 0._pm_reel
   do i = 1,6
      jacob(i,i) = 1._pm_reel
   end do
end if

! sauvegarde des flags superflu de delta_tu1 et pole pour la gestion des parametres en trop
delta_tu1_in_su  = para_in%delta_tu1%superflu
delta_tu1_out_su = para_out%delta_tu1%superflu
pole_in_su       = para_in%pole%superflu
pole_out_su      = para_out%pole%superflu
pole_tsid_planete_in_su = para_in%pole_tsid_planete%superflu
pole_tsid_planete_out_su = para_out%pole_tsid_planete%superflu

! traitements
! ...........

if (nombre_transfo < 1) then
   retour = pm_err_valid
   go to 6000
end if

do while (indice_courant <= nombre_reperes) ! parcours de la liste a partir du 2ieme repere

   indice_precedent = indice_courant - 1      ! indice du repere precedent dans la liste
   numero_transfo = indice_precedent          ! numero de la transfo courante

   ! determination s'il s'agit de la derniere transformation
   if ((numero_transfo - nombre_transfo) == 0) then
      derniere_transfo = pm_i_oui
   else
      derniere_transfo = pm_i_non
   end if

   ! determination du pere et du fils dans le couple
   ! et du sens de la transformation
   if (liste_reperes(indice_precedent) < liste_reperes(indice_courant)) then
      sens = up
      fils = liste_reperes(indice_precedent)
   else
      sens = down
      fils = liste_reperes(indice_courant)
      if (liste_reperes(indice_precedent) == pm_i_EME2000)  branche_2 = pm_i_oui  
      ! si on passe EME2000, alors on bascule dans la branche 2
      ! pour toutes les transformations elementaires qui suivent:
      ! le flag branche_2 passe a pm_i_oui
   end if

   date_J2000 = pm_i_non

   SELECT CASE (fils) ! chaque fils possede un unique pere.
                      ! La resolution de la transformation a appliquer est simple et
                      ! ne depend que du sens de transformation (up ou down)

   ! Pour le choix de la lecture des donnees en in et out: cf document de conception M-NT-0-450-CIS

   ! Rq 1: les donnees val_date et delta_tai sont connues car testees ou affectees anterieurement
   !       donc aucun test de presence n'est necessaire pour ces valeurs a ce stade des calculs

   ! Rq 2: si la derniere transformation se situe dans la branche Terre1 ou Planete1

   !       ==> les dates sont egales donc il est equivalent de lire les 
   !           informations liees aux dates dans para_in ou para_out
   !           (voir les recopies eventuelles effectuees anterieurement ...)
   !           En particulier: valeur du pole, delta TU1, ....

   !       ==> les modeles de precession/nutation/temps sideral sont identiques
   !           donc il est equivalent de lire modele_in ou modele_out

   !       Les donnees ainsi lues, pour la derniere transformation, dans para_out 
   !       sont identiques a celles contenues dans para_in

   ! **************************************************************************************
   case (pm_i_veis_Terre1 , pm_i_veis_Terre2)                ! Pere: terre_vrai_Terre1 | terre_vrai_Terre2

      if ((.NOT.derniere_transfo).AND.(.NOT.branche_2)) then  
      ! lecture des valeurs en "in"
         jul1950 = para_in%val_date%valeur                  ! avec surcharge de tm_jour_sec
         if ((.NOT.para_in%delta_tu1%presence) .AND. (date_in == pm_autre_date)) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est delta_tu1_in'
            go to 6000
         else
            delta_tu1 = para_in%delta_tu1%valeur
            if (date_in == pm_autre_date) then
               if(delta_tu1_in_su) then
                  para_in%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur
               else
                  para_out%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur en out que l'on avait recopiee
               end if
            end if
         end if
         
      else 
      ! lecture des valeurs en "out"

         jul1950 = para_out%val_date%valeur                  ! avec surcharge de tm_jour_sec
         if ((.NOT.para_out%delta_tu1%presence) .AND. (date_out == pm_autre_date)) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est delta_tu1_out'
            go to 6000
         else
            delta_tu1 = para_out%delta_tu1%valeur
            if (date_out == pm_autre_date) then
               if(delta_tu1_out_su) then
                  para_out%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur
               else
                  para_in%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur en in que l'on avait recopiee
               end if
            end if
         end if
      end if

      if (sens == up) then

         if (present(jacob)) then
            call mr_veis_TerVrai(jul1950, delta_tu1, pos_entree, pos_sortie, code_retour_local, &
                 vit_veis = vit_entree, vit_TerVrai = vit_sortie, jacob = jacob_local ) 
         else
            call mr_veis_TerVrai(jul1950, delta_tu1, pos_entree, pos_sortie, code_retour_local, &
                 vit_veis = vit_entree, vit_TerVrai = vit_sortie ) 
         end if
      else 
         if (present(jacob)) then
            call mr_TerVrai_veis(jul1950, delta_tu1, pos_entree, pos_sortie, code_retour_local, &
                 vit_TerVrai = vit_entree, vit_veis = vit_sortie, jacob = jacob_local )
         else
            call mr_TerVrai_veis(jul1950, delta_tu1, pos_entree, pos_sortie, code_retour_local, &
                 vit_TerVrai = vit_entree, vit_veis = vit_sortie)
         end if
      end if

      if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
         retour = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_topo_Cin_Terre1 , pm_i_topo_Cout_Terre1 , pm_i_topo_Cout_Terre2, & ! Pere: terre_ref_long_nul_Terre1 | terre_ref_long_nul_Terre2
        pm_i_topo_Cin_Planete1 , pm_i_topo_Cout_Planete1 , pm_i_topo_Cout_Planete2)  ! Pere: pm_i_planeto_ref_Planete1 | pm_i_planeto_ref_Planete2
     
      if ((.NOT.derniere_transfo).OR.((nombre_transfo == 1).AND.(sens == up))) then
                                     ! (nombre_transfo == 1) : test surabondant avec (sens == up)
                                     ! mais permet de preparer des evolutions eventuelles

      ! lecture des valeurs en "in"

         if (.NOT.para_in%def_topo%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est def_topo_in'
            go to 6000
         else
            para_in%def_topo%superflu = pm_i_non
            ! on utilise la valeur fournie par l'utilisateur
            orig_topo = para_in%def_topo%valeur%geod ! tm_geodesique
            r_equa = para_in%def_topo%valeur%ellips%r_equa ! reel
            apla = para_in%def_topo%valeur%ellips%apla ! reel
            axe_X = para_in%def_topo%valeur%axe_x ! reel
            axe_nul = para_in%def_topo%axe_nul ! logique
         end if
      else 
      ! lecture des valeurs en "out"
         if (.NOT.para_out%def_topo%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est def_topo_out'
            go to 6000
         else
            para_out%def_topo%superflu = pm_i_non
            ! on utilise la valeur fournie par l'utilisateur
            orig_topo = para_out%def_topo%valeur%geod ! tm_geodesique
            r_equa = para_out%def_topo%valeur%ellips%r_equa ! reel
            apla = para_out%def_topo%valeur%ellips%apla ! reel
            axe_X = para_out%def_topo%valeur%axe_x ! reel
            axe_nul = para_out%def_topo%axe_nul ! logique
         end if
      end if

      if (sens == up) then

         ! transformation du topocentrique (quelconque) vers le planetocentrique de reference
         ! ---------------------------------------------------------------------------

         ! determination s'il ne s'agit pas d'un topocentrique Nord
         ! ........................................................
         if (.NOT.axe_nul) then ! axe X non nul

         ! on revient d'abord a un topocentrique Nord: 
         ! par transformation d'un angle " axe_X " autour de Z
            angle = axe_X
            if (present(jacob)) then
               call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                    vit_ref = vit_entree, vit_tourn = vit_sortie, jacob=jacob_local )
            else
               call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                    vit_ref = vit_entree, vit_tourn = vit_sortie )
            end if
            if (retour_local /= pm_OK) then ! traitement du code retour
               retour = retour_local
               message = 'L''erreur est survenue dans le passage du repere topocentrique quelconque vers topocentrique Nord.'
               if (retour < pm_OK) go to 6000
            end if

            if (present(jacob)) then
               call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
               ! Pas d'erreur possible donc le code retour n est pas teste
               jacob(:,:) = jacob_temp(:,:)
            end if
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

         end if

         ! A ce stade les positions et vitesses en entree sont dans le topo Nord

         ! calcul du topo Nord vers le planetocentrique de reference
         ! ..................................................
         if (present(jacob)) then
            call mt_topo_N_ref(orig_topo, r_equa, apla, pos_entree, pos_sortie, &
                 code_retour_local, vit_topo = vit_entree, vit_ref = vit_sortie, jacob = jacob_local)
         else
            call mt_topo_N_ref(orig_topo, r_equa, apla, pos_entree, pos_sortie, &
                 code_retour_local, vit_topo = vit_entree, vit_ref = vit_sortie)
         end if
         if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
            retour = code_retour_local%valeur
            message = code_retour_local%message
            if (retour < pm_OK) go to 6000
         end if

      else ! sens down

         ! transformation du planetocentrique de reference vers le topocentrique (quelconque) 
         ! ---------------------------------------------------------------------------

         ! calcul du planetocentrique de reference vers le topo Nord 
         ! ..................................................
         if (present(jacob)) then
            call mt_ref_topo_N (orig_topo, r_equa, apla, pos_entree, pos_sortie, &
                 code_retour_local, vit_ref = vit_entree, vit_topo = vit_sortie, jacob = jacob_local)
         else
            call mt_ref_topo_N (orig_topo, r_equa, apla, pos_entree, pos_sortie, &
                 code_retour_local, vit_ref = vit_entree, vit_topo = vit_sortie)
         end if         
         if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
            retour = code_retour_local%valeur
            message = code_retour_local%message
            if (retour < pm_OK) go to 6000
         end if

         ! A ce stade les positions et vitesses en entree sont dans le topo Nord

         ! determination s'il ne s'agit pas d'un topocentrique Nord
         ! ........................................................
         if (.NOT.axe_nul) then ! axe X non nul

            if (present(jacob)) then
               call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
               ! Pas d'erreur possible donc le code retour n est pas teste
               jacob(:,:) = jacob_temp(:,:)
            end if

            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

         ! passage a un topocentrique quelconque (axe X non nul): 
         ! par transformation d'un angle "- axe_X " autour de Z
            angle = - axe_X
            if (present(jacob)) then
               call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                    vit_ref = vit_entree, vit_tourn = vit_sortie, jacob = jacob_local)
            else
               call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                    vit_ref = vit_entree, vit_tourn = vit_sortie)
            end if
            if (retour_local /= pm_OK) then ! traitement du code retour
               retour = retour_local
               message = 'L''erreur est survenue dans le passage du repere topocentrique Nord vers topocentrique quelconque.'
               if (retour < pm_OK) go to 6000
            end if
         end if

         ! a ce stade des calculs les positions et vitesses en sortie sont dans le topo demande

      end if
      
   ! **************************************************************************************
   case (pm_i_terre_ref_iner_Cin , pm_i_terre_ref_iner_Cout)          ! Pere: pm_i_EME2000

      if ((.NOT.derniere_transfo).OR.((nombre_transfo == 1).AND.(sens == up))) then

      ! lecture des valeurs en "in"

         if (.NOT.para_in%pole%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est pole_in'
            go to 6000
         else
            pole_uv = para_in%pole%valeur        ! avec surcharge (u,v)
            if (pole_in_su) then
               para_in%pole%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_out%pole%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en out, que l'on avait recopiee      
            end if
         end if

         if (.NOT.para_in%long_ref%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est long_ref_in'
            go to 6000
         else
            ! on utilise la valeur fournie par l'utilisateur
            long_ref = para_in%long_ref%valeur
            para_in%long_ref%superflu = pm_i_non
         end if

         modele = modele_in
         jul1950 = para_in%val_date%valeur                  ! avec surcharge de tm_jour_sec
         delta_tai = para_in%delta_tai%valeur
         if ((.NOT.para_in%delta_tu1%presence) .AND. (date_in == pm_autre_date)) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est delta_tu1_in'
            go to 6000
         else
            delta_tu1 = para_in%delta_tu1%valeur
            if (date_in == pm_autre_date) then
               if(delta_tu1_in_su) then
                  para_in%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur
               else
                  para_out%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur en out que l'on avait recopiee
               end if
            end if
         end if
         if (date_in == pm_1janvier2000_12h00) date_J2000 = pm_i_oui

      else
      ! lecture des valeurs en "out"

         if (.NOT.para_out%pole%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est pole_out'
            go to 6000
         else
            pole_uv = para_out%pole%valeur        ! avec surcharge (u,v)
            if (pole_out_su) then
               para_out%pole%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_in%pole%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en in, que l'on avait recopiee       
            end if
         end if

         if (.NOT.para_out%long_ref%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est long_ref_out'
            go to 6000
         else
            ! on utilise la valeur fournie par l'utilisateur
            long_ref = para_out%long_ref%valeur
            para_out%long_ref%superflu = pm_i_non
         end if

         modele = modele_out
         jul1950 = para_out%val_date%valeur                  ! avec surcharge de tm_jour_sec
         delta_tai = para_out%delta_tai%valeur
         if ((.NOT.para_out%delta_tu1%presence) .AND. (date_out == pm_autre_date)) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est delta_tu1_out'
            go to 6000
         else
            delta_tu1 = para_out%delta_tu1%valeur
            if (date_out == pm_autre_date) then
               if(delta_tu1_out_su) then
                  para_out%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur
               else
                  para_in%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur en in que l'on avait recopiee
               end if
            end if
         end if
         if (date_out == pm_1janvier2000_12h00) date_J2000 = pm_i_oui
      end if

      inertiel = pm_i_oui  ! calculs sans vitesse d'entrainement

      if (sens == up) then

         ! transformation du terre ref longitude non nulle, vers longitude nulle (Greenwich)
         ! --------------------------------------------------------------------------------

         ! angle oriente de terre ref longitude non nulle, vers longitude nulle (Greenwich)
         angle = - long_ref

         if (present(jacob)) then
            call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                 vit_ref = vit_entree, vit_tourn = vit_sortie, jacob = jacob_local) 
         else         
            call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                 vit_ref = vit_entree, vit_tourn = vit_sortie)
         end if

         if (retour_local /= pm_OK) then ! traitement du code retour
            retour = retour_local
            message = 'L''erreur est survenue dans la rotation entre les reperes '&
                     &'Planeto ref longitude non nulle et longitude nulle.'
            if (retour < pm_OK) go to 6000
         end if

         if (present(jacob)) then
            call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
            ! Pas d'erreur possible donc le code retour n est pas teste
            jacob(:,:) = jacob_temp(:,:)
         end if
         pos_entree(1:3) = pos_sortie(1:3)
         vit_entree(1:3) = vit_sortie(1:3)

         ! A ce stade les positions et vitesses en entree sont dans le Terre Ref longitude nulle (Greenwich)

         ! transformation inertielle du terre ref longitude nulle (Greenwich) vers Terre Vrai
         ! puis Equatorial Vrai puis Equatorial Moyen puis EME 2000 
         ! --------------------------------------------------------------------------------
         if (present(jacob)) then
            call mr_TerRef_TerVrai ( pole_uv%u, pole_uv%v, pos_entree, pos_sortie, code_retour_local, &
                 vit_ref = vit_entree, vit_vrai = vit_sortie, jacob = jacob_local )
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
            ! Pas d'erreur possible donc le code retour n est pas teste
            jacob(:,:) = jacob_temp(:,:)
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

            call mr_TerVrai_EquaVrai(modele, jul1950, delta_tu1, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 inertiel = inertiel, vit_TerVrai = vit_entree, vit_EquaVrai = vit_sortie, jacob = jacob_local ) 
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
            ! Pas d'erreur possible donc le code retour n est pas teste
            jacob(:,:) = jacob_temp(:,:)
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

            call mr_EquaVrai_EquaMoy(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 inertiel = inertiel, vit_EquaVrai = vit_entree, vit_EquaMoy = vit_sortie, jacob = jacob_local) 
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if

            if (.NOT.date_J2000) then
               ! la date n'est pas celle de EME2000, donc la transformation n'est pas l'identite: on peut l'effectuer
               call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
               ! Pas d'erreur possible donc le code retour n est pas teste
               jacob(:,:) = jacob_temp(:,:)
               pos_entree(1:3) = pos_sortie(1:3)
               vit_entree(1:3) = vit_sortie(1:3)
               call mr_EquaMoy_J2000(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                    inertiel = inertiel, vit_EquaMoy = vit_entree, vit_J2000 = vit_sortie, jacob = jacob_local) 
               if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
                  retour = code_retour_local%valeur
                  message = code_retour_local%message
                  if (retour < pm_OK) go to 6000
               end if
            end if

         else         
            call mr_TerRef_TerVrai ( pole_uv%u, pole_uv%v, pos_entree, pos_sortie, code_retour_local, &
                 vit_ref = vit_entree, vit_vrai = vit_sortie)
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

            call mr_TerVrai_EquaVrai(modele, jul1950, delta_tu1, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 inertiel = inertiel, vit_TerVrai = vit_entree, vit_EquaVrai = vit_sortie) 
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

            call mr_EquaVrai_EquaMoy(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 inertiel = inertiel, vit_EquaVrai = vit_entree, vit_EquaMoy = vit_sortie) 
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if

            if (.NOT.date_J2000) then
               ! la date n'est pas celle de EME2000, donc la transformation n'est pas l'identite: on peut l'effectuer
               pos_entree(1:3) = pos_sortie(1:3)
               vit_entree(1:3) = vit_sortie(1:3)
               call mr_EquaMoy_J2000(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                    inertiel = inertiel, vit_EquaMoy = vit_entree, vit_J2000 = vit_sortie) 
               if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
                  retour = code_retour_local%valeur
                  message = code_retour_local%message
                  if (retour < pm_OK) go to 6000
               end if
            end if
         end if

      else ! sens down

         ! transformation du Terre Vrai Inertiel vers terre ref longitude nulle (Greenwich) 
         ! --------------------------------------------------------------------------------

         if (present(jacob)) then
            if (.NOT.date_J2000) then
               ! la date n'est pas celle de EME2000, donc la transformation n'est pas l'identite: on peut l'effectuer
               call mr_J2000_EquaMoy(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                    inertiel = inertiel, vit_J2000 = vit_entree, vit_EquaMoy = vit_sortie, jacob = jacob_local)
               if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
                  retour = code_retour_local%valeur
                  message = code_retour_local%message
                  if (retour < pm_OK) go to 6000
               end if
               call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
               ! Pas d'erreur possible donc le code retour n est pas teste
               jacob(:,:) = jacob_temp(:,:)
               pos_entree(1:3) = pos_sortie(1:3)
               vit_entree(1:3) = vit_sortie(1:3)
            end if

            call mr_EquaMoy_EquaVrai(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 inertiel = inertiel, vit_EquaMoy = vit_entree,  vit_EquaVrai = vit_sortie, jacob = jacob_local)
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
            ! Pas d'erreur possible donc le code retour n est pas teste
            jacob(:,:) = jacob_temp(:,:)
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

            call mr_EquaVrai_TerVrai(modele, jul1950, delta_tu1, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 inertiel = inertiel, vit_EquaVrai = vit_entree, vit_TerVrai = vit_sortie, jacob = jacob_local)
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
            ! Pas d'erreur possible donc le code retour n est pas teste
            jacob(:,:) = jacob_temp(:,:)
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

            call mr_TerVrai_TerRef ( pole_uv%u, pole_uv%v, pos_entree, pos_sortie, code_retour_local, &
                 vit_vrai = vit_entree, vit_ref = vit_sortie, jacob = jacob_local)
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
            ! Pas d'erreur possible donc le code retour n est pas teste
            jacob(:,:) = jacob_temp(:,:)
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

         else
            if (.NOT.date_J2000) then
               ! la date n'est pas celle de EME2000, donc la transformation n'est pas l'identite: on peut l'effectuer
               call mr_J2000_EquaMoy(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                    inertiel = inertiel, vit_J2000 = vit_entree, vit_EquaMoy = vit_sortie)
               if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
                  retour = code_retour_local%valeur
                  message = code_retour_local%message
                  if (retour < pm_OK) go to 6000
               end if
               pos_entree(1:3) = pos_sortie(1:3)
               vit_entree(1:3) = vit_sortie(1:3)
            end if

            call mr_EquaMoy_EquaVrai(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 inertiel = inertiel, vit_EquaMoy = vit_entree,  vit_EquaVrai = vit_sortie)
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

           call mr_EquaVrai_TerVrai(modele, jul1950, delta_tu1, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 inertiel = inertiel, vit_EquaVrai = vit_entree, vit_TerVrai = vit_sortie)
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)

            call mr_TerVrai_TerRef ( pole_uv%u, pole_uv%v, pos_entree, pos_sortie, code_retour_local, &
                 vit_vrai = vit_entree, vit_ref = vit_sortie)
            if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
               retour = code_retour_local%valeur
               message = code_retour_local%message
               if (retour < pm_OK) go to 6000
            end if
            pos_entree(1:3) = pos_sortie(1:3)
            vit_entree(1:3) = vit_sortie(1:3)
         end if

         ! A ce stade les positions et vitesses en entree sont dans le Terre Ref longitude nulle (Greenwich)

         ! transformation du terre ref longitude nulle (Greenwich), vers longitude non nulle
         ! --------------------------------------------------------------------------------

         ! angle oriente  de terre ref longitude nulle (Greenwich), vers longitude non nulle
         angle = + long_ref

         if (present(jacob)) then
            call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                 vit_ref = vit_entree, vit_tourn = vit_sortie, jacob = jacob_local)
         else   
            call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                 vit_ref = vit_entree, vit_tourn = vit_sortie)
         end if

         if (retour_local /= pm_OK) then ! traitement du code retour
            retour = retour_local
            message = 'L''erreur est survenue dans la rotation entre les reperes '&
                     &'Planeto ref longitude non nulle et longitude nulle.'
            if (retour < pm_OK) go to 6000
         end if

      end if

   ! **************************************************************************************
   case (pm_i_terre_ref_long_Cin_Terre1 , pm_i_terre_ref_long_Cout_Terre1 ,&
        pm_i_terre_ref_long_Cout_Terre2)                  ! Pere: terre_ref_long_nul_Terre1 | terre_ref_long_nul_Terre2
     
      if ((.NOT.derniere_transfo).OR.((nombre_transfo == 1).AND.(sens == up))) then
                                     ! (nombre_transfo == 1) : test surabondant avec (sens == up)
                                     ! mais permet de preparer des evolutions eventuelles

      ! lecture des valeurs en "in"

         if (.NOT.para_in%long_ref%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est long_ref_in'
            go to 6000
         else
            long_ref = para_in%long_ref%valeur
            para_in%long_ref%superflu = pm_i_non
            ! on utilise la valeur fournie par l'utilisateur
         end if
      else
      ! lecture des valeurs en "out"
         if (.NOT.para_out%long_ref%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est long_ref_out'
            go to 6000
         else
            long_ref = para_out%long_ref%valeur
             para_out%long_ref%superflu = pm_i_non
            ! on utilise la valeur fournie par l'utilisateur
        end if
      end if
      
      if (sens == up) then
         ! de terre ref longitude non nulle, vers longitude nulle (Greenwich)
         angle = - long_ref

         if (present(jacob)) then
            call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                 vit_ref = vit_entree, vit_tourn = vit_sortie, jacob = jacob_local)
         else   
            call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                 vit_ref = vit_entree, vit_tourn = vit_sortie)
         end if         

      else ! sens down

         ! de terre ref longitude nulle (Greenwich), vers longitude non nulle
         angle = + long_ref

         if (present(jacob)) then
            call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                 vit_ref = vit_entree, vit_tourn = vit_sortie, jacob = jacob_local)
         else   
            call mti_rot_axeZ ( angle, pos_entree, pos_sortie, retour_local, &
                 vit_ref = vit_entree, vit_tourn = vit_sortie)
         end if
      end if
      if (retour_local /= pm_OK) then ! traitement du code retour
         retour = retour_local
         message = 'L''erreur est survenue dans la rotation entre les reperes Planeto ref longitude non nulle et longitude nulle '
         if (retour < pm_OK) go to 6000
      end if
      
   ! **************************************************************************************
   case (pm_i_terre_ref_long_nul_Terre1 ,pm_i_terre_ref_long_nul_Terre2)  ! Pere: terre_vrai_Terre1 | terre_vrai_Terre2
     
      if ((.NOT.derniere_transfo).AND.(.NOT.branche_2)) then
      ! lecture des valeurs en "in"

         if (.NOT.para_in%pole%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est pole_in'
            go to 6000
         else
            pole_uv = para_in%pole%valeur        ! avec surcharge (u,v)
            if (pole_in_su) then
               para_in%pole%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_out%pole%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en out, que l'on avait recopiee      
            end if
         end if
      else
      ! lecture des valeurs en "out"
         if (.NOT.para_out%pole%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est pole_out'
            go to 6000
         else
            pole_uv = para_out%pole%valeur        ! avec surcharge (u,v)
            if (pole_out_su) then
               para_out%pole%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_in%pole%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en in, que l'on avait recopiee       
            end if
         end if
      end if
      
      if (sens == up) then
         if (present(jacob)) then
            call mr_TerRef_TerVrai ( pole_uv%u, pole_uv%v, pos_entree, pos_sortie, code_retour_local, &
                 vit_ref = vit_entree, vit_vrai = vit_sortie, jacob = jacob_local )
         else         
            call mr_TerRef_TerVrai ( pole_uv%u, pole_uv%v, pos_entree, pos_sortie, code_retour_local, &
                 vit_ref = vit_entree, vit_vrai = vit_sortie)
         end if
      else
         if (present(jacob)) then
            call mr_TerVrai_TerRef ( pole_uv%u, pole_uv%v, pos_entree, pos_sortie, code_retour_local, &
                 vit_vrai = vit_entree, vit_ref = vit_sortie, jacob = jacob_local)
         else
            call mr_TerVrai_TerRef ( pole_uv%u, pole_uv%v, pos_entree, pos_sortie, code_retour_local, &
                 vit_vrai = vit_entree, vit_ref = vit_sortie)
         end if
      end if
      if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
         retour = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_terre_vrai_Terre1 , pm_i_terre_vrai_Terre2 )           ! Pere: equa_vrai_Terre1 | equa_vrai_Terre2
     
      if ((.NOT.derniere_transfo).AND.(.NOT.branche_2)) then
      ! lecture des valeurs en "in"
         modele = modele_in
         jul1950 = para_in%val_date%valeur                  ! avec surcharge de tm_jour_sec
         delta_tai = para_in%delta_tai%valeur
         if ((.NOT.para_in%delta_tu1%presence) .AND. (date_in == pm_autre_date)) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est delta_tu1_in'
            go to 6000
         else
            delta_tu1 = para_in%delta_tu1%valeur
            if (date_in == pm_autre_date) then
               if(delta_tu1_in_su) then
                  para_in%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur
               else
                  para_out%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur en out que l'on avait recopiee
               end if
            end if
         end if
      else 
      ! lecture des valeurs en "out"
         modele = modele_out
         jul1950 = para_out%val_date%valeur                  ! avec surcharge de tm_jour_sec
         delta_tai = para_out%delta_tai%valeur
         if ((.NOT.para_out%delta_tu1%presence) .AND. (date_out == pm_autre_date)) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est delta_tu1_out'
            go to 6000
         else
            delta_tu1 = para_out%delta_tu1%valeur
            if (date_out == pm_autre_date) then
               if(delta_tu1_out_su) then
                  para_out%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur
               else
                  para_in%delta_tu1%superflu = pm_i_non 
                  ! on utilise la valeur fournie par l'utilisateur en in que l'on avait recopiee
               end if
            end if
         end if
      end if

      if (sens == up) then
         if (present(jacob)) then
            call mr_TerVrai_EquaVrai(modele, jul1950, delta_tu1, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 vit_TerVrai = vit_entree, vit_EquaVrai = vit_sortie, jacob = jacob_local ) 
         else
            call mr_TerVrai_EquaVrai(modele, jul1950, delta_tu1, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 vit_TerVrai = vit_entree, vit_EquaVrai = vit_sortie) 
         end if
      else 
         if (present(jacob)) then
            call mr_EquaVrai_TerVrai(modele, jul1950, delta_tu1, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 vit_EquaVrai = vit_entree, vit_TerVrai = vit_sortie, jacob = jacob_local)
         else
            call mr_EquaVrai_TerVrai(modele, jul1950, delta_tu1, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 vit_EquaVrai = vit_entree, vit_TerVrai = vit_sortie)
         end if
      end if

      if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
         retour = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_equa_vrai_Terre1 , pm_i_equa_vrai_Terre2 )           ! Pere: equa_moy_Terre1 | equa_moy_Terre2
     
      if ((.NOT.derniere_transfo).AND.(.NOT.branche_2)) then
      ! lecture des valeurs en "in"
         modele = modele_in
         jul1950 = para_in%val_date%valeur                  ! avec surcharge de tm_jour_sec
         delta_tai = para_in%delta_tai%valeur
      else 
      ! lecture des valeurs en "out"
         modele = modele_out
         jul1950 = para_out%val_date%valeur                  ! avec surcharge de tm_jour_sec
         delta_tai = para_out%delta_tai%valeur
      end if

      if (sens == up) then
         if (present(jacob)) then
            call mr_EquaVrai_EquaMoy(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 vit_EquaVrai = vit_entree, vit_EquaMoy = vit_sortie, jacob = jacob_local) 
         else
            call mr_EquaVrai_EquaMoy(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 vit_EquaVrai = vit_entree, vit_EquaMoy = vit_sortie) 
         end if
      else 
         if (present(jacob)) then
            call mr_EquaMoy_EquaVrai(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 vit_EquaMoy = vit_entree,  vit_EquaVrai = vit_sortie, jacob = jacob_local)
         else
            call mr_EquaMoy_EquaVrai(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                 vit_EquaMoy = vit_entree,  vit_EquaVrai = vit_sortie)
         end if
      end if
      
      if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
         retour = code_retour_local%valeur
         message = 'L''erreur est survenue dans la transformation Equatorial Moyen <-> Equatorial Vrai'
         if (retour < pm_OK) go to 6000
      end if

  ! **************************************************************************************
   case (pm_i_equa_moy_Terre1 , pm_i_equa_moy_Terre2 )           ! Pere: EME2000
     
      date_J2000 = pm_i_non
      if (.NOT.branche_2) then   ! EME2000 marque la limite entre les branches
      ! lecture des valeurs en "in"
         modele = modele_in
         jul1950 = para_in%val_date%valeur                  ! avec surcharge de tm_jour_sec
         delta_tai = para_in%delta_tai%valeur
         if (date_in == pm_1janvier2000_12h00) date_J2000 = pm_i_oui
      else 
      ! branche 2: lecture des valeurs en "out"
         modele = modele_out
         jul1950 = para_out%val_date%valeur                  ! avec surcharge de tm_jour_sec
         delta_tai = para_out%delta_tai%valeur
         if (date_out == pm_1janvier2000_12h00) date_J2000 = pm_i_oui
      end if

      if (.NOT.date_J2000) then
      ! la date n'est pas celle de EME2000, donc la transformation n'est pas l'identite: on peut l'effectuer
         if (sens == up) then
            if (present(jacob)) then
               call mr_EquaMoy_J2000(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                    vit_EquaMoy = vit_entree, vit_J2000 = vit_sortie, jacob = jacob_local) 
            else 
               call mr_EquaMoy_J2000(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                    vit_EquaMoy = vit_entree, vit_J2000 = vit_sortie) 
            end if
         else 
            if (present(jacob)) then
               call mr_J2000_EquaMoy(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                    vit_J2000 = vit_entree, vit_EquaMoy = vit_sortie, jacob = jacob_local)
            else 
               call mr_J2000_EquaMoy(modele, jul1950, delta_tai, pos_entree, pos_sortie, code_retour_local, &
                    vit_J2000 = vit_entree, vit_EquaMoy = vit_sortie)
            end if
         end if
         
         if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
            retour = code_retour_local%valeur
            message = code_retour_local%message
            if (retour < pm_OK) go to 6000
         end if
      else
      ! on ne fait pas le calcul car les 2 reperes sont les memes !
         pos_sortie(1:3) = pos_entree(1:3)
         vit_sortie(1:3) = vit_entree(1:3)
         if (present(jacob)) then
            jacob_local(:,:) = 0._pm_reel
            do i = 1,6
               jacob_local(i,i) = 1._pm_reel
            end do
         end if
      end if

   ! **************************************************************************************
   case (pm_i_equa_uai_Planete1 , pm_i_equa_uai_Planete2 )           ! Pere: EME2000
     
      if (.NOT.branche_2) then   ! EME2000 marque la limite entre les branches
      ! lecture des valeurs en "in"
         planete = planete_in
         modele = modele_in
         if (modele == pm_UAI_autre_modele) then  
            alpha0 = para_in%pole_tsid_planete%valeur%alpha0 ! presence deja testee
            delta0 = para_in%pole_tsid_planete%valeur%delta0 ! presence deja testee
            if (pole_tsid_planete_in_su) then
               para_in%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_out%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en out, que l'on avait recopiee      
            end if
         end if
         date = para_in%val_date%valeur + para_in%delta_tai%valeur    ! avec surcharge de tm_jour_sec
         call mdi_ech_temps_te_tai (pm_i_tai_te, date, jul1950, delta_t, retour) ! pas de retour /= pm_OK
      else 
      ! branche 2: lecture des valeurs en "out"
         planete = planete_out
         modele = modele_out
         if (modele == pm_UAI_autre_modele) then
            alpha0 = para_out%pole_tsid_planete%valeur%alpha0 ! presence deja testee
            delta0 = para_out%pole_tsid_planete%valeur%delta0 ! presence deja testee
            if (pole_tsid_planete_out_su) then
               para_out%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_in%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en in, que l'on avait recopiee      
            end if
         end if
         date = para_out%val_date%valeur + para_out%delta_tai%valeur    ! avec surcharge de tm_jour_sec
         call mdi_ech_temps_te_tai (pm_i_tai_te, date, jul1950, delta_t, retour) ! pas de retour /= pm_OK
      end if

      if (sens == up) then
         if (modele == pm_UAI_autre_modele) then
            if (present(jacob)) then
               call mr_EquaUAI_J2000 ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    asc_droite = alpha0, declinaison = delta0, &
                    vit_EquaUAI = vit_entree, vit_J2000 = vit_sortie, jacob = jacob_local )
            else 
               call mr_EquaUAI_J2000 ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    asc_droite = alpha0, declinaison = delta0, &
                    vit_EquaUAI = vit_entree, vit_J2000 = vit_sortie )
            end if
         else
            if (present(jacob)) then
               call mr_EquaUAI_J2000 ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    vit_EquaUAI = vit_entree, vit_J2000 = vit_sortie, jacob = jacob_local )
            else 
               call mr_EquaUAI_J2000 ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                 vit_EquaUAI = vit_entree, vit_J2000 = vit_sortie )
            end if
         end if
      else ! sens down
         if (modele == pm_UAI_autre_modele) then
            if (present(jacob)) then
               call mr_J2000_EquaUAI ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    asc_droite = alpha0, declinaison = delta0, &
                    vit_J2000 = vit_entree, vit_EquaUAI = vit_sortie, jacob = jacob_local )
            else 
               call mr_J2000_EquaUAI ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    asc_droite = alpha0, declinaison = delta0, &
                    vit_J2000 = vit_entree, vit_EquaUAI = vit_sortie )
            end if
         else
            if (present(jacob)) then
               call mr_J2000_EquaUAI ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    vit_J2000 = vit_entree, vit_EquaUAI = vit_sortie, jacob = jacob_local )
            else 
               call mr_J2000_EquaUAI ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    vit_J2000 = vit_entree, vit_EquaUAI = vit_sortie )
            end if
         end if
      end if
      if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
         retour = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_planeto_ref_Planete1 , pm_i_planeto_ref_Planete2 )    ! Pere: equaUAI_Planete1 | equaUAI_Planete2
     
      if ((.NOT.derniere_transfo).AND.(.NOT.branche_2)) then
      ! lecture des valeurs en "in"
         planete = planete_in
         modele = modele_in
         if (modele == pm_UAI_autre_modele) then
            W = para_in%pole_tsid_planete%valeur%W ! presence deja testee
            dW = para_in%pole_tsid_planete%valeur%dW ! presence deja testee
            if (pole_tsid_planete_in_su) then
               para_in%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_out%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en out, que l'on avait recopiee      
            end if
         end if
         date = para_in%val_date%valeur + para_in%delta_tai%valeur    ! avec surcharge de tm_jour_sec
         call mdi_ech_temps_te_tai (pm_i_tai_te, date, jul1950, delta_t, retour) ! pas de retour /= pm_OK
      else 
      ! lecture des valeurs en "out"
         planete = planete_out
         modele = modele_out
         if (modele == pm_UAI_autre_modele) then
            W = para_out%pole_tsid_planete%valeur%W ! presence deja testee
            dW = para_out%pole_tsid_planete%valeur%dW ! presence deja testee
            if (pole_tsid_planete_out_su) then
               para_out%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_in%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en in, que l'on avait recopiee      
            end if
         end if
         date = para_out%val_date%valeur + para_out%delta_tai%valeur    ! avec surcharge de tm_jour_sec
         call mdi_ech_temps_te_tai (pm_i_tai_te, date, jul1950, delta_t, retour) ! pas de retour /= pm_OK
      end if

      if (sens == up) then
         if (modele == pm_UAI_autre_modele) then
            if (present(jacob)) then
               call mr_PlanetVrai_EquaUAI ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    tsid = W, deriv_tsid = dW, &
                    vit_PlanetVrai = vit_entree, vit_EquaUAI = vit_sortie, jacob = jacob_local )
            else 
               call mr_PlanetVrai_EquaUAI ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    tsid = W, deriv_tsid = dW, &
                    vit_PlanetVrai = vit_entree, vit_EquaUAI = vit_sortie )
            end if
         else
            if (present(jacob)) then
               call mr_PlanetVrai_EquaUAI ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    vit_PlanetVrai = vit_entree, vit_EquaUAI = vit_sortie, jacob = jacob_local )
            else 
               call mr_PlanetVrai_EquaUAI ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    vit_PlanetVrai = vit_entree, vit_EquaUAI = vit_sortie )
            end if
         end if
      else ! sens down
         if (modele == pm_UAI_autre_modele) then
            if (present(jacob)) then
               call mr_EquaUAI_PlanetVrai ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    tsid = W, deriv_tsid = dW, &
                    vit_EquaUAI = vit_entree, vit_PlanetVrai = vit_sortie, jacob = jacob_local )
            else 
               call mr_EquaUAI_PlanetVrai ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    tsid = W, deriv_tsid = dW, &
                    vit_EquaUAI = vit_entree, vit_PlanetVrai = vit_sortie )
            end if
         else
            if (present(jacob)) then
               call mr_EquaUAI_PlanetVrai ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                    vit_EquaUAI = vit_entree, vit_PlanetVrai = vit_sortie, jacob = jacob_local )
            else 
               call mr_EquaUAI_PlanetVrai ( planete, modele, jul1950, pos_entree, pos_sortie, code_retour_local, &
                 vit_EquaUAI = vit_entree, vit_PlanetVrai = vit_sortie )
            end if
         end if
      end if

      if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
         retour = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_planeto_iner_Cin_Planete1 , pm_i_planeto_iner_Cout_Planete1 ,&
        pm_i_planeto_iner_Cout_Planete2)                  ! Pere: planeto_ref_Planete1 | planeto_ref_Planete2
     
      if ((.NOT.derniere_transfo).OR.((nombre_transfo == 1).AND.(sens == up))) then
                                     ! (nombre_transfo == 1) : test surabondant avec (sens == up)
                                     ! mais permet de preparer des evolutions eventuelles

      ! lecture des valeurs en "in"
         planete = planete_in
         modele = modele_in
         if (modele == pm_UAI_autre_modele) then
            vit_rot = para_in%pole_tsid_planete%valeur%dW ! presence deja testee
            if (pole_tsid_planete_in_su) then
               para_in%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_out%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en out, que l'on avait recopiee      
            end if
         end if 
         ! on recupere la date, mais besoin seulement si planete = Neptune
         date = para_in%val_date%valeur + para_in%delta_tai%valeur    ! avec surcharge de tm_jour_sec
         call mdi_ech_temps_te_tai (pm_i_tai_te, date, jul1950, delta_t, retour) ! pas de retour /= pm_OK
         if (.NOT.para_in%long_ref%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est long_ref_in'
            go to 6000
         else
            ! on utilise la valeur fournie par l'utilisateur
            long_ref = para_in%long_ref%valeur
            para_in%long_ref%superflu = pm_i_non
         end if
      else   ! lecture en out
         planete = planete_out
         modele = modele_out
         if (modele == pm_UAI_autre_modele) then
            vit_rot = para_out%pole_tsid_planete%valeur%dW ! presence deja testee
            if (pole_tsid_planete_out_su) then
               para_out%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur 
            else
               para_out%pole_tsid_planete%superflu = pm_i_non
               ! on utilise la valeur fournie par l'utilisateur en out, que l'on avait recopiee      
            end if
         end if 
         ! on recupere la date, mais besoin seulement si planete = Neptune
         date = para_out%val_date%valeur + para_out%delta_tai%valeur    ! avec surcharge de tm_jour_sec
         call mdi_ech_temps_te_tai (pm_i_tai_te, date, jul1950, delta_t, retour) ! pas de retour /= pm_OK
         if (.NOT.para_out%long_ref%presence) then
            retour = pm_err_para_opt_abs
            message = 'Le parametre optionnel manquant est long_ref_out'
            go to 6000
         else
            ! on utilise la valeur fournie par l'utilisateur
            long_ref = para_out%long_ref%valeur
            para_out%long_ref%superflu = pm_i_non
         end if
      end if
      if (sens == up) then
         if (modele == pm_UAI_autre_modele) then
            if (present(jacob)) then
               call mr_PlaIner_PlaVrai ( planete, modele, long_ref, pos_entree, pos_sortie, code_retour_local, &
                    vit_rot = vit_rot, &
                    jul1950 = jul1950, vit_PlaIner = vit_entree, vit_PlaVrai = vit_sortie, jacob = jacob_local )
            else 
               call mr_PlaIner_PlaVrai ( planete, modele, long_ref, pos_entree, pos_sortie, code_retour_local, &
                    vit_rot = vit_rot, &
                    jul1950 = jul1950, vit_PlaIner = vit_entree, vit_PlaVrai = vit_sortie )
            end if
         else
            if (present(jacob)) then
               call mr_PlaIner_PlaVrai ( planete, modele, long_ref, pos_entree, pos_sortie, code_retour_local, &
                    jul1950 = jul1950, vit_PlaIner = vit_entree, vit_PlaVrai = vit_sortie, jacob = jacob_local )
            else 
               call mr_PlaIner_PlaVrai ( planete, modele, long_ref, pos_entree, pos_sortie, code_retour_local, &
                    jul1950 = jul1950, vit_PlaIner = vit_entree, vit_PlaVrai = vit_sortie )
            end if
         end if
      else ! sens down
         if (modele == pm_UAI_autre_modele) then
            if (present(jacob)) then
               call mr_PlaVrai_PlaIner ( planete, modele, long_ref, pos_entree, pos_sortie, code_retour_local, &
                    vit_rot = vit_rot, &
                    jul1950 = jul1950, vit_PlaVrai = vit_entree, vit_PlaIner = vit_sortie, jacob = jacob_local )
            else 
               call mr_PlaVrai_PlaIner ( planete, modele, long_ref, pos_entree, pos_sortie, code_retour_local, &
                    vit_rot = vit_rot, &
                    jul1950 = jul1950, vit_PlaVrai = vit_entree, vit_PlaIner = vit_sortie )
            end if
         else
            if (present(jacob)) then
               call mr_PlaVrai_PlaIner ( planete, modele, long_ref, pos_entree, pos_sortie, code_retour_local, &
                    jul1950 = jul1950, vit_PlaVrai = vit_entree, vit_PlaIner = vit_sortie, jacob = jacob_local )
            else 
               call mr_PlaVrai_PlaIner ( planete, modele, long_ref, pos_entree, pos_sortie, code_retour_local, &
                    jul1950 = jul1950, vit_PlaVrai = vit_entree, vit_PlaIner = vit_sortie )
            end if
         end if
      end if
      if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
         retour = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_ecli_2000)           ! Pere: EME2000
     
      if (.NOT.branche_2) then   ! EME2000 marque la limite entre les branches

      ! lecture des valeurs en "in"
         if (para_in%obliquite%presence) then
         ! utilisation de l'obliquite en option
            obliquite = para_in%obliquite%valeur
            para_in%obliquite%superflu = pm_i_non
         end if
      else 
      
         if (para_out%obliquite%presence) then
         ! utilisation de l'obliquite en option
            obliquite = para_out%obliquite%valeur
            para_out%obliquite%superflu = pm_i_non
         end if
      end if

      if (sens == up) then
	 if (para_in%obliquite%presence) then
            if (present(jacob)) then
               call mr_EcliJ2000_J2000 ( pos_entree, pos_sortie, code_retour_local, &
                    obliquite = obliquite, &
                    vit_EcliJ2000 = vit_entree, vit_J2000 = vit_sortie, jacob = jacob_local )
            else 
               call mr_EcliJ2000_J2000 ( pos_entree, pos_sortie, code_retour_local, &
                    obliquite = obliquite, &
                    vit_EcliJ2000 = vit_entree, vit_J2000 = vit_sortie )
            end if
         else
            if (present(jacob)) then
               call mr_EcliJ2000_J2000 ( pos_entree, pos_sortie, code_retour_local, &
                    vit_EcliJ2000 = vit_entree, vit_J2000 = vit_sortie, jacob = jacob_local )
            else 
               call mr_EcliJ2000_J2000 ( pos_entree, pos_sortie, code_retour_local, &
                 vit_EcliJ2000 = vit_entree, vit_J2000 = vit_sortie )
            end if
         end if
      else ! sens down
	 if (para_out%obliquite%presence) then
            if (present(jacob)) then
               call mr_J2000_EcliJ2000 ( pos_entree, pos_sortie, code_retour_local, &
                    obliquite = obliquite, &
                    vit_J2000 = vit_entree, vit_EcliJ2000 = vit_sortie, jacob = jacob_local )
            else 
               call mr_J2000_EcliJ2000 ( pos_entree, pos_sortie, code_retour_local, &
                    obliquite = obliquite, &
                    vit_J2000 = vit_entree, vit_EcliJ2000 = vit_sortie )
            end if
         else
            if (present(jacob)) then
               call mr_J2000_EcliJ2000 ( pos_entree, pos_sortie, code_retour_local, &
                    vit_J2000 = vit_entree, vit_EcliJ2000 = vit_sortie, jacob = jacob_local )
            else 
               call mr_J2000_EcliJ2000 ( pos_entree, pos_sortie, code_retour_local, &
                    vit_J2000 = vit_entree, vit_EcliJ2000 = vit_sortie )
            end if
         end if
      end if
      if (code_retour_local%valeur /= pm_OK) then ! traitement du code retour
         retour = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

  ! **************************************************************************************
  ! Pas de case (pm_i_EME2000 ) 
  ! car il s'agit du noeud racine donc ne possede pas de pere ...
  ! **************************************************************************************

   END SELECT ! fin du select case sur le fils

   indice_courant = indice_courant + 1
   ! transfert des sorties en entrees pour transformation suivante si besoin

   if (present(jacob)) then
      call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
      ! Pas d'erreur possible donc le code retour n est pas teste
      jacob(:,:) = jacob_temp(:,:)
   end if
   pos_entree(1:3) = pos_sortie(1:3)
   vit_entree(1:3) = vit_sortie(1:3)
   
end do ! fin boucle while sur tous les reperes de la liste

! Affectation des sorties
pos_out(1:3) = pos_sortie(1:3)
vit_out(1:3) = vit_sortie(1:3)

! pour le jacobien: a ete calcule au fur et a mesure des transformations

! bilan des parametres optionnels eventuellement fournis en trop par l'utilisateur
! ................................................................................

if (para_in%vit_rot%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%vit_rot%superflu).AND.(para_in%vit_rot%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',vit_rot_in'
if (para_in%obliquite%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%obliquite%superflu).AND.(para_in%obliquite%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',obliquite_in'
if (para_in%pole%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%pole%superflu).AND.(pole_in_su)) &
     lst_para_utilises = trim(lst_para_utilises) // ',pole_in'
if (para_in%long_ref%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%long_ref%superflu).AND.(para_in%long_ref%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',long_ref_in'
if (para_in%val_date%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%val_date%superflu).AND.(para_in%val_date%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',val_date_in'
if (para_in%delta_tai%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%delta_tai%superflu).AND.(para_in%delta_tai%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',delta_tai_in'
if (para_in%delta_tu1%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%delta_tu1%superflu).AND.(delta_tu1_in_su)) &
     lst_para_utilises = trim(lst_para_utilises) // ',delta_tu1_in'
if (para_in%def_topo%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%def_topo%superflu).AND.(para_in%def_topo%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',def_topo_in'
if (para_in%pole_tsid_planete%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_in%pole_tsid_planete%superflu).AND.(para_in%pole_tsid_planete%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',pole_tsid_planete_in'

if (para_out%vit_rot%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%vit_rot%superflu).AND.(para_out%vit_rot%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',vit_rot_out'
if (para_out%obliquite%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%obliquite%superflu).AND.(para_out%obliquite%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',obliquite_out'
if (para_out%pole%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%pole%superflu).AND.(pole_out_su)) &
     lst_para_utilises = trim(lst_para_utilises) // ',pole_out'
if (para_out%long_ref%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%long_ref%superflu).AND.(para_out%long_ref%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',long_ref_out'
if (para_out%val_date%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%val_date%superflu).AND.(para_out%val_date%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',val_date_out'
if (para_out%delta_tai%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%delta_tai%superflu).AND.(para_out%delta_tai%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',delta_tai_out'
if (para_out%delta_tu1%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%delta_tu1%superflu).AND.(delta_tu1_out_su)) &
     lst_para_utilises = trim(lst_para_utilises) // ',delta_tu1_out'
if (para_out%def_topo%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%def_topo%superflu).AND.(para_out%def_topo%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',def_topo_out'
if (para_out%pole_tsid_planete%superflu) nb_para_opt_rest = nb_para_opt_rest + 1
if ((.NOT.para_out%pole_tsid_planete%superflu).AND.(para_out%pole_tsid_planete%presence)) &
     lst_para_utilises = trim(lst_para_utilises) // ',pole_tsid_planete_out'

if (trim(lst_para_utilises) == '') lst_para_utilises = ',aucun'
if (nb_para_opt_rest > 0) then
   ! on signale la presence de parametres en trop dans un warning
   retour = pm_warn_para_opt_trop

   ! Rq: lors de la concatenation, la taille de la chaine a inserer dans message est suffisamment petite pour
   !     ne pas necessiter de test specifique
   write(message,1000) trim(lst_para_utilises(2:)),nb_para_opt_rest
end if

6000 continue
1000 format('Les parametres optionnels utilises sont: ',A,' .Il en reste ',I2.2,' non utilise(s).' )

end subroutine mxi_rep_ench_transfo
