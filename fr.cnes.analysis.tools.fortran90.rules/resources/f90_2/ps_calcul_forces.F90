module ps_calcul_forces

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_calcul_forces
!
!$Resume
!  Module regroupant des données et des routines de bas niveau pour les calculs de forces.
!
!$Description
!  Module regroupant des données et des routines de bas niveau pour les calculs de forces.
!  Les données stockées sont les différentes structures MECASPA (pour le potentiel, l'atmosphere, le vent)
!  et des données physiques liées au modèle de forces utilisé.
!  Les routines servent à faire des changements de repère entre les différents repères servant pour les 
!  calculs de forces.
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Version
!  $Id: ps_calcul_forces.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_calcul_forces.F90,v $
!  Revision 1.6  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.5  2009/09/04 15:08:18  mercadig
!  DM-ID 1218: Suppression des champs devenus obsoletes de la structure PS_STR_INT_TROISCORPS et ajout des parametres pour la gestion des codes methodes COMPAS
!
!  Revision 1.4  2008/12/04 15:40:05  tanguyy
!  AQ : suppression de variables inutilisees
!
!  Revision 1.3  2008/12/02 16:45:50  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.2  2008/12/02 08:17:41  tanguyy
!  DM-ID 733 : correction mineures du code suite aux remarques de la relecture de code, mise en forme des cartouches
!
!$FinHistorique
!
!$Usage
!  use ps_calcul_forces
!
!$Structure
!
!: PS_STR_INT_MODELE : 
!>     ikle           : <integer,DIM=(4)>          Indicateurs de prise en compte de : 
!                                                   - attraction lunaire (Terre) ou Phobos/Deimos (Mars) : ikle(1)
!                                                   - attraction du soleil : ikle(2)
!                                                   - prise en compte d'un frottement atmosphérique avec activité solaire standard (1)
!                                                   ou réelle (2). Pour Mars ou Vénus, ikle(3) vaut 1 si on utilise un modèle d'atmosphere
!                                                   - prise en compte de la pression solaire : ikle(4)
!>     pot            : <MSP_POTENTIEL>            Structure stockant le potentiel du corps central
!>     atm            : <MSP_ATMOSPHERE>           Structure stockant les paramètres d'atmosphère et d'activité solaire
!>     vent           : <MSP_MODVENT>              Structure stockant les paramètres de vent
!>     nzo            : <integer>                  Degré des termes zonaux du potentiel (= pot%nzo)
!>     nte            : <integer>                  Degré des termes tesseraux du potentiel (= pot%nte)
!>     vj             : <pm_reel,DIM=(:),pointer>  Tableaux du potentiel pour calculer les paramètres orbitaux moyens 
!                                                   avec le modèle Eckstein-Hechler
!>     requa          : <pm_reel>                  Rayon équatorial du corps central
!>     apla           : <pm_reel>                  Inverse de l'aplatissement du corps central
!>     gmu            : <pm_reel>                  Mu du corps central
!>     vitrotpla      : <pm_reel>                  Vitesse de rotation du corps central (rad/s)
!>     calcul_vent    : <integer>                  Indicateur d'appel au modèle de vent
!>     modvent        : <LEN=80>                   Nom du modèle de vent
!
!: PS_STR_INT_TROISCORPS : 
!>     typephem       : <integer>                  Type d'éphémérides (Tchebytchev, Analytique)
!>     init_soleil    : <logical>                  Indicateur du chargement des éphémérides pour un corps
!>     init_lune      : <logical>                  idem
!>     init_phobos    : <logical>                  idem
!>     init_deimos    : <logical>                  idem
!>     mu_soleil      : <pm_reel>                 
!>     mu_lune        : <pm_reel>                 
!>     mu_phobos      : <pm_reel>                  
!>     mu_deimos      : <pm_reel>                  
!>     ficept         : <LEN=80>                  Fichier d'éphémérides (pour éviter le rechargement) 
!
!$Global
!
!>  str_3co                      : <PS_STR_INT_TROISCORPS,DIM=(PS_NVMAX)>  
!>  str_mod                      : <PS_STR_INT_MODELE,DIM=(PS_NVMAX)>      
!>  PSI_METHODE_EPH_ANALYTIQUE   : <integer,parameter>                     
!$Common
!
!$Routines
!- ps_rotation_tsid
!- ps_mat_rotation_tsid
!- ps_calcul_tsid_RI_Rapp
!- ps_calcul_tsid
!- ps_calcul_potentiel
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- ps_generalites
!- ps_integration_don
!- ps_bulletin
!- cps_poles
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  ps_rotation_tsid ps_mat_rotation_tsid ps_calcul_tsid_RI_Rapp ps_calcul_tsid ps_calcul_potentiel
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use ps_generalites
  
  type PS_STR_INT_MODELE
     integer, dimension(4) :: ikle
     
     type(MSP_POTENTIEL)  :: pot
     type(MSP_ATMOSPHERE) :: atm
     type(MSP_MODVENT)    :: vent

     ! paramètres du modèle de potentiel (degrés zonaux / tesseraux)
     integer :: nzo
     integer :: nte
     
     ! tableau du potentiel utilisé pour les calculs des paramètres
     ! d'Eckstein-Hechler
     real(kind=pm_reel), dimension(:), pointer :: vj => NULL()

     ! mettre les paramètres physiques de la simulation (requa, mu, apla, vitroplaradsec)

     real(kind=pm_reel) :: requa
     real(kind=pm_reel) :: apla
     real(kind=pm_reel) :: gmu
     real(kind=pm_reel) :: vitrotpla

     ! pour le frottement atmosphérique
     integer :: calcul_vent
     character (LEN=80) :: modvent 

   end type PS_STR_INT_MODELE



   type PS_STR_INT_TROISCORPS
      ! type d'éphémérides : analytique ou tchebytchev-madona
      integer :: typephem

      ! flag indiquant que le mu a été chargé
      logical :: init_soleil
      logical :: init_lune
      logical :: init_phobos
      logical :: init_deimos
      
      ! mu des différents '3è corps'
      real (KIND=pm_reel) :: mu_soleil
      real (KIND=pm_reel) :: mu_lune
      real (KIND=pm_reel) :: mu_phobos
      real (KIND=pm_reel) :: mu_deimos

      ! fichier tchebytchev-madona
      character(LEN=80) :: ficept

   end type PS_STR_INT_TROISCORPS  

   type (PS_STR_INT_TROISCORPS), dimension(PS_NVMAX), save :: str_3co



   type (PS_STR_INT_MODELE), dimension(PS_NVMAX), save :: str_mod

   ! Mode analytique: utilisation de la méthode VSOP82 analytique pour Mars et Vénus
   integer,parameter :: PSI_METHODE_EPH_ANALYTIQUE = 10
   ! Mode analytique: utilisation de la méthode VSOP82 sur fichier (coeftche.dat) pour la Terre
   integer,parameter :: PSI_METHODE_EPH_ANALYTIQUE_FIC = 30
   ! Mode Tchebytchev: utilisation de la méthode COMPAS TCHEMAD théorie DE200
   ! Dans la base COMPAS, les fichiers éphémérides sont stockés dans DE200 pour la méthode TCHEMAD
   ! -> code COMPAS = 51
   integer,parameter :: PSI_METHODE_EPH_TCHEMAD = 51

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_calcul_forces.F90 69 2012-09-11 08:33:34Z ffsm $'

  contains

    subroutine ps_rotation_tsid(vec_in,tsid,vec_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_rotation_tsid
!
!$Resume
!  Effectue un changement de repère simple, en effectuant une rotation du temps sidéral
!
!$Description
!  Effectue un changement de repère simple, en effectuant une rotation du temps sidéral
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Usage
!  call ps_rotation_tsid(vec_in,tsid,vec_out)
!.    real(kind=pm_reel), dimension(3) :: vec_in
!.    real(kind=pm_reel) :: tsid
!.    real(kind=pm_reel), dimension(3) :: vec_out
!
!$Arguments
!>E     vec_in   :<pm_reel,DIM=(3)>   Vecteur dans un repère inertiel R0
!>E     tsid     :<pm_reel>           Temps sidéral entre R0 et R1 (angle entre R0 et R1 en radians)
!>S     vec_out  :<pm_reel,DIM=(3)>   Vecteur calculé dans le repère inertiel R1
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      implicit none

      ! Arguments
      !==========
      real(kind=pm_reel), dimension(3), intent(in) :: vec_in
      real(kind=pm_reel)              , intent(in) :: tsid
      real(kind=pm_reel), dimension(3), intent(out) :: vec_out

 
      ! Rotation du temps sidéral
      !-----------------------------------
      ! vec_out = M(tsid) * vec_in
      ! où M(tsid) = ( cos(tsid)   sin(tsid)    0
      !                -sin(tsid)   cos(tsid)    0
      !                0             0             1 )  
      ! En développant, on obtient :
  
      Vec_out(1) = Vec_in(1)*cos(tsid) + Vec_in(2)*sin(tsid)
      Vec_out(2) = - Vec_in(1)*sin(tsid) + Vec_in(2)*cos(tsid) 
      Vec_out(3) = Vec_in(3)

 
    end subroutine ps_rotation_tsid

    subroutine ps_mat_rotation_tsid(tsid,mat_rot)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_mat_rotation_tsid
!
!$Resume
!  Calcule de la matrice de rotation (3x3) pour une rotation du temps sidéral autour de l'axe z
!
!$Description
!  Calcule de la matrice de rotation (3x3) pour une rotation du temps sidéral autour de l'axe z
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_mat_rotation_tsid(tsid,mat_rot)
!.    real(kind=pm_reel) :: tsid
!.    real(kind=pm_reel), dimension(3,3) :: mat_rot
!
!$Arguments
!>E     tsid     :<pm_reel>             Angle de rotation (temps sidéral entre deux repères)
!>S     mat_rot  :<pm_reel,DIM=(3,3)>   Matrice de rotation pour le passage du repère R0 à R1 tel que R0 --(tsid)--> R1
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      implicit none
      
      ! Arguments
      !==========
      real(kind=pm_reel), intent(in) :: tsid
      real(kind=pm_reel), dimension(3,3), intent(out) :: mat_rot
      
      ! Variables locales
      !==================
      integer :: ii,jj
      
      ! Rotation du temps sidéral
      !-----------------------------------
      ! vec_out = M(tsid) * vec_in
      ! où M(tsid) = ( cos(tsid)    sin(tsid)      0
      !                -sin(tsid)   cos(tsid)      0
      !                0             0             1 )  
      
      ! Instanciation de M : 
      ! Construction de la matrice de rotation
      do ii=1,3
         do jj=1,3
            mat_rot(ii,jj) = 0._pm_reel
         end do
      end do

      mat_rot(1,1) = cos(tsid)
      mat_rot(1,2) = sin(tsid)
      mat_rot(2,1) = -sin(tsid)
      mat_rot(2,2) = cos(tsid)
      mat_rot(3,3) = 1._pm_reel

    end subroutine ps_mat_rotation_tsid

    subroutine ps_calcul_tsid_RI_Rapp(date,tsid)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_tsid_RI_Rapp
!
!$Resume
!  Calcule le temps sidéral entre le repère d'intégration (RI) et le repère à date courante
!
!$Description
!  Calcule le temps sidéral entre le repère d'intégration (RI) et le repère à date courante
!  Note : pour la Terre, le tsid est calculé avec mr_tsid_veis. Pour Mars et Vénus, tsid
!  est calculé en multipliant l'écart entre date et date_0 par la vitesse de rotation de la planète
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_calcul_tsid_RI_Rapp(date,tsid)
!.    type (tm_jour_sec) :: date
!.    real(kind=pm_reel) :: tsid
!
!$Arguments
!>E     date  :<tm_jour_sec>   Date en jj 1950 TE (jours/secondes) 
!>S     tsid  :<pm_reel>       Temps sidéral entre le repère d'intégration et le repère à date courante (Rapp)
!
!$Common
!
!$Routines
!- mr_tsid_veis
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!- ps_bulletin
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      use ps_integration_don
      use ps_bulletin

      implicit none

      ! Arguments
      !==========
      type (tm_jour_sec), intent(in) :: date
      real(kind=pm_reel), intent(out) :: tsid

      ! Variables locales
      !==================
      real(kind=pm_reel) :: tsid_absolu
      type (tm_code_retour) :: code_erreur
      type (tm_jour_sec) :: date_tuc


      if ( str_gen(iveh)%planet == eph_terre ) then
         
         date_tuc = date + str_bul(iveh)%ecart_te_tuc
         
         ! Appel à mr_tsid_veis pour connaitre le temps sidéral (terre)
         call mr_tsid_veis (date_tuc, 0._pm_reel,tsid_absolu,code_erreur)

         if (code_erreur%valeur < 0) then
            call MSP_signaler_message (ier_mslib=code_erreur)
            if (MSP_gen_messages("ps_calcul_tsid_RI_Rapp")) return
         end if
         tsid = tsid_absolu - str_int(iveh)%teta_RI_g50
      else
         
         ! Pour Venus et Mars, le temps sidéral est calculé
         ! grâce à la vitesse de rotation de la planète
         tsid = (date-str_bul(iveh)%datbul0_js)*str_mod(iveh)%vitrotpla
      endif


    end subroutine ps_calcul_tsid_RI_Rapp

    subroutine ps_calcul_tsid(date,tsid)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_tsid
!
!$Resume
!  Calcul du temps sidéral absolu 
!
!$Description
!  Calcul du temps sidéral absolu grâce à mr_tsid_veis pour la Terre
!  et grâce à COMPAS (théorie UAI1991) pour Mars et Vénus
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_calcul_tsid(date,tsid)
!.    type (tm_jour_sec) :: date
!.    real(kind=pm_reel) :: tsid
!
!$Arguments
!>E     date  :<tm_jour_sec>   Date jj 1950 TE (jours / secondes)
!>S     tsid  :<pm_reel>       Temps sidéral absolu
!
!$Common
!
!$Routines
!- mr_tsid_veis
!- MSP_signaler_message
!- cps_poletsid
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!- ps_bulletin
!- cps_poles
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      use ps_integration_don
      use ps_bulletin
      use cps_poles

      implicit none

      ! Arguments
      !==========
      type (tm_jour_sec), intent(in) :: date
      real(kind=pm_reel), intent(out) :: tsid

      ! Variables locales
      !==================
      type (tm_code_retour) :: code_erreur
      type (tm_jour_sec) :: date_tuc
      
      ! remarques : ces variables sont sorties par cps_poletsid
      ! mais non utilisées. 
      real(kind=pm_reel) :: alpha0, delta0, dtsid
      

      if ( str_gen(iveh)%planet == eph_terre ) then
         
         date_tuc = date + str_bul(iveh)%ecart_te_tuc
         
         ! Appel à mr_tsid_veis pour connaitre le temps sidéral (terre)
         call mr_tsid_veis (date_tuc, 0._pm_reel,tsid,code_erreur)

         if (code_erreur%valeur < 0) then
            call MSP_signaler_message (ier_mslib=code_erreur)
            if (MSP_gen_messages("ps_calcul_tsid")) return
         end if
      else
         
         ! appel à la routine COMPAS de calcul de temps sidéral avec
         ! le modèle UAI 2000
         call cps_poletsid(str_gen(iveh)%planet, date, alpha0, delta0, tsid, dtsid)
         if (MSP_gen_messages("ps_calcul_tsid")) return

      endif


    end subroutine ps_calcul_tsid

    subroutine ps_calcul_potentiel(Pos_rapp,second_membre_simplifie,Acc_Pot_rapp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_potentiel
!
!$Resume
!  Encapsule l'appel à MSP_calculer_potentiel, en gérant le 2nd membre simplifié
!
!$Description
!  Encapsule l'appel à MSP_calculer_potentiel, en gérant le 2nd membre simplifié
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_calcul_potentiel(Pos_rapp,second_membre_simplifie,Acc_Pot_rapp)
!.    real (KIND=pm_reel),dimension(3) :: Pos_rapp
!.    integer :: second_membre_simplifie 
!.    real (KIND=pm_reel),dimension(3) :: acc_pot_rapp
!
!$Arguments
!>E     Pos_rapp                 :<pm_reel,DIM=(3)>   Position dans le repère planétocentrique à date courante
!>E     second_membre_simplifie  :<integer>           Indicateur de prise en compte (=1) du 2nd membre simplifié. Les degrés
!                                                     tesseraux et zonaux sont limités.
!>S     Acc_Pot_rapp             :<pm_reel,DIM=(3)>   Accélération du potentiel dans le repère d'entrée
!
!$Common
!
!$Routines
!- MSP_modifier_potentiel
!- MSP_calculer_potentiel
!
!$Include
!
!$Module
!#V
!- ps_generalites
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites

      implicit none
      
      ! Arguments
      !==========

      real (KIND=pm_reel),dimension(3), intent(IN)  :: Pos_rapp
      integer, intent(in)                           :: second_membre_simplifie   
      real (KIND=pm_reel),dimension(3), intent(OUT) :: acc_pot_rapp


      ! Début du code
      !==============

      ! Gestion du 2nd membre simplifie : si activé, on limite
      ! les degrés zonaux et tesseraux à min(6,degre)
      ! Et str_mod(iveh)%nzo/nte sauvegardent les degrés 
      ! choisis par l'utilisateur
      if (second_membre_simplifie == 1) then
         call MSP_modifier_potentiel(str_mod(iveh)%pot,nte=min(str_mod(iveh)%nte,6),nzo=min(str_mod(iveh)%nzo,6))
      end if

      call MSP_calculer_potentiel (str_mod(iveh)%pot,Pos_Rapp,Acc_pot_Rapp)
      if (MSP_gen_messages("ps_force")) return

      if (second_membre_simplifie == 1) then
         call MSP_modifier_potentiel(str_mod(iveh)%pot,nzo=str_mod(iveh)%nzo,nte=str_mod(iveh)%nte) 
      end if

     end subroutine ps_calcul_potentiel

end module ps_calcul_forces
