module type_mspro

! (C) Copyright CNES - MSPRO - 2000-2005

!************************************************************************
!
! But:  Definition des types derives MSPRO. 
! ===
!
! Note d'utilisation: Ce module est accessible a l'utilisateur via le module mspro.
! ==================
!
!$Historique
! ==========
!   + Version 0.1 : creation
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 1) : Mise dans un ordre logique l'apparition des champs 
!                          dans la structure (cf volume 3) et ajout de sequence.
!                          Ajout de nouveaux types derives.
!                         (Date: 04/2002 - Realisation: Guylaine Prat et Mickael Hazak)
!   + Version 3.0 (DE 2) : ajout de nouveaux types derives
!                         (Date: 10/2002 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE globale 7) : ajout des parametres perigee/apogee
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 9) : ajout des reperes interplanetaires dans le theme X
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 5.1 (DE 3) : ajout de nouveaux types derives
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE globale 14) : ajout du type coordonnees orbitales hyperbolique
!                         (Date: 10/2004 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE 4) : ajout de nouveaux types derives lies aux integrateurs
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 (DM 408) : ajout de l integrateur Cowell
!                         (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale)
!   + Version 5.4 : DM-ID 472 : Amelioration de la detection d evenement
!                         (Date: 02/2006 - Realisation: Atos Origin)
!                   DM-ID 492 : optimisation de l'utilisation du critere de convergence de
!                               l'integrateur de Cowell
!                         (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                         (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.6 : DM-ID 516 : détection d'évènements simultanés par les intégrateurs MSPRO
!                         (Date: 09/2006 - Réalisation : Atos Origin) 
!   + Version 5.7 : DM-ID 738 : Evolution du Cowell
!                         (Date: 06/2007- Realisation: Sandrine Avril- Atos origin)
!   + Version 5.9 : DM-ID 983 : Epsilons specifiques pour la variable d'intégration de
!                               l'intégrateur de Cowell
!                         (Date: 03/2008- Réalisation: Yannick Tanguy- ATOS Origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

use mslib

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: type_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


type tm_spher              ! type coordonnees spheriques

   sequence
   real(pm_reel) :: lat    ! latitude
   real(pm_reel) :: long   ! longitude
   real(pm_reel) :: dist   ! distance

end type tm_spher

type tm_vit_meca_vol        ! type vitesse mecanique du vol

   sequence
   real(pm_reel) :: pente   ! pente vecteur vitesse (selon l'horizontale locale)
   real(pm_reel) :: azimut  ! azimut vecteur vitesse (selon le nord local)  
   real(pm_reel) :: norme   ! norme vecteur vitesse

end type tm_vit_meca_vol

type tm_vit_gps_ard     ! type vitesse pour le recepteur GPS de la capsule spatiale ARD

   sequence
   real(pm_reel) :: sn  ! vitesse selon l'axe Sud-Nord local oriente du Sud vers le Nord
   real(pm_reel) :: oe  ! vitesse selon l'axe Ouest-Est local oriente de l'Ouest vers Est
   real(pm_reel) :: hb  ! vitesse selon la verticale locale orientee du haut vers le bas

end type tm_vit_gps_ard

type tm_pole_uv         ! type coordonnees du pole vrai (en radians)

   sequence
   real(pm_reel) :: u   ! coordonnee u du pole vrai
   real(pm_reel) :: v   ! coordonnee v du pole vrai

end type tm_pole_uv

type tm_ellipsoide          ! type caracteristiques de l'ellipsoide du corps considere

   sequence
   real(pm_reel) :: r_equa  ! rayon equatorial (m)
   real(pm_reel) :: apla    ! aplatissement (sans unite)

end type tm_ellipsoide

type tm_def_topo                 ! type definition d'un repere topocentrique

   sequence
   type(tm_geodesique) :: geod   ! coordonnees geodesiques de l'origine du repere
   type(tm_ellipsoide) :: ellips ! caracteristiques de l'ellipsoide du corps considere
   real(pm_reel)       :: axe_X  ! angle de l'axe X du repere par rapport au Nord local (rad)
   ! angle = 0 si Nord; +pi/2 si Est; 
   !         -pi/4 Nord-Ouest; -pi ou +pi si Sud....

end type tm_def_topo

! ce type n'est pas connu de l'utilisateur
type tm_orb_hpha           ! type des parametres perigee/apogee

   sequence
   real(pm_reel) :: hp   ! altitude du perigee
   real(pm_reel) :: ha   ! altitude de l'apogee
   real(pm_reel) :: i    ! inclinaison
   real(pm_reel) :: pom  ! argument du perigee
   real(pm_reel) :: gom  ! longitude du noeud ascendant
   real(pm_reel) :: M    ! anomalie moyenne

end type tm_orb_hpha

type tm_orb_Bplane           ! type des parametres de Bplane

   sequence
   real(pm_reel) :: BR   ! composante suivant R du point d'impact B
   real(pm_reel) :: BT   ! composante suivant T du point d'impact B
   real(pm_reel) :: LTF  ! Linearized Time-of-Flight
   real(pm_reel) :: RAI  ! Ascension droite du vecteur vitesse infinie arrivee dans le repere courant
   real(pm_reel) :: DECI ! Declinaison du vecteur vitesse infinie arrivee dans le repere courant
   real(pm_reel) :: C3   ! Carre de la norme du vecteur vitesse infinie

end type tm_orb_Bplane

type tm_orb_Vinf         ! type des parametres orbitaux hyperboliques

   sequence
   real(pm_reel) :: C3   ! carre de la norme du vecteur vitesse infinie
   real(pm_reel) :: DLA  ! declinaison du vecteur vitesse infinie depart
   real(pm_reel) :: RLA  ! ascension droite du vecteur vitesse infinie depart
   real(pm_reel) :: Rp   ! rayon du periastre
   real(pm_reel) :: gom  ! longitude du noeud ascendant
   real(pm_reel) :: M    ! anomalie moyenne

end type tm_orb_Vinf

type tm_pole_tsid_planete         ! type definition du pole de rotation et du meridien origine d'une planete

   sequence
   real(pm_reel)       :: alpha0 ! ascension droite du pole
   real(pm_reel)       :: delta0 ! declinaison du pole
   real(pm_reel)       :: W      ! temps sideral (=longitude du meridien origine)
   real(pm_reel)       :: dW     ! derivee du temps sideral (= vitesse de rotation dela planete)

end type tm_pole_tsid_planete

type tm_g_commut  ! type (interne) decrivant une subroutine de commutation pour les integrateurs

   sequence
   integer       :: adresse        ! adresse de la subroutine
   integer       :: ident          ! identificateur de la subroutine
   real(pm_reel) :: max_deltat     ! ecart max entre 2 verifications de commutation
   real(pm_reel) :: eps_converg    ! tolerance de convergence
   integer       :: action         ! action a effectuer si evenement
   real(pm_reel) :: g0             ! valeur de g a t0
   logical       :: g0positif      ! signe de g0 (reel ou simule)
   logical       :: prem_even      ! indique s'il s'agit du 1er evenement 
   logical       :: even_attendu   ! indique si un evenement est prevu
   real(pm_reel) :: t_even_attendu ! date de cet evenement
   real(pm_reel) :: t_last_even    ! date de l'evenement precedent
   integer       :: nb_commut      ! nombre de commutations

end type tm_g_commut

type tm_integrateur ! type contenant les donnees d'un integrateur

   sequence
   integer                                :: type          ! type de l'integrateur
   logical                                :: pas_variable  ! indique si l'integrateur est a pas variable
   integer                                :: nb_etapes     ! nb d'etapes de l'integrateur
   integer                                :: ordre         ! ordre de l'integrateur
   integer                                :: n             ! dimension du vecteur d'etat
   real(pm_reel)                          :: pas           ! pas d'integration (si pas fixe)
   real(pm_reel)                          :: pas_min       ! pas d'integration minimum (si pas variable)
   real(pm_reel)                          :: pas_max       ! pas d'integration maximum (si pas variable)
   real(pm_reel)                          :: eps_init      ! epsilon d'intialisation de l'integration pour les variables du mouvement (Cowell)
   real(pm_reel)                          :: eps_prog      ! epsilon de progression de l'intégration pour les variables du mouvement (Cowell)
   real(pm_reel)                          :: eps_init_var_integ ! epsilon d'intialisation pour la variable d'intégration (Cowell)
   real(pm_reel)                          :: eps_prog_var_integ ! epsilon de progression pour la variable d'intégration (Cowell)
   real(pm_reel),dimension(:),pointer     :: tol_rel       ! tableau des tolerances relatives (si pas variable)
   real(pm_reel),dimension(:),pointer     :: tol_abs       ! tableau des tolerances absolues (si pas variable)
   real(pm_reel),dimension(:,:),pointer   :: a             ! parametres a du tableau de Butcher
   real(pm_reel),dimension(:),pointer     :: b             ! parametres b du tableau de Butcher
   real(pm_reel),dimension(:),pointer     :: c             ! parametres c du tableau de Butcher
   logical                                :: report_fin_deb! on peut ou non reporter les calculs fin de pas -> debut du pas suivant
   integer                                :: nb_commut     ! nb de subroutines de commutations
   integer                                :: size_commut   ! nb d'elements deja alloues
   type(tm_g_commut),dimension(:),pointer :: g_commut      ! subroutines de commutation
   integer                                :: adr_gest_pas  ! adresse du gestionnaire de pas
   real(pm_reel),dimension(:,:),pointer   :: yDotK         ! tableau des coefficients d'etat
   real(pm_reel)                          :: t_deb         ! debut de pas 
   real(pm_reel)                          :: t_fin         ! fin de pas
   type(tm_jour_sec)                      :: joursec_fin   ! fin de pas en jour et secondes
   real(pm_reel),dimension(:),pointer     :: y_fin         ! etat en fin de pas
   real(pm_reel),dimension(:,:),pointer   :: v             ! vecteurs d'interpolation (pas variable)
   logical                                :: v_initialise  ! indique si v est initialise ou non (pas variable)
   integer                                :: icirc		! cle de circularisation
   integer                                :: ireg		! cle de regularisation (chgt de variable temps)
   real(pm_reel)                          :: rxmu		! constante mu
   logical                                :: iteratif      ! indique mode iteratif/non iteratif
   integer, dimension(6)                  :: liber_param   ! indique les paramètres à liberer
   real(pm_reel) :: theta0  ! partie constante de l'equation du temps
   real(pm_reel) :: a0      ! 1/2 grand axe osculateur a l'origine
   real(pm_reel) :: c02     ! cste des aires au carre , a l'origine
   real(pm_reel) :: anv0    ! anomalie vraie osculatrice a l'origine
   real(pm_reel) :: param   ! parametre de l'ellipse osculatrice a l'origine
   real(pm_reel) :: exc     ! excentricite osculatrice a l'origine
   real(pm_reel) :: xn      ! moyen mouvement
   real(pm_reel) :: ume2    ! utile a ckeptp et cowreg
   real(pm_reel) :: rume2   ! utile a ckeptp et cowreg
   real(pm_reel), dimension(:,:),pointer  :: corr   ! coeff de correction de la circularisation
   real(pm_reel) :: omeg                     ! pulsation du mouvement periodique principal
   real(pm_reel) :: sec  ! secondes dans le jour
   integer :: jj         ! jour julien date initiale
   real(pm_reel) :: d     
   real(pm_reel) :: dp   
   real(pm_reel), dimension(:,:),pointer  :: alpha ! Coefficients pour la circularisation
   real(pm_reel), dimension(:,:),pointer  :: beta  ! Coefficients pour la circularisation
   real(pm_reel), dimension(:,:),pointer :: spys   ! Ecarts entre modèle complet et modèle simplifié (écarts sur les accélérations)
   real(pm_reel) :: tm      ! date courante du point milieu du support d'intégration / 1er point du support d'interpolation (point le plus 'en avant')
   real(pm_reel) :: tm_old  ! tm à l'appel précédent de l'intégrateur
   integer :: iter          ! nb d'itérations effectuées : iter = 0 --> pas initialisé
   integer :: ihyp          ! indique si la trajectoire est hyperbolique
   real(pm_reel), dimension(:,:),pointer :: cys   ! tableau des données à intégrer (positions)  
   real(pm_reel), dimension(:,:),pointer :: cyps  ! tableau des données à intégrer (vitesses)
   real(pm_reel), dimension(:,:),pointer :: cypsi ! tableau des données à interpoler (vitesses)
   real(pm_reel), dimension(:,:),pointer :: cysi  ! tableau des données à interpoler (positions)
   real(pm_reel) :: s      ! variable independante
   real(pm_reel), dimension(:,:),pointer :: dels  ! tableau des dérivées secondes
   integer :: nn  ! ordre integration / 2
   integer :: nd  ! 9-nn
   integer :: nf  ! 7+nn
   integer :: nnd  ! nd+1
   integer :: nnf  ! nf+2
   integer :: nfp  ! nf+1
   integer :: adr_fcowell_simp ! adresse de la fonction simplifié pour le cowell
   logical :: calcul_masse ! booleen indiquant si l'équation de masse est à prendre en compte 
   integer :: nb_param_lib ! nombre de paramètres libérés

end type tm_integrateur

!!$ DM_ID 516
type tm_evt_commut
   
   sequence
   integer :: ident          ! identifiant de la routine qui commute (la valeur de la fonction passe par 0)
   real(pm_reel) :: abscisse ! abscisse du point de pasage à 0

end type tm_evt_commut

!................................................................................................................

character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
     '@(#) Fichier MSPRO type_mspro.f90: derniere modification V5.15 >'

!................................................................................................................

end module type_mspro
