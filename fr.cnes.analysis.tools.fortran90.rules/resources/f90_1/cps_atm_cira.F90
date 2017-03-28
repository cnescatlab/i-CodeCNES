module cps_atm_cira_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_cira_mod
!
!$Resume
!   Modele d'atmosphere terrestre CIRA.
!
!$Description
!   Modele d'atmosphere terrestre CIRA
!
!$Auteur
!   Julien Bouillant (Atos Origin)
!
!$Version
!  $Id: cps_atm_cira.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_atm_cira.F90,v $
!  Revision 1.7  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.6  2009/09/22 14:23:35  cmartel
!  DM-ID 1312 : Optimisation du modele d'atmosphere CIRA
!
!  Revision 1.5  2009/01/28 11:02:14  cml
!  FA-ID 1226 : Passage des tableaux en double precision
!
!  Revision 1.4  2008/10/14 07:47:12  cml
!  DM-ID 1058 : Suppression de cibles non utilisees
!
!  Revision 1.3  2007/11/13 16:52:04  sbd
!  FA-ID 827 suppression variables inutilisees
!
!  Revision 1.2  2006/05/12 12:05:54  bouillaj
!  Amelioration qualite : complements sur les cartouches
!
!  Revision 1.1  2006/01/30 09:06:21  bouillaj
!  Creation a partir de la routine MSPRO mp_atm_cira
!
!
!$FinHistorique
!
!$Usage
!  use cps_atm_cira_mod
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_atm_cira
!- cps_atmi
!- cps_atmi_janvier
!- cps_atmi_fevrier
!- cps_atmi_mars
!- cps_atmi_avril
!- cps_atmi_mai
!- cps_atmi_juin
!- cps_atmi_juillet
!- cps_atmi_aout
!- cps_atmi_septembre
!- cps_atmi_octobre
!- cps_atmi_novembre
!- cps_atmi_decembre
!- cps_atmi_inter
!- cps_atmi_alt
!- cps_atmi_temp
!- cps_atmo
!
!$Fonctions
!- cps_interp_newton
!
!$Include
!
!$Module
!#V
!- mslib
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
!.  cps_interp_newton cps_atm_cira cps_atmi cps_atmi_janvier cps_atmi_fevrier cps_atmi_mars
!.  cps_atmi_avril cps_atmi_mai cps_atmi_juin cps_atmi_juillet cps_atmi_aout cps_atmi_septembre
!.  cps_atmi_octobre cps_atmi_novembre cps_atmi_decembre cps_atmi_inter cps_atmi_alt cps_atmi_temp
!.  cps_atmo
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_cira.F90 69 2012-09-11 08:33:34Z ffsm $'

  
contains

  subroutine cps_atm_cira (mois, pos_geod, tempe, pres, dens, code_retour)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atm_cira
!
!$Resume
!Ce modele d'atmosphere est limite a des:
!                                * 0 < altitudes < 120 km
!                                * -80 < latitudes < +80 degres
!          Forcage des valeurs de la facon suivante:
!                    * si altitude < 0          ==> altitude = 0
!                    * si latitude > +80 degres ==> latitude = 80 deg
!                    * si latitude < -80 degres ==> latitude = -80 deg
!
!$Description
!Ce modele d'atmosphere est limite a des:
!                                * 0 < altitudes < 120 km
!                                * -80 < latitudes < +80 degres
!          Forcage des valeurs de la facon suivante:
!                    * si altitude < 0          ==> altitude = 0
!                    * si latitude > +80 degres ==> latitude = 80 deg
!                    * si latitude < -80 degres ==> latitude = -80 deg
!
!$Auteur
!   Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atm_cira (mois, pos_geod, tempe, pres, dens, code_retour)
!.    integer :: mois 
!.    type(tm_geodesique) :: pos_geod 
!.    real(pm_reel) :: tempe 
!.    real(pm_reel) :: pres 
!.    real(pm_reel) :: dens 
!.    type(tm_code_retour) :: code_retour
!
!$Arguments
!>E     mois         :<integer>          mois choisi pour le calcul
!>E     pos_geod     :<tm_geodesique>    position géodésique
!>S     tempe        :<pm_reel>          température (kelvin)
!>S     pres         :<pm_reel>          pression (Pa)
!>S     dens         :<pm_reel>          densité (kg.m-3)
!>S     code_retour  :<tm_code_retour>   code retour
!
!$Common
!
!$Routines
!- cps_atmi
!- cps_atmo
!
!$Include
!
!$Module
!#V
!- mslib
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
    


    ! Modules
    ! =======
    use mslib
    
   
    ! Declarations
    ! ============
    implicit none
    
    integer, intent(in)                            :: mois     ! mois de l'annee
    type(tm_geodesique), intent(in)                :: pos_geod ! coordonnees geodesiques
    real(pm_reel), intent(out)                     :: tempe    ! temperature
    real(pm_reel), intent(out)                     :: pres     ! pression
    real(pm_reel), intent(out)                     :: dens     ! densite
    type(tm_code_retour), intent(out)              ::  code_retour
  
    
    ! Autres declarations
    ! ===================
    
    real(pm_reel) :: rlong1, rlat1, ralti1 ! variables intermediaires de calcul pour la longitude
    ! la latitude et l'altitude

    real(pm_reel) :: alt_max = 120.e+03_pm_reel       ! 120 km (en metres)
    real(pm_reel) :: lat_borne = 1.396263_pm_reel     ! 79.999977 < 80 degres (en radians)
    real(pm_reel) :: trans_m_km = 1000._pm_reel       ! facteur entre metres et km
    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)


    real(pm_reel), dimension(nb_alt) :: rtarra, rzarra, rpress! sorties de cps_atmi
    real(pm_reel)                       :: rpres, rtemp, rdens
    
    integer       :: retour                ! code retour des routines internes

    !************************************************************************
    
    ! initialisations
    ! ===============
    
    code_retour%valeur = pm_OK
    
    
    ! verification des arguments d'entree
    ! ===================================
    
    if (mois < 1 .or. mois > 12) then ! probleme sur le mois
       
!       code_retour%valeur = pm_err_mois
       go to 6000
       
    end if

    if (pos_geod%haut > alt_max) then ! altitude superieure a 120 km
       
!       code_retour%valeur = pm_err_alt_sup120km
       go to 6000
       
    end if
    
    
    
    ! transformation des (rad, m) en (degres, km)
    ! avec forcage de valeurs si hors des bornes
    ! ===========================================
    
    rlong1 = pos_geod%long * pm_rad_deg
    
    
    if ( abs(pos_geod%lat) > lat_borne) then  ! latitude non comprise dans  ]-80, +80[
       
       rlat1 = sign(1._pm_reel,pos_geod%lat)*lat_borne ! on force la valeur de la latitude
       !  a une valeur proche de +/- 80 
       ! (suivant le signe de la latitude)
!       code_retour%valeur = pm_warn_lat_cira
       
    else ! latitude dans ]-80, +80[
       
       rlat1  = pos_geod%lat
       
    end if
    
    rlat1 = rlat1 * pm_rad_deg
    
    if (pos_geod%haut < 0._pm_reel) then ! altitude negative
       
       ralti1 = 0._pm_reel  ! on force a zero l'altitude
!       code_retour%valeur = pm_warn_alt_negatif
       
    else  ! altitude positive
       
       ralti1 = pos_geod%haut / trans_m_km
       
    end if
    
    
    
    ! interpolation sur le fichier des donnees
    ! ========================================
    
    call cps_atmi (rlat1,rlong1,mois,rtarra,rzarra,rpress,retour)
    
    if (retour /= pm_OK) then
       
!       code_retour%valeur = retour
       if (retour < pm_OK) go to 6000
       
    end if
    
    
    ! calcul du modele atmospherique cira
    ! ===================================
    
    call cps_atmo (ralti1,rtarra,rzarra,rpress,rtemp,rpres,rdens,retour)
    
    if (retour /= pm_OK) then
       
!       code_retour%valeur = retour
       if (retour < pm_OK) go to 6000
       
    end if

    
    ! affectation des sorties
    ! =======================
    tempe = rtemp
    pres  = rpres * 100._pm_reel  ! calculs faits en mbar ==> conversion en Pascal
    dens  = rdens
    
    
6000 continue
    
!    code_retour%routine = pm_num_mp_atm_cira
!    code_retour%biblio = pm_mspro
    if (code_retour%valeur /= pm_OK) code_retour%message = ' '
    
  end subroutine cps_atm_cira

  subroutine cps_atmi ( lat, long, mois, tarra, zarra, press, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi
!
!$Resume
!   Pour le modele ATMospherique cira, preparation de l'Interpolation.
!
!$Description
!   Pour le modele ATMospherique cira, preparation de l'Interpolation.
!
!$Auteur
!   Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi ( lat, long, mois, tarra, zarra, press, retour )
!.    real(pm_reel) :: lat 
!.    real(pm_reel) :: long 
!.    integer :: mois 
!.    real(pm_reel), dimension(:) :: tarra
!.    real(pm_reel), dimension(:) :: zarra
!.    real(pm_reel), dimension(:) :: press
!.    integer :: retour 
!
!$Arguments
!>E     lat     :<pm_reel>           latitude (rad)
!>E     long    :<pm_reel>           longitude (rad)
!>E     mois    :<integer>           mois de l'année
!>S     tarra   :<pm_reel,DIM=(:)>   interpolation des temperatures
!>S     zarra   :<pm_reel,DIM=(:)>   interpolation des altitudes
!>S     press   :<pm_reel,DIM=(:)>   tableau des pressions
!>S     retour  :<integer>           
!
!$Common
!
!$Routines
!- cps_atmi_janvier
!- cps_atmi_fevrier
!- cps_atmi_mars
!- cps_atmi_avril
!- cps_atmi_mai
!- cps_atmi_juin
!- cps_atmi_juillet
!- cps_atmi_aout
!- cps_atmi_septembre
!- cps_atmi_octobre
!- cps_atmi_novembre
!- cps_atmi_decembre
!- cps_atmi_inter
!
!$Include
!
!$Module
!#V
!- mslib
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
    

    ! Modules
    ! =======

    use mslib

    ! Declarations
    ! ============
    implicit none
 
    real(pm_reel), intent(in) :: lat  ! latitude
    real(pm_reel), intent(in) :: long ! longitude
    integer, intent(in)       :: mois ! mois de l'annee
    real(pm_reel), dimension(:), intent(out):: tarra! interpolation des temperatures
    real(pm_reel), dimension(:), intent(out):: zarra! interpolation des altitudes
    real(pm_reel), dimension(:), intent(out):: press! tableau des pressions
    integer, intent(out)      :: retour  

    ! Autres declarations
    ! -------------------
    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi) 
    
    real(pm_reel), save, dimension(nb_alt, nb_lat) :: tbar, zbar   ! temperatures, altitudes 
    real(pm_reel), save, dimension(nb_alt, nb_lat) :: z1, phi1     ! amplitude et phase de l'altitude de l'onde 1
    real(pm_reel), save, dimension(nb_alt, nb_lat) :: t1, phit1    ! amplitude et phase de temperature de l'onde 1
    real(pm_reel), save, dimension(nb_alt, nb_lat) :: z2, phi2     ! amplitude et phase de l'altitude de l'onde 2
    real(pm_reel), save, dimension(nb_alt, nb_lat) :: t2, phit2    ! amplitude et phase de temperature de l'onde 2
    integer                                        :: ialt         ! indice de boucle du calcul des pressions

    integer, save :: dernier_mois = -1    ! Mois correspondant au dernier appel
    
    !************************************************************************
    
    ! initialisation de la valeur du code retour
    ! ..........................................
    retour = pm_OK
    
    do  ialt = 1 , nb_alt , 1
       press (ialt) = 1013._pm_reel * exp (-.5_pm_reel * real( (ialt-1), pm_reel))
    end do
    
    if ( mois /= dernier_mois ) then
    select case (mois)
    case (1) 
       call cps_atmi_janvier(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (2) 
       call cps_atmi_fevrier(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (3) 
       call cps_atmi_mars(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (4) 
       call cps_atmi_avril(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (5) 
       call cps_atmi_mai(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (6) 
       call cps_atmi_juin(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (7) 
       call cps_atmi_juillet(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (8) 
       call cps_atmi_aout(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (9) 
       call cps_atmi_septembre(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (10) 
       call cps_atmi_octobre(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (11) 
       call cps_atmi_novembre(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case (12) 
       call cps_atmi_decembre(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
    case default
!       retour = pm_err_mois
    end select
    if (retour < pm_OK) go to 6000
    dernier_mois=mois
    endif
    
    call cps_atmi_inter(lat,long,tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,tarra,zarra,retour)
    
    
6000 continue
    
    
  end subroutine cps_atmi

  subroutine cps_atmi_janvier (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_janvier
!
!$Resume
!   Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JANVIER
!
!$Description
!   Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JANVIER
!
!$Auteur
!   Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_janvier (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2
!>S     retour  :<integer>             
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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
 
    ! Modules
    ! =======
    
    use mslib
  
  ! Declarations
  ! ============
    implicit none
    
    real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
    real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
    real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
    real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
    real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
    real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
    real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
    real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
    real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
    real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
    integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------
    
    real(pm_r), dimension(10*nb_alt), parameter :: donnees_lat_moins_80 = (/ &   ! donnees latitude -80
264.20_pm_r,  -65._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r, 0._pm_r,   0._pm_r,  0._pm_r,  0._pm_r,  0._pm_r,  &
248.90_pm_r,  3680._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
227.20_pm_r,  7155._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
228.00_pm_r, 10477._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
231.80_pm_r, 13833._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
233.60_pm_r, 17242._pm_r,   2._pm_r,222._pm_r,  0.02_pm_r,288._pm_r,   1._pm_r,133._pm_r,  0.07_pm_r,287._pm_r, & 
234.80_pm_r, 20673._pm_r,   2._pm_r,223._pm_r,  0.04_pm_r,  0._pm_r,   1._pm_r,140._pm_r,  0.09_pm_r,276._pm_r, & 
235.40_pm_r, 24116._pm_r,   2._pm_r,225._pm_r,  0.09_pm_r, 27._pm_r,   0._pm_r,156._pm_r,  0.12_pm_r,263._pm_r, & 
238.10_pm_r, 27577._pm_r,   1._pm_r,227._pm_r,  0.17_pm_r, 33._pm_r,   0._pm_r,182._pm_r,  0.14_pm_r,249._pm_r, & 
249.10_pm_r, 31139._pm_r,   1._pm_r,230._pm_r,  0.24_pm_r, 34._pm_r,   1._pm_r,201._pm_r,  0.16_pm_r,231._pm_r, & 
261.70_pm_r, 34880._pm_r,   1._pm_r,238._pm_r,  0.28_pm_r, 35._pm_r,   1._pm_r,208._pm_r,  0.19_pm_r,216._pm_r, & 
273.30_pm_r, 38802._pm_r,   0._pm_r,264._pm_r,  0.29_pm_r, 34._pm_r,   1._pm_r,209._pm_r,  0.21_pm_r,204._pm_r, & 
283.00_pm_r, 42876._pm_r,   0._pm_r,332._pm_r,  0.26_pm_r, 29._pm_r,   1._pm_r,206._pm_r,  0.24_pm_r,195._pm_r, & 
288.30_pm_r, 47067._pm_r,   1._pm_r,355._pm_r,  0.14_pm_r, 12._pm_r,   2._pm_r,203._pm_r,  0.29_pm_r,188._pm_r, & 
287.20_pm_r, 51289._pm_r,   1._pm_r,354._pm_r,  0.08_pm_r,284._pm_r,   2._pm_r,200._pm_r,  0.40_pm_r,196._pm_r, & 
279.10_pm_r, 55439._pm_r,   1._pm_r,339._pm_r,  0.18_pm_r,232._pm_r,   3._pm_r,200._pm_r,  0.48_pm_r,203._pm_r, & 
267.30_pm_r, 59446._pm_r,   1._pm_r,312._pm_r,  0.26_pm_r,212._pm_r,   4._pm_r,201._pm_r,  0.50_pm_r,208._pm_r, & 
253.40_pm_r, 63257._pm_r,   1._pm_r,280._pm_r,  0.20_pm_r,212._pm_r,   4._pm_r,202._pm_r,  0.30_pm_r,210._pm_r, & 
238.50_pm_r, 66864._pm_r,   1._pm_r,266._pm_r,  0.10_pm_r,253._pm_r,   5._pm_r,203._pm_r,  0.08_pm_r,240._pm_r, & 
222.50_pm_r, 70236._pm_r,   1._pm_r,271._pm_r,  0.16_pm_r,318._pm_r,   4._pm_r,204._pm_r,  0.16_pm_r,  2._pm_r, & 
206.20_pm_r, 73381._pm_r,   1._pm_r,284._pm_r,  0.27_pm_r,332._pm_r,   4._pm_r,205._pm_r,  0.32_pm_r,  9._pm_r, & 
189.90_pm_r, 76277._pm_r,   2._pm_r,298._pm_r,  0.34_pm_r,336._pm_r,   4._pm_r,208._pm_r,  0.40_pm_r,  9._pm_r, & 
174.10_pm_r, 78945._pm_r,   2._pm_r,308._pm_r,  0.35_pm_r,338._pm_r,   3._pm_r,211._pm_r,  0.41_pm_r, 10._pm_r, & 
160.80_pm_r, 81385._pm_r,   2._pm_r,314._pm_r,  0.33_pm_r,338._pm_r,   3._pm_r,216._pm_r,  0.37_pm_r,  9._pm_r, & 
148.10_pm_r, 83647._pm_r,   3._pm_r,318._pm_r,  0.28_pm_r,339._pm_r,   2._pm_r,223._pm_r,  0.32_pm_r,  9._pm_r, & 
138.50_pm_r, 85692._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
136.70_pm_r, 87670._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
140.30_pm_r, 89691._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
148.60_pm_r, 91808._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
162.10_pm_r, 94093._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
182.90_pm_r, 96640._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
214.30_pm_r, 99604._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
257.70_pm_r,103173._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
295.50_pm_r,107454._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
319.40_pm_r,112258._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
404.50_pm_r,117991._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70 = (/ &      ! donnees latitude -70
270.00_pm_r,  -112._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
251.50_pm_r,  3696._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
228.80_pm_r,  7203._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.60_pm_r, 10528._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
230.50_pm_r, 13866._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
232.50_pm_r, 17259._pm_r,   4._pm_r,215._pm_r,  0.19_pm_r, 33._pm_r,   1._pm_r, 92._pm_r,  0.06_pm_r,209._pm_r, & 
233.30_pm_r, 20671._pm_r,   3._pm_r,215._pm_r,  0.29_pm_r, 34._pm_r,   1._pm_r,101._pm_r,  0.09_pm_r,208._pm_r, & 
233.50_pm_r, 24087._pm_r,   3._pm_r,215._pm_r,  0.38_pm_r, 33._pm_r,   1._pm_r,115._pm_r,  0.10_pm_r,207._pm_r, & 
237.20_pm_r, 27526._pm_r,   2._pm_r,216._pm_r,  0.44_pm_r, 33._pm_r,   1._pm_r,130._pm_r,  0.11_pm_r,205._pm_r, & 
248.40_pm_r, 31077._pm_r,   2._pm_r,217._pm_r,  0.44_pm_r, 33._pm_r,   1._pm_r,143._pm_r,  0.09_pm_r,206._pm_r, & 
260.30_pm_r, 34802._pm_r,   1._pm_r,219._pm_r,  0.40_pm_r, 33._pm_r,   1._pm_r,152._pm_r,  0.06_pm_r,209._pm_r, & 
271.10_pm_r, 38698._pm_r,   1._pm_r,225._pm_r,  0.32_pm_r, 34._pm_r,   1._pm_r,156._pm_r,  0.03_pm_r,225._pm_r, & 
280.40_pm_r, 42736._pm_r,   0._pm_r,253._pm_r,  0.22_pm_r, 32._pm_r,   1._pm_r,158._pm_r,  0.02_pm_r,326._pm_r, & 
284.90_pm_r, 46884._pm_r,   0._pm_r,352._pm_r,  0.17_pm_r, 28._pm_r,   1._pm_r,159._pm_r,  0.07_pm_r,327._pm_r, & 
283.20_pm_r, 51051._pm_r,   0._pm_r,  6._pm_r,  0.10_pm_r,  6._pm_r,   1._pm_r,162._pm_r,  0.13_pm_r,333._pm_r, & 
275.60_pm_r, 55146._pm_r,   1._pm_r,  3._pm_r,  0.03_pm_r,299._pm_r,   0._pm_r,161._pm_r,  0.14_pm_r,359._pm_r, & 
264.60_pm_r, 59108._pm_r,   0._pm_r,358._pm_r,  0.09_pm_r,193._pm_r,   0._pm_r,117._pm_r,  0.16_pm_r, 46._pm_r, & 
251.50_pm_r, 62884._pm_r,   0._pm_r,353._pm_r,  0.12_pm_r,179._pm_r,   0._pm_r, 82._pm_r,  0.21_pm_r, 66._pm_r, & 
237.50_pm_r, 66470._pm_r,   0._pm_r,345._pm_r,  0.12_pm_r,175._pm_r,   1._pm_r, 75._pm_r,  0.22_pm_r, 64._pm_r, & 
222.90_pm_r, 69838._pm_r,   0._pm_r,198._pm_r,  0.04_pm_r,180._pm_r,   1._pm_r, 69._pm_r,  0.17_pm_r, 41._pm_r, & 
207.50_pm_r, 72995._pm_r,   0._pm_r,215._pm_r,  0.05_pm_r,328._pm_r,   1._pm_r, 61._pm_r,  0.16_pm_r,  7._pm_r, & 
191.90_pm_r, 75915._pm_r,   0._pm_r,310._pm_r,  0.10_pm_r,335._pm_r,   1._pm_r, 50._pm_r,  0.20_pm_r,343._pm_r, & 
176.70_pm_r, 78617._pm_r,   0._pm_r,326._pm_r,  0.13_pm_r,334._pm_r,   1._pm_r, 38._pm_r,  0.21_pm_r,335._pm_r, & 
163.80_pm_r, 81098._pm_r,   1._pm_r,330._pm_r,  0.14_pm_r,335._pm_r,   2._pm_r, 27._pm_r,  0.21_pm_r,328._pm_r, & 
152.10_pm_r, 83407._pm_r,   1._pm_r,331._pm_r,  0.13_pm_r,336._pm_r,   2._pm_r, 18._pm_r,  0.19_pm_r,325._pm_r, & 
143.80_pm_r, 85533._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
142.10_pm_r, 87597._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
144.90_pm_r, 89692._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
152.40_pm_r, 91871._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
164.80_pm_r, 94204._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
183.80_pm_r, 96781._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
212.50_pm_r, 99741._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
251.60_pm_r,103252._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
289.80_pm_r,107435._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
319.70_pm_r,112202._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
402.60_pm_r,117930._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60 = (/ &      ! donnees latitude -60
275.80_pm_r,  -109._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
255.60_pm_r,  3774._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
232.00_pm_r,  7337._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.10_pm_r, 10677._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.80_pm_r, 13979._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
228.30_pm_r, 17312._pm_r,   5._pm_r,212._pm_r,  0.35_pm_r, 36._pm_r,   1._pm_r, 76._pm_r,  0.14_pm_r,180._pm_r, & 
229.90_pm_r, 20668._pm_r,   4._pm_r,212._pm_r,  0.49_pm_r, 33._pm_r,   1._pm_r, 91._pm_r,  0.19_pm_r,177._pm_r, & 
231.50_pm_r, 24043._pm_r,   4._pm_r,212._pm_r,  0.57_pm_r, 30._pm_r,   1._pm_r,109._pm_r,  0.22_pm_r,177._pm_r, & 
237.30_pm_r, 27469._pm_r,   3._pm_r,213._pm_r,  0.58_pm_r, 27._pm_r,   1._pm_r,124._pm_r,  0.21_pm_r,175._pm_r, & 
246.90_pm_r, 31015._pm_r,   2._pm_r,217._pm_r,  0.51_pm_r, 22._pm_r,   1._pm_r,134._pm_r,  0.18_pm_r,171._pm_r, & 
257.60_pm_r, 34706._pm_r,   1._pm_r,226._pm_r,  0.39_pm_r, 15._pm_r,   2._pm_r,139._pm_r,  0.13_pm_r,162._pm_r, & 
268.10_pm_r, 38560._pm_r,   1._pm_r,242._pm_r,  0.27_pm_r,  9._pm_r,   2._pm_r,140._pm_r,  0.07_pm_r,146._pm_r, & 
277.00_pm_r, 42551._pm_r,   1._pm_r,260._pm_r,  0.15_pm_r,360._pm_r,   2._pm_r,140._pm_r,  0.04_pm_r, 90._pm_r, & 
280.90_pm_r, 46646._pm_r,   1._pm_r,272._pm_r,  0.12_pm_r, 28._pm_r,   2._pm_r,137._pm_r,  0.06_pm_r, 23._pm_r, & 
278.70_pm_r, 50750._pm_r,   1._pm_r,284._pm_r,  0.09_pm_r, 32._pm_r,   2._pm_r,134._pm_r,  0.08_pm_r,  7._pm_r, & 
271.80_pm_r, 54783._pm_r,   1._pm_r,290._pm_r,  0.06_pm_r,315._pm_r,   2._pm_r,131._pm_r,  0.06_pm_r, 24._pm_r, & 
261.40_pm_r, 58694._pm_r,   1._pm_r,288._pm_r,  0.19_pm_r,264._pm_r,   2._pm_r,128._pm_r,  0.07_pm_r, 74._pm_r, & 
248.80_pm_r, 62428._pm_r,   1._pm_r,280._pm_r,  0.23_pm_r,250._pm_r,   2._pm_r,126._pm_r,  0.13_pm_r, 99._pm_r, & 
236.00_pm_r, 65981._pm_r,   2._pm_r,272._pm_r,  0.22_pm_r,236._pm_r,   2._pm_r,123._pm_r,  0.18_pm_r,103._pm_r, & 
222.80_pm_r, 69338._pm_r,   2._pm_r,266._pm_r,  0.17_pm_r,224._pm_r,   2._pm_r,121._pm_r,  0.23_pm_r,109._pm_r, & 
208.90_pm_r, 72504._pm_r,   2._pm_r,261._pm_r,  0.12_pm_r,211._pm_r,   3._pm_r,119._pm_r,  0.26_pm_r,106._pm_r, & 
194.50_pm_r, 75454._pm_r,   2._pm_r,257._pm_r,  0.08_pm_r,173._pm_r,   3._pm_r,118._pm_r,  0.27_pm_r,110._pm_r, & 
180.40_pm_r, 78202._pm_r,   2._pm_r,254._pm_r,  0.06_pm_r,149._pm_r,   3._pm_r,117._pm_r,  0.24_pm_r,109._pm_r, & 
168.40_pm_r, 80746._pm_r,   2._pm_r,252._pm_r,  0.07_pm_r,132._pm_r,   4._pm_r,116._pm_r,  0.22_pm_r,111._pm_r, & 
158.50_pm_r, 83128._pm_r,   2._pm_r,249._pm_r,  0.07_pm_r,117._pm_r,   4._pm_r,116._pm_r,  0.18_pm_r,112._pm_r, & 
152.20_pm_r, 85381._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
150.50_pm_r, 87579._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
151.90_pm_r, 89787._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
158.00_pm_r, 92061._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
168.60_pm_r, 94465._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
185.00_pm_r, 97084._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
209.70_pm_r,100036._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
243.50_pm_r,103467._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
281.40_pm_r,107515._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
319.30_pm_r,112215._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
399.80_pm_r,117929._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50 = (/ &      ! donnees latitude -50
281.60_pm_r,   -55._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
262.10_pm_r,  3922._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
237.80_pm_r,  7578._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
223.50_pm_r, 10952._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
220.40_pm_r, 14198._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
220.90_pm_r, 17427._pm_r,   5._pm_r,207._pm_r,  0.48_pm_r, 11._pm_r,   1._pm_r, 65._pm_r,  0.14_pm_r,170._pm_r, & 
223.50_pm_r, 20679._pm_r,   4._pm_r,210._pm_r,  0.63_pm_r, 10._pm_r,   1._pm_r, 89._pm_r,  0.19_pm_r,168._pm_r, & 
227.50_pm_r, 23980._pm_r,   3._pm_r,216._pm_r,  0.71_pm_r,  8._pm_r,   1._pm_r,114._pm_r,  0.23_pm_r,168._pm_r, & 
233.90_pm_r, 27354._pm_r,   3._pm_r,227._pm_r,  0.67_pm_r,  6._pm_r,   1._pm_r,131._pm_r,  0.23_pm_r,167._pm_r, & 
243.50_pm_r, 30851._pm_r,   2._pm_r,246._pm_r,  0.55_pm_r,  3._pm_r,   1._pm_r,139._pm_r,  0.19_pm_r,167._pm_r, & 
253.60_pm_r, 34488._pm_r,   2._pm_r,266._pm_r,  0.36_pm_r,359._pm_r,   1._pm_r,144._pm_r,  0.14_pm_r,163._pm_r, & 
263.90_pm_r, 38281._pm_r,   2._pm_r,279._pm_r,  0.18_pm_r,350._pm_r,   2._pm_r,145._pm_r,  0.08_pm_r,159._pm_r, & 
272.70_pm_r, 42211._pm_r,   2._pm_r,283._pm_r,  0.04_pm_r,310._pm_r,   2._pm_r,146._pm_r,  0.02_pm_r,135._pm_r, & 
276.80_pm_r, 46241._pm_r,   2._pm_r,284._pm_r,  0.06_pm_r, 60._pm_r,   2._pm_r,145._pm_r,  0.04_pm_r,336._pm_r, & 
274.80_pm_r, 50288._pm_r,   2._pm_r,289._pm_r,  0.20_pm_r, 54._pm_r,   2._pm_r,145._pm_r,  0.09_pm_r,315._pm_r, & 
267.10_pm_r, 54257._pm_r,   2._pm_r,299._pm_r,  0.25_pm_r, 55._pm_r,   1._pm_r,146._pm_r,  0.08_pm_r,314._pm_r, & 
257.00_pm_r, 58100._pm_r,   1._pm_r,309._pm_r,  0.13_pm_r, 81._pm_r,   1._pm_r,146._pm_r,  0.05_pm_r, 53._pm_r, & 
245.10_pm_r, 61776._pm_r,   1._pm_r,305._pm_r,  0.21_pm_r,189._pm_r,   1._pm_r,141._pm_r,  0.14_pm_r, 81._pm_r, & 
232.50_pm_r, 65275._pm_r,   1._pm_r,284._pm_r,  0.43_pm_r,205._pm_r,   2._pm_r,132._pm_r,  0.21_pm_r, 79._pm_r, & 
221.40_pm_r, 68595._pm_r,   2._pm_r,257._pm_r,  0.54_pm_r,209._pm_r,   2._pm_r,123._pm_r,  0.22_pm_r, 68._pm_r, & 
210.70_pm_r, 71763._pm_r,   2._pm_r,241._pm_r,  0.58_pm_r,211._pm_r,   2._pm_r,115._pm_r,  0.21_pm_r, 54._pm_r, & 
199.80_pm_r, 74766._pm_r,   3._pm_r,233._pm_r,  0.56_pm_r,211._pm_r,   2._pm_r,107._pm_r,  0.20_pm_r, 42._pm_r, & 
188.90_pm_r, 77615._pm_r,   4._pm_r,228._pm_r,  0.53_pm_r,211._pm_r,   2._pm_r,100._pm_r,  0.19_pm_r, 34._pm_r, & 
179.10_pm_r, 80302._pm_r,   4._pm_r,226._pm_r,  0.47_pm_r,213._pm_r,   2._pm_r, 94._pm_r,  0.17_pm_r, 27._pm_r, & 
170.50_pm_r, 82857._pm_r,   5._pm_r,224._pm_r,  0.40_pm_r,213._pm_r,   2._pm_r, 89._pm_r,  0.15_pm_r, 21._pm_r, & 
164.00_pm_r, 85291._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
160.80_pm_r, 87654._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
160.50_pm_r, 90002._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
164.80_pm_r, 92391._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
173.10_pm_r, 94881._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.20_pm_r, 97547._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
206.50_pm_r,100488._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
234.80_pm_r,103832._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
271.30_pm_r,107728._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
317.00_pm_r,112333._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
396.40_pm_r,118015._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /) 
       

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40 = (/ &      ! donnees latitude -40
289.50_pm_r,   -13._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
269.20_pm_r,  4077._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
244.00_pm_r,  7833._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
222.90_pm_r, 11251._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
213.00_pm_r, 14442._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
212.10_pm_r, 17545._pm_r,   3._pm_r,196._pm_r,  0.31_pm_r,325._pm_r,   0._pm_r,176._pm_r,  0.05_pm_r,143._pm_r, & 
216.70_pm_r, 20681._pm_r,   3._pm_r,203._pm_r,  0.39_pm_r,324._pm_r,   0._pm_r,159._pm_r,  0.06_pm_r,141._pm_r, & 
223.30_pm_r, 23905._pm_r,   3._pm_r,214._pm_r,  0.43_pm_r,325._pm_r,   0._pm_r,154._pm_r,  0.07_pm_r,146._pm_r, & 
230.00_pm_r, 27223._pm_r,   3._pm_r,227._pm_r,  0.42_pm_r,326._pm_r,   0._pm_r,151._pm_r,  0.07_pm_r,146._pm_r, & 
238.20_pm_r, 30651._pm_r,   3._pm_r,240._pm_r,  0.35_pm_r,325._pm_r,   1._pm_r,150._pm_r,  0.07_pm_r,148._pm_r, & 
248.80_pm_r, 34213._pm_r,   3._pm_r,249._pm_r,  0.24_pm_r,327._pm_r,   1._pm_r,150._pm_r,  0.05_pm_r,143._pm_r, & 
259.40_pm_r, 37938._pm_r,   3._pm_r,254._pm_r,  0.12_pm_r,328._pm_r,   1._pm_r,150._pm_r,  0.04_pm_r,146._pm_r, & 
268.80_pm_r, 41806._pm_r,   3._pm_r,256._pm_r,  0.03_pm_r,342._pm_r,   1._pm_r,149._pm_r,  0.03_pm_r,135._pm_r, & 
273.30_pm_r, 45783._pm_r,   3._pm_r,258._pm_r,  0.08_pm_r, 30._pm_r,   1._pm_r,152._pm_r,  0.09_pm_r,183._pm_r, & 
271.20_pm_r, 49779._pm_r,   3._pm_r,261._pm_r,  0.20_pm_r, 27._pm_r,   1._pm_r,160._pm_r,  0.17_pm_r,197._pm_r, & 
263.10_pm_r, 53693._pm_r,   2._pm_r,267._pm_r,  0.24_pm_r, 43._pm_r,   1._pm_r,167._pm_r,  0.14_pm_r,199._pm_r, & 
253.30_pm_r, 57479._pm_r,   2._pm_r,270._pm_r,  0.23_pm_r, 92._pm_r,   1._pm_r,170._pm_r,  0.04_pm_r,146._pm_r, & 
241.70_pm_r, 61103._pm_r,   2._pm_r,263._pm_r,  0.35_pm_r,144._pm_r,   1._pm_r,164._pm_r,  0.16_pm_r, 60._pm_r, & 
229.10_pm_r, 64553._pm_r,   2._pm_r,243._pm_r,  0.47_pm_r,165._pm_r,   1._pm_r,149._pm_r,  0.24_pm_r, 62._pm_r, & 
218.90_pm_r, 67828._pm_r,   2._pm_r,224._pm_r,  0.47_pm_r,178._pm_r,   1._pm_r,132._pm_r,  0.26_pm_r, 76._pm_r, & 
211.20_pm_r, 70978._pm_r,   3._pm_r,215._pm_r,  0.44_pm_r,191._pm_r,   2._pm_r,121._pm_r,  0.26_pm_r, 90._pm_r, & 
205.10_pm_r, 74024._pm_r,   3._pm_r,211._pm_r,  0.40_pm_r,203._pm_r,   2._pm_r,117._pm_r,  0.26_pm_r,105._pm_r, & 
198.50_pm_r, 76982._pm_r,   4._pm_r,211._pm_r,  0.37_pm_r,213._pm_r,   2._pm_r,116._pm_r,  0.26_pm_r,119._pm_r, & 
191.60_pm_r, 79836._pm_r,   4._pm_r,212._pm_r,  0.35_pm_r,221._pm_r,   3._pm_r,116._pm_r,  0.27_pm_r,124._pm_r, & 
184.10_pm_r, 82593._pm_r,   5._pm_r,213._pm_r,  0.31_pm_r,228._pm_r,   3._pm_r,118._pm_r,  0.25_pm_r,127._pm_r, & 
176.40_pm_r, 85221._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
171.50_pm_r, 87753._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
169.60_pm_r, 90248._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
172.00_pm_r, 92758._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
177.70_pm_r, 95338._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.50_pm_r, 98052._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
203.20_pm_r,100980._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.40_pm_r,104239._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
260.60_pm_r,107986._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
312.50_pm_r,112469._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
392.40_pm_r,118097._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)      

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30 = (/ &      ! donnees latitude -30
296.00_pm_r,   -41._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
274.30_pm_r,  4137._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
249.30_pm_r,  7973._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.70_pm_r, 11446._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
207.00_pm_r, 14609._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
204.90_pm_r, 17605._pm_r,   3._pm_r,186._pm_r,  0.25_pm_r,272._pm_r,   0._pm_r,331._pm_r,  0.15_pm_r,100._pm_r, & 
212.00_pm_r, 20655._pm_r,   3._pm_r,195._pm_r,  0.31_pm_r,272._pm_r,   0._pm_r, 16._pm_r,  0.20_pm_r,100._pm_r, & 
219.90_pm_r, 23820._pm_r,   3._pm_r,204._pm_r,  0.31_pm_r,272._pm_r,   1._pm_r, 59._pm_r,  0.22_pm_r,100._pm_r, & 
226.40_pm_r, 27087._pm_r,   3._pm_r,212._pm_r,  0.27_pm_r,273._pm_r,   1._pm_r, 76._pm_r,  0.23_pm_r,100._pm_r, & 
233.60_pm_r, 30455._pm_r,   3._pm_r,218._pm_r,  0.19_pm_r,275._pm_r,   1._pm_r, 84._pm_r,  0.21_pm_r,100._pm_r, & 
244.30_pm_r, 33950._pm_r,   3._pm_r,222._pm_r,  0.10_pm_r,290._pm_r,   1._pm_r, 87._pm_r,  0.16_pm_r, 99._pm_r, & 
254.70_pm_r, 37608._pm_r,   3._pm_r,223._pm_r,  0.06_pm_r,346._pm_r,   2._pm_r, 89._pm_r,  0.11_pm_r, 99._pm_r, & 
264.30_pm_r, 41407._pm_r,   3._pm_r,225._pm_r,  0.10_pm_r, 21._pm_r,   2._pm_r, 89._pm_r,  0.06_pm_r, 90._pm_r, & 
269.90_pm_r, 45326._pm_r,   3._pm_r,226._pm_r,  0.13_pm_r,  9._pm_r,   2._pm_r, 91._pm_r,  0.08_pm_r,180._pm_r, & 
269.10_pm_r, 49281._pm_r,   3._pm_r,229._pm_r,  0.14_pm_r, 18._pm_r,   2._pm_r, 98._pm_r,  0.21_pm_r,207._pm_r, & 
261.90_pm_r, 53172._pm_r,   3._pm_r,229._pm_r,  0.12_pm_r, 81._pm_r,   2._pm_r,111._pm_r,  0.26_pm_r,214._pm_r, & 
252.40_pm_r, 56942._pm_r,   3._pm_r,223._pm_r,  0.32_pm_r,136._pm_r,   2._pm_r,124._pm_r,  0.20_pm_r,216._pm_r, & 
241.00_pm_r, 60555._pm_r,   3._pm_r,211._pm_r,  0.41_pm_r,147._pm_r,   2._pm_r,130._pm_r,  0.02_pm_r,148._pm_r, & 
228.50_pm_r, 63995._pm_r,   3._pm_r,202._pm_r,  0.35_pm_r,161._pm_r,   2._pm_r,126._pm_r,  0.15_pm_r, 63._pm_r, & 
218.40_pm_r, 67261._pm_r,   3._pm_r,198._pm_r,  0.23_pm_r,186._pm_r,   2._pm_r,118._pm_r,  0.23_pm_r, 65._pm_r, & 
212.20_pm_r, 70411._pm_r,   4._pm_r,199._pm_r,  0.18_pm_r,246._pm_r,   2._pm_r,109._pm_r,  0.27_pm_r, 73._pm_r, & 
208.80_pm_r, 73492._pm_r,   4._pm_r,204._pm_r,  0.28_pm_r,281._pm_r,   2._pm_r,104._pm_r,  0.29_pm_r, 79._pm_r, & 
205.70_pm_r, 76529._pm_r,   4._pm_r,211._pm_r,  0.39_pm_r,293._pm_r,   3._pm_r,100._pm_r,  0.29_pm_r, 84._pm_r, & 
201.80_pm_r, 79513._pm_r,   4._pm_r,221._pm_r,  0.46_pm_r,299._pm_r,   3._pm_r, 98._pm_r,  0.29_pm_r, 88._pm_r, & 
195.50_pm_r, 82441._pm_r,   4._pm_r,230._pm_r,  0.49_pm_r,302._pm_r,   4._pm_r, 97._pm_r,  0.26_pm_r, 90._pm_r, & 
186.80_pm_r, 85227._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
180.60_pm_r, 87900._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
177.70_pm_r, 90524._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
178.50_pm_r, 93146._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
181.90_pm_r, 95809._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
188.60_pm_r, 98565._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.10_pm_r,101481._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
218.90_pm_r,104661._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
250.30_pm_r,108270._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
305.90_pm_r,112616._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
387.90_pm_r,118167._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)   


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20 = (/ &      ! donnees latitude -20
299.20_pm_r,   -77._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
276.60_pm_r,  4145._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
252.60_pm_r,  8025._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.20_pm_r, 11535._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
203.00_pm_r, 14682._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.40_pm_r, 17607._pm_r,   3._pm_r,166._pm_r,  0.31_pm_r,274._pm_r,   0._pm_r,339._pm_r,  0.23_pm_r, 96._pm_r, & 
209.30_pm_r, 20605._pm_r,   3._pm_r,176._pm_r,  0.38_pm_r,273._pm_r,   0._pm_r, 41._pm_r,  0.29_pm_r, 96._pm_r, & 
216.90_pm_r, 23730._pm_r,   3._pm_r,186._pm_r,  0.37_pm_r,273._pm_r,   1._pm_r, 70._pm_r,  0.31_pm_r, 96._pm_r, & 
223.00_pm_r, 26950._pm_r,   3._pm_r,196._pm_r,  0.32_pm_r,274._pm_r,   1._pm_r, 80._pm_r,  0.30_pm_r, 96._pm_r, & 
230.20_pm_r, 30267._pm_r,   3._pm_r,203._pm_r,  0.21_pm_r,276._pm_r,   2._pm_r, 84._pm_r,  0.25_pm_r, 96._pm_r, & 
241.10_pm_r, 33714._pm_r,   3._pm_r,207._pm_r,  0.09_pm_r,291._pm_r,   2._pm_r, 86._pm_r,  0.16_pm_r, 94._pm_r, & 
250.60_pm_r, 37320._pm_r,   3._pm_r,208._pm_r,  0.06_pm_r, 15._pm_r,   2._pm_r, 86._pm_r,  0.09_pm_r, 87._pm_r, & 
260.10_pm_r, 41055._pm_r,   3._pm_r,208._pm_r,  0.11_pm_r, 34._pm_r,   2._pm_r, 86._pm_r,  0.03_pm_r, 45._pm_r, & 
267.40_pm_r, 44926._pm_r,   3._pm_r,208._pm_r,  0.10_pm_r,354._pm_r,   2._pm_r, 85._pm_r,  0.01_pm_r,360._pm_r, & 
268.20_pm_r, 48855._pm_r,   3._pm_r,212._pm_r,  0.22_pm_r,321._pm_r,   2._pm_r, 86._pm_r,  0.06_pm_r,135._pm_r, & 
262.70_pm_r, 52744._pm_r,   3._pm_r,220._pm_r,  0.28_pm_r,310._pm_r,   2._pm_r, 90._pm_r,  0.16_pm_r,176._pm_r, & 
255.00_pm_r, 56538._pm_r,   3._pm_r,227._pm_r,  0.19_pm_r,279._pm_r,   2._pm_r, 98._pm_r,  0.32_pm_r,202._pm_r, & 
244.90_pm_r, 60200._pm_r,   3._pm_r,228._pm_r,  0.18_pm_r,207._pm_r,   2._pm_r,113._pm_r,  0.38_pm_r,217._pm_r, & 
232.40_pm_r, 63700._pm_r,   3._pm_r,224._pm_r,  0.28_pm_r,182._pm_r,   2._pm_r,128._pm_r,  0.34_pm_r,225._pm_r, & 
221.20_pm_r, 67015._pm_r,   4._pm_r,219._pm_r,  0.31_pm_r,178._pm_r,   2._pm_r,140._pm_r,  0.18_pm_r,240._pm_r, & 
214.00_pm_r, 70200._pm_r,   4._pm_r,215._pm_r,  0.28_pm_r,184._pm_r,   2._pm_r,144._pm_r,  0.07_pm_r,344._pm_r, & 
210.80_pm_r, 73308._pm_r,   4._pm_r,213._pm_r,  0.24_pm_r,191._pm_r,   2._pm_r,138._pm_r,  0.23_pm_r, 28._pm_r, & 
208.60_pm_r, 76379._pm_r,   5._pm_r,212._pm_r,  0.22_pm_r,202._pm_r,   2._pm_r,122._pm_r,  0.39_pm_r, 36._pm_r, & 
206.30_pm_r, 79418._pm_r,   5._pm_r,211._pm_r,  0.18_pm_r,207._pm_r,   2._pm_r,101._pm_r,  0.49_pm_r, 37._pm_r, & 
201.20_pm_r, 82424._pm_r,   5._pm_r,211._pm_r,  0.16_pm_r,215._pm_r,   2._pm_r, 84._pm_r,  0.52_pm_r, 40._pm_r, & 
192.90_pm_r, 85298._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.60_pm_r, 88062._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
183.70_pm_r, 90777._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
183.50_pm_r, 93481._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
185.00_pm_r, 96208._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
189.20_pm_r, 98994._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
197.10_pm_r,101895._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
212.30_pm_r,105004._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
240.80_pm_r,108490._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
298.10_pm_r,112695._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
382.90_pm_r,118153._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)       

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10 = (/ &      ! donnees latitude -10
300.70_pm_r,   -77._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
276.70_pm_r,  4159._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
253.30_pm_r,  8046._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.00_pm_r, 11562._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.90_pm_r, 14693._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
197.80_pm_r, 17580._pm_r,   3._pm_r,168._pm_r,  0.28_pm_r,296._pm_r,   1._pm_r, 20._pm_r,  0.19_pm_r,103._pm_r, & 
207.20_pm_r, 20545._pm_r,   3._pm_r,175._pm_r,  0.34_pm_r,295._pm_r,   1._pm_r, 44._pm_r,  0.23_pm_r,104._pm_r, & 
215.50_pm_r, 23644._pm_r,   3._pm_r,185._pm_r,  0.35_pm_r,293._pm_r,   1._pm_r, 62._pm_r,  0.23_pm_r,104._pm_r, & 
221.60_pm_r, 26846._pm_r,   3._pm_r,196._pm_r,  0.32_pm_r,291._pm_r,   1._pm_r, 72._pm_r,  0.20_pm_r,106._pm_r, & 
228.70_pm_r, 30142._pm_r,   3._pm_r,205._pm_r,  0.24_pm_r,289._pm_r,   2._pm_r, 77._pm_r,  0.13_pm_r,108._pm_r, & 
238.90_pm_r, 33563._pm_r,   3._pm_r,212._pm_r,  0.15_pm_r,287._pm_r,   2._pm_r, 80._pm_r,  0.04_pm_r,124._pm_r, & 
248.80_pm_r, 37139._pm_r,   3._pm_r,215._pm_r,  0.07_pm_r,290._pm_r,   2._pm_r, 80._pm_r,  0.05_pm_r,273._pm_r, & 
257.60_pm_r, 40845._pm_r,   3._pm_r,217._pm_r,  0.03_pm_r,333._pm_r,   1._pm_r, 79._pm_r,  0.12_pm_r,280._pm_r, & 
264.10_pm_r, 44671._pm_r,   3._pm_r,216._pm_r,  0.07_pm_r,142._pm_r,   1._pm_r, 74._pm_r,  0.17_pm_r,289._pm_r, & 
267.80_pm_r, 48570._pm_r,   3._pm_r,213._pm_r,  0.15_pm_r,156._pm_r,   1._pm_r, 67._pm_r,  0.20_pm_r,271._pm_r, & 
265.70_pm_r, 52481._pm_r,   3._pm_r,210._pm_r,  0.14_pm_r,206._pm_r,   1._pm_r, 69._pm_r,  0.33_pm_r,226._pm_r, & 
260.00_pm_r, 56336._pm_r,   3._pm_r,213._pm_r,  0.31_pm_r,262._pm_r,   0._pm_r,141._pm_r,  0.60_pm_r,203._pm_r, & 
250.00_pm_r, 60074._pm_r,   3._pm_r,221._pm_r,  0.42_pm_r,286._pm_r,   1._pm_r,180._pm_r,  0.60_pm_r,191._pm_r, & 
236.10_pm_r, 63639._pm_r,   4._pm_r,231._pm_r,  0.40_pm_r,304._pm_r,   2._pm_r,181._pm_r,  0.43_pm_r,172._pm_r, & 
223.80_pm_r, 66999._pm_r,   4._pm_r,238._pm_r,  0.28_pm_r,336._pm_r,   2._pm_r,175._pm_r,  0.28_pm_r,114._pm_r, & 
215.10_pm_r, 70212._pm_r,   4._pm_r,243._pm_r,  0.30_pm_r, 35._pm_r,   2._pm_r,163._pm_r,  0.50_pm_r, 61._pm_r, & 
210.10_pm_r, 73322._pm_r,   3._pm_r,245._pm_r,  0.48_pm_r, 62._pm_r,   2._pm_r,139._pm_r,  0.82_pm_r, 46._pm_r, & 
207.30_pm_r, 76378._pm_r,   2._pm_r,244._pm_r,  0.67_pm_r, 73._pm_r,   3._pm_r,105._pm_r, 1.09_pm_r, 41._pm_r, & 
205.90_pm_r, 79403._pm_r,   1._pm_r,232._pm_r,  0.79_pm_r, 77._pm_r,   4._pm_r, 79._pm_r, 1.24_pm_r, 38._pm_r, & 
202.10_pm_r, 82409._pm_r,   1._pm_r,140._pm_r,  0.84_pm_r, 79._pm_r,   5._pm_r, 65._pm_r, 1.30_pm_r, 37._pm_r, & 
195.00_pm_r, 85311._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
189.40_pm_r, 88115._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.80_pm_r, 90874._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.40_pm_r, 93625._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.90_pm_r, 96389._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
189.00_pm_r, 99189._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
194.30_pm_r,102069._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
206.70_pm_r,105116._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
232.70_pm_r,108496._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
290.10_pm_r,112571._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
377.70_pm_r,117929._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0 = (/ &      ! donnees latitude 0
300.80_pm_r,   -89._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
276.90_pm_r,  4149._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
253.60_pm_r,  8041._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.20_pm_r, 11560._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.40_pm_r, 14690._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
196.70_pm_r, 17568._pm_r,   2._pm_r,169._pm_r,  0.14_pm_r,321._pm_r,   1._pm_r,358._pm_r,  0.17_pm_r,100._pm_r, & 
205.90_pm_r, 20518._pm_r,   2._pm_r,172._pm_r,  0.16_pm_r,317._pm_r,   1._pm_r, 19._pm_r,  0.21_pm_r,101._pm_r, & 
214.80_pm_r, 23605._pm_r,   2._pm_r,178._pm_r,  0.17_pm_r,311._pm_r,   1._pm_r, 39._pm_r,  0.22_pm_r,100._pm_r, & 
220.80_pm_r, 26797._pm_r,   2._pm_r,185._pm_r,  0.15_pm_r,302._pm_r,   1._pm_r, 54._pm_r,  0.20_pm_r,101._pm_r, & 
227.50_pm_r, 30082._pm_r,   2._pm_r,193._pm_r,  0.13_pm_r,284._pm_r,   1._pm_r, 62._pm_r,  0.14_pm_r,107._pm_r, & 
239.10_pm_r, 33495._pm_r,   2._pm_r,201._pm_r,  0.10_pm_r,264._pm_r,   1._pm_r, 66._pm_r,  0.06_pm_r,121._pm_r, & 
249.50_pm_r, 37071._pm_r,   2._pm_r,206._pm_r,  0.09_pm_r,257._pm_r,   1._pm_r, 68._pm_r,  0.05_pm_r,229._pm_r, & 
257.60_pm_r, 40780._pm_r,   2._pm_r,209._pm_r,  0.06_pm_r,261._pm_r,   1._pm_r, 68._pm_r,  0.10_pm_r,253._pm_r, & 
263.60_pm_r, 44602._pm_r,   2._pm_r,210._pm_r,  0.05_pm_r,248._pm_r,   1._pm_r, 67._pm_r,  0.16_pm_r,232._pm_r, & 
267.30_pm_r, 48495._pm_r,   2._pm_r,209._pm_r,  0.07_pm_r,262._pm_r,   1._pm_r, 73._pm_r,  0.32_pm_r,204._pm_r, & 
267.40_pm_r, 52414._pm_r,   2._pm_r,208._pm_r,  0.13_pm_r,261._pm_r,   0._pm_r,113._pm_r,  0.49_pm_r,194._pm_r, & 
261.70_pm_r, 56290._pm_r,   2._pm_r,210._pm_r,  0.21_pm_r,247._pm_r,   1._pm_r,162._pm_r,  0.52_pm_r,192._pm_r, & 
250.70_pm_r, 60037._pm_r,   2._pm_r,215._pm_r,  0.16_pm_r,261._pm_r,   1._pm_r,173._pm_r,  0.30_pm_r,191._pm_r, & 
237.90_pm_r, 63615._pm_r,   3._pm_r,222._pm_r,  0.10_pm_r,336._pm_r,   2._pm_r,172._pm_r,  0.10_pm_r,135._pm_r, & 
225.00_pm_r, 66994._pm_r,   2._pm_r,231._pm_r,  0.27_pm_r, 20._pm_r,   2._pm_r,163._pm_r,  0.24_pm_r, 60._pm_r, & 
215.30_pm_r, 70216._pm_r,   2._pm_r,246._pm_r,  0.48_pm_r, 30._pm_r,   2._pm_r,148._pm_r,  0.44_pm_r, 54._pm_r, & 
210.40_pm_r, 73332._pm_r,   1._pm_r,277._pm_r,  0.68_pm_r, 33._pm_r,   2._pm_r,127._pm_r,  0.60_pm_r, 54._pm_r, & 
207.70_pm_r, 76395._pm_r,   1._pm_r,328._pm_r,  0.84_pm_r, 35._pm_r,   3._pm_r,109._pm_r,  0.72_pm_r, 55._pm_r, & 
206.00_pm_r, 79426._pm_r,   2._pm_r,358._pm_r,  0.92_pm_r, 35._pm_r,   4._pm_r, 95._pm_r,  0.79_pm_r, 55._pm_r, & 
202.30_pm_r, 82434._pm_r,   4._pm_r, 10._pm_r,  0.93_pm_r, 35._pm_r,   5._pm_r, 86._pm_r,  0.80_pm_r, 55._pm_r, & 
195.70_pm_r, 85347._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
190.10_pm_r, 88163._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.50_pm_r, 90932._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.30_pm_r, 93695._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.30_pm_r, 96470._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
188.30_pm_r, 99269._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
191.70_pm_r,102126._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.20_pm_r,105119._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.30_pm_r,108416._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
283.20_pm_r,112382._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
372.50_pm_r,117648._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10 = (/ &      ! donnees latitude 10
300.30_pm_r,   -75._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
277.00_pm_r,  4160._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
253.00_pm_r,  8047._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.70_pm_r, 11559._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.80_pm_r, 14687._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
197.30_pm_r, 17571._pm_r,   3._pm_r,163._pm_r,  0.36_pm_r,344._pm_r,   1._pm_r,323._pm_r,  0.18_pm_r, 88._pm_r, & 
206.40_pm_r, 20525._pm_r,   3._pm_r,163._pm_r,  0.47_pm_r,343._pm_r,   1._pm_r,338._pm_r,  0.22_pm_r, 87._pm_r, & 
215.20_pm_r, 23616._pm_r,   2._pm_r,164._pm_r,  0.49_pm_r,341._pm_r,   1._pm_r,357._pm_r,  0.19_pm_r, 90._pm_r, & 
221.70_pm_r, 26815._pm_r,   1._pm_r,167._pm_r,  0.44_pm_r,337._pm_r,   1._pm_r, 13._pm_r,  0.13_pm_r, 97._pm_r, & 
228.80_pm_r, 30113._pm_r,   1._pm_r,180._pm_r,  0.34_pm_r,332._pm_r,   1._pm_r, 20._pm_r,  0.05_pm_r,152._pm_r, & 
238.70_pm_r, 33531._pm_r,   0._pm_r,227._pm_r,  0.22_pm_r,322._pm_r,   1._pm_r, 19._pm_r,  0.10_pm_r,231._pm_r, & 
248.70_pm_r, 37103._pm_r,   0._pm_r,265._pm_r,  0.12_pm_r,303._pm_r,   1._pm_r,  7._pm_r,  0.18_pm_r,239._pm_r, & 
257.90_pm_r, 40811._pm_r,   1._pm_r,272._pm_r,  0.08_pm_r,278._pm_r,   1._pm_r,338._pm_r,  0.23_pm_r,239._pm_r, & 
264.50_pm_r, 44642._pm_r,   1._pm_r,267._pm_r,  0.07_pm_r,192._pm_r,   1._pm_r,300._pm_r,  0.24_pm_r,239._pm_r, & 
268.00_pm_r, 48544._pm_r,   1._pm_r,252._pm_r,  0.17_pm_r,137._pm_r,   1._pm_r,277._pm_r,  0.25_pm_r,241._pm_r, & 
266.00_pm_r, 52459._pm_r,   1._pm_r,219._pm_r,  0.23_pm_r,121._pm_r,   1._pm_r,264._pm_r,  0.24_pm_r,224._pm_r, & 
258.90_pm_r, 56309._pm_r,   1._pm_r,182._pm_r,  0.16_pm_r,110._pm_r,   1._pm_r,250._pm_r,  0.30_pm_r,180._pm_r, & 
248.00_pm_r, 60020._pm_r,   1._pm_r,171._pm_r,  0.10_pm_r,356._pm_r,   1._pm_r,230._pm_r,  0.37_pm_r,154._pm_r, & 
235.50_pm_r, 63565._pm_r,   0._pm_r,187._pm_r,  0.26_pm_r,329._pm_r,   2._pm_r,209._pm_r,  0.39_pm_r,137._pm_r, & 
223.80_pm_r, 66924._pm_r,   0._pm_r,299._pm_r,  0.33_pm_r,330._pm_r,   2._pm_r,191._pm_r,  0.34_pm_r,119._pm_r, & 
215.50_pm_r, 70138._pm_r,   1._pm_r,320._pm_r,  0.36_pm_r,334._pm_r,   2._pm_r,178._pm_r,  0.27_pm_r, 94._pm_r, & 
211.30_pm_r, 73259._pm_r,   1._pm_r,327._pm_r,  0.38_pm_r,340._pm_r,   2._pm_r,166._pm_r,  0.26_pm_r, 63._pm_r, & 
208.70_pm_r, 76334._pm_r,   2._pm_r,332._pm_r,  0.37_pm_r,344._pm_r,   2._pm_r,154._pm_r,  0.31_pm_r, 40._pm_r, & 
206.60_pm_r, 79375._pm_r,   2._pm_r,335._pm_r,  0.37_pm_r,348._pm_r,   2._pm_r,138._pm_r,  0.34_pm_r, 28._pm_r, & 
202.90_pm_r, 82387._pm_r,   3._pm_r,337._pm_r,  0.35_pm_r,349._pm_r,   1._pm_r,118._pm_r,  0.36_pm_r, 22._pm_r, & 
196.40_pm_r, 85316._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
190.40_pm_r, 88140._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.20_pm_r, 90905._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.00_pm_r, 93663._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.00_pm_r, 96433._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.40_pm_r, 99223._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
189.90_pm_r,102060._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
199.40_pm_r,105017._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
222.10_pm_r,108259._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
278.10_pm_r,112149._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
367.60_pm_r,117338._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20 = (/ &      ! donnees latitude 20
297.40_pm_r,   -76._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
274.50_pm_r,  4117._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
249.90_pm_r,  7961._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.70_pm_r, 11441._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
203.60_pm_r, 14581._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.70_pm_r, 17515._pm_r,   5._pm_r,164._pm_r,  0.53_pm_r,344._pm_r,   2._pm_r,289._pm_r,  0.45_pm_r, 76._pm_r, & 
208.20_pm_r, 20507._pm_r,   4._pm_r,164._pm_r,  0.81_pm_r,346._pm_r,   1._pm_r,308._pm_r,  0.51_pm_r, 70._pm_r, & 
215.80_pm_r, 23616._pm_r,   2._pm_r,161._pm_r, 1.01_pm_r,349._pm_r,   1._pm_r,341._pm_r,  0.41_pm_r, 60._pm_r, & 
221.90_pm_r, 26819._pm_r,   1._pm_r,146._pm_r, 1.15_pm_r,350._pm_r,   1._pm_r,359._pm_r,  0.24_pm_r, 24._pm_r, & 
228.90_pm_r, 30120._pm_r,   1._pm_r, 11._pm_r, 1.17_pm_r,351._pm_r,   2._pm_r,356._pm_r,  0.27_pm_r,310._pm_r, & 
238.20_pm_r, 33535._pm_r,   3._pm_r,359._pm_r, 1.06_pm_r,352._pm_r,   2._pm_r,342._pm_r,  0.49_pm_r,278._pm_r, & 
248.50_pm_r, 37102._pm_r,   4._pm_r,357._pm_r,  0.83_pm_r,352._pm_r,   2._pm_r,322._pm_r,  0.63_pm_r,262._pm_r, & 
258.80_pm_r, 40814._pm_r,   5._pm_r,355._pm_r,  0.55_pm_r,348._pm_r,   3._pm_r,304._pm_r,  0.66_pm_r,248._pm_r, & 
265.90_pm_r, 44662._pm_r,   6._pm_r,354._pm_r,  0.26_pm_r,331._pm_r,   3._pm_r,290._pm_r,  0.52_pm_r,226._pm_r, & 
267.70_pm_r, 48575._pm_r,   6._pm_r,352._pm_r,  0.14_pm_r,291._pm_r,   4._pm_r,279._pm_r,  0.47_pm_r,205._pm_r, & 
262.80_pm_r, 52465._pm_r,   6._pm_r,351._pm_r,  0.10_pm_r,243._pm_r,   4._pm_r,269._pm_r,  0.47_pm_r,187._pm_r, & 
253.50_pm_r, 56251._pm_r,   6._pm_r,350._pm_r,  0.11_pm_r,207._pm_r,   4._pm_r,258._pm_r,  0.44_pm_r,174._pm_r, & 
243.00_pm_r, 59883._pm_r,   6._pm_r,348._pm_r,  0.18_pm_r,196._pm_r,   4._pm_r,248._pm_r,  0.46_pm_r,155._pm_r, & 
233.30_pm_r, 63374._pm_r,   5._pm_r,347._pm_r,  0.18_pm_r,202._pm_r,   4._pm_r,238._pm_r,  0.42_pm_r,138._pm_r, & 
223.30_pm_r, 66714._pm_r,   5._pm_r,345._pm_r,  0.13_pm_r,231._pm_r,   4._pm_r,230._pm_r,  0.34_pm_r,118._pm_r, & 
215.90_pm_r, 69929._pm_r,   5._pm_r,343._pm_r,  0.13_pm_r,283._pm_r,   3._pm_r,223._pm_r,  0.28_pm_r, 90._pm_r, & 
212.70_pm_r, 73064._pm_r,   5._pm_r,341._pm_r,  0.20_pm_r,311._pm_r,   3._pm_r,220._pm_r,  0.27_pm_r, 61._pm_r, & 
210.40_pm_r, 76162._pm_r,   6._pm_r,340._pm_r,  0.27_pm_r,321._pm_r,   3._pm_r,218._pm_r,  0.31_pm_r, 40._pm_r, & 
207.60_pm_r, 79224._pm_r,   6._pm_r,339._pm_r,  0.31_pm_r,325._pm_r,   2._pm_r,218._pm_r,  0.35_pm_r, 31._pm_r, & 
204.00_pm_r, 82245._pm_r,   7._pm_r,338._pm_r,  0.33_pm_r,327._pm_r,   2._pm_r,221._pm_r,  0.36_pm_r, 27._pm_r, & 
198.30_pm_r, 85204._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
191.70_pm_r, 88056._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.10_pm_r, 90826._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.60_pm_r, 93580._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.60_pm_r, 96342._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.10_pm_r, 99126._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
189.60_pm_r,101956._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
198.60_pm_r,104904._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
220.40_pm_r,108126._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
275.00_pm_r,111978._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
363.20_pm_r,117109._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)     

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30 = (/ &      ! donnees latitude 30
291.70_pm_r,   -28._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
267.20_pm_r,  4067._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
242.40_pm_r,  7800._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
222.10_pm_r, 11204._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
209.40_pm_r, 14365._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
206.90_pm_r, 17399._pm_r,   6._pm_r,200._pm_r,  0.61_pm_r,125._pm_r,   3._pm_r,290._pm_r,  0.75_pm_r, 55._pm_r, & 
210.90_pm_r, 20454._pm_r,   6._pm_r,191._pm_r,  0.75_pm_r, 98._pm_r,   2._pm_r,316._pm_r,  0.91_pm_r, 44._pm_r, & 
217.00_pm_r, 23590._pm_r,   6._pm_r,179._pm_r, 1.04_pm_r, 61._pm_r,   3._pm_r,342._pm_r,  0.89_pm_r, 25._pm_r, & 
222.80_pm_r, 26807._pm_r,   5._pm_r,162._pm_r, 1.67_pm_r, 36._pm_r,   4._pm_r,351._pm_r,  0.85_pm_r,353._pm_r, & 
229.00_pm_r, 30117._pm_r,   3._pm_r,120._pm_r, 2.44_pm_r, 22._pm_r,   5._pm_r,347._pm_r, 1.00_pm_r,317._pm_r, & 
236.10_pm_r, 33519._pm_r,   5._pm_r, 62._pm_r, 3.05_pm_r, 14._pm_r,   7._pm_r,337._pm_r, 1.28_pm_r,290._pm_r, & 
245.90_pm_r, 37048._pm_r,   8._pm_r, 36._pm_r, 3.17_pm_r,  9._pm_r,   8._pm_r,324._pm_r, 1.45_pm_r,270._pm_r, & 
256.90_pm_r, 40728._pm_r,  12._pm_r, 26._pm_r, 2.82_pm_r,  4._pm_r,   9._pm_r,312._pm_r, 1.50_pm_r,251._pm_r, & 
264.30_pm_r, 44553._pm_r,  16._pm_r, 21._pm_r, 1.88_pm_r,359._pm_r,  10._pm_r,301._pm_r, 1.29_pm_r,232._pm_r, & 
264.90_pm_r, 48436._pm_r,  18._pm_r, 18._pm_r,  0.97_pm_r,348._pm_r,  10._pm_r,291._pm_r, 1.19_pm_r,213._pm_r, & 
258.00_pm_r, 52269._pm_r,  18._pm_r, 16._pm_r,  0.40_pm_r,276._pm_r,  11._pm_r,282._pm_r, 1.08_pm_r,195._pm_r, & 
247.60_pm_r, 55976._pm_r,  17._pm_r, 14._pm_r,  0.93_pm_r,222._pm_r,  11._pm_r,274._pm_r,  0.90_pm_r,171._pm_r, & 
238.30_pm_r, 59531._pm_r,  16._pm_r, 12._pm_r, 1.28_pm_r,217._pm_r,  10._pm_r,268._pm_r,  0.92_pm_r,146._pm_r, & 
230.50_pm_r, 62965._pm_r,  14._pm_r,  8._pm_r, 1.25_pm_r,220._pm_r,   9._pm_r,262._pm_r,  0.87_pm_r,124._pm_r, & 
224.00_pm_r, 66290._pm_r,  13._pm_r,  4._pm_r,  0.97_pm_r,229._pm_r,   8._pm_r,257._pm_r,  0.75_pm_r,100._pm_r, & 
219.10_pm_r, 69534._pm_r,  12._pm_r,359._pm_r,  0.67_pm_r,253._pm_r,   7._pm_r,256._pm_r,  0.67_pm_r, 74._pm_r, & 
216.50_pm_r, 72720._pm_r,  12._pm_r,355._pm_r,  0.58_pm_r,290._pm_r,   6._pm_r,258._pm_r,  0.67_pm_r, 50._pm_r, & 
214.20_pm_r, 75875._pm_r,  13._pm_r,352._pm_r,  0.70_pm_r,318._pm_r,   6._pm_r,265._pm_r,  0.73_pm_r, 33._pm_r, & 
211.70_pm_r, 78993._pm_r,  14._pm_r,350._pm_r,  0.82_pm_r,331._pm_r,   5._pm_r,275._pm_r,  0.78_pm_r, 23._pm_r, & 
208.70_pm_r, 82076._pm_r,  15._pm_r,349._pm_r,  0.87_pm_r,337._pm_r,   5._pm_r,289._pm_r,  0.78_pm_r, 18._pm_r, & 
203.00_pm_r, 85105._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
195.10_pm_r, 88018._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
188.50_pm_r, 90820._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.20_pm_r, 93586._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.00_pm_r, 96353._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.90_pm_r, 99144._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
191.00_pm_r,101988._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.30_pm_r,104958._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
221.50_pm_r,108200._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
274.30_pm_r,112059._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
359.30_pm_r,117146._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40 = (/ &      ! donnees latitude 40
284.40_pm_r,    -7._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
258.70_pm_r,  3969._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
234.00_pm_r,  7575._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
219.00_pm_r, 10891._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
215.10_pm_r, 14069._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
214.00_pm_r, 17210._pm_r,   7._pm_r,267._pm_r, 3.67_pm_r,167._pm_r,   4._pm_r, 13._pm_r,  0.94_pm_r,349._pm_r, & 
214.70_pm_r, 20346._pm_r,   8._pm_r,220._pm_r, 4.45_pm_r,161._pm_r,   5._pm_r,  4._pm_r, 1.42_pm_r,344._pm_r, & 
217.90_pm_r, 23514._pm_r,  12._pm_r,192._pm_r, 4.11_pm_r,149._pm_r,   8._pm_r,357._pm_r, 1.84_pm_r,339._pm_r, & 
222.20_pm_r, 26734._pm_r,  16._pm_r,177._pm_r, 3.16_pm_r,120._pm_r,  10._pm_r,351._pm_r, 2.10_pm_r,331._pm_r, & 
227.80_pm_r, 30031._pm_r,  17._pm_r,162._pm_r, 3.38_pm_r, 69._pm_r,  13._pm_r,345._pm_r, 2.17_pm_r,320._pm_r, & 
234.30_pm_r, 33411._pm_r,  16._pm_r,141._pm_r, 5.19_pm_r, 38._pm_r,  16._pm_r,339._pm_r, 2.03_pm_r,305._pm_r, & 
242.50_pm_r, 36903._pm_r,  15._pm_r,108._pm_r, 6.70_pm_r, 25._pm_r,  18._pm_r,333._pm_r, 1.82_pm_r,283._pm_r, & 
252.00_pm_r, 40520._pm_r,  19._pm_r, 75._pm_r, 7.10_pm_r, 17._pm_r,  19._pm_r,326._pm_r, 1.73_pm_r,256._pm_r, & 
260.00_pm_r, 44277._pm_r,  25._pm_r, 56._pm_r, 5.28_pm_r, 12._pm_r,  20._pm_r,319._pm_r, 1.62_pm_r,230._pm_r, & 
261.30_pm_r, 48104._pm_r,  29._pm_r, 47._pm_r, 3.21_pm_r,  4._pm_r,  20._pm_r,313._pm_r, 1.49_pm_r,211._pm_r, & 
253.60_pm_r, 51879._pm_r,  31._pm_r, 42._pm_r, 1.34_pm_r,340._pm_r,  19._pm_r,307._pm_r, 1.20_pm_r,186._pm_r, & 
242.00_pm_r, 55513._pm_r,  31._pm_r, 40._pm_r, 1.15_pm_r,253._pm_r,  18._pm_r,304._pm_r,  0.99_pm_r,140._pm_r, & 
233.40_pm_r, 58987._pm_r,  29._pm_r, 38._pm_r, 1.74_pm_r,238._pm_r,  16._pm_r,303._pm_r, 1.17_pm_r,122._pm_r, & 
228.50_pm_r, 62368._pm_r,  27._pm_r, 36._pm_r, 1.86_pm_r,237._pm_r,  14._pm_r,304._pm_r, 1.08_pm_r,111._pm_r, & 
224.70_pm_r, 65686._pm_r,  24._pm_r, 34._pm_r, 1.55_pm_r,243._pm_r,  13._pm_r,306._pm_r,  0.80_pm_r,102._pm_r, & 
221.30_pm_r, 68951._pm_r,  23._pm_r, 31._pm_r, 1.11_pm_r,259._pm_r,  12._pm_r,308._pm_r,  0.44_pm_r, 84._pm_r, & 
220.40_pm_r, 72184._pm_r,  22._pm_r, 28._pm_r,  0.86_pm_r,289._pm_r,  12._pm_r,310._pm_r,  0.21_pm_r, 28._pm_r, & 
219.00_pm_r, 75404._pm_r,  22._pm_r, 25._pm_r,  0.89_pm_r,320._pm_r,  12._pm_r,311._pm_r,  0.34_pm_r,328._pm_r, & 
216.70_pm_r, 78592._pm_r,  23._pm_r, 22._pm_r, 1.02_pm_r,336._pm_r,  13._pm_r,311._pm_r,  0.49_pm_r,314._pm_r, & 
214.10_pm_r, 81749._pm_r,  24._pm_r, 20._pm_r, 1.10_pm_r,345._pm_r,  14._pm_r,311._pm_r,  0.58_pm_r,309._pm_r, & 
208.90_pm_r, 84859._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.30_pm_r, 87862._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
192.00_pm_r, 90725._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
189.30_pm_r, 93529._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
188.70_pm_r, 96321._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
190.00_pm_r, 99138._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
194.20_pm_r,102019._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
204.00_pm_r,105040._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.60_pm_r,108339._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
274.70_pm_r,112235._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
355.70_pm_r,117281._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)       


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50 = (/ &      ! donnees latitude 50
278.10_pm_r,   -81._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
252.00_pm_r,  3796._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
228.60_pm_r,  7311._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
218.20_pm_r, 10579._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
217.90_pm_r, 13768._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
217.40_pm_r, 16957._pm_r,   6._pm_r,277._pm_r, 6.92_pm_r,182._pm_r,  13._pm_r, 40._pm_r, 2.42_pm_r,333._pm_r, & 
216.80_pm_r, 20136._pm_r,  13._pm_r,207._pm_r, 9.06_pm_r,178._pm_r,  15._pm_r, 25._pm_r, 3.36_pm_r,330._pm_r, & 
217.00_pm_r, 23309._pm_r,  25._pm_r,190._pm_r, 9.48_pm_r,171._pm_r,  18._pm_r, 11._pm_r, 3.85_pm_r,327._pm_r, & 
220.40_pm_r, 26507._pm_r,  37._pm_r,182._pm_r, 8.08_pm_r,157._pm_r,  23._pm_r,  0._pm_r, 3.75_pm_r,321._pm_r, & 
226.10_pm_r, 29778._pm_r,  46._pm_r,174._pm_r, 6.00_pm_r,127._pm_r,  26._pm_r,352._pm_r, 3.10_pm_r,309._pm_r, & 
232.00_pm_r, 33130._pm_r,  49._pm_r,165._pm_r, 6.00_pm_r, 80._pm_r,  29._pm_r,346._pm_r, 2.30_pm_r,286._pm_r, & 
239.20_pm_r, 36581._pm_r,  48._pm_r,153._pm_r, 8.02_pm_r, 49._pm_r,  30._pm_r,341._pm_r, 1.92_pm_r,249._pm_r, & 
247.10_pm_r, 40141._pm_r,  45._pm_r,138._pm_r, 9.66_pm_r, 34._pm_r,  29._pm_r,335._pm_r, 2.17_pm_r,216._pm_r, & 
253.40_pm_r, 43811._pm_r,  42._pm_r,120._pm_r, 8.74_pm_r, 23._pm_r,  27._pm_r,330._pm_r, 2.09_pm_r,211._pm_r, & 
255.40_pm_r, 47543._pm_r,  41._pm_r,104._pm_r, 6.95_pm_r, 11._pm_r,  26._pm_r,324._pm_r, 2.02_pm_r,215._pm_r, & 
250.70_pm_r, 51254._pm_r,  41._pm_r, 92._pm_r, 5.13_pm_r,351._pm_r,  25._pm_r,317._pm_r, 2.04_pm_r,213._pm_r, & 
240.90_pm_r, 54860._pm_r,  38._pm_r, 83._pm_r, 4.07_pm_r,318._pm_r,  24._pm_r,311._pm_r, 2.10_pm_r,199._pm_r, & 
232.80_pm_r, 58324._pm_r,  34._pm_r, 77._pm_r, 4.01_pm_r,292._pm_r,  23._pm_r,304._pm_r, 2.16_pm_r,177._pm_r, & 
228.00_pm_r, 61696._pm_r,  29._pm_r, 72._pm_r, 4.03_pm_r,271._pm_r,  21._pm_r,298._pm_r, 2.03_pm_r,158._pm_r, & 
226.00_pm_r, 65017._pm_r,  23._pm_r, 69._pm_r, 3.89_pm_r,254._pm_r,  18._pm_r,294._pm_r, 1.69_pm_r,142._pm_r, & 
225.20_pm_r, 68321._pm_r,  18._pm_r, 70._pm_r, 3.62_pm_r,242._pm_r,  16._pm_r,291._pm_r, 1.33_pm_r,122._pm_r, & 
224.60_pm_r, 71614._pm_r,  13._pm_r, 75._pm_r, 3.33_pm_r,231._pm_r,  15._pm_r,291._pm_r, 1.11_pm_r, 98._pm_r, & 
223.90_pm_r, 74898._pm_r,   9._pm_r, 89._pm_r, 3.09_pm_r,223._pm_r,  13._pm_r,294._pm_r, 1.06_pm_r, 75._pm_r, & 
222.50_pm_r, 78168._pm_r,   7._pm_r,118._pm_r, 2.84_pm_r,216._pm_r,  12._pm_r,300._pm_r, 1.10_pm_r, 59._pm_r, & 
220.30_pm_r, 81417._pm_r,   8._pm_r,149._pm_r, 2.55_pm_r,211._pm_r,  12._pm_r,307._pm_r, 1.10_pm_r, 50._pm_r, & 
215.10_pm_r, 84614._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
206.50_pm_r, 87718._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
197.20_pm_r, 90670._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
192.70_pm_r, 93534._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
191.70_pm_r, 96371._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
193.20_pm_r, 99230._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
198.60_pm_r,102167._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
209.30_pm_r,105259._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
230.90_pm_r,108638._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
275.00_pm_r,112581._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
352.00_pm_r,117578._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)    

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60 = (/ &      ! donnees latitude 60
267.00_pm_r,  -108._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
247.50_pm_r,  3652._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.20_pm_r,  7106._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
216.90_pm_r, 10336._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
217.00_pm_r, 13507._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
216.70_pm_r, 16684._pm_r,   2._pm_r,257._pm_r, 9.98_pm_r,188._pm_r,  21._pm_r, 40._pm_r, 3.63_pm_r,342._pm_r, & 
215.60_pm_r, 19851._pm_r,  18._pm_r,193._pm_r,13.83_pm_r,184._pm_r,  25._pm_r, 27._pm_r, 4.90_pm_r,339._pm_r, & 
213.20_pm_r, 22986._pm_r,  40._pm_r,187._pm_r,15.56_pm_r,178._pm_r,  30._pm_r, 16._pm_r, 5.29_pm_r,334._pm_r, & 
215.60_pm_r, 26118._pm_r,  62._pm_r,182._pm_r,14.13_pm_r,169._pm_r,  35._pm_r,  7._pm_r, 4.50_pm_r,323._pm_r, & 
223.30_pm_r, 29332._pm_r,  79._pm_r,178._pm_r,10.63_pm_r,153._pm_r,  39._pm_r,  1._pm_r, 3.11_pm_r,299._pm_r, & 
230.80_pm_r, 32656._pm_r,  90._pm_r,173._pm_r, 7.70_pm_r,119._pm_r,  40._pm_r,355._pm_r, 2.52_pm_r,252._pm_r, & 
238.50_pm_r, 36094._pm_r,  92._pm_r,166._pm_r, 7.77_pm_r, 75._pm_r,  37._pm_r,350._pm_r, 3.22_pm_r,213._pm_r, & 
246.20_pm_r, 39642._pm_r,  90._pm_r,158._pm_r, 9.75_pm_r, 46._pm_r,  33._pm_r,345._pm_r, 3.93_pm_r,195._pm_r, & 
250.80_pm_r, 43285._pm_r,  83._pm_r,150._pm_r,10.16_pm_r, 31._pm_r,  29._pm_r,340._pm_r, 3.16_pm_r,191._pm_r, & 
253.80_pm_r, 46981._pm_r,  76._pm_r,141._pm_r, 9.45_pm_r, 17._pm_r,  25._pm_r,335._pm_r, 2.49_pm_r,199._pm_r, & 
252.40_pm_r, 50695._pm_r,  68._pm_r,133._pm_r, 8.30_pm_r,  0._pm_r,  23._pm_r,328._pm_r, 2.45_pm_r,209._pm_r, & 
244.80_pm_r, 54342._pm_r,  59._pm_r,126._pm_r, 7.30_pm_r,338._pm_r,  22._pm_r,319._pm_r, 2.85_pm_r,208._pm_r, & 
237.90_pm_r, 57872._pm_r,  49._pm_r,121._pm_r, 7.01_pm_r,318._pm_r,  20._pm_r,309._pm_r, 2.68_pm_r,193._pm_r, & 
232.60_pm_r, 61316._pm_r,  40._pm_r,120._pm_r, 6.61_pm_r,298._pm_r,  18._pm_r,299._pm_r, 2.30_pm_r,179._pm_r, & 
229.50_pm_r, 64697._pm_r,  31._pm_r,123._pm_r, 5.96_pm_r,280._pm_r,  17._pm_r,291._pm_r, 1.77_pm_r,164._pm_r, & 
228.10_pm_r, 68047._pm_r,  24._pm_r,133._pm_r, 5.31_pm_r,263._pm_r,  15._pm_r,285._pm_r, 1.28_pm_r,146._pm_r, & 
227.10_pm_r, 71380._pm_r,  21._pm_r,150._pm_r, 4.83_pm_r,247._pm_r,  14._pm_r,282._pm_r,  0.94_pm_r,121._pm_r, & 
226.10_pm_r, 74699._pm_r,  22._pm_r,168._pm_r, 4.58_pm_r,233._pm_r,  12._pm_r,281._pm_r,  0.85_pm_r, 93._pm_r, & 
225.30_pm_r, 78006._pm_r,  26._pm_r,181._pm_r, 4.40_pm_r,223._pm_r,  11._pm_r,283._pm_r,  0.88_pm_r, 74._pm_r, & 
223.80_pm_r, 81299._pm_r,  31._pm_r,188._pm_r, 4.13_pm_r,215._pm_r,  10._pm_r,287._pm_r,  0.92_pm_r, 62._pm_r, & 
220.30_pm_r, 84555._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
213.00_pm_r, 87750._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.90_pm_r, 90797._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
197.20_pm_r, 93731._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
195.40_pm_r, 96625._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
197.30_pm_r, 99538._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
203.50_pm_r,102538._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
214.90_pm_r,105707._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
236.00_pm_r,109170._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
274.50_pm_r,113151._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
348.50_pm_r,118092._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)      

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70 = (/ &      ! donnees latitude 70
254.20_pm_r,  -106._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
243.80_pm_r,  3530._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
222.70_pm_r,  6936._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
215.20_pm_r, 10134._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
214.40_pm_r, 13271._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
214.10_pm_r, 16408._pm_r,   4._pm_r,  7._pm_r,10.52_pm_r,193._pm_r,  15._pm_r, 34._pm_r, 3.31_pm_r,  2._pm_r, & 
212.50_pm_r, 19536._pm_r,  15._pm_r,193._pm_r,15.17_pm_r,189._pm_r,  20._pm_r, 25._pm_r, 4.44_pm_r,359._pm_r, & 
207.20_pm_r, 22602._pm_r,  39._pm_r,189._pm_r,17.49_pm_r,184._pm_r,  26._pm_r, 18._pm_r, 4.55_pm_r,353._pm_r, & 
208.80_pm_r, 25639._pm_r,  64._pm_r,185._pm_r,16.23_pm_r,175._pm_r,  31._pm_r, 12._pm_r, 3.47_pm_r,340._pm_r, & 
218.00_pm_r, 28763._pm_r,  84._pm_r,181._pm_r,12.56_pm_r,162._pm_r,  34._pm_r,  7._pm_r, 2.09_pm_r,304._pm_r, & 
228.10_pm_r, 32028._pm_r,  98._pm_r,177._pm_r, 8.86_pm_r,136._pm_r,  34._pm_r,  3._pm_r, 2.18_pm_r,244._pm_r, & 
237.60_pm_r, 35442._pm_r, 104._pm_r,171._pm_r, 7.52_pm_r, 95._pm_r,  31._pm_r,358._pm_r, 3.17_pm_r,215._pm_r, & 
245.40_pm_r, 38979._pm_r, 103._pm_r,165._pm_r, 8.74_pm_r, 61._pm_r,  27._pm_r,352._pm_r, 3.73_pm_r,202._pm_r, & 
249.90_pm_r, 42610._pm_r,  98._pm_r,158._pm_r, 9.37_pm_r, 40._pm_r,  23._pm_r,347._pm_r, 2.82_pm_r,192._pm_r, & 
253.40_pm_r, 46294._pm_r,  91._pm_r,151._pm_r, 9.15_pm_r, 22._pm_r,  19._pm_r,344._pm_r, 1.98_pm_r,178._pm_r, & 
254.90_pm_r, 50020._pm_r,  82._pm_r,145._pm_r, 8.59_pm_r,  3._pm_r,  17._pm_r,343._pm_r, 1.65_pm_r,165._pm_r, & 
251.80_pm_r, 53736._pm_r,  71._pm_r,141._pm_r, 8.29_pm_r,341._pm_r,  14._pm_r,343._pm_r, 1.70_pm_r,163._pm_r, & 
246.30_pm_r, 57385._pm_r,  59._pm_r,138._pm_r, 8.25_pm_r,324._pm_r,  12._pm_r,343._pm_r, 1.48_pm_r,161._pm_r, & 
239.10_pm_r, 60942._pm_r,  48._pm_r,138._pm_r, 7.74_pm_r,310._pm_r,  10._pm_r,344._pm_r, 1.11_pm_r,161._pm_r, & 
232.80_pm_r, 64393._pm_r,  38._pm_r,143._pm_r, 6.67_pm_r,296._pm_r,   9._pm_r,344._pm_r,  0.70_pm_r,164._pm_r, & 
228.60_pm_r, 67771._pm_r,  31._pm_r,152._pm_r, 5.45_pm_r,281._pm_r,   8._pm_r,343._pm_r,  0.39_pm_r,178._pm_r, & 
226.40_pm_r, 71101._pm_r,  28._pm_r,165._pm_r, 4.41_pm_r,264._pm_r,   8._pm_r,342._pm_r,  0.21_pm_r,214._pm_r, & 
225.30_pm_r, 74409._pm_r,  28._pm_r,177._pm_r, 3.78_pm_r,246._pm_r,   8._pm_r,340._pm_r,  0.23_pm_r,257._pm_r, & 
224.70_pm_r, 77703._pm_r,  31._pm_r,186._pm_r, 3.49_pm_r,229._pm_r,   8._pm_r,337._pm_r,  0.29_pm_r,278._pm_r, & 
224.30_pm_r, 80990._pm_r,  35._pm_r,190._pm_r, 3.32_pm_r,217._pm_r,   8._pm_r,334._pm_r,  0.33_pm_r,286._pm_r, & 
223.10_pm_r, 84265._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
218.00_pm_r, 87517._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
208.00_pm_r, 90641._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.40_pm_r, 93642._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
199.00_pm_r, 96590._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.10_pm_r, 99554._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
207.90_pm_r,102614._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
219.90_pm_r,105853._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
239.80_pm_r,109385._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
273.10_pm_r,113387._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
345.40_pm_r,118272._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)     

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80 = (/ &      ! donnees latitude 80
248.50_pm_r,  -108._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
241.10_pm_r,  3466._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
220.90_pm_r,  6838._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
214.10_pm_r, 10013._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
212.30_pm_r, 13125._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
210.70_pm_r, 16221._pm_r,   5._pm_r, 26._pm_r, 6.11_pm_r,200._pm_r,   6._pm_r, 30._pm_r, 1.07_pm_r, 19._pm_r, & 
207.80_pm_r, 19290._pm_r,   7._pm_r,192._pm_r, 9.24_pm_r,195._pm_r,   8._pm_r, 27._pm_r, 1.48_pm_r, 15._pm_r, & 
201.60_pm_r, 22280._pm_r,  22._pm_r,191._pm_r,11.23_pm_r,188._pm_r,  10._pm_r, 23._pm_r, 1.53_pm_r,  7._pm_r, & 
202.30_pm_r, 25227._pm_r,  38._pm_r,188._pm_r,10.82_pm_r,179._pm_r,  12._pm_r, 19._pm_r, 1.14_pm_r,350._pm_r, & 
212.30_pm_r, 28260._pm_r,  52._pm_r,184._pm_r, 8.89_pm_r,166._pm_r,  13._pm_r, 15._pm_r,  0.76_pm_r,309._pm_r, & 
225.20_pm_r, 31459._pm_r,  63._pm_r,179._pm_r, 6.73_pm_r,146._pm_r,  13._pm_r, 10._pm_r,  0.85_pm_r,260._pm_r, & 
236.40_pm_r, 34845._pm_r,  69._pm_r,174._pm_r, 5.43_pm_r,116._pm_r,  12._pm_r,  5._pm_r, 1.09_pm_r,235._pm_r, & 
244.90_pm_r, 38369._pm_r,  71._pm_r,168._pm_r, 5.24_pm_r, 84._pm_r,  11._pm_r,359._pm_r, 1.19_pm_r,220._pm_r, & 
248.70_pm_r, 41988._pm_r,  71._pm_r,162._pm_r, 5.26_pm_r, 57._pm_r,  10._pm_r,356._pm_r,  0.53_pm_r,157._pm_r, & 
251.70_pm_r, 45650._pm_r,  68._pm_r,156._pm_r, 5.06_pm_r, 36._pm_r,  10._pm_r,  2._pm_r, 1.02_pm_r, 92._pm_r, & 
255.10_pm_r, 49362._pm_r,  63._pm_r,152._pm_r, 4.88_pm_r, 11._pm_r,  10._pm_r, 11._pm_r, 1.11_pm_r, 85._pm_r, & 
256.00_pm_r, 53108._pm_r,  57._pm_r,148._pm_r, 5.20_pm_r,346._pm_r,  10._pm_r, 18._pm_r,  0.43_pm_r,121._pm_r, & 
252.70_pm_r, 56840._pm_r,  49._pm_r,147._pm_r, 5.70_pm_r,332._pm_r,   9._pm_r, 18._pm_r,  0.85_pm_r,218._pm_r, & 
245.30_pm_r, 60491._pm_r,  41._pm_r,147._pm_r, 5.56_pm_r,321._pm_r,   8._pm_r, 13._pm_r, 1.41_pm_r,229._pm_r, & 
237.40_pm_r, 64023._pm_r,  33._pm_r,149._pm_r, 4.73_pm_r,312._pm_r,   6._pm_r,  1._pm_r, 1.51_pm_r,232._pm_r, & 
231.40_pm_r, 67455._pm_r,  28._pm_r,154._pm_r, 3.63_pm_r,299._pm_r,   5._pm_r,343._pm_r, 1.30_pm_r,232._pm_r, & 
228.10_pm_r, 70818._pm_r,  24._pm_r,161._pm_r, 2.64_pm_r,281._pm_r,   5._pm_r,324._pm_r,  0.98_pm_r,231._pm_r, & 
226.40_pm_r, 74145._pm_r,  24._pm_r,169._pm_r, 2.08_pm_r,255._pm_r,   5._pm_r,310._pm_r,  0.67_pm_r,228._pm_r, & 
225.60_pm_r, 77453._pm_r,  25._pm_r,175._pm_r, 2.00_pm_r,229._pm_r,   5._pm_r,301._pm_r,  0.42_pm_r,221._pm_r, & 
225.40_pm_r, 80752._pm_r,  27._pm_r,180._pm_r, 2.09_pm_r,211._pm_r,   5._pm_r,296._pm_r,  0.25_pm_r,209._pm_r, & 
225.20_pm_r, 84047._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
221.30_pm_r, 87333._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
211.50_pm_r, 90510._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
204.30_pm_r, 93557._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.50_pm_r, 96544._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
203.90_pm_r, 99546._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
211.00_pm_r,102648._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
223.40_pm_r,105935._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
242.10_pm_r,109511._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
271.70_pm_r,113520._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
343.20_pm_r,118363._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)      
 
  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_janvier = &      ! donnees toutes latitudes
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_janvier,(/10,nb_alt,nb_lat/))

  
  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)

end subroutine cps_atmi_janvier

subroutine cps_atmi_fevrier (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_fevrier
!
!$Resume
!   Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de FEVRIER
!
!$Description
!   Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de FEVRIER
!
!$Auteur
!   Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_fevrier (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>             
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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

  ! (C) Copyright CNES - MSPRO - 2000



  ! Modules
  ! =======

  use mslib


  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour
    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------

  real(pm_reel),dimension(10*nb_alt),parameter :: donnees_lat_moins_80= (/ &
266.40_pm_r, -212._pm_r,0._pm_r,0._pm_r, 0.00_pm_r, 0._pm_r,  0._pm_r, 0._pm_r, 0.00_pm_r, 0._pm_r,&
248.00_pm_r, 3542._pm_r,0._pm_r,0._pm_r, 0.00_pm_r, 0._pm_r,  0._pm_r, 0._pm_r, 0.00_pm_r, 0._pm_r,&
226.10_pm_r, 7002._pm_r,0._pm_r,0._pm_r, 0.00_pm_r, 0._pm_r,  0._pm_r, 0._pm_r, 0.00_pm_r, 0._pm_r,&
226.90_pm_r,10309._pm_r,0._pm_r,0._pm_r, 0.00_pm_r, 0._pm_r,  0._pm_r, 0._pm_r, 0.00_pm_r, 0._pm_r,&
230.20_pm_r,13645._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
231.40_pm_r,17026._pm_r,4._pm_r,217._pm_r,0.38_pm_r,145._pm_r,2._pm_r,224._pm_r,0.15_pm_r,18._pm_r, &
232.30_pm_r,20422._pm_r,4._pm_r,209._pm_r,0.47_pm_r,138._pm_r,1._pm_r,228._pm_r,0.20_pm_r,18._pm_r, &
232.80_pm_r,23830._pm_r,5._pm_r,201._pm_r,0.49_pm_r,125._pm_r,1._pm_r,236._pm_r,0.21_pm_r,22._pm_r, &
234.00_pm_r,27243._pm_r,5._pm_r,193._pm_r,0.42_pm_r,101._pm_r,1._pm_r,245._pm_r,0.17_pm_r,31._pm_r, &
241.90_pm_r,30722._pm_r,5._pm_r,186._pm_r,0.39_pm_r,66._pm_r,1._pm_r,254._pm_r,0.11_pm_r,49._pm_r, &
252.30_pm_r,34338._pm_r,4._pm_r,180._pm_r,0.43_pm_r,34._pm_r,1._pm_r,255._pm_r,0.06_pm_r,108._pm_r, &
262.60_pm_r,38112._pm_r,4._pm_r,177._pm_r,0.45_pm_r,11._pm_r,1._pm_r,242._pm_r,0.11_pm_r,162._pm_r, &
272.10_pm_r,42026._pm_r,3._pm_r,175._pm_r,0.41_pm_r,352._pm_r,1._pm_r,224._pm_r,0.16_pm_r,176._pm_r, &
278.20_pm_r,46062._pm_r,3._pm_r,177._pm_r,0.20_pm_r,339._pm_r,1._pm_r,217._pm_r,0.13_pm_r,225._pm_r, &
278.60_pm_r,50146._pm_r,2._pm_r,177._pm_r,0.09_pm_r,111._pm_r,1._pm_r,227._pm_r,0.26_pm_r,281._pm_r, &
272.40_pm_r,54185._pm_r,3._pm_r,172._pm_r,0.34_pm_r,125._pm_r,1._pm_r,246._pm_r,0.37_pm_r,288._pm_r, &
263.60_pm_r,58114._pm_r,3._pm_r,163._pm_r,0.50_pm_r,121._pm_r,2._pm_r,256._pm_r,0.31_pm_r,270._pm_r, &
253.00_pm_r,61896._pm_r,4._pm_r,157._pm_r,0.40_pm_r,139._pm_r,2._pm_r,252._pm_r,0.28_pm_r,196._pm_r, &
240.50_pm_r,65516._pm_r,4._pm_r,156._pm_r,0.30_pm_r,166._pm_r,2._pm_r,238._pm_r,0.49_pm_r,165._pm_r, &
225.50_pm_r,68925._pm_r,4._pm_r,159._pm_r,0.25_pm_r,205._pm_r,3._pm_r,219._pm_r,0.63_pm_r,152._pm_r, &
211.20_pm_r,72126._pm_r,5._pm_r,163._pm_r,0.27_pm_r,236._pm_r,3._pm_r,202._pm_r,0.65_pm_r,146._pm_r, &
198.00_pm_r,75118._pm_r,5._pm_r,167._pm_r,0.28_pm_r,253._pm_r,4._pm_r,189._pm_r,0.61_pm_r,142._pm_r, &
185.50_pm_r,77930._pm_r,5._pm_r,173._pm_r,0.29_pm_r,262._pm_r,4._pm_r,180._pm_r,0.55_pm_r,139._pm_r, &
175.00_pm_r,80562._pm_r,5._pm_r,178._pm_r,0.27_pm_r,270._pm_r,5._pm_r,174._pm_r,0.46_pm_r,135._pm_r, &
163.30_pm_r,83051._pm_r,5._pm_r,182._pm_r,0.24_pm_r,272._pm_r,5._pm_r,170._pm_r,0.37_pm_r,133._pm_r, &
151.70_pm_r,85304._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
146.80_pm_r,87448._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
147.80_pm_r,89596._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
153.60_pm_r,91806._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
164.40_pm_r,94147._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.60_pm_r,96704._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.20_pm_r,99607._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.80_pm_r,103017._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
282.40_pm_r,107076._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
312.50_pm_r,111733._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
392.90_pm_r,117308._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel  /)

  real(pm_reel),dimension(10*nb_alt),parameter :: donnees_lat_moins_70= (/ &
       
271.70_pm_r,-202._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.90_pm_r,3607._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.10_pm_r,7097._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.00_pm_r,10420._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.40_pm_r,13753._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.20_pm_r,17119._pm_r,5._pm_r,223._pm_r,0.35_pm_r,104._pm_r,1._pm_r,237._pm_r,0.17_pm_r,59._pm_r, &
230.80_pm_r,20495._pm_r,5._pm_r,217._pm_r,0.47_pm_r,95._pm_r,1._pm_r,236._pm_r,0.25_pm_r,61._pm_r, &
230.90_pm_r,23875._pm_r,4._pm_r,209._pm_r,0.53_pm_r,80._pm_r,0._pm_r,227._pm_r,0.30_pm_r,62._pm_r, &
233.40_pm_r,27269._pm_r,4._pm_r,201._pm_r,0.55_pm_r,61._pm_r,0._pm_r,74._pm_r,0.30_pm_r,63._pm_r, &
241.60_pm_r,30744._pm_r,3._pm_r,193._pm_r,0.56_pm_r,38._pm_r,1._pm_r,68._pm_r,0.28_pm_r,65._pm_r, &
251.60_pm_r,34353._pm_r,2._pm_r,188._pm_r,0.57_pm_r,16._pm_r,1._pm_r,67._pm_r,0.22_pm_r,66._pm_r, &
261.40_pm_r,38113._pm_r,1._pm_r,188._pm_r,0.53_pm_r,357._pm_r,1._pm_r,67._pm_r,0.16_pm_r,64._pm_r, &
270.50_pm_r,42006._pm_r,1._pm_r,207._pm_r,0.42_pm_r,335._pm_r,2._pm_r,66._pm_r,0.11_pm_r,59._pm_r, &
276.20_pm_r,46017._pm_r,1._pm_r,241._pm_r,0.19_pm_r,293._pm_r,2._pm_r,64._pm_r,0.07_pm_r,0._pm_r, &
275.90_pm_r,50068._pm_r,1._pm_r,242._pm_r,0.24_pm_r,205._pm_r,2._pm_r,60._pm_r,0.12_pm_r,318._pm_r, &
269.40_pm_r,54064._pm_r,1._pm_r,225._pm_r,0.35_pm_r,175._pm_r,2._pm_r,53._pm_r,0.15_pm_r,316._pm_r, &
260.00_pm_r,57944._pm_r,2._pm_r,209._pm_r,0.29_pm_r,145._pm_r,2._pm_r,46._pm_r,0.12_pm_r,340._pm_r, &
249.40_pm_r,61673._pm_r,2._pm_r,198._pm_r,0.16_pm_r,103._pm_r,2._pm_r,43._pm_r,0.11_pm_r,37._pm_r, &
237.60_pm_r,65245._pm_r,2._pm_r,190._pm_r,0.25_pm_r,38._pm_r,2._pm_r,44._pm_r,0.14_pm_r,63._pm_r, &
224.50_pm_r,68626._pm_r,1._pm_r,183._pm_r,0.41_pm_r,19._pm_r,2._pm_r,46._pm_r,0.13_pm_r,76._pm_r, &
211.30_pm_r,71820._pm_r,0._pm_r,159._pm_r,0.52_pm_r,13._pm_r,2._pm_r,49._pm_r,0.11_pm_r,87._pm_r, &
198.80_pm_r,74819._pm_r,1._pm_r,32._pm_r,0.58_pm_r,10._pm_r,2._pm_r,51._pm_r,0.07_pm_r,103._pm_r, &
186.70_pm_r,77645._pm_r,1._pm_r,19._pm_r,0.59_pm_r,9._pm_r,2._pm_r,53._pm_r,0.05_pm_r,139._pm_r, &
176.70_pm_r,80296._pm_r,2._pm_r,15._pm_r,0.54_pm_r,8._pm_r,2._pm_r,54._pm_r,0.03_pm_r,160._pm_r, &
166.20_pm_r,82813._pm_r,3._pm_r,13._pm_r,0.48_pm_r,8._pm_r,2._pm_r,55._pm_r,0.04_pm_r,188._pm_r, &
156.10_pm_r,85133._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
151.50_pm_r,87352._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
151.70_pm_r,89563._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
156.70_pm_r,91825._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
166.60_pm_r,94205._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.20_pm_r,96785._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.90_pm_r,99685._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
239.60_pm_r,103054._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.90_pm_r,107042._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
312.80_pm_r,111668._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
391.90_pm_r,117248._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel  /)

  real(pm_reel),dimension(10*nb_alt),parameter :: donnees_lat_moins_60= (/ &
       
277.00_pm_r,-138._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
254.90_pm_r,3749._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
232.10_pm_r,7308._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.50_pm_r,10652._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.90_pm_r,13950._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.70_pm_r,17264._pm_r,6._pm_r,220._pm_r,0.71_pm_r,61._pm_r,0._pm_r,14._pm_r,0.19_pm_r,104._pm_r, &
227.80_pm_r,20593._pm_r,5._pm_r,216._pm_r,0.93_pm_r,58._pm_r,1._pm_r,57._pm_r,0.27_pm_r,103._pm_r, &
228.40_pm_r,23931._pm_r,4._pm_r,209._pm_r,1.01_pm_r,52._pm_r,1._pm_r,78._pm_r,0.32_pm_r,101._pm_r, &
232.70_pm_r,27300._pm_r,3._pm_r,199._pm_r,0.89_pm_r,42._pm_r,1._pm_r,86._pm_r,0.32_pm_r,99._pm_r, &
240.90_pm_r,30768._pm_r,2._pm_r,189._pm_r,0.66_pm_r,25._pm_r,2._pm_r,89._pm_r,0.29_pm_r,96._pm_r, &
250.50_pm_r,34363._pm_r,1._pm_r,185._pm_r,0.47_pm_r,354._pm_r,2._pm_r,90._pm_r,0.22_pm_r,87._pm_r, &
259.90_pm_r,38104._pm_r,0._pm_r,232._pm_r,0.41_pm_r,314._pm_r,2._pm_r,89._pm_r,0.16_pm_r,74._pm_r, &
268.90_pm_r,41974._pm_r,1._pm_r,271._pm_r,0.40_pm_r,282._pm_r,3._pm_r,87._pm_r,0.11_pm_r,45._pm_r, &
274.40_pm_r,45961._pm_r,1._pm_r,270._pm_r,0.24_pm_r,254._pm_r,3._pm_r,84._pm_r,0.12_pm_r,358._pm_r, &
273.10_pm_r,49977._pm_r,2._pm_r,263._pm_r,0.20_pm_r,195._pm_r,3._pm_r,80._pm_r,0.13_pm_r,4._pm_r, &
266.40_pm_r,53931._pm_r,2._pm_r,251._pm_r,0.27_pm_r,163._pm_r,3._pm_r,76._pm_r,0.15_pm_r,38._pm_r, &
256.40_pm_r,57764._pm_r,2._pm_r,236._pm_r,0.29_pm_r,155._pm_r,3._pm_r,75._pm_r,0.21_pm_r,67._pm_r, &
245.40_pm_r,61436._pm_r,2._pm_r,222._pm_r,0.23_pm_r,130._pm_r,3._pm_r,74._pm_r,0.18_pm_r,67._pm_r, &
234.40_pm_r,64953._pm_r,2._pm_r,212._pm_r,0.25_pm_r,76._pm_r,4._pm_r,73._pm_r,0.13_pm_r,45._pm_r, &
223.50_pm_r,68304._pm_r,1._pm_r,202._pm_r,0.40_pm_r,44._pm_r,4._pm_r,71._pm_r,0.13_pm_r,353._pm_r, &
212.00_pm_r,71497._pm_r,1._pm_r,178._pm_r,0.56_pm_r,33._pm_r,4._pm_r,67._pm_r,0.21_pm_r,331._pm_r, &
200.00_pm_r,74510._pm_r,1._pm_r,56._pm_r,0.66_pm_r,27._pm_r,4._pm_r,62._pm_r,0.26_pm_r,320._pm_r, &
188.50_pm_r,77357._pm_r,2._pm_r,36._pm_r,0.71_pm_r,24._pm_r,3._pm_r,55._pm_r,0.29_pm_r,316._pm_r, &
179.20_pm_r,80040._pm_r,3._pm_r,31._pm_r,0.70_pm_r,22._pm_r,3._pm_r,48._pm_r,0.29_pm_r,316._pm_r, &
170.70_pm_r,82601._pm_r,4._pm_r,28._pm_r,0.65_pm_r,22._pm_r,3._pm_r,41._pm_r,0.28_pm_r,314._pm_r, &
163.10_pm_r,85026._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
158.70_pm_r,87362._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
157.70_pm_r,89670._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
161.50_pm_r,92012._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
169.80_pm_r,94453._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.20_pm_r,97067._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.90_pm_r,99963._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
233.60_pm_r,103273._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
270.90_pm_r,107157._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
312.30_pm_r,111723._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
390.30_pm_r,117301._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel  /)

  real(pm_reel),dimension(10*nb_alt),parameter :: donnees_lat_moins_50= (/ &
       
282.30_pm_r,-27._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.70_pm_r,3959._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
238.60_pm_r,7625._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.40_pm_r,11004._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.40_pm_r,14242._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.70_pm_r,17455._pm_r,6._pm_r,208._pm_r,0.74_pm_r,31._pm_r,2._pm_r,45._pm_r,0.16_pm_r,174._pm_r, &
222.20_pm_r,20689._pm_r,5._pm_r,208._pm_r,0.95_pm_r,29._pm_r,1._pm_r,53._pm_r,0.22_pm_r,175._pm_r, &
225.50_pm_r,23967._pm_r,4._pm_r,208._pm_r,0.99_pm_r,26._pm_r,1._pm_r,66._pm_r,0.24_pm_r,175._pm_r, &
230.60_pm_r,27303._pm_r,2._pm_r,210._pm_r,0.83_pm_r,21._pm_r,1._pm_r,81._pm_r,0.22_pm_r,175._pm_r, &
239.00_pm_r,30741._pm_r,1._pm_r,220._pm_r,0.54_pm_r,12._pm_r,1._pm_r,94._pm_r,0.16_pm_r,173._pm_r, &
248.60_pm_r,34309._pm_r,1._pm_r,239._pm_r,0.24_pm_r,343._pm_r,1._pm_r,102._pm_r,0.09_pm_r,164._pm_r, &
258.30_pm_r,38024._pm_r,1._pm_r,249._pm_r,0.18_pm_r,257._pm_r,1._pm_r,104._pm_r,0.03_pm_r,111._pm_r, &
267.20_pm_r,41871._pm_r,2._pm_r,246._pm_r,0.28_pm_r,219._pm_r,1._pm_r,103._pm_r,0.06_pm_r,45._pm_r, &
272.30_pm_r,45830._pm_r,2._pm_r,237._pm_r,0.24_pm_r,182._pm_r,2._pm_r,99._pm_r,0.08_pm_r,55._pm_r, &
271.30_pm_r,49819._pm_r,2._pm_r,226._pm_r,0.31_pm_r,142._pm_r,2._pm_r,97._pm_r,0.09_pm_r,66._pm_r, &
263.00_pm_r,53734._pm_r,2._pm_r,211._pm_r,0.34_pm_r,127._pm_r,2._pm_r,95._pm_r,0.10_pm_r,76._pm_r, &
252.10_pm_r,57509._pm_r,2._pm_r,199._pm_r,0.21_pm_r,124._pm_r,2._pm_r,93._pm_r,0.12_pm_r,70._pm_r, &
240.90_pm_r,61116._pm_r,2._pm_r,193._pm_r,0.08_pm_r,110._pm_r,2._pm_r,91._pm_r,0.13_pm_r,61._pm_r, &
230.20_pm_r,64568._pm_r,2._pm_r,191._pm_r,0.06_pm_r,27._pm_r,2._pm_r,88._pm_r,0.13_pm_r,53._pm_r, &
221.10_pm_r,67869._pm_r,2._pm_r,191._pm_r,0.08_pm_r,22._pm_r,2._pm_r,85._pm_r,0.10_pm_r,39._pm_r, &
212.60_pm_r,71048._pm_r,2._pm_r,190._pm_r,0.09_pm_r,41._pm_r,2._pm_r,82._pm_r,0.07_pm_r,16._pm_r, &
204.20_pm_r,74099._pm_r,2._pm_r,186._pm_r,0.10_pm_r,63._pm_r,2._pm_r,80._pm_r,0.06_pm_r,342._pm_r, &
195.60_pm_r,77029._pm_r,2._pm_r,181._pm_r,0.12_pm_r,76._pm_r,2._pm_r,78._pm_r,0.06_pm_r,325._pm_r, &
188.00_pm_r,79831._pm_r,2._pm_r,174._pm_r,0.13_pm_r,85._pm_r,2._pm_r,76._pm_r,0.06_pm_r,304._pm_r, &
180.40_pm_r,82530._pm_r,2._pm_r,167._pm_r,0.14_pm_r,86._pm_r,2._pm_r,74._pm_r,0.07_pm_r,297._pm_r, &
172.80_pm_r,85105._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
167.50_pm_r,87579._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
165.10_pm_r,90008._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
167.40_pm_r,92450._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
173.70_pm_r,94965._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.50_pm_r,97622._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.70_pm_r,100514._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.60_pm_r,103759._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.20_pm_r,107519._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
310.00_pm_r,111996._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
388.40_pm_r,117557._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel  /)

  real(pm_reel),dimension(10*nb_alt),parameter :: donnees_lat_moins_40= (/ &
       
289.90_pm_r,9._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
270.00_pm_r,4108._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
244.90_pm_r,7877._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.30_pm_r,11304._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.70_pm_r,14496._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.80_pm_r,17593._pm_r,4._pm_r,194._pm_r,0.30_pm_r,341._pm_r,2._pm_r,47._pm_r,0.23_pm_r,189._pm_r, &
216.40_pm_r,20726._pm_r,3._pm_r,199._pm_r,0.37_pm_r,343._pm_r,1._pm_r,58._pm_r,0.29_pm_r,190._pm_r, &
222.60_pm_r,23943._pm_r,3._pm_r,206._pm_r,0.37_pm_r,345._pm_r,1._pm_r,77._pm_r,0.32_pm_r,191._pm_r, &
228.20_pm_r,27243._pm_r,2._pm_r,214._pm_r,0.30_pm_r,348._pm_r,1._pm_r,102._pm_r,0.29_pm_r,192._pm_r, &
235.70_pm_r,30640._pm_r,2._pm_r,221._pm_r,0.19_pm_r,358._pm_r,1._pm_r,123._pm_r,0.21_pm_r,191._pm_r, &
245.50_pm_r,34159._pm_r,2._pm_r,224._pm_r,0.07_pm_r,45._pm_r,1._pm_r,135._pm_r,0.11_pm_r,185._pm_r, &
256.30_pm_r,37837._pm_r,2._pm_r,221._pm_r,0.10_pm_r,117._pm_r,1._pm_r,138._pm_r,0.03_pm_r,141._pm_r, &
266.00_pm_r,41661._pm_r,2._pm_r,215._pm_r,0.15_pm_r,132._pm_r,1._pm_r,135._pm_r,0.06_pm_r,51._pm_r, &
270.40_pm_r,45597._pm_r,2._pm_r,208._pm_r,0.17_pm_r,108._pm_r,1._pm_r,130._pm_r,0.11_pm_r,68._pm_r, &
268.70_pm_r,49552._pm_r,2._pm_r,198._pm_r,0.28_pm_r,90._pm_r,1._pm_r,124._pm_r,0.12_pm_r,85._pm_r, &
259.80_pm_r,53425._pm_r,2._pm_r,184._pm_r,0.29_pm_r,84._pm_r,2._pm_r,122._pm_r,0.14_pm_r,129._pm_r, &
249.40_pm_r,57157._pm_r,2._pm_r,172._pm_r,0.12_pm_r,90._pm_r,2._pm_r,125._pm_r,0.21_pm_r,161._pm_r, &
238.60_pm_r,60729._pm_r,2._pm_r,174._pm_r,0.17_pm_r,248._pm_r,2._pm_r,131._pm_r,0.22_pm_r,164._pm_r, &
227.90_pm_r,64147._pm_r,2._pm_r,187._pm_r,0.35_pm_r,254._pm_r,2._pm_r,135._pm_r,0.15_pm_r,152._pm_r, &
219.70_pm_r,67420._pm_r,2._pm_r,201._pm_r,0.41_pm_r,252._pm_r,2._pm_r,134._pm_r,0.11_pm_r,98._pm_r, &
213.40_pm_r,70592._pm_r,3._pm_r,212._pm_r,0.40_pm_r,249._pm_r,3._pm_r,130._pm_r,0.18_pm_r,52._pm_r, &
208.20_pm_r,73677._pm_r,3._pm_r,218._pm_r,0.35_pm_r,248._pm_r,3._pm_r,123._pm_r,0.27_pm_r,40._pm_r, &
202.80_pm_r,76688._pm_r,3._pm_r,222._pm_r,0.29_pm_r,243._pm_r,3._pm_r,113._pm_r,0.34_pm_r,34._pm_r, &
197.30_pm_r,79616._pm_r,4._pm_r,224._pm_r,0.25_pm_r,241._pm_r,3._pm_r,103._pm_r,0.38_pm_r,33._pm_r, &
190.60_pm_r,82466._pm_r,4._pm_r,225._pm_r,0.21_pm_r,239._pm_r,3._pm_r,93._pm_r,0.38_pm_r,30._pm_r, &
182.50_pm_r,85189._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
176.20_pm_r,87801._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
172.80_pm_r,90353._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
173.70_pm_r,92901._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
177.90_pm_r,95496._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.80_pm_r,98199._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.40_pm_r,101087._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.90_pm_r,104266._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.70_pm_r,107900._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
305.60_pm_r,112263._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
385.90_pm_r,117785._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel  /)

  real(pm_reel),dimension(10*nb_alt),parameter :: donnees_lat_moins_30= (/ &
       
296.40_pm_r,-37._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.60_pm_r,4147._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.70_pm_r,7988._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.10_pm_r,11466._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.10_pm_r,14633._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.10_pm_r,17630._pm_r,1._pm_r,201._pm_r,0.22_pm_r,266._pm_r,1._pm_r,3._pm_r,0.22_pm_r,142._pm_r, &
212.00_pm_r,20682._pm_r,2._pm_r,213._pm_r,0.26_pm_r,265._pm_r,1._pm_r,31._pm_r,0.28_pm_r,141._pm_r, &
219.80_pm_r,23848._pm_r,2._pm_r,223._pm_r,0.25_pm_r,262._pm_r,1._pm_r,75._pm_r,0.30_pm_r,137._pm_r, &
226.20_pm_r,27112._pm_r,2._pm_r,228._pm_r,0.20_pm_r,257._pm_r,1._pm_r,100._pm_r,0.28_pm_r,133._pm_r, &
233.60_pm_r,30479._pm_r,2._pm_r,230._pm_r,0.11_pm_r,242._pm_r,1._pm_r,109._pm_r,0.22_pm_r,126._pm_r, &
243.50_pm_r,33968._pm_r,2._pm_r,230._pm_r,0.05_pm_r,174._pm_r,2._pm_r,111._pm_r,0.16_pm_r,114._pm_r, &
253.60_pm_r,37613._pm_r,2._pm_r,228._pm_r,0.09_pm_r,114._pm_r,2._pm_r,111._pm_r,0.10_pm_r,96._pm_r, &
262.80_pm_r,41394._pm_r,2._pm_r,224._pm_r,0.13_pm_r,99._pm_r,2._pm_r,109._pm_r,0.07_pm_r,70._pm_r, &
268.30_pm_r,45291._pm_r,2._pm_r,221._pm_r,0.10_pm_r,119._pm_r,2._pm_r,107._pm_r,0.06_pm_r,75._pm_r, &
267.70_pm_r,49224._pm_r,2._pm_r,217._pm_r,0.12_pm_r,145._pm_r,2._pm_r,106._pm_r,0.03_pm_r,108._pm_r, &
259.50_pm_r,53088._pm_r,2._pm_r,212._pm_r,0.13_pm_r,133._pm_r,2._pm_r,107._pm_r,0.05_pm_r,198._pm_r, &
249.70_pm_r,56818._pm_r,2._pm_r,208._pm_r,0.11_pm_r,87._pm_r,2._pm_r,110._pm_r,0.10_pm_r,227._pm_r, &
240.10_pm_r,60403._pm_r,2._pm_r,208._pm_r,0.09_pm_r,349._pm_r,2._pm_r,114._pm_r,0.07_pm_r,226._pm_r, &
229.70_pm_r,63846._pm_r,2._pm_r,213._pm_r,0.24_pm_r,291._pm_r,2._pm_r,116._pm_r,0.01_pm_r,180._pm_r, &
220.70_pm_r,67138._pm_r,2._pm_r,224._pm_r,0.45_pm_r,267._pm_r,2._pm_r,114._pm_r,0.10_pm_r,61._pm_r, &
214.30_pm_r,70322._pm_r,3._pm_r,233._pm_r,0.68_pm_r,257._pm_r,2._pm_r,109._pm_r,0.21_pm_r,62._pm_r, &
210.30_pm_r,73428._pm_r,4._pm_r,238._pm_r,0.87_pm_r,251._pm_r,2._pm_r,102._pm_r,0.30_pm_r,58._pm_r, &
207.00_pm_r,76485._pm_r,6._pm_r,241._pm_r,1.03_pm_r,248._pm_r,3._pm_r,95._pm_r,0.36_pm_r,59._pm_r, &
203.40_pm_r,79491._pm_r,7._pm_r,242._pm_r,1.10_pm_r,246._pm_r,3._pm_r,89._pm_r,0.39_pm_r,59._pm_r, &
197.80_pm_r,82443._pm_r,9._pm_r,243._pm_r,1.10_pm_r,245._pm_r,4._pm_r,84._pm_r,0.39_pm_r,59._pm_r, &
189.80_pm_r,85274._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.40_pm_r,87994._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.70_pm_r,90653._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.50_pm_r,93296._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.80_pm_r,95966._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.10_pm_r,98711._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.10_pm_r,101595._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.90_pm_r,104714._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.50_pm_r,108230._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
299.30_pm_r,112467._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
382.80_pm_r,117929._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel  /)

  real(pm_reel),dimension(10*nb_alt),parameter :: donnees_lat_moins_20= (/ &
       
299.80_pm_r,-90._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.70_pm_r,4136._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.90_pm_r,8019._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.40_pm_r,11533._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.10_pm_r,14682._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.30_pm_r,17607._pm_r,1._pm_r,204._pm_r,0.30_pm_r,266._pm_r,1._pm_r,327._pm_r,0.21_pm_r,118._pm_r, &
209.30_pm_r,20605._pm_r,1._pm_r,226._pm_r,0.36_pm_r,270._pm_r,0._pm_r,6._pm_r,0.27_pm_r,118._pm_r, &
217.20_pm_r,23732._pm_r,2._pm_r,240._pm_r,0.34_pm_r,274._pm_r,0._pm_r,80._pm_r,0.29_pm_r,120._pm_r, &
223.40_pm_r,26957._pm_r,2._pm_r,248._pm_r,0.25_pm_r,281._pm_r,1._pm_r,101._pm_r,0.28_pm_r,120._pm_r, &
231.30_pm_r,30285._pm_r,2._pm_r,253._pm_r,0.15_pm_r,301._pm_r,1._pm_r,108._pm_r,0.23_pm_r,123._pm_r, &
242.50_pm_r,33751._pm_r,2._pm_r,256._pm_r,0.08_pm_r,7._pm_r,1._pm_r,111._pm_r,0.15_pm_r,122._pm_r, &
252.50_pm_r,37380._pm_r,2._pm_r,259._pm_r,0.11_pm_r,54._pm_r,2._pm_r,112._pm_r,0.07_pm_r,124._pm_r, &
261.90_pm_r,41143._pm_r,2._pm_r,260._pm_r,0.12_pm_r,76._pm_r,2._pm_r,113._pm_r,0.01_pm_r,90._pm_r, &
268.70_pm_r,45036._pm_r,2._pm_r,260._pm_r,0.03_pm_r,270._pm_r,2._pm_r,112._pm_r,0.03_pm_r,90._pm_r, &
268.20_pm_r,48976._pm_r,2._pm_r,261._pm_r,0.24_pm_r,282._pm_r,2._pm_r,112._pm_r,0.03_pm_r,90._pm_r, &
261.00_pm_r,52853._pm_r,3._pm_r,266._pm_r,0.35_pm_r,289._pm_r,2._pm_r,112._pm_r,0.08_pm_r,266._pm_r, &
252.70_pm_r,56615._pm_r,3._pm_r,270._pm_r,0.31_pm_r,303._pm_r,1._pm_r,117._pm_r,0.28_pm_r,262._pm_r, &
243.80_pm_r,60250._pm_r,3._pm_r,274._pm_r,0.17_pm_r,304._pm_r,1._pm_r,131._pm_r,0.35_pm_r,262._pm_r, &
232.70_pm_r,63744._pm_r,3._pm_r,275._pm_r,0.14_pm_r,274._pm_r,1._pm_r,156._pm_r,0.25_pm_r,252._pm_r, &
222.80_pm_r,67073._pm_r,4._pm_r,274._pm_r,0.28_pm_r,258._pm_r,1._pm_r,168._pm_r,0.09_pm_r,164._pm_r, &
215.30_pm_r,70280._pm_r,4._pm_r,272._pm_r,0.49_pm_r,258._pm_r,1._pm_r,158._pm_r,0.35_pm_r,108._pm_r, &
210.60_pm_r,73396._pm_r,5._pm_r,269._pm_r,0.70_pm_r,258._pm_r,2._pm_r,137._pm_r,0.62_pm_r,100._pm_r, &
207.20_pm_r,76455._pm_r,6._pm_r,267._pm_r,0.86_pm_r,259._pm_r,3._pm_r,122._pm_r,0.85_pm_r,98._pm_r, &
204.80_pm_r,79470._pm_r,8._pm_r,266._pm_r,0.95_pm_r,260._pm_r,4._pm_r,114._pm_r,0.99_pm_r,97._pm_r, &
200.70_pm_r,82454._pm_r,9._pm_r,265._pm_r,0.96_pm_r,260._pm_r,5._pm_r,109._pm_r,1.04_pm_r,97._pm_r, &
193.90_pm_r,85339._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.90_pm_r,88126._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.50_pm_r,90856._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.90_pm_r,93570._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.90_pm_r,96299._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.10_pm_r,99077._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.00_pm_r,101954._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.70_pm_r,105020._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.30_pm_r,108435._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
292.20_pm_r,112548._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
379.30_pm_r,117935._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel  /)

  real(pm_reel),dimension(10*nb_alt),parameter :: donnees_lat_moins_10= (/ &
301.20_pm_r,-127._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.90_pm_r,4113._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.60_pm_r,8004._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.30_pm_r,11524._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.20_pm_r,14660._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.80_pm_r,17550._pm_r,1._pm_r,197._pm_r,0.29_pm_r,291._pm_r,1._pm_r,260._pm_r,0.26_pm_r,96._pm_r, &
207.10_pm_r,20513._pm_r,1._pm_r,242._pm_r,0.35_pm_r,288._pm_r,1._pm_r,247._pm_r,0.33_pm_r,97._pm_r, &
215.90_pm_r,23616._pm_r,1._pm_r,262._pm_r,0.34_pm_r,287._pm_r,0._pm_r,184._pm_r,0.35_pm_r,97._pm_r, &
222.30_pm_r,26824._pm_r,2._pm_r,268._pm_r,0.28_pm_r,281._pm_r,1._pm_r,126._pm_r,0.32_pm_r,99._pm_r, &
229.80_pm_r,30133._pm_r,2._pm_r,270._pm_r,0.18_pm_r,268._pm_r,1._pm_r,115._pm_r,0.24_pm_r,100._pm_r, &
240.90_pm_r,33576._pm_r,2._pm_r,268._pm_r,0.10_pm_r,233._pm_r,1._pm_r,112._pm_r,0.13_pm_r,101._pm_r, &
252.20_pm_r,37190._pm_r,2._pm_r,265._pm_r,0.10_pm_r,182._pm_r,1._pm_r,111._pm_r,0.02_pm_r,120._pm_r, &
262.20_pm_r,40955._pm_r,2._pm_r,261._pm_r,0.11_pm_r,165._pm_r,1._pm_r,112._pm_r,0.08_pm_r,274._pm_r, &
268.60_pm_r,44848._pm_r,2._pm_r,260._pm_r,0.07_pm_r,301._pm_r,1._pm_r,114._pm_r,0.10_pm_r,277._pm_r, &
270.50_pm_r,48801._pm_r,2._pm_r,265._pm_r,0.31_pm_r,316._pm_r,1._pm_r,117._pm_r,0.11_pm_r,259._pm_r, &
265.80_pm_r,52734._pm_r,3._pm_r,275._pm_r,0.45_pm_r,318._pm_r,1._pm_r,127._pm_r,0.19_pm_r,238._pm_r, &
256.50_pm_r,56565._pm_r,3._pm_r,283._pm_r,0.41_pm_r,319._pm_r,1._pm_r,149._pm_r,0.31_pm_r,224._pm_r, &
245.10_pm_r,60237._pm_r,4._pm_r,287._pm_r,0.18_pm_r,319._pm_r,1._pm_r,172._pm_r,0.31_pm_r,219._pm_r, &
233.00_pm_r,63741._pm_r,4._pm_r,288._pm_r,0.01_pm_r,315._pm_r,2._pm_r,181._pm_r,0.22_pm_r,198._pm_r, &
223.00_pm_r,67075._pm_r,4._pm_r,288._pm_r,0.06_pm_r,135._pm_r,2._pm_r,179._pm_r,0.18_pm_r,130._pm_r, &
215.20_pm_r,70285._pm_r,4._pm_r,287._pm_r,0.06_pm_r,135._pm_r,2._pm_r,168._pm_r,0.37_pm_r,95._pm_r, &
209.90_pm_r,73395._pm_r,4._pm_r,287._pm_r,0.03_pm_r,138._pm_r,2._pm_r,149._pm_r,0.56_pm_r,85._pm_r, &
206.40_pm_r,76443._pm_r,3._pm_r,286._pm_r,0.01_pm_r,90._pm_r,3._pm_r,130._pm_r,0.72_pm_r,82._pm_r, &
205.00_pm_r,79452._pm_r,4._pm_r,287._pm_r,0.03_pm_r,326._pm_r,4._pm_r,115._pm_r,0.82_pm_r,79._pm_r, &
201.90_pm_r,82446._pm_r,4._pm_r,287._pm_r,0.04_pm_r,340._pm_r,5._pm_r,106._pm_r,0.86_pm_r,79._pm_r, &
195.70_pm_r,85355._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.00_pm_r,88172._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,90936._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.50_pm_r,93689._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.80_pm_r,96453._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.50_pm_r,99249._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.10_pm_r,102115._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.40_pm_r,105135._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.70_pm_r,108468._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
285.50_pm_r,112474._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
375.70_pm_r,117784._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel  /)

  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_0=(/&
       
301.10_pm_r,-134._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.00_pm_r,4107._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.80_pm_r,8001._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.40_pm_r,11524._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.10_pm_r,14660._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.90_pm_r,17545._pm_r,1._pm_r,162._pm_r,0.16_pm_r,307._pm_r,1._pm_r,267._pm_r,0.25_pm_r,95._pm_r, &
205.60_pm_r,20493._pm_r,1._pm_r,171._pm_r,0.18_pm_r,304._pm_r,1._pm_r,262._pm_r,0.33_pm_r,95._pm_r, &
215.20_pm_r,23580._pm_r,1._pm_r,191._pm_r,0.16_pm_r,292._pm_r,0._pm_r,177._pm_r,0.34_pm_r,95._pm_r, &
221.90_pm_r,26778._pm_r,1._pm_r,217._pm_r,0.13_pm_r,274._pm_r,1._pm_r,106._pm_r,0.30_pm_r,94._pm_r, &
228.80_pm_r,30077._pm_r,1._pm_r,230._pm_r,0.11_pm_r,236._pm_r,1._pm_r,102._pm_r,0.21_pm_r,94._pm_r, &
240.80_pm_r,33512._pm_r,1._pm_r,232._pm_r,0.13_pm_r,209._pm_r,1._pm_r,101._pm_r,0.09_pm_r,96._pm_r, &
252.90_pm_r,37129._pm_r,1._pm_r,228._pm_r,0.13_pm_r,198._pm_r,1._pm_r,102._pm_r,0.02_pm_r,270._pm_r, &
263.30_pm_r,40904._pm_r,1._pm_r,224._pm_r,0.11_pm_r,202._pm_r,1._pm_r,105._pm_r,0.11_pm_r,275._pm_r, &
269.90_pm_r,44814._pm_r,2._pm_r,225._pm_r,0.14_pm_r,264._pm_r,1._pm_r,109._pm_r,0.18_pm_r,267._pm_r, &
271.90_pm_r,48786._pm_r,2._pm_r,235._pm_r,0.26_pm_r,288._pm_r,1._pm_r,119._pm_r,0.19_pm_r,249._pm_r, &
268.10_pm_r,52742._pm_r,2._pm_r,251._pm_r,0.37_pm_r,300._pm_r,1._pm_r,138._pm_r,0.18_pm_r,193._pm_r, &
257.70_pm_r,56595._pm_r,2._pm_r,265._pm_r,0.41_pm_r,324._pm_r,1._pm_r,143._pm_r,0.38_pm_r,145._pm_r, &
244.60_pm_r,60268._pm_r,2._pm_r,275._pm_r,0.43_pm_r,1._pm_r,2._pm_r,138._pm_r,0.51_pm_r,127._pm_r, &
232.60_pm_r,63765._pm_r,2._pm_r,283._pm_r,0.60_pm_r,32._pm_r,2._pm_r,134._pm_r,0.54_pm_r,113._pm_r, &
222.40_pm_r,67094._pm_r,2._pm_r,297._pm_r,0.83_pm_r,47._pm_r,3._pm_r,130._pm_r,0.52_pm_r,98._pm_r, &
214.70_pm_r,70297._pm_r,2._pm_r,325._pm_r,1.05_pm_r,54._pm_r,4._pm_r,127._pm_r,0.48_pm_r,80._pm_r, &
209.90_pm_r,73408._pm_r,2._pm_r,3._pm_r,1.23_pm_r,59._pm_r,5._pm_r,124._pm_r,0.47_pm_r,63._pm_r, &
206.90_pm_r,76464._pm_r,3._pm_r,28._pm_r,1.37_pm_r,61._pm_r,5._pm_r,121._pm_r,0.48_pm_r,50._pm_r, &
205.70_pm_r,79485._pm_r,4._pm_r,40._pm_r,1.46_pm_r,63._pm_r,6._pm_r,119._pm_r,0.50_pm_r,41._pm_r, &
202.70_pm_r,82489._pm_r,5._pm_r,46._pm_r,1.46_pm_r,63._pm_r,6._pm_r,117._pm_r,0.51_pm_r,36._pm_r, &
196.50_pm_r,85412._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.60_pm_r,88238._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.60_pm_r,91010._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,93773._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,96548._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.20_pm_r,99346._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.40_pm_r,102199._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.30_pm_r,105184._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.00_pm_r,108457._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
280.40_pm_r,112385._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
372.10_pm_r,117629._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)


  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_10=(/&
       
300.20_pm_r,-101._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.20_pm_r,4134._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.30_pm_r,8025._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.20_pm_r,11542._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.30_pm_r,14678._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.60_pm_r,17567._pm_r,4._pm_r,161._pm_r,0.42_pm_r,347._pm_r,1._pm_r,279._pm_r,0.28_pm_r,77._pm_r, &
206.40_pm_r,20523._pm_r,3._pm_r,159._pm_r,0.51_pm_r,346._pm_r,0._pm_r,313._pm_r,0.35_pm_r,76._pm_r, &
215.00_pm_r,23612._pm_r,2._pm_r,157._pm_r,0.45_pm_r,342._pm_r,0._pm_r,35._pm_r,0.31_pm_r,75._pm_r, &
222.20_pm_r,26811._pm_r,2._pm_r,157._pm_r,0.30_pm_r,333._pm_r,1._pm_r,54._pm_r,0.21_pm_r,73._pm_r, &
229.70_pm_r,30120._pm_r,1._pm_r,161._pm_r,0.15_pm_r,297._pm_r,1._pm_r,58._pm_r,0.06_pm_r,69._pm_r, &
240.90_pm_r,33560._pm_r,1._pm_r,170._pm_r,0.18_pm_r,227._pm_r,1._pm_r,58._pm_r,0.09_pm_r,260._pm_r, &
252.70_pm_r,37178._pm_r,1._pm_r,180._pm_r,0.27_pm_r,211._pm_r,1._pm_r,52._pm_r,0.20_pm_r,253._pm_r, &
263.60_pm_r,40957._pm_r,2._pm_r,187._pm_r,0.29_pm_r,212._pm_r,0._pm_r,36._pm_r,0.26_pm_r,251._pm_r, &
270.50_pm_r,44875._pm_r,2._pm_r,194._pm_r,0.26_pm_r,260._pm_r,0._pm_r,333._pm_r,0.24_pm_r,243._pm_r, &
271.90_pm_r,48853._pm_r,2._pm_r,206._pm_r,0.37_pm_r,290._pm_r,0._pm_r,272._pm_r,0.19_pm_r,217._pm_r, &
266.90_pm_r,52804._pm_r,2._pm_r,220._pm_r,0.34_pm_r,307._pm_r,0._pm_r,238._pm_r,0.21_pm_r,158._pm_r, &
256.00_pm_r,56641._pm_r,2._pm_r,230._pm_r,0.16_pm_r,0._pm_r,1._pm_r,188._pm_r,0.44_pm_r,124._pm_r, &
243.00_pm_r,60291._pm_r,2._pm_r,230._pm_r,0.28_pm_r,65._pm_r,1._pm_r,148._pm_r,0.57_pm_r,115._pm_r, &
231.40_pm_r,63767._pm_r,1._pm_r,223._pm_r,0.41_pm_r,75._pm_r,2._pm_r,132._pm_r,0.63_pm_r,110._pm_r, &
221.70_pm_r,67080._pm_r,1._pm_r,203._pm_r,0.44_pm_r,70._pm_r,3._pm_r,125._pm_r,0.62_pm_r,110._pm_r, &
215.10_pm_r,70277._pm_r,1._pm_r,160._pm_r,0.42_pm_r,61._pm_r,4._pm_r,121._pm_r,0.59_pm_r,110._pm_r, &
211.60_pm_r,73398._pm_r,1._pm_r,110._pm_r,0.40_pm_r,50._pm_r,4._pm_r,119._pm_r,0.54_pm_r,112._pm_r, &
208.80_pm_r,76477._pm_r,1._pm_r,83._pm_r,0.38_pm_r,41._pm_r,5._pm_r,118._pm_r,0.51_pm_r,113._pm_r, &
206.70_pm_r,79517._pm_r,2._pm_r,68._pm_r,0.38_pm_r,33._pm_r,6._pm_r,118._pm_r,0.47_pm_r,113._pm_r, &
203.20_pm_r,82529._pm_r,2._pm_r,58._pm_r,0.36_pm_r,26._pm_r,7._pm_r,117._pm_r,0.43_pm_r,114._pm_r, &
196.90_pm_r,85463._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.80_pm_r,88294._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.20_pm_r,91062._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.90_pm_r,93820._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.90_pm_r,96589._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.50_pm_r,99378._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.30_pm_r,102218._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.60_pm_r,105181._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.70_pm_r,108421._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.60_pm_r,112307._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
368.90_pm_r,117504._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_20=(/&
297.10_pm_r,-101._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.50_pm_r,4090._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
250.30_pm_r,7937._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.50_pm_r,11426._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.90_pm_r,14574._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.80_pm_r,17510._pm_r,5._pm_r,161._pm_r,0.56_pm_r,345._pm_r,2._pm_r,263._pm_r,0.55_pm_r,76._pm_r, &
208.40_pm_r,20506._pm_r,4._pm_r,160._pm_r,0.74_pm_r,347._pm_r,1._pm_r,275._pm_r,0.67_pm_r,72._pm_r, &
215.30_pm_r,23611._pm_r,3._pm_r,158._pm_r,0.73_pm_r,347._pm_r,0._pm_r,19._pm_r,0.62_pm_r,64._pm_r, &
222.10_pm_r,26810._pm_r,2._pm_r,152._pm_r,0.61_pm_r,349._pm_r,1._pm_r,44._pm_r,0.44_pm_r,49._pm_r, &
229.90_pm_r,30119._pm_r,1._pm_r,138._pm_r,0.44_pm_r,351._pm_r,2._pm_r,41._pm_r,0.27_pm_r,5._pm_r, &
241.10_pm_r,33563._pm_r,1._pm_r,113._pm_r,0.23_pm_r,353._pm_r,2._pm_r,29._pm_r,0.35_pm_r,303._pm_r, &
252.40_pm_r,37181._pm_r,1._pm_r,92._pm_r,0.09_pm_r,342._pm_r,2._pm_r,9._pm_r,0.50_pm_r,277._pm_r, &
263.10_pm_r,40953._pm_r,1._pm_r,83._pm_r,0.07_pm_r,297._pm_r,2._pm_r,343._pm_r,0.60_pm_r,261._pm_r, &
270.20_pm_r,44867._pm_r,0._pm_r,52._pm_r,0.28_pm_r,339._pm_r,2._pm_r,320._pm_r,0.46_pm_r,236._pm_r, &
270.40_pm_r,48833._pm_r,1._pm_r,18._pm_r,0.48_pm_r,0._pm_r,2._pm_r,302._pm_r,0.41_pm_r,203._pm_r, &
263.20_pm_r,52745._pm_r,2._pm_r,14._pm_r,0.47_pm_r,19._pm_r,2._pm_r,285._pm_r,0.39_pm_r,169._pm_r, &
251.20_pm_r,56519._pm_r,2._pm_r,20._pm_r,0.28_pm_r,73._pm_r,1._pm_r,269._pm_r,0.39_pm_r,130._pm_r, &
239.00_pm_r,60102._pm_r,2._pm_r,28._pm_r,0.29_pm_r,158._pm_r,1._pm_r,252._pm_r,0.38_pm_r,110._pm_r, &
229.60_pm_r,63534._pm_r,2._pm_r,38._pm_r,0.45_pm_r,192._pm_r,1._pm_r,224._pm_r,0.35_pm_r,95._pm_r, &
222.30_pm_r,66840._pm_r,1._pm_r,50._pm_r,0.52_pm_r,211._pm_r,0._pm_r,170._pm_r,0.29_pm_r,81._pm_r, &
217.00_pm_r,70055._pm_r,0._pm_r,89._pm_r,0.54_pm_r,230._pm_r,1._pm_r,125._pm_r,0.22_pm_r,68._pm_r, &
214.30_pm_r,73210._pm_r,1._pm_r,227._pm_r,0.56_pm_r,246._pm_r,1._pm_r,104._pm_r,0.15_pm_r,48._pm_r, &
211.70_pm_r,76331._pm_r,1._pm_r,242._pm_r,0.60_pm_r,258._pm_r,1._pm_r,91._pm_r,0.13_pm_r,18._pm_r, &
208.60_pm_r,79410._pm_r,2._pm_r,250._pm_r,0.62_pm_r,266._pm_r,1._pm_r,79._pm_r,0.11_pm_r,360._pm_r, &
204.40_pm_r,82443._pm_r,3._pm_r,255._pm_r,0.62_pm_r,271._pm_r,1._pm_r,67._pm_r,0.10_pm_r,343._pm_r, &
198.10_pm_r,85397._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.40_pm_r,88245._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.80_pm_r,91013._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.10_pm_r,93760._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.10_pm_r,96516._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,99294._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.20_pm_r,102127._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.70_pm_r,105088._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.90_pm_r,108328._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.00_pm_r,112212._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
366.20_pm_r,117380._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_30=(/&
       
291.20_pm_r,-38._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
267.20_pm_r,4053._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.30_pm_r,7786._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.70_pm_r,11193._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.60_pm_r,14360._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.10_pm_r,17396._pm_r,4._pm_r,194._pm_r,0.19_pm_r,145._pm_r,3._pm_r,292._pm_r,0.93_pm_r,79._pm_r, &
211.10_pm_r,20456._pm_r,5._pm_r,191._pm_r,0.26_pm_r,116._pm_r,2._pm_r,315._pm_r,1.17_pm_r,77._pm_r, &
216.10_pm_r,23585._pm_r,5._pm_r,185._pm_r,0.40_pm_r,83._pm_r,2._pm_r,2._pm_r,1.10_pm_r,74._pm_r, &
222.20_pm_r,26789._pm_r,4._pm_r,176._pm_r,0.63_pm_r,61._pm_r,3._pm_r,31._pm_r,0.78_pm_r,68._pm_r, &
229.40_pm_r,30098._pm_r,4._pm_r,161._pm_r,0.92_pm_r,47._pm_r,4._pm_r,37._pm_r,0.33_pm_r,44._pm_r, &
238.60_pm_r,33520._pm_r,3._pm_r,136._pm_r,1.16_pm_r,39._pm_r,4._pm_r,34._pm_r,0.34_pm_r,289._pm_r, &
249.10_pm_r,37094._pm_r,3._pm_r,105._pm_r,1.27_pm_r,30._pm_r,3._pm_r,21._pm_r,0.78_pm_r,267._pm_r, &
259.80_pm_r,40818._pm_r,4._pm_r,79._pm_r,1.26_pm_r,21._pm_r,3._pm_r,357._pm_r,1.06_pm_r,257._pm_r, &
266.90_pm_r,44683._pm_r,5._pm_r,62._pm_r,1.26_pm_r,17._pm_r,3._pm_r,328._pm_r,0.95_pm_r,243._pm_r, &
266.50_pm_r,48597._pm_r,7._pm_r,52._pm_r,1.23_pm_r,26._pm_r,3._pm_r,304._pm_r,0.69_pm_r,222._pm_r, &
259.20_pm_r,52450._pm_r,8._pm_r,49._pm_r,0.98_pm_r,49._pm_r,3._pm_r,290._pm_r,0.42_pm_r,165._pm_r, &
247.70_pm_r,56168._pm_r,10._pm_r,51._pm_r,0.74_pm_r,104._pm_r,2._pm_r,284._pm_r,0.75_pm_r,105._pm_r, &
236.80_pm_r,59711._pm_r,10._pm_r,57._pm_r,0.74_pm_r,169._pm_r,1._pm_r,291._pm_r,0.89_pm_r,95._pm_r, &
228.70_pm_r,63120._pm_r,9._pm_r,63._pm_r,1.00_pm_r,206._pm_r,1._pm_r,47._pm_r,0.81_pm_r,89._pm_r, &
223.30_pm_r,66427._pm_r,7._pm_r,68._pm_r,1.16_pm_r,230._pm_r,1._pm_r,74._pm_r,0.57_pm_r,80._pm_r, &
219.50_pm_r,69668._pm_r,6._pm_r,70._pm_r,1.28_pm_r,250._pm_r,2._pm_r,74._pm_r,0.30_pm_r,60._pm_r, &
217.00_pm_r,72864._pm_r,4._pm_r,65._pm_r,1.40_pm_r,266._pm_r,2._pm_r,68._pm_r,0.20_pm_r,345._pm_r, &
214.10_pm_r,76023._pm_r,2._pm_r,36._pm_r,1.54_pm_r,277._pm_r,2._pm_r,58._pm_r,0.35_pm_r,308._pm_r, &
210.60_pm_r,79131._pm_r,2._pm_r,332._pm_r,1.60_pm_r,284._pm_r,2._pm_r,42._pm_r,0.48_pm_r,298._pm_r, &
206.60_pm_r,82190._pm_r,4._pm_r,309._pm_r,1.57_pm_r,288._pm_r,2._pm_r,19._pm_r,0.54_pm_r,293._pm_r, &
200.60_pm_r,85179._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.10_pm_r,88060._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.20_pm_r,90840._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.70_pm_r,93585._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.60_pm_r,96330._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,99103._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.40_pm_r,101943._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.60_pm_r,104926._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.70_pm_r,108199._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
278.00_pm_r,112113._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
363.90_pm_r,117264._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_40=(/&
       
284.00_pm_r,-7._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
258.50_pm_r,3965._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
233.70_pm_r,7568._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.30_pm_r,10884._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.70_pm_r,14068._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.50_pm_r,17216._pm_r,7._pm_r,271._pm_r,2.09_pm_r,175._pm_r,5._pm_r,356._pm_r,0.45_pm_r,62._pm_r, &
214.90_pm_r,20358._pm_r,7._pm_r,241._pm_r,2.73_pm_r,167._pm_r,5._pm_r,4._pm_r,0.61_pm_r,62._pm_r, &
217.20_pm_r,23522._pm_r,9._pm_r,213._pm_r,2.91_pm_r,155._pm_r,6._pm_r,12._pm_r,0.68_pm_r,63._pm_r, &
220.60_pm_r,26725._pm_r,11._pm_r,193._pm_r,2.72_pm_r,135._pm_r,7._pm_r,18._pm_r,0.60_pm_r,64._pm_r, &
225.30_pm_r,29991._pm_r,13._pm_r,176._pm_r,2.65_pm_r,105._pm_r,7._pm_r,22._pm_r,0.38_pm_r,68._pm_r, &
233.70_pm_r,33345._pm_r,14._pm_r,159._pm_r,2.96_pm_r,75._pm_r,7._pm_r,24._pm_r,0.04_pm_r,166._pm_r, &
243.90_pm_r,36845._pm_r,14._pm_r,140._pm_r,3.35_pm_r,54._pm_r,7._pm_r,23._pm_r,0.46_pm_r,238._pm_r, &
254.20_pm_r,40490._pm_r,15._pm_r,120._pm_r,3.50_pm_r,38._pm_r,6._pm_r,18._pm_r,0.88_pm_r,241._pm_r, &
261.80_pm_r,44277._pm_r,16._pm_r,102._pm_r,2.90_pm_r,34._pm_r,5._pm_r,7._pm_r,1.12_pm_r,236._pm_r, &
262.40_pm_r,48125._pm_r,17._pm_r,91._pm_r,2.05_pm_r,33._pm_r,4._pm_r,351._pm_r,0.98_pm_r,226._pm_r, &
254.80_pm_r,51916._pm_r,19._pm_r,85._pm_r,1.03_pm_r,37._pm_r,4._pm_r,340._pm_r,0.61_pm_r,181._pm_r, &
243.30_pm_r,55569._pm_r,19._pm_r,84._pm_r,0.04_pm_r,153._pm_r,3._pm_r,347._pm_r,1.03_pm_r,113._pm_r, &
234.10_pm_r,59059._pm_r,19._pm_r,84._pm_r,0.86_pm_r,259._pm_r,2._pm_r,27._pm_r,1.17_pm_r,100._pm_r, &
229.10_pm_r,62449._pm_r,17._pm_r,84._pm_r,1.49_pm_r,265._pm_r,3._pm_r,55._pm_r,1.07_pm_r,87._pm_r, &
225.60_pm_r,65777._pm_r,14._pm_r,83._pm_r,1.85_pm_r,272._pm_r,5._pm_r,61._pm_r,0.83_pm_r,67._pm_r, &
222.60_pm_r,69059._pm_r,12._pm_r,80._pm_r,2.03_pm_r,278._pm_r,6._pm_r,60._pm_r,0.70_pm_r,35._pm_r, &
220.90_pm_r,72305._pm_r,9._pm_r,73._pm_r,2.11_pm_r,284._pm_r,7._pm_r,54._pm_r,0.76_pm_r,4._pm_r, &
218.20_pm_r,75522._pm_r,7._pm_r,58._pm_r,2.13_pm_r,289._pm_r,7._pm_r,45._pm_r,0.91_pm_r,346._pm_r, &
214.70_pm_r,78690._pm_r,5._pm_r,30._pm_r,2.09_pm_r,292._pm_r,8._pm_r,36._pm_r,1.00_pm_r,337._pm_r, &
210.70_pm_r,81810._pm_r,6._pm_r,359._pm_r,1.95_pm_r,295._pm_r,9._pm_r,27._pm_r,1.03_pm_r,333._pm_r, &
204.10_pm_r,84852._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.80_pm_r,87780._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.50_pm_r,90588._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.90_pm_r,93341._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.80_pm_r,96085._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.90_pm_r,98863._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.70_pm_r,101725._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.20_pm_r,104751._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.30_pm_r,108089._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
279.80_pm_r,112055._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
361.60_pm_r,117187._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)


  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_50=(/&
       
277.90_pm_r,-66._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
251.70_pm_r,3807._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.30_pm_r,7318._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.50_pm_r,10585._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.60_pm_r,13782._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.30_pm_r,16983._pm_r,10._pm_r,283._pm_r,4.42_pm_r,198._pm_r,12._pm_r,32._pm_r,1.46_pm_r,348._pm_r, &
217.60_pm_r,20174._pm_r,13._pm_r,246._pm_r,5.88_pm_r,191._pm_r,14._pm_r,25._pm_r,1.73_pm_r,348._pm_r, &
217.60_pm_r,23360._pm_r,19._pm_r,221._pm_r,6.47_pm_r,180._pm_r,16._pm_r,20._pm_r,1.50_pm_r,347._pm_r, &
219.00_pm_r,26555._pm_r,26._pm_r,206._pm_r,6.29_pm_r,163._pm_r,17._pm_r,17._pm_r,0.79_pm_r,339._pm_r, &
221.80_pm_r,29784._pm_r,32._pm_r,192._pm_r,5.94_pm_r,137._pm_r,18._pm_r,15._pm_r,0.46_pm_r,215._pm_r, &
228.10_pm_r,33072._pm_r,36._pm_r,179._pm_r,6.05_pm_r,107._pm_r,16._pm_r,15._pm_r,1.54_pm_r,192._pm_r, &
236.70_pm_r,36477._pm_r,37._pm_r,165._pm_r,6.39_pm_r,82._pm_r,13._pm_r,16._pm_r,2.33_pm_r,191._pm_r, &
246.10_pm_r,40011._pm_r,38._pm_r,151._pm_r,6.38_pm_r,63._pm_r,10._pm_r,17._pm_r,2.58_pm_r,195._pm_r, &
254.10_pm_r,43680._pm_r,39._pm_r,138._pm_r,5.37_pm_r,55._pm_r,6._pm_r,15._pm_r,2.11_pm_r,206._pm_r, &
256.80_pm_r,47428._pm_r,40._pm_r,128._pm_r,4.02_pm_r,45._pm_r,4._pm_r,4._pm_r,1.41_pm_r,216._pm_r, &
252.40_pm_r,51162._pm_r,40._pm_r,121._pm_r,3.00_pm_r,20._pm_r,3._pm_r,346._pm_r,0.57_pm_r,212._pm_r, &
243.70_pm_r,54800._pm_r,38._pm_r,115._pm_r,3.23_pm_r,344._pm_r,2._pm_r,346._pm_r,0.50_pm_r,101._pm_r, &
236.00_pm_r,58308._pm_r,33._pm_r,110._pm_r,4.53_pm_r,322._pm_r,2._pm_r,11._pm_r,0.77_pm_r,70._pm_r, &
231.50_pm_r,61728._pm_r,27._pm_r,103._pm_r,5.20_pm_r,310._pm_r,3._pm_r,29._pm_r,0.93_pm_r,54._pm_r, &
228.80_pm_r,65097._pm_r,20._pm_r,95._pm_r,5.09_pm_r,302._pm_r,5._pm_r,34._pm_r,0.97_pm_r,40._pm_r, &
226.60_pm_r,68432._pm_r,14._pm_r,83._pm_r,4.59_pm_r,294._pm_r,6._pm_r,34._pm_r,0.99_pm_r,26._pm_r, &
225.30_pm_r,71740._pm_r,9._pm_r,64._pm_r,4.00_pm_r,286._pm_r,7._pm_r,31._pm_r,1.00_pm_r,15._pm_r, &
223.20_pm_r,75026._pm_r,6._pm_r,27._pm_r,3.49_pm_r,277._pm_r,9._pm_r,28._pm_r,1.01_pm_r,5._pm_r, &
220.10_pm_r,78273._pm_r,6._pm_r,337._pm_r,3.06_pm_r,270._pm_r,10._pm_r,24._pm_r,0.99_pm_r,359._pm_r, &
215.70_pm_r,81475._pm_r,8._pm_r,308._pm_r,2.65_pm_r,264._pm_r,11._pm_r,21._pm_r,0.93_pm_r,354._pm_r, &
208.20_pm_r,84581._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.00_pm_r,87565._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.60_pm_r,90411._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.80_pm_r,93183._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,95939._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.60_pm_r,98733._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.90_pm_r,101630._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.00_pm_r,104714._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
234.80_pm_r,108130._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
281.10_pm_r,112154._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
359.20_pm_r,117256._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_60=(/&
       
266.90_pm_r,-72._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
247.50_pm_r,3687._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.20_pm_r,7141._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.40_pm_r,10375._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.80_pm_r,13555._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.80_pm_r,16745._pm_r,8._pm_r,257._pm_r,6.74_pm_r,215._pm_r,16._pm_r,35._pm_r,3.21_pm_r,6._pm_r, &
217.10_pm_r,19931._pm_r,18._pm_r,230._pm_r,9.07_pm_r,209._pm_r,20._pm_r,27._pm_r,4.03_pm_r,4._pm_r, &
215.90_pm_r,23099._pm_r,32._pm_r,219._pm_r,9.90_pm_r,199._pm_r,26._pm_r,22._pm_r,3.71_pm_r,0._pm_r, &
217.10_pm_r,26267._pm_r,45._pm_r,211._pm_r,9.12_pm_r,183._pm_r,30._pm_r,18._pm_r,2.17_pm_r,346._pm_r, &
221.50_pm_r,29478._pm_r,54._pm_r,202._pm_r,7.75_pm_r,155._pm_r,31._pm_r,15._pm_r,1.23_pm_r,253._pm_r, &
227.60_pm_r,32763._pm_r,60._pm_r,193._pm_r,7.69_pm_r,120._pm_r,28._pm_r,12._pm_r,3.23_pm_r,211._pm_r, &
234.50_pm_r,36149._pm_r,61._pm_r,182._pm_r,8.57_pm_r,93._pm_r,22._pm_r,9._pm_r,4.72_pm_r,203._pm_r, &
241.60_pm_r,39633._pm_r,61._pm_r,170._pm_r,8.95_pm_r,75._pm_r,15._pm_r,3._pm_r,5.11_pm_r,198._pm_r, &
248.00_pm_r,43222._pm_r,60._pm_r,158._pm_r,7.88_pm_r,64._pm_r,9._pm_r,354._pm_r,3.56_pm_r,194._pm_r, &
252.50_pm_r,46888._pm_r,59._pm_r,148._pm_r,6.57_pm_r,46._pm_r,6._pm_r,341._pm_r,1.72_pm_r,186._pm_r, &
253.00_pm_r,50595._pm_r,55._pm_r,140._pm_r,6.26_pm_r,18._pm_r,4._pm_r,336._pm_r,0.36_pm_r,150._pm_r, &
249.20_pm_r,54277._pm_r,49._pm_r,131._pm_r,7.37_pm_r,353._pm_r,4._pm_r,340._pm_r,0.41_pm_r,25._pm_r, &
243.00_pm_r,57880._pm_r,40._pm_r,123._pm_r,8.32_pm_r,334._pm_r,5._pm_r,345._pm_r,0.71_pm_r,5._pm_r, &
237.50_pm_r,61398._pm_r,29._pm_r,113._pm_r,8.50_pm_r,320._pm_r,6._pm_r,348._pm_r,0.90_pm_r,358._pm_r, &
233.90_pm_r,64848._pm_r,18._pm_r,98._pm_r,7.84_pm_r,310._pm_r,7._pm_r,350._pm_r,0.96_pm_r,357._pm_r, &
231.40_pm_r,68255._pm_r,10._pm_r,69._pm_r,6.83_pm_r,300._pm_r,9._pm_r,351._pm_r,0.91_pm_r,357._pm_r, &
229.00_pm_r,71627._pm_r,7._pm_r,6._pm_r,5.82_pm_r,289._pm_r,10._pm_r,352._pm_r,0.82_pm_r,2._pm_r, &
226.70_pm_r,74964._pm_r,12._pm_r,323._pm_r,5.05_pm_r,277._pm_r,11._pm_r,353._pm_r,0.73_pm_r,5._pm_r, &
223.70_pm_r,78264._pm_r,17._pm_r,304._pm_r,4.47_pm_r,266._pm_r,12._pm_r,354._pm_r,0.63_pm_r,10._pm_r, &
219.20_pm_r,81519._pm_r,22._pm_r,293._pm_r,3.96_pm_r,257._pm_r,13._pm_r,356._pm_r,0.53_pm_r,14._pm_r, &
211.70_pm_r,84673._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.30_pm_r,87709._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.00_pm_r,90598._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.40_pm_r,93395._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.20_pm_r,96171._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.90_pm_r,98988._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.50_pm_r,101926._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.20_pm_r,105073._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
239.90_pm_r,108569._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
281.40_pm_r,112636._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
356.60_pm_r,117697._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_70=(/&
       
254.10_pm_r,-32._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
244.10_pm_r,3606._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.90_pm_r,7016._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.50_pm_r,10217._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.80_pm_r,13359._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.60_pm_r,16503._pm_r,4._pm_r,230._pm_r,7.65_pm_r,219._pm_r,8._pm_r,15._pm_r,3.50_pm_r,35._pm_r, &
214.10_pm_r,19645._pm_r,17._pm_r,220._pm_r,10.92_pm_r,214._pm_r,14._pm_r,23._pm_r,4.73_pm_r,33._pm_r, &
212.20_pm_r,22762._pm_r,34._pm_r,215._pm_r,12.06_pm_r,207._pm_r,21._pm_r,26._pm_r,4.62_pm_r,27._pm_r, &
215.00_pm_r,25883._pm_r,50._pm_r,211._pm_r,10.23_pm_r,195._pm_r,27._pm_r,25._pm_r,2.73_pm_r,12._pm_r, &
222.20_pm_r,29085._pm_r,62._pm_r,206._pm_r,7.32_pm_r,172._pm_r,28._pm_r,22._pm_r,1.44_pm_r,292._pm_r, &
228.60_pm_r,32385._pm_r,68._pm_r,200._pm_r,6.08_pm_r,130._pm_r,27._pm_r,16._pm_r,3.15_pm_r,242._pm_r, &
233.70_pm_r,35773._pm_r,69._pm_r,192._pm_r,7.20_pm_r,94._pm_r,23._pm_r,7._pm_r,4.46_pm_r,229._pm_r, &
238.40_pm_r,39228._pm_r,66._pm_r,182._pm_r,8.45_pm_r,74._pm_r,18._pm_r,354._pm_r,4.73_pm_r,220._pm_r, &
243.40_pm_r,42757._pm_r,62._pm_r,172._pm_r,8.13_pm_r,67._pm_r,14._pm_r,338._pm_r,3.13_pm_r,208._pm_r, &
249.50_pm_r,46365._pm_r,60._pm_r,162._pm_r,7.07_pm_r,55._pm_r,11._pm_r,327._pm_r,1.58_pm_r,185._pm_r, &
255.00_pm_r,50063._pm_r,56._pm_r,153._pm_r,6.37_pm_r,31._pm_r,10._pm_r,324._pm_r,0.65_pm_r,141._pm_r, &
257.60_pm_r,53820._pm_r,49._pm_r,145._pm_r,7.10_pm_r,1._pm_r,10._pm_r,325._pm_r,0.22_pm_r,41._pm_r, &
253.30_pm_r,57565._pm_r,39._pm_r,139._pm_r,8.10_pm_r,339._pm_r,10._pm_r,327._pm_r,0.45_pm_r,10._pm_r, &
246.10_pm_r,61224._pm_r,28._pm_r,133._pm_r,8.50_pm_r,325._pm_r,11._pm_r,330._pm_r,0.59_pm_r,2._pm_r, &
239.10_pm_r,64774._pm_r,16._pm_r,127._pm_r,7.96_pm_r,316._pm_r,11._pm_r,332._pm_r,0.58_pm_r,0._pm_r, &
233.10_pm_r,68232._pm_r,5._pm_r,116._pm_r,6.87_pm_r,307._pm_r,12._pm_r,334._pm_r,0.49_pm_r,2._pm_r, &
228.60_pm_r,71610._pm_r,4._pm_r,310._pm_r,5.65_pm_r,298._pm_r,13._pm_r,335._pm_r,0.36_pm_r,9._pm_r, &
225.80_pm_r,74936._pm_r,12._pm_r,299._pm_r,4.63_pm_r,287._pm_r,13._pm_r,337._pm_r,0.24_pm_r,26._pm_r, &
223.30_pm_r,78226._pm_r,18._pm_r,293._pm_r,3.88_pm_r,276._pm_r,13._pm_r,338._pm_r,0.18_pm_r,52._pm_r, &
219.60_pm_r,81478._pm_r,23._pm_r,288._pm_r,3.30_pm_r,265._pm_r,13._pm_r,339._pm_r,0.16_pm_r,79._pm_r, &
213.50_pm_r,84647._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.80_pm_r,87720._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.10_pm_r,90644._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.10_pm_r,93466._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.70_pm_r,96263._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.20_pm_r,99106._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.70_pm_r,102083._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.60_pm_r,105285._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.70_pm_r,108844._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
280.50_pm_r,112936._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
354.20_pm_r,117951._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel),dimension(10*nb_alt),parameter::donnees_lat_80=(/&
       
247.40_pm_r,35._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
241.50_pm_r,3604._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.80_pm_r,6978._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.60_pm_r,10149._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.20_pm_r,13249._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.20_pm_r,16332._pm_r,2._pm_r,242._pm_r,4.43_pm_r,219._pm_r,5._pm_r,345._pm_r,1.54_pm_r,73._pm_r, &
209.60_pm_r,19407._pm_r,10._pm_r,221._pm_r,7.06_pm_r,214._pm_r,6._pm_r,13._pm_r,2.03_pm_r,72._pm_r, &
208.30_pm_r,22462._pm_r,22._pm_r,215._pm_r,8.54_pm_r,207._pm_r,8._pm_r,32._pm_r,1.65_pm_r,69._pm_r, &
211.40_pm_r,25527._pm_r,33._pm_r,211._pm_r,7.43_pm_r,197._pm_r,9._pm_r,37._pm_r,0.33_pm_r,43._pm_r, &
219.90_pm_r,28685._pm_r,42._pm_r,206._pm_r,5.36_pm_r,180._pm_r,9._pm_r,33._pm_r,1.09_pm_r,259._pm_r, &
227.00_pm_r,31958._pm_r,48._pm_r,201._pm_r,3.79_pm_r,149._pm_r,7._pm_r,20._pm_r,2.06_pm_r,255._pm_r, &
231.80_pm_r,35320._pm_r,49._pm_r,195._pm_r,3.55_pm_r,108._pm_r,6._pm_r,351._pm_r,2.48_pm_r,252._pm_r, &
235.60_pm_r,38741._pm_r,48._pm_r,189._pm_r,4.21_pm_r,80._pm_r,6._pm_r,315._pm_r,2.36_pm_r,248._pm_r, &
240.20_pm_r,42223._pm_r,46._pm_r,182._pm_r,3.84_pm_r,74._pm_r,8._pm_r,295._pm_r,1.46_pm_r,239._pm_r, &
247.80_pm_r,45791._pm_r,45._pm_r,176._pm_r,3.07_pm_r,64._pm_r,8._pm_r,285._pm_r,0.65_pm_r,221._pm_r, &
257.30_pm_r,49492._pm_r,43._pm_r,172._pm_r,2.60_pm_r,38._pm_r,8._pm_r,282._pm_r,0.12_pm_r,147._pm_r, &
265.20_pm_r,53319._pm_r,39._pm_r,169._pm_r,3.10_pm_r,7._pm_r,8._pm_r,284._pm_r,0.36_pm_r,36._pm_r, &
262.80_pm_r,57194._pm_r,34._pm_r,167._pm_r,4.07_pm_r,351._pm_r,8._pm_r,288._pm_r,0.65_pm_r,43._pm_r, &
254.90_pm_r,60991._pm_r,28._pm_r,167._pm_r,4.59_pm_r,343._pm_r,8._pm_r,296._pm_r,0.76_pm_r,46._pm_r, &
245.40_pm_r,64652._pm_r,21._pm_r,170._pm_r,4.56_pm_r,337._pm_r,7._pm_r,304._pm_r,0.69_pm_r,46._pm_r, &
236.40_pm_r,68182._pm_r,15._pm_r,176._pm_r,4.25_pm_r,333._pm_r,7._pm_r,311._pm_r,0.50_pm_r,44._pm_r, &
229.80_pm_r,71592._pm_r,10._pm_r,192._pm_r,3.81_pm_r,328._pm_r,7._pm_r,316._pm_r,0.30_pm_r,39._pm_r, &
225.90_pm_r,74928._pm_r,7._pm_r,224._pm_r,3.41_pm_r,324._pm_r,7._pm_r,318._pm_r,0.10_pm_r,6._pm_r, &
223.10_pm_r,78214._pm_r,8._pm_r,260._pm_r,3.04_pm_r,320._pm_r,7._pm_r,318._pm_r,0.11_pm_r,270._pm_r, &
219.60_pm_r,81461._pm_r,11._pm_r,279._pm_r,2.67_pm_r,317._pm_r,7._pm_r,316._pm_r,0.23_pm_r,247._pm_r, &
214.30_pm_r,84635._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.40_pm_r,87727._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.50_pm_r,90674._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.30_pm_r,93514._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.90_pm_r,96327._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.80_pm_r,99190._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.00_pm_r,102194._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.50_pm_r,105435._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
245.90_pm_r,109033._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
279.40_pm_r,113134._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
352.50_pm_r,118114._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel),dimension(10*nb_lat*nb_alt),parameter::donnees_fevrier=&
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,nb_alt,nb_lat)::donnees=reshape(donnees_fevrier,(/10,nb_alt,nb_lat/))

  !************************************************************************

  !initialisation de la valeur du code retour
  !..........................................
  retour=pm_OK

  tbar(:,:)=donnees(1,:,:)
  zbar(:,:)=donnees(2,:,:)
  z1(:,:)=donnees(3,:,:)
  phi1(:,:)=donnees(4,:,:)
  t1(:,:)=donnees(5,:,:)
  phit1(:,:)=donnees(6,:,:)
  z2(:,:)=donnees(7,:,:)
  phi2(:,:)=donnees(8,:,:)
  t2(:,:)=donnees(9,:,:)
  phit2(:,:)=donnees(10,:,:)
end subroutine cps_atmi_fevrier

subroutine cps_atmi_mars (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_mars
!
!$Resume
!   Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de MARS
!
!$Description
!   Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de MARS
!
!$Auteur
!   Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_mars (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments  
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>  
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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

  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
264.10_pm_r,  -245._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
245.30_pm_r,  3473._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.40_pm_r,  6901._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.50_pm_r, 10177._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.70_pm_r, 13463._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.10_pm_r, 16764._pm_r,   6._pm_r,277._pm_r,  0.94_pm_r,190._pm_r,   1._pm_r,117._pm_r,  0.04_pm_r,353._pm_r, &
224.20_pm_r, 20055._pm_r,   7._pm_r,263._pm_r, 1.22_pm_r,184._pm_r,   1._pm_r,115._pm_r,  0.03_pm_r,342._pm_r, &
222.80_pm_r, 23329._pm_r,   7._pm_r,247._pm_r, 1.29_pm_r,173._pm_r,   1._pm_r,114._pm_r,  0.01_pm_r,315._pm_r, &
221.30_pm_r, 26578._pm_r,   8._pm_r,234._pm_r, 1.14_pm_r,154._pm_r,   1._pm_r,115._pm_r,  0.04_pm_r,194._pm_r, &
225.50_pm_r, 29844._pm_r,   8._pm_r,222._pm_r, 1.02_pm_r,119._pm_r,   1._pm_r,120._pm_r,  0.11_pm_r,185._pm_r, &
231.40_pm_r, 33189._pm_r,   7._pm_r,211._pm_r, 1.22_pm_r, 83._pm_r,   2._pm_r,126._pm_r,  0.16_pm_r,180._pm_r, &
239.20_pm_r, 36634._pm_r,   6._pm_r,197._pm_r, 1.49_pm_r, 62._pm_r,   2._pm_r,134._pm_r,  0.20_pm_r,179._pm_r, &
248.80_pm_r, 40204._pm_r,   4._pm_r,176._pm_r, 1.56_pm_r, 49._pm_r,   2._pm_r,140._pm_r,  0.19_pm_r,174._pm_r, &
258.00_pm_r, 43919._pm_r,   3._pm_r,145._pm_r, 1.31_pm_r, 37._pm_r,   2._pm_r,143._pm_r,  0.04_pm_r,196._pm_r, &
263.60_pm_r, 47742._pm_r,   3._pm_r,112._pm_r,  0.88_pm_r, 21._pm_r,   2._pm_r,143._pm_r,  0.12_pm_r,340._pm_r, &
261.80_pm_r, 51597._pm_r,   3._pm_r, 93._pm_r,  0.45_pm_r,334._pm_r,   2._pm_r,140._pm_r,  0.12_pm_r,355._pm_r, &
255.00_pm_r, 55385._pm_r,   2._pm_r, 86._pm_r,  0.66_pm_r,265._pm_r,   2._pm_r,137._pm_r,  0.08_pm_r, 97._pm_r, &
248.10_pm_r, 59067._pm_r,   1._pm_r,100._pm_r,  0.92_pm_r,250._pm_r,   2._pm_r,136._pm_r,  0.28_pm_r,135._pm_r, &
241.20_pm_r, 62653._pm_r,   1._pm_r,215._pm_r,  0.98_pm_r,246._pm_r,   3._pm_r,136._pm_r,  0.38_pm_r,138._pm_r, &
232.30_pm_r, 66122._pm_r,   2._pm_r,234._pm_r,  0.87_pm_r,246._pm_r,   3._pm_r,136._pm_r,  0.36_pm_r,139._pm_r, &
223.30_pm_r, 69459._pm_r,   3._pm_r,239._pm_r,  0.68_pm_r,251._pm_r,   4._pm_r,137._pm_r,  0.30_pm_r,138._pm_r, &
215.80_pm_r, 72670._pm_r,   4._pm_r,242._pm_r,  0.50_pm_r,261._pm_r,   4._pm_r,137._pm_r,  0.19_pm_r,134._pm_r, &
209.60_pm_r, 75788._pm_r,   5._pm_r,246._pm_r,  0.36_pm_r,276._pm_r,   4._pm_r,136._pm_r,  0.11_pm_r,124._pm_r, &
203.90_pm_r, 78815._pm_r,   5._pm_r,249._pm_r,  0.28_pm_r,296._pm_r,   4._pm_r,136._pm_r,  0.05_pm_r,107._pm_r, &
193.00_pm_r, 81761._pm_r,   5._pm_r,253._pm_r,  0.25_pm_r,313._pm_r,   4._pm_r,135._pm_r,  0.02_pm_r, 63._pm_r, &
176.80_pm_r, 84426._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
166.40_pm_r, 86891._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
163.00_pm_r, 89287._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
164.80_pm_r, 91689._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
171.70_pm_r, 94166._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.00_pm_r, 96797._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.00_pm_r, 99688._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
230.80_pm_r,102969._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
264.30_pm_r,106778._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
299.30_pm_r,111189._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
377.70_pm_r,116534._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
269.90_pm_r,  -184._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
248.40_pm_r,  3601._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.60_pm_r,  7069._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.40_pm_r, 10362._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.70_pm_r, 13649._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.00_pm_r, 16950._pm_r,   7._pm_r,259._pm_r,  0.84_pm_r,177._pm_r,   3._pm_r,119._pm_r,  0.09_pm_r,223._pm_r, &
223.70_pm_r, 20236._pm_r,   7._pm_r,248._pm_r, 1.07_pm_r,165._pm_r,   3._pm_r,122._pm_r,  0.14_pm_r,218._pm_r, &
222.30_pm_r, 23502._pm_r,   7._pm_r,234._pm_r, 1.17_pm_r,145._pm_r,   3._pm_r,126._pm_r,  0.17_pm_r,215._pm_r, &
222.10_pm_r, 26751._pm_r,   7._pm_r,220._pm_r, 1.28_pm_r,115._pm_r,   3._pm_r,131._pm_r,  0.18_pm_r,208._pm_r, &
227.30_pm_r, 30037._pm_r,   6._pm_r,203._pm_r, 1.61_pm_r, 84._pm_r,   3._pm_r,136._pm_r,  0.17_pm_r,197._pm_r, &
233.60_pm_r, 33411._pm_r,   5._pm_r,177._pm_r, 2.07_pm_r, 63._pm_r,   3._pm_r,139._pm_r,  0.14_pm_r,180._pm_r, &
241.10_pm_r, 36887._pm_r,   4._pm_r,135._pm_r, 2.32_pm_r, 49._pm_r,   4._pm_r,140._pm_r,  0.11_pm_r,149._pm_r, &
250.60_pm_r, 40483._pm_r,   5._pm_r, 95._pm_r, 2.25_pm_r, 36._pm_r,   4._pm_r,139._pm_r,  0.11_pm_r,108._pm_r, &
259.90_pm_r, 44227._pm_r,   7._pm_r, 71._pm_r, 1.95_pm_r, 15._pm_r,   4._pm_r,138._pm_r,  0.10_pm_r, 58._pm_r, &
264.50_pm_r, 48071._pm_r,   8._pm_r, 54._pm_r, 1.62_pm_r,349._pm_r,   4._pm_r,135._pm_r,  0.11_pm_r,  3._pm_r, &
260.50_pm_r, 51923._pm_r,   9._pm_r, 40._pm_r, 1.32_pm_r,314._pm_r,   4._pm_r,134._pm_r,  0.17_pm_r,318._pm_r, &
251.70_pm_r, 55678._pm_r,   9._pm_r, 29._pm_r, 1.23_pm_r,269._pm_r,   3._pm_r,135._pm_r,  0.24_pm_r,293._pm_r, &
244.40_pm_r, 59308._pm_r,   7._pm_r, 19._pm_r, 1.33_pm_r,239._pm_r,   3._pm_r,138._pm_r,  0.19_pm_r,287._pm_r, &
237.10_pm_r, 62837._pm_r,   6._pm_r,  9._pm_r, 1.34_pm_r,222._pm_r,   3._pm_r,141._pm_r,  0.13_pm_r,268._pm_r, &
229.30_pm_r, 66250._pm_r,   4._pm_r,357._pm_r, 1.16_pm_r,209._pm_r,   3._pm_r,144._pm_r,  0.09_pm_r,224._pm_r, &
221.60_pm_r, 69553._pm_r,   3._pm_r,344._pm_r,  0.89_pm_r,197._pm_r,   3._pm_r,146._pm_r,  0.13_pm_r,185._pm_r, &
214.90_pm_r, 72746._pm_r,   2._pm_r,329._pm_r,  0.63_pm_r,182._pm_r,   3._pm_r,148._pm_r,  0.16_pm_r,169._pm_r, &
209.00_pm_r, 75852._pm_r,   1._pm_r,315._pm_r,  0.46_pm_r,162._pm_r,   3._pm_r,149._pm_r,  0.19_pm_r,163._pm_r, &
203.60_pm_r, 78872._pm_r,   1._pm_r,302._pm_r,  0.37_pm_r,139._pm_r,   4._pm_r,150._pm_r,  0.20_pm_r,161._pm_r, &
193.90_pm_r, 81816._pm_r,   0._pm_r,284._pm_r,  0.34_pm_r,120._pm_r,   4._pm_r,151._pm_r,  0.20_pm_r,159._pm_r, &
179.40_pm_r, 84514._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
169.40_pm_r, 87027._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
165.40_pm_r, 89465._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
166.60_pm_r, 91899._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
172.80_pm_r, 94398._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.00_pm_r, 97039._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.70_pm_r, 99923._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
227.90_pm_r,103172._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
261.40_pm_r,106934._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
300.00_pm_r,111328._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
377.80_pm_r,116692._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
275.70_pm_r,  -120._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
254.60_pm_r,  3755._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
231.10_pm_r,  7304._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.20_pm_r, 10624._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.40_pm_r, 13887._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.80_pm_r, 17157._pm_r,   8._pm_r,238._pm_r,  0.78_pm_r,120._pm_r,   4._pm_r,102._pm_r,  0.28_pm_r,214._pm_r, &
222.30_pm_r, 20415._pm_r,   7._pm_r,230._pm_r, 1.11_pm_r,108._pm_r,   4._pm_r,109._pm_r,  0.38_pm_r,213._pm_r, &
222.90_pm_r, 23675._pm_r,   6._pm_r,217._pm_r, 1.41_pm_r, 93._pm_r,   3._pm_r,119._pm_r,  0.45_pm_r,213._pm_r, &
224.30_pm_r, 26947._pm_r,   5._pm_r,198._pm_r, 1.68_pm_r, 74._pm_r,   3._pm_r,130._pm_r,  0.42_pm_r,211._pm_r, &
229.10_pm_r, 30264._pm_r,   4._pm_r,167._pm_r, 1.94_pm_r, 56._pm_r,   4._pm_r,139._pm_r,  0.32_pm_r,209._pm_r, &
236.60_pm_r, 33671._pm_r,   4._pm_r,119._pm_r, 2.15_pm_r, 39._pm_r,   4._pm_r,144._pm_r,  0.17_pm_r,201._pm_r, &
244.80_pm_r, 37198._pm_r,   5._pm_r, 77._pm_r, 2.19_pm_r, 25._pm_r,   4._pm_r,145._pm_r,  0.05_pm_r,127._pm_r, &
254.20_pm_r, 40848._pm_r,   7._pm_r, 55._pm_r, 2.04_pm_r, 10._pm_r,   4._pm_r,144._pm_r,  0.14_pm_r, 60._pm_r, &
263.20_pm_r, 44644._pm_r,   9._pm_r, 40._pm_r, 1.74_pm_r,349._pm_r,   4._pm_r,139._pm_r,  0.27_pm_r, 41._pm_r, &
265.70_pm_r, 48524._pm_r,  10._pm_r, 28._pm_r, 1.50_pm_r,321._pm_r,   4._pm_r,132._pm_r,  0.31_pm_r, 27._pm_r, &
259.10_pm_r, 52373._pm_r,  10._pm_r, 17._pm_r, 1.34_pm_r,285._pm_r,   4._pm_r,126._pm_r,  0.23_pm_r,  8._pm_r, &
248.90_pm_r, 56096._pm_r,  10._pm_r,  6._pm_r, 1.38_pm_r,245._pm_r,   3._pm_r,124._pm_r,  0.09_pm_r,288._pm_r, &
241.20_pm_r, 59682._pm_r,   9._pm_r,357._pm_r, 1.34_pm_r,218._pm_r,   3._pm_r,127._pm_r,  0.20_pm_r,214._pm_r, &
233.20_pm_r, 63158._pm_r,   7._pm_r,350._pm_r, 1.19_pm_r,196._pm_r,   4._pm_r,133._pm_r,  0.28_pm_r,197._pm_r, &
226.20_pm_r, 66519._pm_r,   6._pm_r,345._pm_r,  0.91_pm_r,172._pm_r,   4._pm_r,138._pm_r,  0.28_pm_r,187._pm_r, &
220.10_pm_r, 69788._pm_r,   5._pm_r,347._pm_r,  0.71_pm_r,141._pm_r,   4._pm_r,142._pm_r,  0.24_pm_r,178._pm_r, &
213.50_pm_r, 72962._pm_r,   4._pm_r,358._pm_r,  0.70_pm_r,109._pm_r,   4._pm_r,144._pm_r,  0.17_pm_r,165._pm_r, &
207.60_pm_r, 76047._pm_r,   4._pm_r, 14._pm_r,  0.80_pm_r, 86._pm_r,   4._pm_r,145._pm_r,  0.12_pm_r,145._pm_r, &
202.30_pm_r, 79045._pm_r,   4._pm_r, 29._pm_r,  0.88_pm_r, 76._pm_r,   5._pm_r,144._pm_r,  0.11_pm_r,131._pm_r, &
194.40_pm_r, 81970._pm_r,   5._pm_r, 39._pm_r,  0.89_pm_r, 70._pm_r,   5._pm_r,144._pm_r,  0.09_pm_r,117._pm_r, &
182.80_pm_r, 84710._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
173.80_pm_r, 87293._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
169.20_pm_r, 89793._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
169.60_pm_r, 92280._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
174.60_pm_r, 94817._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.20_pm_r, 97475._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.90_pm_r,100349._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.40_pm_r,103552._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
256.50_pm_r,107242._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
300.20_pm_r,111598._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
377.90_pm_r,116979._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)       


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
281.50_pm_r,   -57._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
262.20_pm_r,  3919._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
237.60_pm_r,  7575._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.80_pm_r, 10934._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.70_pm_r, 14156._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.70_pm_r, 17357._pm_r,   6._pm_r,219._pm_r,  0.62_pm_r, 86._pm_r,   2._pm_r, 95._pm_r,  0.21_pm_r,219._pm_r, &
220.00_pm_r, 20569._pm_r,   6._pm_r,211._pm_r,  0.87_pm_r, 76._pm_r,   2._pm_r,104._pm_r,  0.29_pm_r,220._pm_r, &
222.50_pm_r, 23810._pm_r,   5._pm_r,200._pm_r, 1.07_pm_r, 63._pm_r,   2._pm_r,117._pm_r,  0.31_pm_r,220._pm_r, &
225.00_pm_r, 27084._pm_r,   3._pm_r,183._pm_r, 1.21_pm_r, 47._pm_r,   2._pm_r,131._pm_r,  0.28_pm_r,224._pm_r, &
230.20_pm_r, 30415._pm_r,   2._pm_r,152._pm_r, 1.30_pm_r, 30._pm_r,   2._pm_r,143._pm_r,  0.19_pm_r,227._pm_r, &
238.80_pm_r, 33845._pm_r,   2._pm_r, 91._pm_r, 1.34_pm_r, 13._pm_r,   2._pm_r,149._pm_r,  0.07_pm_r,242._pm_r, &
248.70_pm_r, 37416._pm_r,   3._pm_r, 44._pm_r, 1.29_pm_r,357._pm_r,   2._pm_r,150._pm_r,  0.05_pm_r, 17._pm_r, &
258.60_pm_r, 41128._pm_r,   4._pm_r, 23._pm_r, 1.14_pm_r,342._pm_r,   2._pm_r,145._pm_r,  0.14_pm_r, 33._pm_r, &
266.00_pm_r, 44977._pm_r,   5._pm_r, 10._pm_r,  0.86_pm_r,321._pm_r,   2._pm_r,138._pm_r,  0.18_pm_r, 24._pm_r, &
267.40_pm_r, 48891._pm_r,   6._pm_r,360._pm_r,  0.71_pm_r,289._pm_r,   1._pm_r,127._pm_r,  0.21_pm_r, 14._pm_r, &
259.20_pm_r, 52752._pm_r,   6._pm_r,349._pm_r,  0.73_pm_r,253._pm_r,   1._pm_r,116._pm_r,  0.17_pm_r, 10._pm_r, &
247.70_pm_r, 56467._pm_r,   5._pm_r,338._pm_r,  0.81_pm_r,226._pm_r,   1._pm_r,109._pm_r,  0.07_pm_r, 62._pm_r, &
237.90_pm_r, 60020._pm_r,   5._pm_r,327._pm_r,  0.76_pm_r,209._pm_r,   2._pm_r,109._pm_r,  0.19_pm_r,123._pm_r, &
228.80_pm_r, 63439._pm_r,   4._pm_r,316._pm_r,  0.62_pm_r,192._pm_r,   2._pm_r,113._pm_r,  0.31_pm_r,130._pm_r, &
221.70_pm_r, 66734._pm_r,   4._pm_r,307._pm_r,  0.43_pm_r,172._pm_r,   2._pm_r,117._pm_r,  0.37_pm_r,130._pm_r, &
216.50_pm_r, 69943._pm_r,   4._pm_r,303._pm_r,  0.30_pm_r,138._pm_r,   3._pm_r,119._pm_r,  0.41_pm_r,128._pm_r, &
212.10_pm_r, 73081._pm_r,   3._pm_r,304._pm_r,  0.31_pm_r, 98._pm_r,   4._pm_r,120._pm_r,  0.41_pm_r,127._pm_r, &
207.90_pm_r, 76158._pm_r,   3._pm_r,310._pm_r,  0.37_pm_r, 76._pm_r,   4._pm_r,121._pm_r,  0.41_pm_r,125._pm_r, &
203.70_pm_r, 79170._pm_r,   3._pm_r,323._pm_r,  0.43_pm_r, 65._pm_r,   5._pm_r,122._pm_r,  0.39_pm_r,123._pm_r, &
197.20_pm_r, 82125._pm_r,   2._pm_r,338._pm_r,  0.45_pm_r, 58._pm_r,   5._pm_r,122._pm_r,  0.36_pm_r,123._pm_r, &
187.30_pm_r, 84927._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.90_pm_r, 87589._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
173.80_pm_r, 90164._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
173.50_pm_r, 92715._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
177.10_pm_r, 95301._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.70_pm_r, 97985._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.80_pm_r,100850._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.10_pm_r,104001._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
250.00_pm_r,107599._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
298.80_pm_r,111889._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
377.70_pm_r,117278._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
289.70_pm_r,   -11._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
268.90_pm_r,  4078._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
243.60_pm_r,  7830._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.00_pm_r, 11238._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
213.10_pm_r, 14423._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
212.50_pm_r, 17531._pm_r,   3._pm_r,184._pm_r,  0.08_pm_r,135._pm_r,   1._pm_r, 98._pm_r,  0.10_pm_r,298._pm_r, &
216.30_pm_r, 20668._pm_r,   3._pm_r,182._pm_r,  0.06_pm_r,121._pm_r,   1._pm_r, 95._pm_r,  0.14_pm_r,300._pm_r, &
221.30_pm_r, 23876._pm_r,   3._pm_r,181._pm_r,  0.04_pm_r,360._pm_r,   1._pm_r, 88._pm_r,  0.15_pm_r,302._pm_r, &
225.20_pm_r, 27144._pm_r,   3._pm_r,182._pm_r,  0.17_pm_r,335._pm_r,   1._pm_r, 78._pm_r,  0.14_pm_r,306._pm_r, &
231.20_pm_r, 30485._pm_r,   2._pm_r,187._pm_r,  0.30_pm_r,329._pm_r,   1._pm_r, 65._pm_r,  0.10_pm_r,313._pm_r, &
239.70_pm_r, 33929._pm_r,   2._pm_r,197._pm_r,  0.39_pm_r,325._pm_r,   1._pm_r, 55._pm_r,  0.04_pm_r,346._pm_r, &
250.10_pm_r, 37517._pm_r,   2._pm_r,214._pm_r,  0.39_pm_r,322._pm_r,   1._pm_r, 54._pm_r,  0.05_pm_r, 84._pm_r, &
260.20_pm_r, 41253._pm_r,   2._pm_r,234._pm_r,  0.33_pm_r,317._pm_r,   1._pm_r, 60._pm_r,  0.09_pm_r,108._pm_r, &
266.80_pm_r, 45119._pm_r,   2._pm_r,246._pm_r,  0.17_pm_r,299._pm_r,   1._pm_r, 68._pm_r,  0.09_pm_r,118._pm_r, &
268.10_pm_r, 49043._pm_r,   2._pm_r,249._pm_r,  0.09_pm_r,234._pm_r,   1._pm_r, 73._pm_r,  0.06_pm_r, 99._pm_r, &
259.60_pm_r, 52913._pm_r,   2._pm_r,244._pm_r,  0.20_pm_r,180._pm_r,   1._pm_r, 74._pm_r,  0.07_pm_r, 74._pm_r, &
247.30_pm_r, 56628._pm_r,   2._pm_r,234._pm_r,  0.33_pm_r,171._pm_r,   1._pm_r, 74._pm_r,  0.18_pm_r, 74._pm_r, &
237.00_pm_r, 60170._pm_r,   2._pm_r,222._pm_r,  0.37_pm_r,173._pm_r,   1._pm_r, 75._pm_r,  0.28_pm_r, 86._pm_r, &
228.30_pm_r, 63578._pm_r,   3._pm_r,214._pm_r,  0.32_pm_r,178._pm_r,   2._pm_r, 79._pm_r,  0.38_pm_r, 92._pm_r, &
221.10_pm_r, 66866._pm_r,   3._pm_r,210._pm_r,  0.22_pm_r,194._pm_r,   3._pm_r, 82._pm_r,  0.44_pm_r, 95._pm_r, &
215.30_pm_r, 70061._pm_r,   3._pm_r,210._pm_r,  0.13_pm_r,243._pm_r,   3._pm_r, 85._pm_r,  0.47_pm_r, 97._pm_r, &
211.70_pm_r, 73186._pm_r,   3._pm_r,214._pm_r,  0.19_pm_r,287._pm_r,   4._pm_r, 88._pm_r,  0.49_pm_r, 98._pm_r, &
208.70_pm_r, 76265._pm_r,   4._pm_r,219._pm_r,  0.29_pm_r,304._pm_r,   5._pm_r, 89._pm_r,  0.52_pm_r, 99._pm_r, &
205.40_pm_r, 79297._pm_r,   4._pm_r,227._pm_r,  0.34_pm_r,310._pm_r,   5._pm_r, 91._pm_r,  0.51_pm_r, 98._pm_r, &
200.00_pm_r, 82283._pm_r,   4._pm_r,235._pm_r,  0.37_pm_r,313._pm_r,   6._pm_r, 92._pm_r,  0.49_pm_r,101._pm_r, &
191.40_pm_r, 85141._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.60_pm_r, 87874._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.60_pm_r, 90522._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
177.70_pm_r, 93142._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
179.90_pm_r, 95782._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.40_pm_r, 98497._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
195.80_pm_r,101355._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
212.70_pm_r,104454._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
242.70_pm_r,107955._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
295.50_pm_r,112157._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
377.10_pm_r,117533._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
296.10_pm_r,   -47._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
273.70_pm_r,  4128._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
248.90_pm_r,  7957._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.00_pm_r, 11421._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
207.40_pm_r, 14582._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
205.80_pm_r, 17588._pm_r,   2._pm_r,147._pm_r,  0.18_pm_r,264._pm_r,   0._pm_r,332._pm_r,  0.02_pm_r, 56._pm_r, &
212.50_pm_r, 20649._pm_r,   2._pm_r,155._pm_r,  0.23_pm_r,261._pm_r,   0._pm_r,337._pm_r,  0.03_pm_r, 68._pm_r, &
219.70_pm_r, 23818._pm_r,   2._pm_r,166._pm_r,  0.26_pm_r,258._pm_r,   0._pm_r,345._pm_r,  0.04_pm_r, 68._pm_r, &
225.10_pm_r, 27074._pm_r,   2._pm_r,177._pm_r,  0.25_pm_r,252._pm_r,   0._pm_r,357._pm_r,  0.06_pm_r, 80._pm_r, &
231.50_pm_r, 30417._pm_r,   2._pm_r,186._pm_r,  0.21_pm_r,242._pm_r,   0._pm_r, 13._pm_r,  0.08_pm_r, 92._pm_r, &
240.80_pm_r, 33871._pm_r,   2._pm_r,192._pm_r,  0.18_pm_r,226._pm_r,   0._pm_r, 33._pm_r,  0.11_pm_r, 98._pm_r, &
250.70_pm_r, 37474._pm_r,   2._pm_r,194._pm_r,  0.17_pm_r,213._pm_r,   1._pm_r, 51._pm_r,  0.13_pm_r,104._pm_r, &
260.00_pm_r, 41211._pm_r,   3._pm_r,196._pm_r,  0.16_pm_r,203._pm_r,   1._pm_r, 66._pm_r,  0.14_pm_r,108._pm_r, &
266.60_pm_r, 45073._pm_r,   3._pm_r,196._pm_r,  0.12_pm_r,191._pm_r,   1._pm_r, 75._pm_r,  0.11_pm_r, 98._pm_r, &
268.00_pm_r, 48995._pm_r,   3._pm_r,195._pm_r,  0.14_pm_r,172._pm_r,   1._pm_r, 76._pm_r,  0.04_pm_r, 56._pm_r, &
259.90_pm_r, 52866._pm_r,   3._pm_r,193._pm_r,  0.24_pm_r,168._pm_r,   1._pm_r, 72._pm_r,  0.14_pm_r,285._pm_r, &
248.40_pm_r, 56590._pm_r,   4._pm_r,191._pm_r,  0.39_pm_r,180._pm_r,   1._pm_r, 55._pm_r,  0.36_pm_r,275._pm_r, &
239.40_pm_r, 60157._pm_r,   4._pm_r,190._pm_r,  0.45_pm_r,196._pm_r,   0._pm_r,349._pm_r,  0.32_pm_r,271._pm_r, &
231.90_pm_r, 63610._pm_r,   5._pm_r,192._pm_r,  0.42_pm_r,215._pm_r,   1._pm_r,309._pm_r,  0.14_pm_r,276._pm_r, &
223.70_pm_r, 66944._pm_r,   6._pm_r,196._pm_r,  0.37_pm_r,242._pm_r,   1._pm_r,312._pm_r,  0.14_pm_r, 68._pm_r, &
216.50_pm_r, 70167._pm_r,   6._pm_r,200._pm_r,  0.38_pm_r,274._pm_r,   1._pm_r,356._pm_r,  0.42_pm_r, 75._pm_r, &
211.80_pm_r, 73300._pm_r,   6._pm_r,207._pm_r,  0.47_pm_r,298._pm_r,   1._pm_r, 47._pm_r,  0.68_pm_r, 76._pm_r, &
208.50_pm_r, 76378._pm_r,   6._pm_r,214._pm_r,  0.56_pm_r,310._pm_r,   2._pm_r, 62._pm_r,  0.87_pm_r, 76._pm_r, &
205.60_pm_r, 79408._pm_r,   6._pm_r,223._pm_r,  0.63_pm_r,316._pm_r,   4._pm_r, 68._pm_r,  0.99_pm_r, 76._pm_r, &
201.10_pm_r, 82399._pm_r,   6._pm_r,232._pm_r,  0.65_pm_r,320._pm_r,   5._pm_r, 70._pm_r, 1.02_pm_r, 76._pm_r, &
194.00_pm_r, 85289._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.20_pm_r, 88072._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.70_pm_r, 90780._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.70_pm_r, 93461._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.80_pm_r, 96155._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.40_pm_r, 98901._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.10_pm_r,101757._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.20_pm_r,104810._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
235.70_pm_r,108221._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
290.90_pm_r,112327._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
376.10_pm_r,117673._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
299.70_pm_r,   -73._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
276.30_pm_r,  4150._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
252.60_pm_r,  8028._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.60_pm_r, 11534._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.00_pm_r, 14676._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.40_pm_r, 17601._pm_r,   2._pm_r,153._pm_r,  0.22_pm_r,287._pm_r,   1._pm_r,306._pm_r,  0.17_pm_r,102._pm_r, &
209.40_pm_r, 20599._pm_r,   2._pm_r,161._pm_r,  0.28_pm_r,285._pm_r,   1._pm_r,316._pm_r,  0.23_pm_r,103._pm_r, &
218.30_pm_r, 23736._pm_r,   2._pm_r,170._pm_r,  0.28_pm_r,282._pm_r,   0._pm_r,342._pm_r,  0.25_pm_r,106._pm_r, &
224.60_pm_r, 26978._pm_r,   2._pm_r,181._pm_r,  0.24_pm_r,272._pm_r,   0._pm_r, 42._pm_r,  0.27_pm_r,110._pm_r, &
231.80_pm_r, 30320._pm_r,   2._pm_r,190._pm_r,  0.19_pm_r,258._pm_r,   1._pm_r, 77._pm_r,  0.24_pm_r,114._pm_r, &
241.60_pm_r, 33782._pm_r,   2._pm_r,196._pm_r,  0.16_pm_r,235._pm_r,   1._pm_r, 91._pm_r,  0.19_pm_r,122._pm_r, &
252.30_pm_r, 37401._pm_r,   2._pm_r,199._pm_r,  0.16_pm_r,217._pm_r,   1._pm_r, 98._pm_r,  0.14_pm_r,129._pm_r, &
262.50_pm_r, 41168._pm_r,   3._pm_r,200._pm_r,  0.17_pm_r,213._pm_r,   1._pm_r,103._pm_r,  0.10_pm_r,143._pm_r, &
268.70_pm_r, 45065._pm_r,   3._pm_r,201._pm_r,  0.15_pm_r,204._pm_r,   1._pm_r,106._pm_r,  0.03_pm_r,202._pm_r, &
268.60_pm_r, 49008._pm_r,   3._pm_r,201._pm_r,  0.12_pm_r,228._pm_r,   1._pm_r,107._pm_r,  0.11_pm_r,280._pm_r, &
260.30_pm_r, 52884._pm_r,   3._pm_r,205._pm_r,  0.19_pm_r,273._pm_r,   1._pm_r,109._pm_r,  0.26_pm_r,285._pm_r, &
250.50_pm_r, 56625._pm_r,   3._pm_r,211._pm_r,  0.37_pm_r,290._pm_r,   0._pm_r,108._pm_r,  0.40_pm_r,290._pm_r, &
242.30_pm_r, 60232._pm_r,   3._pm_r,222._pm_r,  0.44_pm_r,291._pm_r,   0._pm_r,308._pm_r,  0.35_pm_r,297._pm_r, &
233.20_pm_r, 63718._pm_r,   4._pm_r,231._pm_r,  0.40_pm_r,291._pm_r,   1._pm_r,303._pm_r,  0.22_pm_r,313._pm_r, &
224.10_pm_r, 67063._pm_r,   4._pm_r,237._pm_r,  0.23_pm_r,285._pm_r,   1._pm_r,312._pm_r,  0.12_pm_r, 21._pm_r, &
216.70_pm_r, 70291._pm_r,   4._pm_r,239._pm_r,  0.04_pm_r,256._pm_r,   1._pm_r,332._pm_r,  0.27_pm_r, 75._pm_r, &
211.10_pm_r, 73420._pm_r,   4._pm_r,237._pm_r,  0.16_pm_r,125._pm_r,   1._pm_r, 13._pm_r,  0.43_pm_r, 87._pm_r, &
207.20_pm_r, 76483._pm_r,   4._pm_r,233._pm_r,  0.31_pm_r,119._pm_r,   1._pm_r, 51._pm_r,  0.55_pm_r, 90._pm_r, &
205.40_pm_r, 79499._pm_r,   4._pm_r,225._pm_r,  0.40_pm_r,117._pm_r,   2._pm_r, 68._pm_r,  0.63_pm_r, 92._pm_r, &
202.10_pm_r, 82496._pm_r,   4._pm_r,216._pm_r,  0.46_pm_r,116._pm_r,   3._pm_r, 77._pm_r,  0.66_pm_r, 93._pm_r, &
195.70_pm_r, 85407._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.40_pm_r, 88219._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.60_pm_r, 90966._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.80_pm_r, 93695._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.20_pm_r, 96432._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 99206._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.80_pm_r,102060._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.60_pm_r,105079._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
229.80_pm_r,108417._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
286.00_pm_r,112436._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
374.80_pm_r,117742._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
301.40_pm_r,  -106._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
276.90_pm_r,  4136._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.70_pm_r,  8028._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.20_pm_r, 11548._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.80_pm_r, 14680._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.70_pm_r, 17566._pm_r,   3._pm_r,157._pm_r,  0.23_pm_r,295._pm_r,   1._pm_r,306._pm_r,  0.22_pm_r,112._pm_r, &
207.70_pm_r, 20532._pm_r,   2._pm_r,164._pm_r,  0.28_pm_r,291._pm_r,   1._pm_r,312._pm_r,  0.28_pm_r,112._pm_r, &
217.50_pm_r, 23650._pm_r,   2._pm_r,173._pm_r,  0.28_pm_r,287._pm_r,   1._pm_r,329._pm_r,  0.30_pm_r,112._pm_r, &
224.00_pm_r, 26884._pm_r,   2._pm_r,183._pm_r,  0.26_pm_r,279._pm_r,   0._pm_r, 22._pm_r,  0.28_pm_r,113._pm_r, &
231.10_pm_r, 30215._pm_r,   2._pm_r,193._pm_r,  0.21_pm_r,267._pm_r,   1._pm_r, 72._pm_r,  0.21_pm_r,116._pm_r, &
242.00_pm_r, 33674._pm_r,   2._pm_r,200._pm_r,  0.17_pm_r,253._pm_r,   1._pm_r, 87._pm_r,  0.12_pm_r,121._pm_r, &
253.80_pm_r, 37307._pm_r,   2._pm_r,204._pm_r,  0.15_pm_r,246._pm_r,   1._pm_r, 92._pm_r,  0.03_pm_r,145._pm_r, &
264.60_pm_r, 41102._pm_r,   3._pm_r,207._pm_r,  0.13_pm_r,249._pm_r,   1._pm_r, 93._pm_r,  0.05_pm_r,276._pm_r, &
270.80_pm_r, 45031._pm_r,   3._pm_r,211._pm_r,  0.17_pm_r,272._pm_r,   1._pm_r, 93._pm_r,  0.11_pm_r,266._pm_r, &
270.80_pm_r, 49003._pm_r,   3._pm_r,216._pm_r,  0.23_pm_r,270._pm_r,   0._pm_r,103._pm_r,  0.17_pm_r,240._pm_r, &
264.50_pm_r, 52927._pm_r,   3._pm_r,222._pm_r,  0.25_pm_r,265._pm_r,   0._pm_r,147._pm_r,  0.23_pm_r,234._pm_r, &
253.30_pm_r, 56726._pm_r,   3._pm_r,225._pm_r,  0.14_pm_r,253._pm_r,   1._pm_r,197._pm_r,  0.25_pm_r,250._pm_r, &
241.80_pm_r, 60346._pm_r,   3._pm_r,226._pm_r,  0.05_pm_r, 41._pm_r,   1._pm_r,224._pm_r,  0.20_pm_r,281._pm_r, &
231.80_pm_r, 63816._pm_r,   3._pm_r,225._pm_r,  0.25_pm_r, 56._pm_r,   1._pm_r,242._pm_r,  0.13_pm_r,323._pm_r, &
222.50_pm_r, 67139._pm_r,   3._pm_r,223._pm_r,  0.41_pm_r, 57._pm_r,   1._pm_r,255._pm_r,  0.13_pm_r, 32._pm_r, &
215.30_pm_r, 70344._pm_r,   2._pm_r,218._pm_r,  0.54_pm_r, 57._pm_r,   1._pm_r,264._pm_r,  0.24_pm_r, 72._pm_r, &
210.80_pm_r, 73461._pm_r,   1._pm_r,205._pm_r,  0.62_pm_r, 56._pm_r,   0._pm_r,325._pm_r,  0.38_pm_r, 87._pm_r, &
207.80_pm_r, 76526._pm_r,   1._pm_r,150._pm_r,  0.69_pm_r, 56._pm_r,   1._pm_r, 87._pm_r,  0.49_pm_r, 93._pm_r, &
207.40_pm_r, 79561._pm_r,   1._pm_r, 88._pm_r,  0.72_pm_r, 55._pm_r,   1._pm_r, 91._pm_r,  0.57_pm_r, 96._pm_r, &
204.50_pm_r, 82596._pm_r,   2._pm_r, 72._pm_r,  0.72_pm_r, 55._pm_r,   2._pm_r, 93._pm_r,  0.60_pm_r, 98._pm_r, &
197.20_pm_r, 85531._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.50_pm_r, 88357._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.20_pm_r, 91125._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.60_pm_r, 93880._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 96646._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.00_pm_r, 99438._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.90_pm_r,102291._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.20_pm_r,105286._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.70_pm_r,108575._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
282.10_pm_r,112530._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
373.50_pm_r,117798._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
301.40_pm_r,  -132._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.20_pm_r,  4112._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
254.00_pm_r,  8009._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.50_pm_r, 11534._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.40_pm_r, 14666._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
196.90_pm_r, 17546._pm_r,   2._pm_r,157._pm_r,  0.09_pm_r,277._pm_r,   1._pm_r,309._pm_r,  0.22_pm_r,107._pm_r, &
207.00_pm_r, 20506._pm_r,   2._pm_r,161._pm_r,  0.12_pm_r,270._pm_r,   1._pm_r,319._pm_r,  0.27_pm_r,107._pm_r, &
217.10_pm_r, 23613._pm_r,   2._pm_r,167._pm_r,  0.14_pm_r,266._pm_r,   1._pm_r,342._pm_r,  0.27_pm_r,107._pm_r, &
223.90_pm_r, 26834._pm_r,   2._pm_r,175._pm_r,  0.15_pm_r,266._pm_r,   0._pm_r, 23._pm_r,  0.21_pm_r,109._pm_r, &
230.60_pm_r, 30158._pm_r,   2._pm_r,184._pm_r,  0.17_pm_r,267._pm_r,   0._pm_r, 55._pm_r,  0.12_pm_r,117._pm_r, &
242.20_pm_r, 33615._pm_r,   2._pm_r,194._pm_r,  0.18_pm_r,273._pm_r,   1._pm_r, 68._pm_r,  0.03_pm_r,162._pm_r, &
254.70_pm_r, 37257._pm_r,   2._pm_r,203._pm_r,  0.19_pm_r,284._pm_r,   0._pm_r, 71._pm_r,  0.09_pm_r,267._pm_r, &
265.90_pm_r, 41065._pm_r,   2._pm_r,213._pm_r,  0.22_pm_r,292._pm_r,   0._pm_r, 61._pm_r,  0.16_pm_r,277._pm_r, &
272.40_pm_r, 45010._pm_r,   2._pm_r,223._pm_r,  0.29_pm_r,293._pm_r,   0._pm_r,349._pm_r,  0.18_pm_r,279._pm_r, &
272.40_pm_r, 49001._pm_r,   2._pm_r,233._pm_r,  0.26_pm_r,295._pm_r,   0._pm_r,303._pm_r,  0.20_pm_r,294._pm_r, &
266.10_pm_r, 52943._pm_r,   2._pm_r,239._pm_r,  0.07_pm_r,321._pm_r,   1._pm_r,296._pm_r,  0.21_pm_r,307._pm_r, &
254.50_pm_r, 56758._pm_r,   2._pm_r,238._pm_r,  0.31_pm_r, 86._pm_r,   1._pm_r,293._pm_r,  0.21_pm_r,315._pm_r, &
242.00_pm_r, 60386._pm_r,   2._pm_r,232._pm_r,  0.53_pm_r, 81._pm_r,   1._pm_r,290._pm_r,  0.10_pm_r,309._pm_r, &
230.50_pm_r, 63848._pm_r,   1._pm_r,219._pm_r,  0.67_pm_r, 73._pm_r,   1._pm_r,285._pm_r,  0.04_pm_r,124._pm_r, &
221.20_pm_r, 67158._pm_r,   1._pm_r,186._pm_r,  0.70_pm_r, 64._pm_r,   1._pm_r,277._pm_r,  0.18_pm_r,119._pm_r, &
214.50_pm_r, 70352._pm_r,   1._pm_r,119._pm_r,  0.72_pm_r, 55._pm_r,   0._pm_r,249._pm_r,  0.34_pm_r,122._pm_r, &
210.50_pm_r, 73465._pm_r,   2._pm_r, 86._pm_r,  0.76_pm_r, 47._pm_r,   0._pm_r,157._pm_r,  0.47_pm_r,118._pm_r, &
208.80_pm_r, 76536._pm_r,   2._pm_r, 73._pm_r,  0.80_pm_r, 39._pm_r,   1._pm_r,131._pm_r,  0.58_pm_r,119._pm_r, &
209.20_pm_r, 79592._pm_r,   3._pm_r, 66._pm_r,  0.82_pm_r, 36._pm_r,   2._pm_r,123._pm_r,  0.65_pm_r,118._pm_r, &
206.30_pm_r, 82655._pm_r,   4._pm_r, 62._pm_r,  0.81_pm_r, 33._pm_r,   2._pm_r,120._pm_r,  0.67_pm_r,118._pm_r, &
198.00_pm_r, 85606._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.80_pm_r, 88436._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.60_pm_r, 91210._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 93974._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 96749._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.10_pm_r, 99547._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.30_pm_r,102398._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.00_pm_r,105379._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.70_pm_r,108644._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
280.10_pm_r,112567._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
372.50_pm_r,117812._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
300.50_pm_r,  -112._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.30_pm_r,  4126._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.30_pm_r,  8018._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.20_pm_r, 11535._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.20_pm_r, 14670._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.90_pm_r, 17560._pm_r,   3._pm_r,157._pm_r,  0.29_pm_r,333._pm_r,   1._pm_r,318._pm_r,  0.22_pm_r, 93._pm_r, &
207.30_pm_r, 20525._pm_r,   2._pm_r,158._pm_r,  0.36_pm_r,331._pm_r,   1._pm_r,343._pm_r,  0.26_pm_r, 97._pm_r, &
216.00_pm_r, 23628._pm_r,   2._pm_r,162._pm_r,  0.36_pm_r,325._pm_r,   1._pm_r, 20._pm_r,  0.23_pm_r,101._pm_r, &
223.30_pm_r, 26843._pm_r,   1._pm_r,170._pm_r,  0.33_pm_r,317._pm_r,   1._pm_r, 48._pm_r,  0.15_pm_r,116._pm_r, &
230.70_pm_r, 30167._pm_r,   1._pm_r,187._pm_r,  0.29_pm_r,303._pm_r,   1._pm_r, 61._pm_r,  0.08_pm_r,166._pm_r, &
242.60_pm_r, 33626._pm_r,   1._pm_r,212._pm_r,  0.26_pm_r,288._pm_r,   1._pm_r, 70._pm_r,  0.13_pm_r,233._pm_r, &
254.50_pm_r, 37271._pm_r,   1._pm_r,231._pm_r,  0.26_pm_r,278._pm_r,   0._pm_r, 75._pm_r,  0.21_pm_r,250._pm_r, &
265.30_pm_r, 41076._pm_r,   1._pm_r,242._pm_r,  0.27_pm_r,279._pm_r,   0._pm_r,254._pm_r,  0.25_pm_r,258._pm_r, &
271.60_pm_r, 45016._pm_r,   2._pm_r,253._pm_r,  0.34_pm_r,291._pm_r,   0._pm_r,262._pm_r,  0.23_pm_r,267._pm_r, &
271.40_pm_r, 49000._pm_r,   2._pm_r,262._pm_r,  0.32_pm_r,304._pm_r,   1._pm_r,266._pm_r,  0.17_pm_r,279._pm_r, &
264.90_pm_r, 52930._pm_r,   2._pm_r,269._pm_r,  0.14_pm_r,335._pm_r,   1._pm_r,272._pm_r,  0.12_pm_r,315._pm_r, &
252.90_pm_r, 56731._pm_r,   2._pm_r,271._pm_r,  0.22_pm_r, 99._pm_r,   1._pm_r,282._pm_r,  0.15_pm_r,352._pm_r, &
240.40_pm_r, 60337._pm_r,   2._pm_r,267._pm_r,  0.30_pm_r,118._pm_r,   1._pm_r,294._pm_r,  0.14_pm_r, 10._pm_r, &
230.30_pm_r, 63785._pm_r,   2._pm_r,257._pm_r,  0.26_pm_r,127._pm_r,   1._pm_r,304._pm_r,  0.10_pm_r, 57._pm_r, &
221.40_pm_r, 67090._pm_r,   1._pm_r,247._pm_r,  0.16_pm_r,155._pm_r,   1._pm_r,311._pm_r,  0.16_pm_r,106._pm_r, &
214.60_pm_r, 70282._pm_r,   2._pm_r,241._pm_r,  0.14_pm_r,232._pm_r,   1._pm_r,317._pm_r,  0.26_pm_r,127._pm_r, &
211.00_pm_r, 73394._pm_r,   2._pm_r,243._pm_r,  0.26_pm_r,265._pm_r,   0._pm_r,333._pm_r,  0.36_pm_r,135._pm_r, &
209.10_pm_r, 76469._pm_r,   2._pm_r,248._pm_r,  0.38_pm_r,273._pm_r,   0._pm_r,131._pm_r,  0.44_pm_r,137._pm_r, &
208.70_pm_r, 79526._pm_r,   3._pm_r,254._pm_r,  0.47_pm_r,277._pm_r,   1._pm_r,135._pm_r,  0.49_pm_r,139._pm_r, &
205.40_pm_r, 82580._pm_r,   4._pm_r,259._pm_r,  0.51_pm_r,279._pm_r,   2._pm_r,137._pm_r,  0.51_pm_r,141._pm_r, &
197.60_pm_r, 85525._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.70_pm_r, 88355._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.20_pm_r, 91124._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.70_pm_r, 93881._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 96647._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.70_pm_r, 99438._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.20_pm_r,102285._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.00_pm_r,105266._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.00_pm_r,108533._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
280.30_pm_r,112460._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
371.90_pm_r,117701._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)       


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
297.40_pm_r,  -111._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
275.00_pm_r,  4086._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
250.10_pm_r,  7936._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.30_pm_r, 11421._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.20_pm_r, 14570._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.20_pm_r, 17512._pm_r,   5._pm_r,155._pm_r,  0.59_pm_r,314._pm_r,   1._pm_r,303._pm_r,  0.34_pm_r, 87._pm_r, &
208.80_pm_r, 20512._pm_r,   4._pm_r,160._pm_r,  0.81_pm_r,320._pm_r,   1._pm_r,338._pm_r,  0.41_pm_r, 86._pm_r, &
216.30_pm_r, 23628._pm_r,   2._pm_r,168._pm_r,  0.89_pm_r,329._pm_r,   1._pm_r, 32._pm_r,  0.37_pm_r, 87._pm_r, &
223.20_pm_r, 26844._pm_r,   1._pm_r,185._pm_r,  0.90_pm_r,340._pm_r,   1._pm_r, 54._pm_r,  0.25_pm_r, 85._pm_r, &
231.10_pm_r, 30170._pm_r,   0._pm_r,285._pm_r,  0.88_pm_r,352._pm_r,   1._pm_r, 59._pm_r,  0.07_pm_r, 81._pm_r, &
241.80_pm_r, 33628._pm_r,   1._pm_r,340._pm_r,  0.80_pm_r,  1._pm_r,   1._pm_r, 58._pm_r,  0.12_pm_r,270._pm_r, &
253.00_pm_r, 37254._pm_r,   3._pm_r,350._pm_r,  0.67_pm_r,  5._pm_r,   1._pm_r, 49._pm_r,  0.27_pm_r,269._pm_r, &
263.80_pm_r, 41037._pm_r,   3._pm_r,354._pm_r,  0.51_pm_r,359._pm_r,   1._pm_r, 24._pm_r,  0.34_pm_r,270._pm_r, &
270.30_pm_r, 44956._pm_r,   4._pm_r,353._pm_r,  0.44_pm_r,332._pm_r,   1._pm_r,342._pm_r,  0.31_pm_r,274._pm_r, &
269.70_pm_r, 48918._pm_r,   5._pm_r,348._pm_r,  0.42_pm_r,306._pm_r,   1._pm_r,320._pm_r,  0.22_pm_r,291._pm_r, &
262.10_pm_r, 52816._pm_r,   5._pm_r,344._pm_r,  0.24_pm_r,276._pm_r,   1._pm_r,316._pm_r,  0.14_pm_r,327._pm_r, &
249.90_pm_r, 56571._pm_r,   5._pm_r,341._pm_r,  0.25_pm_r,162._pm_r,   1._pm_r,322._pm_r,  0.13_pm_r, 42._pm_r, &
238.30_pm_r, 60139._pm_r,   4._pm_r,341._pm_r,  0.40_pm_r,163._pm_r,   1._pm_r,333._pm_r,  0.22_pm_r, 89._pm_r, &
230.30_pm_r, 63569._pm_r,   4._pm_r,340._pm_r,  0.43_pm_r,179._pm_r,   1._pm_r,351._pm_r,  0.32_pm_r,108._pm_r, &
223.80_pm_r, 66892._pm_r,   3._pm_r,334._pm_r,  0.43_pm_r,212._pm_r,   1._pm_r, 21._pm_r,  0.39_pm_r,120._pm_r, &
218.30_pm_r, 70130._pm_r,   3._pm_r,321._pm_r,  0.55_pm_r,242._pm_r,   1._pm_r, 61._pm_r,  0.44_pm_r,128._pm_r, &
214.10_pm_r, 73293._pm_r,   4._pm_r,306._pm_r,  0.76_pm_r,258._pm_r,   1._pm_r, 90._pm_r,  0.48_pm_r,135._pm_r, &
211.20_pm_r, 76408._pm_r,   5._pm_r,295._pm_r,  0.95_pm_r,265._pm_r,   2._pm_r,107._pm_r,  0.52_pm_r,139._pm_r, &
209.20_pm_r, 79484._pm_r,   6._pm_r,288._pm_r, 1.07_pm_r,269._pm_r,   3._pm_r,116._pm_r,  0.52_pm_r,142._pm_r, &
205.00_pm_r, 82533._pm_r,   7._pm_r,284._pm_r, 1.10_pm_r,271._pm_r,   3._pm_r,122._pm_r,  0.50_pm_r,145._pm_r, &
197.30_pm_r, 85475._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.30_pm_r, 88302._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.10_pm_r, 91059._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.30_pm_r, 93795._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.50_pm_r, 96539._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.10_pm_r, 99314._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.70_pm_r,102158._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.50_pm_r,105151._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.60_pm_r,108448._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
282.50_pm_r,112412._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
371.70_pm_r,117666._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
291.60_pm_r,   -52._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
268.30_pm_r,  4050._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
242.80_pm_r,  7795._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.20_pm_r, 11202._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
209.70_pm_r, 14366._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
207.60_pm_r, 17408._pm_r,   5._pm_r,182._pm_r,  0.34_pm_r,260._pm_r,   1._pm_r, 28._pm_r,  0.28_pm_r, 24._pm_r, &
211.50_pm_r, 20474._pm_r,   5._pm_r,187._pm_r,  0.30_pm_r,304._pm_r,   1._pm_r, 27._pm_r,  0.36_pm_r, 29._pm_r, &
216.80_pm_r, 23611._pm_r,   5._pm_r,189._pm_r,  0.59_pm_r,  6._pm_r,   2._pm_r, 29._pm_r,  0.39_pm_r, 35._pm_r, &
222.80_pm_r, 26825._pm_r,   3._pm_r,186._pm_r, 1.23_pm_r, 24._pm_r,   2._pm_r, 31._pm_r,  0.36_pm_r, 44._pm_r, &
230.00_pm_r, 30142._pm_r,   2._pm_r,152._pm_r, 1.89_pm_r, 29._pm_r,   3._pm_r, 35._pm_r,  0.29_pm_r, 59._pm_r, &
239.60_pm_r, 33576._pm_r,   3._pm_r, 58._pm_r, 2.31_pm_r, 30._pm_r,   3._pm_r, 39._pm_r,  0.20_pm_r, 83._pm_r, &
250.00_pm_r, 37165._pm_r,   6._pm_r, 42._pm_r, 2.32_pm_r, 28._pm_r,   3._pm_r, 43._pm_r,  0.14_pm_r,124._pm_r, &
260.50_pm_r, 40901._pm_r,   9._pm_r, 36._pm_r, 1.96_pm_r, 23._pm_r,   3._pm_r, 46._pm_r,  0.13_pm_r,176._pm_r, &
267.60_pm_r, 44777._pm_r,  11._pm_r, 32._pm_r, 1.36_pm_r, 11._pm_r,   3._pm_r, 47._pm_r,  0.11_pm_r,249._pm_r, &
267.20_pm_r, 48702._pm_r,  13._pm_r, 29._pm_r,  0.88_pm_r,355._pm_r,   3._pm_r, 44._pm_r,  0.19_pm_r,301._pm_r, &
259.70_pm_r, 52564._pm_r,  14._pm_r, 26._pm_r,  0.36_pm_r,341._pm_r,   3._pm_r, 37._pm_r,  0.19_pm_r,319._pm_r, &
248.20_pm_r, 56290._pm_r,  14._pm_r, 26._pm_r,  0.27_pm_r,161._pm_r,   3._pm_r, 34._pm_r,  0.04_pm_r, 56._pm_r, &
238.10_pm_r, 59845._pm_r,  13._pm_r, 28._pm_r,  0.75_pm_r,187._pm_r,   3._pm_r, 38._pm_r,  0.21_pm_r,143._pm_r, &
231.50_pm_r, 63283._pm_r,  12._pm_r, 29._pm_r, 1.00_pm_r,198._pm_r,   3._pm_r, 46._pm_r,  0.33_pm_r,146._pm_r, &
226.30_pm_r, 66634._pm_r,  10._pm_r, 30._pm_r, 1.05_pm_r,210._pm_r,   3._pm_r, 57._pm_r,  0.35_pm_r,142._pm_r, &
221.90_pm_r, 69915._pm_r,   9._pm_r, 29._pm_r, 1.01_pm_r,224._pm_r,   3._pm_r, 67._pm_r,  0.30_pm_r,136._pm_r, &
218.20_pm_r, 73136._pm_r,   7._pm_r, 24._pm_r, 1.00_pm_r,239._pm_r,   3._pm_r, 74._pm_r,  0.24_pm_r,127._pm_r, &
214.40_pm_r, 76305._pm_r,   6._pm_r, 16._pm_r, 1.02_pm_r,251._pm_r,   3._pm_r, 79._pm_r,  0.20_pm_r,113._pm_r, &
210.60_pm_r, 79415._pm_r,   6._pm_r,  2._pm_r, 1.01_pm_r,259._pm_r,   3._pm_r, 81._pm_r,  0.16_pm_r,102._pm_r, &
205.20_pm_r, 82474._pm_r,   6._pm_r,347._pm_r,  0.97_pm_r,265._pm_r,   4._pm_r, 82._pm_r,  0.14_pm_r, 90._pm_r, &
197.10_pm_r, 85416._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.50_pm_r, 88238._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.50_pm_r, 90975._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.20_pm_r, 93681._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.70_pm_r, 96393._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.40_pm_r, 99147._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.80_pm_r,101992._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
205.30_pm_r,105014._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
231.20_pm_r,108366._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
285.90_pm_r,112396._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
371.60_pm_r,117668._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
284.70_pm_r,   -25._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
260.60_pm_r,  3966._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
235.20_pm_r,  7596._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.00_pm_r, 10920._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.50_pm_r, 14101._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.80_pm_r, 17250._pm_r,   5._pm_r,267._pm_r, 2.49_pm_r,192._pm_r,   5._pm_r, 64._pm_r,  0.90_pm_r,298._pm_r, &
215.30_pm_r, 20397._pm_r,   7._pm_r,231._pm_r, 2.89_pm_r,184._pm_r,   4._pm_r, 48._pm_r, 1.04_pm_r,298._pm_r, &
217.40_pm_r, 23565._pm_r,  10._pm_r,211._pm_r, 2.55_pm_r,165._pm_r,   4._pm_r, 29._pm_r,  0.86_pm_r,298._pm_r, &
220.50_pm_r, 26768._pm_r,  11._pm_r,196._pm_r, 2.17_pm_r,123._pm_r,   4._pm_r, 15._pm_r,  0.38_pm_r,299._pm_r, &
225.80_pm_r, 30035._pm_r,  11._pm_r,177._pm_r, 3.11_pm_r, 77._pm_r,   4._pm_r, 14._pm_r,  0.28_pm_r,118._pm_r, &
236.00_pm_r, 33409._pm_r,  11._pm_r,149._pm_r, 4.48_pm_r, 57._pm_r,   4._pm_r, 26._pm_r,  0.85_pm_r,119._pm_r, &
247.40_pm_r, 36952._pm_r,  12._pm_r,112._pm_r, 5.23_pm_r, 46._pm_r,   4._pm_r, 47._pm_r, 1.14_pm_r,118._pm_r, &
258.30_pm_r, 40653._pm_r,  16._pm_r, 86._pm_r, 5.10_pm_r, 38._pm_r,   5._pm_r, 66._pm_r, 1.09_pm_r,118._pm_r, &
265.70_pm_r, 44498._pm_r,  21._pm_r, 72._pm_r, 3.49_pm_r, 31._pm_r,   6._pm_r, 76._pm_r,  0.53_pm_r,135._pm_r, &
266.40_pm_r, 48403._pm_r,  24._pm_r, 65._pm_r, 2.03_pm_r, 17._pm_r,   6._pm_r, 81._pm_r,  0.32_pm_r,198._pm_r, &
258.90_pm_r, 52254._pm_r,  25._pm_r, 61._pm_r, 1.28_pm_r,344._pm_r,   5._pm_r, 85._pm_r,  0.44_pm_r,223._pm_r, &
247.40_pm_r, 55967._pm_r,  25._pm_r, 56._pm_r, 1.32_pm_r,317._pm_r,   5._pm_r, 91._pm_r,  0.43_pm_r,199._pm_r, &
237.80_pm_r, 59513._pm_r,  24._pm_r, 52._pm_r, 1.59_pm_r,301._pm_r,   5._pm_r, 97._pm_r,  0.28_pm_r,176._pm_r, &
231.70_pm_r, 62948._pm_r,  23._pm_r, 46._pm_r, 1.73_pm_r,292._pm_r,   5._pm_r,100._pm_r,  0.18_pm_r,133._pm_r, &
227.50_pm_r, 66309._pm_r,  22._pm_r, 40._pm_r, 1.73_pm_r,289._pm_r,   5._pm_r,100._pm_r,  0.21_pm_r, 79._pm_r, &
223.70_pm_r, 69614._pm_r,  22._pm_r, 34._pm_r, 1.61_pm_r,288._pm_r,   6._pm_r, 98._pm_r,  0.29_pm_r, 55._pm_r, &
220.10_pm_r, 72863._pm_r,  21._pm_r, 28._pm_r, 1.49_pm_r,289._pm_r,   6._pm_r, 94._pm_r,  0.35_pm_r, 42._pm_r, &
215.40_pm_r, 76055._pm_r,  21._pm_r, 23._pm_r, 1.35_pm_r,290._pm_r,   6._pm_r, 90._pm_r,  0.38_pm_r, 35._pm_r, &
210.20_pm_r, 79168._pm_r,  21._pm_r, 18._pm_r, 1.23_pm_r,292._pm_r,   7._pm_r, 86._pm_r,  0.41_pm_r, 32._pm_r, &
204.10_pm_r, 82209._pm_r,  21._pm_r, 13._pm_r, 1.06_pm_r,293._pm_r,   7._pm_r, 82._pm_r,  0.38_pm_r, 30._pm_r, &
196.20_pm_r, 85139._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.30_pm_r, 87947._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.40_pm_r, 90656._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
180.70_pm_r, 93326._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.90_pm_r, 96003._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.90_pm_r, 98736._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.50_pm_r,101589._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
209.30_pm_r,104652._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
237.10_pm_r,108082._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
289.50_pm_r,112189._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
371.30_pm_r,117473._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
279.10_pm_r,   -52._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
254.00_pm_r,  3847._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
229.80_pm_r,  7385._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.90_pm_r, 10667._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.70_pm_r, 13867._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.60_pm_r, 17071._pm_r,   8._pm_r,297._pm_r, 6.05_pm_r,209._pm_r,  13._pm_r, 54._pm_r, 1.90_pm_r,284._pm_r, &
218.20_pm_r, 20269._pm_r,  13._pm_r,244._pm_r, 7.59_pm_r,204._pm_r,  11._pm_r, 42._pm_r, 2.33_pm_r,281._pm_r, &
218.10_pm_r, 23463._pm_r,  22._pm_r,223._pm_r, 7.49_pm_r,194._pm_r,  10._pm_r, 25._pm_r, 2.18_pm_r,274._pm_r, &
218.70_pm_r, 26659._pm_r,  31._pm_r,212._pm_r, 5.86_pm_r,173._pm_r,   9._pm_r, 10._pm_r, 1.47_pm_r,257._pm_r, &
221.90_pm_r, 29882._pm_r,  35._pm_r,201._pm_r, 4.91_pm_r,126._pm_r,   8._pm_r,  2._pm_r,  0.96_pm_r,195._pm_r, &
231.00_pm_r, 33191._pm_r,  34._pm_r,188._pm_r, 6.77_pm_r, 85._pm_r,   6._pm_r,  7._pm_r, 1.66_pm_r,145._pm_r, &
242.40_pm_r, 36660._pm_r,  32._pm_r,168._pm_r, 8.79_pm_r, 66._pm_r,   5._pm_r, 35._pm_r, 2.27_pm_r,131._pm_r, &
253.90_pm_r, 40292._pm_r,  30._pm_r,143._pm_r, 9.44_pm_r, 56._pm_r,   6._pm_r, 70._pm_r, 2.31_pm_r,123._pm_r, &
262.50_pm_r, 44083._pm_r,  33._pm_r,121._pm_r, 7.62_pm_r, 48._pm_r,   8._pm_r, 85._pm_r, 1.19_pm_r,124._pm_r, &
264.20_pm_r, 47948._pm_r,  36._pm_r,105._pm_r, 5.62_pm_r, 33._pm_r,   8._pm_r, 90._pm_r,  0.25_pm_r,200._pm_r, &
258.20_pm_r, 51779._pm_r,  37._pm_r, 94._pm_r, 4.43_pm_r,  4._pm_r,   8._pm_r, 92._pm_r,  0.81_pm_r,256._pm_r, &
247.80_pm_r, 55490._pm_r,  36._pm_r, 85._pm_r, 4.45_pm_r,334._pm_r,   7._pm_r, 97._pm_r,  0.94_pm_r,235._pm_r, &
239.30_pm_r, 59050._pm_r,  33._pm_r, 75._pm_r, 4.39_pm_r,314._pm_r,   6._pm_r,105._pm_r,  0.65_pm_r,215._pm_r, &
234.20_pm_r, 62515._pm_r,  30._pm_r, 66._pm_r, 4.11_pm_r,299._pm_r,   6._pm_r,113._pm_r,  0.40_pm_r,183._pm_r, &
230.30_pm_r, 65915._pm_r,  26._pm_r, 57._pm_r, 3.57_pm_r,287._pm_r,   6._pm_r,116._pm_r,  0.26_pm_r,129._pm_r, &
227.00_pm_r, 69263._pm_r,  23._pm_r, 48._pm_r, 2.91_pm_r,279._pm_r,   7._pm_r,115._pm_r,  0.33_pm_r, 75._pm_r, &
224.10_pm_r, 72566._pm_r,  21._pm_r, 40._pm_r, 2.27_pm_r,271._pm_r,   7._pm_r,111._pm_r,  0.48_pm_r, 50._pm_r, &
219.10_pm_r, 75815._pm_r,  19._pm_r, 34._pm_r, 1.74_pm_r,263._pm_r,   7._pm_r,105._pm_r,  0.61_pm_r, 39._pm_r, &
212.60_pm_r, 78973._pm_r,  17._pm_r, 29._pm_r, 1.33_pm_r,255._pm_r,   8._pm_r, 98._pm_r,  0.67_pm_r, 33._pm_r, &
205.20_pm_r, 82042._pm_r,  16._pm_r, 25._pm_r, 1.01_pm_r,246._pm_r,   8._pm_r, 92._pm_r,  0.67_pm_r, 30._pm_r, &
195.60_pm_r, 84970._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.60_pm_r, 87756._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
180.10_pm_r, 90432._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.20_pm_r, 93063._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
180.20_pm_r, 95707._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.90_pm_r, 98425._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
196.70_pm_r,101291._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.20_pm_r,104404._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
243.50_pm_r,107921._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
292.00_pm_r,112102._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
370.70_pm_r,117382._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
268.60_pm_r,   -21._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
249.20_pm_r,  3763._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.50_pm_r,  7239._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.80_pm_r, 10493._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.30_pm_r, 13694._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.20_pm_r, 16906._pm_r,  10._pm_r,264._pm_r, 8.58_pm_r,216._pm_r,  20._pm_r, 37._pm_r, 1.77_pm_r,275._pm_r, &
218.80_pm_r, 20113._pm_r,  23._pm_r,235._pm_r,11.10_pm_r,212._pm_r,  19._pm_r, 29._pm_r, 2.27_pm_r,269._pm_r, &
218.50_pm_r, 23315._pm_r,  38._pm_r,224._pm_r,11.42_pm_r,205._pm_r,  17._pm_r, 20._pm_r, 2.34_pm_r,259._pm_r, &
218.10_pm_r, 26511._pm_r,  53._pm_r,217._pm_r, 9.13_pm_r,191._pm_r,  15._pm_r, 10._pm_r, 1.97_pm_r,240._pm_r, &
220.80_pm_r, 29722._pm_r,  61._pm_r,210._pm_r, 6.18_pm_r,153._pm_r,  13._pm_r,  4._pm_r, 1.64_pm_r,204._pm_r, &
227.90_pm_r, 33001._pm_r,  63._pm_r,202._pm_r, 7.24_pm_r, 99._pm_r,  11._pm_r,  5._pm_r, 1.92_pm_r,165._pm_r, &
237.60_pm_r, 36410._pm_r,  58._pm_r,191._pm_r,10.23_pm_r, 74._pm_r,   8._pm_r, 16._pm_r, 2.32_pm_r,143._pm_r, &
248.30_pm_r, 39965._pm_r,  51._pm_r,175._pm_r,11.80_pm_r, 63._pm_r,   7._pm_r, 40._pm_r, 2.41_pm_r,129._pm_r, &
258.20_pm_r, 43682._pm_r,  46._pm_r,157._pm_r,10.14_pm_r, 55._pm_r,   8._pm_r, 61._pm_r, 1.47_pm_r,130._pm_r, &
262.60_pm_r, 47502._pm_r,  44._pm_r,140._pm_r, 8.16_pm_r, 40._pm_r,   8._pm_r, 71._pm_r,  0.62_pm_r,174._pm_r, &
258.40_pm_r, 51324._pm_r,  41._pm_r,125._pm_r, 7.26_pm_r, 15._pm_r,   8._pm_r, 76._pm_r, 1.00_pm_r,229._pm_r, &
249.00_pm_r, 55045._pm_r,  36._pm_r,111._pm_r, 7.69_pm_r,352._pm_r,   6._pm_r, 84._pm_r, 1.37_pm_r,223._pm_r, &
240.20_pm_r, 58622._pm_r,  31._pm_r, 94._pm_r, 7.22_pm_r,336._pm_r,   5._pm_r, 98._pm_r,  0.86_pm_r,206._pm_r, &
233.80_pm_r, 62091._pm_r,  27._pm_r, 76._pm_r, 6.26_pm_r,322._pm_r,   5._pm_r,108._pm_r,  0.41_pm_r,166._pm_r, &
230.10_pm_r, 65485._pm_r,  23._pm_r, 59._pm_r, 5.03_pm_r,310._pm_r,   6._pm_r,110._pm_r,  0.41_pm_r, 91._pm_r, &
227.60_pm_r, 68836._pm_r,  22._pm_r, 43._pm_r, 3.82_pm_r,296._pm_r,   6._pm_r,106._pm_r,  0.67_pm_r, 58._pm_r, &
225.00_pm_r, 72150._pm_r,  20._pm_r, 30._pm_r, 2.87_pm_r,281._pm_r,   7._pm_r, 97._pm_r,  0.89_pm_r, 44._pm_r, &
221.40_pm_r, 75422._pm_r,  19._pm_r, 20._pm_r, 2.24_pm_r,262._pm_r,   8._pm_r, 88._pm_r, 1.06_pm_r, 37._pm_r, &
215.60_pm_r, 78625._pm_r,  17._pm_r, 12._pm_r, 1.89_pm_r,244._pm_r,   9._pm_r, 80._pm_r, 1.12_pm_r, 33._pm_r, &
206.90_pm_r, 81740._pm_r,  15._pm_r,  5._pm_r, 1.67_pm_r,229._pm_r,  10._pm_r, 73._pm_r, 1.09_pm_r, 31._pm_r, &
194.90_pm_r, 84669._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.60_pm_r, 87427._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
177.80_pm_r, 90067._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
176.10_pm_r, 92663._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
179.00_pm_r, 95280._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.30_pm_r, 97987._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.10_pm_r,100871._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.10_pm_r,104036._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
249.20_pm_r,107634._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
293.00_pm_r,111871._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
369.80_pm_r,117130._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
256.40_pm_r,    77._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
245.00_pm_r,  3739._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.60_pm_r,  7160._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.70_pm_r, 10383._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.70_pm_r, 13562._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.20_pm_r, 16748._pm_r,  14._pm_r,241._pm_r, 8.76_pm_r,214._pm_r,  19._pm_r, 30._pm_r, 1.26_pm_r,269._pm_r, &
217.10_pm_r, 19927._pm_r,  28._pm_r,227._pm_r,11.51_pm_r,212._pm_r,  18._pm_r, 24._pm_r, 1.64_pm_r,262._pm_r, &
218.40_pm_r, 23117._pm_r,  45._pm_r,220._pm_r,11.68_pm_r,208._pm_r,  16._pm_r, 17._pm_r, 1.75_pm_r,250._pm_r, &
219.30_pm_r, 26321._pm_r,  60._pm_r,216._pm_r, 8.98_pm_r,198._pm_r,  15._pm_r, 10._pm_r, 1.63_pm_r,230._pm_r, &
222.00_pm_r, 29550._pm_r,  69._pm_r,212._pm_r, 5.08_pm_r,167._pm_r,  13._pm_r,  6._pm_r, 1.47_pm_r,200._pm_r, &
227.60_pm_r, 32837._pm_r,  70._pm_r,207._pm_r, 5.23_pm_r, 97._pm_r,  11._pm_r,  6._pm_r, 1.57_pm_r,168._pm_r, &
234.70_pm_r, 36223._pm_r,  64._pm_r,200._pm_r, 8.71_pm_r, 68._pm_r,   9._pm_r, 15._pm_r, 1.76_pm_r,147._pm_r, &
242.70_pm_r, 39715._pm_r,  54._pm_r,189._pm_r,10.93_pm_r, 58._pm_r,   7._pm_r, 32._pm_r, 1.77_pm_r,132._pm_r, &
253.30_pm_r, 43352._pm_r,  45._pm_r,175._pm_r, 9.85_pm_r, 53._pm_r,   7._pm_r, 49._pm_r, 1.19_pm_r,130._pm_r, &
260.40_pm_r, 47117._pm_r,  39._pm_r,159._pm_r, 8.18_pm_r, 43._pm_r,   8._pm_r, 59._pm_r,  0.66_pm_r,138._pm_r, &
260.30_pm_r, 50938._pm_r,  33._pm_r,143._pm_r, 7.32_pm_r, 22._pm_r,   8._pm_r, 64._pm_r,  0.40_pm_r,165._pm_r, &
253.50_pm_r, 54709._pm_r,  27._pm_r,126._pm_r, 7.88_pm_r,359._pm_r,   7._pm_r, 69._pm_r,  0.43_pm_r,181._pm_r, &
243.30_pm_r, 58344._pm_r,  21._pm_r,104._pm_r, 7.44_pm_r,344._pm_r,   7._pm_r, 71._pm_r,  0.14_pm_r,156._pm_r, &
234.60_pm_r, 61843._pm_r,  17._pm_r, 75._pm_r, 6.51_pm_r,331._pm_r,   7._pm_r, 72._pm_r,  0.16_pm_r, 34._pm_r, &
229.00_pm_r, 65233._pm_r,  16._pm_r, 45._pm_r, 5.20_pm_r,319._pm_r,   8._pm_r, 70._pm_r,  0.33_pm_r, 19._pm_r, &
225.50_pm_r, 68561._pm_r,  17._pm_r, 22._pm_r, 3.85_pm_r,307._pm_r,   8._pm_r, 66._pm_r,  0.43_pm_r, 16._pm_r, &
222.30_pm_r, 71839._pm_r,  19._pm_r,  8._pm_r, 2.70_pm_r,293._pm_r,   8._pm_r, 63._pm_r,  0.48_pm_r, 18._pm_r, &
218.90_pm_r, 75071._pm_r,  19._pm_r,358._pm_r, 1.90_pm_r,273._pm_r,   9._pm_r, 60._pm_r,  0.51_pm_r, 20._pm_r, &
214.80_pm_r, 78251._pm_r,  19._pm_r,351._pm_r, 1.49_pm_r,250._pm_r,  10._pm_r, 57._pm_r,  0.50_pm_r, 22._pm_r, &
206.60_pm_r, 81367._pm_r,  19._pm_r,345._pm_r, 1.31_pm_r,229._pm_r,  10._pm_r, 55._pm_r,  0.46_pm_r, 24._pm_r, &
193.70_pm_r, 84281._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.60_pm_r, 87009._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
175.90_pm_r, 89619._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
174.60_pm_r, 92187._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.20_pm_r, 94786._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 97489._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.20_pm_r,100391._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.20_pm_r,103601._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.30_pm_r,107264._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
292.50_pm_r,111531._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
368.80_pm_r,116761._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
248.40_pm_r,   189._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
241.40_pm_r,  3764._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.30_pm_r,  7141._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.90_pm_r, 10332._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.00_pm_r, 13477._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
213.90_pm_r, 16619._pm_r,  12._pm_r,230._pm_r, 5.61_pm_r,210._pm_r,   8._pm_r, 23._pm_r,  0.50_pm_r,241._pm_r, &
214.30_pm_r, 19751._pm_r,  21._pm_r,221._pm_r, 7.41_pm_r,208._pm_r,   8._pm_r, 20._pm_r,  0.62_pm_r,237._pm_r, &
218.10_pm_r, 22919._pm_r,  32._pm_r,216._pm_r, 7.32_pm_r,207._pm_r,   7._pm_r, 15._pm_r,  0.63_pm_r,228._pm_r, &
220.60_pm_r, 26131._pm_r,  41._pm_r,214._pm_r, 5.32_pm_r,203._pm_r,   6._pm_r, 12._pm_r,  0.52_pm_r,210._pm_r, &
223.80_pm_r, 29385._pm_r,  47._pm_r,212._pm_r, 2.25_pm_r,184._pm_r,   6._pm_r, 12._pm_r,  0.40_pm_r,171._pm_r, &
228.60_pm_r, 32694._pm_r,  47._pm_r,210._pm_r, 2.00_pm_r, 70._pm_r,   5._pm_r, 17._pm_r,  0.48_pm_r,127._pm_r, &
233.70_pm_r, 36081._pm_r,  43._pm_r,207._pm_r, 4.67_pm_r, 49._pm_r,   5._pm_r, 26._pm_r,  0.63_pm_r,107._pm_r, &
239.20_pm_r, 39540._pm_r,  35._pm_r,203._pm_r, 6.29_pm_r, 44._pm_r,   5._pm_r, 36._pm_r,  0.68_pm_r, 98._pm_r, &
249.80_pm_r, 43123._pm_r,  27._pm_r,196._pm_r, 5.67_pm_r, 45._pm_r,   6._pm_r, 43._pm_r,  0.43_pm_r, 98._pm_r, &
258.80_pm_r, 46849._pm_r,  21._pm_r,186._pm_r, 4.60_pm_r, 39._pm_r,   6._pm_r, 46._pm_r,  0.27_pm_r, 90._pm_r, &
261.90_pm_r, 50671._pm_r,  15._pm_r,176._pm_r, 4.14_pm_r, 20._pm_r,   6._pm_r, 49._pm_r,  0.30_pm_r, 89._pm_r, &
257.10_pm_r, 54480._pm_r,   9._pm_r,167._pm_r, 4.83_pm_r,359._pm_r,   7._pm_r, 52._pm_r,  0.46_pm_r,101._pm_r, &
245.50_pm_r, 58160._pm_r,   2._pm_r,148._pm_r, 4.65_pm_r,349._pm_r,   7._pm_r, 56._pm_r,  0.49_pm_r,112._pm_r, &
234.80_pm_r, 61677._pm_r,   4._pm_r,353._pm_r, 4.00_pm_r,340._pm_r,   8._pm_r, 61._pm_r,  0.47_pm_r,124._pm_r, &
227.80_pm_r, 65059._pm_r,   9._pm_r,343._pm_r, 3.05_pm_r,331._pm_r,   8._pm_r, 65._pm_r,  0.41_pm_r,136._pm_r, &
223.30_pm_r, 68363._pm_r,  13._pm_r,339._pm_r, 2.08_pm_r,321._pm_r,   8._pm_r, 69._pm_r,  0.36_pm_r,148._pm_r, &
219.60_pm_r, 71605._pm_r,  15._pm_r,335._pm_r, 1.30_pm_r,304._pm_r,   8._pm_r, 73._pm_r,  0.32_pm_r,159._pm_r, &
216.30_pm_r, 74797._pm_r,  16._pm_r,331._pm_r,  0.83_pm_r,273._pm_r,   8._pm_r, 76._pm_r,  0.28_pm_r,172._pm_r, &
213.10_pm_r, 77944._pm_r,  17._pm_r,327._pm_r,  0.73_pm_r,236._pm_r,   8._pm_r, 79._pm_r,  0.26_pm_r,178._pm_r, &
205.50_pm_r, 81043._pm_r,  16._pm_r,324._pm_r,  0.80_pm_r,213._pm_r,   8._pm_r, 81._pm_r,  0.23_pm_r,187._pm_r, &
192.50_pm_r, 83938._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.20_pm_r, 86645._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
174.60_pm_r, 89234._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
173.70_pm_r, 91786._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
177.90_pm_r, 94374._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 97076._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.70_pm_r, 99991._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.80_pm_r,103233._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
255.70_pm_r,106934._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
291.60_pm_r,111214._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
368.10_pm_r,116417._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel),dimension(10*nb_lat*nb_alt),parameter::donnees_mars=&
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_mars,(/10,nb_alt,nb_lat/))


  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)

end subroutine cps_atmi_mars

subroutine cps_atmi_avril (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_avril
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de AVRIL
!
!$Description
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de AVRIL
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_avril (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments       
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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


  ! Modules
  ! =======

  use mslib


  ! Declarations
  ! ============
  implicit none

 real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel


  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
264.00_pm_r,  -196._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
243.00_pm_r,  3505._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.90_pm_r,  6898._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.40_pm_r, 10111._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.20_pm_r, 13298._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.90_pm_r, 16477._pm_r,  10._pm_r,244._pm_r, 1.22_pm_r,179._pm_r,   1._pm_r, 53._pm_r,  0.09_pm_r,  7._pm_r, &
212.60_pm_r, 19617._pm_r,  11._pm_r,234._pm_r, 1.64_pm_r,176._pm_r,   1._pm_r, 48._pm_r,  0.11_pm_r, 10._pm_r, &
208.00_pm_r, 22697._pm_r,  12._pm_r,223._pm_r, 1.75_pm_r,170._pm_r,   1._pm_r, 43._pm_r,  0.12_pm_r, 21._pm_r, &
202.80_pm_r, 25703._pm_r,  13._pm_r,215._pm_r, 1.29_pm_r,156._pm_r,   1._pm_r, 41._pm_r,  0.10_pm_r, 37._pm_r, &
207.50_pm_r, 28695._pm_r,  14._pm_r,209._pm_r,  0.73_pm_r,104._pm_r,   2._pm_r, 42._pm_r,  0.06_pm_r, 75._pm_r, &
212.70_pm_r, 31775._pm_r,  13._pm_r,206._pm_r, 1.24_pm_r, 43._pm_r,   2._pm_r, 44._pm_r,  0.07_pm_r,153._pm_r, &
220.10_pm_r, 34941._pm_r,  10._pm_r,204._pm_r, 1.94_pm_r, 26._pm_r,   1._pm_r, 49._pm_r,  0.13_pm_r,189._pm_r, &
231.30_pm_r, 38241._pm_r,   7._pm_r,205._pm_r, 2.23_pm_r, 19._pm_r,   1._pm_r, 54._pm_r,  0.18_pm_r,207._pm_r, &
243.10_pm_r, 41718._pm_r,   4._pm_r,205._pm_r, 1.88_pm_r, 33._pm_r,   1._pm_r, 59._pm_r,  0.11_pm_r,203._pm_r, &
254.40_pm_r, 45359._pm_r,   2._pm_r,183._pm_r, 1.76_pm_r, 47._pm_r,   1._pm_r, 66._pm_r,  0.11_pm_r,170._pm_r, &
258.90_pm_r, 49130._pm_r,   2._pm_r, 94._pm_r, 1.85_pm_r, 48._pm_r,   1._pm_r, 80._pm_r,  0.23_pm_r,172._pm_r, &
256.20_pm_r, 52905._pm_r,   4._pm_r, 64._pm_r, 1.95_pm_r, 36._pm_r,   1._pm_r,107._pm_r,  0.43_pm_r,185._pm_r, &
253.00_pm_r, 56633._pm_r,   7._pm_r, 49._pm_r, 1.69_pm_r, 15._pm_r,   1._pm_r,141._pm_r,  0.52_pm_r,199._pm_r, &
249.10_pm_r, 60313._pm_r,   8._pm_r, 38._pm_r, 1.49_pm_r,350._pm_r,   2._pm_r,164._pm_r,  0.55_pm_r,204._pm_r, &
242.60_pm_r, 63914._pm_r,  10._pm_r, 26._pm_r, 1.44_pm_r,324._pm_r,   2._pm_r,175._pm_r,  0.47_pm_r,202._pm_r, &
236.00_pm_r, 67421._pm_r,  10._pm_r, 15._pm_r, 1.56_pm_r,301._pm_r,   3._pm_r,180._pm_r,  0.36_pm_r,193._pm_r, &
231.40_pm_r, 70839._pm_r,  11._pm_r,  2._pm_r, 1.77_pm_r,285._pm_r,   3._pm_r,181._pm_r,  0.26_pm_r,178._pm_r, &
229.70_pm_r, 74213._pm_r,  12._pm_r,348._pm_r, 2.03_pm_r,275._pm_r,   4._pm_r,180._pm_r,  0.20_pm_r,153._pm_r, &
228.60_pm_r, 77573._pm_r,  13._pm_r,334._pm_r, 2.19_pm_r,269._pm_r,   4._pm_r,177._pm_r,  0.20_pm_r,130._pm_r, &
221.80_pm_r, 80914._pm_r,  14._pm_r,322._pm_r, 2.21_pm_r,265._pm_r,   4._pm_r,174._pm_r,  0.21_pm_r,113._pm_r, &
206.90_pm_r, 84034._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
193.00_pm_r, 86930._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.10_pm_r, 89675._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.30_pm_r, 92350._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.30_pm_r, 95034._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.60_pm_r, 97802._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.40_pm_r,100747._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.00_pm_r,103972._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
249.20_pm_r,107600._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
283.50_pm_r,111762._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
357.30_pm_r,116810._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
269.70_pm_r,  -156._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
246.40_pm_r,  3613._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.50_pm_r,  7051._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.00_pm_r, 10297._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.90_pm_r, 13509._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.60_pm_r, 16714._pm_r,  13._pm_r,238._pm_r, 1.39_pm_r,163._pm_r,   2._pm_r, 13._pm_r,  0.20_pm_r, 80._pm_r, &
214.70_pm_r, 19880._pm_r,  14._pm_r,229._pm_r, 1.80_pm_r,156._pm_r,   2._pm_r, 23._pm_r,  0.27_pm_r, 77._pm_r, &
211.70_pm_r, 23003._pm_r,  15._pm_r,218._pm_r, 1.88_pm_r,141._pm_r,   2._pm_r, 32._pm_r,  0.29_pm_r, 70._pm_r, &
208.10_pm_r, 26075._pm_r,  15._pm_r,208._pm_r, 1.68_pm_r,111._pm_r,   2._pm_r, 37._pm_r,  0.25_pm_r, 52._pm_r, &
210.50_pm_r, 29132._pm_r,  14._pm_r,199._pm_r, 1.95_pm_r, 66._pm_r,   3._pm_r, 37._pm_r,  0.21_pm_r, 12._pm_r, &
214.20_pm_r, 32242._pm_r,  11._pm_r,190._pm_r, 2.89_pm_r, 39._pm_r,   3._pm_r, 32._pm_r,  0.29_pm_r,333._pm_r, &
220.90_pm_r, 35425._pm_r,   7._pm_r,175._pm_r, 3.59_pm_r, 26._pm_r,   3._pm_r, 23._pm_r,  0.40_pm_r,316._pm_r, &
231.90_pm_r, 38734._pm_r,   3._pm_r,124._pm_r, 3.62_pm_r, 18._pm_r,   3._pm_r, 13._pm_r,  0.46_pm_r,309._pm_r, &
244.60_pm_r, 42228._pm_r,   5._pm_r, 60._pm_r, 2.82_pm_r, 27._pm_r,   4._pm_r,  5._pm_r,  0.29_pm_r,286._pm_r, &
255.00_pm_r, 45887._pm_r,   9._pm_r, 47._pm_r, 2.49_pm_r, 34._pm_r,   4._pm_r,359._pm_r,  0.24_pm_r,250._pm_r, &
257.20_pm_r, 49650._pm_r,  12._pm_r, 42._pm_r, 2.51_pm_r, 26._pm_r,   3._pm_r,354._pm_r,  0.18_pm_r,238._pm_r, &
252.30_pm_r, 53386._pm_r,  16._pm_r, 37._pm_r, 2.75_pm_r,  8._pm_r,   3._pm_r,352._pm_r,  0.03_pm_r,333._pm_r, &
248.50_pm_r, 57052._pm_r,  19._pm_r, 29._pm_r, 2.58_pm_r,345._pm_r,   4._pm_r,353._pm_r,  0.27_pm_r, 27._pm_r, &
244.00_pm_r, 60661._pm_r,  21._pm_r, 21._pm_r, 2.42_pm_r,324._pm_r,   4._pm_r,358._pm_r,  0.44_pm_r, 34._pm_r, &
238.40_pm_r, 64193._pm_r,  23._pm_r, 13._pm_r, 2.23_pm_r,307._pm_r,   5._pm_r,  4._pm_r,  0.51_pm_r, 39._pm_r, &
233.00_pm_r, 67646._pm_r,  24._pm_r,  6._pm_r, 2.06_pm_r,290._pm_r,   5._pm_r,  9._pm_r,  0.54_pm_r, 45._pm_r, &
229.30_pm_r, 71028._pm_r,  24._pm_r,359._pm_r, 1.99_pm_r,275._pm_r,   6._pm_r, 14._pm_r,  0.53_pm_r, 51._pm_r, &
227.20_pm_r, 74370._pm_r,  24._pm_r,352._pm_r, 1.99_pm_r,262._pm_r,   6._pm_r, 18._pm_r,  0.52_pm_r, 56._pm_r, &
225.40_pm_r, 77686._pm_r,  24._pm_r,345._pm_r, 1.99_pm_r,254._pm_r,   7._pm_r, 22._pm_r,  0.51_pm_r, 60._pm_r, &
218.90_pm_r, 80974._pm_r,  24._pm_r,338._pm_r, 1.92_pm_r,248._pm_r,   8._pm_r, 26._pm_r,  0.48_pm_r, 63._pm_r, &
205.60_pm_r, 84067._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.80_pm_r, 86958._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.20_pm_r, 89706._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.30_pm_r, 92384._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.10_pm_r, 95067._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.60_pm_r, 97827._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.60_pm_r,100754._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.30_pm_r,103946._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
247.10_pm_r,107537._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
284.60_pm_r,111690._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
358.60_pm_r,116770._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /) 

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
275.40_pm_r,  -133._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
252.20_pm_r,  3723._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
228.90_pm_r,  7238._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.40_pm_r, 10521._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.10_pm_r, 13740._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.40_pm_r, 16954._pm_r,  15._pm_r,225._pm_r, 1.11_pm_r,120._pm_r,   3._pm_r, 21._pm_r,  0.20_pm_r,122._pm_r, &
216.90_pm_r, 20139._pm_r,  14._pm_r,218._pm_r, 1.58_pm_r,107._pm_r,   3._pm_r, 28._pm_r,  0.25_pm_r,119._pm_r, &
217.40_pm_r, 23321._pm_r,  13._pm_r,208._pm_r, 2.09_pm_r, 89._pm_r,   3._pm_r, 37._pm_r,  0.24_pm_r,112._pm_r, &
215.10_pm_r, 26490._pm_r,  11._pm_r,193._pm_r, 2.67_pm_r, 69._pm_r,   3._pm_r, 43._pm_r,  0.16_pm_r, 90._pm_r, &
214.80_pm_r, 29633._pm_r,   9._pm_r,172._pm_r, 3.31_pm_r, 51._pm_r,   3._pm_r, 43._pm_r,  0.14_pm_r, 10._pm_r, &
219.20_pm_r, 32805._pm_r,   7._pm_r,135._pm_r, 3.83_pm_r, 35._pm_r,   3._pm_r, 38._pm_r,  0.33_pm_r,333._pm_r, &
226.60_pm_r, 36069._pm_r,   8._pm_r, 88._pm_r, 3.90_pm_r, 23._pm_r,   3._pm_r, 27._pm_r,  0.51_pm_r,322._pm_r, &
236.60_pm_r, 39455._pm_r,  11._pm_r, 59._pm_r, 3.51_pm_r, 12._pm_r,   4._pm_r, 15._pm_r,  0.62_pm_r,316._pm_r, &
248.00_pm_r, 43010._pm_r,  14._pm_r, 46._pm_r, 2.61_pm_r, 15._pm_r,   4._pm_r,  6._pm_r,  0.35_pm_r,301._pm_r, &
255.50_pm_r, 46700._pm_r,  17._pm_r, 40._pm_r, 2.22_pm_r, 14._pm_r,   4._pm_r,  2._pm_r,  0.11_pm_r,229._pm_r, &
254.40_pm_r, 50443._pm_r,  20._pm_r, 35._pm_r, 2.39_pm_r,360._pm_r,   4._pm_r,  2._pm_r,  0.21_pm_r,146._pm_r, &
247.90_pm_r, 54124._pm_r,  23._pm_r, 28._pm_r, 3.02_pm_r,341._pm_r,   4._pm_r,  7._pm_r,  0.28_pm_r,109._pm_r, &
243.60_pm_r, 57722._pm_r,  26._pm_r, 20._pm_r, 3.06_pm_r,323._pm_r,   4._pm_r, 12._pm_r,  0.23_pm_r, 48._pm_r, &
238.40_pm_r, 61254._pm_r,  28._pm_r, 12._pm_r, 2.84_pm_r,309._pm_r,   4._pm_r, 13._pm_r,  0.35_pm_r, 15._pm_r, &
233.70_pm_r, 64709._pm_r,  30._pm_r,  5._pm_r, 2.36_pm_r,296._pm_r,   5._pm_r, 13._pm_r,  0.43_pm_r,  5._pm_r, &
229.70_pm_r, 68103._pm_r,  31._pm_r,359._pm_r, 1.86_pm_r,281._pm_r,   5._pm_r, 12._pm_r,  0.46_pm_r,  1._pm_r, &
226.50_pm_r, 71442._pm_r,  31._pm_r,355._pm_r, 1.45_pm_r,262._pm_r,   6._pm_r, 10._pm_r,  0.46_pm_r,  1._pm_r, &
223.20_pm_r, 74736._pm_r,  30._pm_r,351._pm_r, 1.26_pm_r,240._pm_r,   7._pm_r, 10._pm_r,  0.44_pm_r,  3._pm_r, &
219.70_pm_r, 77977._pm_r,  29._pm_r,348._pm_r, 1.21_pm_r,222._pm_r,   7._pm_r,  9._pm_r,  0.42_pm_r,  5._pm_r, &
213.80_pm_r, 81170._pm_r,  28._pm_r,346._pm_r, 1.20_pm_r,209._pm_r,   8._pm_r,  9._pm_r,  0.38_pm_r,  6._pm_r, &
203.20_pm_r, 84214._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.50_pm_r, 87097._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.50_pm_r, 89848._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.40_pm_r, 92532._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.90_pm_r, 95217._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.40_pm_r, 97968._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.10_pm_r,100869._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
216.30_pm_r,104013._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
243.30_pm_r,107545._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
285.20_pm_r,111672._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
360.20_pm_r,116790._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
281.10_pm_r,   -78._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
259.60_pm_r,  3877._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
234.80_pm_r,  7493._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.80_pm_r, 10817._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.70_pm_r, 14017._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.30_pm_r, 17203._pm_r,  11._pm_r,217._pm_r, 1.00_pm_r, 94._pm_r,   2._pm_r, 34._pm_r,  0.05_pm_r,133._pm_r, &
217.50_pm_r, 20385._pm_r,  10._pm_r,209._pm_r, 1.48_pm_r, 80._pm_r,   2._pm_r, 36._pm_r,  0.03_pm_r,108._pm_r, &
219.60_pm_r, 23588._pm_r,   8._pm_r,196._pm_r, 2.01_pm_r, 64._pm_r,   2._pm_r, 36._pm_r,  0.06_pm_r,345._pm_r, &
219.50_pm_r, 26805._pm_r,   6._pm_r,174._pm_r, 2.62_pm_r, 49._pm_r,   2._pm_r, 32._pm_r,  0.17_pm_r,334._pm_r, &
220.70_pm_r, 30025._pm_r,   4._pm_r,128._pm_r, 3.17_pm_r, 34._pm_r,   3._pm_r, 25._pm_r,  0.31_pm_r,330._pm_r, &
225.90_pm_r, 33289._pm_r,   6._pm_r, 73._pm_r, 3.47_pm_r, 22._pm_r,   3._pm_r, 16._pm_r,  0.43_pm_r,328._pm_r, &
234.60_pm_r, 36661._pm_r,  10._pm_r, 47._pm_r, 3.30_pm_r, 10._pm_r,   3._pm_r,  8._pm_r,  0.49_pm_r,326._pm_r, &
244.90_pm_r, 40168._pm_r,  13._pm_r, 34._pm_r, 2.75_pm_r,357._pm_r,   4._pm_r,  0._pm_r,  0.49_pm_r,323._pm_r, &
254.40_pm_r, 43830._pm_r,  16._pm_r, 27._pm_r, 1.81_pm_r,359._pm_r,   4._pm_r,355._pm_r,  0.27_pm_r,295._pm_r, &
258.80_pm_r, 47593._pm_r,  18._pm_r, 23._pm_r, 1.37_pm_r,  1._pm_r,   4._pm_r,351._pm_r,  0.21_pm_r,217._pm_r, &
255.30_pm_r, 51364._pm_r,  20._pm_r, 21._pm_r, 1.40_pm_r,346._pm_r,   4._pm_r,350._pm_r,  0.30_pm_r,161._pm_r, &
247.70_pm_r, 55050._pm_r,  22._pm_r, 16._pm_r, 1.92_pm_r,323._pm_r,   4._pm_r,355._pm_r,  0.43_pm_r,111._pm_r, &
240.40_pm_r, 58623._pm_r,  23._pm_r,  9._pm_r, 2.21_pm_r,299._pm_r,   4._pm_r,  6._pm_r,  0.55_pm_r, 77._pm_r, &
232.80_pm_r, 62089._pm_r,  24._pm_r,  1._pm_r, 2.29_pm_r,284._pm_r,   4._pm_r, 17._pm_r,  0.64_pm_r, 62._pm_r, &
226.90_pm_r, 65451._pm_r,  24._pm_r,354._pm_r, 2.07_pm_r,274._pm_r,   5._pm_r, 24._pm_r,  0.60_pm_r, 56._pm_r, &
223.00_pm_r, 68746._pm_r,  25._pm_r,347._pm_r, 1.71_pm_r,263._pm_r,   6._pm_r, 29._pm_r,  0.50_pm_r, 52._pm_r, &
220.10_pm_r, 71990._pm_r,  25._pm_r,342._pm_r, 1.35_pm_r,250._pm_r,   6._pm_r, 31._pm_r,  0.37_pm_r, 50._pm_r, &
217.60_pm_r, 75196._pm_r,  25._pm_r,338._pm_r, 1.09_pm_r,236._pm_r,   7._pm_r, 32._pm_r,  0.26_pm_r, 49._pm_r, &
215.20_pm_r, 78367._pm_r,  24._pm_r,335._pm_r,  0.94_pm_r,220._pm_r,   7._pm_r, 33._pm_r,  0.17_pm_r, 49._pm_r, &
210.40_pm_r, 81501._pm_r,  24._pm_r,332._pm_r,  0.85_pm_r,208._pm_r,   7._pm_r, 33._pm_r,  0.12_pm_r, 43._pm_r, &
201.60_pm_r, 84513._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.10_pm_r, 87386._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.80_pm_r, 90138._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.00_pm_r, 92832._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.00_pm_r, 95524._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 98272._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
196.30_pm_r,101146._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
211.50_pm_r,104236._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
238.10_pm_r,107691._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
284.60_pm_r,111770._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
362.00_pm_r,116920._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /) 

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
288.90_pm_r,   -20._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
266.60_pm_r,  4046._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
240.70_pm_r,  7760._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.10_pm_r, 11133._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
213.50_pm_r, 14307._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
212.90_pm_r, 17425._pm_r,   4._pm_r,207._pm_r,  0.44_pm_r, 97._pm_r,   2._pm_r, 82._pm_r,  0.04_pm_r,247._pm_r, &
215.50_pm_r, 20559._pm_r,   4._pm_r,197._pm_r,  0.61_pm_r, 75._pm_r,   2._pm_r, 82._pm_r,  0.07_pm_r,278._pm_r, &
220.20_pm_r, 23753._pm_r,   3._pm_r,185._pm_r,  0.86_pm_r, 48._pm_r,   2._pm_r, 79._pm_r,  0.13_pm_r,299._pm_r, &
222.50_pm_r, 26996._pm_r,   2._pm_r,162._pm_r, 1.25_pm_r, 28._pm_r,   1._pm_r, 72._pm_r,  0.21_pm_r,311._pm_r, &
225.70_pm_r, 30276._pm_r,   1._pm_r, 89._pm_r, 1.66_pm_r, 14._pm_r,   1._pm_r, 57._pm_r,  0.31_pm_r,320._pm_r, &
232.40_pm_r, 33626._pm_r,   3._pm_r, 34._pm_r, 1.90_pm_r,  4._pm_r,   1._pm_r, 36._pm_r,  0.37_pm_r,326._pm_r, &
241.20_pm_r, 37094._pm_r,   6._pm_r, 17._pm_r, 1.82_pm_r,354._pm_r,   2._pm_r, 18._pm_r,  0.39_pm_r,332._pm_r, &
251.30_pm_r, 40697._pm_r,   8._pm_r,  9._pm_r, 1.45_pm_r,343._pm_r,   2._pm_r,  8._pm_r,  0.38_pm_r,338._pm_r, &
260.40_pm_r, 44450._pm_r,   9._pm_r,  5._pm_r,  0.77_pm_r,349._pm_r,   3._pm_r,  3._pm_r,  0.15_pm_r,336._pm_r, &
264.30_pm_r, 48299._pm_r,  10._pm_r,  4._pm_r,  0.42_pm_r,355._pm_r,   3._pm_r,  3._pm_r,  0.14_pm_r,144._pm_r, &
258.80_pm_r, 52136._pm_r,  11._pm_r,  3._pm_r,  0.45_pm_r,316._pm_r,   2._pm_r, 10._pm_r,  0.38_pm_r,132._pm_r, &
248.40_pm_r, 55854._pm_r,  11._pm_r,358._pm_r,  0.97_pm_r,290._pm_r,   2._pm_r, 28._pm_r,  0.54_pm_r,115._pm_r, &
239.20_pm_r, 59422._pm_r,  12._pm_r,350._pm_r, 1.36_pm_r,269._pm_r,   2._pm_r, 46._pm_r,  0.52_pm_r, 92._pm_r, &
231.30_pm_r, 62868._pm_r,  12._pm_r,340._pm_r, 1.51_pm_r,258._pm_r,   3._pm_r, 54._pm_r,  0.49_pm_r, 73._pm_r, &
224.60_pm_r, 66205._pm_r,  12._pm_r,329._pm_r, 1.39_pm_r,250._pm_r,   4._pm_r, 56._pm_r,  0.43_pm_r, 60._pm_r, &
219.40_pm_r, 69456._pm_r,  13._pm_r,321._pm_r, 1.18_pm_r,242._pm_r,   4._pm_r, 56._pm_r,  0.35_pm_r, 47._pm_r, &
216.10_pm_r, 72643._pm_r,  13._pm_r,314._pm_r,  0.95_pm_r,232._pm_r,   5._pm_r, 55._pm_r,  0.28_pm_r, 38._pm_r, &
213.70_pm_r, 75790._pm_r,  13._pm_r,308._pm_r,  0.77_pm_r,220._pm_r,   5._pm_r, 53._pm_r,  0.23_pm_r, 23._pm_r, &
211.50_pm_r, 78902._pm_r,  13._pm_r,304._pm_r,  0.67_pm_r,208._pm_r,   5._pm_r, 51._pm_r,  0.20_pm_r, 10._pm_r, &
207.60_pm_r, 81984._pm_r,  13._pm_r,300._pm_r,  0.59_pm_r,197._pm_r,   6._pm_r, 49._pm_r,  0.17_pm_r,  0._pm_r, &
200.00_pm_r, 84966._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.60_pm_r, 87826._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.20_pm_r, 90580._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.00_pm_r, 93287._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.50_pm_r, 95994._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.50_pm_r, 98745._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
193.80_pm_r,101597._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
206.80_pm_r,104635._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
232.40_pm_r,108010._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
282.70_pm_r,112026._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
363.40_pm_r,117191._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
295.00_pm_r,   -28._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
271.90_pm_r,  4125._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
246.40_pm_r,  7922._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.50_pm_r, 11358._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.30_pm_r, 14514._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
206.90_pm_r, 17539._pm_r,   3._pm_r,178._pm_r,  0.17_pm_r,157._pm_r,   1._pm_r, 99._pm_r,  0.08_pm_r,103._pm_r, &
212.60_pm_r, 20608._pm_r,   3._pm_r,176._pm_r,  0.12_pm_r,158._pm_r,   1._pm_r, 99._pm_r,  0.09_pm_r,103._pm_r, &
219.70_pm_r, 23778._pm_r,   3._pm_r,176._pm_r,  0.04_pm_r,328._pm_r,   1._pm_r, 99._pm_r,  0.06_pm_r, 99._pm_r, &
224.50_pm_r, 27031._pm_r,   3._pm_r,178._pm_r,  0.26_pm_r,333._pm_r,   1._pm_r, 99._pm_r,  0.02_pm_r, 63._pm_r, &
229.30_pm_r, 30353._pm_r,   2._pm_r,185._pm_r,  0.50_pm_r,331._pm_r,   1._pm_r, 97._pm_r,  0.05_pm_r,323._pm_r, &
236.90_pm_r, 33761._pm_r,   2._pm_r,204._pm_r,  0.67_pm_r,328._pm_r,   1._pm_r, 92._pm_r,  0.12_pm_r,310._pm_r, &
245.90_pm_r, 37297._pm_r,   1._pm_r,244._pm_r,  0.69_pm_r,323._pm_r,   1._pm_r, 83._pm_r,  0.16_pm_r,311._pm_r, &
255.70_pm_r, 40966._pm_r,   2._pm_r,274._pm_r,  0.57_pm_r,313._pm_r,   1._pm_r, 66._pm_r,  0.19_pm_r,316._pm_r, &
264.00_pm_r, 44779._pm_r,   2._pm_r,285._pm_r,  0.27_pm_r,329._pm_r,   1._pm_r, 47._pm_r,  0.13_pm_r,283._pm_r, &
266.80_pm_r, 48672._pm_r,   3._pm_r,290._pm_r,  0.20_pm_r,322._pm_r,   0._pm_r, 34._pm_r,  0.16_pm_r,236._pm_r, &
260.20_pm_r, 52537._pm_r,   3._pm_r,291._pm_r,  0.36_pm_r,283._pm_r,   0._pm_r, 18._pm_r,  0.15_pm_r,222._pm_r, &
248.60_pm_r, 56269._pm_r,   4._pm_r,288._pm_r,  0.67_pm_r,269._pm_r,   0._pm_r,349._pm_r,  0.01_pm_r, 63._pm_r, &
239.40_pm_r, 59836._pm_r,   5._pm_r,281._pm_r,  0.77_pm_r,251._pm_r,   0._pm_r, 19._pm_r,  0.26_pm_r, 32._pm_r, &
232.50_pm_r, 63293._pm_r,   6._pm_r,274._pm_r,  0.74_pm_r,239._pm_r,   1._pm_r, 28._pm_r,  0.41_pm_r, 33._pm_r, &
224.40_pm_r, 66639._pm_r,   6._pm_r,268._pm_r,  0.62_pm_r,230._pm_r,   1._pm_r, 31._pm_r,  0.43_pm_r, 38._pm_r, &
217.20_pm_r, 69872._pm_r,   7._pm_r,264._pm_r,  0.47_pm_r,219._pm_r,   2._pm_r, 34._pm_r,  0.39_pm_r, 43._pm_r, &
213.30_pm_r, 73022._pm_r,   7._pm_r,260._pm_r,  0.32_pm_r,206._pm_r,   3._pm_r, 37._pm_r,  0.34_pm_r, 52._pm_r, &
210.50_pm_r, 76125._pm_r,   7._pm_r,258._pm_r,  0.23_pm_r,186._pm_r,   3._pm_r, 40._pm_r,  0.29_pm_r, 61._pm_r, &
208.20_pm_r, 79188._pm_r,   8._pm_r,255._pm_r,  0.19_pm_r,158._pm_r,   3._pm_r, 43._pm_r,  0.25_pm_r, 70._pm_r, &
204.70_pm_r, 82221._pm_r,   7._pm_r,253._pm_r,  0.18_pm_r,136._pm_r,   4._pm_r, 46._pm_r,  0.22_pm_r, 77._pm_r, &
198.30_pm_r, 85174._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.10_pm_r, 88020._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.80_pm_r, 90776._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.30_pm_r, 93501._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.50_pm_r, 96227._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.50_pm_r, 98988._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.90_pm_r,101828._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.10_pm_r,104827._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
227.20_pm_r,108132._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
280.00_pm_r,112083._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
364.60_pm_r,117251._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
299.00_pm_r,   -61._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
275.70_pm_r,  4152._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
251.30_pm_r,  8016._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.10_pm_r, 11509._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.60_pm_r, 14652._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.20_pm_r, 17589._pm_r,   3._pm_r,174._pm_r,  0.08_pm_r,247._pm_r,   0._pm_r,344._pm_r,  0.11_pm_r,105._pm_r, &
210.00_pm_r, 20597._pm_r,   3._pm_r,177._pm_r,  0.13_pm_r,252._pm_r,   0._pm_r, 12._pm_r,  0.15_pm_r,110._pm_r, &
219.10_pm_r, 23744._pm_r,   3._pm_r,182._pm_r,  0.19_pm_r,253._pm_r,   0._pm_r, 48._pm_r,  0.16_pm_r,112._pm_r, &
225.50_pm_r, 26999._pm_r,   3._pm_r,188._pm_r,  0.27_pm_r,255._pm_r,   1._pm_r, 70._pm_r,  0.13_pm_r,117._pm_r, &
231.80_pm_r, 30348._pm_r,   3._pm_r,195._pm_r,  0.35_pm_r,254._pm_r,   1._pm_r, 82._pm_r,  0.09_pm_r,133._pm_r, &
240.40_pm_r, 33801._pm_r,   4._pm_r,203._pm_r,  0.41_pm_r,252._pm_r,   1._pm_r, 89._pm_r,  0.04_pm_r,180._pm_r, &
250.50_pm_r, 37398._pm_r,   4._pm_r,210._pm_r,  0.46_pm_r,247._pm_r,   1._pm_r, 93._pm_r,  0.07_pm_r,257._pm_r, &
260.70_pm_r, 41140._pm_r,   5._pm_r,215._pm_r,  0.49_pm_r,242._pm_r,   1._pm_r, 94._pm_r,  0.11_pm_r,275._pm_r, &
267.40_pm_r, 45014._pm_r,   5._pm_r,217._pm_r,  0.35_pm_r,231._pm_r,   0._pm_r, 96._pm_r,  0.15_pm_r,270._pm_r, &
267.80_pm_r, 48940._pm_r,   6._pm_r,218._pm_r,  0.32_pm_r,234._pm_r,   0._pm_r,127._pm_r,  0.19_pm_r,267._pm_r, &
261.30_pm_r, 52818._pm_r,   6._pm_r,220._pm_r,  0.36_pm_r,256._pm_r,   0._pm_r,264._pm_r,  0.24_pm_r,275._pm_r, &
250.30_pm_r, 56571._pm_r,   7._pm_r,224._pm_r,  0.46_pm_r,277._pm_r,   1._pm_r,274._pm_r,  0.28_pm_r,288._pm_r, &
239.70_pm_r, 60155._pm_r,   7._pm_r,229._pm_r,  0.43_pm_r,277._pm_r,   1._pm_r,283._pm_r,  0.29_pm_r,306._pm_r, &
230.40_pm_r, 63600._pm_r,   7._pm_r,232._pm_r,  0.29_pm_r,260._pm_r,   1._pm_r,292._pm_r,  0.26_pm_r,320._pm_r, &
221.10_pm_r, 66903._pm_r,   8._pm_r,232._pm_r,  0.23_pm_r,208._pm_r,   2._pm_r,299._pm_r,  0.22_pm_r,340._pm_r, &
214.20_pm_r, 70089._pm_r,   8._pm_r,230._pm_r,  0.38_pm_r,166._pm_r,   2._pm_r,306._pm_r,  0.19_pm_r,357._pm_r, &
210.50_pm_r, 73195._pm_r,   8._pm_r,225._pm_r,  0.58_pm_r,155._pm_r,   2._pm_r,313._pm_r,  0.17_pm_r, 24._pm_r, &
208.10_pm_r, 76260._pm_r,   9._pm_r,219._pm_r,  0.73_pm_r,151._pm_r,   2._pm_r,320._pm_r,  0.18_pm_r, 43._pm_r, &
207.40_pm_r, 79298._pm_r,   9._pm_r,212._pm_r,  0.83_pm_r,149._pm_r,   2._pm_r,327._pm_r,  0.19_pm_r, 53._pm_r, &
204.60_pm_r, 82332._pm_r,  10._pm_r,205._pm_r,  0.85_pm_r,147._pm_r,   2._pm_r,334._pm_r,  0.18_pm_r, 61._pm_r, &
197.80_pm_r, 85277._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.70_pm_r, 88112._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.50_pm_r, 90874._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.70_pm_r, 93616._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.70_pm_r, 96365._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.00_pm_r, 99141._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.80_pm_r,101978._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.80_pm_r,104953._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.60_pm_r,108213._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.60_pm_r,112117._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
365.70_pm_r,117283._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /) 


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
301.30_pm_r,   -99._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
276.90_pm_r,  4142._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.60_pm_r,  8034._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.30_pm_r, 11554._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.60_pm_r, 14685._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.00_pm_r, 17570._pm_r,   3._pm_r,165._pm_r,  0.19_pm_r,312._pm_r,   1._pm_r,289._pm_r,  0.16_pm_r,101._pm_r, &
208.70_pm_r, 20546._pm_r,   2._pm_r,170._pm_r,  0.24_pm_r,302._pm_r,   0._pm_r,296._pm_r,  0.21_pm_r,102._pm_r, &
218.30_pm_r, 23678._pm_r,   2._pm_r,177._pm_r,  0.26_pm_r,285._pm_r,   0._pm_r, 58._pm_r,  0.22_pm_r,103._pm_r, &
225.20_pm_r, 26925._pm_r,   2._pm_r,187._pm_r,  0.29_pm_r,263._pm_r,   0._pm_r, 94._pm_r,  0.20_pm_r,105._pm_r, &
232.90_pm_r, 30278._pm_r,   3._pm_r,197._pm_r,  0.35_pm_r,240._pm_r,   1._pm_r, 99._pm_r,  0.14_pm_r,109._pm_r, &
243.50_pm_r, 33761._pm_r,   3._pm_r,203._pm_r,  0.43_pm_r,225._pm_r,   1._pm_r,101._pm_r,  0.06_pm_r,128._pm_r, &
254.40_pm_r, 37411._pm_r,   4._pm_r,207._pm_r,  0.52_pm_r,218._pm_r,   1._pm_r,104._pm_r,  0.05_pm_r,241._pm_r, &
264.10_pm_r, 41206._pm_r,   4._pm_r,208._pm_r,  0.56_pm_r,214._pm_r,   1._pm_r,110._pm_r,  0.13_pm_r,261._pm_r, &
269.80_pm_r, 45123._pm_r,   5._pm_r,209._pm_r,  0.43_pm_r,224._pm_r,   1._pm_r,125._pm_r,  0.19_pm_r,254._pm_r, &
269.70_pm_r, 49079._pm_r,   6._pm_r,212._pm_r,  0.30_pm_r,261._pm_r,   0._pm_r,161._pm_r,  0.19_pm_r,243._pm_r, &
263.00_pm_r, 52984._pm_r,   6._pm_r,216._pm_r,  0.27_pm_r,304._pm_r,   1._pm_r,187._pm_r,  0.10_pm_r,237._pm_r, &
251.40_pm_r, 56758._pm_r,   6._pm_r,219._pm_r,  0.20_pm_r,345._pm_r,   1._pm_r,191._pm_r,  0.05_pm_r, 72._pm_r, &
239.00_pm_r, 60346._pm_r,   6._pm_r,219._pm_r,  0.11_pm_r,107._pm_r,   0._pm_r,183._pm_r,  0.09_pm_r, 32._pm_r, &
227.50_pm_r, 63764._pm_r,   6._pm_r,216._pm_r,  0.30_pm_r,136._pm_r,   0._pm_r,177._pm_r,  0.08_pm_r,  0._pm_r, &
217.90_pm_r, 67021._pm_r,   6._pm_r,211._pm_r,  0.40_pm_r,142._pm_r,   0._pm_r,190._pm_r,  0.09_pm_r,286._pm_r, &
211.70_pm_r, 70164._pm_r,   6._pm_r,206._pm_r,  0.43_pm_r,146._pm_r,   0._pm_r,224._pm_r,  0.19_pm_r,253._pm_r, &
209.70_pm_r, 73247._pm_r,   6._pm_r,201._pm_r,  0.42_pm_r,149._pm_r,   1._pm_r,236._pm_r,  0.31_pm_r,244._pm_r, &
209.60_pm_r, 76316._pm_r,   7._pm_r,197._pm_r,  0.41_pm_r,151._pm_r,   1._pm_r,239._pm_r,  0.39_pm_r,241._pm_r, &
211.00_pm_r, 79395._pm_r,   7._pm_r,193._pm_r,  0.40_pm_r,153._pm_r,   2._pm_r,239._pm_r,  0.46_pm_r,239._pm_r, &
208.00_pm_r, 82497._pm_r,   8._pm_r,191._pm_r,  0.37_pm_r,154._pm_r,   3._pm_r,239._pm_r,  0.48_pm_r,238._pm_r, &
198.70_pm_r, 85462._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.70_pm_r, 88293._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.20_pm_r, 91062._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 93819._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 96586._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.60_pm_r, 99376._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.70_pm_r,102219._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.00_pm_r,105188._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.10_pm_r,108434._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
276.70_pm_r,112319._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
367.00_pm_r,117490._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
301.60_pm_r,  -126._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.10_pm_r,  4120._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
254.20_pm_r,  8017._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.90_pm_r, 11547._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.20_pm_r, 14680._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.10_pm_r, 17558._pm_r,   2._pm_r,153._pm_r,  0.13_pm_r,299._pm_r,   1._pm_r,288._pm_r,  0.16_pm_r, 94._pm_r, &
207.90_pm_r, 20526._pm_r,   2._pm_r,156._pm_r,  0.17_pm_r,287._pm_r,   0._pm_r,298._pm_r,  0.20_pm_r, 96._pm_r, &
217.90_pm_r, 23646._pm_r,   2._pm_r,163._pm_r,  0.20_pm_r,269._pm_r,   0._pm_r,350._pm_r,  0.20_pm_r, 96._pm_r, &
225.00_pm_r, 26882._pm_r,   2._pm_r,174._pm_r,  0.26_pm_r,249._pm_r,   0._pm_r, 69._pm_r,  0.16_pm_r,101._pm_r, &
232.40_pm_r, 30225._pm_r,   2._pm_r,188._pm_r,  0.32_pm_r,234._pm_r,   0._pm_r, 86._pm_r,  0.10_pm_r,110._pm_r, &
244.50_pm_r, 33711._pm_r,   2._pm_r,198._pm_r,  0.38_pm_r,223._pm_r,   0._pm_r, 96._pm_r,  0.04_pm_r,166._pm_r, &
255.90_pm_r, 37378._pm_r,   3._pm_r,203._pm_r,  0.42_pm_r,216._pm_r,   0._pm_r,108._pm_r,  0.09_pm_r,238._pm_r, &
265.20_pm_r, 41190._pm_r,   3._pm_r,205._pm_r,  0.44_pm_r,211._pm_r,   0._pm_r,135._pm_r,  0.16_pm_r,248._pm_r, &
270.60_pm_r, 45118._pm_r,   4._pm_r,206._pm_r,  0.38_pm_r,219._pm_r,   0._pm_r,182._pm_r,  0.22_pm_r,249._pm_r, &
270.20_pm_r, 49084._pm_r,   5._pm_r,208._pm_r,  0.30_pm_r,244._pm_r,   1._pm_r,206._pm_r,  0.19_pm_r,245._pm_r, &
263.90_pm_r, 52996._pm_r,   5._pm_r,211._pm_r,  0.28_pm_r,275._pm_r,   1._pm_r,209._pm_r,  0.04_pm_r,210._pm_r, &
252.50_pm_r, 56778._pm_r,   5._pm_r,215._pm_r,  0.25_pm_r,307._pm_r,   1._pm_r,196._pm_r,  0.14_pm_r, 98._pm_r, &
239.50_pm_r, 60371._pm_r,   5._pm_r,217._pm_r,  0.18_pm_r,328._pm_r,   1._pm_r,178._pm_r,  0.11_pm_r,120._pm_r, &
226.90_pm_r, 63789._pm_r,   5._pm_r,217._pm_r,  0.18_pm_r,  0._pm_r,   1._pm_r,176._pm_r,  0.14_pm_r,201._pm_r, &
216.60_pm_r, 67039._pm_r,   4._pm_r,217._pm_r,  0.24_pm_r,  8._pm_r,   1._pm_r,191._pm_r,  0.34_pm_r,230._pm_r, &
210.10_pm_r, 70172._pm_r,   4._pm_r,217._pm_r,  0.30_pm_r,  9._pm_r,   2._pm_r,207._pm_r,  0.54_pm_r,237._pm_r, &
208.40_pm_r, 73242._pm_r,   4._pm_r,219._pm_r,  0.37_pm_r,  9._pm_r,   2._pm_r,218._pm_r,  0.72_pm_r,241._pm_r, &
209.40_pm_r, 76303._pm_r,   3._pm_r,222._pm_r,  0.43_pm_r,  5._pm_r,   3._pm_r,225._pm_r,  0.89_pm_r,242._pm_r, &
212.00_pm_r, 79388._pm_r,   3._pm_r,227._pm_r,  0.47_pm_r,  5._pm_r,   5._pm_r,229._pm_r, 1.00_pm_r,243._pm_r, &
209.10_pm_r, 82503._pm_r,   3._pm_r,233._pm_r,  0.48_pm_r,  5._pm_r,   6._pm_r,232._pm_r, 1.03_pm_r,243._pm_r, &
198.90_pm_r, 85472._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.70_pm_r, 88301._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.60_pm_r, 91074._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 93838._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 96613._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.10_pm_r, 99411._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.30_pm_r,102262._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.80_pm_r,105241._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.00_pm_r,108501._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.90_pm_r,112403._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
368.60_pm_r,117596._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
301.00_pm_r,  -114._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.20_pm_r,  4127._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.30_pm_r,  8018._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.30_pm_r, 11536._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.00_pm_r, 14670._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.30_pm_r, 17560._pm_r,   2._pm_r,156._pm_r,  0.26_pm_r,342._pm_r,   0._pm_r,298._pm_r,  0.11_pm_r, 52._pm_r, &
208.40_pm_r, 20537._pm_r,   2._pm_r,156._pm_r,  0.30_pm_r,334._pm_r,   0._pm_r, 19._pm_r,  0.12_pm_r, 48._pm_r, &
217.30_pm_r, 23658._pm_r,   2._pm_r,158._pm_r,  0.26_pm_r,315._pm_r,   0._pm_r, 32._pm_r,  0.10_pm_r, 41._pm_r, &
224.30_pm_r, 26891._pm_r,   1._pm_r,168._pm_r,  0.25_pm_r,279._pm_r,   0._pm_r, 32._pm_r,  0.06_pm_r, 24._pm_r, &
231.60_pm_r, 30228._pm_r,   1._pm_r,186._pm_r,  0.33_pm_r,243._pm_r,   1._pm_r, 28._pm_r,  0.04_pm_r,300._pm_r, &
244.10_pm_r, 33705._pm_r,   2._pm_r,199._pm_r,  0.45_pm_r,225._pm_r,   0._pm_r, 17._pm_r,  0.09_pm_r,257._pm_r, &
255.30_pm_r, 37368._pm_r,   3._pm_r,205._pm_r,  0.54_pm_r,216._pm_r,   0._pm_r,357._pm_r,  0.13_pm_r,243._pm_r, &
264.50_pm_r, 41173._pm_r,   3._pm_r,207._pm_r,  0.56_pm_r,212._pm_r,   0._pm_r,321._pm_r,  0.15_pm_r,234._pm_r, &
270.10_pm_r, 45094._pm_r,   4._pm_r,208._pm_r,  0.41_pm_r,215._pm_r,   0._pm_r,285._pm_r,  0.17_pm_r,236._pm_r, &
269.80_pm_r, 49055._pm_r,   5._pm_r,210._pm_r,  0.25_pm_r,242._pm_r,   1._pm_r,272._pm_r,  0.11_pm_r,265._pm_r, &
262.30_pm_r, 52956._pm_r,   5._pm_r,213._pm_r,  0.21_pm_r,279._pm_r,   1._pm_r,280._pm_r,  0.15_pm_r,  6._pm_r, &
250.70_pm_r, 56719._pm_r,   5._pm_r,216._pm_r,  0.20_pm_r,280._pm_r,   1._pm_r,309._pm_r,  0.34_pm_r, 32._pm_r, &
239.00_pm_r, 60301._pm_r,   5._pm_r,218._pm_r,  0.15_pm_r,244._pm_r,   1._pm_r,346._pm_r,  0.32_pm_r, 49._pm_r, &
228.40_pm_r, 63726._pm_r,   5._pm_r,219._pm_r,  0.13_pm_r,214._pm_r,   1._pm_r,  6._pm_r,  0.19_pm_r, 78._pm_r, &
219.00_pm_r, 66996._pm_r,   5._pm_r,218._pm_r,  0.08_pm_r,191._pm_r,   1._pm_r, 18._pm_r,  0.18_pm_r,164._pm_r, &
211.90_pm_r, 70150._pm_r,   5._pm_r,218._pm_r,  0.02_pm_r,117._pm_r,   1._pm_r, 27._pm_r,  0.40_pm_r,196._pm_r, &
208.50_pm_r, 73223._pm_r,   5._pm_r,218._pm_r,  0.08_pm_r, 36._pm_r,   0._pm_r,189._pm_r,  0.61_pm_r,205._pm_r, &
208.30_pm_r, 76272._pm_r,   5._pm_r,218._pm_r,  0.15_pm_r, 27._pm_r,   1._pm_r,204._pm_r,  0.80_pm_r,208._pm_r, &
209.30_pm_r, 79331._pm_r,   5._pm_r,219._pm_r,  0.19_pm_r, 25._pm_r,   3._pm_r,207._pm_r,  0.91_pm_r,210._pm_r, &
206.10_pm_r, 82404._pm_r,   5._pm_r,220._pm_r,  0.21_pm_r, 22._pm_r,   4._pm_r,208._pm_r,  0.95_pm_r,212._pm_r, &
197.40_pm_r, 85347._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.30_pm_r, 88169._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.10_pm_r, 90936._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.60_pm_r, 93690._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 96455._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.10_pm_r, 99248._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.30_pm_r,102106._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.80_pm_r,105108._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.20_pm_r,108407._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
281.30_pm_r,112361._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
370.80_pm_r,117597._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
298.30_pm_r,  -131._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
275.60_pm_r,  4076._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
250.30_pm_r,  7932._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.80_pm_r, 11415._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.40_pm_r, 14562._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.10_pm_r, 17513._pm_r,   3._pm_r,148._pm_r,  0.38_pm_r,303._pm_r,   1._pm_r,200._pm_r,  0.28_pm_r, 41._pm_r, &
210.10_pm_r, 20529._pm_r,   2._pm_r,155._pm_r,  0.49_pm_r,298._pm_r,   0._pm_r,157._pm_r,  0.36_pm_r, 38._pm_r, &
218.00_pm_r, 23666._pm_r,   2._pm_r,170._pm_r,  0.51_pm_r,291._pm_r,   1._pm_r, 65._pm_r,  0.35_pm_r, 35._pm_r, &
224.80_pm_r, 26907._pm_r,   2._pm_r,193._pm_r,  0.49_pm_r,279._pm_r,   1._pm_r, 47._pm_r,  0.31_pm_r, 27._pm_r, &
232.80_pm_r, 30258._pm_r,   2._pm_r,212._pm_r,  0.43_pm_r,261._pm_r,   1._pm_r, 40._pm_r,  0.21_pm_r, 15._pm_r, &
243.00_pm_r, 33737._pm_r,   3._pm_r,221._pm_r,  0.40_pm_r,238._pm_r,   2._pm_r, 34._pm_r,  0.11_pm_r,345._pm_r, &
253.30_pm_r, 37375._pm_r,   3._pm_r,222._pm_r,  0.41_pm_r,217._pm_r,   2._pm_r, 29._pm_r,  0.10_pm_r,276._pm_r, &
263.00_pm_r, 41154._pm_r,   4._pm_r,220._pm_r,  0.42_pm_r,204._pm_r,   1._pm_r, 24._pm_r,  0.14_pm_r,236._pm_r, &
269.40_pm_r, 45061._pm_r,   4._pm_r,218._pm_r,  0.32_pm_r,204._pm_r,   1._pm_r, 20._pm_r,  0.12_pm_r,227._pm_r, &
269.10_pm_r, 49012._pm_r,   5._pm_r,217._pm_r,  0.16_pm_r,227._pm_r,   1._pm_r, 17._pm_r,  0.04_pm_r,256._pm_r, &
260.10_pm_r, 52891._pm_r,   5._pm_r,219._pm_r,  0.13_pm_r,299._pm_r,   1._pm_r, 16._pm_r,  0.11_pm_r, 18._pm_r, &
247.90_pm_r, 56616._pm_r,   5._pm_r,221._pm_r,  0.17_pm_r,339._pm_r,   1._pm_r, 17._pm_r,  0.18_pm_r, 29._pm_r, &
238.20_pm_r, 60169._pm_r,   5._pm_r,223._pm_r,  0.10_pm_r,348._pm_r,   2._pm_r, 21._pm_r,  0.10_pm_r, 72._pm_r, &
230.90_pm_r, 63605._pm_r,   4._pm_r,224._pm_r,  0.04_pm_r, 45._pm_r,   2._pm_r, 26._pm_r,  0.16_pm_r,158._pm_r, &
224.00_pm_r, 66934._pm_r,   4._pm_r,223._pm_r,  0.07_pm_r,111._pm_r,   1._pm_r, 35._pm_r,  0.32_pm_r,178._pm_r, &
217.10_pm_r, 70166._pm_r,   4._pm_r,222._pm_r,  0.10_pm_r,114._pm_r,   1._pm_r, 57._pm_r,  0.46_pm_r,186._pm_r, &
211.80_pm_r, 73303._pm_r,   4._pm_r,220._pm_r,  0.11_pm_r,111._pm_r,   1._pm_r,119._pm_r,  0.58_pm_r,189._pm_r, &
209.00_pm_r, 76382._pm_r,   4._pm_r,218._pm_r,  0.13_pm_r,113._pm_r,   1._pm_r,161._pm_r,  0.66_pm_r,191._pm_r, &
208.40_pm_r, 79438._pm_r,   4._pm_r,215._pm_r,  0.13_pm_r,112._pm_r,   2._pm_r,175._pm_r,  0.70_pm_r,193._pm_r, &
204.50_pm_r, 82488._pm_r,   4._pm_r,213._pm_r,  0.13_pm_r,113._pm_r,   3._pm_r,181._pm_r,  0.70_pm_r,194._pm_r, &
195.90_pm_r, 85408._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.70_pm_r, 88209._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.10_pm_r, 90949._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.40_pm_r, 93670._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.00_pm_r, 96403._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.60_pm_r, 99178._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
193.60_pm_r,102040._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
206.00_pm_r,105074._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
231.30_pm_r,108436._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
286.30_pm_r,112472._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
373.20_pm_r,117762._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
       
293.00_pm_r,   -96._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
270.70_pm_r,  4034._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
244.80_pm_r,  7811._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.50_pm_r, 11228._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
209.60_pm_r, 14386._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.20_pm_r, 17433._pm_r,   6._pm_r,166._pm_r,  0.73_pm_r,318._pm_r,   1._pm_r,173._pm_r,  0.43_pm_r, 36._pm_r, &
212.40_pm_r, 20510._pm_r,   4._pm_r,174._pm_r,  0.97_pm_r,318._pm_r,   1._pm_r,116._pm_r,  0.54_pm_r, 35._pm_r, &
218.20_pm_r, 23663._pm_r,   3._pm_r,189._pm_r, 1.09_pm_r,317._pm_r,   1._pm_r, 67._pm_r,  0.56_pm_r, 33._pm_r, &
224.30_pm_r, 26900._pm_r,   3._pm_r,218._pm_r, 1.06_pm_r,315._pm_r,   2._pm_r, 52._pm_r,  0.48_pm_r, 31._pm_r, &
231.70_pm_r, 30240._pm_r,   3._pm_r,247._pm_r,  0.86_pm_r,311._pm_r,   2._pm_r, 46._pm_r,  0.31_pm_r, 25._pm_r, &
241.90_pm_r, 33703._pm_r,   4._pm_r,262._pm_r,  0.55_pm_r,303._pm_r,   3._pm_r, 43._pm_r,  0.12_pm_r,358._pm_r, &
252.50_pm_r, 37327._pm_r,   4._pm_r,266._pm_r,  0.25_pm_r,272._pm_r,   3._pm_r, 41._pm_r,  0.12_pm_r,253._pm_r, &
262.60_pm_r, 41096._pm_r,   4._pm_r,264._pm_r,  0.25_pm_r,195._pm_r,   2._pm_r, 38._pm_r,  0.24_pm_r,234._pm_r, &
269.60_pm_r, 45000._pm_r,   4._pm_r,258._pm_r,  0.35_pm_r,185._pm_r,   2._pm_r, 35._pm_r,  0.23_pm_r,251._pm_r, &
269.30_pm_r, 48955._pm_r,   5._pm_r,252._pm_r,  0.35_pm_r,191._pm_r,   2._pm_r, 25._pm_r,  0.34_pm_r,272._pm_r, &
260.50_pm_r, 52837._pm_r,   5._pm_r,247._pm_r,  0.26_pm_r,192._pm_r,   2._pm_r,  6._pm_r,  0.40_pm_r,279._pm_r, &
248.80_pm_r, 56569._pm_r,   5._pm_r,244._pm_r,  0.10_pm_r,169._pm_r,   2._pm_r,349._pm_r,  0.27_pm_r,280._pm_r, &
239.60_pm_r, 60140._pm_r,   5._pm_r,243._pm_r,  0.13_pm_r,100._pm_r,   2._pm_r,343._pm_r,  0.12_pm_r,179._pm_r, &
233.20_pm_r, 63602._pm_r,   5._pm_r,242._pm_r,  0.17_pm_r, 58._pm_r,   1._pm_r,346._pm_r,  0.34_pm_r,147._pm_r, &
226.60_pm_r, 66967._pm_r,   4._pm_r,244._pm_r,  0.23_pm_r, 19._pm_r,   1._pm_r,359._pm_r,  0.51_pm_r,150._pm_r, &
220.30_pm_r, 70240._pm_r,   4._pm_r,249._pm_r,  0.34_pm_r,354._pm_r,   0._pm_r, 71._pm_r,  0.61_pm_r,157._pm_r, &
215.10_pm_r, 73425._pm_r,   4._pm_r,256._pm_r,  0.45_pm_r,342._pm_r,   1._pm_r,139._pm_r,  0.68_pm_r,163._pm_r, &
210.50_pm_r, 76543._pm_r,   4._pm_r,266._pm_r,  0.53_pm_r,336._pm_r,   2._pm_r,152._pm_r,  0.72_pm_r,168._pm_r, &
207.00_pm_r, 79598._pm_r,   5._pm_r,275._pm_r,  0.58_pm_r,331._pm_r,   3._pm_r,159._pm_r,  0.73_pm_r,172._pm_r, &
201.40_pm_r, 82606._pm_r,   5._pm_r,283._pm_r,  0.58_pm_r,330._pm_r,   4._pm_r,162._pm_r,  0.69_pm_r,174._pm_r, &
192.60_pm_r, 85481._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.40_pm_r, 88235._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.30_pm_r, 90920._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
180.60_pm_r, 93584._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.20_pm_r, 96266._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.50_pm_r, 99009._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
195.10_pm_r,101873._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
210.10_pm_r,104949._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
238.10_pm_r,108394._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
292.10_pm_r,112531._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
375.50_pm_r,117878._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
286.10_pm_r,   -49._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
264.00_pm_r,  3978._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
238.10_pm_r,  7653._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.00_pm_r, 11000._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.80_pm_r, 14175._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.40_pm_r, 17317._pm_r,   6._pm_r,212._pm_r,  0.18_pm_r,149._pm_r,   3._pm_r,118._pm_r,  0.53_pm_r,  1._pm_r, &
215.30_pm_r, 20461._pm_r,   6._pm_r,210._pm_r,  0.09_pm_r, 78._pm_r,   3._pm_r,100._pm_r,  0.66_pm_r,359._pm_r, &
218.00_pm_r, 23632._pm_r,   6._pm_r,211._pm_r,  0.41_pm_r,  7._pm_r,   3._pm_r, 76._pm_r,  0.67_pm_r,356._pm_r, &
222.40_pm_r, 26853._pm_r,   5._pm_r,216._pm_r,  0.91_pm_r,358._pm_r,   3._pm_r, 57._pm_r,  0.53_pm_r,349._pm_r, &
229.10_pm_r, 30158._pm_r,   4._pm_r,233._pm_r, 1.35_pm_r,355._pm_r,   3._pm_r, 45._pm_r,  0.31_pm_r,329._pm_r, &
240.70_pm_r, 33591._pm_r,   3._pm_r,266._pm_r, 1.53_pm_r,354._pm_r,   3._pm_r, 39._pm_r,  0.20_pm_r,267._pm_r, &
253.30_pm_r, 37213._pm_r,   4._pm_r,298._pm_r, 1.37_pm_r,353._pm_r,   3._pm_r, 36._pm_r,  0.30_pm_r,225._pm_r, &
264.10_pm_r, 41001._pm_r,   5._pm_r,314._pm_r,  0.92_pm_r,353._pm_r,   2._pm_r, 36._pm_r,  0.36_pm_r,212._pm_r, &
270.60_pm_r, 44924._pm_r,   6._pm_r,319._pm_r,  0.27_pm_r,344._pm_r,   2._pm_r, 35._pm_r,  0.21_pm_r,229._pm_r, &
270.70_pm_r, 48896._pm_r,   6._pm_r,317._pm_r,  0.40_pm_r,205._pm_r,   2._pm_r, 30._pm_r,  0.16_pm_r,277._pm_r, &
262.70_pm_r, 52806._pm_r,   6._pm_r,309._pm_r,  0.85_pm_r,202._pm_r,   2._pm_r, 20._pm_r,  0.18_pm_r,310._pm_r, &
251.10_pm_r, 56572._pm_r,   5._pm_r,295._pm_r,  0.96_pm_r,205._pm_r,   2._pm_r, 12._pm_r,  0.13_pm_r,321._pm_r, &
241.60_pm_r, 60174._pm_r,   6._pm_r,280._pm_r,  0.88_pm_r,196._pm_r,   2._pm_r, 11._pm_r,  0.04_pm_r, 40._pm_r, &
235.00_pm_r, 63664._pm_r,   6._pm_r,269._pm_r,  0.62_pm_r,190._pm_r,   2._pm_r, 14._pm_r,  0.09_pm_r,111._pm_r, &
228.20_pm_r, 67054._pm_r,   6._pm_r,262._pm_r,  0.27_pm_r,186._pm_r,   2._pm_r, 18._pm_r,  0.11_pm_r,143._pm_r, &
221.80_pm_r, 70351._pm_r,   6._pm_r,261._pm_r,  0.09_pm_r,360._pm_r,   2._pm_r, 22._pm_r,  0.11_pm_r,170._pm_r, &
215.80_pm_r, 73553._pm_r,   6._pm_r,265._pm_r,  0.38_pm_r,359._pm_r,   1._pm_r, 24._pm_r,  0.14_pm_r,196._pm_r, &
209.30_pm_r, 76669._pm_r,   6._pm_r,272._pm_r,  0.59_pm_r,360._pm_r,   1._pm_r, 24._pm_r,  0.16_pm_r,210._pm_r, &
202.80_pm_r, 79684._pm_r,   6._pm_r,281._pm_r,  0.69_pm_r,358._pm_r,   1._pm_r, 22._pm_r,  0.18_pm_r,216._pm_r, &
195.50_pm_r, 82610._pm_r,   6._pm_r,291._pm_r,  0.72_pm_r,358._pm_r,   1._pm_r, 16._pm_r,  0.18_pm_r,218._pm_r, &
187.00_pm_r, 85404._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
180.10_pm_r, 88080._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
175.90_pm_r, 90683._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
175.70_pm_r, 93268._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.80_pm_r, 95885._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.20_pm_r, 98591._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
196.90_pm_r,101456._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.10_pm_r,104581._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
245.90_pm_r,108126._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
297.40_pm_r,112370._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
377.40_pm_r,117761._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
280.40_pm_r,   -43._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
257.70_pm_r,  3893._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
233.00_pm_r,  7481._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.60_pm_r, 10792._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.70_pm_r, 13997._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.70_pm_r, 17201._pm_r,  10._pm_r,271._pm_r, 1.94_pm_r,163._pm_r,   5._pm_r, 93._pm_r,  0.51_pm_r,319._pm_r, &
218.60_pm_r, 20402._pm_r,   9._pm_r,252._pm_r, 2.32_pm_r,159._pm_r,   4._pm_r, 84._pm_r,  0.69_pm_r,318._pm_r, &
219.20_pm_r, 23606._pm_r,   9._pm_r,231._pm_r, 2.08_pm_r,151._pm_r,   4._pm_r, 70._pm_r,  0.77_pm_r,314._pm_r, &
221.60_pm_r, 26829._pm_r,  10._pm_r,216._pm_r, 1.33_pm_r,124._pm_r,   3._pm_r, 53._pm_r,  0.71_pm_r,309._pm_r, &
227.20_pm_r, 30112._pm_r,   9._pm_r,208._pm_r, 1.23_pm_r, 55._pm_r,   3._pm_r, 37._pm_r,  0.53_pm_r,298._pm_r, &
238.70_pm_r, 33516._pm_r,   6._pm_r,205._pm_r, 2.10_pm_r, 21._pm_r,   3._pm_r, 26._pm_r,  0.33_pm_r,273._pm_r, &
251.40_pm_r, 37110._pm_r,   3._pm_r,217._pm_r, 2.61_pm_r, 10._pm_r,   3._pm_r, 20._pm_r,  0.25_pm_r,229._pm_r, &
263.30_pm_r, 40876._pm_r,   2._pm_r,315._pm_r, 2.49_pm_r,  4._pm_r,   2._pm_r, 18._pm_r,  0.28_pm_r,200._pm_r, &
271.00_pm_r, 44798._pm_r,   5._pm_r,342._pm_r, 1.53_pm_r,350._pm_r,   2._pm_r, 18._pm_r,  0.11_pm_r,193._pm_r, &
270.80_pm_r, 48774._pm_r,   6._pm_r,339._pm_r,  0.88_pm_r,293._pm_r,   2._pm_r, 16._pm_r,  0.17_pm_r,321._pm_r, &
263.50_pm_r, 52689._pm_r,   7._pm_r,326._pm_r, 1.26_pm_r,242._pm_r,   2._pm_r,  7._pm_r,  0.46_pm_r,325._pm_r, &
252.80_pm_r, 56475._pm_r,   7._pm_r,309._pm_r, 1.37_pm_r,227._pm_r,   3._pm_r,356._pm_r,  0.69_pm_r,324._pm_r, &
243.10_pm_r, 60100._pm_r,   7._pm_r,294._pm_r, 1.12_pm_r,207._pm_r,   4._pm_r,349._pm_r,  0.56_pm_r,324._pm_r, &
236.40_pm_r, 63610._pm_r,   7._pm_r,282._pm_r,  0.84_pm_r,192._pm_r,   5._pm_r,345._pm_r,  0.32_pm_r,331._pm_r, &
230.20_pm_r, 67024._pm_r,   7._pm_r,273._pm_r,  0.54_pm_r,186._pm_r,   5._pm_r,345._pm_r,  0.11_pm_r, 29._pm_r, &
224.10_pm_r, 70352._pm_r,   7._pm_r,269._pm_r,  0.25_pm_r,198._pm_r,   5._pm_r,348._pm_r,  0.27_pm_r,106._pm_r, &
217.70_pm_r, 73586._pm_r,   7._pm_r,268._pm_r,  0.15_pm_r,277._pm_r,   4._pm_r,354._pm_r,  0.46_pm_r,117._pm_r, &
209.00_pm_r, 76716._pm_r,   8._pm_r,269._pm_r,  0.30_pm_r,312._pm_r,   4._pm_r,  3._pm_r,  0.57_pm_r,119._pm_r, &
200.00_pm_r, 79706._pm_r,   8._pm_r,272._pm_r,  0.40_pm_r,319._pm_r,   4._pm_r, 15._pm_r,  0.62_pm_r,122._pm_r, &
190.40_pm_r, 82572._pm_r,   8._pm_r,275._pm_r,  0.44_pm_r,321._pm_r,   4._pm_r, 29._pm_r,  0.60_pm_r,122._pm_r, &
180.50_pm_r, 85274._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
173.40_pm_r, 87847._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
169.60_pm_r, 90352._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
170.50_pm_r, 92850._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
175.30_pm_r, 95400._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.10_pm_r, 98066._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.90_pm_r,100935._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.90_pm_r,104116._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.90_pm_r,107767._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
301.20_pm_r,112109._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
378.80_pm_r,117523._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
273.00_pm_r,    26._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
252.40_pm_r,  3865._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
229.00_pm_r,  7383._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.00_pm_r, 10671._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.30_pm_r, 13903._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.20_pm_r, 17145._pm_r,  12._pm_r,280._pm_r, 2.26_pm_r,169._pm_r,   6._pm_r, 59._pm_r,  0.55_pm_r,245._pm_r, &
221.10_pm_r, 20382._pm_r,  11._pm_r,261._pm_r, 2.95_pm_r,166._pm_r,   5._pm_r, 57._pm_r,  0.72_pm_r,244._pm_r, &
221.50_pm_r, 23622._pm_r,  11._pm_r,237._pm_r, 3.13_pm_r,160._pm_r,   4._pm_r, 56._pm_r,  0.80_pm_r,242._pm_r, &
222.20_pm_r, 26868._pm_r,  12._pm_r,217._pm_r, 2.56_pm_r,148._pm_r,   2._pm_r, 53._pm_r,  0.69_pm_r,239._pm_r, &
226.30_pm_r, 30149._pm_r,  13._pm_r,204._pm_r, 1.68_pm_r,118._pm_r,   2._pm_r, 51._pm_r,  0.44_pm_r,232._pm_r, &
235.60_pm_r, 33524._pm_r,  13._pm_r,195._pm_r, 1.59_pm_r, 63._pm_r,   1._pm_r, 54._pm_r,  0.17_pm_r,211._pm_r, &
247.10_pm_r, 37061._pm_r,  10._pm_r,187._pm_r, 2.12_pm_r, 32._pm_r,   1._pm_r, 60._pm_r,  0.11_pm_r,111._pm_r, &
259.30_pm_r, 40765._pm_r,   7._pm_r,180._pm_r, 2.36_pm_r, 17._pm_r,   1._pm_r, 64._pm_r,  0.19_pm_r, 74._pm_r, &
268.50_pm_r, 44640._pm_r,   4._pm_r,172._pm_r, 1.91_pm_r,  1._pm_r,   2._pm_r, 63._pm_r,  0.18_pm_r, 45._pm_r, &
270.60_pm_r, 48596._pm_r,   2._pm_r,177._pm_r, 1.43_pm_r,329._pm_r,   2._pm_r, 59._pm_r,  0.20_pm_r, 15._pm_r, &
263.80_pm_r, 52514._pm_r,   2._pm_r,252._pm_r, 1.33_pm_r,290._pm_r,   2._pm_r, 50._pm_r,  0.28_pm_r,346._pm_r, &
252.90_pm_r, 56302._pm_r,   3._pm_r,266._pm_r, 1.19_pm_r,264._pm_r,   2._pm_r, 37._pm_r,  0.44_pm_r,328._pm_r, &
243.90_pm_r, 59935._pm_r,   5._pm_r,261._pm_r,  0.76_pm_r,233._pm_r,   2._pm_r, 21._pm_r,  0.50_pm_r,313._pm_r, &
236.90_pm_r, 63455._pm_r,   5._pm_r,254._pm_r,  0.57_pm_r,187._pm_r,   3._pm_r,  7._pm_r,  0.45_pm_r,303._pm_r, &
231.10_pm_r, 66880._pm_r,   5._pm_r,245._pm_r,  0.57_pm_r,150._pm_r,   3._pm_r,357._pm_r,  0.30_pm_r,294._pm_r, &
225.70_pm_r, 70227._pm_r,   5._pm_r,236._pm_r,  0.57_pm_r,125._pm_r,   3._pm_r,351._pm_r,  0.13_pm_r,274._pm_r, &
219.00_pm_r, 73483._pm_r,   5._pm_r,228._pm_r,  0.55_pm_r,106._pm_r,   3._pm_r,350._pm_r,  0.11_pm_r,172._pm_r, &
210.30_pm_r, 76632._pm_r,   4._pm_r,220._pm_r,  0.51_pm_r, 91._pm_r,   3._pm_r,351._pm_r,  0.21_pm_r,149._pm_r, &
200.20_pm_r, 79637._pm_r,   4._pm_r,212._pm_r,  0.49_pm_r, 81._pm_r,   3._pm_r,355._pm_r,  0.28_pm_r,138._pm_r, &
187.90_pm_r, 82498._pm_r,   3._pm_r,204._pm_r,  0.45_pm_r, 73._pm_r,   2._pm_r,  2._pm_r,  0.30_pm_r,135._pm_r, &
174.60_pm_r, 85120._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
166.50_pm_r, 87584._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
163.60_pm_r, 89991._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
165.60_pm_r, 92407._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
172.00_pm_r, 94895._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.20_pm_r, 97526._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.90_pm_r,100401._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.60_pm_r,103636._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
261.10_pm_r,107388._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
303.10_pm_r,111805._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
379.70_pm_r,117222._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
261.90_pm_r,   179._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
247.70_pm_r,  3900._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.10_pm_r,  7359._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.90_pm_r, 10631._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.80_pm_r, 13878._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.70_pm_r, 17141._pm_r,   8._pm_r,267._pm_r, 1.26_pm_r,173._pm_r,   4._pm_r, 43._pm_r,  0.45_pm_r,211._pm_r, &
223.00_pm_r, 20403._pm_r,   8._pm_r,251._pm_r, 1.77_pm_r,169._pm_r,   3._pm_r, 46._pm_r,  0.62_pm_r,209._pm_r, &
224.50_pm_r, 23681._pm_r,   9._pm_r,232._pm_r, 2.10_pm_r,163._pm_r,   2._pm_r, 54._pm_r,  0.72_pm_r,207._pm_r, &
223.90_pm_r, 26965._pm_r,  10._pm_r,215._pm_r, 2.10_pm_r,155._pm_r,   1._pm_r, 79._pm_r,  0.70_pm_r,204._pm_r, &
225.50_pm_r, 30253._pm_r,  12._pm_r,202._pm_r, 1.79_pm_r,141._pm_r,   1._pm_r,126._pm_r,  0.56_pm_r,200._pm_r, &
232.00_pm_r, 33596._pm_r,  13._pm_r,192._pm_r, 1.35_pm_r,116._pm_r,   1._pm_r,153._pm_r,  0.35_pm_r,192._pm_r, &
242.60_pm_r, 37071._pm_r,  13._pm_r,185._pm_r, 1.09_pm_r, 78._pm_r,   2._pm_r,159._pm_r,  0.14_pm_r,172._pm_r, &
254.70_pm_r, 40710._pm_r,  12._pm_r,178._pm_r, 1.14_pm_r, 40._pm_r,   2._pm_r,158._pm_r,  0.05_pm_r, 68._pm_r, &
264.90_pm_r, 44522._pm_r,  10._pm_r,174._pm_r, 1.15_pm_r, 10._pm_r,   2._pm_r,155._pm_r,  0.12_pm_r, 24._pm_r, &
269.70_pm_r, 48444._pm_r,   9._pm_r,174._pm_r, 1.11_pm_r,341._pm_r,   2._pm_r,150._pm_r,  0.11_pm_r, 15._pm_r, &
264.80_pm_r, 52366._pm_r,   7._pm_r,179._pm_r, 1.02_pm_r,313._pm_r,   2._pm_r,146._pm_r,  0.11_pm_r,  8._pm_r, &
254.40_pm_r, 56174._pm_r,   7._pm_r,189._pm_r,  0.77_pm_r,283._pm_r,   1._pm_r,141._pm_r,  0.16_pm_r,  4._pm_r, &
244.30_pm_r, 59822._pm_r,   7._pm_r,197._pm_r,  0.46_pm_r,259._pm_r,   1._pm_r,134._pm_r,  0.21_pm_r,352._pm_r, &
235.90_pm_r, 63338._pm_r,   7._pm_r,199._pm_r,  0.24_pm_r,203._pm_r,   1._pm_r,121._pm_r,  0.22_pm_r,351._pm_r, &
230.90_pm_r, 66753._pm_r,   7._pm_r,198._pm_r,  0.33_pm_r,134._pm_r,   1._pm_r,104._pm_r,  0.20_pm_r,350._pm_r, &
225.70_pm_r, 70100._pm_r,   8._pm_r,193._pm_r,  0.49_pm_r,111._pm_r,   1._pm_r, 84._pm_r,  0.15_pm_r,350._pm_r, &
218.20_pm_r, 73351._pm_r,   8._pm_r,187._pm_r,  0.59_pm_r, 98._pm_r,   1._pm_r, 70._pm_r,  0.09_pm_r,355._pm_r, &
209.20_pm_r, 76484._pm_r,   8._pm_r,180._pm_r,  0.65_pm_r, 90._pm_r,   1._pm_r, 64._pm_r,  0.04_pm_r,  7._pm_r, &
199.50_pm_r, 79475._pm_r,   8._pm_r,173._pm_r,  0.66_pm_r, 85._pm_r,   1._pm_r, 62._pm_r,  0.01_pm_r, 76._pm_r, &
185.90_pm_r, 82328._pm_r,   8._pm_r,166._pm_r,  0.61_pm_r, 82._pm_r,   1._pm_r, 64._pm_r,  0.02_pm_r,146._pm_r, &
169.60_pm_r, 84878._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
160.60_pm_r, 87247._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
158.70_pm_r, 89574._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
161.70_pm_r, 91924._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
169.50_pm_r, 94363._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.60_pm_r, 96968._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.70_pm_r, 99849._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
231.50_pm_r,103133._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
266.60_pm_r,106967._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
303.30_pm_r,111430._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
380.30_pm_r,116838._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
253.20_pm_r,   324._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
244.20_pm_r,  3954._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.20_pm_r,  7373._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.10_pm_r, 10630._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.30_pm_r, 13881._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.50_pm_r, 17154._pm_r,   3._pm_r,233._pm_r,  0.52_pm_r,213._pm_r,   2._pm_r, 38._pm_r,  0.20_pm_r,190._pm_r, &
224.30_pm_r, 20430._pm_r,   4._pm_r,228._pm_r,  0.72_pm_r,206._pm_r,   1._pm_r, 46._pm_r,  0.28_pm_r,193._pm_r, &
227.30_pm_r, 23742._pm_r,   5._pm_r,221._pm_r,  0.86_pm_r,196._pm_r,   1._pm_r, 62._pm_r,  0.32_pm_r,197._pm_r, &
225.50_pm_r, 27061._pm_r,   6._pm_r,215._pm_r,  0.90_pm_r,181._pm_r,   1._pm_r, 96._pm_r,  0.31_pm_r,202._pm_r, &
225.20_pm_r, 30359._pm_r,   7._pm_r,207._pm_r,  0.87_pm_r,160._pm_r,   1._pm_r,140._pm_r,  0.27_pm_r,211._pm_r, &
230.20_pm_r, 33685._pm_r,   8._pm_r,199._pm_r,  0.81_pm_r,135._pm_r,   1._pm_r,167._pm_r,  0.21_pm_r,223._pm_r, &
240.20_pm_r, 37129._pm_r,   8._pm_r,191._pm_r,  0.75_pm_r,107._pm_r,   1._pm_r,182._pm_r,  0.14_pm_r,244._pm_r, &
251.80_pm_r, 40729._pm_r,   8._pm_r,183._pm_r,  0.70_pm_r, 80._pm_r,   1._pm_r,192._pm_r,  0.11_pm_r,265._pm_r, &
262.50_pm_r, 44500._pm_r,   8._pm_r,178._pm_r,  0.47_pm_r, 51._pm_r,   1._pm_r,198._pm_r,  0.04_pm_r,247._pm_r, &
268.80_pm_r, 48397._pm_r,   7._pm_r,175._pm_r,  0.28_pm_r,354._pm_r,   1._pm_r,198._pm_r,  0.07_pm_r,180._pm_r, &
265.10_pm_r, 52316._pm_r,   7._pm_r,178._pm_r,  0.43_pm_r,290._pm_r,   1._pm_r,196._pm_r,  0.19_pm_r,187._pm_r, &
255.00_pm_r, 56131._pm_r,   7._pm_r,185._pm_r,  0.60_pm_r,272._pm_r,   2._pm_r,195._pm_r,  0.38_pm_r,194._pm_r, &
243.90_pm_r, 59782._pm_r,   7._pm_r,192._pm_r,  0.52_pm_r,285._pm_r,   2._pm_r,195._pm_r,  0.44_pm_r,196._pm_r, &
234.80_pm_r, 63286._pm_r,   7._pm_r,198._pm_r,  0.43_pm_r,308._pm_r,   3._pm_r,196._pm_r,  0.38_pm_r,198._pm_r, &
230.10_pm_r, 66687._pm_r,   6._pm_r,202._pm_r,  0.40_pm_r,342._pm_r,   3._pm_r,196._pm_r,  0.22_pm_r,203._pm_r, &
225.40_pm_r, 70027._pm_r,   6._pm_r,205._pm_r,  0.44_pm_r,  8._pm_r,   4._pm_r,197._pm_r,  0.03_pm_r,270._pm_r, &
217.50_pm_r, 73271._pm_r,   5._pm_r,206._pm_r,  0.53_pm_r, 20._pm_r,   3._pm_r,198._pm_r,  0.18_pm_r,  5._pm_r, &
208.60_pm_r, 76394._pm_r,   4._pm_r,206._pm_r,  0.57_pm_r, 26._pm_r,   3._pm_r,199._pm_r,  0.32_pm_r,  9._pm_r, &
199.20_pm_r, 79377._pm_r,   3._pm_r,206._pm_r,  0.57_pm_r, 30._pm_r,   3._pm_r,201._pm_r,  0.40_pm_r,  9._pm_r, &
184.70_pm_r, 82227._pm_r,   3._pm_r,204._pm_r,  0.52_pm_r, 32._pm_r,   2._pm_r,205._pm_r,  0.40_pm_r, 12._pm_r, &
166.40_pm_r, 84727._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
156.80_pm_r, 87034._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
155.60_pm_r, 89309._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
159.30_pm_r, 91617._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
168.00_pm_r, 94025._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.30_pm_r, 96615._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.90_pm_r, 99500._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
234.90_pm_r,102819._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
270.00_pm_r,106706._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
302.80_pm_r,111192._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
380.60_pm_r,116587._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_avril = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_avril,(/10,nb_alt,nb_lat/))

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)

end subroutine cps_atmi_avril

subroutine cps_atmi_mai (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_mai
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de MAI
!
!$Description
!  LectuLecture du fichier ATMospherique pour l'Interplation correspondant au mois de MAI 
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_mai (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments            
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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


  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)
    
    ! Redefinition des reels pour réduire la longueur des lignes
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
268.90_pm_r,  -187._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
241.20_pm_r,  3536._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.40_pm_r,  6898._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
211.70_pm_r, 10044._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.90_pm_r, 13114._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.90_pm_r, 16144._pm_r,   6._pm_r,195._pm_r, 1.24_pm_r,188._pm_r,   3._pm_r,146._pm_r,  0.20_pm_r,317._pm_r, &
200.10_pm_r, 19111._pm_r,   8._pm_r,192._pm_r, 1.78_pm_r,183._pm_r,   3._pm_r,147._pm_r,  0.31_pm_r,315._pm_r, &
194.30_pm_r, 22000._pm_r,  11._pm_r,188._pm_r, 2.06_pm_r,173._pm_r,   3._pm_r,150._pm_r,  0.40_pm_r,311._pm_r, &
187.40_pm_r, 24792._pm_r,  14._pm_r,184._pm_r, 1.76_pm_r,153._pm_r,   2._pm_r,156._pm_r,  0.39_pm_r,308._pm_r, &
194.80_pm_r, 27573._pm_r,  15._pm_r,177._pm_r, 1.42_pm_r,114._pm_r,   2._pm_r,166._pm_r,  0.30_pm_r,300._pm_r, &
206.20_pm_r, 30512._pm_r,  16._pm_r,169._pm_r, 1.72_pm_r, 79._pm_r,   1._pm_r,178._pm_r,  0.20_pm_r,288._pm_r, &
216.40_pm_r, 33609._pm_r,  15._pm_r,159._pm_r, 2.09_pm_r, 62._pm_r,   1._pm_r,188._pm_r,  0.14_pm_r,266._pm_r, &
227.00_pm_r, 36852._pm_r,  15._pm_r,147._pm_r, 2.13_pm_r, 54._pm_r,   1._pm_r,194._pm_r,  0.11_pm_r,243._pm_r, &
238.00_pm_r, 40259._pm_r,  15._pm_r,137._pm_r, 1.73_pm_r, 54._pm_r,   2._pm_r,199._pm_r,  0.15_pm_r,240._pm_r, &
250.40_pm_r, 43830._pm_r,  16._pm_r,128._pm_r, 1.52_pm_r, 58._pm_r,   2._pm_r,204._pm_r,  0.19_pm_r,231._pm_r, &
259.40_pm_r, 47576._pm_r,  17._pm_r,121._pm_r, 1.62_pm_r, 57._pm_r,   2._pm_r,207._pm_r,  0.19_pm_r,215._pm_r, &
260.10_pm_r, 51385._pm_r,  18._pm_r,113._pm_r, 1.89_pm_r, 52._pm_r,   2._pm_r,206._pm_r,  0.17_pm_r,187._pm_r, &
257.00_pm_r, 55173._pm_r,  19._pm_r,106._pm_r, 1.59_pm_r, 44._pm_r,   3._pm_r,203._pm_r,  0.12_pm_r,137._pm_r, &
251.20_pm_r, 58901._pm_r,  20._pm_r,101._pm_r, 1.14_pm_r, 31._pm_r,   3._pm_r,199._pm_r,  0.13_pm_r, 94._pm_r, &
243.90_pm_r, 62524._pm_r,  20._pm_r, 97._pm_r,  0.66_pm_r,  1._pm_r,   2._pm_r,195._pm_r,  0.14_pm_r, 67._pm_r, &
238.50_pm_r, 66056._pm_r,  20._pm_r, 95._pm_r,  0.64_pm_r,298._pm_r,   2._pm_r,191._pm_r,  0.15_pm_r, 48._pm_r, &
234.90_pm_r, 69519._pm_r,  18._pm_r, 95._pm_r, 1.12_pm_r,267._pm_r,   2._pm_r,189._pm_r,  0.17_pm_r, 27._pm_r, &
233.50_pm_r, 72948._pm_r,  17._pm_r, 97._pm_r, 1.62_pm_r,257._pm_r,   2._pm_r,187._pm_r,  0.19_pm_r, 12._pm_r, &
234.00_pm_r, 76372._pm_r,  14._pm_r,101._pm_r, 2.03_pm_r,253._pm_r,   2._pm_r,187._pm_r,  0.21_pm_r,  1._pm_r, &
231.80_pm_r, 79802._pm_r,  11._pm_r,109._pm_r, 2.23_pm_r,250._pm_r,   1._pm_r,190._pm_r,  0.21_pm_r,357._pm_r, &
225.10_pm_r, 83143._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.70_pm_r, 86370._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.70_pm_r, 89444._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.20_pm_r, 92395._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
196.10_pm_r, 95297._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.30_pm_r, 98225._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
207.10_pm_r,101264._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.70_pm_r,104501._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
240.30_pm_r,108043._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
269.40_pm_r,112023._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
335.10_pm_r,116790._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
272.90_pm_r,   -96._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
245.20_pm_r,  3687._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.20_pm_r,  7100._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.20_pm_r, 10286._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
211.80_pm_r, 13397._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.50_pm_r, 16475._pm_r,   9._pm_r,205._pm_r, 2.14_pm_r,155._pm_r,   5._pm_r,150._pm_r,  0.19_pm_r,348._pm_r, &
204.90_pm_r, 19502._pm_r,  12._pm_r,190._pm_r, 3.14_pm_r,151._pm_r,   5._pm_r,149._pm_r,  0.34_pm_r,342._pm_r, &
201.10_pm_r, 22475._pm_r,  16._pm_r,178._pm_r, 3.76_pm_r,144._pm_r,   4._pm_r,147._pm_r,  0.54_pm_r,337._pm_r, &
195.90_pm_r, 25382._pm_r,  21._pm_r,168._pm_r, 3.49_pm_r,133._pm_r,   3._pm_r,145._pm_r,  0.73_pm_r,332._pm_r, &
198.60_pm_r, 28260._pm_r,  24._pm_r,160._pm_r, 2.66_pm_r,108._pm_r,   2._pm_r,142._pm_r,  0.82_pm_r,328._pm_r, &
206.30_pm_r, 31222._pm_r,  25._pm_r,153._pm_r, 2.38_pm_r, 70._pm_r,   1._pm_r,135._pm_r,  0.83_pm_r,323._pm_r, &
215.70_pm_r, 34313._pm_r,  25._pm_r,145._pm_r, 2.74_pm_r, 40._pm_r,   0._pm_r,330._pm_r,  0.72_pm_r,319._pm_r, &
226.80_pm_r, 37548._pm_r,  24._pm_r,135._pm_r, 3.02_pm_r, 23._pm_r,   1._pm_r,321._pm_r,  0.55_pm_r,315._pm_r, &
238.60_pm_r, 40959._pm_r,  22._pm_r,125._pm_r, 2.65_pm_r, 22._pm_r,   2._pm_r,320._pm_r,  0.27_pm_r,319._pm_r, &
250.80_pm_r, 44539._pm_r,  22._pm_r,116._pm_r, 2.43_pm_r, 25._pm_r,   2._pm_r,320._pm_r,  0.11_pm_r,331._pm_r, &
258.80_pm_r, 48282._pm_r,  22._pm_r,106._pm_r, 2.58_pm_r, 25._pm_r,   2._pm_r,321._pm_r,  0.05_pm_r,311._pm_r, &
258.80_pm_r, 52078._pm_r,  23._pm_r, 96._pm_r, 2.88_pm_r, 22._pm_r,   2._pm_r,319._pm_r,  0.08_pm_r,245._pm_r, &
255.80_pm_r, 55849._pm_r,  24._pm_r, 87._pm_r, 2.49_pm_r, 12._pm_r,   2._pm_r,315._pm_r,  0.13_pm_r,221._pm_r, &
250.50_pm_r, 59561._pm_r,  25._pm_r, 79._pm_r, 1.91_pm_r,359._pm_r,   2._pm_r,311._pm_r,  0.14_pm_r,209._pm_r, &
244.20_pm_r, 63181._pm_r,  25._pm_r, 74._pm_r, 1.35_pm_r,333._pm_r,   2._pm_r,306._pm_r,  0.11_pm_r,199._pm_r, &
238.60_pm_r, 66717._pm_r,  24._pm_r, 70._pm_r, 1.24_pm_r,292._pm_r,   2._pm_r,303._pm_r,  0.06_pm_r,180._pm_r, &
234.30_pm_r, 70177._pm_r,  22._pm_r, 68._pm_r, 1.66_pm_r,261._pm_r,   2._pm_r,302._pm_r,  0.04_pm_r,104._pm_r, &
231.40_pm_r, 73587._pm_r,  19._pm_r, 68._pm_r, 2.23_pm_r,247._pm_r,   2._pm_r,304._pm_r,  0.08_pm_r, 60._pm_r, &
230.00_pm_r, 76965._pm_r,  16._pm_r, 69._pm_r, 2.68_pm_r,241._pm_r,   2._pm_r,307._pm_r,  0.13_pm_r, 49._pm_r, &
227.10_pm_r, 80323._pm_r,  12._pm_r, 72._pm_r, 2.90_pm_r,237._pm_r,   2._pm_r,313._pm_r,  0.15_pm_r, 46._pm_r, &
221.20_pm_r, 83601._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
212.60_pm_r, 86783._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.10_pm_r, 89818._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
196.10_pm_r, 92736._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.20_pm_r, 95610._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.10_pm_r, 98511._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.50_pm_r,101515._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.70_pm_r,104711._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
238.00_pm_r,108212._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
270.30_pm_r,112180._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
337.30_pm_r,116983._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
       
276.90_pm_r,   -54._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
251.00_pm_r,  3804._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
227.00_pm_r,  7297._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
216.10_pm_r, 10534._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.80_pm_r, 13683._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
212.80_pm_r, 16816._pm_r,   9._pm_r,201._pm_r, 2.58_pm_r,137._pm_r,   3._pm_r,162._pm_r,  0.32_pm_r, 39._pm_r, &
210.40_pm_r, 19914._pm_r,  12._pm_r,181._pm_r, 3.71_pm_r,134._pm_r,   3._pm_r,153._pm_r,  0.46_pm_r, 30._pm_r, &
209.10_pm_r, 22988._pm_r,  17._pm_r,165._pm_r, 4.45_pm_r,130._pm_r,   3._pm_r,140._pm_r,  0.58_pm_r, 15._pm_r, &
205.40_pm_r, 26024._pm_r,  22._pm_r,154._pm_r, 4.27_pm_r,122._pm_r,   2._pm_r,122._pm_r,  0.69_pm_r,353._pm_r, &
204.10_pm_r, 29018._pm_r,  27._pm_r,147._pm_r, 3.32_pm_r,105._pm_r,   1._pm_r, 91._pm_r,  0.83_pm_r,332._pm_r, &
207.70_pm_r, 32028._pm_r,  29._pm_r,140._pm_r, 2.46_pm_r, 68._pm_r,   1._pm_r, 30._pm_r,  0.95_pm_r,314._pm_r, &
215.80_pm_r, 35127._pm_r,  29._pm_r,133._pm_r, 2.71_pm_r, 25._pm_r,   2._pm_r,344._pm_r,  0.94_pm_r,302._pm_r, &
226.80_pm_r, 38362._pm_r,  27._pm_r,125._pm_r, 3.39_pm_r,  2._pm_r,   3._pm_r,326._pm_r,  0.79_pm_r,293._pm_r, &
238.10_pm_r, 41771._pm_r,  25._pm_r,115._pm_r, 3.25_pm_r,  3._pm_r,   4._pm_r,318._pm_r,  0.34_pm_r,283._pm_r, &
248.40_pm_r, 45331._pm_r,  23._pm_r,104._pm_r, 3.05_pm_r,  6._pm_r,   4._pm_r,316._pm_r,  0.07_pm_r,297._pm_r, &
254.30_pm_r, 49021._pm_r,  23._pm_r, 93._pm_r, 3.06_pm_r,  3._pm_r,   4._pm_r,317._pm_r,  0.12_pm_r,350._pm_r, &
254.20_pm_r, 52750._pm_r,  23._pm_r, 82._pm_r, 3.24_pm_r,353._pm_r,   4._pm_r,318._pm_r,  0.34_pm_r,323._pm_r, &
251.90_pm_r, 56457._pm_r,  23._pm_r, 71._pm_r, 3.07_pm_r,337._pm_r,   5._pm_r,318._pm_r,  0.45_pm_r,312._pm_r, &
248.80_pm_r, 60125._pm_r,  23._pm_r, 60._pm_r, 2.71_pm_r,321._pm_r,   6._pm_r,317._pm_r,  0.47_pm_r,312._pm_r, &
244.70_pm_r, 63739._pm_r,  22._pm_r, 51._pm_r, 2.27_pm_r,302._pm_r,   6._pm_r,317._pm_r,  0.41_pm_r,322._pm_r, &
239.50_pm_r, 67286._pm_r,  20._pm_r, 43._pm_r, 1.98_pm_r,276._pm_r,   7._pm_r,318._pm_r,  0.33_pm_r,342._pm_r, &
234.80_pm_r, 70757._pm_r,  18._pm_r, 38._pm_r, 2.06_pm_r,250._pm_r,   7._pm_r,320._pm_r,  0.30_pm_r, 13._pm_r, &
230.20_pm_r, 74164._pm_r,  15._pm_r, 33._pm_r, 2.37_pm_r,232._pm_r,   7._pm_r,324._pm_r,  0.35_pm_r, 39._pm_r, &
225.70_pm_r, 77496._pm_r,  12._pm_r, 29._pm_r, 2.66_pm_r,221._pm_r,   7._pm_r,328._pm_r,  0.42_pm_r, 54._pm_r, &
221.60_pm_r, 80770._pm_r,   8._pm_r, 25._pm_r, 2.79_pm_r,214._pm_r,   7._pm_r,333._pm_r,  0.47_pm_r, 62._pm_r, &
216.20_pm_r, 83977._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.20_pm_r, 87098._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.50_pm_r, 90075._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
193.10_pm_r, 92947._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.70_pm_r, 95784._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.20_pm_r, 98645._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.80_pm_r,101603._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
213.10_pm_r,104736._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
234.20_pm_r,108173._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
270.90_pm_r,112115._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
340.40_pm_r,116964._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
280.90_pm_r,   -52._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
257.70_pm_r,  3887._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
232.70_pm_r,  7473._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.30_pm_r, 10764._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
216.10_pm_r, 13934._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.70_pm_r, 17098._pm_r,   5._pm_r,207._pm_r, 1.89_pm_r,126._pm_r,   1._pm_r,145._pm_r,  0.37_pm_r, 54._pm_r, &
214.90_pm_r, 20250._pm_r,   6._pm_r,176._pm_r, 2.60_pm_r,123._pm_r,   1._pm_r,107._pm_r,  0.47_pm_r, 43._pm_r, &
215.20_pm_r, 23401._pm_r,   9._pm_r,154._pm_r, 2.98_pm_r,117._pm_r,   1._pm_r, 78._pm_r,  0.49_pm_r, 21._pm_r, &
213.60_pm_r, 26542._pm_r,  13._pm_r,141._pm_r, 2.90_pm_r,107._pm_r,   2._pm_r, 54._pm_r,  0.54_pm_r,346._pm_r, &
212.70_pm_r, 29662._pm_r,  16._pm_r,131._pm_r, 2.39_pm_r, 88._pm_r,   2._pm_r, 27._pm_r,  0.75_pm_r,312._pm_r, &
214.60_pm_r, 32785._pm_r,  18._pm_r,123._pm_r, 2.00_pm_r, 52._pm_r,   2._pm_r,355._pm_r, 1.05_pm_r,291._pm_r, &
221.40_pm_r, 35974._pm_r,  18._pm_r,113._pm_r, 2.22_pm_r, 14._pm_r,   3._pm_r,327._pm_r, 1.19_pm_r,276._pm_r, &
232.10_pm_r, 39289._pm_r,  17._pm_r,102._pm_r, 2.59_pm_r,353._pm_r,   5._pm_r,308._pm_r, 1.11_pm_r,262._pm_r, &
242.60_pm_r, 42770._pm_r,  16._pm_r, 89._pm_r, 2.50_pm_r,354._pm_r,   5._pm_r,297._pm_r,  0.65_pm_r,241._pm_r, &
249.80_pm_r, 46378._pm_r,  16._pm_r, 77._pm_r, 2.38_pm_r,353._pm_r,   6._pm_r,291._pm_r,  0.33_pm_r,211._pm_r, &
252.10_pm_r, 50060._pm_r,  17._pm_r, 65._pm_r, 2.42_pm_r,341._pm_r,   6._pm_r,288._pm_r,  0.09_pm_r,173._pm_r, &
249.80_pm_r, 53740._pm_r,  17._pm_r, 52._pm_r, 2.71_pm_r,323._pm_r,   6._pm_r,288._pm_r,  0.19_pm_r,357._pm_r, &
246.20_pm_r, 57373._pm_r,  17._pm_r, 38._pm_r, 2.74_pm_r,307._pm_r,   6._pm_r,291._pm_r,  0.21_pm_r,350._pm_r, &
241.60_pm_r, 60947._pm_r,  17._pm_r, 25._pm_r, 2.50_pm_r,292._pm_r,   6._pm_r,293._pm_r,  0.19_pm_r,348._pm_r, &
237.20_pm_r, 64454._pm_r,  16._pm_r, 13._pm_r, 2.09_pm_r,275._pm_r,   6._pm_r,295._pm_r,  0.16_pm_r,346._pm_r, &
232.70_pm_r, 67897._pm_r,  16._pm_r,  4._pm_r, 1.74_pm_r,253._pm_r,   6._pm_r,297._pm_r,  0.12_pm_r,346._pm_r, &
228.20_pm_r, 71270._pm_r,  15._pm_r,356._pm_r, 1.65_pm_r,227._pm_r,   6._pm_r,298._pm_r,  0.08_pm_r,358._pm_r, &
224.40_pm_r, 74585._pm_r,  13._pm_r,348._pm_r, 1.80_pm_r,206._pm_r,   6._pm_r,299._pm_r,  0.06_pm_r, 27._pm_r, &
221.20_pm_r, 77847._pm_r,  10._pm_r,341._pm_r, 1.98_pm_r,194._pm_r,   6._pm_r,300._pm_r,  0.07_pm_r, 59._pm_r, &
217.50_pm_r, 81064._pm_r,   8._pm_r,330._pm_r, 2.06_pm_r,186._pm_r,   6._pm_r,300._pm_r,  0.08_pm_r, 67._pm_r, &
211.50_pm_r, 84211._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.10_pm_r, 87261._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.40_pm_r, 90169._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.10_pm_r, 92992._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.20_pm_r, 95790._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.10_pm_r, 98613._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
196.80_pm_r,101521._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
207.70_pm_r,104587._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
229.20_pm_r,107944._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
270.50_pm_r,111841._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
343.70_pm_r,116732._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
287.60_pm_r,   -40._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
264.20_pm_r,  4000._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
238.20_pm_r,  7677._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.50_pm_r, 11021._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.20_pm_r, 14188._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.00_pm_r, 17321._pm_r,   2._pm_r,213._pm_r,  0.99_pm_r,112._pm_r,   1._pm_r, 97._pm_r,  0.31_pm_r, 14._pm_r, &
215.40_pm_r, 20463._pm_r,   2._pm_r,163._pm_r, 1.25_pm_r,105._pm_r,   1._pm_r, 70._pm_r,  0.40_pm_r,  1._pm_r, &
218.70_pm_r, 23645._pm_r,   3._pm_r,133._pm_r, 1.31_pm_r, 93._pm_r,   1._pm_r, 45._pm_r,  0.44_pm_r,341._pm_r, &
219.70_pm_r, 26856._pm_r,   5._pm_r,116._pm_r, 1.21_pm_r, 72._pm_r,   2._pm_r, 22._pm_r,  0.50_pm_r,312._pm_r, &
221.10_pm_r, 30082._pm_r,   6._pm_r,101._pm_r, 1.20_pm_r, 38._pm_r,   2._pm_r,356._pm_r,  0.66_pm_r,281._pm_r, &
225.60_pm_r, 33349._pm_r,   6._pm_r, 85._pm_r, 1.44_pm_r,  8._pm_r,   2._pm_r,327._pm_r,  0.90_pm_r,259._pm_r, &
232.80_pm_r, 36705._pm_r,   7._pm_r, 65._pm_r, 1.68_pm_r,350._pm_r,   3._pm_r,298._pm_r, 1.09_pm_r,244._pm_r, &
242.90_pm_r, 40181._pm_r,   8._pm_r, 47._pm_r, 1.67_pm_r,339._pm_r,   4._pm_r,277._pm_r, 1.17_pm_r,230._pm_r, &
253.60_pm_r, 43823._pm_r,   9._pm_r, 34._pm_r, 1.41_pm_r,345._pm_r,   5._pm_r,264._pm_r,  0.85_pm_r,217._pm_r, &
257.00_pm_r, 47570._pm_r,  10._pm_r, 26._pm_r, 1.19_pm_r,340._pm_r,   6._pm_r,256._pm_r,  0.51_pm_r,202._pm_r, &
253.80_pm_r, 51313._pm_r,  11._pm_r, 19._pm_r, 1.10_pm_r,315._pm_r,   6._pm_r,251._pm_r,  0.26_pm_r,170._pm_r, &
247.20_pm_r, 54987._pm_r,  12._pm_r, 10._pm_r, 1.33_pm_r,283._pm_r,   6._pm_r,248._pm_r,  0.27_pm_r,121._pm_r, &
240.10_pm_r, 58553._pm_r,  12._pm_r,  1._pm_r, 1.41_pm_r,260._pm_r,   6._pm_r,244._pm_r,  0.38_pm_r,130._pm_r, &
234.50_pm_r, 62028._pm_r,  11._pm_r,350._pm_r, 1.41_pm_r,241._pm_r,   6._pm_r,238._pm_r,  0.40_pm_r,134._pm_r, &
229.30_pm_r, 65423._pm_r,  10._pm_r,340._pm_r, 1.36_pm_r,226._pm_r,   5._pm_r,232._pm_r,  0.32_pm_r,134._pm_r, &
224.40_pm_r, 68746._pm_r,   9._pm_r,330._pm_r, 1.29_pm_r,212._pm_r,   5._pm_r,228._pm_r,  0.18_pm_r,128._pm_r, &
220.40_pm_r, 72002._pm_r,   8._pm_r,319._pm_r, 1.26_pm_r,199._pm_r,   5._pm_r,227._pm_r,  0.05_pm_r, 90._pm_r, &
217.10_pm_r, 75206._pm_r,   8._pm_r,308._pm_r, 1.25_pm_r,189._pm_r,   5._pm_r,227._pm_r,  0.09_pm_r,347._pm_r, &
214.80_pm_r, 78366._pm_r,   7._pm_r,295._pm_r, 1.23_pm_r,183._pm_r,   5._pm_r,229._pm_r,  0.18_pm_r,337._pm_r, &
212.00_pm_r, 81497._pm_r,   6._pm_r,280._pm_r, 1.18_pm_r,178._pm_r,   5._pm_r,232._pm_r,  0.21_pm_r,332._pm_r, &
206.40_pm_r, 84570._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.30_pm_r, 87540._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.70_pm_r, 90382._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.70_pm_r, 93164._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.20_pm_r, 95934._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.60_pm_r, 98728._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
193.20_pm_r,101592._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.70_pm_r,104596._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.00_pm_r,107875._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
269.20_pm_r,111719._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
346.90_pm_r,116642._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
293.50_pm_r,   -53._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
269.90_pm_r,  4075._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
244.30_pm_r,  7842._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.40_pm_r, 11254._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
209.50_pm_r, 14411._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.50_pm_r, 17459._pm_r,   2._pm_r,162._pm_r,  0.20_pm_r, 68._pm_r,   2._pm_r, 80._pm_r,  0.22_pm_r,312._pm_r, &
213.30_pm_r, 20545._pm_r,   2._pm_r,154._pm_r,  0.31_pm_r, 33._pm_r,   1._pm_r, 68._pm_r,  0.33_pm_r,302._pm_r, &
219.50_pm_r, 23718._pm_r,   2._pm_r,141._pm_r,  0.54_pm_r,  6._pm_r,   1._pm_r, 46._pm_r,  0.42_pm_r,290._pm_r, &
223.50_pm_r, 26962._pm_r,   1._pm_r,110._pm_r,  0.88_pm_r,352._pm_r,   1._pm_r,  8._pm_r,  0.51_pm_r,276._pm_r, &
227.50_pm_r, 30263._pm_r,   1._pm_r, 35._pm_r, 1.23_pm_r,345._pm_r,   1._pm_r,321._pm_r,  0.61_pm_r,260._pm_r, &
233.60_pm_r, 33636._pm_r,   3._pm_r,  4._pm_r, 1.45_pm_r,341._pm_r,   2._pm_r,290._pm_r,  0.71_pm_r,245._pm_r, &
242.30_pm_r, 37121._pm_r,   5._pm_r,354._pm_r, 1.41_pm_r,341._pm_r,   3._pm_r,270._pm_r,  0.77_pm_r,231._pm_r, &
252.70_pm_r, 40742._pm_r,   7._pm_r,351._pm_r, 1.14_pm_r,342._pm_r,   4._pm_r,257._pm_r,  0.80_pm_r,219._pm_r, &
260.80_pm_r, 44509._pm_r,   8._pm_r,351._pm_r,  0.96_pm_r,  1._pm_r,   4._pm_r,247._pm_r,  0.66_pm_r,210._pm_r, &
262.40_pm_r, 48347._pm_r,  10._pm_r,353._pm_r,  0.77_pm_r, 14._pm_r,   5._pm_r,241._pm_r,  0.53_pm_r,203._pm_r, &
257.30_pm_r, 52157._pm_r,  10._pm_r,355._pm_r,  0.14_pm_r, 18._pm_r,   6._pm_r,236._pm_r,  0.36_pm_r,182._pm_r, &
248.40_pm_r, 55864._pm_r,  10._pm_r,354._pm_r,  0.88_pm_r,201._pm_r,   6._pm_r,231._pm_r,  0.35_pm_r,121._pm_r, &
240.00_pm_r, 59437._pm_r,   8._pm_r,348._pm_r, 1.61_pm_r,195._pm_r,   5._pm_r,225._pm_r,  0.48_pm_r,103._pm_r, &
232.40_pm_r, 62899._pm_r,   6._pm_r,337._pm_r, 1.85_pm_r,193._pm_r,   5._pm_r,219._pm_r,  0.48_pm_r, 92._pm_r, &
223.70_pm_r, 66237._pm_r,   4._pm_r,316._pm_r, 1.63_pm_r,190._pm_r,   5._pm_r,213._pm_r,  0.33_pm_r, 76._pm_r, &
216.40_pm_r, 69458._pm_r,   3._pm_r,285._pm_r, 1.21_pm_r,188._pm_r,   4._pm_r,211._pm_r,  0.20_pm_r, 32._pm_r, &
212.70_pm_r, 72596._pm_r,   3._pm_r,260._pm_r,  0.77_pm_r,183._pm_r,   4._pm_r,213._pm_r,  0.25_pm_r,336._pm_r, &
209.90_pm_r, 75691._pm_r,   4._pm_r,246._pm_r,  0.41_pm_r,171._pm_r,   4._pm_r,220._pm_r,  0.39_pm_r,314._pm_r, &
208.30_pm_r, 78750._pm_r,   4._pm_r,240._pm_r,  0.20_pm_r,136._pm_r,   4._pm_r,230._pm_r,  0.49_pm_r,307._pm_r, &
206.40_pm_r, 81790._pm_r,   3._pm_r,237._pm_r,  0.18_pm_r, 79._pm_r,   4._pm_r,240._pm_r,  0.54_pm_r,305._pm_r, &
201.80_pm_r, 84790._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.40_pm_r, 87693._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.10_pm_r, 90489._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.50_pm_r, 93246._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.30_pm_r, 96003._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.20_pm_r, 98783._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.60_pm_r,101620._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.20_pm_r,104580._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.80_pm_r,107799._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
267.80_pm_r,111598._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
349.70_pm_r,116545._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
298.00_pm_r,   -68._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
274.70_pm_r,  4131._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
250.10_pm_r,  7979._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.40_pm_r, 11458._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.20_pm_r, 14600._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.40_pm_r, 17552._pm_r,   2._pm_r,132._pm_r,  0.29_pm_r,266._pm_r,   1._pm_r, 67._pm_r,  0.07_pm_r,295._pm_r, &
211.10_pm_r, 20578._pm_r,   2._pm_r,143._pm_r,  0.46_pm_r,288._pm_r,   1._pm_r, 62._pm_r,  0.11_pm_r,270._pm_r, &
219.20_pm_r, 23734._pm_r,   1._pm_r,164._pm_r,  0.68_pm_r,306._pm_r,   1._pm_r, 57._pm_r,  0.17_pm_r,254._pm_r, &
225.20_pm_r, 26988._pm_r,   1._pm_r,259._pm_r,  0.96_pm_r,320._pm_r,   1._pm_r, 51._pm_r,  0.24_pm_r,240._pm_r, &
230.90_pm_r, 30327._pm_r,   2._pm_r,308._pm_r, 1.22_pm_r,329._pm_r,   0._pm_r, 20._pm_r,  0.32_pm_r,232._pm_r, &
239.70_pm_r, 33768._pm_r,   4._pm_r,320._pm_r, 1.32_pm_r,336._pm_r,   1._pm_r,232._pm_r,  0.38_pm_r,225._pm_r, &
249.40_pm_r, 37353._pm_r,   6._pm_r,327._pm_r, 1.24_pm_r,343._pm_r,   1._pm_r,226._pm_r,  0.41_pm_r,219._pm_r, &
259.00_pm_r, 41073._pm_r,   7._pm_r,331._pm_r,  0.97_pm_r,351._pm_r,   2._pm_r,223._pm_r,  0.39_pm_r,212._pm_r, &
265.60_pm_r, 44923._pm_r,   8._pm_r,335._pm_r,  0.71_pm_r,  8._pm_r,   2._pm_r,220._pm_r,  0.32_pm_r,215._pm_r, &
265.60_pm_r, 48820._pm_r,   9._pm_r,339._pm_r,  0.35_pm_r, 46._pm_r,   3._pm_r,220._pm_r,  0.31_pm_r,218._pm_r, &
260.20_pm_r, 52672._pm_r,   9._pm_r,341._pm_r,  0.42_pm_r,150._pm_r,   3._pm_r,218._pm_r,  0.30_pm_r,202._pm_r, &
250.60_pm_r, 56419._pm_r,   8._pm_r,340._pm_r,  0.94_pm_r,178._pm_r,   3._pm_r,214._pm_r,  0.35_pm_r,167._pm_r, &
239.60_pm_r, 60006._pm_r,   6._pm_r,334._pm_r, 1.20_pm_r,180._pm_r,   4._pm_r,207._pm_r,  0.39_pm_r,139._pm_r, &
228.30_pm_r, 63435._pm_r,   5._pm_r,325._pm_r, 1.19_pm_r,178._pm_r,   4._pm_r,198._pm_r,  0.36_pm_r,124._pm_r, &
216.90_pm_r, 66691._pm_r,   3._pm_r,311._pm_r, 1.00_pm_r,171._pm_r,   4._pm_r,192._pm_r,  0.24_pm_r,112._pm_r, &
209.40_pm_r, 69809._pm_r,   2._pm_r,292._pm_r,  0.80_pm_r,156._pm_r,   4._pm_r,189._pm_r,  0.07_pm_r, 74._pm_r, &
206.80_pm_r, 72853._pm_r,   2._pm_r,270._pm_r,  0.67_pm_r,137._pm_r,   4._pm_r,189._pm_r,  0.14_pm_r,327._pm_r, &
205.50_pm_r, 75872._pm_r,   1._pm_r,234._pm_r,  0.64_pm_r,118._pm_r,   4._pm_r,193._pm_r,  0.28_pm_r,311._pm_r, &
205.40_pm_r, 78878._pm_r,   1._pm_r,175._pm_r,  0.66_pm_r,104._pm_r,   4._pm_r,200._pm_r,  0.38_pm_r,307._pm_r, &
204.00_pm_r, 81886._pm_r,   2._pm_r,136._pm_r,  0.65_pm_r, 96._pm_r,   3._pm_r,210._pm_r,  0.42_pm_r,305._pm_r, &
198.80_pm_r, 84843._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.90_pm_r, 87700._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.10_pm_r, 90473._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.40_pm_r, 93224._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.30_pm_r, 95983._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.90_pm_r, 98763._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.50_pm_r,101590._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.60_pm_r,104531._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.70_pm_r,107721._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
267.40_pm_r,111500._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
352.60_pm_r,116474._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
300.80_pm_r,   -91._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
276.70_pm_r,  4145._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.30_pm_r,  8032._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.80_pm_r, 11546._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.00_pm_r, 14677._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.90_pm_r, 17572._pm_r,   2._pm_r,123._pm_r,  0.22_pm_r,282._pm_r,   0._pm_r,298._pm_r,  0.08_pm_r, 88._pm_r, &
209.60_pm_r, 20562._pm_r,   1._pm_r,128._pm_r,  0.35_pm_r,290._pm_r,   0._pm_r,357._pm_r,  0.10_pm_r, 99._pm_r, &
218.60_pm_r, 23702._pm_r,   1._pm_r,138._pm_r,  0.45_pm_r,299._pm_r,   0._pm_r, 81._pm_r,  0.10_pm_r,117._pm_r, &
225.60_pm_r, 26953._pm_r,   0._pm_r,212._pm_r,  0.55_pm_r,307._pm_r,   0._pm_r,105._pm_r,  0.10_pm_r,141._pm_r, &
233.20_pm_r, 30311._pm_r,   1._pm_r,297._pm_r,  0.61_pm_r,314._pm_r,   0._pm_r,124._pm_r,  0.13_pm_r,166._pm_r, &
243.50_pm_r, 33798._pm_r,   2._pm_r,307._pm_r,  0.59_pm_r,322._pm_r,   1._pm_r,141._pm_r,  0.15_pm_r,188._pm_r, &
253.00_pm_r, 37438._pm_r,   3._pm_r,313._pm_r,  0.50_pm_r,329._pm_r,   1._pm_r,157._pm_r,  0.16_pm_r,203._pm_r, &
261.00_pm_r, 41202._pm_r,   3._pm_r,317._pm_r,  0.36_pm_r,343._pm_r,   1._pm_r,170._pm_r,  0.17_pm_r,216._pm_r, &
266.20_pm_r, 45066._pm_r,   4._pm_r,322._pm_r,  0.35_pm_r,360._pm_r,   1._pm_r,181._pm_r,  0.14_pm_r,234._pm_r, &
267.10_pm_r, 48977._pm_r,   4._pm_r,327._pm_r,  0.31_pm_r,358._pm_r,   1._pm_r,189._pm_r,  0.16_pm_r,225._pm_r, &
262.10_pm_r, 52856._pm_r,   4._pm_r,328._pm_r,  0.07_pm_r,324._pm_r,   1._pm_r,192._pm_r,  0.24_pm_r,192._pm_r, &
252.30_pm_r, 56630._pm_r,   4._pm_r,327._pm_r,  0.39_pm_r,176._pm_r,   2._pm_r,190._pm_r,  0.40_pm_r,178._pm_r, &
239.30_pm_r, 60229._pm_r,   3._pm_r,321._pm_r,  0.76_pm_r,161._pm_r,   3._pm_r,187._pm_r,  0.43_pm_r,179._pm_r, &
224.70_pm_r, 63631._pm_r,   2._pm_r,311._pm_r,  0.92_pm_r,153._pm_r,   3._pm_r,186._pm_r,  0.39_pm_r,186._pm_r, &
212.70_pm_r, 66827._pm_r,   1._pm_r,283._pm_r,  0.86_pm_r,145._pm_r,   4._pm_r,187._pm_r,  0.31_pm_r,203._pm_r, &
205.60_pm_r, 69886._pm_r,   1._pm_r,186._pm_r,  0.71_pm_r,134._pm_r,   4._pm_r,190._pm_r,  0.27_pm_r,232._pm_r, &
204.10_pm_r, 72883._pm_r,   1._pm_r,151._pm_r,  0.58_pm_r,118._pm_r,   4._pm_r,195._pm_r,  0.31_pm_r,256._pm_r, &
205.30_pm_r, 75878._pm_r,   2._pm_r,136._pm_r,  0.52_pm_r,100._pm_r,   4._pm_r,201._pm_r,  0.37_pm_r,270._pm_r, &
207.10_pm_r, 78901._pm_r,   3._pm_r,125._pm_r,  0.51_pm_r, 86._pm_r,   5._pm_r,208._pm_r,  0.41_pm_r,278._pm_r, &
205.20_pm_r, 81949._pm_r,   3._pm_r,116._pm_r,  0.50_pm_r, 76._pm_r,   5._pm_r,214._pm_r,  0.42_pm_r,281._pm_r, &
198.00_pm_r, 84899._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.90_pm_r, 87734._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.20_pm_r, 90503._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.90_pm_r, 93261._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.90_pm_r, 96030._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 98819._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.80_pm_r,101653._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.90_pm_r,104599._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.20_pm_r,107799._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
268.90_pm_r,111595._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
355.80_pm_r,116608._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
301.40_pm_r,  -114._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.10_pm_r,  4130._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
254.10_pm_r,  8027._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.70_pm_r, 11555._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.70_pm_r, 14690._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.00_pm_r, 17578._pm_r,   2._pm_r,134._pm_r,  0.19_pm_r,267._pm_r,   0._pm_r,234._pm_r,  0.12_pm_r, 88._pm_r, &
208.80_pm_r, 20560._pm_r,   1._pm_r,140._pm_r,  0.27_pm_r,272._pm_r,   0._pm_r,130._pm_r,  0.15_pm_r, 94._pm_r, &
218.10_pm_r, 23689._pm_r,   1._pm_r,153._pm_r,  0.32_pm_r,277._pm_r,   0._pm_r,108._pm_r,  0.16_pm_r,103._pm_r, &
225.30_pm_r, 26931._pm_r,   1._pm_r,179._pm_r,  0.36_pm_r,284._pm_r,   0._pm_r,110._pm_r,  0.16_pm_r,117._pm_r, &
233.10_pm_r, 30283._pm_r,   1._pm_r,221._pm_r,  0.35_pm_r,292._pm_r,   1._pm_r,118._pm_r,  0.14_pm_r,136._pm_r, &
244.70_pm_r, 33774._pm_r,   1._pm_r,252._pm_r,  0.31_pm_r,302._pm_r,   1._pm_r,127._pm_r,  0.14_pm_r,163._pm_r, &
254.20_pm_r, 37430._pm_r,   1._pm_r,267._pm_r,  0.23_pm_r,317._pm_r,   1._pm_r,139._pm_r,  0.14_pm_r,188._pm_r, &
261.30_pm_r, 41203._pm_r,   2._pm_r,277._pm_r,  0.16_pm_r,346._pm_r,   1._pm_r,151._pm_r,  0.15_pm_r,212._pm_r, &
266.30_pm_r, 45073._pm_r,   2._pm_r,287._pm_r,  0.20_pm_r, 13._pm_r,   1._pm_r,161._pm_r,  0.09_pm_r,225._pm_r, &
267.10_pm_r, 48985._pm_r,   2._pm_r,298._pm_r,  0.13_pm_r, 18._pm_r,   1._pm_r,168._pm_r,  0.05_pm_r,259._pm_r, &
262.60_pm_r, 52866._pm_r,   2._pm_r,303._pm_r,  0.11_pm_r,177._pm_r,   1._pm_r,175._pm_r,  0.06_pm_r,215._pm_r, &
253.00_pm_r, 56642._pm_r,   1._pm_r,295._pm_r,  0.39_pm_r,179._pm_r,   1._pm_r,182._pm_r,  0.24_pm_r,189._pm_r, &
239.90_pm_r, 60247._pm_r,   1._pm_r,263._pm_r,  0.50_pm_r,164._pm_r,   2._pm_r,186._pm_r,  0.39_pm_r,192._pm_r, &
225.20_pm_r, 63660._pm_r,   1._pm_r,217._pm_r,  0.46_pm_r,148._pm_r,   2._pm_r,187._pm_r,  0.47_pm_r,193._pm_r, &
212.60_pm_r, 66867._pm_r,   2._pm_r,191._pm_r,  0.36_pm_r,122._pm_r,   3._pm_r,187._pm_r,  0.45_pm_r,196._pm_r, &
205.00_pm_r, 69932._pm_r,   2._pm_r,176._pm_r,  0.34_pm_r, 88._pm_r,   3._pm_r,186._pm_r,  0.38_pm_r,198._pm_r, &
203.50_pm_r, 72930._pm_r,   2._pm_r,165._pm_r,  0.42_pm_r, 60._pm_r,   4._pm_r,185._pm_r,  0.29_pm_r,205._pm_r, &
205.70_pm_r, 75929._pm_r,   3._pm_r,153._pm_r,  0.53_pm_r, 46._pm_r,   4._pm_r,185._pm_r,  0.23_pm_r,214._pm_r, &
208.20_pm_r, 78962._pm_r,   3._pm_r,141._pm_r,  0.61_pm_r, 38._pm_r,   4._pm_r,184._pm_r,  0.20_pm_r,218._pm_r, &
205.80_pm_r, 82020._pm_r,   3._pm_r,128._pm_r,  0.64_pm_r, 35._pm_r,   4._pm_r,183._pm_r,  0.16_pm_r,222._pm_r, &
197.60_pm_r, 84965._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.60_pm_r, 87792._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.60_pm_r, 90564._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 93328._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 96102._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.00_pm_r, 98900._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.10_pm_r,101749._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.10_pm_r,104723._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.30_pm_r,107964._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
272.60_pm_r,111814._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
359.70_pm_r,116887._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
301.30_pm_r,  -104._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.30_pm_r,  4140._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.70_pm_r,  8035._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.40_pm_r, 11557._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.30_pm_r, 14694._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.00_pm_r, 17592._pm_r,   3._pm_r,159._pm_r,  0.26_pm_r,304._pm_r,   1._pm_r,120._pm_r,  0.11_pm_r,  8._pm_r, &
209.60_pm_r, 20582._pm_r,   2._pm_r,165._pm_r,  0.35_pm_r,303._pm_r,   1._pm_r,105._pm_r,  0.12_pm_r,  5._pm_r, &
218.20_pm_r, 23717._pm_r,   2._pm_r,177._pm_r,  0.40_pm_r,300._pm_r,   1._pm_r, 89._pm_r,  0.10_pm_r,354._pm_r, &
225.00_pm_r, 26962._pm_r,   2._pm_r,196._pm_r,  0.43_pm_r,298._pm_r,   1._pm_r, 75._pm_r,  0.08_pm_r,328._pm_r, &
232.20_pm_r, 30308._pm_r,   2._pm_r,217._pm_r,  0.39_pm_r,296._pm_r,   1._pm_r, 67._pm_r,  0.07_pm_r,279._pm_r, &
243.90_pm_r, 33789._pm_r,   2._pm_r,234._pm_r,  0.30_pm_r,295._pm_r,   0._pm_r, 64._pm_r,  0.10_pm_r,242._pm_r, &
253.80_pm_r, 37439._pm_r,   2._pm_r,243._pm_r,  0.19_pm_r,298._pm_r,   0._pm_r, 73._pm_r,  0.12_pm_r,225._pm_r, &
261.60_pm_r, 41212._pm_r,   2._pm_r,247._pm_r,  0.07_pm_r,321._pm_r,   0._pm_r,128._pm_r,  0.13_pm_r,214._pm_r, &
266.80_pm_r, 45087._pm_r,   2._pm_r,251._pm_r,  0.13_pm_r, 20._pm_r,   0._pm_r,175._pm_r,  0.10_pm_r,200._pm_r, &
267.10_pm_r, 49003._pm_r,   2._pm_r,256._pm_r,  0.20_pm_r, 40._pm_r,   0._pm_r,182._pm_r,  0.04_pm_r,187._pm_r, &
260.60_pm_r, 52871._pm_r,   2._pm_r,259._pm_r,  0.18_pm_r, 81._pm_r,   0._pm_r,179._pm_r,  0.06_pm_r, 38._pm_r, &
251.00_pm_r, 56622._pm_r,   1._pm_r,251._pm_r,  0.32_pm_r,141._pm_r,   0._pm_r,156._pm_r,  0.14_pm_r, 34._pm_r, &
240.30_pm_r, 60220._pm_r,   1._pm_r,225._pm_r,  0.49_pm_r,152._pm_r,   0._pm_r,103._pm_r,  0.14_pm_r, 61._pm_r, &
227.90_pm_r, 63653._pm_r,   2._pm_r,201._pm_r,  0.55_pm_r,152._pm_r,   0._pm_r, 96._pm_r,  0.15_pm_r,116._pm_r, &
216.80_pm_r, 66904._pm_r,   2._pm_r,186._pm_r,  0.49_pm_r,148._pm_r,   1._pm_r,113._pm_r,  0.28_pm_r,149._pm_r, &
209.10_pm_r, 70020._pm_r,   3._pm_r,177._pm_r,  0.39_pm_r,137._pm_r,   1._pm_r,132._pm_r,  0.43_pm_r,161._pm_r, &
206.00_pm_r, 73055._pm_r,   3._pm_r,170._pm_r,  0.32_pm_r,117._pm_r,   2._pm_r,145._pm_r,  0.56_pm_r,167._pm_r, &
205.80_pm_r, 76067._pm_r,   3._pm_r,164._pm_r,  0.28_pm_r, 98._pm_r,   3._pm_r,153._pm_r,  0.66_pm_r,170._pm_r, &
206.40_pm_r, 79088._pm_r,   4._pm_r,158._pm_r,  0.28_pm_r, 83._pm_r,   4._pm_r,158._pm_r,  0.73_pm_r,171._pm_r, &
203.60_pm_r, 82116._pm_r,   4._pm_r,151._pm_r,  0.28_pm_r, 74._pm_r,   5._pm_r,161._pm_r,  0.74_pm_r,171._pm_r, &
196.20_pm_r, 85036._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.00_pm_r, 87851._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.00_pm_r, 90615._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.40_pm_r, 93367._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.70_pm_r, 96130._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.40_pm_r, 98926._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
193.00_pm_r,101792._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.70_pm_r,104806._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.60_pm_r,108116._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
278.30_pm_r,112054._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
364.10_pm_r,117205._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
299.20_pm_r,  -124._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
276.60_pm_r,  4098._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
251.80_pm_r,  7972._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.30_pm_r, 11470._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.10_pm_r, 14618._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.40_pm_r, 17568._pm_r,   3._pm_r,146._pm_r,  0.60_pm_r,280._pm_r,   1._pm_r,173._pm_r,  0.34_pm_r, 18._pm_r, &
211.40_pm_r, 20596._pm_r,   3._pm_r,162._pm_r,  0.77_pm_r,280._pm_r,   1._pm_r,157._pm_r,  0.44_pm_r, 16._pm_r, &
219.60_pm_r, 23755._pm_r,   2._pm_r,187._pm_r,  0.79_pm_r,282._pm_r,   1._pm_r,103._pm_r,  0.45_pm_r, 15._pm_r, &
226.20_pm_r, 27019._pm_r,   3._pm_r,214._pm_r,  0.72_pm_r,284._pm_r,   1._pm_r, 51._pm_r,  0.42_pm_r, 12._pm_r, &
233.20_pm_r, 30384._pm_r,   3._pm_r,232._pm_r,  0.55_pm_r,287._pm_r,   1._pm_r, 35._pm_r,  0.32_pm_r,  9._pm_r, &
242.90_pm_r, 33866._pm_r,   3._pm_r,241._pm_r,  0.32_pm_r,292._pm_r,   2._pm_r, 29._pm_r,  0.20_pm_r,  0._pm_r, &
252.70_pm_r, 37498._pm_r,   4._pm_r,245._pm_r,  0.11_pm_r,307._pm_r,   2._pm_r, 25._pm_r,  0.08_pm_r,343._pm_r, &
261.50_pm_r, 41262._pm_r,   4._pm_r,246._pm_r,  0.06_pm_r, 99._pm_r,   2._pm_r, 23._pm_r,  0.04_pm_r,236._pm_r, &
267.90_pm_r, 45144._pm_r,   4._pm_r,245._pm_r,  0.04_pm_r,187._pm_r,   2._pm_r, 22._pm_r,  0.06_pm_r,198._pm_r, &
268.70_pm_r, 49081._pm_r,   4._pm_r,245._pm_r,  0.16_pm_r,263._pm_r,   2._pm_r, 24._pm_r,  0.07_pm_r,146._pm_r, &
259.70_pm_r, 52955._pm_r,   4._pm_r,247._pm_r,  0.23_pm_r,279._pm_r,   2._pm_r, 29._pm_r,  0.14_pm_r, 92._pm_r, &
248.90_pm_r, 56680._pm_r,   4._pm_r,249._pm_r,  0.14_pm_r,306._pm_r,   2._pm_r, 35._pm_r,  0.23_pm_r, 70._pm_r, &
239.80_pm_r, 60256._pm_r,   4._pm_r,250._pm_r,  0.16_pm_r, 76._pm_r,   2._pm_r, 40._pm_r,  0.18_pm_r, 71._pm_r, &
230.30_pm_r, 63701._pm_r,   4._pm_r,249._pm_r,  0.36_pm_r, 93._pm_r,   2._pm_r, 44._pm_r,  0.10_pm_r,119._pm_r, &
220.90_pm_r, 67001._pm_r,   3._pm_r,244._pm_r,  0.47_pm_r, 98._pm_r,   2._pm_r, 48._pm_r,  0.21_pm_r,181._pm_r, &
213.20_pm_r, 70180._pm_r,   3._pm_r,234._pm_r,  0.51_pm_r,101._pm_r,   2._pm_r, 57._pm_r,  0.40_pm_r,194._pm_r, &
208.20_pm_r, 73261._pm_r,   2._pm_r,218._pm_r,  0.52_pm_r,104._pm_r,   1._pm_r, 76._pm_r,  0.57_pm_r,197._pm_r, &
205.10_pm_r, 76286._pm_r,   2._pm_r,198._pm_r,  0.51_pm_r,106._pm_r,   1._pm_r,116._pm_r,  0.68_pm_r,199._pm_r, &
204.10_pm_r, 79278._pm_r,   2._pm_r,177._pm_r,  0.50_pm_r,108._pm_r,   2._pm_r,154._pm_r,  0.76_pm_r,200._pm_r, &
200.70_pm_r, 82262._pm_r,   2._pm_r,161._pm_r,  0.46_pm_r,109._pm_r,   3._pm_r,172._pm_r,  0.77_pm_r,200._pm_r, &
193.60_pm_r, 85142._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.50_pm_r, 87921._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.20_pm_r, 90646._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.70_pm_r, 93356._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.80_pm_r, 96081._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.10_pm_r, 98857._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
195.10_pm_r,101735._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.20_pm_r,104798._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
233.60_pm_r,108196._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
285.50_pm_r,112248._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
368.80_pm_r,117488._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
294.60_pm_r,   -95._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
273.20_pm_r,  4065._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
247.40_pm_r,  7879._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.60_pm_r, 11322._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
209.10_pm_r, 14485._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.20_pm_r, 17525._pm_r,   4._pm_r,158._pm_r,  0.89_pm_r,294._pm_r,   1._pm_r,179._pm_r,  0.36_pm_r, 31._pm_r, &
213.80_pm_r, 20613._pm_r,   3._pm_r,175._pm_r, 1.09_pm_r,294._pm_r,   1._pm_r,157._pm_r,  0.47_pm_r, 29._pm_r, &
219.70_pm_r, 23789._pm_r,   3._pm_r,203._pm_r, 1.08_pm_r,294._pm_r,   1._pm_r,106._pm_r,  0.50_pm_r, 27._pm_r, &
226.20_pm_r, 27051._pm_r,   3._pm_r,229._pm_r,  0.91_pm_r,293._pm_r,   1._pm_r, 65._pm_r,  0.49_pm_r, 24._pm_r, &
233.30_pm_r, 30417._pm_r,   4._pm_r,244._pm_r,  0.59_pm_r,291._pm_r,   2._pm_r, 49._pm_r,  0.41_pm_r, 19._pm_r, &
243.10_pm_r, 33900._pm_r,   4._pm_r,249._pm_r,  0.23_pm_r,280._pm_r,   2._pm_r, 41._pm_r,  0.29_pm_r, 11._pm_r, &
253.50_pm_r, 37539._pm_r,   5._pm_r,249._pm_r,  0.10_pm_r,173._pm_r,   2._pm_r, 37._pm_r,  0.18_pm_r,357._pm_r, &
263.50_pm_r, 41322._pm_r,   5._pm_r,246._pm_r,  0.22_pm_r,148._pm_r,   3._pm_r, 33._pm_r,  0.10_pm_r,332._pm_r, &
270.20_pm_r, 45238._pm_r,   5._pm_r,243._pm_r,  0.10_pm_r,191._pm_r,   3._pm_r, 31._pm_r,  0.06_pm_r,342._pm_r, &
269.90_pm_r, 49202._pm_r,   5._pm_r,244._pm_r,  0.30_pm_r,282._pm_r,   3._pm_r, 31._pm_r,  0.13_pm_r, 45._pm_r, &
260.80_pm_r, 53091._pm_r,   5._pm_r,249._pm_r,  0.50_pm_r,292._pm_r,   3._pm_r, 33._pm_r,  0.21_pm_r, 55._pm_r, &
249.60_pm_r, 56830._pm_r,   6._pm_r,254._pm_r,  0.39_pm_r,298._pm_r,   3._pm_r, 35._pm_r,  0.18_pm_r, 53._pm_r, &
240.10_pm_r, 60413._pm_r,   6._pm_r,256._pm_r,  0.08_pm_r,330._pm_r,   3._pm_r, 35._pm_r,  0.02_pm_r, 49._pm_r, &
231.10_pm_r, 63865._pm_r,   6._pm_r,257._pm_r,  0.24_pm_r, 79._pm_r,   3._pm_r, 35._pm_r,  0.12_pm_r,223._pm_r, &
222.50_pm_r, 67183._pm_r,   5._pm_r,256._pm_r,  0.45_pm_r, 77._pm_r,   3._pm_r, 35._pm_r,  0.20_pm_r,218._pm_r, &
214.60_pm_r, 70385._pm_r,   5._pm_r,257._pm_r,  0.61_pm_r, 72._pm_r,   3._pm_r, 35._pm_r,  0.25_pm_r,210._pm_r, &
208.20_pm_r, 73478._pm_r,   4._pm_r,259._pm_r,  0.69_pm_r, 66._pm_r,   2._pm_r, 36._pm_r,  0.28_pm_r,204._pm_r, &
203.50_pm_r, 76493._pm_r,   3._pm_r,265._pm_r,  0.73_pm_r, 62._pm_r,   2._pm_r, 40._pm_r,  0.29_pm_r,199._pm_r, &
200.30_pm_r, 79448._pm_r,   2._pm_r,281._pm_r,  0.75_pm_r, 60._pm_r,   2._pm_r, 46._pm_r,  0.29_pm_r,195._pm_r, &
195.40_pm_r, 82359._pm_r,   1._pm_r,324._pm_r,  0.71_pm_r, 58._pm_r,   1._pm_r, 56._pm_r,  0.28_pm_r,194._pm_r, &
188.10_pm_r, 85161._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.20_pm_r, 87861._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.80_pm_r, 90504._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.80_pm_r, 93136._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.40_pm_r, 95798._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.90_pm_r, 98538._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.20_pm_r,101421._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
213.50_pm_r,104539._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
242.10_pm_r,108044._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
293.00_pm_r,112225._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
373.00_pm_r,117551._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)
       


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
       
288.00_pm_r,   -38._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
267.90_pm_r,  4031._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
241.70_pm_r,  7762._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.90_pm_r, 11141._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.40_pm_r, 14320._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.60_pm_r, 17458._pm_r,   4._pm_r,201._pm_r,  0.32_pm_r,293._pm_r,   1._pm_r, 99._pm_r,  0.10_pm_r,313._pm_r, &
216.70_pm_r, 20614._pm_r,   4._pm_r,210._pm_r,  0.39_pm_r,293._pm_r,   1._pm_r, 94._pm_r,  0.14_pm_r,315._pm_r, &
219.80_pm_r, 23809._pm_r,   4._pm_r,219._pm_r,  0.39_pm_r,290._pm_r,   1._pm_r, 85._pm_r,  0.18_pm_r,316._pm_r, &
225.20_pm_r, 27064._pm_r,   4._pm_r,226._pm_r,  0.32_pm_r,286._pm_r,   1._pm_r, 70._pm_r,  0.20_pm_r,319._pm_r, &
232.30_pm_r, 30413._pm_r,   4._pm_r,231._pm_r,  0.20_pm_r,279._pm_r,   1._pm_r, 51._pm_r,  0.20_pm_r,323._pm_r, &
243.70_pm_r, 33890._pm_r,   4._pm_r,233._pm_r,  0.09_pm_r,249._pm_r,   1._pm_r, 33._pm_r,  0.18_pm_r,326._pm_r, &
256.20_pm_r, 37555._pm_r,   4._pm_r,232._pm_r,  0.06_pm_r,180._pm_r,   1._pm_r, 21._pm_r,  0.14_pm_r,333._pm_r, &
267.20_pm_r, 41387._pm_r,   4._pm_r,231._pm_r,  0.07_pm_r,172._pm_r,   1._pm_r, 15._pm_r,  0.10_pm_r,343._pm_r, &
273.20_pm_r, 45354._pm_r,   5._pm_r,231._pm_r,  0.07_pm_r,250._pm_r,   1._pm_r, 14._pm_r,  0.06_pm_r, 48._pm_r, &
271.60_pm_r, 49352._pm_r,   5._pm_r,232._pm_r,  0.13_pm_r,261._pm_r,   1._pm_r, 19._pm_r,  0.12_pm_r,110._pm_r, &
263.50_pm_r, 53273._pm_r,   5._pm_r,233._pm_r,  0.11_pm_r,259._pm_r,   1._pm_r, 27._pm_r,  0.13_pm_r,146._pm_r, &
251.20_pm_r, 57048._pm_r,   5._pm_r,233._pm_r,  0.02_pm_r,180._pm_r,   1._pm_r, 32._pm_r,  0.14_pm_r,214._pm_r, &
240.40_pm_r, 60640._pm_r,   5._pm_r,233._pm_r,  0.06_pm_r, 39._pm_r,   1._pm_r, 22._pm_r,  0.25_pm_r,266._pm_r, &
231.70_pm_r, 64099._pm_r,   5._pm_r,234._pm_r,  0.15_pm_r,  4._pm_r,   1._pm_r,355._pm_r,  0.34_pm_r,291._pm_r, &
222.70_pm_r, 67424._pm_r,   5._pm_r,237._pm_r,  0.24_pm_r,344._pm_r,   1._pm_r,336._pm_r,  0.37_pm_r,311._pm_r, &
214.20_pm_r, 70625._pm_r,   5._pm_r,242._pm_r,  0.32_pm_r,334._pm_r,   2._pm_r,331._pm_r,  0.39_pm_r,329._pm_r, &
206.60_pm_r, 73703._pm_r,   5._pm_r,249._pm_r,  0.38_pm_r,328._pm_r,   2._pm_r,332._pm_r,  0.41_pm_r,344._pm_r, &
199.30_pm_r, 76678._pm_r,   5._pm_r,256._pm_r,  0.42_pm_r,325._pm_r,   3._pm_r,336._pm_r,  0.41_pm_r,354._pm_r, &
192.60_pm_r, 79543._pm_r,   5._pm_r,262._pm_r,  0.43_pm_r,322._pm_r,   4._pm_r,340._pm_r,  0.42_pm_r,  2._pm_r, &
185.80_pm_r, 82317._pm_r,   5._pm_r,268._pm_r,  0.40_pm_r,320._pm_r,   4._pm_r,343._pm_r,  0.39_pm_r,  6._pm_r, &
179.00_pm_r, 84984._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
174.00_pm_r, 87558._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
171.10_pm_r, 90082._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
172.40_pm_r, 92608._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
177.10_pm_r, 95188._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.50_pm_r, 97882._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.30_pm_r,100767._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.70_pm_r,103946._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
251.50_pm_r,107572._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
299.80_pm_r,111885._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
376.80_pm_r,117282._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
281.70_pm_r,   -20._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
262.00_pm_r,  3956._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
236.70_pm_r,  7603._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.80_pm_r, 10949._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.30_pm_r, 14168._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.70_pm_r, 17383._pm_r,   4._pm_r,283._pm_r,  0.58_pm_r,174._pm_r,   3._pm_r, 86._pm_r,  0.28_pm_r,298._pm_r, &
220.40_pm_r, 20605._pm_r,   4._pm_r,268._pm_r,  0.78_pm_r,174._pm_r,   2._pm_r, 79._pm_r,  0.38_pm_r,297._pm_r, &
221.50_pm_r, 23837._pm_r,   4._pm_r,249._pm_r,  0.87_pm_r,175._pm_r,   2._pm_r, 67._pm_r,  0.42_pm_r,296._pm_r, &
225.60_pm_r, 27106._pm_r,   4._pm_r,233._pm_r,  0.80_pm_r,176._pm_r,   1._pm_r, 50._pm_r,  0.37_pm_r,293._pm_r, &
232.30_pm_r, 30457._pm_r,   5._pm_r,223._pm_r,  0.58_pm_r,178._pm_r,   1._pm_r, 31._pm_r,  0.27_pm_r,290._pm_r, &
244.30_pm_r, 33939._pm_r,   5._pm_r,218._pm_r,  0.28_pm_r,186._pm_r,   1._pm_r, 17._pm_r,  0.13_pm_r,283._pm_r, &
257.10_pm_r, 37615._pm_r,   6._pm_r,218._pm_r,  0.07_pm_r,262._pm_r,   1._pm_r, 13._pm_r,  0.02_pm_r,246._pm_r, &
268.50_pm_r, 41462._pm_r,   6._pm_r,219._pm_r,  0.21_pm_r,333._pm_r,   1._pm_r, 14._pm_r,  0.05_pm_r,107._pm_r, &
275.00_pm_r, 45452._pm_r,   6._pm_r,223._pm_r,  0.22_pm_r,313._pm_r,   1._pm_r, 17._pm_r,  0.06_pm_r, 45._pm_r, &
273.70_pm_r, 49477._pm_r,   6._pm_r,225._pm_r,  0.26_pm_r,255._pm_r,   1._pm_r, 16._pm_r,  0.15_pm_r,345._pm_r, &
265.90_pm_r, 53431._pm_r,   6._pm_r,226._pm_r,  0.42_pm_r,228._pm_r,   2._pm_r, 10._pm_r,  0.21_pm_r,333._pm_r, &
254.60_pm_r, 57247._pm_r,   7._pm_r,226._pm_r,  0.41_pm_r,226._pm_r,   2._pm_r,  5._pm_r,  0.11_pm_r,342._pm_r, &
243.80_pm_r, 60891._pm_r,   7._pm_r,227._pm_r,  0.14_pm_r,280._pm_r,   2._pm_r,  6._pm_r,  0.11_pm_r, 95._pm_r, &
234.00_pm_r, 64392._pm_r,   7._pm_r,229._pm_r,  0.31_pm_r,  9._pm_r,   2._pm_r, 14._pm_r,  0.21_pm_r,104._pm_r, &
224.40_pm_r, 67746._pm_r,   7._pm_r,232._pm_r,  0.55_pm_r, 22._pm_r,   2._pm_r, 24._pm_r,  0.24_pm_r, 95._pm_r, &
215.50_pm_r, 70968._pm_r,   6._pm_r,236._pm_r,  0.69_pm_r, 29._pm_r,   2._pm_r, 32._pm_r,  0.23_pm_r, 77._pm_r, &
206.40_pm_r, 74056._pm_r,   5._pm_r,241._pm_r,  0.75_pm_r, 32._pm_r,   2._pm_r, 37._pm_r,  0.21_pm_r, 60._pm_r, &
195.90_pm_r, 77006._pm_r,   4._pm_r,249._pm_r,  0.74_pm_r, 34._pm_r,   3._pm_r, 39._pm_r,  0.22_pm_r, 44._pm_r, &
185.50_pm_r, 79793._pm_r,   3._pm_r,261._pm_r,  0.70_pm_r, 35._pm_r,   3._pm_r, 39._pm_r,  0.21_pm_r, 35._pm_r, &
176.00_pm_r, 82438._pm_r,   2._pm_r,278._pm_r,  0.62_pm_r, 36._pm_r,   3._pm_r, 38._pm_r,  0.19_pm_r, 28._pm_r, &
168.40_pm_r, 84947._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
164.10_pm_r, 87366._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
162.50_pm_r, 89751._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
165.50_pm_r, 92161._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
172.50_pm_r, 94653._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.90_pm_r, 97296._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.60_pm_r,100183._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.80_pm_r,103428._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
261.20_pm_r,107185._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
304.90_pm_r,111618._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
379.80_pm_r,117063._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
       
277.50_pm_r,   -28._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
257.20_pm_r,  3879._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
232.80_pm_r,  7460._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.30_pm_r, 10793._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.30_pm_r, 14056._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.80_pm_r, 17330._pm_r,   5._pm_r,299._pm_r,  0.61_pm_r,170._pm_r,   3._pm_r, 73._pm_r,  0.21_pm_r,291._pm_r, &
224.10_pm_r, 20610._pm_r,   5._pm_r,290._pm_r,  0.84_pm_r,168._pm_r,   2._pm_r, 68._pm_r,  0.27_pm_r,287._pm_r, &
223.80_pm_r, 23887._pm_r,   4._pm_r,274._pm_r,  0.95_pm_r,165._pm_r,   2._pm_r, 60._pm_r,  0.31_pm_r,282._pm_r, &
226.70_pm_r, 27180._pm_r,   4._pm_r,255._pm_r,  0.88_pm_r,161._pm_r,   2._pm_r, 51._pm_r,  0.27_pm_r,272._pm_r, &
233.70_pm_r, 30548._pm_r,   4._pm_r,238._pm_r,  0.66_pm_r,154._pm_r,   1._pm_r, 44._pm_r,  0.20_pm_r,254._pm_r, &
245.20_pm_r, 34048._pm_r,   4._pm_r,227._pm_r,  0.37_pm_r,139._pm_r,   1._pm_r, 40._pm_r,  0.14_pm_r,225._pm_r, &
257.30_pm_r, 37731._pm_r,   4._pm_r,222._pm_r,  0.18_pm_r, 98._pm_r,   1._pm_r, 43._pm_r,  0.12_pm_r,187._pm_r, &
269.00_pm_r, 41583._pm_r,   4._pm_r,221._pm_r,  0.18_pm_r, 34._pm_r,   1._pm_r, 50._pm_r,  0.10_pm_r,156._pm_r, &
276.20_pm_r, 45585._pm_r,   4._pm_r,223._pm_r,  0.13_pm_r,301._pm_r,   1._pm_r, 56._pm_r,  0.10_pm_r, 78._pm_r, &
275.40_pm_r, 49632._pm_r,   4._pm_r,226._pm_r,  0.28_pm_r,249._pm_r,   1._pm_r, 55._pm_r,  0.22_pm_r, 34._pm_r, &
268.10_pm_r, 53615._pm_r,   4._pm_r,227._pm_r,  0.37_pm_r,235._pm_r,   2._pm_r, 48._pm_r,  0.24_pm_r, 15._pm_r, &
257.30_pm_r, 57467._pm_r,   5._pm_r,228._pm_r,  0.28_pm_r,243._pm_r,   2._pm_r, 41._pm_r,  0.13_pm_r,333._pm_r, &
246.10_pm_r, 61149._pm_r,   5._pm_r,230._pm_r,  0.16_pm_r,333._pm_r,   2._pm_r, 36._pm_r,  0.18_pm_r,247._pm_r, &
236.10_pm_r, 64682._pm_r,   5._pm_r,234._pm_r,  0.44_pm_r, 16._pm_r,   1._pm_r, 31._pm_r,  0.33_pm_r,227._pm_r, &
226.80_pm_r, 68068._pm_r,   4._pm_r,240._pm_r,  0.71_pm_r, 28._pm_r,   1._pm_r, 23._pm_r,  0.38_pm_r,220._pm_r, &
217.50_pm_r, 71324._pm_r,   3._pm_r,251._pm_r,  0.90_pm_r, 33._pm_r,   0._pm_r,350._pm_r,  0.37_pm_r,216._pm_r, &
206.60_pm_r, 74430._pm_r,   2._pm_r,274._pm_r,  0.98_pm_r, 36._pm_r,   0._pm_r,249._pm_r,  0.33_pm_r,216._pm_r, &
193.50_pm_r, 77365._pm_r,   2._pm_r,318._pm_r,  0.97_pm_r, 38._pm_r,   1._pm_r,231._pm_r,  0.28_pm_r,215._pm_r, &
180.80_pm_r, 80098._pm_r,   2._pm_r,353._pm_r,  0.90_pm_r, 40._pm_r,   1._pm_r,225._pm_r,  0.23_pm_r,213._pm_r, &
168.60_pm_r, 82658._pm_r,   3._pm_r,  9._pm_r,  0.76_pm_r, 40._pm_r,   2._pm_r,223._pm_r,  0.17_pm_r,211._pm_r, &
158.70_pm_r, 85018._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
154.40_pm_r, 87282._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
154.30_pm_r, 89535._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
159.00_pm_r, 91835._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
168.00_pm_r, 94243._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.30_pm_r, 96837._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.70_pm_r, 99725._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
234.10_pm_r,103038._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
270.40_pm_r,106925._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
307.70_pm_r,111456._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
382.10_pm_r,116924._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
269.30_pm_r,    17._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.40_pm_r,  3834._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
230.20_pm_r,  7365._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.30_pm_r, 10691._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.20_pm_r, 13988._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
227.00_pm_r, 17306._pm_r,   4._pm_r,290._pm_r,  0.12_pm_r,170._pm_r,   2._pm_r, 44._pm_r,  0.12_pm_r,195._pm_r, &
227.50_pm_r, 20635._pm_r,   4._pm_r,287._pm_r,  0.23_pm_r,162._pm_r,   2._pm_r, 47._pm_r,  0.18_pm_r,199._pm_r, &
226.50_pm_r, 23956._pm_r,   3._pm_r,281._pm_r,  0.37_pm_r,156._pm_r,   1._pm_r, 52._pm_r,  0.24_pm_r,203._pm_r, &
228.30_pm_r, 27280._pm_r,   3._pm_r,271._pm_r,  0.50_pm_r,151._pm_r,   1._pm_r, 61._pm_r,  0.27_pm_r,208._pm_r, &
235.60_pm_r, 30676._pm_r,   3._pm_r,256._pm_r,  0.58_pm_r,146._pm_r,   1._pm_r, 76._pm_r,  0.27_pm_r,211._pm_r, &
245.70_pm_r, 34195._pm_r,   3._pm_r,237._pm_r,  0.59_pm_r,141._pm_r,   1._pm_r,102._pm_r,  0.24_pm_r,216._pm_r, &
258.30_pm_r, 37887._pm_r,   3._pm_r,219._pm_r,  0.51_pm_r,135._pm_r,   1._pm_r,134._pm_r,  0.19_pm_r,224._pm_r, &
270.50_pm_r, 41758._pm_r,   3._pm_r,204._pm_r,  0.37_pm_r,126._pm_r,   1._pm_r,159._pm_r,  0.13_pm_r,236._pm_r, &
277.10_pm_r, 45775._pm_r,   3._pm_r,197._pm_r,  0.12_pm_r,165._pm_r,   1._pm_r,167._pm_r,  0.01_pm_r, 11._pm_r, &
277.90_pm_r, 49846._pm_r,   3._pm_r,200._pm_r,  0.26_pm_r,260._pm_r,   1._pm_r,162._pm_r,  0.05_pm_r, 37._pm_r, &
270.90_pm_r, 53869._pm_r,   3._pm_r,209._pm_r,  0.50_pm_r,270._pm_r,   1._pm_r,160._pm_r,  0.03_pm_r,290._pm_r, &
260.40_pm_r, 57763._pm_r,   4._pm_r,220._pm_r,  0.57_pm_r,271._pm_r,   1._pm_r,173._pm_r,  0.14_pm_r,243._pm_r, &
248.40_pm_r, 61486._pm_r,   4._pm_r,228._pm_r,  0.32_pm_r,296._pm_r,   1._pm_r,192._pm_r,  0.17_pm_r,229._pm_r, &
237.00_pm_r, 65041._pm_r,   4._pm_r,234._pm_r,  0.30_pm_r, 14._pm_r,   1._pm_r,199._pm_r,  0.17_pm_r,203._pm_r, &
227.80_pm_r, 68441._pm_r,   3._pm_r,237._pm_r,  0.62_pm_r, 44._pm_r,   1._pm_r,196._pm_r,  0.18_pm_r,167._pm_r, &
217.10_pm_r, 71705._pm_r,   2._pm_r,241._pm_r,  0.91_pm_r, 53._pm_r,   1._pm_r,187._pm_r,  0.23_pm_r,140._pm_r, &
203.90_pm_r, 74786._pm_r,   1._pm_r,255._pm_r, 1.08_pm_r, 55._pm_r,   2._pm_r,175._pm_r,  0.30_pm_r,127._pm_r, &
189.90_pm_r, 77674._pm_r,   1._pm_r, 41._pm_r, 1.12_pm_r, 57._pm_r,   2._pm_r,165._pm_r,  0.32_pm_r,120._pm_r, &
177.10_pm_r, 80353._pm_r,   3._pm_r, 51._pm_r, 1.06_pm_r, 58._pm_r,   2._pm_r,156._pm_r,  0.31_pm_r,117._pm_r, &
163.40_pm_r, 82857._pm_r,   4._pm_r, 54._pm_r,  0.92_pm_r, 59._pm_r,   3._pm_r,150._pm_r,  0.28_pm_r,114._pm_r, &
151.10_pm_r, 85103._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
146.40_pm_r, 87240._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
147.70_pm_r, 89385._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
153.60_pm_r, 91596._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
164.30_pm_r, 93936._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.00_pm_r, 96490._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
205.60_pm_r, 99379._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
240.50_pm_r,102754._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
278.10_pm_r,106753._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
308.70_pm_r,111353._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
383.80_pm_r,116827._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
263.60_pm_r,    42._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
250.40_pm_r,  3794._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
228.60_pm_r,  7290._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
227.10_pm_r, 10616._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
228.50_pm_r, 13941._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
229.40_pm_r, 17293._pm_r,   2._pm_r,287._pm_r,  0.08_pm_r,105._pm_r,   1._pm_r, 31._pm_r,  0.08_pm_r,191._pm_r, &
230.00_pm_r, 20658._pm_r,   2._pm_r,287._pm_r,  0.10_pm_r,114._pm_r,   1._pm_r, 35._pm_r,  0.13_pm_r,193._pm_r, &
229.10_pm_r, 24018._pm_r,   2._pm_r,286._pm_r,  0.11_pm_r,126._pm_r,   1._pm_r, 43._pm_r,  0.19_pm_r,197._pm_r, &
230.00_pm_r, 27374._pm_r,   2._pm_r,284._pm_r,  0.11_pm_r,142._pm_r,   0._pm_r, 66._pm_r,  0.22_pm_r,201._pm_r, &
236.40_pm_r, 30790._pm_r,   2._pm_r,279._pm_r,  0.13_pm_r,171._pm_r,   0._pm_r,136._pm_r,  0.24_pm_r,203._pm_r, &
245.20_pm_r, 34310._pm_r,   2._pm_r,272._pm_r,  0.16_pm_r,187._pm_r,   1._pm_r,177._pm_r,  0.23_pm_r,209._pm_r, &
258.00_pm_r, 37996._pm_r,   2._pm_r,263._pm_r,  0.19_pm_r,197._pm_r,   1._pm_r,190._pm_r,  0.18_pm_r,214._pm_r, &
270.90_pm_r, 41868._pm_r,   2._pm_r,255._pm_r,  0.22_pm_r,202._pm_r,   1._pm_r,196._pm_r,  0.12_pm_r,222._pm_r, &
278.40_pm_r, 45897._pm_r,   2._pm_r,249._pm_r,  0.22_pm_r,207._pm_r,   1._pm_r,193._pm_r,  0.11_pm_r,105._pm_r, &
280.60_pm_r, 49997._pm_r,   2._pm_r,244._pm_r,  0.28_pm_r,229._pm_r,   1._pm_r,177._pm_r,  0.30_pm_r, 96._pm_r, &
273.90_pm_r, 54064._pm_r,   3._pm_r,244._pm_r,  0.35_pm_r,258._pm_r,   1._pm_r,150._pm_r,  0.42_pm_r, 83._pm_r, &
263.00_pm_r, 58001._pm_r,   3._pm_r,248._pm_r,  0.43_pm_r,281._pm_r,   1._pm_r,124._pm_r,  0.48_pm_r, 53._pm_r, &
250.10_pm_r, 61757._pm_r,   4._pm_r,254._pm_r,  0.34_pm_r,301._pm_r,   2._pm_r, 99._pm_r,  0.45_pm_r, 11._pm_r, &
237.30_pm_r, 65327._pm_r,   4._pm_r,259._pm_r,  0.23_pm_r,326._pm_r,   2._pm_r, 75._pm_r,  0.49_pm_r,332._pm_r, &
227.90_pm_r, 68731._pm_r,   4._pm_r,263._pm_r,  0.15_pm_r, 14._pm_r,   1._pm_r, 44._pm_r,  0.59_pm_r,293._pm_r, &
216.40_pm_r, 71991._pm_r,   4._pm_r,265._pm_r,  0.17_pm_r, 65._pm_r,   1._pm_r,355._pm_r,  0.75_pm_r,268._pm_r, &
201.80_pm_r, 75051._pm_r,   4._pm_r,265._pm_r,  0.23_pm_r, 95._pm_r,   2._pm_r,302._pm_r,  0.89_pm_r,253._pm_r, &
187.40_pm_r, 77904._pm_r,   3._pm_r,263._pm_r,  0.26_pm_r,106._pm_r,   3._pm_r,277._pm_r,  0.95_pm_r,245._pm_r, &
174.50_pm_r, 80546._pm_r,   3._pm_r,260._pm_r,  0.27_pm_r,114._pm_r,   4._pm_r,265._pm_r,  0.92_pm_r,240._pm_r, &
160.00_pm_r, 83012._pm_r,   3._pm_r,255._pm_r,  0.25_pm_r,117._pm_r,   5._pm_r,258._pm_r,  0.81_pm_r,238._pm_r, &
146.10_pm_r, 85183._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
141.30_pm_r, 87238._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
143.40_pm_r, 89315._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
150.10_pm_r, 91467._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
161.80_pm_r, 93763._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
180.00_pm_r, 96290._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
206.80_pm_r, 99178._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
245.10_pm_r,102598._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
283.20_pm_r,106676._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
308.80_pm_r,111315._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
384.80_pm_r,116788._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)


  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_mai = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_mai,(/10,nb_alt,nb_lat/))


  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)


end subroutine cps_atmi_mai

subroutine cps_atmi_juin (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_juin
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JUIN
!
!$Description
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JUIN
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_juin (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>         
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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



  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
264.30_pm_r,-219._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
241.10_pm_r,3470._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.50_pm_r,6825._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.30_pm_r,9932._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.50_pm_r,12916._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.70_pm_r,15830._pm_r,11._pm_r,221._pm_r,0.62_pm_r,207._pm_r,3._pm_r,172._pm_r,0.34_pm_r,346._pm_r, &
192.80_pm_r,18682._pm_r,12._pm_r,219._pm_r,0.95_pm_r,202._pm_r,3._pm_r,173._pm_r,0.50_pm_r,347._pm_r, &
189.40_pm_r,21481._pm_r,13._pm_r,217._pm_r,1.11_pm_r,192._pm_r,2._pm_r,175._pm_r,0.58_pm_r,348._pm_r, &
181.40_pm_r,24199._pm_r,15._pm_r,214._pm_r,0.82_pm_r,173._pm_r,1._pm_r,179._pm_r,0.43_pm_r,352._pm_r, &
182.70_pm_r,26848._pm_r,15._pm_r,211._pm_r,0.51_pm_r,117._pm_r,1._pm_r,181._pm_r,0.13_pm_r,5._pm_r, &
198.00_pm_r,29624._pm_r,15._pm_r,208._pm_r,0.66_pm_r,68._pm_r,1._pm_r,177._pm_r,0.14_pm_r,159._pm_r, &
213.70_pm_r,32645._pm_r,14._pm_r,206._pm_r,0.82_pm_r,44._pm_r,1._pm_r,174._pm_r,0.24_pm_r,168._pm_r, &
227.10_pm_r,35871._pm_r,12._pm_r,205._pm_r,0.91_pm_r,24._pm_r,1._pm_r,173._pm_r,0.23_pm_r,173._pm_r, &
238.20_pm_r,39283._pm_r,11._pm_r,205._pm_r,0.89_pm_r,28._pm_r,2._pm_r,175._pm_r,0.13_pm_r,225._pm_r, &
250.30_pm_r,42854._pm_r,10._pm_r,205._pm_r,1.06_pm_r,26._pm_r,2._pm_r,184._pm_r,0.25_pm_r,256._pm_r, &
262.30_pm_r,46615._pm_r,8._pm_r,206._pm_r,1.40_pm_r,16._pm_r,2._pm_r,197._pm_r,0.41_pm_r,240._pm_r, &
267.90_pm_r,50504._pm_r,6._pm_r,212._pm_r,1.83_pm_r,3._pm_r,3._pm_r,205._pm_r,0.59_pm_r,216._pm_r, &
265.40_pm_r,54416._pm_r,4._pm_r,237._pm_r,1.85_pm_r,348._pm_r,3._pm_r,206._pm_r,0.58_pm_r,203._pm_r, &
257.70_pm_r,58254._pm_r,4._pm_r,276._pm_r,1.70_pm_r,336._pm_r,4._pm_r,204._pm_r,0.48_pm_r,188._pm_r, &
248.40_pm_r,61957._pm_r,6._pm_r,295._pm_r,1.38_pm_r,323._pm_r,5._pm_r,202._pm_r,0.29_pm_r,170._pm_r, &
241.10_pm_r,65542._pm_r,7._pm_r,300._pm_r,1.03_pm_r,305._pm_r,5._pm_r,199._pm_r,0.17_pm_r,111._pm_r, &
235.30_pm_r,69028._pm_r,9._pm_r,298._pm_r,0.83_pm_r,275._pm_r,5._pm_r,196._pm_r,0.32_pm_r,51._pm_r, &
230.60_pm_r,72439._pm_r,10._pm_r,294._pm_r,0.85_pm_r,243._pm_r,4._pm_r,192._pm_r,0.54_pm_r,35._pm_r, &
228.80_pm_r,75799._pm_r,10._pm_r,287._pm_r,1.02_pm_r,223._pm_r,3._pm_r,187._pm_r,0.71_pm_r,30._pm_r, &
227.70_pm_r,79139._pm_r,11._pm_r,279._pm_r,1.16_pm_r,213._pm_r,2._pm_r,176._pm_r,0.81_pm_r,28._pm_r, &
227.80_pm_r,82463._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.20_pm_r,85789._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.60_pm_r,89039._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.00_pm_r,92150._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.10_pm_r,95182._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.90_pm_r,98212._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.30_pm_r,101316._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.90_pm_r,104579._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
236.90_pm_r,108097._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
261.80_pm_r,111991._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
320.50_pm_r,116585._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
269.60_pm_r,-169._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
244.10_pm_r,3582._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.20_pm_r,6980._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.10_pm_r,10129._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.10_pm_r,13168._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.70_pm_r,16160._pm_r,15._pm_r,223._pm_r,1.06_pm_r,169._pm_r,2._pm_r,152._pm_r,0.14_pm_r,49._pm_r, &
199.80_pm_r,19107._pm_r,16._pm_r,218._pm_r,1.59_pm_r,164._pm_r,2._pm_r,145._pm_r,0.25_pm_r,45._pm_r, &
196.50_pm_r,22009._pm_r,18._pm_r,210._pm_r,1.88_pm_r,155._pm_r,2._pm_r,132._pm_r,0.39_pm_r,42._pm_r, &
189.00_pm_r,24834._pm_r,19._pm_r,203._pm_r,1.62_pm_r,136._pm_r,2._pm_r,115._pm_r,0.48_pm_r,38._pm_r, &
189.90_pm_r,27595._pm_r,19._pm_r,197._pm_r,1.24_pm_r,97._pm_r,2._pm_r,99._pm_r,0.48_pm_r,33._pm_r, &
202.40_pm_r,30457._pm_r,18._pm_r,193._pm_r,1.35_pm_r,53._pm_r,3._pm_r,85._pm_r,0.42_pm_r,27._pm_r, &
216.50_pm_r,33528._pm_r,17._pm_r,189._pm_r,1.64_pm_r,29._pm_r,3._pm_r,76._pm_r,0.38_pm_r,20._pm_r, &
229.70_pm_r,36793._pm_r,14._pm_r,187._pm_r,1.82_pm_r,14._pm_r,3._pm_r,68._pm_r,0.33_pm_r,13._pm_r, &
240.60_pm_r,40240._pm_r,11._pm_r,186._pm_r,1.91_pm_r,15._pm_r,4._pm_r,62._pm_r,0.29_pm_r,11._pm_r, &
252.00_pm_r,43842._pm_r,8._pm_r,182._pm_r,2.29_pm_r,15._pm_r,4._pm_r,57._pm_r,0.27_pm_r,5._pm_r, &
262.20_pm_r,47614._pm_r,5._pm_r,174._pm_r,2.92_pm_r,11._pm_r,4._pm_r,53._pm_r,0.17_pm_r,345._pm_r, &
266.00_pm_r,51488._pm_r,1._pm_r,91._pm_r,3.52_pm_r,5._pm_r,4._pm_r,51._pm_r,0.16_pm_r,254._pm_r, &
263.10_pm_r,55367._pm_r,5._pm_r,14._pm_r,3.15_pm_r,355._pm_r,4._pm_r,51._pm_r,0.27_pm_r,220._pm_r, &
256.30_pm_r,59176._pm_r,9._pm_r,3._pm_r,2.55_pm_r,344._pm_r,3._pm_r,53._pm_r,0.31_pm_r,207._pm_r, &
247.90_pm_r,62866._pm_r,12._pm_r,356._pm_r,1.87_pm_r,327._pm_r,3._pm_r,58._pm_r,0.24_pm_r,187._pm_r, &
240.40_pm_r,66443._pm_r,14._pm_r,350._pm_r,1.37_pm_r,299._pm_r,3._pm_r,63._pm_r,0.17_pm_r,139._pm_r, &
233.90_pm_r,69915._pm_r,15._pm_r,343._pm_r,1.35_pm_r,261._pm_r,3._pm_r,67._pm_r,0.27_pm_r,85._pm_r, &
228.40_pm_r,73300._pm_r,15._pm_r,334._pm_r,1.71_pm_r,236._pm_r,4._pm_r,68._pm_r,0.44_pm_r,69._pm_r, &
225.10_pm_r,76616._pm_r,14._pm_r,323._pm_r,2.10_pm_r,224._pm_r,4._pm_r,67._pm_r,0.58_pm_r,62._pm_r, &
223.60_pm_r,79892._pm_r,14._pm_r,310._pm_r,2.33_pm_r,217._pm_r,5._pm_r,66._pm_r,0.66_pm_r,61._pm_r, &
224.30_pm_r,83165._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.00_pm_r,86449._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.50_pm_r,89642._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.60_pm_r,92700._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.10_pm_r,95687._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.00_pm_r,98675._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.00_pm_r,101734._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.40_pm_r,104948._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
234.20_pm_r,108418._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.40_pm_r,112295._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
323.20_pm_r,116924._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
274.90_pm_r,-99._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
250.00_pm_r,3736._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.70_pm_r,7213._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.20_pm_r,10420._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.50_pm_r,13523._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.50_pm_r,16608._pm_r,17._pm_r,224._pm_r,1.60_pm_r,142._pm_r,2._pm_r,354._pm_r,0.34_pm_r,150._pm_r, &
206.80_pm_r,19655._pm_r,17._pm_r,215._pm_r,2.20_pm_r,138._pm_r,1._pm_r,6._pm_r,0.47_pm_r,145._pm_r, &
204.80_pm_r,22671._pm_r,18._pm_r,204._pm_r,2.40_pm_r,129._pm_r,1._pm_r,38._pm_r,0.53_pm_r,135._pm_r, &
199.20_pm_r,25631._pm_r,19._pm_r,194._pm_r,1.90_pm_r,111._pm_r,1._pm_r,74._pm_r,0.46_pm_r,113._pm_r, &
198.50_pm_r,28535._pm_r,18._pm_r,187._pm_r,1.51_pm_r,63._pm_r,2._pm_r,80._pm_r,0.41_pm_r,69._pm_r, &
204.00_pm_r,31476._pm_r,16._pm_r,183._pm_r,2.14_pm_r,20._pm_r,2._pm_r,71._pm_r,0.56_pm_r,33._pm_r, &
214.40_pm_r,34538._pm_r,13._pm_r,180._pm_r,2.81_pm_r,3._pm_r,3._pm_r,58._pm_r,0.71_pm_r,16._pm_r, &
227.60_pm_r,37769._pm_r,8._pm_r,181._pm_r,2.97_pm_r,356._pm_r,4._pm_r,47._pm_r,0.75_pm_r,8._pm_r, &
239.40_pm_r,41195._pm_r,4._pm_r,185._pm_r,2.87_pm_r,0._pm_r,5._pm_r,39._pm_r,0.58_pm_r,2._pm_r, &
250.60_pm_r,44780._pm_r,0._pm_r,325._pm_r,3.09_pm_r,2._pm_r,5._pm_r,34._pm_r,0.44_pm_r,360._pm_r, &
258.40_pm_r,48517._pm_r,5._pm_r,358._pm_r,3.48_pm_r,357._pm_r,6._pm_r,31._pm_r,0.30_pm_r,360._pm_r, &
259.80_pm_r,52317._pm_r,10._pm_r,355._pm_r,3.73_pm_r,348._pm_r,6._pm_r,30._pm_r,0.10_pm_r,6._pm_r, &
257.90_pm_r,56110._pm_r,15._pm_r,351._pm_r,2.98_pm_r,336._pm_r,6._pm_r,30._pm_r,0.04_pm_r,157._pm_r, &
253.90_pm_r,59862._pm_r,19._pm_r,347._pm_r,2.25_pm_r,319._pm_r,6._pm_r,30._pm_r,0.10_pm_r,156._pm_r, &
248.00_pm_r,63537._pm_r,21._pm_r,342._pm_r,1.72_pm_r,297._pm_r,6._pm_r,32._pm_r,0.11_pm_r,129._pm_r, &
241.00_pm_r,67121._pm_r,23._pm_r,337._pm_r,1.44_pm_r,270._pm_r,6._pm_r,33._pm_r,0.15_pm_r,79._pm_r, &
234.00_pm_r,70597._pm_r,23._pm_r,332._pm_r,1.45_pm_r,245._pm_r,6._pm_r,35._pm_r,0.28_pm_r,55._pm_r, &
227.30_pm_r,73977._pm_r,23._pm_r,327._pm_r,1.59_pm_r,226._pm_r,7._pm_r,36._pm_r,0.41_pm_r,45._pm_r, &
222.00_pm_r,77260._pm_r,22._pm_r,320._pm_r,1.76_pm_r,215._pm_r,7._pm_r,37._pm_r,0.51_pm_r,43._pm_r, &
219.50_pm_r,80474._pm_r,21._pm_r,314._pm_r,1.82_pm_r,208._pm_r,8._pm_r,37._pm_r,0.57_pm_r,41._pm_r, &
219.70_pm_r,83692._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.30_pm_r,86913._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.40_pm_r,90017._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.80_pm_r,92996._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.90_pm_r,95920._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.90_pm_r,98850._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.40_pm_r,101848._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.20_pm_r,104988._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.90_pm_r,108386._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.40_pm_r,112228._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
327.10_pm_r,116902._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
280.20_pm_r,-59._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
256.40_pm_r,3865._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
231.00_pm_r,7430._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.20_pm_r,10700._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.70_pm_r,13859._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.90_pm_r,17015._pm_r,9._pm_r,208._pm_r,1.37_pm_r,137._pm_r,3._pm_r,55._pm_r,0.48_pm_r,195._pm_r, &
213.20_pm_r,20149._pm_r,10._pm_r,195._pm_r,1.70_pm_r,130._pm_r,2._pm_r,68._pm_r,0.60_pm_r,192._pm_r, &
212.90_pm_r,23270._pm_r,11._pm_r,183._pm_r,1.63_pm_r,116._pm_r,2._pm_r,91._pm_r,0.55_pm_r,185._pm_r, &
209.50_pm_r,26366._pm_r,12._pm_r,173._pm_r,1.33_pm_r,82._pm_r,2._pm_r,109._pm_r,0.33_pm_r,161._pm_r, &
206.80_pm_r,29410._pm_r,11._pm_r,163._pm_r,1.66_pm_r,30._pm_r,2._pm_r,110._pm_r,0.35_pm_r,68._pm_r, &
207.60_pm_r,32439._pm_r,8._pm_r,152._pm_r,2.65_pm_r,3._pm_r,3._pm_r,95._pm_r,0.84_pm_r,38._pm_r, &
214.70_pm_r,35526._pm_r,5._pm_r,130._pm_r,3.24_pm_r,350._pm_r,4._pm_r,75._pm_r,1.20_pm_r,30._pm_r, &
227.20_pm_r,38755._pm_r,3._pm_r,55._pm_r,3.11_pm_r,341._pm_r,5._pm_r,60._pm_r,1.26_pm_r,26._pm_r, &
239.90_pm_r,42182._pm_r,6._pm_r,9._pm_r,2.54_pm_r,338._pm_r,7._pm_r,53._pm_r,0.68_pm_r,26._pm_r, &
249.80_pm_r,45768._pm_r,9._pm_r,357._pm_r,2.33_pm_r,336._pm_r,7._pm_r,51._pm_r,0.20_pm_r,44._pm_r, &
254.40_pm_r,49468._pm_r,12._pm_r,350._pm_r,2.39_pm_r,332._pm_r,7._pm_r,52._pm_r,0.14_pm_r,148._pm_r, &
253.80_pm_r,53194._pm_r,15._pm_r,345._pm_r,2.48_pm_r,326._pm_r,7._pm_r,54._pm_r,0.19_pm_r,161._pm_r, &
251.40_pm_r,56895._pm_r,18._pm_r,341._pm_r,1.88_pm_r,310._pm_r,7._pm_r,55._pm_r,0.05_pm_r,90._pm_r, &
247.00_pm_r,60548._pm_r,20._pm_r,336._pm_r,1.43_pm_r,290._pm_r,7._pm_r,55._pm_r,0.15_pm_r,23._pm_r, &
241.70_pm_r,64125._pm_r,21._pm_r,332._pm_r,1.16_pm_r,269._pm_r,8._pm_r,53._pm_r,0.28_pm_r,21._pm_r, &
235.70_pm_r,67623._pm_r,22._pm_r,328._pm_r,1.01_pm_r,250._pm_r,8._pm_r,52._pm_r,0.40_pm_r,25._pm_r, &
229.30_pm_r,71026._pm_r,22._pm_r,325._pm_r,0.90_pm_r,235._pm_r,9._pm_r,50._pm_r,0.49_pm_r,28._pm_r, &
223.30_pm_r,74341._pm_r,22._pm_r,321._pm_r,0.83_pm_r,223._pm_r,9._pm_r,48._pm_r,0.57_pm_r,30._pm_r, &
218.70_pm_r,77574._pm_r,22._pm_r,318._pm_r,0.78_pm_r,213._pm_r,10._pm_r,47._pm_r,0.61_pm_r,32._pm_r, &
216.20_pm_r,80746._pm_r,21._pm_r,315._pm_r,0.72_pm_r,205._pm_r,11._pm_r,45._pm_r,0.62_pm_r,33._pm_r, &
214.80_pm_r,83915._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.40_pm_r,87059._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.70_pm_r,90060._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.60_pm_r,92954._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.80_pm_r,95812._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.60_pm_r,98681._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.60_pm_r,101614._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.30_pm_r,104674._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.60_pm_r,107986._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
261.60_pm_r,111777._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
331.20_pm_r,116493._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
286.30_pm_r,-32._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.00_pm_r,3982._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
236.10_pm_r,7628._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.70_pm_r,10957._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.70_pm_r,14137._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.00_pm_r,17291._pm_r,4._pm_r,180._pm_r,0.97_pm_r,139._pm_r,4._pm_r,68._pm_r,0.62_pm_r,241._pm_r, &
215.40_pm_r,20441._pm_r,6._pm_r,169._pm_r,1.05_pm_r,128._pm_r,3._pm_r,70._pm_r,0.68_pm_r,243._pm_r, &
218.00_pm_r,23618._pm_r,7._pm_r,159._pm_r,0.85_pm_r,99._pm_r,2._pm_r,71._pm_r,0.52_pm_r,250._pm_r, &
217.70_pm_r,26811._pm_r,7._pm_r,149._pm_r,0.96_pm_r,41._pm_r,2._pm_r,68._pm_r,0.19_pm_r,306._pm_r, &
217.20_pm_r,29993._pm_r,6._pm_r,134._pm_r,1.69_pm_r,7._pm_r,2._pm_r,57._pm_r,0.63_pm_r,33._pm_r, &
219.10_pm_r,33184._pm_r,4._pm_r,103._pm_r,2.43_pm_r,352._pm_r,3._pm_r,49._pm_r,1.29_pm_r,41._pm_r, &
225.60_pm_r,36437._pm_r,4._pm_r,48._pm_r,2.59_pm_r,341._pm_r,6._pm_r,46._pm_r,1.68_pm_r,43._pm_r, &
237.10_pm_r,39816._pm_r,6._pm_r,15._pm_r,2.14_pm_r,325._pm_r,8._pm_r,45._pm_r,1.66_pm_r,43._pm_r, &
249.90_pm_r,43391._pm_r,8._pm_r,359._pm_r,1.45_pm_r,308._pm_r,10._pm_r,46._pm_r,0.88_pm_r,66._pm_r, &
255.30_pm_r,47098._pm_r,9._pm_r,348._pm_r,1.11_pm_r,285._pm_r,11._pm_r,50._pm_r,0.69_pm_r,127._pm_r, &
253.70_pm_r,50829._pm_r,9._pm_r,339._pm_r,1.10_pm_r,258._pm_r,10._pm_r,57._pm_r,0.90_pm_r,158._pm_r, &
248.70_pm_r,54512._pm_r,10._pm_r,328._pm_r,1.28_pm_r,240._pm_r,10._pm_r,64._pm_r,0.78_pm_r,171._pm_r, &
243.30_pm_r,58113._pm_r,9._pm_r,315._pm_r,1.69_pm_r,218._pm_r,10._pm_r,67._pm_r,0.22_pm_r,213._pm_r, &
238.50_pm_r,61643._pm_r,9._pm_r,299._pm_r,1.80_pm_r,208._pm_r,10._pm_r,66._pm_r,0.39_pm_r,318._pm_r, &
232.40_pm_r,65091._pm_r,9._pm_r,283._pm_r,1.52_pm_r,202._pm_r,10._pm_r,61._pm_r,0.69_pm_r,337._pm_r, &
225.70_pm_r,68447._pm_r,10._pm_r,272._pm_r,1.08_pm_r,196._pm_r,10._pm_r,54._pm_r,0.88_pm_r,345._pm_r, &
220.80_pm_r,71713._pm_r,10._pm_r,265._pm_r,0.58_pm_r,185._pm_r,11._pm_r,48._pm_r,0.99_pm_r,352._pm_r, &
216.40_pm_r,74917._pm_r,10._pm_r,262._pm_r,0.21_pm_r,141._pm_r,11._pm_r,42._pm_r,1.05_pm_r,356._pm_r, &
212.70_pm_r,78055._pm_r,10._pm_r,262._pm_r,0.30_pm_r,62._pm_r,13._pm_r,37._pm_r,1.07_pm_r,359._pm_r, &
210.90_pm_r,81144._pm_r,9._pm_r,263._pm_r,0.47_pm_r,46._pm_r,14._pm_r,33._pm_r,1.04_pm_r,1._pm_r, &
208.70_pm_r,84236._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.20_pm_r,87274._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.70_pm_r,90171._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.20_pm_r,92994._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.40_pm_r,95798._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.00_pm_r,98619._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.20_pm_r,101494._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.80_pm_r,104484._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.30_pm_r,107713._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
260.10_pm_r,111450._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
335.00_pm_r,116199._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
292.00_pm_r,-34._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
268.20_pm_r,4070._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.10_pm_r,7816._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.70_pm_r,11222._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.60_pm_r,14389._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.60_pm_r,17454._pm_r,4._pm_r,152._pm_r,0.10_pm_r,169._pm_r,3._pm_r,81._pm_r,0.58_pm_r,259._pm_r, &
213.90_pm_r,20553._pm_r,4._pm_r,152._pm_r,0.07_pm_r,340._pm_r,2._pm_r,81._pm_r,0.63_pm_r,265._pm_r, &
219.30_pm_r,23728._pm_r,4._pm_r,152._pm_r,0.41_pm_r,342._pm_r,1._pm_r,75._pm_r,0.49_pm_r,281._pm_r, &
223.00_pm_r,26967._pm_r,3._pm_r,149._pm_r,0.82_pm_r,342._pm_r,1._pm_r,50._pm_r,0.36_pm_r,336._pm_r, &
226.10_pm_r,30256._pm_r,2._pm_r,137._pm_r,1.18_pm_r,340._pm_r,2._pm_r,33._pm_r,0.68_pm_r,26._pm_r, &
230.20_pm_r,33594._pm_r,1._pm_r,40._pm_r,1.33_pm_r,337._pm_r,3._pm_r,34._pm_r,1.13_pm_r,39._pm_r, &
238.50_pm_r,37024._pm_r,2._pm_r,351._pm_r,1.12_pm_r,331._pm_r,5._pm_r,37._pm_r,1.34_pm_r,43._pm_r, &
249.80_pm_r,40595._pm_r,3._pm_r,341._pm_r,0.66_pm_r,309._pm_r,7._pm_r,39._pm_r,1.24_pm_r,44._pm_r, &
258.80_pm_r,44328._pm_r,4._pm_r,334._pm_r,0.36_pm_r,277._pm_r,8._pm_r,41._pm_r,0.59_pm_r,69._pm_r, &
261.50_pm_r,48144._pm_r,4._pm_r,326._pm_r,0.55_pm_r,215._pm_r,8._pm_r,45._pm_r,0.49_pm_r,128._pm_r, &
256.90_pm_r,51944._pm_r,3._pm_r,308._pm_r,1.31_pm_r,191._pm_r,8._pm_r,50._pm_r,0.57_pm_r,136._pm_r, &
248.90_pm_r,55651._pm_r,3._pm_r,259._pm_r,2.32_pm_r,182._pm_r,9._pm_r,55._pm_r,0.53_pm_r,102._pm_r, &
241.40_pm_r,59240._pm_r,5._pm_r,213._pm_r,2.97_pm_r,177._pm_r,9._pm_r,56._pm_r,0.45_pm_r,42._pm_r, &
233.20_pm_r,62719._pm_r,9._pm_r,196._pm_r,2.94_pm_r,173._pm_r,10._pm_r,54._pm_r,0.60_pm_r,4._pm_r, &
223.60_pm_r,66063._pm_r,13._pm_r,189._pm_r,2.34_pm_r,168._pm_r,10._pm_r,49._pm_r,0.70_pm_r,343._pm_r, &
215.30_pm_r,69276._pm_r,16._pm_r,184._pm_r,1.55_pm_r,160._pm_r,11._pm_r,44._pm_r,0.75_pm_r,326._pm_r, &
210.50_pm_r,72389._pm_r,17._pm_r,181._pm_r,0.86_pm_r,139._pm_r,11._pm_r,38._pm_r,0.79_pm_r,312._pm_r, &
207.30_pm_r,75449._pm_r,18._pm_r,178._pm_r,0.59_pm_r,88._pm_r,11._pm_r,31._pm_r,0.83_pm_r,301._pm_r, &
205.50_pm_r,78469._pm_r,17._pm_r,176._pm_r,0.75_pm_r,51._pm_r,11._pm_r,25._pm_r,0.84_pm_r,295._pm_r, &
205.00_pm_r,81468._pm_r,16._pm_r,172._pm_r,0.93_pm_r,37._pm_r,11._pm_r,18._pm_r,0.82_pm_r,290._pm_r, &
203.10_pm_r,84478._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.60_pm_r,87421._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.50_pm_r,90241._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.60_pm_r,93016._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,95789._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.70_pm_r,98581._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.10_pm_r,101418._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.90_pm_r,104358._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.40_pm_r,107528._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
259.10_pm_r,111225._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
338.50_pm_r,116006._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
296.90_pm_r,-46._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.00_pm_r,4140._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.80_pm_r,7980._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.40_pm_r,11457._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.90_pm_r,14604._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.50_pm_r,17569._pm_r,4._pm_r,140._pm_r,0.49_pm_r,272._pm_r,1._pm_r,74._pm_r,0.27_pm_r,270._pm_r, &
212.00_pm_r,20612._pm_r,3._pm_r,151._pm_r,0.66_pm_r,279._pm_r,1._pm_r,66._pm_r,0.28_pm_r,285._pm_r, &
219.30_pm_r,23773._pm_r,2._pm_r,170._pm_r,0.73_pm_r,286._pm_r,1._pm_r,47._pm_r,0.23_pm_r,316._pm_r, &
224.60_pm_r,27024._pm_r,2._pm_r,198._pm_r,0.75_pm_r,295._pm_r,1._pm_r,30._pm_r,0.30_pm_r,4._pm_r, &
229.70_pm_r,30350._pm_r,2._pm_r,227._pm_r,0.67_pm_r,306._pm_r,2._pm_r,27._pm_r,0.48_pm_r,30._pm_r, &
237.90_pm_r,33769._pm_r,2._pm_r,249._pm_r,0.48_pm_r,319._pm_r,2._pm_r,30._pm_r,0.64_pm_r,40._pm_r, &
247.30_pm_r,37325._pm_r,3._pm_r,260._pm_r,0.23_pm_r,342._pm_r,3._pm_r,33._pm_r,0.67_pm_r,44._pm_r, &
256.80_pm_r,41013._pm_r,2._pm_r,264._pm_r,0.14_pm_r,98._pm_r,4._pm_r,36._pm_r,0.56_pm_r,44._pm_r, &
264.00_pm_r,44833._pm_r,2._pm_r,263._pm_r,0.23_pm_r,96._pm_r,5._pm_r,38._pm_r,0.25_pm_r,83._pm_r, &
265.20_pm_r,48714._pm_r,2._pm_r,256._pm_r,0.39_pm_r,136._pm_r,5._pm_r,42._pm_r,0.28_pm_r,122._pm_r, &
259.60_pm_r,52561._pm_r,2._pm_r,223._pm_r,1.08_pm_r,155._pm_r,5._pm_r,47._pm_r,0.32_pm_r,105._pm_r, &
249.90_pm_r,56298._pm_r,4._pm_r,186._pm_r,2.10_pm_r,159._pm_r,6._pm_r,50._pm_r,0.35_pm_r,63._pm_r, &
239.50_pm_r,59880._pm_r,7._pm_r,173._pm_r,2.56_pm_r,159._pm_r,6._pm_r,49._pm_r,0.28_pm_r,14._pm_r, &
228.00_pm_r,63309._pm_r,10._pm_r,168._pm_r,2.38_pm_r,159._pm_r,6._pm_r,46._pm_r,0.31_pm_r,327._pm_r, &
215.70_pm_r,66554._pm_r,13._pm_r,166._pm_r,1.70_pm_r,158._pm_r,6._pm_r,41._pm_r,0.35_pm_r,296._pm_r, &
207.20_pm_r,69645._pm_r,15._pm_r,165._pm_r,0.84_pm_r,155._pm_r,6._pm_r,36._pm_r,0.40_pm_r,273._pm_r, &
204.50_pm_r,72656._pm_r,16._pm_r,164._pm_r,0.10_pm_r,108._pm_r,5._pm_r,32._pm_r,0.45_pm_r,257._pm_r, &
202.90_pm_r,75640._pm_r,16._pm_r,164._pm_r,0.57_pm_r,349._pm_r,5._pm_r,27._pm_r,0.51_pm_r,243._pm_r, &
202.10_pm_r,78602._pm_r,14._pm_r,164._pm_r,0.97_pm_r,345._pm_r,4._pm_r,21._pm_r,0.54_pm_r,237._pm_r, &
201.70_pm_r,81557._pm_r,13._pm_r,164._pm_r,1.18_pm_r,345._pm_r,4._pm_r,14._pm_r,0.55_pm_r,233._pm_r, &
199.10_pm_r,84510._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.00_pm_r,87387._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.50_pm_r,90167._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,92924._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,95688._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.90_pm_r,98472._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.80_pm_r,101295._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.50_pm_r,104215._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.90_pm_r,107362._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
259.60_pm_r,111051._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
342.10_pm_r,115871._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
300.10_pm_r,-61._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.30_pm_r,4167._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.80_pm_r,8048._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.20_pm_r,11554._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.80_pm_r,14686._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.60_pm_r,17601._pm_r,2._pm_r,148._pm_r,0.20_pm_r,288._pm_r,0._pm_r,82._pm_r,0.05_pm_r,18._pm_r, &
211.10_pm_r,20615._pm_r,2._pm_r,157._pm_r,0.29_pm_r,285._pm_r,0._pm_r,67._pm_r,0.09_pm_r,29._pm_r, &
218.30_pm_r,23762._pm_r,1._pm_r,174._pm_r,0.33_pm_r,283._pm_r,1._pm_r,55._pm_r,0.14_pm_r,35._pm_r, &
224.70_pm_r,27004._pm_r,1._pm_r,198._pm_r,0.36_pm_r,281._pm_r,1._pm_r,49._pm_r,0.19_pm_r,38._pm_r, &
232.10_pm_r,30348._pm_r,1._pm_r,219._pm_r,0.32_pm_r,275._pm_r,1._pm_r,46._pm_r,0.22_pm_r,42._pm_r, &
241.70_pm_r,33814._pm_r,2._pm_r,231._pm_r,0.24_pm_r,266._pm_r,1._pm_r,46._pm_r,0.22_pm_r,46._pm_r, &
250.20_pm_r,37420._pm_r,2._pm_r,235._pm_r,0.14_pm_r,242._pm_r,2._pm_r,46._pm_r,0.19_pm_r,47._pm_r, &
257.30_pm_r,41136._pm_r,2._pm_r,233._pm_r,0.11_pm_r,170._pm_r,2._pm_r,46._pm_r,0.14_pm_r,46._pm_r, &
262.90_pm_r,44949._pm_r,2._pm_r,231._pm_r,0.12_pm_r,58._pm_r,2._pm_r,49._pm_r,0.13_pm_r,138._pm_r, &
265.10_pm_r,48821._pm_r,2._pm_r,229._pm_r,0.20_pm_r,83._pm_r,2._pm_r,57._pm_r,0.29_pm_r,132._pm_r, &
261.20_pm_r,52680._pm_r,2._pm_r,213._pm_r,0.57_pm_r,143._pm_r,2._pm_r,70._pm_r,0.44_pm_r,114._pm_r, &
252.00_pm_r,56445._pm_r,3._pm_r,187._pm_r,1.34_pm_r,158._pm_r,3._pm_r,78._pm_r,0.52_pm_r,98._pm_r, &
239.70_pm_r,60046._pm_r,5._pm_r,174._pm_r,1.72_pm_r,161._pm_r,4._pm_r,80._pm_r,0.34_pm_r,82._pm_r, &
225.20_pm_r,63456._pm_r,7._pm_r,170._pm_r,1.67_pm_r,163._pm_r,4._pm_r,79._pm_r,0.16_pm_r,37._pm_r, &
212.60_pm_r,66654._pm_r,9._pm_r,168._pm_r,1.24_pm_r,164._pm_r,4._pm_r,76._pm_r,0.21_pm_r,313._pm_r, &
204.70_pm_r,69706._pm_r,11._pm_r,168._pm_r,0.69_pm_r,167._pm_r,4._pm_r,71._pm_r,0.39_pm_r,292._pm_r, &
202.10_pm_r,72680._pm_r,11._pm_r,168._pm_r,0.17_pm_r,181._pm_r,3._pm_r,64._pm_r,0.54_pm_r,285._pm_r, &
202.30_pm_r,75639._pm_r,11._pm_r,168._pm_r,0.26_pm_r,331._pm_r,2._pm_r,50._pm_r,0.65_pm_r,281._pm_r, &
202.80_pm_r,78608._pm_r,11._pm_r,169._pm_r,0.54_pm_r,337._pm_r,2._pm_r,26._pm_r,0.71_pm_r,279._pm_r, &
201.60_pm_r,81582._pm_r,10._pm_r,170._pm_r,0.68_pm_r,338._pm_r,2._pm_r,354._pm_r,0.70_pm_r,278._pm_r, &
197.00_pm_r,84510._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.20_pm_r,87352._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,90123._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,92882._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,95652._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.20_pm_r,98440._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.20_pm_r,101269._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.30_pm_r,104199._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.30_pm_r,107364._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.20_pm_r,111086._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
346.30_pm_r,115961._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
301.00_pm_r,-99._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.70_pm_r,4139._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.40_pm_r,8028._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.90_pm_r,11544._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.80_pm_r,14682._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.00_pm_r,17593._pm_r,1._pm_r,155._pm_r,0.17_pm_r,273._pm_r,0._pm_r,203._pm_r,0.10_pm_r,63._pm_r, &
210.10_pm_r,20598._pm_r,1._pm_r,165._pm_r,0.22_pm_r,267._pm_r,0._pm_r,151._pm_r,0.14_pm_r,60._pm_r, &
217.70_pm_r,23736._pm_r,1._pm_r,180._pm_r,0.26_pm_r,262._pm_r,0._pm_r,99._pm_r,0.17_pm_r,60._pm_r, &
224.30_pm_r,26974._pm_r,1._pm_r,195._pm_r,0.27_pm_r,255._pm_r,1._pm_r,82._pm_r,0.17_pm_r,59._pm_r, &
231.80_pm_r,30314._pm_r,2._pm_r,207._pm_r,0.24_pm_r,247._pm_r,1._pm_r,76._pm_r,0.17_pm_r,57._pm_r, &
242.50_pm_r,33781._pm_r,2._pm_r,212._pm_r,0.18_pm_r,232._pm_r,1._pm_r,72._pm_r,0.14_pm_r,56._pm_r, &
251.00_pm_r,37395._pm_r,2._pm_r,213._pm_r,0.12_pm_r,202._pm_r,1._pm_r,70._pm_r,0.10_pm_r,55._pm_r, &
257.50_pm_r,41116._pm_r,2._pm_r,211._pm_r,0.11_pm_r,146._pm_r,1._pm_r,69._pm_r,0.06_pm_r,51._pm_r, &
262.60_pm_r,44930._pm_r,2._pm_r,208._pm_r,0.12_pm_r,54._pm_r,1._pm_r,73._pm_r,0.12_pm_r,185._pm_r, &
264.50_pm_r,48797._pm_r,2._pm_r,208._pm_r,0.15_pm_r,16._pm_r,1._pm_r,85._pm_r,0.23_pm_r,193._pm_r, &
261.30_pm_r,52652._pm_r,2._pm_r,209._pm_r,0.03_pm_r,225._pm_r,1._pm_r,99._pm_r,0.20_pm_r,173._pm_r, &
252.40_pm_r,56419._pm_r,2._pm_r,202._pm_r,0.35_pm_r,185._pm_r,2._pm_r,104._pm_r,0.25_pm_r,104._pm_r, &
240.30_pm_r,60026._pm_r,3._pm_r,191._pm_r,0.57_pm_r,173._pm_r,2._pm_r,100._pm_r,0.37_pm_r,75._pm_r, &
226.00_pm_r,63445._pm_r,4._pm_r,184._pm_r,0.64_pm_r,171._pm_r,3._pm_r,93._pm_r,0.37_pm_r,66._pm_r, &
212.80_pm_r,66653._pm_r,5._pm_r,181._pm_r,0.53_pm_r,172._pm_r,3._pm_r,87._pm_r,0.23_pm_r,62._pm_r, &
204.60_pm_r,69710._pm_r,6._pm_r,180._pm_r,0.39_pm_r,177._pm_r,3._pm_r,83._pm_r,0.04_pm_r,34._pm_r, &
202.30_pm_r,72688._pm_r,6._pm_r,180._pm_r,0.25_pm_r,195._pm_r,3._pm_r,81._pm_r,0.14_pm_r,250._pm_r, &
203.40_pm_r,75656._pm_r,7._pm_r,182._pm_r,0.16_pm_r,227._pm_r,3._pm_r,81._pm_r,0.30_pm_r,244._pm_r, &
204.20_pm_r,78637._pm_r,6._pm_r,185._pm_r,0.16_pm_r,257._pm_r,2._pm_r,82._pm_r,0.39_pm_r,245._pm_r, &
202.10_pm_r,81624._pm_r,6._pm_r,189._pm_r,0.17_pm_r,277._pm_r,2._pm_r,86._pm_r,0.42_pm_r,245._pm_r, &
196.30_pm_r,84543._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.80_pm_r,87372._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.60_pm_r,90145._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,92909._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,95684._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.90_pm_r,98481._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.80_pm_r,101328._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.10_pm_r,104293._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.30_pm_r,107510._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
267.10_pm_r,111305._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
351.20_pm_r,116259._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
301.20_pm_r,-110._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.10_pm_r,4132._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.50_pm_r,8024._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.10_pm_r,11541._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.20_pm_r,14683._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.50_pm_r,17600._pm_r,2._pm_r,168._pm_r,0.22_pm_r,274._pm_r,1._pm_r,165._pm_r,0.12_pm_r,22._pm_r, &
210.80_pm_r,20610._pm_r,2._pm_r,177._pm_r,0.29_pm_r,271._pm_r,1._pm_r,150._pm_r,0.16_pm_r,24._pm_r, &
218.60_pm_r,23757._pm_r,2._pm_r,189._pm_r,0.32_pm_r,266._pm_r,0._pm_r,119._pm_r,0.18_pm_r,23._pm_r, &
225.00_pm_r,27005._pm_r,2._pm_r,200._pm_r,0.33_pm_r,260._pm_r,0._pm_r,82._pm_r,0.19_pm_r,23._pm_r, &
231.80_pm_r,30349._pm_r,3._pm_r,208._pm_r,0.28_pm_r,250._pm_r,1._pm_r,61._pm_r,0.17_pm_r,26._pm_r, &
241.30_pm_r,33809._pm_r,3._pm_r,212._pm_r,0.21_pm_r,236._pm_r,1._pm_r,52._pm_r,0.14_pm_r,27._pm_r, &
250.00_pm_r,37410._pm_r,3._pm_r,213._pm_r,0.13_pm_r,199._pm_r,1._pm_r,48._pm_r,0.09_pm_r,33._pm_r, &
257.50_pm_r,41125._pm_r,3._pm_r,211._pm_r,0.14_pm_r,144._pm_r,1._pm_r,47._pm_r,0.05_pm_r,45._pm_r, &
263.40_pm_r,44944._pm_r,3._pm_r,208._pm_r,0.13_pm_r,99._pm_r,1._pm_r,51._pm_r,0.12_pm_r,167._pm_r, &
265.20_pm_r,48820._pm_r,3._pm_r,204._pm_r,0.18_pm_r,98._pm_r,1._pm_r,64._pm_r,0.21_pm_r,162._pm_r, &
260.40_pm_r,52673._pm_r,3._pm_r,198._pm_r,0.26_pm_r,124._pm_r,1._pm_r,80._pm_r,0.16_pm_r,152._pm_r, &
251.50_pm_r,56428._pm_r,4._pm_r,191._pm_r,0.42_pm_r,149._pm_r,1._pm_r,85._pm_r,0.10_pm_r,51._pm_r, &
240.70_pm_r,60034._pm_r,4._pm_r,186._pm_r,0.48_pm_r,163._pm_r,1._pm_r,74._pm_r,0.25_pm_r,13._pm_r, &
226.90_pm_r,63464._pm_r,5._pm_r,184._pm_r,0.50_pm_r,183._pm_r,2._pm_r,59._pm_r,0.33_pm_r,10._pm_r, &
214.60_pm_r,66690._pm_r,6._pm_r,186._pm_r,0.55_pm_r,206._pm_r,2._pm_r,49._pm_r,0.31_pm_r,17._pm_r, &
206.60_pm_r,69771._pm_r,6._pm_r,190._pm_r,0.64_pm_r,223._pm_r,2._pm_r,44._pm_r,0.26_pm_r,30._pm_r, &
203.60_pm_r,72770._pm_r,7._pm_r,195._pm_r,0.74_pm_r,234._pm_r,3._pm_r,44._pm_r,0.23_pm_r,52._pm_r, &
202.50_pm_r,75743._pm_r,8._pm_r,200._pm_r,0.83_pm_r,240._pm_r,3._pm_r,45._pm_r,0.22_pm_r,70._pm_r, &
202.60_pm_r,78711._pm_r,9._pm_r,206._pm_r,0.87_pm_r,243._pm_r,3._pm_r,49._pm_r,0.23_pm_r,81._pm_r, &
200.50_pm_r,81680._pm_r,10._pm_r,210._pm_r,0.86_pm_r,246._pm_r,4._pm_r,52._pm_r,0.24_pm_r,91._pm_r, &
195.10_pm_r,84576._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.90_pm_r,87391._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,90155._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.40_pm_r,92907._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,95669._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.40_pm_r,98465._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.10_pm_r,101332._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.30_pm_r,104345._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.60_pm_r,107644._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.10_pm_r,111544._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
356.80_pm_r,116596._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
300.00_pm_r,-134._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.50_pm_r,4100._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.20_pm_r,7991._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.40_pm_r,11507._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.30_pm_r,14665._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.80_pm_r,17617._pm_r,3._pm_r,150._pm_r,0.70_pm_r,274._pm_r,1._pm_r,165._pm_r,0.29_pm_r,360._pm_r, &
212.30_pm_r,20656._pm_r,3._pm_r,172._pm_r,0.85_pm_r,274._pm_r,1._pm_r,157._pm_r,0.39_pm_r,360._pm_r, &
219.70_pm_r,23822._pm_r,3._pm_r,201._pm_r,0.88_pm_r,274._pm_r,0._pm_r,123._pm_r,0.44_pm_r,360._pm_r, &
226.20_pm_r,27087._pm_r,3._pm_r,222._pm_r,0.80_pm_r,274._pm_r,1._pm_r,41._pm_r,0.46_pm_r,1._pm_r, &
232.80_pm_r,30449._pm_r,4._pm_r,234._pm_r,0.59_pm_r,272._pm_r,1._pm_r,19._pm_r,0.43_pm_r,1._pm_r, &
241.00_pm_r,33915._pm_r,5._pm_r,239._pm_r,0.32_pm_r,266._pm_r,2._pm_r,13._pm_r,0.35_pm_r,360._pm_r, &
250.00_pm_r,37513._pm_r,5._pm_r,240._pm_r,0.10_pm_r,223._pm_r,2._pm_r,10._pm_r,0.25_pm_r,359._pm_r, &
258.90_pm_r,41237._pm_r,5._pm_r,238._pm_r,0.18_pm_r,137._pm_r,2._pm_r,9._pm_r,0.14_pm_r,356._pm_r, &
265.70_pm_r,45085._pm_r,5._pm_r,235._pm_r,0.21_pm_r,166._pm_r,3._pm_r,9._pm_r,0.03_pm_r,162._pm_r, &
266.90_pm_r,48992._pm_r,5._pm_r,232._pm_r,0.29_pm_r,196._pm_r,2._pm_r,10._pm_r,0.16_pm_r,176._pm_r, &
259.80_pm_r,52853._pm_r,5._pm_r,230._pm_r,0.30_pm_r,206._pm_r,2._pm_r,10._pm_r,0.18_pm_r,202._pm_r, &
250.00_pm_r,56589._pm_r,6._pm_r,228._pm_r,0.19_pm_r,201._pm_r,2._pm_r,5._pm_r,0.23_pm_r,270._pm_r, &
239.30_pm_r,60172._pm_r,6._pm_r,227._pm_r,0.13_pm_r,178._pm_r,2._pm_r,353._pm_r,0.33_pm_r,300._pm_r, &
226.80_pm_r,63590._pm_r,6._pm_r,225._pm_r,0.18_pm_r,196._pm_r,3._pm_r,345._pm_r,0.35_pm_r,317._pm_r, &
215.90_pm_r,66825._pm_r,7._pm_r,224._pm_r,0.37_pm_r,217._pm_r,3._pm_r,342._pm_r,0.25_pm_r,336._pm_r, &
208.80_pm_r,69932._pm_r,7._pm_r,224._pm_r,0.63_pm_r,223._pm_r,3._pm_r,343._pm_r,0.17_pm_r,25._pm_r, &
205.40_pm_r,72962._pm_r,8._pm_r,224._pm_r,0.85_pm_r,226._pm_r,3._pm_r,348._pm_r,0.24_pm_r,75._pm_r, &
202.80_pm_r,75951._pm_r,10._pm_r,224._pm_r,1.03_pm_r,227._pm_r,3._pm_r,355._pm_r,0.36_pm_r,92._pm_r, &
201.10_pm_r,78906._pm_r,11._pm_r,225._pm_r,1.13_pm_r,228._pm_r,3._pm_r,6._pm_r,0.43_pm_r,98._pm_r, &
198.00_pm_r,81842._pm_r,13._pm_r,225._pm_r,1.15_pm_r,229._pm_r,3._pm_r,17._pm_r,0.46_pm_r,101._pm_r, &
192.20_pm_r,84698._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,87469._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.80_pm_r,90188._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.30_pm_r,92892._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.60_pm_r,95614._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.10_pm_r,98390._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.50_pm_r,101271._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.40_pm_r,104340._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
233.50_pm_r,107740._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
282.40_pm_r,111770._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
362.70_pm_r,116929._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
296.70_pm_r,-106._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
275.70_pm_r,4087._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
250.80_pm_r,7945._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.20_pm_r,11432._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.40_pm_r,14609._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.30_pm_r,17631._pm_r,4._pm_r,163._pm_r,0.92_pm_r,284._pm_r,2._pm_r,157._pm_r,0.29_pm_r,352._pm_r, &
214.40_pm_r,20718._pm_r,3._pm_r,186._pm_r,1.11_pm_r,284._pm_r,1._pm_r,151._pm_r,0.38_pm_r,352._pm_r, &
220.50_pm_r,23904._pm_r,4._pm_r,214._pm_r,1.12_pm_r,284._pm_r,1._pm_r,132._pm_r,0.44_pm_r,352._pm_r, &
227.10_pm_r,27179._pm_r,4._pm_r,234._pm_r,0.98_pm_r,283._pm_r,1._pm_r,63._pm_r,0.46_pm_r,350._pm_r, &
233.70_pm_r,30553._pm_r,5._pm_r,244._pm_r,0.67_pm_r,280._pm_r,1._pm_r,18._pm_r,0.43_pm_r,349._pm_r, &
242.90_pm_r,34038._pm_r,6._pm_r,248._pm_r,0.31_pm_r,266._pm_r,1._pm_r,6._pm_r,0.36_pm_r,346._pm_r, &
252.50_pm_r,37668._pm_r,6._pm_r,247._pm_r,0.15_pm_r,189._pm_r,2._pm_r,1._pm_r,0.27_pm_r,340._pm_r, &
262.00_pm_r,41433._pm_r,6._pm_r,244._pm_r,0.29_pm_r,152._pm_r,2._pm_r,357._pm_r,0.19_pm_r,331._pm_r, &
268.60_pm_r,45327._pm_r,6._pm_r,240._pm_r,0.35_pm_r,175._pm_r,2._pm_r,354._pm_r,0.15_pm_r,308._pm_r, &
268.10_pm_r,49265._pm_r,7._pm_r,236._pm_r,0.46_pm_r,209._pm_r,3._pm_r,349._pm_r,0.22_pm_r,302._pm_r, &
260.70_pm_r,53141._pm_r,7._pm_r,235._pm_r,0.58_pm_r,235._pm_r,3._pm_r,343._pm_r,0.28_pm_r,297._pm_r, &
250.20_pm_r,56886._pm_r,8._pm_r,236._pm_r,0.60_pm_r,258._pm_r,3._pm_r,338._pm_r,0.24_pm_r,283._pm_r, &
238.60_pm_r,60464._pm_r,9._pm_r,239._pm_r,0.49_pm_r,276._pm_r,3._pm_r,333._pm_r,0.15_pm_r,257._pm_r, &
226.40_pm_r,63872._pm_r,9._pm_r,242._pm_r,0.37_pm_r,295._pm_r,3._pm_r,330._pm_r,0.11_pm_r,238._pm_r, &
216.40_pm_r,67108._pm_r,10._pm_r,244._pm_r,0.24_pm_r,311._pm_r,3._pm_r,327._pm_r,0.08_pm_r,264._pm_r, &
209.10_pm_r,70224._pm_r,10._pm_r,245._pm_r,0.13_pm_r,337._pm_r,3._pm_r,326._pm_r,0.12_pm_r,310._pm_r, &
204.10_pm_r,73247._pm_r,10._pm_r,246._pm_r,0.07_pm_r,37._pm_r,4._pm_r,326._pm_r,0.19_pm_r,328._pm_r, &
200.10_pm_r,76208._pm_r,10._pm_r,246._pm_r,0.11_pm_r,87._pm_r,4._pm_r,326._pm_r,0.26_pm_r,332._pm_r, &
196.70_pm_r,79112._pm_r,9._pm_r,245._pm_r,0.15_pm_r,102._pm_r,4._pm_r,327._pm_r,0.30_pm_r,333._pm_r, &
192.20_pm_r,81969._pm_r,9._pm_r,245._pm_r,0.17_pm_r,107._pm_r,5._pm_r,327._pm_r,0.30_pm_r,333._pm_r, &
185.90_pm_r,84734._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
180.70_pm_r,87412._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
177.60_pm_r,90035._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
177.90_pm_r,92652._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.00_pm_r,95304._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,98042._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.70_pm_r,100930._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.30_pm_r,104058._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.70_pm_r,107577._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
290.90_pm_r,111749._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
368.10_pm_r,117013._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
290.20_pm_r,-43._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
271.40_pm_r,4068._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
245.40_pm_r,7851._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.90_pm_r,11279._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.20_pm_r,14479._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.00_pm_r,17606._pm_r,3._pm_r,210._pm_r,0.35_pm_r,266._pm_r,2._pm_r,143._pm_r,0.27_pm_r,336._pm_r, &
217.70_pm_r,20766._pm_r,3._pm_r,218._pm_r,0.43_pm_r,263._pm_r,1._pm_r,139._pm_r,0.37_pm_r,336._pm_r, &
221.90_pm_r,23984._pm_r,4._pm_r,225._pm_r,0.46_pm_r,257._pm_r,1._pm_r,128._pm_r,0.43_pm_r,335._pm_r, &
228.10_pm_r,27275._pm_r,4._pm_r,229._pm_r,0.44_pm_r,250._pm_r,0._pm_r,80._pm_r,0.46_pm_r,334._pm_r, &
235.60_pm_r,30671._pm_r,5._pm_r,231._pm_r,0.36_pm_r,238._pm_r,1._pm_r,8._pm_r,0.42_pm_r,333._pm_r, &
245.10_pm_r,34186._pm_r,5._pm_r,231._pm_r,0.28_pm_r,222._pm_r,1._pm_r,352._pm_r,0.34_pm_r,330._pm_r, &
255.50_pm_r,37854._pm_r,6._pm_r,230._pm_r,0.24_pm_r,204._pm_r,2._pm_r,346._pm_r,0.25_pm_r,327._pm_r, &
266.00_pm_r,41670._pm_r,6._pm_r,228._pm_r,0.20_pm_r,195._pm_r,2._pm_r,342._pm_r,0.16_pm_r,317._pm_r, &
272.20_pm_r,45621._pm_r,6._pm_r,228._pm_r,0.43_pm_r,44._pm_r,2._pm_r,332._pm_r,0.58_pm_r,277._pm_r, &
269.80_pm_r,49598._pm_r,5._pm_r,230._pm_r,1.47_pm_r,41._pm_r,3._pm_r,309._pm_r,1.42_pm_r,274._pm_r, &
262.40_pm_r,53496._pm_r,2._pm_r,239._pm_r,1.40_pm_r,39._pm_r,5._pm_r,295._pm_r,1.46_pm_r,274._pm_r, &
251.80_pm_r,57266._pm_r,1._pm_r,263._pm_r,0.15_pm_r,332._pm_r,7._pm_r,290._pm_r,0.51_pm_r,274._pm_r, &
240.50_pm_r,60868._pm_r,2._pm_r,251._pm_r,1.19_pm_r,230._pm_r,7._pm_r,290._pm_r,0.41_pm_r,93._pm_r, &
229.10_pm_r,64310._pm_r,4._pm_r,241._pm_r,1.33_pm_r,230._pm_r,6._pm_r,293._pm_r,0.53_pm_r,89._pm_r, &
217.90_pm_r,67580._pm_r,6._pm_r,239._pm_r,0.56_pm_r,239._pm_r,6._pm_r,295._pm_r,0.11_pm_r,350._pm_r, &
208.10_pm_r,70701._pm_r,6._pm_r,241._pm_r,0.71_pm_r,31._pm_r,6._pm_r,295._pm_r,0.94_pm_r,284._pm_r, &
199.80_pm_r,73685._pm_r,4._pm_r,254._pm_r,2.17_pm_r,41._pm_r,8._pm_r,291._pm_r,2.04_pm_r,280._pm_r, &
191.90_pm_r,76556._pm_r,2._pm_r,343._pm_r,3.77_pm_r,43._pm_r,12._pm_r,287._pm_r,3.26_pm_r,278._pm_r, &
184.90_pm_r,79309._pm_r,8._pm_r,29._pm_r,4.70_pm_r,43._pm_r,18._pm_r,284._pm_r,3.99_pm_r,278._pm_r, &
179.10_pm_r,81970._pm_r,14._pm_r,35._pm_r,4.20_pm_r,43._pm_r,23._pm_r,283._pm_r,3.53_pm_r,278._pm_r, &
174.70_pm_r,84563._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
171.20_pm_r,87092._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
169.00_pm_r,89579._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
170.80_pm_r,92077._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
176.20_pm_r,94638._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.40_pm_r,97324._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.00_pm_r,100214._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.40_pm_r,103410._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.90_pm_r,107061._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
298.60_pm_r,111379._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
372.90_pm_r,116728._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
283.20_pm_r,-13._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
266.20_pm_r,4006._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
240.60_pm_r,7712._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.70_pm_r,11100._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.00_pm_r,14338._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.60_pm_r,17563._pm_r,2._pm_r,264._pm_r,0.41_pm_r,194._pm_r,2._pm_r,137._pm_r,0.21_pm_r,5._pm_r, &
222.00_pm_r,20803._pm_r,3._pm_r,249._pm_r,0.54_pm_r,195._pm_r,1._pm_r,124._pm_r,0.30_pm_r,3._pm_r, &
224.10_pm_r,24066._pm_r,3._pm_r,237._pm_r,0.61_pm_r,199._pm_r,1._pm_r,102._pm_r,0.35_pm_r,0._pm_r, &
229.30_pm_r,27381._pm_r,4._pm_r,229._pm_r,0.59_pm_r,202._pm_r,1._pm_r,72._pm_r,0.38_pm_r,356._pm_r, &
237.50_pm_r,30799._pm_r,5._pm_r,225._pm_r,0.48_pm_r,209._pm_r,1._pm_r,48._pm_r,0.35_pm_r,350._pm_r, &
248.10_pm_r,34349._pm_r,5._pm_r,224._pm_r,0.33_pm_r,221._pm_r,2._pm_r,33._pm_r,0.29_pm_r,342._pm_r, &
259.70_pm_r,38072._pm_r,6._pm_r,224._pm_r,0.21_pm_r,238._pm_r,2._pm_r,23._pm_r,0.22_pm_r,331._pm_r, &
270.00_pm_r,41950._pm_r,6._pm_r,225._pm_r,0.14_pm_r,260._pm_r,2._pm_r,16._pm_r,0.17_pm_r,317._pm_r, &
275.10_pm_r,45951._pm_r,6._pm_r,226._pm_r,0.15_pm_r,235._pm_r,2._pm_r,10._pm_r,0.16_pm_r,297._pm_r, &
273.60_pm_r,49975._pm_r,6._pm_r,226._pm_r,0.27_pm_r,208._pm_r,2._pm_r,4._pm_r,0.15_pm_r,292._pm_r, &
267.00_pm_r,53936._pm_r,7._pm_r,224._pm_r,0.38_pm_r,208._pm_r,2._pm_r,359._pm_r,0.12_pm_r,316._pm_r, &
256.20_pm_r,57774._pm_r,7._pm_r,224._pm_r,0.41_pm_r,220._pm_r,2._pm_r,358._pm_r,0.17_pm_r,353._pm_r, &
244.00_pm_r,61434._pm_r,8._pm_r,224._pm_r,0.29_pm_r,236._pm_r,3._pm_r,357._pm_r,0.18_pm_r,352._pm_r, &
231.50_pm_r,64919._pm_r,8._pm_r,224._pm_r,0.10_pm_r,259._pm_r,3._pm_r,356._pm_r,0.17_pm_r,330._pm_r, &
219.40_pm_r,68217._pm_r,8._pm_r,225._pm_r,0.10_pm_r,44._pm_r,3._pm_r,353._pm_r,0.20_pm_r,292._pm_r, &
208.20_pm_r,71349._pm_r,8._pm_r,224._pm_r,0.28_pm_r,63._pm_r,3._pm_r,347._pm_r,0.27_pm_r,270._pm_r, &
197.50_pm_r,74317._pm_r,7._pm_r,223._pm_r,0.39_pm_r,68._pm_r,3._pm_r,339._pm_r,0.34_pm_r,259._pm_r, &
186.50_pm_r,77131._pm_r,7._pm_r,221._pm_r,0.45_pm_r,71._pm_r,3._pm_r,330._pm_r,0.37_pm_r,254._pm_r, &
176.10_pm_r,79781._pm_r,6._pm_r,218._pm_r,0.45_pm_r,72._pm_r,4._pm_r,322._pm_r,0.37_pm_r,251._pm_r, &
167.70_pm_r,82288._pm_r,6._pm_r,214._pm_r,0.41_pm_r,73._pm_r,4._pm_r,314._pm_r,0.33_pm_r,251._pm_r, &
162.40_pm_r,84699._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
159.90_pm_r,87049._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
159.30_pm_r,89381._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
163.00_pm_r,91749._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
170.90_pm_r,94212._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.50_pm_r,96840._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.30_pm_r,99729._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.10_pm_r,102997._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
263.70_pm_r,106792._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
304.40_pm_r,111246._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
376.70_pm_r,116655._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
281.50_pm_r,-32._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.40_pm_r,3942._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
237.30_pm_r,7594._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.60_pm_r,10969._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.30_pm_r,14249._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.20_pm_r,17540._pm_r,1._pm_r,254._pm_r,0.26_pm_r,230._pm_r,1._pm_r,84._pm_r,0.04_pm_r,337._pm_r, &
226.10_pm_r,20845._pm_r,2._pm_r,247._pm_r,0.35_pm_r,231._pm_r,1._pm_r,81._pm_r,0.06_pm_r,329._pm_r, &
227.10_pm_r,24160._pm_r,2._pm_r,243._pm_r,0.40_pm_r,230._pm_r,1._pm_r,77._pm_r,0.08_pm_r,330._pm_r, &
231.70_pm_r,27513._pm_r,3._pm_r,240._pm_r,0.39_pm_r,228._pm_r,1._pm_r,70._pm_r,0.10_pm_r,323._pm_r, &
240.50_pm_r,30971._pm_r,3._pm_r,238._pm_r,0.32_pm_r,227._pm_r,1._pm_r,62._pm_r,0.11_pm_r,322._pm_r, &
251.50_pm_r,34569._pm_r,4._pm_r,237._pm_r,0.23_pm_r,223._pm_r,1._pm_r,54._pm_r,0.12_pm_r,318._pm_r, &
263.40_pm_r,38344._pm_r,4._pm_r,235._pm_r,0.15_pm_r,215._pm_r,1._pm_r,44._pm_r,0.12_pm_r,312._pm_r, &
274.00_pm_r,42278._pm_r,4._pm_r,234._pm_r,0.10_pm_r,204._pm_r,1._pm_r,35._pm_r,0.11_pm_r,315._pm_r, &
278.90_pm_r,46335._pm_r,4._pm_r,233._pm_r,0.15_pm_r,200._pm_r,1._pm_r,27._pm_r,0.18_pm_r,352._pm_r, &
277.00_pm_r,50412._pm_r,4._pm_r,231._pm_r,0.18_pm_r,209._pm_r,2._pm_r,22._pm_r,0.24_pm_r,12._pm_r, &
270.10_pm_r,54420._pm_r,5._pm_r,231._pm_r,0.30_pm_r,251._pm_r,2._pm_r,22._pm_r,0.05_pm_r,77._pm_r, &
259.70_pm_r,58305._pm_r,5._pm_r,235._pm_r,0.62_pm_r,273._pm_r,2._pm_r,27._pm_r,0.44_pm_r,185._pm_r, &
247.70_pm_r,62018._pm_r,6._pm_r,242._pm_r,0.74_pm_r,279._pm_r,1._pm_r,49._pm_r,0.67_pm_r,189._pm_r, &
235.00_pm_r,65556._pm_r,7._pm_r,247._pm_r,0.69_pm_r,285._pm_r,1._pm_r,145._pm_r,0.64_pm_r,188._pm_r, &
221.90_pm_r,68898._pm_r,8._pm_r,252._pm_r,0.48_pm_r,294._pm_r,1._pm_r,165._pm_r,0.37_pm_r,175._pm_r, &
208.60_pm_r,72055._pm_r,8._pm_r,255._pm_r,0.25_pm_r,320._pm_r,2._pm_r,164._pm_r,0.16_pm_r,104._pm_r, &
194.60_pm_r,75004._pm_r,8._pm_r,257._pm_r,0.18_pm_r,24._pm_r,2._pm_r,151._pm_r,0.37_pm_r,45._pm_r, &
180.20_pm_r,77753._pm_r,8._pm_r,258._pm_r,0.28_pm_r,63._pm_r,2._pm_r,126._pm_r,0.54_pm_r,34._pm_r, &
167.30_pm_r,80288._pm_r,7._pm_r,258._pm_r,0.36_pm_r,72._pm_r,2._pm_r,95._pm_r,0.60_pm_r,30._pm_r, &
156.90_pm_r,82650._pm_r,7._pm_r,259._pm_r,0.35_pm_r,75._pm_r,2._pm_r,73._pm_r,0.58_pm_r,28._pm_r, &
150.70_pm_r,84880._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
149.00_pm_r,87057._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
150.20_pm_r,89241._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
155.60_pm_r,91485._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
165.80_pm_r,93854._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.50_pm_r,96426._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.50_pm_r,99313._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
237.20_pm_r,102657._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.40_pm_r,106601._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
308.10_pm_r,111171._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
379.60_pm_r,116614._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
276.40_pm_r,-30._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
259.50_pm_r,3883._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
234.90_pm_r,7493._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.30_pm_r,10868._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.80_pm_r,14191._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.80_pm_r,17533._pm_r,1._pm_r,172._pm_r,0.17_pm_r,287._pm_r,1._pm_r,53._pm_r,0.09_pm_r,215._pm_r, &
229.90_pm_r,20892._pm_r,1._pm_r,203._pm_r,0.23_pm_r,280._pm_r,1._pm_r,56._pm_r,0.14_pm_r,216._pm_r, &
229.90_pm_r,24254._pm_r,1._pm_r,230._pm_r,0.27_pm_r,267._pm_r,1._pm_r,60._pm_r,0.17_pm_r,219._pm_r, &
233.90_pm_r,27643._pm_r,1._pm_r,241._pm_r,0.28_pm_r,252._pm_r,1._pm_r,68._pm_r,0.19_pm_r,221._pm_r, &
243.00_pm_r,31135._pm_r,2._pm_r,241._pm_r,0.28_pm_r,232._pm_r,1._pm_r,82._pm_r,0.19_pm_r,223._pm_r, &
254.70_pm_r,34774._pm_r,2._pm_r,237._pm_r,0.28_pm_r,213._pm_r,0._pm_r,111._pm_r,0.16_pm_r,229._pm_r, &
267.00_pm_r,38598._pm_r,2._pm_r,231._pm_r,0.27_pm_r,198._pm_r,0._pm_r,152._pm_r,0.12_pm_r,238._pm_r, &
277.60_pm_r,42586._pm_r,3._pm_r,226._pm_r,0.25_pm_r,187._pm_r,0._pm_r,181._pm_r,0.08_pm_r,253._pm_r, &
282.60_pm_r,46695._pm_r,3._pm_r,223._pm_r,0.22_pm_r,215._pm_r,0._pm_r,193._pm_r,0.03_pm_r,232._pm_r, &
281.30_pm_r,50832._pm_r,3._pm_r,224._pm_r,0.28_pm_r,246._pm_r,0._pm_r,193._pm_r,0.02_pm_r,90._pm_r, &
274.10_pm_r,54901._pm_r,4._pm_r,227._pm_r,0.28_pm_r,251._pm_r,0._pm_r,194._pm_r,0.08_pm_r,349._pm_r, &
263.30_pm_r,58842._pm_r,4._pm_r,229._pm_r,0.22_pm_r,231._pm_r,0._pm_r,233._pm_r,0.20_pm_r,328._pm_r, &
249.70_pm_r,62597._pm_r,4._pm_r,228._pm_r,0.20_pm_r,202._pm_r,0._pm_r,289._pm_r,0.19_pm_r,318._pm_r, &
235.90_pm_r,66156._pm_r,5._pm_r,226._pm_r,0.23_pm_r,203._pm_r,1._pm_r,296._pm_r,0.07_pm_r,286._pm_r, &
222.30_pm_r,69508._pm_r,5._pm_r,225._pm_r,0.31_pm_r,227._pm_r,1._pm_r,286._pm_r,0.14_pm_r,163._pm_r, &
207.40_pm_r,72660._pm_r,5._pm_r,226._pm_r,0.42_pm_r,241._pm_r,0._pm_r,250._pm_r,0.32_pm_r,150._pm_r, &
191.90_pm_r,75580._pm_r,6._pm_r,228._pm_r,0.51_pm_r,250._pm_r,1._pm_r,187._pm_r,0.43_pm_r,145._pm_r, &
176.40_pm_r,78281._pm_r,7._pm_r,231._pm_r,0.53_pm_r,253._pm_r,1._pm_r,165._pm_r,0.48_pm_r,142._pm_r, &
163.00_pm_r,80756._pm_r,8._pm_r,233._pm_r,0.49_pm_r,256._pm_r,2._pm_r,156._pm_r,0.47_pm_r,141._pm_r, &
150.80_pm_r,83049._pm_r,8._pm_r,235._pm_r,0.43_pm_r,258._pm_r,3._pm_r,152._pm_r,0.41_pm_r,141._pm_r, &
142.20_pm_r,85150._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
140.20_pm_r,87187._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
142.70_pm_r,89251._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
149.50_pm_r,91395._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
161.30_pm_r,93685._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.60_pm_r,96207._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.50_pm_r,99090._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
244.70_pm_r,102505._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
283.80_pm_r,106584._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
309.90_pm_r,111244._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
381.70_pm_r,116701._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /) 
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
271.30_pm_r,-14._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
256.70_pm_r,3839._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
233.10_pm_r,7414._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.80_pm_r,10793._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.70_pm_r,14154._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
231.40_pm_r,17536._pm_r,0._pm_r,99._pm_r,0.07_pm_r,155._pm_r,1._pm_r,38._pm_r,0.07_pm_r,200._pm_r, &
232.40_pm_r,20933._pm_r,0._pm_r,144._pm_r,0.09_pm_r,180._pm_r,1._pm_r,41._pm_r,0.11_pm_r,202._pm_r, &
232.60_pm_r,24335._pm_r,0._pm_r,173._pm_r,0.14_pm_r,213._pm_r,1._pm_r,48._pm_r,0.13_pm_r,202._pm_r, &
236.00_pm_r,27761._pm_r,1._pm_r,198._pm_r,0.22_pm_r,236._pm_r,0._pm_r,63._pm_r,0.15_pm_r,200._pm_r, &
244.40_pm_r,31280._pm_r,1._pm_r,218._pm_r,0.30_pm_r,248._pm_r,0._pm_r,98._pm_r,0.14_pm_r,200._pm_r, &
255.80_pm_r,34936._pm_r,1._pm_r,231._pm_r,0.34_pm_r,255._pm_r,0._pm_r,140._pm_r,0.13_pm_r,203._pm_r, &
268.60_pm_r,38780._pm_r,2._pm_r,238._pm_r,0.35_pm_r,258._pm_r,0._pm_r,163._pm_r,0.10_pm_r,203._pm_r, &
279.80_pm_r,42795._pm_r,2._pm_r,242._pm_r,0.30_pm_r,258._pm_r,1._pm_r,173._pm_r,0.08_pm_r,203._pm_r, &
285.70_pm_r,46944._pm_r,3._pm_r,244._pm_r,0.19_pm_r,233._pm_r,1._pm_r,175._pm_r,0.03_pm_r,112._pm_r, &
285.00_pm_r,51130._pm_r,3._pm_r,239._pm_r,0.26_pm_r,171._pm_r,1._pm_r,162._pm_r,0.16_pm_r,72._pm_r, &
277.40_pm_r,55251._pm_r,3._pm_r,230._pm_r,0.38_pm_r,152._pm_r,1._pm_r,129._pm_r,0.30_pm_r,61._pm_r, &
265.80_pm_r,59235._pm_r,3._pm_r,219._pm_r,0.34_pm_r,148._pm_r,1._pm_r,98._pm_r,0.36_pm_r,52._pm_r, &
251.00_pm_r,63017._pm_r,3._pm_r,214._pm_r,0.13_pm_r,162._pm_r,1._pm_r,82._pm_r,0.24_pm_r,44._pm_r, &
236.30_pm_r,66589._pm_r,3._pm_r,213._pm_r,0.11_pm_r,236._pm_r,1._pm_r,76._pm_r,0.07_pm_r,56._pm_r, &
222.20_pm_r,69943._pm_r,3._pm_r,215._pm_r,0.17_pm_r,248._pm_r,1._pm_r,79._pm_r,0.13_pm_r,189._pm_r, &
206.40_pm_r,73089._pm_r,4._pm_r,217._pm_r,0.24_pm_r,235._pm_r,1._pm_r,90._pm_r,0.27_pm_r,193._pm_r, &
189.90_pm_r,75987._pm_r,4._pm_r,218._pm_r,0.27_pm_r,227._pm_r,1._pm_r,110._pm_r,0.35_pm_r,190._pm_r, &
174.00_pm_r,78656._pm_r,4._pm_r,219._pm_r,0.28_pm_r,218._pm_r,2._pm_r,130._pm_r,0.37_pm_r,189._pm_r, &
160.20_pm_r,81091._pm_r,5._pm_r,218._pm_r,0.25_pm_r,213._pm_r,2._pm_r,144._pm_r,0.36_pm_r,188._pm_r, &
147.00_pm_r,83341._pm_r,5._pm_r,218._pm_r,0.21_pm_r,211._pm_r,2._pm_r,153._pm_r,0.30_pm_r,189._pm_r, &
136.70_pm_r,85360._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
134.50_pm_r,87307._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
137.90_pm_r,89293._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
145.50_pm_r,91371._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
158.20_pm_r,93607._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
178.20_pm_r,96095._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.90_pm_r,98973._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
250.20_pm_r,102438._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
290.40_pm_r,106617._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
310.40_pm_r,111332._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
382.90_pm_r,116793._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)


  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_juin = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_juin,(/10,nb_alt,nb_lat/))


  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)

end subroutine cps_atmi_juin

subroutine cps_atmi_juillet (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_juillet
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JUILLET
!
!$Description
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JUILLET
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_juillet (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments     
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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


  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
265.20_pm_r,-354._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
240.50_pm_r,3337._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.50_pm_r,6687._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.40_pm_r,9767._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.30_pm_r,12684._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.50_pm_r,15521._pm_r,4._pm_r,288._pm_r,1.53_pm_r,207._pm_r,4._pm_r,185._pm_r,0.42_pm_r,3._pm_r, &
187.90_pm_r,18299._pm_r,5._pm_r,256._pm_r,2.34_pm_r,205._pm_r,3._pm_r,185._pm_r,0.75_pm_r,6._pm_r, &
185.00_pm_r,21031._pm_r,8._pm_r,234._pm_r,2.46_pm_r,198._pm_r,2._pm_r,182._pm_r,1.03_pm_r,11._pm_r, &
178.00_pm_r,23689._pm_r,10._pm_r,223._pm_r,1.13_pm_r,158._pm_r,1._pm_r,163._pm_r,0.63_pm_r,14._pm_r, &
185.10_pm_r,26330._pm_r,9._pm_r,215._pm_r,1.52_pm_r,70._pm_r,0._pm_r,101._pm_r,0.21_pm_r,33._pm_r, &
204.40_pm_r,29173._pm_r,7._pm_r,205._pm_r,2.51_pm_r,52._pm_r,1._pm_r,94._pm_r,0.14_pm_r,150._pm_r, &
222.30_pm_r,32305._pm_r,3._pm_r,176._pm_r,3.04_pm_r,47._pm_r,1._pm_r,121._pm_r,0.30_pm_r,177._pm_r, &
236.90_pm_r,35667._pm_r,4._pm_r,93._pm_r,3.17_pm_r,45._pm_r,1._pm_r,147._pm_r,0.40_pm_r,187._pm_r, &
246.60_pm_r,39212._pm_r,7._pm_r,69._pm_r,2.43_pm_r,54._pm_r,2._pm_r,159._pm_r,0.25_pm_r,180._pm_r, &
256.00_pm_r,42889._pm_r,10._pm_r,66._pm_r,1.95_pm_r,65._pm_r,2._pm_r,160._pm_r,0.11_pm_r,146._pm_r, &
266.00_pm_r,46714._pm_r,13._pm_r,66._pm_r,1.84_pm_r,66._pm_r,2._pm_r,159._pm_r,0.02_pm_r,63._pm_r, &
273.10_pm_r,50665._pm_r,16._pm_r,65._pm_r,1.99_pm_r,52._pm_r,2._pm_r,161._pm_r,0.14_pm_r,270._pm_r, &
270.80_pm_r,54657._pm_r,18._pm_r,62._pm_r,1.96_pm_r,35._pm_r,2._pm_r,174._pm_r,0.37_pm_r,257._pm_r, &
261.80_pm_r,58565._pm_r,21._pm_r,58._pm_r,1.80_pm_r,20._pm_r,2._pm_r,194._pm_r,0.52_pm_r,252._pm_r, &
250.40_pm_r,62313._pm_r,23._pm_r,53._pm_r,1.51_pm_r,6._pm_r,3._pm_r,210._pm_r,0.56_pm_r,250._pm_r, &
239.80_pm_r,65903._pm_r,24._pm_r,49._pm_r,1.18_pm_r,349._pm_r,3._pm_r,219._pm_r,0.51_pm_r,247._pm_r, &
231.90_pm_r,69354._pm_r,24._pm_r,46._pm_r,0.93_pm_r,325._pm_r,4._pm_r,224._pm_r,0.42_pm_r,242._pm_r, &
226.30_pm_r,72709._pm_r,24._pm_r,43._pm_r,0.87_pm_r,296._pm_r,4._pm_r,226._pm_r,0.35_pm_r,237._pm_r, &
222.70_pm_r,75990._pm_r,24._pm_r,40._pm_r,0.96_pm_r,274._pm_r,5._pm_r,226._pm_r,0.25_pm_r,231._pm_r, &
221.40_pm_r,79226._pm_r,23._pm_r,37._pm_r,1.04_pm_r,261._pm_r,5._pm_r,226._pm_r,0.19_pm_r,223._pm_r, &
223.30_pm_r,82468._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.90_pm_r,85749._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.00_pm_r,88975._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.20_pm_r,92062._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.50_pm_r,95069._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.30_pm_r,98075._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.10_pm_r,101157._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.20_pm_r,104406._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
236.90_pm_r,107918._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.80_pm_r,111822._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
321.10_pm_r,116430._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
270.00_pm_r,-321._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.00_pm_r,3425._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.40_pm_r,6809._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.80_pm_r,9921._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.30_pm_r,12886._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.20_pm_r,15793._pm_r,13._pm_r,240._pm_r,2.23_pm_r,193._pm_r,4._pm_r,188._pm_r,0.45_pm_r,343._pm_r, &
195.00_pm_r,18665._pm_r,16._pm_r,229._pm_r,3.10_pm_r,185._pm_r,4._pm_r,193._pm_r,0.66_pm_r,347._pm_r, &
192.50_pm_r,21503._pm_r,19._pm_r,217._pm_r,3.09_pm_r,162._pm_r,3._pm_r,201._pm_r,0.64_pm_r,358._pm_r, &
186.70_pm_r,24278._pm_r,20._pm_r,206._pm_r,2.84_pm_r,109._pm_r,2._pm_r,205._pm_r,0.37_pm_r,35._pm_r, &
192.30_pm_r,27040._pm_r,18._pm_r,192._pm_r,4.06_pm_r,68._pm_r,2._pm_r,192._pm_r,0.40_pm_r,107._pm_r, &
208.50_pm_r,29964._pm_r,14._pm_r,172._pm_r,5.33_pm_r,51._pm_r,2._pm_r,172._pm_r,0.63_pm_r,133._pm_r, &
225.20_pm_r,33146._pm_r,12._pm_r,135._pm_r,5.89_pm_r,43._pm_r,3._pm_r,161._pm_r,0.75_pm_r,145._pm_r, &
239.90_pm_r,36550._pm_r,14._pm_r,96._pm_r,5.79_pm_r,36._pm_r,4._pm_r,158._pm_r,0.77_pm_r,153._pm_r, &
249.90_pm_r,40142._pm_r,19._pm_r,75._pm_r,4.73_pm_r,34._pm_r,5._pm_r,158._pm_r,0.32_pm_r,164._pm_r, &
258.90_pm_r,43864._pm_r,24._pm_r,65._pm_r,3.77_pm_r,33._pm_r,5._pm_r,159._pm_r,0.19_pm_r,278._pm_r, &
266.60_pm_r,47718._pm_r,28._pm_r,60._pm_r,3.07_pm_r,30._pm_r,5._pm_r,163._pm_r,0.44_pm_r,291._pm_r, &
269.70_pm_r,51651._pm_r,31._pm_r,55._pm_r,2.59_pm_r,21._pm_r,4._pm_r,172._pm_r,0.55_pm_r,271._pm_r, &
265.50_pm_r,55576._pm_r,34._pm_r,52._pm_r,1.96_pm_r,360._pm_r,5._pm_r,182._pm_r,0.62_pm_r,238._pm_r, &
256.70_pm_r,59406._pm_r,35._pm_r,48._pm_r,1.52_pm_r,332._pm_r,5._pm_r,190._pm_r,0.73_pm_r,217._pm_r, &
246.30_pm_r,63086._pm_r,35._pm_r,45._pm_r,1.28_pm_r,303._pm_r,6._pm_r,193._pm_r,0.80_pm_r,206._pm_r, &
237.00_pm_r,66626._pm_r,34._pm_r,42._pm_r,1.21_pm_r,273._pm_r,8._pm_r,194._pm_r,0.80_pm_r,197._pm_r, &
229.60_pm_r,70039._pm_r,33._pm_r,40._pm_r,1.28_pm_r,248._pm_r,9._pm_r,194._pm_r,0.76_pm_r,189._pm_r, &
224.00_pm_r,73360._pm_r,31._pm_r,39._pm_r,1.45_pm_r,229._pm_r,10._pm_r,193._pm_r,0.72_pm_r,182._pm_r, &
220.30_pm_r,76608._pm_r,29._pm_r,39._pm_r,1.61_pm_r,218._pm_r,11._pm_r,192._pm_r,0.68_pm_r,177._pm_r, &
219.00_pm_r,79809._pm_r,26._pm_r,39._pm_r,1.68_pm_r,210._pm_r,12._pm_r,191._pm_r,0.62_pm_r,171._pm_r, &
221.20_pm_r,83024._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.30_pm_r,86276._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.20_pm_r,89450._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.10_pm_r,92487._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.70_pm_r,95452._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.60_pm_r,98419._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.00_pm_r,101460._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.80_pm_r,104661._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
234.20_pm_r,108126._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
263.10_pm_r,112010._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
323.80_pm_r,116651._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
274.80_pm_r,-124._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.00_pm_r,3704._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.30_pm_r,7162._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.00_pm_r,10336._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.30_pm_r,13385._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.30_pm_r,16407._pm_r,21._pm_r,234._pm_r,3.05_pm_r,159._pm_r,0._pm_r,210._pm_r,0.54_pm_r,244._pm_r, &
203.10_pm_r,19398._pm_r,22._pm_r,220._pm_r,4.13_pm_r,148._pm_r,1._pm_r,232._pm_r,0.89_pm_r,229._pm_r, &
201.30_pm_r,22359._pm_r,24._pm_r,205._pm_r,4.61_pm_r,126._pm_r,3._pm_r,224._pm_r,1.29_pm_r,210._pm_r, &
197.10_pm_r,25275._pm_r,24._pm_r,188._pm_r,4.90_pm_r,91._pm_r,5._pm_r,214._pm_r,1.58_pm_r,191._pm_r, &
200.70_pm_r,28178._pm_r,22._pm_r,170._pm_r,6.01_pm_r,58._pm_r,7._pm_r,203._pm_r,1.67_pm_r,173._pm_r, &
211.50_pm_r,31189._pm_r,19._pm_r,144._pm_r,7.49_pm_r,39._pm_r,9._pm_r,194._pm_r,1.66_pm_r,160._pm_r, &
225.30_pm_r,34390._pm_r,18._pm_r,107._pm_r,8.10_pm_r,28._pm_r,11._pm_r,186._pm_r,1.52_pm_r,149._pm_r, &
239.80_pm_r,37793._pm_r,23._pm_r,76._pm_r,7.68_pm_r,20._pm_r,13._pm_r,180._pm_r,1.26_pm_r,139._pm_r, &
250.30_pm_r,41388._pm_r,29._pm_r,59._pm_r,6.28_pm_r,17._pm_r,13._pm_r,176._pm_r,0.52_pm_r,121._pm_r, &
258.80_pm_r,45114._pm_r,36._pm_r,50._pm_r,5.07_pm_r,13._pm_r,14._pm_r,175._pm_r,0.10_pm_r,270._pm_r, &
263.70_pm_r,48948._pm_r,41._pm_r,44._pm_r,4.11_pm_r,6._pm_r,14._pm_r,177._pm_r,0.73_pm_r,262._pm_r, &
262.10_pm_r,52805._pm_r,45._pm_r,39._pm_r,3.22_pm_r,351._pm_r,14._pm_r,183._pm_r,1.33_pm_r,251._pm_r, &
256.80_pm_r,56606._pm_r,47._pm_r,35._pm_r,2.16_pm_r,320._pm_r,15._pm_r,190._pm_r,1.35_pm_r,245._pm_r, &
249.80_pm_r,60319._pm_r,47._pm_r,32._pm_r,1.85_pm_r,279._pm_r,16._pm_r,195._pm_r,1.22_pm_r,236._pm_r, &
242.20_pm_r,63920._pm_r,45._pm_r,29._pm_r,1.92_pm_r,248._pm_r,18._pm_r,198._pm_r,1.00_pm_r,222._pm_r, &
235.20_pm_r,67417._pm_r,43._pm_r,27._pm_r,1.99_pm_r,226._pm_r,19._pm_r,199._pm_r,0.84_pm_r,199._pm_r, &
228.70_pm_r,70811._pm_r,40._pm_r,27._pm_r,2.03_pm_r,207._pm_r,20._pm_r,198._pm_r,0.85_pm_r,171._pm_r, &
223.00_pm_r,74119._pm_r,37._pm_r,27._pm_r,2.11_pm_r,192._pm_r,21._pm_r,196._pm_r,1.00_pm_r,151._pm_r, &
219.00_pm_r,77352._pm_r,34._pm_r,29._pm_r,2.18_pm_r,181._pm_r,22._pm_r,193._pm_r,1.14_pm_r,140._pm_r, &
217.30_pm_r,80533._pm_r,31._pm_r,32._pm_r,2.16_pm_r,173._pm_r,23._pm_r,189._pm_r,1.20_pm_r,133._pm_r, &
218.10_pm_r,83724._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.20_pm_r,86927._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.40_pm_r,90016._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
198.70_pm_r,92980._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.80_pm_r,95887._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.90_pm_r,98801._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.50_pm_r,101784._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.80_pm_r,104916._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.90_pm_r,108310._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.90_pm_r,112155._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
327.60_pm_r,116838._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)


  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
279.60_pm_r,-49._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
255.30_pm_r,3863._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.30_pm_r,7407._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.10_pm_r,10657._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.20_pm_r,13797._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.80_pm_r,16927._pm_r,14._pm_r,207._pm_r,3.32_pm_r,147._pm_r,4._pm_r,30._pm_r,0.76_pm_r,228._pm_r, &
211.30_pm_r,20030._pm_r,17._pm_r,190._pm_r,4.20_pm_r,135._pm_r,2._pm_r,21._pm_r,1.25_pm_r,221._pm_r, &
212.30_pm_r,23134._pm_r,20._pm_r,173._pm_r,4.52_pm_r,113._pm_r,1._pm_r,291._pm_r,1.76_pm_r,213._pm_r, &
210.20_pm_r,26230._pm_r,23._pm_r,157._pm_r,5.02_pm_r,81._pm_r,3._pm_r,220._pm_r,2.15_pm_r,204._pm_r, &
210.00_pm_r,29303._pm_r,24._pm_r,137._pm_r,6.59_pm_r,49._pm_r,6._pm_r,209._pm_r,2.27_pm_r,195._pm_r, &
215.00_pm_r,32407._pm_r,25._pm_r,111._pm_r,8.48_pm_r,29._pm_r,9._pm_r,202._pm_r,2.06_pm_r,184._pm_r, &
225.00_pm_r,35627._pm_r,28._pm_r,83._pm_r,9.11_pm_r,16._pm_r,12._pm_r,197._pm_r,1.66_pm_r,168._pm_r, &
238.00_pm_r,39013._pm_r,34._pm_r,62._pm_r,8.24_pm_r,5._pm_r,14._pm_r,191._pm_r,1.31_pm_r,144._pm_r, &
249.70_pm_r,42592._pm_r,41._pm_r,49._pm_r,6.08_pm_r,359._pm_r,14._pm_r,185._pm_r,0.94_pm_r,94._pm_r, &
256.90_pm_r,46304._pm_r,45._pm_r,42._pm_r,4.46_pm_r,351._pm_r,14._pm_r,180._pm_r,0.72_pm_r,62._pm_r, &
257.80_pm_r,50081._pm_r,49._pm_r,36._pm_r,3.35_pm_r,338._pm_r,13._pm_r,179._pm_r,0.23_pm_r,299._pm_r, &
253.30_pm_r,53830._pm_r,51._pm_r,32._pm_r,2.63_pm_r,317._pm_r,14._pm_r,183._pm_r,1.51_pm_r,247._pm_r, &
247.10_pm_r,57494._pm_r,50._pm_r,28._pm_r,2.44_pm_r,273._pm_r,15._pm_r,192._pm_r,1.96_pm_r,241._pm_r, &
240.90_pm_r,61069._pm_r,48._pm_r,24._pm_r,2.70_pm_r,244._pm_r,17._pm_r,199._pm_r,2.00_pm_r,236._pm_r, &
235.10_pm_r,64552._pm_r,45._pm_r,22._pm_r,2.57_pm_r,226._pm_r,20._pm_r,204._pm_r,1.67_pm_r,230._pm_r, &
229.60_pm_r,67957._pm_r,41._pm_r,20._pm_r,2.16_pm_r,207._pm_r,22._pm_r,206._pm_r,1.18_pm_r,218._pm_r, &
224.60_pm_r,71280._pm_r,39._pm_r,21._pm_r,1.80_pm_r,182._pm_r,23._pm_r,206._pm_r,0.76_pm_r,192._pm_r, &
220.10_pm_r,74537._pm_r,36._pm_r,23._pm_r,1.75_pm_r,155._pm_r,24._pm_r,204._pm_r,0.64_pm_r,150._pm_r, &
216.50_pm_r,77732._pm_r,35._pm_r,27._pm_r,1.89_pm_r,136._pm_r,24._pm_r,202._pm_r,0.78_pm_r,121._pm_r, &
214.60_pm_r,80877._pm_r,35._pm_r,31._pm_r,1.98_pm_r,125._pm_r,24._pm_r,199._pm_r,0.91_pm_r,107._pm_r, &
213.70_pm_r,84026._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.70_pm_r,87156._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.10_pm_r,90148._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.90_pm_r,93033._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.00_pm_r,95880._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.90_pm_r,98738._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.00_pm_r,101661._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.10_pm_r,104715._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.50_pm_r,108025._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
261.60_pm_r,111816._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
331.60_pm_r,116535._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
286.10_pm_r,-24._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
261.30_pm_r,3983._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.10_pm_r,7617._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.30_pm_r,10943._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.80_pm_r,14128._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.70_pm_r,17280._pm_r,4._pm_r,214._pm_r,2.55_pm_r,146._pm_r,3._pm_r,62._pm_r,0.70_pm_r,277._pm_r, &
215.30_pm_r,20427._pm_r,7._pm_r,178._pm_r,2.96_pm_r,134._pm_r,2._pm_r,43._pm_r,0.98_pm_r,264._pm_r, &
218.60_pm_r,23607._pm_r,10._pm_r,157._pm_r,2.84_pm_r,109._pm_r,1._pm_r,338._pm_r,1.22_pm_r,245._pm_r, &
219.70_pm_r,26817._pm_r,12._pm_r,138._pm_r,3.19_pm_r,68._pm_r,2._pm_r,261._pm_r,1.48_pm_r,225._pm_r, &
220.40_pm_r,30039._pm_r,13._pm_r,112._pm_r,4.79_pm_r,34._pm_r,4._pm_r,236._pm_r,1.73_pm_r,205._pm_r, &
223.70_pm_r,33287._pm_r,16._pm_r,80._pm_r,6.66_pm_r,16._pm_r,6._pm_r,220._pm_r,1.88_pm_r,187._pm_r, &
231.40_pm_r,36617._pm_r,22._pm_r,53._pm_r,7.33_pm_r,4._pm_r,8._pm_r,208._pm_r,1.81_pm_r,169._pm_r, &
243.00_pm_r,40084._pm_r,29._pm_r,37._pm_r,6.52_pm_r,353._pm_r,10._pm_r,197._pm_r,1.63_pm_r,147._pm_r, &
254.90_pm_r,43739._pm_r,35._pm_r,27._pm_r,4.47_pm_r,341._pm_r,11._pm_r,188._pm_r,1.18_pm_r,108._pm_r, &
259.90_pm_r,47517._pm_r,38._pm_r,20._pm_r,3.11_pm_r,322._pm_r,11._pm_r,180._pm_r,0.95_pm_r,75._pm_r, &
255.60_pm_r,51299._pm_r,40._pm_r,15._pm_r,2.53_pm_r,291._pm_r,11._pm_r,175._pm_r,0.40_pm_r,28._pm_r, &
246.30_pm_r,54980._pm_r,39._pm_r,9._pm_r,2.52_pm_r,261._pm_r,10._pm_r,178._pm_r,0.91_pm_r,269._pm_r, &
238.40_pm_r,58525._pm_r,37._pm_r,4._pm_r,3.12_pm_r,233._pm_r,11._pm_r,186._pm_r,1.27_pm_r,258._pm_r, &
232.50_pm_r,61973._pm_r,34._pm_r,359._pm_r,3.29_pm_r,217._pm_r,11._pm_r,195._pm_r,1.31_pm_r,251._pm_r, &
226.30_pm_r,65332._pm_r,30._pm_r,354._pm_r,2.84_pm_r,206._pm_r,13._pm_r,202._pm_r,1.11_pm_r,242._pm_r, &
220.50_pm_r,68604._pm_r,27._pm_r,351._pm_r,2.07_pm_r,191._pm_r,14._pm_r,205._pm_r,0.81_pm_r,227._pm_r, &
217.20_pm_r,71807._pm_r,24._pm_r,350._pm_r,1.42_pm_r,167._pm_r,15._pm_r,206._pm_r,0.60_pm_r,204._pm_r, &
214.10_pm_r,74967._pm_r,22._pm_r,351._pm_r,1.20_pm_r,131._pm_r,16._pm_r,205._pm_r,0.54_pm_r,175._pm_r, &
210.80_pm_r,78075._pm_r,22._pm_r,355._pm_r,1.30_pm_r,105._pm_r,16._pm_r,203._pm_r,0.57_pm_r,155._pm_r, &
209.30_pm_r,81139._pm_r,21._pm_r,0._pm_r,1.43_pm_r,90._pm_r,17._pm_r,201._pm_r,0.59_pm_r,143._pm_r, &
207.80_pm_r,84213._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.80_pm_r,87246._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.50_pm_r,90141._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.80_pm_r,92958._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.90_pm_r,95756._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.60_pm_r,98570._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.90_pm_r,101440._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.70_pm_r,104426._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.20_pm_r,107654._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
259.80_pm_r,111387._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
335.30_pm_r,116135._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
291.20_pm_r,-29._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
267.80_pm_r,4067._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.80_pm_r,7807._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.40_pm_r,11216._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.00_pm_r,14391._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.00_pm_r,17461._pm_r,2._pm_r,178._pm_r,1.09_pm_r,161._pm_r,3._pm_r,91._pm_r,0.75_pm_r,298._pm_r, &
214.50_pm_r,20569._pm_r,3._pm_r,167._pm_r,1.00_pm_r,148._pm_r,2._pm_r,76._pm_r,0.91_pm_r,289._pm_r, &
219.50_pm_r,23750._pm_r,4._pm_r,157._pm_r,0.67_pm_r,95._pm_r,1._pm_r,38._pm_r,0.87_pm_r,272._pm_r, &
223.80_pm_r,26995._pm_r,4._pm_r,140._pm_r,1.35_pm_r,32._pm_r,1._pm_r,305._pm_r,0.80_pm_r,242._pm_r, &
227.30_pm_r,30301._pm_r,4._pm_r,95._pm_r,2.64_pm_r,13._pm_r,2._pm_r,253._pm_r,0.92_pm_r,205._pm_r, &
231.50_pm_r,33656._pm_r,6._pm_r,45._pm_r,3.75_pm_r,4._pm_r,3._pm_r,222._pm_r,1.17_pm_r,178._pm_r, &
239.70_pm_r,37103._pm_r,11._pm_r,23._pm_r,3.99_pm_r,357._pm_r,4._pm_r,200._pm_r,1.30_pm_r,159._pm_r, &
251.00_pm_r,40692._pm_r,16._pm_r,13._pm_r,3.39_pm_r,347._pm_r,5._pm_r,184._pm_r,1.25_pm_r,142._pm_r, &
260.40_pm_r,44445._pm_r,20._pm_r,7._pm_r,2.23_pm_r,331._pm_r,6._pm_r,174._pm_r,0.80_pm_r,117._pm_r, &
263.20_pm_r,48286._pm_r,21._pm_r,1._pm_r,1.71_pm_r,298._pm_r,7._pm_r,166._pm_r,0.43_pm_r,81._pm_r, &
257.60_pm_r,52105._pm_r,22._pm_r,355._pm_r,1.81_pm_r,261._pm_r,7._pm_r,164._pm_r,0.21_pm_r,347._pm_r, &
247.00_pm_r,55806._pm_r,21._pm_r,348._pm_r,2.09_pm_r,231._pm_r,6._pm_r,167._pm_r,0.55_pm_r,273._pm_r, &
236.90_pm_r,59345._pm_r,19._pm_r,340._pm_r,2.55_pm_r,208._pm_r,6._pm_r,175._pm_r,0.57_pm_r,266._pm_r, &
227.90_pm_r,62750._pm_r,16._pm_r,331._pm_r,2.61_pm_r,195._pm_r,6._pm_r,182._pm_r,0.49_pm_r,260._pm_r, &
220.10_pm_r,66027._pm_r,14._pm_r,321._pm_r,2.18_pm_r,185._pm_r,6._pm_r,187._pm_r,0.37_pm_r,251._pm_r, &
214.20_pm_r,69207._pm_r,12._pm_r,313._pm_r,1.55_pm_r,174._pm_r,7._pm_r,191._pm_r,0.26_pm_r,235._pm_r, &
210.80_pm_r,72317._pm_r,10._pm_r,308._pm_r,0.97_pm_r,154._pm_r,7._pm_r,192._pm_r,0.19_pm_r,213._pm_r, &
208.00_pm_r,75385._pm_r,9._pm_r,306._pm_r,0.67_pm_r,118._pm_r,7._pm_r,192._pm_r,0.17_pm_r,188._pm_r, &
205.40_pm_r,78411._pm_r,8._pm_r,309._pm_r,0.69_pm_r,84._pm_r,7._pm_r,192._pm_r,0.17_pm_r,170._pm_r, &
204.30_pm_r,81403._pm_r,8._pm_r,316._pm_r,0.78_pm_r,65._pm_r,8._pm_r,191._pm_r,0.17_pm_r,158._pm_r, &
202.50_pm_r,84405._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.50_pm_r,87348._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.50_pm_r,90169._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.40_pm_r,92942._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.10_pm_r,95712._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.50_pm_r,98501._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.00_pm_r,101336._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.70_pm_r,104274._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.20_pm_r,107442._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
258.40_pm_r,111133._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
338.60_pm_r,115906._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
295.90_pm_r,-35._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
273.70_pm_r,4141._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.70_pm_r,7978._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.50_pm_r,11455._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.30_pm_r,14606._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.20_pm_r,17580._pm_r,2._pm_r,146._pm_r,0.37_pm_r,239._pm_r,2._pm_r,77._pm_r,0.37_pm_r,305._pm_r, &
212.70_pm_r,20633._pm_r,2._pm_r,163._pm_r,0.39_pm_r,266._pm_r,2._pm_r,61._pm_r,0.43_pm_r,298._pm_r, &
219.30_pm_r,23800._pm_r,2._pm_r,180._pm_r,0.48_pm_r,311._pm_r,1._pm_r,39._pm_r,0.38_pm_r,284._pm_r, &
224.40_pm_r,27049._pm_r,1._pm_r,210._pm_r,0.80_pm_r,339._pm_r,1._pm_r,16._pm_r,0.30_pm_r,255._pm_r, &
229.50_pm_r,30373._pm_r,1._pm_r,306._pm_r,1.21_pm_r,351._pm_r,1._pm_r,356._pm_r,0.30_pm_r,209._pm_r, &
236.50_pm_r,33781._pm_r,3._pm_r,338._pm_r,1.47_pm_r,356._pm_r,0._pm_r,326._pm_r,0.41_pm_r,176._pm_r, &
245.20_pm_r,37309._pm_r,5._pm_r,346._pm_r,1.42_pm_r,356._pm_r,0._pm_r,175._pm_r,0.49_pm_r,156._pm_r, &
254.90_pm_r,40968._pm_r,7._pm_r,348._pm_r,1.07_pm_r,351._pm_r,1._pm_r,158._pm_r,0.49_pm_r,139._pm_r, &
263.10_pm_r,44769._pm_r,8._pm_r,348._pm_r,0.63_pm_r,330._pm_r,2._pm_r,148._pm_r,0.30_pm_r,111._pm_r, &
265.70_pm_r,48648._pm_r,9._pm_r,344._pm_r,0.60_pm_r,281._pm_r,2._pm_r,138._pm_r,0.21_pm_r,62._pm_r, &
260.40_pm_r,52506._pm_r,9._pm_r,338._pm_r,0.75_pm_r,238._pm_r,2._pm_r,131._pm_r,0.11_pm_r,27._pm_r, &
249.30_pm_r,56246._pm_r,8._pm_r,330._pm_r,1.05_pm_r,199._pm_r,2._pm_r,131._pm_r,0.17_pm_r,225._pm_r, &
237.90_pm_r,59809._pm_r,7._pm_r,320._pm_r,1.47_pm_r,180._pm_r,2._pm_r,143._pm_r,0.32_pm_r,215._pm_r, &
227.40_pm_r,63218._pm_r,5._pm_r,306._pm_r,1.53_pm_r,171._pm_r,2._pm_r,157._pm_r,0.39_pm_r,216._pm_r, &
217.20_pm_r,66471._pm_r,4._pm_r,284._pm_r,1.22_pm_r,167._pm_r,2._pm_r,169._pm_r,0.37_pm_r,224._pm_r, &
209.80_pm_r,69596._pm_r,3._pm_r,262._pm_r,0.73_pm_r,164._pm_r,3._pm_r,178._pm_r,0.31_pm_r,239._pm_r, &
206.60_pm_r,72643._pm_r,3._pm_r,250._pm_r,0.24_pm_r,158._pm_r,3._pm_r,186._pm_r,0.27_pm_r,256._pm_r, &
204.50_pm_r,75654._pm_r,3._pm_r,249._pm_r,0.15_pm_r,356._pm_r,3._pm_r,194._pm_r,0.26_pm_r,274._pm_r, &
202.00_pm_r,78632._pm_r,3._pm_r,256._pm_r,0.41_pm_r,349._pm_r,3._pm_r,201._pm_r,0.26_pm_r,285._pm_r, &
200.50_pm_r,81572._pm_r,3._pm_r,269._pm_r,0.55_pm_r,347._pm_r,3._pm_r,209._pm_r,0.26_pm_r,295._pm_r, &
198.50_pm_r,84516._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.10_pm_r,87397._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.60_pm_r,90179._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,92936._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.60_pm_r,95699._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.80_pm_r,98482._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.80_pm_r,101304._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.20_pm_r,104222._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.60_pm_r,107366._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
258.70_pm_r,111046._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
341.90_pm_r,115853._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
299.40_pm_r,-59._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.10_pm_r,4162._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.50_pm_r,8040._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.90_pm_r,11541._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.00_pm_r,14680._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.90_pm_r,17615._pm_r,1._pm_r,143._pm_r,0.16_pm_r,299._pm_r,1._pm_r,20._pm_r,0.04_pm_r,23._pm_r, &
211.40_pm_r,20641._pm_r,1._pm_r,149._pm_r,0.24_pm_r,309._pm_r,1._pm_r,20._pm_r,0.03_pm_r,27._pm_r, &
218.00_pm_r,23789._pm_r,1._pm_r,161._pm_r,0.30_pm_r,318._pm_r,1._pm_r,20._pm_r,0.02_pm_r,50._pm_r, &
223.80_pm_r,27022._pm_r,0._pm_r,229._pm_r,0.35_pm_r,328._pm_r,1._pm_r,22._pm_r,0.03_pm_r,141._pm_r, &
230.30_pm_r,30347._pm_r,1._pm_r,315._pm_r,0.38_pm_r,336._pm_r,1._pm_r,24._pm_r,0.06_pm_r,170._pm_r, &
238.50_pm_r,33776._pm_r,1._pm_r,328._pm_r,0.37_pm_r,344._pm_r,1._pm_r,28._pm_r,0.10_pm_r,174._pm_r, &
247.00_pm_r,37333._pm_r,2._pm_r,334._pm_r,0.32_pm_r,349._pm_r,1._pm_r,34._pm_r,0.12_pm_r,175._pm_r, &
255.40_pm_r,41010._pm_r,2._pm_r,338._pm_r,0.23_pm_r,356._pm_r,1._pm_r,44._pm_r,0.13_pm_r,175._pm_r, &
262.30_pm_r,44806._pm_r,2._pm_r,341._pm_r,0.16_pm_r,15._pm_r,1._pm_r,57._pm_r,0.08_pm_r,153._pm_r, &
265.40_pm_r,48675._pm_r,2._pm_r,344._pm_r,0.09_pm_r,52._pm_r,1._pm_r,63._pm_r,0.05_pm_r,58._pm_r, &
262.10_pm_r,52544._pm_r,2._pm_r,347._pm_r,0.17_pm_r,144._pm_r,1._pm_r,58._pm_r,0.08_pm_r,358._pm_r, &
252.50_pm_r,56319._pm_r,2._pm_r,349._pm_r,0.43_pm_r,159._pm_r,1._pm_r,48._pm_r,0.13_pm_r,275._pm_r, &
240.50_pm_r,59927._pm_r,1._pm_r,353._pm_r,0.56_pm_r,165._pm_r,0._pm_r,31._pm_r,0.27_pm_r,245._pm_r, &
227.80_pm_r,63360._pm_r,0._pm_r,14._pm_r,0.54_pm_r,166._pm_r,0._pm_r,302._pm_r,0.40_pm_r,238._pm_r, &
216.50_pm_r,66607._pm_r,0._pm_r,149._pm_r,0.39_pm_r,168._pm_r,1._pm_r,254._pm_r,0.46_pm_r,238._pm_r, &
208.80_pm_r,69720._pm_r,1._pm_r,159._pm_r,0.20_pm_r,173._pm_r,2._pm_r,247._pm_r,0.49_pm_r,240._pm_r, &
205.30_pm_r,72749._pm_r,1._pm_r,162._pm_r,0.02_pm_r,229._pm_r,2._pm_r,245._pm_r,0.51_pm_r,244._pm_r, &
203.30_pm_r,75740._pm_r,1._pm_r,164._pm_r,0.13_pm_r,335._pm_r,3._pm_r,245._pm_r,0.51_pm_r,247._pm_r, &
201.70_pm_r,78705._pm_r,1._pm_r,166._pm_r,0.22_pm_r,341._pm_r,4._pm_r,246._pm_r,0.51_pm_r,249._pm_r, &
199.90_pm_r,81647._pm_r,0._pm_r,172._pm_r,0.28_pm_r,341._pm_r,4._pm_r,247._pm_r,0.48_pm_r,251._pm_r, &
196.70_pm_r,84566._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.50_pm_r,87413._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.40_pm_r,90186._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,92946._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,95715._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.20_pm_r,98504._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.10_pm_r,101332._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.00_pm_r,104260._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.80_pm_r,107419._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
261.10_pm_r,111129._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
345.60_pm_r,115988._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
300.30_pm_r,-89._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.30_pm_r,4141._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.90_pm_r,8023._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.30_pm_r,11531._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.00_pm_r,14673._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.30_pm_r,17605._pm_r,1._pm_r,155._pm_r,0.13_pm_r,301._pm_r,0._pm_r,346._pm_r,0.09_pm_r,22._pm_r, &
210.30_pm_r,20622._pm_r,1._pm_r,163._pm_r,0.17_pm_r,303._pm_r,1._pm_r,356._pm_r,0.12_pm_r,24._pm_r, &
217.30_pm_r,23758._pm_r,1._pm_r,180._pm_r,0.19_pm_r,303._pm_r,1._pm_r,4._pm_r,0.12_pm_r,27._pm_r, &
223.10_pm_r,26982._pm_r,0._pm_r,213._pm_r,0.20_pm_r,307._pm_r,1._pm_r,10._pm_r,0.12_pm_r,31._pm_r, &
229.40_pm_r,30297._pm_r,0._pm_r,253._pm_r,0.18_pm_r,312._pm_r,1._pm_r,16._pm_r,0.09_pm_r,43._pm_r, &
239.30_pm_r,33724._pm_r,1._pm_r,277._pm_r,0.15_pm_r,323._pm_r,1._pm_r,21._pm_r,0.06_pm_r,72._pm_r, &
248.10_pm_r,37290._pm_r,1._pm_r,291._pm_r,0.13_pm_r,342._pm_r,1._pm_r,28._pm_r,0.07_pm_r,117._pm_r, &
255.30_pm_r,40969._pm_r,1._pm_r,301._pm_r,0.12_pm_r,0._pm_r,1._pm_r,36._pm_r,0.09_pm_r,139._pm_r, &
261.10_pm_r,44754._pm_r,1._pm_r,314._pm_r,0.19_pm_r,52._pm_r,1._pm_r,45._pm_r,0.06_pm_r,132._pm_r, &
264.10_pm_r,48607._pm_r,1._pm_r,338._pm_r,0.32_pm_r,83._pm_r,1._pm_r,51._pm_r,0.02_pm_r,0._pm_r, &
262.40_pm_r,52469._pm_r,1._pm_r,15._pm_r,0.32_pm_r,104._pm_r,1._pm_r,51._pm_r,0.05_pm_r,302._pm_r, &
254.00_pm_r,56255._pm_r,1._pm_r,48._pm_r,0.25_pm_r,168._pm_r,1._pm_r,48._pm_r,0.05_pm_r,217._pm_r, &
242.20_pm_r,59885._pm_r,0._pm_r,93._pm_r,0.38_pm_r,220._pm_r,1._pm_r,46._pm_r,0.16_pm_r,173._pm_r, &
229.50_pm_r,63340._pm_r,1._pm_r,189._pm_r,0.50_pm_r,230._pm_r,1._pm_r,46._pm_r,0.23_pm_r,170._pm_r, &
217.40_pm_r,66603._pm_r,1._pm_r,210._pm_r,0.46_pm_r,226._pm_r,0._pm_r,52._pm_r,0.23_pm_r,171._pm_r, &
208.90_pm_r,69724._pm_r,2._pm_r,214._pm_r,0.36_pm_r,212._pm_r,0._pm_r,75._pm_r,0.19_pm_r,183._pm_r, &
204.90_pm_r,72754._pm_r,2._pm_r,212._pm_r,0.29_pm_r,183._pm_r,0._pm_r,150._pm_r,0.16_pm_r,196._pm_r, &
203.00_pm_r,75744._pm_r,3._pm_r,208._pm_r,0.31_pm_r,158._pm_r,0._pm_r,186._pm_r,0.14_pm_r,214._pm_r, &
201.90_pm_r,78712._pm_r,3._pm_r,202._pm_r,0.35_pm_r,142._pm_r,0._pm_r,196._pm_r,0.14_pm_r,226._pm_r, &
199.80_pm_r,81664._pm_r,4._pm_r,197._pm_r,0.37_pm_r,133._pm_r,0._pm_r,202._pm_r,0.13_pm_r,238._pm_r, &
195.80_pm_r,84569._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.00_pm_r,87403._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.70_pm_r,90179._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,92944._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,95718._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.90_pm_r,98515._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.70_pm_r,101360._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
198.60_pm_r,104320._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.70_pm_r,107530._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
265.80_pm_r,111309._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
350.30_pm_r,116243._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
301.10_pm_r,-104._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.60_pm_r,4133._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.10_pm_r,8019._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.70_pm_r,11531._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.40_pm_r,14678._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.80_pm_r,17616._pm_r,2._pm_r,176._pm_r,0.17_pm_r,279._pm_r,0._pm_r,144._pm_r,0.09_pm_r,338._pm_r, &
211.10_pm_r,20639._pm_r,2._pm_r,185._pm_r,0.22_pm_r,281._pm_r,0._pm_r,130._pm_r,0.11_pm_r,333._pm_r, &
217.90_pm_r,23783._pm_r,2._pm_r,196._pm_r,0.24_pm_r,282._pm_r,0._pm_r,12._pm_r,0.11_pm_r,328._pm_r, &
223.80_pm_r,27016._pm_r,2._pm_r,207._pm_r,0.23_pm_r,285._pm_r,0._pm_r,337._pm_r,0.10_pm_r,317._pm_r, &
230.00_pm_r,30338._pm_r,2._pm_r,217._pm_r,0.19_pm_r,290._pm_r,0._pm_r,327._pm_r,0.07_pm_r,302._pm_r, &
238.20_pm_r,33763._pm_r,2._pm_r,224._pm_r,0.13_pm_r,302._pm_r,0._pm_r,321._pm_r,0.04_pm_r,270._pm_r, &
246.40_pm_r,37314._pm_r,2._pm_r,228._pm_r,0.08_pm_r,332._pm_r,0._pm_r,314._pm_r,0.04_pm_r,192._pm_r, &
254.30_pm_r,40978._pm_r,2._pm_r,231._pm_r,0.07_pm_r,25._pm_r,0._pm_r,309._pm_r,0.06_pm_r,149._pm_r, &
261.10_pm_r,44756._pm_r,2._pm_r,230._pm_r,0.13_pm_r,74._pm_r,0._pm_r,311._pm_r,0.10_pm_r,110._pm_r, &
264.40_pm_r,48608._pm_r,2._pm_r,224._pm_r,0.25_pm_r,105._pm_r,0._pm_r,0._pm_r,0.16_pm_r,104._pm_r, &
261.00_pm_r,52461._pm_r,2._pm_r,208._pm_r,0.32_pm_r,127._pm_r,0._pm_r,92._pm_r,0.17_pm_r,139._pm_r, &
253.10_pm_r,56231._pm_r,2._pm_r,193._pm_r,0.37_pm_r,160._pm_r,0._pm_r,136._pm_r,0.27_pm_r,187._pm_r, &
242.60_pm_r,59863._pm_r,2._pm_r,190._pm_r,0.38_pm_r,192._pm_r,1._pm_r,169._pm_r,0.31_pm_r,207._pm_r, &
228.90_pm_r,63321._pm_r,3._pm_r,192._pm_r,0.47_pm_r,210._pm_r,1._pm_r,184._pm_r,0.23_pm_r,221._pm_r, &
217.60_pm_r,66584._pm_r,4._pm_r,197._pm_r,0.53_pm_r,215._pm_r,1._pm_r,192._pm_r,0.06_pm_r,265._pm_r, &
209.90_pm_r,69713._pm_r,4._pm_r,200._pm_r,0.55_pm_r,214._pm_r,1._pm_r,193._pm_r,0.19_pm_r,18._pm_r, &
205.80_pm_r,72753._pm_r,5._pm_r,202._pm_r,0.57_pm_r,211._pm_r,1._pm_r,185._pm_r,0.40_pm_r,30._pm_r, &
203.60_pm_r,75750._pm_r,6._pm_r,203._pm_r,0.59_pm_r,207._pm_r,0._pm_r,102._pm_r,0.55_pm_r,32._pm_r, &
202.70_pm_r,78724._pm_r,7._pm_r,203._pm_r,0.57_pm_r,204._pm_r,1._pm_r,49._pm_r,0.65_pm_r,34._pm_r, &
200.30_pm_r,81687._pm_r,8._pm_r,203._pm_r,0.54_pm_r,203._pm_r,2._pm_r,42._pm_r,0.69_pm_r,35._pm_r, &
195.20_pm_r,84585._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.20_pm_r,87405._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.10_pm_r,90171._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.40_pm_r,92924._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,95686._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.20_pm_r,98480._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.80_pm_r,101344._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.50_pm_r,104349._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.70_pm_r,107637._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
272.70_pm_r,111519._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
355.70_pm_r,116550._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
300.40_pm_r,-124._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.30_pm_r,4111._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.50_pm_r,8003._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.60_pm_r,11523._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.10_pm_r,14688._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.70_pm_r,17653._pm_r,2._pm_r,143._pm_r,0.65_pm_r,257._pm_r,1._pm_r,152._pm_r,0.27_pm_r,341._pm_r, &
212.60_pm_r,20702._pm_r,2._pm_r,171._pm_r,0.80_pm_r,258._pm_r,1._pm_r,145._pm_r,0.35_pm_r,340._pm_r, &
219.00_pm_r,23865._pm_r,3._pm_r,200._pm_r,0.83_pm_r,258._pm_r,0._pm_r,90._pm_r,0.39_pm_r,338._pm_r, &
224.60_pm_r,27112._pm_r,3._pm_r,218._pm_r,0.76_pm_r,259._pm_r,1._pm_r,353._pm_r,0.40_pm_r,337._pm_r, &
230.40_pm_r,30444._pm_r,4._pm_r,227._pm_r,0.56_pm_r,261._pm_r,1._pm_r,344._pm_r,0.35_pm_r,335._pm_r, &
238.70_pm_r,33875._pm_r,5._pm_r,232._pm_r,0.29_pm_r,262._pm_r,2._pm_r,341._pm_r,0.26_pm_r,332._pm_r, &
246.50_pm_r,37432._pm_r,5._pm_r,233._pm_r,0.05_pm_r,236._pm_r,2._pm_r,339._pm_r,0.16_pm_r,329._pm_r, &
255.20_pm_r,41100._pm_r,5._pm_r,232._pm_r,0.14_pm_r,111._pm_r,2._pm_r,338._pm_r,0.07_pm_r,326._pm_r, &
263.50_pm_r,44906._pm_r,5._pm_r,229._pm_r,0.25_pm_r,131._pm_r,2._pm_r,338._pm_r,0.02_pm_r,117._pm_r, &
264.70_pm_r,48781._pm_r,5._pm_r,223._pm_r,0.40_pm_r,148._pm_r,2._pm_r,340._pm_r,0.14_pm_r,124._pm_r, &
258.90_pm_r,52619._pm_r,5._pm_r,216._pm_r,0.51_pm_r,156._pm_r,2._pm_r,344._pm_r,0.17_pm_r,143._pm_r, &
250.20_pm_r,56353._pm_r,5._pm_r,209._pm_r,0.53_pm_r,164._pm_r,2._pm_r,343._pm_r,0.20_pm_r,198._pm_r, &
239.90_pm_r,59943._pm_r,6._pm_r,205._pm_r,0.38_pm_r,177._pm_r,1._pm_r,331._pm_r,0.26_pm_r,239._pm_r, &
227.50_pm_r,63370._pm_r,6._pm_r,204._pm_r,0.26_pm_r,205._pm_r,2._pm_r,315._pm_r,0.28_pm_r,260._pm_r, &
216.60_pm_r,66615._pm_r,7._pm_r,205._pm_r,0.22_pm_r,244._pm_r,2._pm_r,307._pm_r,0.22_pm_r,289._pm_r, &
209.70_pm_r,69734._pm_r,7._pm_r,207._pm_r,0.25_pm_r,275._pm_r,2._pm_r,308._pm_r,0.20_pm_r,340._pm_r, &
206.70_pm_r,72781._pm_r,7._pm_r,210._pm_r,0.29_pm_r,290._pm_r,2._pm_r,315._pm_r,0.28_pm_r,14._pm_r, &
204.70_pm_r,75793._pm_r,7._pm_r,214._pm_r,0.33_pm_r,297._pm_r,3._pm_r,326._pm_r,0.39_pm_r,29._pm_r, &
202.70_pm_r,78776._pm_r,7._pm_r,218._pm_r,0.34_pm_r,301._pm_r,3._pm_r,338._pm_r,0.46_pm_r,34._pm_r, &
198.90_pm_r,81730._pm_r,7._pm_r,222._pm_r,0.34_pm_r,306._pm_r,3._pm_r,348._pm_r,0.48_pm_r,38._pm_r, &
192.80_pm_r,84598._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.40_pm_r,87377._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.00_pm_r,90099._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.30_pm_r,92806._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.60_pm_r,95527._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.80_pm_r,98300._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.90_pm_r,101175._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.40_pm_r,104233._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
232.40_pm_r,107618._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
281.00_pm_r,111628._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
361.40_pm_r,116766._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
298.50_pm_r,-116._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.10_pm_r,4101._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.90_pm_r,7984._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.10_pm_r,11501._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.20_pm_r,14690._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.90_pm_r,17706._pm_r,2._pm_r,139._pm_r,0.88_pm_r,253._pm_r,2._pm_r,157._pm_r,0.35_pm_r,336._pm_r, &
214.60_pm_r,20792._pm_r,2._pm_r,182._pm_r,1.08_pm_r,253._pm_r,1._pm_r,157._pm_r,0.47_pm_r,337._pm_r, &
220.50_pm_r,23979._pm_r,3._pm_r,213._pm_r,1.11_pm_r,252._pm_r,0._pm_r,161._pm_r,0.54_pm_r,336._pm_r, &
226.40_pm_r,27249._pm_r,4._pm_r,227._pm_r,1.02_pm_r,252._pm_r,1._pm_r,334._pm_r,0.57_pm_r,336._pm_r, &
232.00_pm_r,30607._pm_r,6._pm_r,232._pm_r,0.76_pm_r,249._pm_r,1._pm_r,335._pm_r,0.54_pm_r,335._pm_r, &
240.20_pm_r,34060._pm_r,6._pm_r,234._pm_r,0.43_pm_r,240._pm_r,2._pm_r,334._pm_r,0.44_pm_r,333._pm_r, &
249.00_pm_r,37644._pm_r,7._pm_r,234._pm_r,0.20_pm_r,202._pm_r,3._pm_r,334._pm_r,0.33_pm_r,329._pm_r, &
258.30_pm_r,41356._pm_r,7._pm_r,232._pm_r,0.23_pm_r,150._pm_r,3._pm_r,333._pm_r,0.21_pm_r,323._pm_r, &
265.00_pm_r,45195._pm_r,7._pm_r,228._pm_r,0.34_pm_r,159._pm_r,3._pm_r,332._pm_r,0.08_pm_r,279._pm_r, &
265.20_pm_r,49085._pm_r,7._pm_r,224._pm_r,0.47_pm_r,171._pm_r,3._pm_r,329._pm_r,0.14_pm_r,212._pm_r, &
258.60_pm_r,52924._pm_r,8._pm_r,220._pm_r,0.55_pm_r,185._pm_r,3._pm_r,325._pm_r,0.23_pm_r,225._pm_r, &
248.90_pm_r,56645._pm_r,9._pm_r,218._pm_r,0.55_pm_r,200._pm_r,3._pm_r,318._pm_r,0.35_pm_r,258._pm_r, &
237.80_pm_r,60210._pm_r,9._pm_r,217._pm_r,0.46_pm_r,224._pm_r,4._pm_r,311._pm_r,0.42_pm_r,281._pm_r, &
225.40_pm_r,63605._pm_r,10._pm_r,218._pm_r,0.37_pm_r,257._pm_r,4._pm_r,308._pm_r,0.44_pm_r,297._pm_r, &
215.40_pm_r,66826._pm_r,10._pm_r,221._pm_r,0.37_pm_r,295._pm_r,5._pm_r,307._pm_r,0.39_pm_r,311._pm_r, &
209.00_pm_r,69932._pm_r,10._pm_r,224._pm_r,0.45_pm_r,323._pm_r,5._pm_r,309._pm_r,0.31_pm_r,329._pm_r, &
205.70_pm_r,72967._pm_r,10._pm_r,228._pm_r,0.56_pm_r,340._pm_r,6._pm_r,311._pm_r,0.28_pm_r,352._pm_r, &
202.40_pm_r,75958._pm_r,10._pm_r,233._pm_r,0.65_pm_r,347._pm_r,6._pm_r,314._pm_r,0.27_pm_r,12._pm_r, &
198.60_pm_r,78895._pm_r,9._pm_r,238._pm_r,0.69_pm_r,352._pm_r,6._pm_r,317._pm_r,0.27_pm_r,27._pm_r, &
193.50_pm_r,81778._pm_r,9._pm_r,244._pm_r,0.67_pm_r,354._pm_r,6._pm_r,321._pm_r,0.27_pm_r,34._pm_r, &
186.90_pm_r,84560._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.30_pm_r,87249._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
178.00_pm_r,89879._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
178.00_pm_r,92499._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
180.80_pm_r,95151._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,97884._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.00_pm_r,100764._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.40_pm_r,103878._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
241.40_pm_r,107379._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
289.60_pm_r,111531._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
366.70_pm_r,116771._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
293.30_pm_r,-66._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.10_pm_r,4087._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.00_pm_r,7916._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.90_pm_r,11393._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.50_pm_r,14609._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.80_pm_r,17716._pm_r,2._pm_r,196._pm_r,0.59_pm_r,257._pm_r,2._pm_r,170._pm_r,0.28_pm_r,358._pm_r, &
217.80_pm_r,20868._pm_r,3._pm_r,214._pm_r,0.75_pm_r,257._pm_r,1._pm_r,166._pm_r,0.38_pm_r,357._pm_r, &
222.40_pm_r,24091._pm_r,4._pm_r,226._pm_r,0.78_pm_r,256._pm_r,1._pm_r,155._pm_r,0.45_pm_r,356._pm_r, &
228.00_pm_r,27386._pm_r,5._pm_r,232._pm_r,0.71_pm_r,253._pm_r,0._pm_r,36._pm_r,0.48_pm_r,353._pm_r, &
234.60_pm_r,30773._pm_r,6._pm_r,235._pm_r,0.53_pm_r,247._pm_r,1._pm_r,3._pm_r,0.46_pm_r,349._pm_r, &
243.10_pm_r,34268._pm_r,6._pm_r,236._pm_r,0.33_pm_r,230._pm_r,2._pm_r,357._pm_r,0.39_pm_r,345._pm_r, &
252.80_pm_r,37901._pm_r,7._pm_r,234._pm_r,0.23_pm_r,195._pm_r,2._pm_r,353._pm_r,0.31_pm_r,336._pm_r, &
262.40_pm_r,41672._pm_r,7._pm_r,232._pm_r,0.25_pm_r,168._pm_r,2._pm_r,349._pm_r,0.24_pm_r,324._pm_r, &
268.40_pm_r,45567._pm_r,7._pm_r,229._pm_r,0.38_pm_r,183._pm_r,3._pm_r,345._pm_r,0.15_pm_r,298._pm_r, &
267.40_pm_r,49498._pm_r,8._pm_r,226._pm_r,0.53_pm_r,200._pm_r,3._pm_r,342._pm_r,0.13_pm_r,279._pm_r, &
259.90_pm_r,53362._pm_r,8._pm_r,224._pm_r,0.60_pm_r,210._pm_r,3._pm_r,338._pm_r,0.15_pm_r,285._pm_r, &
249.80_pm_r,57099._pm_r,9._pm_r,223._pm_r,0.47_pm_r,225._pm_r,3._pm_r,335._pm_r,0.23_pm_r,310._pm_r, &
238.30_pm_r,60672._pm_r,10._pm_r,224._pm_r,0.29_pm_r,259._pm_r,3._pm_r,333._pm_r,0.31_pm_r,316._pm_r, &
225.80_pm_r,64074._pm_r,10._pm_r,226._pm_r,0.33_pm_r,310._pm_r,4._pm_r,331._pm_r,0.33_pm_r,316._pm_r, &
215.80_pm_r,67302._pm_r,10._pm_r,230._pm_r,0.48_pm_r,328._pm_r,4._pm_r,329._pm_r,0.32_pm_r,309._pm_r, &
208.20_pm_r,70407._pm_r,10._pm_r,234._pm_r,0.59_pm_r,333._pm_r,5._pm_r,326._pm_r,0.28_pm_r,300._pm_r, &
202.20_pm_r,73411._pm_r,10._pm_r,240._pm_r,0.67_pm_r,336._pm_r,5._pm_r,324._pm_r,0.24_pm_r,292._pm_r, &
195.80_pm_r,76328._pm_r,10._pm_r,246._pm_r,0.69_pm_r,335._pm_r,5._pm_r,322._pm_r,0.22_pm_r,283._pm_r, &
189.10_pm_r,79143._pm_r,10._pm_r,252._pm_r,0.68_pm_r,336._pm_r,6._pm_r,320._pm_r,0.20_pm_r,276._pm_r, &
182.60_pm_r,81865._pm_r,10._pm_r,257._pm_r,0.64_pm_r,336._pm_r,6._pm_r,318._pm_r,0.18_pm_r,273._pm_r, &
176.50_pm_r,84492._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
172.00_pm_r,87036._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
169.50_pm_r,89532._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
170.80_pm_r,92035._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
176.00_pm_r,94595._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.90_pm_r,97275._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.00_pm_r,100153._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.10_pm_r,103330._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
251.50_pm_r,106960._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
297.60_pm_r,111258._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
371.30_pm_r,116583._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
285.50_pm_r,-25._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
269.30_pm_r,4033._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
244.00_pm_r,7787._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.50_pm_r,11213._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.90_pm_r,14463._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.20_pm_r,17682._pm_r,3._pm_r,217._pm_r,0.24_pm_r,283._pm_r,1._pm_r,205._pm_r,0.26_pm_r,62._pm_r, &
222.30_pm_r,20920._pm_r,3._pm_r,223._pm_r,0.32_pm_r,280._pm_r,1._pm_r,185._pm_r,0.36_pm_r,59._pm_r, &
224.90_pm_r,24192._pm_r,3._pm_r,231._pm_r,0.37_pm_r,275._pm_r,1._pm_r,137._pm_r,0.43_pm_r,57._pm_r, &
230.20_pm_r,27520._pm_r,4._pm_r,236._pm_r,0.38_pm_r,268._pm_r,1._pm_r,96._pm_r,0.44_pm_r,53._pm_r, &
238.40_pm_r,30951._pm_r,4._pm_r,239._pm_r,0.34_pm_r,257._pm_r,1._pm_r,79._pm_r,0.37_pm_r,47._pm_r, &
247.50_pm_r,34506._pm_r,5._pm_r,240._pm_r,0.30_pm_r,240._pm_r,2._pm_r,70._pm_r,0.28_pm_r,35._pm_r, &
257.50_pm_r,38206._pm_r,5._pm_r,240._pm_r,0.27_pm_r,221._pm_r,2._pm_r,64._pm_r,0.18_pm_r,12._pm_r, &
266.70_pm_r,42044._pm_r,6._pm_r,238._pm_r,0.27_pm_r,204._pm_r,2._pm_r,58._pm_r,0.14_pm_r,331._pm_r, &
271.80_pm_r,45995._pm_r,6._pm_r,235._pm_r,0.36_pm_r,203._pm_r,2._pm_r,54._pm_r,0.11_pm_r,288._pm_r, &
270.80_pm_r,49975._pm_r,7._pm_r,233._pm_r,0.49_pm_r,210._pm_r,2._pm_r,51._pm_r,0.09_pm_r,264._pm_r, &
263.70_pm_r,53891._pm_r,7._pm_r,231._pm_r,0.51_pm_r,217._pm_r,2._pm_r,49._pm_r,0.08_pm_r,303._pm_r, &
253.80_pm_r,57685._pm_r,8._pm_r,230._pm_r,0.41_pm_r,226._pm_r,2._pm_r,43._pm_r,0.20_pm_r,340._pm_r, &
242.00_pm_r,61314._pm_r,8._pm_r,230._pm_r,0.24_pm_r,243._pm_r,2._pm_r,34._pm_r,0.31_pm_r,334._pm_r, &
229.40_pm_r,64768._pm_r,9._pm_r,231._pm_r,0.17_pm_r,270._pm_r,2._pm_r,24._pm_r,0.34_pm_r,327._pm_r, &
218.20_pm_r,68041._pm_r,9._pm_r,232._pm_r,0.20_pm_r,282._pm_r,3._pm_r,15._pm_r,0.30_pm_r,318._pm_r, &
207.40_pm_r,71161._pm_r,9._pm_r,234._pm_r,0.22_pm_r,279._pm_r,3._pm_r,8._pm_r,0.23_pm_r,305._pm_r, &
196.60_pm_r,74117._pm_r,9._pm_r,235._pm_r,0.25_pm_r,273._pm_r,3._pm_r,3._pm_r,0.18_pm_r,286._pm_r, &
185.90_pm_r,76922._pm_r,10._pm_r,236._pm_r,0.26_pm_r,268._pm_r,3._pm_r,358._pm_r,0.15_pm_r,266._pm_r, &
176.40_pm_r,79567._pm_r,10._pm_r,237._pm_r,0.25_pm_r,264._pm_r,3._pm_r,354._pm_r,0.14_pm_r,253._pm_r, &
168.70_pm_r,82084._pm_r,10._pm_r,238._pm_r,0.24_pm_r,263._pm_r,3._pm_r,351._pm_r,0.13_pm_r,242._pm_r, &
163.80_pm_r,84515._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
161.00_pm_r,86884._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
159.90_pm_r,89226._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
163.10_pm_r,91599._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
170.60_pm_r,94062._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.80_pm_r,96682._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.90_pm_r,99556._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.50_pm_r,102802._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
262.20_pm_r,106573._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
303.80_pm_r,111010._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
374.90_pm_r,116394._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
283.80_pm_r,-38._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
265.80_pm_r,3978._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
240.40_pm_r,7677._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.30_pm_r,11080._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.40_pm_r,14366._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.50_pm_r,17660._pm_r,3._pm_r,190._pm_r,0.45_pm_r,318._pm_r,0._pm_r,166._pm_r,0.10_pm_r,90._pm_r, &
226.60_pm_r,20970._pm_r,2._pm_r,206._pm_r,0.61_pm_r,316._pm_r,0._pm_r,136._pm_r,0.14_pm_r,94._pm_r, &
227.80_pm_r,24295._pm_r,2._pm_r,230._pm_r,0.67_pm_r,313._pm_r,1._pm_r,120._pm_r,0.15_pm_r,96._pm_r, &
232.50_pm_r,27660._pm_r,3._pm_r,252._pm_r,0.61_pm_r,308._pm_r,1._pm_r,114._pm_r,0.14_pm_r,102._pm_r, &
241.00_pm_r,31126._pm_r,3._pm_r,264._pm_r,0.44_pm_r,298._pm_r,1._pm_r,112._pm_r,0.12_pm_r,110._pm_r, &
251.30_pm_r,34727._pm_r,4._pm_r,267._pm_r,0.27_pm_r,272._pm_r,1._pm_r,113._pm_r,0.08_pm_r,130._pm_r, &
262.10_pm_r,38490._pm_r,4._pm_r,266._pm_r,0.21_pm_r,225._pm_r,1._pm_r,115._pm_r,0.05_pm_r,158._pm_r, &
271.60_pm_r,42397._pm_r,4._pm_r,262._pm_r,0.26_pm_r,191._pm_r,1._pm_r,118._pm_r,0.05_pm_r,191._pm_r, &
276.30_pm_r,46419._pm_r,4._pm_r,256._pm_r,0.27_pm_r,178._pm_r,1._pm_r,121._pm_r,0.03_pm_r,189._pm_r, &
274.80_pm_r,50461._pm_r,4._pm_r,252._pm_r,0.18_pm_r,171._pm_r,1._pm_r,121._pm_r,0.01_pm_r,90._pm_r, &
268.70_pm_r,54443._pm_r,4._pm_r,249._pm_r,0.11_pm_r,234._pm_r,1._pm_r,120._pm_r,0.03_pm_r,39._pm_r, &
258.70_pm_r,58311._pm_r,5._pm_r,250._pm_r,0.33_pm_r,270._pm_r,1._pm_r,118._pm_r,0.02_pm_r,154._pm_r, &
246.40_pm_r,62007._pm_r,5._pm_r,252._pm_r,0.45_pm_r,264._pm_r,1._pm_r,123._pm_r,0.12_pm_r,207._pm_r, &
233.30_pm_r,65524._pm_r,6._pm_r,253._pm_r,0.43_pm_r,255._pm_r,1._pm_r,134._pm_r,0.22_pm_r,210._pm_r, &
220.00_pm_r,68841._pm_r,6._pm_r,253._pm_r,0.34_pm_r,241._pm_r,1._pm_r,150._pm_r,0.30_pm_r,213._pm_r, &
205.90_pm_r,71965._pm_r,7._pm_r,251._pm_r,0.24_pm_r,215._pm_r,2._pm_r,164._pm_r,0.32_pm_r,214._pm_r, &
191.50_pm_r,74870._pm_r,7._pm_r,249._pm_r,0.22_pm_r,180._pm_r,2._pm_r,174._pm_r,0.31_pm_r,216._pm_r, &
177.70_pm_r,77577._pm_r,7._pm_r,246._pm_r,0.23_pm_r,157._pm_r,2._pm_r,181._pm_r,0.28_pm_r,215._pm_r, &
166.00_pm_r,80084._pm_r,7._pm_r,244._pm_r,0.23_pm_r,146._pm_r,3._pm_r,186._pm_r,0.25_pm_r,216._pm_r, &
156.90_pm_r,82433._pm_r,7._pm_r,241._pm_r,0.22_pm_r,141._pm_r,3._pm_r,190._pm_r,0.22_pm_r,219._pm_r, &
151.80_pm_r,84679._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
150.20_pm_r,86876._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
150.90_pm_r,89073._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
155.90_pm_r,91324._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
165.40_pm_r,93691._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
180.60_pm_r,96255._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.90_pm_r,99122._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.10_pm_r,102439._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
272.80_pm_r,106353._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
307.70_pm_r,110910._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
377.50_pm_r,116326._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
279.80_pm_r,-31._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
263.00_pm_r,3933._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
237.60_pm_r,7588._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.40_pm_r,10983._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.90_pm_r,14308._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.10_pm_r,17654._pm_r,2._pm_r,139._pm_r,0.40_pm_r,287._pm_r,0._pm_r,65._pm_r,0.02_pm_r,76._pm_r, &
230.00_pm_r,21016._pm_r,1._pm_r,158._pm_r,0.55_pm_r,285._pm_r,0._pm_r,68._pm_r,0.02_pm_r,104._pm_r, &
230.50_pm_r,24384._pm_r,1._pm_r,206._pm_r,0.63_pm_r,281._pm_r,0._pm_r,74._pm_r,0.03_pm_r,135._pm_r, &
234.90_pm_r,27785._pm_r,2._pm_r,241._pm_r,0.60_pm_r,275._pm_r,0._pm_r,85._pm_r,0.05_pm_r,169._pm_r, &
243.50_pm_r,31288._pm_r,2._pm_r,251._pm_r,0.50_pm_r,266._pm_r,0._pm_r,101._pm_r,0.07_pm_r,182._pm_r, &
254.30_pm_r,34927._pm_r,3._pm_r,253._pm_r,0.36_pm_r,251._pm_r,0._pm_r,119._pm_r,0.08_pm_r,188._pm_r, &
265.60_pm_r,38739._pm_r,3._pm_r,251._pm_r,0.26_pm_r,227._pm_r,0._pm_r,135._pm_r,0.08_pm_r,189._pm_r, &
275.40_pm_r,42700._pm_r,4._pm_r,248._pm_r,0.22_pm_r,202._pm_r,1._pm_r,146._pm_r,0.07_pm_r,189._pm_r, &
280.50_pm_r,46779._pm_r,4._pm_r,245._pm_r,0.23_pm_r,205._pm_r,1._pm_r,153._pm_r,0.08_pm_r,189._pm_r, &
279.70_pm_r,50886._pm_r,4._pm_r,242._pm_r,0.30_pm_r,222._pm_r,1._pm_r,162._pm_r,0.13_pm_r,203._pm_r, &
273.10_pm_r,54936._pm_r,5._pm_r,241._pm_r,0.35_pm_r,243._pm_r,1._pm_r,172._pm_r,0.14_pm_r,212._pm_r, &
263.10_pm_r,58867._pm_r,5._pm_r,242._pm_r,0.31_pm_r,268._pm_r,1._pm_r,180._pm_r,0.15_pm_r,218._pm_r, &
250.20_pm_r,62623._pm_r,5._pm_r,245._pm_r,0.16_pm_r,312._pm_r,1._pm_r,185._pm_r,0.11_pm_r,205._pm_r, &
236.00_pm_r,66188._pm_r,5._pm_r,246._pm_r,0.12_pm_r,50._pm_r,1._pm_r,187._pm_r,0.12_pm_r,187._pm_r, &
220.70_pm_r,69529._pm_r,5._pm_r,245._pm_r,0.28_pm_r,103._pm_r,2._pm_r,186._pm_r,0.17_pm_r,179._pm_r, &
204.90_pm_r,72651._pm_r,5._pm_r,240._pm_r,0.46_pm_r,120._pm_r,2._pm_r,185._pm_r,0.21_pm_r,180._pm_r, &
189.20_pm_r,75532._pm_r,4._pm_r,231._pm_r,0.56_pm_r,128._pm_r,2._pm_r,184._pm_r,0.23_pm_r,181._pm_r, &
174.00_pm_r,78195._pm_r,4._pm_r,220._pm_r,0.60_pm_r,133._pm_r,3._pm_r,184._pm_r,0.24_pm_r,181._pm_r, &
161.40_pm_r,80639._pm_r,5._pm_r,209._pm_r,0.57_pm_r,136._pm_r,3._pm_r,184._pm_r,0.22_pm_r,182._pm_r, &
150.50_pm_r,82915._pm_r,5._pm_r,200._pm_r,0.49_pm_r,137._pm_r,3._pm_r,183._pm_r,0.19_pm_r,182._pm_r, &
143.20_pm_r,85030._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
141.40_pm_r,87088._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
143.50_pm_r,89166._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
149.90_pm_r,91317._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
160.90_pm_r,93607._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
178.60_pm_r,96120._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.70_pm_r,98980._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.20_pm_r,102362._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
282.10_pm_r,106407._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
309.50_pm_r,111054._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
379.30_pm_r,116482._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /) 
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
274.30_pm_r,-12._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
260.40_pm_r,3891._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.90_pm_r,7513._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.60_pm_r,10910._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.70_pm_r,14270._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
231.60_pm_r,17655._pm_r,1._pm_r,85._pm_r,0.24_pm_r,244._pm_r,0._pm_r,67._pm_r,0.08_pm_r,292._pm_r, &
232.50_pm_r,21053._pm_r,1._pm_r,93._pm_r,0.35_pm_r,250._pm_r,0._pm_r,47._pm_r,0.10_pm_r,294._pm_r, &
233.00_pm_r,24459._pm_r,0._pm_r,124._pm_r,0.43_pm_r,255._pm_r,0._pm_r,18._pm_r,0.10_pm_r,291._pm_r, &
236.80_pm_r,27894._pm_r,1._pm_r,222._pm_r,0.47_pm_r,263._pm_r,0._pm_r,353._pm_r,0.07_pm_r,278._pm_r, &
244.60_pm_r,31420._pm_r,1._pm_r,249._pm_r,0.46_pm_r,271._pm_r,0._pm_r,340._pm_r,0.03_pm_r,243._pm_r, &
255.40_pm_r,35075._pm_r,2._pm_r,259._pm_r,0.40_pm_r,280._pm_r,0._pm_r,335._pm_r,0.04_pm_r,166._pm_r, &
267.20_pm_r,38906._pm_r,2._pm_r,265._pm_r,0.31_pm_r,289._pm_r,0._pm_r,334._pm_r,0.07_pm_r,152._pm_r, &
277.50_pm_r,42893._pm_r,3._pm_r,269._pm_r,0.20_pm_r,297._pm_r,0._pm_r,345._pm_r,0.09_pm_r,148._pm_r, &
283.80_pm_r,47011._pm_r,3._pm_r,270._pm_r,0.08_pm_r,266._pm_r,0._pm_r,177._pm_r,0.07_pm_r,184._pm_r, &
283.70_pm_r,51174._pm_r,3._pm_r,269._pm_r,0.09_pm_r,212._pm_r,0._pm_r,187._pm_r,0.04_pm_r,207._pm_r, &
276.80_pm_r,55282._pm_r,3._pm_r,266._pm_r,0.09_pm_r,214._pm_r,0._pm_r,212._pm_r,0.07_pm_r,287._pm_r, &
265.80_pm_r,59261._pm_r,3._pm_r,266._pm_r,0.06_pm_r,329._pm_r,0._pm_r,264._pm_r,0.19_pm_r,317._pm_r, &
252.20_pm_r,63052._pm_r,3._pm_r,268._pm_r,0.12_pm_r,5._pm_r,1._pm_r,299._pm_r,0.21_pm_r,333._pm_r, &
237.30_pm_r,66643._pm_r,3._pm_r,272._pm_r,0.13_pm_r,4._pm_r,1._pm_r,316._pm_r,0.17_pm_r,3._pm_r, &
221.20_pm_r,69996._pm_r,3._pm_r,275._pm_r,0.08_pm_r,342._pm_r,1._pm_r,334._pm_r,0.19_pm_r,60._pm_r, &
204.70_pm_r,73121._pm_r,3._pm_r,276._pm_r,0.07_pm_r,278._pm_r,1._pm_r,1._pm_r,0.30_pm_r,90._pm_r, &
188.30_pm_r,75994._pm_r,3._pm_r,275._pm_r,0.11_pm_r,236._pm_r,1._pm_r,36._pm_r,0.38_pm_r,99._pm_r, &
172.70_pm_r,78642._pm_r,3._pm_r,273._pm_r,0.13_pm_r,228._pm_r,1._pm_r,62._pm_r,0.41_pm_r,103._pm_r, &
159.50_pm_r,81062._pm_r,3._pm_r,270._pm_r,0.14_pm_r,226._pm_r,2._pm_r,75._pm_r,0.37_pm_r,107._pm_r, &
147.20_pm_r,83308._pm_r,4._pm_r,268._pm_r,0.13_pm_r,225._pm_r,2._pm_r,82._pm_r,0.33_pm_r,110._pm_r, &
137.90_pm_r,85345._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
135.80_pm_r,87313._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
138.70_pm_r,89314._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
145.90_pm_r,91401._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
157.80_pm_r,93637._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
177.10_pm_r,96115._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.90_pm_r,98969._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
247.30_pm_r,102395._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
288.70_pm_r,106535._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
310.00_pm_r,111239._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
380.40_pm_r,116668._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)


  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_juillet = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_juillet,(/10,nb_alt,nb_lat/))


  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)

end subroutine cps_atmi_juillet

subroutine cps_atmi_aout (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_aout
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de AOUT
!
!$Description
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de AOUT
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_aout (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments            
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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

  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
263.10_pm_r,-320._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
239.80_pm_r,3351._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.50_pm_r,6689._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.00_pm_r,9750._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.00_pm_r,12648._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.90_pm_r,15470._pm_r,9._pm_r,242._pm_r,1.17_pm_r,206._pm_r,6._pm_r,166._pm_r,0.47_pm_r,13._pm_r, &
188.30_pm_r,18246._pm_r,11._pm_r,233._pm_r,2.42_pm_r,191._pm_r,5._pm_r,161._pm_r,0.97_pm_r,5._pm_r, &
186.20_pm_r,20992._pm_r,15._pm_r,216._pm_r,4.21_pm_r,166._pm_r,3._pm_r,148._pm_r,1.56_pm_r,354._pm_r, &
179.00_pm_r,23660._pm_r,18._pm_r,200._pm_r,3.55_pm_r,143._pm_r,1._pm_r,112._pm_r,1.03_pm_r,340._pm_r, &
192.70_pm_r,26363._pm_r,20._pm_r,188._pm_r,2.90_pm_r,122._pm_r,1._pm_r,42._pm_r,0.64_pm_r,315._pm_r, &
215.70_pm_r,29348._pm_r,22._pm_r,178._pm_r,2.69_pm_r,104._pm_r,1._pm_r,348._pm_r,0.52_pm_r,285._pm_r, &
234.90_pm_r,32658._pm_r,23._pm_r,168._pm_r,2.56_pm_r,89._pm_r,1._pm_r,316._pm_r,0.52_pm_r,262._pm_r, &
249.80_pm_r,36206._pm_r,23._pm_r,159._pm_r,2.38_pm_r,73._pm_r,2._pm_r,295._pm_r,0.52_pm_r,245._pm_r, &
259.00_pm_r,39937._pm_r,23._pm_r,150._pm_r,2.64_pm_r,48._pm_r,2._pm_r,282._pm_r,0.36_pm_r,237._pm_r, &
267.10_pm_r,43788._pm_r,22._pm_r,141._pm_r,2.89_pm_r,24._pm_r,3._pm_r,275._pm_r,0.18_pm_r,236._pm_r, &
273.60_pm_r,47752._pm_r,19._pm_r,131._pm_r,2.79_pm_r,355._pm_r,3._pm_r,273._pm_r,0.09_pm_r,241._pm_r, &
275.50_pm_r,51778._pm_r,16._pm_r,125._pm_r,2.96_pm_r,312._pm_r,3._pm_r,271._pm_r,0.15_pm_r,228._pm_r, &
267.80_pm_r,55763._pm_r,11._pm_r,130._pm_r,3.59_pm_r,279._pm_r,3._pm_r,266._pm_r,0.28_pm_r,217._pm_r, &
255.50_pm_r,59601._pm_r,8._pm_r,160._pm_r,4.27_pm_r,261._pm_r,3._pm_r,260._pm_r,0.34_pm_r,214._pm_r, &
243.40_pm_r,63248._pm_r,10._pm_r,202._pm_r,4.41_pm_r,251._pm_r,4._pm_r,255._pm_r,0.31_pm_r,214._pm_r, &
233.20_pm_r,66740._pm_r,15._pm_r,219._pm_r,4.14_pm_r,243._pm_r,4._pm_r,251._pm_r,0.21_pm_r,215._pm_r, &
226.40_pm_r,70101._pm_r,20._pm_r,225._pm_r,3.66_pm_r,235._pm_r,4._pm_r,250._pm_r,0.07_pm_r,219._pm_r, &
222.50_pm_r,73386._pm_r,25._pm_r,226._pm_r,3.19_pm_r,228._pm_r,4._pm_r,250._pm_r,0.06_pm_r,31._pm_r, &
219.90_pm_r,76624._pm_r,30._pm_r,226._pm_r,2.80_pm_r,221._pm_r,4._pm_r,251._pm_r,0.18_pm_r,40._pm_r, &
217.50_pm_r,79827._pm_r,33._pm_r,225._pm_r,2.43_pm_r,214._pm_r,4._pm_r,253._pm_r,0.25_pm_r,40._pm_r, &
214.80_pm_r,82989._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.10_pm_r,86108._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.10_pm_r,89099._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.10_pm_r,91973._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.80_pm_r,94805._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.70_pm_r,97675._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.70_pm_r,100668._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.00_pm_r,103882._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
241.90_pm_r,107431._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
273.20_pm_r,111454._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
338.70_pm_r,116282._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
268.10_pm_r,-178._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.80_pm_r,3560._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.80_pm_r,6953._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.70_pm_r,10060._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
198.90_pm_r,13007._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.00_pm_r,15896._pm_r,16._pm_r,254._pm_r,3.07_pm_r,181._pm_r,13._pm_r,156._pm_r,1.59_pm_r,354._pm_r, &
194.70_pm_r,18755._pm_r,18._pm_r,235._pm_r,5.11_pm_r,170._pm_r,10._pm_r,151._pm_r,2.75_pm_r,349._pm_r, &
194.20_pm_r,21604._pm_r,22._pm_r,213._pm_r,6.34_pm_r,153._pm_r,5._pm_r,138._pm_r,3.45_pm_r,342._pm_r, &
191.50_pm_r,24420._pm_r,26._pm_r,195._pm_r,5.31_pm_r,127._pm_r,2._pm_r,86._pm_r,2.62_pm_r,329._pm_r, &
203.70_pm_r,27301._pm_r,28._pm_r,180._pm_r,4.66_pm_r,96._pm_r,3._pm_r,358._pm_r,1.63_pm_r,307._pm_r, &
223.90_pm_r,30426._pm_r,28._pm_r,166._pm_r,4.83_pm_r,71._pm_r,4._pm_r,329._pm_r,1.14_pm_r,271._pm_r, &
241.30_pm_r,33842._pm_r,27._pm_r,151._pm_r,5.02_pm_r,55._pm_r,4._pm_r,308._pm_r,1.10_pm_r,233._pm_r, &
254.70_pm_r,37473._pm_r,26._pm_r,135._pm_r,4.88_pm_r,41._pm_r,5._pm_r,287._pm_r,1.21_pm_r,208._pm_r, &
262.20_pm_r,41262._pm_r,26._pm_r,120._pm_r,4.49_pm_r,27._pm_r,5._pm_r,272._pm_r,0.64_pm_r,203._pm_r, &
268.00_pm_r,45145._pm_r,26._pm_r,106._pm_r,4.01_pm_r,5._pm_r,5._pm_r,267._pm_r,0.27_pm_r,250._pm_r, &
270.50_pm_r,49094._pm_r,23._pm_r,94._pm_r,4.06_pm_r,331._pm_r,6._pm_r,267._pm_r,0.46_pm_r,274._pm_r, &
267.60_pm_r,53042._pm_r,19._pm_r,81._pm_r,5.06_pm_r,300._pm_r,7._pm_r,266._pm_r,0.75_pm_r,241._pm_r, &
257.80_pm_r,56892._pm_r,13._pm_r,64._pm_r,5.75_pm_r,280._pm_r,8._pm_r,258._pm_r,1.10_pm_r,201._pm_r, &
246.10_pm_r,60585._pm_r,6._pm_r,23._pm_r,6.07_pm_r,265._pm_r,9._pm_r,247._pm_r,1.45_pm_r,185._pm_r, &
236.30_pm_r,64112._pm_r,7._pm_r,304._pm_r,5.85_pm_r,253._pm_r,10._pm_r,235._pm_r,1.58_pm_r,176._pm_r, &
229.00_pm_r,67520._pm_r,14._pm_r,274._pm_r,5.34_pm_r,242._pm_r,11._pm_r,224._pm_r,1.51_pm_r,169._pm_r, &
224.20_pm_r,70834._pm_r,20._pm_r,261._pm_r,4.78_pm_r,230._pm_r,12._pm_r,216._pm_r,1.36_pm_r,163._pm_r, &
221.40_pm_r,74096._pm_r,26._pm_r,252._pm_r,4.34_pm_r,219._pm_r,13._pm_r,209._pm_r,1.20_pm_r,155._pm_r, &
219.20_pm_r,77323._pm_r,31._pm_r,245._pm_r,4.00_pm_r,210._pm_r,14._pm_r,203._pm_r,1.06_pm_r,148._pm_r, &
216.80_pm_r,80517._pm_r,35._pm_r,240._pm_r,3.63_pm_r,203._pm_r,15._pm_r,199._pm_r,0.92_pm_r,141._pm_r, &
213.60_pm_r,83668._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.30_pm_r,86766._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.40_pm_r,89731._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.80_pm_r,92583._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.30_pm_r,95397._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.00_pm_r,98246._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.30_pm_r,101211._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.20_pm_r,104387._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
239.50_pm_r,107895._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
273.70_pm_r,111903._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
340.80_pm_r,116764._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
273.10_pm_r,-31._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.10_pm_r,3785._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.80_pm_r,7248._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.40_pm_r,10421._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.40_pm_r,13459._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.10_pm_r,16473._pm_r,19._pm_r,261._pm_r,4.33_pm_r,151._pm_r,16._pm_r,137._pm_r,2.77_pm_r,334._pm_r, &
204.30_pm_r,19469._pm_r,17._pm_r,237._pm_r,6.13_pm_r,143._pm_r,12._pm_r,131._pm_r,4.05_pm_r,330._pm_r, &
204.90_pm_r,22465._pm_r,18._pm_r,206._pm_r,6.75_pm_r,129._pm_r,6._pm_r,113._pm_r,4.60_pm_r,323._pm_r, &
204.50_pm_r,25460._pm_r,21._pm_r,179._pm_r,6.03_pm_r,106._pm_r,3._pm_r,19._pm_r,3.93_pm_r,312._pm_r, &
212.60_pm_r,28506._pm_r,22._pm_r,158._pm_r,5.49_pm_r,73._pm_r,6._pm_r,330._pm_r,2.54_pm_r,290._pm_r, &
227.50_pm_r,31722._pm_r,22._pm_r,137._pm_r,6.05_pm_r,43._pm_r,8._pm_r,311._pm_r,1.86_pm_r,242._pm_r, &
241.80_pm_r,35166._pm_r,22._pm_r,113._pm_r,6.59_pm_r,25._pm_r,8._pm_r,291._pm_r,2.24_pm_r,202._pm_r, &
253.70_pm_r,38795._pm_r,24._pm_r,89._pm_r,6.49_pm_r,12._pm_r,9._pm_r,266._pm_r,2.75_pm_r,181._pm_r, &
260.40_pm_r,42564._pm_r,26._pm_r,70._pm_r,5.28_pm_r,360._pm_r,9._pm_r,243._pm_r,2.08_pm_r,172._pm_r, &
264.20_pm_r,46409._pm_r,28._pm_r,56._pm_r,4.16_pm_r,334._pm_r,10._pm_r,229._pm_r,1.52_pm_r,178._pm_r, &
262.70_pm_r,50273._pm_r,28._pm_r,43._pm_r,4.54_pm_r,299._pm_r,12._pm_r,223._pm_r,1.44_pm_r,199._pm_r, &
255.70_pm_r,54077._pm_r,25._pm_r,28._pm_r,6.00_pm_r,279._pm_r,14._pm_r,220._pm_r,1.80_pm_r,214._pm_r, &
245.80_pm_r,57748._pm_r,23._pm_r,6._pm_r,6.46_pm_r,267._pm_r,17._pm_r,219._pm_r,1.67_pm_r,203._pm_r, &
237.10_pm_r,61284._pm_r,23._pm_r,342._pm_r,6.19_pm_r,258._pm_r,19._pm_r,216._pm_r,1.42_pm_r,188._pm_r, &
231.40_pm_r,64711._pm_r,24._pm_r,322._pm_r,5.36_pm_r,249._pm_r,21._pm_r,213._pm_r,1.19_pm_r,169._pm_r, &
227.70_pm_r,68072._pm_r,27._pm_r,307._pm_r,4.35_pm_r,238._pm_r,22._pm_r,209._pm_r,1.08_pm_r,148._pm_r, &
224.90_pm_r,71386._pm_r,29._pm_r,296._pm_r,3.47_pm_r,224._pm_r,22._pm_r,205._pm_r,1.10_pm_r,130._pm_r, &
222.30_pm_r,74661._pm_r,30._pm_r,287._pm_r,2.88_pm_r,207._pm_r,22._pm_r,201._pm_r,1.15_pm_r,116._pm_r, &
219.50_pm_r,77898._pm_r,30._pm_r,279._pm_r,2.54_pm_r,191._pm_r,22._pm_r,197._pm_r,1.19_pm_r,108._pm_r, &
216.20_pm_r,81091._pm_r,30._pm_r,273._pm_r,2.29_pm_r,178._pm_r,22._pm_r,192._pm_r,1.17_pm_r,102._pm_r, &
211.70_pm_r,84228._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.50_pm_r,87293._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.90_pm_r,90218._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.80_pm_r,93039._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.60_pm_r,95828._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.80_pm_r,98650._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.20_pm_r,101577._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.90_pm_r,104697._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.50_pm_r,108142._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
273.80_pm_r,112117._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
343.70_pm_r,117016._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
278.10_pm_r,2._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
255.20_pm_r,3902._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.80_pm_r,7449._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.50_pm_r,10706._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.30_pm_r,13849._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.80_pm_r,16985._pm_r,13._pm_r,263._pm_r,3.44_pm_r,130._pm_r,14._pm_r,126._pm_r,3.16_pm_r,328._pm_r, &
213.40_pm_r,20111._pm_r,9._pm_r,238._pm_r,4.49_pm_r,122._pm_r,9._pm_r,113._pm_r,4.33_pm_r,324._pm_r, &
214.70_pm_r,23246._pm_r,8._pm_r,192._pm_r,4.73_pm_r,110._pm_r,4._pm_r,65._pm_r,4.83_pm_r,318._pm_r, &
216.20_pm_r,26400._pm_r,10._pm_r,152._pm_r,4.38_pm_r,88._pm_r,7._pm_r,350._pm_r,4.47_pm_r,307._pm_r, &
219.60_pm_r,29589._pm_r,13._pm_r,124._pm_r,4.31_pm_r,56._pm_r,11._pm_r,326._pm_r,3.45_pm_r,287._pm_r, &
227.60_pm_r,32856._pm_r,15._pm_r,98._pm_r,4.93_pm_r,26._pm_r,14._pm_r,311._pm_r,2.68_pm_r,246._pm_r, &
237.80_pm_r,36266._pm_r,18._pm_r,73._pm_r,5.42_pm_r,7._pm_r,15._pm_r,295._pm_r,3.06_pm_r,205._pm_r, &
248.00_pm_r,39821._pm_r,21._pm_r,52._pm_r,5.21_pm_r,352._pm_r,14._pm_r,276._pm_r,3.77_pm_r,181._pm_r, &
255.40_pm_r,43514._pm_r,25._pm_r,38._pm_r,3.82_pm_r,336._pm_r,14._pm_r,256._pm_r,2.94_pm_r,167._pm_r, &
257.90_pm_r,47279._pm_r,26._pm_r,28._pm_r,2.92_pm_r,294._pm_r,15._pm_r,241._pm_r,2.17_pm_r,161._pm_r, &
253.80_pm_r,51031._pm_r,24._pm_r,18._pm_r,4.02_pm_r,258._pm_r,16._pm_r,231._pm_r,1.74_pm_r,178._pm_r, &
245.60_pm_r,54693._pm_r,21._pm_r,2._pm_r,5.41_pm_r,248._pm_r,18._pm_r,226._pm_r,2.07_pm_r,207._pm_r, &
237.60_pm_r,58228._pm_r,19._pm_r,340._pm_r,5.38_pm_r,245._pm_r,21._pm_r,223._pm_r,2.24_pm_r,203._pm_r, &
232.30_pm_r,61668._pm_r,20._pm_r,318._pm_r,4.70_pm_r,243._pm_r,24._pm_r,220._pm_r,2.02_pm_r,196._pm_r, &
228.50_pm_r,65042._pm_r,22._pm_r,302._pm_r,3.72_pm_r,241._pm_r,26._pm_r,217._pm_r,1.63_pm_r,184._pm_r, &
225.20_pm_r,68364._pm_r,25._pm_r,292._pm_r,2.74_pm_r,238._pm_r,28._pm_r,214._pm_r,1.34_pm_r,162._pm_r, &
222.80_pm_r,71644._pm_r,27._pm_r,286._pm_r,1.91_pm_r,232._pm_r,29._pm_r,211._pm_r,1.30_pm_r,137._pm_r, &
220.00_pm_r,74889._pm_r,28._pm_r,282._pm_r,1.28_pm_r,224._pm_r,29._pm_r,207._pm_r,1.41_pm_r,119._pm_r, &
216.60_pm_r,78085._pm_r,29._pm_r,279._pm_r,0.83_pm_r,211._pm_r,29._pm_r,202._pm_r,1.52_pm_r,109._pm_r, &
213.10_pm_r,81233._pm_r,29._pm_r,277._pm_r,0.55_pm_r,192._pm_r,29._pm_r,198._pm_r,1.53_pm_r,102._pm_r, &
208.20_pm_r,84325._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.70_pm_r,87336._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.10_pm_r,90211._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.80_pm_r,93001._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.10_pm_r,95767._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.50_pm_r,98562._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.80_pm_r,101449._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.90_pm_r,104506._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.40_pm_r,107874._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
272.60_pm_r,111797._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
346.60_pm_r,116728._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
285.10_pm_r,-6._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
261.00_pm_r,3991._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.40_pm_r,7625._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.10_pm_r,10959._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.20_pm_r,14153._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.90_pm_r,17314._pm_r,6._pm_r,248._pm_r,1.58_pm_r,118._pm_r,8._pm_r,126._pm_r,2.18_pm_r,330._pm_r, &
217.30_pm_r,20485._pm_r,5._pm_r,223._pm_r,1.97_pm_r,108._pm_r,4._pm_r,107._pm_r,2.94_pm_r,325._pm_r, &
220.10_pm_r,23689._pm_r,4._pm_r,183._pm_r,2.08_pm_r,90._pm_r,3._pm_r,28._pm_r,3.33_pm_r,317._pm_r, &
222.80_pm_r,26930._pm_r,4._pm_r,138._pm_r,2.15_pm_r,64._pm_r,6._pm_r,337._pm_r,3.24_pm_r,305._pm_r, &
225.10_pm_r,30210._pm_r,5._pm_r,100._pm_r,2.53_pm_r,36._pm_r,10._pm_r,319._pm_r,2.83_pm_r,284._pm_r, &
230.30_pm_r,33538._pm_r,8._pm_r,69._pm_r,3.01_pm_r,14._pm_r,13._pm_r,306._pm_r,2.52_pm_r,250._pm_r, &
238.30_pm_r,36970._pm_r,10._pm_r,46._pm_r,3.12_pm_r,359._pm_r,14._pm_r,291._pm_r,2.80_pm_r,214._pm_r, &
247.80_pm_r,40526._pm_r,13._pm_r,31._pm_r,2.71_pm_r,345._pm_r,15._pm_r,273._pm_r,3.41_pm_r,189._pm_r, &
256.30_pm_r,44224._pm_r,15._pm_r,20._pm_r,2.03_pm_r,313._pm_r,15._pm_r,256._pm_r,2.62_pm_r,170._pm_r, &
258.10_pm_r,47999._pm_r,16._pm_r,10._pm_r,2.16_pm_r,261._pm_r,15._pm_r,244._pm_r,1.84_pm_r,151._pm_r, &
252.10_pm_r,51739._pm_r,14._pm_r,357._pm_r,3.20_pm_r,232._pm_r,15._pm_r,236._pm_r,1.16_pm_r,156._pm_r, &
241.70_pm_r,55360._pm_r,11._pm_r,335._pm_r,3.92_pm_r,220._pm_r,16._pm_r,231._pm_r,1.30_pm_r,209._pm_r, &
233.00_pm_r,58831._pm_r,10._pm_r,304._pm_r,3.60_pm_r,213._pm_r,18._pm_r,228._pm_r,2.07_pm_r,205._pm_r, &
228.10_pm_r,62206._pm_r,10._pm_r,276._pm_r,2.94_pm_r,208._pm_r,21._pm_r,224._pm_r,2.25_pm_r,198._pm_r, &
224.20_pm_r,65517._pm_r,12._pm_r,260._pm_r,2.20_pm_r,206._pm_r,24._pm_r,220._pm_r,1.98_pm_r,186._pm_r, &
220.90_pm_r,68777._pm_r,14._pm_r,251._pm_r,1.55_pm_r,208._pm_r,26._pm_r,216._pm_r,1.63_pm_r,165._pm_r, &
218.80_pm_r,71996._pm_r,16._pm_r,246._pm_r,1.05_pm_r,213._pm_r,27._pm_r,212._pm_r,1.52_pm_r,138._pm_r, &
215.70_pm_r,75181._pm_r,17._pm_r,244._pm_r,0.69_pm_r,224._pm_r,27._pm_r,207._pm_r,1.67_pm_r,117._pm_r, &
212.10_pm_r,78311._pm_r,17._pm_r,244._pm_r,0.50_pm_r,242._pm_r,27._pm_r,202._pm_r,1.83_pm_r,105._pm_r, &
208.90_pm_r,81392._pm_r,18._pm_r,244._pm_r,0.38_pm_r,263._pm_r,27._pm_r,196._pm_r,1.87_pm_r,99._pm_r, &
204.20_pm_r,84427._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.00_pm_r,87377._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.60_pm_r,90205._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.40_pm_r,92969._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.00_pm_r,95719._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.70_pm_r,98497._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.70_pm_r,101351._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.00_pm_r,104352._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.00_pm_r,107641._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
270.50_pm_r,111502._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
349.30_pm_r,116454._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
290.90_pm_r,-27._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
267.60_pm_r,4065._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.80_pm_r,7804._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.90_pm_r,11216._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.40_pm_r,14398._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.70_pm_r,17475._pm_r,3._pm_r,199._pm_r,0.21_pm_r,103._pm_r,5._pm_r,129._pm_r,1.33_pm_r,334._pm_r, &
215.70_pm_r,20597._pm_r,3._pm_r,191._pm_r,0.30_pm_r,60._pm_r,3._pm_r,111._pm_r,1.72_pm_r,328._pm_r, &
220.30_pm_r,23791._pm_r,2._pm_r,182._pm_r,0.57_pm_r,29._pm_r,2._pm_r,39._pm_r,1.81_pm_r,318._pm_r, &
224.90_pm_r,27047._pm_r,1._pm_r,163._pm_r,0.94_pm_r,13._pm_r,3._pm_r,341._pm_r,1.66_pm_r,302._pm_r, &
228.70_pm_r,30371._pm_r,1._pm_r,34._pm_r,1.35_pm_r,5._pm_r,5._pm_r,319._pm_r,1.46_pm_r,273._pm_r, &
234.20_pm_r,33756._pm_r,3._pm_r,11._pm_r,1.58_pm_r,358._pm_r,6._pm_r,300._pm_r,1.55_pm_r,234._pm_r, &
242.20_pm_r,37243._pm_r,5._pm_r,4._pm_r,1.49_pm_r,351._pm_r,7._pm_r,278._pm_r,1.95_pm_r,203._pm_r, &
252.20_pm_r,40859._pm_r,7._pm_r,359._pm_r,1.12_pm_r,338._pm_r,8._pm_r,253._pm_r,2.39_pm_r,182._pm_r, &
261.00_pm_r,44625._pm_r,8._pm_r,352._pm_r,1.04_pm_r,288._pm_r,9._pm_r,233._pm_r,1.91_pm_r,168._pm_r, &
262.50_pm_r,48467._pm_r,8._pm_r,340._pm_r,1.50_pm_r,246._pm_r,10._pm_r,220._pm_r,1.34_pm_r,161._pm_r, &
256.30_pm_r,52270._pm_r,8._pm_r,322._pm_r,1.99_pm_r,215._pm_r,11._pm_r,213._pm_r,0.79_pm_r,172._pm_r, &
245.10_pm_r,55948._pm_r,7._pm_r,298._pm_r,2.47_pm_r,188._pm_r,12._pm_r,212._pm_r,0.64_pm_r,221._pm_r, &
234.30_pm_r,59453._pm_r,6._pm_r,265._pm_r,2.58_pm_r,172._pm_r,13._pm_r,211._pm_r,0.70_pm_r,200._pm_r, &
226.30_pm_r,62826._pm_r,7._pm_r,232._pm_r,2.32_pm_r,164._pm_r,14._pm_r,210._pm_r,0.75_pm_r,177._pm_r, &
220.90_pm_r,66098._pm_r,8._pm_r,212._pm_r,1.71_pm_r,164._pm_r,15._pm_r,207._pm_r,0.85_pm_r,156._pm_r, &
217.10_pm_r,69306._pm_r,10._pm_r,204._pm_r,1.01_pm_r,177._pm_r,15._pm_r,202._pm_r,0.98_pm_r,140._pm_r, &
214.70_pm_r,72467._pm_r,11._pm_r,203._pm_r,0.60_pm_r,221._pm_r,16._pm_r,197._pm_r,1.12_pm_r,130._pm_r, &
211.70_pm_r,75591._pm_r,11._pm_r,206._pm_r,0.77_pm_r,269._pm_r,17._pm_r,191._pm_r,1.23_pm_r,124._pm_r, &
208.20_pm_r,78665._pm_r,12._pm_r,212._pm_r,1.05_pm_r,286._pm_r,17._pm_r,186._pm_r,1.27_pm_r,120._pm_r, &
205.00_pm_r,81690._pm_r,12._pm_r,220._pm_r,1.20_pm_r,292._pm_r,18._pm_r,180._pm_r,1.23_pm_r,118._pm_r, &
200.70_pm_r,84674._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.10_pm_r,87573._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.80_pm_r,90364._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.90_pm_r,93115._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.70_pm_r,95863._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,98635._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.50_pm_r,101468._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.10_pm_r,104427._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.50_pm_r,107653._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
268.30_pm_r,111461._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
351.50_pm_r,116424._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
295.70_pm_r,-27._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
273.80_pm_r,4148._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.60_pm_r,7986._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.40_pm_r,11461._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.40_pm_r,14612._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.60_pm_r,17590._pm_r,2._pm_r,153._pm_r,0.41_pm_r,293._pm_r,2._pm_r,130._pm_r,0.55_pm_r,346._pm_r, &
213.10_pm_r,20649._pm_r,1._pm_r,171._pm_r,0.59_pm_r,300._pm_r,2._pm_r,112._pm_r,0.68_pm_r,340._pm_r, &
219.00_pm_r,23816._pm_r,1._pm_r,213._pm_r,0.69_pm_r,309._pm_r,1._pm_r,73._pm_r,0.64_pm_r,330._pm_r, &
224.30_pm_r,27060._pm_r,1._pm_r,264._pm_r,0.74_pm_r,318._pm_r,1._pm_r,29._pm_r,0.49_pm_r,311._pm_r, &
229.40_pm_r,30381._pm_r,2._pm_r,291._pm_r,0.75_pm_r,328._pm_r,1._pm_r,359._pm_r,0.38_pm_r,263._pm_r, &
237.50_pm_r,33795._pm_r,3._pm_r,304._pm_r,0.66_pm_r,338._pm_r,1._pm_r,328._pm_r,0.54_pm_r,212._pm_r, &
246.90_pm_r,37345._pm_r,4._pm_r,312._pm_r,0.48_pm_r,344._pm_r,1._pm_r,262._pm_r,0.82_pm_r,188._pm_r, &
257.10_pm_r,41031._pm_r,4._pm_r,316._pm_r,0.25_pm_r,346._pm_r,2._pm_r,210._pm_r,1.04_pm_r,174._pm_r, &
265.50_pm_r,44866._pm_r,4._pm_r,314._pm_r,0.38_pm_r,255._pm_r,3._pm_r,192._pm_r,0.86_pm_r,170._pm_r, &
266.90_pm_r,48773._pm_r,5._pm_r,304._pm_r,0.82_pm_r,226._pm_r,4._pm_r,188._pm_r,0.65_pm_r,186._pm_r, &
260.50_pm_r,52640._pm_r,5._pm_r,286._pm_r,1.20_pm_r,196._pm_r,5._pm_r,191._pm_r,0.56_pm_r,224._pm_r, &
248.50_pm_r,56374._pm_r,5._pm_r,261._pm_r,1.74_pm_r,165._pm_r,5._pm_r,197._pm_r,0.60_pm_r,259._pm_r, &
236.50_pm_r,59920._pm_r,5._pm_r,227._pm_r,1.99_pm_r,149._pm_r,6._pm_r,203._pm_r,0.31_pm_r,263._pm_r, &
227.20_pm_r,63316._pm_r,6._pm_r,198._pm_r,1.89_pm_r,143._pm_r,6._pm_r,206._pm_r,0.05_pm_r,233._pm_r, &
219.60_pm_r,66584._pm_r,8._pm_r,183._pm_r,1.42_pm_r,146._pm_r,6._pm_r,205._pm_r,0.14_pm_r,134._pm_r, &
214.10_pm_r,69759._pm_r,9._pm_r,177._pm_r,0.86_pm_r,160._pm_r,6._pm_r,202._pm_r,0.24_pm_r,138._pm_r, &
211.10_pm_r,72871._pm_r,10._pm_r,177._pm_r,0.54_pm_r,205._pm_r,6._pm_r,199._pm_r,0.35_pm_r,147._pm_r, &
208.40_pm_r,75945._pm_r,11._pm_r,181._pm_r,0.69_pm_r,250._pm_r,7._pm_r,195._pm_r,0.43_pm_r,152._pm_r, &
205.20_pm_r,78975._pm_r,11._pm_r,187._pm_r,0.94_pm_r,267._pm_r,7._pm_r,191._pm_r,0.48_pm_r,155._pm_r, &
202.20_pm_r,81958._pm_r,11._pm_r,195._pm_r,1.08_pm_r,274._pm_r,8._pm_r,188._pm_r,0.49_pm_r,156._pm_r, &
198.10_pm_r,84903._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.20_pm_r,87766._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.20_pm_r,90541._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.20_pm_r,93291._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.10_pm_r,96047._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,98825._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.60_pm_r,101652._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.40_pm_r,104592._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.00_pm_r,107786._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
267.20_pm_r,111564._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
353.60_pm_r,116541._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
299.00_pm_r,-60._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.00_pm_r,4158._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.30_pm_r,8033._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.70_pm_r,11532._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.00_pm_r,14669._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.30_pm_r,17607._pm_r,2._pm_r,167._pm_r,0.15_pm_r,295._pm_r,1._pm_r,72._pm_r,0.10_pm_r,22._pm_r, &
211.90_pm_r,20642._pm_r,2._pm_r,174._pm_r,0.21_pm_r,298._pm_r,1._pm_r,62._pm_r,0.12_pm_r,22._pm_r, &
217.40_pm_r,23787._pm_r,2._pm_r,184._pm_r,0.24_pm_r,299._pm_r,1._pm_r,55._pm_r,0.11_pm_r,23._pm_r, &
223.30_pm_r,27011._pm_r,2._pm_r,197._pm_r,0.25_pm_r,302._pm_r,1._pm_r,51._pm_r,0.07_pm_r,25._pm_r, &
229.70_pm_r,30329._pm_r,1._pm_r,211._pm_r,0.24_pm_r,305._pm_r,1._pm_r,51._pm_r,0.02_pm_r,108._pm_r, &
238.60_pm_r,33752._pm_r,1._pm_r,224._pm_r,0.20_pm_r,309._pm_r,1._pm_r,55._pm_r,0.10_pm_r,165._pm_r, &
248.60_pm_r,37321._pm_r,2._pm_r,233._pm_r,0.13_pm_r,315._pm_r,1._pm_r,66._pm_r,0.19_pm_r,172._pm_r, &
258.70_pm_r,41032._pm_r,2._pm_r,239._pm_r,0.06_pm_r,329._pm_r,1._pm_r,87._pm_r,0.27_pm_r,170._pm_r, &
266.00_pm_r,44881._pm_r,2._pm_r,241._pm_r,0.14_pm_r,249._pm_r,1._pm_r,107._pm_r,0.23_pm_r,168._pm_r, &
267.90_pm_r,48797._pm_r,2._pm_r,240._pm_r,0.33_pm_r,221._pm_r,1._pm_r,120._pm_r,0.18_pm_r,183._pm_r, &
263.10_pm_r,52691._pm_r,2._pm_r,231._pm_r,0.47_pm_r,185._pm_r,1._pm_r,131._pm_r,0.18_pm_r,226._pm_r, &
252.50_pm_r,56475._pm_r,3._pm_r,216._pm_r,0.73_pm_r,150._pm_r,1._pm_r,146._pm_r,0.26_pm_r,239._pm_r, &
239.90_pm_r,60077._pm_r,3._pm_r,195._pm_r,0.86_pm_r,132._pm_r,1._pm_r,162._pm_r,0.29_pm_r,208._pm_r, &
228.60_pm_r,63509._pm_r,4._pm_r,179._pm_r,0.80_pm_r,126._pm_r,2._pm_r,170._pm_r,0.40_pm_r,177._pm_r, &
218.80_pm_r,66780._pm_r,5._pm_r,169._pm_r,0.56_pm_r,133._pm_r,3._pm_r,169._pm_r,0.57_pm_r,160._pm_r, &
211.90_pm_r,69933._pm_r,5._pm_r,166._pm_r,0.32_pm_r,164._pm_r,3._pm_r,165._pm_r,0.74_pm_r,151._pm_r, &
208.10_pm_r,73006._pm_r,6._pm_r,168._pm_r,0.35_pm_r,221._pm_r,5._pm_r,161._pm_r,0.90_pm_r,146._pm_r, &
205.10_pm_r,76033._pm_r,6._pm_r,174._pm_r,0.54_pm_r,246._pm_r,6._pm_r,157._pm_r,1.02_pm_r,143._pm_r, &
202.90_pm_r,79016._pm_r,6._pm_r,182._pm_r,0.70_pm_r,254._pm_r,8._pm_r,154._pm_r,1.09_pm_r,142._pm_r, &
200.70_pm_r,81973._pm_r,7._pm_r,192._pm_r,0.78_pm_r,257._pm_r,9._pm_r,152._pm_r,1.08_pm_r,141._pm_r, &
196.70_pm_r,84894._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.40_pm_r,87738._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.40_pm_r,90511._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.90_pm_r,93270._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.90_pm_r,96038._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,98827._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.80_pm_r,101661._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.50_pm_r,104605._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.90_pm_r,107798._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
267.90_pm_r,111583._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
356.10_pm_r,116588._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
300.10_pm_r,-100._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.10_pm_r,4127._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.80_pm_r,8007._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.10_pm_r,11513._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.00_pm_r,14654._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.00_pm_r,17591._pm_r,2._pm_r,169._pm_r,0.11_pm_r,265._pm_r,0._pm_r,14._pm_r,0.06_pm_r,45._pm_r, &
211.20_pm_r,20619._pm_r,2._pm_r,174._pm_r,0.14_pm_r,262._pm_r,0._pm_r,30._pm_r,0.08_pm_r,45._pm_r, &
216.40_pm_r,23751._pm_r,2._pm_r,181._pm_r,0.17_pm_r,260._pm_r,0._pm_r,39._pm_r,0.09_pm_r,49._pm_r, &
222.40_pm_r,26959._pm_r,2._pm_r,189._pm_r,0.17_pm_r,260._pm_r,1._pm_r,45._pm_r,0.08_pm_r,60._pm_r, &
228.90_pm_r,30266._pm_r,2._pm_r,196._pm_r,0.15_pm_r,258._pm_r,1._pm_r,50._pm_r,0.06_pm_r,76._pm_r, &
238.60_pm_r,33685._pm_r,2._pm_r,201._pm_r,0.10_pm_r,259._pm_r,1._pm_r,56._pm_r,0.05_pm_r,112._pm_r, &
248.80_pm_r,37253._pm_r,2._pm_r,203._pm_r,0.04_pm_r,278._pm_r,1._pm_r,64._pm_r,0.07_pm_r,153._pm_r, &
258.30_pm_r,40960._pm_r,2._pm_r,203._pm_r,0.04_pm_r,56._pm_r,1._pm_r,76._pm_r,0.10_pm_r,169._pm_r, &
265.30_pm_r,44799._pm_r,2._pm_r,203._pm_r,0.06_pm_r,305._pm_r,1._pm_r,89._pm_r,0.11_pm_r,129._pm_r, &
267.40_pm_r,48705._pm_r,2._pm_r,207._pm_r,0.15_pm_r,259._pm_r,1._pm_r,95._pm_r,0.18_pm_r,93._pm_r, &
264.00_pm_r,52599._pm_r,2._pm_r,209._pm_r,0.18_pm_r,193._pm_r,1._pm_r,96._pm_r,0.15_pm_r,62._pm_r, &
254.10_pm_r,56397._pm_r,2._pm_r,202._pm_r,0.47_pm_r,140._pm_r,1._pm_r,96._pm_r,0.13_pm_r,312._pm_r, &
241.90_pm_r,60023._pm_r,3._pm_r,186._pm_r,0.67_pm_r,128._pm_r,1._pm_r,104._pm_r,0.29_pm_r,254._pm_r, &
230.50_pm_r,63483._pm_r,3._pm_r,173._pm_r,0.64_pm_r,132._pm_r,1._pm_r,141._pm_r,0.43_pm_r,220._pm_r, &
220.50_pm_r,66781._pm_r,4._pm_r,167._pm_r,0.48_pm_r,161._pm_r,1._pm_r,172._pm_r,0.62_pm_r,193._pm_r, &
212.70_pm_r,69955._pm_r,5._pm_r,170._pm_r,0.51_pm_r,207._pm_r,2._pm_r,176._pm_r,0.85_pm_r,176._pm_r, &
207.70_pm_r,73035._pm_r,5._pm_r,178._pm_r,0.76_pm_r,235._pm_r,4._pm_r,172._pm_r,1.10_pm_r,165._pm_r, &
204.50_pm_r,76059._pm_r,6._pm_r,189._pm_r,1.03_pm_r,245._pm_r,5._pm_r,168._pm_r,1.33_pm_r,160._pm_r, &
203.20_pm_r,79045._pm_r,7._pm_r,200._pm_r,1.21_pm_r,250._pm_r,7._pm_r,164._pm_r,1.47_pm_r,157._pm_r, &
201.00_pm_r,82014._pm_r,8._pm_r,209._pm_r,1.29_pm_r,252._pm_r,9._pm_r,161._pm_r,1.51_pm_r,155._pm_r, &
196.40_pm_r,84930._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.10_pm_r,87766._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.70_pm_r,90541._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,93306._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,96081._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.90_pm_r,98878._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.90_pm_r,101725._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.20_pm_r,104690._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.40_pm_r,107917._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
270.90_pm_r,111746._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
359.20_pm_r,116799._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
301.30_pm_r,-106._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.60_pm_r,4133._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.20_pm_r,8019._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.60_pm_r,11531._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.30_pm_r,14677._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.10_pm_r,17616._pm_r,2._pm_r,173._pm_r,0.17_pm_r,278._pm_r,0._pm_r,242._pm_r,0.06_pm_r,38._pm_r, &
211.30_pm_r,20644._pm_r,2._pm_r,182._pm_r,0.22_pm_r,275._pm_r,0._pm_r,263._pm_r,0.07_pm_r,36._pm_r, &
216.80_pm_r,23780._pm_r,2._pm_r,193._pm_r,0.23_pm_r,270._pm_r,0._pm_r,324._pm_r,0.07_pm_r,41._pm_r, &
222.70_pm_r,26995._pm_r,2._pm_r,203._pm_r,0.22_pm_r,262._pm_r,0._pm_r,8._pm_r,0.06_pm_r,48._pm_r, &
229.40_pm_r,30305._pm_r,2._pm_r,210._pm_r,0.18_pm_r,250._pm_r,0._pm_r,25._pm_r,0.05_pm_r,69._pm_r, &
237.90_pm_r,33723._pm_r,2._pm_r,213._pm_r,0.13_pm_r,228._pm_r,0._pm_r,40._pm_r,0.05_pm_r,114._pm_r, &
247.30_pm_r,37278._pm_r,2._pm_r,213._pm_r,0.09_pm_r,195._pm_r,0._pm_r,61._pm_r,0.07_pm_r,145._pm_r, &
256.70_pm_r,40967._pm_r,3._pm_r,211._pm_r,0.08_pm_r,155._pm_r,0._pm_r,88._pm_r,0.10_pm_r,151._pm_r, &
264.00_pm_r,44787._pm_r,3._pm_r,210._pm_r,0.02_pm_r,234._pm_r,0._pm_r,104._pm_r,0.13_pm_r,117._pm_r, &
266.50_pm_r,48677._pm_r,3._pm_r,212._pm_r,0.15_pm_r,291._pm_r,1._pm_r,104._pm_r,0.23_pm_r,99._pm_r, &
262.20_pm_r,52555._pm_r,3._pm_r,217._pm_r,0.12_pm_r,300._pm_r,1._pm_r,101._pm_r,0.22_pm_r,95._pm_r, &
253.00_pm_r,56335._pm_r,3._pm_r,218._pm_r,0.14_pm_r,117._pm_r,1._pm_r,101._pm_r,0.05_pm_r,139._pm_r, &
242.70_pm_r,59964._pm_r,3._pm_r,208._pm_r,0.40_pm_r,132._pm_r,1._pm_r,109._pm_r,0.21_pm_r,230._pm_r, &
231.50_pm_r,63440._pm_r,3._pm_r,196._pm_r,0.52_pm_r,154._pm_r,1._pm_r,128._pm_r,0.31_pm_r,219._pm_r, &
222.10_pm_r,66756._pm_r,4._pm_r,191._pm_r,0.63_pm_r,185._pm_r,1._pm_r,151._pm_r,0.37_pm_r,192._pm_r, &
214.70_pm_r,69954._pm_r,5._pm_r,192._pm_r,0.84_pm_r,209._pm_r,2._pm_r,160._pm_r,0.47_pm_r,167._pm_r, &
209.40_pm_r,73056._pm_r,6._pm_r,198._pm_r,1.11_pm_r,224._pm_r,3._pm_r,159._pm_r,0.61_pm_r,151._pm_r, &
206.00_pm_r,76097._pm_r,8._pm_r,205._pm_r,1.35_pm_r,231._pm_r,4._pm_r,156._pm_r,0.74_pm_r,142._pm_r, &
204.60_pm_r,79099._pm_r,10._pm_r,211._pm_r,1.50_pm_r,235._pm_r,5._pm_r,152._pm_r,0.83_pm_r,138._pm_r, &
201.80_pm_r,82087._pm_r,12._pm_r,215._pm_r,1.54_pm_r,237._pm_r,6._pm_r,148._pm_r,0.85_pm_r,135._pm_r, &
196.00_pm_r,85000._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.50_pm_r,87825._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.20_pm_r,90594._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.50_pm_r,93348._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,96111._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.10_pm_r,98904._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.40_pm_r,101764._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.20_pm_r,104763._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.00_pm_r,108049._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.20_pm_r,111957._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
363.00_pm_r,117081._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
300.70_pm_r,-119._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.40_pm_r,4119._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.50_pm_r,8012._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.60_pm_r,11532._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.00_pm_r,14696._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.70_pm_r,17660._pm_r,2._pm_r,148._pm_r,0.54_pm_r,261._pm_r,1._pm_r,181._pm_r,0.26_pm_r,360._pm_r, &
212.40_pm_r,20708._pm_r,2._pm_r,170._pm_r,0.68_pm_r,261._pm_r,0._pm_r,184._pm_r,0.35_pm_r,358._pm_r, &
217.80_pm_r,23859._pm_r,3._pm_r,195._pm_r,0.72_pm_r,259._pm_r,0._pm_r,346._pm_r,0.39_pm_r,358._pm_r, &
223.40_pm_r,27088._pm_r,3._pm_r,212._pm_r,0.68_pm_r,257._pm_r,1._pm_r,355._pm_r,0.40_pm_r,357._pm_r, &
229.80_pm_r,30407._pm_r,4._pm_r,221._pm_r,0.52_pm_r,252._pm_r,1._pm_r,356._pm_r,0.35_pm_r,357._pm_r, &
238.20_pm_r,33829._pm_r,4._pm_r,225._pm_r,0.33_pm_r,241._pm_r,2._pm_r,356._pm_r,0.25_pm_r,355._pm_r, &
246.40_pm_r,37381._pm_r,5._pm_r,226._pm_r,0.19_pm_r,211._pm_r,2._pm_r,356._pm_r,0.14_pm_r,354._pm_r, &
255.60_pm_r,41051._pm_r,5._pm_r,224._pm_r,0.16_pm_r,162._pm_r,2._pm_r,356._pm_r,0.03_pm_r,360._pm_r, &
263.90_pm_r,44865._pm_r,5._pm_r,222._pm_r,0.17_pm_r,190._pm_r,2._pm_r,356._pm_r,0.05_pm_r,135._pm_r, &
264.80_pm_r,48745._pm_r,5._pm_r,221._pm_r,0.29_pm_r,232._pm_r,2._pm_r,358._pm_r,0.08_pm_r,120._pm_r, &
258.20_pm_r,52578._pm_r,6._pm_r,222._pm_r,0.24_pm_r,220._pm_r,2._pm_r,0._pm_r,0.02_pm_r,53._pm_r, &
250.20_pm_r,56303._pm_r,6._pm_r,220._pm_r,0.35_pm_r,127._pm_r,2._pm_r,358._pm_r,0.16_pm_r,308._pm_r, &
242.30_pm_r,59910._pm_r,6._pm_r,212._pm_r,0.66_pm_r,115._pm_r,2._pm_r,352._pm_r,0.21_pm_r,290._pm_r, &
232.10_pm_r,63387._pm_r,6._pm_r,203._pm_r,0.66_pm_r,126._pm_r,2._pm_r,346._pm_r,0.12_pm_r,265._pm_r, &
222.70_pm_r,66712._pm_r,6._pm_r,196._pm_r,0.55_pm_r,170._pm_r,2._pm_r,345._pm_r,0.14_pm_r,145._pm_r, &
215.60_pm_r,69921._pm_r,7._pm_r,196._pm_r,0.84_pm_r,217._pm_r,2._pm_r,351._pm_r,0.39_pm_r,127._pm_r, &
211.00_pm_r,73041._pm_r,9._pm_r,202._pm_r,1.32_pm_r,234._pm_r,2._pm_r,11._pm_r,0.62_pm_r,121._pm_r, &
207.60_pm_r,76106._pm_r,11._pm_r,209._pm_r,1.75_pm_r,240._pm_r,2._pm_r,49._pm_r,0.81_pm_r,119._pm_r, &
205.20_pm_r,79127._pm_r,13._pm_r,216._pm_r,2.01_pm_r,244._pm_r,2._pm_r,79._pm_r,0.91_pm_r,118._pm_r, &
201.20_pm_r,82117._pm_r,16._pm_r,221._pm_r,2.11_pm_r,245._pm_r,4._pm_r,93._pm_r,0.93_pm_r,117._pm_r, &
194.40_pm_r,85010._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.30_pm_r,87805._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.70_pm_r,90539._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.80_pm_r,93254._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.70_pm_r,95981._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.50_pm_r,98752._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.00_pm_r,101618._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.20_pm_r,104659._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
231.50_pm_r,108026._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
283.00_pm_r,112041._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
367.10_pm_r,117247._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
299.30_pm_r,-110._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.10_pm_r,4113._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.80_pm_r,7995._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.00_pm_r,11511._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.90_pm_r,14697._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.80_pm_r,17709._pm_r,3._pm_r,154._pm_r,0.75_pm_r,259._pm_r,1._pm_r,182._pm_r,0.33_pm_r,0._pm_r, &
214.50_pm_r,20794._pm_r,3._pm_r,180._pm_r,0.95_pm_r,258._pm_r,1._pm_r,184._pm_r,0.45_pm_r,359._pm_r, &
219.60_pm_r,23973._pm_r,3._pm_r,204._pm_r,1.01_pm_r,256._pm_r,0._pm_r,276._pm_r,0.52_pm_r,358._pm_r, &
225.20_pm_r,27227._pm_r,5._pm_r,219._pm_r,0.95_pm_r,253._pm_r,1._pm_r,353._pm_r,0.55_pm_r,356._pm_r, &
230.90_pm_r,30568._pm_r,6._pm_r,226._pm_r,0.76_pm_r,248._pm_r,2._pm_r,354._pm_r,0.52_pm_r,353._pm_r, &
238.30_pm_r,34000._pm_r,7._pm_r,228._pm_r,0.51_pm_r,237._pm_r,2._pm_r,353._pm_r,0.44_pm_r,349._pm_r, &
247.00_pm_r,37555._pm_r,7._pm_r,228._pm_r,0.31_pm_r,214._pm_r,3._pm_r,352._pm_r,0.33_pm_r,342._pm_r, &
256.30_pm_r,41238._pm_r,7._pm_r,227._pm_r,0.25_pm_r,180._pm_r,3._pm_r,350._pm_r,0.22_pm_r,329._pm_r, &
263.30_pm_r,45050._pm_r,8._pm_r,225._pm_r,0.23_pm_r,199._pm_r,4._pm_r,348._pm_r,0.12_pm_r,321._pm_r, &
263.90_pm_r,48918._pm_r,8._pm_r,225._pm_r,0.29_pm_r,241._pm_r,4._pm_r,347._pm_r,0.07_pm_r,312._pm_r, &
256.80_pm_r,52735._pm_r,8._pm_r,226._pm_r,0.17_pm_r,238._pm_r,4._pm_r,345._pm_r,0.17_pm_r,285._pm_r, &
248.00_pm_r,56434._pm_r,8._pm_r,225._pm_r,0.32_pm_r,114._pm_r,4._pm_r,340._pm_r,0.37_pm_r,278._pm_r, &
239.80_pm_r,60006._pm_r,8._pm_r,220._pm_r,0.60_pm_r,114._pm_r,4._pm_r,333._pm_r,0.40_pm_r,271._pm_r, &
230.30_pm_r,63452._pm_r,8._pm_r,214._pm_r,0.52_pm_r,130._pm_r,4._pm_r,327._pm_r,0.21_pm_r,258._pm_r, &
221.60_pm_r,66757._pm_r,8._pm_r,210._pm_r,0.40_pm_r,188._pm_r,4._pm_r,326._pm_r,0.18_pm_r,128._pm_r, &
215.10_pm_r,69954._pm_r,9._pm_r,211._pm_r,0.75_pm_r,236._pm_r,4._pm_r,330._pm_r,0.55_pm_r,108._pm_r, &
210.90_pm_r,73071._pm_r,10._pm_r,216._pm_r,1.22_pm_r,251._pm_r,3._pm_r,343._pm_r,0.88_pm_r,105._pm_r, &
207.30_pm_r,76134._pm_r,12._pm_r,222._pm_r,1.63_pm_r,256._pm_r,3._pm_r,11._pm_r,1.14_pm_r,103._pm_r, &
203.50_pm_r,79143._pm_r,14._pm_r,228._pm_r,1.86_pm_r,259._pm_r,3._pm_r,45._pm_r,1.29_pm_r,103._pm_r, &
197.90_pm_r,82096._pm_r,17._pm_r,233._pm_r,1.93_pm_r,260._pm_r,5._pm_r,66._pm_r,1.32_pm_r,102._pm_r, &
190.10_pm_r,84932._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.70_pm_r,87659._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.80_pm_r,90319._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.20_pm_r,92961._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.40_pm_r,95626._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.30_pm_r,98360._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.70_pm_r,101228._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.20_pm_r,104317._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
239.40_pm_r,107784._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
290.50_pm_r,111922._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
371.00_pm_r,117210._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
295.10_pm_r,-79._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.30_pm_r,4089._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.10_pm_r,7921._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.80_pm_r,11397._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.00_pm_r,14609._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.40_pm_r,17709._pm_r,4._pm_r,183._pm_r,0.59_pm_r,276._pm_r,1._pm_r,181._pm_r,0.23_pm_r,34._pm_r, &
217.50_pm_r,20857._pm_r,4._pm_r,199._pm_r,0.75_pm_r,276._pm_r,0._pm_r,149._pm_r,0.31_pm_r,31._pm_r, &
221.50_pm_r,24072._pm_r,4._pm_r,214._pm_r,0.83_pm_r,276._pm_r,1._pm_r,77._pm_r,0.37_pm_r,25._pm_r, &
226.30_pm_r,27348._pm_r,5._pm_r,227._pm_r,0.77_pm_r,274._pm_r,1._pm_r,46._pm_r,0.39_pm_r,17._pm_r, &
232.50_pm_r,30708._pm_r,6._pm_r,234._pm_r,0.61_pm_r,272._pm_r,1._pm_r,33._pm_r,0.37_pm_r,5._pm_r, &
240.20_pm_r,34165._pm_r,6._pm_r,238._pm_r,0.39_pm_r,264._pm_r,2._pm_r,24._pm_r,0.32_pm_r,351._pm_r, &
250.10_pm_r,37757._pm_r,7._pm_r,239._pm_r,0.23_pm_r,247._pm_r,2._pm_r,16._pm_r,0.28_pm_r,336._pm_r, &
259.70_pm_r,41489._pm_r,7._pm_r,239._pm_r,0.16_pm_r,218._pm_r,2._pm_r,10._pm_r,0.24_pm_r,318._pm_r, &
265.30_pm_r,45341._pm_r,7._pm_r,238._pm_r,0.31_pm_r,234._pm_r,3._pm_r,4._pm_r,0.15_pm_r,306._pm_r, &
264.50_pm_r,49228._pm_r,8._pm_r,240._pm_r,0.53_pm_r,267._pm_r,3._pm_r,1._pm_r,0.10_pm_r,287._pm_r, &
256.80_pm_r,53046._pm_r,8._pm_r,243._pm_r,0.44_pm_r,288._pm_r,3._pm_r,356._pm_r,0.23_pm_r,286._pm_r, &
247.60_pm_r,56742._pm_r,9._pm_r,245._pm_r,0.38_pm_r,43._pm_r,3._pm_r,347._pm_r,0.55_pm_r,291._pm_r, &
238.30_pm_r,60300._pm_r,8._pm_r,245._pm_r,0.84_pm_r,73._pm_r,4._pm_r,335._pm_r,0.70_pm_r,296._pm_r, &
228.40_pm_r,63720._pm_r,6._pm_r,243._pm_r,0.95_pm_r,75._pm_r,5._pm_r,327._pm_r,0.62_pm_r,301._pm_r, &
220.30_pm_r,67001._pm_r,5._pm_r,241._pm_r,0.73_pm_r,66._pm_r,5._pm_r,324._pm_r,0.41_pm_r,314._pm_r, &
213.80_pm_r,70180._pm_r,4._pm_r,242._pm_r,0.44_pm_r,36._pm_r,6._pm_r,325._pm_r,0.20_pm_r,357._pm_r, &
208.00_pm_r,73266._pm_r,4._pm_r,249._pm_r,0.47_pm_r,339._pm_r,6._pm_r,327._pm_r,0.28_pm_r,62._pm_r, &
202.30_pm_r,76273._pm_r,4._pm_r,260._pm_r,0.70_pm_r,313._pm_r,6._pm_r,333._pm_r,0.46_pm_r,81._pm_r, &
196.60_pm_r,79191._pm_r,5._pm_r,270._pm_r,0.89_pm_r,306._pm_r,5._pm_r,340._pm_r,0.56_pm_r,87._pm_r, &
190.00_pm_r,82030._pm_r,6._pm_r,277._pm_r,0.97_pm_r,302._pm_r,5._pm_r,349._pm_r,0.61_pm_r,90._pm_r, &
182.40_pm_r,84751._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
176.30_pm_r,87366._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
172.70_pm_r,89916._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
173.10_pm_r,92458._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
177.10_pm_r,95043._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.70_pm_r,97730._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.40_pm_r,100595._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.20_pm_r,103738._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
248.40_pm_r,107320._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
297.40_pm_r,111588._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
374.20_pm_r,116942._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
287.20_pm_r,-46._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
268.90_pm_r,4021._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.60_pm_r,7770._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.40_pm_r,11192._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.40_pm_r,14438._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.60_pm_r,17649._pm_r,4._pm_r,205._pm_r,0.29_pm_r,330._pm_r,1._pm_r,80._pm_r,0.06_pm_r,333._pm_r, &
221.60_pm_r,20878._pm_r,4._pm_r,211._pm_r,0.38_pm_r,326._pm_r,1._pm_r,73._pm_r,0.10_pm_r,332._pm_r, &
223.70_pm_r,24137._pm_r,4._pm_r,220._pm_r,0.43_pm_r,321._pm_r,1._pm_r,63._pm_r,0.13_pm_r,329._pm_r, &
228.30_pm_r,27442._pm_r,4._pm_r,230._pm_r,0.41_pm_r,311._pm_r,1._pm_r,48._pm_r,0.17_pm_r,327._pm_r, &
235.60_pm_r,30840._pm_r,4._pm_r,238._pm_r,0.36_pm_r,295._pm_r,1._pm_r,33._pm_r,0.18_pm_r,324._pm_r, &
243.90_pm_r,34348._pm_r,4._pm_r,243._pm_r,0.33_pm_r,274._pm_r,1._pm_r,19._pm_r,0.19_pm_r,323._pm_r, &
252.80_pm_r,37987._pm_r,5._pm_r,245._pm_r,0.32_pm_r,254._pm_r,1._pm_r,8._pm_r,0.18_pm_r,319._pm_r, &
261.80_pm_r,41753._pm_r,5._pm_r,245._pm_r,0.32_pm_r,241._pm_r,1._pm_r,0._pm_r,0.16_pm_r,317._pm_r, &
267.70_pm_r,45637._pm_r,6._pm_r,244._pm_r,0.29_pm_r,230._pm_r,2._pm_r,354._pm_r,0.14_pm_r,301._pm_r, &
267.30_pm_r,49562._pm_r,6._pm_r,243._pm_r,0.31_pm_r,239._pm_r,2._pm_r,348._pm_r,0.13_pm_r,301._pm_r, &
259.50_pm_r,53421._pm_r,6._pm_r,243._pm_r,0.26_pm_r,254._pm_r,2._pm_r,344._pm_r,0.15_pm_r,305._pm_r, &
249.60_pm_r,57152._pm_r,7._pm_r,244._pm_r,0.13_pm_r,272._pm_r,2._pm_r,340._pm_r,0.20_pm_r,309._pm_r, &
239.60_pm_r,60732._pm_r,7._pm_r,245._pm_r,0.11_pm_r,327._pm_r,2._pm_r,336._pm_r,0.16_pm_r,314._pm_r, &
229.70_pm_r,64170._pm_r,7._pm_r,247._pm_r,0.19_pm_r,316._pm_r,3._pm_r,335._pm_r,0.08_pm_r,338._pm_r, &
220.90_pm_r,67467._pm_r,7._pm_r,250._pm_r,0.35_pm_r,300._pm_r,3._pm_r,337._pm_r,0.10_pm_r,75._pm_r, &
212.20_pm_r,70641._pm_r,8._pm_r,253._pm_r,0.54_pm_r,290._pm_r,2._pm_r,342._pm_r,0.22_pm_r,95._pm_r, &
203.70_pm_r,73684._pm_r,8._pm_r,257._pm_r,0.70_pm_r,287._pm_r,2._pm_r,351._pm_r,0.31_pm_r,99._pm_r, &
194.90_pm_r,76605._pm_r,9._pm_r,260._pm_r,0.80_pm_r,285._pm_r,2._pm_r,3._pm_r,0.37_pm_r,100._pm_r, &
187.20_pm_r,79397._pm_r,10._pm_r,263._pm_r,0.83_pm_r,284._pm_r,2._pm_r,18._pm_r,0.39_pm_r,102._pm_r, &
179.70_pm_r,82085._pm_r,12._pm_r,265._pm_r,0.81_pm_r,284._pm_r,2._pm_r,32._pm_r,0.39_pm_r,102._pm_r, &
172.40_pm_r,84655._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
167.20_pm_r,87127._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
164.60_pm_r,89548._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
166.40_pm_r,91979._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
172.40_pm_r,94479._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.90_pm_r,97113._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.10_pm_r,99975._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.60_pm_r,103177._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
257.80_pm_r,106881._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
302.80_pm_r,111271._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
376.70_pm_r,116668._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
284.00_pm_r,-46._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
264.70_pm_r,3964._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
239.40_pm_r,7647._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.50_pm_r,11037._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.70_pm_r,14312._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.50_pm_r,17595._pm_r,4._pm_r,192._pm_r,0.46_pm_r,331._pm_r,2._pm_r,65._pm_r,0.16_pm_r,279._pm_r, &
225.10_pm_r,20887._pm_r,4._pm_r,201._pm_r,0.61_pm_r,330._pm_r,1._pm_r,58._pm_r,0.22_pm_r,280._pm_r, &
225.80_pm_r,24186._pm_r,3._pm_r,216._pm_r,0.67_pm_r,329._pm_r,1._pm_r,45._pm_r,0.27_pm_r,281._pm_r, &
229.80_pm_r,27516._pm_r,3._pm_r,234._pm_r,0.60_pm_r,327._pm_r,1._pm_r,21._pm_r,0.28_pm_r,283._pm_r, &
237.20_pm_r,30935._pm_r,3._pm_r,250._pm_r,0.44_pm_r,322._pm_r,1._pm_r,356._pm_r,0.25_pm_r,284._pm_r, &
246.30_pm_r,34472._pm_r,3._pm_r,259._pm_r,0.25_pm_r,310._pm_r,1._pm_r,339._pm_r,0.20_pm_r,285._pm_r, &
255.40_pm_r,38149._pm_r,3._pm_r,261._pm_r,0.13_pm_r,272._pm_r,1._pm_r,329._pm_r,0.15_pm_r,290._pm_r, &
264.60_pm_r,41955._pm_r,4._pm_r,261._pm_r,0.13_pm_r,231._pm_r,1._pm_r,325._pm_r,0.10_pm_r,299._pm_r, &
270.30_pm_r,45882._pm_r,4._pm_r,258._pm_r,0.17_pm_r,219._pm_r,2._pm_r,325._pm_r,0.10_pm_r,345._pm_r, &
269.10_pm_r,49838._pm_r,4._pm_r,256._pm_r,0.17_pm_r,234._pm_r,2._pm_r,328._pm_r,0.21_pm_r,346._pm_r, &
262.80_pm_r,53735._pm_r,4._pm_r,256._pm_r,0.02_pm_r,63._pm_r,2._pm_r,329._pm_r,0.33_pm_r,329._pm_r, &
254.10_pm_r,57524._pm_r,4._pm_r,256._pm_r,0.35_pm_r,69._pm_r,3._pm_r,327._pm_r,0.41_pm_r,308._pm_r, &
244.50_pm_r,61174._pm_r,3._pm_r,258._pm_r,0.36_pm_r,57._pm_r,3._pm_r,322._pm_r,0.31_pm_r,266._pm_r, &
234.30_pm_r,64683._pm_r,3._pm_r,263._pm_r,0.18_pm_r,348._pm_r,3._pm_r,313._pm_r,0.40_pm_r,210._pm_r, &
223.50_pm_r,68034._pm_r,3._pm_r,268._pm_r,0.58_pm_r,281._pm_r,3._pm_r,299._pm_r,0.66_pm_r,184._pm_r, &
211.80_pm_r,71226._pm_r,5._pm_r,270._pm_r,1.08_pm_r,272._pm_r,2._pm_r,276._pm_r,0.87_pm_r,173._pm_r, &
199.30_pm_r,74233._pm_r,6._pm_r,270._pm_r,1.45_pm_r,268._pm_r,2._pm_r,242._pm_r,0.99_pm_r,167._pm_r, &
187.60_pm_r,77068._pm_r,9._pm_r,269._pm_r,1.64_pm_r,267._pm_r,3._pm_r,215._pm_r,1.03_pm_r,164._pm_r, &
178.40_pm_r,79740._pm_r,11._pm_r,269._pm_r,1.70_pm_r,267._pm_r,4._pm_r,198._pm_r,1.00_pm_r,162._pm_r, &
169.90_pm_r,82288._pm_r,14._pm_r,268._pm_r,1.60_pm_r,266._pm_r,5._pm_r,189._pm_r,0.91_pm_r,160._pm_r, &
162.40_pm_r,84705._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
158.10_pm_r,87031._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
156.90_pm_r,89327._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
160.20_pm_r,91654._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
168.10_pm_r,94073._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.20_pm_r,96660._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.80_pm_r,99518._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.00_pm_r,102780._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
266.60_pm_r,106603._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
306.00_pm_r,111093._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
378.30_pm_r,116508._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
279.50_pm_r,-40._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
261.50_pm_r,3911._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
236.40_pm_r,7546._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.90_pm_r,10922._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.70_pm_r,14227._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.40_pm_r,17554._pm_r,3._pm_r,176._pm_r,0.43_pm_r,318._pm_r,1._pm_r,50._pm_r,0.05_pm_r,8._pm_r, &
227.80_pm_r,20887._pm_r,2._pm_r,187._pm_r,0.57_pm_r,317._pm_r,1._pm_r,45._pm_r,0.08_pm_r,0._pm_r, &
227.60_pm_r,24218._pm_r,2._pm_r,207._pm_r,0.61_pm_r,315._pm_r,1._pm_r,37._pm_r,0.09_pm_r,344._pm_r, &
230.90_pm_r,27569._pm_r,2._pm_r,233._pm_r,0.54_pm_r,313._pm_r,1._pm_r,29._pm_r,0.10_pm_r,327._pm_r, &
238.30_pm_r,31004._pm_r,2._pm_r,251._pm_r,0.37_pm_r,310._pm_r,1._pm_r,19._pm_r,0.11_pm_r,308._pm_r, &
247.70_pm_r,34558._pm_r,2._pm_r,259._pm_r,0.19_pm_r,299._pm_r,1._pm_r,9._pm_r,0.13_pm_r,296._pm_r, &
257.50_pm_r,38261._pm_r,3._pm_r,261._pm_r,0.07_pm_r,258._pm_r,1._pm_r,358._pm_r,0.13_pm_r,288._pm_r, &
266.90_pm_r,42099._pm_r,3._pm_r,260._pm_r,0.06_pm_r,195._pm_r,1._pm_r,348._pm_r,0.13_pm_r,283._pm_r, &
272.80_pm_r,46059._pm_r,3._pm_r,258._pm_r,0.02_pm_r,212._pm_r,1._pm_r,340._pm_r,0.13_pm_r,289._pm_r, &
272.20_pm_r,50056._pm_r,3._pm_r,260._pm_r,0.12_pm_r,333._pm_r,1._pm_r,333._pm_r,0.28_pm_r,308._pm_r, &
266.30_pm_r,54002._pm_r,3._pm_r,266._pm_r,0.27_pm_r,340._pm_r,2._pm_r,326._pm_r,0.36_pm_r,306._pm_r, &
258.70_pm_r,57849._pm_r,3._pm_r,274._pm_r,0.31_pm_r,345._pm_r,2._pm_r,321._pm_r,0.28_pm_r,281._pm_r, &
249.70_pm_r,61571._pm_r,3._pm_r,281._pm_r,0.19_pm_r,344._pm_r,2._pm_r,313._pm_r,0.24_pm_r,213._pm_r, &
238.40_pm_r,65152._pm_r,3._pm_r,283._pm_r,0.01_pm_r,135._pm_r,2._pm_r,303._pm_r,0.34_pm_r,185._pm_r, &
224.90_pm_r,68542._pm_r,3._pm_r,280._pm_r,0.24_pm_r,166._pm_r,2._pm_r,291._pm_r,0.34_pm_r,179._pm_r, &
210.90_pm_r,71737._pm_r,3._pm_r,271._pm_r,0.43_pm_r,169._pm_r,2._pm_r,277._pm_r,0.28_pm_r,180._pm_r, &
197.50_pm_r,74723._pm_r,3._pm_r,256._pm_r,0.55_pm_r,169._pm_r,2._pm_r,267._pm_r,0.19_pm_r,187._pm_r, &
184.90_pm_r,77526._pm_r,3._pm_r,240._pm_r,0.59_pm_r,171._pm_r,2._pm_r,260._pm_r,0.12_pm_r,201._pm_r, &
174.70_pm_r,80149._pm_r,3._pm_r,227._pm_r,0.57_pm_r,171._pm_r,2._pm_r,257._pm_r,0.09_pm_r,223._pm_r, &
164.40_pm_r,82636._pm_r,4._pm_r,217._pm_r,0.52_pm_r,171._pm_r,2._pm_r,256._pm_r,0.07_pm_r,249._pm_r, &
155.00_pm_r,84940._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
150.60_pm_r,87145._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
150.60_pm_r,89340._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
155.20_pm_r,91582._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
164.50_pm_r,93936._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.80_pm_r,96485._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.50_pm_r,99340._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.70_pm_r,102655._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.00_pm_r,106581._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
307.20_pm_r,111141._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
379.40_pm_r,116557._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /) 
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
273.50_pm_r,-29._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
258.70_pm_r,3855._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
234.00_pm_r,7451._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.90_pm_r,10823._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.20_pm_r,14159._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.60_pm_r,17519._pm_r,1._pm_r,212._pm_r,0.09_pm_r,74._pm_r,0._pm_r,13._pm_r,0.05_pm_r,34._pm_r, &
229.80_pm_r,20884._pm_r,1._pm_r,208._pm_r,0.10_pm_r,61._pm_r,0._pm_r,24._pm_r,0.06_pm_r,39._pm_r, &
229.30_pm_r,24243._pm_r,1._pm_r,205._pm_r,0.12_pm_r,28._pm_r,0._pm_r,33._pm_r,0.06_pm_r,63._pm_r, &
232.00_pm_r,27615._pm_r,1._pm_r,209._pm_r,0.16_pm_r,353._pm_r,0._pm_r,43._pm_r,0.05_pm_r,112._pm_r, &
238.80_pm_r,31061._pm_r,1._pm_r,230._pm_r,0.24_pm_r,332._pm_r,0._pm_r,60._pm_r,0.09_pm_r,144._pm_r, &
247.90_pm_r,34620._pm_r,1._pm_r,264._pm_r,0.29_pm_r,322._pm_r,0._pm_r,85._pm_r,0.13_pm_r,162._pm_r, &
258.10_pm_r,38328._pm_r,1._pm_r,285._pm_r,0.31_pm_r,317._pm_r,0._pm_r,113._pm_r,0.15_pm_r,169._pm_r, &
267.90_pm_r,42178._pm_r,1._pm_r,294._pm_r,0.27_pm_r,315._pm_r,1._pm_r,132._pm_r,0.15_pm_r,172._pm_r, &
274.70_pm_r,46159._pm_r,2._pm_r,295._pm_r,0.14_pm_r,278._pm_r,1._pm_r,132._pm_r,0.14_pm_r,86._pm_r, &
275.30_pm_r,50192._pm_r,2._pm_r,292._pm_r,0.21_pm_r,273._pm_r,1._pm_r,115._pm_r,0.28_pm_r,69._pm_r, &
269.70_pm_r,54186._pm_r,2._pm_r,297._pm_r,0.62_pm_r,330._pm_r,1._pm_r,106._pm_r,0.29_pm_r,109._pm_r, &
262.20_pm_r,58084._pm_r,4._pm_r,314._pm_r,1.46_pm_r,349._pm_r,2._pm_r,118._pm_r,0.56_pm_r,169._pm_r, &
253.20_pm_r,61857._pm_r,6._pm_r,329._pm_r,1.61_pm_r,358._pm_r,2._pm_r,141._pm_r,0.77_pm_r,197._pm_r, &
241.50_pm_r,65486._pm_r,7._pm_r,338._pm_r,1.09_pm_r,10._pm_r,3._pm_r,163._pm_r,0.80_pm_r,227._pm_r, &
226.70_pm_r,68912._pm_r,8._pm_r,343._pm_r,0.34_pm_r,84._pm_r,3._pm_r,183._pm_r,0.85_pm_r,264._pm_r, &
212.10_pm_r,72128._pm_r,7._pm_r,345._pm_r,1.12_pm_r,158._pm_r,3._pm_r,207._pm_r,1.07_pm_r,291._pm_r, &
198.40_pm_r,75130._pm_r,5._pm_r,346._pm_r,1.92_pm_r,167._pm_r,4._pm_r,235._pm_r,1.28_pm_r,307._pm_r, &
185.60_pm_r,77946._pm_r,2._pm_r,340._pm_r,2.37_pm_r,171._pm_r,5._pm_r,259._pm_r,1.37_pm_r,315._pm_r, &
174.90_pm_r,80576._pm_r,2._pm_r,181._pm_r,2.47_pm_r,172._pm_r,6._pm_r,276._pm_r,1.33_pm_r,319._pm_r, &
162.90_pm_r,83061._pm_r,6._pm_r,175._pm_r,2.33_pm_r,173._pm_r,7._pm_r,286._pm_r,1.21_pm_r,322._pm_r, &
150.80_pm_r,85301._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
145.70_pm_r,87429._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
146.60_pm_r,89557._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
152.00_pm_r,91745._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
162.10_pm_r,94058._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
178.80_pm_r,96580._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.60_pm_r,99434._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
239.60_pm_r,102786._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
278.80_pm_r,106782._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
307.30_pm_r,111383._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
380.00_pm_r,116792._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)


  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_aout = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_aout,(/10,nb_alt,nb_lat/))


  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)

end subroutine cps_atmi_aout

subroutine cps_atmi_septembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_septembre
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de SEPTEMBRE
!
!$Description
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de SEPTEMBRE
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_septembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments            
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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

 
  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
265.50_pm_r,  -422._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
240.20_pm_r,  3269._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
218.40_pm_r,  6616._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.80_pm_r,  9691._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
196.10_pm_r, 12602._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
193.60_pm_r, 15452._pm_r,  13._pm_r,270._pm_r, 2.66_pm_r,169._pm_r,   2._pm_r,232._pm_r,  0.24_pm_r, 24._pm_r, & 
194.40_pm_r, 18289._pm_r,  12._pm_r,246._pm_r, 4.79_pm_r,143._pm_r,   1._pm_r,242._pm_r,  0.44_pm_r, 28._pm_r, & 
197.70_pm_r, 21162._pm_r,  10._pm_r,199._pm_r, 7.04_pm_r,107._pm_r,   1._pm_r,274._pm_r,  0.67_pm_r, 37._pm_r, & 
198.50_pm_r, 24053._pm_r,  12._pm_r,144._pm_r, 7.28_pm_r, 81._pm_r,   1._pm_r,345._pm_r,  0.49_pm_r, 32._pm_r, & 
215.90_pm_r, 27077._pm_r,  19._pm_r,112._pm_r, 7.34_pm_r, 67._pm_r,   1._pm_r,  2._pm_r,  0.28_pm_r, 21._pm_r, & 
240.60_pm_r, 30414._pm_r,  27._pm_r, 94._pm_r, 7.27_pm_r, 57._pm_r,   2._pm_r,  4._pm_r,  0.16_pm_r,349._pm_r, & 
259.50_pm_r, 34089._pm_r,  35._pm_r, 83._pm_r, 6.73_pm_r, 49._pm_r,   2._pm_r,359._pm_r,  0.17_pm_r,296._pm_r, & 
271.50_pm_r, 37980._pm_r,  43._pm_r, 76._pm_r, 5.55_pm_r, 40._pm_r,   2._pm_r,349._pm_r,  0.29_pm_r,272._pm_r, & 
275.30_pm_r, 41987._pm_r,  48._pm_r, 70._pm_r, 3.60_pm_r, 26._pm_r,   2._pm_r,335._pm_r,  0.35_pm_r,282._pm_r, & 
277.90_pm_r, 46038._pm_r,  50._pm_r, 66._pm_r, 2.86_pm_r,348._pm_r,   2._pm_r,327._pm_r,  0.31_pm_r,299._pm_r, & 
277.80_pm_r, 50113._pm_r,  50._pm_r, 61._pm_r, 3.93_pm_r,317._pm_r,   3._pm_r,325._pm_r,  0.15_pm_r,335._pm_r, & 
272.60_pm_r, 54150._pm_r,  48._pm_r, 53._pm_r, 5.21_pm_r,306._pm_r,   3._pm_r,327._pm_r,  0.15_pm_r,106._pm_r, & 
258.70_pm_r, 58038._pm_r,  45._pm_r, 44._pm_r, 5.51_pm_r,291._pm_r,   3._pm_r,328._pm_r,  0.34_pm_r,159._pm_r, & 
244.90_pm_r, 61728._pm_r,  42._pm_r, 34._pm_r, 5.34_pm_r,279._pm_r,   2._pm_r,322._pm_r,  0.51_pm_r,170._pm_r, & 
234.80_pm_r, 65233._pm_r,  39._pm_r, 25._pm_r, 4.73_pm_r,269._pm_r,   1._pm_r,304._pm_r,  0.57_pm_r,172._pm_r, & 
227.80_pm_r, 68621._pm_r,  36._pm_r, 16._pm_r, 3.95_pm_r,258._pm_r,   1._pm_r,263._pm_r,  0.55_pm_r,171._pm_r, & 
222.20_pm_r, 71914._pm_r,  33._pm_r,  9._pm_r, 3.23_pm_r,246._pm_r,   1._pm_r,222._pm_r,  0.49_pm_r,165._pm_r, & 
217.70_pm_r, 75135._pm_r,  31._pm_r,  3._pm_r, 2.66_pm_r,233._pm_r,   2._pm_r,201._pm_r,  0.43_pm_r,158._pm_r, & 
213.80_pm_r, 78295._pm_r,  28._pm_r,358._pm_r, 2.26_pm_r,221._pm_r,   2._pm_r,189._pm_r,  0.37_pm_r,152._pm_r, & 
206.30_pm_r, 81399._pm_r,  26._pm_r,353._pm_r, 1.94_pm_r,210._pm_r,   2._pm_r,181._pm_r,  0.32_pm_r,145._pm_r, & 
194.20_pm_r, 84314._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
183.30_pm_r, 87053._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
176.40_pm_r, 89671._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
175.20_pm_r, 92246._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
179.00_pm_r, 94853._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
188.10_pm_r, 97570._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
203.20_pm_r,100494._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.70_pm_r,103738._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
254.90_pm_r,107432._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
290.80_pm_r,111698._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
367.90_pm_r,116894._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
270.00_pm_r,  -230._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
243.60_pm_r,  3520._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
220.70_pm_r,  6910._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
205.10_pm_r, 10020._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
199.90_pm_r, 12977._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
198.60_pm_r, 15891._pm_r,  33._pm_r,233._pm_r, 4.92_pm_r,124._pm_r,   8._pm_r,307._pm_r,  0.74_pm_r,104._pm_r, & 
199.70_pm_r, 18806._pm_r,  30._pm_r,217._pm_r, 8.27_pm_r,111._pm_r,   6._pm_r,313._pm_r, 1.18_pm_r,100._pm_r, & 
202.30_pm_r, 21748._pm_r,  27._pm_r,188._pm_r,10.76_pm_r, 94._pm_r,   5._pm_r,326._pm_r, 1.28_pm_r, 92._pm_r, & 
207.10_pm_r, 24736._pm_r,  29._pm_r,155._pm_r,10.75_pm_r, 78._pm_r,   4._pm_r,345._pm_r,  0.90_pm_r, 79._pm_r, & 
222.70_pm_r, 27878._pm_r,  33._pm_r,128._pm_r,10.02_pm_r, 62._pm_r,   5._pm_r,357._pm_r,  0.49_pm_r, 51._pm_r, & 
242.20_pm_r, 31278._pm_r,  40._pm_r,109._pm_r, 9.18_pm_r, 47._pm_r,   5._pm_r,360._pm_r,  0.40_pm_r,351._pm_r, & 
257.40_pm_r, 34947._pm_r,  46._pm_r, 94._pm_r, 8.01_pm_r, 32._pm_r,   6._pm_r,356._pm_r,  0.59_pm_r,314._pm_r, & 
267.20_pm_r, 38791._pm_r,  50._pm_r, 82._pm_r, 6.40_pm_r, 13._pm_r,   6._pm_r,349._pm_r,  0.81_pm_r,299._pm_r, & 
270.20_pm_r, 42731._pm_r,  52._pm_r, 74._pm_r, 4.50_pm_r,347._pm_r,   7._pm_r,341._pm_r,  0.84_pm_r,295._pm_r, & 
271.10_pm_r, 46697._pm_r,  51._pm_r, 67._pm_r, 4.81_pm_r,317._pm_r,   8._pm_r,335._pm_r,  0.82_pm_r,288._pm_r, & 
268.30_pm_r, 50651._pm_r,  48._pm_r, 58._pm_r, 6.33_pm_r,308._pm_r,   9._pm_r,329._pm_r,  0.68_pm_r,272._pm_r, & 
261.70_pm_r, 54537._pm_r,  46._pm_r, 46._pm_r, 7.56_pm_r,312._pm_r,   9._pm_r,324._pm_r,  0.50_pm_r,233._pm_r, & 
249.90_pm_r, 58281._pm_r,  45._pm_r, 33._pm_r, 7.10_pm_r,303._pm_r,   9._pm_r,319._pm_r,  0.58_pm_r,197._pm_r, & 
239.20_pm_r, 61863._pm_r,  46._pm_r, 20._pm_r, 5.97_pm_r,294._pm_r,   8._pm_r,314._pm_r,  0.69_pm_r,180._pm_r, & 
232.00_pm_r, 65310._pm_r,  46._pm_r, 11._pm_r, 4.63_pm_r,281._pm_r,   8._pm_r,309._pm_r,  0.68_pm_r,173._pm_r, & 
227.10_pm_r, 68671._pm_r,  46._pm_r,  3._pm_r, 3.53_pm_r,263._pm_r,   7._pm_r,304._pm_r,  0.60_pm_r,171._pm_r, & 
222.90_pm_r, 71965._pm_r,  44._pm_r,358._pm_r, 2.94_pm_r,241._pm_r,   6._pm_r,298._pm_r,  0.49_pm_r,172._pm_r, & 
219.00_pm_r, 75202._pm_r,  42._pm_r,353._pm_r, 2.79_pm_r,221._pm_r,   6._pm_r,293._pm_r,  0.38_pm_r,172._pm_r, & 
214.60_pm_r, 78379._pm_r,  39._pm_r,350._pm_r, 2.79_pm_r,206._pm_r,   6._pm_r,289._pm_r,  0.28_pm_r,173._pm_r, & 
206.90_pm_r, 81489._pm_r,  36._pm_r,346._pm_r, 2.72_pm_r,197._pm_r,   6._pm_r,286._pm_r,  0.20_pm_r,174._pm_r, & 
195.10_pm_r, 84418._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
184.50_pm_r, 87175._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
177.40_pm_r, 89810._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
175.90_pm_r, 92399._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
179.30_pm_r, 95014._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.50_pm_r, 97729._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.60_pm_r,100639._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
223.00_pm_r,103851._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
252.60_pm_r,107506._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
291.80_pm_r,111762._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
368.60_pm_r,116985._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
274.50_pm_r,   -63._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
249.30_pm_r,  3764._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.00_pm_r,  7230._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
210.30_pm_r, 10411._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
207.40_pm_r, 13463._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
207.20_pm_r, 16498._pm_r,  45._pm_r,220._pm_r, 7.27_pm_r, 94._pm_r,  13._pm_r,315._pm_r, 1.23_pm_r,121._pm_r, & 
208.10_pm_r, 19537._pm_r,  39._pm_r,204._pm_r,10.43_pm_r, 89._pm_r,  11._pm_r,318._pm_r, 1.60_pm_r,121._pm_r, & 
210.50_pm_r, 22601._pm_r,  34._pm_r,179._pm_r,12.19_pm_r, 82._pm_r,   8._pm_r,323._pm_r, 1.54_pm_r,121._pm_r, & 
215.70_pm_r, 25716._pm_r,  35._pm_r,149._pm_r,11.57_pm_r, 72._pm_r,   7._pm_r,329._pm_r, 1.01_pm_r,122._pm_r, & 
225.50_pm_r, 28946._pm_r,  40._pm_r,127._pm_r, 9.36_pm_r, 56._pm_r,   6._pm_r,333._pm_r,  0.27_pm_r,130._pm_r, & 
237.70_pm_r, 32334._pm_r,  43._pm_r,111._pm_r, 7.21_pm_r, 32._pm_r,   6._pm_r,331._pm_r,  0.43_pm_r,292._pm_r, & 
249.10_pm_r, 35904._pm_r,  43._pm_r, 98._pm_r, 6.04_pm_r,360._pm_r,   7._pm_r,326._pm_r,  0.91_pm_r,295._pm_r, & 
258.30_pm_r, 39620._pm_r,  41._pm_r, 87._pm_r, 5.86_pm_r,328._pm_r,   8._pm_r,320._pm_r, 1.16_pm_r,293._pm_r, & 
262.80_pm_r, 43442._pm_r,  36._pm_r, 77._pm_r, 5.30_pm_r,309._pm_r,   9._pm_r,316._pm_r,  0.95_pm_r,290._pm_r, & 
263.40_pm_r, 47300._pm_r,  31._pm_r, 67._pm_r, 5.53_pm_r,301._pm_r,  10._pm_r,312._pm_r,  0.77_pm_r,271._pm_r, & 
258.10_pm_r, 51124._pm_r,  27._pm_r, 52._pm_r, 6.41_pm_r,302._pm_r,  11._pm_r,308._pm_r,  0.70_pm_r,243._pm_r, & 
250.40_pm_r, 54850._pm_r,  26._pm_r, 30._pm_r, 7.50_pm_r,309._pm_r,  11._pm_r,303._pm_r,  0.72_pm_r,220._pm_r, & 
242.00_pm_r, 58453._pm_r,  29._pm_r,  9._pm_r, 6.77_pm_r,302._pm_r,  11._pm_r,298._pm_r,  0.59_pm_r,199._pm_r, & 
235.80_pm_r, 61951._pm_r,  33._pm_r,354._pm_r, 5.39_pm_r,294._pm_r,  11._pm_r,294._pm_r,  0.48_pm_r,184._pm_r, & 
231.60_pm_r, 65370._pm_r,  37._pm_r,344._pm_r, 3.86_pm_r,283._pm_r,  11._pm_r,291._pm_r,  0.35_pm_r,175._pm_r, & 
228.60_pm_r, 68741._pm_r,  39._pm_r,338._pm_r, 2.64_pm_r,263._pm_r,  11._pm_r,289._pm_r,  0.23_pm_r,178._pm_r, & 
225.50_pm_r, 72067._pm_r,  39._pm_r,333._pm_r, 2.08_pm_r,233._pm_r,  11._pm_r,288._pm_r,  0.12_pm_r,204._pm_r, & 
221.60_pm_r, 75344._pm_r,  38._pm_r,329._pm_r, 2.08_pm_r,206._pm_r,  11._pm_r,287._pm_r,  0.11_pm_r,260._pm_r, & 
215.50_pm_r, 78547._pm_r,  36._pm_r,325._pm_r, 2.25_pm_r,189._pm_r,  11._pm_r,287._pm_r,  0.14_pm_r,290._pm_r, & 
207.20_pm_r, 81659._pm_r,  33._pm_r,321._pm_r, 2.27_pm_r,180._pm_r,  11._pm_r,287._pm_r,  0.17_pm_r,303._pm_r, & 
196.10_pm_r, 84601._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.00_pm_r, 87382._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
179.00_pm_r, 90042._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
177.10_pm_r, 92654._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
179.80_pm_r, 95285._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.80_pm_r, 98002._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
199.30_pm_r,100892._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
218.90_pm_r,104058._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
248.60_pm_r,107650._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
292.40_pm_r,111877._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
369.70_pm_r,117131._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
279.00_pm_r,   -51._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
255.60_pm_r,  3859._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
230.50_pm_r,  7414._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
216.80_pm_r, 10686._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
215.60_pm_r, 13848._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
215.90_pm_r, 17009._pm_r,  33._pm_r,212._pm_r, 5.91_pm_r, 84._pm_r,   7._pm_r,312._pm_r,  0.55_pm_r,129._pm_r, & 
216.90_pm_r, 20175._pm_r,  28._pm_r,196._pm_r, 7.95_pm_r, 80._pm_r,   6._pm_r,313._pm_r,  0.69_pm_r,129._pm_r, & 
219.70_pm_r, 23373._pm_r,  25._pm_r,169._pm_r, 8.87_pm_r, 74._pm_r,   5._pm_r,313._pm_r,  0.68_pm_r,133._pm_r, & 
222.10_pm_r, 26607._pm_r,  26._pm_r,140._pm_r, 8.31_pm_r, 65._pm_r,   4._pm_r,312._pm_r,  0.50_pm_r,140._pm_r, & 
225.10_pm_r, 29879._pm_r,  29._pm_r,118._pm_r, 6.61_pm_r, 48._pm_r,   4._pm_r,311._pm_r,  0.20_pm_r,162._pm_r, & 
232.30_pm_r, 33223._pm_r,  31._pm_r,103._pm_r, 4.91_pm_r, 16._pm_r,   4._pm_r,308._pm_r,  0.20_pm_r,283._pm_r, & 
241.40_pm_r, 36694._pm_r,  30._pm_r, 91._pm_r, 4.65_pm_r,336._pm_r,   4._pm_r,307._pm_r,  0.44_pm_r,306._pm_r, & 
250.60_pm_r, 40295._pm_r,  26._pm_r, 79._pm_r, 5.22_pm_r,308._pm_r,   5._pm_r,308._pm_r,  0.60_pm_r,316._pm_r, & 
257.50_pm_r, 44024._pm_r,  21._pm_r, 65._pm_r, 4.53_pm_r,298._pm_r,   6._pm_r,310._pm_r,  0.46_pm_r,325._pm_r, & 
258.60_pm_r, 47811._pm_r,  18._pm_r, 50._pm_r, 4.08_pm_r,297._pm_r,   6._pm_r,311._pm_r,  0.16_pm_r,311._pm_r, & 
253.50_pm_r, 51565._pm_r,  17._pm_r, 30._pm_r, 4.29_pm_r,301._pm_r,   6._pm_r,309._pm_r,  0.30_pm_r,191._pm_r, & 
245.40_pm_r, 55222._pm_r,  19._pm_r,  9._pm_r, 5.00_pm_r,305._pm_r,   6._pm_r,303._pm_r,  0.68_pm_r,193._pm_r, & 
238.20_pm_r, 58758._pm_r,  22._pm_r,352._pm_r, 4.51_pm_r,297._pm_r,   6._pm_r,294._pm_r,  0.64_pm_r,204._pm_r, & 
234.10_pm_r, 62216._pm_r,  26._pm_r,341._pm_r, 3.62_pm_r,288._pm_r,   6._pm_r,286._pm_r,  0.56_pm_r,219._pm_r, & 
230.60_pm_r, 65619._pm_r,  29._pm_r,333._pm_r, 2.62_pm_r,276._pm_r,   7._pm_r,280._pm_r,  0.48_pm_r,238._pm_r, & 
227.30_pm_r, 68972._pm_r,  30._pm_r,327._pm_r, 1.78_pm_r,259._pm_r,   7._pm_r,277._pm_r,  0.45_pm_r,259._pm_r, & 
224.30_pm_r, 72279._pm_r,  30._pm_r,323._pm_r, 1.31_pm_r,231._pm_r,   8._pm_r,276._pm_r,  0.45_pm_r,276._pm_r, & 
219.10_pm_r, 75531._pm_r,  30._pm_r,320._pm_r, 1.22_pm_r,202._pm_r,   8._pm_r,277._pm_r,  0.46_pm_r,288._pm_r, & 
212.60_pm_r, 78688._pm_r,  29._pm_r,317._pm_r, 1.27_pm_r,183._pm_r,   9._pm_r,278._pm_r,  0.47_pm_r,296._pm_r, & 
205.40_pm_r, 81756._pm_r,  28._pm_r,315._pm_r, 1.29_pm_r,172._pm_r,  10._pm_r,279._pm_r,  0.44_pm_r,301._pm_r, & 
196.40_pm_r, 84694._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.60_pm_r, 87495._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
180.90_pm_r, 90185._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
179.00_pm_r, 92828._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
180.80_pm_r, 95482._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.20_pm_r, 98206._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
196.90_pm_r,101077._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
214.00_pm_r,104190._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
243.00_pm_r,107701._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
291.60_pm_r,111875._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
370.70_pm_r,117151._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
285.40_pm_r,   -38._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
262.10_pm_r,  3969._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
236.40_pm_r,  7618._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
220.80_pm_r, 10965._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
217.00_pm_r, 14170._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
216.90_pm_r, 17345._pm_r,  17._pm_r,205._pm_r, 3.13_pm_r, 83._pm_r,   3._pm_r,347._pm_r,  0.16_pm_r,166._pm_r, & 
218.70_pm_r, 20532._pm_r,  15._pm_r,188._pm_r, 4.03_pm_r, 78._pm_r,   3._pm_r,346._pm_r,  0.22_pm_r,175._pm_r, & 
222.10_pm_r, 23763._pm_r,  13._pm_r,163._pm_r, 4.28_pm_r, 69._pm_r,   2._pm_r,344._pm_r,  0.25_pm_r,186._pm_r, & 
224.20_pm_r, 27030._pm_r,  14._pm_r,137._pm_r, 3.88_pm_r, 54._pm_r,   2._pm_r,339._pm_r,  0.24_pm_r,199._pm_r, & 
226.10_pm_r, 30326._pm_r,  14._pm_r,115._pm_r, 3.28_pm_r, 27._pm_r,   2._pm_r,331._pm_r,  0.21_pm_r,221._pm_r, & 
232.50_pm_r, 33676._pm_r,  14._pm_r, 97._pm_r, 3.20_pm_r,349._pm_r,   2._pm_r,324._pm_r,  0.15_pm_r,266._pm_r, & 
241.20_pm_r, 37147._pm_r,  12._pm_r, 76._pm_r, 3.66_pm_r,320._pm_r,   2._pm_r,321._pm_r,  0.19_pm_r,325._pm_r, & 
250.60_pm_r, 40745._pm_r,  10._pm_r, 47._pm_r, 3.93_pm_r,301._pm_r,   2._pm_r,324._pm_r,  0.32_pm_r,356._pm_r, & 
258.80_pm_r, 44481._pm_r,   9._pm_r, 15._pm_r, 3.08_pm_r,297._pm_r,   3._pm_r,332._pm_r,  0.37_pm_r, 26._pm_r, & 
260.40_pm_r, 48291._pm_r,  11._pm_r,354._pm_r, 2.40_pm_r,300._pm_r,   3._pm_r,342._pm_r,  0.35_pm_r, 72._pm_r, & 
254.50_pm_r, 52066._pm_r,  13._pm_r,343._pm_r, 2.11_pm_r,304._pm_r,   3._pm_r,352._pm_r,  0.43_pm_r,125._pm_r, & 
244.80_pm_r, 55727._pm_r,  16._pm_r,336._pm_r, 2.10_pm_r,306._pm_r,   2._pm_r,  0._pm_r,  0.64_pm_r,166._pm_r, & 
236.60_pm_r, 59246._pm_r,  18._pm_r,330._pm_r, 1.68_pm_r,286._pm_r,   1._pm_r,  3._pm_r,  0.56_pm_r,188._pm_r, & 
231.60_pm_r, 62673._pm_r,  19._pm_r,325._pm_r, 1.41_pm_r,265._pm_r,   1._pm_r,339._pm_r,  0.50_pm_r,210._pm_r, & 
227.70_pm_r, 66035._pm_r,  20._pm_r,320._pm_r, 1.23_pm_r,249._pm_r,   1._pm_r,266._pm_r,  0.45_pm_r,228._pm_r, & 
224.00_pm_r, 69343._pm_r,  21._pm_r,315._pm_r, 1.08_pm_r,239._pm_r,   1._pm_r,251._pm_r,  0.41_pm_r,242._pm_r, & 
220.20_pm_r, 72596._pm_r,  21._pm_r,311._pm_r,  0.92_pm_r,232._pm_r,   2._pm_r,250._pm_r,  0.36_pm_r,252._pm_r, & 
215.30_pm_r, 75788._pm_r,  21._pm_r,308._pm_r,  0.77_pm_r,228._pm_r,   2._pm_r,251._pm_r,  0.30_pm_r,261._pm_r, & 
210.00_pm_r, 78900._pm_r,  21._pm_r,305._pm_r,  0.65_pm_r,224._pm_r,   3._pm_r,253._pm_r,  0.28_pm_r,268._pm_r, & 
204.20_pm_r, 81940._pm_r,  21._pm_r,303._pm_r,  0.53_pm_r,223._pm_r,   3._pm_r,255._pm_r,  0.24_pm_r,275._pm_r, & 
196.60_pm_r, 84875._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
188.90_pm_r, 87692._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
182.90_pm_r, 90409._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
181.20_pm_r, 93086._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
182.20_pm_r, 95769._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.10_pm_r, 98507._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
194.60_pm_r,101361._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
209.10_pm_r,104423._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
236.80_pm_r,107849._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
289.20_pm_r,111951._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
371.40_pm_r,117234._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
290.90_pm_r,   -24._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
268.70_pm_r,  4076._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
243.50_pm_r,  7829._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
223.00_pm_r, 11246._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
211.40_pm_r, 14429._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
210.70_pm_r, 17507._pm_r,   7._pm_r,194._pm_r, 1.28_pm_r, 82._pm_r,   1._pm_r, 15._pm_r,  0.05_pm_r,171._pm_r, & 
215.70_pm_r, 20629._pm_r,   7._pm_r,178._pm_r, 1.47_pm_r, 75._pm_r,   1._pm_r, 16._pm_r,  0.08_pm_r,176._pm_r, & 
220.40_pm_r, 23825._pm_r,   6._pm_r,160._pm_r, 1.28_pm_r, 59._pm_r,   1._pm_r, 19._pm_r,  0.12_pm_r,178._pm_r, & 
224.70_pm_r, 27082._pm_r,   6._pm_r,146._pm_r,  0.98_pm_r, 21._pm_r,   1._pm_r, 24._pm_r,  0.15_pm_r,176._pm_r, & 
229.20_pm_r, 30405._pm_r,   5._pm_r,137._pm_r, 1.29_pm_r,329._pm_r,   1._pm_r, 33._pm_r,  0.15_pm_r,172._pm_r, & 
236.50_pm_r, 33810._pm_r,   2._pm_r,142._pm_r, 2.02_pm_r,303._pm_r,   1._pm_r, 47._pm_r,  0.13_pm_r,164._pm_r, & 
245.20_pm_r, 37338._pm_r,   2._pm_r,263._pm_r, 2.52_pm_r,290._pm_r,   1._pm_r, 63._pm_r,  0.08_pm_r,128._pm_r, & 
255.20_pm_r, 40998._pm_r,   6._pm_r,278._pm_r, 2.62_pm_r,279._pm_r,   1._pm_r, 68._pm_r,  0.11_pm_r, 66._pm_r, & 
263.40_pm_r, 44804._pm_r,   9._pm_r,278._pm_r, 1.86_pm_r,281._pm_r,   1._pm_r, 64._pm_r,  0.16_pm_r, 48._pm_r, & 
264.30_pm_r, 48676._pm_r,  11._pm_r,280._pm_r, 1.14_pm_r,293._pm_r,   1._pm_r, 61._pm_r,  0.19_pm_r, 50._pm_r, & 
257.80_pm_r, 52503._pm_r,  12._pm_r,282._pm_r,  0.62_pm_r,312._pm_r,   1._pm_r, 60._pm_r,  0.18_pm_r, 66._pm_r, & 
246.70_pm_r, 56205._pm_r,  13._pm_r,284._pm_r,  0.25_pm_r,311._pm_r,   2._pm_r, 63._pm_r,  0.15_pm_r, 90._pm_r, & 
236.40_pm_r, 59737._pm_r,  13._pm_r,282._pm_r,  0.54_pm_r,197._pm_r,   2._pm_r, 66._pm_r,  0.12_pm_r, 90._pm_r, & 
229.60_pm_r, 63148._pm_r,  13._pm_r,277._pm_r,  0.91_pm_r,191._pm_r,   2._pm_r, 67._pm_r,  0.06_pm_r, 95._pm_r, & 
224.10_pm_r, 66469._pm_r,  13._pm_r,271._pm_r,  0.96_pm_r,192._pm_r,   2._pm_r, 68._pm_r,  0.03_pm_r,197._pm_r, & 
219.40_pm_r, 69716._pm_r,  13._pm_r,265._pm_r,  0.85_pm_r,197._pm_r,   2._pm_r, 69._pm_r,  0.09_pm_r,234._pm_r, & 
215.70_pm_r, 72901._pm_r,  14._pm_r,261._pm_r,  0.66_pm_r,206._pm_r,   2._pm_r, 71._pm_r,  0.15_pm_r,239._pm_r, & 
211.70_pm_r, 76032._pm_r,  15._pm_r,259._pm_r,  0.51_pm_r,219._pm_r,   1._pm_r, 73._pm_r,  0.18_pm_r,239._pm_r, & 
207.90_pm_r, 79103._pm_r,  15._pm_r,257._pm_r,  0.41_pm_r,234._pm_r,   1._pm_r, 76._pm_r,  0.20_pm_r,240._pm_r, & 
203.30_pm_r, 82123._pm_r,  16._pm_r,257._pm_r,  0.34_pm_r,249._pm_r,   1._pm_r, 82._pm_r,  0.20_pm_r,240._pm_r, & 
196.60_pm_r, 85053._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
189.80_pm_r, 87878._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
184.70_pm_r, 90618._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
183.40_pm_r, 93328._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
183.90_pm_r, 96043._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.50_pm_r, 98798._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
192.70_pm_r,101644._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
205.20_pm_r,104664._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
230.90_pm_r,108013._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
285.90_pm_r,112040._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
371.80_pm_r,117315._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
295.80_pm_r,   -26._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
274.20_pm_r,  4153._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
249.90_pm_r,  7995._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.40_pm_r, 11473._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
205.40_pm_r, 14624._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
204.40_pm_r, 17600._pm_r,   4._pm_r,176._pm_r,  0.24_pm_r, 31._pm_r,   1._pm_r, 29._pm_r,  0.11_pm_r, 41._pm_r, & 
212.60_pm_r, 20654._pm_r,   3._pm_r,173._pm_r,  0.25_pm_r,  7._pm_r,   1._pm_r, 32._pm_r,  0.15_pm_r, 48._pm_r, & 
218.40_pm_r, 23812._pm_r,   3._pm_r,175._pm_r,  0.28_pm_r,316._pm_r,   1._pm_r, 37._pm_r,  0.17_pm_r, 57._pm_r, & 
224.10_pm_r, 27049._pm_r,   3._pm_r,185._pm_r,  0.52_pm_r,281._pm_r,   1._pm_r, 42._pm_r,  0.18_pm_r, 71._pm_r, & 
230.90_pm_r, 30380._pm_r,   3._pm_r,205._pm_r,  0.85_pm_r,266._pm_r,   2._pm_r, 47._pm_r,  0.18_pm_r, 83._pm_r, & 
240.30_pm_r, 33826._pm_r,   4._pm_r,223._pm_r, 1.13_pm_r,257._pm_r,   2._pm_r, 53._pm_r,  0.16_pm_r,104._pm_r, & 
250.50_pm_r, 37422._pm_r,   6._pm_r,233._pm_r, 1.29_pm_r,251._pm_r,   2._pm_r, 59._pm_r,  0.14_pm_r,124._pm_r, & 
260.50_pm_r, 41161._pm_r,   7._pm_r,237._pm_r, 1.28_pm_r,245._pm_r,   2._pm_r, 65._pm_r,  0.13_pm_r,148._pm_r, & 
267.50_pm_r, 45036._pm_r,   9._pm_r,239._pm_r,  0.89_pm_r,251._pm_r,   2._pm_r, 69._pm_r,  0.09_pm_r,232._pm_r, & 
267.50_pm_r, 48961._pm_r,  10._pm_r,240._pm_r,  0.51_pm_r,267._pm_r,   2._pm_r, 66._pm_r,  0.27_pm_r,274._pm_r, & 
260.30_pm_r, 52829._pm_r,  10._pm_r,242._pm_r,  0.17_pm_r,291._pm_r,   1._pm_r, 56._pm_r,  0.31_pm_r,275._pm_r, & 
248.20_pm_r, 56560._pm_r,  10._pm_r,242._pm_r,  0.20_pm_r,101._pm_r,   1._pm_r, 46._pm_r,  0.14_pm_r,231._pm_r, & 
236.30_pm_r, 60101._pm_r,  10._pm_r,240._pm_r,  0.47_pm_r,129._pm_r,   1._pm_r, 61._pm_r,  0.28_pm_r,145._pm_r, & 
228.10_pm_r, 63501._pm_r,  10._pm_r,235._pm_r,  0.55_pm_r,134._pm_r,   1._pm_r, 87._pm_r,  0.37_pm_r,131._pm_r, & 
221.40_pm_r, 66790._pm_r,  10._pm_r,231._pm_r,  0.45_pm_r,136._pm_r,   2._pm_r, 99._pm_r,  0.27_pm_r,124._pm_r, & 
215.70_pm_r, 69992._pm_r,  10._pm_r,228._pm_r,  0.25_pm_r,135._pm_r,   2._pm_r,101._pm_r,  0.09_pm_r, 90._pm_r, & 
211.40_pm_r, 73116._pm_r,  10._pm_r,227._pm_r,  0.04_pm_r,120._pm_r,   2._pm_r, 98._pm_r,  0.17_pm_r,333._pm_r, & 
208.40_pm_r, 76191._pm_r,  10._pm_r,227._pm_r,  0.14_pm_r,326._pm_r,   2._pm_r, 87._pm_r,  0.34_pm_r,322._pm_r, & 
206.40_pm_r, 79228._pm_r,  10._pm_r,229._pm_r,  0.27_pm_r,322._pm_r,   1._pm_r, 63._pm_r,  0.47_pm_r,320._pm_r, & 
202.90_pm_r, 82236._pm_r,  10._pm_r,232._pm_r,  0.34_pm_r,322._pm_r,   1._pm_r, 30._pm_r,  0.52_pm_r,318._pm_r, & 
196.60_pm_r, 85163._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
190.30_pm_r, 87990._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.20_pm_r, 90747._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
185.40_pm_r, 93485._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
185.50_pm_r, 96230._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.10_pm_r, 99005._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
191.60_pm_r,101849._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.40_pm_r,104841._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.40_pm_r,108137._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
282.60_pm_r,112101._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
372.10_pm_r,117361._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
299.10_pm_r,   -46._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
276.10_pm_r,  4173._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
252.50_pm_r,  8051._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.10_pm_r, 11554._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.60_pm_r, 14691._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.50_pm_r, 17620._pm_r,   3._pm_r,178._pm_r,  0.18_pm_r,316._pm_r,   1._pm_r,  2._pm_r,  0.10_pm_r, 42._pm_r, & 
211.10_pm_r, 20643._pm_r,   2._pm_r,183._pm_r,  0.23_pm_r,306._pm_r,   1._pm_r,  9._pm_r,  0.14_pm_r, 44._pm_r, & 
217.00_pm_r, 23780._pm_r,   2._pm_r,191._pm_r,  0.23_pm_r,289._pm_r,   1._pm_r, 16._pm_r,  0.15_pm_r, 48._pm_r, & 
223.90_pm_r, 27005._pm_r,   2._pm_r,200._pm_r,  0.25_pm_r,265._pm_r,   1._pm_r, 21._pm_r,  0.15_pm_r, 55._pm_r, & 
231.10_pm_r, 30336._pm_r,   3._pm_r,207._pm_r,  0.31_pm_r,242._pm_r,   2._pm_r, 26._pm_r,  0.13_pm_r, 64._pm_r, & 
241.60_pm_r, 33791._pm_r,   3._pm_r,211._pm_r,  0.39_pm_r,226._pm_r,   2._pm_r, 30._pm_r,  0.10_pm_r, 81._pm_r, & 
252.30_pm_r, 37411._pm_r,   4._pm_r,213._pm_r,  0.46_pm_r,218._pm_r,   2._pm_r, 34._pm_r,  0.07_pm_r,117._pm_r, & 
262.20_pm_r, 41176._pm_r,   4._pm_r,213._pm_r,  0.51_pm_r,215._pm_r,   2._pm_r, 38._pm_r,  0.08_pm_r,152._pm_r, & 
268.60_pm_r, 45070._pm_r,   5._pm_r,215._pm_r,  0.54_pm_r,232._pm_r,   2._pm_r, 41._pm_r,  0.06_pm_r,187._pm_r, & 
268.70_pm_r, 49011._pm_r,   6._pm_r,218._pm_r,  0.59_pm_r,247._pm_r,   2._pm_r, 41._pm_r,  0.04_pm_r,240._pm_r, & 
262.50_pm_r, 52905._pm_r,   7._pm_r,222._pm_r,  0.41_pm_r,257._pm_r,   2._pm_r, 40._pm_r,  0.05_pm_r,235._pm_r, & 
250.90_pm_r, 56672._pm_r,   7._pm_r,224._pm_r,  0.10_pm_r, 39._pm_r,   1._pm_r, 41._pm_r,  0.15_pm_r,199._pm_r, & 
238.30_pm_r, 60248._pm_r,   6._pm_r,222._pm_r,  0.52_pm_r, 76._pm_r,   1._pm_r, 48._pm_r,  0.28_pm_r,198._pm_r, & 
228.10_pm_r, 63664._pm_r,   6._pm_r,216._pm_r,  0.77_pm_r, 79._pm_r,   1._pm_r, 68._pm_r,  0.35_pm_r,202._pm_r, & 
219.10_pm_r, 66935._pm_r,   5._pm_r,206._pm_r,  0.81_pm_r, 81._pm_r,   0._pm_r,120._pm_r,  0.35_pm_r,214._pm_r, & 
212.00_pm_r, 70091._pm_r,   4._pm_r,193._pm_r,  0.73_pm_r, 83._pm_r,   1._pm_r,176._pm_r,  0.34_pm_r,228._pm_r, & 
208.20_pm_r, 73164._pm_r,   4._pm_r,180._pm_r,  0.64_pm_r, 86._pm_r,   1._pm_r,203._pm_r,  0.32_pm_r,244._pm_r, & 
206.10_pm_r, 76198._pm_r,   4._pm_r,168._pm_r,  0.54_pm_r, 89._pm_r,   1._pm_r,219._pm_r,  0.34_pm_r,256._pm_r, & 
205.60_pm_r, 79211._pm_r,   4._pm_r,158._pm_r,  0.47_pm_r, 92._pm_r,   2._pm_r,231._pm_r,  0.36_pm_r,266._pm_r, & 
203.10_pm_r, 82219._pm_r,   5._pm_r,151._pm_r,  0.41_pm_r, 95._pm_r,   2._pm_r,239._pm_r,  0.36_pm_r,270._pm_r, & 
196.80_pm_r, 85147._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
190.60_pm_r, 87976._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.20_pm_r, 90744._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.80_pm_r, 93501._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.80_pm_r, 96267._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.70_pm_r, 99058._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
191.20_pm_r,101906._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.10_pm_r,104886._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.10_pm_r,108156._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
280.60_pm_r,112085._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
372.40_pm_r,117335._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
300.20_pm_r,  -100._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
276.40_pm_r,  4130._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
253.20_pm_r,  8015._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.60_pm_r, 11527._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.50_pm_r, 14668._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.00_pm_r, 17593._pm_r,   2._pm_r,177._pm_r,  0.16_pm_r,290._pm_r,   1._pm_r,342._pm_r,  0.10_pm_r, 35._pm_r, & 
210.40_pm_r, 20609._pm_r,   2._pm_r,185._pm_r,  0.21_pm_r,287._pm_r,   1._pm_r,357._pm_r,  0.14_pm_r, 36._pm_r, & 
216.20_pm_r, 23733._pm_r,   2._pm_r,194._pm_r,  0.23_pm_r,279._pm_r,   1._pm_r,  9._pm_r,  0.16_pm_r, 40._pm_r, & 
223.40_pm_r, 26944._pm_r,   2._pm_r,204._pm_r,  0.23_pm_r,268._pm_r,   1._pm_r, 18._pm_r,  0.15_pm_r, 42._pm_r, & 
230.80_pm_r, 30268._pm_r,   2._pm_r,211._pm_r,  0.23_pm_r,252._pm_r,   1._pm_r, 24._pm_r,  0.13_pm_r, 48._pm_r, & 
240.90_pm_r, 33718._pm_r,   2._pm_r,215._pm_r,  0.23_pm_r,232._pm_r,   1._pm_r, 29._pm_r,  0.09_pm_r, 58._pm_r, & 
251.90_pm_r, 37328._pm_r,   3._pm_r,215._pm_r,  0.24_pm_r,217._pm_r,   1._pm_r, 33._pm_r,  0.05_pm_r, 84._pm_r, & 
262.30_pm_r, 41089._pm_r,   3._pm_r,215._pm_r,  0.27_pm_r,207._pm_r,   1._pm_r, 35._pm_r,  0.04_pm_r,153._pm_r, & 
268.90_pm_r, 44982._pm_r,   4._pm_r,215._pm_r,  0.36_pm_r,235._pm_r,   1._pm_r, 39._pm_r,  0.10_pm_r,159._pm_r, & 
269.20_pm_r, 48926._pm_r,   4._pm_r,221._pm_r,  0.60_pm_r,255._pm_r,   1._pm_r, 45._pm_r,  0.17_pm_r,144._pm_r, & 
263.30_pm_r, 52827._pm_r,   5._pm_r,228._pm_r,  0.62_pm_r,264._pm_r,   1._pm_r, 53._pm_r,  0.15_pm_r,148._pm_r, & 
252.20_pm_r, 56605._pm_r,   5._pm_r,233._pm_r,  0.23_pm_r,275._pm_r,   1._pm_r, 59._pm_r,  0.11_pm_r,225._pm_r, & 
239.60_pm_r, 60198._pm_r,   5._pm_r,234._pm_r,  0.35_pm_r, 86._pm_r,   1._pm_r, 58._pm_r,  0.27_pm_r,256._pm_r, & 
228.20_pm_r, 63628._pm_r,   5._pm_r,230._pm_r,  0.70_pm_r, 90._pm_r,   0._pm_r, 54._pm_r,  0.40_pm_r,248._pm_r, & 
218.90_pm_r, 66905._pm_r,   4._pm_r,220._pm_r,  0.81_pm_r, 94._pm_r,   0._pm_r,227._pm_r,  0.46_pm_r,234._pm_r, & 
211.90_pm_r, 70067._pm_r,   3._pm_r,204._pm_r,  0.76_pm_r, 98._pm_r,   1._pm_r,220._pm_r,  0.54_pm_r,215._pm_r, & 
207.80_pm_r, 73145._pm_r,   3._pm_r,185._pm_r,  0.66_pm_r,104._pm_r,   2._pm_r,212._pm_r,  0.62_pm_r,203._pm_r, & 
205.90_pm_r, 76179._pm_r,   4._pm_r,168._pm_r,  0.57_pm_r,109._pm_r,   2._pm_r,205._pm_r,  0.73_pm_r,194._pm_r, & 
206.20_pm_r, 79194._pm_r,   4._pm_r,157._pm_r,  0.51_pm_r,116._pm_r,   3._pm_r,201._pm_r,  0.80_pm_r,190._pm_r, & 
204.00_pm_r, 82216._pm_r,   5._pm_r,150._pm_r,  0.44_pm_r,121._pm_r,   4._pm_r,197._pm_r,  0.84_pm_r,187._pm_r, & 
197.10_pm_r, 85150._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
190.80_pm_r, 87978._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.60_pm_r, 90751._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.30_pm_r, 93515._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.30_pm_r, 96290._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
188.10_pm_r, 99088._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
191.40_pm_r,101940._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.20_pm_r,104923._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.00_pm_r,108193._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
280.70_pm_r,112123._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
373.20_pm_r,117380._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
301.20_pm_r,  -115._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
277.00_pm_r,  4126._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
253.40_pm_r,  8016._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.80_pm_r, 11531._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.80_pm_r, 14675._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
201.30_pm_r, 17603._pm_r,   2._pm_r,168._pm_r,  0.15_pm_r,309._pm_r,   0._pm_r,179._pm_r,  0.09_pm_r, 19._pm_r, & 
210.70_pm_r, 20621._pm_r,   2._pm_r,173._pm_r,  0.19_pm_r,304._pm_r,   0._pm_r,138._pm_r,  0.12_pm_r, 17._pm_r, & 
216.10_pm_r, 23747._pm_r,   2._pm_r,180._pm_r,  0.18_pm_r,293._pm_r,   0._pm_r, 44._pm_r,  0.12_pm_r, 19._pm_r, & 
223.00_pm_r, 26958._pm_r,   2._pm_r,188._pm_r,  0.18_pm_r,275._pm_r,   0._pm_r, 31._pm_r,  0.11_pm_r, 22._pm_r, & 
230.90_pm_r, 30282._pm_r,   2._pm_r,195._pm_r,  0.18_pm_r,246._pm_r,   1._pm_r, 29._pm_r,  0.08_pm_r, 26._pm_r, & 
240.10_pm_r, 33727._pm_r,   2._pm_r,200._pm_r,  0.21_pm_r,222._pm_r,   1._pm_r, 29._pm_r,  0.04_pm_r, 36._pm_r, & 
250.60_pm_r, 37323._pm_r,   3._pm_r,202._pm_r,  0.24_pm_r,209._pm_r,   1._pm_r, 30._pm_r,  0.01_pm_r,101._pm_r, & 
260.80_pm_r, 41066._pm_r,   3._pm_r,202._pm_r,  0.26_pm_r,202._pm_r,   1._pm_r, 32._pm_r,  0.03_pm_r,171._pm_r, & 
267.50_pm_r, 44942._pm_r,   3._pm_r,204._pm_r,  0.31_pm_r,224._pm_r,   1._pm_r, 38._pm_r,  0.07_pm_r,169._pm_r, & 
268.10_pm_r, 48871._pm_r,   4._pm_r,209._pm_r,  0.52_pm_r,249._pm_r,   0._pm_r, 47._pm_r,  0.07_pm_r,180._pm_r, & 
262.20_pm_r, 52759._pm_r,   4._pm_r,218._pm_r,  0.70_pm_r,262._pm_r,   0._pm_r, 55._pm_r,  0.09_pm_r,218._pm_r, & 
251.10_pm_r, 56526._pm_r,   5._pm_r,226._pm_r,  0.63_pm_r,271._pm_r,   0._pm_r, 56._pm_r,  0.16_pm_r,244._pm_r, & 
239.90_pm_r, 60117._pm_r,   6._pm_r,231._pm_r,  0.22_pm_r,285._pm_r,   0._pm_r,270._pm_r,  0.18_pm_r,243._pm_r, & 
230.30_pm_r, 63561._pm_r,   6._pm_r,231._pm_r,  0.21_pm_r, 81._pm_r,   0._pm_r,245._pm_r,  0.17_pm_r,231._pm_r, & 
221.20_pm_r, 66864._pm_r,   5._pm_r,227._pm_r,  0.52_pm_r, 96._pm_r,   1._pm_r,234._pm_r,  0.17_pm_r,210._pm_r, & 
214.30_pm_r, 70052._pm_r,   5._pm_r,218._pm_r,  0.74_pm_r,104._pm_r,   1._pm_r,224._pm_r,  0.18_pm_r,187._pm_r, & 
210.00_pm_r, 73155._pm_r,   4._pm_r,203._pm_r,  0.90_pm_r,108._pm_r,   1._pm_r,212._pm_r,  0.21_pm_r,168._pm_r, & 
207.10_pm_r, 76208._pm_r,   4._pm_r,184._pm_r, 1.02_pm_r,112._pm_r,   1._pm_r,200._pm_r,  0.24_pm_r,158._pm_r, & 
206.70_pm_r, 79232._pm_r,   5._pm_r,168._pm_r, 1.08_pm_r,113._pm_r,   2._pm_r,190._pm_r,  0.27_pm_r,152._pm_r, & 
203.90_pm_r, 82258._pm_r,   6._pm_r,156._pm_r, 1.08_pm_r,115._pm_r,   2._pm_r,182._pm_r,  0.27_pm_r,149._pm_r, & 
196.90_pm_r, 85188._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
190.40_pm_r, 88012._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.10_pm_r, 90779._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.60_pm_r, 93534._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.80_pm_r, 96300._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
188.00_pm_r, 99092._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
192.00_pm_r,101948._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.60_pm_r,104946._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.20_pm_r,108243._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
283.00_pm_r,112209._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
374.30_pm_r,117491._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
300.80_pm_r,  -119._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
277.20_pm_r,  4119._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
253.20_pm_r,  8008._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
226.20_pm_r, 11522._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
204.20_pm_r, 14678._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
203.00_pm_r, 17630._pm_r,   3._pm_r,153._pm_r,  0.39_pm_r,281._pm_r,   1._pm_r,171._pm_r,  0.32_pm_r,355._pm_r, & 
212.10_pm_r, 20671._pm_r,   3._pm_r,164._pm_r,  0.49_pm_r,282._pm_r,   1._pm_r,168._pm_r,  0.42_pm_r,355._pm_r, & 
217.20_pm_r, 23815._pm_r,   2._pm_r,180._pm_r,  0.50_pm_r,280._pm_r,   0._pm_r, 80._pm_r,  0.47_pm_r,354._pm_r, & 
223.50_pm_r, 27039._pm_r,   2._pm_r,197._pm_r,  0.43_pm_r,278._pm_r,   1._pm_r,360._pm_r,  0.45_pm_r,354._pm_r, & 
230.90_pm_r, 30367._pm_r,   3._pm_r,209._pm_r,  0.30_pm_r,272._pm_r,   1._pm_r,357._pm_r,  0.39_pm_r,353._pm_r, & 
239.20_pm_r, 33807._pm_r,   3._pm_r,215._pm_r,  0.16_pm_r,252._pm_r,   2._pm_r,355._pm_r,  0.28_pm_r,352._pm_r, & 
248.80_pm_r, 37382._pm_r,   3._pm_r,215._pm_r,  0.11_pm_r,188._pm_r,   2._pm_r,355._pm_r,  0.16_pm_r,349._pm_r, & 
258.70_pm_r, 41097._pm_r,   3._pm_r,213._pm_r,  0.17_pm_r,159._pm_r,   2._pm_r,354._pm_r,  0.05_pm_r,338._pm_r, & 
266.00_pm_r, 44946._pm_r,   3._pm_r,209._pm_r,  0.18_pm_r,177._pm_r,   2._pm_r,355._pm_r,  0.09_pm_r,126._pm_r, & 
267.00_pm_r, 48857._pm_r,   4._pm_r,208._pm_r,  0.21_pm_r,223._pm_r,   2._pm_r,359._pm_r,  0.14_pm_r,120._pm_r, & 
259.10_pm_r, 52714._pm_r,   4._pm_r,212._pm_r,  0.36_pm_r,263._pm_r,   2._pm_r,  2._pm_r,  0.03_pm_r,117._pm_r, & 
249.40_pm_r, 56439._pm_r,   4._pm_r,220._pm_r,  0.56_pm_r,277._pm_r,   2._pm_r,  0._pm_r,  0.20_pm_r,307._pm_r, & 
241.10_pm_r, 60029._pm_r,   5._pm_r,229._pm_r,  0.52_pm_r,278._pm_r,   2._pm_r,352._pm_r,  0.31_pm_r,304._pm_r, & 
232.10_pm_r, 63498._pm_r,   5._pm_r,234._pm_r,  0.35_pm_r,267._pm_r,   3._pm_r,345._pm_r,  0.26_pm_r,307._pm_r, & 
223.50_pm_r, 66830._pm_r,   6._pm_r,235._pm_r,  0.18_pm_r,218._pm_r,   3._pm_r,343._pm_r,  0.10_pm_r,342._pm_r, & 
216.40_pm_r, 70051._pm_r,   6._pm_r,232._pm_r,  0.33_pm_r,155._pm_r,   3._pm_r,345._pm_r,  0.18_pm_r, 90._pm_r, & 
211.40_pm_r, 73180._pm_r,   6._pm_r,226._pm_r,  0.56_pm_r,140._pm_r,   3._pm_r,354._pm_r,  0.41_pm_r,103._pm_r, & 
207.80_pm_r, 76249._pm_r,   6._pm_r,216._pm_r,  0.74_pm_r,136._pm_r,   3._pm_r,  8._pm_r,  0.59_pm_r,107._pm_r, & 
206.20_pm_r, 79275._pm_r,   6._pm_r,205._pm_r,  0.85_pm_r,134._pm_r,   3._pm_r, 29._pm_r,  0.71_pm_r,107._pm_r, & 
202.60_pm_r, 82285._pm_r,   7._pm_r,195._pm_r,  0.89_pm_r,133._pm_r,   3._pm_r, 49._pm_r,  0.76_pm_r,108._pm_r, & 
195.70_pm_r, 85198._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
189.20_pm_r, 88007._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
185.50_pm_r, 90752._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
184.70_pm_r, 93479._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
185.20_pm_r, 96216._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
187.40_pm_r, 98990._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
193.10_pm_r,101847._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
205.20_pm_r,104872._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
230.60_pm_r,108221._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
287.10_pm_r,112255._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
375.80_pm_r,117576._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
299.00_pm_r,  -111._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
276.00_pm_r,  4102._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
251.30_pm_r,  7965._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.60_pm_r, 11459._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
207.20_pm_r, 14630._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
206.70_pm_r, 17636._pm_r,   4._pm_r,165._pm_r,  0.47_pm_r,294._pm_r,   1._pm_r,187._pm_r,  0.35_pm_r,  3._pm_r, & 
214.50_pm_r, 20721._pm_r,   4._pm_r,175._pm_r,  0.61_pm_r,294._pm_r,   0._pm_r,195._pm_r,  0.47_pm_r,  1._pm_r, & 
218.70_pm_r, 23893._pm_r,   3._pm_r,189._pm_r,  0.65_pm_r,291._pm_r,   1._pm_r,351._pm_r,  0.54_pm_r,  1._pm_r, & 
223.70_pm_r, 27129._pm_r,   3._pm_r,205._pm_r,  0.61_pm_r,289._pm_r,   1._pm_r,357._pm_r,  0.56_pm_r,359._pm_r, & 
230.00_pm_r, 30452._pm_r,   4._pm_r,218._pm_r,  0.47_pm_r,284._pm_r,   2._pm_r,357._pm_r,  0.50_pm_r,357._pm_r, & 
238.10_pm_r, 33876._pm_r,   4._pm_r,225._pm_r,  0.29_pm_r,273._pm_r,   3._pm_r,357._pm_r,  0.39_pm_r,356._pm_r, & 
247.20_pm_r, 37432._pm_r,   4._pm_r,228._pm_r,  0.15_pm_r,247._pm_r,   3._pm_r,357._pm_r,  0.26_pm_r,353._pm_r, & 
256.30_pm_r, 41116._pm_r,   4._pm_r,228._pm_r,  0.10_pm_r,192._pm_r,   4._pm_r,356._pm_r,  0.14_pm_r,352._pm_r, & 
264.00_pm_r, 44933._pm_r,   4._pm_r,226._pm_r,  0.10_pm_r,135._pm_r,   4._pm_r,357._pm_r,  0.06_pm_r, 49._pm_r, & 
266.30_pm_r, 48823._pm_r,   4._pm_r,224._pm_r,  0.10_pm_r, 93._pm_r,   4._pm_r,358._pm_r,  0.10_pm_r, 87._pm_r, & 
258.80_pm_r, 52675._pm_r,   4._pm_r,224._pm_r,  0.09_pm_r,301._pm_r,   4._pm_r,  1._pm_r,  0.11_pm_r, 98._pm_r, & 
247.70_pm_r, 56386._pm_r,   4._pm_r,228._pm_r,  0.45_pm_r,277._pm_r,   4._pm_r,  3._pm_r,  0.10_pm_r,118._pm_r, & 
239.10_pm_r, 59946._pm_r,   5._pm_r,235._pm_r,  0.69_pm_r,271._pm_r,   4._pm_r,  5._pm_r,  0.11_pm_r,146._pm_r, & 
231.90_pm_r, 63397._pm_r,   6._pm_r,241._pm_r,  0.73_pm_r,265._pm_r,   3._pm_r,  7._pm_r,  0.15_pm_r,140._pm_r, & 
224.00_pm_r, 66734._pm_r,   7._pm_r,244._pm_r,  0.58_pm_r,258._pm_r,   3._pm_r, 11._pm_r,  0.23_pm_r,122._pm_r, & 
217.10_pm_r, 69963._pm_r,   8._pm_r,245._pm_r,  0.37_pm_r,244._pm_r,   3._pm_r, 18._pm_r,  0.31_pm_r,112._pm_r, & 
212.60_pm_r, 73107._pm_r,   8._pm_r,244._pm_r,  0.21_pm_r,204._pm_r,   3._pm_r, 27._pm_r,  0.39_pm_r,107._pm_r, & 
209.40_pm_r, 76197._pm_r,   8._pm_r,242._pm_r,  0.25_pm_r,155._pm_r,   3._pm_r, 38._pm_r,  0.47_pm_r,104._pm_r, & 
206.60_pm_r, 79242._pm_r,   8._pm_r,239._pm_r,  0.33_pm_r,133._pm_r,   4._pm_r, 48._pm_r,  0.50_pm_r,101._pm_r, & 
201.70_pm_r, 82247._pm_r,   8._pm_r,235._pm_r,  0.39_pm_r,126._pm_r,   4._pm_r, 56._pm_r,  0.50_pm_r,100._pm_r, & 
193.80_pm_r, 85138._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.80_pm_r, 87914._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
182.40_pm_r, 90617._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
181.40_pm_r, 93295._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
182.70_pm_r, 95986._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
186.50_pm_r, 98732._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
194.50_pm_r,101591._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
208.90_pm_r,104653._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
236.70_pm_r,108076._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
292.10_pm_r,112201._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
377.20_pm_r,117564._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
293.80_pm_r,   -66._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
271.60_pm_r,  4073._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
246.00_pm_r,  7862._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
223.50_pm_r, 11299._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
212.70_pm_r, 14492._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
212.50_pm_r, 17593._pm_r,   4._pm_r,206._pm_r,  0.12_pm_r, 76._pm_r,   1._pm_r,349._pm_r,  0.11_pm_r, 18._pm_r, & 
217.00_pm_r, 20738._pm_r,   4._pm_r,203._pm_r,  0.14_pm_r, 78._pm_r,   1._pm_r,355._pm_r,  0.16_pm_r, 18._pm_r, & 
220.00_pm_r, 23938._pm_r,   4._pm_r,201._pm_r,  0.11_pm_r, 75._pm_r,   1._pm_r,360._pm_r,  0.20_pm_r, 19._pm_r, & 
224.00_pm_r, 27186._pm_r,   4._pm_r,200._pm_r,  0.06_pm_r, 59._pm_r,   2._pm_r,  4._pm_r,  0.22_pm_r, 22._pm_r, & 
230.00_pm_r, 30510._pm_r,   4._pm_r,200._pm_r,  0.05_pm_r,319._pm_r,   2._pm_r,  8._pm_r,  0.22_pm_r, 25._pm_r, & 
237.30_pm_r, 33928._pm_r,   4._pm_r,202._pm_r,  0.13_pm_r,299._pm_r,   2._pm_r, 11._pm_r,  0.21_pm_r, 31._pm_r, & 
246.90_pm_r, 37475._pm_r,   4._pm_r,205._pm_r,  0.17_pm_r,301._pm_r,   2._pm_r, 13._pm_r,  0.19_pm_r, 39._pm_r, & 
256.70_pm_r, 41161._pm_r,   4._pm_r,210._pm_r,  0.19_pm_r,313._pm_r,   3._pm_r, 16._pm_r,  0.17_pm_r, 45._pm_r, & 
263.60_pm_r, 44977._pm_r,   3._pm_r,214._pm_r,  0.27_pm_r,  4._pm_r,   3._pm_r, 19._pm_r,  0.11_pm_r, 52._pm_r, & 
265.40_pm_r, 48858._pm_r,   3._pm_r,217._pm_r,  0.51_pm_r, 23._pm_r,   3._pm_r, 20._pm_r,  0.05_pm_r, 53._pm_r, & 
257.70_pm_r, 52695._pm_r,   2._pm_r,223._pm_r,  0.51_pm_r, 18._pm_r,   3._pm_r, 20._pm_r,  0.03_pm_r, 53._pm_r, & 
246.70_pm_r, 56390._pm_r,   2._pm_r,237._pm_r,  0.34_pm_r,323._pm_r,   3._pm_r, 21._pm_r,  0.08_pm_r, 76._pm_r, & 
237.30_pm_r, 59931._pm_r,   2._pm_r,249._pm_r,  0.53_pm_r,268._pm_r,   3._pm_r, 24._pm_r,  0.16_pm_r, 79._pm_r, & 
229.20_pm_r, 63349._pm_r,   3._pm_r,253._pm_r,  0.66_pm_r,257._pm_r,   3._pm_r, 28._pm_r,  0.20_pm_r, 87._pm_r, & 
221.90_pm_r, 66650._pm_r,   4._pm_r,254._pm_r,  0.55_pm_r,259._pm_r,   3._pm_r, 33._pm_r,  0.18_pm_r,103._pm_r, & 
216.00_pm_r, 69856._pm_r,   5._pm_r,255._pm_r,  0.32_pm_r,277._pm_r,   3._pm_r, 37._pm_r,  0.17_pm_r,123._pm_r, & 
212.00_pm_r, 72988._pm_r,   5._pm_r,258._pm_r,  0.22_pm_r,329._pm_r,   3._pm_r, 41._pm_r,  0.18_pm_r,145._pm_r, & 
208.70_pm_r, 76070._pm_r,   5._pm_r,263._pm_r,  0.34_pm_r, 12._pm_r,   3._pm_r, 46._pm_r,  0.19_pm_r,159._pm_r, & 
205.20_pm_r, 79100._pm_r,   5._pm_r,270._pm_r,  0.46_pm_r, 25._pm_r,   3._pm_r, 50._pm_r,  0.21_pm_r,172._pm_r, & 
199.60_pm_r, 82081._pm_r,   4._pm_r,278._pm_r,  0.53_pm_r, 29._pm_r,   3._pm_r, 55._pm_r,  0.21_pm_r,175._pm_r, & 
190.70_pm_r, 84929._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
182.90_pm_r, 87649._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
178.00_pm_r, 90287._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
177.30_pm_r, 92900._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
179.80_pm_r, 95536._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
185.40_pm_r, 98250._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
196.30_pm_r,101112._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
213.60_pm_r,104221._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
243.90_pm_r,107737._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
296.80_pm_r,111959._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
378.20_pm_r,117355._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
286.30_pm_r,   -54._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
265.40_pm_r,  3981._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
240.30_pm_r,  7679._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
222.20_pm_r, 11062._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
218.40_pm_r, 14284._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
218.60_pm_r, 17481._pm_r,   4._pm_r,235._pm_r,  0.31_pm_r,117._pm_r,   4._pm_r, 36._pm_r,  0.46_pm_r,258._pm_r, & 
219.90_pm_r, 20691._pm_r,   4._pm_r,227._pm_r,  0.43_pm_r,119._pm_r,   3._pm_r, 27._pm_r,  0.59_pm_r,253._pm_r, & 
220.90_pm_r, 23917._pm_r,   4._pm_r,216._pm_r,  0.52_pm_r,121._pm_r,   2._pm_r, 12._pm_r,  0.63_pm_r,245._pm_r, & 
224.10_pm_r, 27172._pm_r,   4._pm_r,203._pm_r,  0.55_pm_r,122._pm_r,   2._pm_r,353._pm_r,  0.55_pm_r,231._pm_r, & 
229.70_pm_r, 30493._pm_r,   4._pm_r,192._pm_r,  0.49_pm_r,123._pm_r,   2._pm_r,334._pm_r,  0.43_pm_r,203._pm_r, & 
237.30_pm_r, 33909._pm_r,   4._pm_r,183._pm_r,  0.38_pm_r,118._pm_r,   1._pm_r,319._pm_r,  0.42_pm_r,160._pm_r, & 
246.30_pm_r, 37452._pm_r,   4._pm_r,177._pm_r,  0.25_pm_r,101._pm_r,   0._pm_r,312._pm_r,  0.49_pm_r,129._pm_r, & 
255.70_pm_r, 41126._pm_r,   4._pm_r,173._pm_r,  0.22_pm_r, 59._pm_r,   0._pm_r,105._pm_r,  0.56_pm_r,109._pm_r, & 
263.00_pm_r, 44932._pm_r,   4._pm_r,169._pm_r,  0.31_pm_r, 36._pm_r,   1._pm_r,102._pm_r,  0.49_pm_r, 93._pm_r, & 
264.60_pm_r, 48803._pm_r,   4._pm_r,164._pm_r,  0.37_pm_r, 21._pm_r,   2._pm_r, 97._pm_r,  0.51_pm_r, 86._pm_r, & 
257.30_pm_r, 52630._pm_r,   3._pm_r,160._pm_r,  0.32_pm_r,339._pm_r,   3._pm_r, 95._pm_r,  0.51_pm_r, 93._pm_r, & 
246.90_pm_r, 56324._pm_r,   3._pm_r,167._pm_r,  0.54_pm_r,282._pm_r,   3._pm_r, 96._pm_r,  0.45_pm_r,110._pm_r, & 
237.80_pm_r, 59870._pm_r,   3._pm_r,187._pm_r,  0.67_pm_r,276._pm_r,   4._pm_r, 98._pm_r,  0.28_pm_r,119._pm_r, & 
229.00_pm_r, 63290._pm_r,   3._pm_r,207._pm_r,  0.58_pm_r,281._pm_r,   4._pm_r,100._pm_r,  0.16_pm_r,132._pm_r, & 
221.60_pm_r, 66585._pm_r,   3._pm_r,222._pm_r,  0.39_pm_r,309._pm_r,   4._pm_r,101._pm_r,  0.07_pm_r,130._pm_r, & 
216.00_pm_r, 69789._pm_r,   3._pm_r,232._pm_r,  0.40_pm_r,  1._pm_r,   4._pm_r,102._pm_r,  0.05_pm_r, 96._pm_r, & 
211.20_pm_r, 72916._pm_r,   2._pm_r,243._pm_r,  0.62_pm_r, 29._pm_r,   4._pm_r,101._pm_r,  0.08_pm_r, 56._pm_r, & 
206.60_pm_r, 75977._pm_r,   1._pm_r,267._pm_r,  0.84_pm_r, 38._pm_r,   5._pm_r,100._pm_r,  0.11_pm_r, 45._pm_r, & 
202.20_pm_r, 78967._pm_r,   1._pm_r,338._pm_r,  0.97_pm_r, 43._pm_r,   5._pm_r, 98._pm_r,  0.13_pm_r, 41._pm_r, & 
195.60_pm_r, 81897._pm_r,   2._pm_r, 17._pm_r, 1.02_pm_r, 45._pm_r,   5._pm_r, 96._pm_r,  0.14_pm_r, 39._pm_r, & 
185.90_pm_r, 84677._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
177.70_pm_r, 87320._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
172.90_pm_r, 89879._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
172.90_pm_r, 92420._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
176.80_pm_r, 94998._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
184.70_pm_r, 97681._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
198.30_pm_r,100550._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
219.10_pm_r,103711._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
251.40_pm_r,107327._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
300.20_pm_r,111639._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
378.90_pm_r,117049._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
281.70_pm_r,   -73._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
260.50_pm_r,  3889._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
235.80_pm_r,  7516._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
222.60_pm_r, 10865._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
222.10_pm_r, 14115._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
222.30_pm_r, 17370._pm_r,   2._pm_r,207._pm_r,  0.46_pm_r,269._pm_r,   5._pm_r, 40._pm_r,  0.63_pm_r,265._pm_r, & 
222.00_pm_r, 20623._pm_r,   3._pm_r,219._pm_r,  0.52_pm_r,260._pm_r,   5._pm_r, 30._pm_r,  0.88_pm_r,259._pm_r, & 
221.70_pm_r, 23869._pm_r,   3._pm_r,226._pm_r,  0.45_pm_r,241._pm_r,   4._pm_r, 15._pm_r, 1.03_pm_r,250._pm_r, & 
223.60_pm_r, 27126._pm_r,   4._pm_r,225._pm_r,  0.37_pm_r,191._pm_r,   3._pm_r,350._pm_r, 1.06_pm_r,237._pm_r, & 
229.10_pm_r, 30439._pm_r,   4._pm_r,217._pm_r,  0.59_pm_r,141._pm_r,   2._pm_r,319._pm_r,  0.97_pm_r,218._pm_r, & 
236.30_pm_r, 33845._pm_r,   5._pm_r,203._pm_r,  0.93_pm_r,122._pm_r,   2._pm_r,286._pm_r,  0.88_pm_r,193._pm_r, & 
243.80_pm_r, 37363._pm_r,   5._pm_r,184._pm_r, 1.13_pm_r,111._pm_r,   2._pm_r,254._pm_r,  0.83_pm_r,166._pm_r, & 
252.50_pm_r, 40993._pm_r,   5._pm_r,167._pm_r, 1.13_pm_r,100._pm_r,   2._pm_r,224._pm_r,  0.80_pm_r,141._pm_r, & 
260.90_pm_r, 44760._pm_r,   6._pm_r,153._pm_r,  0.85_pm_r, 79._pm_r,   3._pm_r,201._pm_r,  0.62_pm_r,129._pm_r, & 
263.50_pm_r, 48607._pm_r,   6._pm_r,143._pm_r,  0.55_pm_r, 51._pm_r,   3._pm_r,185._pm_r,  0.50_pm_r,115._pm_r, & 
257.60_pm_r, 52429._pm_r,   6._pm_r,138._pm_r,  0.34_pm_r,344._pm_r,   3._pm_r,173._pm_r,  0.42_pm_r,102._pm_r, & 
248.20_pm_r, 56135._pm_r,   5._pm_r,140._pm_r,  0.64_pm_r,284._pm_r,   3._pm_r,163._pm_r,  0.33_pm_r, 88._pm_r, & 
240.70_pm_r, 59714._pm_r,   4._pm_r,149._pm_r,  0.79_pm_r,281._pm_r,   3._pm_r,155._pm_r,  0.32_pm_r, 54._pm_r, & 
232.50_pm_r, 63181._pm_r,   4._pm_r,161._pm_r,  0.69_pm_r,288._pm_r,   3._pm_r,147._pm_r,  0.35_pm_r, 29._pm_r, & 
225.30_pm_r, 66529._pm_r,   3._pm_r,172._pm_r,  0.45_pm_r,310._pm_r,   3._pm_r,138._pm_r,  0.36_pm_r, 15._pm_r, & 
218.50_pm_r, 69781._pm_r,   3._pm_r,175._pm_r,  0.37_pm_r,  3._pm_r,   3._pm_r,129._pm_r,  0.35_pm_r,  8._pm_r, & 
211.70_pm_r, 72929._pm_r,   2._pm_r,166._pm_r,  0.56_pm_r, 39._pm_r,   2._pm_r,119._pm_r,  0.34_pm_r,  4._pm_r, & 
205.30_pm_r, 75984._pm_r,   2._pm_r,139._pm_r,  0.75_pm_r, 51._pm_r,   2._pm_r,107._pm_r,  0.31_pm_r,  2._pm_r, & 
199.90_pm_r, 78947._pm_r,   2._pm_r,108._pm_r,  0.88_pm_r, 56._pm_r,   2._pm_r, 95._pm_r,  0.28_pm_r,360._pm_r, & 
192.10_pm_r, 81837._pm_r,   3._pm_r, 90._pm_r,  0.91_pm_r, 59._pm_r,   2._pm_r, 85._pm_r,  0.26_pm_r,358._pm_r, & 
180.80_pm_r, 84546._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
172.20_pm_r, 87103._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
168.00_pm_r, 89584._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
168.80_pm_r, 92056._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
174.20_pm_r, 94583._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
184.20_pm_r, 97238._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
200.40_pm_r,100116._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.50_pm_r,103331._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
258.00_pm_r,107041._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
301.60_pm_r,111420._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
379.20_pm_r,116824._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
275.80_pm_r,   -54._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
256.80_pm_r,  3835._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
232.50_pm_r,  7408._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
223.40_pm_r, 10737._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.00_pm_r, 14004._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
223.80_pm_r, 17285._pm_r,   1._pm_r,177._pm_r,  0.68_pm_r,273._pm_r,   4._pm_r, 39._pm_r,  0.42_pm_r,264._pm_r, & 
222.80_pm_r, 20555._pm_r,   2._pm_r,218._pm_r,  0.84_pm_r,268._pm_r,   4._pm_r, 31._pm_r,  0.61_pm_r,258._pm_r, & 
221.80_pm_r, 23808._pm_r,   3._pm_r,237._pm_r,  0.80_pm_r,259._pm_r,   3._pm_r, 18._pm_r,  0.77_pm_r,250._pm_r, & 
222.90_pm_r, 27060._pm_r,   4._pm_r,241._pm_r,  0.55_pm_r,238._pm_r,   2._pm_r,356._pm_r,  0.85_pm_r,240._pm_r, & 
227.40_pm_r, 30357._pm_r,   4._pm_r,237._pm_r,  0.41_pm_r,172._pm_r,   2._pm_r,324._pm_r,  0.83_pm_r,226._pm_r, & 
232.70_pm_r, 33724._pm_r,   4._pm_r,226._pm_r,  0.80_pm_r,127._pm_r,   2._pm_r,289._pm_r,  0.80_pm_r,208._pm_r, & 
239.60_pm_r, 37183._pm_r,   4._pm_r,205._pm_r, 1.17_pm_r,112._pm_r,   2._pm_r,260._pm_r,  0.71_pm_r,186._pm_r, & 
248.40_pm_r, 40752._pm_r,   4._pm_r,179._pm_r, 1.31_pm_r,101._pm_r,   3._pm_r,237._pm_r,  0.64_pm_r,162._pm_r, & 
257.60_pm_r, 44463._pm_r,   5._pm_r,157._pm_r, 1.03_pm_r, 82._pm_r,   3._pm_r,220._pm_r,  0.53_pm_r,153._pm_r, & 
262.40_pm_r, 48276._pm_r,   5._pm_r,142._pm_r,  0.69_pm_r, 48._pm_r,   3._pm_r,208._pm_r,  0.41_pm_r,147._pm_r, & 
259.00_pm_r, 52101._pm_r,   4._pm_r,132._pm_r,  0.58_pm_r,357._pm_r,   3._pm_r,201._pm_r,  0.23_pm_r,136._pm_r, & 
251.10_pm_r, 55839._pm_r,   4._pm_r,127._pm_r,  0.63_pm_r,315._pm_r,   3._pm_r,198._pm_r,  0.07_pm_r, 39._pm_r, & 
243.90_pm_r, 59462._pm_r,   3._pm_r,127._pm_r,  0.51_pm_r,299._pm_r,   3._pm_r,198._pm_r,  0.30_pm_r, 13._pm_r, & 
236.50_pm_r, 62982._pm_r,   2._pm_r,132._pm_r,  0.32_pm_r,278._pm_r,   3._pm_r,199._pm_r,  0.49_pm_r, 12._pm_r, & 
228.20_pm_r, 66382._pm_r,   2._pm_r,139._pm_r,  0.17_pm_r,225._pm_r,   2._pm_r,201._pm_r,  0.60_pm_r, 17._pm_r, & 
220.00_pm_r, 69665._pm_r,   2._pm_r,144._pm_r,  0.25_pm_r,159._pm_r,   1._pm_r,202._pm_r,  0.68_pm_r, 22._pm_r, & 
212.90_pm_r, 72832._pm_r,   3._pm_r,145._pm_r,  0.40_pm_r,139._pm_r,   0._pm_r, 36._pm_r,  0.73_pm_r, 28._pm_r, & 
206.70_pm_r, 75905._pm_r,   3._pm_r,143._pm_r,  0.51_pm_r,133._pm_r,   1._pm_r, 31._pm_r,  0.75_pm_r, 32._pm_r, & 
201.10_pm_r, 78889._pm_r,   4._pm_r,141._pm_r,  0.57_pm_r,129._pm_r,   2._pm_r, 32._pm_r,  0.73_pm_r, 34._pm_r, & 
191.40_pm_r, 81795._pm_r,   5._pm_r,138._pm_r,  0.57_pm_r,126._pm_r,   3._pm_r, 33._pm_r,  0.68_pm_r, 36._pm_r, & 
177.10_pm_r, 84457._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
167.50_pm_r, 86938._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
164.00_pm_r, 89353._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
165.70_pm_r, 91770._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
172.20_pm_r, 94258._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
183.90_pm_r, 96894._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
202.20_pm_r, 99781._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
229.00_pm_r,103042._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
263.10_pm_r,106828._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
301.50_pm_r,111247._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
379.30_pm_r,116634._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
268.80_pm_r,   -32._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
254.00_pm_r,  3784._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
230.20_pm_r,  7318._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.40_pm_r, 10636._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
225.30_pm_r, 13918._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
224.50_pm_r, 17213._pm_r,   0._pm_r, 71._pm_r,  0.47_pm_r,256._pm_r,   2._pm_r, 23._pm_r,  0.09_pm_r,220._pm_r, & 
223.10_pm_r, 20492._pm_r,   1._pm_r,258._pm_r,  0.62_pm_r,256._pm_r,   1._pm_r, 21._pm_r,  0.14_pm_r,216._pm_r, & 
221.60_pm_r, 23745._pm_r,   2._pm_r,256._pm_r,  0.64_pm_r,255._pm_r,   1._pm_r, 18._pm_r,  0.19_pm_r,213._pm_r, & 
222.10_pm_r, 26990._pm_r,   2._pm_r,256._pm_r,  0.48_pm_r,254._pm_r,   1._pm_r, 14._pm_r,  0.23_pm_r,209._pm_r, & 
226.00_pm_r, 30270._pm_r,   3._pm_r,255._pm_r,  0.19_pm_r,250._pm_r,   1._pm_r,  4._pm_r,  0.26_pm_r,205._pm_r, & 
230.60_pm_r, 33611._pm_r,   3._pm_r,255._pm_r,  0.17_pm_r, 87._pm_r,   0._pm_r,326._pm_r,  0.28_pm_r,199._pm_r, & 
237.40_pm_r, 37037._pm_r,   2._pm_r,253._pm_r,  0.50_pm_r, 81._pm_r,   0._pm_r,225._pm_r,  0.26_pm_r,192._pm_r, & 
246.10_pm_r, 40574._pm_r,   1._pm_r,248._pm_r,  0.73_pm_r, 80._pm_r,   1._pm_r,205._pm_r,  0.23_pm_r,185._pm_r, & 
255.30_pm_r, 44250._pm_r,   1._pm_r,235._pm_r,  0.57_pm_r, 64._pm_r,   1._pm_r,199._pm_r,  0.18_pm_r,191._pm_r, & 
261.30_pm_r, 48036._pm_r,   0._pm_r, 27._pm_r,  0.47_pm_r, 18._pm_r,   1._pm_r,201._pm_r,  0.14_pm_r,234._pm_r, & 
260.20_pm_r, 51862._pm_r,   1._pm_r,  4._pm_r,  0.58_pm_r,340._pm_r,   1._pm_r,211._pm_r,  0.22_pm_r,278._pm_r, & 
254.50_pm_r, 55634._pm_r,   2._pm_r,348._pm_r,  0.60_pm_r,322._pm_r,   1._pm_r,227._pm_r,  0.35_pm_r,290._pm_r, & 
247.70_pm_r, 59310._pm_r,   2._pm_r,341._pm_r,  0.28_pm_r,324._pm_r,   2._pm_r,241._pm_r,  0.27_pm_r,298._pm_r, & 
240.50_pm_r, 62888._pm_r,   3._pm_r,341._pm_r,  0.10_pm_r, 61._pm_r,   2._pm_r,251._pm_r,  0.16_pm_r,330._pm_r, & 
230.90_pm_r, 66340._pm_r,   3._pm_r,349._pm_r,  0.34_pm_r, 89._pm_r,   2._pm_r,257._pm_r,  0.21_pm_r, 43._pm_r, & 
220.90_pm_r, 69650._pm_r,   3._pm_r,  3._pm_r,  0.53_pm_r, 86._pm_r,   1._pm_r,264._pm_r,  0.40_pm_r, 63._pm_r, & 
212.80_pm_r, 72822._pm_r,   3._pm_r, 21._pm_r,  0.68_pm_r, 81._pm_r,   1._pm_r,283._pm_r,  0.58_pm_r, 70._pm_r, & 
205.90_pm_r, 75890._pm_r,   4._pm_r, 37._pm_r,  0.79_pm_r, 78._pm_r,   1._pm_r, 31._pm_r,  0.72_pm_r, 71._pm_r, & 
200.00_pm_r, 78860._pm_r,   5._pm_r, 46._pm_r,  0.82_pm_r, 75._pm_r,   2._pm_r, 59._pm_r,  0.77_pm_r, 73._pm_r, & 
189.40_pm_r, 81747._pm_r,   6._pm_r, 52._pm_r,  0.78_pm_r, 74._pm_r,   3._pm_r, 65._pm_r,  0.75_pm_r, 74._pm_r, & 
174.10_pm_r, 84364._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
164.40_pm_r, 86795._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
161.40_pm_r, 89166._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
163.60_pm_r, 91548._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
171.00_pm_r, 94012._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
183.80_pm_r, 96637._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
203.50_pm_r, 99531._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
232.00_pm_r,102825._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
266.00_pm_r,106657._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
300.80_pm_r,111095._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, & 
379.30_pm_r,116465._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)


  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_septembre = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_septembre,(/10,nb_alt,nb_lat/))

 
  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)


end subroutine cps_atmi_septembre

subroutine cps_atmi_octobre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_octobre
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de OCTOBRE
!
!$Description
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de OCTOBRE
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_octobre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments            
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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

  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
267.00_pm_r,-221._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.60_pm_r,3499._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.20_pm_r,6877._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.70_pm_r,9985._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.70_pm_r,12966._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.70_pm_r,15938._pm_r,21._pm_r,224._pm_r,2.89_pm_r,147._pm_r,4._pm_r,122._pm_r,0.65_pm_r,308._pm_r, &
207.20_pm_r,18943._pm_r,22._pm_r,211._pm_r,4.34_pm_r,142._pm_r,2._pm_r,119._pm_r,0.92_pm_r,309._pm_r, &
212.70_pm_r,22020._pm_r,25._pm_r,196._pm_r,4.91_pm_r,133._pm_r,1._pm_r,104._pm_r,0.86_pm_r,311._pm_r, &
217.00_pm_r,25157._pm_r,28._pm_r,183._pm_r,4.16_pm_r,121._pm_r,1._pm_r,28._pm_r,0.50_pm_r,317._pm_r, &
233.60_pm_r,28452._pm_r,31._pm_r,174._pm_r,3.07_pm_r,103._pm_r,1._pm_r,1._pm_r,0.16_pm_r,356._pm_r, &
255.00_pm_r,32024._pm_r,31._pm_r,167._pm_r,2.25_pm_r,76._pm_r,1._pm_r,11._pm_r,0.25_pm_r,95._pm_r, &
272.80_pm_r,35899._pm_r,31._pm_r,162._pm_r,1.90_pm_r,41._pm_r,1._pm_r,39._pm_r,0.41_pm_r,108._pm_r, &
283.80_pm_r,39980._pm_r,29._pm_r,158._pm_r,1.95_pm_r,9._pm_r,1._pm_r,65._pm_r,0.46_pm_r,114._pm_r, &
284.20_pm_r,44143._pm_r,26._pm_r,155._pm_r,2.31_pm_r,354._pm_r,2._pm_r,80._pm_r,0.31_pm_r,127._pm_r, &
281.60_pm_r,48291._pm_r,22._pm_r,153._pm_r,2.78_pm_r,349._pm_r,2._pm_r,87._pm_r,0.09_pm_r,186._pm_r, &
273.30_pm_r,52358._pm_r,18._pm_r,149._pm_r,3.11_pm_r,347._pm_r,2._pm_r,88._pm_r,0.22_pm_r,275._pm_r, &
263.50_pm_r,56290._pm_r,14._pm_r,144._pm_r,3.05_pm_r,345._pm_r,1._pm_r,85._pm_r,0.26_pm_r,288._pm_r, &
249.80_pm_r,60044._pm_r,10._pm_r,139._pm_r,2.31_pm_r,326._pm_r,1._pm_r,77._pm_r,0.17_pm_r,302._pm_r, &
238.40_pm_r,63618._pm_r,7._pm_r,140._pm_r,1.72_pm_r,301._pm_r,1._pm_r,68._pm_r,0.14_pm_r,12._pm_r, &
232.20_pm_r,67059._pm_r,5._pm_r,154._pm_r,1.43_pm_r,272._pm_r,1._pm_r,61._pm_r,0.29_pm_r,42._pm_r, &
226.20_pm_r,70420._pm_r,5._pm_r,176._pm_r,1.31_pm_r,247._pm_r,2._pm_r,57._pm_r,0.46_pm_r,48._pm_r, &
217.60_pm_r,73670._pm_r,6._pm_r,191._pm_r,1.24_pm_r,228._pm_r,3._pm_r,54._pm_r,0.59_pm_r,47._pm_r, &
208.20_pm_r,76791._pm_r,8._pm_r,197._pm_r,1.18_pm_r,214._pm_r,4._pm_r,52._pm_r,0.68_pm_r,46._pm_r, &
198.50_pm_r,79766._pm_r,10._pm_r,199._pm_r,1.09_pm_r,204._pm_r,5._pm_r,51._pm_r,0.69_pm_r,46._pm_r, &
184.40_pm_r,82605._pm_r,11._pm_r,199._pm_r,0.97_pm_r,196._pm_r,6._pm_r,50._pm_r,0.65_pm_r,45._pm_r, &
167.10_pm_r,85106._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
158.00_pm_r,87430._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
157.00_pm_r,89726._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
161.50_pm_r,92061._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
170.50_pm_r,94502._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.20_pm_r,97133._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.10_pm_r,100072._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
239.20_pm_r,103456._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
273.80_pm_r,107408._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
309.70_pm_r,111965._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
395.20_pm_r,117550._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
271.50_pm_r,-39._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
244.90_pm_r,3731._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.00_pm_r,7141._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.50_pm_r,10284._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.10_pm_r,13319._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.00_pm_r,16359._pm_r,35._pm_r,217._pm_r,5.21_pm_r,137._pm_r,7._pm_r,102._pm_r,0.97_pm_r,284._pm_r, &
210.80_pm_r,19421._pm_r,38._pm_r,203._pm_r,7.27_pm_r,134._pm_r,5._pm_r,100._pm_r,1.15_pm_r,290._pm_r, &
217.10_pm_r,22558._pm_r,42._pm_r,189._pm_r,7.80_pm_r,128._pm_r,4._pm_r,94._pm_r,0.88_pm_r,305._pm_r, &
221.80_pm_r,25766._pm_r,48._pm_r,177._pm_r,6.35_pm_r,118._pm_r,3._pm_r,80._pm_r,0.50_pm_r,3._pm_r, &
234.40_pm_r,29102._pm_r,51._pm_r,169._pm_r,4.18_pm_r,98._pm_r,4._pm_r,72._pm_r,0.95_pm_r,62._pm_r, &
251.00_pm_r,32652._pm_r,52._pm_r,164._pm_r,2.81_pm_r,55._pm_r,6._pm_r,71._pm_r,1.51_pm_r,76._pm_r, &
265.40_pm_r,36442._pm_r,49._pm_r,160._pm_r,3.20_pm_r,9._pm_r,8._pm_r,74._pm_r,1.78_pm_r,82._pm_r, &
275.20_pm_r,40404._pm_r,44._pm_r,158._pm_r,4.06_pm_r,346._pm_r,11._pm_r,77._pm_r,1.74_pm_r,87._pm_r, &
277.20_pm_r,44454._pm_r,37._pm_r,158._pm_r,4.71_pm_r,340._pm_r,13._pm_r,79._pm_r,1.04_pm_r,97._pm_r, &
275.10_pm_r,48504._pm_r,30._pm_r,158._pm_r,5.35_pm_r,338._pm_r,14._pm_r,81._pm_r,0.40_pm_r,130._pm_r, &
267.20_pm_r,52478._pm_r,22._pm_r,158._pm_r,5.57_pm_r,337._pm_r,14._pm_r,82._pm_r,0.28_pm_r,213._pm_r, &
258.10_pm_r,56326._pm_r,14._pm_r,158._pm_r,4.97_pm_r,338._pm_r,13._pm_r,83._pm_r,0.20_pm_r,257._pm_r, &
247.10_pm_r,60022._pm_r,8._pm_r,161._pm_r,3.10_pm_r,327._pm_r,13._pm_r,83._pm_r,0.16_pm_r,304._pm_r, &
238.10_pm_r,63575._pm_r,5._pm_r,176._pm_r,1.65_pm_r,301._pm_r,13._pm_r,82._pm_r,0.19_pm_r,328._pm_r, &
232.60_pm_r,67018._pm_r,5._pm_r,197._pm_r,1.24_pm_r,255._pm_r,13._pm_r,81._pm_r,0.20_pm_r,321._pm_r, &
227.00_pm_r,70387._pm_r,7._pm_r,208._pm_r,1.39_pm_r,228._pm_r,13._pm_r,80._pm_r,0.22_pm_r,304._pm_r, &
219.10_pm_r,73653._pm_r,9._pm_r,211._pm_r,1.51_pm_r,218._pm_r,12._pm_r,79._pm_r,0.25_pm_r,289._pm_r, &
209.80_pm_r,76798._pm_r,11._pm_r,212._pm_r,1.48_pm_r,214._pm_r,12._pm_r,78._pm_r,0.28_pm_r,278._pm_r, &
199.90_pm_r,79795._pm_r,13._pm_r,212._pm_r,1.36_pm_r,213._pm_r,12._pm_r,77._pm_r,0.29_pm_r,272._pm_r, &
186.40_pm_r,82653._pm_r,15._pm_r,212._pm_r,1.19_pm_r,212._pm_r,11._pm_r,77._pm_r,0.27_pm_r,269._pm_r, &
170.40_pm_r,85206._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
161.60_pm_r,87589._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
159.90_pm_r,89934._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
163.70_pm_r,92307._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
171.80_pm_r,94774._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.30_pm_r,97419._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.50_pm_r,100348._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.80_pm_r,103695._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
270.90_pm_r,107595._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
310.70_pm_r,112140._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
394.60_pm_r,117737._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
276.00_pm_r,23._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
250.10_pm_r,3867._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.30_pm_r,7349._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.30_pm_r,10561._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.30_pm_r,13678._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.00_pm_r,16810._pm_r,36._pm_r,211._pm_r,5.52_pm_r,130._pm_r,7._pm_r,73._pm_r,0.62_pm_r,272._pm_r, &
216.30_pm_r,19954._pm_r,38._pm_r,196._pm_r,7.36_pm_r,126._pm_r,6._pm_r,70._pm_r,0.59_pm_r,278._pm_r, &
224.10_pm_r,23184._pm_r,43._pm_r,182._pm_r,7.92_pm_r,121._pm_r,6._pm_r,66._pm_r,0.23_pm_r,312._pm_r, &
226.90_pm_r,26488._pm_r,49._pm_r,170._pm_r,6.56_pm_r,111._pm_r,6._pm_r,65._pm_r,0.58_pm_r,67._pm_r, &
231.80_pm_r,29842._pm_r,52._pm_r,162._pm_r,4.33_pm_r,88._pm_r,8._pm_r,67._pm_r,1.35_pm_r,78._pm_r, &
242.60_pm_r,33309._pm_r,52._pm_r,156._pm_r,3.33_pm_r,38._pm_r,10._pm_r,70._pm_r,1.92_pm_r,81._pm_r, &
253.70_pm_r,36947._pm_r,48._pm_r,153._pm_r,4.40_pm_r,358._pm_r,13._pm_r,73._pm_r,2.13_pm_r,83._pm_r, &
263.90_pm_r,40736._pm_r,41._pm_r,150._pm_r,5.49_pm_r,341._pm_r,16._pm_r,75._pm_r,1.95_pm_r,86._pm_r, &
269.70_pm_r,44651._pm_r,32._pm_r,147._pm_r,5.91_pm_r,337._pm_r,18._pm_r,76._pm_r,0.94_pm_r,83._pm_r, &
269.20_pm_r,48605._pm_r,24._pm_r,144._pm_r,6.24_pm_r,335._pm_r,19._pm_r,76._pm_r,0.15_pm_r,344._pm_r, &
261.80_pm_r,52497._pm_r,15._pm_r,139._pm_r,6.02_pm_r,331._pm_r,18._pm_r,75._pm_r,0.59_pm_r,280._pm_r, &
252.70_pm_r,56266._pm_r,7._pm_r,127._pm_r,4.93_pm_r,325._pm_r,17._pm_r,74._pm_r,0.47_pm_r,271._pm_r, &
244.50_pm_r,59903._pm_r,2._pm_r,90._pm_r,2.91_pm_r,306._pm_r,17._pm_r,74._pm_r,0.25_pm_r,208._pm_r, &
238.20_pm_r,63438._pm_r,2._pm_r,313._pm_r,1.75_pm_r,269._pm_r,17._pm_r,76._pm_r,0.38_pm_r,157._pm_r, &
232.90_pm_r,66886._pm_r,4._pm_r,276._pm_r,1.60_pm_r,233._pm_r,17._pm_r,78._pm_r,0.44_pm_r,142._pm_r, &
227.60_pm_r,70260._pm_r,5._pm_r,256._pm_r,1.61_pm_r,215._pm_r,17._pm_r,79._pm_r,0.38_pm_r,135._pm_r, &
220.80_pm_r,73544._pm_r,7._pm_r,243._pm_r,1.49_pm_r,209._pm_r,18._pm_r,81._pm_r,0.27_pm_r,130._pm_r, &
211.90_pm_r,76718._pm_r,9._pm_r,235._pm_r,1.29_pm_r,206._pm_r,18._pm_r,81._pm_r,0.16_pm_r,120._pm_r, &
201.60_pm_r,79745._pm_r,10._pm_r,230._pm_r,1.06_pm_r,205._pm_r,18._pm_r,82._pm_r,0.10_pm_r,102._pm_r, &
188.90_pm_r,82623._pm_r,12._pm_r,227._pm_r,0.84_pm_r,204._pm_r,18._pm_r,82._pm_r,0.06_pm_r,72._pm_r, &
175.20_pm_r,85251._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
167.10_pm_r,87722._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
164.50_pm_r,90142._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
167.20_pm_r,92576._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
173.90_pm_r,95087._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.40_pm_r,97751._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.20_pm_r,100666._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.70_pm_r,103960._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
265.90_pm_r,107780._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
311.40_pm_r,112292._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
393.50_pm_r,117897._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
280.50_pm_r,-27._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
256.60_pm_r,3901._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
232.10_pm_r,7475._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.60_pm_r,10771._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.20_pm_r,13966._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.70_pm_r,17167._pm_r,23._pm_r,207._pm_r,3.66_pm_r,124._pm_r,6._pm_r,81._pm_r,0.59_pm_r,256._pm_r, &
219.90_pm_r,20375._pm_r,24._pm_r,192._pm_r,4.81_pm_r,120._pm_r,5._pm_r,82._pm_r,0.62_pm_r,257._pm_r, &
223.90_pm_r,23628._pm_r,27._pm_r,177._pm_r,5.20_pm_r,114._pm_r,4._pm_r,83._pm_r,0.40_pm_r,259._pm_r, &
225.10_pm_r,26917._pm_r,30._pm_r,164._pm_r,4.65_pm_r,102._pm_r,4._pm_r,83._pm_r,0.08_pm_r,47._pm_r, &
227.20_pm_r,30225._pm_r,32._pm_r,154._pm_r,3.59_pm_r,77._pm_r,4._pm_r,81._pm_r,0.68_pm_r,72._pm_r, &
236.20_pm_r,33610._pm_r,32._pm_r,145._pm_r,3.18_pm_r,35._pm_r,6._pm_r,79._pm_r,1.17_pm_r,74._pm_r, &
247.10_pm_r,37152._pm_r,29._pm_r,138._pm_r,3.86_pm_r,2._pm_r,8._pm_r,78._pm_r,1.38_pm_r,76._pm_r, &
258.10_pm_r,40849._pm_r,25._pm_r,129._pm_r,4.54_pm_r,345._pm_r,10._pm_r,78._pm_r,1.29_pm_r,77._pm_r, &
266.10_pm_r,44698._pm_r,19._pm_r,118._pm_r,4.80_pm_r,339._pm_r,11._pm_r,78._pm_r,0.59_pm_r,71._pm_r, &
266.20_pm_r,48606._pm_r,14._pm_r,101._pm_r,4.85_pm_r,332._pm_r,11._pm_r,77._pm_r,0.09_pm_r,301._pm_r, &
259.70_pm_r,52460._pm_r,11._pm_r,74._pm_r,4.29_pm_r,321._pm_r,11._pm_r,77._pm_r,0.43_pm_r,254._pm_r, &
250.70_pm_r,56201._pm_r,9._pm_r,43._pm_r,3.20_pm_r,301._pm_r,10._pm_r,78._pm_r,0.45_pm_r,238._pm_r, &
242.80_pm_r,59810._pm_r,8._pm_r,18._pm_r,2.26_pm_r,268._pm_r,10._pm_r,79._pm_r,0.37_pm_r,211._pm_r, &
237.40_pm_r,63327._pm_r,7._pm_r,358._pm_r,1.94_pm_r,236._pm_r,9._pm_r,82._pm_r,0.31_pm_r,187._pm_r, &
232.00_pm_r,66764._pm_r,6._pm_r,337._pm_r,1.67_pm_r,218._pm_r,9._pm_r,85._pm_r,0.27_pm_r,172._pm_r, &
226.10_pm_r,70121._pm_r,5._pm_r,315._pm_r,1.26_pm_r,206._pm_r,9._pm_r,87._pm_r,0.21_pm_r,165._pm_r, &
219.50_pm_r,73385._pm_r,4._pm_r,296._pm_r,0.80_pm_r,197._pm_r,10._pm_r,88._pm_r,0.15_pm_r,161._pm_r, &
210.70_pm_r,76541._pm_r,4._pm_r,284._pm_r,0.42_pm_r,181._pm_r,10._pm_r,89._pm_r,0.10_pm_r,160._pm_r, &
201.50_pm_r,79554._pm_r,4._pm_r,279._pm_r,0.19_pm_r,139._pm_r,10._pm_r,90._pm_r,0.07_pm_r,153._pm_r, &
191.40_pm_r,82439._pm_r,4._pm_r,279._pm_r,0.19_pm_r,82._pm_r,10._pm_r,90._pm_r,0.03_pm_r,149._pm_r, &
180.80_pm_r,85146._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
173.60_pm_r,87720._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
170.20_pm_r,90232._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
171.60_pm_r,92742._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
176.60_pm_r,95308._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.00_pm_r,98000._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.60_pm_r,100902._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.80_pm_r,104133._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
259.10_pm_r,107854._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
310.60_pm_r,112306._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
392.20_pm_r,117910._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
286.40_pm_r,-29._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
263.50_pm_r,3997._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
238.00_pm_r,7668._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.20_pm_r,11029._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.00_pm_r,14237._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.90_pm_r,17412._pm_r,11._pm_r,199._pm_r,1.78_pm_r,123._pm_r,4._pm_r,87._pm_r,0.64_pm_r,260._pm_r, &
218.90_pm_r,20601._pm_r,12._pm_r,185._pm_r,2.28_pm_r,117._pm_r,3._pm_r,89._pm_r,0.78_pm_r,263._pm_r, &
222.20_pm_r,23834._pm_r,14._pm_r,171._pm_r,2.43_pm_r,107._pm_r,2._pm_r,92._pm_r,0.75_pm_r,267._pm_r, &
224.00_pm_r,27101._pm_r,15._pm_r,159._pm_r,2.25_pm_r,89._pm_r,1._pm_r,94._pm_r,0.54_pm_r,276._pm_r, &
226.70_pm_r,30398._pm_r,16._pm_r,147._pm_r,2.05_pm_r,59._pm_r,1._pm_r,76._pm_r,0.25_pm_r,315._pm_r, &
235.80_pm_r,33776._pm_r,15._pm_r,136._pm_r,2.22_pm_r,24._pm_r,1._pm_r,48._pm_r,0.33_pm_r,41._pm_r, &
247.20_pm_r,37316._pm_r,13._pm_r,124._pm_r,2.63_pm_r,0._pm_r,1._pm_r,51._pm_r,0.56_pm_r,62._pm_r, &
258.20_pm_r,41015._pm_r,11._pm_r,108._pm_r,2.93_pm_r,345._pm_r,2._pm_r,57._pm_r,0.64_pm_r,71._pm_r, &
266.30_pm_r,44864._pm_r,9._pm_r,86._pm_r,2.86_pm_r,334._pm_r,3._pm_r,61._pm_r,0.36_pm_r,74._pm_r, &
267.40_pm_r,48780._pm_r,8._pm_r,60._pm_r,2.62_pm_r,318._pm_r,3._pm_r,63._pm_r,0.14_pm_r,115._pm_r, &
260.10_pm_r,52647._pm_r,7._pm_r,34._pm_r,2.16_pm_r,293._pm_r,3._pm_r,67._pm_r,0.19_pm_r,186._pm_r, &
249.10_pm_r,56380._pm_r,7._pm_r,12._pm_r,1.80_pm_r,254._pm_r,3._pm_r,72._pm_r,0.24_pm_r,208._pm_r, &
241.00_pm_r,59963._pm_r,5._pm_r,351._pm_r,1.67_pm_r,224._pm_r,3._pm_r,75._pm_r,0.19_pm_r,227._pm_r, &
235.50_pm_r,63453._pm_r,4._pm_r,326._pm_r,1.47_pm_r,205._pm_r,3._pm_r,77._pm_r,0.12_pm_r,256._pm_r, &
229.20_pm_r,66855._pm_r,3._pm_r,297._pm_r,1.03_pm_r,196._pm_r,2._pm_r,76._pm_r,0.04_pm_r,333._pm_r, &
223.00_pm_r,70168._pm_r,3._pm_r,276._pm_r,0.46_pm_r,191._pm_r,3._pm_r,74._pm_r,0.12_pm_r,42._pm_r, &
216.90_pm_r,73387._pm_r,3._pm_r,271._pm_r,0.08_pm_r,352._pm_r,3._pm_r,72._pm_r,0.21_pm_r,57._pm_r, &
210.30_pm_r,76518._pm_r,3._pm_r,279._pm_r,0.50_pm_r,2._pm_r,3._pm_r,71._pm_r,0.27_pm_r,62._pm_r, &
203.70_pm_r,79546._pm_r,3._pm_r,296._pm_r,0.77_pm_r,3._pm_r,4._pm_r,70._pm_r,0.31_pm_r,66._pm_r, &
195.90_pm_r,82482._pm_r,4._pm_r,312._pm_r,0.87_pm_r,3._pm_r,4._pm_r,69._pm_r,0.30_pm_r,69._pm_r, &
186.90_pm_r,85275._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
180.00_pm_r,87945._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
176.10_pm_r,90549._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
176.40_pm_r,93141._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.70_pm_r,95769._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.80_pm_r,98493._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.00_pm_r,101384._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.80_pm_r,104551._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
251.50_pm_r,108165._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
308.10_pm_r,112534._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
390.80_pm_r,118119._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
292.10_pm_r,-26._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
269.80_pm_r,4091._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
244.30_pm_r,7857._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.90_pm_r,11280._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
211.00_pm_r,14459._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.20_pm_r,17530._pm_r,5._pm_r,175._pm_r,0.45_pm_r,124._pm_r,2._pm_r,77._pm_r,0.37_pm_r,251._pm_r, &
215.10_pm_r,20642._pm_r,5._pm_r,168._pm_r,0.50_pm_r,115._pm_r,1._pm_r,80._pm_r,0.46_pm_r,250._pm_r, &
220.10_pm_r,23831._pm_r,6._pm_r,162._pm_r,0.44_pm_r,94._pm_r,1._pm_r,90._pm_r,0.47_pm_r,249._pm_r, &
224.50_pm_r,27086._pm_r,6._pm_r,156._pm_r,0.41_pm_r,51._pm_r,0._pm_r,167._pm_r,0.40_pm_r,249._pm_r, &
230.00_pm_r,30413._pm_r,5._pm_r,151._pm_r,0.59_pm_r,8._pm_r,1._pm_r,224._pm_r,0.24_pm_r,248._pm_r, &
239.00_pm_r,33842._pm_r,4._pm_r,145._pm_r,0.86_pm_r,346._pm_r,1._pm_r,230._pm_r,0.06_pm_r,235._pm_r, &
249.20_pm_r,37419._pm_r,3._pm_r,138._pm_r,1.05_pm_r,333._pm_r,1._pm_r,228._pm_r,0.10_pm_r,76._pm_r, &
259.70_pm_r,41142._pm_r,1._pm_r,127._pm_r,1.11_pm_r,322._pm_r,1._pm_r,218._pm_r,0.20_pm_r,71._pm_r, &
267.90_pm_r,45014._pm_r,0._pm_r,14._pm_r,1.03_pm_r,311._pm_r,0._pm_r,188._pm_r,0.21_pm_r,82._pm_r, &
268.50_pm_r,48951._pm_r,2._pm_r,313._pm_r,0.80_pm_r,289._pm_r,1._pm_r,139._pm_r,0.30_pm_r,96._pm_r, &
260.20_pm_r,52826._pm_r,2._pm_r,293._pm_r,0.72_pm_r,236._pm_r,1._pm_r,120._pm_r,0.30_pm_r,103._pm_r, &
248.60_pm_r,56555._pm_r,3._pm_r,266._pm_r,1.15_pm_r,194._pm_r,1._pm_r,114._pm_r,0.15_pm_r,90._pm_r, &
239.60_pm_r,60124._pm_r,4._pm_r,236._pm_r,1.30_pm_r,183._pm_r,1._pm_r,109._pm_r,0.16_pm_r,343._pm_r, &
233.00_pm_r,63586._pm_r,5._pm_r,218._pm_r,1.15_pm_r,177._pm_r,1._pm_r,93._pm_r,0.32_pm_r,347._pm_r, &
226.10_pm_r,66947._pm_r,6._pm_r,208._pm_r,0.76_pm_r,174._pm_r,1._pm_r,63._pm_r,0.45_pm_r,6._pm_r, &
219.40_pm_r,70211._pm_r,7._pm_r,204._pm_r,0.30_pm_r,170._pm_r,2._pm_r,45._pm_r,0.57_pm_r,25._pm_r, &
213.80_pm_r,73380._pm_r,7._pm_r,204._pm_r,0.12_pm_r,358._pm_r,3._pm_r,40._pm_r,0.73_pm_r,37._pm_r, &
209.00_pm_r,76477._pm_r,6._pm_r,206._pm_r,0.44_pm_r,352._pm_r,4._pm_r,40._pm_r,0.85_pm_r,45._pm_r, &
205.50_pm_r,79509._pm_r,6._pm_r,210._pm_r,0.65_pm_r,351._pm_r,5._pm_r,42._pm_r,0.92_pm_r,49._pm_r, &
200.00_pm_r,82494._pm_r,5._pm_r,218._pm_r,0.74_pm_r,351._pm_r,7._pm_r,44._pm_r,0.91_pm_r,51._pm_r, &
191.70_pm_r,85354._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.00_pm_r,88098._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.30_pm_r,90780._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
180.90_pm_r,93446._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.70_pm_r,96134._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.60_pm_r,98889._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.60_pm_r,101770._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.30_pm_r,104881._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.90_pm_r,108395._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
303.80_pm_r,112667._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
389.00_pm_r,118216._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
296.50_pm_r,-34._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.30_pm_r,4151._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
250.10_pm_r,7996._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.60_pm_r,11476._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.90_pm_r,14625._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.50_pm_r,17590._pm_r,3._pm_r,151._pm_r,0.12_pm_r,261._pm_r,0._pm_r,330._pm_r,0.13_pm_r,115._pm_r, &
211.90_pm_r,20632._pm_r,2._pm_r,156._pm_r,0.21_pm_r,262._pm_r,0._pm_r,356._pm_r,0.17_pm_r,118._pm_r, &
218.20_pm_r,23783._pm_r,2._pm_r,165._pm_r,0.30_pm_r,263._pm_r,0._pm_r,56._pm_r,0.18_pm_r,125._pm_r, &
224.70_pm_r,27023._pm_r,2._pm_r,178._pm_r,0.40_pm_r,263._pm_r,0._pm_r,93._pm_r,0.16_pm_r,133._pm_r, &
232.60_pm_r,30371._pm_r,2._pm_r,194._pm_r,0.50_pm_r,263._pm_r,1._pm_r,109._pm_r,0.13_pm_r,148._pm_r, &
242.40_pm_r,33845._pm_r,3._pm_r,209._pm_r,0.57_pm_r,261._pm_r,1._pm_r,119._pm_r,0.09_pm_r,174._pm_r, &
252.40_pm_r,37472._pm_r,3._pm_r,220._pm_r,0.59_pm_r,259._pm_r,1._pm_r,127._pm_r,0.07_pm_r,225._pm_r, &
262.00_pm_r,41237._pm_r,4._pm_r,227._pm_r,0.59_pm_r,255._pm_r,1._pm_r,135._pm_r,0.09_pm_r,270._pm_r, &
268.80_pm_r,45131._pm_r,5._pm_r,232._pm_r,0.56_pm_r,262._pm_r,1._pm_r,141._pm_r,0.12_pm_r,302._pm_r, &
268.90_pm_r,49076._pm_r,6._pm_r,237._pm_r,0.49_pm_r,274._pm_r,0._pm_r,144._pm_r,0.19_pm_r,328._pm_r, &
260.10_pm_r,52954._pm_r,6._pm_r,240._pm_r,0.26_pm_r,267._pm_r,0._pm_r,106._pm_r,0.16_pm_r,339._pm_r, &
248.00_pm_r,56679._pm_r,6._pm_r,239._pm_r,0.30_pm_r,147._pm_r,0._pm_r,21._pm_r,0.02_pm_r,63._pm_r, &
237.80_pm_r,60230._pm_r,6._pm_r,232._pm_r,0.67_pm_r,136._pm_r,0._pm_r,64._pm_r,0.09_pm_r,131._pm_r, &
230.10_pm_r,63657._pm_r,6._pm_r,222._pm_r,0.82_pm_r,130._pm_r,0._pm_r,91._pm_r,0.12_pm_r,90._pm_r, &
222.70_pm_r,66970._pm_r,6._pm_r,211._pm_r,0.76_pm_r,119._pm_r,1._pm_r,72._pm_r,0.25_pm_r,35._pm_r, &
215.60_pm_r,70181._pm_r,6._pm_r,201._pm_r,0.64_pm_r,103._pm_r,1._pm_r,48._pm_r,0.48_pm_r,20._pm_r, &
209.80_pm_r,73293._pm_r,6._pm_r,193._pm_r,0.53_pm_r,80._pm_r,2._pm_r,32._pm_r,0.70_pm_r,13._pm_r, &
207.00_pm_r,76342._pm_r,5._pm_r,186._pm_r,0.54_pm_r,59._pm_r,3._pm_r,24._pm_r,0.87_pm_r,11._pm_r, &
206.40_pm_r,79367._pm_r,5._pm_r,179._pm_r,0.56_pm_r,45._pm_r,4._pm_r,19._pm_r,0.99_pm_r,9._pm_r, &
202.70_pm_r,82385._pm_r,4._pm_r,171._pm_r,0.58_pm_r,36._pm_r,6._pm_r,16._pm_r,1.02_pm_r,8._pm_r, &
194.80_pm_r,85286._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.20_pm_r,88075._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.00_pm_r,90810._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.40_pm_r,93531._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.20_pm_r,96267._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.20_pm_r,99048._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.60_pm_r,101923._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.70_pm_r,104986._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
237.40_pm_r,108415._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
298.90_pm_r,112593._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
387.20_pm_r,118096._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
299.40_pm_r,-60._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.20_pm_r,4162._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.70_pm_r,8041._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.40_pm_r,11548._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.60_pm_r,14680._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.20_pm_r,17590._pm_r,2._pm_r,155._pm_r,0.16_pm_r,301._pm_r,1._pm_r,261._pm_r,0.15_pm_r,82._pm_r, &
210.50_pm_r,20597._pm_r,2._pm_r,161._pm_r,0.22_pm_r,288._pm_r,0._pm_r,260._pm_r,0.19_pm_r,81._pm_r, &
217.10_pm_r,23729._pm_r,2._pm_r,171._pm_r,0.25_pm_r,271._pm_r,0._pm_r,264._pm_r,0.19_pm_r,78._pm_r, &
224.30_pm_r,26959._pm_r,2._pm_r,185._pm_r,0.32_pm_r,253._pm_r,0._pm_r,72._pm_r,0.18_pm_r,76._pm_r, &
232.20_pm_r,30302._pm_r,2._pm_r,198._pm_r,0.40_pm_r,241._pm_r,0._pm_r,72._pm_r,0.13_pm_r,67._pm_r, &
244.00_pm_r,33783._pm_r,3._pm_r,207._pm_r,0.46_pm_r,234._pm_r,1._pm_r,69._pm_r,0.05_pm_r,49._pm_r, &
254.70_pm_r,37441._pm_r,3._pm_r,213._pm_r,0.49_pm_r,233._pm_r,1._pm_r,64._pm_r,0.05_pm_r,302._pm_r, &
263.40_pm_r,41233._pm_r,4._pm_r,217._pm_r,0.50_pm_r,236._pm_r,1._pm_r,55._pm_r,0.12_pm_r,272._pm_r, &
268.80_pm_r,45137._pm_r,5._pm_r,221._pm_r,0.57_pm_r,253._pm_r,0._pm_r,34._pm_r,0.13_pm_r,285._pm_r, &
268.70_pm_r,49080._pm_r,5._pm_r,227._pm_r,0.66_pm_r,264._pm_r,0._pm_r,6._pm_r,0.18_pm_r,341._pm_r, &
261.50_pm_r,52966._pm_r,6._pm_r,233._pm_r,0.47_pm_r,269._pm_r,1._pm_r,0._pm_r,0.22_pm_r,357._pm_r, &
250.20_pm_r,56719._pm_r,6._pm_r,235._pm_r,0.15_pm_r,72._pm_r,1._pm_r,359._pm_r,0.10_pm_r,332._pm_r, &
238.40_pm_r,60293._pm_r,6._pm_r,231._pm_r,0.77_pm_r,85._pm_r,1._pm_r,350._pm_r,0.16_pm_r,239._pm_r, &
227.60_pm_r,63707._pm_r,5._pm_r,220._pm_r,1.22_pm_r,87._pm_r,1._pm_r,333._pm_r,0.21_pm_r,244._pm_r, &
218.10_pm_r,66967._pm_r,4._pm_r,196._pm_r,1.41_pm_r,89._pm_r,1._pm_r,319._pm_r,0.18_pm_r,291._pm_r, &
210.90_pm_r,70107._pm_r,4._pm_r,161._pm_r,1.47_pm_r,91._pm_r,1._pm_r,320._pm_r,0.31_pm_r,338._pm_r, &
207.50_pm_r,73167._pm_r,5._pm_r,136._pm_r,1.47_pm_r,93._pm_r,2._pm_r,329._pm_r,0.51_pm_r,355._pm_r, &
207.20_pm_r,76200._pm_r,7._pm_r,124._pm_r,1.47_pm_r,95._pm_r,3._pm_r,338._pm_r,0.69_pm_r,0._pm_r, &
208.20_pm_r,79244._pm_r,9._pm_r,117._pm_r,1.45_pm_r,96._pm_r,4._pm_r,345._pm_r,0.82_pm_r,3._pm_r, &
205.10_pm_r,82301._pm_r,10._pm_r,113._pm_r,1.38_pm_r,97._pm_r,5._pm_r,350._pm_r,0.87_pm_r,4._pm_r, &
196.50_pm_r,85230._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.70_pm_r,88039._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,90801._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.60_pm_r,93555._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.80_pm_r,96320._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.50_pm_r,99117._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.10_pm_r,101985._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.40_pm_r,105011._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
232.40_pm_r,108377._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
294.40_pm_r,112479._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
385.20_pm_r,117935._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
300.50_pm_r,-109._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.50_pm_r,4124._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.40_pm_r,8012._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.90_pm_r,11528._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.20_pm_r,14661._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.60_pm_r,17563._pm_r,2._pm_r,159._pm_r,0.17_pm_r,297._pm_r,1._pm_r,272._pm_r,0.13_pm_r,74._pm_r, &
210.00_pm_r,20565._pm_r,2._pm_r,166._pm_r,0.23_pm_r,288._pm_r,1._pm_r,280._pm_r,0.17_pm_r,73._pm_r, &
216.40_pm_r,23688._pm_r,1._pm_r,179._pm_r,0.27_pm_r,274._pm_r,0._pm_r,305._pm_r,0.16_pm_r,72._pm_r, &
224.00_pm_r,26907._pm_r,2._pm_r,195._pm_r,0.32_pm_r,261._pm_r,0._pm_r,2._pm_r,0.13_pm_r,67._pm_r, &
232.80_pm_r,30251._pm_r,2._pm_r,209._pm_r,0.36_pm_r,251._pm_r,0._pm_r,33._pm_r,0.08_pm_r,58._pm_r, &
244.30_pm_r,33738._pm_r,2._pm_r,218._pm_r,0.37_pm_r,244._pm_r,0._pm_r,38._pm_r,0.02_pm_r,333._pm_r, &
254.90_pm_r,37396._pm_r,3._pm_r,223._pm_r,0.36_pm_r,242._pm_r,0._pm_r,31._pm_r,0.09_pm_r,277._pm_r, &
263.80_pm_r,41190._pm_r,3._pm_r,226._pm_r,0.31_pm_r,248._pm_r,0._pm_r,8._pm_r,0.15_pm_r,270._pm_r, &
269.20_pm_r,45099._pm_r,4._pm_r,231._pm_r,0.35_pm_r,270._pm_r,0._pm_r,330._pm_r,0.17_pm_r,268._pm_r, &
269.10_pm_r,49046._pm_r,4._pm_r,237._pm_r,0.41_pm_r,281._pm_r,1._pm_r,309._pm_r,0.07_pm_r,278._pm_r, &
262.80_pm_r,52943._pm_r,5._pm_r,242._pm_r,0.29_pm_r,283._pm_r,1._pm_r,307._pm_r,0.08_pm_r,62._pm_r, &
251.50_pm_r,56711._pm_r,5._pm_r,244._pm_r,0.08_pm_r,113._pm_r,1._pm_r,316._pm_r,0.22_pm_r,82._pm_r, &
238.40_pm_r,60292._pm_r,5._pm_r,243._pm_r,0.52_pm_r,99._pm_r,1._pm_r,332._pm_r,0.14_pm_r,92._pm_r, &
225.90_pm_r,63698._pm_r,4._pm_r,236._pm_r,0.89_pm_r,96._pm_r,0._pm_r,359._pm_r,0.03_pm_r,198._pm_r, &
215.70_pm_r,66936._pm_r,3._pm_r,216._pm_r,1.16_pm_r,94._pm_r,0._pm_r,26._pm_r,0.20_pm_r,266._pm_r, &
209.50_pm_r,70058._pm_r,2._pm_r,166._pm_r,1.34_pm_r,92._pm_r,0._pm_r,340._pm_r,0.36_pm_r,270._pm_r, &
208.00_pm_r,73119._pm_r,3._pm_r,126._pm_r,1.49_pm_r,90._pm_r,0._pm_r,279._pm_r,0.51_pm_r,272._pm_r, &
209.20_pm_r,76175._pm_r,5._pm_r,111._pm_r,1.62_pm_r,89._pm_r,1._pm_r,278._pm_r,0.63_pm_r,274._pm_r, &
211.90_pm_r,79256._pm_r,8._pm_r,105._pm_r,1.70_pm_r,88._pm_r,2._pm_r,279._pm_r,0.72_pm_r,274._pm_r, &
208.80_pm_r,82367._pm_r,10._pm_r,101._pm_r,1.69_pm_r,88._pm_r,2._pm_r,280._pm_r,0.73_pm_r,275._pm_r, &
198.10_pm_r,85326._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.10_pm_r,88142._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.50_pm_r,90910._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,93674._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,96448._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.40_pm_r,99248._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.00_pm_r,102108._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.30_pm_r,105111._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.40_pm_r,108438._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
291.30_pm_r,112490._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
383.10_pm_r,117908._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
301.30_pm_r,-101._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.90_pm_r,4140._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.30_pm_r,8029._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.70_pm_r,11542._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.40_pm_r,14675._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.90_pm_r,17581._pm_r,3._pm_r,165._pm_r,0.29_pm_r,336._pm_r,1._pm_r,282._pm_r,0.18_pm_r,62._pm_r, &
210.20_pm_r,20584._pm_r,2._pm_r,168._pm_r,0.37_pm_r,327._pm_r,1._pm_r,307._pm_r,0.22_pm_r,60._pm_r, &
216.70_pm_r,23710._pm_r,2._pm_r,176._pm_r,0.39_pm_r,315._pm_r,0._pm_r,348._pm_r,0.21_pm_r,60._pm_r, &
224.10_pm_r,26934._pm_r,2._pm_r,192._pm_r,0.40_pm_r,297._pm_r,1._pm_r,14._pm_r,0.17_pm_r,55._pm_r, &
232.90_pm_r,30281._pm_r,2._pm_r,214._pm_r,0.43_pm_r,278._pm_r,1._pm_r,23._pm_r,0.08_pm_r,47._pm_r, &
243.00_pm_r,33763._pm_r,2._pm_r,230._pm_r,0.46_pm_r,265._pm_r,1._pm_r,24._pm_r,0.03_pm_r,315._pm_r, &
253.30_pm_r,37401._pm_r,3._pm_r,238._pm_r,0.47_pm_r,261._pm_r,1._pm_r,18._pm_r,0.09_pm_r,265._pm_r, &
262.70_pm_r,41179._pm_r,3._pm_r,243._pm_r,0.45_pm_r,264._pm_r,1._pm_r,7._pm_r,0.13_pm_r,259._pm_r, &
268.50_pm_r,45077._pm_r,4._pm_r,247._pm_r,0.47_pm_r,275._pm_r,1._pm_r,352._pm_r,0.13_pm_r,263._pm_r, &
268.60_pm_r,49017._pm_r,5._pm_r,252._pm_r,0.56_pm_r,277._pm_r,1._pm_r,337._pm_r,0.15_pm_r,276._pm_r, &
262.20_pm_r,52908._pm_r,5._pm_r,256._pm_r,0.52_pm_r,280._pm_r,1._pm_r,326._pm_r,0.11_pm_r,293._pm_r, &
250.70_pm_r,56672._pm_r,6._pm_r,259._pm_r,0.23_pm_r,304._pm_r,1._pm_r,326._pm_r,0.05_pm_r,34._pm_r, &
238.30_pm_r,60248._pm_r,6._pm_r,261._pm_r,0.34_pm_r,49._pm_r,1._pm_r,333._pm_r,0.15_pm_r,98._pm_r, &
227.00_pm_r,63657._pm_r,5._pm_r,264._pm_r,0.67_pm_r,70._pm_r,1._pm_r,347._pm_r,0.21_pm_r,124._pm_r, &
217.50_pm_r,66907._pm_r,4._pm_r,266._pm_r,0.88_pm_r,81._pm_r,1._pm_r,5._pm_r,0.24_pm_r,151._pm_r, &
211.50_pm_r,70045._pm_r,3._pm_r,266._pm_r,0.99_pm_r,91._pm_r,0._pm_r,48._pm_r,0.30_pm_r,176._pm_r, &
209.70_pm_r,73125._pm_r,1._pm_r,254._pm_r,1.06_pm_r,97._pm_r,0._pm_r,163._pm_r,0.37_pm_r,192._pm_r, &
209.80_pm_r,76195._pm_r,1._pm_r,134._pm_r,1.11_pm_r,102._pm_r,1._pm_r,184._pm_r,0.44_pm_r,201._pm_r, &
211.20_pm_r,79278._pm_r,2._pm_r,113._pm_r,1.14_pm_r,105._pm_r,2._pm_r,192._pm_r,0.49_pm_r,206._pm_r, &
207.80_pm_r,82384._pm_r,4._pm_r,110._pm_r,1.12_pm_r,107._pm_r,2._pm_r,197._pm_r,0.50_pm_r,209._pm_r, &
197.90_pm_r,85339._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.00_pm_r,88155._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.10_pm_r,90919._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.80_pm_r,93675._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.90_pm_r,96442._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.90_pm_r,99235._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.50_pm_r,102087._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.70_pm_r,105081._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.60_pm_r,108397._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
290.20_pm_r,112432._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
381.40_pm_r,117828._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
300.50_pm_r,-96._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.50_pm_r,4134._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.10_pm_r,8010._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.40_pm_r,11511._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.20_pm_r,14653._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.00_pm_r,17591._pm_r,4._pm_r,150._pm_r,0.46_pm_r,302._pm_r,1._pm_r,222._pm_r,0.40_pm_r,31._pm_r, &
211.40_pm_r,20618._pm_r,3._pm_r,156._pm_r,0.66_pm_r,303._pm_r,0._pm_r,241._pm_r,0.50_pm_r,30._pm_r, &
217.70_pm_r,23761._pm_r,2._pm_r,170._pm_r,0.79_pm_r,304._pm_r,0._pm_r,358._pm_r,0.49_pm_r,27._pm_r, &
224.40_pm_r,26995._pm_r,2._pm_r,201._pm_r,0.89_pm_r,306._pm_r,1._pm_r,14._pm_r,0.41_pm_r,22._pm_r, &
231.80_pm_r,30338._pm_r,2._pm_r,245._pm_r,0.93_pm_r,308._pm_r,2._pm_r,15._pm_r,0.27_pm_r,12._pm_r, &
240.30_pm_r,33792._pm_r,3._pm_r,271._pm_r,0.90_pm_r,310._pm_r,2._pm_r,13._pm_r,0.12_pm_r,336._pm_r, &
250.10_pm_r,37385._pm_r,4._pm_r,284._pm_r,0.82_pm_r,314._pm_r,2._pm_r,9._pm_r,0.12_pm_r,261._pm_r, &
260.00_pm_r,41119._pm_r,5._pm_r,291._pm_r,0.74_pm_r,318._pm_r,2._pm_r,3._pm_r,0.19_pm_r,234._pm_r, &
266.70_pm_r,44984._pm_r,6._pm_r,295._pm_r,0.64_pm_r,315._pm_r,2._pm_r,358._pm_r,0.16_pm_r,197._pm_r, &
267.20_pm_r,48901._pm_r,7._pm_r,298._pm_r,0.65_pm_r,307._pm_r,1._pm_r,358._pm_r,0.14_pm_r,155._pm_r, &
260.80_pm_r,52772._pm_r,8._pm_r,299._pm_r,0.60_pm_r,311._pm_r,1._pm_r,2._pm_r,0.04_pm_r,90._pm_r, &
249.80_pm_r,56517._pm_r,8._pm_r,301._pm_r,0.42_pm_r,339._pm_r,1._pm_r,360._pm_r,0.23_pm_r,326._pm_r, &
239.10_pm_r,60093._pm_r,8._pm_r,304._pm_r,0.34_pm_r,34._pm_r,2._pm_r,350._pm_r,0.35_pm_r,316._pm_r, &
230.10_pm_r,63531._pm_r,8._pm_r,307._pm_r,0.41_pm_r,80._pm_r,2._pm_r,342._pm_r,0.27_pm_r,304._pm_r, &
221.20_pm_r,66832._pm_r,8._pm_r,309._pm_r,0.46_pm_r,122._pm_r,2._pm_r,337._pm_r,0.10_pm_r,233._pm_r, &
214.50_pm_r,70020._pm_r,7._pm_r,308._pm_r,0.62_pm_r,153._pm_r,2._pm_r,335._pm_r,0.33_pm_r,160._pm_r, &
211.30_pm_r,73134._pm_r,6._pm_r,303._pm_r,0.81_pm_r,169._pm_r,1._pm_r,336._pm_r,0.61_pm_r,150._pm_r, &
209.10_pm_r,76212._pm_r,5._pm_r,292._pm_r,1.00_pm_r,178._pm_r,0._pm_r,2._pm_r,0.84_pm_r,147._pm_r, &
208.60_pm_r,79267._pm_r,5._pm_r,275._pm_r,1.10_pm_r,183._pm_r,1._pm_r,138._pm_r,0.98_pm_r,147._pm_r, &
205.10_pm_r,82321._pm_r,5._pm_r,257._pm_r,1.14_pm_r,186._pm_r,3._pm_r,142._pm_r,1.03_pm_r,146._pm_r, &
197.10_pm_r,85260._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.90_pm_r,88078._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.20_pm_r,90832._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.60_pm_r,93571._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.70_pm_r,96320._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.40_pm_r,99098._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.80_pm_r,101947._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.70_pm_r,104951._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.00_pm_r,108284._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
290.80_pm_r,112335._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
380.00_pm_r,117722._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
297.30_pm_r,-75._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
273.40_pm_r,4106._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
248.20_pm_r,7927._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.60_pm_r,11384._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.30_pm_r,14541._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.40_pm_r,17549._pm_r,6._pm_r,160._pm_r,0.74_pm_r,305._pm_r,1._pm_r,275._pm_r,0.34_pm_r,43._pm_r, &
213.20_pm_r,20621._pm_r,5._pm_r,169._pm_r,0.99_pm_r,312._pm_r,1._pm_r,312._pm_r,0.44_pm_r,42._pm_r, &
218.60_pm_r,23784._pm_r,3._pm_r,183._pm_r,1.12_pm_r,323._pm_r,1._pm_r,352._pm_r,0.42_pm_r,40._pm_r, &
223.30_pm_r,27018._pm_r,2._pm_r,208._pm_r,1.21_pm_r,337._pm_r,1._pm_r,8._pm_r,0.34_pm_r,37._pm_r, &
228.90_pm_r,30330._pm_r,2._pm_r,263._pm_r,1.29_pm_r,352._pm_r,2._pm_r,14._pm_r,0.19_pm_r,29._pm_r, &
236.70_pm_r,33736._pm_r,2._pm_r,319._pm_r,1.36_pm_r,5._pm_r,2._pm_r,14._pm_r,0.05_pm_r,331._pm_r, &
245.80_pm_r,37271._pm_r,4._pm_r,342._pm_r,1.37_pm_r,14._pm_r,2._pm_r,11._pm_r,0.12_pm_r,245._pm_r, &
255.60_pm_r,40938._pm_r,6._pm_r,353._pm_r,1.29_pm_r,17._pm_r,2._pm_r,6._pm_r,0.17_pm_r,234._pm_r, &
264.10_pm_r,44750._pm_r,7._pm_r,358._pm_r,0.98_pm_r,12._pm_r,2._pm_r,1._pm_r,0.13_pm_r,227._pm_r, &
266.90_pm_r,48645._pm_r,8._pm_r,359._pm_r,0.70_pm_r,357._pm_r,2._pm_r,355._pm_r,0.16_pm_r,235._pm_r, &
260.40_pm_r,52513._pm_r,9._pm_r,359._pm_r,0.56_pm_r,349._pm_r,2._pm_r,345._pm_r,0.22_pm_r,258._pm_r, &
248.60_pm_r,56246._pm_r,10._pm_r,358._pm_r,0.46_pm_r,4._pm_r,2._pm_r,331._pm_r,0.32_pm_r,276._pm_r, &
238.80_pm_r,59809._pm_r,11._pm_r,359._pm_r,0.34_pm_r,13._pm_r,2._pm_r,322._pm_r,0.24_pm_r,297._pm_r, &
231.90_pm_r,63258._pm_r,11._pm_r,359._pm_r,0.04_pm_r,324._pm_r,2._pm_r,321._pm_r,0.10_pm_r,331._pm_r, &
224.00_pm_r,66596._pm_r,11._pm_r,358._pm_r,0.45_pm_r,219._pm_r,2._pm_r,324._pm_r,0.14_pm_r,86._pm_r, &
217.00_pm_r,69823._pm_r,10._pm_r,354._pm_r,0.96_pm_r,218._pm_r,2._pm_r,330._pm_r,0.33_pm_r,110._pm_r, &
213.40_pm_r,72972._pm_r,9._pm_r,346._pm_r,1.38_pm_r,218._pm_r,2._pm_r,344._pm_r,0.50_pm_r,118._pm_r, &
210.90_pm_r,76079._pm_r,8._pm_r,333._pm_r,1.72_pm_r,218._pm_r,1._pm_r,17._pm_r,0.63_pm_r,121._pm_r, &
208.80_pm_r,79150._pm_r,7._pm_r,312._pm_r,1.91_pm_r,218._pm_r,1._pm_r,66._pm_r,0.70_pm_r,123._pm_r, &
204.70_pm_r,82192._pm_r,7._pm_r,289._pm_r,1.94_pm_r,219._pm_r,2._pm_r,92._pm_r,0.71_pm_r,123._pm_r, &
197.20_pm_r,85133._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.80_pm_r,87956._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.20_pm_r,90700._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.00_pm_r,93417._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.50_pm_r,96142._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.20_pm_r,98907._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.20_pm_r,101761._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.40_pm_r,104794._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
233.40_pm_r,108173._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
292.50_pm_r,112267._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
378.60_pm_r,117651._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
291.10_pm_r,-32._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
267.70_pm_r,4059._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.00_pm_r,7790._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.80_pm_r,11177._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.60_pm_r,14350._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.10_pm_r,17452._pm_r,5._pm_r,206._pm_r,0.16_pm_r,247._pm_r,2._pm_r,3._pm_r,0.24_pm_r,316._pm_r, &
215.20_pm_r,20580._pm_r,5._pm_r,207._pm_r,0.13_pm_r,99._pm_r,2._pm_r,356._pm_r,0.30_pm_r,311._pm_r, &
218.50_pm_r,23757._pm_r,5._pm_r,201._pm_r,0.71_pm_r,81._pm_r,3._pm_r,348._pm_r,0.32_pm_r,305._pm_r, &
221.40_pm_r,26977._pm_r,4._pm_r,182._pm_r,1.50_pm_r,77._pm_r,3._pm_r,342._pm_r,0.28_pm_r,291._pm_r, &
225.70_pm_r,30249._pm_r,4._pm_r,144._pm_r,2.34_pm_r,74._pm_r,3._pm_r,336._pm_r,0.21_pm_r,266._pm_r, &
232.80_pm_r,33602._pm_r,7._pm_r,111._pm_r,2.94_pm_r,70._pm_r,3._pm_r,331._pm_r,0.18_pm_r,225._pm_r, &
241.80_pm_r,37079._pm_r,10._pm_r,94._pm_r,3.12_pm_r,65._pm_r,3._pm_r,327._pm_r,0.20_pm_r,193._pm_r, &
251.70_pm_r,40689._pm_r,14._pm_r,84._pm_r,2.87_pm_r,57._pm_r,3._pm_r,323._pm_r,0.20_pm_r,174._pm_r, &
260.80_pm_r,44449._pm_r,18._pm_r,78._pm_r,2.24_pm_r,50._pm_r,3._pm_r,322._pm_r,0.08_pm_r,155._pm_r, &
264.60_pm_r,48304._pm_r,20._pm_r,74._pm_r,1.61_pm_r,42._pm_r,3._pm_r,322._pm_r,0.05_pm_r,0._pm_r, &
258.90_pm_r,52145._pm_r,22._pm_r,71._pm_r,1.16_pm_r,21._pm_r,3._pm_r,323._pm_r,0.13_pm_r,351._pm_r, &
248.50_pm_r,55865._pm_r,23._pm_r,67._pm_r,1.15_pm_r,351._pm_r,3._pm_r,325._pm_r,0.14_pm_r,352._pm_r, &
239.00_pm_r,59431._pm_r,23._pm_r,62._pm_r,1.51_pm_r,327._pm_r,3._pm_r,328._pm_r,0.12_pm_r,15._pm_r, &
230.90_pm_r,62872._pm_r,22._pm_r,56._pm_r,1.64_pm_r,311._pm_r,3._pm_r,330._pm_r,0.12_pm_r,31._pm_r, &
223.90_pm_r,66200._pm_r,21._pm_r,50._pm_r,1.56_pm_r,294._pm_r,3._pm_r,332._pm_r,0.12_pm_r,7._pm_r, &
218.70_pm_r,69441._pm_r,20._pm_r,45._pm_r,1.48_pm_r,275._pm_r,3._pm_r,334._pm_r,0.14_pm_r,343._pm_r, &
215.50_pm_r,72617._pm_r,19._pm_r,41._pm_r,1.52_pm_r,256._pm_r,4._pm_r,334._pm_r,0.17_pm_r,327._pm_r, &
213.10_pm_r,75756._pm_r,17._pm_r,37._pm_r,1.64_pm_r,242._pm_r,4._pm_r,333._pm_r,0.21_pm_r,317._pm_r, &
210.90_pm_r,78860._pm_r,14._pm_r,34._pm_r,1.73_pm_r,233._pm_r,4._pm_r,332._pm_r,0.23_pm_r,314._pm_r, &
206.60_pm_r,81933._pm_r,12._pm_r,30._pm_r,1.73_pm_r,228._pm_r,5._pm_r,330._pm_r,0.24_pm_r,312._pm_r, &
198.30_pm_r,84894._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.90_pm_r,87724._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
184.00_pm_r,90458._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.60_pm_r,93153._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.50_pm_r,95856._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.40_pm_r,98612._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.40_pm_r,101481._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.50_pm_r,104560._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
238.40_pm_r,108008._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
294.40_pm_r,112159._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
377.30_pm_r,117535._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
283.70_pm_r,-58._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
261.30_pm_r,3928._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
236.60_pm_r,7569._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.20_pm_r,10910._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.10_pm_r,14108._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.90_pm_r,17285._pm_r,6._pm_r,285._pm_r,1.62_pm_r,200._pm_r,5._pm_r,29._pm_r,0.88_pm_r,309._pm_r, &
217.30_pm_r,20464._pm_r,7._pm_r,261._pm_r,2.25_pm_r,181._pm_r,5._pm_r,12._pm_r,1.16_pm_r,304._pm_r, &
217.80_pm_r,23649._pm_r,8._pm_r,230._pm_r,3.07_pm_r,156._pm_r,6._pm_r,355._pm_r,1.27_pm_r,298._pm_r, &
219.20_pm_r,26848._pm_r,10._pm_r,197._pm_r,4.31_pm_r,134._pm_r,7._pm_r,342._pm_r,1.17_pm_r,286._pm_r, &
222.40_pm_r,30080._pm_r,14._pm_r,166._pm_r,5.79_pm_r,117._pm_r,8._pm_r,332._pm_r,0.92_pm_r,265._pm_r, &
228.20_pm_r,33375._pm_r,21._pm_r,144._pm_r,6.94_pm_r,105._pm_r,8._pm_r,323._pm_r,0.75_pm_r,228._pm_r, &
237.00_pm_r,36782._pm_r,29._pm_r,130._pm_r,7.11_pm_r,95._pm_r,8._pm_r,316._pm_r,0.77_pm_r,191._pm_r, &
246.90_pm_r,40321._pm_r,37._pm_r,120._pm_r,6.35_pm_r,84._pm_r,7._pm_r,309._pm_r,0.79_pm_r,166._pm_r, &
255.80_pm_r,44009._pm_r,44._pm_r,113._pm_r,4.89_pm_r,72._pm_r,6._pm_r,305._pm_r,0.63_pm_r,144._pm_r, &
259.90_pm_r,47791._pm_r,48._pm_r,107._pm_r,3.57_pm_r,58._pm_r,5._pm_r,303._pm_r,0.46_pm_r,129._pm_r, &
256.30_pm_r,51578._pm_r,50._pm_r,103._pm_r,2.56_pm_r,32._pm_r,5._pm_r,303._pm_r,0.33_pm_r,115._pm_r, &
248.10_pm_r,55276._pm_r,51._pm_r,99._pm_r,2.43_pm_r,352._pm_r,4._pm_r,305._pm_r,0.30_pm_r,100._pm_r, &
239.80_pm_r,58847._pm_r,48._pm_r,95._pm_r,3.18_pm_r,325._pm_r,4._pm_r,309._pm_r,0.37_pm_r,73._pm_r, &
231.50_pm_r,62300._pm_r,45._pm_r,91._pm_r,3.54_pm_r,312._pm_r,4._pm_r,318._pm_r,0.39_pm_r,54._pm_r, &
225.40_pm_r,65641._pm_r,41._pm_r,87._pm_r,3.30_pm_r,302._pm_r,4._pm_r,326._pm_r,0.33_pm_r,31._pm_r, &
221.40_pm_r,68911._pm_r,37._pm_r,83._pm_r,2.78_pm_r,292._pm_r,4._pm_r,331._pm_r,0.31_pm_r,357._pm_r, &
218.50_pm_r,72131._pm_r,34._pm_r,81._pm_r,2.25_pm_r,280._pm_r,4._pm_r,332._pm_r,0.34_pm_r,327._pm_r, &
216.10_pm_r,75313._pm_r,31._pm_r,79._pm_r,1.88_pm_r,265._pm_r,5._pm_r,331._pm_r,0.43_pm_r,310._pm_r, &
213.80_pm_r,78461._pm_r,28._pm_r,79._pm_r,1.64_pm_r,251._pm_r,6._pm_r,328._pm_r,0.47_pm_r,301._pm_r, &
208.80_pm_r,81574._pm_r,26._pm_r,81._pm_r,1.48_pm_r,239._pm_r,6._pm_r,325._pm_r,0.50_pm_r,298._pm_r, &
199.40_pm_r,84556._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.90_pm_r,87391._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.10_pm_r,90115._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.30_pm_r,92792._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.90_pm_r,95477._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.00_pm_r,98231._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
198.30_pm_r,101125._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.40_pm_r,104260._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
244.00_pm_r,107789._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
295.30_pm_r,111995._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
376.10_pm_r,117352._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
277.50_pm_r,-92._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
255.80_pm_r,3805._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
232.00_pm_r,7370._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.70_pm_r,10678._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.00_pm_r,13898._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.50_pm_r,17118._pm_r,8._pm_r,309._pm_r,2.57_pm_r,224._pm_r,7._pm_r,21._pm_r,0.86_pm_r,318._pm_r, &
218.30_pm_r,20324._pm_r,9._pm_r,278._pm_r,3.45_pm_r,206._pm_r,8._pm_r,11._pm_r,1.19_pm_r,312._pm_r, &
216.90_pm_r,23510._pm_r,11._pm_r,245._pm_r,4.54_pm_r,180._pm_r,9._pm_r,360._pm_r,1.43_pm_r,302._pm_r, &
216.30_pm_r,26680._pm_r,14._pm_r,213._pm_r,6.36_pm_r,155._pm_r,10._pm_r,349._pm_r,1.48_pm_r,289._pm_r, &
218.40_pm_r,29861._pm_r,21._pm_r,183._pm_r,8.76_pm_r,135._pm_r,11._pm_r,339._pm_r,1.38_pm_r,269._pm_r, &
223.40_pm_r,33093._pm_r,31._pm_r,161._pm_r,10.81_pm_r,122._pm_r,12._pm_r,329._pm_r,1.29_pm_r,242._pm_r, &
230.30_pm_r,36416._pm_r,45._pm_r,147._pm_r,11.34_pm_r,113._pm_r,11._pm_r,320._pm_r,1.26_pm_r,215._pm_r, &
239.30_pm_r,39850._pm_r,58._pm_r,137._pm_r,10.20_pm_r,103._pm_r,11._pm_r,311._pm_r,1.23_pm_r,191._pm_r, &
249.50_pm_r,43435._pm_r,68._pm_r,130._pm_r,7.24_pm_r,90._pm_r,10._pm_r,303._pm_r,1.18_pm_r,167._pm_r, &
256.40_pm_r,47142._pm_r,74._pm_r,125._pm_r,4.83_pm_r,69._pm_r,8._pm_r,296._pm_r,1.19_pm_r,150._pm_r, &
255.00_pm_r,50897._pm_r,76._pm_r,121._pm_r,3.54_pm_r,35._pm_r,7._pm_r,289._pm_r,1.14_pm_r,140._pm_r, &
247.90_pm_r,54582._pm_r,75._pm_r,117._pm_r,3.55_pm_r,356._pm_r,5._pm_r,281._pm_r,0.93_pm_r,133._pm_r, &
242.40_pm_r,58172._pm_r,71._pm_r,114._pm_r,4.18_pm_r,331._pm_r,4._pm_r,276._pm_r,0.62_pm_r,113._pm_r, &
236.30_pm_r,61679._pm_r,66._pm_r,111._pm_r,4.48_pm_r,317._pm_r,4._pm_r,275._pm_r,0.44_pm_r,73._pm_r, &
231.00_pm_r,65097._pm_r,60._pm_r,109._pm_r,4.20_pm_r,307._pm_r,3._pm_r,284._pm_r,0.50_pm_r,36._pm_r, &
226.60_pm_r,68448._pm_r,54._pm_r,108._pm_r,3.63_pm_r,298._pm_r,3._pm_r,298._pm_r,0.61_pm_r,18._pm_r, &
223.10_pm_r,71739._pm_r,50._pm_r,107._pm_r,3.02_pm_r,288._pm_r,4._pm_r,314._pm_r,0.70_pm_r,9._pm_r, &
219.80_pm_r,74984._pm_r,46._pm_r,108._pm_r,2.51_pm_r,278._pm_r,4._pm_r,325._pm_r,0.75_pm_r,5._pm_r, &
216.30_pm_r,78174._pm_r,42._pm_r,109._pm_r,2.14_pm_r,268._pm_r,5._pm_r,333._pm_r,0.77_pm_r,3._pm_r, &
210.40_pm_r,81317._pm_r,40._pm_r,110._pm_r,1.83_pm_r,259._pm_r,6._pm_r,338._pm_r,0.75_pm_r,0._pm_r, &
200.10_pm_r,84312._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.70_pm_r,87148._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.40_pm_r,89861._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
180.40_pm_r,92523._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.80_pm_r,95199._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.10_pm_r,97957._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.40_pm_r,100880._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.30_pm_r,104073._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.10_pm_r,107678._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
294.90_pm_r,111924._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
374.90_pm_r,117250._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
268.50_pm_r,-36._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
251.00_pm_r,3758._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.00_pm_r,7255._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.70_pm_r,10532._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.00_pm_r,13757._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.40_pm_r,16983._pm_r,8._pm_r,327._pm_r,2.60_pm_r,229._pm_r,7._pm_r,7._pm_r,0.49_pm_r,350._pm_r, &
217.00_pm_r,20180._pm_r,8._pm_r,296._pm_r,3.62_pm_r,213._pm_r,7._pm_r,4._pm_r,0.65_pm_r,339._pm_r, &
214.60_pm_r,23339._pm_r,10._pm_r,258._pm_r,4.85_pm_r,190._pm_r,8._pm_r,0._pm_r,0.79_pm_r,321._pm_r, &
213.60_pm_r,26472._pm_r,14._pm_r,223._pm_r,6.53_pm_r,167._pm_r,9._pm_r,354._pm_r,0.90_pm_r,298._pm_r, &
215.30_pm_r,29611._pm_r,21._pm_r,193._pm_r,8.71_pm_r,147._pm_r,10._pm_r,346._pm_r,1.07_pm_r,271._pm_r, &
216.70_pm_r,32775._pm_r,32._pm_r,172._pm_r,10.97_pm_r,134._pm_r,10._pm_r,336._pm_r,1.34_pm_r,250._pm_r, &
221.30_pm_r,35979._pm_r,45._pm_r,157._pm_r,11.75_pm_r,123._pm_r,10._pm_r,324._pm_r,1.52_pm_r,233._pm_r, &
230.50_pm_r,39280._pm_r,59._pm_r,147._pm_r,10.55_pm_r,112._pm_r,10._pm_r,311._pm_r,1.48_pm_r,217._pm_r, &
242.80_pm_r,42751._pm_r,70._pm_r,139._pm_r,7.83_pm_r,97._pm_r,10._pm_r,299._pm_r,1.33_pm_r,195._pm_r, &
253.10_pm_r,46384._pm_r,76._pm_r,134._pm_r,5.78_pm_r,75._pm_r,9._pm_r,288._pm_r,1.32_pm_r,177._pm_r, &
255.40_pm_r,50121._pm_r,78._pm_r,129._pm_r,4.26_pm_r,42._pm_r,8._pm_r,277._pm_r,1.16_pm_r,164._pm_r, &
250.60_pm_r,53829._pm_r,77._pm_r,125._pm_r,3.97_pm_r,351._pm_r,8._pm_r,268._pm_r,0.68_pm_r,147._pm_r, &
246.60_pm_r,57468._pm_r,71._pm_r,122._pm_r,5.05_pm_r,318._pm_r,7._pm_r,265._pm_r,0.33_pm_r,77._pm_r, &
242.00_pm_r,61049._pm_r,63._pm_r,121._pm_r,5.81_pm_r,303._pm_r,7._pm_r,269._pm_r,0.60_pm_r,24._pm_r, &
236.00_pm_r,64548._pm_r,54._pm_r,122._pm_r,5.69_pm_r,293._pm_r,7._pm_r,277._pm_r,0.80_pm_r,12._pm_r, &
230.20_pm_r,67962._pm_r,47._pm_r,124._pm_r,5.04_pm_r,286._pm_r,7._pm_r,288._pm_r,0.83_pm_r,8._pm_r, &
225.90_pm_r,71298._pm_r,41._pm_r,128._pm_r,4.23_pm_r,278._pm_r,7._pm_r,298._pm_r,0.79_pm_r,5._pm_r, &
223.40_pm_r,74588._pm_r,36._pm_r,133._pm_r,3.52_pm_r,270._pm_r,7._pm_r,306._pm_r,0.71_pm_r,4._pm_r, &
221.50_pm_r,77847._pm_r,33._pm_r,139._pm_r,2.96_pm_r,261._pm_r,8._pm_r,312._pm_r,0.63_pm_r,4._pm_r, &
214.90_pm_r,81078._pm_r,31._pm_r,145._pm_r,2.49_pm_r,253._pm_r,9._pm_r,316._pm_r,0.54_pm_r,3._pm_r, &
201.60_pm_r,84110._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.50_pm_r,86944._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.90_pm_r,89647._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.90_pm_r,92299._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.90_pm_r,94971._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.40_pm_r,97737._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.10_pm_r,100688._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.40_pm_r,103931._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
252.70_pm_r,107596._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
293.50_pm_r,111859._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
373.80_pm_r,117151._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /) 
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
261.60_pm_r,23._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
246.60_pm_r,3732._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.90_pm_r,7174._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.90_pm_r,10427._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.90_pm_r,13652._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.50_pm_r,16871._pm_r,7._pm_r,330._pm_r,1.40_pm_r,226._pm_r,3._pm_r,358._pm_r,0.08_pm_r,45._pm_r, &
215.10_pm_r,20047._pm_r,7._pm_r,310._pm_r,2.05_pm_r,210._pm_r,3._pm_r,359._pm_r,0.09_pm_r,18._pm_r, &
212.10_pm_r,23173._pm_r,6._pm_r,278._pm_r,2.90_pm_r,189._pm_r,4._pm_r,359._pm_r,0.13_pm_r,331._pm_r, &
210.20_pm_r,26263._pm_r,7._pm_r,237._pm_r,4.01_pm_r,169._pm_r,4._pm_r,356._pm_r,0.25_pm_r,299._pm_r, &
211.10_pm_r,29346._pm_r,11._pm_r,200._pm_r,5.29_pm_r,152._pm_r,4._pm_r,350._pm_r,0.40_pm_r,281._pm_r, &
210.60_pm_r,32434._pm_r,18._pm_r,176._pm_r,6.64_pm_r,139._pm_r,4._pm_r,341._pm_r,0.54_pm_r,268._pm_r, &
214.30_pm_r,35539._pm_r,26._pm_r,161._pm_r,7.06_pm_r,128._pm_r,4._pm_r,330._pm_r,0.60_pm_r,255._pm_r, &
224.20_pm_r,38742._pm_r,34._pm_r,151._pm_r,6.19_pm_r,118._pm_r,5._pm_r,319._pm_r,0.55_pm_r,236._pm_r, &
237.90_pm_r,42129._pm_r,41._pm_r,144._pm_r,4.41_pm_r,102._pm_r,5._pm_r,310._pm_r,0.47_pm_r,222._pm_r, &
250.60_pm_r,45705._pm_r,44._pm_r,138._pm_r,3.35_pm_r,78._pm_r,5._pm_r,303._pm_r,0.30_pm_r,222._pm_r, &
256.30_pm_r,49430._pm_r,46._pm_r,133._pm_r,2.76_pm_r,51._pm_r,5._pm_r,300._pm_r,0.10_pm_r,273._pm_r, &
254.30_pm_r,53172._pm_r,45._pm_r,128._pm_r,2.18_pm_r,20._pm_r,5._pm_r,301._pm_r,0.24_pm_r,2._pm_r, &
251.90_pm_r,56879._pm_r,44._pm_r,125._pm_r,1.89_pm_r,343._pm_r,5._pm_r,305._pm_r,0.27_pm_r,48._pm_r, &
248.00_pm_r,60544._pm_r,41._pm_r,124._pm_r,2.01_pm_r,315._pm_r,5._pm_r,310._pm_r,0.37_pm_r,85._pm_r, &
241.30_pm_r,64127._pm_r,38._pm_r,124._pm_r,2.11_pm_r,300._pm_r,4._pm_r,314._pm_r,0.55_pm_r,111._pm_r, &
233.90_pm_r,67608._pm_r,35._pm_r,125._pm_r,2.09_pm_r,290._pm_r,3._pm_r,319._pm_r,0.77_pm_r,126._pm_r, &
228.50_pm_r,70988._pm_r,32._pm_r,126._pm_r,2.00_pm_r,284._pm_r,2._pm_r,325._pm_r,1.03_pm_r,133._pm_r, &
225.90_pm_r,74314._pm_r,30._pm_r,128._pm_r,1.89_pm_r,280._pm_r,1._pm_r,6._pm_r,1.25_pm_r,137._pm_r, &
224.50_pm_r,77614._pm_r,28._pm_r,131._pm_r,1.78_pm_r,276._pm_r,2._pm_r,127._pm_r,1.39_pm_r,139._pm_r, &
217.30_pm_r,80891._pm_r,26._pm_r,135._pm_r,1.62_pm_r,273._pm_r,4._pm_r,134._pm_r,1.43_pm_r,141._pm_r, &
202.40_pm_r,83942._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.30_pm_r,86775._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
181.50_pm_r,89472._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.70_pm_r,92119._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.20_pm_r,94789._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.50_pm_r,97563._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.00_pm_r,100534._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.00_pm_r,103810._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
254.70_pm_r,107512._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
292.00_pm_r,111778._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
373.00_pm_r,117041._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_octobre = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_octobre,(/10,nb_alt,nb_lat/))



  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)


end subroutine cps_atmi_octobre

subroutine cps_atmi_novembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_novembre
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de NOVEMBRE
!
!$Description
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de NOVEMBRE
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_novembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments           
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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

  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
264.20_pm_r,  -388._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
246.40_pm_r,  3339._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
223.40_pm_r,  6768._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
214.80_pm_r,  9966._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.80_pm_r, 13153._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
228.90_pm_r, 16458._pm_r,   8._pm_r,234._pm_r, 2.84_pm_r,135._pm_r,   2._pm_r,212._pm_r,  0.12_pm_r,332._pm_r, &
233.70_pm_r, 19846._pm_r,   9._pm_r,202._pm_r, 3.32_pm_r,131._pm_r,   2._pm_r,218._pm_r,  0.14_pm_r,326._pm_r, &
235.80_pm_r, 23289._pm_r,  11._pm_r,178._pm_r, 2.63_pm_r,123._pm_r,   2._pm_r,226._pm_r,  0.12_pm_r,326._pm_r, &
236.60_pm_r, 26742._pm_r,  12._pm_r,166._pm_r, 1.21_pm_r, 88._pm_r,   2._pm_r,231._pm_r,  0.05_pm_r,323._pm_r, &
248.10_pm_r, 30284._pm_r,  11._pm_r,160._pm_r, 1.66_pm_r,  2._pm_r,   2._pm_r,231._pm_r,  0.04_pm_r,187._pm_r, &
260.50_pm_r, 34010._pm_r,   8._pm_r,156._pm_r, 3.14_pm_r,342._pm_r,   2._pm_r,228._pm_r,  0.11_pm_r,175._pm_r, &
272.50_pm_r, 37915._pm_r,   3._pm_r,152._pm_r, 3.94_pm_r,335._pm_r,   2._pm_r,222._pm_r,  0.17_pm_r,175._pm_r, &
282.10_pm_r, 41979._pm_r,   3._pm_r,334._pm_r, 3.89_pm_r,330._pm_r,   2._pm_r,216._pm_r,  0.19_pm_r,174._pm_r, &
284.90_pm_r, 46136._pm_r,   8._pm_r,330._pm_r, 2.56_pm_r,324._pm_r,   2._pm_r,213._pm_r,  0.19_pm_r,201._pm_r, &
283.20_pm_r, 50303._pm_r,  11._pm_r,327._pm_r, 1.18_pm_r,311._pm_r,   2._pm_r,213._pm_r,  0.21_pm_r,219._pm_r, &
274.50_pm_r, 54391._pm_r,  12._pm_r,325._pm_r,  0.49_pm_r,267._pm_r,   3._pm_r,214._pm_r,  0.17_pm_r,224._pm_r, &
263.90_pm_r, 58336._pm_r,  12._pm_r,322._pm_r,  0.47_pm_r,268._pm_r,   3._pm_r,215._pm_r,  0.13_pm_r,225._pm_r, &
250.00_pm_r, 62096._pm_r,  12._pm_r,319._pm_r,  0.46_pm_r,241._pm_r,   3._pm_r,215._pm_r,  0.06_pm_r,209._pm_r, &
236.80_pm_r, 65661._pm_r,  12._pm_r,316._pm_r,  0.45_pm_r,220._pm_r,   3._pm_r,214._pm_r,  0.03_pm_r,162._pm_r, &
227.40_pm_r, 69058._pm_r,  12._pm_r,313._pm_r,  0.42_pm_r,197._pm_r,   3._pm_r,213._pm_r,  0.04_pm_r,144._pm_r, &
216.00_pm_r, 72313._pm_r,  12._pm_r,310._pm_r,  0.41_pm_r,174._pm_r,   3._pm_r,212._pm_r,  0.05_pm_r,169._pm_r, &
201.60_pm_r, 75368._pm_r,  11._pm_r,309._pm_r,  0.43_pm_r,157._pm_r,   3._pm_r,211._pm_r,  0.08_pm_r,176._pm_r, &
187.30_pm_r, 78219._pm_r,  11._pm_r,307._pm_r,  0.42_pm_r,144._pm_r,   3._pm_r,210._pm_r,  0.09_pm_r,180._pm_r, &
174.60_pm_r, 80860._pm_r,  10._pm_r,307._pm_r,  0.41_pm_r,135._pm_r,   4._pm_r,209._pm_r,  0.10_pm_r,183._pm_r, &
160.30_pm_r, 83329._pm_r,  10._pm_r,306._pm_r,  0.36_pm_r,132._pm_r,   4._pm_r,208._pm_r,  0.08_pm_r,187._pm_r, &
146.80_pm_r, 85507._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
142.70_pm_r, 87577._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
145.70_pm_r, 89679._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
153.50_pm_r, 91870._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
166.30_pm_r, 94219._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.80_pm_r, 96823._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.90_pm_r, 99823._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
254.90_pm_r,103387._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
289.30_pm_r,107591._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
319.50_pm_r,112337._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
410.90_pm_r,118142._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
269.60_pm_r,  -235._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
247.70_pm_r,  3542._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.10_pm_r,  6995._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
216.50_pm_r, 10219._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.40_pm_r, 13424._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
228.00_pm_r, 16726._pm_r,  15._pm_r,233._pm_r, 4.73_pm_r,119._pm_r,   3._pm_r,212._pm_r,  0.21_pm_r,  1._pm_r, &
232.00_pm_r, 20094._pm_r,  13._pm_r,201._pm_r, 5.60_pm_r,115._pm_r,   3._pm_r,215._pm_r,  0.27_pm_r, 13._pm_r, &
234.40_pm_r, 23512._pm_r,  15._pm_r,171._pm_r, 4.71_pm_r,106._pm_r,   2._pm_r,217._pm_r,  0.32_pm_r, 36._pm_r, &
235.90_pm_r, 26951._pm_r,  18._pm_r,154._pm_r, 2.67_pm_r, 77._pm_r,   2._pm_r,214._pm_r,  0.38_pm_r, 61._pm_r, &
245.60_pm_r, 30470._pm_r,  16._pm_r,144._pm_r, 2.73_pm_r,  3._pm_r,   1._pm_r,199._pm_r,  0.45_pm_r, 82._pm_r, &
257.60_pm_r, 34153._pm_r,  12._pm_r,134._pm_r, 4.89_pm_r,334._pm_r,   1._pm_r,169._pm_r,  0.52_pm_r, 97._pm_r, &
269.30_pm_r, 38016._pm_r,   4._pm_r,103._pm_r, 6.23_pm_r,324._pm_r,   2._pm_r,146._pm_r,  0.54_pm_r,107._pm_r, &
278.80_pm_r, 42032._pm_r,   7._pm_r,343._pm_r, 6.26_pm_r,318._pm_r,   3._pm_r,135._pm_r,  0.50_pm_r,113._pm_r, &
281.90_pm_r, 46145._pm_r,  14._pm_r,329._pm_r, 4.21_pm_r,315._pm_r,   3._pm_r,132._pm_r,  0.24_pm_r,124._pm_r, &
279.80_pm_r, 50265._pm_r,  18._pm_r,325._pm_r, 2.04_pm_r,308._pm_r,   3._pm_r,133._pm_r,  0.09_pm_r,190._pm_r, &
271.20_pm_r, 54302._pm_r,  20._pm_r,323._pm_r,  0.68_pm_r,277._pm_r,   3._pm_r,135._pm_r,  0.12_pm_r,236._pm_r, &
260.60_pm_r, 58200._pm_r,  20._pm_r,321._pm_r,  0.57_pm_r,256._pm_r,   3._pm_r,138._pm_r,  0.09_pm_r,202._pm_r, &
247.90_pm_r, 61920._pm_r,  20._pm_r,318._pm_r,  0.67_pm_r,213._pm_r,   3._pm_r,139._pm_r,  0.13_pm_r,122._pm_r, &
236.10_pm_r, 65465._pm_r,  20._pm_r,316._pm_r,  0.76_pm_r,196._pm_r,   4._pm_r,136._pm_r,  0.24_pm_r, 95._pm_r, &
227.10_pm_r, 68855._pm_r,  19._pm_r,313._pm_r,  0.68_pm_r,189._pm_r,   4._pm_r,131._pm_r,  0.33_pm_r, 82._pm_r, &
216.90_pm_r, 72112._pm_r,  19._pm_r,311._pm_r,  0.52_pm_r,185._pm_r,   4._pm_r,126._pm_r,  0.39_pm_r, 75._pm_r, &
204.00_pm_r, 75192._pm_r,  19._pm_r,309._pm_r,  0.32_pm_r,182._pm_r,   5._pm_r,120._pm_r,  0.41_pm_r, 70._pm_r, &
190.20_pm_r, 78082._pm_r,  18._pm_r,309._pm_r,  0.18_pm_r,175._pm_r,   5._pm_r,114._pm_r,  0.39_pm_r, 67._pm_r, &
177.60_pm_r, 80767._pm_r,  18._pm_r,308._pm_r,  0.07_pm_r,170._pm_r,   5._pm_r,110._pm_r,  0.36_pm_r, 62._pm_r, &
163.90_pm_r, 83279._pm_r,  18._pm_r,308._pm_r,  0.01_pm_r, 90._pm_r,   6._pm_r,106._pm_r,  0.30_pm_r, 61._pm_r, &
151.50_pm_r, 85529._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
147.40_pm_r, 87676._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
149.70_pm_r, 89843._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
156.60_pm_r, 92087._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
168.30_pm_r, 94476._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.20_pm_r, 97101._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
213.80_pm_r,100089._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
249.80_pm_r,103599._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
285.40_pm_r,107729._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
320.50_pm_r,112458._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
408.90_pm_r,118260._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
275.00_pm_r,   -97._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
252.20_pm_r,  3755._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
228.80_pm_r,  7270._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.70_pm_r, 10540._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.80_pm_r, 13759._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.50_pm_r, 17037._pm_r,  16._pm_r,221._pm_r, 4.66_pm_r,106._pm_r,   2._pm_r,222._pm_r,  0.12_pm_r,111._pm_r, &
228.20_pm_r, 20360._pm_r,  14._pm_r,193._pm_r, 5.68_pm_r,101._pm_r,   2._pm_r,215._pm_r,  0.22_pm_r,108._pm_r, &
229.70_pm_r, 23713._pm_r,  16._pm_r,163._pm_r, 5.19_pm_r, 92._pm_r,   2._pm_r,204._pm_r,  0.35_pm_r,106._pm_r, &
232.80_pm_r, 27094._pm_r,  18._pm_r,142._pm_r, 3.44_pm_r, 68._pm_r,   2._pm_r,186._pm_r,  0.47_pm_r,105._pm_r, &
240.40_pm_r, 30557._pm_r,  18._pm_r,129._pm_r, 2.86_pm_r,  8._pm_r,   2._pm_r,166._pm_r,  0.56_pm_r,103._pm_r, &
252.80_pm_r, 34163._pm_r,  14._pm_r,117._pm_r, 4.51_pm_r,330._pm_r,   3._pm_r,150._pm_r,  0.61_pm_r,103._pm_r, &
264.40_pm_r, 37956._pm_r,   8._pm_r, 92._pm_r, 5.78_pm_r,317._pm_r,   3._pm_r,139._pm_r,  0.60_pm_r,103._pm_r, &
274.50_pm_r, 41901._pm_r,   6._pm_r, 13._pm_r, 5.83_pm_r,310._pm_r,   4._pm_r,132._pm_r,  0.56_pm_r,105._pm_r, &
278.90_pm_r, 45962._pm_r,  11._pm_r,337._pm_r, 3.95_pm_r,307._pm_r,   5._pm_r,129._pm_r,  0.28_pm_r,116._pm_r, &
276.30_pm_r, 50036._pm_r,  15._pm_r,328._pm_r, 1.98_pm_r,303._pm_r,   5._pm_r,129._pm_r,  0.18_pm_r,161._pm_r, &
267.80_pm_r, 54022._pm_r,  17._pm_r,325._pm_r,  0.68_pm_r,277._pm_r,   5._pm_r,131._pm_r,  0.19_pm_r,163._pm_r, &
256.80_pm_r, 57868._pm_r,  17._pm_r,322._pm_r,  0.63_pm_r,245._pm_r,   6._pm_r,132._pm_r,  0.24_pm_r,118._pm_r, &
245.20_pm_r, 61539._pm_r,  17._pm_r,319._pm_r,  0.81_pm_r,205._pm_r,   6._pm_r,129._pm_r,  0.41_pm_r, 87._pm_r, &
235.30_pm_r, 65059._pm_r,  16._pm_r,315._pm_r,  0.97_pm_r,189._pm_r,   6._pm_r,125._pm_r,  0.52_pm_r, 78._pm_r, &
226.30_pm_r, 68436._pm_r,  15._pm_r,311._pm_r,  0.97_pm_r,182._pm_r,   7._pm_r,120._pm_r,  0.54_pm_r, 74._pm_r, &
217.60_pm_r, 71689._pm_r,  14._pm_r,307._pm_r,  0.85_pm_r,177._pm_r,   7._pm_r,116._pm_r,  0.48_pm_r, 73._pm_r, &
207.10_pm_r, 74799._pm_r,  14._pm_r,303._pm_r,  0.69_pm_r,175._pm_r,   8._pm_r,112._pm_r,  0.39_pm_r, 73._pm_r, &
194.30_pm_r, 77744._pm_r,  13._pm_r,300._pm_r,  0.55_pm_r,172._pm_r,   8._pm_r,110._pm_r,  0.30_pm_r, 75._pm_r, &
181.80_pm_r, 80489._pm_r,  13._pm_r,297._pm_r,  0.41_pm_r,170._pm_r,   9._pm_r,109._pm_r,  0.24_pm_r, 74._pm_r, &
169.20_pm_r, 83063._pm_r,  12._pm_r,295._pm_r,  0.31_pm_r,167._pm_r,   9._pm_r,108._pm_r,  0.18_pm_r, 74._pm_r, &
158.80_pm_r, 85425._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
154.90_pm_r, 87692._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
155.80_pm_r, 89960._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
161.40_pm_r, 92286._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
171.30_pm_r, 94735._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 97391._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
210.70_pm_r,100364._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
242.60_pm_r,103797._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
279.20_pm_r,107819._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
321.00_pm_r,112508._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
406.30_pm_r,118300._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)
 
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
280.40_pm_r,   -52._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
258.90_pm_r,  3892._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
234.40_pm_r,  7500._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.50_pm_r, 10826._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.00_pm_r, 14048._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
220.90_pm_r, 17279._pm_r,  13._pm_r,211._pm_r, 3.30_pm_r, 95._pm_r,   2._pm_r,200._pm_r,  0.03_pm_r,350._pm_r, &
222.40_pm_r, 20523._pm_r,  11._pm_r,187._pm_r, 4.03_pm_r, 91._pm_r,   2._pm_r,199._pm_r,  0.03_pm_r,101._pm_r, &
226.20_pm_r, 23810._pm_r,  12._pm_r,158._pm_r, 3.69_pm_r, 83._pm_r,   2._pm_r,196._pm_r,  0.12_pm_r,135._pm_r, &
228.50_pm_r, 27139._pm_r,  13._pm_r,138._pm_r, 2.54_pm_r, 61._pm_r,   2._pm_r,189._pm_r,  0.26_pm_r,140._pm_r, &
234.30_pm_r, 30522._pm_r,  13._pm_r,125._pm_r, 1.91_pm_r,  4._pm_r,   2._pm_r,180._pm_r,  0.39_pm_r,140._pm_r, &
247.60_pm_r, 34043._pm_r,  10._pm_r,115._pm_r, 2.88_pm_r,321._pm_r,   3._pm_r,171._pm_r,  0.49_pm_r,137._pm_r, &
260.90_pm_r, 37773._pm_r,   6._pm_r,102._pm_r, 3.70_pm_r,306._pm_r,   3._pm_r,164._pm_r,  0.52_pm_r,132._pm_r, &
271.90_pm_r, 41675._pm_r,   2._pm_r, 30._pm_r, 3.73_pm_r,297._pm_r,   4._pm_r,158._pm_r,  0.51_pm_r,127._pm_r, &
276.80_pm_r, 45702._pm_r,   5._pm_r,320._pm_r, 2.54_pm_r,294._pm_r,   5._pm_r,154._pm_r,  0.35_pm_r,130._pm_r, &
274.10_pm_r, 49744._pm_r,   8._pm_r,309._pm_r, 1.36_pm_r,286._pm_r,   5._pm_r,152._pm_r,  0.27_pm_r,147._pm_r, &
265.90_pm_r, 53701._pm_r,   9._pm_r,304._pm_r,  0.64_pm_r,258._pm_r,   5._pm_r,152._pm_r,  0.21_pm_r,152._pm_r, &
254.80_pm_r, 57519._pm_r,   9._pm_r,299._pm_r,  0.60_pm_r,230._pm_r,   6._pm_r,152._pm_r,  0.19_pm_r,121._pm_r, &
244.10_pm_r, 61167._pm_r,   9._pm_r,294._pm_r,  0.72_pm_r,190._pm_r,   6._pm_r,149._pm_r,  0.26_pm_r, 73._pm_r, &
234.60_pm_r, 64674._pm_r,   9._pm_r,287._pm_r,  0.82_pm_r,172._pm_r,   6._pm_r,145._pm_r,  0.33_pm_r, 58._pm_r, &
225.30_pm_r, 68039._pm_r,   8._pm_r,280._pm_r,  0.77_pm_r,162._pm_r,   6._pm_r,140._pm_r,  0.31_pm_r, 52._pm_r, &
216.80_pm_r, 71278._pm_r,   8._pm_r,273._pm_r,  0.64_pm_r,157._pm_r,   6._pm_r,136._pm_r,  0.24_pm_r, 48._pm_r, &
207.80_pm_r, 74387._pm_r,   7._pm_r,267._pm_r,  0.48_pm_r,152._pm_r,   6._pm_r,133._pm_r,  0.15_pm_r, 45._pm_r, &
197.40_pm_r, 77358._pm_r,   7._pm_r,263._pm_r,  0.34_pm_r,148._pm_r,   6._pm_r,131._pm_r,  0.08_pm_r, 43._pm_r, &
187.00_pm_r, 80167._pm_r,   7._pm_r,260._pm_r,  0.23_pm_r,142._pm_r,   6._pm_r,131._pm_r,  0.04_pm_r, 28._pm_r, &
176.90_pm_r, 82834._pm_r,   7._pm_r,258._pm_r,  0.16_pm_r,135._pm_r,   6._pm_r,130._pm_r,  0.01_pm_r,  0._pm_r, &
168.30_pm_r, 85340._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
164.00_pm_r, 87754._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
163.40_pm_r, 90146._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
167.30_pm_r, 92574._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
175.00_pm_r, 95097._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.70_pm_r, 97789._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
207.10_pm_r,100745._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
234.70_pm_r,104093._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
271.10_pm_r,107986._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
320.10_pm_r,112608._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
403.10_pm_r,118377._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
287.20_pm_r,    -9._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
265.60_pm_r,  4038._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
240.00_pm_r,  7739._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
221.00_pm_r, 11114._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.50_pm_r, 14309._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.00_pm_r, 17458._pm_r,   6._pm_r,186._pm_r, 1.04_pm_r, 84._pm_r,   2._pm_r,133._pm_r,  0.34_pm_r,261._pm_r, &
217.20_pm_r, 20619._pm_r,   6._pm_r,170._pm_r, 1.19_pm_r, 79._pm_r,   2._pm_r,149._pm_r,  0.42_pm_r,256._pm_r, &
221.80_pm_r, 23837._pm_r,   6._pm_r,154._pm_r, 1.01_pm_r, 68._pm_r,   2._pm_r,170._pm_r,  0.42_pm_r,249._pm_r, &
224.80_pm_r, 27107._pm_r,   6._pm_r,142._pm_r,  0.67_pm_r, 33._pm_r,   2._pm_r,186._pm_r,  0.34_pm_r,234._pm_r, &
231.00_pm_r, 30440._pm_r,   5._pm_r,136._pm_r,  0.76_pm_r,330._pm_r,   2._pm_r,192._pm_r,  0.25_pm_r,200._pm_r, &
244.30_pm_r, 33911._pm_r,   4._pm_r,136._pm_r, 1.17_pm_r,302._pm_r,   3._pm_r,190._pm_r,  0.26_pm_r,152._pm_r, &
258.20_pm_r, 37597._pm_r,   2._pm_r,156._pm_r, 1.38_pm_r,290._pm_r,   3._pm_r,183._pm_r,  0.34_pm_r,126._pm_r, &
270.00_pm_r, 41465._pm_r,   2._pm_r,219._pm_r, 1.30_pm_r,281._pm_r,   3._pm_r,174._pm_r,  0.39_pm_r,113._pm_r, &
275.40_pm_r, 45468._pm_r,   3._pm_r,248._pm_r,  0.86_pm_r,279._pm_r,   4._pm_r,168._pm_r,  0.29_pm_r,130._pm_r, &
273.20_pm_r, 49493._pm_r,   4._pm_r,256._pm_r,  0.43_pm_r,288._pm_r,   4._pm_r,166._pm_r,  0.33_pm_r,175._pm_r, &
264.70_pm_r, 53435._pm_r,   4._pm_r,259._pm_r,  0.10_pm_r,336._pm_r,   4._pm_r,169._pm_r,  0.37_pm_r,196._pm_r, &
252.60_pm_r, 57229._pm_r,   4._pm_r,260._pm_r,  0.20_pm_r,101._pm_r,   5._pm_r,171._pm_r,  0.17_pm_r,201._pm_r, &
241.70_pm_r, 60842._pm_r,   3._pm_r,253._pm_r,  0.55_pm_r,127._pm_r,   5._pm_r,170._pm_r,  0.26_pm_r, 38._pm_r, &
233.40_pm_r, 64323._pm_r,   3._pm_r,239._pm_r,  0.66_pm_r,129._pm_r,   4._pm_r,164._pm_r,  0.56_pm_r, 36._pm_r, &
224.50_pm_r, 67674._pm_r,   3._pm_r,221._pm_r,  0.57_pm_r,128._pm_r,   4._pm_r,153._pm_r,  0.68_pm_r, 38._pm_r, &
216.10_pm_r, 70902._pm_r,   3._pm_r,206._pm_r,  0.38_pm_r,117._pm_r,   4._pm_r,138._pm_r,  0.67_pm_r, 42._pm_r, &
208.60_pm_r, 74009._pm_r,   3._pm_r,197._pm_r,  0.21_pm_r, 94._pm_r,   4._pm_r,123._pm_r,  0.61_pm_r, 45._pm_r, &
201.20_pm_r, 77013._pm_r,   3._pm_r,193._pm_r,  0.17_pm_r, 40._pm_r,   4._pm_r,111._pm_r,  0.55_pm_r, 49._pm_r, &
194.50_pm_r, 79906._pm_r,   2._pm_r,192._pm_r,  0.20_pm_r, 12._pm_r,   4._pm_r,103._pm_r,  0.47_pm_r, 52._pm_r, &
186.90_pm_r, 82708._pm_r,   2._pm_r,193._pm_r,  0.22_pm_r,360._pm_r,   5._pm_r, 97._pm_r,  0.41_pm_r, 54._pm_r, &
178.70_pm_r, 85371._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
173.40_pm_r, 87934._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
171.50_pm_r, 90457._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
173.60_pm_r, 92992._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.90_pm_r, 95594._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.60_pm_r, 98324._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.60_pm_r,101263._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.80_pm_r,104527._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
262.00_pm_r,108286._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
317.10_pm_r,112813._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
399.80_pm_r,118541._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
292.90_pm_r,   -18._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
271.40_pm_r,  4116._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
245.70_pm_r,  7904._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.50_pm_r, 11335._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
209.40_pm_r, 14499._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.00_pm_r, 17542._pm_r,   3._pm_r,134._pm_r,  0.18_pm_r,269._pm_r,   1._pm_r,100._pm_r,  0.21_pm_r,225._pm_r, &
213.00_pm_r, 20623._pm_r,   3._pm_r,139._pm_r,  0.25_pm_r,266._pm_r,   1._pm_r,118._pm_r,  0.27_pm_r,224._pm_r, &
219.30_pm_r, 23792._pm_r,   2._pm_r,146._pm_r,  0.29_pm_r,266._pm_r,   1._pm_r,144._pm_r,  0.27_pm_r,221._pm_r, &
224.20_pm_r, 27038._pm_r,   2._pm_r,156._pm_r,  0.32_pm_r,265._pm_r,   1._pm_r,164._pm_r,  0.24_pm_r,216._pm_r, &
231.40_pm_r, 30372._pm_r,   2._pm_r,168._pm_r,  0.30_pm_r,262._pm_r,   1._pm_r,174._pm_r,  0.18_pm_r,203._pm_r, &
243.40_pm_r, 33843._pm_r,   2._pm_r,179._pm_r,  0.25_pm_r,257._pm_r,   2._pm_r,177._pm_r,  0.12_pm_r,178._pm_r, &
255.60_pm_r, 37501._pm_r,   2._pm_r,186._pm_r,  0.17_pm_r,248._pm_r,   2._pm_r,175._pm_r,  0.11_pm_r,133._pm_r, &
266.60_pm_r, 41323._pm_r,   2._pm_r,190._pm_r,  0.10_pm_r,225._pm_r,   2._pm_r,170._pm_r,  0.13_pm_r,105._pm_r, &
273.10_pm_r, 45284._pm_r,   3._pm_r,191._pm_r,  0.03_pm_r,218._pm_r,   2._pm_r,165._pm_r,  0.11_pm_r,106._pm_r, &
272.50_pm_r, 49289._pm_r,   3._pm_r,191._pm_r,  0.02_pm_r, 63._pm_r,   2._pm_r,162._pm_r,  0.06_pm_r,151._pm_r, &
262.90_pm_r, 53212._pm_r,   3._pm_r,189._pm_r,  0.09_pm_r, 99._pm_r,   2._pm_r,163._pm_r,  0.09_pm_r,191._pm_r, &
251.00_pm_r, 56978._pm_r,   3._pm_r,184._pm_r,  0.21_pm_r,114._pm_r,   2._pm_r,165._pm_r,  0.12_pm_r,178._pm_r, &
241.10_pm_r, 60579._pm_r,   3._pm_r,177._pm_r,  0.28_pm_r,111._pm_r,   2._pm_r,164._pm_r,  0.13_pm_r,116._pm_r, &
231.70_pm_r, 64042._pm_r,   3._pm_r,170._pm_r,  0.21_pm_r,102._pm_r,   2._pm_r,158._pm_r,  0.22_pm_r, 78._pm_r, &
223.00_pm_r, 67369._pm_r,   3._pm_r,165._pm_r,  0.09_pm_r, 54._pm_r,   2._pm_r,149._pm_r,  0.32_pm_r, 63._pm_r, &
215.20_pm_r, 70579._pm_r,   3._pm_r,165._pm_r,  0.17_pm_r,334._pm_r,   3._pm_r,136._pm_r,  0.41_pm_r, 53._pm_r, &
208.80_pm_r, 73680._pm_r,   2._pm_r,168._pm_r,  0.34_pm_r,319._pm_r,   3._pm_r,122._pm_r,  0.47_pm_r, 47._pm_r, &
204.30_pm_r, 76704._pm_r,   2._pm_r,177._pm_r,  0.45_pm_r,315._pm_r,   3._pm_r,108._pm_r,  0.50_pm_r, 43._pm_r, &
201.10_pm_r, 79671._pm_r,   1._pm_r,197._pm_r,  0.51_pm_r,314._pm_r,   3._pm_r, 96._pm_r,  0.51_pm_r, 40._pm_r, &
195.70_pm_r, 82594._pm_r,   1._pm_r,229._pm_r,  0.52_pm_r,313._pm_r,   4._pm_r, 86._pm_r,  0.49_pm_r, 39._pm_r, &
187.40_pm_r, 85386._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
181.40_pm_r, 88071._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
178.70_pm_r, 90708._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
179.40_pm_r, 93343._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
182.40_pm_r, 96017._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.20_pm_r, 98782._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.30_pm_r,101705._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.60_pm_r,104891._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
252.80_pm_r,108523._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
312.10_pm_r,112936._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
396.30_pm_r,118604._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
297.20_pm_r,   -29._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
275.20_pm_r,  4168._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
250.50_pm_r,  8022._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.60_pm_r, 11505._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.80_pm_r, 14646._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.00_pm_r, 17591._pm_r,   3._pm_r,113._pm_r,  0.45_pm_r,247._pm_r,   0._pm_r,293._pm_r,  0.24_pm_r,129._pm_r, &
210.50_pm_r, 20610._pm_r,   2._pm_r,127._pm_r,  0.55_pm_r,244._pm_r,   0._pm_r,158._pm_r,  0.31_pm_r,128._pm_r, &
217.80_pm_r, 23749._pm_r,   2._pm_r,148._pm_r,  0.53_pm_r,243._pm_r,   1._pm_r,136._pm_r,  0.30_pm_r,129._pm_r, &
223.70_pm_r, 26981._pm_r,   2._pm_r,169._pm_r,  0.43_pm_r,239._pm_r,   1._pm_r,133._pm_r,  0.27_pm_r,129._pm_r, &
232.00_pm_r, 30317._pm_r,   2._pm_r,180._pm_r,  0.25_pm_r,236._pm_r,   1._pm_r,133._pm_r,  0.20_pm_r,131._pm_r, &
243.70_pm_r, 33795._pm_r,   2._pm_r,185._pm_r,  0.06_pm_r,231._pm_r,   2._pm_r,132._pm_r,  0.11_pm_r,131._pm_r, &
255.00_pm_r, 37452._pm_r,   2._pm_r,184._pm_r,  0.10_pm_r, 39._pm_r,   2._pm_r,132._pm_r,  0.03_pm_r,121._pm_r, &
264.60_pm_r, 41257._pm_r,   2._pm_r,181._pm_r,  0.22_pm_r, 30._pm_r,   2._pm_r,132._pm_r,  0.03_pm_r,342._pm_r, &
270.30_pm_r, 45180._pm_r,   2._pm_r,178._pm_r,  0.25_pm_r,  2._pm_r,   2._pm_r,132._pm_r,  0.04_pm_r,225._pm_r, &
270.50_pm_r, 49149._pm_r,   2._pm_r,182._pm_r,  0.33_pm_r,329._pm_r,   2._pm_r,136._pm_r,  0.14_pm_r,216._pm_r, &
261.10_pm_r, 53045._pm_r,   1._pm_r,200._pm_r,  0.39_pm_r,324._pm_r,   2._pm_r,143._pm_r,  0.16_pm_r,207._pm_r, &
250.00_pm_r, 56789._pm_r,   1._pm_r,226._pm_r,  0.32_pm_r,351._pm_r,   2._pm_r,148._pm_r,  0.08_pm_r,173._pm_r, &
240.50_pm_r, 60378._pm_r,   1._pm_r,258._pm_r,  0.36_pm_r, 32._pm_r,   2._pm_r,146._pm_r,  0.14_pm_r, 72._pm_r, &
230.90_pm_r, 63833._pm_r,   0._pm_r,341._pm_r,  0.39_pm_r, 55._pm_r,   2._pm_r,138._pm_r,  0.22_pm_r, 63._pm_r, &
221.60_pm_r, 67143._pm_r,   1._pm_r, 34._pm_r,  0.35_pm_r, 68._pm_r,   2._pm_r,129._pm_r,  0.21_pm_r, 65._pm_r, &
214.00_pm_r, 70332._pm_r,   1._pm_r, 50._pm_r,  0.27_pm_r, 86._pm_r,   2._pm_r,123._pm_r,  0.15_pm_r, 74._pm_r, &
209.10_pm_r, 73427._pm_r,   1._pm_r, 60._pm_r,  0.19_pm_r,110._pm_r,   2._pm_r,121._pm_r,  0.08_pm_r, 98._pm_r, &
206.20_pm_r, 76467._pm_r,   1._pm_r, 69._pm_r,  0.17_pm_r,140._pm_r,   3._pm_r,121._pm_r,  0.06_pm_r,162._pm_r, &
205.30_pm_r, 79477._pm_r,   2._pm_r, 79._pm_r,  0.19_pm_r,161._pm_r,   3._pm_r,123._pm_r,  0.10_pm_r,183._pm_r, &
201.30_pm_r, 82478._pm_r,   2._pm_r, 89._pm_r,  0.18_pm_r,174._pm_r,   3._pm_r,126._pm_r,  0.12_pm_r,194._pm_r, &
192.90_pm_r, 85350._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.60_pm_r, 88113._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
184.00_pm_r, 90830._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
183.80_pm_r, 93538._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
185.30_pm_r, 96270._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.50_pm_r, 99060._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.30_pm_r,101966._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
213.30_pm_r,105083._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
244.30_pm_r,108602._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
305.90_pm_r,112895._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
392.50_pm_r,118490._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /) 

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
299.90_pm_r,   -70._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
276.30_pm_r,  4157._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
252.80_pm_r,  8038._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.50_pm_r, 11546._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.40_pm_r, 14670._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.60_pm_r, 17559._pm_r,   3._pm_r,139._pm_r,  0.32_pm_r,264._pm_r,   0._pm_r,233._pm_r,  0.20_pm_r,102._pm_r, &
209.40_pm_r, 20546._pm_r,   2._pm_r,150._pm_r,  0.40_pm_r,269._pm_r,   0._pm_r,162._pm_r,  0.26_pm_r,103._pm_r, &
216.80_pm_r, 23669._pm_r,   2._pm_r,164._pm_r,  0.39_pm_r,275._pm_r,   1._pm_r,128._pm_r,  0.27_pm_r,105._pm_r, &
223.20_pm_r, 26890._pm_r,   2._pm_r,179._pm_r,  0.37_pm_r,287._pm_r,   1._pm_r,119._pm_r,  0.26_pm_r,107._pm_r, &
231.30_pm_r, 30215._pm_r,   2._pm_r,194._pm_r,  0.31_pm_r,305._pm_r,   1._pm_r,116._pm_r,  0.20_pm_r,109._pm_r, &
244.30_pm_r, 33692._pm_r,   2._pm_r,208._pm_r,  0.28_pm_r,331._pm_r,   2._pm_r,115._pm_r,  0.12_pm_r,108._pm_r, &
255.00_pm_r, 37355._pm_r,   1._pm_r,222._pm_r,  0.29_pm_r,350._pm_r,   2._pm_r,115._pm_r,  0.04_pm_r, 94._pm_r, &
263.20_pm_r, 41150._pm_r,   1._pm_r,240._pm_r,  0.32_pm_r,360._pm_r,   2._pm_r,114._pm_r,  0.04_pm_r,333._pm_r, &
267.90_pm_r, 45044._pm_r,   1._pm_r,267._pm_r,  0.29_pm_r,355._pm_r,   2._pm_r,113._pm_r,  0.03_pm_r,287._pm_r, &
267.60_pm_r, 48972._pm_r,   1._pm_r,289._pm_r,  0.29_pm_r,329._pm_r,   2._pm_r,115._pm_r,  0.10_pm_r,183._pm_r, &
261.20_pm_r, 52848._pm_r,   2._pm_r,298._pm_r,  0.34_pm_r,316._pm_r,   2._pm_r,122._pm_r,  0.19_pm_r,180._pm_r, &
252.40_pm_r, 56612._pm_r,   2._pm_r,304._pm_r,  0.31_pm_r,328._pm_r,   2._pm_r,130._pm_r,  0.22_pm_r,192._pm_r, &
242.10_pm_r, 60233._pm_r,   2._pm_r,311._pm_r,  0.22_pm_r, 21._pm_r,   2._pm_r,137._pm_r,  0.12_pm_r,217._pm_r, &
230.00_pm_r, 63695._pm_r,   2._pm_r,320._pm_r,  0.35_pm_r, 76._pm_r,   2._pm_r,141._pm_r,  0.07_pm_r,274._pm_r, &
219.00_pm_r, 66978._pm_r,   2._pm_r,337._pm_r,  0.57_pm_r, 99._pm_r,   2._pm_r,142._pm_r,  0.05_pm_r,335._pm_r, &
211.30_pm_r, 70127._pm_r,   1._pm_r, 10._pm_r,  0.77_pm_r,110._pm_r,   2._pm_r,140._pm_r,  0.07_pm_r, 36._pm_r, &
208.10_pm_r, 73194._pm_r,   2._pm_r, 57._pm_r,  0.94_pm_r,115._pm_r,   2._pm_r,136._pm_r,  0.10_pm_r, 63._pm_r, &
207.80_pm_r, 76237._pm_r,   3._pm_r, 85._pm_r, 1.07_pm_r,118._pm_r,   2._pm_r,131._pm_r,  0.14_pm_r, 71._pm_r, &
208.50_pm_r, 79288._pm_r,   4._pm_r, 97._pm_r, 1.14_pm_r,120._pm_r,   2._pm_r,126._pm_r,  0.17_pm_r, 78._pm_r, &
204.70_pm_r, 82346._pm_r,   6._pm_r,104._pm_r, 1.14_pm_r,121._pm_r,   2._pm_r,121._pm_r,  0.17_pm_r, 82._pm_r, &
195.60_pm_r, 85261._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.10_pm_r, 88059._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 90816._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.40_pm_r, 93566._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.00_pm_r, 96332._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.20_pm_r, 99134._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.80_pm_r,102021._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.10_pm_r,105080._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
237.10_pm_r,108502._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
299.50_pm_r,112685._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
388.30_pm_r,118202._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
300.50_pm_r,  -111._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
276.70_pm_r,  4123._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.60_pm_r,  8013._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
226.00_pm_r, 11532._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.00_pm_r, 14657._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.90_pm_r, 17537._pm_r,   2._pm_r,140._pm_r,  0.28_pm_r,271._pm_r,   0._pm_r,251._pm_r,  0.16_pm_r, 79._pm_r, &
208.60_pm_r, 20516._pm_r,   2._pm_r,148._pm_r,  0.37_pm_r,276._pm_r,   0._pm_r, 97._pm_r,  0.21_pm_r, 82._pm_r, &
216.20_pm_r, 23630._pm_r,   2._pm_r,161._pm_r,  0.38_pm_r,286._pm_r,   1._pm_r, 90._pm_r,  0.22_pm_r, 82._pm_r, &
223.20_pm_r, 26847._pm_r,   1._pm_r,178._pm_r,  0.39_pm_r,300._pm_r,   1._pm_r, 89._pm_r,  0.20_pm_r, 84._pm_r, &
232.10_pm_r, 30177._pm_r,   1._pm_r,204._pm_r,  0.39_pm_r,317._pm_r,   1._pm_r, 89._pm_r,  0.15_pm_r, 82._pm_r, &
244.80_pm_r, 33662._pm_r,   1._pm_r,242._pm_r,  0.39_pm_r,334._pm_r,   1._pm_r, 88._pm_r,  0.08_pm_r, 76._pm_r, &
255.10_pm_r, 37325._pm_r,   1._pm_r,282._pm_r,  0.39_pm_r,348._pm_r,   1._pm_r, 87._pm_r,  0.04_pm_r, 30._pm_r, &
262.70_pm_r, 41115._pm_r,   1._pm_r,308._pm_r,  0.40_pm_r,357._pm_r,   1._pm_r, 84._pm_r,  0.06_pm_r,329._pm_r, &
266.90_pm_r, 44999._pm_r,   2._pm_r,322._pm_r,  0.43_pm_r,353._pm_r,   1._pm_r, 81._pm_r,  0.10_pm_r,279._pm_r, &
267.00_pm_r, 48914._pm_r,   2._pm_r,328._pm_r,  0.36_pm_r,344._pm_r,   1._pm_r, 80._pm_r,  0.22_pm_r,254._pm_r, &
262.40_pm_r, 52792._pm_r,   3._pm_r,329._pm_r,  0.16_pm_r,346._pm_r,   1._pm_r, 86._pm_r,  0.30_pm_r,237._pm_r, &
253.50_pm_r, 56571._pm_r,   3._pm_r,328._pm_r,  0.19_pm_r,126._pm_r,   1._pm_r,110._pm_r,  0.30_pm_r,217._pm_r, &
241.60_pm_r, 60196._pm_r,   3._pm_r,328._pm_r,  0.45_pm_r,127._pm_r,   1._pm_r,137._pm_r,  0.21_pm_r,173._pm_r, &
227.80_pm_r, 63642._pm_r,   2._pm_r,332._pm_r,  0.63_pm_r,129._pm_r,   1._pm_r,144._pm_r,  0.29_pm_r,131._pm_r, &
215.60_pm_r, 66893._pm_r,   1._pm_r,344._pm_r,  0.70_pm_r,129._pm_r,   2._pm_r,142._pm_r,  0.40_pm_r,118._pm_r, &
208.50_pm_r, 70008._pm_r,   1._pm_r, 26._pm_r,  0.72_pm_r,129._pm_r,   2._pm_r,137._pm_r,  0.48_pm_r,114._pm_r, &
207.30_pm_r, 73057._pm_r,   1._pm_r, 86._pm_r,  0.73_pm_r,129._pm_r,   3._pm_r,131._pm_r,  0.54_pm_r,113._pm_r, &
209.70_pm_r, 76110._pm_r,   2._pm_r,106._pm_r,  0.75_pm_r,130._pm_r,   3._pm_r,127._pm_r,  0.58_pm_r,114._pm_r, &
212.30_pm_r, 79199._pm_r,   4._pm_r,114._pm_r,  0.73_pm_r,130._pm_r,   4._pm_r,123._pm_r,  0.61_pm_r,114._pm_r, &
208.40_pm_r, 82311._pm_r,   5._pm_r,118._pm_r,  0.70_pm_r,130._pm_r,   5._pm_r,120._pm_r,  0.60_pm_r,115._pm_r, &
197.40_pm_r, 85259._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.70_pm_r, 88066._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.50_pm_r, 90832._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 93595._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.40_pm_r, 96370._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.50_pm_r, 99171._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.60_pm_r,102036._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.10_pm_r,105048._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
231.60_pm_r,108398._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
294.10_pm_r,112493._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
384.10_pm_r,117935._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
301.00_pm_r,  -100._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
277.00_pm_r,  4139._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
253.20_pm_r,  8028._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.70_pm_r, 11541._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.50_pm_r, 14667._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
198.30_pm_r, 17553._pm_r,   3._pm_r,142._pm_r,  0.33_pm_r,313._pm_r,   1._pm_r,286._pm_r,  0.26_pm_r, 67._pm_r, &
208.80_pm_r, 20533._pm_r,   3._pm_r,144._pm_r,  0.50_pm_r,320._pm_r,   0._pm_r,335._pm_r,  0.32_pm_r, 71._pm_r, &
216.60_pm_r, 23651._pm_r,   2._pm_r,144._pm_r,  0.61_pm_r,326._pm_r,   1._pm_r, 33._pm_r,  0.30_pm_r, 76._pm_r, &
223.50_pm_r, 26872._pm_r,   1._pm_r,138._pm_r,  0.71_pm_r,333._pm_r,   1._pm_r, 53._pm_r,  0.25_pm_r, 83._pm_r, &
232.20_pm_r, 30207._pm_r,   0._pm_r, 19._pm_r,  0.78_pm_r,339._pm_r,   1._pm_r, 62._pm_r,  0.17_pm_r, 96._pm_r, &
243.60_pm_r, 33688._pm_r,   2._pm_r,350._pm_r,  0.79_pm_r,344._pm_r,   1._pm_r, 68._pm_r,  0.09_pm_r,119._pm_r, &
254.00_pm_r, 37337._pm_r,   3._pm_r,348._pm_r,  0.75_pm_r,347._pm_r,   1._pm_r, 71._pm_r,  0.04_pm_r,169._pm_r, &
262.50_pm_r, 41119._pm_r,   4._pm_r,348._pm_r,  0.68_pm_r,349._pm_r,   1._pm_r, 72._pm_r,  0.04_pm_r,270._pm_r, &
266.90_pm_r, 45000._pm_r,   5._pm_r,348._pm_r,  0.67_pm_r,347._pm_r,   1._pm_r, 71._pm_r,  0.02_pm_r,315._pm_r, &
267.00_pm_r, 48914._pm_r,   6._pm_r,348._pm_r,  0.53_pm_r,338._pm_r,   1._pm_r, 70._pm_r,  0.03_pm_r,252._pm_r, &
261.80_pm_r, 52790._pm_r,   6._pm_r,346._pm_r,  0.19_pm_r,307._pm_r,   1._pm_r, 72._pm_r,  0.14_pm_r,238._pm_r, &
252.70_pm_r, 56565._pm_r,   6._pm_r,345._pm_r,  0.43_pm_r,175._pm_r,   1._pm_r, 76._pm_r,  0.28_pm_r,240._pm_r, &
241.10_pm_r, 60181._pm_r,   5._pm_r,344._pm_r,  0.79_pm_r,163._pm_r,   1._pm_r, 85._pm_r,  0.23_pm_r,249._pm_r, &
227.60_pm_r, 63618._pm_r,   4._pm_r,345._pm_r,  0.95_pm_r,159._pm_r,   0._pm_r,103._pm_r,  0.10_pm_r,240._pm_r, &
216.40_pm_r, 66862._pm_r,   2._pm_r,350._pm_r,  0.92_pm_r,157._pm_r,   0._pm_r,117._pm_r,  0.13_pm_r,114._pm_r, &
209.70_pm_r, 69977._pm_r,   1._pm_r,  4._pm_r,  0.80_pm_r,157._pm_r,   1._pm_r,112._pm_r,  0.32_pm_r,104._pm_r, &
208.50_pm_r, 73035._pm_r,   1._pm_r, 75._pm_r,  0.67_pm_r,157._pm_r,   1._pm_r,107._pm_r,  0.50_pm_r,102._pm_r, &
209.80_pm_r, 76095._pm_r,   1._pm_r,129._pm_r,  0.56_pm_r,157._pm_r,   2._pm_r,105._pm_r,  0.65_pm_r,102._pm_r, &
211.60_pm_r, 79184._pm_r,   2._pm_r,141._pm_r,  0.48_pm_r,157._pm_r,   3._pm_r,104._pm_r,  0.74_pm_r,101._pm_r, &
208.00_pm_r, 82298._pm_r,   2._pm_r,145._pm_r,  0.41_pm_r,158._pm_r,   4._pm_r,103._pm_r,  0.77_pm_r,102._pm_r, &
197.60_pm_r, 85252._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.70_pm_r, 88063._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.10_pm_r, 90824._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.90_pm_r, 93581._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.90_pm_r, 96350._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.70_pm_r, 99141._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.20_pm_r,101989._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.90_pm_r,104972._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
228.30_pm_r,108278._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
290.30_pm_r,112316._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
379.80_pm_r,117696._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
299.70_pm_r,   -99._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
275.70_pm_r,  4120._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
250.90_pm_r,  7981._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
224.30_pm_r, 11465._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
202.80_pm_r, 14596._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.20_pm_r, 17526._pm_r,   6._pm_r,137._pm_r,  0.87_pm_r,292._pm_r,   1._pm_r,253._pm_r,  0.50_pm_r, 43._pm_r, &
210.20_pm_r, 20538._pm_r,   5._pm_r,142._pm_r, 1.15_pm_r,307._pm_r,   1._pm_r,284._pm_r,  0.58_pm_r, 48._pm_r, &
217.20_pm_r, 23671._pm_r,   3._pm_r,146._pm_r, 1.32_pm_r,326._pm_r,   1._pm_r,355._pm_r,  0.50_pm_r, 57._pm_r, &
223.00_pm_r, 26893._pm_r,   1._pm_r,116._pm_r, 1.60_pm_r,345._pm_r,   1._pm_r, 29._pm_r,  0.37_pm_r, 77._pm_r, &
229.90_pm_r, 30208._pm_r,   2._pm_r, 10._pm_r, 1.91_pm_r,  0._pm_r,   1._pm_r, 48._pm_r,  0.28_pm_r,116._pm_r, &
240.40_pm_r, 33648._pm_r,   5._pm_r,  7._pm_r, 2.08_pm_r,  9._pm_r,   1._pm_r, 65._pm_r,  0.29_pm_r,151._pm_r, &
251.20_pm_r, 37252._pm_r,   8._pm_r,  9._pm_r, 1.99_pm_r, 13._pm_r,   1._pm_r, 81._pm_r,  0.26_pm_r,165._pm_r, &
261.20_pm_r, 41002._pm_r,  11._pm_r, 10._pm_r, 1.65_pm_r, 13._pm_r,   2._pm_r, 93._pm_r,  0.14_pm_r,159._pm_r, &
267.10_pm_r, 44879._pm_r,  13._pm_r,  9._pm_r, 1.24_pm_r,  2._pm_r,   2._pm_r, 93._pm_r,  0.16_pm_r, 57._pm_r, &
266.20_pm_r, 48791._pm_r,  14._pm_r,  8._pm_r,  0.76_pm_r,346._pm_r,   2._pm_r, 85._pm_r,  0.22_pm_r, 21._pm_r, &
260.10_pm_r, 52648._pm_r,  15._pm_r,  7._pm_r,  0.25_pm_r,244._pm_r,   2._pm_r, 75._pm_r,  0.26_pm_r,315._pm_r, &
250.70_pm_r, 56394._pm_r,  14._pm_r,  6._pm_r, 1.21_pm_r,187._pm_r,   1._pm_r, 62._pm_r,  0.60_pm_r,273._pm_r, &
240.30_pm_r, 59987._pm_r,  12._pm_r,  5._pm_r, 1.82_pm_r,189._pm_r,   1._pm_r, 23._pm_r,  0.62_pm_r,272._pm_r, &
229.80_pm_r, 63432._pm_r,   9._pm_r,  4._pm_r, 1.90_pm_r,193._pm_r,   1._pm_r,325._pm_r,  0.39_pm_r,274._pm_r, &
219.50_pm_r, 66717._pm_r,   6._pm_r,359._pm_r, 1.52_pm_r,201._pm_r,   1._pm_r,313._pm_r,  0.01_pm_r,333._pm_r, &
212.80_pm_r, 69879._pm_r,   5._pm_r,348._pm_r, 1.02_pm_r,216._pm_r,   1._pm_r,326._pm_r,  0.41_pm_r, 90._pm_r, &
210.80_pm_r, 72977._pm_r,   4._pm_r,334._pm_r,  0.72_pm_r,249._pm_r,   1._pm_r, 30._pm_r,  0.77_pm_r, 92._pm_r, &
209.90_pm_r, 76058._pm_r,   5._pm_r,323._pm_r,  0.74_pm_r,285._pm_r,   2._pm_r, 69._pm_r, 1.05_pm_r, 92._pm_r, &
210.00_pm_r, 79130._pm_r,   6._pm_r,318._pm_r,  0.88_pm_r,304._pm_r,   4._pm_r, 80._pm_r, 1.22_pm_r, 93._pm_r, &
206.70_pm_r, 82207._pm_r,   7._pm_r,317._pm_r,  0.97_pm_r,314._pm_r,   5._pm_r, 85._pm_r, 1.27_pm_r, 92._pm_r, &
198.10_pm_r, 85168._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.30_pm_r, 87995._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.80_pm_r, 90756._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.30_pm_r, 93504._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.40_pm_r, 96263._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.30_pm_r, 99045._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
190.80_pm_r,101886._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.60_pm_r,104864._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
227.50_pm_r,108162._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
288.30_pm_r,112176._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
375.90_pm_r,117509._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
295.20_pm_r,   -59._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
270.60_pm_r,  4086._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
245.20_pm_r,  7865._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.00_pm_r, 11288._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
207.70_pm_r, 14436._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
206.30_pm_r, 17451._pm_r,   9._pm_r,170._pm_r,  0.84_pm_r,340._pm_r,   2._pm_r,318._pm_r,  0.26_pm_r, 32._pm_r, &
211.80_pm_r, 20510._pm_r,   7._pm_r,170._pm_r, 1.29_pm_r,357._pm_r,   2._pm_r,330._pm_r,  0.26_pm_r, 45._pm_r, &
217.10_pm_r, 23654._pm_r,   5._pm_r,163._pm_r, 1.79_pm_r, 15._pm_r,   2._pm_r,339._pm_r,  0.22_pm_r, 86._pm_r, &
221.00_pm_r, 26861._pm_r,   3._pm_r,127._pm_r, 2.39_pm_r, 27._pm_r,   2._pm_r,348._pm_r,  0.32_pm_r,132._pm_r, &
226.40_pm_r, 30136._pm_r,   5._pm_r, 74._pm_r, 2.87_pm_r, 34._pm_r,   1._pm_r,360._pm_r,  0.49_pm_r,147._pm_r, &
234.60_pm_r, 33507._pm_r,   9._pm_r, 56._pm_r, 3.03_pm_r, 37._pm_r,   1._pm_r, 36._pm_r,  0.59_pm_r,146._pm_r, &
244.70_pm_r, 37018._pm_r,  13._pm_r, 50._pm_r, 2.71_pm_r, 35._pm_r,   1._pm_r, 94._pm_r,  0.56_pm_r,133._pm_r, &
255.40_pm_r, 40678._pm_r,  16._pm_r, 46._pm_r, 2.06_pm_r, 24._pm_r,   2._pm_r,105._pm_r,  0.53_pm_r,102._pm_r, &
263.30_pm_r, 44485._pm_r,  18._pm_r, 41._pm_r, 1.57_pm_r,358._pm_r,   3._pm_r, 98._pm_r,  0.67_pm_r, 69._pm_r, &
264.60_pm_r, 48358._pm_r,  20._pm_r, 36._pm_r, 1.36_pm_r,327._pm_r,   3._pm_r, 86._pm_r,  0.74_pm_r, 46._pm_r, &
258.70_pm_r, 52195._pm_r,  20._pm_r, 31._pm_r, 1.14_pm_r,284._pm_r,   4._pm_r, 74._pm_r,  0.72_pm_r, 11._pm_r, &
249.10_pm_r, 55919._pm_r,  19._pm_r, 27._pm_r, 1.46_pm_r,230._pm_r,   4._pm_r, 59._pm_r,  0.87_pm_r,326._pm_r, &
240.00_pm_r, 59496._pm_r,  16._pm_r, 24._pm_r, 2.09_pm_r,221._pm_r,   4._pm_r, 41._pm_r,  0.96_pm_r,319._pm_r, &
232.20_pm_r, 62955._pm_r,  13._pm_r, 20._pm_r, 2.17_pm_r,221._pm_r,   5._pm_r, 24._pm_r,  0.79_pm_r,318._pm_r, &
224.20_pm_r, 66295._pm_r,  10._pm_r, 14._pm_r, 1.76_pm_r,227._pm_r,   5._pm_r, 16._pm_r,  0.41_pm_r,327._pm_r, &
217.70_pm_r, 69529._pm_r,   9._pm_r,  5._pm_r, 1.24_pm_r,243._pm_r,   5._pm_r, 14._pm_r,  0.15_pm_r, 52._pm_r, &
214.80_pm_r, 72693._pm_r,   8._pm_r,355._pm_r,  0.92_pm_r,274._pm_r,   6._pm_r, 18._pm_r,  0.47_pm_r,106._pm_r, &
212.70_pm_r, 75824._pm_r,   9._pm_r,347._pm_r,  0.96_pm_r,307._pm_r,   6._pm_r, 27._pm_r,  0.75_pm_r,113._pm_r, &
211.40_pm_r, 78927._pm_r,  10._pm_r,343._pm_r, 1.11_pm_r,324._pm_r,   6._pm_r, 40._pm_r,  0.94_pm_r,116._pm_r, &
208.10_pm_r, 82015._pm_r,  12._pm_r,341._pm_r, 1.21_pm_r,332._pm_r,   6._pm_r, 53._pm_r, 1.02_pm_r,116._pm_r, &
200.50_pm_r, 85011._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.40_pm_r, 87876._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.40_pm_r, 90653._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.20_pm_r, 93403._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
186.20_pm_r, 96157._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.80_pm_r, 98941._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.00_pm_r,101793._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.60_pm_r,104794._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
229.20_pm_r,108119._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
287.60_pm_r,112140._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
372.40_pm_r,117435._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
288.30_pm_r,   -27._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
263.50_pm_r,  4012._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
238.10_pm_r,  7684._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
219.20_pm_r, 11031._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
213.00_pm_r, 14195._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
212.40_pm_r, 17305._pm_r,   8._pm_r,228._pm_r, 1.27_pm_r,138._pm_r,   5._pm_r, 18._pm_r,  0.78_pm_r,273._pm_r, &
214.10_pm_r, 20426._pm_r,   8._pm_r,210._pm_r, 2.05_pm_r,127._pm_r,   5._pm_r,  2._pm_r, 1.03_pm_r,265._pm_r, &
216.50_pm_r, 23582._pm_r,   9._pm_r,185._pm_r, 2.92_pm_r,116._pm_r,   4._pm_r,342._pm_r, 1.11_pm_r,251._pm_r, &
217.40_pm_r, 26758._pm_r,  11._pm_r,161._pm_r, 3.68_pm_r,106._pm_r,   4._pm_r,321._pm_r, 1.06_pm_r,228._pm_r, &
220.50_pm_r, 29962._pm_r,  15._pm_r,141._pm_r, 4.08_pm_r, 96._pm_r,   4._pm_r,301._pm_r, 1.03_pm_r,196._pm_r, &
227.50_pm_r, 33238._pm_r,  19._pm_r,127._pm_r, 3.92_pm_r, 85._pm_r,   4._pm_r,280._pm_r, 1.14_pm_r,162._pm_r, &
236.30_pm_r, 36636._pm_r,  23._pm_r,117._pm_r, 3.21_pm_r, 71._pm_r,   3._pm_r,251._pm_r, 1.31_pm_r,135._pm_r, &
246.90_pm_r, 40168._pm_r,  25._pm_r,110._pm_r, 2.28_pm_r, 46._pm_r,   2._pm_r,203._pm_r, 1.46_pm_r,112._pm_r, &
257.50_pm_r, 43870._pm_r,  26._pm_r,103._pm_r, 2.05_pm_r, 18._pm_r,   3._pm_r,154._pm_r, 1.41_pm_r, 93._pm_r, &
260.90_pm_r, 47675._pm_r,  26._pm_r, 96._pm_r, 2.34_pm_r,  1._pm_r,   4._pm_r,126._pm_r, 1.25_pm_r, 76._pm_r, &
256.80_pm_r, 51471._pm_r,  26._pm_r, 89._pm_r, 2.34_pm_r,351._pm_r,   5._pm_r,109._pm_r,  0.90_pm_r, 50._pm_r, &
248.70_pm_r, 55179._pm_r,  25._pm_r, 82._pm_r, 1.77_pm_r,336._pm_r,   5._pm_r, 97._pm_r,  0.71_pm_r,355._pm_r, &
239.90_pm_r, 58753._pm_r,  24._pm_r, 77._pm_r, 1.84_pm_r,293._pm_r,   5._pm_r, 84._pm_r,  0.93_pm_r,334._pm_r, &
233.10_pm_r, 62216._pm_r,  21._pm_r, 74._pm_r, 2.24_pm_r,274._pm_r,   4._pm_r, 66._pm_r,  0.95_pm_r,327._pm_r, &
227.80_pm_r, 65589._pm_r,  18._pm_r, 71._pm_r, 2.30_pm_r,271._pm_r,   4._pm_r, 49._pm_r,  0.76_pm_r,329._pm_r, &
223.30_pm_r, 68892._pm_r,  15._pm_r, 66._pm_r, 2.11_pm_r,276._pm_r,   5._pm_r, 38._pm_r,  0.48_pm_r,339._pm_r, &
220.10_pm_r, 72137._pm_r,  13._pm_r, 58._pm_r, 1.88_pm_r,286._pm_r,   5._pm_r, 34._pm_r,  0.28_pm_r, 19._pm_r, &
217.60_pm_r, 75343._pm_r,  11._pm_r, 48._pm_r, 1.72_pm_r,297._pm_r,   5._pm_r, 35._pm_r,  0.32_pm_r, 70._pm_r, &
215.80_pm_r, 78514._pm_r,  11._pm_r, 35._pm_r, 1.64_pm_r,307._pm_r,   6._pm_r, 39._pm_r,  0.44_pm_r, 91._pm_r, &
212.30_pm_r, 81661._pm_r,  11._pm_r, 23._pm_r, 1.53_pm_r,314._pm_r,   6._pm_r, 45._pm_r,  0.52_pm_r, 97._pm_r, &
205.00_pm_r, 84721._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
196.00_pm_r, 87648._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.10_pm_r, 90462._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.10_pm_r, 93229._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
187.10_pm_r, 95993._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.40_pm_r, 98792._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
195.00_pm_r,101675._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
207.50_pm_r,104726._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
232.90_pm_r,108108._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
287.40_pm_r,112158._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
369.30_pm_r,117416._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
281.40_pm_r,   -69._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
256.80_pm_r,  3867._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
232.60_pm_r,  7446._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.70_pm_r, 10747._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
216.70_pm_r, 13931._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
216.10_pm_r, 17100._pm_r,  12._pm_r,276._pm_r, 3.45_pm_r,183._pm_r,   8._pm_r, 34._pm_r, 1.44_pm_r,300._pm_r, &
215.50_pm_r, 20260._pm_r,  13._pm_r,248._pm_r, 5.14_pm_r,176._pm_r,   8._pm_r, 16._pm_r, 1.92_pm_r,294._pm_r, &
215.20_pm_r, 23414._pm_r,  18._pm_r,219._pm_r, 6.66_pm_r,168._pm_r,   9._pm_r,356._pm_r, 2.08_pm_r,284._pm_r, &
213.90_pm_r, 26557._pm_r,  25._pm_r,199._pm_r, 7.64_pm_r,159._pm_r,  10._pm_r,339._pm_r, 1.88_pm_r,267._pm_r, &
215.00_pm_r, 29695._pm_r,  34._pm_r,185._pm_r, 7.85_pm_r,147._pm_r,  10._pm_r,325._pm_r, 1.60_pm_r,234._pm_r, &
219.30_pm_r, 32870._pm_r,  43._pm_r,174._pm_r, 7.25_pm_r,134._pm_r,  10._pm_r,312._pm_r, 1.73_pm_r,194._pm_r, &
227.50_pm_r, 36140._pm_r,  50._pm_r,166._pm_r, 5.82_pm_r,118._pm_r,   8._pm_r,298._pm_r, 2.05_pm_r,166._pm_r, &
238.20_pm_r, 39546._pm_r,  54._pm_r,160._pm_r, 4.14_pm_r, 98._pm_r,   6._pm_r,278._pm_r, 2.21_pm_r,146._pm_r, &
248.90_pm_r, 43119._pm_r,  56._pm_r,155._pm_r, 3.23_pm_r, 82._pm_r,   4._pm_r,248._pm_r, 2.06_pm_r,127._pm_r, &
255.80_pm_r, 46819._pm_r,  57._pm_r,150._pm_r, 3.32_pm_r, 68._pm_r,   3._pm_r,200._pm_r, 1.87_pm_r,112._pm_r, &
256.20_pm_r, 50576._pm_r,  57._pm_r,145._pm_r, 3.93_pm_r, 54._pm_r,   4._pm_r,160._pm_r, 1.37_pm_r,101._pm_r, &
250.40_pm_r, 54292._pm_r,  57._pm_r,138._pm_r, 4.55_pm_r, 40._pm_r,   5._pm_r,144._pm_r,  0.50_pm_r, 85._pm_r, &
243.10_pm_r, 57903._pm_r,  55._pm_r,132._pm_r, 4.33_pm_r, 13._pm_r,   4._pm_r,139._pm_r,  0.45_pm_r,338._pm_r, &
236.50_pm_r, 61414._pm_r,  51._pm_r,127._pm_r, 4.38_pm_r,350._pm_r,   3._pm_r,137._pm_r,  0.89_pm_r,322._pm_r, &
231.90_pm_r, 64841._pm_r,  46._pm_r,122._pm_r, 4.36_pm_r,333._pm_r,   2._pm_r,134._pm_r,  0.99_pm_r,323._pm_r, &
228.20_pm_r, 68211._pm_r,  40._pm_r,119._pm_r, 4.22_pm_r,321._pm_r,   1._pm_r,108._pm_r,  0.89_pm_r,332._pm_r, &
225.10_pm_r, 71528._pm_r,  34._pm_r,115._pm_r, 4.00_pm_r,312._pm_r,   1._pm_r, 19._pm_r,  0.73_pm_r,349._pm_r, &
222.60_pm_r, 74806._pm_r,  29._pm_r,113._pm_r, 3.79_pm_r,304._pm_r,   2._pm_r,  9._pm_r,  0.65_pm_r, 11._pm_r, &
220.50_pm_r, 78052._pm_r,  24._pm_r,111._pm_r, 3.56_pm_r,299._pm_r,   3._pm_r, 13._pm_r,  0.64_pm_r, 29._pm_r, &
217.00_pm_r, 81265._pm_r,  19._pm_r,110._pm_r, 3.26_pm_r,294._pm_r,   4._pm_r, 18._pm_r,  0.64_pm_r, 42._pm_r, &
209.80_pm_r, 84393._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.50_pm_r, 87394._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
192.20_pm_r, 90261._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
189.10_pm_r, 93061._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
188.90_pm_r, 95849._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.90_pm_r, 98678._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.00_pm_r,101605._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
212.50_pm_r,104723._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
237.50_pm_r,108179._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
286.80_pm_r,112264._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
366.50_pm_r,117478._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
272.30_pm_r,   -80._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
251.10_pm_r,  3745._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
228.20_pm_r,  7247._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.90_pm_r, 10514._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.20_pm_r, 13708._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.00_pm_r, 16897._pm_r,  16._pm_r,279._pm_r, 4.62_pm_r,209._pm_r,  11._pm_r, 29._pm_r, 1.22_pm_r,324._pm_r, &
215.20_pm_r, 20063._pm_r,  20._pm_r,255._pm_r, 7.05_pm_r,202._pm_r,  12._pm_r, 19._pm_r, 1.70_pm_r,319._pm_r, &
212.80_pm_r, 23197._pm_r,  28._pm_r,234._pm_r, 9.45_pm_r,193._pm_r,  13._pm_r,  8._pm_r, 1.95_pm_r,310._pm_r, &
209.40_pm_r, 26289._pm_r,  40._pm_r,218._pm_r,10.90_pm_r,181._pm_r,  14._pm_r,358._pm_r, 1.80_pm_r,296._pm_r, &
209.40_pm_r, 29351._pm_r,  53._pm_r,206._pm_r,11.50_pm_r,168._pm_r,  15._pm_r,350._pm_r, 1.50_pm_r,267._pm_r, &
213.70_pm_r, 32445._pm_r,  66._pm_r,195._pm_r,11.20_pm_r,153._pm_r,  15._pm_r,342._pm_r, 1.52_pm_r,228._pm_r, &
221.60_pm_r, 35632._pm_r,  77._pm_r,186._pm_r, 9.90_pm_r,138._pm_r,  14._pm_r,334._pm_r, 1.80_pm_r,199._pm_r, &
231.80_pm_r, 38947._pm_r,  85._pm_r,179._pm_r, 8.08_pm_r,121._pm_r,  11._pm_r,325._pm_r, 1.97_pm_r,182._pm_r, &
243.20_pm_r, 42431._pm_r,  90._pm_r,173._pm_r, 6.59_pm_r,110._pm_r,   9._pm_r,317._pm_r, 1.84_pm_r,167._pm_r, &
252.90_pm_r, 46064._pm_r,  94._pm_r,168._pm_r, 6.20_pm_r, 96._pm_r,   7._pm_r,308._pm_r, 1.62_pm_r,154._pm_r, &
256.60_pm_r, 49805._pm_r,  96._pm_r,162._pm_r, 6.62_pm_r, 77._pm_r,   5._pm_r,298._pm_r, 1.29_pm_r,145._pm_r, &
252.10_pm_r, 53538._pm_r,  95._pm_r,156._pm_r, 7.53_pm_r, 54._pm_r,   4._pm_r,288._pm_r,  0.78_pm_r,140._pm_r, &
246.50_pm_r, 57188._pm_r,  91._pm_r,150._pm_r, 7.62_pm_r, 28._pm_r,   3._pm_r,281._pm_r,  0.28_pm_r,116._pm_r, &
241.50_pm_r, 60762._pm_r,  84._pm_r,144._pm_r, 7.81_pm_r,  8._pm_r,   3._pm_r,285._pm_r,  0.28_pm_r, 19._pm_r, &
237.40_pm_r, 64266._pm_r,  75._pm_r,139._pm_r, 7.58_pm_r,354._pm_r,   3._pm_r,297._pm_r,  0.51_pm_r,359._pm_r, &
233.30_pm_r, 67713._pm_r,  66._pm_r,135._pm_r, 7.03_pm_r,342._pm_r,   3._pm_r,309._pm_r,  0.60_pm_r,359._pm_r, &
230.10_pm_r, 71104._pm_r,  57._pm_r,131._pm_r, 6.36_pm_r,331._pm_r,   4._pm_r,319._pm_r,  0.58_pm_r,360._pm_r, &
227.10_pm_r, 74454._pm_r,  48._pm_r,128._pm_r, 5.80_pm_r,320._pm_r,   5._pm_r,326._pm_r,  0.52_pm_r,  0._pm_r, &
223.90_pm_r, 77752._pm_r,  40._pm_r,127._pm_r, 5.32_pm_r,312._pm_r,   5._pm_r,330._pm_r,  0.45_pm_r,  3._pm_r, &
220.40_pm_r, 81009._pm_r,  33._pm_r,126._pm_r, 4.82_pm_r,305._pm_r,   6._pm_r,334._pm_r,  0.37_pm_r,  3._pm_r, &
213.90_pm_r, 84190._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
204.80_pm_r, 87260._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
195.80_pm_r, 90185._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.50_pm_r, 93027._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
191.40_pm_r, 95849._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.90_pm_r, 98715._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
203.30_pm_r,101696._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.80_pm_r,104885._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
241.90_pm_r,108415._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
285.20_pm_r,112524._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
363.90_pm_r,117687._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
261.30_pm_r,    -3._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
246.60_pm_r,  3706._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.00_pm_r,  7149._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
218.10_pm_r, 10385._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.30_pm_r, 13564._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.20_pm_r, 16733._pm_r,  13._pm_r,287._pm_r, 5.27_pm_r,224._pm_r,   8._pm_r, 18._pm_r,  0.62_pm_r,  9._pm_r, &
212.30_pm_r, 19864._pm_r,  19._pm_r,260._pm_r, 7.78_pm_r,218._pm_r,   9._pm_r, 17._pm_r,  0.88_pm_r,  4._pm_r, &
209.40_pm_r, 22952._pm_r,  29._pm_r,241._pm_r, 9.94_pm_r,208._pm_r,  11._pm_r, 15._pm_r, 1.02_pm_r,355._pm_r, &
204.80_pm_r, 25986._pm_r,  42._pm_r,228._pm_r,10.99_pm_r,196._pm_r,  12._pm_r, 12._pm_r,  0.96_pm_r,340._pm_r, &
204.40_pm_r, 28976._pm_r,  56._pm_r,217._pm_r,10.96_pm_r,178._pm_r,  13._pm_r,  8._pm_r,  0.80_pm_r,310._pm_r, &
208.40_pm_r, 31995._pm_r,  67._pm_r,207._pm_r,10.80_pm_r,159._pm_r,  13._pm_r,  3._pm_r,  0.85_pm_r,270._pm_r, &
215.60_pm_r, 35099._pm_r,  77._pm_r,197._pm_r,10.31_pm_r,140._pm_r,  13._pm_r,357._pm_r, 1.06_pm_r,241._pm_r, &
225.90_pm_r, 38326._pm_r,  84._pm_r,188._pm_r, 9.17_pm_r,125._pm_r,  12._pm_r,351._pm_r, 1.20_pm_r,224._pm_r, &
240.00_pm_r, 41742._pm_r,  89._pm_r,181._pm_r, 7.24_pm_r,118._pm_r,  11._pm_r,344._pm_r, 1.20_pm_r,203._pm_r, &
253.30_pm_r, 45352._pm_r,  93._pm_r,176._pm_r, 6.14_pm_r,106._pm_r,   9._pm_r,339._pm_r, 1.17_pm_r,188._pm_r, &
260.00_pm_r, 49124._pm_r,  95._pm_r,170._pm_r, 6.06_pm_r, 82._pm_r,   8._pm_r,334._pm_r,  0.93_pm_r,182._pm_r, &
256.70_pm_r, 52916._pm_r,  94._pm_r,165._pm_r, 7.19_pm_r, 54._pm_r,   7._pm_r,330._pm_r,  0.44_pm_r,189._pm_r, &
251.20_pm_r, 56635._pm_r,  88._pm_r,159._pm_r, 7.50_pm_r, 32._pm_r,   7._pm_r,328._pm_r,  0.11_pm_r,301._pm_r, &
244.70_pm_r, 60269._pm_r,  81._pm_r,153._pm_r, 7.56_pm_r, 16._pm_r,   7._pm_r,329._pm_r,  0.40_pm_r,353._pm_r, &
238.10_pm_r, 63801._pm_r,  73._pm_r,148._pm_r, 7.08_pm_r,  4._pm_r,   8._pm_r,331._pm_r,  0.55_pm_r,  5._pm_r, &
232.90_pm_r, 67250._pm_r,  64._pm_r,144._pm_r, 6.34_pm_r,354._pm_r,   9._pm_r,335._pm_r,  0.58_pm_r, 16._pm_r, &
229.40_pm_r, 70633._pm_r,  57._pm_r,140._pm_r, 5.57_pm_r,344._pm_r,   9._pm_r,339._pm_r,  0.54_pm_r, 29._pm_r, &
227.40_pm_r, 73976._pm_r,  49._pm_r,137._pm_r, 4.97_pm_r,333._pm_r,  10._pm_r,342._pm_r,  0.51_pm_r, 45._pm_r, &
226.50_pm_r, 77298._pm_r,  43._pm_r,135._pm_r, 4.50_pm_r,324._pm_r,  10._pm_r,346._pm_r,  0.49_pm_r, 57._pm_r, &
224.10_pm_r, 80609._pm_r,  36._pm_r,134._pm_r, 4.05_pm_r,316._pm_r,  10._pm_r,350._pm_r,  0.47_pm_r, 67._pm_r, &
217.70_pm_r, 83844._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
208.50_pm_r, 86971._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
199.00_pm_r, 89948._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
194.00_pm_r, 92829._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
193.90_pm_r, 95686._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
197.90_pm_r, 98589._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
207.20_pm_r,101619._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.30_pm_r,104871._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
245.10_pm_r,108460._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
283.10_pm_r,112578._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
361.80_pm_r,117690._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
255.70_pm_r,    53._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
243.80_pm_r,  3699._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
222.90_pm_r,  7105._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
217.00_pm_r, 10316._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
215.70_pm_r, 13474._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
212.50_pm_r, 16611._pm_r,   7._pm_r,283._pm_r, 3.42_pm_r,232._pm_r,   4._pm_r, 12._pm_r,  0.21_pm_r, 98._pm_r, &
208.90_pm_r, 19697._pm_r,  12._pm_r,259._pm_r, 4.97_pm_r,226._pm_r,   4._pm_r, 18._pm_r,  0.28_pm_r, 92._pm_r, &
206.20_pm_r, 22738._pm_r,  19._pm_r,244._pm_r, 6.24_pm_r,217._pm_r,   4._pm_r, 23._pm_r,  0.31_pm_r, 79._pm_r, &
200.50_pm_r, 25719._pm_r,  28._pm_r,233._pm_r, 6.73_pm_r,204._pm_r,   5._pm_r, 27._pm_r,  0.27_pm_r, 54._pm_r, &
199.60_pm_r, 28640._pm_r,  36._pm_r,223._pm_r, 6.44_pm_r,184._pm_r,   5._pm_r, 27._pm_r,  0.25_pm_r,  9._pm_r, &
203.40_pm_r, 31587._pm_r,  42._pm_r,213._pm_r, 6.37_pm_r,161._pm_r,   5._pm_r, 24._pm_r,  0.34_pm_r,330._pm_r, &
210.40_pm_r, 34615._pm_r,  47._pm_r,203._pm_r, 6.36_pm_r,141._pm_r,   5._pm_r, 19._pm_r,  0.42_pm_r,306._pm_r, &
221.30_pm_r, 37769._pm_r,  51._pm_r,194._pm_r, 5.90_pm_r,126._pm_r,   6._pm_r, 13._pm_r,  0.44_pm_r,286._pm_r, &
237.30_pm_r, 41132._pm_r,  54._pm_r,186._pm_r, 4.44_pm_r,117._pm_r,   6._pm_r,  8._pm_r,  0.20_pm_r,257._pm_r, &
252.80_pm_r, 44718._pm_r,  55._pm_r,180._pm_r, 3.52_pm_r, 97._pm_r,   5._pm_r,  6._pm_r,  0.14_pm_r,201._pm_r, &
261.60_pm_r, 48501._pm_r,  55._pm_r,175._pm_r, 3.68_pm_r, 67._pm_r,   5._pm_r,  6._pm_r,  0.19_pm_r,206._pm_r, &
259.10_pm_r, 52322._pm_r,  52._pm_r,169._pm_r, 4.66_pm_r, 43._pm_r,   5._pm_r,  3._pm_r,  0.35_pm_r,226._pm_r, &
254.10_pm_r, 56082._pm_r,  48._pm_r,164._pm_r, 4.58_pm_r, 29._pm_r,   4._pm_r,357._pm_r,  0.50_pm_r,234._pm_r, &
246.80_pm_r, 59754._pm_r,  43._pm_r,158._pm_r, 4.21_pm_r, 17._pm_r,   4._pm_r,348._pm_r,  0.52_pm_r,238._pm_r, &
238.90_pm_r, 63309._pm_r,  38._pm_r,153._pm_r, 3.65_pm_r,  7._pm_r,   4._pm_r,337._pm_r,  0.44_pm_r,243._pm_r, &
233.10_pm_r, 66765._pm_r,  34._pm_r,149._pm_r, 3.07_pm_r,357._pm_r,   4._pm_r,329._pm_r,  0.32_pm_r,256._pm_r, &
229.40_pm_r, 70148._pm_r,  30._pm_r,146._pm_r, 2.58_pm_r,348._pm_r,   4._pm_r,324._pm_r,  0.21_pm_r,281._pm_r, &
227.90_pm_r, 73494._pm_r,  27._pm_r,144._pm_r, 2.21_pm_r,337._pm_r,   4._pm_r,323._pm_r,  0.19_pm_r,325._pm_r, &
228.30_pm_r, 76834._pm_r,  24._pm_r,143._pm_r, 1.95_pm_r,327._pm_r,   5._pm_r,325._pm_r,  0.25_pm_r,354._pm_r, &
226.60_pm_r, 80182._pm_r,  21._pm_r,143._pm_r, 1.73_pm_r,318._pm_r,   5._pm_r,327._pm_r,  0.29_pm_r,  8._pm_r, &
220.40_pm_r, 83453._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
211.10_pm_r, 86618._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
201.20_pm_r, 89631._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
195.90_pm_r, 92539._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
195.70_pm_r, 95422._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
200.10_pm_r, 98352._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
209.90_pm_r,101418._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
225.20_pm_r,104711._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
246.80_pm_r,108338._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
281.10_pm_r,112453._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r, &
360.30_pm_r,117528._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  0.00_pm_r,  0._pm_reel /)


  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_novembre = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_novembre,(/10,nb_alt,nb_lat/))


  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)


end subroutine cps_atmi_novembre

subroutine cps_atmi_decembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_decembre
!
!$Resume
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de DECEMBRE
!
!$Description
!  Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de DECEMBRE
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_decembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1 
!.    real(pm_reel), dimension(:,:) :: phi1 
!.    real(pm_reel), dimension(:,:) :: t1 
!.    real(pm_reel), dimension(:,:) :: phit1 
!.    real(pm_reel), dimension(:,:) :: z2 
!.    real(pm_reel), dimension(:,:) :: phi2 
!.    real(pm_reel), dimension(:,:) :: t2 
!.    real(pm_reel), dimension(:,:) :: phit2 
!.    integer :: retour
!
!$Arguments             
!>S     tbar    :<pm_reel,DIM=(:,:)>   temperatures 
!>S     zbar    :<pm_reel,DIM=(:,:)>   altitudes
!>S     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>S     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>S     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>S     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>S     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>S     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>S     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>S     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>S     retour  :<integer>
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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


  ! Modules
  ! =======

  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

    integer, parameter  :: nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
    integer, parameter  :: nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)

    ! Pour des questions de longueur des lignes, redéclaration de la preéision pour les réels
    integer, parameter :: pm_r = pm_reel

  ! Autres declarations
  ! -------------------
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_80= (/ &
264.10_pm_r,-113._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.20_pm_r,3633._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.50_pm_r,7105._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.90_pm_r,10392._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.10_pm_r,13706._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
234.20_pm_r,17109._pm_r,6._pm_r,198._pm_r,0.11_pm_r,267._pm_r,0._pm_r,178._pm_r,0.04_pm_r,360._pm_r, &
236.30_pm_r,20556._pm_r,6._pm_r,200._pm_r,0.07_pm_r,297._pm_r,0._pm_r,179._pm_r,0.05_pm_r,349._pm_r, &
236.40_pm_r,24019._pm_r,5._pm_r,200._pm_r,0.11_pm_r,41._pm_r,0._pm_r,186._pm_r,0.05_pm_r,315._pm_r, &
237.70_pm_r,27485._pm_r,5._pm_r,198._pm_r,0.32_pm_r,58._pm_r,0._pm_r,201._pm_r,0.06_pm_r,270._pm_r, &
249.90_pm_r,31047._pm_r,5._pm_r,193._pm_r,0.53_pm_r,62._pm_r,0._pm_r,214._pm_r,0.09_pm_r,238._pm_r, &
263.10_pm_r,34805._pm_r,4._pm_r,183._pm_r,0.69_pm_r,64._pm_r,1._pm_r,219._pm_r,0.15_pm_r,222._pm_r, &
274.80_pm_r,38749._pm_r,4._pm_r,169._pm_r,0.73_pm_r,64._pm_r,1._pm_r,218._pm_r,0.18_pm_r,213._pm_r, &
284.40_pm_r,42844._pm_r,4._pm_r,154._pm_r,0.65_pm_r,61._pm_r,1._pm_r,216._pm_r,0.21_pm_r,208._pm_r, &
288.90_pm_r,47049._pm_r,4._pm_r,142._pm_r,0.36_pm_r,46._pm_r,1._pm_r,213._pm_r,0.26_pm_r,201._pm_r, &
287.50_pm_r,51278._pm_r,4._pm_r,137._pm_r,0.18_pm_r,351._pm_r,2._pm_r,210._pm_r,0.35_pm_r,198._pm_r, &
279.30_pm_r,55431._pm_r,3._pm_r,136._pm_r,0.22_pm_r,312._pm_r,2._pm_r,207._pm_r,0.37_pm_r,199._pm_r, &
267.10_pm_r,59438._pm_r,3._pm_r,136._pm_r,0.20_pm_r,336._pm_r,3._pm_r,206._pm_r,0.27_pm_r,202._pm_r, &
251.30_pm_r,63231._pm_r,3._pm_r,133._pm_r,0.16_pm_r,18._pm_r,3._pm_r,205._pm_r,0.10_pm_r,169._pm_r, &
236.10_pm_r,66803._pm_r,3._pm_r,128._pm_r,0.16_pm_r,55._pm_r,3._pm_r,202._pm_r,0.16_pm_r,94._pm_r, &
221.80_pm_r,70153._pm_r,3._pm_r,125._pm_r,0.14_pm_r,100._pm_r,3._pm_r,196._pm_r,0.31_pm_r,86._pm_r, &
206.20_pm_r,73295._pm_r,3._pm_r,124._pm_r,0.18_pm_r,139._pm_r,3._pm_r,186._pm_r,0.39_pm_r,89._pm_r, &
189.90_pm_r,76190._pm_r,3._pm_r,127._pm_r,0.21_pm_r,159._pm_r,3._pm_r,174._pm_r,0.42_pm_r,95._pm_r, &
174.00_pm_r,78858._pm_r,4._pm_r,130._pm_r,0.23_pm_r,170._pm_r,3._pm_r,163._pm_r,0.41_pm_r,100._pm_r, &
160.30_pm_r,81295._pm_r,4._pm_r,133._pm_r,0.22_pm_r,175._pm_r,3._pm_r,155._pm_r,0.36_pm_r,101._pm_r, &
147.10_pm_r,83547._pm_r,4._pm_r,137._pm_r,0.20_pm_r,180._pm_r,4._pm_r,149._pm_r,0.30_pm_r,104._pm_r, &
137.20_pm_r,85569._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
135.70_pm_r,87527._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
140.10_pm_r,89537._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
149.20_pm_r,91655._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
163.60_pm_r,93953._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.70_pm_r,96534._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.40_pm_r,99557._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
263.40_pm_r,103213._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
297.30_pm_r,107553._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
321.50_pm_r,112376._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
412.90_pm_r,118213._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_70= (/ &
269.90_pm_r,-139._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
251.10_pm_r,3665._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.00_pm_r,7164._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.30_pm_r,10459._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.50_pm_r,13758._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
232.10_pm_r,17134._pm_r,10._pm_r,194._pm_r,0.32_pm_r,300._pm_r,1._pm_r,216._pm_r,0.05_pm_r,155._pm_r, &
234.30_pm_r,20551._pm_r,10._pm_r,197._pm_r,0.35_pm_r,315._pm_r,1._pm_r,207._pm_r,0.08_pm_r,148._pm_r, &
235.10_pm_r,23988._pm_r,9._pm_r,199._pm_r,0.33_pm_r,350._pm_r,1._pm_r,197._pm_r,0.10_pm_r,148._pm_r, &
238.00_pm_r,27445._pm_r,9._pm_r,199._pm_r,0.45_pm_r,34._pm_r,1._pm_r,188._pm_r,0.11_pm_r,144._pm_r, &
248.90_pm_r,31005._pm_r,8._pm_r,196._pm_r,0.70_pm_r,56._pm_r,1._pm_r,181._pm_r,0.11_pm_r,141._pm_r, &
261.30_pm_r,34740._pm_r,7._pm_r,189._pm_r,0.92_pm_r,66._pm_r,1._pm_r,175._pm_r,0.10_pm_r,137._pm_r, &
272.70_pm_r,38654._pm_r,7._pm_r,178._pm_r,0.98_pm_r,69._pm_r,1._pm_r,170._pm_r,0.08_pm_r,128._pm_r, &
281.90_pm_r,42716._pm_r,6._pm_r,166._pm_r,0.86_pm_r,70._pm_r,1._pm_r,167._pm_r,0.05_pm_r,114._pm_r, &
285.70_pm_r,46880._pm_r,6._pm_r,157._pm_r,0.49_pm_r,54._pm_r,1._pm_r,165._pm_r,0.02_pm_r,113._pm_r, &
283.60_pm_r,51057._pm_r,6._pm_r,153._pm_r,0.23_pm_r,4._pm_r,1._pm_r,165._pm_r,0.02_pm_r,198._pm_r, &
275.60_pm_r,55154._pm_r,6._pm_r,153._pm_r,0.21_pm_r,320._pm_r,1._pm_r,166._pm_r,0.03_pm_r,261._pm_r, &
264.30_pm_r,59113._pm_r,5._pm_r,153._pm_r,0.11_pm_r,3._pm_r,1._pm_r,169._pm_r,0.07_pm_r,298._pm_r, &
250.00_pm_r,62875._pm_r,5._pm_r,152._pm_r,0.12_pm_r,33._pm_r,1._pm_r,174._pm_r,0.12_pm_r,322._pm_r, &
235.90_pm_r,66436._pm_r,5._pm_r,150._pm_r,0.12_pm_r,31._pm_r,1._pm_r,180._pm_r,0.15_pm_r,330._pm_r, &
222.30_pm_r,69788._pm_r,5._pm_r,149._pm_r,0.09_pm_r,348._pm_r,1._pm_r,188._pm_r,0.14_pm_r,333._pm_r, &
207.80_pm_r,72943._pm_r,5._pm_r,149._pm_r,0.14_pm_r,299._pm_r,1._pm_r,199._pm_r,0.11_pm_r,339._pm_r, &
192.40_pm_r,75869._pm_r,5._pm_r,151._pm_r,0.20_pm_r,283._pm_r,1._pm_r,210._pm_r,0.08_pm_r,341._pm_r, &
177.00_pm_r,78577._pm_r,4._pm_r,154._pm_r,0.23_pm_r,279._pm_r,0._pm_r,220._pm_r,0.06_pm_r,345._pm_r, &
163.60_pm_r,81061._pm_r,4._pm_r,158._pm_r,0.23_pm_r,274._pm_r,0._pm_r,227._pm_r,0.03_pm_r,338._pm_r, &
151.20_pm_r,83363._pm_r,4._pm_r,162._pm_r,0.21_pm_r,273._pm_r,0._pm_r,230._pm_r,0.02_pm_r,0._pm_r, &
142.40_pm_r,85466._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
141.10_pm_r,87509._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
144.70_pm_r,89593._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
152.90_pm_r,91773._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
166.20_pm_r,94119._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.30_pm_r,96727._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.00_pm_r,99740._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
256.80_pm_r,103329._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
292.20_pm_r,107572._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
322.30_pm_r,112367._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
410.50_pm_r,118196._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_60= (/ &
275.70_pm_r,-127._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
254.80_pm_r,3750._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
231.00_pm_r,7299._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.70_pm_r,10615._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.80_pm_r,13892._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
228.00_pm_r,17219._pm_r,11._pm_r,196._pm_r,0.37_pm_r,334._pm_r,1._pm_r,251._pm_r,0.11_pm_r,156._pm_r, &
229.60_pm_r,20569._pm_r,11._pm_r,198._pm_r,0.49_pm_r,346._pm_r,1._pm_r,239._pm_r,0.16_pm_r,153._pm_r, &
231.80_pm_r,23946._pm_r,10._pm_r,200._pm_r,0.59_pm_r,2._pm_r,1._pm_r,223._pm_r,0.19_pm_r,151._pm_r, &
236.30_pm_r,27369._pm_r,9._pm_r,201._pm_r,0.66_pm_r,21._pm_r,1._pm_r,209._pm_r,0.20_pm_r,150._pm_r, &
245.60_pm_r,30896._pm_r,8._pm_r,199._pm_r,0.75_pm_r,39._pm_r,1._pm_r,197._pm_r,0.18_pm_r,147._pm_r, &
257.40_pm_r,34575._pm_r,7._pm_r,195._pm_r,0.81_pm_r,53._pm_r,1._pm_r,189._pm_r,0.15_pm_r,143._pm_r, &
269.10_pm_r,38436._pm_r,6._pm_r,188._pm_r,0.79_pm_r,61._pm_r,2._pm_r,183._pm_r,0.11_pm_r,131._pm_r, &
278.70_pm_r,42448._pm_r,6._pm_r,179._pm_r,0.68_pm_r,65._pm_r,2._pm_r,179._pm_r,0.07_pm_r,117._pm_r, &
282.40_pm_r,46566._pm_r,6._pm_r,172._pm_r,0.39_pm_r,58._pm_r,2._pm_r,177._pm_r,0.04_pm_r,82._pm_r, &
279.50_pm_r,50687._pm_r,5._pm_r,168._pm_r,0.11_pm_r,45._pm_r,2._pm_r,175._pm_r,0.07_pm_r,82._pm_r, &
271.50_pm_r,54724._pm_r,5._pm_r,168._pm_r,0.01_pm_r,270._pm_r,2._pm_r,170._pm_r,0.12_pm_r,65._pm_r, &
260.80_pm_r,58627._pm_r,5._pm_r,168._pm_r,0.05_pm_r,53._pm_r,2._pm_r,163._pm_r,0.19_pm_r,39._pm_r, &
247.90_pm_r,62349._pm_r,5._pm_r,167._pm_r,0.11_pm_r,45._pm_r,1._pm_r,153._pm_r,0.24_pm_r,16._pm_r, &
235.00_pm_r,65888._pm_r,5._pm_r,165._pm_r,0.08_pm_r,60._pm_r,1._pm_r,139._pm_r,0.26_pm_r,7._pm_r, &
222.30_pm_r,69233._pm_r,5._pm_r,164._pm_r,0.08_pm_r,137._pm_r,1._pm_r,119._pm_r,0.26_pm_r,358._pm_r, &
209.60_pm_r,72400._pm_r,5._pm_r,164._pm_r,0.19_pm_r,171._pm_r,1._pm_r,91._pm_r,0.22_pm_r,352._pm_r, &
195.90_pm_r,75366._pm_r,6._pm_r,165._pm_r,0.30_pm_r,179._pm_r,1._pm_r,66._pm_r,0.16_pm_r,348._pm_r, &
181.60_pm_r,78134._pm_r,6._pm_r,166._pm_r,0.36_pm_r,183._pm_r,1._pm_r,49._pm_r,0.12_pm_r,346._pm_r, &
168.80_pm_r,80690._pm_r,7._pm_r,168._pm_r,0.34_pm_r,184._pm_r,1._pm_r,39._pm_r,0.09_pm_r,341._pm_r, &
157.70_pm_r,83072._pm_r,7._pm_r,169._pm_r,0.31_pm_r,184._pm_r,1._pm_r,33._pm_r,0.06_pm_r,342._pm_r, &
150.80_pm_r,85302._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
149.40_pm_r,87478._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
151.70_pm_r,89676._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
158.50_pm_r,91950._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
169.90_pm_r,94366._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.10_pm_r,97012._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.50_pm_r,100008._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
248.00_pm_r,103504._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
284.40_pm_r,107612._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
322.50_pm_r,112355._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
407.10_pm_r,118165._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_50= (/ &
281.50_pm_r,-50._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
260.40_pm_r,3913._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
236.00_pm_r,7543._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.70_pm_r,10891._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
220.70_pm_r,14126._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.20_pm_r,17364._pm_r,9._pm_r,199._pm_r,0.45_pm_r,4._pm_r,0._pm_r,211._pm_r,0.05_pm_r,192._pm_r, &
222.70_pm_r,20612._pm_r,8._pm_r,200._pm_r,0.62_pm_r,8._pm_r,1._pm_r,207._pm_r,0.08_pm_r,188._pm_r, &
227.10_pm_r,23906._pm_r,7._pm_r,201._pm_r,0.72_pm_r,13._pm_r,1._pm_r,203._pm_r,0.10_pm_r,185._pm_r, &
231.90_pm_r,27264._pm_r,6._pm_r,202._pm_r,0.73_pm_r,19._pm_r,1._pm_r,199._pm_r,0.11_pm_r,183._pm_r, &
240.60_pm_r,30722._pm_r,5._pm_r,202._pm_r,0.65_pm_r,28._pm_r,1._pm_r,196._pm_r,0.10_pm_r,180._pm_r, &
252.90_pm_r,34330._pm_r,4._pm_r,200._pm_r,0.53_pm_r,39._pm_r,1._pm_r,193._pm_r,0.09_pm_r,171._pm_r, &
265.30_pm_r,38131._pm_r,4._pm_r,195._pm_r,0.40_pm_r,52._pm_r,1._pm_r,191._pm_r,0.07_pm_r,164._pm_r, &
275.10_pm_r,42089._pm_r,3._pm_r,189._pm_r,0.29_pm_r,66._pm_r,1._pm_r,188._pm_r,0.05_pm_r,148._pm_r, &
278.80_pm_r,46154._pm_r,3._pm_r,184._pm_r,0.15_pm_r,54._pm_r,1._pm_r,187._pm_r,0.01_pm_r,117._pm_r, &
276.00_pm_r,50223._pm_r,3._pm_r,182._pm_r,0.07_pm_r,30._pm_r,1._pm_r,186._pm_r,0.04_pm_r,83._pm_r, &
268.70_pm_r,54212._pm_r,3._pm_r,181._pm_r,0.03_pm_r,129._pm_r,1._pm_r,181._pm_r,0.11_pm_r,83._pm_r, &
258.20_pm_r,58076._pm_r,3._pm_r,180._pm_r,0.16_pm_r,146._pm_r,1._pm_r,171._pm_r,0.19_pm_r,70._pm_r, &
245.90_pm_r,61764._pm_r,3._pm_r,177._pm_r,0.25_pm_r,147._pm_r,1._pm_r,156._pm_r,0.22_pm_r,50._pm_r, &
233.40_pm_r,65277._pm_r,4._pm_r,174._pm_r,0.28_pm_r,147._pm_r,1._pm_r,139._pm_r,0.22_pm_r,32._pm_r, &
221.20_pm_r,68602._pm_r,4._pm_r,171._pm_r,0.28_pm_r,149._pm_r,1._pm_r,121._pm_r,0.21_pm_r,10._pm_r, &
210.10_pm_r,71764._pm_r,4._pm_r,169._pm_r,0.27_pm_r,153._pm_r,1._pm_r,103._pm_r,0.19_pm_r,350._pm_r, &
199.40_pm_r,74759._pm_r,5._pm_r,168._pm_r,0.24_pm_r,158._pm_r,1._pm_r,82._pm_r,0.20_pm_r,333._pm_r, &
188.30_pm_r,77601._pm_r,5._pm_r,168._pm_r,0.20_pm_r,161._pm_r,1._pm_r,57._pm_r,0.18_pm_r,321._pm_r, &
177.80_pm_r,80275._pm_r,5._pm_r,167._pm_r,0.18_pm_r,164._pm_r,1._pm_r,32._pm_r,0.18_pm_r,315._pm_r, &
168.60_pm_r,82805._pm_r,6._pm_r,167._pm_r,0.14_pm_r,168._pm_r,1._pm_r,12._pm_r,0.16_pm_r,309._pm_r, &
162.10_pm_r,85210._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
159.70_pm_r,87552._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
160.20_pm_r,89889._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
165.20_pm_r,92278._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
174.10_pm_r,94778._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.20_pm_r,97467._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.50_pm_r,100444._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
238.70_pm_r,103841._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
275.00_pm_r,107798._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
321.10_pm_r,112460._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
403.20_pm_r,118235._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_40= (/ &
288.50_pm_r,-1._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
267.20_pm_r,4066._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.00_pm_r,7794._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.30_pm_r,11185._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.90_pm_r,14371._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.30_pm_r,17494._pm_r,4._pm_r,194._pm_r,0.11_pm_r,311._pm_r,1._pm_r,117._pm_r,0.10_pm_r,261._pm_r, &
216.70_pm_r,20638._pm_r,4._pm_r,196._pm_r,0.13_pm_r,318._pm_r,1._pm_r,128._pm_r,0.13_pm_r,261._pm_r, &
222.90_pm_r,23860._pm_r,4._pm_r,198._pm_r,0.13_pm_r,338._pm_r,0._pm_r,148._pm_r,0.15_pm_r,258._pm_r, &
228.50_pm_r,27163._pm_r,4._pm_r,200._pm_r,0.13_pm_r,4._pm_r,0._pm_r,177._pm_r,0.14_pm_r,258._pm_r, &
236.90_pm_r,30569._pm_r,3._pm_r,200._pm_r,0.15_pm_r,31._pm_r,1._pm_r,201._pm_r,0.13_pm_r,257._pm_r, &
249.30_pm_r,34124._pm_r,3._pm_r,198._pm_r,0.17_pm_r,50._pm_r,1._pm_r,213._pm_r,0.09_pm_r,257._pm_r, &
261.30_pm_r,37867._pm_r,3._pm_r,195._pm_r,0.17_pm_r,60._pm_r,1._pm_r,219._pm_r,0.06_pm_r,260._pm_r, &
271.90_pm_r,41771._pm_r,3._pm_r,191._pm_r,0.15_pm_r,62._pm_r,1._pm_r,223._pm_r,0.03_pm_r,270._pm_r, &
276.70_pm_r,45798._pm_r,3._pm_r,190._pm_r,0.07_pm_r,21._pm_r,1._pm_r,224._pm_r,0.00_pm_r,143._pm_r, &
273.50_pm_r,49835._pm_r,3._pm_r,190._pm_r,0.02_pm_r,243._pm_r,1._pm_r,221._pm_r,0.06_pm_r,108._pm_r, &
265.50_pm_r,53783._pm_r,3._pm_r,189._pm_r,0.15_pm_r,156._pm_r,1._pm_r,209._pm_r,0.14_pm_r,105._pm_r, &
254.90_pm_r,57600._pm_r,3._pm_r,184._pm_r,0.35_pm_r,130._pm_r,1._pm_r,188._pm_r,0.18_pm_r,99._pm_r, &
242.80_pm_r,61240._pm_r,3._pm_r,175._pm_r,0.40_pm_r,111._pm_r,1._pm_r,168._pm_r,0.13_pm_r,90._pm_r, &
231.10_pm_r,64714._pm_r,3._pm_r,165._pm_r,0.37_pm_r,90._pm_r,1._pm_r,156._pm_r,0.07_pm_r,56._pm_r, &
219.90_pm_r,68013._pm_r,3._pm_r,157._pm_r,0.30_pm_r,60._pm_r,1._pm_r,149._pm_r,0.08_pm_r,360._pm_r, &
210.50_pm_r,71165._pm_r,3._pm_r,151._pm_r,0.31_pm_r,25._pm_r,1._pm_r,145._pm_r,0.11_pm_r,333._pm_r, &
202.80_pm_r,74188._pm_r,3._pm_r,145._pm_r,0.36_pm_r,2._pm_r,0._pm_r,143._pm_r,0.13_pm_r,326._pm_r, &
195.40_pm_r,77107._pm_r,3._pm_r,138._pm_r,0.42_pm_r,349._pm_r,0._pm_r,143._pm_r,0.15_pm_r,323._pm_r, &
188.60_pm_r,79915._pm_r,2._pm_r,129._pm_r,0.44_pm_r,343._pm_r,0._pm_r,321._pm_r,0.16_pm_r,321._pm_r, &
181.50_pm_r,82630._pm_r,1._pm_r,115._pm_r,0.43_pm_r,339._pm_r,0._pm_r,320._pm_r,0.15_pm_r,318._pm_r, &
174.60_pm_r,85226._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
170.50_pm_r,87739._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
169.30_pm_r,90224._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
172.20_pm_r,92733._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
178.40_pm_r,95321._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.20_pm_r,98052._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.50_pm_r,101009._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
229.70_pm_r,104310._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
264.80_pm_r,108115._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
317.40_pm_r,112667._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
398.90_pm_r,118388._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_30= (/ &
294.60_pm_r,-28._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
272.80_pm_r,4129._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
247.50_pm_r,7941._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.20_pm_r,11389._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
207.80_pm_r,14547._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.20_pm_r,17562._pm_r,2._pm_r,162._pm_r,0.25_pm_r,229._pm_r,0._pm_r,88._pm_r,0.04_pm_r,183._pm_r, &
212.50_pm_r,20626._pm_r,2._pm_r,173._pm_r,0.29_pm_r,227._pm_r,0._pm_r,103._pm_r,0.06_pm_r,185._pm_r, &
220.20_pm_r,23798._pm_r,2._pm_r,182._pm_r,0.27_pm_r,223._pm_r,0._pm_r,120._pm_r,0.06_pm_r,191._pm_r, &
225.70_pm_r,27063._pm_r,3._pm_r,187._pm_r,0.20_pm_r,214._pm_r,0._pm_r,135._pm_r,0.06_pm_r,198._pm_r, &
233.50_pm_r,30424._pm_r,3._pm_r,188._pm_r,0.10_pm_r,191._pm_r,0._pm_r,147._pm_r,0.06_pm_r,203._pm_r, &
246.00_pm_r,33930._pm_r,3._pm_r,187._pm_r,0.08_pm_r,122._pm_r,0._pm_r,156._pm_r,0.05_pm_r,204._pm_r, &
257.90_pm_r,37625._pm_r,3._pm_r,184._pm_r,0.12_pm_r,81._pm_r,1._pm_r,162._pm_r,0.04_pm_r,208._pm_r, &
267.90_pm_r,41474._pm_r,3._pm_r,180._pm_r,0.14_pm_r,69._pm_r,1._pm_r,166._pm_r,0.03_pm_r,198._pm_r, &
272.90_pm_r,45442._pm_r,3._pm_r,177._pm_r,0.12_pm_r,77._pm_r,1._pm_r,166._pm_r,0.02_pm_r,124._pm_r, &
271.30_pm_r,49435._pm_r,3._pm_r,172._pm_r,0.18_pm_r,93._pm_r,1._pm_r,160._pm_r,0.07_pm_r,73._pm_r, &
263.60_pm_r,53353._pm_r,3._pm_r,166._pm_r,0.22_pm_r,91._pm_r,1._pm_r,147._pm_r,0.10_pm_r,80._pm_r, &
253.70_pm_r,57145._pm_r,3._pm_r,160._pm_r,0.21_pm_r,69._pm_r,1._pm_r,136._pm_r,0.12_pm_r,107._pm_r, &
241.90_pm_r,60773._pm_r,3._pm_r,155._pm_r,0.17_pm_r,22._pm_r,1._pm_r,135._pm_r,0.12_pm_r,146._pm_r, &
229.30_pm_r,64226._pm_r,3._pm_r,152._pm_r,0.20_pm_r,350._pm_r,1._pm_r,139._pm_r,0.11_pm_r,177._pm_r, &
218.90_pm_r,67503._pm_r,2._pm_r,151._pm_r,0.20_pm_r,338._pm_r,1._pm_r,145._pm_r,0.08_pm_r,217._pm_r, &
211.20_pm_r,70651._pm_r,2._pm_r,150._pm_r,0.17_pm_r,333._pm_r,1._pm_r,151._pm_r,0.11_pm_r,259._pm_r, &
205.90_pm_r,73702._pm_r,2._pm_r,150._pm_r,0.11_pm_r,335._pm_r,1._pm_r,160._pm_r,0.14_pm_r,286._pm_r, &
201.70_pm_r,76687._pm_r,2._pm_r,149._pm_r,0.08_pm_r,338._pm_r,1._pm_r,170._pm_r,0.17_pm_r,298._pm_r, &
198.10_pm_r,79614._pm_r,2._pm_r,148._pm_r,0.05_pm_r,346._pm_r,1._pm_r,187._pm_r,0.19_pm_r,304._pm_r, &
192.70_pm_r,82491._pm_r,2._pm_r,147._pm_r,0.03_pm_r,0._pm_r,1._pm_r,211._pm_r,0.20_pm_r,306._pm_r, &
185.10_pm_r,85247._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
179.70_pm_r,87904._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
177.50_pm_r,90521._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
178.60_pm_r,93140._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
182.30_pm_r,95808._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.80_pm_r,98576._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.70_pm_r,101514._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.70_pm_r,104727._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
254.50_pm_r,108390._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
311.50_pm_r,112813._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
394.30_pm_r,118459._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_20= (/ &
298.40_pm_r,-65._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
275.90_pm_r,4145._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
251.80_pm_r,8014._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.20_pm_r,11511._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.30_pm_r,14653._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.00_pm_r,17586._pm_r,3._pm_r,140._pm_r,0.32_pm_r,262._pm_r,0._pm_r,329._pm_r,0.16_pm_r,120._pm_r, &
210.00_pm_r,20594._pm_r,2._pm_r,152._pm_r,0.38_pm_r,261._pm_r,0._pm_r,4._pm_r,0.21_pm_r,119._pm_r, &
218.20_pm_r,23734._pm_r,2._pm_r,165._pm_r,0.35_pm_r,262._pm_r,0._pm_r,77._pm_r,0.21_pm_r,123._pm_r, &
223.80_pm_r,26971._pm_r,2._pm_r,178._pm_r,0.28_pm_r,262._pm_r,1._pm_r,101._pm_r,0.21_pm_r,125._pm_r, &
231.60_pm_r,30304._pm_r,2._pm_r,186._pm_r,0.15_pm_r,259._pm_r,1._pm_r,110._pm_r,0.17_pm_r,129._pm_r, &
242.90_pm_r,33774._pm_r,2._pm_r,189._pm_r,0.02_pm_r,207._pm_r,1._pm_r,114._pm_r,0.11_pm_r,135._pm_r, &
254.00_pm_r,37417._pm_r,2._pm_r,187._pm_r,0.10_pm_r,102._pm_r,1._pm_r,117._pm_r,0.06_pm_r,151._pm_r, &
263.50_pm_r,41206._pm_r,2._pm_r,182._pm_r,0.16_pm_r,97._pm_r,1._pm_r,119._pm_r,0.03_pm_r,198._pm_r, &
269.10_pm_r,45112._pm_r,2._pm_r,176._pm_r,0.16_pm_r,97._pm_r,1._pm_r,122._pm_r,0.09_pm_r,263._pm_r, &
269.50_pm_r,49063._pm_r,2._pm_r,170._pm_r,0.18_pm_r,90._pm_r,1._pm_r,128._pm_r,0.18_pm_r,276._pm_r, &
262.60_pm_r,52963._pm_r,2._pm_r,165._pm_r,0.16_pm_r,73._pm_r,1._pm_r,139._pm_r,0.14_pm_r,274._pm_r, &
254.00_pm_r,56747._pm_r,2._pm_r,160._pm_r,0.16_pm_r,22._pm_r,1._pm_r,144._pm_r,0.11_pm_r,124._pm_r, &
243.40_pm_r,60390._pm_r,2._pm_r,157._pm_r,0.23_pm_r,346._pm_r,1._pm_r,135._pm_r,0.31_pm_r,110._pm_r, &
230.80_pm_r,63867._pm_r,2._pm_r,157._pm_r,0.30_pm_r,332._pm_r,2._pm_r,126._pm_r,0.37_pm_r,109._pm_r, &
219.50_pm_r,67158._pm_r,1._pm_r,159._pm_r,0.31_pm_r,329._pm_r,2._pm_r,122._pm_r,0.27_pm_r,106._pm_r, &
212.00_pm_r,70315._pm_r,1._pm_r,165._pm_r,0.30_pm_r,327._pm_r,2._pm_r,120._pm_r,0.10_pm_r,101._pm_r, &
208.30_pm_r,73390._pm_r,0._pm_r,179._pm_r,0.28_pm_r,332._pm_r,2._pm_r,119._pm_r,0.08_pm_r,300._pm_r, &
205.60_pm_r,76421._pm_r,0._pm_r,244._pm_r,0.26_pm_r,337._pm_r,2._pm_r,120._pm_r,0.21_pm_r,289._pm_r, &
203.80_pm_r,79416._pm_r,0._pm_r,310._pm_r,0.25_pm_r,339._pm_r,2._pm_r,122._pm_r,0.31_pm_r,290._pm_r, &
199.40_pm_r,82388._pm_r,1._pm_r,324._pm_r,0.22_pm_r,342._pm_r,1._pm_r,127._pm_r,0.35_pm_r,292._pm_r, &
191.70_pm_r,85241._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.00_pm_r,87993._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.50_pm_r,90703._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
183.50_pm_r,93405._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
185.30_pm_r,96135._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.00_pm_r,98928._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
198.20_pm_r,101844._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.60_pm_r,104979._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
245.00_pm_r,108513._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
304.10_pm_r,112801._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
389.30_pm_r,118355._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_moins_10= (/ &
300.40_pm_r,-67._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.50_pm_r,4164._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.10_pm_r,8049._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.70_pm_r,11561._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.50_pm_r,14687._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
198.20_pm_r,17574._pm_r,2._pm_r,138._pm_r,0.37_pm_r,290._pm_r,0._pm_r,296._pm_r,0.21_pm_r,107._pm_r, &
208.60_pm_r,20551._pm_r,2._pm_r,148._pm_r,0.43_pm_r,289._pm_r,0._pm_r,339._pm_r,0.26_pm_r,108._pm_r, &
217.20_pm_r,23674._pm_r,1._pm_r,164._pm_r,0.42_pm_r,289._pm_r,0._pm_r,98._pm_r,0.26_pm_r,107._pm_r, &
222.70_pm_r,26896._pm_r,1._pm_r,188._pm_r,0.35_pm_r,288._pm_r,1._pm_r,102._pm_r,0.23_pm_r,106._pm_r, &
229.90_pm_r,30206._pm_r,1._pm_r,210._pm_r,0.22_pm_r,288._pm_r,1._pm_r,104._pm_r,0.16_pm_r,106._pm_r, &
241.40_pm_r,33653._pm_r,1._pm_r,220._pm_r,0.08_pm_r,285._pm_r,1._pm_r,104._pm_r,0.06_pm_r,104._pm_r, &
251.70_pm_r,37270._pm_r,1._pm_r,220._pm_r,0.04_pm_r,115._pm_r,1._pm_r,103._pm_r,0.03_pm_r,308._pm_r, &
260.10_pm_r,41017._pm_r,1._pm_r,215._pm_r,0.11_pm_r,109._pm_r,1._pm_r,102._pm_r,0.10_pm_r,298._pm_r, &
265.20_pm_r,44868._pm_r,1._pm_r,212._pm_r,0.09_pm_r,18._pm_r,1._pm_r,101._pm_r,0.14_pm_r,275._pm_r, &
266.80_pm_r,48768._pm_r,1._pm_r,225._pm_r,0.35_pm_r,344._pm_r,1._pm_r,106._pm_r,0.16_pm_r,249._pm_r, &
263.40_pm_r,52654._pm_r,1._pm_r,270._pm_r,0.53_pm_r,342._pm_r,1._pm_r,121._pm_r,0.12_pm_r,213._pm_r, &
257.20_pm_r,56470._pm_r,1._pm_r,304._pm_r,0.48_pm_r,344._pm_r,1._pm_r,132._pm_r,0.11_pm_r,149._pm_r, &
247.50_pm_r,60168._pm_r,2._pm_r,315._pm_r,0.19_pm_r,354._pm_r,1._pm_r,129._pm_r,0.10_pm_r,86._pm_r, &
233.90_pm_r,63699._pm_r,2._pm_r,319._pm_r,0.07_pm_r,118._pm_r,1._pm_r,120._pm_r,0.12_pm_r,40._pm_r, &
220.90_pm_r,67023._pm_r,2._pm_r,319._pm_r,0.19_pm_r,149._pm_r,1._pm_r,109._pm_r,0.13_pm_r,351._pm_r, &
212.10_pm_r,70191._pm_r,1._pm_r,314._pm_r,0.25_pm_r,158._pm_r,1._pm_r,96._pm_r,0.21_pm_r,319._pm_r, &
208.20_pm_r,73263._pm_r,1._pm_r,304._pm_r,0.26_pm_r,162._pm_r,1._pm_r,70._pm_r,0.29_pm_r,305._pm_r, &
206.60_pm_r,76299._pm_r,1._pm_r,281._pm_r,0.26_pm_r,166._pm_r,0._pm_r,5._pm_r,0.36_pm_r,298._pm_r, &
206.10_pm_r,79321._pm_r,1._pm_r,247._pm_r,0.25_pm_r,169._pm_r,1._pm_r,324._pm_r,0.41_pm_r,294._pm_r, &
202.40_pm_r,82337._pm_r,1._pm_r,219._pm_r,0.23_pm_r,170._pm_r,1._pm_r,311._pm_r,0.41_pm_r,292._pm_r, &
194.70_pm_r,85235._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.90_pm_r,88030._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,90785._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.40_pm_r,93535._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,96300._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.50_pm_r,99104._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
195.10_pm_r,101995._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.50_pm_r,105060._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
236.70_pm_r,108484._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
296.60_pm_r,112642._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
383.80_pm_r,118098._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_0= (/ &
300.60_pm_r,-91._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.80_pm_r,4145._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.50_pm_r,8035._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.00_pm_r,11553._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.00_pm_r,14678._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.00_pm_r,17554._pm_r,2._pm_r,138._pm_r,0.33_pm_r,306._pm_r,1._pm_r,313._pm_r,0.19_pm_r,98._pm_r, &
207.40_pm_r,20518._pm_r,2._pm_r,141._pm_r,0.43_pm_r,306._pm_r,0._pm_r,347._pm_r,0.23_pm_r,97._pm_r, &
216.50_pm_r,23627._pm_r,1._pm_r,149._pm_r,0.44_pm_r,307._pm_r,1._pm_r,36._pm_r,0.23_pm_r,94._pm_r, &
222.20_pm_r,26837._pm_r,0._pm_r,188._pm_r,0.40_pm_r,307._pm_r,1._pm_r,58._pm_r,0.19_pm_r,90._pm_r, &
229.60_pm_r,30141._pm_r,1._pm_r,268._pm_r,0.30_pm_r,306._pm_r,1._pm_r,65._pm_r,0.12_pm_r,83._pm_r, &
242.00_pm_r,33588._pm_r,1._pm_r,285._pm_r,0.17_pm_r,303._pm_r,1._pm_r,65._pm_r,0.04_pm_r,45._pm_r, &
252.00_pm_r,37208._pm_r,1._pm_r,287._pm_r,0.07_pm_r,283._pm_r,1._pm_r,62._pm_r,0.07_pm_r,309._pm_r, &
259.30_pm_r,40949._pm_r,1._pm_r,285._pm_r,0.05_pm_r,191._pm_r,1._pm_r,56._pm_r,0.13_pm_r,293._pm_r, &
263.80_pm_r,44786._pm_r,1._pm_r,284._pm_r,0.04_pm_r,315._pm_r,1._pm_r,49._pm_r,0.10_pm_r,256._pm_r, &
265.60_pm_r,48668._pm_r,1._pm_r,292._pm_r,0.21_pm_r,331._pm_r,1._pm_r,48._pm_r,0.14_pm_r,192._pm_r, &
264.10_pm_r,52551._pm_r,2._pm_r,304._pm_r,0.38_pm_r,335._pm_r,1._pm_r,63._pm_r,0.16_pm_r,166._pm_r, &
258.50_pm_r,56379._pm_r,2._pm_r,313._pm_r,0.47_pm_r,339._pm_r,1._pm_r,82._pm_r,0.07_pm_r,124._pm_r, &
248.20_pm_r,60088._pm_r,3._pm_r,319._pm_r,0.30_pm_r,340._pm_r,1._pm_r,79._pm_r,0.13_pm_r,16._pm_r, &
234.50_pm_r,63627._pm_r,3._pm_r,321._pm_r,0.10_pm_r,343._pm_r,1._pm_r,64._pm_r,0.19_pm_r,12._pm_r, &
220.60_pm_r,66953._pm_r,3._pm_r,322._pm_r,0.08_pm_r,137._pm_r,1._pm_r,53._pm_r,0.21_pm_r,33._pm_r, &
211.40_pm_r,70117._pm_r,2._pm_r,321._pm_r,0.24_pm_r,152._pm_r,1._pm_r,51._pm_r,0.23_pm_r,61._pm_r, &
208.10_pm_r,73187._pm_r,2._pm_r,318._pm_r,0.37_pm_r,152._pm_r,1._pm_r,55._pm_r,0.29_pm_r,82._pm_r, &
208.30_pm_r,76232._pm_r,1._pm_r,308._pm_r,0.47_pm_r,152._pm_r,2._pm_r,62._pm_r,0.36_pm_r,96._pm_r, &
208.50_pm_r,79280._pm_r,1._pm_r,284._pm_r,0.52_pm_r,153._pm_r,2._pm_r,69._pm_r,0.41_pm_r,101._pm_r, &
204.40_pm_r,82325._pm_r,1._pm_r,234._pm_r,0.52_pm_r,154._pm_r,2._pm_r,76._pm_r,0.41_pm_r,105._pm_r, &
195.90_pm_r,85245._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.60_pm_r,88052._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.50_pm_r,90818._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,93581._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.40_pm_r,96356._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.50_pm_r,99156._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.40_pm_r,102020._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.60_pm_r,105027._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.00_pm_r,108362._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
289.80_pm_r,112411._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
378.20_pm_r,117772._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_10= (/ &
300.70_pm_r,-105._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.10_pm_r,4133._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.00_pm_r,8021._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.50_pm_r,11531._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.40_pm_r,14655._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.90_pm_r,17538._pm_r,4._pm_r,142._pm_r,0.68_pm_r,325._pm_r,1._pm_r,312._pm_r,0.24_pm_r,65._pm_r, &
208.00_pm_r,20508._pm_r,3._pm_r,140._pm_r,0.89_pm_r,326._pm_r,1._pm_r,338._pm_r,0.29_pm_r,63._pm_r, &
216.30_pm_r,23619._pm_r,2._pm_r,135._pm_r,0.91_pm_r,326._pm_r,1._pm_r,3._pm_r,0.26_pm_r,61._pm_r, &
222.30_pm_r,26830._pm_r,0._pm_r,96._pm_r,0.82_pm_r,328._pm_r,1._pm_r,16._pm_r,0.19_pm_r,54._pm_r, &
230.20_pm_r,30140._pm_r,1._pm_r,350._pm_r,0.62_pm_r,330._pm_r,1._pm_r,20._pm_r,0.08_pm_r,27._pm_r, &
241.80_pm_r,33592._pm_r,2._pm_r,342._pm_r,0.37_pm_r,333._pm_r,1._pm_r,18._pm_r,0.08_pm_r,295._pm_r, &
251.80_pm_r,37212._pm_r,2._pm_r,340._pm_r,0.15_pm_r,340._pm_r,1._pm_r,11._pm_r,0.17_pm_r,272._pm_r, &
259.70_pm_r,40958._pm_r,2._pm_r,341._pm_r,0.02_pm_r,90._pm_r,1._pm_r,359._pm_r,0.22_pm_r,266._pm_r, &
264.30_pm_r,44798._pm_r,2._pm_r,340._pm_r,0.11_pm_r,313._pm_r,1._pm_r,348._pm_r,0.13_pm_r,260._pm_r, &
266.10_pm_r,48685._pm_r,2._pm_r,336._pm_r,0.28_pm_r,296._pm_r,1._pm_r,343._pm_r,0.03_pm_r,259._pm_r, &
263.30_pm_r,52565._pm_r,3._pm_r,329._pm_r,0.30_pm_r,283._pm_r,1._pm_r,342._pm_r,0.02_pm_r,324._pm_r, &
257.00_pm_r,56381._pm_r,3._pm_r,323._pm_r,0.16_pm_r,230._pm_r,1._pm_r,340._pm_r,0.09_pm_r,270._pm_r, &
246.50_pm_r,60070._pm_r,3._pm_r,320._pm_r,0.28_pm_r,162._pm_r,2._pm_r,334._pm_r,0.13_pm_r,258._pm_r, &
232.80_pm_r,63586._pm_r,2._pm_r,316._pm_r,0.40_pm_r,154._pm_r,2._pm_r,327._pm_r,0.11_pm_r,212._pm_r, &
219.90_pm_r,66894._pm_r,2._pm_r,308._pm_r,0.41_pm_r,160._pm_r,1._pm_r,323._pm_r,0.18_pm_r,150._pm_r, &
211.30_pm_r,70049._pm_r,1._pm_r,291._pm_r,0.37_pm_r,175._pm_r,1._pm_r,326._pm_r,0.34_pm_r,128._pm_r, &
208.10_pm_r,73116._pm_r,1._pm_r,265._pm_r,0.35_pm_r,194._pm_r,0._pm_r,359._pm_r,0.51_pm_r,121._pm_r, &
207.60_pm_r,76159._pm_r,2._pm_r,247._pm_r,0.37_pm_r,210._pm_r,1._pm_r,89._pm_r,0.65_pm_r,118._pm_r, &
207.60_pm_r,79202._pm_r,2._pm_r,239._pm_r,0.40_pm_r,222._pm_r,2._pm_r,105._pm_r,0.74_pm_r,116._pm_r, &
204.20_pm_r,82244._pm_r,3._pm_r,236._pm_r,0.39_pm_r,229._pm_r,3._pm_r,109._pm_r,0.75_pm_r,115._pm_r, &
196.30_pm_r,85175._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.80_pm_r,87989._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.10_pm_r,90751._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,93508._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.00_pm_r,96278._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.50_pm_r,99069._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.50_pm_r,101910._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.50_pm_r,104878._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
225.50_pm_r,108153._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
284.60_pm_r,112123._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
372.90_pm_r,117403._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_20= (/ &
298.50_pm_r,-100._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
275.00_pm_r,4104._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
250.20_pm_r,7955._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.20_pm_r,11433._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.00_pm_r,14565._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.90_pm_r,17496._pm_r,7._pm_r,145._pm_r,1.17_pm_r,323._pm_r,2._pm_r,290._pm_r,0.47_pm_r,67._pm_r, &
209.40_pm_r,20498._pm_r,5._pm_r,144._pm_r,1.61_pm_r,329._pm_r,1._pm_r,319._pm_r,0.53_pm_r,65._pm_r, &
217.00_pm_r,23625._pm_r,3._pm_r,135._pm_r,1.80_pm_r,338._pm_r,1._pm_r,354._pm_r,0.40_pm_r,59._pm_r, &
222.10_pm_r,26840._pm_r,1._pm_r,57._pm_r,1.88_pm_r,347._pm_r,1._pm_r,9._pm_r,0.19_pm_r,32._pm_r, &
229.20_pm_r,30142._pm_r,3._pm_r,12._pm_r,1.76_pm_r,357._pm_r,2._pm_r,5._pm_r,0.22_pm_r,295._pm_r, &
240.50_pm_r,33576._pm_r,6._pm_r,7._pm_r,1.46_pm_r,5._pm_r,2._pm_r,348._pm_r,0.47_pm_r,275._pm_r, &
251.80_pm_r,37186._pm_r,8._pm_r,7._pm_r,1.03_pm_r,9._pm_r,2._pm_r,326._pm_r,0.62_pm_r,272._pm_r, &
261.60_pm_r,40944._pm_r,9._pm_r,7._pm_r,0.51_pm_r,2._pm_r,3._pm_r,310._pm_r,0.65_pm_r,274._pm_r, &
266.90_pm_r,44821._pm_r,9._pm_r,6._pm_r,0.36_pm_r,305._pm_r,3._pm_r,303._pm_r,0.37_pm_r,279._pm_r, &
266.60_pm_r,48733._pm_r,9._pm_r,2._pm_r,0.64_pm_r,263._pm_r,4._pm_r,302._pm_r,0.16_pm_r,333._pm_r, &
260.80_pm_r,52598._pm_r,9._pm_r,355._pm_r,0.89_pm_r,245._pm_r,4._pm_r,305._pm_r,0.21_pm_r,35._pm_r, &
252.60_pm_r,56362._pm_r,9._pm_r,346._pm_r,0.95_pm_r,231._pm_r,4._pm_r,310._pm_r,0.19_pm_r,54._pm_r, &
243.10_pm_r,59990._pm_r,8._pm_r,337._pm_r,1.02_pm_r,221._pm_r,4._pm_r,313._pm_r,0.09_pm_r,56._pm_r, &
232.30_pm_r,63476._pm_r,7._pm_r,327._pm_r,0.93_pm_r,218._pm_r,4._pm_r,314._pm_r,0.04_pm_r,76._pm_r, &
220.30_pm_r,66786._pm_r,7._pm_r,317._pm_r,0.74_pm_r,220._pm_r,4._pm_r,315._pm_r,0.05_pm_r,68._pm_r, &
212.00_pm_r,69947._pm_r,7._pm_r,310._pm_r,0.53_pm_r,227._pm_r,4._pm_r,317._pm_r,0.09_pm_r,58._pm_r, &
209.20_pm_r,73027._pm_r,7._pm_r,305._pm_r,0.37_pm_r,243._pm_r,4._pm_r,319._pm_r,0.14_pm_r,62._pm_r, &
207.60_pm_r,76080._pm_r,7._pm_r,302._pm_r,0.30_pm_r,268._pm_r,4._pm_r,323._pm_r,0.18_pm_r,61._pm_r, &
206.70_pm_r,79111._pm_r,8._pm_r,301._pm_r,0.28_pm_r,287._pm_r,4._pm_r,328._pm_r,0.20_pm_r,57._pm_r, &
204.10_pm_r,82133._pm_r,8._pm_r,300._pm_r,0.28_pm_r,302._pm_r,4._pm_r,333._pm_r,0.21_pm_r,59._pm_r, &
197.80_pm_r,85088._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.00_pm_r,87928._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.10_pm_r,90694._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,93448._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
186.70_pm_r,96212._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.20_pm_r,98997._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.00_pm_r,101831._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.60_pm_r,104787._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.60_pm_r,108039._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
281.30_pm_r,111966._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
368.00_pm_r,117181._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_30= (/ &
293.10_pm_r,-52._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
268.60_pm_r,4063._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
243.50_pm_r,7815._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.40_pm_r,11221._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
208.40_pm_r,14370._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.70_pm_r,17395._pm_r,9._pm_r,187._pm_r,0.47_pm_r,38._pm_r,4._pm_r,305._pm_r,0.88_pm_r,92._pm_r, &
211.30_pm_r,20453._pm_r,8._pm_r,182._pm_r,1.15_pm_r,39._pm_r,2._pm_r,323._pm_r,0.99_pm_r,90._pm_r, &
217.00_pm_r,23592._pm_r,6._pm_r,169._pm_r,2.04_pm_r,38._pm_r,2._pm_r,354._pm_r,0.72_pm_r,83._pm_r, &
221.50_pm_r,26801._pm_r,4._pm_r,131._pm_r,2.91_pm_r,36._pm_r,2._pm_r,12._pm_r,0.26_pm_r,35._pm_r, &
228.10_pm_r,30093._pm_r,6._pm_r,80._pm_r,3.52_pm_r,32._pm_r,3._pm_r,3._pm_r,0.66_pm_r,303._pm_r, &
236.50_pm_r,33492._pm_r,10._pm_r,57._pm_r,3.67_pm_r,26._pm_r,3._pm_r,341._pm_r,1.31_pm_r,292._pm_r, &
247.00_pm_r,37034._pm_r,15._pm_r,45._pm_r,3.24_pm_r,15._pm_r,5._pm_r,321._pm_r,1.71_pm_r,289._pm_r, &
258.00_pm_r,40731._pm_r,18._pm_r,37._pm_r,2.58_pm_r,355._pm_r,8._pm_r,311._pm_r,1.80_pm_r,289._pm_r, &
264.60_pm_r,44566._pm_r,21._pm_r,29._pm_r,2.22_pm_r,330._pm_r,10._pm_r,306._pm_r,1.16_pm_r,289._pm_r, &
264.60_pm_r,48448._pm_r,22._pm_r,21._pm_r,2.36_pm_r,304._pm_r,11._pm_r,304._pm_r,0.42_pm_r,301._pm_r, &
257.50_pm_r,52274._pm_r,22._pm_r,12._pm_r,2.55_pm_r,284._pm_r,11._pm_r,305._pm_r,0.26_pm_r,62._pm_r, &
247.80_pm_r,55978._pm_r,22._pm_r,2._pm_r,2.41_pm_r,265._pm_r,11._pm_r,307._pm_r,0.49_pm_r,82._pm_r, &
239.40_pm_r,59542._pm_r,21._pm_r,353._pm_r,2.54_pm_r,249._pm_r,10._pm_r,310._pm_r,0.40_pm_r,69._pm_r, &
231.60_pm_r,62994._pm_r,20._pm_r,343._pm_r,2.33_pm_r,241._pm_r,10._pm_r,313._pm_r,0.40_pm_r,57._pm_r, &
223.40_pm_r,66323._pm_r,20._pm_r,334._pm_r,1.79_pm_r,241._pm_r,10._pm_r,317._pm_r,0.49_pm_r,51._pm_r, &
216.70_pm_r,69544._pm_r,20._pm_r,328._pm_r,1.17_pm_r,250._pm_r,10._pm_r,322._pm_r,0.62_pm_r,50._pm_r, &
213.30_pm_r,72688._pm_r,21._pm_r,325._pm_r,0.74_pm_r,275._pm_r,10._pm_r,327._pm_r,0.74_pm_r,51._pm_r, &
211.30_pm_r,75797._pm_r,21._pm_r,323._pm_r,0.65_pm_r,313._pm_r,10._pm_r,334._pm_r,0.83_pm_r,52._pm_r, &
210.00_pm_r,78879._pm_r,22._pm_r,324._pm_r,0.75_pm_r,338._pm_r,10._pm_r,341._pm_r,0.88_pm_r,52._pm_r, &
207.90_pm_r,81948._pm_r,24._pm_r,325._pm_r,0.84_pm_r,349._pm_r,11._pm_r,347._pm_r,0.86_pm_r,53._pm_r, &
202.30_pm_r,84968._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.50_pm_r,87868._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.60_pm_r,90666._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.40_pm_r,93433._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
187.30_pm_r,96205._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
188.20_pm_r,98999._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
191.30_pm_r,101849._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
201.20_pm_r,104828._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.40_pm_r,108098._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
279.50_pm_r,112018._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
363.60_pm_r,117178._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_40= (/ &
285.90_pm_r,-33._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
260.40_pm_r,3966._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
235.50_pm_r,7596._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.80_pm_r,10922._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.00_pm_r,14090._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.20_pm_r,17215._pm_r,10._pm_r,270._pm_r,3.69_pm_r,167._pm_r,6._pm_r,353._pm_r,0.74_pm_r,131._pm_r, &
214.40_pm_r,20345._pm_r,10._pm_r,234._pm_r,4.80_pm_r,156._pm_r,5._pm_r,2._pm_r,0.75_pm_r,122._pm_r, &
216.70_pm_r,23503._pm_r,13._pm_r,199._pm_r,5.28_pm_r,138._pm_r,5._pm_r,12._pm_r,0.42_pm_r,88._pm_r, &
218.50_pm_r,26688._pm_r,17._pm_r,172._pm_r,5.58_pm_r,112._pm_r,5._pm_r,14._pm_r,0.67_pm_r,355._pm_r, &
223.10_pm_r,29921._pm_r,21._pm_r,149._pm_r,6.12_pm_r,84._pm_r,7._pm_r,6._pm_r,1.57_pm_r,332._pm_r, &
230.00_pm_r,33235._pm_r,25._pm_r,127._pm_r,6.88_pm_r,60._pm_r,9._pm_r,355._pm_r,2.43_pm_r,325._pm_r, &
238.40_pm_r,36667._pm_r,29._pm_r,107._pm_r,7.14_pm_r,40._pm_r,13._pm_r,345._pm_r,2.96_pm_r,319._pm_r, &
248.50_pm_r,40228._pm_r,33._pm_r,89._pm_r,6.77_pm_r,20._pm_r,17._pm_r,338._pm_r,3.04_pm_r,314._pm_r, &
258.20_pm_r,43946._pm_r,36._pm_r,75._pm_r,5.25_pm_r,0._pm_r,20._pm_r,333._pm_r,2.02_pm_r,307._pm_r, &
260.30_pm_r,47753._pm_r,37._pm_r,64._pm_r,4.39_pm_r,335._pm_r,22._pm_r,330._pm_r,0.90_pm_r,299._pm_r, &
254.20_pm_r,51526._pm_r,36._pm_r,54._pm_r,4.19_pm_r,314._pm_r,22._pm_r,329._pm_r,0.07_pm_r,135._pm_r, &
244.30_pm_r,55182._pm_r,35._pm_r,45._pm_r,3.80_pm_r,302._pm_r,22._pm_r,330._pm_r,0.65_pm_r,111._pm_r, &
235.50_pm_r,58690._pm_r,33._pm_r,36._pm_r,3.85_pm_r,291._pm_r,21._pm_r,331._pm_r,0.43_pm_r,119._pm_r, &
230.00_pm_r,62097._pm_r,32._pm_r,27._pm_r,3.40_pm_r,283._pm_r,21._pm_r,332._pm_r,0.19_pm_r,122._pm_r, &
225.90_pm_r,65435._pm_r,31._pm_r,19._pm_r,2.64_pm_r,278._pm_r,21._pm_r,332._pm_r,0.08_pm_r,33._pm_r, &
222.10_pm_r,68715._pm_r,31._pm_r,13._pm_r,1.83_pm_r,275._pm_r,21._pm_r,333._pm_r,0.26_pm_r,9._pm_r, &
220.40_pm_r,71954._pm_r,30._pm_r,9._pm_r,1.12_pm_r,274._pm_r,22._pm_r,333._pm_r,0.44_pm_r,10._pm_r, &
218.50_pm_r,75169._pm_r,30._pm_r,7._pm_r,0.57_pm_r,274._pm_r,22._pm_r,335._pm_r,0.58_pm_r,12._pm_r, &
216.30_pm_r,78351._pm_r,30._pm_r,6._pm_r,0.18_pm_r,276._pm_r,23._pm_r,336._pm_r,0.66_pm_r,13._pm_r, &
214.20_pm_r,81505._pm_r,30._pm_r,5._pm_r,0.07_pm_r,74._pm_r,24._pm_r,337._pm_r,0.69_pm_r,13._pm_r, &
209.10_pm_r,84618._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.20_pm_r,87617._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.00_pm_r,90480._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.70_pm_r,93287._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
189.30_pm_r,96087._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
190.70_pm_r,98912._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.80_pm_r,101803._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.20_pm_r,104836._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
227.70_pm_r,108161._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
278.60_pm_r,112100._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
359.50_pm_r,117210._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_50= (/ &
279.60_pm_r,-87._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
253.60_pm_r,3812._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
230.00_pm_r,7349._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
218.30_pm_r,10628._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.10_pm_r,13812._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.60_pm_r,16988._pm_r,16._pm_r,309._pm_r,7.59_pm_r,194._pm_r,9._pm_r,33._pm_r,0.52_pm_r,6._pm_r, &
215.90_pm_r,20156._pm_r,15._pm_r,259._pm_r,10.25_pm_r,186._pm_r,10._pm_r,30._pm_r,0.88_pm_r,5._pm_r, &
214.90_pm_r,23309._pm_r,24._pm_r,218._pm_r,11.45_pm_r,174._pm_r,11._pm_r,26._pm_r,1.30_pm_r,2._pm_r, &
214.60_pm_r,26452._pm_r,37._pm_r,197._pm_r,11.14_pm_r,156._pm_r,13._pm_r,22._pm_r,1.67_pm_r,357._pm_r, &
216.80_pm_r,29610._pm_r,48._pm_r,182._pm_r,10.36_pm_r,130._pm_r,16._pm_r,17._pm_r,1.92_pm_r,350._pm_r, &
220.10_pm_r,32807._pm_r,55._pm_r,168._pm_r,10.40_pm_r,100._pm_r,18._pm_r,12._pm_r,2.07_pm_r,342._pm_r, &
226.70_pm_r,36078._pm_r,59._pm_r,153._pm_r,10.69_pm_r,73._pm_r,21._pm_r,8._pm_r,2.06_pm_r,332._pm_r, &
235.80_pm_r,39460._pm_r,61._pm_r,138._pm_r,10.47_pm_r,50._pm_r,23._pm_r,3._pm_r,1.94_pm_r,323._pm_r, &
245.60_pm_r,42991._pm_r,61._pm_r,125._pm_r,8.55_pm_r,33._pm_r,25._pm_r,358._pm_r,1.58_pm_r,299._pm_r, &
252.50_pm_r,46641._pm_r,60._pm_r,115._pm_r,6.89_pm_r,15._pm_r,25._pm_r,354._pm_r,1.20_pm_r,272._pm_r, &
252.80_pm_r,50349._pm_r,58._pm_r,106._pm_r,5.92_pm_r,357._pm_r,25._pm_r,351._pm_r,0.85_pm_r,228._pm_r, &
246.70_pm_r,54013._pm_r,55._pm_r,98._pm_r,5.25_pm_r,342._pm_r,24._pm_r,349._pm_r,1.03_pm_r,169._pm_r, &
240.10_pm_r,57574._pm_r,50._pm_r,91._pm_r,5.49_pm_r,319._pm_r,23._pm_r,350._pm_r,1.01_pm_r,148._pm_r, &
234.80_pm_r,61052._pm_r,45._pm_r,85._pm_r,5.54_pm_r,303._pm_r,21._pm_r,352._pm_r,0.94_pm_r,127._pm_r, &
231.30_pm_r,64461._pm_r,38._pm_r,79._pm_r,5.19_pm_r,291._pm_r,21._pm_r,355._pm_r,0.86_pm_r,103._pm_r, &
228.80_pm_r,67830._pm_r,32._pm_r,73._pm_r,4.58_pm_r,281._pm_r,21._pm_r,359._pm_r,0.89_pm_r,81._pm_r, &
226.20_pm_r,71161._pm_r,26._pm_r,67._pm_r,3.93_pm_r,272._pm_r,21._pm_r,2._pm_r,0.97_pm_r,65._pm_r, &
223.70_pm_r,74456._pm_r,21._pm_r,62._pm_r,3.38_pm_r,263._pm_r,22._pm_r,5._pm_r,1.07_pm_r,55._pm_r, &
221.50_pm_r,77716._pm_r,17._pm_r,57._pm_r,2.93_pm_r,255._pm_r,23._pm_r,8._pm_r,1.12_pm_r,48._pm_r, &
219.40_pm_r,80944._pm_r,13._pm_r,53._pm_r,2.52_pm_r,248._pm_r,24._pm_r,11._pm_r,1.11_pm_r,44._pm_r, &
215.10_pm_r,84138._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.80_pm_r,87242._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
197.60_pm_r,90196._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
193.60_pm_r,93069._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
192.60_pm_r,95918._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
194.30_pm_r,98792._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
199.50_pm_r,101743._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
210.60_pm_r,104851._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
232.20_pm_r,108252._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
277.70_pm_r,112224._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
355.80_pm_r,117278._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_60= (/ &
268.70_pm_r,-94._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
249.10_pm_r,3690._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
226.50_pm_r,7165._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
217.60_pm_r,10410._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.90_pm_r,13585._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.00_pm_r,16756._pm_r,13._pm_r,317._pm_r,9.36_pm_r,212._pm_r,13._pm_r,40._pm_r,1.33_pm_r,18._pm_r, &
214.20_pm_r,19908._pm_r,17._pm_r,254._pm_r,12.95_pm_r,206._pm_r,15._pm_r,36._pm_r,1.90_pm_r,16._pm_r, &
210.90_pm_r,23019._pm_r,34._pm_r,225._pm_r,14.92_pm_r,195._pm_r,18._pm_r,32._pm_r,2.25_pm_r,13._pm_r, &
208.60_pm_r,26088._pm_r,53._pm_r,211._pm_r,14.05_pm_r,180._pm_r,21._pm_r,29._pm_r,2.14_pm_r,8._pm_r, &
210.40_pm_r,29154._pm_r,68._pm_r,200._pm_r,12.10_pm_r,155._pm_r,23._pm_r,26._pm_r,1.68_pm_r,356._pm_r, &
214.10_pm_r,32260._pm_r,78._pm_r,189._pm_r,11.39_pm_r,122._pm_r,25._pm_r,23._pm_r,1.20_pm_r,332._pm_r, &
220.70_pm_r,35443._pm_r,81._pm_r,177._pm_r,11.91_pm_r,92._pm_r,26._pm_r,20._pm_r,0.98_pm_r,293._pm_r, &
229.70_pm_r,38736._pm_r,82._pm_r,165._pm_r,12.09_pm_r,71._pm_r,25._pm_r,17._pm_r,1.05_pm_r,262._pm_r, &
239.60_pm_r,42177._pm_r,80._pm_r,154._pm_r,10.00_pm_r,57._pm_r,24._pm_r,13._pm_r,1.18_pm_r,242._pm_r, &
249.00_pm_r,45754._pm_r,78._pm_r,144._pm_r,8.05_pm_r,44._pm_r,23._pm_r,11._pm_r,1.15_pm_r,230._pm_r, &
254.10_pm_r,49447._pm_r,76._pm_r,136._pm_r,6.89_pm_r,32._pm_r,22._pm_r,8._pm_r,1.08_pm_r,213._pm_r, &
251.90_pm_r,53159._pm_r,73._pm_r,129._pm_r,6.28_pm_r,20._pm_r,20._pm_r,7._pm_r,1.09_pm_r,192._pm_r, &
247.60_pm_r,56815._pm_r,69._pm_r,123._pm_r,6.03_pm_r,354._pm_r,19._pm_r,8._pm_r,0.90_pm_r,171._pm_r, &
243.20_pm_r,60410._pm_r,62._pm_r,118._pm_r,6.11_pm_r,331._pm_r,18._pm_r,10._pm_r,0.72_pm_r,142._pm_r, &
239.20_pm_r,63942._pm_r,54._pm_r,114._pm_r,6.08_pm_r,314._pm_r,17._pm_r,13._pm_r,0.72_pm_r,107._pm_r, &
235.00_pm_r,67415._pm_r,46._pm_r,111._pm_r,5.89_pm_r,300._pm_r,18._pm_r,16._pm_r,0.88_pm_r,81._pm_r, &
231.40_pm_r,70829._pm_r,37._pm_r,111._pm_r,5.67_pm_r,289._pm_r,18._pm_r,20._pm_r,1.09_pm_r,67._pm_r, &
227.70_pm_r,74191._pm_r,29._pm_r,113._pm_r,5.47_pm_r,278._pm_r,20._pm_r,23._pm_r,1.26_pm_r,59._pm_r, &
224.60_pm_r,77498._pm_r,22._pm_r,119._pm_r,5.24_pm_r,270._pm_r,21._pm_r,26._pm_r,1.35_pm_r,56._pm_r, &
222.60_pm_r,80764._pm_r,16._pm_r,133._pm_r,4.88_pm_r,264._pm_r,23._pm_r,29._pm_r,1.33_pm_r,53._pm_r, &
220.00_pm_r,84014._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
213.40_pm_r,87217._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.70_pm_r,90272._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
198.20_pm_r,93221._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
196.80_pm_r,96132._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
198.50_pm_r,99065._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
204.80_pm_r,102085._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
216.40_pm_r,105275._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
236.90_pm_r,108756._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
276.20_pm_r,112756._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
352.20_pm_r,117750._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)

  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_70= (/ &
257.20_pm_r,-80._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
245.40_pm_r,3590._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
223.80_pm_r,7016._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
215.90_pm_r,10227._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.50_pm_r,13370._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.70_pm_r,16499._pm_r,4._pm_r,313._pm_r,7.63_pm_r,227._pm_r,10._pm_r,32._pm_r,1.50_pm_r,57._pm_r, &
209.80_pm_r,19595._pm_r,14._pm_r,241._pm_r,10.74_pm_r,220._pm_r,12._pm_r,37._pm_r,2.00_pm_r,52._pm_r, &
205.00_pm_r,22631._pm_r,30._pm_r,226._pm_r,12.46_pm_r,208._pm_r,15._pm_r,39._pm_r,2.10_pm_r,43._pm_r, &
200.30_pm_r,25598._pm_r,48._pm_r,217._pm_r,12.26_pm_r,192._pm_r,18._pm_r,38._pm_r,1.70_pm_r,25._pm_r, &
201.00_pm_r,28530._pm_r,62._pm_r,207._pm_r,10.95_pm_r,166._pm_r,20._pm_r,35._pm_r,1.29_pm_r,341._pm_r, &
207.60_pm_r,31516._pm_r,72._pm_r,197._pm_r,10.36_pm_r,135._pm_r,20._pm_r,30._pm_r,1.63_pm_r,294._pm_r, &
216.70_pm_r,34625._pm_r,76._pm_r,186._pm_r,10.57_pm_r,108._pm_r,19._pm_r,22._pm_r,2.10_pm_r,271._pm_r, &
226.90_pm_r,37869._pm_r,79._pm_r,175._pm_r,10.34_pm_r,88._pm_r,18._pm_r,13._pm_r,2.19_pm_r,257._pm_r, &
238.10_pm_r,41278._pm_r,79._pm_r,165._pm_r,8.28_pm_r,74._pm_r,17._pm_r,5._pm_r,1.57_pm_r,242._pm_r, &
249.20_pm_r,44844._pm_r,78._pm_r,157._pm_r,6.73_pm_r,63._pm_r,16._pm_r,360._pm_r,0.92_pm_r,225._pm_r, &
257.50_pm_r,48563._pm_r,77._pm_r,150._pm_r,5.94_pm_r,52._pm_r,15._pm_r,358._pm_r,0.43_pm_r,197._pm_r, &
258.30_pm_r,52348._pm_r,76._pm_r,144._pm_r,5.45_pm_r,41._pm_r,14._pm_r,358._pm_r,0.23_pm_r,128._pm_r, &
254.50_pm_r,56105._pm_r,73._pm_r,139._pm_r,4.80_pm_r,13._pm_r,14._pm_r,360._pm_r,0.45_pm_r,100._pm_r, &
248.20_pm_r,59789._pm_r,68._pm_r,135._pm_r,4.72_pm_r,347._pm_r,14._pm_r,3._pm_r,0.60_pm_r,90._pm_r, &
241.50_pm_r,63372._pm_r,62._pm_r,133._pm_r,4.81_pm_r,327._pm_r,14._pm_r,6._pm_r,0.62_pm_r,85._pm_r, &
236.30_pm_r,66870._pm_r,55._pm_r,132._pm_r,4.83_pm_r,313._pm_r,15._pm_r,10._pm_r,0.53_pm_r,80._pm_r, &
232.20_pm_r,70299._pm_r,48._pm_r,133._pm_r,4.78_pm_r,302._pm_r,15._pm_r,12._pm_r,0.41_pm_r,77._pm_r, &
228.70_pm_r,73674._pm_r,41._pm_r,135._pm_r,4.73_pm_r,292._pm_r,15._pm_r,14._pm_r,0.26_pm_r,70._pm_r, &
227.10_pm_r,77008._pm_r,35._pm_r,140._pm_r,4.63_pm_r,286._pm_r,15._pm_r,15._pm_r,0.14_pm_r,63._pm_r, &
226.00_pm_r,80322._pm_r,30._pm_r,148._pm_r,4.39_pm_r,281._pm_r,15._pm_r,15._pm_r,0.06_pm_r,38._pm_r, &
224.50_pm_r,83622._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
219.10_pm_r,86892._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.20_pm_r,90032._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.80_pm_r,93051._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
200.70_pm_r,96023._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
202.80_pm_r,99012._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.50_pm_r,102097._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.40_pm_r,105360._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
240.50_pm_r,108908._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
274.10_pm_r,112921._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
349.30_pm_r,117856._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /) 
  real(pm_reel), dimension(10*nb_alt), parameter :: donnees_lat_80= (/ &
251.70_pm_r,-111._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.50_pm_r,3496._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
221.90_pm_r,6886._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
214.80_pm_r,10073._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.10_pm_r,13189._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
209.30_pm_r,16273._pm_r,2._pm_r,179._pm_r,3.81_pm_r,243._pm_r,4._pm_r,11._pm_r,0.86_pm_r,93._pm_r, &
205.70_pm_r,19316._pm_r,8._pm_r,227._pm_r,5.49_pm_r,234._pm_r,5._pm_r,28._pm_r,1.05_pm_r,89._pm_r, &
199.70_pm_r,22282._pm_r,17._pm_r,227._pm_r,6.52_pm_r,220._pm_r,6._pm_r,40._pm_r,0.87_pm_r,78._pm_r, &
193.50_pm_r,25161._pm_r,26._pm_r,221._pm_r,6.71_pm_r,200._pm_r,7._pm_r,44._pm_r,0.45_pm_r,33._pm_r, &
194.70_pm_r,27993._pm_r,35._pm_r,213._pm_r,6.35_pm_r,176._pm_r,7._pm_r,38._pm_r,0.77_pm_r,313._pm_r, &
204.90_pm_r,30909._pm_r,41._pm_r,203._pm_r,6.13_pm_r,149._pm_r,7._pm_r,25._pm_r,1.34_pm_r,294._pm_r, &
216.40_pm_r,33999._pm_r,45._pm_r,193._pm_r,6.09_pm_r,128._pm_r,7._pm_r,8._pm_r,1.57_pm_r,287._pm_r, &
227.20_pm_r,37244._pm_r,49._pm_r,183._pm_r,5.77_pm_r,110._pm_r,8._pm_r,350._pm_r,1.45_pm_r,280._pm_r, &
237.90_pm_r,40655._pm_r,51._pm_r,175._pm_r,4.33_pm_r,98._pm_r,8._pm_r,340._pm_r,0.72_pm_r,273._pm_r, &
248.70_pm_r,44214._pm_r,52._pm_r,169._pm_r,3.21_pm_r,84._pm_r,8._pm_r,336._pm_r,0.12_pm_r,318._pm_r, &
258.00_pm_r,47932._pm_r,52._pm_r,164._pm_r,2.77_pm_r,66._pm_r,9._pm_r,338._pm_r,0.43_pm_r,43._pm_r, &
260.30_pm_r,51735._pm_r,51._pm_r,159._pm_r,2.79_pm_r,49._pm_r,9._pm_r,343._pm_r,0.71_pm_r,34._pm_r, &
257.20_pm_r,55531._pm_r,48._pm_r,155._pm_r,2.78_pm_r,27._pm_r,10._pm_r,348._pm_r,0.63_pm_r,40._pm_r, &
250.20_pm_r,59251._pm_r,46._pm_r,152._pm_r,2.77_pm_r,8._pm_r,10._pm_r,351._pm_r,0.49_pm_r,47._pm_r, &
242.40_pm_r,62855._pm_r,42._pm_r,149._pm_r,2.67_pm_r,352._pm_r,11._pm_r,354._pm_r,0.33_pm_r,64._pm_r, &
237.00_pm_r,66364._pm_r,38._pm_r,148._pm_r,2.54_pm_r,338._pm_r,11._pm_r,357._pm_r,0.24_pm_r,95._pm_r, &
233.20_pm_r,69806._pm_r,35._pm_r,147._pm_r,2.46_pm_r,326._pm_r,10._pm_r,358._pm_r,0.24_pm_r,138._pm_r, &
230.20_pm_r,73198._pm_r,31._pm_r,148._pm_r,2.42_pm_r,315._pm_r,10._pm_r,359._pm_r,0.34_pm_r,163._pm_r, &
229.70_pm_r,76563._pm_r,28._pm_r,150._pm_r,2.40_pm_r,307._pm_r,10._pm_r,360._pm_r,0.41_pm_r,172._pm_r, &
229.10_pm_r,79924._pm_r,25._pm_r,154._pm_r,2.31_pm_r,301._pm_r,9._pm_r,360._pm_r,0.46_pm_r,179._pm_r, &
227.70_pm_r,83265._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
222.70_pm_r,86579._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.90_pm_r,89774._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
206.00_pm_r,92844._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
203.50_pm_r,95858._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
205.70_pm_r,98889._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
212.80_pm_r,102019._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
224.80_pm_r,105332._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
242.60_pm_r,108923._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
272.10_pm_r,112937._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r, &
347.20_pm_r,117828._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_r,0._pm_r,0._pm_r,0.00_pm_r,0._pm_reel /)


  real(pm_reel), dimension(10*nb_lat*nb_alt), parameter :: donnees_decembre = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel), dimension(10, nb_alt, nb_lat) :: donnees=reshape(donnees_decembre,(/10,nb_alt,nb_lat/))


  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)


end subroutine cps_atmi_decembre

subroutine cps_atmi_inter (lat,long,tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,tarra,zarra,retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_inter
!
!$Resume
!  Realisation des INTERpolations pour le modele atmospherique cira.
!
!$Description
!  Realisation des INTERpolations pour le modele atmospherique cira.
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_inter (lat,long,tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,tarra,zarra,retour )
!.    real(pm_reel) :: lat 
!.    real(pm_reel) :: long 
!.    real(pm_reel), dimension(:,:) :: tbar, zbar
!.    real(pm_reel), dimension(:,:) :: z1, phi1 
!.    real(pm_reel), dimension(:,:) :: t1, phit1 
!.    real(pm_reel), dimension(:,:) :: z2, phi2 
!.    real(pm_reel), dimension(:,:) :: t2, phit2 
!.    real(pm_reel), dimension(:) :: tarra 
!.    real(pm_reel), dimension(:) :: zarra 
!.    integer :: retour
!
!$Arguments
!>E     lat     :<pm_reel>             latitude(rad)
!>E     long    :<pm_reel>             longitude(rad)
!>E     tbar    :<pm_reel,DIM=(:,:)>   temperatures (Kelvin)
!>E     zbar    :<pm_reel,DIM=(:,:)>   altitudes (m)
!>E     z1      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 1
!>E     phi1    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 1
!>E     t1      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 1
!>E     phit1   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 1
!>E     z2      :<pm_reel,DIM=(:,:)>   amplitude de l'altitude de l'onde 2
!>E     phi2    :<pm_reel,DIM=(:,:)>   phase de l'altitude de l'onde 2
!>E     t2      :<pm_reel,DIM=(:,:)>   amplitude de la temperature de l'onde 2
!>E     phit2   :<pm_reel,DIM=(:,:)>   phase de la temperature de l'onde 2 
!>Z     tarra   :<pm_reel,DIM=(:)>     interpolation sur la température
!>S     zarra   :<pm_reel,DIM=(:)>     interpolation sur l'altitude
!>S     retour  :<integer>            
!
!$Common
!
!$Routines
!- cps_atmi_temp
!- cps_atmi_alt
!
!$Include
!
!$Module
!#V
!- mslib
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

   ! Modules
  ! =======
  use mslib

  ! Declarations
  ! ============
  implicit none


  real(pm_reel), intent(in) :: lat  ! latitude
  real(pm_reel), intent(in) :: long ! longitude
  real(pm_reel), dimension(:,:), intent(in) :: tbar, zbar! temperatures et altitudes
  real(pm_reel), dimension(:,:), intent(in) :: z1, phi1  ! amplitude et phase de l'altitude de l'onde 1
  real(pm_reel), dimension(:,:), intent(in) :: t1, phit1 ! amplitude et phase de la temperature de l'onde 1
  real(pm_reel), dimension(:,:), intent(in) :: z2, phi2  ! amplitude et phase de l'altitude de l'onde 2
  real(pm_reel), dimension(:,:), intent(in) :: t2, phit2 ! amplitude et phase de la temperature de l'onde 2 
  real(pm_reel), dimension(:), intent(out)  :: tarra     ! interpolation des temperatures
  real(pm_reel), dimension(:), intent(out)  :: zarra     ! interpolation des altitudes
  integer, intent(out)                      :: retour
  
  ! Autres declarations
  ! -------------------
  integer, parameter :: nb_alt = 36 

  real(pm_reel) :: eps100          ! epsilon de comparaison pour les reels
  integer       :: latmin, latmax  ! latitudes minimum et maximum
  real(pm_reel), dimension(nb_alt, 2) :: tempe, altit 
  integer       :: i               ! indice de boucle
  real(pm_reel) :: latmi, latma
  real(pm_reel) :: temp1, temp2, alt1, alt2, tar, zar




  !************************************************************************
  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  ! initialisations 
  !...................................................
  eps100 = 100._pm_reel * epsilon(1._pm_reel)  !epsilon de test pour les reels


  latmin = int (lat/10._pm_reel) + 9           ! latitude minimum
  latmax = latmin                              ! latitude maximum


  !     test si (lat/10 + 9) differe de latmin
  !     la precision du test est en deca du maximum de precision 
  !     possible sur la latitude en radians

  if ( abs( lat/10._pm_reel+9._pm_reel - real(latmin,pm_reel))  >  eps100 ) then

     if (lat < 0._pm_reel) then
        latmin = latmin - 1
     else
        latmax = latmax + 1
     end if

  end if

  ! calcul des temperatures
  ! -----------------------

call cps_atmi_temp (long,latmin,latmax ,tbar,t1,phit1,t2,phit2, tempe,retour)
  if (retour < pm_OK) go to 6000

  ! calcul des altitudes
  ! --------------------

call cps_atmi_alt (long,latmin,latmax,zbar,z1,phi1,z2,phi2,altit,retour)
  if (retour < pm_OK) go to 6000

  ! realisation de l'interpolation
  ! ------------------------------

  if ( latmin == latmax ) then
     tarra (:) = tempe (:,1)
     zarra (:) = altit (:,1)
  else
     latmi     = real(latmin,pm_reel) * 10._pm_reel - 90._pm_reel
     latma     = real(latmax,pm_reel) * 10._pm_reel - 90._pm_reel
     do i =1, nb_alt
        temp1 = tempe(i,1)
        temp2 = tempe(i,2)
        tar = cps_interp_newton (temp1,temp2,latmi,latma,lat)
        tarra(i) = tar
        alt1 = altit(i,1)
        alt2 = altit(i,2)
        zar = cps_interp_newton (alt1,alt2,latmi,latma,lat)
        zarra(i) = zar
     end do
  end if

6000 continue


   end subroutine cps_atmi_inter

subroutine cps_atmi_alt ( long, latmin, latmax, zbar, z1, phi1, z2, phi2, altit, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_alt
!
!$Resume
!   Pour l'interpolation dans le modele ATMospherique cira, calcul des
!    ALTitudes
!
!$Description
!   Pour l'interpolation dans le modele ATMospherique cira, calcul des
!    ALTitudes
!
!$Auteur
!  Julien Bouillant (ATOS ORGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_alt ( long, latmin, latmax, zbar, z1, phi1, z2, phi2, altit, retour )
!.    real(pm_reel) :: long 
!.    integer :: latmin 
!.    integer :: latmax 
!.    real(pm_reel), dimension(:,:) :: zbar 
!.    real(pm_reel), dimension(:,:) :: z1, phi1 
!.    real(pm_reel), dimension(:,:) :: z2, phi2 
!.    real(pm_reel),dimension(:,:) :: altit 
!.    integer :: retour
!
!$Arguments
!>E     long    :<pm_reel>             longitude (rad)
!>E     latmin  :<integer>             latitude minimum(rad)
!>E     latmax  :<integer>             matitude maximum (rad)
!>E     zbar    :<pm_reel,DIM=(:,:)>   altitude (m)
!>E     z1      :<pm_reel,DIM=(:,:)>   amplitudes et phases des altitudes de l'onde 1
!>E     phi1    :<pm_reel,DIM=(:,:)>   
!>E     z2      :<pm_reel,DIM=(:,:)>   amplitude et phase de la temperature de l'onde 2
!>E     phi2    :<pm_reel,DIM=(:,:)>   
!>S     altit   :<pm_reel,DIM=(:,:)>   altitudes en sortie
!>S     retour  :<integer>             
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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

  ! (C) Copyright CNES - MSPRO - 2001

  ! Modules
  ! =======
  use mslib

  ! Declarations
  ! ============
  implicit none

real(pm_reel), intent(in) :: long ! longitude
integer, intent(in)                                        ::  latmin   ! latitude minimum
integer, intent(in)                                        ::  latmax   ! latitude maximum
real(pm_reel), dimension(:,:), intent(in) ::  zbar     ! altitudes
! amplitude et phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(in) ::  z1, phi1 ! amplitudes et phases des altitudes de l'onde 1
! amplitude et phase de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(in) ::  z2, phi2 ! amplitudes et phases des altitudes de l'onde 2
real(pm_reel),dimension(:,:), intent(out) ::  altit    ! altitudes en sortie
integer, intent(out)                      ::  retour

  ! Autres declarations
  ! -------------------

  real(pm_reel) :: zlat, tmp, gravi ! variables intermediaires de calcul
  integer       :: i,j              ! indices de boucle

  integer, parameter :: nb_alt = 36 
   
  real(pm_reel) :: rt     = 6371220._pm_reel  ! rayon terrestre pour le modele
  real(pm_reel) :: rv1    = 2637.e-06_pm_reel
  real(pm_reel) :: rv2    =   59.e-07_pm_reel
  real(pm_reel) :: rv3    = 1000._pm_reel


  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  do i = latmin,latmax
     zlat = real(i,pm_reel) * 10._pm_reel -90._pm_reel
     do j = 1, nb_alt
        tmp = zbar(j,i)+z1(j,i)*10._pm_reel*cos(pm_deg_rad*(long-phi1(j,i))) &
             +z2(j,i)*10._pm_reel*cos(pm_deg_rad*(2._pm_reel*long-phi2(j,i)))

        ! note--actual mks (si) gravity is gravity*9.81

        gravi = 1._pm_reel - rv1 *  cos(pm_deg_rad*zlat*2._pm_reel)&
             + rv2 * (cos(pm_deg_rad*zlat*2._pm_reel))**2._pm_reel
        altit(j,i-latmin+1) = rt*tmp/(rt*gravi-tmp)/rv3
     end do
  end do


end subroutine cps_atmi_alt

subroutine cps_atmi_temp ( long, latmin, latmax, tbar, t1, phit1, t2, phit2, tempe, retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmi_temp
!
!$Resume
!   Pour l'interpolation dans le modele ATMospherique de cira, calcul des
!   TEMPeratures.
!
!$Description
!   Pour l'interpolation dans le modele ATMospherique de cira, calcul des
!   TEMPeratures.
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmi_temp ( long, latmin, latmax, tbar, t1, phit1, t2, phit2, tempe, retour )
!.    real(pm_reel) :: long 
!.    integer :: latmin 
!.    integer :: latmax 
!.    real(pm_reel), dimension(:,:) :: tbar 
!.    real(pm_reel), dimension(:,:) :: t1, phit1 
!.    real(pm_reel), dimension(:,:) :: t2, phit2 
!.    real(pm_reel),dimension(:,:) :: tempe 
!.    integer :: retour
!
!$Arguments
!>E     long    :<pm_reel>             longitude
!>E     latmin  :<integer>             latitude minimum
!>E     latmax  :<integer>             latitude maximum
!>E     tbar    :<pm_reel,DIM=(:,:)>   température (Kelvin)
!>E     t1      :<pm_reel,DIM=(:,:)>   amplitude et phase de la temperature de l'onde 1
!>E     phit1   :<pm_reel,DIM=(:,:)>   
!>E     t2      :<pm_reel,DIM=(:,:)>   amplitude et phase de la temperature de l'onde 2
!>E     phit2   :<pm_reel,DIM=(:,:)>   
!>S     tempe   :<pm_reel,DIM=(:,:)>   temperature
!>S     retour  :<integer>             
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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
  ! Modules
  ! =======
  use mslib
  ! Declarations
  ! ============
  implicit none

real(pm_reel), intent(in)                 :: long      ! longitude
integer, intent(in)                       :: latmin    ! latitude minimum
integer, intent(in)                       :: latmax    ! latitude maximum
real(pm_reel), dimension(:,:), intent(in) :: tbar      ! temperatures
real(pm_reel), dimension(:,:), intent(in) :: t1, phit1 ! amplitude et phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(in) :: t2, phit2 ! amplitude et phase de la temperature de l'onde 2
real(pm_reel),dimension(:,:), intent(out) :: tempe     ! temperature
integer, intent(out)                      :: retour


  ! Autres declarations
  ! -------------------

  integer :: i,kk ! indices de boucle



  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  do i = latmin , latmax
        kk      = i-latmin+1
        tempe (:,kk) = tbar(:,i)+t1(:,i)* &
                       cos(pm_deg_rad*(long-phit1(:,i)))+t2(:,i)*cos(pm_deg_rad*(2._pm_reel*long-phit2(:,i)))
     end do

end subroutine cps_atmi_temp

subroutine cps_atmo (ralti1,rtarra,rzarra,rpress,rtemp,rpres,rdens,retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmo
!
!$Resume
!  Calcul de la temperature, pression, densite du modele ATMOspherique cira.
!
!$Description
!  Calcul de la temperature, pression, densite du modele ATMOspherique cira.
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmo (ralti1,rtarra,rzarra,rpress,rtemp,rpres,rdens,retour )
!.    real(pm_reel) :: ralti1 
!.    real(pm_reel), dimension(:) :: rtarra 
!.    real(pm_reel), dimension(:) :: rzarra 
!.    real(pm_reel), dimension(:) :: rpress 
!.    real(pm_reel) :: rtemp 
!.    real(pm_reel) :: rpres 
!.    real(pm_reel) :: rdens 
!.    integer :: retour
!
!$Arguments
!>E     ralti1  :<pm_reel>           altitude (m)
!>E     rtarra  :<pm_reel,DIM=(:)>   interplation des temperatures
!>E     rzarra  :<pm_reel,DIM=(:)>   interpolation des altitude
!>E     rpress  :<pm_reel,DIM=(:)>   tableau des pressions
!>S     rtemp   :<pm_reel>           temperature (Kelvin)
!>S     rpres   :<pm_reel>           pression (Pa)
!>S     rdens   :<pm_reel>           densite atmospherique (kg.m-3)
!>S     retour  :<integer>           
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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


  ! Modules
  ! =======
  use mslib

  ! Declarations
  ! ============
  implicit none

  real(pm_reel), intent(in)               ::  ralti1 ! altitude
  real(pm_reel), dimension(:), intent(in) ::  rtarra ! interplation des temperatures
  real(pm_reel), dimension(:), intent(in) ::  rzarra ! interpolation des altitude
  real(pm_reel), dimension(:), intent(in) ::  rpress ! tableau des pressions
  real(pm_reel), intent(out)              ::  rtemp  ! temperature
  real(pm_reel), intent(out)              ::  rpres  ! pression
  real(pm_reel), intent(out)              ::  rdens  ! densite atmospherique
  integer, intent(out)                    ::  retour
  
  ! Autres declarations
  ! -------------------
  integer, parameter :: nb_alt = 36
  real(pm_reel) :: ro     = 2.8705_pm_reel

  real(pm_reel) :: alti, rgrad, rbeta, ralfa ! variables intermediaires de calcul
  integer       :: i                         ! indice de boucle
  real(pm_reel) :: temperature, pression     ! variables intermediaires du calcul de la temperature et de
                                             ! la pression
  logical       :: termine                   ! flag d'arret de boucle



  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK


  ! Calculs
  ! .......

  i    = 0
  termine = .false.

  do while (.not.termine)
     i    = i + 1
     if ((ralti1 <= rzarra(i+1)) .or. (i >= nb_alt-1)) termine = .true.
  end do

  ! calcul de la temperature

  alti = ralti1  
  temperature = cps_interp_newton (rtarra(i),rtarra(i+1),rzarra(i),rzarra(i+1),alti)

  rtemp = temperature
  rgrad = (rtarra(i+1)-rtarra(i)) / (rzarra(i+1)-rzarra(i))
  

  ! calcul de la pression
 
  if ( abs(rgrad) >= 1.e-3_pm_reel ) then
     rbeta = log(rpress(i+1)/rpress(i)) / log(rtarra(i+1)/rtarra(i))
     pression = (temperature/rtarra(i))**rbeta * rpress(i)
  else
     ralfa = (ralti1-rzarra(i)) / (rzarra(i+1)-rzarra(i))
     pression = rpress(i) * (rpress(i+1)/rpress(i))**ralfa
  end if
  rpres = pression
  

  ! calcul de la densite atmospherique

  rdens = pression / ( ro * temperature )


end subroutine cps_atmo

 function cps_interp_newton ( A1, A2, B1, B2, B )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_interp_newton
!
!$Resume
!  INTERPolation de NEWTON 
!
!$Description
!  INTERPolation de NEWTON 
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.     function cps_interp_newton ( A1, A2, B1, B2, B )
!.    real(pm_reel) :: a1 
!.    real(pm_reel) :: a2 
!.    real(pm_reel) :: b1 
!.    real(pm_reel) :: b2 
!.    real(pm_reel) :: b 
!
!$Arguments
!>E     A1  :<pm_reel>   element 1 de type a
!>E     A2  :<pm_reel>   element 2 de type a
!>E     B1  :<pm_reel>   element 1 de type b
!>E     B2  :<pm_reel>   element 2 de type b
!>E     B   :<pm_reel>   grandeur physique
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
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

  ! Modules
  ! =======
  use mslib

  ! Declarations
  ! ============
  implicit none

  real(pm_reel), intent(in) :: a1 ! element 1 de type a
  real(pm_reel), intent(in) :: a2 ! element 2 de type a
  real(pm_reel), intent(in) :: b1 ! element 1 de type b
  real(pm_reel), intent(in) :: b2 ! element 2 de type b
  real(pm_reel), intent(in) :: b  ! grandeur physique
  real(pm_reel)             :: cps_interp_newton ! valeur calculee

  ! Autres declarations
  ! -------------------
  real(pm_reel) :: v1  = 1.e-05_pm_reel    ! parametre associe a la fonction mui_interp_newton


  !************************************************************************

  if ( abs(b2-b1) < v1 ) then
     cps_interp_newton = a1
  else
     cps_interp_newton = a1 + (a2-a1)/(b2-b1) * (b-b1)
  end if

end function cps_interp_newton

  
end module cps_atm_cira_mod
