module   CPS_ATMOSPHERE_VENUS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  CPS_ATMOSPHERE_VENUS
!
!$Resume
!  Modèle d'Atmosphère Venus
!
!$Description
! Ce module contient 2 modèles :
! - le modele Venusien PETROPOULOS88
! - le modele Venusien a partir d'un fichier tabulé
!
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_atmosphere_venus.F90 379 2013-02-22 16:59:49Z ffsm $
!
!$Historique
!  $Log: cps_atmosphere_venus.F90,v $
!  Revision 379  2013/02/22 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.8  2010/10/29 12:47:32  mercadig
!  VERSION::AQ::29/10/2010:Suppression des variables inutilisees FORMAT_LIGNE et i (2 occurences)
!
!  Revision 1.7  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.6  2010/10/19 13:11:39  mercadig
!  VERSION::DM-ID:1422:19/10/2010:Ajout de la sortie azimut du vent dans le modele
!
!  Revision 1.5  2010/05/05 09:45:12  jlrobin
!  V2.9::DM-ID:1361:05/05/2010:portage vers LINUX du modele atm Venus TAB
!
!  Revision 1.4  2010/04/30 14:12:37  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.3.2.8  2010/04/06 09:07:33  jlrobin
!  V2.9::DM-ID:456:05/04/2010: gestion des altitudes en dehors des bornes
!
!  Revision 1.3.2.7  2010/04/02 15:01:00  jlrobin
!  V2.9::DM-ID:456:01/04/2010:symetrie des heures sol locales
!
!  Revision 1.3.2.3  2010/03/29 07:58:06  jlrobin
!  V2.9::DM-ID:461:29/03/2010:Intgration avec CRASH
!
!  Revision 1.3.2.2  2010/03/09 12:39:14  fabrec
!  V2.9::DM-ID:1361:09/03/2010:Ajout du modele d'atmosphere venus tabule
!
!  Revision 1.3  2008/04/07 09:13:56  vivaresf
!  FA-ID 1009 :
!  - rajout de constantes pour chaque modèle
!  - suppression de cps_acces (obsolète)
!  - correction des cartouches
!  - rajout des modèles associés à chaque corps
!
!  Revision 1.2  2008/03/18 16:46:22  vivaresf
!  version 2.4 : rajout des Log pour l'historique
!
!
!$FinHistorique
!
!$Usage
!  use CPS_ATMOSPHERE_VENUS
!
!$Remarques
!
!$Mots-cles
!  ATMOSPHERE VENUS
!
!$Voir-Aussi
!.  cps_atm_venus
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mslib

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atmosphere_venus.F90 379 2013-02-22 16:59:49Z ffsm $'


  character(len=20), parameter :: cps_modatm_petropoulos88="PETROPOULOS88"
  character(len=20), parameter :: cps_modatm_venustabule="VENUS TABULE"

! variables globales pour le modele venus tabule
  integer,save,private :: nb_lignes
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_hsol => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_lat => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_alt => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_dens => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_temp => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_press => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_umin => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_umoy => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_umax => NULL()
  real(kind=pm_reel),save,private, dimension(:), pointer :: tab_azim => NULL()
  integer,save,private :: nb_hsol, nb_lat
  real(kind=pm_reel),save,private :: max_hsol, max_lat, max_alt
  real(kind=pm_reel),save,private, dimension(:,:), pointer :: tab_indhsol => NULL()
  real(kind=pm_reel),save,private, dimension(:,:), pointer :: tab_indlat => NULL()

! tableaux alloués oui/non
  logical, save, private :: modele_init=.false.


contains
  
  subroutine cps_atm_venus (z ,rho, rho0_optional, beta_optional)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_venus
!
!$Resume
!     Modèle B.Petropoulos de l'atmosphère vénusienne (40->160km).
!
!$Description
!     Calcul de la densité rho suivant une loi exponentielle de l'atmosphère
!     de la planète Vénus (Modèle B.Petropoulos de 06/1988).
!     Ce modèle est bien adapté aux altitudes entre 40 et 160 km.
!     En dessous de 40 km, rho est considérée constante et égale à celle de 40km.
!     En dessus de 160 km, rho est considérée nulle.
!>        rho(z) = rho0_optional * exp(- beta_optional * z )
!     avec
!.        z               = Altitude (m)
!.        rho0_optional   = Densité à z0 = 530 kg/m^3
!.        beta_optional   = 1.58*(10^-4) /m
!
!$Version
!     1.0 02/10/2007
!
!$Acces
!  PUBLIC
!
!$Arguments
!>E     z              :<pm_reel>   Altitude (m)
!>S     rho            :<pm_reel>   
!>[E]   rho0_optional  :<pm_reel>   densité à la hauteur de référence z0 (kg/m^3)
!>[E]   beta_optional  :<pm_reel>   constante de Petropoulos
!
!$<>
!***********************************************************************

    implicit none
    ! Variables locales
    real(kind=pm_reel), parameter :: petropoulos88_cste_alt_min = 40.e+03_pm_reel
    real(kind=pm_reel), parameter :: petropoulos88_cste_alt_max = 160.e+03_pm_reel
    real(kind=pm_reel), parameter :: petropoulos88_cste_rho0    = 530.0_pm_reel
    real(kind=pm_reel), parameter :: petropoulos88_cste_beta    = 1.58e-04_pm_reel
    real(kind=pm_reel)  :: rho0, beta

    ! Autres variables
    real(kind=pm_reel), intent (IN) :: z
    real(kind=pm_reel), intent (IN), optional :: rho0_optional, beta_optional
    real(kind=pm_reel), intent (OUT) :: rho
    
    ! initialisation de rho0 et beta
    if (present(rho0_optional)) then
       rho0 = rho0_optional
    else
       rho0 = petropoulos88_cste_rho0
    endif

    if (present(beta_optional)) then
       beta = beta_optional
    else
       beta = petropoulos88_cste_beta
    endif

    ! Calcul de la densité rho en fonction de l'altitude z
    if ( z < petropoulos88_cste_alt_min ) then
       rho = rho0*exp(-beta*petropoulos88_cste_alt_min)
    else if ( z >= petropoulos88_cste_alt_min .and. z <= petropoulos88_cste_alt_max ) then
       rho = rho0*exp(-beta*z)
    else
       rho = 0_pm_reel
    endif

  end subroutine cps_atm_venus


  subroutine cps_atm_ouvrir_fictab(fichier)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_lire_fictab
!
!$Resume
!     Lecture du fichier
!
!$Description
!     Routine de lecture du fichier d'atmosphere tabule et sauvegarde des donnees
!     Recherche des valeurs maximales
!
!$Version
!     1.0 25/02/2010
!
!$Acces
!  PUBLIC
!
!$Arguments
!>E     fichier     :<LEN=*>                nom du fichier tabulé à lire
!
!$<>
!***********************************************************************

   use cps_accesMadona
   use cps_util

    implicit none
 
    ! arguments
    character(LEN=*), intent(in) :: fichier
 
    ! Variables locales
    integer :: ierr, ierr2
    integer :: i, ind
    integer :: unit_read, ligne
    ! Format d'une ligne de commentaire
    character(len=38), parameter :: FORMAT_LIGNE_TXT = '(A)'
    character(len=256) :: buff 

    ! Initialisation

    if (.not.modele_init) then
       if ( associated(tab_hsol)) deallocate(tab_hsol)
       if ( associated(tab_lat)) deallocate(tab_lat)
       if ( associated(tab_alt)) deallocate(tab_alt)
       if ( associated(tab_dens)) deallocate(tab_dens)
       if ( associated(tab_temp)) deallocate(tab_temp)
       if ( associated(tab_press)) deallocate(tab_press)
       if ( associated(tab_umin)) deallocate(tab_umin)
       if ( associated(tab_umoy)) deallocate(tab_umoy)
       if ( associated(tab_umax)) deallocate(tab_umax)
       if ( associated(tab_azim)) deallocate(tab_azim)
       if ( associated(tab_indhsol)) deallocate(tab_indhsol)
       if ( associated(tab_indlat)) deallocate(tab_indlat)
       modele_init=.true.
    endif

    ! Init var de parcours
    ligne = 0
    ierr = 0

    
    ! Numero d'unite disponible
    call cps_file_unit(unit_read,ierr)
    if(ierr.lt.0)then
       call MSP_signaler_message (cle_mes="CPS_ERR_UNIT", &
            routine="cps_atm_lire_fictab", &
            partie_variable=trim(fichier))
       return
    endif
    
#ifdef __GFORTRAN__
    open(unit=unit_read,file=fichier,status='old',&
         form='formatted', convert='big_endian', iostat=ierr)
#else
    open(unit=unit_read,file=fichier,status='old',&
         form='formatted', iostat=ierr)
#endif
    if ( ierr /= 0 ) then
       call MSP_signaler_message (cle_mes="CPS_ERR_OPEN", &
            routine="cps_atm_lire_fictab", &
            partie_variable=trim(fichier))
       return
    endif       
    
    ! Lecture d'une ligne de commentaires
    read(unit=unit_read, fmt=FORMAT_LIGNE_TXT)
    
    ! Lecture du nombre de lignes de donnees
    read(unit=unit_read, fmt=FORMAT_LIGNE_TXT, iostat=ierr) buff
    ! S'il on n'est pas à la fin du fichier 
    ! et qu'il ne s'agit pas d'une ligne blanche
    if ( ierr == 0 .and. len_trim(buff) /= 0 ) then
       read(buff, fmt='(i4)', iostat=ierr2) nb_lignes
    endif
    ! Lecture d'une ligne de commentaires
    read(unit=unit_read, fmt=FORMAT_LIGNE_TXT)
 
    ! Allocation memoire
    allocate(tab_hsol(nb_lignes))
    allocate(tab_lat(nb_lignes))
    allocate(tab_alt(nb_lignes))
    allocate(tab_dens(nb_lignes))
    allocate(tab_temp(nb_lignes))
    allocate(tab_press(nb_lignes))
    allocate(tab_umin(nb_lignes))
    allocate(tab_umoy(nb_lignes))
    allocate(tab_umax(nb_lignes))
    allocate(tab_azim(nb_lignes))

    ! Lecture du fichier ligne par ligne
    ierr = 0
    do ligne = 1, nb_lignes
       ! Lecture de la ligne dans le fichier
       read(unit=unit_read, fmt=FORMAT_LIGNE_TXT, iostat=ierr) buff
       ! S'il on n'est pas à la fin du fichier 
       ! et qu'il ne s'agit pas d'une ligne blanche
       if ( ierr == 0 .and. len_trim(buff) /= 0 ) then
          ! On lit les données mais on stocke pas
 
          read(buff, fmt=*, iostat=ierr) tab_hsol(ligne),             &
               tab_lat(ligne),tab_alt(ligne),tab_dens(ligne),           &
               tab_temp(ligne),tab_press(ligne),tab_umin(ligne),        &
               tab_umoy(ligne),tab_umax(ligne),tab_azim(ligne) 
       endif
    enddo

    ! Fermeture du fichier
    close(unit_read)

    ! Nombre de valeurs differentes de hsol
    nb_hsol = 0
    max_hsol = tab_hsol(1)
    do i = 1, nb_lignes
       if(tab_hsol(i+1) .eq. tab_hsol(i)) then
       else
          nb_hsol = nb_hsol + 1
       endif
       ! recherche de la valeur maximale pour l'heure
       if (tab_hsol(i) .gt. max_hsol) max_hsol = tab_hsol(i)
    enddo

    allocate(tab_indhsol(nb_hsol,2))
    tab_indhsol(:,:)=0._pm_reel

    ! creation du tableau donnant pour chaque valeur d'heure solaire 
    ! l'indice inferieur correspondant
    ! pour chaque valeur de hsol : colonne 1 = valeur, colonne 2 = indice dans tableau hsol

    tab_indhsol(1,1) = tab_hsol(1)
    tab_indhsol(1,2) = 1.0d0
    ind = 1

    do i = 1, nb_lignes-1
       if(tab_hsol(i+1) .eq. tab_hsol(i)) then
       else
          ind = ind+1
          tab_indhsol(ind, 1) = tab_hsol(i+1)
          tab_indhsol(ind, 2) = i+1
       endif
    enddo

    ! Nombre de valeurs differentes de lat
    nb_lat = 0
    max_lat = tab_lat(1)
    do i = 1, nb_lignes
       if(tab_lat(i+1) .eq. tab_lat(i)) then
       else
          nb_lat = nb_lat + 1
       endif
       ! recherche de la valeur maximale pour la latitude
       if (tab_lat(i) .gt. max_lat) max_lat = tab_lat(i)
    enddo

    allocate(tab_indlat(nb_lat,2))
    tab_indlat(:,:)=0._pm_reel

    ! creation du tableau donnant pour chaque valeur d'heure solaire 
    ! l'indice inferieur correspondant
    ! pour chaque valeur de hsol : colonne 1 = valeur, colonne 2 = indice dans tableau hsol

    tab_indlat(1,1) = tab_lat(1)
    tab_indlat(1,2) = 1.0d0
    ind = 1

    do i = 1, nb_lignes-1
       if(tab_lat(i+1) .eq. tab_lat(i)) then
       else
          ind = ind+1
          tab_indlat(ind, 1) = tab_lat(i+1)
          tab_indlat(ind, 2) = i+1
       endif
    enddo

    ! Recherche de la valeur maximale pour l'altitude
    max_alt = tab_alt(1)
    do i = 1, nb_lignes
       if (tab_alt(i) .gt. max_alt) max_alt = tab_alt(i)
    enddo

  end subroutine cps_atm_ouvrir_fictab


  subroutine cps_atm_tv_ponderation_vent(sigma, vent_min, vent_moy, vent_max, vent)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_tv_ponderation_vent
!
!$Resume
!     Ponderation du vent
!
!$Description
!     Calcul du vent à partir de sigma, vent_min, vent_moy et vent_max
!
!$Version
!     1.0 25/02/2010
!
!$Acces
!  PUBLIC
!
!$Arguments
!>E     sigma   :<pm_reel>      
!>E     vent_min    :<pm_reel>      
!>E     vent_moy    :<pm_reel>      
!>E     vent_max    :<pm_reel>      
!>S     vent        :<pm_reel>      
!
!$<>
!***********************************************************************

   use cps_accesMadona
   use cps_util

    implicit none
 
    ! arguments
    real(kind=pm_reel), intent(in) :: sigma
    real(kind=pm_reel), intent(in) :: vent_min, vent_moy, vent_max
    real(kind=pm_reel), intent(out) :: vent

    ! Variables locales

    ! Initialisation

    ! cas sigma nul
    if (sigma .eq. 0.d0) vent = vent_moy

    ! cas sigma egal à +3
    if (sigma .eq. +3.d0) vent = vent_max

    ! cas sigma egal à -3
    if (sigma .eq. -3.d0) vent = vent_min

    ! cas sigma entre -3 et 0 : interpolation entre vent_min et vent_moy
    if ((sigma .gt. -3.d0) .and. (sigma .lt. 0.d0)) then
       vent = vent_min + ((sigma+3.d0)*(vent_moy- vent_min)/(3.d0))
    endif

    ! cas sigma entre 0 et +3 : interpolation entre vent_moy et vent_max
    if ((sigma .gt. 0.d0) .and. (sigma .lt. 3.d0)) then
       vent = vent_moy + (sigma*(vent_max- vent_moy)/(3.d0))
    endif

  end subroutine cps_atm_tv_ponderation_vent

  subroutine cps_atm_tv_recherche_bornes(valeur, colonne, index_depart, index_inferieur, index_superieur)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_tv_recherche_bornes
!
!$Resume
!     Recherche des bornes
!
!$Description
!     Calcul de l'index inferieur et de l'index superieur à partir de 
!     valeur, colonne, index de depart
!
!$Version
!     1.0 25/02/2010
!
!$Acces
!  PUBLIC
!
!$Arguments
!>E     valeur       :<pm_reel>      
!>E     colonne      :<integer> 1 : traitement pour l'heure locale
!                               2 : traitement pour la latitude
!                               3 : traitement pour l'altitude  
!>E     index_depart :<integer>      
!>S     index_inferieur   :<integer>      
!>S     index_superieur   :<integer>      
!
!$<>
!***********************************************************************

   use cps_accesMadona
   use cps_util

    implicit none
 
    ! arguments
    real(kind=pm_reel), intent(in) :: valeur
    integer, intent(in) :: colonne, index_depart
    integer, intent(out) :: index_inferieur, index_superieur


    ! Variables locales
    integer :: i, j
    logical       :: termine, trouve_lat                  ! flag d'arret de boucle

    ! Initialisation
    i = 1
    j = 1
    termine = .false.
    trouve_lat = .false.

    ! Traitement pour l'heure locale
    if (colonne .eq. 1) then
       do while ((i .lt. nb_hsol) .and. (.not.termine))
          if ((valeur .lt. tab_indhsol(i+1,1)) .and. (valeur .ge. tab_indhsol(i,1))) then
             index_inferieur = int(tab_indhsol(i,2))
             index_superieur = int(tab_indhsol(i+1,2))
             termine = .true.
          endif
          i = i+1
       enddo
      ! cas particulier : dernière heure possible
       if (.not.termine .and. (valeur .eq. tab_indhsol(nb_hsol,1))) then
          index_inferieur = int(tab_indhsol(nb_hsol-1,2))
          index_superieur = int(tab_indhsol(nb_hsol,2))
          termine = .true.
       endif

    else if (colonne .eq. 2) then
    ! Traitement pour la latitude
       i = index_depart
       do while ((i .lt. nb_lignes) .and. (.not.termine))
          if (((valeur .lt. tab_lat(i+1)) .and. (valeur .ge. tab_lat(i))) .or. &
             (((valeur .eq. max_lat) .and. (valeur .eq. tab_lat(i+1))))) then
             index_superieur = i+1
             termine = .true.
          endif
          i = i+1
       enddo
       ! recherche de l'index inferieur
       if (termine) then
          do while ((j .le. nb_lat) .and. (.not.trouve_lat))
             if(tab_indlat(j,2) .eq. index_superieur) then
                index_inferieur = int(tab_indlat(j-1,2))
                trouve_lat = .true.
             endif
             j = j+1
          enddo
       else
      ! cas particulier : dernière latitude possible
          if(valeur .eq. max_lat) then
             index_inferieur = int(tab_indlat(nb_lat-1,2))
             index_superieur = int(tab_indlat(nb_lat,2))
          endif
       endif

    else if (colonne .eq. 3) then
    ! Traitement pour l'altitude
       i = index_depart
       do while ((i .lt. nb_lignes) .and. (.not.termine))
          if (((valeur .lt. tab_alt(i+1)) .and. (valeur .ge. tab_alt(i))) .or. &
             (((valeur .eq. max_alt) .and. (valeur .eq. tab_alt(i+1))))) then
             index_inferieur = i
             index_superieur = i+1
             termine = .true.
          endif
          i = i+1
       enddo
      ! cas particulier : dernière altitude possible
      if (.not.termine .and. (valeur .eq. tab_alt(nb_lignes))) then
          index_inferieur = int(tab_alt(nb_lignes -1))
          index_superieur = int(tab_alt(nb_lignes))
          termine = .true.
       endif

    endif


  end subroutine cps_atm_tv_recherche_bornes

  subroutine cps_atm_tv_interpolation(X, X1, X2, Y1, Y2, Y)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_tv_interpolation
!
!$Resume
!     Interpolation 
!
!$Description
!     Calcul de Y
!
!$Version
!     1.0 25/02/2010
!
!$Acces
!  PUBLIC
!
!$Arguments
!>E     X      :<pm_reel>      
!>E     X1     :<pm_reel>      
!>E     X2     :<pm_reel>      
!>E     Y1     :<pm_reel, dimension(7)>      
!>E     Y2     :<pm_reel, dimension(7)>      
!>S     Y      :<pm_reel, dimension(7)>      
!
!$<>
!***********************************************************************

    use cps_accesMadona
    use cps_util
    
    implicit none
 
    ! arguments
    real(kind=pm_reel), intent(in) :: X, X1, X2
    real(kind=pm_reel), intent(in) , dimension(7) :: Y1, Y2
    real(kind=pm_reel), intent(out), dimension(7) :: Y
    
    ! Variables locales
    integer :: i

    do i =1,7
       Y(i) = Y1(i) + ((Y2(i) - Y1(i))/(X2 - X1))*(X - X1)
    enddo


  end subroutine cps_atm_tv_interpolation

  subroutine cps_atm_tv_calcul(heure, latitude, altitude, tab_index, vecteur_interpole)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_tv_calcul
!
!$Resume
!     Calcul des grandeurs physiques interpolees
!
!$Description
!     Calcul du vecteur interpole (densite, temperature, pression, vent min, vent moy, vent max)
!
!$Version
!     1.0 25/02/2010
!
!$Acces
!  PUBLIC
!
!$Arguments
!>E     heure    :<pm_reel>      
!>E     latitude :<pm_reel>      
!>E     altitude :<pm_reel>      
!>E     index    :<integer, dimension 8>      
!>S     vecteur_interpole :<pm_reel, dimension 7>      
!
!$<>
!***********************************************************************

   use cps_accesMadona
   use cps_util

    implicit none
 
    ! arguments
    real(kind=pm_reel), intent(in) :: heure, latitude, altitude
    integer, intent(in), dimension(8) :: tab_index
    real(kind=pm_reel), intent(out), dimension(7) :: vecteur_interpole

    ! Variables locales
    real(kind=pm_reel), dimension(7) :: vecteur_alt_inf_lat_inf_heure_inf, vecteur_alt_sup_lat_inf_heure_inf
    real(kind=pm_reel), dimension(7) :: vecteur_alt_inf_lat_sup_heure_inf, vecteur_alt_sup_lat_sup_heure_inf
    real(kind=pm_reel), dimension(7) :: vecteur_alt_inf_lat_inf_heure_sup, vecteur_alt_sup_lat_inf_heure_sup
    real(kind=pm_reel), dimension(7) :: vecteur_alt_inf_lat_sup_heure_sup, vecteur_alt_sup_lat_sup_heure_sup

    real(kind=pm_reel), dimension(7) :: vecteur_interpole_lat_inf_heure_inf, vecteur_interpole_lat_sup_heure_inf
    real(kind=pm_reel), dimension(7) :: vecteur_interpole_lat_inf_heure_sup, vecteur_interpole_lat_sup_heure_sup
    real(kind=pm_reel), dimension(7) :: vecteur_interpole_heure_inf, vecteur_interpole_heure_sup

    ! Initialisation
    vecteur_alt_inf_lat_inf_heure_inf(1) = tab_dens(tab_index(1))
    vecteur_alt_inf_lat_inf_heure_inf(2) = tab_temp(tab_index(1))
    vecteur_alt_inf_lat_inf_heure_inf(3) = tab_press(tab_index(1))
    vecteur_alt_inf_lat_inf_heure_inf(4) = tab_umin(tab_index(1))
    vecteur_alt_inf_lat_inf_heure_inf(5) = tab_umoy(tab_index(1))
    vecteur_alt_inf_lat_inf_heure_inf(6) = tab_umax(tab_index(1))
    vecteur_alt_inf_lat_inf_heure_inf(7) = tab_azim(tab_index(1))
    vecteur_alt_sup_lat_inf_heure_inf(1) = tab_dens(tab_index(2))
    vecteur_alt_sup_lat_inf_heure_inf(2) = tab_temp(tab_index(2))
    vecteur_alt_sup_lat_inf_heure_inf(3) = tab_press(tab_index(2))
    vecteur_alt_sup_lat_inf_heure_inf(4) = tab_umin(tab_index(2))
    vecteur_alt_sup_lat_inf_heure_inf(5) = tab_umoy(tab_index(2))
    vecteur_alt_sup_lat_inf_heure_inf(6) = tab_umax(tab_index(2))
    vecteur_alt_sup_lat_inf_heure_inf(7) = tab_azim(tab_index(2))
    vecteur_alt_inf_lat_sup_heure_inf(1) = tab_dens(tab_index(3))
    vecteur_alt_inf_lat_sup_heure_inf(2) = tab_temp(tab_index(3))
    vecteur_alt_inf_lat_sup_heure_inf(3) = tab_press(tab_index(3))
    vecteur_alt_inf_lat_sup_heure_inf(4) = tab_umin(tab_index(3))
    vecteur_alt_inf_lat_sup_heure_inf(5) = tab_umoy(tab_index(3))
    vecteur_alt_inf_lat_sup_heure_inf(6) = tab_umax(tab_index(3))
    vecteur_alt_inf_lat_sup_heure_inf(7) = tab_azim(tab_index(3))
    vecteur_alt_sup_lat_sup_heure_inf(1) = tab_dens(tab_index(4))
    vecteur_alt_sup_lat_sup_heure_inf(2) = tab_temp(tab_index(4))
    vecteur_alt_sup_lat_sup_heure_inf(3) = tab_press(tab_index(4))
    vecteur_alt_sup_lat_sup_heure_inf(4) = tab_umin(tab_index(4))
    vecteur_alt_sup_lat_sup_heure_inf(5) = tab_umoy(tab_index(4))
    vecteur_alt_sup_lat_sup_heure_inf(6) = tab_umax(tab_index(4))
    vecteur_alt_sup_lat_sup_heure_inf(7) = tab_azim(tab_index(4))
    vecteur_alt_inf_lat_inf_heure_sup(1) = tab_dens(tab_index(5))
    vecteur_alt_inf_lat_inf_heure_sup(2) = tab_temp(tab_index(5))
    vecteur_alt_inf_lat_inf_heure_sup(3) = tab_press(tab_index(5))
    vecteur_alt_inf_lat_inf_heure_sup(4) = tab_umin(tab_index(5))
    vecteur_alt_inf_lat_inf_heure_sup(5) = tab_umoy(tab_index(5))
    vecteur_alt_inf_lat_inf_heure_sup(6) = tab_umax(tab_index(5))
    vecteur_alt_inf_lat_inf_heure_sup(7) = tab_azim(tab_index(5))
    vecteur_alt_sup_lat_inf_heure_sup(1) = tab_dens(tab_index(6))
    vecteur_alt_sup_lat_inf_heure_sup(2) = tab_temp(tab_index(6))
    vecteur_alt_sup_lat_inf_heure_sup(3) = tab_press(tab_index(6))
    vecteur_alt_sup_lat_inf_heure_sup(4) = tab_umin(tab_index(6))
    vecteur_alt_sup_lat_inf_heure_sup(5) = tab_umoy(tab_index(6))
    vecteur_alt_sup_lat_inf_heure_sup(6) = tab_umax(tab_index(6))
    vecteur_alt_sup_lat_inf_heure_sup(7) = tab_azim(tab_index(6))
    vecteur_alt_inf_lat_sup_heure_sup(1) = tab_dens(tab_index(7))
    vecteur_alt_inf_lat_sup_heure_sup(2) = tab_temp(tab_index(7))
    vecteur_alt_inf_lat_sup_heure_sup(3) = tab_press(tab_index(7))
    vecteur_alt_inf_lat_sup_heure_sup(4) = tab_umin(tab_index(7))
    vecteur_alt_inf_lat_sup_heure_sup(5) = tab_umoy(tab_index(7))
    vecteur_alt_inf_lat_sup_heure_sup(6) = tab_umax(tab_index(7))
    vecteur_alt_inf_lat_sup_heure_sup(7) = tab_azim(tab_index(7))
    vecteur_alt_sup_lat_sup_heure_sup(1) = tab_dens(tab_index(8))
    vecteur_alt_sup_lat_sup_heure_sup(2) = tab_temp(tab_index(8))
    vecteur_alt_sup_lat_sup_heure_sup(3) = tab_press(tab_index(8))
    vecteur_alt_sup_lat_sup_heure_sup(4) = tab_umin(tab_index(8))
    vecteur_alt_sup_lat_sup_heure_sup(5) = tab_umoy(tab_index(8))
    vecteur_alt_sup_lat_sup_heure_sup(6) = tab_umax(tab_index(8))
    vecteur_alt_sup_lat_sup_heure_sup(7) = tab_azim(tab_index(8))

    ! Interpolation avec les altitudes (à latitude et heure donnees)
    call cps_atm_tv_interpolation(altitude, tab_alt(tab_index(1)), tab_alt(tab_index(2)), &
                                  vecteur_alt_inf_lat_inf_heure_inf, vecteur_alt_sup_lat_inf_heure_inf, &
                                  vecteur_interpole_lat_inf_heure_inf)
    call cps_atm_tv_interpolation(altitude, tab_alt(tab_index(3)), tab_alt(tab_index(4)), &
                                  vecteur_alt_inf_lat_sup_heure_inf, vecteur_alt_sup_lat_sup_heure_inf, &
                                  vecteur_interpole_lat_sup_heure_inf)
    call cps_atm_tv_interpolation(altitude, tab_alt(tab_index(5)), tab_alt(tab_index(6)), &
                                  vecteur_alt_inf_lat_inf_heure_sup, vecteur_alt_sup_lat_inf_heure_sup, &
                                  vecteur_interpole_lat_inf_heure_sup)
    call cps_atm_tv_interpolation(altitude, tab_alt(tab_index(7)), tab_alt(tab_index(8)), &
                                  vecteur_alt_inf_lat_sup_heure_sup, vecteur_alt_sup_lat_sup_heure_sup, &
                                  vecteur_interpole_lat_sup_heure_sup)

    ! Interpolation avec les latitudes (à heure donnee)
    call cps_atm_tv_interpolation(latitude, tab_lat(tab_index(1)), tab_lat(tab_index(3)), &
         vecteur_interpole_lat_inf_heure_inf, vecteur_interpole_lat_sup_heure_inf, vecteur_interpole_heure_inf)

    call cps_atm_tv_interpolation(latitude, tab_lat(tab_index(5)), tab_lat(tab_index(7)), &
         vecteur_interpole_lat_inf_heure_sup, vecteur_interpole_lat_sup_heure_sup, vecteur_interpole_heure_sup)

    ! Interpolation avec les heures
    call cps_atm_tv_interpolation(heure, tab_hsol(tab_index(1)), tab_hsol(tab_index(5)), &
         vecteur_interpole_heure_inf, vecteur_interpole_heure_sup, vecteur_interpole)


  end subroutine cps_atm_tv_calcul

  subroutine cps_atm_tv_calculer(sigma,heure,latitude,altitude,densite,temperature,pression,norme_vent,azimut_vent)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_tv_calculer
!
!$Resume
!     Calcul des grandeurs physiques meteorologiques
!
!$Description
!     Calcul de densite, temperature, pression, vent (norme et azimut) a partir de 
!     sigma, heure, latitude, altitude
!
!$Version
!     1.0 25/02/2010
!
!$Acces
!  PUBLIC
!
!$Arguments
!>E     sigma      :<pm_reel>      
!>E     heure      :<pm_reel>      
!>E     latitude   :<pm_reel>      
!>E     altitude   :<pm_reel>      
!>S     densite    :<pm_reel>      
!>S     temperature:<pm_reel>      
!>S     pression   :<pm_reel>      
!>S     norme_vent :<pm_reel>  
!>S     azimut_vent:<pm_reel>      
!
!$<>
!***********************************************************************

    use cps_accesMadona
    use cps_util
    
    implicit none
 
    ! arguments
    real(kind=pm_reel), intent(in) :: sigma,heure,latitude,altitude
    real(kind=pm_reel), intent(out) :: densite,temperature,pression
    real(kind=pm_reel), intent(out) :: norme_vent, azimut_vent
    
    ! Variables locales
    integer :: index_heure_inf, index_heure_sup
    integer :: index_lat_inf_heure_sup, index_lat_sup_heure_sup
    integer :: index_lat_inf_heure_inf, index_lat_sup_heure_inf
    integer :: index_alt_inf_lat_inf_heure_sup, index_alt_sup_lat_inf_heure_sup
    integer :: index_alt_inf_lat_sup_heure_sup, index_alt_sup_lat_sup_heure_sup
    integer :: index_alt_inf_lat_inf_heure_inf, index_alt_sup_lat_inf_heure_inf
    integer :: index_alt_inf_lat_sup_heure_inf, index_alt_sup_lat_sup_heure_inf
    integer, dimension(8) :: index
    real(kind=pm_reel), dimension(7) :: vecteur_interpole  ! dens, temp, press, vent_moy, vent_max, vent_min, azim_vent

    real(kind=pm_reel) :: latitude_elargie, heure_base12, altitude_elargie
    ! les latitudes et altitudes elargies correspondent au traitement des latitudes
    ! et altitudes qui se trouvent en dehors des valeurs du tableau
    ! heure en base 12 correspond aux heures du tableau en prennant la symetrie par
    ! rapport à 12 heures (la plus ensoleillée) pour la base des heures passée en 
    ! paramètre qui s'étend elle sur 24 heures

! Initialisation

! Test sur les valeurs d'entree : sigma, heure, latitude, altitude
 

!---------------------------------------------
! Traitement du facteur de ponderation du vent
!---------------------------------------------
   if ((sigma < -3.d0) .or. (sigma > +3.d0)) then
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_VENUS_SIGMA",&
            routine="cps_atm_tv_calculer")
       return
   endif

!----------------------------------------
! Traitement de la symetrie sur le 'heure
!----------------------------------------
    ! symetrie des heures (ie : 11 heures = 13 heures)
    if (heure > 12._pm_reel) then
       heure_base12 = 24._pm_reel - heure
    else
       heure_base12 = heure
    endif   
    if ((heure_base12 < 0.d0) .or. (heure_base12 > max_hsol)) then
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_VENUS_HSOL",&
            routine="cps_atm_tv_calculer")
       return
    endif

!------------------------------------
! Traitement des limites de latitudes
!------------------------------------
! si latitude est hors des bornes acceptables [0-90] deg -> return
   if ((latitude < 0.d0) .or. (latitude > 90.d0)) then
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_VENUS_LAT",&
            routine="cps_atm_tv_calculer")
       return
   endif
! si latitude est acceptable mais en dehors du tableau 
   !si latitude est inferieure à 30 entre [0,30]deg alors -> latitude = 30
   if ((latitude >= 0.d0) .and. (latitude < 30.d0)) then
       latitude_elargie = 30.d0
   !si latitude est superieure à 60 entre [60,90]deg alors -> latitude= 60
   elseif ((latitude > 60.d0) .and. (latitude <= 90.d0)) then 
           latitude_elargie = 60.d0
   !sinon
   else
        latitude_elargie = latitude   
   endif

!-------------------------------------
! Traitement des limites des altitudes
!-------------------------------------
! si l'altitude est inferieure à zero, l'altitude est conservée
! en effet l'intégrateur extrapole sous le niveau du sol
   if (altitude < 0.d0) then
       call MSP_signaler_message(cle_mes="CPS_WRN_ATM_VENUS_ALT_MIN",&
            routine="cps_atm_tv_calculer")
       altitude_elargie=0.d0
! si l'altitude est supérieure aux bornes, l'altitude est conservée
! en effet le vehicule se trouve sur l'arc orbital et pas encore sur l'arc atm
   elseif (altitude > max_alt) then
       call MSP_signaler_message(cle_mes="CPS_WRN_ATM_VENUS_ALT_MAX",&
            routine="cps_atm_tv_calculer")
       altitude_elargie=max_alt
! sinon inchangée
   else
       altitude_elargie=altitude
   endif


    ! Decouverte des 8 index de points de mesure
    ! Heure locale
    call cps_atm_tv_recherche_bornes(heure_base12, 1, 1, index_heure_inf, index_heure_sup)

    ! Latitude 
    call cps_atm_tv_recherche_bornes(latitude_elargie, 2, index_heure_inf, index_lat_inf_heure_inf, index_lat_sup_heure_inf)
    call cps_atm_tv_recherche_bornes(latitude_elargie, 2, index_heure_sup, index_lat_inf_heure_sup, index_lat_sup_heure_sup)

    ! Altitude
    call cps_atm_tv_recherche_bornes(altitude_elargie, 3, index_lat_inf_heure_inf, index_alt_inf_lat_inf_heure_inf, &
         index_alt_sup_lat_inf_heure_inf)
    call cps_atm_tv_recherche_bornes(altitude_elargie, 3, index_lat_sup_heure_inf, index_alt_inf_lat_sup_heure_inf, &
         index_alt_sup_lat_sup_heure_inf)
    call cps_atm_tv_recherche_bornes(altitude_elargie, 3, index_lat_inf_heure_sup, index_alt_inf_lat_inf_heure_sup, &
         index_alt_sup_lat_inf_heure_sup)
    call cps_atm_tv_recherche_bornes(altitude_elargie, 3, index_lat_sup_heure_sup, index_alt_inf_lat_sup_heure_sup, &
         index_alt_sup_lat_sup_heure_sup)

    ! Initialisation du tableau des index
    index(1) = index_alt_inf_lat_inf_heure_inf
    index(2) = index_alt_sup_lat_inf_heure_inf
    index(3) = index_alt_inf_lat_sup_heure_inf
    index(4) = index_alt_sup_lat_sup_heure_inf
    index(5) = index_alt_inf_lat_inf_heure_sup
    index(6) = index_alt_sup_lat_inf_heure_sup
    index(7) = index_alt_inf_lat_sup_heure_sup
    index(8) = index_alt_sup_lat_sup_heure_sup

    ! Calcul des grandeurs physiques interpolees
    call cps_atm_tv_calcul(heure_base12, latitude_elargie, altitude_elargie, index, vecteur_interpole)
    densite = vecteur_interpole(1)
    temperature = vecteur_interpole(2)
    pression = vecteur_interpole(3)
    azimut_vent = vecteur_interpole(7)

    ! Ponderation du vent
    call cps_atm_tv_ponderation_vent(sigma, vecteur_interpole(4), vecteur_interpole(5), vecteur_interpole(6), norme_vent)
     

  end subroutine cps_atm_tv_calculer


  subroutine cps_atm_fermer_fictab ()
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_fermer_fictab
!
!$Resume
!     routine de desallocation memoire
!
!$Description
!
!$Version
!     1.0 25/02/2010
!
!$Acces
!  PUBLIC
!
!$Arguments
!
!$<>
!***********************************************************************

! Variable pour la gestion d'erreur de deallocate
    integer :: ierralloc

    if (modele_init) then
! desallocation des variables globales :
!
       if ( associated(tab_hsol)) deallocate(tab_hsol, stat=ierralloc)
       if ( associated(tab_lat)) deallocate(tab_lat, stat=ierralloc)
       if ( associated(tab_alt)) deallocate(tab_alt, stat=ierralloc)
       if ( associated(tab_dens)) deallocate(tab_dens, stat=ierralloc)
       if ( associated(tab_temp)) deallocate(tab_temp, stat=ierralloc)
       if ( associated(tab_press)) deallocate(tab_press, stat=ierralloc)
       if ( associated(tab_umin)) deallocate(tab_umin, stat=ierralloc)
       if ( associated(tab_umoy)) deallocate(tab_umoy, stat=ierralloc)
       if ( associated(tab_umax)) deallocate(tab_umax, stat=ierralloc)
       if ( associated(tab_azim)) deallocate(tab_azim, stat=ierralloc)
       if ( associated(tab_indhsol)) deallocate(tab_indhsol, stat=ierralloc)
       if ( associated(tab_indlat)) deallocate(tab_indlat, stat=ierralloc)
       modele_init = .false.
    endif

  end subroutine cps_atm_fermer_fictab

end module CPS_ATMOSPHERE_VENUS
