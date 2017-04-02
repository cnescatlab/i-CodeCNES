module cps_acsol2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_acsol2
!
!$Resume
! Module contenant une fonction utilitaire relative à la manipulation
! des données issues du fichier ACSOL2.
!
!$Description
! Module contenant une fonction utilitaire relative à la manipulation
! des données issues du fichier ACSOL2.
!
!$Auteur
! Vincent Philavong (ATOS ORIGIN)
!
!$Version
!  $Id: cps_acsol2.F90 379 2013-02-22 16:59:49Z ffsm $
!
!$Historique
!  $Log: cps_acsol2.F90,v $
!  Revision 379  2013/02/22 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.7  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.6  2010/02/16 13:04:19  vivaresf
!  VERSION::FA-ID:1334: format standard pour le code d'erreur
!
!  Revision 1.5  2009/11/02 13:07:57  cmartel
!  DM-ID 842 : Correction d'un appel a cps_lireAcsol2
!
!  Revision 1.4  2009/11/02 09:50:55  cmartel
!  DM-ID 842 : Ajout de routines de lecture des fichiers ACSOL2 bruts
!
!  Revision 1.3  2006/05/12 12:05:39  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.2  2006/03/20 15:52:59  vpg
!  Mise a jour des cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_acsol2
!
!$Structure
!
!: ligne_acsol2 : 
!>     julsol   : <integer>          
!>     indflu   : <integer>          
!>     indgeo   : <integer>          
!>     iaamoy   : <integer>          
!>     iapmoy   : <integer>          
!>     iaampr   : <integer>          
!>     iaa      : <integer,DIM=(8)>  
!>     iap      : <integer,DIM=(8)>  
!>     iaapr    : <integer,DIM=(8)>  
!>     flux     : <PM_REEL>          
!>     fluxa    : <PM_REEL>          
!>     fluxm    : <PM_REEL>          
!>     fluxpr   : <PM_REEL>          
!>     fluxmp   : <PM_REEL>          
!>     tmp      : <integer,DIM=(6)>  
!
!$Global
!
!$Common
!
!$Routines
!- cpsi_analyserCleAcsol2
!- cpsi_lireAcsol2_Brut
!- cps_lireAcsol2
!- cpsi_lire_unficAcsol2_MADONA
!- cpsi_cpter_lignes_Acsol2_MADONA
!
!$Fonctions
!- cpsi_getCleAcsol2
!
!$Include
!
!$Module
!#V
!- MSLIB
!- cps_accesMadona
!- cps_util
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
!.  cpsi_getCleAcsol2 cpsi_analyserCleAcsol2 cpsi_lireAcsol2_Brut cps_lireAcsol2 cpsi_lire_unficAcsol2_MADONA
!.  cpsi_cpter_lignes_Acsol2_MADONA
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_acsol2.F90 379 2013-02-22 16:59:49Z ffsm $'

  
  type ligne_acsol2
     integer :: julsol, indflu, indgeo, iaamoy, iapmoy, iaampr
     integer, dimension(8) :: iaa, iap, iaapr   
     real(KIND=PM_REEL) :: flux, fluxa, fluxm, fluxpr, fluxmp
     integer, dimension(6) :: tmp
  end type ligne_acsol2

contains
  
  function cpsi_getCleAcsol2(date) result(cle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCleAcsol2
!
!$Resume
! Fonction qui permet d'obtenir le nom d'une structure MADONA
! correspondant a une donnee issue du fichier ACSOL2 dans COMPAS.
! 'date' est en jour julien 1950.
!
!$Description
! V2.0
! Fonction qui permet d'obtenir le nom d'une structure MADONA
! correspondant a une donnee issue du fichier ACSOL2 dans COMPAS.
! 'date' est en jour julien 1950.
!
!$Auteur
! vpg
!
!$Acces
!  PRIVE
!
!$Usage
!  cle = cpsi_getCleAcsol2(date)
!.    real(KIND=PM_REEL) :: date
!.    character(LEN=256) :: cle
!
!$Arguments
!>E     date  :<PM_REEL>   date (jj 1950)
!>S     cle   :<LEN=256>   Nom de la structure MADONA
!
!$Routines
!- md_jourfrac_joursec
!- md_julien_calend
!
!$Include
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    real(KIND=PM_REEL), intent(in) :: date
    
    ! resultat
    character(LEN=256) :: cle
    
    ! variables locales
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_rEEL) :: sec
    character(LEN=4) :: buff_jj, buff_mm, buff_aaaa
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour

    call md_jourfrac_joursec(date, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
    
    write(buff_jj,10) jj
    if (buff_jj(1:1).eq.' ') then
       buff_jj(1:1) = '0'
    end if
    write(buff_mm,10) mm
    if (buff_mm(1:1).eq.' ') then
       buff_mm(1:1) = '0'
    end if
    write(buff_aaaa,20) aaaa

    cle = "acsol2_"//trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)

10  format (i2)
20  format (i4)

  end function cpsi_getCleAcsol2

  subroutine  cpsi_analyserCleAcsol2(cle, valide, date)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_analyserCleAcsol2
!
!$Resume
!  Analyse du nom d'une structure au format ACSOL2 pour
!  en extraire la date des données
!
!$Description
!  Analyse du nom d'une structure au format ACSOL2 pour
!  en extraire la date des données
!
!$Auteur
! vpg
!
!$Acces
!  PRIVE
!
!$Usage
!  call cpsi_analyserCleAcsol2(cle, valide, date)
!.    character(LEN=*) :: cle
!.    logical :: valide
!.    real(KIND=PM_REEL) :: date
!
!$Arguments
!>E     cle     :<LEN=*>     nom de la structure MADONA
!>S     valide  :<logical>   
!>S     date    :<PM_REEL>   date (jj 1950)
!
!$Routines
!- md_calend_julien
!- md_joursec_jourfrac
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    ! arguments
    character(LEN=*), intent(in)  :: cle
    logical, intent(out) :: valide
    real(KIND=PM_REEL), intent(out) :: date

    ! Constante
    character(LEN=19), parameter :: FORMAT_CLE = '(7x,i2,1x,i2,1x,i4)'

    ! variables locales
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_rEEL) :: sec
    integer :: retour
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour

    ! Init
    valide = .false.
    date = 0.0_pm_reel
    h = 0
    min = 0
    sec = 0.0_pm_reel

    ! Analyse
    read(cle, FMT=FORMAT_CLE, IOSTAT=retour) jj, mm, aaaa
    if ( retour == 0 ) then
       ! Transformation de la date
       ! en cas d'échec valid reste à .false.
       call md_calend_julien(aaaa, mm, jj, h, min, sec, jour_sec, code_retour)
       if ( code_retour%valeur /= 0 ) return
       call md_joursec_jourfrac(jour_sec, date, code_retour)
       if ( code_retour%valeur /= 0 ) return

       ! Reussite de l'analyse
       valide = .true.
    endif

  end subroutine cpsi_analyserCleAcsol2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!        Fonction de lecture d'un fichier ACSOL2 (fichier brut)      !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine cps_lireAcsol2(fichier, tab_lignes, nb_lignes) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_lireAcsol2
!
!$Resume
!  Lecture d'un fichier ACSOL2 (fichier brut avant  intégration des données en base).
!
!$Description
!  Lecture d'un fichier ACSOL2 (fichier brut avant  intégration des données en base).
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_lireAcsol2(fichier, tab_lignes, nb_lignes) 
!.    character(LEN=*) :: fichier
!.    integer :: nb_lignes
!.    type(ligne_acsol2), dimension(:), pointer :: tab_lignes
!
!$Arguments
!>E     fichier     :<LEN=*>                          fichier ACSOL2 brut à lire
!>E/S   tab_lignes  :<ligne_acsol2,DIM=(:),pointer>   lignes du fichier ACSOL2 brut lu
!>S     nb_lignes   :<integer>                        nombre de lignes lues
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_accesMadona
   use cps_util

    implicit none 

    ! arguments
    character(LEN=*), intent(in) :: fichier
    integer, intent(out) :: nb_lignes
    type(ligne_acsol2), dimension(:), pointer :: tab_lignes
    
    
    ! Variables locales
    integer :: ierr, ierr2
    integer :: i
    integer :: unit_read, ligne
    type(ligne_acsol2) :: ligne_tmp
    character(len=38), parameter :: FORMAT_LIGNE &
         = '(i5,2x,i1,5(1x,F6.2),2x,i1,33(1x,i3))'
    ! Format d'une ligne de commentaire
    character(len=38), parameter :: FORMAT_LIGNE_TXT = '(A)'
    ! Gestion d'une erreur à la lecture
    logical :: une_ligne_nok
    integer :: indice_ligne_nok
    character(len=256) :: buff 

    ! Initialisation
    nb_lignes = 0
    if ( associated(tab_lignes)) deallocate(tab_lignes)

    ! Init var de parcours
    ligne = 0
    ierr = 0
    une_ligne_nok = .false.
    indice_ligne_nok = 0

    
    ! Numero d'unite disponible
    call cps_file_unit(unit_read,ierr)
    if(ierr.lt.0)then
       call MSP_signaler_message (cle_mes="CPS_ERR_UNIT", &
            routine="cps_lireAcsol2", &
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
            routine="cps_lireAcsol2", &
            partie_variable=trim(fichier))
       return
    endif       
    
    ! Lecture des lignes de commentaires
    do i = 1,7
       read(unit=unit_read, fmt=FORMAT_LIGNE_TXT)
    end do
    
    ! Détermination du nombre de lignes
    ierr = 0
    do while (ierr == 0 ) 
       ! Lecture de la ligne dans le fichier
       read(unit=unit_read, fmt=FORMAT_LIGNE_TXT, iostat=ierr) buff
       ! S'il on n'est pas à la fin du fichier 
       ! et qu'il ne s'agit pas d'une ligne blanche
       if ( ierr == 0 .and. len_trim(buff) /= 0 ) then
          ! On lit les données mais on stocke pas
          read(buff, fmt=FORMAT_LIGNE, iostat=ierr2) ligne_tmp%julsol, &
              ligne_tmp%indflu,ligne_tmp%flux,ligne_tmp%fluxa,           &
              ligne_tmp%fluxm,ligne_tmp%fluxpr,ligne_tmp%fluxmp,         &
              ligne_tmp%indgeo,ligne_tmp%iaamoy, ligne_tmp%iaa(1:8),     &
              ligne_tmp%iapmoy, ligne_tmp%iap(1:8),                      &
              ligne_tmp%iaampr,ligne_tmp%iaapr(1:8),ligne_tmp%tmp(1:6)
          ! Tant que l'on n'est pas a la fin et que la ligne est valable
          if (ierr2 == 0 .and. ligne_tmp%indflu /= 0 ) then
             nb_lignes = nb_lignes+1
          end if
       endif
    end do
    ! Fermeture du fichier
    close (unit_read)

    ! Allocation memoire
    allocate(tab_lignes(nb_lignes))
    
    ! Ouverture du fichier
#ifdef __GFORTRAN__
    open(unit=unit_read,file=fichier,status='old',form='formatted', convert='big_endian')
#else
    open(unit=unit_read,file=fichier,status='old',form='formatted')
#endif

    do i = 1,7
       read(unit=unit_read, fmt=FORMAT_LIGNE) 
    end do

    ! Lecture du fichier ligne par ligne
    ligne = 1
    ierr = 0
    do while (ierr == 0 .and. ligne <= nb_lignes )
       ! Lecture de la ligne dans le fichier
       read(unit=unit_read, fmt=FORMAT_LIGNE_TXT, iostat=ierr) buff
       ! S'il on n'est pas à la fin du fichier 
       ! et qu'il ne s'agit pas d'une ligne blanche
       if ( ierr == 0 .and. len_trim(buff) /= 0 ) then
          read(buff, fmt=FORMAT_LIGNE, iostat=ierr2) tab_lignes(ligne)%julsol,                     &
            tab_lignes(ligne)%indflu,tab_lignes(ligne)%flux,tab_lignes(ligne)%fluxa,           &
            tab_lignes(ligne)%fluxm,tab_lignes(ligne)%fluxpr,tab_lignes(ligne)%fluxmp,         &
            tab_lignes(ligne)%indgeo,tab_lignes(ligne)%iaamoy, tab_lignes(ligne)%iaa(1:8),     &
            tab_lignes(ligne)%iapmoy, tab_lignes(ligne)%iap(1:8),                              &
            tab_lignes(ligne)%iaampr,tab_lignes(ligne)%iaapr(1:8),tab_lignes(ligne)%tmp(1:6)
          ! Ligne suivante, on conserve la courante
          if ( ierr2 /= 0) then
             if ( .not. une_ligne_nok ) then
                ! Enregistrement de la première ligne NOK
                une_ligne_nok = .true. 
                indice_ligne_nok = ligne + 7
             endif
          else
             if (tab_lignes(ligne)%indflu /= 0 )  then
                ! La ligne est valable, on peut passser à la suivante
                ligne = ligne+1
             endif
          endif

       else
                indice_ligne_nok = ligne + 7             
       endif
    enddo

    ! Affichage d'un warning si une des lignes était incorrecte
    if (une_ligne_nok ) then
       write(buff, fmt='(i6)') indice_ligne_nok
       call MSP_signaler_message (cle_mes="CPS_WRN_LIRE_ACSOL2", &
            routine="cps_lireAcsol2", &
            partie_variable=trim(buff))
    endif
    

    ! Fermeture du fichier
    close(unit_read)
    
  end subroutine cps_lireAcsol2



  subroutine cps_lireEtConvertirAcsol2(fichier, tab_date, tab_data_flux, tab_data_ia, nb_data) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_lireEtConvertirAcsol2
!
!$Resume
!  Lecture d'un fichier ACSOL2 et transformation en structure
!  compatibles avec les structures de l'activité réelle
!
!$Description
!  Lecture d'un fichier ACSOL2 et transformation en structure
!  compatibles avec les structures de l'activité réelle
!
!$Auteur
!  Cédric Martel (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_lireEtConvertirAcsol2(fichier, tab_date, tab_data_flux, tab_data_ia, nb_data) 
!.    character(LEN=*) :: fichier
!.    real(KIND=PM_REEL), dimension(:), pointer :: tab_date
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
!.    integer, dimension(:,:), pointer :: tab_data_ia
!.    integer :: nb_data
!
!$Arguments
!>E     fichier        :<LEN=*>                       Nom du fichier
!>E/S   tab_date       :<PM_REEL,DIM=(:),pointer>     Tableau des dates en jj1950 fractionnaires
!>E/S   tab_data_flux  :<PM_REEL,DIM=(:,:),pointer>   Tableau des flux
!.                                                  data_flux(1) : flux observe
!.                                                  data_flux(2) : flux moyen observe
!>E/S   tab_data_ia    :<integer,DIM=(:,:),pointer>   Tableau des indices trihoraires
!.                                                  data_ia(1)   : indice AA moyen journalier
!.                                                  data_ia(2:9) : indice AA trihoraire
!.                                                  data_ia(10)  : indice AP moyen journalier
!>S     nb_data        :<integer>                     Nombre de lignes des tableaux
!
!$Common
!
!$Routines
!- cpsi_lireAcsol2_Brut
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- cps_accesMadona
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

   use cps_accesMadona

    implicit none 

    ! arguments
    character(LEN=*), intent(in) :: fichier
    real(KIND=PM_REEL), dimension(:), pointer :: tab_date
    real(KIND=PM_REEL), dimension(:,:), pointer  :: tab_data_flux
    integer, dimension(:,:), pointer :: tab_data_ia
    integer, intent(out) :: nb_data

    type(ligne_acsol2), dimension(:), pointer :: tab_lignes => NULL()
    
    
    ! Variables locales
    integer :: ligne, nb_lignes

    ! Initialisation
    nb_data = 0
    if ( associated(tab_date) ) deallocate(tab_date)
    if ( associated(tab_data_flux) ) deallocate(tab_data_flux)
    if ( associated(tab_data_ia) ) deallocate(tab_data_ia)
    
    ! Appel à la lecture du fichier brut
    call cps_lireAcsol2(fichier, tab_lignes, nb_lignes) 
    if ( MSP_gen_messages("cps_lireAcsol2") ) return 

    ! Allocations
    allocate(tab_date(nb_lignes)) 
    allocate(tab_data_flux(nb_lignes,2)) 
    allocate(tab_data_ia(nb_lignes,10)) 

    ! Lecture du fichier ligne par ligne
    ! if ( nb_lignes > CPS_MAX_ACSOL2_SURE ) nb_lignes=CPS_MAX_ACSOL2_SURE
    do ligne= 1, nb_lignes
          
       ! Recopie de la date
       tab_date(ligne) = tab_lignes(ligne)%julsol

       ! Extraction des données intéressantes
       if ( tab_lignes(ligne)%indflu == 3 ) then
          ! Données réelles
          tab_data_flux(ligne,1) = tab_lignes(ligne)%flux
          tab_data_flux(ligne,2) = tab_lignes(ligne)%fluxm

          tab_data_ia(ligne,1) = tab_lignes(ligne)%iaamoy
          tab_data_ia(ligne,2) = tab_lignes(ligne)%iaa(1)
          tab_data_ia(ligne,3) = tab_lignes(ligne)%iaa(2)
          tab_data_ia(ligne,4) = tab_lignes(ligne)%iaa(3)
          tab_data_ia(ligne,5) = tab_lignes(ligne)%iaa(4)
          tab_data_ia(ligne,6) = tab_lignes(ligne)%iaa(5)
          tab_data_ia(ligne,7) = tab_lignes(ligne)%iaa(6)
          tab_data_ia(ligne,8) = tab_lignes(ligne)%iaa(7)
          tab_data_ia(ligne,9) = tab_lignes(ligne)%iaa(8)
          tab_data_ia(ligne,10) = tab_lignes(ligne)%iapmoy

       else if ( tab_lignes(ligne)%indflu == 2 ) then
          ! Données prédites
          tab_data_flux(ligne,1) = tab_lignes(ligne)%fluxpr
          tab_data_flux(ligne,2) = tab_lignes(ligne)%fluxmp

          tab_data_ia(ligne,1) = tab_lignes(ligne)%iaampr
          tab_data_ia(ligne,2) = tab_lignes(ligne)%iaapr(1)
          tab_data_ia(ligne,3) = tab_lignes(ligne)%iaapr(2)
          tab_data_ia(ligne,4) = tab_lignes(ligne)%iaapr(3)
          tab_data_ia(ligne,5) = tab_lignes(ligne)%iaapr(4)
          tab_data_ia(ligne,6) = tab_lignes(ligne)%iaapr(5)
          tab_data_ia(ligne,7) = tab_lignes(ligne)%iaapr(6)
          tab_data_ia(ligne,8) = tab_lignes(ligne)%iaapr(7)
          tab_data_ia(ligne,9) = tab_lignes(ligne)%iaapr(8)
          tab_data_ia(ligne,10) = 0
          
       else
          ! Le code est soit 2 soit 3, la lecture des données 
          ! brutes a du permettre d faire le tri
          call MSP_signaler_message (cle_mes="CPS_ERR_LIRE_ACSOL2", &
             routine="cps_lireAcsol2_Brut")
          return   

       endif 
       
    enddo
    
    ! Recopie du nombre de ligne si succès
    nb_data = nb_lignes
    
  end subroutine cps_lireEtConvertirAcsol2


  subroutine cpsi_lire_unficAcsol2_MADONA(fichier, categ, indice_depart, nb_data, &
       tab_date, tab_data_flux, tab_data_ia, date_deb, date_fin) 
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lire_unficAcsol2_MADONA
!
!$Resume
!  lecture des donnees d'activité solaire d'un fichier MADONA d'archive
!  des données ACSOL2.
!
!$Description
!  lecture des donnees d'activité solaire d'un fichier MADONA d'archive
!  des données ACSOL2.
!
!$Auteur
!   Cédric Martel (Atos Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call cpsi_lire_unficAcsol2_MADONA(fichier, categ, indice_depart, nb_data, &
!.           tab_date, tab_data_flux, tab_data_ia, date_deb, date_fin) 
!.    character(LEN=*) :: fichier
!.    character(LEN=*) :: categ
!.    integer :: indice_depart
!.    integer :: nb_data
!.    real(KIND=PM_REEL), dimension(:), pointer :: tab_date
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
!.    integer, dimension(:,:), pointer :: tab_data_ia
!.    real(KIND=PM_REEL) :: date_deb
!.    real(KIND=PM_REEL) :: date_fin
!
!$Arguments
!>E     fichier        :<LEN=*>                       nom du fichier de données
!>E     categ          :<LEN=*>                       
!>E/S   indice_depart  :<integer>                     
!>E     nb_data        :<integer>                     nombre déléments des tableaux
!>E/S   tab_date       :<PM_REEL,DIM=(:),pointer>     tableau des dates
!>E/S   tab_data_flux  :<PM_REEL,DIM=(:,:),pointer>   tableau des données de flux
!>E/S   tab_data_ia    :<integer,DIM=(:,:),pointer>   tableau des données liées aux indices
!>E     date_deb       :<PM_REEL>                     
!>E     date_fin       :<PM_REEL>                     
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_analyserCleAcsol2
!
!$Include
!
!$Module
!#V
!- cps_accesMadona
!#
!
!$Remarques
!  Cette routine est utilisée pour extraire l'ensembles des achives disponibles.
!  Les dates de début et de fin doivent être renseignées. Si une de ces contraintes
!  n'est pas nécessaire, indiquer -1 pour la date de début et/ou 1E8 pour la date
!  de fin.
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_accesMadona

   implicit none

   ! arguments
   character(LEN=*), intent(in) :: fichier
   character(LEN=*), intent(in) :: categ
   integer, intent(inout) :: indice_depart
   integer, intent(in) :: nb_data
   real(KIND=PM_REEL), dimension(:), pointer :: tab_date
   real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
   integer, dimension(:,:), pointer :: tab_data_ia
   real(KIND=PM_REEL), intent(in) :: date_deb
   real(KIND=PM_REEL), intent(in) :: date_fin

   ! Varaibles locales
   integer :: acces     ! Accès MADONA au fichier
   integer :: ret, ii
   character(LEN=CPS_MAXLG) :: libelle
   integer :: nature
   logical :: analyse_ok
   logical :: categ_acsol_sure
   real(KIND=PM_REEL) :: date

   ! Traitement particulier selon prédit ou sure, les noms de schamps changent
   categ_acsol_sure = .true.
   if ( categ == CPS_CATEG_ACSOL2_PRED) then
      categ_acsol_sure = .false.
   endif

   ! Ouverture du fichier
   ! On ne pas utiliser cps_getAccesMadonna 
   ! car le fichier appelant utilise déjà une unité madona 
   ! qui du coup risque d'être fermée
   acces  = acc_load(fichier)
   if (acces < 0) then
        call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
             routine="cpsi_lire_unficAcsol2_MADONA", &
             partie_variable=trim(fichier))
        return
   end if

   ! Init des variables de parcours
   ii = 0
   ret = ACC_OK

   ! Parcours de l'ensemble du fichier des données
   ret = acc_scan_reset(acces)
   ii = indice_depart
   boucle_struct : do while (ret /= ACC_EOF .and. &
        ii <= nb_data)

      ! Lecture du contenu 
      ret = acc_scan(acces, libelle, nature)
      ! On veut des strcutures
      if (nature /= ACC_STRUCT )  cycle boucle_struct

      ! Anlayse de la cle
      call cpsi_analyserCleAcsol2(libelle, analyse_ok, date)
         
      ! Si la cle a bien été analysée
      ! et si la date est dans le créneau (optionnel valide)
      ! alors recopie
      if ( .not. analyse_ok .or. &
         date < date_deb .or. &
         date > date_fin ) cycle boucle_struct

      ! la structure existe : selection de la structure
      ret = acc_select(acces, libelle, ACC_STRUCT)

      ! Recopie de la date 
      tab_date(ii) = date

      if ( categ_acsol_sure ) then
         ! Lecture de tous les champs
         ret = acc_getd(acces, "flux", tab_data_flux(ii,1), "sfu")
         ret = acc_getd(acces, "fluxm",tab_data_flux(ii,2), "sfu")

         ret = acc_geti(acces, "iaamoy", tab_data_ia(ii,1))
         ret = acc_geti(acces, "iaa_1", tab_data_ia(ii,2))
         ret = acc_geti(acces, "iaa_2", tab_data_ia(ii,3))
         ret = acc_geti(acces, "iaa_3", tab_data_ia(ii,4))
         ret = acc_geti(acces, "iaa_4", tab_data_ia(ii,5))
         ret = acc_geti(acces, "iaa_5", tab_data_ia(ii,6))
         ret = acc_geti(acces, "iaa_6", tab_data_ia(ii,7))
         ret = acc_geti(acces, "iaa_7", tab_data_ia(ii,8))
         ret = acc_geti(acces, "iaa_8", tab_data_ia(ii,9))
         ret = acc_geti(acces, "iapmoy", tab_data_ia(ii,10))
      else
            
         ! Lecture de tous les champs
         ret = acc_getd(acces, "fluxpr", tab_data_flux(ii,1), "sfu")
         ret = acc_getd(acces, "fluxmp",tab_data_flux(ii,2), "sfu")

         ret = acc_geti(acces, "iaampr", tab_data_ia(ii,1))
         ret = acc_geti(acces, "iaapr_1", tab_data_ia(ii,2))
         ret = acc_geti(acces, "iaapr_2", tab_data_ia(ii,3))
         ret = acc_geti(acces, "iaapr_3", tab_data_ia(ii,4))
         ret = acc_geti(acces, "iaapr_4", tab_data_ia(ii,5))
         ret = acc_geti(acces, "iaapr_5", tab_data_ia(ii,6))
         ret = acc_geti(acces, "iaapr_6", tab_data_ia(ii,7))
         ret = acc_geti(acces, "iaapr_7", tab_data_ia(ii,8))
         ret = acc_geti(acces, "iaapr_8", tab_data_ia(ii,9))

         ! Pas de valeur iapmoy dans la prédiction
         tab_data_ia(ii,10) = 0

      endif

      ! fin de la selection
      ret = acc_select_end(acces)

      ! suivante
      ii = ii+1

   enddo boucle_struct

   ! Message d'avertissement on est sorti du tableau !!!
   if ( ii > nb_data+1 ) then
      call MSP_signaler_message (cle_mes="CPS_ERR_LIRE_ACSOL2", &
                  routine="cps_lireAcsol2")
   endif 

   ! Mise a jour du compteur de ligne
   indice_depart = ii

   ! Fermeture du fichier
   ret=acc_close(acces)

  end subroutine cpsi_lire_unficAcsol2_MADONA

  subroutine cpsi_cpter_lignes_Acsol2_MADONA(fichier, nb_data, &
       date_deb, date_fin) 
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_cpter_lignes_Acsol2_MADONA
!
!$Resume
!  lecture des donnees d'activité solaire d'un fichier MADONA de données,
!  et comptage des lignes respectant le critère de dates
!
!$Description
!  lecture des donnees d'activité solaire d'un fichier MADONA de données,
!  et comptage des lignes respectant le critère de dates
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call cpsi_cpter_lignes_Acsol2_MADONA(fichier, nb_data, &
!.           date_deb, date_fin) 
!.    character(LEN=*) :: fichier
!.    integer :: nb_data
!.    real(KIND=PM_REEL) :: date_deb
!.    real(KIND=PM_REEL) :: date_fin
!
!$Arguments
!>E     fichier   :<LEN=*>     nom du fichier de données
!>E/S   nb_data   :<integer>   nombre déléments des tableaux
!>E     date_deb  :<PM_REEL>   date de début en jj1950 frac (-1 si non nécessaire)
!>E     date_fin  :<PM_REEL>   date de fin (1E8 si non nécessaire)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_analyserCleAcsol2
!
!$Include
!
!$Module
!#V
!- cps_accesMadona
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_accesMadona

   implicit none

   ! arguments
   character(LEN=*), intent(in) :: fichier
   integer, intent(inout) :: nb_data
   real(KIND=PM_REEL), intent(in) :: date_deb
   real(KIND=PM_REEL), intent(in) :: date_fin

   ! Varaibles locales
   integer :: acces     ! Accès MADONA au fichier
   integer :: ret, ii
   character(LEN=CPS_MAXLG) :: libelle
   integer :: nature
   logical :: analyse_ok
   real(KIND=PM_REEL) :: date

   ! Ouverture du fichier
   ! On ne pas utiliser cps_getAccesMadonna 
   ! car le fichier appelant utilise déjà une unité madona 
   ! qui du coup risque d'être fermée
   acces  = acc_load(fichier)
   if (acces < 0) then
        call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
             routine="cpsi_lire_unficAcsol2_MADONA", &
             partie_variable=trim(fichier))
        return
   end if

   ! Init des variables de parcours
   ii = 0
   ret = ACC_OK

   ! Parcours de l'ensemble du fichier des données
   ret = acc_scan_reset(acces)
   ii = nb_data
   boucle_struct : do while (ret /= ACC_EOF )

      ! Lecture du contenu 
      ret = acc_scan(acces, libelle, nature)
      ! On veut des strcutures
      if (nature /= ACC_STRUCT )  cycle boucle_struct

      ! Anlayse de la cle
      call cpsi_analyserCleAcsol2(libelle, analyse_ok, date)
         
      ! Si la cle a bien été analysée
      ! et si la date est dans le créneau (optionnel valide)
      ! alors recopie
      if (analyse_ok .and. &
         date >= date_deb .and. &
         date <= date_fin ) then

         ! On peut compter cette valeur
         ii = ii+1
      endif

   enddo boucle_struct

   ! Mise a jour du compteur de ligne
   nb_data = ii

   ! Fermeture du fichier
   ret=acc_close(acces)

end subroutine cpsi_cpter_lignes_Acsol2_MADONA

end module cps_acsol2
