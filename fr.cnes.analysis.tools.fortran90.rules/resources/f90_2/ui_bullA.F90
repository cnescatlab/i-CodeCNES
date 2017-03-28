module ui_bullA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ui_bullA
!
!$Resume
!
!$Description
!  Module de gestion de la mise a jour automatique des donnees issues
!  du bulletin A de l'IERS dans COMPAS.
!
!$Auteur
!  vpg
!
!$Version
!  $Id: ui_bullA.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_bullA.F90,v $
!  Revision 1.9  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.8  2009/03/24 10:19:06  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!
!  Revision 1.7  2009/03/24 08:59:04  cml
!  DM-ID 1159 : Ajout d'initialisations de pointeurs manquantes
!
!  Revision 1.6  2008/11/04 10:21:10  cml
!  AQ : Ajout de mots clefs manquants pour les read
!
!  Revision 1.5  2008/04/11 12:45:04  vivaresf
!  Version 2.4 AQ : correction des cartouches
!
!  Revision 1.4  2008/04/11 12:02:24  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches
!
!  Revision 1.3  2008/04/11 10:09:20  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!  Revision 1.2  2008/03/04 11:08:48  vivaresf
!  FA-ID 996 : modification du test d'existence des
!  fichiers bulletinA_sure par années pour ne pas afficher d'erreur MADONA
!  Revision 1.1  2008/02/08 17:51:20  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.11  2006/12/01 10:00:56  vivaresf
!  Version 2-1p1
!  Revision 1.10  2006/11/30 12:27:27  vivaresf
!  FA 647, Version V2-1p1 :  Mauvais test d'existence des fichiers du bulletin A
!  Revision 1.9  2006/10/23 12:55:31  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.8.4.1  2006/10/23 10:11:44  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.8  2006/06/16 17:32:00  vivaresf
!  Cartouches d'entete
!  Revision 1.7  2006/06/14 12:38:48  vivaresf
!  Suppression code mort
!  lfn alloues par cps_file_unit
!  Revision 1.6  2006/05/31 13:08:36  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.5  2006/05/12 11:40:26  vpg
!  Remplissage des cartouches
!  Revision 1.4  2006/05/02 09:39:52  vpg
!  Diminution de la complexite de certaines fonctions pour respecter les seuils des metriques
!  Revision 1.3  2006/03/21 11:07:11  vpg
!  Ajout de la fonction de regeneration d'un bulletin
!  Revision 1.2  2006/03/20 16:12:54  vpg
!  Mise a jour des cartouches
!
!$FinHistorique
!
!$Usage
!  use ui_bullA
!
!$Structure
!
!: ligne_bullA : 
!>     year           : <integer>  
!>     month          : <integer>  
!>     day            : <integer>  
!>     mjd            : <PM_REEL>  
!>     pmxa           : <PM_REEL>  
!>     err_pmx        : <PM_REEL>  
!>     pmya           : <PM_REEL>  
!>     err_pmy        : <PM_REEL>  
!>     ut1_utc_a      : <PM_REEL>  
!>     err_ut1_utc    : <PM_REEL>  
!>     lod            : <PM_REEL>  
!>     err_lod        : <PM_REEL>  
!>     dpsi_a         : <PM_REEL>  
!>     err_dpsi       : <PM_REEL>  
!>     depsilon_a     : <PM_REEL>  
!>     err_depsilon   : <PM_REEL>  
!>     pmxb           : <PM_REEL>  
!>     pmyb           : <PM_REEL>  
!>     ut1_utc_b      : <PM_REEL>  
!>     dpsi_b         : <PM_REEL>  
!>     depsilon_b     : <PM_REEL>  
!>     flag1          : <LEN=1>    
!>     flag2          : <LEN=1>    
!>     flag3          : <LEN=1>    
!
!$Global
!
!#V
!>  ret                    : <integer,private>            
!>  CPS_MAX_LIGNES_BULLA   : <integer,parameter,private>  
!#
!$Common
!
!$Routines
!- cpsi_construirePartie
!- cpsi_getDerniereDate
!- cpsi_determinerFlags
!
!$Fonctions
!- cps_lireBullA
!- cps_majBullA
!- cpsi_getIndice
!- cpsi_majPartieSure
!- cpsi_majPartieSureAnnee
!- cpsi_majPartiePred
!- cps_regenererBullA
!- cpsi_getBullAPremiereDate
!
!$Include
!
!$Module
!#V
!- cps_util
!- cps_utilisateur
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
!.  cps_lireBullA cps_majBullA cpsi_getIndice cpsi_majPartieSure cpsi_majPartieSureAnnee cpsi_majPartiePred
!.  cps_regenererBullA cpsi_getBullAPremiereDate cpsi_construirePartie cpsi_getDerniereDate
!.  cpsi_determinerFlags
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use cps_util
  use cps_utilisateur

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ui_bullA.F90 69 2012-09-11 08:33:34Z ffsm $'


  type ligne_bullA
     integer :: year, month, day
     real(KIND=PM_REEL) :: mjd, pmxa, err_pmx, pmya, err_pmy, ut1_utc_a, err_ut1_utc
     real(KIND=PM_REEL) :: lod, err_lod, dpsi_a, err_dpsi, depsilon_a, err_depsilon
     real(KIND=PM_REEL) :: pmxb, pmyb, ut1_utc_b, dpsi_b, depsilon_b
     character (LEN=1) :: flag1, flag2, flag3
  end type ligne_bullA
  
  integer, private :: ret
  integer, parameter, private :: CPS_MAX_LIGNES_BULLA = 200

contains

  function cps_lireBullA(fichier, tab_lignes, ligne) result(ierr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_lireBullA
!
!$Resume
!  Lecture des fichiers bulletin A
!$Description
!  Fonction de lecture d'un bulletin A (fichier brut).
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ierr = cps_lireBullA(fichier, tab_lignes, ligne)
!.    character(LEN=*) :: fichier
!.    type(ligne_bullA), dimension(:), pointer :: tab_lignes
!.    integer :: ligne
!.    integer :: ierr
!
!$Arguments
!>E     fichier     :<LEN=*>                         chemin complet du fichier brut correspondant au bulletin A
!>E/S   tab_lignes  :<ligne_bullA,DIM=(:),pointer>   tableau des lignes du fichier brut
!>S     ligne       :<integer>                       numéro de la dernière ligne du fichier brut lue
!>S     ierr        :<integer>                       0 si la lecture s'est correctement effectuée
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
    ! arguments
    character(LEN=*), intent(in) :: fichier
    type(ligne_bullA), dimension(:), pointer :: tab_lignes
    integer, intent(out) :: ligne

    ! resultat
    integer :: ierr
    
    ! variables locales
    integer :: unit, nb_lignes
    
    ! initialisation
    nb_lignes = 0
    ligne = 0
    ierr = 0
!    unit = 13

    ! determination du nombre de lignes
    call cps_file_unit(unit,ierr)
    if(ierr.lt.0)then
       return
    endif
    open(unit=unit,file=fichier,status='old',form='formatted')
    do
       read(unit=unit, fmt=10, iostat=ierr)
       if (ierr<0) then
          ! fin du fichier
          exit
       else
          nb_lignes = nb_lignes+1
       end if
    end do
    close (unit)

    ! allocation memoire
    allocate(tab_lignes(nb_lignes))


    ! ouverture du fichier
    open(unit=unit,file=fichier,status='old',form='formatted')
    
    ! lecture du fichier ligne par ligne
    do ligne = 1,nb_lignes
       read(unit=unit, fmt=10, iostat=ierr) tab_lignes(ligne)%year,&
            tab_lignes(ligne)%month,tab_lignes(ligne)%day,tab_lignes(ligne)%mjd,&
            tab_lignes(ligne)%flag1,tab_lignes(ligne)%pmxa,tab_lignes(ligne)%err_pmx,&
            tab_lignes(ligne)%pmya,tab_lignes(ligne)%err_pmy, tab_lignes(ligne)%flag2,&
            tab_lignes(ligne)%ut1_utc_a, tab_lignes(ligne)%err_ut1_utc,&
            tab_lignes(ligne)%lod,tab_lignes(ligne)%err_lod,tab_lignes(ligne)%flag3,&
            tab_lignes(ligne)%dpsi_a, tab_lignes(ligne)%err_dpsi,&
            tab_lignes(ligne)%depsilon_a,tab_lignes(ligne)%err_depsilon,tab_lignes(ligne)%pmxb,&
            tab_lignes(ligne)%pmyb, tab_lignes(ligne)%ut1_utc_b,tab_lignes(ligne)%dpsi_b,&
            tab_lignes(ligne)%depsilon_b

       if (ierr > 0) then
          exit
       endif
    enddo

    ! fermeture du fichier
    close(unit)

10  format (3i2,1x,f8.2,1x,1a1,2(1x,2f9.6),2x,1a1,2f10.7,1x,2f7.4,2x,1a1,2(1x,2f9.3),2f10.6,f11.7,2f10.3)
    
  end function cps_lireBullA


  function cps_majBullA(date_du_jour, fichier_brut, fichier_brut_2000) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_majBullA
!
!$Resume
! Mise a jour des fichiers bulletin A
!$Description
!  Mise a jour des fichiers MADONA de donnees fixes et des donnees
!  predites issues du bulletin A.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_majBullA(date_du_jour, fichier_brut, fichier_brut_2000)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    character(LEN=*) :: fichier_brut, fichier_brut_2000
!.    integer :: ok
!
!$Arguments
!>E     date_du_jour       :<PM_REEL>   date de mise à jour (unité MJD)
!>E     fichier_brut       :<LEN=*>     chemin complet du fichier brut correspondant au bulletin A (ancien standard)
!>E     fichier_brut_2000  :<LEN=*>     chemin complet du fichier brut correspondant au bulletin A (nouveau standard)
!>S     ok                 :<integer>   CPS_OK si la mise à jour s'est effectuée correctement
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
    ! arguments
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    character(LEN=*), intent(in) :: fichier_brut, fichier_brut_2000
    
    ! resultat
    integer :: ok
    
    ! variables locales
    type(ligne_bullA), dimension(:), pointer :: data0 => NULL()
    type(ligne_bullA), dimension(:), pointer :: data_2000 => NULL()
    integer :: nb_lignes, nb_lignes_2000, l_sure, l_pred
    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15) :: data_sure, data_pred
    
    
    ! initialisation
    ok = CPS_ERR_DEF
    nb_lignes = 0
    l_sure = 0
    l_pred = 0
    
    ! lecture du fichier brut ancien standard
    ok = cps_lireBullA(trim(fichier_brut), data0, nb_lignes)
    if(ok.ne.0) then
       ! erreur de lecture
       return
    end if
    ! lecture du fichier brut nouveau standard
    ok = cps_lireBullA(trim(fichier_brut_2000), data_2000, nb_lignes_2000)
    if(ok.ne.0) then
       ! erreur de lecture
       return
    end if
    
    ! construction des donnes sures avec le nouveau et l'ancien format
    call cpsi_construirePartie(0, data0(1:nb_lignes), data_2000(1:nb_lignes_2000), data_sure, l_sure)
    ! construction des donnes predites avec le nouveau et l'ancien format
    call cpsi_construirePartie(1, data0(1:nb_lignes), data_2000(1:nb_lignes_2000), data_pred, l_pred)
    
    ! mise a jour des donnees sures
    ok = cpsi_majPartieSure(date_du_jour, data_sure, l_sure)

    ! mis a jour des donnees predites
    ok = cpsi_majPartiePred(date_du_jour, data_pred, l_pred)
    
    ! liberation memoire
    if (associated(data0)) then
       deallocate(data0)
    end if
    if (associated(data_2000)) then
       deallocate(data_2000)
    end if

  end function cps_majBullA
  

  subroutine cpsi_construirePartie(type, data, data_2000, tab, l_tab)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_construirePartie
!
!$Resume
!  Construit un tableau avec les données du bulletinA
!$Description
!  Construction d'un tableau contenant a la fois les donnees a
!  l'ancien et au nouveau standard.
!   - si type = 0 : partie sure.
!   - si type = 1 : partie predite.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_construirePartie(type, data, data_2000, tab, l_tab)
!.    integer :: type
!.    type(ligne_bullA), dimension(:) :: data, data_2000
!.    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15) :: tab
!.    integer :: l_tab
!
!$Arguments
!>E     type       :<integer>                                 0 pour les données sures, 1 pour les prédictions   
!>E     data       :<ligne_bullA,DIM=(:)>                     lignes du bulletin A contenant les données sous l'ancien standard
!>E     data_2000  :<ligne_bullA,DIM=(:)>                     lignes du bulletin A contenant les données sous le nouveau standard
!>S     tab        :<PM_REEL,DIM=(CPS_MAX_LIGNES_BULLA,15)>   tableau contenant les données sûres
!>S     l_tab      :<integer>                                 nombre d'éléments de 'tab'
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
    ! arguments
    integer, intent(in) :: type
    type(ligne_bullA), dimension(:), intent(in) :: data, data_2000
    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15), intent(out) :: tab
    integer, intent(out) :: l_tab
    
    ! variables locales
    integer :: ii, indice, l_tab_2000, res
    real(KIND=PM_REEL) :: date
    logical :: decrementer
    
    ! initialisation
    l_tab = 0
    tab(:,:) = 0.0_pm_reel

    ! ancien standard
    do ii=1,size(data)
       ! on recupere la date des donnees
       date = data(ii)%mjd
       ! on recupere l'indice dans le tableau
       indice = cpsi_getIndice(tab, date, l_tab)
       
       decrementer = .true.

       ! UTC-UT1 + erreur
       if (((type.eq.0).and.(data(ii)%flag2.eq.'I')).or.                   &
            ((type.eq.1).and.(data(ii)%flag2.eq.'P'))) then
          tab(indice,2) = data(ii)%ut1_utc_a
          tab(indice,3) = data(ii)%err_ut1_utc
          decrementer = .false.
       end if

       ! PM_x et PM_y
       if (((type.eq.0).and.(data(ii)%flag1.eq.'I')).or.                   &
            ((type.eq.1).and.(data(ii)%flag1.eq.'P'))) then
          tab(indice,4) = data(ii)%pmxa
          tab(indice,5) = data(ii)%pmya
          tab(indice,6) = data(ii)%err_pmx
          tab(indice,7) = data(ii)%err_pmy
          decrementer = .false.
       end if
       
       ! dpsi et deps
       if (((type.eq.0).and.(data(ii)%flag3.eq.'I')).or.                   &
            ((type.eq.1).and.(data(ii)%flag3.eq.'P'))) then
          tab(indice,8) = data(ii)%dpsi_a
          tab(indice,9) = data(ii)%depsilon_a
          tab(indice,10) = data(ii)%err_dpsi
          tab(indice,11) = data(ii)%err_depsilon
          decrementer = .false.
       end if
       
       if (decrementer) then
          l_tab = l_tab-1
       end if
    end do

    l_tab_2000 = l_tab

    ! ajout du nouveau standard
    do ii=1,size(data_2000)
       ! on recupere la date des donnees
       date = data_2000(ii)%mjd
       ! on recupere l'indice dans le tableau
       indice = cpsi_getIndice(tab, date, l_tab_2000)
       
       res = cpsi_compareReels(date, tab(l_tab,1))
       if (res.eq.-1) then
          ! dX et dY
          if (((type.eq.0).and.(data_2000(ii)%flag3.eq.'I')).or.                   &
               ((type.eq.1).and.(data_2000(ii)%flag3.eq.'P'))) then
             tab(indice,12) = data_2000(ii)%dpsi_a
             tab(indice,13) = data_2000(ii)%depsilon_a
             tab(indice,14) = data_2000(ii)%err_dpsi
             tab(indice,15) = data_2000(ii)%err_depsilon
          end if
       end if
    end do

  end subroutine cpsi_construirePartie


  function cpsi_getIndice(tab, date, l_tab) result(indice)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getIndice
!
!$Resume
!  Associe un indice à une date
!$Description
!  Fonction interne : renvoie l'indice dans le tableau 'tab' dont la
!  ligne est associee a la date 'date'.
!  Si la date n'est pas presente, elle est ajoutee a la fin.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  indice = cpsi_getIndice(tab, date, l_tab)
!.    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15) :: tab
!.    real(KIND=PM_REEL) :: date
!.    integer :: l_tab
!.    integer :: indice
!
!$Arguments
!>E/S   tab     :<PM_REEL,DIM=(CPS_MAX_LIGNES_BULLA,15)>   tableau contenant les données sûres ou les prédictions
!>E     date    :<PM_REEL>                                 date recherchée (unité MJD)
!>E/S   l_tab   :<integer>                                 nombre d'éléments de 'tab'
!>S     indice  :<integer>                                 indice dans 'tab' dont les données sont associée à 'date'
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
    ! arguments
    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15), intent(inout) :: tab
    real(KIND=PM_REEL), intent(in) :: date
    integer, intent(inout) :: l_tab
    
    ! resultat
    integer :: indice

    ! variables locales
    integer :: res, ii
    logical :: trouve

    ! initialisation
    trouve = .false.
    indice = 0
    
    do ii=1, l_tab
       res = cpsi_compareReels(date, tab(ii,1))
       if (res.eq.0) then
          indice = ii
          trouve = .true.
          exit
       end if
    end do
    
    if (.not.trouve) then
       ! la date n'est pas presente
       ! on la rajoute a la fin
       l_tab = l_tab+1
       indice = l_tab
       tab(indice, 1) = date
    end if

  end function cpsi_getIndice




  function cpsi_majPartieSure(date_du_jour, data_sure, l_sure) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_majPartieSure
!
!$Resume
!  Mise à jour du bulletin A (partie sure)
!$Description
!  Mise a jour de la partie sure du bulletin A (fonction principale)
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cpsi_majPartieSure(date_du_jour, data_sure, l_sure)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15) :: data_sure
!.    integer :: l_sure
!.    integer :: ok
!
!$Arguments
!>E     date_du_jour  :<PM_REEL>                                 date de mise à jour (unité MJD)
!>E/S   data_sure     :<PM_REEL,DIM=(CPS_MAX_LIGNES_BULLA,15)>   données sûres à intégrer
!>E     l_sure        :<integer>                                 nombre d'éléments de 'data_sure'
!>S     ok            :<integer>                                 CPS_OK si la mise à jour s'est correctement effectuée
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
    ! arguments
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15), intent(inout) :: data_sure
    integer, intent(in) :: l_sure
    
    ! retour
    integer :: ok

    ! variables locales
    integer :: ii, ind_debut, ind_fin
    real(KIND=PM_REEL) :: date1950, sec
    integer :: jj, mm, aaaa, h, min, aaaa_courante
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour

    ! initialisation
    ok = CPS_ERR_DEF
    ind_debut = 1
    ind_fin = 1

    date1950 = data_sure(1,1)-33282.00
    call md_jourfrac_joursec(date1950, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa_courante, mm, jj, h, min, sec, code_retour)

    ! decoupage de data_sure en annee
    do
       do ii=ind_debut, l_sure
          date1950 = data_sure(ii,1)-33282.00
          call md_jourfrac_joursec(date1950, jour_sec, code_retour)
          call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
          if (aaaa_courante.ne.aaaa) then
             aaaa_courante = aaaa
             exit
          else
             ind_fin = ii
          end if
       end do
       
       !
       ok = cpsi_majPartieSureAnnee(date_du_jour, data_sure, ind_debut, ind_fin) 
       
       ind_debut = ind_fin+1

       if (ind_fin.eq.l_sure) then
          exit
       end if
    end do
 
    
  end function cpsi_majPartieSure




  function cpsi_majPartieSureAnnee(date_du_jour, data_sure_annee, ind_debut, ind_fin) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_majPartieSureAnnee
!
!$Resume
!  Mise à jour du bulletin A partie sure
!$Description
!  Mise a jour de la partie sure du bulletin A dans le fichier d'année
!  concerné
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cpsi_majPartieSureAnnee(date_du_jour, data_sure_annee, ind_debut, ind_fin)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15) :: data_sure_annee
!.    integer :: ind_debut, ind_fin
!.    integer :: ok
!
!$Arguments
!>E     date_du_jour     :<PM_REEL>                                 date de mise à jour (unité MJD)
!>E/S   data_sure_annee  :<PM_REEL,DIM=(CPS_MAX_LIGNES_BULLA,15)>   données sûres
!>E     ind_debut        :<integer>                                 indice de début de lecture dans 'data_sure_annee'
!>E     ind_fin          :<integer>                                 indice de fin de lecture dans 'data_sure_annee'
!>S     ok               :<integer>                                 CPS_OK si la mise à jour s'est bien passée, CPS_ERR_DEF sinon
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
    ! arguments
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15), intent(inout) :: data_sure_annee
    integer, intent(in) :: ind_debut, ind_fin
    
    ! retour
    integer :: ok, trouve

    ! variables locales
    integer :: res, acces, ii, l_sure, ll, nature, clos, indice
    character(LEN=256) :: fichier, cle, libelle
    real(KIND=PM_REEL) :: date, derniere_date, zero
    logical :: maj

    integer :: acces_fic, jj, mm, aaaa, h, min, nb_fic
    real(KIND=PM_REEL) :: sec, date1950
    character(LEN=256) :: buff_aaaa
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    logical :: okexist
    integer :: ret

    ! fonctions utilisees
    integer :: cps_ihm_creerFichier
    
    ! initialisation
    l_sure = ind_fin-ind_debut+1
    ll = l_sure
    ok = CPS_ERR_DEF
    zero = 0.0
    
    ! obtenir le fichier concerne
    fichier=""
    trouve = cps_getFichierBullASure(data_sure_annee(ind_debut,1), acces, fichier)

    if (trouve.eq.CPS_OK) then
       ! Vérifier que le fichier existe bien et est au format MADONA
       inquire(file=trim(fichier), exist=okexist)
       if (.not.okexist) then
          trouve=CPS_ERR_DEF
       else
          ret=acc_err_setmode(ACC_ERR_MEMO)
          acces=acc_load(trim(fichier))
          ret=acc_err_setmode(ACC_ERR_ECHO)
          if (acces < 1) trouve=CPS_ERR_DEF
       endif
    endif


    if (trouve.ne.CPS_OK) then
       ! si le fichier n'existe pas, on le crée
       date1950 = data_sure_annee(ind_debut,1)-33282.00
       call md_jourfrac_joursec(date1950, jour_sec, code_retour)
       call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
       write(buff_aaaa,20) aaaa
       fichier="Poles/bullA_sure_"//buff_aaaa
       ! referencer le fichier
       call cps_getListFichiersCateg("bulletinA_sure", fichiers)
       nb_fic = size(fichiers)
       call cpsi_getAccesMadona(trim(fichiers(nb_fic)), acces_fic)
       ret = acc_create(acces_fic, "annee_"//trim(buff_aaaa), ACC_STRUCT, "")
       ret = acc_puti(acces_fic, "annee_"//trim(buff_aaaa)//".annee", aaaa)
       ret = acc_puts(acces_fic, "annee_"//trim(buff_aaaa)//".fichier", trim(fichier))
       ! enregistrement
       call cps_ihm_ecrireFichier(acces_fic, trim(fichiers(nb_fic)))
       fichier = trim(rep_base_ref)//"/"//trim(fichier)
       ! creer le fichier
       ret = cps_ihm_creerFichier(trim(fichier))
       ! acces sur le fichier
       call cpsi_getAccesMadona(trim(fichier), acces)
       ! liberation memoire
       if (associated(fichiers)) then
          deallocate(fichiers)
       end if
    end if
    
    ! obtenir la derniere date rentree dans COMPAS
    call cpsi_getDerniereDate(acces, derniere_date)

    ! obtenir l'indice dans data_sure correspondant
    ! a la premiere ligne de data_sure a rentrer dans COMPAS
    res = cpsi_compareReels(derniere_date, zero)
    if (res.eq.1) then
       ! obtenir l'indice dans data_sure_annee correspondant a la premiere date
       ! apres 'derniere_date'
       indice = 0
       do ii=ind_debut, ind_fin
          res = cpsi_compareReels(derniere_date, data_sure_annee(ii,1))
          if (res.eq.-1) then
             indice = ii
             exit
          end if
       end do
       
       if ((indice.ge.ind_debut).and.(ind_fin.le.ind_fin)) then
          ! pas de mise a jour a effectuer
          maj = .true.
       else
          maj = .false.
       end if
    else
       indice = ind_debut
       maj = .true.
    end if
    
    ! cloture des donnees COMPAS qui ne sont plus dans
    ! le bulletin courant
    ret = acc_scan_reset(acces)
    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       elseif (nature.eq.ACC_STRUCT) then
          ret = acc_getd(acces, trim(libelle)//".date", date, "")
          res = cpsi_compareReels(date, data_sure_annee(ind_debut,1))
          if (res.eq.-1) then
             ! la date de la structure est anterieure a la
             ! premiere date du bulletin du jour
             ret = acc_puti(acces, trim(libelle)//".clos", 1)
          end if
       end if
    end do

    ! mise a jour de date_fin pour les donnees non closes
    ret = acc_scan_reset(acces)
    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       elseif (nature.eq.ACC_STRUCT) then
          ret = acc_geti(acces, trim(libelle)//".clos", clos)
          if (clos.eq.0) then
             ! mise a jour de date_fin
             ret = acc_putd(acces, trim(libelle)//".date_fin", date_du_jour, "")
          end if
       end if
    end do

    ! ecrire les nouvelles donnees
    if (maj) then
       do ii=indice, ind_fin
          date = data_sure_annee(ii,1)
          ! nom de la structure
          cle = cpsi_getCleBullA(date)
          ! creation de la structure
          ret = acc_create(acces, trim(cle), ACC_STRUCT, "")
          ! selection de la structure
          ret = acc_select(acces, trim(cle), ACC_STRUCT)
          ! ecriture de la date
          ret = acc_putd(acces, "date", date, "")
          ! UTC-UT1 + erreur
          ret = acc_putd(acces, "UTC_UT1", -data_sure_annee(ii,2), "s")
          ret = acc_putd(acces, "UTC_UT1_err", -data_sure_annee(ii,3), "s")
          ! PM_x, PM_y + erreurs
          ret = acc_putd(acces, "PM_x", data_sure_annee(ii,4), "")
          ret = acc_putd(acces, "PM_y", data_sure_annee(ii,5), "")
          ret = acc_putd(acces, "PM_x_err", data_sure_annee(ii,6), "")
          ret = acc_putd(acces, "PM_y_err", data_sure_annee(ii,7), "")
          ! dpsi, deps + erreurs
          ret = acc_putd(acces, "dpsi", data_sure_annee(ii,8), "")
          ret = acc_putd(acces, "deps", data_sure_annee(ii,9), "")
          ret = acc_putd(acces, "dpsi_err", data_sure_annee(ii,10), "")
          ret = acc_putd(acces, "deps_err", data_sure_annee(ii,11), "")
          ! dX, dY + erreurs
          ret = acc_putd(acces, "dX", data_sure_annee(ii,12), "")
          ret = acc_putd(acces, "dY", data_sure_annee(ii,13), "")
          ret = acc_putd(acces, "dX_err", data_sure_annee(ii,14), "")
          ret = acc_putd(acces, "dY_err", data_sure_annee(ii,15), "")
          ! date_debut, date_fin et clos
          ret = acc_putd(acces, "date_debut", date_du_jour, "")
          ret = acc_putd(acces, "date_fin", date_du_jour, "")
          ret = acc_puti(acces, "clos", 0)
          ! fin de la selection
          ret = acc_select_end(acces)
       end do
       ok = CPS_OK
    end if
    
    ! enregistrer les modifs
    call cps_ihm_ecrireFichier(acces, trim(fichier))

20  format(i4)
    
  end function cpsi_majPartieSureAnnee

  subroutine cpsi_getDerniereDate(acces, derniere_date)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getDerniereDate
!
!$Resume
!  dernière date rentrée dans COMPAS
!$Description
!  Routine interne : obtention de la derniere date rentree dans COMPAS.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_getDerniereDate(acces, derniere_date)
!.    integer :: acces
!.    real(KIND=PM_REEL) :: derniere_date
!
!$Arguments
!>E     acces          :<integer>   accès MADONA ouvert sur un fichier de données
!>S     derniere_date  :<PM_REEL>   dernière date de données référencées dans le fichier
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
    ! arguments
    integer, intent(in) :: acces
    real(KIND=PM_REEL), intent(out) :: derniere_date
    
    ! variables locales
    character(LEN=256) :: libelle
    integer :: nature

    ! initialisation
    derniere_date = 0.0
    ret = acc_scan_reset(acces)
    
    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       elseif (nature.eq.ACC_STRUCT) then
          ret = acc_getd(acces, trim(libelle)//".date", derniere_date, "")
       end if
    end do
    
  end subroutine cpsi_getDerniereDate




  function cpsi_majPartiePred(date_du_jour, data_pred, l_pred) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_majPartiePred
!
!$Resume
!  Mise a jour de la partie predite du bulletin A 
!$Description
!  Mise a jour de la partie predite du bulletin A (fonction principale)
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cpsi_majPartiePred(date_du_jour, data_pred, l_pred)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15) :: data_pred
!.    integer :: l_pred
!.    integer :: ok
!
!$Arguments
!>E     date_du_jour  :<PM_REEL>                                 date de mise à jour
!>E/S   data_pred     :<PM_REEL,DIM=(CPS_MAX_LIGNES_BULLA,15)>   prédictions
!>E     l_pred        :<integer>                                 nombre d'éléments de 'data_pred'
!>S     ok            :<integer>                                 CPS_OK si la mise à jour s'est bien passée
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
    ! arguments
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    real(KIND=PM_REEL), dimension(CPS_MAX_LIGNES_BULLA,15), intent(inout) :: data_pred
    integer, intent(in) :: l_pred
     
    ! resultat
    integer :: ok

    ! variables locales
    integer :: trouve, acces
    character(LEN=256) :: fichier, cle
    real(KIND=PM_REEL) :: date1950, sec
    integer :: jj, mm, aaaa, h, min
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    character(LEN=256) :: buff_jj, buff_mm, buff_aaaa
    
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    integer :: nb_fic, acces_fic, ii, ret
    character(LEN=256) :: cle_fic

    logical :: okexist

    ! fonction utilisee
    integer :: cps_ihm_creerFichier


    ! Initialisations
    ok = CPS_ERR_DEF


    ! Dates en JJ50
    date1950 = date_du_jour-33282.00
    call md_jourfrac_joursec(date1950, jour_sec, code_retour)
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
    
    ! obtenir le fichier concerne
    fichier=""
    trouve = cps_getFichierBullAPred(date_du_jour, acces, fichier)

    if (trouve.eq.CPS_OK) then
       ! Vérifier que le fichier existe bien et est au format MADONA
       inquire(file=trim(fichier), exist=okexist)
       if (.not.okexist) then
          trouve=CPS_ERR_DEF
       else
          ret=acc_err_setmode(ACC_ERR_MEMO)
          acces=acc_load(trim(fichier))
          ret=acc_err_setmode(ACC_ERR_ECHO)
          if (acces < 1) trouve=CPS_ERR_DEF
       endif
    endif
    
    if (trouve.eq.CPS_ERR_DEF) then
       ! fichier n'existe pas
       ! il faut le creer
       fichier = "Poles/bullA_pred_"//trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)
       ! on le reference
       call cps_getListFichiersCateg("bulletinA_pred", fichiers)
       nb_fic = size(fichiers)
       call cpsi_getAccesMadona(trim(fichiers(nb_fic)), acces_fic)
       cle_fic = "jour_"//trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)
       ret = acc_create(acces_fic, trim(cle_fic), ACC_STRUCT, "")
       ret = acc_putd(acces_fic, trim(cle_fic)//".date", date_du_jour, "")
       ret = acc_puts(acces_fic, trim(cle_fic)//".fichier", trim(fichier))
       ret = acc_putd(acces_fic, trim(cle_fic)//".pred_min", data_pred(1,1), "")
       ret = acc_putd(acces_fic, trim(cle_fic)//".pred_max", data_pred(l_pred,1), "")
       call cps_ihm_ecrireFichier(acces_fic, trim(fichiers(nb_fic)))
       ! creer le fichier
       fichier = trim(rep_base_ref)//"/"//trim(fichier)
       ret = cps_ihm_creerFichier(trim(fichier))
       ! ouvrir l'acces
       call cpsi_getAccesMadona(trim(fichier), acces)
    else
       ! on ecrase le fichier
       ret = acc_delete(acces, ACC_ALL)
    end if



    do ii=1, l_pred
       ! ecriture de chaque
       cle = cpsi_getCleBullA(data_pred(ii,1))
       cle = trim(cle)//"_"//trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)
       
       ! creation de la structure
       ret = acc_create(acces, trim(cle), ACC_STRUCT, "")
       ! selection de la structure
       ret = acc_select(acces, trim(cle), ACC_STRUCT)
       ! ecriture de la date
       ret = acc_putd(acces, "date", data_pred(ii,1), "")
       ! UTC-UT1 + erreur
       ret = acc_putd(acces, "UTC_UT1", -data_pred(ii,2), "s")
       ret = acc_putd(acces, "UTC_UT1_err", -data_pred(ii,3), "s")
       ! PM_x, PM_y + erreurs
       ret = acc_putd(acces, "PM_x", data_pred(ii,4), "")
       ret = acc_putd(acces, "PM_y", data_pred(ii,5), "")
       ret = acc_putd(acces, "PM_x_err", data_pred(ii,6), "")
       ret = acc_putd(acces, "PM_y_err", data_pred(ii,7), "")
       ! dpsi, deps + erreurs
       ret = acc_putd(acces, "dpsi", data_pred(ii,8), "")
       ret = acc_putd(acces, "deps", data_pred(ii,9), "")
       ret = acc_putd(acces, "dpsi_err", data_pred(ii,10), "")
       ret = acc_putd(acces, "deps_err", data_pred(ii,11), "")
       ! dX, dY + erreurs
       ret = acc_putd(acces, "dX", data_pred(ii,12), "")
       ret = acc_putd(acces, "dY", data_pred(ii,13), "")
       ret = acc_putd(acces, "dX_err", data_pred(ii,14), "")
       ret = acc_putd(acces, "dY_err", data_pred(ii,15), "")
       ! date_debut, date_fin et clos
       ret = acc_putd(acces, "date_debut", date_du_jour, "")
       ret = acc_putd(acces, "date_fin", date_du_jour, "")
       ret = acc_puti(acces, "clos", 0)
       ! fin de la selection
       ret = acc_select_end(acces)
       ok = CPS_OK
    end do

    ! enregistrer le fichier
    call cps_ihm_ecrireFichier(acces, trim(fichier))

    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers)
    end if

10  format (i2)
20  format (i4)
    
  end function cpsi_majPartiePred

  
  function cps_regenererBullA(date, nom_fic, nom_fic_2000A) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_regenererBullA
!
!$Resume
!  Création d'un fichier bulletin A
!$Description
!  Creer un fichier correspondant au bulletin A ancien standard et un
!  bulletin A nouveau standard tel qu'ils étaient à une date donnée.
!  Toutefois, les champs LOD sont à zéro, ainsi que les 5 derniers champs
!  correspondant au bulletin B.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_regenererBullA(date, nom_fic, nom_fic_2000A)
!.    real(KIND=PM_REEL) :: date
!.    character(LEN=*) :: nom_fic, nom_fic_2000A
!.    integer :: ok
!
!$Arguments
!>E     date           :<PM_REEL>   date du bulletin A (unité MJD) à regénérer
!>E     nom_fic        :<LEN=*>     nom du fichier correspondant au bulletin A regénéré avec les anciens standards
!>E     nom_fic_2000A  :<LEN=*>     nom du fichier correspondant au bulletin A regénéré avec les nouveaux standards
!>S     ok             :<integer>   CPS_OK si la regénération s'est bien passée
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
    ! arguments
    real(KIND=PM_REEL), intent(in) :: date
    character(LEN=*), intent(in) :: nom_fic, nom_fic_2000A
    
    ! resultat
    integer :: ok
    
    ! variables locales
    integer :: unit, unit_2000A, ierr, ierr_2000A
    real(KIND=PM_REEL) :: date_deb, date_fin, sec
    integer :: nb_iter, i, nb_data, nb_pred, res, k
    integer :: jj, mm, aa, h, min
    type(tm_jour_sec) :: joursec
    type(tm_code_retour) :: code_ret
    character(LEN=1) :: I1, I2, I3
    real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_SURE,15) :: data_sure
    real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_PRED,15) :: data_pred
    real(KIND=PM_REEL), dimension(2) :: lod
    real(KIND=PM_REEL), dimension(5) :: bullB

    ! initialisation
    ok = CPS_ERR_DEF
    ! date la plus ancienne
    date_deb = cpsi_getBullAPremiereDate()
    nb_iter = int(date)-int(date_deb)
    nb_iter = (nb_iter/CPS_MAX_BULLA_SURE)+1    

    ! creation et ouverture des fichiers
!    unit = 15
!    unit_2000A = 16
    call cps_file_unit(unit,ierr)
    if(ierr.lt.0)then
       return
    endif
    open(unit, file=trim(nom_fic), iostat=ierr)
    call cps_file_unit(unit_2000A,ierr)
    if(ierr.lt.0)then
       return
    endif
    open(unit_2000A, file=trim(nom_fic_2000A), iostat=ierr_2000A)
    
    ! predictions
    ok = cps_getBullAPred(date, data_pred, nb_pred)

    ! parties sures
    do i=1, nb_iter
       date_fin = date_deb+CPS_MAX_BULLA_SURE-1
       res = cpsi_compareReels(date_fin, date)
       if (res.eq.1) then
          date_fin = date
       end if
       ok = cps_getBullASureCreneau(date_deb, date_fin, data_sure, nb_data)
       if (ok.ne.CPS_OK) then
          return
       end if
       do k = 1, nb_data
          ! calcul de la date en jj/mm/aa
          call md_jourfrac_joursec(data_sure(k,1)-33282.0_pm_reel, joursec, code_ret)
          call md_julien_calend(joursec, aa, mm, jj, h, min, sec, code_ret)
          aa = aa-2000
          ! ancien standard
          ! determination des flags I1, I2, I3
          call cpsi_determinerFlags(nb_pred, k, data_sure, data_pred, I1, I2, I3)

          ! ecriture dans le fichier
          write(unit,10) aa, mm, jj, data_sure(k,1), I1, data_sure(k,4),&
               data_sure(k,6), data_sure(k,5), data_sure(k,7), I2,      &
               -data_sure(k,2), -data_sure(k,3), lod(1), lod(2), I3,    &
               data_sure(k,8), data_sure(k,10), data_sure(k,9),         &
               data_sure(k,11), bullB(1), bullB(2), bullB(3), bullB(4), &
               bullB(5)
          
          ! nouveau standard
          ! ecriture dans le fichier
          write(unit_2000A,10) aa, mm, jj, data_sure(k,1), I1,          &
               data_sure(k,4), data_sure(k,6), data_sure(k,5),          &
               data_sure(k,7), I2, -data_sure(k,2), -data_sure(k,3),    &
               lod(1), lod(2), I3, data_sure(k,12), data_sure(k,14),    &
               data_sure(k,13), data_sure(k,15), bullB(1), bullB(2),    &
               bullB(3), bullB(4), bullB(5)
       end do
       ! fin
       date_deb = date_fin+1
    end do
        
    ! predictions
    I1 = 'P'
    I2 = 'P'
    I3 = 'P'
    do k=1,nb_pred
       res = cpsi_compareReels(data_pred(k,1), date)
       if (res.eq.1) then
          ! calcul de la date en jj/mm/aa
          call md_jourfrac_joursec(data_pred(k,1)-33282.0_pm_reel, joursec, code_ret)
          call md_julien_calend(joursec, aa, mm, jj, h, min, sec, code_ret)
          aa = aa-2000
          ! ancien standard
           write(unit,10) aa, mm, jj, data_pred(k,1), I1, data_pred(k,4),&
               data_pred(k,6), data_pred(k,5), data_pred(k,7), I2,      &
               -data_pred(k,2), -data_pred(k,3), lod(1), lod(2), I3,    &
               data_pred(k,8), data_pred(k,10), data_pred(k,9),         &
               data_pred(k,11), bullB(1), bullB(2), bullB(3), bullB(4), &
               bullB(5)
          ! nouveau standard
          write(unit_2000A,10) aa, mm, jj, data_pred(k,1), I1,          &
               data_pred(k,4), data_pred(k,6), data_pred(k,5),          &
               data_pred(k,7), I2, -data_pred(k,2), -data_pred(k,3),    &
               lod(1), lod(2), I3, data_pred(k,12), data_pred(k,14),    &
               data_pred(k,13), data_pred(k,15), bullB(1), bullB(2),    &
               bullB(3), bullB(4), bullB(5)
       end if
    end do

    ! fermeture des fichiers
    close(unit, iostat=ierr)
    close(unit_2000A, iostat=ierr_2000A)

    ! format
10  format(3i2,(1x,f8.2),(1x,1a),2(1x,2f9.6),(2x,1a),2f10.7,1x,2f7.4,(2x,1a),2(1x,2f9.3),2f10.6,f11.7,2f10.3)

  end function cps_regenererBullA

  
  subroutine cpsi_determinerFlags(nb_pred, k, data_sure, data_pred, I1, I2, I3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_determinerFlags
!
!$Resume
! routine interne pour la routine cps_regenererBullA()
!
!$Description
! routine interne pour la routine cps_regenererBullA()
!
!$Auteur
!   VPG
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_determinerFlags(nb_pred, k, data_sure, data_pred, I1, I2, I3)
!.    integer :: nb_pred, k
!.    character(LEN=1) :: I1, I2, I3
!.    real(PM_REEL), dimension(:,:) :: data_sure, data_pred
!
!$Arguments
!>E     nb_pred    :<integer>             
!>E     k          :<integer>             
!>E/S   data_sure  :<PM_REEL,DIM=(:,:)>   
!>E/S   data_pred  :<PM_REEL,DIM=(:,:)>   
!>S     I1         :<LEN=1>               
!>S     I2         :<LEN=1>               
!>S     I3         :<LEN=1>               
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
    ! arguments
    integer, intent(in) :: nb_pred, k
    character(LEN=1), intent(out) :: I1, I2, I3
    real(PM_REEL), dimension(:,:), intent(inout) :: data_sure, data_pred

    ! variables locales
    integer :: j, res
    real(PM_REEL) :: zero=0._pm_reel
    
    do j=1, nb_pred
       res = cpsi_compareReels(data_sure(k,1), data_pred(j,1))
       if (res.eq.0) then
          ! PM_x et PM_y
          res = cpsi_compareReels(data_pred(j,4), zero)
          if (res.eq.0) then
             ! donnee sure
             I1 = 'I'
          else
             ! prediction
             I1 = 'P'
             data_sure(k,4) = data_pred(j,4)
             data_sure(k,5) = data_pred(j,5)
             data_sure(k,6) = data_pred(j,6)
             data_sure(k,7) = data_pred(j,7)
          end if
          ! UTC-UT1
          res = cpsi_compareReels(data_pred(j,2), zero)
          if (res.eq.0) then
             ! donnee sure
             I2 = 'I'
          else
             ! prediction
             I2 = 'P'
             data_sure(k,2) = data_pred(j,2)
             data_sure(k,3) = data_pred(j,3)
          end if
          ! dpsi et deps
          res = cpsi_compareReels(data_pred(j,8), zero)
          if (res.eq.0) then
             ! donnee sure
             I3 = 'I'
          else
             ! prediction
             I3 = 'P'
             data_sure(k,8) = data_pred(j,8)
             data_sure(k,9) = data_pred(j,9)
             data_sure(k,10) = data_pred(j,10)
             data_sure(k,11) = data_pred(j,11)
             data_sure(k,12) = data_pred(j,12)
             data_sure(k,13) = data_pred(j,13)
             data_sure(k,14) = data_pred(j,14)
             data_sure(k,15) = data_pred(j,15)
          end if
          exit
       else
          I1 = 'I'
          I2 = 'I'
          I3 = 'I'
       end if
    end do
    
  end subroutine cpsi_determinerFlags


  function cpsi_getBullAPremiereDate() result(date)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getBullAPremiereDate
!
!$Resume
!
!$Description
!  Retourne la premiere date de données disponibles issues du bulletin A.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  date = cpsi_getBullAPremiereDate()
!.    real(KIND=PM_REEL) :: date
!
!$Arguments
!>S     date  :<PM_REEL>   première date disponible pour les données issues d'un bulletin A (unité MJD)
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
    ! resultat
    real(KIND=PM_REEL) :: date
    
    ! variables locales
    integer :: acces_fic, acces, ret, nature
    character(LEN=256) :: nom_fic, libelle
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()

    ! initialisation
    call cps_getListFichiersCateg(CPS_CATEG_BULLA_SURE, fichiers)
    nom_fic = ""
    
    ! les fichiers d'index sont classes chronologiquement
    ! on prend le premier
    call cpsi_getAccesMadona(trim(fichiers(1)), acces)
    ! recheche du premier fichier de données du premier
    ! fichier d'index
    ret = acc_scan_reset(acces)
    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.0) then
          exit
       else if (nature.eq.ACC_STRUCT) then
          ! extraction du nom du fichier
          ret = acc_gets(acces, trim(libelle)//".fichier", nom_fic)
          if (ret>=0) then
             nom_fic = trim(rep_base_ref)//"/"//trim(nom_fic)
             exit
          endif
       end if
    end do
    
    ! ouverture du fichier de données
    if (trim(nom_fic).ne."") then
       call cpsi_getAccesMadona(trim(nom_fic), acces_fic)
       ! recherche de la premiere structure
       ret = acc_scan_reset(acces_fic)
       do
          ret = acc_scan(acces_fic, libelle, nature)
          if (nature.eq.0) then
             exit
          else
             ret = acc_getd(acces_fic, trim(libelle)//".date", date, "")
             exit
          end if
       end do
    end if

    if (associated(fichiers)) then
       deallocate(fichiers)
    end if
    
  end function cpsi_getBullAPremiereDate

end module ui_bullA
