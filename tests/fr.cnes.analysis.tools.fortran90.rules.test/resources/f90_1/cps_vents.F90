module cps_vent

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_vent
!
!$Resume
!   Routines concernant les modèles de vents
!
!$Description
!   Ce module regroupe les routines concernant les modèles de vent.
!
!$Auteur
!   Florence Vivarès (ATOS Origin)
!
!$Version
!  $Id: cps_vents.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_vents.F90,v $
!  Revision 1.8  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.7  2008/10/28 13:06:07  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!
!  Revision 1.6  2007/11/13 16:52:06  sbd
!  FA-ID 827 suppression variables inutilisees
!
!  Revision 1.5  2007/02/01 16:37:20  vivaresf
!  DM-ID 643 : mise à jour des commentaires
!
!  Revision 1.4  2007/01/25 08:48:31  vivaresf
!  DM-ID 643 : validation avec modèles fournis par le CNES
!  - rajout des modèles dans la base locale de test
!  - nommage des formats ATM/ARPEGE
!
!  Revision 1.3  2007/01/18 17:24:29  vivaresf
!  DM-ID 643 : tests avec accès de type interface GSLIB
!  Revision 1.2  2007/01/18 16:30:12  vivaresf
!  DM-ID 643 : récupération des modèles dans la base
!  conversion des altitude de hm en m
!  cartouches d'entete
!
!$FinHistorique
!
!$Usage
!  use cps_vent
!
!$Routines
!- cps_lireVents_ATM
!- cps_lireVents_ARPEGE
!
!$Module
!#V
!- eph_util
!- cps_utilisateur
!#
!
!$Voir-Aussi
!.  cps_lireVents_ATM cps_lireVents_ARPEGE
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  use eph_util, only : eph_util_ficunit90
  use cps_utilisateur
  use cps_desc

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_vents.F90 69 2012-09-11 08:33:34Z ffsm $'

  
contains

  subroutine cps_lireVents_ATM(fichier, nbvent, alti, vEst, vNord, titre, stitre)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_lireVents_ATM
!
!$Resume
!  Lecture d'un modèle de vent variant en altitude, format ATM
!
!$Description
!  Lecture d'un modèle de vent variant en altitude par paliers
!  les altitudes sont des paliers supérieurs.
!  Format ATM : la direction et vitesse du vent est données par ses
!  coordonnées Est et Nord.
!
!$Auteur
!   Florence Vivarès (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_lireVents_ATM(fichier, nbvent, alti, vEst, vNord, [titre], [stitre])
!.    character(LEN=*) :: fichier
!.    integer :: nbvent
!.    real(KIND=PM_REEL), dimension(:), pointer :: alti
!.    real(KIND=PM_REEL), dimension(:), pointer :: vEst
!.    real(KIND=PM_REEL), dimension(:), pointer :: vNord
!.    character(LEN=*) :: titre
!.    character(LEN=*) :: stitre
!
!$Arguments
!>E     fichier  :<LEN=*>                     Fichier modèle de vent
!>S     nbvent   :<integer>                   Nombre de paliers
!>S   alti     :<PM_REEL,DIM=(:),pointer>   Palier (m)
!>S   vEst     :<PM_REEL,DIM=(:),pointer>   Vitesse Est (m/s)
!>S   vNord    :<PM_REEL,DIM=(:),pointer>   Vitesse Nord (m/s)
!>[S]   titre    :<LEN=*>                     Titre indiqué dans le modèle
!>[S]   stitre   :<LEN=*>                     sous-titre indiqué dans le modèle
!
!$Routines
!- eph_util_ficunit90
!- MSP_signaler_message
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
    implicit none

    ! arguments
    ! nom complet du fichier de vents
    character(LEN=*), intent(in) :: fichier

    ! Sorties
    integer, intent(out) :: nbvent
    real(KIND=PM_REEL), dimension(:), pointer :: alti
    real(KIND=PM_REEL), dimension(:), pointer :: vEst
    real(KIND=PM_REEL), dimension(:), pointer :: vNord
    
    ! Sorties optionnelles
    character(LEN=*), intent(out), optional :: titre
    character(LEN=*), intent(out), optional :: stitre

    ! variables locales
    integer :: file_unit, iostat, nbold, ii
    character(LEN=80) :: fichiertmp


    ! Initialisations
    nbvent = 0
    fichiertmp = ""
    fichiertmp = trim(fichier)

    ! Unité logique libre
    call eph_util_ficunit90(file_unit,iostat) 
    if (iostat.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN",&
            routine="cps_lirevents_ATM", &
            partie_variable=trim(fichiertmp))
       return
    end if
    
    ! Première lecture pour obtenir le nombre de ligne dans le fichier modèle
    open(file_unit, file=fichier, iostat=iostat, status='OLD')
    if (iostat.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN",&
            routine="cps_lirevents_ATM", &
            partie_variable=trim(fichiertmp))
       return
    end if

    ! Lecture des champs de titre si besoin
    if (present(titre)) then
       read(file_unit, '(a100)', iostat=iostat) titre
    else
       read(file_unit, *, iostat=iostat)
    endif
    if (present(titre)) then
       read(file_unit, '(a100)', iostat=iostat) stitre
    else
       read(file_unit, *, iostat=iostat)
    endif

    ! Lecture jusqu'au bout du fichier pour comptage des lignes
    nbvent = -1
    do while (iostat==0)
       read(file_unit, *, iostat=iostat)
       nbvent=nbvent+1
    enddo
    close(file_unit)

    ! Initialisation des tableaux
    nbold = cpsi_size_ptr(alti)
    if (nbvent /= nbold) then
       if(associated(alti)) deallocate(alti)
       allocate(alti(nbvent))
    endif
    alti(:) = 0.0_PM_REEL

    nbold = cpsi_size_ptr(vEst)
    if (nbvent /= nbold) then
       if(associated(vEst)) deallocate(vEst)
       allocate(vEst(nbvent))
    endif
    vEst(:) = 0.0_PM_REEL

    nbold = cpsi_size_ptr(vNord)
    if (nbvent /= nbold) then
       if(associated(vNord)) deallocate(vNord)
       allocate(vNord(nbvent))
    endif
    vNord(:) = 0.0_PM_REEL

    ! lecture et remplissage
    open(file_unit, file=fichier, iostat=iostat, status='OLD')

    ! On ne relit poas les champs de titre
    read(file_unit, *, iostat=iostat)
    read(file_unit, *, iostat=iostat)

    ! Colonnes de saisie
    do ii=1, nbvent
       read(file_unit, *, iostat=iostat) alti(ii), vEst(ii), vNord(ii)
    enddo
    close(file_unit)

    ! Conversion des altitudes qui sont en hm en m 
    
    alti(:)=alti(:)*100_PM_REEL

  end subroutine cps_lireVents_ATM

  subroutine cps_lireVents_ARPEGE(fichier, nbvent, alti, vit, dir)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_lireVents_ARPEGE
!
!$Resume
!  Lecture d'un modèle de vent variant en altitude, format ARPEGE
!
!$Description
!  Lecture d'un modèle de vent variant en altitude par paliers
!  Format ARPEGE : la direction du vent est donnée par un angle
!  par rapport à l'axe Sud-Nord (###AC)
!
!$Auteur
!   Florence Vivarès (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_lireVents_ARPEGE(fichier, nbvent, alti, vit, dir)
!.    character(LEN=*) :: fichier
!.    integer :: nbvent
!.    real(KIND=PM_REEL), dimension(:), pointer :: alti, vit, dir
!
!$Arguments
!>E     fichier  :<LEN=*>                     Fichier modèle de vent
!>S     nbvent   :<integer>                   Nombre de paliers
!>S   alti     :<PM_REEL,DIM=(:),pointer>   Palier (m)
!>S   vit      :<PM_REEL,DIM=(:),pointer>   Vitesse (m/s)
!>S   dir      :<PM_REEL,DIM=(:),pointer>   direction (deg)
!
!$Routines
!- eph_util_ficunit90
!- MSP_signaler_message
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
    implicit none

    ! arguments
    ! nom complet du fichier de vents
    character(LEN=*), intent(in) :: fichier

    ! Sorties
    integer, intent(out) :: nbvent
    real(KIND=PM_REEL), dimension(:), pointer :: alti, vit, dir
    
    ! variables locales
    integer :: file_unit, iostat, nbold, ii
    character(LEN=80) :: fichiertmp


    ! Initialisations
    nbvent = 0
    fichiertmp = ""
    fichiertmp = trim(fichier)

    ! Unité logique libre
    call eph_util_ficunit90(file_unit,iostat) 
    if (iostat.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN",&
            routine="cps_lirevents_ARPEGE", &
            partie_variable=trim(fichiertmp))
       return
    end if
    
    ! Première lecture pour obtenir le nombre de ligne dans le fichier modèle
    open(file_unit, file=fichier, iostat=iostat, status='OLD')
    if (iostat.NE.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN",&
            routine="cps_lirevents_ARPEGE", &
            partie_variable=trim(fichiertmp))
       return
    end if

    ! Lecture jusqu'au bout du fichier pour comptage des lignes
    nbvent = -1
    do while (iostat==0)
       read(file_unit, *, iostat=iostat)
       nbvent=nbvent+1
    enddo
    close(file_unit)

    ! Initialisation des tableaux
    nbold = cpsi_size_ptr(alti)
    if (nbvent /= nbold) then
       if(associated(alti)) deallocate(alti)
       allocate(alti(nbvent))
    endif
    alti(:) = 0.0_PM_REEL

    nbold = cpsi_size_ptr(vit)
    if (nbvent /= nbold) then
       if(associated(vit)) deallocate(vit)
       allocate(vit(nbvent))
    endif
    vit(:) = 0.0_PM_REEL

    nbold = cpsi_size_ptr(dir)
    if (nbvent /= nbold) then
       if(associated(dir)) deallocate(dir)
       allocate(dir(nbvent))
    endif
    dir(:) = 0.0_PM_REEL

    ! lecture et remplissage
    open(file_unit, file=fichier, iostat=iostat, status='OLD')

    ! Colonnes de saisie
    do ii=1, nbvent
       read(file_unit, *, iostat=iostat) alti(ii), dir(ii), vit(ii)
    enddo
    close(file_unit)

    ! Conversion de la direction du vent en radian
    dir(:)=dir(:)*PM_PI/180_PM_REEL
    
  end subroutine cps_lireVents_ARPEGE

end module cps_vent
