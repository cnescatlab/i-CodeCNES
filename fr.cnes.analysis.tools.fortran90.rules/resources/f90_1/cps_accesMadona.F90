module cps_accesMadona

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_accesMadona
!
!$Resume
!
!$Description
!  Ce module gère les accès MADONA ur les fichiers de la base par
!  la bibliothèque COMPAS.
!
!$Auteur
!  vpg
!
!$Version
!  $Id: cps_accesMadona.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_accesMadona.F90,v $
!  Revision 1.14  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.13  2008/08/04 12:57:16  gss
!  DM-ID 1058 : (portage g95) ajout de la gestion d'erreur lors de la fermeture
!  des accès aux fichiers madona.
!
!  Revision 1.12  2007/05/21 06:56:18  vivaresf
!  Version 2.2 : révision des cartouches
!
!  Revision 1.11  2006/11/20 17:27:34  vivaresf
!  Version 2-1 : métriques understand, déclarations obsolètes
!  Revision 1.10  2006/05/30 15:20:00  vivaresf
!  regle de codage : suppression de *(*) obsolete
!  Revision 1.9  2006/05/30 12:29:01  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.8  2006/05/30 08:27:45  vivaresf
!  DM-ID 387 : entete MADONA
!  suppression des codes commentés
!  commentaires vrais sur le code
!  Revision 1.7  2006/05/15 15:06:36  vpg
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.6  2006/03/20 15:52:42  vpg
!  Mise a jour des cartouches
!  Revision 1.5  2006/01/23 14:05:13  vpg
!  rajout des traitements MAGE, rajout de fonctions liees aux ihm
!  Revision 1.4  2005/12/08 18:20:26  vivaresf
!  Cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_accesMadona
!
!$Structure
!
!: cpsi_acces : 
!>     fichier     : <LEN=??>   
!>     acces       : <integer>  
!>     frequence   : <integer>  
!
!$Global
!
!>  tab_acces              : <cpsi_acces,DIM=(:)>       
!#V
!>  nb_max_acces_madona    : <integer,private>          
!>  fichier_defaut         : <LEN=4,parameter,private>  
!>  ret                    : <integer,private>          
!>  cps_accesMadona_init   : <logical,private>          
!#
!$Common
!
!$Routines
!- cpsi_init_accesMadona
!- cpsi_getAccesMadona
!- cpsi_close_accesMadona
!
!$Fonctions
!- cpsi_getFichierAcces
!
!$Include
!
!$Module
!#V
!- cps_constantes
!- msp_gestion_erreur
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
!.  cpsi_getFichierAcces cpsi_init_accesMadona cpsi_getAccesMadona cpsi_close_accesMadona
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use cps_constantes
  use msp_gestion_erreur

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_accesMadona.F90 69 2012-09-11 08:33:34Z ffsm $'


  integer, private :: nb_max_acces_madona
  character(len=4), parameter, private :: fichier_defaut = 'NULL'
  integer, private :: ret
  logical, private, save :: cps_accesMadona_init = .false.

  type cpsi_acces
	character(len=CPS_MAXLG) :: fichier
	integer :: acces
	integer(kind=4) :: frequence
  end type cpsi_acces

  type (cpsi_acces), dimension(:), allocatable :: tab_acces

contains

  subroutine cpsi_init_accesMadona(n)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_init_accesMadona
!
!$Resume
!
!$Description
!  Routine d'initialisation du module.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_init_accesMadona(n)
!.    integer :: n
!
!$Arguments
!>E     n  :<integer>   nombre maximal d'accès MADONA pouvant être ouverts en même temps
!
!$Common
!
!$Routines
!- cpsi_close_accesMadona
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
    ! argument
    integer, intent(in) :: n

    ! variable locale
    integer :: i
    
    if (cps_accesMadona_init) then
       call cpsi_close_accesMadona
    end if
    
    nb_max_acces_madona = n
    allocate (tab_acces(nb_max_acces_madona))
    do i=1, nb_max_acces_madona
       tab_acces(i) = cpsi_acces(fichier_defaut, 0, 0)
    end do
    
    cps_accesMadona_init = .true.
    
  end subroutine cpsi_init_accesMadona
  

  subroutine cpsi_getAccesMadona(nom_fichier, acces)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getAccesMadona
!
!$Resume
!
!$Description
!  Routine qui permet d'obtenir un accès MADONA sur un fichier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_getAccesMadona(nom_fichier, acces)
!.    character(len=*) :: nom_fichier
!.    integer :: acces
!
!$Arguments
!>E     nom_fichier  :<LEN=*>     nom du fichier à ouvrir 
!>S     acces        :<integer>   accès MADONA sur le fichier
!
!$Common
!
!$Routines
!- MSP_signaler_message
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
    ! arguments
    character(len=*), intent(in) :: nom_fichier
    integer, intent(out) :: acces
    
    ! variables locales
    integer :: i
    integer :: index_min = 0
    integer(kind=4) :: freq_min = 2147483647
    integer :: trouve = 0

    if (.not.cps_accesMadona_init) then
       ! erreur : le module n'a pas ete initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getAccesMadona", &
            partie_variable="cps_accesMadona")
       return
    end if
    
    trouve = 0
    do i=1, nb_max_acces_madona
       if (trouve==0 .and. tab_acces(i)%fichier.eq.nom_fichier) then
          acces = tab_acces(i)%acces
          tab_acces(i)%frequence = tab_acces(i)%frequence + 1
          trouve = 1
       elseif (trouve==0 .and. tab_acces(i)%fichier.eq.fichier_defaut) then
          tab_acces(i)%acces = acc_load(trim(nom_fichier))
          if ( tab_acces(i)%acces.LT.0) then
             ! erreur a l'ouverture du fichier
             call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
                  routine="cpsi_getAccesMadona", &
                  partie_variable=trim(nom_fichier))
             return
          end if
          tab_acces(i)%fichier = nom_fichier
          tab_acces(i)%frequence = 1
          acces = tab_acces(i)%acces
          trouve = 1
       elseif (tab_acces(i)%fichier.ne.fichier_defaut) then
          tab_acces(i)%frequence = tab_acces(i)%frequence - 1
          if (tab_acces(i)%frequence.lt.freq_min) then
             freq_min = tab_acces(i)%frequence
             index_min = i
          end if
       end if
    end do
    
    if (trouve == 0) then
       if (tab_acces(index_min)%acces.GE.0) then
          ret = acc_close(tab_acces(index_min)%acces)
          if (ret<0) then
             call MSP_signaler_message(cle_mes="CPS_ERR_CLOSE", &
                  routine="cpsi_getAccesMadona",                &
                  partie_variable=trim(tab_acces(index_min)%fichier))
          endif
       end if
       tab_acces(index_min)%acces = acc_load(trim(nom_fichier))
       if (tab_acces(index_min)%acces.LT.0) then
          ! erreur a l'ouverture du fichier
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getAccesMadona", &
               partie_variable=trim(nom_fichier))
          return
       end if
       tab_acces(index_min)%fichier = nom_fichier
       tab_acces(index_min)%frequence = 1
       acces = tab_acces(index_min)%acces
    end if

  end subroutine cpsi_getAccesMadona

  
  function cpsi_getFichierAcces(acces, nom_fichier) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getFichierAcces
!
!$Resume
!
!$Description
!  Fonction qui retourne le nom du fichier correspondant a l'acces.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getFichierAcces(acces, nom_fichier)
!.    integer :: acces
!.    character(len=*) :: nom_fichier
!.    integer :: trouve
!
!$Arguments
!>E     acces        :<integer>   accès MADONA dont on souhaite obtenir le fichier
!>S     nom_fichier  :<LEN=*>     nom du fichier
!>S     trouve       :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
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
    ! arguments
    integer, intent(in) :: acces
    character(len=*), intent(out) :: nom_fichier
    
    ! resultat
    integer :: trouve

    ! variable locale
    integer :: i

    trouve = CPS_ERR_DEF
    i = 1

    if (.not.cps_accesMadona_init) then
       ! erreur : le module n'a pas ete initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getFichierAcces", &
            partie_variable="cps_accesMadona")
       return
    end if

    do
       if (tab_acces(i)%acces.eq.acces) then
          nom_fichier = tab_acces(i)%fichier
          trouve = CPS_OK
          exit
       else 
          i = i+1
          if (i.gt.nb_max_acces_madona) then
             ! erreur : pas de fichier associe a l'acces
             call MSP_signaler_message(cle_mes="CPS_ERR_ACCES", &
                  routine="cpsi_getFichierAcces", &
                  partie_variable=trim(cpsi_intToChar(acces)))
             exit
          end if
       end if
    end do
       
  end function cpsi_getFichierAcces


  subroutine cpsi_close_accesMadona()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_close_accesMadona
!
!$Resume
!
!$Description
!  Routine de fermeture du module.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_close_accesMadona()
!
!$Arguments
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
    ! variable locale
    integer :: i, iostat

    if (cps_accesMadona_init) then
       do i=1, nb_max_acces_madona
          if (tab_acces(i)%fichier.ne.fichier_defaut) then
             ret = acc_close(tab_acces(i)%acces)
             if (ret<0) then
                call MSP_signaler_message(cle_mes="CPS_ERR_CLOSE", &
                     routine="cpsi_close_ccesMadona",                &
                     partie_variable=trim(tab_acces(i)%fichier))
             endif
          end if
       end do
       deallocate (tab_acces, stat=iostat)
       cps_accesMadona_init = .false.
    end if
  end subroutine cpsi_close_accesMadona



end module cps_accesMadona
