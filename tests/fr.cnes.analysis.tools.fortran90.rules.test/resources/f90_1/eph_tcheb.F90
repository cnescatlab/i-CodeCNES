module eph_tcheb

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_tcheb
!
!$Resume
!  Module des sous-programmes relatifs à la méthode Tchebychev.
!
!$Description
!  Module contenant les sous-programmes d'initialisation, de fermeture,
!  de calcul ainsi que les variables globales pour la méthode de 
!  Tchebychev
!
!$Auteur
! Philippe Brémard / Florence Vivares (SchlumbergerSema)
!
!$Version
!  $Id: eph_tcheb.F90 379 2013-02-22 16:59:49Z ffsm $
!
!$Historique
!  $Log: eph_tcheb.F90,v $
!  Revision 379  2013/02/22 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 158 2012/11/27 Fran Simarro/FFSM - GMV
!  Fermeture de code source pour la livraison BIBMS 1.13 (dec 2012)
!
!  Revision 147 2012/11/20 iils
!  DM-ID PALMES 192: Portage DEVIL sur Linux. L'attribute SAVE doit desormais etre explicit
!
!  Revision 1.14  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.13  2009/08/26 15:07:24  cml
!  AQ : Suppression de code commenté et amélioration message d'erreur
!
!  Revision 1.12  2008/10/03 13:35:15  cml
!  FA-ID 1024 : Remplacement de la variable index par indice
!
!  Revision 1.11  2008/08/04 13:33:31  gss
!  DM-ID 1058 : (portage g95) suppression des variables non utilisées et suppression
!  des labels (format) non utilisés.
!
!  Revision 1.10  2007/11/05 12:40:41  jpi
!  DM-ID551 : coquilles (apostrophes, print inutiles)
!
!  Revision 1.9  2006/05/31 13:07:27  vivaresf
!  version 2.0 : mise au point COMPAS/COMPAS_UI
!
!  Revision 1.8  2006/05/30 08:21:37  vivaresf
!  Informations brutes en option
!
!  Revision 1.7  2006/04/26 11:54:17  bouillaj
!  FA-ID 371 : Modification de la fabrication de certains noms de fichiers pour eviter les blancs
!
!  Revision 1.6  2006/04/10 09:32:10  vpg
!  Ajout de la compatibilite ascendante du format du code du repere : il est possible de lire une chaine de caracteres et le code a trois chiffre
!
!  Revision 1.5  2006/04/06 16:02:18  vpg
!  Mise a jour de la lecture du code et de la date de reference du repere
!
!  Revision 1.4  2006/03/09 12:04:40  vivaresf
!  FA-ID 500 : trace de la DM 391
!
!  Revision 1.3  2005/12/09 08:38:10  vivaresf
!  suppression de codes commentés
!
!  Revision 1.2  2005/12/08 18:39:05  vivaresf
!  Cartouches et vérification des déclarations
!
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.8  2005/11/07 15:56:25  bouillaj
!  DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!  Revision 1.7  2005/10/13 10:38:10  bouillaj
!  FA-ID 371 : Cloture du FT (Corrections dans libephem pour passage linux)
!  Revision 1.6.2.1  2005/10/13 10:29:46  bouillaj
!  FA-ID 371 : Version initiale du FT (Corrections dans libephem pour passage linux)
!  Revision 1.6  2005/05/09 14:17:32  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.5  2005/03/09 09:12:05  vivaresf
!  Correction des cartouches
!  Revision 1.4  2004/12/17 14:58:11  vivaresf
!  Documentation
!  Revision 1.3  2004/06/02 15:17:02  bremard
!  AQ_158
!  Revision 1.2.4.1  2004/06/02 14:10:48  bremard
!  PhB - Ajout de la subroutine eph_rep_frame_dico + restauration de numframe au lieu de nomframe en interface
!  Revision 1.2  2004/05/25 13:16:03  vivaresf
!  DM_158
!  Revision 1.1.1.1.2.1  2004/05/24 16:37:52  vivaresf
!  Suppression de l'appel a AM_rep_frame_dico et
!  remplacement du code repere (inutilise) par le nom du repere
!  Utilisation de eph_infoinit pour la lecture des ressources
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.21  2004/01/13 09:16:26  bremard
!  Mise à jour cartouche
!  Revision 1.18  2004/01/07 16:14:33  bremard
!  Mise à jour cartouche + AM_date_carjul50 remplacé par eph_cdatejj50
!  Revision 1.17  2003/12/30 16:06:56  bremard
!  Remplacement AM_math_tchapp par eph_math_tchapp + Définition de eph_math_tchapp
!  Revision 1.16  2003/06/06 14:52:41  bremard
!  Particularisation des parametres
!  Revision 1.15  2002/10/16 13:27:41  vivaresf
!  PhB - AM_util_ficunit90 changé en eph_util_ficunit
!  Revision 1.11  2001/12/20 08:41:50  bremard
!  PhB - Declaration ii pour boucle d'init pv1, pv2
!  Revision 1.10  2001/12/18 17:42:21  vivaresf
!  boucle au lieu de l'initialisation directe
!  Revision 1.9  2001/12/18 17:21:26  vivaresf
!  Suppression initialisation
!  Revision 1.8  2001/12/18 17:04:38  vivaresf
!  Renommage des variables corpsc, numframe et tframe
!  Revision 1.7  2001/12/18 16:12:25  vivaresf
!  possibilite d'avoir tous les corps en corps central
!  Revision 1.6  2001/12/17 17:40:32  vivaresf
!  Num. de corps vrai
!  Revision 1.5  2001/12/13 09:28:32  vivaresf
!  Correction du raz tcheb (0 au lieu de 1)
!  Revision 1.4  2001/12/07 16:45:26  vivaresf
!  Presentation fm des cartouches
!  Revision 1.3  2001/12/05 15:20:46  bremard
!  PhB - Mise à jour cartouche
!  Revision 1.2  2001/12/04 10:39:29  bremard
!  PhB - Mise à jour cartouche + mise en forme du code
!
!$FinHistorique
!
!$Usage
!  use eph_tcheb
!
!$Global
!
!>  inittche       : <logical>  =.true.: la méthode a été initialisée
!>  corpsctche     : <integer>  corps central
!>  numframetche   : <integer>  code repère
!>  tframetche     : <PM_REEL>  date de définition du repère de Tchebychev
!$Remarques
!
!$Mots-cles
!   éphémérides, polynômes de Tchebytchev
!
!$Voir-Aussi
!.  eph_inittche eph_closetche eph_pvtcheb eph_ephemtche eph_lentfic_pla eph_util_lecbuf eph_math_tchapp
!.  eph_rep_frame_dico
!
!$Routines
!- eph_inittche
!- eph_closetche
!- eph_pvtcheb
!- eph_ephemtche
!- eph_lentfic_pla
!- eph_util_lecbuf
!- eph_math_tchapp
!- eph_rep_frame_dico
!
!$Module
!#V
!- msp_gestion_erreur
!- eph_util
!- eph_constantes
!- eph_info
!- eph_varglob
!#
!
!$Interface
!#V
!#
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use msp_gestion_erreur
      use eph_util
      use eph_constantes
      use eph_info
      use eph_varglob, only : fictche, ephv_lfntche, reset_tcheb

      implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_tcheb.F90 379 2013-02-22 16:59:49Z ffsm $'


      ! methode Tchebytchef
      logical :: inittche=.false.                 ! initialisée ou non
      integer :: corpsctche                       ! corps central
      integer :: numframetche                     ! code repere pour Tchebychev
      real(KIND=PM_REEL)   :: tframetche          ! date de reference du repere

contains

  subroutine eph_inittche(nomfic)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_inittche
!
!$Resume
!   Initialisation du calcul d'éphémérides par polynômes de Tchebytchev
!
!$Description
!   Ouverture du fichier de polynômes de Tchebychev ou 
!   repositionnement en début de fichier
!
!$Auteur
!    Philippe Brémard (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Routines
!- eph_util_ficunit90
!- MSP_signaler_message
!- eph_lentfic_pla
!
!$Usage
!  call eph_inittche(nomfic)
!.    character(len=*) :: nomfic
!
!$Arguments
!>E     nomfic  :<LEN=*>   nom avec pathname complet du fichier de polynômes de 
!                        Tchebychev
!
!$Remarques
!
!$Mots-cles
!   éphémérides, Tchebychev
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    ! Arguments
    character(len=*), intent(in) :: nomfic

    ! Variables locales
    integer :: ier
    integer :: tmp0, tmp1, tmp4
    integer, dimension(15) :: tmp2
    real(kind=PM_REEL) :: tmp3,tmp5, tmp6

    ! chargement uniquement si le fichier est différent du fichier courant

    if ( (trim(nomfic)/=trim(fictche) &
                     .or. len_trim(nomfic)/=len_trim(fictche) )   &
                     .and. nomfic/="" ) then
       if (ephv_lfntche==0) then
         ! réserver un lfn pour fichiers Tcheb.
         call eph_util_ficunit90(ephv_lfntche,ier)
         if (ier < 0) then
            call MSP_signaler_message(cle_mes="EPH_ERR_SYST", &
                       partie_variable="reservation d'un lfn impossible", &
                       routine="eph_inittche" )
            return
         endif
       else
          close(ephv_lfntche, iostat=ier)
       endif

#ifdef __GFORTRAN__
       open(ephv_lfntche,recl=16606,file=nomfic,blank='NULL', form='FORMATTED', &
            access='SEQUENTIAL', convert='big_endian', iostat=ier)
#else
       open(ephv_lfntche,recl=16606,file=nomfic,blank='NULL', form='FORMATTED', &
            access='SEQUENTIAL', iostat=ier)
#endif


       if (ier < 0) then
         call MSP_signaler_message (cle_mes="EPH_ERR_OPEN", &
              partie_variable=trim(nomfic), &
              routine="eph_inittche")
         return  
       endif

       if (ier/=0) then
         call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
              partie_variable=trim(nomfic), &
              routine="eph_inittche")
         return
       endif

       reset_tcheb = 0

       ! méthode initialisée
       if(.not.MSP_ERREUR) then 
          fictche=nomfic
          inittche=.true.
       endif

       ! on recupere le code du corps central, le code et la date du repere
       call eph_lentfic_pla(ephv_lfntche, tmp0, corpsctche, numframetche,tframetche, &
            tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)

    elseif (trim(nomfic)==trim(fictche) &
            .and. len_trim(nomfic)==len_trim(fictche)) then
       ! Initialisation demandée avec le même fichier: le pointeur est 
       ! repositionné en début de fichier  
       reset_tcheb = 0

    else
       call MSP_signaler_message (cle_mes="EPH_ERR_INITABS", &
            partie_variable="tchebychev", &
            routine="eph_inittche")
       return
    endif

  endsubroutine eph_inittche

   subroutine eph_closetche()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_closetche
!
!$Resume
!    Fin d'utilisation de la méthode Tchebychev (fermeture du fichier)
!
!$Description
!    Fermeture du fichier contenant les coefficients de Tchebychev
!
!$Auteur
!    Philippe Brémard (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_closetche()
!
!$Remarques
!
!$Mots-cles
!   éphémérides, Tchebytchev
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     implicit none

        if(inittche) then 
           fictche=""
           ! méthode non initialisée
           inittche=.false.
           close(ephv_lfntche)
           corpsctche=0
           numframetche=0
           tframetche=0._PM_REEL
        endif

      endsubroutine eph_closetche

      subroutine eph_pvtcheb(ncorpsc, npla, date, pv)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_pvtcheb
!
!$Resume
!  Calcul d'éphémérides avec une méthode utilisant les polynômes de Tchebychev
!
!$Description
!  Calcul d'éphémérides avec une méthode utilisant les polynômes de Tchebychev
!
!$Auteur
!  Philippe Brémard / Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_pvtcheb(ncorpsc, npla, date, pv)
!.    integer :: ncorpsc, npla
!.    real(kind=PM_REEL) :: date
!.    real(kind=PM_REEL), dimension(6) :: pv
!
!$Arguments
!>E     ncorpsc  :<integer>           numéro du corps central
!>E     npla     :<integer>           numéro du corps d'intérêt
!>E     date     :<PM_REEL>           date
!>S     pv       :<PM_REEL,DIM=(6)>   coordonnées cartésiennes (positions/vitesses)
!
!$Remarques
!   On ne peut manipuler qu'un seul fichier Tchebychev à la fois.
!
!$Mots-cles
!   éphémérides, Tchebychev
!
!$Voir-Aussi
!
!$Routines
!- MSP_signaler_message
!- eph_ephemtche
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        implicit none

        integer, intent(in) :: ncorpsc, npla
        real(kind=PM_REEL), intent(in) :: date
        real(kind=PM_REEL), dimension(6), intent(out) :: pv

        ! Variables locales
        integer :: ii
        real(kind=PM_REEL), dimension(6) :: pv1, pv2

        ! méthode initialisée ?
        if (.not.inittche) then
           call MSP_signaler_message(cle_mes="EPH_ERR_INITABS", &
                partie_variable=" Tchebytchev", & 
                routine="eph_pvtcheb")
           return
        endif

        ! initialisation de pv1 et pv2
        do ii=1,6 
           pv1(ii)=0._PM_REEL
           pv2(ii)=0._PM_REEL
        end do

        ! Calcul 
        if (corpsctche/=ncorpsc) call eph_ephemtche(ephv_lfntche,date,corpsctche,ncorpsc,pv1)
        if (MSP_PROBLEME) then
           call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
                partie_variable=" eph_ephemtche", & 
                routine="eph_pvtcheb")
        endif

        if (corpsctche/=npla) call eph_ephemtche(ephv_lfntche,date,corpsctche,npla,pv2)
        if (MSP_PROBLEME) then
           call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
                partie_variable=" eph_ephemtche", & 
                routine="eph_pvtcheb")
        endif

        ! coordonnées corps d'interet moins coordonnées corps central
        pv(1:6) = pv2(1:6)-pv1(1:6)

        return
      end subroutine eph_pvtcheb


      subroutine eph_ephemtche(lfn,tcalcul,numcorps,numpla,xpla)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_ephemtche
!
!$Resume
!  Calcul des éphémérides d une planète à partir d un fichier de Tchebychev
!
!$Description
!  On lit dans un fichier les coefficients de Tchebychev relatifs \
!  à la position permettant d approximer les positions. \
!  Les vitesses se déduisent alors en s aidant des propriétés des \
!  polynômes de Tchebychev.
!
!$Auteur
!  Philippe Brémard / Florence VIVARES (SchlumbergerSema)
!  à partir de EI_ephemtche
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_ephemtche(lfn,tcalcul,numcorps,numpla,xpla)
!.    integer :: lfn
!.    real(KIND=PM_REEL) :: tcalcul
!.    integer :: numcorps
!.    integer :: numpla
!.    real(KIND=PM_REEL),dimension(6) :: xpla(6)
!
!$Arguments
!>E     lfn       :<integer>                   unité logique du fichier 
!>E     tcalcul   :<PM_REEL>                   date 1950.0 CNES de calcul des éphémérides
!                                   (en échelle TE)
!>E     numcorps  :<integer>                   numéro de la planète (sur 10 caractères)
!>E     numpla    :<integer>                   numéro du corps central (numérotation CNES)
!>S     xpla      :<PM_REEL,DIM=(6),DIM=(6)>   positions et vitesses calculées
!
!$Remarques
!
!$Mots-cles
!   éphémérides, Tchebychev
!
!$Voir-Aussi
!
!$Routines
!- eph_lentfic_pla
!- MSP_signaler_message
!- eph_util_lecbuf
!- eph_math_tchapp
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

! Arguments
      integer,                        intent(in)  :: lfn
      real(KIND=PM_REEL),             intent(in)  :: tcalcul
      integer,                        intent(in)  :: numcorps
      integer,                        intent(in)  :: numpla
      real(KIND=PM_REEL),dimension(6),intent(out) :: xpla(6)


!	COMMONS

! Variables locales
      integer, parameter :: NBPLANETMX = 15

      integer, parameter :: NPARAM     = 3

      integer, parameter :: NCOEFFMX   = NPARAM*NBPLANETMX*16

      integer, save      :: ifois      = 0

! Dates de debut/fin de l'intervalle
      real(KIND=PM_REEL),save :: t1 = 999._PM_REEL
      real(KIND=PM_REEL),save :: t2 = -999._PM_REEL

! Variables liees a la lecture du fichier
      integer, save            :: lentetel, numplacl, numframe
      integer, save            :: nbplanetl, ndegl
      integer, dimension(NBPLANETMX), save :: numplal
      real(KIND=PM_REEL), save :: tframel, dureejl, tdebl,tfinl
      
! Variables lues du fichier
      real(KIND=PM_REEL), dimension(NCOEFFMX), save :: coeff
      integer, save  :: ncoeff

      integer :: indice
      integer :: iplanet
      integer :: ier_appel
      logical :: trouve

! INITIALISATION

! CORPS DU SOUS-PROGRAMME <eph_ephemtche>

      if (reset_tcheb == 0)then
         ifois = 0
         rewind(lfn)
         t1 =  999.d0
         t2 = -999.d0
      endif

      if (ifois == 0) then

!-----------------------------------------------------------------------
!	Lecture de l'Entête
!       (A la première éxécution)
!-----------------------------------------------------------------------

         ifois = 1
         call eph_lentfic_pla(lfn,lentetel,numplacl,numframe,tframel, &
              nbplanetl,numplal,dureejl,ndegl,tdebl,tfinl)
         if (MSP_PROBLEME) then
            call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
                                      partie_variable="eph_lentfic_pla", & 
                                      routine="eph_ephemtche")
            return
         endif

!-----------------------------------------------------------------------
!/     Contrôles
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     Vérification que le numéro du corps central est celui du fichier
!-----------------------------------------------------------------------
         if (numcorps.ne.numplacl) then
            call MSP_signaler_message(cle_mes="EPH_ERR_CODECC", &
                                      routine="eph_ephemtche")
            return
         endif

!-----------------------------------------------------------------------
!     Vérification que le nombre de planètes est cohérent
!-----------------------------------------------------------------------
         if (nbplanetl.gt.NBPLANETMX) then
            call MSP_signaler_message(cle_mes="EPH_ERR_NBPLASUP", &
                                      routine="eph_ephemtche")
            return
         endif

!-----------------------------------------------------------------------
!     Vérification que la planète demandée appartient au fichier
!-----------------------------------------------------------------------
         iplanet = 1
         trouve = .false.
         do while ((iplanet <= nbplanetl) .and. .not.trouve)
            if (numpla == numplal(iplanet)) then
               trouve = .true.
            else
               iplanet = iplanet + 1
            endif
         enddo
         
         if (.not. trouve) then
            call MSP_signaler_message(cle_mes="EPH_ERR_PLANOTINFIC", &
                                      routine="eph_ephemtche")
            return
         endif

!-----------------------------------------------------------------------
!     Calcul de la longueur d'un buffer
!     (Nombre de coefficients de Tchebychev à lire) 
!-----------------------------------------------------------------------
         ncoeff = NPARAM*(ndegl+1)*nbplanetl
         if (ncoeff > NCOEFFMX) then
            call MSP_signaler_message(cle_mes="EPH_ERR_NBCOEFSUP", &
                                      routine="eph_ephemtche")
            return
         endif

      endif ! ifois == 0

!-----------------------------------------------------------------------
!     Récupération des coefficients 
!     (Uniquement si on change d'intervalle.
!     Ceci est géré dans eph_util_lecbuf)
!-----------------------------------------------------------------------

      if ( (tcalcul < t1) .or. (tcalcul > t2) ) then
         call eph_util_lecbuf(lfn,lentetel,tdebl,tfinl,dureejl,ncoeff, &
                              tcalcul,t1,t2,coeff)
         if (MSP_PROBLEME) then
            call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
                                      partie_variable="eph_util_lecbuf", & 
                                      routine="eph_ephemtche")
            return
         endif
      endif

!-----------------------------------------------------------------------
!     Interpolation type Tchebychev
!-----------------------------------------------------------------------
      indice = 1
      iplanet = 1
      trouve = .false.
      do while ((iplanet <= nbplanetl) .and. .not.trouve)
         if (numpla == numplal(iplanet)) then
            indice = (iplanet-1)*NPARAM*(ndegl+1)+1
            trouve = .true.
         else
            iplanet = iplanet + 1 
         endif
      enddo 

      if (.not. trouve) then
         call MSP_signaler_message(cle_mes="EPH_ERR_PLANOTINFIC", &
                                   routine="eph_ephemtche")
         return
      endif
      
      call eph_math_tchapp(t1,t2,tcalcul,coeff(indice),NCOEFFMX,ndegl, &
                          NPARAM,xpla,xpla(4),ier_appel)
      if (ier_appel < 0) then
         call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
                                   partie_variable="eph_math_tchapp", & 
                                   routine="eph_ephemtche")
         return
      endif

      ! Vitesses par secondes et non par jour
      xpla(4) = xpla(4)/86400._PM_REEL
      xpla(5) = xpla(5)/86400._PM_REEL
      xpla(6) = xpla(6)/86400._PM_REEL

      end subroutine eph_ephemtche

      subroutine eph_lentfic_pla(lfn,lentete,numplac,numframe,tframe, &
          nbplanet,numpla,dureej,ndeg,tdeb,tfin, echtout, nomframeout)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_lentfic_pla
!
!$Resume
!  Calcul des éphémérides d'une planète à partir d'un fichier Tchebychev
!
!$Description
!  Lecture entête fichier éphémérides planètes CNES présentées
!  sous forme de coefficients de Tchebychev
!
!$Auteur
!  Philippe Brémard / Florence VIVARES (SchlumbergerSema)
!  à partir de EI_lentfic_pla
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_lentfic_pla(lfn,lentete,numplac,numframe,tframe, &
!.              nbplanet,numpla,dureej,ndeg,tdeb,tfin)
!.    integer :: lfn
!.    integer :: lentete
!.    integer :: numplac
!.    integer :: numframe
!.    real(KIND=PM_REEL) :: tframe
!.    integer :: nbplanet
!.    integer,dimension(NBPLANETMX) :: numpla
!.    real(KIND=PM_REEL) :: dureej
!.    integer :: ndeg
!.    real(KIND=PM_REEL) :: tdeb
!.    real(KIND=PM_REEL) :: tfin
!
!$Arguments
!>E     lfn       :<integer>                    unité logique fichier
!>S     lentete   :<integer>                    longueur du fichier (Nb d'enregistrements)
!>S     numplac   :<integer>                    numéro (CNES) du corps central
!>S     numframe  :<integer>                    nunéro du repère 
!>S     tframe    :<PM_REEL>                    date de définition du repère
!>S     nbplanet  :<integer>                    nombre de planètes
!>S     numpla    :<integer,DIM=(NBPLANETMX)>   liste des numéros des planètes figurant dans le fichier
!>S     dureej    :<PM_REEL>                    intervalle de validité des polynômes
!>S     ndeg      :<integer>                    degré des polynômes de Tchebycheff
!>S     tdeb      :<PM_REEL>                    date de début du fichier
!>S     tfin      :<PM_REEL>                    date de fin de fichier
!
!$Remarques
!
!$Mots-cles
!   éphémérides, Tchebychev
!
!$Voir-Aussi
!
!$Routines
!- eph_infoinit
!- eph_fic2code
!- eph_rep_frame_dico
!- MSP_signaler_message
!- eph_cdatejj50
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none 

! Paramètres
      integer, parameter :: NBPLANETMX = 15

! Arguments 
      integer                       , intent(in)  :: lfn
      integer                       , intent(out) :: lentete
      integer                       , intent(out) :: numplac
      integer                       , intent(out) :: numframe
      real(KIND=PM_REEL)            , intent(out) :: tframe
      integer                       , intent(out) :: nbplanet
      integer,dimension(NBPLANETMX) , intent(out) :: numpla
      real(KIND=PM_REEL)            , intent(out) :: dureej
      integer                       , intent(out) :: ndeg
      real(KIND=PM_REEL)            , intent(out) :: tdeb
      real(KIND=PM_REEL)            , intent(out) :: tfin
      character(LEN=*),     optional, intent(out) :: echtout
      character(LEN=*),     optional, intent(out) :: nomframeout

! Variables locales
      character(LEN=30) :: nomplac
      character(LEN=30) :: ctdeb, ctfin
      character(LEN=30), dimension(NBPLANETMX) :: nompla
      character(LEN=52) :: champframe
      character(LEN=32) :: dateframe, echt,nomframe

      integer :: lchframe,indframe,i
      integer :: iplanet
      integer :: NB_LIGNES_ENTETE = 11  ! Attention, le nombre de lignes de l'entête est codé en dur ici !!
      character (LEN=200) :: ficconf

      integer :: cmul
      logical :: trouve, nouveau_format
!------------------------------------------
! CORPS DU SOUS-PROGRAMME <eph_lentfic_pla>
!------------------------------------------


      if(.not.ephv_acces_open) call eph_infoinit()
      ficconf = trim(ephv_ephempath) // trim(ephv_fichierconf)

!-----------------------------------------------------------------------
!     Lecture brutale de l'entête
!-----------------------------------------------------------------------
      read(lfn,*)
      read(lfn,*)
      read(lfn,110) nomplac
      read(lfn,110) champframe
      read(lfn,150) nbplanet
      read(lfn,110) (nompla(iplanet), iplanet=1,nbplanet)
      read(lfn,140) dureej
      read(lfn,150) ndeg
      read(lfn,110) echt
      read(lfn,120) ctdeb
      read(lfn,120) ctfin
      read(lfn,*)


!-----------------------------------------------------------------------
!     Longueur de l'entête
!-----------------------------------------------------------------------
      lentete = NB_LIGNES_ENTETE + nbplanet

!-----------------------------------------------------------------------
!     Conversion Nom Corps central et Planètes en numéros CNES 
!-----------------------------------------------------------------------

      call eph_fic2code(ficconf, nomplac, code=numplac)

      do iplanet=1,nbplanet
         call eph_fic2code(ficconf, nompla(iplanet),code=numpla(iplanet))
      end do

!------------------------------------------------------------------------------- 
!	Decoupage champframe en nomframe et dateframe 
!-------------------------------------------------------------------------------

      lchframe=len_trim(champframe)
      indframe=lchframe
      trouve = .false.
      i=1
        
      ! DM 387 : integration CREATEPHEM
      do while ((i <= lchframe) .and. .not. trouve) 
         if (champframe(i:i+1) == 'au') then
            indframe=i
            trouve = .true.
         else
            i=i+1
         endif
      enddo
      !read(champframe(1:3),'(I3)') numframe
      
      if (indframe /= lchframe) then
         nomframe  = champframe(1:indframe-2)
         dateframe = champframe(indframe+3:lchframe)
      else
         nomframe  = champframe(1:indframe)
         dateframe = ''
      endif
      !dateframe = champframe(4:)

!-----------------------------------------------------------------------
!     Conversion Nom Repère Ephémérides 
!-----------------------------------------------------------------------
! DM 387 : integration CREATEPHEM
! on regarde le format de nomframe :
! - soit il s'agit d'une chaine de caracteres (ancien format)
! - soit il s'agit du code a trois chiffres (nouveau format)

      ! on regarde si les trois premiers caracteres
      ! de 'nomframe' correspondent a un code a trois
      ! chiffres
      nouveau_format = .true.
      numframe = 0
      cmul = 100
      do i=1,3
         select case (nomframe(i:i))
         case ('1')
            numframe = numframe+1*cmul
         case ('2')
            numframe = numframe+2*cmul
         case ('3')
            numframe = numframe+3*cmul
         case ('4')
            numframe = numframe+4*cmul
         case ('5')
            numframe = numframe+5*cmul
         case ('6')
            numframe = numframe+6*cmul
         case ('7')
            numframe = numframe+7*cmul
         case ('8')
            numframe = numframe+8*cmul
         case ('9')
            numframe = numframe+9*cmul
         case default
            nouveau_format = .false.
         end select
         if (.not.nouveau_format) then
            ! pas de caractere numerique
            ! 'nomframe' est a l'ancien format
            exit
         end if
         cmul = cmul/10
      end do

      if (.not.nouveau_format) then
         ! ancien format
         call eph_rep_frame_dico(nomframe,numframe)
      end if


      if (echt /= 'TE') then
         call MSP_signaler_message(cle_mes="EPH_ERR_ECHTE", &
                                   routine="eph_lentfic_pla")
         return
      endif

!-------------------------------------------------------------------------------
!	date de definition du repere
!-------------------------------------------------------------------------------

      if (indframe /= lchframe) then

         call eph_cdatejj50(dateframe,tframe)
         if (MSP_gen_messages("eph_lentfic_pla")) return
         
      endif

!-----------------------------------------------------------------------
!     Date début du fichier
!-----------------------------------------------------------------------
      call eph_cdatejj50(ctdeb,tdeb)
      if (MSP_gen_messages("eph_lentfic_pla")) return

!-----------------------------------------------------------------------
!     Date fin du fichier
!-----------------------------------------------------------------------
      call eph_cdatejj50(ctfin,tfin)
      if (MSP_gen_messages("eph_lentfic_pla")) return

!
! Sortie optionnelles
!
      if (present(echtout)) echtout=trim(echt)
      if (present(nomframeout)) nomframeout=trim(champframe)


!-----------------------------------------------------------------------
!  FORMATS
!-----------------------------------------------------------------------

 110  format(53x,a)
 120  format(53x,a)
 140  format(53x,d23.16)
 150  format(53x,i3)

      end subroutine eph_lentfic_pla

      subroutine eph_util_lecbuf(lfn,lent,xmin,xmax,pasx,lbuf,x,x1,x2,buf)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_util_lecbuf
!
!$Resume
!  Lecture sur fichiers séquentiels d'un enregistrement
!
!$Description
!     On peut traiter jusqu'à  nblfnmax fichiers (nblfnmax = 10).
!     On suppose que chaque fichier est constitué de la manière suivante :
!.        - partie EN-TETE (de longueur lent),
!.        - suite d'enregistrements relatifs à des intervalles [x1,x2]
!          de longueurs égales et à l'intérieur desquels sont définis
!          des buffers de dimension lbuf.
!          La structure de chaque enregistrement noté i est ainsi 
!          définie :
!.                        i i   i            i 
!.                       X,X,BUF,........,BUF 
!.                        1 2   1            lbuf 
!              On a évidemment : 
!.                                 i+1     i 
!.                      		 X    =  X 
!.                                 1       2   
!              et : 
!.                             I  i+1  i+1 I  = I  i - i I   
!.                             I X  - X    I  = I X - X  I 
!.                             I  2    1   I  = I  2 - 1 I               	
!     Un enregistrement ne sera effectivement lu que si x appartient à 
!     un intervalle (x1,x2) différent de celui traité au coup précédent. 
!     Dans le cas contraire, on n'a rien à faire.
!
!$Auteur
!  Philippe Brémard / Florence VIVARES (SchlumbergerSema)
!  à partir de EI_util_lecbuf
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_util_lecbuf(lfn,lent,xmin,xmax,pasx,lbuf,x,x1,x2,buf)
!.    integer :: lfn
!.    integer :: lent 
!.    real(KIND=PM_REEL) :: xmin
!.    real(KIND=PM_REEL) :: xmax
!.    real(KIND=PM_REEL) :: pasx
!.    integer :: lbuf 
!.    real(KIND=PM_REEL) :: x
!.    real(KIND=PM_REEL) :: x1
!.    real(KIND=PM_REEL) :: x2
!.    real(KIND=PM_REEL) , dimension(:) :: buf
!
!$Arguments
!>E     lfn   :<integer>           unité logique du fichier
!>E     lent  :<integer>           nombre de lignes de l'entête du fichier
!>E     xmin  :<PM_REEL>           date min (borne inférieure)
!>E     xmax  :<PM_REEL>           date max (borne supérieure)
!>E     pasx  :<PM_REEL>           pas de calcul des polynômes
!>E     lbuf  :<integer>           longueur d'un buffer
!>E     x     :<PM_REEL>           date courante
!>S     x1    :<PM_REEL>           date inférieure de l'intervalle courant
!>S     x2    :<PM_REEL>           date supérieure de l'intervalle courant
!>S     buf   :<PM_REEL,DIM=(:)>   buffer des coefficients de Tchebychev pour l'intervalle courant
!
!$Remarques
!
!$Mots-cles
!   éphémérides, Tchebychev
!
!$Voir-Aussi
!
!$Routines
!- MSP_signaler_message
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

! Arguments
      integer, intent(in)             :: lfn
      integer, intent(in)             :: lent      
      real(KIND=PM_REEL) , intent(in) :: xmin
      real(KIND=PM_REEL) , intent(in) :: xmax
      real(KIND=PM_REEL) , intent(in) :: pasx
      integer, intent(in)             :: lbuf   
      real(KIND=PM_REEL) , intent(in) :: x

      real(KIND=PM_REEL) , intent(out) :: x1
      real(KIND=PM_REEL) , intent(out) :: x2
      real(KIND=PM_REEL) , dimension(:), intent(out) :: buf

!	COMMONS

! Variables locales
      integer, parameter :: nblfnmax = 10
      integer :: ilfni = 0
      integer :: ilfn
      integer, dimension(nblfnmax) :: itablfn = (/0,0,0,0,0,0,0,0,0,0/)

      integer :: ienr,nenr
      integer, dimension(nblfnmax),save :: nenri = (/0,0,0,0,0,0,0,0,0,0/)

      integer :: nlec

      integer :: ibuf
      logical :: trouve


!-------------------------------
!	INITIALISATION
!-------------------------------
 
!-------------------------------
! CORPS DU SOUS-PROGRAMME <eph_util_lecbuf>
!-------------------------------

!-----------------------------------------------------------------------
!     Test en vue rejet numéro tape erroné
!-----------------------------------------------------------------------
      if (lfn < 0) then
         call MSP_signaler_message(cle_mes="EPH_ERR_LFN", &
                                   routine="eph_util_lecbuf")
         return
      endif

!-----------------------------------------------------------------------
!     Détection d'un nouveau fichier
!     On compare le numéro de tape du fichier à traiter avec les numéros
!     de tape déjà traités.
!     Le nombre est limité à nblfnmax
!-----------------------------------------------------------------------

      trouve = .false. 
	    do ilfn=1,ilfni 
	       if(lfn == itablfn(ilfn))then 
            trouve = .true. 
         endif 
      enddo 

!-----------------------------------------------------------------------
!        Cas où la tape de numéro lfn a déjà été enregistrée
!        On sort alors de la boucle et on va en 10 pour calculer le 
!        numéro de l'enregistrement du fichier qui est concerné)
!-----------------------------------------------------------------------

      if (trouve) then
         if (reset_tcheb == 0) then
                !-----------------------------------------------------------------
                !     - Rembobinage du fichier
                !     - Lecture de lent enregistrements
                !     -----> Positionnement au premier enregistrement des buffers
                !-----------------------------------------------------------------

                rewind lfn
                if (lent.ne.0) then
                   do ienr=1,lent
                      read(lfn,*)
                   enddo
                endif
                nenri(ilfn) = lent
                reset_tcheb = 1
         endif
	    else

!-----------------------------------------------------------------------
!     Cas d'un nouveau fichier
!     C'est le cas où la boucle précédente est terminée sans que le test
!     d'enregistrement ait été vérifié)
!     Il faut alors incrémenter de 1 le nombre de tapes enregistrées et
!     ensuite enregistrer le nouveau numéro de tape
!-----------------------------------------------------------------------

           ilfni = ilfni + 1
           ilfn  = ilfni
           if (ilfn > nblfnmax) then
              call MSP_signaler_message(cle_mes="EPH_ERR_LFNSUP", &
                                      routine="eph_util_lecbuf",&
                                      partie_variable="99")
              return
           else

!-----------------------------------------------------------------------
!        Enregistrement du nouveau numéro de tape
!-----------------------------------------------------------------------

              itablfn(ilfn) = lfn

              rewind lfn
              if (lent.ne.0) then
                 do ienr=1,lent
                    read(lfn,*)
                 enddo
              endif
              nenri(ilfn) = lent
         
           endif
      endif

!-----------------------------------------------------------------------
!     Numéro de l'enregistrement du fichier à atteindre et à lire
!     On doit évidemment tenir compte de la longueur de l'En-Tête
!     On commence par tester que x appartient à l'intervalle
!     (xmin-1.d-12,xmax-1.d-12)...Il est possible de diminuer d'un 
!     chouia la borne inférieure. Et, on doit diminuer également d'un 
!     chouia la borne supérieure afin de ne pas compter un 
!     enregistrement de trop dans le cas ou x = xmax 
!     Ensuite,on jette les cas aberrants (x en dehors de l'intervalle)
!-----------------------------------------------------------------------

      if ( (x >= (xmin-1.e-12_PM_REEL)) .and. (x <= (xmax-1e-12_PM_REEL)) )then
         nenr = int(abs(x-xmin)/pasx) + lent + 1 
!         nenr = int(abs(x-xmin-1.e-14_PM_REEL)/pasx) + lent + 1 
!         write(0,*) "x entre xmin et xmax : x-xmin=", x-xmin
!         write(0,*) "x entre xmin et xmax : diff (reel) =", int((x-xmin-1.e-14_PM_REEL)/pasx)
!         write(0,*) "x entre xmin et xmax : diff (test)=", int((x-xmin)/pasx)
!         write(0,*) " pasx=", pasx, " nenr=", nenr
      else 
         if (x <= xmin) then
	          if (abs(x-xmin) > 1.e-12_PM_REEL) then
               call MSP_signaler_message(cle_mes="EPH_ERR_INTERVALLE", &
                                   partie_variable=" x < xmin", & 
                                   routine="eph_util_lecbuf")
!              write(0,*) "x=",x, " x1=",x1-1.e-12_PM_REEL, " x2=", x2+1.e-12_PM_REEL
                 return
	          else
               nenr = lent + 1 
            endif 
         else 
	          if (abs(x-xmax) > 1.e-12_PM_REEL) then
               call MSP_signaler_message(cle_mes="EPH_ERR_INTERVALLE", &
                                   partie_variable=" x > xmax", & 
                                   routine="eph_util_lecbuf")
!               write(0,*) "x=",x, " x1=",x1-1.e-12_PM_REEL, " x2=", x2+1.e-12_PM_REEL
                 return
	          else
               nenr = int((x-xmin - 2.e-14_PM_REEL)/pasx) + lent + 1 
!               write(0,*) "x proche de xmax : x-xmin=", x-xmin
!               write(0,*) "x proche de xmax : pasx=", pasx
!               write(0,*) "x proche de xmax : nenr=", nenr
	          endif 
         endif 
      endif 

!-----------------------------------------------------------------------
!     Calcul nlec = Nombre de lectures pour atteindre et lire 
!                   l'enregistrement de numéro nenr
!     Si nlec = 0, l'enregistrement concerné est l'enregistement lu à 
!     l'appel précédent...Il n y a donc rien à faire
!     Si nlec  < 0, (1-nlec)  représente le nombre de backspaces à 
!     effectuer avant d'atteindre l'enregistrement à lire
!-----------------------------------------------------------------------
      nlec = nenr - nenri(ilfn) 

      if (nlec /= 0) then
         if (nlec > 0) then

            do ienr=1,nlec 
              read(lfn,1000,end=990) x1,x2,(buf(ibuf),ibuf=1,lbuf)
            enddo
            if ( (x < (x1-1.e-12_PM_REEL)) .or. (x > (x2+1.e-12_PM_REEL)) ) then
               call MSP_signaler_message(cle_mes="EPH_ERR_INTERVALLE", &
                                   partie_variable=" analyse erronée", & 
                                   routine="eph_util_lecbuf")

               return
            endif
            goto 9999
         else
            nlec = -nlec + 1  
!            write(0,*) "backspace(s), nlec=",nlec
            do ienr=1,nlec 
               backspace(lfn) 
            enddo
            read(lfn,1000,end=990) x1,x2,(buf(ibuf),ibuf=1,lbuf)
            goto 9999
         endif
      endif
 990    continue

        call MSP_signaler_message(cle_mes="EPH_ERR_INTERVALLE", &
             partie_variable=" Analyse erronée... Fin de fichier rencontrée", & 
             routine="eph_util_lecbuf")


!-----------------------------------------------------------------------
!     On a prévu par exemple pour le format suivant:
!           - 3 composantes (X,Y et Z)
!           - 15 planetes
!           - polynômes degré 15 (16 coefficients) pour décrire chaque 
!             composante
!           - La longeur maximale est donc : 3*15*16 = 720  
!
!     Et on lit d'autre part les t1950s de début et de fin de validité 
!     de l'enregistrement d'où le chiffre de 722
!-----------------------------------------------------------------------


 9999    continue
         nenri(ilfn) = nenr

!-------------------------------
!	FORMATS
!-------------------------------
 1000    format(722d23.16)

      end subroutine eph_util_lecbuf

      subroutine eph_math_tchapp (tdeb,tfin,t,ctch,ncmax,ndeg,nparam,f,dfsdt,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_math_tchapp
!
!$Resume
!  Approximation au sens de Tchebychev d'un ensemble de fonctions f(t)
!
!$Description
!	La méthode de calcul est décrite dans la note CNES CT/DTI/MS/AE/262
!       du 29/08/85-J.Bernard-O.Zarrouati.
!		
!	L'approximation est effectuée à l'aide de polynômes de
!	Tchebychev et des coefficients associés avec la formule :
!.	f       = < C | T >
!.	f       = somme (Ci*Ti(x) )
!	           i=0,ndeg + 1 
!.	df/dt   = somme (Ci*dTi(x)/dx )*dx/dt
!	              i=0,ndeg + 1 
!.      Ti      = polynôme de Tchebychev
!.	Ci      = coefficients de Tchebychev
!.	x       = point où l'on approxime la fonction f dans l'intervalle (tdeb,tfin) ramené à (-1,+1)
!
!$Auteur
!  Philippe Brémard / Florence VIVARES (SchlumbergerSema)
!  à partir de AM_math_tchapp
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_math_tchapp (tdeb,tfin,t,ctch,ncmax,ndeg,nparam,f,dfsdt,ier)
!.    real(KIND=PM_REEL) :: tdeb
!.    real(KIND=PM_REEL) :: tfin
!.    real(KIND=PM_REEL) :: t
!.    real(KIND=PM_REEL), dimension(*) :: ctch
!.    integer :: ncmax
!.    integer :: ndeg
!.    integer :: nparam
!.    real(KIND=PM_REEL), dimension(*) :: f
!.    real(KIND=PM_REEL), dimension(*) :: dfsdt
!.    integer :: ier
!
!$Arguments
!>E     tdeb    :<PM_REEL>            début de validité de l'intervalle de définition
!>E     tfin    :<PM_REEL>            fin de validité de l'intervalle de définition
!>E     t       :<PM_REEL,DIM=(in)>   valeur dans l'intervalle (tdeb,tfin) pour laquelle on approxime
!>E/S   ctch    :<PM_REEL,DIM=(*)>    coefficients de Tchebychev associés à l'approximation
!>E     ncmax   :<integer>            = (ndeg+1)*nparam )
!>E     ndeg    :<integer>            degré du polynôme approximant
!>E     nparam  :<integer>            nombre de fonctions (ou paramètres)  approximées
!>E/S   f       :<PM_REEL,DIM=(*)>    approximations des fonctions au point courant t
!>E/S   dfsdt   :<PM_REEL,DIM=(*)>    dérivée des fonctions f au point courant t
!>S     ier     :<integer>            code d'erreur 
!.                                             = 0  : pas d'erreur 
!.                                             < 0  : problème
!.                                             = -1 : problème dimensionnement vecteur ctch
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
!   Etant donné la rapidité de calcul des dérivées, on a préféré tout 
!   regrouper dans un seul module (même si celui-ci n'est pas nécessaire
!   à l'application utilisant eph_math_tchapp).
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        implicit none

! Arguments
      real(KIND=PM_REEL), intent(in) :: tdeb
      real(KIND=PM_REEL), intent(in) :: tfin
      real(KIND=PM_REEL), intent(in) :: t
      real(KIND=PM_REEL), dimension(*),intent(inout) :: ctch
      integer, intent(in) :: ncmax
      integer, intent(in) :: ndeg
      integer, intent(in) :: nparam
      real(KIND=PM_REEL), dimension(*),intent(inout) :: f
      real(KIND=PM_REEL), dimension(*),intent(inout) :: dfsdt
      integer, intent(out) :: ier

! Variables locales

      integer :: iparam,ind,ntot
      real(KIND=PM_REEL) :: x,xmul,tn0,tn1,tni,dtn0,dtn1,dtni
      integer :: ntott,i

! Code de calcul

        ier = 0

! Contrôle des parametres d'entree

        ntot = ndeg + 1

        if(ncmax.lt.(ntot*nparam))then
          ier=-1
          call MSP_signaler_message(cle_mes="EPH_ERR_CTCH", &
                                    routine="eph_math_tchapp" )
          return
        endif

! Calcul de la valeur x correspondant à l'intervalle réduit (-1,+1)

	xmul = 2._PM_REEL/(tfin-tdeb)
        x =  (t-tdeb)*xmul - 1._PM_REEL
        ntott = ntot
        ind=1

        do iparam=1,nparam

           ! Polynôme de Tchebychev de degré 0 (cf. Termes C0 du développement)
           tn0 = 1._PM_REEL        
           dtn0 = 0._PM_REEL
           ind=(iparam-1)*ntot+1
           f(iparam) = ctch(ind)
           dfsdt(iparam) = 0._PM_REEL
           
           if (ndeg /= 0) then 
              
              ! Polynôme de Tchebychev de degré 1 (cf. Termes C1 du développement)
              tn1 = x
              dtn1=1._PM_REEL
              ind = ind + 1
              f(iparam)= f(iparam) + ctch(ind)*tn1
              dfsdt(iparam) = ctch(ind)
              dtn1 = 1._PM_REEL
              
              if (ndeg /= 1) then
                 
                 ! Bouclage sur i = 2,ndeg + 1
                 
                 ind = ind + 1
                 
                 do i =ind,ntott
                    
                    ! Polynôme degré  i (cf formule de récurrence des poly. de Tchebychev)
                    
                    tni = 2._PM_REEL*x*tn1 - tn0
                    dtni = 2._PM_REEL*tn1+2._PM_REEL*x*dtn1-dtn0
                    
                    ! Contribution à la fonction du polynôme de degré i
                    
                    f(iparam) = f (iparam)+ ctch(i)*tni
                    dfsdt(iparam) = dfsdt(iparam)+ ctch(i)*dtni
                    
                    ! Rafraichissements pour l'itération suivante
                    
                    tn0 = tn1
                    tn1 = tni
                    dtn0 = dtn1
                    dtn1 = dtni
                 enddo
              endif
           endif
           
           ind=ind+ntot-2
           ntott=ntot+ntott
           dfsdt(iparam) = dfsdt(iparam)*xmul
           
        enddo
        
      end subroutine eph_math_tchapp


      subroutine eph_rep_frame_dico(nom,numero)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_rep_frame_dico
!
!$Resume
!  Conversion des noms de repères à leur numéro et réciproquement
!
!$Description
!  Cette subroutine convertit les noms de repères terrestres 
!  et planétaires (chaînes de caractères) en leur numéro.
!  Elle permet aussi de vérifier que le numéro du repère
!  appartient a la liste des repères disponibles.
!
!$Auteur
!  Philippe Brémard (Atos Origin)
!  (à partir de AM_rep_frame_dico)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_rep_frame_dico(nom,numero)
!.    	character(LEN=*) :: nom
!.    	integer :: numero
!
!$Arguments
!>E     nom     :<LEN=??>    nom du repère 
!>S     numero  :<integer>   code repère 
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

! Arguments
	character(LEN=*),intent(in) :: nom
	integer, intent(out) :: numero

!-------------------------------------------------------------------------------
!	Nombre de type de paramètres reconnus
!-------------------------------------------------------------------------------

	integer :: nombrep	
	parameter (nombrep = 59)
	character(LEN=20), dimension(nombrep) :: nomcni
	integer, dimension(nombrep)  :: numcni
	integer :: i
        logical :: trouve

!-------------------------------------------------------------------------------
! Définition des codes repères (à partir de AM_frame.h)
!-------------------------------------------------------------------------------

	integer AM_gv1950
	parameter (AM_gv1950 = 113)

	integer AM_gv2000
	parameter (AM_gv2000 = 123)

	integer AM_gvd
	parameter (AM_gvd = 133)

	integer AM_mer_gm1950
	parameter (AM_mer_gm1950 = 211)

	integer AM_ven_gm1950
	parameter (AM_ven_gm1950 = 212)

	integer AM_gm1950
	parameter (AM_gm1950 = 213)

	integer AM_mar_gm1950
	parameter (AM_mar_gm1950 = 214)

	integer AM_jup_gm1950
	parameter (AM_jup_gm1950 = 215)

	integer AM_sat_gm1950
	parameter (AM_sat_gm1950 = 216)

	integer AM_ura_gm1950
	parameter (AM_ura_gm1950 = 217)

	integer AM_nep_gm1950
	parameter (AM_nep_gm1950 = 218)

	integer AM_plu_gm1950
	parameter (AM_plu_gm1950 = 219)

	integer AM_mer_gm2000
	parameter (AM_mer_gm2000 = 221)

	integer AM_ven_gm2000
	parameter (AM_ven_gm2000 = 222)

	integer AM_gm2000
	parameter (AM_gm2000 = 223)

	integer AM_mar_gm2000
	parameter (AM_mar_gm2000 = 224)

	integer AM_jup_gm2000
	parameter (AM_jup_gm2000 = 225)

	integer AM_sat_gm2000
	parameter (AM_sat_gm2000 = 226)

	integer AM_ura_gm2000
	parameter (AM_ura_gm2000 = 227)

	integer AM_nep_gm2000
	parameter (AM_nep_gm2000 = 228)

	integer AM_plu_gm2000
	parameter (AM_plu_gm2000 = 229)

	integer AM_mer_gmd
	parameter (AM_mer_gmd = 231)

	integer AM_ven_gmd
	parameter (AM_ven_gmd = 232)

	integer AM_gmd
	parameter (AM_gmd = 233)

	integer AM_mar_gmd
	parameter (AM_mar_gmd = 234)

	integer AM_jup_gmd
	parameter (AM_jup_gmd = 235)

	integer AM_sat_gmd
	parameter (AM_sat_gmd = 236)

	integer AM_ura_gmd
	parameter (AM_ura_gmd = 237)

	integer AM_nep_gmd
	parameter (AM_nep_gmd = 238)

	integer AM_plu_gmd
	parameter (AM_plu_gmd = 239)

	integer AM_ecl1950
	parameter (AM_ecl1950 = 313)

	integer AM_ecl2000
	parameter (AM_ecl2000= 323)

	integer AM_ecld
	parameter (AM_ecld = 333)

	integer AM_de118
	parameter (AM_de118 = 400)

	integer AM_de200
	parameter (AM_de200 = 500)

	integer AM_veis
	parameter (AM_veis = 600)

	integer AM_veis1950
	parameter (AM_veis1950 = 610)

	integer AM_mer_pld
	parameter (AM_mer_pld = 731)

	integer AM_ven_pld
	parameter (AM_ven_pld = 732)

	integer AM_pld
	parameter (AM_pld = 733)

	integer AM_mar_pld
	parameter (AM_mar_pld = 734)

	integer AM_jup_pld
	parameter (AM_jup_pld = 735)

	integer AM_sat_pld
	parameter (AM_sat_pld = 736)

	integer AM_ura_pld
	parameter (AM_ura_pld = 737)

	integer AM_nep_pld
	parameter (AM_nep_pld = 738)

	integer AM_plu_pld
	parameter (AM_plu_pld = 739)

	integer AM_mer_PQ
	parameter (AM_mer_PQ = 801)

	integer AM_ven_PQ
	parameter (AM_ven_PQ = 802)

	integer AM_ter_PQ
	parameter (AM_ter_PQ = 803)

	integer AM_mar_PQ
	parameter (AM_mar_PQ = 804)

	integer AM_jup_PQ
	parameter (AM_jup_PQ = 805)

	integer AM_sat_PQ
	parameter (AM_sat_PQ = 806)

	integer AM_ura_PQ
	parameter (AM_ura_PQ = 807)

	integer AM_nep_PQ
	parameter (AM_nep_PQ = 808)

	integer AM_plu_PQ
	parameter (AM_plu_PQ = 809)

        integer AM_g50_cne
        parameter (AM_g50_cne = 903)

        integer AM_station
        parameter (AM_station = 1000)

        integer AM_rampe
        parameter (AM_rampe = 1100)

        integer AM_tiv
        parameter (AM_tiv =1500)

!-------------------------------------------------------------------------------
! Tableau des repères autorisés
!-------------------------------------------------------------------------------

       data nomcni /'AM_gv1950','AM_gv2000','AM_gvd','AM_mer_gm1950',    &
       'AM_ven_gm1950','AM_gm1950','AM_mar_gm1950','AM_jup_gm1950',      &
       'AM_sat_gm1950','AM_ura_gm1950','AM_nep_gm1950','AM_plu_gm1950',  &
       'AM_mer_gm2000','AM_ven_gm2000','AM_gm2000','AM_mar_gm2000',      &
       'AM_jup_gm2000','AM_sat_gm2000','AM_ura_gm2000','AM_nep_gm2000',  & 
       'AM_plu_gm2000','AM_mer_gmd','AM_ven_gmd','AM_gmd','AM_mar_gmd',  &
       'AM_jup_gmd','AM_sat_gmd','AM_ura_gmd','AM_nep_gmd','AM_plu_gmd', &
       'AM_ecl1950','AM_ecl2000','AM_ecld','AM_de118','AM_de200',        &
       'AM_veis','AM_veis1950','AM_mer_pld','AM_ven_pld',                &
       'AM_pld','AM_mar_pld','AM_jup_pld','AM_sat_pld','AM_ura_pld',     &
       'AM_nep_pld','AM_plu_pld','AM_mer_PQ','AM_ven_PQ','AM_ter_PQ',    &
       'AM_mar_PQ','AM_jup_PQ','AM_sat_PQ','AM_ura_PQ','AM_nep_PQ',      &
       'AM_plu_PQ','AM_g50_cne','AM_station','AM_rampe','AM_tiv'/


       data numcni /AM_gv1950,AM_gv2000,AM_gvd,AM_mer_gm1950,           &
       AM_ven_gm1950,AM_gm1950,AM_mar_gm1950,AM_jup_gm1950,             &
       AM_sat_gm1950,AM_ura_gm1950,AM_nep_gm1950,AM_plu_gm1950,         &
       AM_mer_gm2000,AM_ven_gm2000,AM_gm2000,AM_mar_gm2000,             &
       AM_jup_gm2000,AM_sat_gm2000,AM_ura_gm2000,AM_nep_gm2000,         &
       AM_plu_gm2000,AM_mer_gmd,AM_ven_gmd,AM_gmd,AM_mar_gmd,           &
       AM_jup_gmd,AM_sat_gmd,AM_ura_gmd,AM_nep_gmd,AM_plu_gmd,          &
       AM_ecl1950,AM_ecl2000,AM_ecld,AM_de118,AM_de200,                 &
       AM_veis,AM_veis1950,AM_mer_pld,AM_ven_pld,                       &
       AM_pld,AM_mar_pld,AM_jup_pld,AM_sat_pld,AM_ura_pld,              &
       AM_nep_pld,AM_plu_pld,AM_mer_PQ,AM_ven_PQ,AM_ter_PQ,             &
       AM_mar_PQ,AM_jup_PQ,AM_sat_PQ,AM_ura_PQ,AM_nep_PQ,               &
       AM_plu_PQ,AM_g50_cne,AM_station,AM_rampe,AM_tiv/

!//-------------------------------------------------------------------------------
!// Début du code
!//-------------------------------------------------------------------------------
        i = 1
        trouve = .false.

        do while (i <= nombrep .and. .not.trouve) 
           if (trim(nom) == trim(nomcni(i))) then
              numero = numcni(i)
              trouve=.true.
           endif
           i = i+1
        end do

        if (.not. trouve) then
           ! Si on n'est pas sorti de la boucle, 
           ! le nom n'est pas dans la liste
          call MSP_signaler_message(cle_mes="EPH_ERR_REP_INC", &
                                    routine="eph_rep_frame_dico" )
          return
        endif

	end subroutine eph_rep_frame_dico

end module eph_tcheb


