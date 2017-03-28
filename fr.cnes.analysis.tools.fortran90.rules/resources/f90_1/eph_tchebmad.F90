module eph_tchebmad

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_tchebmad
!
!$Resume
!  Module contenant les sous-programmes de la méthode de  Tchebychev MADONA.
!
!$Description
!  Module contenant les sous-programmes d'initialisation, de fermeture
!  et de calcul d'éphémérides relatifs à la méthode Tchebychev MADONA
!
!$Auteur
!   Florence Vivares / Philippe Brémard (SchlumbergerSema) 
!
!$Version
!  $Id: eph_tchebmad.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: eph_tchebmad.F90,v $
!  Revision 1.26  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.25  2009/09/01 13:30:33  cmartel
!  AQ : Suppression de variables inutilisées
!
!  Revision 1.24  2009/08/28 09:20:58  cmartel
!  AQ : Suppression d'une dsallocation inutile
!
!  Revision 1.23  2008/10/16 08:17:37  cml
!  DM-ID 1058 : Correction d une mauvaise intialisation
!
!  Revision 1.22  2008/10/15 15:01:13  tanguyy
!  DM-ID 1058 : Liberation mémoire (sauts du TUC) lors de la cloture de la méthode tcheb-mad
!
!  Revision 1.21  2008/10/01 17:40:53  tanguyy
!  DM-ID 1058 : amélioration de la gestion mémoire
!
!  Revision 1.20  2008/07/09 07:52:04  cml
!  FA-ID 1068 : Correction du cartouche du module
!
!  Revision 1.19  2008/07/09 07:40:49  cml
!  FA-ID 1068 : Mise a jour du cartouche de la subroutine eph_posvit_3corps
!
!  Revision 1.18  2008/07/04 11:47:58  huec
!  DM-ID 1058 : Ajout de l\'attribut save pour une variable globale, gestion de la memoire
!  Revision 1.17  2008/04/25 11:19:27  vivaresf
!  DM-ID 553 : validation solaris 2.10, l'initialisation de eph_lu est indispensable
!  Revision 1.16  2008/04/10 16:44:09  vivaresf
!  Version AQ : relecture de code, suppression de l'initialisation de eph_lu (inutile)
!  Revision 1.15  2008/03/26 07:58:33  huec
!  DM-ID 953 : Portage Solaris 10, initialisation d un booleen
!  Revision 1.14  2008/02/11 08:58:08  huec
!  DM-ID 11 : Gestion du delta de saut de TUC par COMPAS pour les ephemerdides du troisieme corps
!  Revision 1.13  2008/02/05 10:14:23  huec
!  DM-ID 11 : Ajout du calcul des nouveaux parametres optionnels dans eph_posvit_3corps et de la date TUC
!  Revision 1.12  2008/02/05 08:54:22  huec
!  DM-ID 11 : Ajout du calcul des nouveaux parametres optionnels dans eph_posvit_3corps et de la date TUC
!  Revision 1.11  2007/11/02 10:59:51  huec
!  DM-ID 11 : Gestion des donnees par MECASPA
!  Revision 1.10  2006/05/30 16:57:58  vivaresf
!  Variables inutiles
!  Revision 1.9  2006/05/30 15:24:29  vivaresf
!  Metriques : supression d'un niveau d'imbrication
!  regle de codage : traitement des deallocate
!  Revision 1.8  2006/05/30 12:29:08  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.7  2006/05/30 08:19:09  vivaresf
!  DM-ID 387 : variables locales inutilisées
!  Revision 1.6  2006/04/26 11:54:17  bouillaj
!  FA-ID 371 : Modification de la fabrication de certains noms de fichiers pour eviter les blancs
!  Revision 1.5  2006/04/06 16:02:18  vpg
!  Mise a jour de la lecture du code et de la date de reference du repere
!  Revision 1.4  2005/12/08 18:39:05  vivaresf
!  Cartouches et vérification des déclarations
!  Revision 1.3  2005/12/08 08:59:44  vpg
!  Suppression des commentaires inutiles
!  Revision 1.2  2005/12/07 09:01:54  vpg
!  suppression du chargement du fichier unites MADONA qui est fait dans cps_init()
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.6  2005/10/13 10:38:15  bouillaj
!  FA-ID 371 : Cloture du FT (Corrections dans libephem pour passage linux)
!  Revision 1.5.2.1  2005/10/13 10:29:53  bouillaj
!  FA-ID 371 : Version initiale du FT (Corrections dans libephem pour passage linux)
!  Revision 1.5  2005/05/09 14:17:32  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.4  2005/03/09 09:12:05  vivaresf
!  Correction des cartouches
!  Revision 1.3  2004/12/17 14:58:11  vivaresf
!  Documentation
!  Revision 1.2  2004/05/25 13:16:08  vivaresf
!  DM_158
!  Revision 1.1.1.1.2.1  2004/05/25 09:52:56  vivaresf
!  LIBEPHEM V1_9 : indépendante par rapport a la MECASPA
!  - rajout des fonctions EPH_lire_fictchebmad, eph_acc_gettab
!  - definition de la structure EPHV_EPHEMERIDES
!  - rajout messages LIBEPHEM a la place des messages MECASPA
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.19  2004/01/13 09:16:26  bremard
!  Mise à jour cartouche
!  Revision 1.16  2004/01/07 16:13:04  bremard
!  Mise à jour cartouche + simplification eph_posvit_3corps
!  Revision 1.15  2003/12/31 15:57:34  bremard
!  Suppression conversion eph_cnes2old non nécessaire avec MECASPA V3.1
!  Revision 1.14  2003/12/30 16:05:52  bremard
!  Remplacement AM_math_tchapp par eph_math_tchapp
!  Revision 1.13  2003/06/06 14:52:41  bremard
!  Particularisation des parametres
!  Revision 1.12  2002/10/17 09:44:26  vivaresf
!  Correction bug MECASPA
!  Revision 1.11  2002/09/26 15:39:31  vivaresf
!  reinitialisation des variables intermediaires en debut d'appel
!  Revision 1.10  2001/12/18 16:11:38  vivaresf
!  Maj documentation
!  Revision 1.9  2001/12/17 17:40:31  vivaresf
!  Num. de corps vrai
!  Revision 1.8  2001/12/07 16:44:44  vivaresf
!  Presentation fm des cartouches
!  Revision 1.7  2001/12/06 09:16:15  vivaresf
!  maj doc
!  Revision 1.6  2001/12/05 15:54:17  bremard
!  PhB - Mise à jour des cartouches
!  Revision 1.5  2001/12/04 11:13:51  vivaresf
!  Mise au point et cas test
!  Revision 1.4  2001/12/04 09:23:17  vivaresf
!  Appel avec nom et pathname
!  Revision 1.3  2001/11/30 11:19:01  vivaresf
!  Desallocation impossible en l'etat
!  Revision 1.2  2001/11/29 16:12:54  vivaresf
!  Cartouche + gestion d'erreur
!  Revision 1.1  2001/11/27 15:45:09  vivaresf
!  Version initiale
!
!$FinHistorique
!
!$Structure
!
!: EPH_ephemerides : 
!>     pas         : <pm_reel>                      pas des éphémérides
!>     deg_tcheb   : <integer>                      degré des polynomes de Tchebytchev
!>     nb_corps    : <integer>                      nombre de corps
!>     nb_dates    : <integer>                      nombre de date
!>     corps       : <integer,DIM=(:),pointer>      tableau nb_corps des codes
!                                        LIBEPHEM des corps
!>     repere      : <LEN=32>                       
!>     ech_temps   : <integer>                      échelle de temps (TUC=333,TE=111)
!>     dates       : <pm_reel,DIM=(:),pointer>      tableau nb_dates des dates
!>     mu          : <pm_reel,DIM=(:),pointer>      coef. de gravition des corps
!>     coeff       : <pm_reel,DIM=(:,:,:),pointer>  tableau (deg_tcheb, nb_corps, 
!                                        nb_dates) des coef. de Tchebytchev
!>     eph_lu      : <logical>                      indique si la structure a été remplie
!
!$Routines
!- eph_inittchemad
!- eph_closetchemad
!- eph_pvtchebmad
!- eph_posvit_3corps
!- EPH_lire_fictchebmad
!#V
!- eph_acc_gettab
!#
!
!$Module
!#V
!- mslib
!- msp_gestion_erreur
!- eph_varglob
!- eph_constantes
!- eph_info
!- eph_tcheb
!#
!
!$Interface
!#V
!#
!
!$Usage
!  use eph_tchebmad
!
!$Global
!
!>  ephv_inittchebmad    : <logical>                   =.true. si la méthode initialisation
!>  ephv_fictchemad      : <LEN=EPHV_LGMAX>            fichier MADONA lu dans EPHV_EPHEMERIDES
!>  ephv_pathtchemad     : <LEN=EPHV_LGMAX>            répertoire où trouver fictchemad
!>  EPHV_EPHEMERIDES     : <EPH_ephemerides>           structure contenant les coefficients
!>  ephv_corpsctchemad   : <integer>                   corps central
!>  cps_TE               : <integer,parameter,public>  
!>  cps_TAI              : <integer,parameter,public>  
!>  cps_TUC              : <integer,parameter,public>  
!$Remarques
!
!$Mots-cles
!   éphémérides, polynomes de Tchebychev, MADONA
!
!$Voir-Aussi
!#V
!.  eph_acc_gettab
!#
!.  eph_inittchemad eph_closetchemad eph_pvtchebmad eph_posvit_3corps EPH_lire_fictchebmad
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use mslib
  use msp_gestion_erreur
  use eph_varglob
  use eph_constantes
  use eph_info
  use eph_tcheb
  
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_tchebmad.F90 69 2012-09-11 08:33:34Z ffsm $'



  type EPH_ephemerides
      real(kind=pm_reel)                            :: pas
      integer                                       :: deg_tcheb,nb_corps,nb_dates
      integer, pointer, dimension(:)                :: corps => NULL()
      character(LEN=32)                             :: repere
      integer                                       :: ech_temps
      real(kind=pm_reel), pointer, dimension(:)     :: dates => NULL()
      real(kind=pm_reel), pointer, dimension(:)     :: mu    => NULL()
      real(kind=pm_reel), pointer, dimension(:,:,:) :: coeff => NULL()
      logical :: eph_lu = .false.
   end type EPH_ephemerides

   ! methode Tchebychef MADONA
   logical :: ephv_inittchebmad=.false.            ! initialisée ou non
   character(len=EPHV_LGMAX) :: ephv_fictchemad    ! fichier chargé
   character(len=EPHV_LGMAX) :: ephv_pathtchemad   ! repertoire
   type(EPH_ephemerides), save :: EPHV_EPHEMERIDES       ! structure éphémérides contenant
                                                   ! le fichier chargé
   integer :: ephv_corpsctchemad                   ! Code du corpsm central

   private eph_acc_gettab

   ! constantes qu'on ne peux plus prendre dans la MSPRO
   integer, parameter, public :: cps_TE  = 111
   integer, parameter, public :: cps_TAI = 222
   integer, parameter, public :: cps_TUC = 333


contains

      subroutine eph_inittchemad(nompath, nomfic)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_inittchemad
!
!$Resume
!   Initialisation de la méthode par polynômes de Tchebychev MADONA
!
!$Description
!   Lecture du fichier de polynômes de Tchebychev. Allocation et
!   affectation de la variable EPHV_EPHEMERIDES.
!   Lecture du nom du corps central et enregistrement dans ephv_corpsctchemad.
!
!$Auteur
!    Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Routines
!- MSP_signaler_message
!- EPH_lire_fictchebmad
!- eph_fic2code
!
!$Usage
!  call eph_inittchemad(nompath, nomfic)
!.    character(len=*) :: nompath, nomfic
!
!$Arguments
!>E     nompath  :<LEN=*>   répertoire ou trouver le fichier nomfic
!>E     nomfic   :<LEN=*>   nom du fichier de polynômes de Tchebychev au
!                           format MADONA
!
!$Remarques
!
!$Mots-cles
!   éphémérides, Tchebychev, MADONA
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        implicit none

        ! arguments
        character(len=*), intent(in) :: nompath, nomfic

        ! variable locales
        integer, dimension(13) :: statb
        integer :: ier, stat
        character(len=30) :: nomcorps

        ! chargement uniquement si le fichier est différent du fichier courrant
        if(nomfic.ne.ephv_fictchemad.and.nomfic.ne."") then
           ! et s'il existe
           ier=stat(trim(nompath)//trim(nomfic), statb)
           if(ier.ne.0) then
              call MSP_signaler_message(cle_mes="EPH_ERR_FICABS", &
                   partie_variable=trim(nomfic), &
                   routine="eph_inittchemad")
              goto 100
           endif

           call EPH_lire_fictchebmad(trim(nompath)//trim(nomfic), &
                EPHV_EPHEMERIDES)
           if (MSP_gen_MESSAGES("eph_inittchemad")) return

           ! recuperer le corps central
           call eph_fic2code(trim(nompath)//trim(nomfic), &
                nomcorps, libelle="Corps_central", code=ephv_corpsctchemad)
           if (MSP_gen_MESSAGES("eph_inittchemad")) return
           
           ! méthode initialisée
           if(.not.MSP_ERREUR) then 
              ephv_fictchemad=nomfic
              ephv_pathtchemad=nompath
              ephv_inittchebmad=.true.
           endif
        endif
        
100 continue

   endsubroutine eph_inittchemad

   subroutine eph_closetchemad()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_closetchemad
!
!$Resume
!    Fin d'utilisation de la méthode  Tchebytchev/MADONA
!
!$Description
!    Suppression des tableaux initialisés dans la structure EPHV_EPHEMERIDES.
!    Remise à zéro des variables de contrôle.
!
!$Auteur
!    Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_closetchemad()
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

! variables locales
        integer :: iostat

        if(ephv_inittchebmad) then 
           if(associated(EPHV_EPHEMERIDES%corps)) then
              DEALLOCATE (EPHV_EPHEMERIDES%corps, stat=iostat)
              if (iostat < 0) then
                 call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                      routine="eph_closetchemad")
                 return
              end if
           end if
           if(associated(EPHV_EPHEMERIDES%dates)) then
              DEALLOCATE (EPHV_EPHEMERIDES%dates, stat=iostat)
              if (iostat < 0) then
                 call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                      routine="eph_closetchemad")
                 return
              end if
           end if
           if(associated(EPHV_EPHEMERIDES%mu)) then
              DEALLOCATE (EPHV_EPHEMERIDES%mu, stat=iostat)
              if (iostat < 0) then
                 call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                      routine="eph_closetchemad")
                 return
              end if
           end if
           if(associated(EPHV_EPHEMERIDES%coeff)) then

              DEALLOCATE (EPHV_EPHEMERIDES%coeff, stat=iostat)
              if (iostat < 0) then
                 call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                      routine="eph_closetchemad")
                 return
              end if
           end if

           ephv_fictchemad=""
           ephv_pathtchemad=""
           ! méthode non initialisée
           ephv_inittchebmad=.false.
        endif

      endsubroutine eph_closetchemad

      subroutine eph_pvtchebmad(ncorpsc, npla, date, pv)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_pvtchebmad
!
!$Resume
!  Calcul d'éphémérides avec la méthode de Tchebychev / MADONA
!
!$Description
!  Calcul d'éphémérides à l'aide de la structure EPHV_EPHEMERIDES.
!  Tous les corps disponibles dans la structure, y compris le corps
!  central, sont accessibles.
!
!$Auteur
!    Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Routines
!- MSP_signaler_message
!- eph_posvit_3corps
!
!$Mots-cles
!   éphémérides, Tchebytchev, MADONA
!
!$Voir-Aussi
!   mecaspa (MSP_posvit_3corps)
!
!$Usage
!  call eph_pvtchebmad(ncorpsc, npla, date, pv)
!.    integer :: ncorpsc, npla
!.    real(kind=PM_REEL) :: date
!.    real(kind=PM_REEL), dimension(6) :: pv
!
!$Arguments
!>E     ncorpsc  :<integer>           code du corps central
!>E     npla     :<integer>           code du corps d'intérêt
!>E     date     :<PM_REEL>           date
!>S     pv       :<PM_REEL,DIM=(6)>   coordonnées cartésiennes (positions/vitesses)
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        implicit none

        integer, intent(in) :: ncorpsc, npla
        real(kind=PM_REEL), intent(in) :: date
        real(kind=PM_REEL), dimension(6), intent(out) :: pv

        ! Variables locales
        real(kind=PM_REEL), dimension(3) :: p1, p2, v1, v2
        integer :: idate

        ! méthode initialisée ?
        if (.not.ephv_inittchebmad) then
           call MSP_signaler_message(cle_mes="EPH_ERR_INITABS", &
                partie_variable=" Tchebytchev / MADONA ", & 
                routine="eph_pvtchebmad")
           goto 999
        endif

        ! Calcul par la MECASPA
        ! on considere que l'on ne generera que des fichiers 
        ! avec numerotation de corps OK

        ! Appels avec idate pour optimisation dans le cas d'appel en boucle 

        p2(1:3)=0.
        p1(1:3)=0.
        v2(1:3)=0.
        v1(1:3)=0.
        if(ncorpsc.ne.ephv_corpsctchemad) &
             call eph_posvit_3corps (date, ncorpsc, EPHV_EPHEMERIDES, p1, v1, idate)
        if(MSP_ERREUR) goto 999

        if(npla.ne.ephv_corpsctchemad) &
             call eph_posvit_3corps (date, npla, EPHV_EPHEMERIDES, p2, v2, idate)
        if(MSP_ERREUR) goto 999

        ! positions/vitesses relatives en km et km/s
        pv(1:3) = (p2(1:3) - p1(1:3))/1000._PM_REEL
        pv(4:6) = (v2(1:3) - v1(1:3))/1000._PM_REEL

999     continue

      end subroutine eph_pvtchebmad


   subroutine eph_posvit_3corps (date,numcorps,ephemerides,pos3c,vit3c,idate,mu3c,dis3c, &
                                 dir3c,echt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_posvit_3corps
!
!$Resume
!  Calcul de la position et de la vitesse d'un corps céleste dans le repère des
!  éphémérides
!
!$Description
!  Calcul de la position et de la vitesse d'un corps céleste dans le repère des
!  éphémérides.
!  Copie corrigée et simplifiée de la fonction MECASPA MSP_posvit_3corps.
!
!$Auteur
!  J. F. GOESTER
!  Ph Bremard (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_posvit_3corps (date,numcorps,ephemerides,pos3c,vit3c,[idate],[mu3c],[dis3c], &
!.                                     [dir3c],[echt])
!.    real(kind=pm_reel) :: date
!.    integer :: numcorps 
!.    type(EPH_EPHEMERIDES) :: ephemerides
!.    real(kind=pm_reel), dimension(3) :: pos3c,vit3c
!.    integer :: idate
!.    real(kind=pm_reel) :: mu3c
!.    real(kind=pm_reel) :: dis3c
!.    real(kind=pm_reel), dimension(3) :: dir3c
!.    integer :: echt 
!
!$Arguments
!>E     date         :<pm_reel>           date à laquelle on veut calculer les 
!                                         positions-vitesses d'un corps [JJ CNES]
!>E     numcorps     :<integer>           numéro du corps considéré
!>E     ephemerides  :<EPH_EPHEMERIDES>   structure contenant les éphémérides
!>S     pos3c        :<pm_reel,DIM=(3)>   tableau contenant la position du corps dans
!                                         le repère des éphémérides [m]
!>S     vit3c        :<pm_reel,DIM=(3)>   tableau contenant la vitesse du corps dans le repère des éphémérides [m/s]
!>[E/S] idate        :<integer>           indice de la précédente date (optimisation de la recherche)
!>[S]   mu3c         :<pm_reel>           constante d'attraction du corps considéré [m^3/s^2]
!>[S]   dis3c        :<pm_reel>           distance du corps dans le repère des éphémérides [m]
!>[S]   dir3c        :<pm_reel,DIM=(3)>   cosinus directeurs du corps dans le repère des éphémérides [-]
!>[E]   echt         :<integer>           échelle de temps de la date (pm_TUC ou pm_TE)
!                                         par défaut pm_TE
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_jourfrac_joursec
!- cps_get_sautTAITUC
!- md_joursec_jourfrac
!- EPH_math_tchapp
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

      real(kind=pm_reel), intent(IN) :: date
      integer, intent(IN) :: numcorps 
      type(EPH_EPHEMERIDES), intent(IN) :: ephemerides
      real(kind=pm_reel), intent(OUT), dimension(3) :: pos3c,vit3c
      integer, intent(INOUT), optional :: idate
      real(kind=pm_reel), intent(OUT), optional :: mu3c
      real(kind=pm_reel), intent(OUT), optional :: dis3c
      real(kind=pm_reel), intent(OUT), optional, dimension(3) :: dir3c
      integer, intent(IN), optional :: echt 

      integer :: indc,indd,i,ier,idd,i1,i2,incr, iostat
      integer :: nparam,ncoeffmx,k
      real(kind=pm_reel) :: date_te, norme, reste
      character(LEN=20), dimension(2) :: com_pvar
      type(tm_jour_sec)    :: date_pm, date_pm_out, date_inter
      type(tm_code_retour) :: code_retour
      integer   :: delta_TAITUC
      real(pm_reel), parameter :: delta_sec_TE_TAI = 32.184_pm_reel ! ecart en secondes entre les echelles TE et TAI
      
     
      ! Parametres de la structure EPHV_EPHEMERIDES
      integer :: deg_tcheb,nb_corps,nb_dates
      logical :: eph_lu
      real(kind=pm_reel), dimension(:), pointer :: coeff => NULL()
      
      ! Initialisations
      eph_lu = ephemerides%eph_lu
      nb_dates = ephemerides%nb_dates
      nb_corps = ephemerides%nb_corps
      deg_tcheb = ephemerides%deg_tcheb

      if ( .not. eph_lu ) then
         call MSP_signaler_message (cle_mes="EPH_TCHEBMAD_000", &
              routine="eph_posvit_3corps")
         return
      endif

      ! -- Par défaut, date en ECHT_TE
      date_te = date

      ! Présence de l'argument echt:
      if ( present(echt) ) then
         ! La date est en TUC, on la passe en TE
         if (echt == CPS_TUC) then

            ! Calcul de la date TE
            call md_jourfrac_joursec(date,date_pm,code_retour)
            call MSP_signaler_message (ier_mslib=code_retour)
            if (MSP_gen_messages("eph_posvit_3corps - conv. date en entrée - ")) return

            ! passage TUC-> TAI
            ! Calcul du delta TAI - TUC
            call cps_get_sautTAITUC(date_pm,delta_TAITUC)
            if (MSP_gen_messages("eph_posvit_3corps")) return
            ! Calcul de la date TAI
            date_inter%jour = date_pm%jour
            date_inter%sec  = date_pm%sec + real(delta_TAITUC, kind=pm_reel)
            ! calcul du nombre de jours lies aux secondes
            reste = date_inter%sec / 86400._pm_reel
            k     = floor(reste)
            ! calcul de la quantite normalisee, k vaut 0 ou 1
            date_pm_out%jour = date_inter%jour + k
            date_pm_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel

            ! Passage TAI -> TE
            date_inter%jour = date_pm_out%jour
            date_inter%sec  = date_pm_out%sec + delta_sec_TE_TAI
            ! calcul du nombre de jours lies aux secondes
            reste = date_inter%sec / 86400._pm_reel
            k     = floor(reste)
            ! calcul de la quantite normalisee, k vaut 0 ou 1
            date_pm_out%jour = date_inter%jour + k
            date_pm_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel

   
            ! -- Conversion de la date en structure  à la date en réel
            call md_joursec_jourfrac(date_pm_out,date_te,code_retour)
            call MSP_signaler_message (ier_mslib=code_retour)
            if (MSP_gen_messages("eph_posvit_3corps - conv. date en sortie - ")) return

         endif !Fin du passage en date TUC
      endif

      ! Tests sur la date par rapport aux éphémérides:

      if ((date_te<ephemerides%dates(1)).or. &
           (date_te>ephemerides%dates(nb_dates))) then
         call MSP_signaler_message (cle_mes="EPH_TCHEBMAD_001", &
              routine="eph_posvit_3corps")
         return
      endif
      indd = -1
      ! Test sur la présence de l'argument idate:
      if ( present (idate) ) then
         idd = idate
      else
         idd = 0
      endif
      ! Optimisation de la recherche
      if ( idd <= 0 ) then
         ! On part du début:
         i1   = 2
         i2   = nb_dates
         incr = 1
      else
         if ( idd >= nb_dates ) then
            ! On part de la fin:
            i1 = nb_dates - 1
            i2 = 1
            incr = -1
         else
            if ( date_te >  ephemerides%dates(idd+1) ) then
               ! On cherche à partir de idd +1
               i1 = idd+2
               i2 = nb_dates
               incr = 1
            else if ( date_te < ephemerides%dates(idd) ) then
               ! On cherche à partir de idd dans l'ordre décroissant
               i1 = idd - 1
               i2 = 1
               incr = -1
            else
               ! L'indice est idd
               indd = idd
               goto 100
            endif
         endif
      endif
      ! Recherche de la date:
      int_date: do i = i1 , i2 , incr
         if ( incr == 1 ) then
            if ( date_te <  ephemerides%dates(i) ) then
               indd = i-1
               exit int_date
            endif
         else
            if ( date_te >  ephemerides%dates(i) ) then
               indd = i
               exit int_date
            endif
         endif        
      enddo int_date
      if ( indd < 0 ) then
         call MSP_signaler_message (cle_mes="EPH_TCHEBMAD_001",&
              routine="eph_posvit_3corps")
         return
      endif

 100  continue

      ! Test pour savoir si le corps demandé appartient bien aux éphémérides:
      indc = -1
      nomco: do i = 1 , nb_corps
         if ( numcorps == ephemerides%corps(i) ) then
            indc = i
            exit nomco
         endif
      enddo nomco
      if ( indc < 0 ) then
         call MSP_signaler_message (cle_mes="EPH_TCHEBMAD_002",&
              routine="eph_posvit_3corps")
         return
      endif

      ! Initialisations de variables nécessaires à l'appel de eph_math_tchapp:

      nparam   = 3
      ncoeffmx = (deg_tcheb + 1) * nparam

      if (associated(coeff)) deallocate(coeff, stat=iostat)
      allocate(coeff(ncoeffmx))
      coeff(:) = ephemerides%coeff(indd, indc, 1:ncoeffmx)

      ! Calcul des position / vitesse du corps:
      call EPH_math_tchapp (ephemerides%dates(indd), ephemerides%dates(indd+1), &
           date_te, coeff, ncoeffmx,deg_tcheb,nparam,pos3c,vit3c,ier)

      if ( ier /= 0 ) then
         write(com_pvar(1),'(i8)') ier
         com_pvar(2) = "EPH_math_tchapp"
          call MSP_signaler_message (cle_mes="EPH_AMLIB",partie_variable=com_pvar, &
               routine="EPH_posvit_3corps")
         return
      else
         pos3c(:) = pos3c(:)*1000._pm_reel                   ! m
         vit3c(:) = vit3c(:)*1000._pm_reel/86400._pm_reel    ! m/s
      endif

      ! Récupération du mu si nécessaire
      if (present(mu3c)) then
         mu3c = ephemerides%mu(indc)
      endif

      ! Calcul de dis3c et/ou pos3c si demandé

      if ( present(dis3c) .or. present(dir3c) ) then
         norme = sqrt ( pos3c(1)**2 +  pos3c(2)**2 +  pos3c(3)**2 )
         if ( present(dis3c) ) dis3c = norme
         ! Calcul du coefficient directeur avec test de non nullite de la norme
         if ( present(dir3c) ) then
            if (cpsi_compareReels(norme,0._pm_reel) .eq. 0) then
               ! Norme nulle
               dir3c(:) = 0._pm_reel
            else
               !Norme non nulle
               dir3c(:) = pos3c(:)/norme
            endif
         endif
      endif
      
      if(associated(coeff)) deallocate(coeff, stat=iostat)

   end subroutine eph_posvit_3corps


   subroutine EPH_lire_fictchebmad (nomfic,ephemerides)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  EPH_lire_fictchebmad
!
!$Resume
!  Lecture d'un fichier éphémérides au format MADONA.
!
!$Description
!  Lecture d'un fichier éphémérides au format MADONA dans une structure 
!  EPH_EPHEMERIDES (Copie de MSP_lire_3corps).
!  Les tableaux MADONA sont lus par une fonction spécifique.
!
!$Auteur
!  J. F. GOESTER
!  F. Vivares (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call EPH_lire_fictchebmad (nomfic,ephemerides)
!.    character(LEN=*) :: nomfic
!.    type(EPH_ephemerides) :: ephemerides
!
!$Arguments
!>E     nomfic       :<LEN=*>             nom du fichier où se trouvent les éphémérides
!>S     ephemerides  :<EPH_ephemerides>   structure contenant les éphémérides
!
!$Common
!
!$Routines
!- eph_infoinit
!- MSP_signaler_message
!- eph_fic2code
!#V
!- EPH_acc_gettab
!- eph_acc_gettab
!#
!
!$Include
!
!$Module
!
!$Remarques
!   Le nom du repère des éphémérides est lu et stocké dans la structure.
!   Il s'agit des noms AMLIB.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      character(LEN=*), intent(IN) :: nomfic
      type(EPH_ephemerides), intent(OUT) :: ephemerides

      ! Variables locales
      integer :: acc,ier,ii,iexist,dim_mu, iostat
      character(LEN=EPHV_LGMAX), pointer, dimension(:) :: cplanetes
      character(LEN=EPHV_LGMAX) :: cechelle,ficconf

      ! Initialisations
      if(.not.ephv_acces_open) call eph_infoinit()

      ephemerides%eph_lu = .false.
      acc = acc_load ( nomfic)
      if ( acc < 0 ) then
         call MSP_signaler_message (cle_mes="EPH_OUVERTURE_FICHIER",&
              partie_variable=nomfic, &
              routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
         return
      endif
      
      ! Lecture des planètes considérées:
      
      call EPH_acc_gettab(acc, "Planetes", ephemerides%nb_corps, pvarc=cplanetes)
      if (MSP_gen_MESSAGES("EPH_lire_fictchebmad")) return

      if ( ASSOCIATED (ephemerides%corps) ) then
         DEALLOCATE (ephemerides%corps, stat=iostat)
      end if
      ALLOCATE (ephemerides%corps(ephemerides%nb_corps))
      do ii = 1 , ephemerides%nb_corps
         ficconf = trim(ephv_ephempath) // trim(ephv_fichierconf)
         call eph_fic2code(ficconf, cplanetes(ii), code=ephemerides%corps(ii))
      enddo
      if (associated(cplanetes)) deallocate(cplanetes)

      ! Lecture des "mu" des planètes considérées:

      call eph_acc_gettab (acc,"Mu_planetes", dim_mu, &
           pvard=ephemerides%mu, unite="m^3/s^2")
      if (MSP_gen_MESSAGES("EPH_lire_fictchebmad")) return

      ! Lecture du pas de temps
      iexist = acc_exist (acc,"Pas_tchebycheff")
      if ( iexist < 0 ) then
         call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                 partie_variable='de la variable Pas_tchebycheff', &
                 routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
         return
      else
         ier = acc_getd (acc,"Pas_tchebycheff",ephemerides%pas,"s")
         if ( ier /= 0 ) then
            call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                 partie_variable='de la variable Pas_tchebycheff', &
                 routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
            return
         endif
      endif

      ! Lecture du nom du repere
      ! DM 387 : integration CREATEPHEM
      iexist = acc_exist (acc,"Repere")
      ephemerides%repere="223"
      if ( iexist >= 0 ) then
         ier = acc_gets (acc,"Repere",ephemerides%repere)
         if ( ier /= 0 ) then
            call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                 partie_variable='de la variable Degre_tchebycheff', &
                 routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
            return
         endif
      endif


      ! Lecture du degré
      iexist = acc_exist (acc,"Degre_tchebycheff")
      if ( iexist < 0 ) then
         call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                 partie_variable='de la variable Degre_tchebycheff', &
                 routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
         return
      else
         ier = acc_geti (acc,"Degre_tchebycheff",ephemerides%deg_tcheb)
         if ( ier /= 0 ) then
            call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                 partie_variable='de la variable Degre_tchebycheff', &
                 routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
            return
         endif
      endif

      ! Lecture de l'échelle de temps
      iexist = acc_exist (acc,"Echelle_temps")
      if ( iexist < 0 ) then
         call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                 partie_variable='de la variable Echelle_temps', &
                 routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
         return
      else
         ier = acc_gets (acc,"Echelle_temps",cechelle)
         if ( ier /= 0 ) then
            call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                 partie_variable='de la variable Echelle_temps', &
                 routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
            return
         endif

         select case (cechelle)
         case ("TE" ) 
            ephemerides%ech_temps = cps_TE
         case ("TUC" )
            ephemerides%ech_temps = cps_TUC
         case ("TAI" )
            ephemerides%ech_temps = cps_TAI
         case default
            call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                 partie_variable='de la variable Echelle_temps', &
                 routine="EPH_lire_fictchebmad",type=MSP_ENUM_ERREUR)
            return
         endselect
      endif

      ! Lecture des dates:
      call eph_acc_gettab (acc,"Dates_tchebycheff",&
           ephemerides%nb_dates, pvard=ephemerides%dates)
      if (MSP_gen_MESSAGES("EPH_lire_fictchebmad")) return

      ! Lecture des coefficients:
      call eph_acc_gettab (acc,"Coefficients_tchebycheff",&
           ephemerides%nb_dates, pvard3=ephemerides%coeff)
      if (MSP_gen_MESSAGES("EPH_lire_fictchebmad")) return


      ephemerides%eph_lu = .true.

      ier = acc_close (acc)

   end subroutine EPH_lire_fictchebmad

   subroutine eph_acc_gettab(acc, nom, dim, pvard, pvard3, unite, pvarc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_acc_gettab
!
!$Resume
!  Lecture de tableaux MADONA
!
!$Description
!  D'après les fonctions de MSP_acc_gettab de la MECASPA.
!  Suivant les pointeurs présents, le tableau est lu en réel (pvard, pvard3),
!  ou string (pvarc). 
!  Les pointeurs sont affectés à des tableaux alloués à la taille du tableau
!  à lire.
!
!$Auteur
!  Florence VIVARES (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  call eph_acc_gettab(acc, nom, dim, [pvard], [pvard3], [unite], [pvarc])
!.    integer :: acc
!.    character(len=*) :: nom
!.    integer :: dim
!.    character(len=*) :: unite
!.    character(len=*), dimension(:), pointer :: pvarc
!.    real(kind=PM_REEL), dimension(:), pointer :: pvard
!.    real(kind=PM_REEL), dimension(:,:,:), pointer :: pvard3
!
!$Arguments
!>E     acc     :<integer>                       zone d'accès MADONA
!>E     nom     :<LEN=*>                         libellé de la variable à lire
!>S     dim     :<integer>                       dimension du tableau (première)
!>[E/S] pvard   :<PM_REEL,DIM=(:),pointer>       tableau 1D réel
!>[E/S] pvard3  :<PM_REEL,DIM=(:,:,:),pointer>   tableau 3D réel
!>[E]   unite   :<LEN=*>                         unité pour la lecture des réels
!>[E/S] pvarc   :<LEN=*,DIM=(:),pointer>         tableau 1D string
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
! Aucun test de typage. Si le type demandé ne correspond à ce qui est lu, il
! y a risque d'erreur à l'exécution.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     implicit none
     integer, intent(in) :: acc
     character(len=*), intent(in) :: nom
     integer, intent(out)  :: dim
     character(len=*), intent(in), optional :: unite
     character(len=*), dimension(:), pointer, optional :: pvarc
     real(kind=PM_REEL), dimension(:), pointer, optional :: pvard
     real(kind=PM_REEL), dimension(:,:,:), pointer, optional :: pvard3

     ! variables locales
     character(len=30) :: unitelue
     integer :: ii, jj, kk, ier, dim1, dim2, dim3, iostat

     ! Initialisations
     if (present(unite)) then 
        unitelue=""
        unitelue=trim(unite)
     else
        unitelue=""
     endif


     ! Selection
     ier = acc_exist (acc,nom)
     if ( ier < 0 ) then
        call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
             partie_variable='de la variable '//trim(nom)//' (absente)', &
             routine="eph_acc_gettab",type=MSP_ENUM_ERREUR)
        return
     endif
     
     dim=acc_get_dim(acc, nom)
     if ( ier <= 0 ) then
        call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
             partie_variable='de la variable '//trim(nom)//' (dimension)', &
             routine="eph_acc_gettab",type=MSP_ENUM_ERREUR)
        return
     endif
     ier = acc_select(acc,nom, ACC_TABL)
     if ( ier < 0 ) then
        call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
             partie_variable='de la variable '//trim(nom)//' (type)', &
             routine="eph_acc_gettab",type=MSP_ENUM_ERREUR)
        return
     endif

     ! Initialisations
     if (present(pvard)) then
        if (associated(pvard)) deallocate(pvard, stat=iostat)
        allocate(pvard(dim))
     endif
     if (present(pvarc)) then
        if (associated(pvarc)) deallocate(pvarc, stat=iostat)
        allocate(pvarc(dim))
     endif

     ! Boucle triple
     if (present(pvard3)) then
        dim1=dim
        ier = acc_set_index(acc,1)
        dim2=acc_get_dim(acc, ACC_INDEX)
        ier = acc_select(acc,ACC_INDEX, ACC_TABL)
        ier = acc_set_index(acc,1)
        dim3=acc_get_dim(acc, ACC_INDEX)
        if (associated(pvard3)) deallocate(pvard3, stat=iostat)
        allocate(pvard3(dim1,dim2,dim3))
        ier = acc_select_end(acc)        
        do ii=1, dim1
           ier = acc_set_index(acc,ii)
           if ( ier < 0 ) then
              call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                   partie_variable='de la variable '//trim(nom)// ' (index)', &
                   routine="eph_acc_gettab",type=MSP_ENUM_ERREUR)
              return
           endif

           ier = acc_select(acc,ACC_INDEX, ACC_TABL)

           do jj=1, dim2
              ier = acc_set_index(acc,jj)
              ier = acc_select(acc,ACC_INDEX, ACC_TABL)
              if ( ier < 0 ) then
                 call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                      partie_variable='de la variable '//trim(nom)// ' (index)', &
                      routine="eph_acc_gettab",type=MSP_ENUM_ERREUR)
                 return
              endif

              do kk=1, dim3
                 ier = acc_set_index(acc,kk)
                 ier = acc_getd(acc,ACC_INDEX,pvard3(ii,jj,kk),unitelue)
              enddo
              if ( ier < 0 ) then
                 call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                      partie_variable='de la variable '//trim(nom)// &
                      ' (valeur element)', &
                      routine="eph_acc_gettab",type=MSP_ENUM_ERREUR)
                 return
              endif
              ier = acc_select_end(acc)
           enddo
           ier = acc_select_end(acc)
        enddo
        ier = acc_select_end(acc)
        return
     endif

     ! Boucle de lecture simple
     do ii=1, dim
        ier = acc_set_index(acc,ii)
        if ( ier < 0 ) then
           call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                partie_variable='de la variable '//trim(nom)// ' (index)', &
                routine="eph_acc_gettab")
           return
        endif
        if(present(pvard)) ier = acc_getd(acc,ACC_INDEX,pvard(ii),trim(unitelue))
        if(present(pvarc)) ier = acc_gets(acc,ACC_INDEX,pvarc(ii))
        if ( ier < 0 ) then
           call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
                partie_variable='de la variable '//trim(nom)//' (valeur element)', &
                routine="eph_acc_gettab")
           return
        endif
     enddo

     ier = acc_select_end(acc)
     if ( ier < 0 ) then
        call MSP_signaler_message (cle_mes="EPH_ERREUR_LECTURE",&
             partie_variable='de la variable '//trim(nom)//' (type)', &
             routine="eph_acc_gettab")
        return
     endif


   end subroutine eph_acc_gettab

end module eph_tchebmad


