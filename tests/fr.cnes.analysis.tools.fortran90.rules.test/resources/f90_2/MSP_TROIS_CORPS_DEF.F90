module MSP_TROIS_CORPS_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_TROIS_CORPS_DEF
!
!$Resume
!  Module de définition de modèles de troisième corps.
!
!$Description
!  Module de définition de modèles de troisième corps.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_TROIS_CORPS_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!  30/03/1999
!
!$Historique
!  $Log: MSP_TROIS_CORPS_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.32  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.31  2008/08/08 15:07:35  gss
!  DM-ID 1058 : (portage g95) ajout du champs répertoire des fichiers epehemrides
!  dans la structure trois_corps et modification des fonctions d'initialisation,
!  modification, etc associées.
!  Suppression des variables non utilisées.
!
!  Revision 1.30  2008/07/04 14:55:46  huec
!  DM-ID 1058 : Gestion memoire, ajout d\'une nouvelle routine pour liberer la memoire d\'une structure MSP_EPHEMERIDES
!
!  Revision 1.29  2008/04/22 06:45:04  huec
!  AQ : Correction des commentaires sur les conversions d'unites
!
!  Revision 1.28  2008/04/09 15:44:24  huec
!  AQ : Correction d une coquille sur les unites des vitesses dans les commentaires de MSP_posvit_3corps
!
!  Revision 1.27  2008/03/26 13:38:33  huec
!  AQ : Suppression de variables inutilisees
!
!  Revision 1.26  2008/03/18 08:17:35  huec
!  DM-ID 859 : Utilisation de boucle explicites a la place des boucles implicites
!
!  Revision 1.25  2008/02/22 13:10:54  huec
!  AQ : Recuperation de l erreur dans MSP_posvit_3corps
!
!  Revision 1.24  2008/02/13 13:12:23  huec
!  DM-ID 11 : Appel a cps_get_sautTAITUC
!
!  Revision 1.23  2008/02/05 16:02:06  huec
!  DM-ID 11 : Suppression de la routine MSP_math_tchapp
!
!  Revision 1.22  2008/02/05 10:19:57  huec
!  DM-ID 11 : Utilisation d une structure eph_ephemerides dans MSP_ephemerides et utilisation des fonctions COMPAS equivalentes
!
!  Revision 1.21  2007/10/17 12:17:29  huec
!  DM-ID 733 : Extraction du module de calcul de forces de PSIMU
!
!  Revision 1.20  2007/10/15 12:01:39  huec
!  DM-ID 733 : Extraction du module de calcul de PSIMU : force due au troisieme corps
!
!  Revision 1.19  2007/06/18 10:14:26  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!
!  Revision 1.18  2007/06/15 13:43:48  vivaresf
!  FA-ID 746 : FA-ID 746 : mise en inout de la structure ephemerides pour éviter un écrasement mémoire dans la fonction de lecture
!
!  Revision 1.17  2007/02/02 10:34:43  vivaresf
!  Version 4.4a1 : variables inutilisées
!
!  Revision 1.16  2006/11/09 09:14:02  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!
!  Revision 1.15  2006/06/02 11:21:56  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!
!  Revision 1.14  2005/03/08 07:32:37  fabrec
!  DM-ID 111 : mise à jour des cartouches
!
!  Revision 1.13  2005/01/20 13:56:35  pauh
!  FA_332
!  Revision 1.12.2.1  2005/01/19 12:54:53  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.12  2005/01/06 09:30:18  vivaresf
!  FA_320
!  Revision 1.11.4.1  2005/01/06 09:29:56  vivaresf
!  FA-ID 320 : desallocation de la variables locale cplanetes
!  Revision 1.11  2003/09/17 09:50:00  adm_ipsi
!  FA-ID 41 : Utilisation  du message MSP_trois_corps_005
!  Revision 1.10  2003/02/18 17:44:28  adm_ipsi
!  maj cartouche
!  Revision 1.9  2003/02/18 17:27:46  adm_ipsi
!  MSP_posvit_3corps, ajout du paramètre nom_fic
!  Revision 1.8  2003/02/12 15:43:03  adm_ipsi
!  MSP_date_etutc_mspro renommé MSP_date_etutc
!  Revision 1.7  2003/02/06 15:41:16  adm_ipsi
!  MSP_lire_ephem_3corps, suppression de l'appel à AM_ipla_dico
!  Revision 1.6  2003/02/06 10:48:06  adm_ipsi
!  MSP_posvit_3corps, utilisation de MSP_math_tchapp au lieu de AM_math_tchapp
!  Revision 1.5  2003/01/07 18:11:41  adm_ipsi
!   suppression des variables non utilisées
!  Revision 1.4  2002/12/04 18:08:25  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.3  2002/12/03 18:09:00  adm_ipsi
!  MSP_posvit_3corps, Remplacement du goto 100 destiné à ne pas exécuter la boucle de recherche sur les indices de dates par des conditions permettant de ne pas exécuter cette boucle
!  Revision 1.2  2002/12/03 17:21:05  adm_ipsi
!   Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:09:36  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.9  2002/06/24 13:15:43  util_am
!  Récupération de indd dans idate en sortie
!  Revision 1.8  2002/05/03 07:54:04  util_am
!  Modifications dues au passage avec le compilateur 6.2 (=> NULL())
!  Revision 1.7  2002/03/27 08:58:00  util_am
!  Prise en compte de l'échelle de temps pour le calcul de la position/vitesse d'un troisième corps
!  Revision 1.6  2001/09/14 12:09:17  util_am
!  ajout des la routine de consultation de la structure ephemeride
!  Revision 1.5  2000/06/15 08:47:24  util_am
!  !  - Privatisation du contenu de la structure MSP_TROIS_CORPS
!  !  - Ajout des routines MSP_consulter_trois_corps, MSP_modifier_trois_corps
!  !  - Ajout des interfaces anglaises
!  !  - Mise à jour des cartouches
!  Revision 1.4  1999/11/05 14:34:02  util_am
!  Fermeture de la zone de moyen d'accès après lecture
!  Revision 1.3  1999/10/21 15:06:06  util_am
!  Ajout des routines permettant de lire et traiter des éphémérides pour le troisième corps
!  Revision 1.2  1999/10/19 11:35:20  util_am
!  Ajout de routines permettant de traiter des éphémérides
!  Création le 15/03/1999
!
!$FinHistorique
!
!$Usage
!  use MSP_TROIS_CORPS_DEF
!
!$Structure
!
!: MSP_EPHEMERIDES : 
!#V
!>     pas               : <pm_reel,private>                      pas des éphémérides [s]
!>     debut             : <pm_reel,private>                      date de début des éphémérides [JJ CNES]
!>     fin               : <pm_reel,private>                      date de fin des éphémérides [JJ CNES]
!>     deg_tcheb         : <integer,private>                      degré du polynome de Tchébitcheff
!>     nb_corps          : <integer,private>                      nombre de corps considérés dans les éphémérides
!>     nb_dates          : <integer,private>                      nombre de dates contenues dans les éphémérides
!>     corps             : <integer,DIM=(:),pointer,private>      tableau des numéros des corps considérés dans les éphémérides
!>     repere            : <MSP_TYPREP,private>                   repère dans lequel sont exprimées les éphémérides
!>     ech_temps         : <integer,private>                      échelle de temps utilisée pour les éphémérides
!>     dates             : <pm_reel,DIM=(:),pointer,private>      tableau des dates des éphémérides [JJ CNES]
!>     mu                : <pm_reel,DIM=(:),pointer,private>      tableau des constantes d'attraction des corps considérés dans les éphémérides
!>     coeff             : <pm_reel,DIM=(:,:,:),pointer,private>  tableau des coefficients de Tchébitcheff
!>     eph_lu            : <logical,private>                      booléen permettant de savoir si la structure éphéméride est bien remplie
!#
!
!: MSP_TROIS_CORPS : 
!#V
!>     nom_trois_corps   : <LEN=MSP_LONG_CHAINE,private>          nom du modèle
!>     soleil            : <integer,private>                      prise en compte du soleil si /= 0
!>     lune              : <integer,private>                      prise en compte de la lune si /= 0
!>     ficept            : <LEN=MSP_LONG_NOMFIC,private>          nom du fichier éphémérides
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_3rd_body
!- MSP_get_3rd_body_data
!- MSP_set_3rd_body_data
!- MSP_read_3rd_body_ephemeris
!- MSP_compute_3rd_body_orbit
!- MSP_compute_3rd_body_accel
!- MSP_consulter_ephemerides
!- MSP_consulter_trois_corps
!- MSP_modifier_trois_corps
!- MSP_lire_ephem_3corps
!- MSP_posvit_3corps
!- MSP_acc_3corps
!- MSP_calcule_acc_3corps
!
!$Fonctions
!- MSP_creer_trois_corps
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSPRO
!- MSP_GESTION_ERREUR
!- MSP_BULLETIN_DEF
!- MSP_MECASPA_DEF
!#
!
!$Interface
!> msp_create_3rd_body :          MSP_creer_trois_corps
!> msp_compute_3rd_body_orbit :   MSP_posvit_3corps
!> msp_compute_3rd_body_accel :   MSP_acc_3corps
!> msp_set_3rd_body_data :        MSP_modifier_trois_corps
!> msp_read_3rd_body_ephemeris :  MSP_lire_ephem_3corps
!> msp_get_3rd_body_data :        MSP_consulter_trois_corps
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  MSP_creer_trois_corps MSP_create_3rd_body MSP_get_3rd_body_data MSP_set_3rd_body_data
!.  MSP_read_3rd_body_ephemeris MSP_compute_3rd_body_orbit MSP_compute_3rd_body_accel MSP_consulter_ephemerides
!.  MSP_consulter_trois_corps MSP_modifier_trois_corps MSP_lire_ephem_3corps MSP_posvit_3corps
!.  MSP_acc_3corps
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSLIB, only : pm_reel
   use MSPRO
   use MSP_GESTION_ERREUR
   use MSP_BULLETIN_DEF
   use MSP_MECASPA_DEF
   use eph_tchebmad

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_TROIS_CORPS_DEF.F90 365 2013-02-18 12:36:19Z aadt $'


   type MSP_TROIS_CORPS

      private

      character(LEN=MSP_LONG_CHAINE)  :: nom_trois_corps

      integer :: soleil
      integer :: lune
      character(LEN=MSP_LONG_NOMFIC) :: ficept
      character(LEN=MSP_LONG_NOMFIC) :: dirept
      integer :: code_trois_corps
      real(kind=pm_reel) :: mu_trois_corps
      logical :: activation

   end type MSP_TROIS_CORPS

   type MSP_EPHEMERIDES

      private

      real(kind=pm_reel)                            :: debut,fin
      type (EPH_ephemerides)                        :: eph_ephemerides
      type(MSP_TYPREP)                              :: repere

   end type MSP_EPHEMERIDES

   
   interface MSP_create_3rd_body

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_3rd_body
!
!$Resume
!  Creates a 3rd body model structure 
!
!$Description
!  Creates a 3rd body model structure 
!
!$Acces
!  PUBLIC
!
!$Usage
!  trois_corps = MSP_create_3rd_body ([soleil],[lune],[ficept],[dirept],[nom_trois_corps],[code_trois_corps],[mu_trois_corps],[activation])
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    integer :: soleil,lune
!.    character(LEN=*) :: ficept,dirept,nom_trois_corps
!.    integer :: code_trois_corps
!.    real(kind=pm_reel) :: mu_trois_corps
!.    logical :: activation
!
!$Procedures
!- MSP_creer_trois_corps
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_trois_corps
   end interface

   interface MSP_get_3rd_body_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_3rd_body_data
!
!$Resume
!  Gets information on a 3rd body model characteristics
!
!$Description
!  Gets information on a 3rd body model characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_3rd_body_data(trois_corps, [soleil],[lune],[ficept],[nom_trois_corps],[code_trois_corps],[mu_trois_corps],[activation])
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    integer :: soleil,lune
!.    character(LEN=*) :: ficept,nom_trois_corps
!.    integer :: code_trois_corps
!.    real(kind=pm_reel) :: mu_trois_corps
!.    logical :: activation
!
!$Procedures
!- MSP_consulter_trois_corps
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_trois_corps
   end interface

   interface MSP_set_3rd_body_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_3rd_body_data
!
!$Resume
!  Modifies information on a 3rd body model characteristics
!
!$Description
!  Modifies information on a 3rd body model characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_3rd_body_data(trois_corps, [soleil],[lune],[ficept],[nom_trois_corps],[code_trois_corps],[mu_trois_corps],[activation])
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    integer :: soleil,lune
!.    character(LEN=*) :: ficept,nom_trois_corps
!.    integer :: code_trois_corps
!.    real(kind=pm_reel) :: mu_troi_corps
!.    logical :: activation
!
!$Procedures
!- MSP_modifier_trois_corps
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_trois_corps
   end interface

   interface MSP_read_3rd_body_ephemeris

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_read_3rd_body_ephemeris
!
!$Resume
!  reads 3rd body ephemeris
!
!$Description
!  reads 3rd body ephemeris
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_read_3rd_body_ephemeris (nomfic,ephemerides)
!.    character(LEN=*) :: nomfic
!.    type(MSP_EPHEMERIDES) :: ephemerides
!
!$Procedures
!- MSP_lire_ephem_3corps
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_lire_ephem_3corps
   end interface

   interface MSP_compute_3rd_body_orbit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_compute_3rd_body_orbit
!
!$Resume
!  Computes 3rd body orbit
!
!$Description
!  Computes 3rd body orbit
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_compute_3rd_body_orbit (date,numcorps,ephemerides,pos3c,vit3c,muc3,[idate],[dis3c],[dir3c],[echt])
!.    real(kind=pm_reel) :: date
!.    integer :: numcorps 
!.    type(MSP_EPHEMERIDES) :: ephemerides
!.    real(kind=pm_reel), dimension(3) :: pos3c,vit3c
!.    real(kind=pm_reel) :: muc3
!.    integer :: idate 
!.    real(kind=pm_reel) :: dis3c
!.    real(kind=pm_reel), dimension(3) :: dir3c
!.    integer :: echt 
!
!$Procedures
!- MSP_posvit_3corps
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_posvit_3corps
   end interface

   interface MSP_compute_3rd_body_accel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_compute_3rd_body_accel
!
!$Resume
!  Computes celestial body acceleration, direction and distance in the ephemeris reference frame
!
!$Description
!  Computes celestial body acceleration, direction and distance in the ephemeris reference frame
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_compute_3rd_body_accel (xsat,mu3c,pos3c,vit3c,dis3c,dir3c,acc3c)
!.    real(kind=pm_reel), dimension(3) :: xsat
!.    real(kind=pm_reel) :: mu3c
!.    real(kind=pm_reel), dimension(3) :: pos3c,vit3c
!.    real(kind=pm_reel) :: dis3c
!.    real(kind=pm_reel), dimension(3) :: dir3c,acc3c
!
!$Procedures
!- MSP_acc_3corps
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_acc_3corps
   end interface


   contains


     subroutine MSP_consulter_ephemerides(ephemerides,pas,debut,fin,deg_tcheb,nb_corps,nb_dates,&
          corps,repere,ech_temps,dates,mu,coeff,eph_lu)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_ephemerides
!
!$Resume
!	Routine de consultation de la structure ephemeride
!
!$Description
!	Routine de consultation de la structure ephemeride
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_ephemerides(ephemerides,[pas],[debut],[fin],[deg_tcheb],[nb_corps],[nb_dates],&
!.              [corps],[repere],[ech_temps],[dates],[mu],[coeff],[eph_lu])
!.    type(MSP_EPHEMERIDES) :: ephemerides
!.    real(kind=pm_reel) :: pas,debut,fin
!.    integer :: deg_tcheb,nb_corps,nb_dates
!.    integer, pointer, dimension(:) :: corps
!.    type(MSP_TYPREP) :: repere
!.    integer :: ech_temps
!.    real(kind=pm_reel), pointer, dimension(:) :: dates
!.    real(kind=pm_reel), pointer, dimension(:) :: mu
!.    real(kind=pm_reel), pointer, dimension(:,:,:) :: coeff
!.    logical :: eph_lu
!
!$Arguments
!>E     ephemerides  :<MSP_EPHEMERIDES>               structure ephemirides
!>[S]   pas          :<pm_reel>                       pas des ephemirides
!>[S]   debut        :<pm_reel>                       date de debut
!>[S]   fin          :<pm_reel>                       date de fin
!>[S]   deg_tcheb    :<integer>                       degre du polynome de tchebichiev
!>[S]   nb_corps     :<integer>                       nombre de corps
!>[S]   nb_dates     :<integer>                       nombre de dates
!>[E/S] corps        :<integer,DIM=(:),pointer>       tableau contenant les numero des diffenrents corps
!>[S]   repere       :<MSP_TYPREP>                    repere d'expression des ephemerides
!>[S]   ech_temps    :<integer>                       echelle de temps
!>[E/S] dates        :<pm_reel,DIM=(:),pointer>       tableau contenant les differentes dates
!>[E/S] mu           :<pm_reel,DIM=(:),pointer>       mu des differents corps
!>[E/S] coeff        :<pm_reel,DIM=(:,:,:),pointer>   coefficients de tchebichiev
!>[S]   eph_lu       :<logical>                       booleen indiquant si les pehemrides ont ete initialisees
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

       type(MSP_EPHEMERIDES),intent(in)                        ::ephemerides
       
       real(kind=pm_reel),intent(out)  ,optional               :: pas,debut,fin
       
       integer ,intent(out)     ,optional                      :: deg_tcheb,nb_corps,nb_dates
       integer, pointer, dimension(:)   ,optional              :: corps
       
       type(MSP_TYPREP)    ,intent(out)     ,optional          :: repere
       
       integer     ,intent(out)        ,optional               :: ech_temps
       real(kind=pm_reel), pointer, dimension(:)  ,optional    :: dates
       real(kind=pm_reel), pointer, dimension(:) ,optional     :: mu
       real(kind=pm_reel), pointer, dimension(:,:,:) ,optional :: coeff

       logical,optional    ,intent(out)                        :: eph_lu
       
       
       ! Variables locales
       integer :: MSP_iostat, ii, jj, kk

       MSP_iostat = 0

       if (present(pas)) then
          pas = ephemerides%eph_ephemerides%pas
       end if

       if (present(debut)) then
          debut=ephemerides%debut
       endif


       if ( present(fin)) then
          fin=ephemerides%fin
       end if


       if (present(deg_tcheb)) then
          deg_tcheb=ephemerides%eph_ephemerides%deg_tcheb
       end if

       if ( present(nb_corps)) then
          nb_corps=ephemerides%eph_ephemerides%nb_corps
       end if


       if ( present(nb_dates)) then
          nb_dates=ephemerides%eph_ephemerides%nb_dates
       end if


       if ( present(corps)) then
          if ( associated(corps)) then
             deallocate(corps,stat=MSP_iostat)
          endif
          if (ephemerides%eph_ephemerides%nb_corps /=0) then
             allocate(corps(size(ephemerides%eph_ephemerides%corps)))
             do ii = 1,size(ephemerides%eph_ephemerides%corps)
                corps(ii) = ephemerides%eph_ephemerides%corps(ii)
             end do
          end if
       end if


       if ( present(repere)) then
          repere=ephemerides%repere
       end if


       if ( present(ech_temps)) then
          ech_temps=ephemerides%eph_ephemerides%ech_temps
       end if


       if ( present(dates)) then
          if (associated(dates)) then
             deallocate(dates,stat=MSP_iostat)
          end if
          if (nb_dates /= 0) then
             allocate(dates(size(ephemerides%eph_ephemerides%dates)))
             do ii = 1,size(ephemerides%eph_ephemerides%dates)
                dates(ii) = ephemerides%eph_ephemerides%dates(ii)
             end do
          end if
       end if

       if ( present(mu)) then
          if (associated(mu)) then
             deallocate(mu,stat=MSP_iostat)
          end if
          if (nb_corps /= 0) then
             allocate(mu(size(ephemerides%eph_ephemerides%mu)))
             do ii = 1,size(ephemerides%eph_ephemerides%mu)
                mu(ii) = ephemerides%eph_ephemerides%mu(ii)
             end do
          end if
       end if

       if (present(coeff)) then
          if (associated(coeff)) then
             deallocate(coeff,stat=MSP_iostat)
          end if

          if ( nb_corps*nb_dates /= 0) then
             allocate(coeff(size(ephemerides%eph_ephemerides%coeff,dim=1),size(ephemerides%eph_ephemerides%coeff,dim=2),&
                  size(ephemerides%eph_ephemerides%coeff,dim=3)))
             do kk = 1,size(ephemerides%eph_ephemerides%coeff,dim=3)
                do jj = 1,size(ephemerides%eph_ephemerides%coeff,dim=2)
                   do ii = 1,size(ephemerides%eph_ephemerides%coeff,dim=1)
                      coeff(ii,jj,kk) = ephemerides%eph_ephemerides%coeff(ii,jj,kk)
                   end do
                end do
             end do
          end if
       end if

       if ( present(eph_lu)) then
          eph_lu = ephemerides%eph_ephemerides%eph_lu
       end if
     end subroutine MSP_consulter_ephemerides




   function MSP_creer_trois_corps (soleil,lune,ficept,dirept,nom_trois_corps,code_trois_corps, &
                                   mu_trois_corps,activation) result (trois_corps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_trois_corps
!
!$Resume
!  Création d'un modèle de troisième corps.
!
!$Description
!  Création d'un modèle de troisième corps.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  trois_corps = MSP_creer_trois_corps ([soleil],[lune],[ficept],[dirept],[nom_trois_corps],[code_trois_corps],[mu_trois_corps],[activation])
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    integer :: soleil,lune
!.    character(LEN=*) :: ficept,dirept,nom_trois_corps
!.    integer :: code_trois_corps
!.    real(kind=pm_reel) :: mu_trois_corps
!.    logical :: activation
!
!$Arguments
!>[E]   soleil           :<integer>           prise en compte du soleil si /= 0
!>[E]   lune             :<integer>           prise en compte de la lune si /= 0
!>[E]   ficept           :<LEN=*>             nom du fichier éphémérides 
!>[E]   dirept           :<LEN=*>             répertoire où se trouve le fichier éphémérides 
!>[E]   nom_trois_corps  :<LEN=*>             nom du modèle 
!>[E]   code_trois_corps :<integer>           Code du troisieme corps (code NAIF)
!>[E]   mu_trois_corps   :<pm_reel>           mu du troisieme corps
!>[E]   activation       :<logical>           Booleen de desactivation (besoin EXORSYST)
!>S     trois_corps      :<MSP_TROIS_CORPS>   Structure troisième corps créée
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- MSP_MECASPA_DEF
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

      use MSP_MECASPA_DEF

      implicit none

      type(MSP_TROIS_CORPS) :: trois_corps

      integer, optional, intent(IN) :: soleil,lune
      character(LEN=*), optional, intent(IN) :: ficept,dirept,nom_trois_corps
      integer, optional, intent(IN) :: code_trois_corps
      real(kind=pm_reel), optional, intent(IN) :: mu_trois_corps
      logical, optional, intent(IN) :: activation

      integer :: lnom

      character(LEN=80),dimension(2)       :: tmessage_var

      if ( present(nom_trois_corps) ) then
         lnom = LEN_TRIM(nom_trois_corps)
         if ( lnom <= MSP_LONG_CHAINE ) then 
            trois_corps%nom_trois_corps  = nom_trois_corps
         else
            trois_corps%nom_trois_corps  = nom_trois_corps(1:MSP_LONG_CHAINE)
            tmessage_var(1) = 'Le nom du modèle'
            write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_trois_corps",type=MSP_ENUM_WARNING,partie_variable=tmessage_var)
         endif
      else
         trois_corps%nom_trois_corps = ""
      endif

      if ( present(soleil) ) then
         trois_corps%soleil = soleil
      else
         trois_corps%soleil = MSP_ENUM_NUL
      endif

      if ( present(lune) ) then
         trois_corps%lune = lune
      else
         trois_corps%lune = MSP_ENUM_NUL
      endif

      if ( present(dirept) ) then
         lnom = LEN_TRIM(dirept)
         if ( lnom <= MSP_LONG_NOMFIC ) then 
            trois_corps%dirept = dirept
         else
            trois_corps%dirept = dirept(1:MSP_LONG_NOMFIC)
            tmessage_var(1) = 'Le nom du repertoire'
            write(tmessage_var(2),'(I8)')   MSP_LONG_NOMFIC

            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_trois_corps",type=MSP_ENUM_WARNING, &
               partie_variable=tmessage_var)
         endif
      else
         trois_corps%dirept = "."
      endif

      if ( present(ficept) ) then
         lnom = LEN_TRIM(ficept)
         if ( lnom <= MSP_LONG_NOMFIC ) then 
            trois_corps%ficept  = ficept
         else
            trois_corps%ficept  = ficept(1:MSP_LONG_NOMFIC)
            tmessage_var(1) = 'Le nom du fichier éphémérides'
            write(tmessage_var(2),'(I8)')   MSP_LONG_NOMFIC

            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_trois_corps",type=MSP_ENUM_WARNING, &
               partie_variable=tmessage_var)
         endif
      else
         trois_corps%ficept = ""
      endif

      if (present(code_trois_corps)) then
         trois_corps%code_trois_corps = code_trois_corps
      else
         trois_corps%code_trois_corps = -1 !Valeur artificielle
      endif

      if (present(mu_trois_corps)) then
         trois_corps%mu_trois_corps = mu_trois_corps
      else
         trois_corps%mu_trois_corps = 0._pm_reel
      endif

      if (present(activation)) then
         trois_corps%activation = activation
      else
         trois_corps%activation = .true.
      endif

   end function MSP_creer_trois_corps

   subroutine MSP_consulter_trois_corps(trois_corps, soleil,lune,ficept,dirept,nom_trois_corps, &
                                        code_trois_corps,mu_trois_corps,activation)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_trois_corps
!
!$Resume
!  Consulter les caractéristiques du modèle de troisième corps
!
!$Description
!  Consulter les caractéristiques du modèle de troisième corps
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_trois_corps(trois_corps, [soleil],[lune],[ficept],[nom_trois_corps],[code_trois_corps],[mu_trois_corps],[activation])
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    integer :: soleil,lune
!.    character(LEN=*) :: ficept,nom_trois_corps
!.    integer :: code_trois_corps
!.    real(kind=pm_reel) :: mu_trois_corps
!.    logical :: activation
!
!$Arguments
!>E     trois_corps      :<MSP_TROIS_CORPS>   Modèle de troisième corps à consulter
!>[S]   soleil           :<integer>           prise en compte du Soleil
!>[S]   lune             :<integer>           prise en compte de la Lune
!>[S]   ficept           :<LEN=*>             nom du fichier éphémérides
!>[S]   nom_trois_corps  :<LEN=*>             nom du modèle de troisième corps
!>[S]   code_trois_corps :<integer>           Code du troisieme corps (code NAIF)
!>[S]   mu_trois_corps   :<pm_reel>           mu du troisieme corps
!>[S]   activation       :<logical>           Booleen de desactivation
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
     
     type(MSP_TROIS_CORPS), intent(IN) :: trois_corps

     integer, optional, intent(OUT) :: soleil,lune
     character(LEN=*), optional, intent(OUT) :: ficept,dirept,nom_trois_corps
     integer, optional, intent(OUT) :: code_trois_corps
     real(kind=pm_reel), optional, intent(OUT) :: mu_trois_corps
     logical, optional, intent(OUT) :: activation

     if (present(soleil)) soleil = trois_corps%soleil
     if (present(lune))   lune   = trois_corps%lune
     if (present(ficept)) ficept = trois_corps%ficept
     if (present(dirept)) dirept = trois_corps%dirept
     if (present(nom_trois_corps)) nom_trois_corps = trois_corps%nom_trois_corps
     if (present(code_trois_corps)) code_trois_corps = trois_corps%code_trois_corps
     if (present(mu_trois_corps)) mu_trois_corps = trois_corps%mu_trois_corps
     if (present(activation)) activation = trois_corps%activation

   end subroutine MSP_consulter_trois_corps

   subroutine MSP_modifier_trois_corps(trois_corps, soleil,lune,ficept,dirept, &
        nom_trois_corps,code_trois_corps,mu_trois_corps,activation)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_trois_corps
!
!$Resume
!  Modifier les caractéristiques du modèle de troisième corps
!
!$Description
!  Modifier les caractéristiques du modèle de troisième corps
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_trois_corps(trois_corps, [soleil],[lune],[ficept],[nom_trois_corps],[code_trois_corps],[mu_trois_corps],[activation])
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    integer :: soleil,lune
!.    character(LEN=*) :: ficept,nom_trois_corps
!.    integer :: code_trois_corps
!.    real(kind=pm_reel) :: mu_trois_corps
!.    logical :: activation
!
!$Arguments
!>E/S   trois_corps      :<MSP_TROIS_CORPS>   Modèle de troisième corps à consulter
!>[E]   soleil           :<integer>           prise en compte du Soleil
!>[E]   lune             :<integer>           prise en compte de la Lune
!>[E]   ficept           :<LEN=*>             nom du fichier éphémérides
!>[E]   nom_trois_corps  :<LEN=*>             nom du modèle de troisième corps
!>[E]   code_trois_corps :<integer>           Code du troisieme corps
!>[E]   mu_trois_corps   :<pm_reel>           mu du troisieme corps
!>[E]   activation       :<logical            Booleen de desactivation
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
     
     type(MSP_TROIS_CORPS), intent(INOUT) :: trois_corps

     integer, optional, intent(IN) :: soleil,lune
     character(LEN=*), optional, intent(IN) :: ficept,dirept,nom_trois_corps
     integer, optional, intent(IN) :: code_trois_corps
     real(kind=pm_reel), optional, intent(IN) :: mu_trois_corps
     logical, optional, intent(IN) :: activation

     if (present(soleil)) trois_corps%soleil = soleil
     if (present(lune))   trois_corps%lune = lune
     if (present(ficept)) trois_corps%ficept = ficept
     if (present(dirept)) trois_corps%dirept = dirept
     if (present(nom_trois_corps)) trois_corps%nom_trois_corps = nom_trois_corps
     if (present(code_trois_corps)) trois_corps%code_trois_corps = code_trois_corps
     if (present(mu_trois_corps)) trois_corps%mu_trois_corps = mu_trois_corps
     if (present(activation)) trois_corps%activation = activation

   end subroutine MSP_modifier_trois_corps

   subroutine MSP_lire_ephem_3corps (nomfic,ephemerides)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_lire_ephem_3corps
!
!$Resume
!  Lecture d'un fichier éphéméride au format MADONA.
!
!$Description
!  Lecture d'un fichier éphéméride au format MADONA.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_lire_ephem_3corps (nomfic,ephemerides)
!.    character(LEN=*) :: nomfic
!.    type(MSP_EPHEMERIDES) :: ephemerides
!
!$Arguments
!>E     nomfic       :<LEN=*>             nom du fichier où se trouvent les éphémérides
!>S     ephemerides  :<MSP_EPHEMERIDES>   structure contenant les éphémérides
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_acc_get_tab
!
!$Include
!
!$Module
!
!$Remarques
!   Pour l'instant, on considère que le repère des éphémérides est l'EME2000
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      character(LEN=*), intent(IN) :: nomfic
      type(MSP_EPHEMERIDES), intent(INOUT) :: ephemerides

      ! Pour l'instant, on ne lit pas les données d'entête sur le repère ...

      call EPH_lire_fictchebmad (nomfic,ephemerides%eph_ephemerides)
      if (MSP_gen_messages ("MSP_lire_ephem_3corps")) return

   return
   end subroutine MSP_lire_ephem_3corps

   subroutine MSP_posvit_3corps (date,numcorps,ephemerides,pos3c,vit3c,muc3,idate,dis3c,dir3c,echt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_posvit_3corps
!
!$Resume
!  Calcul de la position et de la vitesse d'un corps celeste dans le repère des  éphémérides.
!
!$Description
!  Calcul de la position et de la vitesse d'un corps celeste dans le repère des  éphémérides.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_posvit_3corps (date,numcorps,ephemerides,pos3c,vit3c,muc3,[idate],[dis3c],[dir3c],[echt])
!.    real(kind=pm_reel) :: date
!.    integer :: numcorps 
!.    type(MSP_EPHEMERIDES) :: ephemerides
!.    real(kind=pm_reel), dimension(3) :: pos3c,vit3c
!.    real(kind=pm_reel) :: muc3
!.    integer :: idate 
!.    real(kind=pm_reel) :: dis3c
!.    real(kind=pm_reel), dimension(3) :: dir3c
!.    integer :: echt 
!
!$Arguments
!>E     date         :<pm_reel>           date à laquelle on veut calculer les position-vitesse d'un corps [JJ CNES]
!>E     numcorps     :<integer>           numéro du corps considéré
!>E     ephemerides  :<MSP_EPHEMERIDES>   structure contenant les éphémérides
!>S     pos3c        :<pm_reel,DIM=(3)>   tableau contenant la position du corps dans le repère des éphémérides [m]
!>S     vit3c        :<pm_reel,DIM=(3)>   tableau contenant la vitesse du corps dans le repère des éphémérides [m/j]
!>S     muc3         :<pm_reel>           constante d'attraction du corps considéré [m^3/s^2]
!>[E/S] idate        :<integer>           indice de la précédente date (optimisation de la recherche)
!>[S]   dis3c        :<pm_reel>           distance du corps dans le repère des éphémérides [m]
!>[S]   dir3c        :<pm_reel,DIM=(3)>   cosinus directeurs du corps dans le repère des éphémérides [-]
!>[E]   echt         :<integer>           échelle de temps de la date (pm_TUC ou pm_TE)
!.                                        par défaut pm_TE
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_date_etutc
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
      type(MSP_EPHEMERIDES), intent(IN) :: ephemerides
      real(kind=pm_reel), intent(OUT), dimension(3) :: pos3c,vit3c
      real(kind=pm_reel), intent(OUT) :: muc3
      integer, intent(INOUT), optional :: idate 
      real(kind=pm_reel), intent(OUT), optional :: dis3c
      real(kind=pm_reel), intent(OUT), optional, dimension(3) :: dir3c
      integer, intent(IN), optional :: echt 
      integer :: echt_tmp, ii

      ! Conversion de PM_TUC en CPS_TUC si besoin
      if (present(echt)) then
         if (echt.eq.PM_TUC) then
            echt_tmp = CPS_TUC
         else
            echt_tmp = CPS_TE
         endif
      else
         echt_tmp = CPS_TE   
      endif
      call eph_posvit_3corps(date,numcorps,ephemerides%eph_ephemerides,pos3c,vit3c,idate=idate, &
           mu3c=muc3,dis3c=dis3c,dir3c=dir3c,echt=echt_tmp)
      if (MSP_gen_messages ("MSP_posvit_3corps")) return
      ! Conversion des vitesses de m/s a m/j pour compatibilite ascendante
      do ii =1,3
         vit3c(ii) = vit3c(ii)*86400
      end do

   end subroutine MSP_posvit_3corps

   subroutine MSP_acc_3corps (xsat,mu3c,pos3c,dis3c,dir3c,acc3c)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_3corps
!
!$Resume
!  Calcul de l'accélération d'un corps celeste dans le repère des éphémérides ainsi que de la direction et de la distance.
!
!$Description
!  Calcul de l'accélération d'un corps celeste dans le repère des éphémérides ainsi que de la direction et de la distance.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_acc_3corps (xsat,mu3c,pos3c,dis3c,dir3c,acc3c)
!.    real(kind=pm_reel), dimension(3) :: xsat
!.    real(kind=pm_reel) :: mu3c
!.    real(kind=pm_reel), dimension(3) :: pos3c
!.    real(kind=pm_reel) :: dis3c
!.    real(kind=pm_reel), dimension(3) :: dir3c,acc3c
!
!$Arguments
!>E     xsat   :<pm_reel,DIM=(3)>   position du satellite dans le repère de l'éphéméride [m]
!>E     mu3c   :<pm_reel>           constante d'attraction du corps considéré [m^3/s^2]
!>E     pos3c  :<pm_reel,DIM=(3)>   position du corps dans le repère des éphémérides [m]
!>S     dis3c  :<pm_reel>           distance corps-satellite [m]
!>S     dir3c  :<pm_reel,DIM=(3)>   coefficients directeurs corps-satellite [-]
!>S     acc3c  :<pm_reel,DIM=(3)>   tableau contenant l'accélération dû au corps considéré dans le repère des éphémérides [m/s^2]
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

      real(kind=pm_reel), intent(IN), dimension(3) :: xsat
      real(kind=pm_reel), intent(IN) :: mu3c
      real(kind=pm_reel), intent(IN), dimension(3) :: pos3c

      real(kind=pm_reel), intent(OUT) :: dis3c
      real(kind=pm_reel), intent(OUT), dimension(3) :: dir3c,acc3c

      real(kind=pm_reel), dimension(3) :: acc3c_tmp
      real(kind=pm_reel) :: dcenpla3s2
      integer :: ii

      ! Direction Satellite - Corps
      do ii = 1,3
         dir3c(ii) = pos3c(ii) - xsat(ii)
      end do
      dis3c = sqrt(dir3c(1)*dir3c(1)+dir3c(2)*dir3c(2)+dir3c(3)*dir3c(3))
      do ii = 1,3
         dir3c(ii) = dir3c(ii)/dis3c
      end do
  
      !Calcul de l'accélération sans accélération d'inertie:
      do ii = 1,3
         acc3c_tmp(ii) = mu3c * dir3c(ii) / (dis3c**2)
      end do

      !Calcul de l'accélération  totale aprés soustraction de l'accélération d'inertie
      dcenpla3s2 = (pos3c(1)**2+pos3c(2)**2+pos3c(3)**2)**1.5_pm_reel
      do ii = 1,3
         acc3c(ii) = acc3c_tmp(ii) - ((mu3c * pos3c(ii)) / dcenpla3s2)
      end do

   end subroutine MSP_acc_3corps

   subroutine MSP_calculer_acc_3corps(trois_corps,pos,acc,pos3corps,ephemerides, &
                                      date,idate,echt)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_acc_3corps
!
!$Resume
!  Calcul de l'accélération due a la force du troisieme corps
!
!$Description
!  Calcul de l'accélération due a la force du troisieme corps
!
!$Auteur
!  Camille Hue
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_3corps (trois_corps,pos,acc,pos3corps,ephemerides)
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    real(kind=pm_reel), dimension(3) :: pos,acc
!.    real(kind=pm_reel), dimension(3), optional :: pos3corps
!.    type(MSP_EPHEMERIDES), optional :: ephemerides
!.    real(kind=pm_reel) :: date
!
!$Arguments
!>E     trois_corps :<type(MSP_TROIS_CORPS)>   Structure MSP_TROIS_CORPS de la configuration
!>E     pos         :<pm_reel,DIM=(3)>         Position du vehicule par rapport au corps central [m]
!>S     acc         :<pm_reel,DIM=(3)>         Acceleration due a la force du 3eme corps [m/s^2]
!>[E/S] pos3corps   :<pm_reel,DIM=(3)>         Position du troisieme corps, obligatoire en entree si ephemerides est absent
!                                              ou en sortie optionnelle si ehemerides est present
!>[E]   ephemerides :<type(MSP_EPHEMERIDES)>   Structure d'ephemerides du troisieme corps, optionnel
!>[E]   date        :<pm_reel>                 Date du calcul dans le cas ou on travaille a partir d'ephemerides
!>[E/S] idate       :<integer>                 code permettant d'optimiser l'accès a MSP_EPHEMERIDES
!>[E]   echt        :<integer>                 echelle de temps pour la date (PM_TUC ou PM_TE)
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

      type(MSP_TROIS_CORPS), intent(IN)             :: trois_corps
      real(kind=pm_reel), intent(IN), dimension(3)  :: pos

      real(kind=pm_reel), intent(OUT), dimension(3) :: acc

      real(kind=pm_reel), intent(INOUT), optional   :: pos3corps(3)
      type(MSP_EPHEMERIDES), intent(IN), optional   :: ephemerides
      real(kind=pm_reel), intent(IN), optional      :: date
      integer, intent(INOUT), optional              :: idate
      integer, intent(IN), optional                 :: echt

      integer                                       :: pos3corps_a_determiner, code_3corps
      character(LEN=80)                             :: tmessage_var
      real(kind=pm_reel)                            :: mu_3corps,dis3c,dir3c(3),vit3c(3)
      real(kind=pm_reel), dimension(3)              :: pos_3corps_opt

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Position du troisieme corps !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      pos3corps_a_determiner = 0
      if (present(ephemerides)) then
         pos3corps_a_determiner = 1
         ! il faut qu'il y ait une date
         if (.not. present(date)) then
            write(tmessage_var,'(a)') "Manque un argument"
            call MSP_signaler_message (cle_mes="MSP_ARGUMENTS",routine="MSP_calculer_acc_3corps", &
                                       type=MSP_ENUM_ERREUR,partie_variable=tmessage_var)
         endif
      else
         ! Si ephemerides et pos3corps sont absents, c'est une erreur
         if (.not. present(pos3corps)) then
            write(tmessage_var,'(a)') "Manque un argument"
            call MSP_signaler_message (cle_mes="MSP_ARGUMENTS",routine="MSP_calculer_acc_3corps", &
                                       type=MSP_ENUM_ERREUR,partie_variable=tmessage_var)
         endif
      endif

      if (pos3corps_a_determiner == 1) then
         ! On doit lire la position du 3eme corps dans la structure trois_corps
         ! On a besoin du numero de corps (code NAIF)
         call MSP_consulter_trois_corps(trois_corps,code_trois_corps=code_3corps)
         if (present(pos3corps)) then
            call MSP_posvit_3corps(date,code_3corps,ephemerides,pos3corps,vit3c,mu_3corps, &
                      idate=idate,echt=echt)
         else
            call MSP_posvit_3corps(date,code_3corps,ephemerides,pos_3corps_opt,vit3c,mu_3corps, &
                       idate=idate,echt=echt)
         endif
      endif

      ! On recupere le mu du 3eme corps, si besoin
      if (pos3corps_a_determiner == 0) then
         call MSP_consulter_trois_corps(trois_corps,mu_trois_corps=mu_3corps)
      endif

      ! Calcul de l'acceleration
      if (present(pos3corps)) then
!         write(*,'(a,3(g21.12))') "pos = ",pos
!         write(*,'(a,g21.12)') "mu_3corps = ",mu_3corps
!         write(*,'(a,3(g21.12))') "pos3corps = ",pos3corps
         call MSP_acc_3corps(pos,mu_3corps,pos3corps,dis3c,dir3c,acc)
!         write(*,'(a,g21.12)') "dis3c = ",dis3c
!         write(*,'(a,3(g21.12))') "dir3c = ",dir3c
!         write(*,'(a,3(g21.12))') "acc = ",acc
      else
!         write(*,'(a,3(g21.12))') "pos = ",pos
!         write(*,'(a,g21.12)') "mu_3corps = ",mu_3corps
!         write(*,'(a,3(g21.12))') "pos3corps = ",pos_3corps_opt
         call MSP_acc_3corps(pos,mu_3corps,pos_3corps_opt,dis3c,dir3c,acc)
!         write(*,'(a,g21.12)') "dis3c = ",dis3c
!         write(*,'(a,3(g21.12))') "dir3c = ",dir3c
!         write(*,'(a,3(g21.12))') "acc = ",acc
      endif

    end subroutine MSP_calculer_acc_3corps

    subroutine MSP_effacer_ephemerides(ephemerides)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_ephemerides
!
!$Resume
!  Liberation de la memoire occupee par une structure MSP_EPHEMERIDES
!
!$Description
!  Liberation de la memoire occupee par une structure MSP_EPHEMERIDES
!
!$Auteur
!  Camille Hue
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_ephemerides
!
!$Arguments
!>E/S     ephemerdides :<type(MSP_EPHEMERIDES)>   Structure MSP_EPHEMERIDES
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

      type(MSP_EPHEMERIDES), intent(INOUT)             :: ephemerides

      ephemerides%debut = 0._pm_reel
      ephemerides%fin = 0._pm_reel

      if (associated(ephemerides%eph_ephemerides%corps)) deallocate(ephemerides%eph_ephemerides%corps)
      if (associated(ephemerides%eph_ephemerides%dates)) deallocate(ephemerides%eph_ephemerides%dates)
      if (associated(ephemerides%eph_ephemerides%mu)) deallocate(ephemerides%eph_ephemerides%mu)
      if (associated(ephemerides%eph_ephemerides%coeff)) deallocate(ephemerides%eph_ephemerides%coeff)
      

    end subroutine MSP_effacer_ephemerides

end module MSP_TROIS_CORPS_DEF
