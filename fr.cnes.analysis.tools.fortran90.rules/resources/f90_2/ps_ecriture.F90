module ps_ecriture

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_ecriture
!
!$Resume
!  Module regroupant les données et les sous-programmes d'écriture des résultats.
!
!$Description
!  Module regroupant les données et les sous-programmes d'écriture des résultats
!  (dans les fichiers EPHEM ou EVENT).
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_ecriture.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_ecriture.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.40  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.39  2009/11/19 15:52:46  mercadig
!  DM-ID 1019: Gestion du repere Gamma vrai de la date
!
!  Revision 1.38  2009/10/28 16:06:37  mercadig
!  DM-ID 1019: Gestion du RIS Gamma vrai de la date
!
!  Revision 1.37  2009/10/19 07:46:26  kvernelo
!  VERSION:9.5:FA-ID:1324:19/10/2009:Ecriture du repere en francais
!
!  Revision 1.36  2009/04/14 16:04:04  tanguyy
!  FA-ID 1260 : simplification du format d ecriture
!
!  Revision 1.35  2008/12/02 16:46:38  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.34  2008/10/24 09:38:51  huec
!  DM-ID 1058 : Initialisations
!  Revision 1.33  2008/10/17 10:03:51  mercadig
!  DM-ID 1058 Initialisations de variables et traitement type evenement 15 et 16 dans psweve
!  Revision 1.32  2008/10/15 12:48:41  tanguyy
!  DM-ID 1058 : Controles sur les acces MADONA
!  Revision 1.31  2008/09/04 07:52:59  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.30  2008/05/02 07:57:25  tanguyy
!  FA-ID 798 : utilisation de psnum_version
!  Revision 1.29  2008/04/04 13:37:08  ttn
!  FA-ID 940 : Comparaison de deux entites physiques reelles
!  Revision 1.27  2008/04/03 14:32:41  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.26  2008/03/07 09:59:41  huec
!  DM-ID 859 : tests des code_erreur
!  Revision 1.25  2007/12/06 15:27:58  huec
!  FA-ID 816 : Suppression d un appel systeme
!  Revision 1.24  2007/10/30 08:45:33  huec
!  DM-ID 744 : Modele d atmosphere de Venus a implementer dans PSIMU
!  Revision 1.23  2007/10/02 08:01:57  tanguyy
!  DM-ID 733 - utilisationd du calcul du potentiel de la MECASPA
!  Revision 1.22  2007/09/24 15:06:16  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.21  2007/08/22 12:17:00  tanguyy
!  FA-ID 781 : correction des FA 779 / 781
!  Revision 1.20  2007/07/20 08:36:09  tanguyy
!  FA-ID 779 / FA-ID 781 : gestion correcte des écritures de fichiers lorsque l'on utilise plusieurs véhicules
!  Revision 1.19  2007/07/09 11:58:36  tanguyy
!  DM-ID 688 : mode Terre/Mars basé sur le code NAIF
!  Revision 1.18  2007/03/19 14:56:43  mle
!  DM-ID 600 : permettre de traiter les ephemerides PSIMU par SORTILEGE
!  Revision 1.17  2006/10/17 09:54:22  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.16  2006/05/30 09:32:17  tanguyy
!  DM-ID 232 : nommage des parametres optionnels
!  Revision 1.15  2006/04/10 14:51:14  tanguyy
!  FA-ID 524 : Cloture du FT (Integration a rebours et utilisation des sorties a pas fixes)
!  Revision 1.14.2.1  2006/04/10 14:51:01  tanguyy
!  FA-ID 524 : gestion de str_ecr(iveh)%nsor modifiee
!  Revision 1.14  2006/03/15 13:25:18  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.13  2006/02/27 15:13:46  tanguyy
!  FA-ID 482 : allocation dynamique du tableau des dates de sortie
!  Revision 1.12  2005/11/10 18:37:05  vivaresf
!  Mise à jour des cartouches
!  Revision 1.11  2005/02/17 13:57:31  fabrec
!  DM-ID 98 : pas des ephemerides
!  Revision 1.10  2005/01/28 10:38:15  fabrec
!  maj cartouches
!  Revision 1.9  2004/05/26 14:56:35  adm_ipsi
!  DM-ID 97 : Unités dans les fichiers de sortie
!  Revision 1.8  2003/09/16 15:39:42  adm_ipsi
!  FA-ID 47, Ajout du blanc apres le # des commentaires
!  Revision 1.7  2003/07/03 15:04:37  adm_ipsi
!  FA-ID 19 : Correction du commentaire de ific dans le cartouche de pswresu
!  Revision 1.9  2003/03/28 09:56:46  util_am
!  SLB - Version industrialisée
!  Revision 1.6  2003/02/07 11:36:59  boschett
!  A Deramecourt : modification de l'appel de mucdat (mslib77?) en appel a md_duree_jhms
!  Revision 1.5  2003/01/28 16:22:04  boschett
!  A Deramecourt : modification sorties madona pour fichier evenements
!  Revision 1.4  2003/01/16 17:52:43  boschett
!  A Deramecourt : mise à jour au format MADONA (partielle)
!  Revision 1.3  2002/12/04 14:23:27  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.2  2002/11/26 16:06:57  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.8  2001/11/20 16:51:37  util_am
!  Gestion des chiffres significatifs pour la date en TE
!  Revision 1.7  2000/07/18 13:40:20  util_am
!  Modification de l'affichage à l'écran
!  Revision 1.6  2000/06/21 15:28:59  util_am
!  Gestion des ressources (fichiers de donnees)
!  Revision 1.5  2000/05/29 09:11:24  util_am
!  Utilisation de Domtraduire
!  Revision 1.4  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.3  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:11  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_ecriture
!
!$Structure
!
!: PS_STR_INT_ECRITURE : 
!>     nvarw            : <integer>                  nombre de variables à écrire dans les fichiers EPHEM/EVENT
!>     iresw            : <integer,DIM=(nvmax)>      numéro d'ordre des variables à écrire dans les fichiers EPHEM/EVENT
!>     indw             : <integer>                  indice d'écriture dans le fichier EPHEM (modulo indw)
!>     iforw            : <integer>                  format d'écriture des variables à écrire dans le fichier EPHEM
!>     etatn            : <LEN=10,DIM=(nvmax)>       tableau des noms des variables
!>     unitn            : <LEN=7,DIM=(nvmax)>        
!>     ipas             : <integer>                  type de pas de sortie (1 = multiple pas integ, 2 = pas fixe)
!>     pas_sortie       : <pm_reel>                  valeur du pas de sortie en sec
!>     nb_sorties       : <integer>                  nb de sorties sur l'intervalle d'étude
!>     nsor             : <integer>                  indice d'écriture (pas de sortie courant)
!>     ypas_sorties     : <pm_reel,DIM=(:),pointer>  tableau dynamique des dates de sortie
!>     nevecour         : <integer>                  numero de l'événement
!>     type_de_sortie   : <integer>                  SORTILEGE
!
!$Global
!
!>  str_ecr   : <PS_STR_INT_ECRITURE,DIM=(PS_NVMAX)>  type structure ecriture
!$Common
!
!$Routines
!- pswresu
!- pslnvar
!#V
!- pswentetes
!- psweve
!#
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_integration_don
!- ps_bulletin
!- ps_troiscorps
!- ps_evenements
!- cps_utilisateur
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MECASPA
   use ps_generalites

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_ecriture.F90 368 2013-02-19 14:43:59Z aadt $'


   type PS_STR_INT_ECRITURE
      integer::  nvarw
      integer,dimension(nvmax) ::  iresw
      integer::  indw
      integer::  iforw
      character(LEN=10), dimension(nvmax) :: etatn
      character(LEN=7), dimension(nvmax) :: unitn
      integer :: ipas                 ! type de pas de sortie 
                                      !(1 = multiple pas integration ; 2 = pas fixe)
      real (KIND=pm_reel) :: pas_sortie
      integer :: nb_sorties
      integer :: nsor
      real (KIND=pm_reel), dimension(:), pointer :: ypas_sorties => NULL()
      integer :: nevecour = 0
      integer :: type_de_sortie = 1 ! type de sortie 
                                    !(1 = personnalisé ; 2 = SORTILEGE)
   end type PS_STR_INT_ECRITURE

   type (PS_STR_INT_ECRITURE), dimension(PS_NVMAX), save :: str_ecr

   private pswentetes, psweve

   contains


      subroutine pswresu (iecr,ific,ifla,ieve,iter,ypas,etat,nvarw,iresw,iforw,ifore,neve,aeve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pswresu
!
!$Resume
!  Ecriture des résultats à l'écran et dans les fichiers
!
!$Description
!  Ecriture des résultats à l'écran et dans les fichiers
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pswresu (iecr,ific,ifla,ieve,iter,ypas,etat,nvarw,iresw,iforw,ifore,neve,aeve)
!.    integer :: iecr,ific,ifla,ieve,iter,nvarw,iresw(*),iforw,ifore,neve
!.    real (KIND=pm_reel) :: ypas,etat(*)
!.    character (LEN=80) :: aeve(*)
!
!$Arguments
!>E     iecr   :<integer>           indique où écrire:
!.                                  1 -> fichier EPHEM
!.                                  2 -> fichier EVENT
!.                                  0 -> pas d'écriture
!>E     ific   :<integer>           numéro logique de la zone d'accès MADONA  sur laquelle on va écrire
!>E     ifla   :<integer>           indice permettant ou non d'écrire à l'écran:
!.                                  0 -> non
!.                                  1 -> oui
!>E     ieve   :<integer>           type d'évenement:
!.                                  1  -> date de début de simulation
!.                                  10 -> début de poussée impulsionnelle
!.                                  11 -> fin de poussée impulsionnelle
!.                                  12 -> début de poussée continue
!.                                  13 -> fin de poussée continue
!.                                  14 -> réservoirs vides
!.                                  20 -> début de séparation
!.                                  21 -> fin de séparation
!.                                  30 -> date de fin de simulation
!.                                  31 -> altitude de fin de simulation
!.                                  40 -> évenement date
!>E     iter   :<integer>           numéro de l'itération
!>E     ypas   :<pm_reel>           temps écoulé depuis le début de la simulation
!>E     etat   :<pm_reel,DIM=(*)>   tableau des variables courantes
!>E     nvarw  :<integer>           nombre de variables à écrire dans les fichiers EPHEM/EVENT
!>E     iresw  :<integer,DIM=(*)>   numéro d'ordre des variables à écrire dans les fichiers EPHEM/EVENT
!>E     iforw  :<integer>           format d'écriture des variables à écrire dans le fichier EPHEM
!>E     ifore  :<integer>           format d'écriture des variables à écrire dans le fichier EVENT
!>E     neve   :<integer>           numéro de l'événement
!>E     aeve   :<LEN=80,DIM=(*)>    commentaire pour le fichier EVENT
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_julien_calend
!- md_duree_jhms
!- flush
!#V
!- pswentetes
!- psweve
!#
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!- ps_bulletin
!- ps_generalites
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_integration_don
      use ps_bulletin
      use ps_generalites

      implicit none

      integer, intent(IN)             :: iecr,ific,ifla,ieve,iter,nvarw,iresw(*),iforw,ifore,neve
      real (KIND=pm_reel), intent(IN) :: ypas,etat(*)
      character (LEN=80), intent(IN)  :: aeve(*)

      integer :: init,i,iform,ijour,iheure,imin,annee,mois,jour,heure,minutes
      integer :: retres
      real (KIND=pm_reel) :: rsec,secondes,date_te
      character (LEN=2)   :: format_col
      character (LEN=149) :: ligne_comment
      character (LEN=150) :: ligne_com

      type (tm_code_retour) :: code_erreur
      type (tm_jour_sec) :: jul1950

      !*** FA-ID 779 : variables permettant le traitement des écritures de fichiers
      integer :: indice,ii,dernier_lu
      !***
      
      ! Initialisations
      retres = 0

      if ( iecr == 0 ) goto 999

      if ( (str_ecr(iveh)%ipas == 2) .and. (iecr == 1) ) then
         if(abs(ypas-str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor)) > PS_EPSILON) then
            goto 999
         endif
      endif

      if ( nvarw == 0 ) goto 998

!*** FA-ID 779
! recherche si le fichier a déjà été lu
      indice = -1
      dernier_lu = 1
      do ii=1, ps_nvmax
         if (tab_fic_lu(1,ii)==ific) indice =ii
         if (ii > 1) then
            if ((tab_fic_lu(1,ii)==0).and.(tab_fic_lu(1,ii-1)/=0)) dernier_lu =ii
         end if
      end do
! si il n'a pas encore été lu, on remplit tab_fic_lu
      if (indice==-1) then
         indice = dernier_lu
         tab_fic_lu(1,indice) = ific
      end if
! initialisation de init (1 si entetes écrites, 0 sinon)
      init = tab_fic_lu(2,indice)
!***

      ! DM-ID 600 : rajout de la variable 117, NUM_EVENT
      ! MAJ de cette variable
      if (iecr == 2) then
         str_ecr(iveh)%nevecour = str_ecr(iveh)%nevecour + 1
         str_gen(iveh)%etat(117) = str_ecr(iveh)%nevecour
      else
         str_gen(iveh)%etat(117) = 0
      end if


!     Ecriture de l'entete des fichiers EPHEM et EVENT:
!     ------------------------------------------------
      if ( iecr == 1 ) then
!        cas fichier EPHEM
         iform=iforw
      else
!        cas fichier EVENT
         iform=ifore
      endif

!     protection sur le format fourni
!     permet aussi de gerer le cas ou ifore n'est pas fourni alors que deux 
!     evenements seront de toute facon communiques : debut et fin
      if ( ( iform < 1 ) .or. ( iform > 16 ) ) then
         iform = 17
      endif


! ******************************************************************************
!     Si c'est la premiere passe dans cette procedure (init = 0), 
!     on ecrit l'entete du fichier (EPHEM ou EVENT)
! ******************************************************************************
      if ( init == 0 ) then


         ! pas pour le fichier Evenement dans le cas de sortie SORTILEGE
         if((iecr == 1) .or. (str_ecr(iveh)%type_de_sortie ==1))then 
            retres = acc_begin_desc (ific) ! début descripteur des colonnes
            if (retres < 0) then
               call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                    routine='Initialiser_FichierRES', &
                    partie_variable='acc_begin_desc' )
               return
            endif

            
            i = 1
            
            ! fixe les noms des colonnes
            do while (i <= nvarw)
               retres = acc_create (ific, str_ecr(iveh)%etatn(iresw(i)), acc_param, str_ecr(iveh)%unitn(iresw(i)))
               
               ! Evolution possible : le dernier "" de la ligne précédente pourrait contenir les unites.
               
               if (retres < 0) then
                  call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                       routine='Initialiser_FichierRES', &
                       partie_variable='acc_create' )
                  return
               endif
               
               i = i + 1
            enddo
            
            
            retres = acc_end_desc (ific)  ! fin descripteur des colonnes
            if (retres < 0) then
               call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                    routine='Initialiser_FichierRES', &
                    partie_variable='acc_end_desc' )
               return
            endif
   
 
            i = 1

            ! fixe le format des colonnes
            do while (i <= nvarw)
               
               if ( (iresw(i) <= 2) .or. (iresw(i) == 106) ) then 
                  !              les cas traités ici sont des dates ou des durees
                  !              1  'DATE'  =>  date (JJ CNES) exprimée en TUC
                  !              2  'TMPS'  =>  duree de la simulation (s)
                  !              106  'DEPH'  =>  date (JJ CNES) exprimée en TE
                  format_col = "23"
               else
                  write ( format_col, '(i2)' ) (iform + 6)
               endif
               
               retres = acc_set_format (ific, str_ecr(iveh)%etatn(iresw(i)), format_col)
               
               if (retres < 0) then
                  call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                       routine='Initialiser_FichierRES', &
                       partie_variable='acc_set_format' )
                  return
               endif
               
               i = i + 1
            enddo

         end if
 
         ! creation des entetes

         retres = acc_begin_header (ific)
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                                       routine='Initialiser_FichierRES', &
                                       partie_variable='acc_begin_header' )
            return
         endif

         call pswentetes(ific,iecr)

         retres = acc_end_header (ific)  ! fin descripteur du header
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                                       routine='Initialiser_FichierRES', &
                                       partie_variable='acc_end_header' )
            return
         endif

!        On positionne le drapeau pour passer au prochain type de fichier le coup 
!        suivant si necessaire , ou pour ne plus faire d'initialisation.
!*** 
         tab_fic_lu(2,indice) = 1
!***

      end if


! ***************************************************
!     Dans tous les cas, on ecrit la ligne de valeurs
! ***************************************************

!     Si c'est un fichier evenement, on ecrit le type d'evenement
!     cas fichier EVENT
      ! cas de sortie personnalisé
      if ( (iecr == 2) .and. (str_ecr(iveh)%type_de_sortie==1) ) then
!        selon le type d'evenement, commentaire adapte
         select case ( ieve )
            case (1)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_01") )
            case (10)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_10"))
            case (11)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_11"))
            case (12)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_12"))
            case (13)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_13"))
            case (14)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_14"))
            case (15)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_15"))
            case (16)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_16"))
            case (20)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_20"))
            case (21)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_21"))
            case (30)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_30"))
            case (31)
               ligne_comment = trim ( DOMtraduire(nomdomaine,"PS_EVENT_31"))
            case (40)
               write (ligne_comment, '(a,i3,a,a80)') trim ( DOMtraduire(nomdomaine,"PS_EVENT_40")),neve," : ",aeve(neve)
         end select

         ! Ajout d'un espace en tête pour séparer du caractere # de acc_putcom
         write(ligne_com, '(1X,a)') ligne_comment
         
!        ecrit une ligne de commentaire vide
         retres = acc_putcom (ific, ACC_LIG_COM, 1, " ")
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                                       routine='pswresu', &
                                       partie_variable='acc_putcom' )
            return
         endif

         retres = acc_write (ific, ACC_ALL)
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                                       routine='pswresu', &
                                       partie_variable='acc_write' )
            return
         endif

!        ecrit la ligne du commentaire decrivant l'evenement
         retres = acc_putcom (ific, ACC_LIG_COM, 1, ligne_com)
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                                       routine='pswresu', &
                                       partie_variable='acc_putcom' )
            return
         endif

         retres = acc_write (ific, ACC_ALL)
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                                       routine='pswresu', &
                                       partie_variable='acc_write' )
            return
         endif

!        ecrit une ligne de commentaire vide
         retres = acc_putcom (ific, ACC_LIG_COM, 1, " ")
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                                       routine='pswresu', &
                                       partie_variable='acc_putcom' )
            return
         endif

         retres = acc_write (ific, ACC_ALL)
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                                       routine='pswresu', &
                                       partie_variable='acc_write' )
            return
         endif

      ! cas de sortie SORTILEGE
      elseif((iecr == 2) .and. (str_ecr(iveh)%type_de_sortie==2)) then

         date_te = str_gen(iveh)%etat(106)  ! variable "DEPH"
         call psweve(ific,date_te,INT(str_gen(iveh)%etat(117)),aeve(neve),ieve)

      endif


!     Ecriture des variables sélectionnées:
!     ------------------------------------
      ! pas pour le fichier évènement dans le cas de sortie SORTILEGE
      if (.not. (str_ecr(iveh)%type_de_sortie==2 .and. iecr==2))then
         i = 1
         ! remplit les colonnes 
         do while (i <= nvarw)
            retres = acc_putd (ific, str_ecr(iveh)%etatn(iresw(i)), etat(iresw(i)), str_ecr(iveh)%unitn(iresw(i)))
            
            if (retres < 0) then
               call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                    routine='pswresu', &
                    partie_variable='acc_putd' )
               return
            endif
            
            i = i + 1
         enddo
         
      end if

      ! ecrit la ligne
      retres = acc_write (ific, ACC_ALL)
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswresu', &
              partie_variable='acc_write' )
         return
      endif


!     Ecriture à l'écran:
!     ------------------

 998  continue

      if (ifla == 1) then
         jul1950%jour = int(etat(1))
         jul1950%sec  = (etat(1) - jul1950%jour)*86400._pm_reel
         call md_julien_calend(jul1950,annee,mois,jour,heure,minutes,secondes,code_erreur)
         if (code_erreur%valeur < 0) then
            call MSP_signaler_message (ier_mslib=code_erreur)
         end if

         if (ypas >= 0._pm_reel) then

            if (ypas < 60._pm_reel) then
               write(6,2000) iter,jour,mois,annee,heure,minutes,secondes,ypas,etat(27),etat(29)

            else
               call md_duree_jhms (ypas,ijour,iheure,imin,rsec,code_erreur)
               if (code_erreur%valeur < 0) then
                  call MSP_signaler_message (ier_mslib=code_erreur)
               end if
               if ( MSP_ERREUR ) return

               if (ypas < 3600._pm_reel) then
                  write(6,2100) iter,jour,mois,annee,heure,minutes,secondes,imin,rsec,etat(27),etat(29)

               else if (ypas < 86400._pm_reel) then
                  write(6,2200) iter,jour,mois,annee,heure,minutes,secondes,iheure,imin,rsec,etat(27),etat(29)

               else
                  write(6,2300) iter,jour,mois,annee,heure,minutes,secondes,ijour,iheure,imin,rsec,etat(27),etat(29)
               end if
            end if
         else

            if (abs(ypas) < 60._pm_reel) then
               write(6,2001) iter,jour,mois,annee,heure,minutes,secondes,ypas,etat(27),etat(29)

            else
               call md_duree_jhms (abs(ypas),ijour,iheure,imin,rsec,code_erreur)
               if (code_erreur%valeur < 0) then
                  call MSP_signaler_message (ier_mslib=code_erreur)
               end if
               if ( MSP_ERREUR ) return

               if (abs(ypas) < 3600._pm_reel) then
                  write(6,2101) iter,jour,mois,annee,heure,minutes,secondes,-imin,rsec,etat(27),etat(29)

               else if (abs(ypas) < 86400._pm_reel) then
                  write(6,2201) iter,jour,mois,annee,heure,minutes,secondes,-iheure,imin,rsec,etat(27),etat(29)

               else
                  write(6,2301) iter,jour,mois,annee,heure,minutes,secondes,-ijour,iheure,imin,rsec,etat(27),etat(29)

               end if
            end if
         end if
         call flush (6)
      end if

      if ( (str_ecr(iveh)%ipas == 2) .and. (iecr == 1) ) then
         str_ecr(iveh)%nsor = str_ecr(iveh)%nsor + 1
      endif


 2000 format('ITERATION No ',i6,' ; date = ',i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',f6.3, &
             ' ; dt = ',f5.2,' s',18x, &
             ' ; zkm =',f10.3,' km ; h =',f10.3,' km')
 2001  format('ITERATION No ',i6,' ; date = ',i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',f6.3, &
             ' ; dt = ',f5.2,' s',18x, &
             ' ; zkm =',f10.3,' km ; h =',f10.3,' km')
 2100 format('ITERATION No ',i6,' ; date = ',i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',f6.3, &
             ' ; dt = ',i2,' mn ',f5.2,' s',12x, &
             ' ; zkm =',f10.3,' km ; h =',f10.3,' km')
 2101 format('ITERATION No ',i6,' ; date = ',i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',f6.3, &
             ' ; dt = ',i3,' mn ',f5.2,' s',12x, &
             ' ; zkm =',f10.3,' km ; h =',f10.3,' km')
 2200 format('ITERATION No ',i6,' ; date = ',i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',f6.3, &
             ' ; dt = ',i2,' h ',i2,' mn ',f5.2,' s',7x, &
             ' ; zkm =',f10.3,' km ; h =',f10.3,' km')
 2201 format('ITERATION No ',i6,' ; date = ',i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',f6.3, &
             ' ; dt = ',i3,' h ',i2,' mn ',f5.2,' s',7x, &
             ' ; zkm =',f10.3,' km ; h =',f10.3,' km')
 2300 format('ITERATION No ',i6,' ; date = ',i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',f6.3, &
             ' ; dt = ',i4,' j ',i2,' h ',i2,' mn ',f5.2,' s', &
             ' ; zkm =',f10.3,' km ; h =',f10.3,' km')
 2301 format('ITERATION No ',i6,' ; date = ',i2.2,'/',i2.2,'/',i4,1x,i2.2,':',i2.2,':',f6.3, &
             ' ; dt = ',i5,' j ',i2,' h ',i2,' mn ',f5.2,' s', &
             ' ; zkm =',f10.3,' km ; h =',f10.3,' km')

 999  end subroutine pswresu

      subroutine pslnvar ()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pslnvar
!
!$Resume
!  Sous-programme lisant le fichier "PS_VARIABLES"
!
!$Description
!  Sous-programme lisant le fichier "PS_VARIABLES"
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pslnvar ()
!
!$Arguments
!
!$Common
!
!$Routines
!- MSP_effacer_message
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: num,i,nnn,ier

      num = MSP_ouverture_fichier ('PS_VARIABLES',repertoire=dirfcf_psimu,status="OLD")
      if ( MSP_PROBLEME ) then
         call MSP_effacer_message ()
         call MSP_signaler_message (cle_mes="PS_LECT_VARIABLES_000")
         return
      endif

      boucle : do i = 1 , nvmax

         read (num,*,IOSTAT=ier) nnn,str_ecr(iveh)%etatn(i),str_ecr(iveh)%unitn(i)
         if ( ier > 0 ) then
            call MSP_signaler_message (cle_mes="PS_LECT_VARIABLES_001")
            return
         else if ( ier < 0 ) then
            exit boucle
         endif

         ! Traitement des variables sans unité
         
         if (str_ecr(iveh)%unitn(i)(1:2) == "=>") then
            ! La 3eme variable lue correspond au champ commentaire,
            ! il n'y a donc pas d'unité.
            str_ecr(iveh)%unitn(i) = ""
         endif

      enddo boucle

      close (num)

      end subroutine pslnvar


      subroutine pswentetes(ific, iecr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pswentetes
!
!$Resume
!  Sous-programme créant les entetes
!
!$Description
!  Sous-programme créant les entetes des sorties MADONA colonne des fichiers 
!  "EPHEM" et "EVENT"
!
!$Auteur
!  Marie LARROQUE (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  call pswentetes(ific, iecr)
!.    integer :: ific, iecr
!
!$Arguments
!>E     ific  :<integer>   numero logique de la zone d'accès MADONA fichier d'option
!>E     iecr  :<integer>   type de sorties : 1 pour Ephemerides, sinon Analytique
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- getenv
!- date_and_time
!- gsrwnotauto
!- gssetaccout
!- write_gs_repere_ip
!- gsrwauto
!
!$Include
!
!$Module
!#V
!- ps_troiscorps
!- ps_evenements
!- ps_bulletin
!- ps_integration_don
!- cps_utilisateur
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        use ps_troiscorps
        use ps_evenements
        use ps_bulletin
        use ps_integration_don
        use cps_utilisateur
        

        implicit none

        ! arguments
        !----------
        integer, intent(in) :: ific, iecr
        
        ! variables locales
        !------------------
        integer :: retres, echelle_temps, ii, retres1, retres2, val, &
             cle_date, pla, corcen
        character(len=8) :: date,echelle_temps_car
        character(len=20)::nomOS, unite
        logical :: pres_DEPH, pres_DATE
        type(tm_jour_sec) :: date_debut, date_fin
        real(kind=pm_reel) :: date_debut_ecr, date_fin_ecr, vitrot, obli, requa, apla
        real(kind=pm_reel),dimension(10) :: repere
        character(len=12) :: psnum_version

        ! debut du code
        !--------------
        ! Initialisations
        retres = 0
        retres1 = 0
        retres2 = 0

        ! nom du logiciel
        retres = acc_puts(ific, "Logiciel","PSIMU")  
        if (retres < 0) then
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                routine='pswentetes', &
                partie_variable='acc_puts' )
           return
        endif

        ! DM-ID 600 : rajout version PSIMU
        retres = acc_puts(ific, "version",psnum_version())  
        if (retres < 0) then
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                routine='pswentetes', &
                partie_variable='acc_puts' )
           return
        endif
        
        ! DM-ID 600 / FA-ID 816 : récupération du nom de l'OS par un getenv
        call getenv("OS",nomOS)
        retres = acc_puts(ific,"nom_OS" ,trim(nomOS))
        if (retres < 0) then
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                routine='pswentetes', &
                partie_variable='acc_puts' )
           return
        endif
        
        ! DM-ID 600 : rajout date execution
        call date_and_time(date=date)
        retres = acc_puts(ific, "date_execution",date(7:8)//"/"//date(5:6)//"/"//date(1:4))
        if (retres < 0) then
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                routine='pswentetes', &
                partie_variable='acc_puts' )
           return
        endif
        

        ! type de sorties
        if ( iecr == 1 ) then
           !  cas du fichier EPHEM
           retres = acc_puts(ific, "Type_de_sorties","Ephemerides")  
        else
           retres = acc_puts(ific, "Type_de_sorties","Evenements")  
        endif
        if (retres < 0) then
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                routine='pswentetes', &
                partie_variable='acc_puts' )
           return
        endif

        ! DM-ID 600 : rajout nom fichier option
        retres = acc_puts(ific, "fichieroption", trim(cficin))
        if (retres < 0) then
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                routine='pswentetes', &
                partie_variable='acc_puts' )
           return
        endif
      ! DM-ID 600 : rajout nom sonde
      retres = acc_puts(ific, "nom_sonde","")
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_puts' )
         return
      endif
      ! DM-ID 600 : rajout identificateur sonde
      retres = acc_puts(ific, "identificateur_sonde","-999")
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_puts' )
         return
      endif
      ! (DM-ID 600) initialisation de l'échelle de temps
      ! TE si colonne DEPH presente et DATE absente ou si mode SORTILEGE, TUC sinon
      pres_DEPH = .false.
      pres_DATE = .false.
      do ii=1, str_ecr(iveh)%nvarw
         if (str_ecr(iveh)%etatn(ii).eq."DEPH") pres_DEPH = .true.
         if (str_ecr(iveh)%etatn(ii).eq."DATE") pres_DATE = .true.
      end do
      if((pres_DEPH.and. .not. pres_DATE) .or. (str_ecr(iveh)%type_de_sortie==2) ) then
         echelle_temps = MSP_ENUM_ECHT_TE
         echelle_temps_car = "TE"
         date_debut = str_bul(iveh)%datbul0_js
         date_fin = str_bul(iveh)%datbul0_js + str_int(iveh)%tmax
      else
         echelle_temps = MSP_ENUM_ECHT_TUC
         echelle_temps_car = "TUC"
         date_debut = str_bul(iveh)%datbul0_js + str_bul(iveh)%ecart_te_tuc
         date_fin = date_debut + str_int(iveh)%tmax
      end if
      ! DM-ID 600 : rajout structure repere
      val = 1 ! mode compas
      if ( str_gen(iveh)%planet == eph_terre ) then
         corcen = eph_terre
         pla = eph_terre
      else if ( str_gen(iveh)%planet == eph_mars ) then
         ! pla : planète définissant le repère
         if (str_ecr(iveh)%type_de_sortie == 1) then
            pla = eph_mars
         else
            pla = eph_terre
         endif
         corcen = eph_mars
      else if ( str_gen(iveh)%planet == eph_venus ) then
         ! pla : planète définissant le repère
         if (str_ecr(iveh)%type_de_sortie == 1) then
            pla = eph_venus
         else
            pla = eph_terre
         endif
         corcen = eph_venus
      end if
      retres = cps_getCsteTh(pla,"UAI1994","vrot",vitrot,unite)
      retres = cps_getCsteGenTh("UAI1994","obliq",obli,unite)
      retres = cps_getCsteTh(pla,"UAI1994","apla",apla,unite)
      retres = cps_getCsteTh(pla,"UAI1994","requa",requa,unite)
      ! caracteristiques du repere
      repere(:)=0._pm_reel
      if (PS_MODOUT==1) then
         if ( str_gen(iveh)%planet == eph_terre ) then
            !Terre: Gamma 50 CNES : veis 1950
            repere(1)=pm_veis
            repere(4)=0._pm_reel
            repere(5)=0._pm_reel
            cle_date = pm_1janvier1950_00h00   
         else if ( str_gen(iveh)%planet == eph_mars ) then
            !Mars: équatorial planétaire UAI à la date du bulletin
            repere(1)=pm_equa_uai
            repere(4)=str_bul(iveh)%datbul0_js%jour
            repere(5)=str_bul(iveh)%datbul0_js%sec
            cle_date = pm_autre_date
         else if ( str_gen(iveh)%planet == eph_venus ) then
            !Venus: équatorial planétaire UAI à la date du bulletin
            repere(1)=pm_equa_uai
            repere(4)=str_bul(iveh)%datbul0_js%jour
            repere(5)=str_bul(iveh)%datbul0_js%sec
            cle_date = pm_autre_date
         end if
      else if (PS_MODOUT==2) then
         ! J2000
         repere(1)=pm_equa_moy
         repere(4)=18262._pm_reel
         repere(5)=43200._pm_reel
         cle_date = pm_1janvier2000_12h00
      else
         if ( str_gen(iveh)%planet == eph_terre ) then
         ! Terre: Gamma vrai de la date
            repere(1)=pm_equa_vrai
            repere(4)=0._pm_reel
            repere(5)=0._pm_reel
	    cle_date = pm_autre_date 
	 end if 	 
      end if
      repere(2) = echelle_temps
      repere(3) = 0._pm_reel
      ! Sequence pour passer en mode 'manuel' : GENESIS ne s'occupe pas des acc_open...
      call gsrwnotauto()
      call gssetaccout(ific)
      call write_gs_repere_ip (repere, requa, apla, &
           pla, corcen, cle_date, vitrot, &
           echelle_temps, "gslib_fr", obli, 0._pm_reel, 0._pm_reel, val)
      ! On repasse GENESIS en mode automatique
      call gsrwauto()
      ! DM-ID 600 : rajout echelle de temps
      retres = acc_puts(ific, "Echelle_temps",echelle_temps_car)
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_puts' )
         return
      endif

      ! DM-ID 600 : rajout de l'origine des dates
      ! origine date = MJD1950
      retres = acc_puts(ific, "Origine_date", "MJD1950")
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_puts' )
         return
      endif
      ! DM-ID 600 : rajout de la date de début
      date_debut_ecr = date_debut%jour+date_debut%sec/86400._pm_reel
      retres = acc_putd(ific, "Date_debut", date_debut_ecr, "jj50")
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_putr' )
         return
      endif
      ! DM-ID 600 : rajout de la date de fin theorique
      date_fin_ecr = date_fin%jour+date_fin%sec/86400._pm_reel
      retres = acc_putd(ific, "Date_fin", date_fin_ecr, "jj50")
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_putr' )
         return
      endif
      retres =acc_putcom(ific,"Date_fin",1,"Date de fin théorique")
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_putcom' )
         return
      endif


      if (str_3co(iveh)%typephem==1) then
         ! DM-ID 600 : rajout theorie d'ephemerides
         retres = acc_puts(ific, "Theorie_ephemerides", "Tchebytchev")
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                 routine='pswentetes', &
                 partie_variable='acc_puts' )
            return
         endif
         ! DM-ID 600 : rajout de la cle de la theorie d'ephemerides
         retres1 = acc_puti(ific, "Cle_theorie_ephemerides", 41)
         if (retres1 < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                 routine='pswentetes', &
                 partie_variable='acc_puti' )
            return
         endif
         
         ! DM-ID 600 : rajout fichier ephemerides
         retres2 = acc_puts(ific, "Fichier_ephemerides", str_3co(iveh)%ficept)
         if (retres2 < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                 routine='pswentetes', &
                 partie_variable='acc_puts' )
            return
         endif

      else
         ! DM-ID 600 : rajout theorie d'ephemerides
         retres = acc_puts(ific, "Theorie_ephemerides", "Analytique")
         if (retres < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                 routine='pswentetes', &
                 partie_variable='acc_puts' )
            return
         endif
         
         ! DM-ID 600 : rajout de la cle de la theorie d'ephemerides
         retres1 = acc_puti(ific, "Cle_theorie_ephemerides", 10)
         if (retres1 < 0) then
            call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                 routine='pswentetes', &
                 partie_variable='acc_puti' )
            return
         endif
      end if

      ! DM-ID 600 : rajout de systeme de constantes, code COMPAS entier pour UAI1994
      ! pour l'instant, ça correspond à 2 dans SORTILEGE
      retres = acc_puti(ific, "Systeme_de_constantes", 2)
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_puti' )
         return
      endif
      ! DM-ID 600 : rajout du nb d'evenements explicites
      retres = acc_puti(ific, "NB_EVENTS", str_eve(iveh)%ndeve)
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_puti' )
         return
      endif
      retres =acc_putcom(ific,"NB_EVENTS",1,"Nombre d'évènements définis explicitement")
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_putcom' )
         return
      endif
      ! DM-ID 600 : rajout du nombre de phases
      retres = acc_puti(ific, "NB_PHASES",1)
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_puti' )
         return
      endif
      ! DM-ID 600 : rajout du nombre de corps centraux
      retres = acc_puti(ific, "NB_CORPS_SCEN",1)
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_puti' )
         return
      endif
      retres =acc_putcom(ific,"NB_CORPS_SCEN",1,"Nombre de corps centraux dans le scénario")
      if (retres < 0) then
         call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
              routine='pswentetes', &
              partie_variable='acc_putcom' )
         return
      endif

    end subroutine pswentetes


      subroutine psweve(ific,date,num_eve,com_eve,type_eve)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psweve
!
!$Resume
!  Sous-programme d'écriture des évènements au format attendu par SORTILEGE
!
!$Description
!  Sous-programme d'écriture des évènements au format attendu par SORTILEGE
!
!$Auteur
!  Marie LARROQUE (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  call psweve(ific,date,num_eve,com_eve,type_eve)
!.    integer :: ific, num_eve, type_eve
!.    real(kind=pm_reel) :: date
!.    character(len=80) :: com_eve
!
!$Arguments
!>E     ific      :<integer>   numero logique de la zone d'acces MADONA du fichier évènement
!>E     date      :<pm_reel>   date TE, JJ CNES
!>E     num_eve   :<integer>   numero de l'evenement
!>E     com_eve   :<LEN=80>    commentaire pour le fichier EVENT
!>E     type_eve  :<integer>   type d'evenement
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
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        implicit none

        !ARGUMENTS
        integer,intent(in) :: ific, num_eve, type_eve
        real(kind=pm_reel),intent(in) :: date
        character(len=80), intent(in) :: com_eve

        !VARIABLES LOCALES
        integer :: reteve,reteve_glob
        character(len=80) :: nomevent
        character :: car1
        character(len=2) :: car2
        integer :: indpousi, indpousc, indsep, indeve, indpousif, indpouscf,indsepf
        
        data indpousi /0/
        data indpousif /0/
        data indpousc /0/
        data indpouscf /0/
        data indsep /0/
        data indsepf /0/
        data indeve /0/

        !les indices des évènement pourront aller jusqu'à 99

        !DEBUT DU CODE
	
	! Initialisations
	reteve = 0
	nomevent = ""
	
        ! definition de nomevent
        select case(type_eve)
           case(1) ! debut de simulation
              nomevent = "DEB_SIMUL"
           case(10)! debut de poussee impulsionnelle
              indpousi = indpousi +1
              if (indpousi<10) then
                 write(car1,'(i1)')indpousi
                 nomevent = "DEB_POUS_IMP_0"//car1
              else
                 write(car2,'(i2)')indpousi
                 nomevent = "DEB_POUS_IMP_"//car2
              end if
           case(11)! fin de poussee impulsionnelle
              indpousif = indpousif +1
              if (indpousi<10) then
                 write(car1,'(i1)')indpousif
                 nomevent = "FIN_POUS_IMP_0"//car1
              else
                 write(car2,'(i2)')indpousif
                 nomevent = "FIN_POUS_IMP_"//car2
              end if
           case(12)! debut de poussee continue
              indpousc = indpousc + 1
              if (indpousc<10) then
                 write(car1,'(i1)')indpousc
                 nomevent = "DEB_POUS_CONT_0"//car1
              else
                 write(car2,'(i2)')indpousc
                 nomevent = "DEB_POUS_CONT_"//car2
              end if
           case(13)! fin de poussee continue
              indpouscf = indpouscf + 1
              if (indpouscf<10) then
                 write(car1,'(i1)')indpouscf
                 nomevent = "FIN_POUS_CONT_0"//car1
              else
                 write(car2,'(i2)')indpouscf
                 nomevent = "FIN_POUS_CONT_"//car2
              end if
           case(14)! reservoirs vides
              nomevent = "RES_VIDES"	
	   case(15)! debut de créneau de poussée
	      nomevent = "DEB_CRENEAU_POUS"  
	   case(16)! fin de créneau de poussée
	      nomevent = "FIN_CRENEAU_POUS"
           case(20)! debut de separation
              indsep = indsep + 1
              if (indsep<10) then
                 write(car1,'(i1)')indsep
                 nomevent = "DEB_SEP_0"//car1
              else
                 write(car2,'(i2)')indsep
                 nomevent = "DEB_SEP_"//car2
              end if
           case(21)! fin de separation
              indsepf = indsepf + 1
              if (indsepf<10) then
                 write(car1,'(i1)')indsepf
                 nomevent = "DEB_SEP_0"//car1
              else
                 write(car2,'(i2)')indsepf
                 nomevent = "DEB_SEP_"//car2
              end if
           case(30)! date de fin de simulation
              nomevent = "FIN_SIMUL_DATE"
           case(31)! altitude de fin de simulation
              nomevent = "FIN_SIMUL_ALT"
           case(40)! evenement date
              indeve = indeve + 1
              if (indeve<10) then
                 write(car1,'(i1)')indeve
                 nomevent = "EVE_NO_0"//car1
              else
                 write(car2,'(i2)')indeve
                 nomevent = "EVE_NO_"//car2
              end if
        end select

        reteve=acc_exist(ific,nomevent)
	
        if (reteve < 0) then
           ! cas d'erreur
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                partie_variable='acc_exist' )
           return
        endif
 
        if (reteve == 0 ) then
           ! la donnée n'existe pas, création de la donnée vide
           reteve = acc_create(ific,trim(nomevent),ACC_STRUCT,"")
           if (reteve < 0) then
              ! cas d'erreur
              call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                   partie_variable='acc_create' )
              return
           endif
           
           reteve = acc_select(ific,trim(nomevent),ACC_STRUCT)
           if (reteve < 0) then
              ! cas d'erreur
              call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                   partie_variable='acc_select' )
              return
           endif
      
           reteve_glob = 0
           reteve = acc_create(ific,"libelle",ACC_PARAM,"")
           reteve_glob = reteve_glob + reteve
           reteve = acc_create(ific,"date",ACC_PARAM,"")
           reteve_glob = reteve_glob + reteve
           reteve = acc_create(ific,"numero",ACC_PARAM,"")
           reteve_glob = reteve_glob + reteve
           if (reteve_glob < 0) then
              ! cas d'erreur
              call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                   partie_variable='acc_create' )
              return
           endif

           reteve = acc_select_end(ific)
           if (reteve < 0) then
              ! cas d'erreur
              call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                   partie_variable='acc_select_end' )
              return
           endif
           
        endif ! Création de la données structurée
        
   
        ! Renseignement des champs
 
        reteve = acc_select(ific,trim(nomevent),ACC_STRUCT)
        if (reteve < 0) then
           ! cas d'erreur
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                partie_variable='acc_select' )
           return
        endif
        
        ! Numéro
        reteve = acc_puti(ific,"numero",num_eve)
        if (reteve < 0) then
           ! cas d'erreur
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                partie_variable='acc_puti' )
           return
        endif
        
        ! Libellé
        reteve = acc_puts(ific,"libelle",com_eve)
        if (reteve < 0) then
           ! cas d'erreur
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                partie_variable='acc_puts' )
           return
        endif
        
        ! Date
        reteve = acc_putd(ific,"date",date,'')
        if (reteve < 0) then
           ! cas d'erreur
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                partie_variable='acc_putd' )
           return
        endif
        
        reteve = acc_select_end(ific)
        if (reteve < 0) then
           ! cas d'erreur
           call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",routine='psweve', &
                partie_variable='acc_select_end' )
           return
        endif
        

      end subroutine psweve

end module ps_ecriture
