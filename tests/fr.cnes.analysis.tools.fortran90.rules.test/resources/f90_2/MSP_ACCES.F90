module MSP_ACCES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ACCES
!
!$Resume
!  Module contenant des utilitaires d'accès aux fichiers.
!
!$Description
!  Module contenant des utilitaires d'accès aux fichiers.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_ACCES.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ACCES.F90,v $
!  Revision 1.18  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.17  2009/10/02 15:11:47  mercadig
!  DM-ID 1113: L attribut de acc_deconnect est mis a ACC_ALL
!
!  Revision 1.16  2008/11/19 13:28:26  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.15  2008/02/22 13:58:29  huec
!  FA-ID 968 : Ajout du IMPLICIT NONE manquant
!  Revision 1.14  2007/10/23 15:03:02  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.13  2007/03/19 09:41:29  tanguyy
!  AQ : mise a jour des cartouches
!  Revision 1.12  2006/11/15 10:09:34  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.11  2006/11/09 09:13:50  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.10  2005/03/08 07:32:32  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.9  2005/01/20 13:56:01  pauh
!  FA_332
!  Revision 1.8.2.1  2005/01/19 09:14:36  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.8  2005/01/06 09:35:29  vivaresf
!  Presentation
!  Revision 1.7  2003/07/11 09:33:36  adm_ipsi
!  DM-ID 20 : Ajout des routines MSP_ouverture_MADONA_col et MSP_fermeture_MADONA
!  Revision 1.6  2003/07/10 14:02:34  adm_ipsi
!  FA-ID 14 : Ajout des fonctions MSP_acc_get_val_X
!  Revision 1.9  2003/03/27 16:44:07  util_am
!  SLB - Version industrialisée
!  Revision 1.5  2003/01/07 18:11:39  adm_ipsi
!   suppression des variables non utilisées
!  Revision 1.4  2002/12/09 16:45:36  adm_ipsi
!  Utilisation du parametre MSP_ENUM_ECRAN pour les sorties ecran
!  Revision 1.3  2002/12/04 18:08:23  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.2  2002/12/03 17:21:00  adm_ipsi
!   Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.8  2000/08/31 15:00:27  util_am
!  Ajout de la fonction MSP_acc_create.
!  Revision 1.7  2000/06/13 11:00:01  util_am
!  Introduction des interfaces en anglais
!  Ajout de cartouches aux interfaces
!  Mise à jour des cartouches sections Voir-Aussi, Mots-Cles
!  Revision 1.4  1999/10/21 15:03:59  util_am
!  Bug sur l'argument status de la routine MSP_ouverture_fichier
!  Revision 1.3  1999/09/22 14:46:07  util_am
!  Mise a jour des cartouches
!  Revision 1.2  1999/09/03 16:42:37  util_am
!  MSP_ACCES.F90 :Ajout de routine pour la connexion et la deconnexion de section MADONA
!  MSP_BULLETIN_DEF.F90 : Ajout de MSP_modifier_bulletin, MSP_lire_BULLETIN
!  MECASPA.F90 : Ajout des use aux nouveaux modules
!  Revision 1.1.1.1  1999/07/13 08:37:58  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_ACCES
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- MSP_acc_get_tab
!- MSP_acc_get_val
!- MSP_read_data_com
!- MSP_acc_load_data
!- MSP_open_file
!- MSP_acc_get_tab1
!- MSP_acc_get_tab2
!- MSP_acc_get_tab3
!- MSP_leccom
!- MSP_ouverture_MADONA_col
!- MSP_fermeture_MADONA
!#V
!- MSP_acc_get_tab1i
!- MSP_acc_get_tab1d
!- MSP_acc_get_tab1c
!- MSP_acc_get_val_i
!- MSP_acc_get_val_d
!- MSP_acc_get_val_c
!- MSP_acc_get_tab2i
!- MSP_acc_get_tab2d
!- MSP_acc_get_tab2c
!- MSP_acc_get_tab3i
!- MSP_acc_get_tab3d
!- MSP_acc_get_tab3c
!#
!
!$Fonctions
!- MSP_acc_select
!- MSP_acc_create
!- MSP_acc_select_end
!- MSP_acc_charger_donnees
!- MSP_ouverture_fichier
!
!$Include
!#V
!- acces_F.h
!#
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- MSP_MECASPA_DEF
!#
!
!$Interface
!> msp_open_file :      MSP_ouverture_fichier
!> msp_acc_get_val :    MSP_acc_get_val_d, MSP_acc_get_val_i, MSP_acc_get_val_c
!> msp_read_data_com :  MSP_leccom
!> msp_acc_load_data :  MSP_acc_charger_donnees
!> msp_acc_get_tab :    MSP_acc_get_tab1i, MSP_acc_get_tab1d, MSP_acc_get_tab1c, 
!                       MSP_acc_get_tab2i, MSP_acc_get_tab2d, MSP_acc_get_tab2c, 
!                       MSP_acc_get_tab3i, MSP_acc_get_tab3d, MSP_acc_get_tab3c
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  ACCES FICHIER MADONA
!
!$Voir-Aussi
!#V
!.  MSP_acc_get_tab1i MSP_acc_get_tab1d MSP_acc_get_tab1c MSP_acc_get_val_i MSP_acc_get_val_d
!.  MSP_acc_get_val_c MSP_acc_get_tab2i MSP_acc_get_tab2d MSP_acc_get_tab2c MSP_acc_get_tab3i
!.  MSP_acc_get_tab3d MSP_acc_get_tab3c
!#
!.  MSP_acc_select MSP_acc_create MSP_acc_select_end MSP_acc_charger_donnees MSP_ouverture_fichier
!.  MSP_acc_get_tab MSP_acc_get_val MSP_read_data_com MSP_acc_load_data MSP_open_file MSP_acc_get_tab1
!.  MSP_acc_get_tab2 MSP_acc_get_tab3 MSP_leccom MSP_ouverture_MADONA_col MSP_fermeture_MADONA
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSLIB
   use MSP_GESTION_ERREUR
   use MSP_MECASPA_DEF


   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ACCES.F90 69 2012-09-11 08:33:34Z ffsm $'


!#include "acces_F.h"
!#include "AMenv_F.h"

   interface MSP_acc_get_tab

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab
!
!$Resume
!  Lecture d'un tableau (entier,pm_reel ou chaîne de charactères) à 1, 2 ou 3 dimensions.
!
!$Description
!  Lecture d'un tableau (entier,pm_reel ou chaîne de charactères) à 1, 2 ou 3 dimensions.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_acc_get_tab (acc,libtab,tabi,[dim1])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:) :: tabi
!.    integer :: dim1
!
!  call MSP_acc_get_tab (acc,libtab,tabd,[unit],[dim1])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    real(kind=pm_reel), pointer, dimension(:) :: tabd
!.    character(LEN=*) :: unit
!.    integer :: dim1
!
!  call MSP_acc_get_tab (acc,libtab,tabc,[dim1])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    character(LEN=*), pointer, dimension(:) :: tabc
!.    integer :: dim1
!
!  call MSP_acc_get_tab (acc,libtab,tabi,[dim1],[dim2])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:,:) :: tabi
!.    integer :: dim1,dim2
!
!  call MSP_acc_get_tab (acc,libtab,tabd,unit,[dim1],[dim2])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    real(kind=pm_reel), pointer, dimension(:,:) :: tabd
!.    character(LEN=*) :: unit
!.    integer :: dim1,dim2
!
!  call MSP_acc_get_tab (acc,libtab,tabc,[dim1],[dim2])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    character(LEN=*), pointer, dimension(:,:) :: tabc
!.    integer :: dim1,dim2
!
!  call MSP_acc_get_tab (acc,libtab,tabi,[dim1],[dim2],[dim3])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:,:,:) :: tabi
!.    integer :: dim1,dim2,dim3
!
!  call MSP_acc_get_tab (acc,libtab,tabd,unit,[dim1],[dim2],[dim3])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    real(kind=pm_reel), pointer, dimension(:,:,:) :: tabd
!.    character(LEN=*) :: unit
!.    integer :: dim1,dim2,dim3
!
!  call MSP_acc_get_tab (acc,libtab,tabc,[dim1],[dim2],[dim3])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    character(LEN=*), pointer, dimension(:,:,:) :: tabc
!.    integer :: dim1,dim2,dim3
!
!$Procedures
!#V
!- MSP_acc_get_tab1i
!- MSP_acc_get_tab1d
!- MSP_acc_get_tab1c
!- MSP_acc_get_tab2i
!- MSP_acc_get_tab2d
!- MSP_acc_get_tab2c
!- MSP_acc_get_tab3i
!- MSP_acc_get_tab3d
!- MSP_acc_get_tab3c
!#
!
!$Remarques
!
!$Mots-cles
! ACCES FICHIER MADONA TABLEAU DIMENSION1 ENTIER REEL CARACTERE DIMENSION2 DIMENSION3
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      module procedure MSP_acc_get_tab1i,MSP_acc_get_tab1d,MSP_acc_get_tab1c
      module procedure MSP_acc_get_tab2i,MSP_acc_get_tab2d,MSP_acc_get_tab2c
      module procedure MSP_acc_get_tab3i,MSP_acc_get_tab3d,MSP_acc_get_tab3c

   end interface

   interface MSP_acc_get_val

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_val
!
!$Resume
!    Lecture d'une valeur (entier,pm_reel ou chaîne de charactères)
!
!$Description
!    Lecture d'une valeur (entier,pm_reel ou chaîne de charactères)
!    dans une zone d'accès MADONA
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_acc_get_val (acc,libzone,vald,[unit],[valdef]) 
!.    integer :: acc
!.    character(LEN=*) :: libzone
!.    real(kind=pm_reel) :: vald 
!.    character(LEN=*) :: unit
!.    real(kind=pm_reel) :: valdef 
!
!  call MSP_acc_get_val (acc,libzone,vali,[valdef]) 
!.    integer :: acc
!.    character(LEN=*) :: libzone
!.    integer :: vali 
!.    integer :: valdef
!
!  call MSP_acc_get_val (acc,libzone,valc,[valdef])
!.    integer :: acc
!.    character(LEN=*) :: libzone
!.    character(LEN=*) :: valc 
!.    character(LEN=*) :: valdef
!
!$Procedures
!#V
!- MSP_acc_get_val_d
!- MSP_acc_get_val_i
!- MSP_acc_get_val_c
!#
!
!$Remarques
!
!$Mots-cles
! ACCES FICHIER MADONA REEL ENTIER CARACTERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      module procedure MSP_acc_get_val_d, MSP_acc_get_val_i, MSP_acc_get_val_c

   end interface
   
   ! >>>>>>>>>>>>>> Interface anglaise pour les routines ACCES
   interface MSP_read_data_com

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_read_data_com
!
!$Resume
!  Read the lines of a file which are not commentary
!
!$Description
!  This subroutine jumps over the commentary lines of a file linked to the logical
!  unit "num_fich", (these lines shall begin by the single character "carcom")
!  The output is a line ("ligne") containing understandable numerical values 
!  used by the instruction : read (ligne,*) (... iolist ...)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_read_data_com (num_fich,carcom,ligne)
!.    integer :: num_fich
!.    character(LEN=1) :: carcom
!.    character(LEN=*) :: ligne
!
!$Procedures
!- MSP_leccom
!
!$Remarques
!  English interface of existing subroutines
!
!$Mots-cles
! FICHIER LECTURE I/O
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_leccom
   end interface

   interface MSP_acc_load_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_load_data
!
!$Resume
!  Loading in memory a MADONA format file
!
!$Description
!  Loading in memory a MADONA format file
!
!$Acces
!  PUBLIC
!
!$Usage
!  ierreur = MSP_acc_load_data (fichier,acc,[type])
!.    character(LEN=*) :: fichier
!.    integer :: acc
!.    character(LEN=1) :: type
!.    integer :: ierreur
!
!$Procedures
!- MSP_acc_charger_donnees
!
!$Remarques
!  English interface of existing subroutines
!
!$Mots-cles
! ACCES FICHIER MADONA CHARGER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_acc_charger_donnees
   end interface

   interface MSP_open_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_open_file
!
!$Resume
!  Openning of a regular ASCII file
!
!$Description
!  Openning of a regular ASCII file with a non-used logical unit
!
!$Acces
!  PUBLIC
!
!$Usage
!  ifich = MSP_open_file (fichier,[repertoire],[recl],[status],[nlogd])
!.    character(LEN=*) :: fichier
!.    character(LEN=*) :: repertoire
!.    integer :: recl
!.    character(LEN=*) :: status
!.    integer :: nlogd
!.    integer :: ifich
!
!$Procedures
!- MSP_ouverture_fichier
!
!$Remarques
!  English interface of existing subroutines
!
!$Mots-cles
! FICHIER OUVERTURE I/O
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ouverture_fichier
   end interface

   private MSP_acc_get_tab1i,MSP_acc_get_tab1d,MSP_acc_get_tab1c
   private MSP_acc_get_tab2i,MSP_acc_get_tab2d,MSP_acc_get_tab2c
   private MSP_acc_get_tab3i,MSP_acc_get_tab3d,MSP_acc_get_tab3c
   private MSP_acc_get_val_i, MSP_acc_get_val_d, MSP_acc_get_val_c

   contains

  integer FUNCTION MSP_acc_select(nacc, sect_tab, type_tab, ntab) result(ierreur)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_select
!
!$Resume
!  Cette routine selectionne successivement plusieurs niveaux de structures MADONA
!  imbriqués
!
!$Description
!  Cette routine selectionne successivement plusieurs niveaux de structures MADONA
!  imbriqués
!
!$Auteur
!  02/09/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  ierreur = MSP_acc_select(nacc, sect_tab, type_tab, ntab)
!.    integer :: nacc, ntab
!.    integer, dimension(:) :: type_tab
!.    character(LEN=*), dimension(:) :: sect_tab
!
!$Arguments
!>E     nacc      :<integer>           Unité logique de la zone d'acces MADONA
!>E     sect_tab  :<LEN=*,DIM=(:)>     Tableau des noms des sections à sélectionner
!>E     type_tab  :<integer,DIM=(:)>   Type des sections à sélectionner
!>E     ntab      :<integer>           nombre de diveaux à sélectionner
!>S     ierreur   :<integer>           Retour d'erreur MADONA
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
!   ACCES MADONA SELECT
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(IN) :: nacc, ntab
    integer, dimension(:), intent(in) :: type_tab
    character(LEN=*), dimension(:), intent(IN) :: sect_tab

    integer :: i

    ierreur = 0
    do i = 1, ntab

       ierreur = acc_select(nacc, trim(sect_tab(i)), type_tab(i))
       if ( ierreur < 0 ) then
          call MSP_signaler_message (cle_mes="MSP_probleme_select", &
               partie_variable=sect_tab(i))
       endif

    end do

  end FUNCTION MSP_acc_select


  integer FUNCTION MSP_acc_create(nacc, sect_tab, type_tab, ntab) result(ierreur)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_create
!
!$Resume
!  Cette routine crée des sections MADONA et les sélectionne
!
!$Description
!  Cette routine crée des sections MADONA et les sélectionne
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  ierreur = MSP_acc_create(nacc, sect_tab, type_tab, ntab)
!.    integer :: nacc, ntab
!.    integer, dimension(:) :: type_tab
!.    character(LEN=*), dimension(:) :: sect_tab
!
!$Arguments
!>E     nacc      :<integer>           Numéro d'accès MADONA où réaliser cette création
!>E     sect_tab  :<LEN=*,DIM=(:)>     Nom des sections à créer
!>E     type_tab  :<integer,DIM=(:)>   Type des sections à créer
!>E     ntab      :<integer>           Nombre de section à créer
!>S     ierreur   :<integer>           Retour d'erreur MADONA
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
!   ACCES MADONA CREATE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(IN) :: nacc, ntab
    integer, dimension(:), intent(in) :: type_tab
    character(LEN=*), dimension(:), intent(IN) :: sect_tab

    integer :: i

    ierreur = 0
    do i = 1, ntab

       ierreur = acc_create(nacc, trim(sect_tab(i)), type_tab(i), "")
       if ( ierreur < 0 ) then
          call MSP_signaler_message (cle_mes="MSP_probleme_create", &
               partie_variable=sect_tab(i))
       endif
       ierreur = acc_select(nacc, trim(sect_tab(i)), type_tab(i))
       if ( ierreur < 0 ) then
          call MSP_signaler_message (cle_mes="MSP_probleme_select", &
               partie_variable=sect_tab(i))
       endif

    end do

  end FUNCTION MSP_acc_create


  integer FUNCTION MSP_acc_select_end(nacc, ntab) result(ierreur)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_select_end
!
!$Resume
!  Cette routine désélectionne successivement plusieurs niveaux de structures MADONA
!  imbriqués
!
!$Description
!  Cette routine désélectionne successivement plusieurs niveaux de structures MADONA
!  imbriqués
!
!$Auteur
!  02/09/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  ierreur = MSP_acc_select_end(nacc, ntab)
!.    integer :: nacc, ntab
!
!$Arguments
!>E     nacc     :<integer>   Numero du moyen d'acces MADONA
!>E     ntab     :<integer>   Nombre de section a désélectionner (imbriquées les unes dans les autres)
!>S     ierreur  :<integer>   Retour d'erreur
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
!   ACCES MADONA SELECT
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer, intent(IN) :: nacc, ntab
    integer :: i

    ierreur = 0
    do i = ntab, 1, -1

       ierreur = acc_select_end(nacc)
       if ( ierreur < 0 ) then
          call MSP_signaler_message (cle_mes="MSP_probleme_deselect")     
       endif

    end do

  end FUNCTION MSP_acc_select_end

   function MSP_acc_charger_donnees (fichier,acc,type) result (ierreur)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_charger_donnees
!
!$Resume
!  Chargement en mémoire d'un fichier MADONA.
!
!$Description
!  Chargement en mémoire d'un fichier MADONA.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  ierreur = MSP_acc_charger_donnees (fichier,acc,[type])
!.    character(LEN=*) :: fichier
!.    integer :: acc
!.    character(LEN=1) :: type
!.    integer :: ierreur
!
!$Arguments
!>E     fichier  :<LEN=*>     nom du fichier
!>S     acc      :<integer>   numéro d'accès MADONA
!>[E]   type     :<LEN=1>     type d'ouverture [par défaut "r"]
!>S     ierreur  :<integer>   indicateur d'erreur [cf. doc MADONA]
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
!   ACCES FICHIER MADONA CHARGER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      character(LEN=*), intent(IN)           :: fichier
      integer, intent(OUT)                   :: acc
      character(LEN=1), intent(IN), optional :: type

      integer :: ierreur,ier
      real(kind=pm_reel) :: vartmp
      character(LEN=1) :: typeloc
    
      if ( present(type) ) then
         typeloc = type
      else
         typeloc = "r"
      end if

      ! Test pour savoir si un fichier unité a déjà été chargé en effectuant une conversion de base (km -> m):
      ier = AMv_unit_convert ("km",1._pm_reel,"m",vartmp)
      if ( ier /= 0 ) then
         ! Chargement du fichier unité:
         ierreur = AMv_unit_load ('unites')
         if ( ierreur /= 0 ) then
            call MSP_signaler_message (cle_mes="MSP_OUVERTURE_FICHIER",partie_variable=fichier(1:LEN_TRIM(fichier)), &
                                       routine="MSP_acc_charger_donnees",type=MSP_ENUM_WARNING)     
         endif
      endif

      ! Ouverture du fichier:
      acc = acc_open()
      ierreur = acc_connect(acc,fichier(1:LEN_TRIM(fichier)),typeloc)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_CONNECTION_FICHIER",partie_variable=fichier(1:LEN_TRIM(fichier)), &
                                    routine="MSP_acc_charger_donnees",type=MSP_ENUM_ERREUR)
         return     
      endif
    
      ! Lecture du fichier 
      ierreur = acc_read(acc,ACC_ALL)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du fichier '//fichier(1:LEN_TRIM(fichier)), &
                                    routine="MSP_acc_charger_donnees",type=MSP_ENUM_ERREUR)
         return     
      endif

   end function MSP_acc_charger_donnees

   subroutine MSP_acc_get_tab1i (acc,libtab,tabi,dim1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab1i
!
!$Resume
!  Lecture d'un tableau d'entier à une dimension.
!
!$Description
!  Lecture d'un tableau d'entier à une dimension.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab1i (acc,libtab,tabi,[dim1])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:) :: tabi
!.    integer :: dim1
!
!$Arguments
!>E     acc     :<integer>                   numéro d'accès MADONA
!>E     libtab  :<LEN=*>                     libellé du tableau
!>E/S   tabi    :<integer,DIM=(:),pointer>   tableau lu
!>[S]   dim1    :<integer>                   dimension du tableau lu
!
!$Common
!
!$Routines
!- MSP_acc_get_tab1
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION1 ENTIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      character(LEN=*), intent(IN) :: libtab
      integer, pointer, dimension(:) :: tabi
      integer, intent(OUT), optional :: dim1

      call MSP_acc_get_tab1 (acc,libtab,tabi=tabi,dim1=dim1)

   end subroutine MSP_acc_get_tab1i

   subroutine MSP_acc_get_tab1d (acc,libtab,tabd,unit,dim1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab1d
!
!$Resume
!  Lecture d'un tableau de "pm_reel" à une dimension.
!
!$Description
!  Lecture d'un tableau de "pm_reel" à une dimension.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab1d (acc,libtab,tabd,[unit],[dim1])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    real(kind=pm_reel), pointer, dimension(:) :: tabd
!.    character(LEN=*) :: unit
!.    integer :: dim1
!
!$Arguments
!>E     acc     :<integer>                   numéro d'accès MADONA
!>E     libtab  :<LEN=*>                     libellé du tableau
!>E/S   tabd    :<pm_reel,DIM=(:),pointer>   tableau lu
!>[E]   unit    :<LEN=*>                     unité [par défaut ""]
!>[S]   dim1    :<integer>                   dimension du tableau lu
!
!$Common
!
!$Routines
!- MSP_acc_get_tab1
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION1 REEL
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      character(LEN=*), intent(IN) :: libtab
      real(kind=pm_reel), pointer, dimension(:) :: tabd
      character(LEN=*), intent(IN), optional :: unit
      integer, intent(OUT), optional :: dim1

      call MSP_acc_get_tab1 (acc,libtab,tabd=tabd,unit=unit,dim1=dim1)

   end subroutine MSP_acc_get_tab1d

   subroutine MSP_acc_get_tab1c (acc,libtab,tabc,dim1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab1c
!
!$Resume
!  Lecture d'un tableau de chaines de caractères à une dimension.
!
!$Description
!  Lecture d'un tableau de chaines de caractères à une dimension.
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab1c (acc,libtab,tabc,[dim1])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    character(LEN=*), pointer, dimension(:) :: tabc
!.    integer :: dim1
!
!$Arguments
!>E     acc     :<integer>                 numéro d'accès MADONA
!>E     libtab  :<LEN=*>                   libellé du tableau
!>E/S   tabc    :<LEN=*,DIM=(:),pointer>   tableau lu
!>[S]   dim1    :<integer>                 dimension du tableau lu
!
!$Common
!
!$Routines
!- MSP_acc_get_tab1
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION1 CARACTERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      character(LEN=*), intent(IN) :: libtab
      character(LEN=*), pointer, dimension(:) :: tabc
      integer, intent(OUT), optional :: dim1

      call MSP_acc_get_tab1 (acc,libtab,tabc=tabc,dim1=dim1)

   end subroutine MSP_acc_get_tab1c

   subroutine MSP_acc_get_tab1 (acc,libtab,tabi,tabd,tabc,unit,dim1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab1
!
!$Resume
!  Lecture d'un tableau (entier,pm_reel ou chaine de charactères) à une dimension.
!
!$Description
!  Lecture d'un tableau (entier,pm_reel ou chaine de charactères) à une dimension.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_acc_get_tab1 (acc,libtab,[tabi],[tabd],[tabc],[unit],[dim1])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:) :: tabi
!.    real(kind=pm_reel), pointer, dimension(:) :: tabd
!.    character(LEN=MSP_LONG_CHAINE), pointer, dimension(:) :: tabc
!.    character(LEN=*) :: unit
!.    integer :: dim1
!
!$Arguments
!>E     acc     :<integer>                               numéro d'accès MADONA
!>E     libtab  :<LEN=*>                                 libellé du tableau
!>[E/S] tabi    :<integer,DIM=(:),pointer>               tableau lu (si entier)
!>[E/S] tabd    :<pm_reel,DIM=(:),pointer>               tableau lu (si pm_reel)
!>[E/S] tabc    :<LEN=MSP_LONG_CHAINE,DIM=(:),pointer>   tableau lu (si chaîne de caractère)
!>[E]   unit    :<LEN=*>                                 unité [par défaut ""]
!>[S]   dim1    :<integer>                               dimension du tableau lu
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
!   ACCES FICHIER MADONA TABLEAU DIMENSION1 ENTIER REEL CARACTERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      character(LEN=*), intent(IN) :: libtab
      integer, pointer, dimension(:), optional :: tabi
      real(kind=pm_reel), pointer, dimension(:), optional :: tabd
    ! character(LEN=*), pointer, dimension(:), optional :: tabc   
      character(LEN=MSP_LONG_CHAINE), pointer, dimension(:), optional :: tabc
      character(LEN=*), intent(IN), optional :: unit
      integer, intent(OUT), optional :: dim1

      integer :: ierreur
      integer :: MSP_iostat
      integer :: nb_el,i,itab

      ! initialisation MSP_iostat
      MSP_iostat=0
      
      ! Lecture de la dimension de la grille 
      nb_el = acc_get_dim(acc,libtab)
      if ( nb_el <= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
              partie_variable='de la dimension du tableau '//libtab(1:LEN_TRIM(libtab)), &
              routine="MSP_acc_get_tab1",type=MSP_ENUM_ERREUR)
         return
      else
         if ( present(dim1) ) then
            dim1 = nb_el
         endif 
      endif

      ! Tests sur la présence des arguments optionnels:
      itab = 0
      if ( present(tabi) ) then
         itab = itab + 1
      endif
      if ( present(tabd) ) then
         itab = itab + 1
      endif
      if ( present(tabc) ) then
         itab = itab + 1
      endif
      if ( itab == 0 ) then
         call MSP_signaler_message (cle_mes="MSP_acc_get_tab_001")
         return
      else  if ( itab > 1 ) then
         call MSP_signaler_message (cle_mes="MSP_acc_get_tab_002")
         return
      endif

      ! Allocation mémoire du tableau à remplir:
      if ( present(tabi) ) then    
         if ( ASSOCIATED (tabi) ) then
            DEALLOCATE (tabi,stat=MSP_iostat)
         end if
         ALLOCATE (tabi(nb_el))
      else if ( present(tabd) ) then    
         if ( ASSOCIATED (tabd) ) then
            DEALLOCATE (tabd,stat=MSP_iostat)
         end if
         ALLOCATE (tabd(nb_el))
      else  
         if ( ASSOCIATED (tabc) ) then
            DEALLOCATE (tabc,stat=MSP_iostat)
         end if
         ALLOCATE (tabc(nb_el))
      endif

      ! Lecture de la grille 
      ierreur = acc_select(acc,libtab,ACC_TABL)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab1",type=MSP_ENUM_ERREUR)
         return     
      endif

      boucle_1d : do i = 1 , nb_el
                     ierreur = acc_set_index(acc,i)
                     if ( ierreur < 0 ) then
                        call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                   routine="MSP_acc_get_tab1",type=MSP_ENUM_ERREUR)
                        return     
                     endif
                     if ( present(tabi) ) then    
                        ierreur = acc_geti(acc,ACC_INDEX,tabi(i))
                     else if ( present(tabd) ) then    
                        ierreur = acc_getd(acc,ACC_INDEX,tabd(i),unit)
                     else
                        ierreur = acc_gets(acc,ACC_INDEX,tabc(i))
                     endif
                     if ( ierreur < 0 ) then
                        call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                   routine="MSP_acc_get_tab1",type=MSP_ENUM_ERREUR)
                        return     
                     endif
                  end do boucle_1d

      ierreur = acc_select_end(acc)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab1",type=MSP_ENUM_ERREUR)
         return     
      endif

   end subroutine MSP_acc_get_tab1

   subroutine MSP_acc_get_val_i (acc,libzone,vali,valdef) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_val_i
!
!$Resume
!  Lecture d'un entier dans une zone d'accès MADONA.
!
!$Description
!  Lecture d'un entier dans une zone d'accès MADONA..
!
!$Auteur
!  D. Semeux (SLB)
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_val_i (acc,libzone,vali,[valdef]) 
!.    integer :: acc
!.    character(LEN=*) :: libzone
!.    integer :: vali 
!.    integer :: valdef
!
!$Arguments
!>E     acc      :<integer>   numéro d'accès MADONA
!>E     libzone  :<LEN=*>     libellé de la valeur
!>S     vali     :<integer>   valeur entière lue
!>[E]   valdef   :<integer>   valeur par défaut
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
!   ACCES FICHIER MADONA  ENTIER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      character(LEN=*), intent(IN) :: libzone
      integer, intent(OUT) :: vali 
      integer, intent(IN), optional :: valdef
 
      integer :: ierreur

      ! Lecture de la valeur 
      ierreur = acc_exist(acc,libzone)
      if ( ierreur < 0 ) then
         if (present(valdef)) then
            vali = valdef
         else
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la valeur '//libzone(1:LEN_TRIM(libzone)), &
                                    routine="MSP_acc_get_val_i",type=MSP_ENUM_ERREUR)
         endif
         return     

      else if ( ierreur == 0 ) then
         if (present(valdef)) then
            vali = valdef
         else
            call MSP_signaler_message (cle_mes="MSP_VALEUR_ABSENTE",partie_variable=libzone(1:LEN_TRIM(libzone)), &
                                    routine="MSP_acc_get_val_i")
         endif
         return     

      else

         ! Lecture d'un entier  
         ierreur = acc_geti(acc,libzone,vali)

         if ( ierreur /= 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='de la valeur '//libzone(1:LEN_TRIM(libzone)), &
                                                   routine="MSP_acc_get_val",type=MSP_ENUM_ERREUR)
            return     
         endif
      endif

   end subroutine MSP_acc_get_val_i

   subroutine MSP_acc_get_val_d (acc,libzone,vald,unit,valdef) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_val_d
!
!$Resume
!  Lecture d'une valeur réelle dans une zone d'accès MADONA.
!
!$Description
!  Lecture d'une valeur réelle dans une zone d'accès MADONA.
!
!$Auteur
!  D. Semeux (SLB)
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_val_d (acc,libzone,vald,[unit],[valdef]) 
!.    integer :: acc
!.    character(LEN=*) :: libzone
!.    real(kind=pm_reel) :: vald 
!.    character(LEN=*) :: unit
!.    real(kind=pm_reel) :: valdef 
!
!$Arguments
!>E     acc      :<integer>   numéro d'accès MADONA
!>E     libzone  :<LEN=*>     libellé de la valeur
!>S     vald     :<pm_reel>   valeur lue
!>[E]   unit     :<LEN=*>     unité [par défaut ""]
!>[E]   valdef   :<pm_reel>   valeur par défaut
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
!   ACCES FICHIER MADONA  REEL 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      character(LEN=*), intent(IN) :: libzone
      real(kind=pm_reel), intent(OUT) :: vald  
      character(LEN=*), intent(IN), optional :: unit
     real(kind=pm_reel), intent(IN), optional :: valdef 
 
      integer :: ierreur

      ! Lecture de la valeur 
      ierreur = acc_exist(acc,libzone)

      if ( ierreur <0 ) then
         if (present(valdef)) then
            vald = valdef
         else
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la valeur '//libzone(1:LEN_TRIM(libzone)), &
                                    routine="MSP_acc_get_val",type=MSP_ENUM_ERREUR)
         endif
         return     
      else if ( ierreur == 0 ) then
         if (present(valdef)) then
            vald = valdef
         else
            call MSP_signaler_message (cle_mes="MSP_VALEUR_ABSENTE",partie_variable=libzone(1:LEN_TRIM(libzone)), &
                                    routine="MSP_acc_get_val",type=MSP_ENUM_WARNING)
         endif
         return  
   
      else
         ! Lecture d'un réel
         ierreur = acc_getd(acc,libzone,vald,unit)
 
         if ( ierreur /= 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='de la valeur '//libzone(1:LEN_TRIM(libzone)), &
                                                   routine="MSP_acc_get_val",type=MSP_ENUM_ERREUR)
            return     
         endif
      endif

   end subroutine MSP_acc_get_val_d


   subroutine MSP_acc_get_val_c (acc,libzone,valc,valdef)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_val_c
!
!$Resume
!  Lecture d'une chaine de charactères dans une zone d'accès MADONA.
!
!$Description
!  Lecture d'une chaine de charactères dans une zone d'accès MADONA.
!
!$Auteur
!  D. Semeux (SLB)
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_val_c (acc,libzone,valc,[valdef])
!.    integer :: acc
!.    character(LEN=*) :: libzone
!.    character(LEN=*) :: valc 
!.    character(LEN=*) :: valdef
!
!$Arguments
!>E     acc      :<integer>   numéro d'accès MADONA
!>E     libzone  :<LEN=*>     libellé de la valeur
!>S     valc     :<LEN=*>     valeur lue
!>[E]   valdef   :<LEN=*>     valeur par défaut
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
!   ACCES FICHIER MADONA CARACTERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      character(LEN=*), intent(IN) :: libzone
      character(LEN=*), intent(OUT) :: valc   
      character(LEN=*), intent(IN), optional :: valdef
 
      integer :: ierreur

      ! Lecture de la valeur 
      ierreur = acc_exist(acc,libzone)

      if ( ierreur < 0 ) then
         if (present(valdef)) then
            valc = valdef
         else
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la valeur '//libzone(1:LEN_TRIM(libzone)), &
                                    routine="MSP_acc_get_val",type=MSP_ENUM_ERREUR)
         endif
         return     
      else if ( ierreur == 0 ) then
         if (present(valdef)) then
            valc = valdef
         else
            call MSP_signaler_message (cle_mes="MSP_VALEUR_ABSENTE",partie_variable=libzone(1:LEN_TRIM(libzone)), &
                                    routine="MSP_acc_get_val")
         endif
         return 
   
      else

         ! Lecture d'une chaine
         ierreur = acc_gets(acc,libzone,valc)

         if ( ierreur /= 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='de la valeur '//libzone(1:LEN_TRIM(libzone)), &
                                                   routine="MSP_acc_get_val",type=MSP_ENUM_ERREUR)
            return     
         endif
      endif

   end subroutine MSP_acc_get_val_c


   subroutine MSP_acc_get_tab2i (acc,libtab,tabi,dim1,dim2)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab2i
!
!$Resume
!  Lecture d'un tableau d'entier à deux dimensions.
!
!$Description
!  Lecture d'un tableau d'entier à deux dimensions.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab2i (acc,libtab,tabi,[dim1],[dim2])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:,:) :: tabi
!.    integer :: dim1,dim2
!
!$Arguments
!>E/S   acc     :<integer>                     numéro d'accès MADONA
!>E/S   libtab  :<LEN=*>                       libellé du tableau
!>E/S   tabi    :<integer,DIM=(:,:),pointer>   tableau lu
!>[S]   dim1    :<integer>                     première dimension du tableau lu
!>[S]   dim2    :<integer>                     seconde dimension du tableau lu
!
!$Common
!
!$Routines
!- MSP_acc_get_tab2
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION2 ENTIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: acc
      character(LEN=*) :: libtab
      integer, pointer, dimension(:,:) :: tabi
      integer, intent(OUT), optional :: dim1,dim2

      call MSP_acc_get_tab2 (acc,libtab,tabi=tabi,dim1=dim1,dim2=dim2)

   end subroutine MSP_acc_get_tab2i

   subroutine MSP_acc_get_tab2d (acc,libtab,tabd,unit,dim1,dim2)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab2d
!
!$Resume
!  Lecture d'un tableau de "pm_reel" à deux dimensions.
!
!$Description
!  Lecture d'un tableau de "pm_reel" à deux dimensions.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab2d (acc,libtab,tabd,unit,[dim1],[dim2])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    real(kind=pm_reel), pointer, dimension(:,:) :: tabd
!.    character(LEN=*) :: unit
!.    integer :: dim1,dim2
!
!$Arguments
!>E/S   acc     :<integer>                     numéro d'accès MADONA
!>E/S   libtab  :<LEN=*>                       libellé du tableau
!>E/S   tabd    :<pm_reel,DIM=(:,:),pointer>   tableau lu
!>E/S   unit    :<LEN=*>                       unité [par défaut ""]
!>[S]   dim1    :<integer>                     première dimension du tableau lu
!>[S]   dim2    :<integer>                     seconde dimension du tableau lu
!
!$Common
!
!$Routines
!- MSP_acc_get_tab2
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION2 REEL
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: acc
      character(LEN=*) :: libtab
      real(kind=pm_reel), pointer, dimension(:,:) :: tabd
      character(LEN=*) :: unit
      integer, intent(OUT), optional :: dim1,dim2

      call MSP_acc_get_tab2 (acc,libtab,tabd=tabd,unit=unit,dim1=dim1,dim2=dim2)

   end subroutine MSP_acc_get_tab2d

   subroutine MSP_acc_get_tab2c (acc,libtab,tabc,dim1,dim2)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab2c
!
!$Resume
!  Lecture d'un tableau de chaîne de caractères à deux dimensions.
!
!$Description
!  Lecture d'un tableau de chaîne de caractères à deux dimensions.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab2c (acc,libtab,tabc,[dim1],[dim2])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    character(LEN=*), pointer, dimension(:,:) :: tabc
!.    integer :: dim1,dim2
!
!$Arguments
!>E/S   acc     :<integer>                   numéro d'accès MADONA
!>E/S   libtab  :<LEN=*>                     libellé du tableau
!>E/S   tabc    :<LEN=*,DIM=(:,:),pointer>   tableau lu
!>[S]   dim1    :<integer>                   première dimension du tableau lu
!>[S]   dim2    :<integer>                   seconde dimension du tableau lu
!
!$Common
!
!$Routines
!- MSP_acc_get_tab2
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION2 CARACTERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: acc
      character(LEN=*) :: libtab
      character(LEN=*), pointer, dimension(:,:) :: tabc
      integer, intent(OUT), optional :: dim1,dim2

      call MSP_acc_get_tab2 (acc,libtab,tabc=tabc,dim1=dim1,dim2=dim2)

   end subroutine MSP_acc_get_tab2c

   subroutine MSP_acc_get_tab2 (acc,libtab,tabi,tabd,tabc,unit,dim1,dim2)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab2
!
!$Resume
!  Lecture d'un tableau (entier,pm_reel ou chaîne de charactères) à deux dimensions.
!
!$Description
!  Lecture d'un tableau (entier,pm_reel ou chaîne de charactères) à deux dimensions.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_acc_get_tab2 (acc,libtab,[tabi],[tabd],[tabc],[unit],[dim1],[dim2])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:,:) :: tabi
!.    real(kind=pm_reel), pointer, dimension(:,:) :: tabd
!.    character(LEN=*), pointer, dimension(:,:) :: tabc
!.    character(LEN=*) :: unit
!.    integer :: dim1,dim2
!
!$Arguments
!>E/S   acc     :<integer>                     numéro d'accès MADONA
!>E/S   libtab  :<LEN=*>                       libellé du tableau
!>[E/S] tabi    :<integer,DIM=(:,:),pointer>   tableau lu (si entier)
!>[E/S] tabd    :<pm_reel,DIM=(:,:),pointer>   tableau lu (si pm_reel)
!>[E/S] tabc    :<LEN=*,DIM=(:,:),pointer>     tableau lu (si chaîne de caractère)
!>[E/S] unit    :<LEN=*>                       unité [par défaut ""]
!>[S]   dim1    :<integer>                     première dimension du tableau lu
!>[S]   dim2    :<integer>                     seconde dimension du tableau lu
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
!   ACCES FICHIER MADONA TABLEAU DIMENSION2 ENTIER REEL CARACTERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: acc
      character(LEN=*) :: libtab
      integer, pointer, dimension(:,:), optional :: tabi
      real(kind=pm_reel), pointer, dimension(:,:), optional :: tabd
      character(LEN=*), pointer, dimension(:,:), optional :: tabc
      character(LEN=*), optional :: unit
      integer, intent(OUT), optional :: dim1,dim2

      integer :: ierreur

      integer :: i,j,itab

      integer :: nb_el1,nb_el2
      
      integer :: MSP_iostat
      
      ! initialisation de MSP_iostat
      MSP_iostat = 0

      ! Lecture de la dimension de la grille en x
      nb_el1 = acc_get_dim(acc,libtab)
      if ( nb_el1 <= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                    partie_variable='de la première dimension du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
         return 
      else    
         if ( present(dim1) ) then
            dim1 = nb_el1
         endif 
      endif
    
      ierreur = acc_select(acc,libtab,ACC_TABL)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
         return     
      endif
      ierreur = acc_set_index(acc,1)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
         return     
      endif

      ! Lecture de la dimension de la grille en y
      nb_el2 = acc_get_dim(acc,ACC_INDEX)
      if ( nb_el2 <= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                    partie_variable='de la deuxième dimension du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
         return 
      else    
         if ( present(dim2) ) then
            dim2 = nb_el2
         endif 
      endif

      ! Tests sur la présence des arguments optionnels:
      itab = 0
      if ( present(tabi) ) then
         itab = itab + 1
      endif
      if ( present(tabd) ) then
         itab = itab + 1
      endif
      if ( present(tabc) ) then
         itab = itab + 1
      endif
      if ( itab == 0 ) then
         call MSP_signaler_message (cle_mes="MSP_acc_get_tab_001")
         return
      else  if ( itab > 1 ) then
         call MSP_signaler_message (cle_mes="MSP_acc_get_tab_002")
         return
      endif

      ! Allocation mémoire du tableau à remplir:
      if ( present(tabi) ) then    
         if ( ASSOCIATED (tabi) ) then
            DEALLOCATE (tabi,stat=MSP_iostat)
         end if
         ALLOCATE (tabi(nb_el2,nb_el1))
      else if ( present(tabd) ) then    
         if ( ASSOCIATED (tabd) ) then
            DEALLOCATE (tabd,stat=MSP_iostat)
         end if
         ALLOCATE (tabd(nb_el2,nb_el1))
      else  
         if ( ASSOCIATED (tabc) ) then
            DEALLOCATE (tabc,stat=MSP_iostat)
         end if
         ALLOCATE (tabc(nb_el2,nb_el1))
      endif

      boucle_2d : do i = 1 , nb_el1
                     ierreur = acc_set_index(acc,i)
                     if ( ierreur < 0 ) then
                        call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                   routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
                        return     
                     endif
                     ierreur = acc_select(acc,ACC_INDEX,ACC_TABL)
                     if ( ierreur < 0 ) then
                        call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                   routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
                        return     
                     endif
                     do j = 1 , nb_el2
                        ierreur = acc_set_index(acc,j)
                        if ( ierreur < 0 ) then
                           call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                      partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                      routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
                           return     
                        endif
                        if ( present(tabi) ) then    
                           ierreur = acc_geti(acc,ACC_INDEX,tabi(j,i))
                        else if ( present(tabd) ) then    
                           ierreur = acc_getd(acc,ACC_INDEX,tabd(j,i),unit)
                        else
                           ierreur = acc_gets(acc,ACC_INDEX,tabc(j,i))
                        endif
                        if ( ierreur < 0 ) then
                           call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                      partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                      routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
                           return     
                        endif
                     end do
                     ierreur = acc_select_end(acc)
                     if ( ierreur < 0 ) then
                        call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                   routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
                        return     
                     endif
                  end do boucle_2d

      ierreur = acc_select_end(acc)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab2",type=MSP_ENUM_ERREUR)
         return     
      endif

   end subroutine MSP_acc_get_tab2

   subroutine MSP_acc_get_tab3i (acc,libtab,tabi,dim1,dim2,dim3)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab3i
!
!$Resume
!  Lecture d'un tableau d'entier à trois dimensions.
!
!$Description
!  Lecture d'un tableau d'entier à trois dimensions.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab3i (acc,libtab,tabi,[dim1],[dim2],[dim3])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:,:,:) :: tabi
!.    integer :: dim1,dim2,dim3
!
!$Arguments
!>E/S   acc     :<integer>                       numéro d'accès MADONA 
!>E/S   libtab  :<LEN=*>                         libellé du tableau 
!>E/S   tabi    :<integer,DIM=(:,:,:),pointer>   tableau lu 
!>[S]   dim1    :<integer>                       première dimension du tableau lu 
!>[S]   dim2    :<integer>                       seconde dimension du tableau lu 
!>[S]   dim3    :<integer>                       troisième dimension du tableau lu 
!
!$Common
!
!$Routines
!- MSP_acc_get_tab3
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION3 ENTIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: acc
      character(LEN=*) :: libtab
      integer, pointer, dimension(:,:,:) :: tabi
      integer, intent(OUT), optional :: dim1,dim2,dim3

      call MSP_acc_get_tab3 (acc,libtab,tabi=tabi,dim1=dim1,dim2=dim2,dim3=dim3)

   end subroutine MSP_acc_get_tab3i

   subroutine MSP_acc_get_tab3d (acc,libtab,tabd,unit,dim1,dim2,dim3)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab3d
!
!$Resume
!  Lecture d'un tableau de "pm_reel" à trois dimensions.
!
!$Description
!  Lecture d'un tableau de "pm_reel" à trois dimensions.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab3d (acc,libtab,tabd,unit,[dim1],[dim2],[dim3])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    real(kind=pm_reel), pointer, dimension(:,:,:) :: tabd
!.    character(LEN=*) :: unit
!.    integer :: dim1,dim2,dim3
!
!$Arguments
!>E/S   acc     :<integer>                       numéro d'accès MADONA 
!>E/S   libtab  :<LEN=*>                         libellé du tableau 
!>E/S   tabd    :<pm_reel,DIM=(:,:,:),pointer>   tableau lu
!>E/S   unit    :<LEN=*>                         unité [par défaut ""]
!>[S]   dim1    :<integer>                       première dimension du tableau lu 
!>[S]   dim2    :<integer>                       seconde dimension du tableau lu 
!>[S]   dim3    :<integer>                       troisième dimension du tableau lu 
!
!$Common
!
!$Routines
!- MSP_acc_get_tab3
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION3 REEL
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: acc
      character(LEN=*) :: libtab
      real(kind=pm_reel), pointer, dimension(:,:,:) :: tabd
      character(LEN=*) :: unit
      integer, intent(OUT), optional :: dim1,dim2,dim3

      call MSP_acc_get_tab3 (acc,libtab,tabd=tabd,unit=unit,dim1=dim1,dim2=dim2,dim3=dim3)

   end subroutine MSP_acc_get_tab3d

   subroutine MSP_acc_get_tab3c (acc,libtab,tabc,dim1,dim2,dim3)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab3c
!
!$Resume
!  Lecture d'un tableau de chaîne de caractères à trois dimensions.
!
!$Description
!  Lecture d'un tableau de chaîne de caractères à trois dimensions.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_acc_get_tab3c (acc,libtab,tabc,[dim1],[dim2],[dim3])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    character(LEN=*), pointer, dimension(:,:,:) :: tabc
!.    integer :: dim1,dim2,dim3
!
!$Arguments
!>E/S   acc     :<integer>                     numéro d'accès MADONA 
!>E/S   libtab  :<LEN=*>                       libellé du tableau 
!>E/S   tabc    :<LEN=*,DIM=(:,:,:),pointer>   tableau lu 
!>[S]   dim1    :<integer>                     première dimension du tableau lu 
!>[S]   dim2    :<integer>                     seconde dimension du tableau lu 
!>[S]   dim3    :<integer>                     troisième dimension du tableau lu
!
!$Common
!
!$Routines
!- MSP_acc_get_tab3
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   ACCES FICHIER MADONA TABLEAU DIMENSION3 CARACTERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: acc
      character(LEN=*) :: libtab
      character(LEN=*), pointer, dimension(:,:,:) :: tabc
      integer, intent(OUT), optional :: dim1,dim2,dim3

      call MSP_acc_get_tab3 (acc,libtab,tabc=tabc,dim1=dim1,dim2=dim2,dim3=dim3)

   end subroutine MSP_acc_get_tab3c

   subroutine MSP_acc_get_tab3 (acc,libtab,tabi,tabd,tabc,unit,dim1,dim2,dim3)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_get_tab3
!
!$Resume
!  Lecture d'un tableau (entier,pm_reel ou chaîne de charactères) à trois dimensions.
!
!$Description
!  Lecture d'un tableau (entier,pm_reel ou chaîne de charactères) à trois dimensions.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_acc_get_tab3 (acc,libtab,[tabi],[tabd],[tabc],[unit],[dim1],[dim2],[dim3])
!.    integer :: acc
!.    character(LEN=*) :: libtab
!.    integer, pointer, dimension(:,:,:) :: tabi
!.    real(kind=pm_reel), pointer, dimension(:,:,:) :: tabd
!.    character(LEN=*), pointer, dimension(:,:,:) :: tabc
!.    character(LEN=*) :: unit
!.    integer :: dim1,dim2,dim3
!
!$Arguments
!>E/S   acc     :<integer>                       numéro d'accès MADONA 
!>E/S   libtab  :<LEN=*>                         libellé du tableau 
!>[E/S] tabi    :<integer,DIM=(:,:,:),pointer>   tableau lu (si entier)
!>[E/S] tabd    :<pm_reel,DIM=(:,:,:),pointer>   tableau lu (si pm_reel)
!>[E/S] tabc    :<LEN=*,DIM=(:,:,:),pointer>     tableau lu (si chaîne de caractères)
!>[E/S] unit    :<LEN=*>                         unité [par défaut ""]
!>[S]   dim1    :<integer>                       première dimension du tableau lu 
!>[S]   dim2    :<integer>                       seconde dimension du tableau lu 
!>[S]   dim3    :<integer>                       troisième dimension du tableau lu 
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
!   ACCES FICHIER MADONA TABLEAU DIMENSION3 ENTIER REEL CARACTERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer :: acc
      character(LEN=*) :: libtab
      integer, pointer, dimension(:,:,:), optional :: tabi
      real(kind=pm_reel), pointer, dimension(:,:,:), optional :: tabd
      character(LEN=*), pointer, dimension(:,:,:), optional :: tabc
      character(LEN=*), optional :: unit
      integer, intent(OUT), optional :: dim1,dim2,dim3

      integer :: ierreur

      integer :: i,j,k,itab

      integer :: nb_el1,nb_el2,nb_el3
      
      integer :: MSP_iostat
      
      ! initialisation MSP_iostat
      MSP_iostat = 0

      ! Lecture de la dimension de la grille en x
      nb_el1 = acc_get_dim(acc,libtab)
      if ( nb_el1 <= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                    partie_variable='de la première dimension du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return 
      else    
         if ( present(dim1) ) then
            dim1 = nb_el1
         endif 
      endif
    
      ierreur = acc_select(acc,libtab,ACC_TABL)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return     
      endif
      ierreur = acc_set_index(acc,1)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return     
      endif

      ! Lecture de la dimension de la grille en y
      nb_el2 = acc_get_dim(acc,ACC_INDEX)
      if ( nb_el2 <= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                    partie_variable='de la deuxième dimension du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return 
      else    
         if ( present(dim2) ) then
            dim2 = nb_el2
         endif 
      endif

      ierreur = acc_get_type(acc,ACC_INDEX)
      ierreur = acc_select(acc,ACC_INDEX,ACC_TABL)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return     
      endif
      ierreur = acc_set_index(acc,1)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return     
      endif

      ! Lecture de la dimension de la grille en z
      nb_el3 = acc_get_dim(acc,ACC_INDEX)
      if ( nb_el3 <= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                    partie_variable='de la troisième dimension du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return 
      else    
         if ( present(dim3) ) then
            dim3 = nb_el3
         endif 
      endif

      ierreur = acc_select_end(acc)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return     
      endif

      ! Tests sur la présence des arguments optionnels:
      itab = 0
      if ( present(tabi) ) then
         itab = itab + 1
      endif
      if ( present(tabd) ) then
         itab = itab + 1
      endif
      if ( present(tabc) ) then
         itab = itab + 1
      endif
      if ( itab == 0 ) then
         call MSP_signaler_message (cle_mes="MSP_acc_get_tab_001")
         return
      else  if ( itab > 1 ) then
         call MSP_signaler_message (cle_mes="MSP_acc_get_tab_002")
         return
      endif

      ! Allocation mémoire du tableau à remplir:
      if ( present(tabi) ) then    
         if ( ASSOCIATED (tabi) ) then
            DEALLOCATE (tabi,stat=MSP_iostat)
         end if
         ALLOCATE (tabi(nb_el3,nb_el2,nb_el1))
      else if ( present(tabd) ) then    
         if ( ASSOCIATED (tabd) ) then
            DEALLOCATE (tabd,stat=MSP_iostat)
         end if
         ALLOCATE (tabd(nb_el3,nb_el2,nb_el1))
      else  
         if ( ASSOCIATED (tabc) ) then
            DEALLOCATE (tabc,stat=MSP_iostat)
         end if
         ALLOCATE (tabc(nb_el3,nb_el2,nb_el1))
      endif

      boucle_x  : do i = 1 , nb_el1
                     ierreur = acc_set_index(acc,i)
                     if ( ierreur < 0 ) then
                        call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                   routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
                        return     
                     endif
                     ierreur = acc_select(acc,ACC_INDEX,ACC_TABL)
                     if ( ierreur < 0 ) then
                        call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                   routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
                        return     
                     endif
          boucle_y : do j = 1 , nb_el2
                        ierreur = acc_set_index(acc,j)
                        if ( ierreur < 0 ) then
                           call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                      partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                      routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
                           return     
                        endif
                        ierreur = acc_select(acc,ACC_INDEX,ACC_TABL)
                        if ( ierreur < 0 ) then
                           call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                      partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                      routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
                           return     
                        endif
             boucle_z : do k = 1 , nb_el3
                           ierreur = acc_set_index(acc,k)
                           if ( ierreur < 0 ) then
                              call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                         partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                         routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
                              return     
                           endif
                           if ( present(tabi) ) then    
                              ierreur = acc_geti(acc,ACC_INDEX,tabi(k,j,i))
                           else if ( present(tabd) ) then    
                              ierreur = acc_getd(acc,ACC_INDEX,tabd(k,j,i),unit)
                           else
                              ierreur = acc_gets(acc,ACC_INDEX,tabc(k,j,i))
                           endif
                           if ( ierreur < 0 ) then
                              call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                         partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                         routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
                              return     
                           endif
                        end do boucle_z
                        ierreur = acc_select_end(acc)
                        if ( ierreur < 0 ) then
                           call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                      partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                      routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
                           return     
                        endif
                     end do boucle_y
                     ierreur = acc_select_end(acc)
                     if ( ierreur < 0 ) then
                        call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                                                   partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                                   routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
                        return     
                     endif
                  end do boucle_x

      ierreur = acc_select_end(acc)
      if ( ierreur < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du tableau '//libtab(1:LEN_TRIM(libtab)), &
                                    routine="MSP_acc_get_tab3",type=MSP_ENUM_ERREUR)
         return     
      endif

   end subroutine MSP_acc_get_tab3

   function MSP_ouverture_fichier (fichier,repertoire,recl,status,nlogd) result (ifich)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ouverture_fichier
!
!$Resume
!  Ouverture d'un fichier ASCII normal.
!
!$Description
!  Ouverture d'un fichier ASCII normal avec une recherche d'un numéro logique encore non attribué.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  ifich = MSP_ouverture_fichier (fichier,[repertoire],[recl],[status],[nlogd])
!.    character(LEN=*) :: fichier
!.    character(LEN=*) :: repertoire
!.    integer :: recl
!.    character(LEN=*) :: status
!.    integer :: nlogd
!.    integer :: ifich
!
!$Arguments
!>E     fichier     :<LEN=*>     nom du fichier
!>[E]   repertoire  :<LEN=*>     nom du répertoire [par défaut "."]
!>[E]   recl        :<integer>   taille maximum d'une ligne [par défaut MSP_RECMAX]
!>[E]   status      :<LEN=*>     status (cf. documentation compilateur)
!>[E]   nlogd       :<integer>   numéro logique de départ
!>S     ifich       :<integer>   numéro logique associé au fichier en ouverture
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
!   FICHIER OUVERTURE I/O
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSP_MECASPA_DEF

      implicit none

      character(LEN=*), intent(IN) :: fichier
      character(LEN=*), intent(IN), optional :: repertoire
      integer, intent(IN), optional :: recl
      character(LEN=*), intent(IN), optional :: status
      integer, intent(IN), optional :: nlogd

      integer :: ifich

      logical :: fichier_ouvert
      integer :: reclen,ilog,ier

      fichier_ouvert = .true.

      ! Longueur d'enregistrement:
      if ( present (recl) ) then
         reclen = recl
      else
         reclen = MSP_RECMAX
      endif

      ! Numéro logique de départ pour la recherche:
      if ( present (nlogd) ) then
         ilog = nlogd - 1
      else
         ilog = MSP_ENUM_ECRAN
      endif

      ! Boucle pour trouver un numéro logique libre:
      do while ( fichier_ouvert )
         ilog = ilog + 1
         inquire(ilog,opened=fichier_ouvert)
      enddo

      ifich = ilog
      if ( present(repertoire) ) then
         if ( present (status) ) then
            open (ifich,file=repertoire(1:LEN_TRIM(repertoire))//"/"//fichier,RECL=reclen,IOSTAT=ier,STATUS=status)
         else
            open (ifich,file=repertoire(1:LEN_TRIM(repertoire))//"/"//fichier,RECL=reclen,IOSTAT=ier)
         endif
      else
         if ( present (status) ) then
            open (ifich,file=fichier,RECL=reclen,IOSTAT=ier,STATUS=status)
         else
            open (ifich,file=fichier,RECL=reclen,IOSTAT=ier)
         endif
      endif
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_OUVERTURE_FICHIER",partie_variable=fichier(1:LEN_TRIM(fichier)), &
                                    routine="MSP_ouverture_fichier",type=MSP_ENUM_WARNING)     
      endif
         
   end function MSP_ouverture_fichier

   subroutine MSP_leccom (num_fich,carcom,ligne)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_leccom
!
!$Resume
!  Lecture des lignes d'un fichier qui ne sont pas des commentaires.
!
!$Description
!  Sous-programme sautant les lignes de commentaires dans un fichier possédant
!  le numéro logique "num_fich", (ces lignes doivent commencer par le caractere
!  "carcom").
!  En sortie, on aura une ligne "ligne" dans laquelle on pourra lire les valeurs
!  numériques attendues par l'instruction: read (ligne,*) (... iolist ...)
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_leccom (num_fich,carcom,ligne)
!.    integer :: num_fich
!.    character(LEN=1) :: carcom
!.    character(LEN=*) :: ligne
!
!$Arguments
!>E     num_fich  :<integer>   numéro logique du fichier
!>E     carcom    :<LEN=1>     caractère identifiant une ligne commentaire
!>S     ligne     :<LEN=*>     ligne qui n'est pas un commentaire
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
!   FICHIER LECTURE I/O
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN)           :: num_fich
      character(LEN=1), intent(IN)  :: carcom
      character(LEN=*), intent(OUT) :: ligne

      integer :: ier

      ligne(1:1)=carcom

      do while ( ligne(1:1) == carcom )
         read (num_fich,'(a)',iostat=ier) ligne
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="MSP_FIN_FICHIER",routine="MSP_leccom",type=MSP_ENUM_WARNING)
            exit
         else if ( ier > 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",routine="MSP_leccom",type=MSP_ENUM_ERREUR)
            exit
         endif
      end do

   end subroutine MSP_leccom


   subroutine MSP_ouverture_MADONA_col(nomfic, accres)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ouverture_MADONA_col
!
!$Resume
!  Initialisation du fichier RESULT
!
!$Description
!  Ouverture MADONA d'un fichier colonnes. 
!  L'initialisation du header du fichier ainsi que
!  les initialisations du descripteur de colonnes devront
!  être réalisées plus tard.
!
!$Auteur
!  A. Deramecourt/D. Semeux (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ouverture_MADONA_col(nomfic, accres)
!.    character(LEN=*) :: nomfic
!.    integer :: accres
!
!$Arguments
!>E     nomfic  :<LEN=*>     nom du fichier 
!>E/S   accres  :<integer>   numéro de la zone d'accès MADONA ouverte correspondante
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!#V
!- acces_F.h
!#
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

     !	INCLUDES
#include "acces_F.h"

!	ARGUMENTS
     character(LEN=*),intent(in)            :: nomfic
     integer, intent(inout)                 :: accres

!    Variables locales
     integer    ::  retres

!/======================================
!/ CORPS DU PROGRAMME
!/======================================   

 
    accres = acc_open ()
    if (accres < 0) then
       call MSP_signaler_message (cle_mes="MSP_OPEN_MADONA",routine='MSP_ouverture_MADONA_col')
       return
    endif

    retres = acc_connect (accres,nomfic, ACC_W)
    if (retres /= 0) then
       call MSP_signaler_message (cle_mes ="MSP_CONNECT_MADONA",routine='MSP_ouverture_MADONA_col', &
                                  partie_variable=nomfic)
       return
    endif
       
    !/ creation du fichier colonne
    retres = acc_set_ftype (accres,ACC_FIL_COL)
    if (retres /= 0) then
       call MSP_signaler_message (cle_mes ="MSP_ERR_FCT_MADONA",routine='MSP_ouverture_MADONA_col', &
                                  partie_variable='acc_set_ftype' )
       return
    endif
     
    !/ Force le SELECT même en cas d'erreur
    retres = acc_set_mode (accres,ACC_FORCE_SELECT,ACC_ON)
    if (retres /= 0) then
       call MSP_signaler_message (cle_mes ="MSP_ERR_FCT_MADONA",routine='MSP_ouverture_MADONA_col', &
                                  partie_variable='acc_set_mode' )
       return
    endif

   end subroutine MSP_ouverture_MADONA_col


   subroutine MSP_fermeture_MADONA (acc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_fermeture_MADONA
!
!$Resume
!  Fermeture des fichiers MADONA.
!
!$Description
!  Fermeture des fichiers MADONA.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_fermeture_MADONA (acc)
!.    integer :: acc
!
!$Arguments
!>E     acc  :<integer>   
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

     integer, intent(IN) :: acc

     !    Variables locales
     integer    ::  retres


     retres = acc_deconnect (acc, ACC_ALL)
     
     if (retres /= 0) then
        call MSP_signaler_message (cle_mes ="MSP_ERR_FCT_MADONA",routine='MSP_fermeture_MADONA', &
                                  partie_variable='acc_deconnect' )
        return
     endif

     retres = acc_close (acc)
    if (retres /= 0) then
       call MSP_signaler_message (cle_mes ="MSP_ERR_FCT_MADONA",routine='MSP_fermeture_MADONA', &
                                  partie_variable='acc_close' )
       return
    endif

   end subroutine MSP_fermeture_MADONA


end module MSP_ACCES
