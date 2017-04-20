!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!	Routines utiles pour les IHM de COMPAS
!
!$Version
!  $Id: ui_ihm.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_ihm.F90,v $
!  Revision 1.21  2010/11/18 14:52:31  ogarat
!  VERSION::FA-ID:1442:18/11/2010:Initialisation de variable
!
!  Revision 1.20  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.19  2010/05/28 08:19:55  jlrobin
!  VERSION::FA-ID:1357:28/05/2010:Ajout d'un test de verification
!
!  Revision 1.17  2010/05/25 09:39:46  jlrobin
!  VERSION::FA-ID:1356:25/05/2010:Correction dans l'affichage des messages du journal de bord
!
!  Revision 1.16  2009/11/12 16:12:34  cmartel
!  FA-ID 1197 : Simplification du systeme de copie des donnees associees a un fichier
!
!  Revision 1.15  2009/09/14 15:35:09  cmartel
!  FA-ID 1276 : Correction de l appel systeme au chemin courant
!
!  Revision 1.14  2009/09/09 12:53:14  cmartel
!  FA-ID 1157 : Correction de constantes non nommées
!
!  Revision 1.13  2008/11/07 14:35:30  cml
!  AQ : Correction de pointeurs non initialises
!
!  Revision 1.12  2008/11/07 14:14:29  cml
!  AQ : Suppression de variables inutilises
!
!  Revision 1.11  2008/11/07 09:56:10  cml
!  AQ : Correction de warning foresys
!
!  Revision 1.10  2008/11/04 16:27:22  cml
!  FA-ID 1098 : Correction pour prise en comptes des chemins relatifs lors de la copie
!
!  Revision 1.9  2008/11/04 10:25:43  cml
!  FA-ID 1098 : Correction historique des modifications
!
!  Revision 1.8  2008/11/04 10:22:55  cml
!  FA-ID 1098 : Remplacement de l analyse des fichiers et repertoire par des appels a tar
!
!  Revision 1.7  2008/11/03 17:05:47  cml
!  FA-ID 1098 : Copie des liens dynamique lors de la recopie d une partie de la base de ref
!
!  Revision 1.6  2008/10/31 16:43:46  cml
!  FA-ID 1098 : Ajout de la verification des liens symboliques avant copie d un repertoire
!
!  Revision 1.5  2008/04/29 08:09:08  vivaresf
!  COMPAS_UI, V2.4, AQ : suppression des variables inutilisées
!
!  Revision 1.4  2008/04/11 12:13:50  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches
!
!  Revision 1.3  2008/04/11 10:09:24  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!
!  Revision 1.2  2008/03/04 10:35:40  vivaresf
!  FA-ID 843 : gestion d'erreur lors de la mise à jour des champs
!
!  Revision 1.1  2008/02/08 17:51:22  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!
!  Revision 1.29  2006/11/21 12:08:43  vivaresf
!  Version 2.1 : passage Understand, variables inutilisées
!
!  Revision 1.28  2006/10/23 12:56:28  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!
!  Revision 1.27.2.1  2006/10/23 10:12:12  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!
!  Revision 1.27  2006/09/04 09:11:45  vivaresf
!  Validation V2.0.1
!
!  Revision 1.26  2006/08/29 14:55:33  vpg
!  FA-ID 583 : copie de fichiers de ressources associe aux elements d'une table, comme les modeles de potentiels par exemple
!
!  Revision 1.25  2006/08/28 13:31:19  vpg
!  FA-ID 582 : correction de l'initialisation du fichier de configuration de la base locale
!
!  Revision 1.24  2006/08/28 13:27:04  vpg
!  FA-ID 583 : copie de fichiers de ressources associe aux elements d'une table, comme les modeles de potentiels par exemple
!
!  Revision 1.23  2006/07/05 17:24:02  vpg
!  Ajout de l'option -t a la commande ls
!
!  Revision 1.22  2006/06/16 17:32:05  vivaresf
!  Cartouches d'entete
!
!  Revision 1.21  2006/06/14 12:38:12  vivaresf
!  DM-ID 387 : mise au point de CREATEPHEM
!
!  Revision 1.20  2006/06/01 12:49:28  vivaresf
!  Copie depuis la base de ref et la nouvelle base
!
!  Revision 1.19  2006/05/31 13:08:39  vivaresf
!  COMPAS 2.0 : validation
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function cps_ihm_creerFichier(abs_path) result(creation_ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_creerFichier
!
!$Resume
!
!$Description
!  Creation d\un fichier MADONA vide
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  creation_ok = cps_ihm_creerFichier(abs_path)
!.    character(LEN=*) :: abs_path
!.    integer :: creation_ok
!
!$Arguments
!>E     abs_path     :<LEN=*>     chemin complet du fichier à créer
!>S     creation_ok  :<integer>   0 si le fichier a été correctement créé
!
!$Common
!
!$Routines
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
#include "acces_F.h"
  ! arguments
  character(LEN=*), intent(in) :: abs_path
  
  ! variables locales
  integer :: acces, ret, creation_ok

  !ret = acc_err_setmode(ACC_ERR_ECHO)
  
  ! creation d\'un acces MADONA
  acces = acc_open()
  ! connection en ecriture au fichier
  ret = acc_connect(acces, trim(abs_path), ACC_W)
  ! ecriture d\'une zone vide : seule la ligne #<AM-acces-V2.0>
  ! est ecrite
  creation_ok = acc_write(acces, ACC_ALL)
  ! fermeture de l\'acces
  ret = acc_close(acces)
end function cps_ihm_creerFichier


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Ecriture de tout le contenu d\'un acces MADONA dans un fichier. Le !!
!! fichier doit avoir deja ete cree.                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_ecrireFichier(acces, fichier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_ecrireFichier
!
!$Resume
!
!$Description
!  Ecriture de tout le contenu d\'un acces MADONA dans un fichier.
!  Le fichier doit avoir deja ete cree.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_ecrireFichier(acces, fichier)
!.    integer :: acces
!.    character(LEN=*) :: fichier
!
!$Arguments
!>E     acces    :<integer>   accès MADONA ouvert (en lecture) sur le fichier à écrire 
!>E     fichier  :<LEN=*>     nom complet du fichier à écrire
!
!$Common
!
!$Routines
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
#include "acces_F.h"
  ! arguments
  integer, intent(in) :: acces
  character(LEN=*), intent(in) :: fichier

  ! variables locales
  integer :: ret

  if (acces.ge.0) then
     ! deconnection de l'acces en lecture
     ! C'est une simple précaution, pour éviter un message d'erreur, on le zappe
     ! puis on le remet
     ret = acc_err_setmode(ACC_ERR_MEMO)
     ret = acc_deconnect(acces, ACC_R)
     ret = acc_err_setmode(ACC_ERR_ECHO)
     ! connection en écriture
     ret = acc_connect(acces, trim(fichier), ACC_W)
     ! écriture de la zone d'acces
     ret = acc_write(acces, ACC_ALL)
     ! déconnection en écriture
     ret = acc_deconnect(acces, ACC_W)
     ! il faut recharger tout le fichier dans la zone
     ! la zone est entièrement vidée apres écriture
     ret = acc_connect(acces, trim(fichier), ACC_R)
     ret = acc_read(acces, ACC_ALL)
  end if
end subroutine cps_ihm_ecrireFichier


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Ajouter un id dans le tableau \'cps_id\' de l\'acces MADONA associe!!
!! au fichier de configuration.                                       !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_ajouterIdConfig(acces, id)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_ajouterIdConfig
!
!$Resume
!
!$Description
!  Ajouter un id dans le tableau \'cps_id\' de l\'acces MADONA associe
!  au fichier de configuration.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_ajouterIdConfig(acces, id)
!.    integer :: acces
!.    character(LEN=*) :: id
!
!$Arguments
!>E     acces  :<integer>   accès MADONA ouvert sur le fichier de configuration de la base
!>E     id     :<LEN=*>     id à rajouter
!
!$Common
!
!$Routines
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
#include "acces_F.h"
  ! arguments
  integer, intent(in) :: acces
  character(LEN=*), intent(in) :: id
  
  ! variables locales
  integer :: ret, nb
  character(LEN=256) :: buff_tmp

  ret = acc_exist(acces, "cps_id")
  if (ret.ne.1) then
     ! si le tableau \'cps_id\' n\'existe pas, on le cree
     ! c\'est le cas lors de l\'ajout du premier id
     ret = acc_create(acces, "cps_id", ACC_TABL, "")
  end if
  ! nombre d\'id deja presents
  nb = acc_get_dim(acces, "cps_id")
  nb = nb+1
  write(buff_tmp,*) nb
  ! creation du nouveau element du tableau
  ret = acc_create(acces, "cps_id["//trim(buff_tmp) &
               //"]", ACC_PARAM, "")
  ret = acc_puts(acces, "cps_id["//trim(buff_tmp)   &
       //"]", trim(id))
end subroutine cps_ihm_ajouterIdConfig


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Supprimer un id dans le tableau \'cps_id\' de l\'acces MADONA      !!
!! associe au fichier de configuration.                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_supprimerIdConfig(acces, id)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_supprimerIdConfig
!
!$Resume
!
!$Description
!  Supprimer un id dans le tableau \'cps_id\' de l\'acces MADONA
!  associe au fichier de configuration.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_supprimerIdConfig(acces, id)
!.    integer :: acces
!.    character(LEN=*) :: id
!
!$Arguments
!>E     acces  :<integer>   accès MADONA ouvert sur le fichier de configuration de la base
!>E     id     :<LEN=*>     id à supprimer
!
!$Common
!
!$Routines
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
#include "acces_F.h"
  ! arguments
  integer, intent(in) :: acces
  character(LEN=*), intent(in) :: id
  
  ! variables locales
  integer :: ret, i, nb
  character(len=256) :: buff_tmp
  logical :: trouve = .false.
  
  ! oin verifie que le talbeau contenant les id dans le fichier
  ! de configuration existe
  ret = acc_exist(acces, "cps_id")
  if (ret.eq.1) then
     ! nombre d\'id presents
     nb = acc_get_dim(acces, "cps_id")
     ! parcourt des elements
     ret = acc_select(acces, "cps_id", ACC_TABL)
     do i=1, nb
        ret = acc_set_index(acces, i)
        ret  =acc_gets(acces, ACC_INDEX, buff_tmp)
        if (trim(buff_tmp).eq.trim(id)) then
           ! l\'id a supprimer est trouve
           trouve = .true.
           exit
        end if
     end do
     ret = acc_select_end(acces)
     if (trouve) then
        write(buff_tmp,*) i
        ! supppression de l\'id
        ret = acc_delete(acces, "cps_id["//trim(buff_tmp)//"]")
     end if
  end if
  
end subroutine cps_ihm_supprimerIdConfig


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Copie des descriptions du fichier de configuration de reference    !!
!! dans le fichier de configuration local                             !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_copyDescConfig(acces_ref, fichier_ref, acces_local)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_copyDescConfig
!
!$Resume
!
!$Description
!  Copie des descriptions du fichier de configuration de reference
!  dans le fichier de configuration local
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_copyDescConfig(acces_ref, fichier_ref, acces_local)
!.    integer :: acces_ref, acces_local
!.    character(LEN=*) :: fichier_ref
!
!$Arguments
!>E     acces_ref    :<integer>   accès MADONA ouvert sur le fichier de configuration de la base de référence
!>E     fichier_ref  :<LEN=*>     fichier à copié
!>E     acces_local  :<integer>   accès MADONA ouvert sur le fichier de configuration de la base locale
!
!$Common
!
!$Routines
!
!$Include
!#V
!- acces_F.h
!- cps_ihm.h
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
#include "acces_F.h"
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces_ref, acces_local
  character(LEN=*), intent(in) :: fichier_ref
  
  ! variables locales
  integer :: i, j, ret, nb_fic_ref
  character(LEN=256) :: buff_ref, buff_local, buff_tmp
  logical :: trouve

  ret = acc_exist(acces_local, "fichiers")
  if (ret.ne.1) then
     ! il faut creer le tableau \'fichiers\'
     ret = acc_create(acces_local, "fichiers", ACC_TABL, "")
  end if

  j = acc_get_dim(acces_local, "fichiers")+1
  
  ! acces à copier dans le fichier de configuration de référence
  nb_fic_ref = acc_get_dim(acces_ref, "fichiers")
  ret = acc_select(acces_ref, "fichiers", ACC_TABL)
  trouve = .false.
  i = 1
  do while ((i<=nb_fic_ref) .and. (.not.trouve))
     ret = acc_set_index(acces_ref, i)
     ! selection de la structure
     ret = acc_select(acces_ref, ACC_INDEX, ACC_STRUCT)
     ret = acc_gets(acces_ref, "fichier", buff_tmp)
     if (trim(buff_tmp)==trim(fichier_ref)) then
        ! fichier trouve
        trouve = .true.
     else
        i = i+1
     end if
     ! fin de la selection de la structure
     ret = acc_select_end(acces_ref)
  end do
  ret = acc_select_end(acces_ref)
  
  if (trouve) then
     ! copie de la description
     write(buff_tmp,*) i
     buff_ref = "fichiers["//trim(buff_tmp)//"]"
     write(buff_tmp,*) j
     buff_local = "fichiers["//trim(buff_tmp)//"]"
     ret = acc_copy(acces_ref, trim(buff_ref),               &
          acces_local, trim(buff_local))
  end if
  
end subroutine cps_ihm_copyDescConfig


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   Lecture d\'une description depuis un fichier de configuration   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_lireDesc(acces, fichier, categ, id, nb_champs,     &
     noms_champs, types_champs, vd_i_champs, vd_d_champs, vd_s_champs,&
     unites_champs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_lireDesc
!
!$Resume
!
!$Description
!  Lecture d'une description depuis un fichier de configuration
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_lireDesc(acces, fichier, categ, id, nb_champs,     &
!.         noms_champs, types_champs, vd_i_champs, vd_d_champs, vd_s_champs,&
!.         unites_champs)
!.    integer :: acces
!.    character(LEN=*) :: fichier
!.    character(LEN=*) :: categ, id
!.    integer :: nb_champs
!.    character(len=256), dimension(CPS_MAX_CHAMPS) :: noms_champs, vd_s_champs, unites_champs
!.    integer, dimension(CPS_MAX_CHAMPS) :: types_champs, vd_i_champs
!.    real(kind=8), dimension(CPS_MAX_CHAMPS) :: vd_d_champs
!
!$Arguments
!>E     acces          :<integer>                        accès MADONA ouvert sur un fichier de configuration  
!>E     fichier        :<LEN=*>                          nom du fichier à lire
!>S     categ          :<LEN=*>                          catégorie à laquelle est associé le fichier
!>S     id             :<LEN=*>                          id du fichier
!>S     nb_champs      :<integer>                        nombre d'attribut décrits
!>S     noms_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   noms des attributs
!>S     types_champs   :<integer,DIM=(CPS_MAX_CHAMPS)>   types des attributs
!>S     vd_i_champs    :<integer,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type entier
!>S     vd_d_champs    :<KIND=8,DIM=(CPS_MAX_CHAMPS)>    valeurs par défaut des attributs de type réel
!>S     vd_s_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type chaîne de caractères
!>S     unites_champs  :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   unites des attributs de type réel
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
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
#include "cps_ihm.h"
#include "acces_F.h"

  ! arguments
  integer, intent(in) :: acces
  character(LEN=*), intent(in) :: fichier
  character(LEN=*), intent(out) :: categ, id
  integer, intent(out) :: nb_champs
  character(len=256), dimension(CPS_MAX_CHAMPS), intent(out) :: noms_champs, vd_s_champs, unites_champs
  integer, dimension(CPS_MAX_CHAMPS), intent(out) :: types_champs, vd_i_champs
  real(kind=8), dimension(CPS_MAX_CHAMPS), intent(out) :: vd_d_champs

  ! variables locales
  integer :: ret, i, nb_fichiers, j
  character(len=256) :: buff_tmp

  buff_tmp = ""

  nb_fichiers = acc_get_dim(acces, "fichiers")
  ret = acc_select(acces, "fichiers", ACC_TABL)
  do i=1, nb_fichiers
     ret = acc_set_index(acces, i)
     ret = acc_select(acces, ACC_INDEX, ACC_STRUCT)
     ret = acc_gets(acces, "fichier", buff_tmp)
     if (trim(buff_tmp).eq.trim(fichier)) then
        ! le fichier est trouve
        ret = acc_gets(acces, "categorie", categ)
        ret = acc_gets(acces, "id", id)
        ret = acc_geti(acces, "nb_champs", nb_champs)
        ret = acc_select(acces, "infos_champs", ACC_TABL)
        do j=1, nb_champs
           ret = acc_set_index(acces, j)
           ret = acc_select(acces, ACC_INDEX, ACC_STRUCT)
           ret = acc_gets(acces, "nom", noms_champs(j))
           ret = acc_geti(acces, "type_donnee", types_champs(j))
           select case (types_champs(j))
           case (0)
              ! champ de type CPS_ENTIER
              ret = acc_geti(acces, "valeur_defaut", vd_i_champs(j))
           case (1)
              ! champ de type CPS_REEL
              ret = acc_gets(acces, "unite", unites_champs(j))
              ret = acc_getd(acces, "valeur_defaut", vd_d_champs(j),   &
                   unites_champs(j))
           case (2)
              ! champ de type CPS_STRING
              ret = acc_gets(acces, "valeur_defaut", vd_s_champs(j))
           end select
           ! Cas d'erreur sur la valeurs par défaut (par exemple si modifications
           ! manuelles)
           if (ret <0) &
                write(*,*) "Attention, erreur sur valeur_defaut, champs ", &
                trim(noms_champs(j))
           ret = acc_select_end(acces)
        end do
        ret = acc_select_end(acces)
        ret = acc_select_end(acces)
        exit
     end if
     ret = acc_select_end(acces)
  end do
  ret = acc_select_end(acces)

end subroutine cps_ihm_lireDesc


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! renvoie la liste des noms des structures d\'un fichier de donnees  !!
!! de classe 1                                                        !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_getEltsFichier(acces, nb_elts, noms_elts)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getEltsFichier
!
!$Resume
!
!$Description
!  Renvoie la liste des noms des structures d\'un fichier de donnees
!  de classe 1.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getEltsFichier(acces, nb_elts, noms_elts)
!.    integer :: acces
!.    integer :: nb_elts
!.    character(LEN=256), dimension(CPS_MAX_ELTS) :: noms_elts
!
!$Arguments
!>E     acces      :<integer>                      accès MADONA ouvert sur le fichier de données
!>S     nb_elts    :<integer>                      nombre d'éléments contenus dans le fichier
!>S     noms_elts  :<LEN=256,DIM=(CPS_MAX_ELTS)>   noms des éléments contenus dans le fichier
!
!$Common
!
!$Routines
!
!$Include
!#V
!- acces_F.h
!- cps_ihm.h
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
#include "acces_F.h"
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces
  integer, intent(out) :: nb_elts
  character(LEN=256), dimension(CPS_MAX_ELTS), intent(out) :: noms_elts

  ! variables locales
  integer :: ret, nature
  character(LEN=256) :: libelle

  nb_elts = 0
  noms_elts(:) = ""

  if (acces.LT.0) then
     ! acces MADONA non valide
     return
  end if
  
  ret = acc_scan_reset(acces)
  
  do
     ret = acc_scan(acces, libelle, nature)
     if (nature.eq.0) then
        exit
     endif
     if (nature.eq.ACC_STRUCT) then
           nb_elts = nb_elts+1
           noms_elts(nb_elts) = trim(libelle)
     end if
  end do
  
end subroutine cps_ihm_getEltsFichier





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! lecture de tous les attributs d'une structure MADONA.             !!
!! 'nom_elt' correspond qu nom de la structure.                      !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_lireElt(nom_elt, nom_fichier, rep_base, acces_config,&
     acces_fic, nb_att, noms_att, types_att, vals_i, vals_d, vals_s,   &
     unites, att_def)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_lireElt
!
!$Resume
!
!$Description
!  Lecture de tous les attributs d'une structure MADONA.
!  'nom_elt' correspond qu nom de la structure.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_lireElt(nom_elt, nom_fichier, rep_base, acces_config,&
!.         acces_fic, nb_att, noms_att, types_att, vals_i, vals_d, vals_s,   &
!.         unites, att_def)
!.    character(LEN=*) :: nom_elt, nom_fichier, rep_base
!.    integer :: acces_fic, acces_config
!.    integer :: nb_att
!.    character(LEN=256), dimension(CPS_MAX_CHAMPS) :: noms_att, vals_s, unites
!.    integer, dimension(CPS_MAX_CHAMPS) :: types_att, vals_i
!.    real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS) :: vals_d
!.    logical, dimension(CPS_MAX_CHAMPS) :: att_def
!
!$Arguments
!>E     nom_elt       :<LEN=*>                          nom de la structure MADONA à lire
!>E     nom_fichier   :<LEN=*>                          nom du fichier dans lequel lire
!>E     rep_base      :<LEN=*>                          répertoire de la base
!>E     acces_config  :<integer>                        accès MADONA ouvert sur le fichier de configuration
!>E     acces_fic     :<integer>                        ouvert MADONA ouvert sur le fichier de données
!>S     nb_att        :<integer>                        nombre d'attributs de la structure
!>S     noms_att      :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   noms des attributs
!>S     types_att     :<integer,DIM=(CPS_MAX_CHAMPS)>   types des attributs
!>S     vals_i        :<integer,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type entier
!>S     vals_d        :<PM_REEL,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type réel
!>S     vals_s        :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type chaîne de caractères
!>S     unites        :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   unités des attributs de type réel
!>S     att_def       :<logical,DIM=(CPS_MAX_CHAMPS)>   tableau de booléens indiquant si chaque attribut est défini ou non
!
!$Common
!
!$Routines
!- cpsi_lireDescription
!- cpsi_readStructure
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  character(LEN=*), intent(in) :: nom_elt, nom_fichier, rep_base
  integer, intent(in) :: acces_fic, acces_config
  integer, intent(out) :: nb_att
  character(LEN=256), dimension(CPS_MAX_CHAMPS), intent(out) :: noms_att, vals_s, unites
  integer, dimension(CPS_MAX_CHAMPS), intent(out) :: types_att,  vals_i
  real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS), intent(out) :: vals_d
  logical,  dimension(CPS_MAX_CHAMPS), intent(out) :: att_def

   ! variables locales
  integer :: ret, i, nb_fic
  type(cpsi_desc) :: desc
  character(LEN=256) :: buff_tmp

  nb_att = 0

  ! on verifie d'abord que l'element existe
  ret = acc_exist(acces_fic, trim(nom_elt))
  if (ret.ne.1) then
     ! l'element n'existe pas
     return
  end if
  
  ! recuperation de la description du fichier dans le
  ! fichier de configuration
  nb_fic = acc_get_dim(acces_config, "fichiers")
  ret = acc_select(acces_config, "fichiers", ACC_TABL)
  do i=1, nb_fic
     ret = acc_set_index(acces_config, i)
     ret = acc_select(acces_config, ACC_INDEX, ACC_STRUCT)
     ret = acc_gets(acces_config, "fichier", buff_tmp)
     if (trim(nom_fichier).eq.trim(buff_tmp)) then
        ! la description est trouvee
        call cpsi_lireDescription(acces_config, rep_base, desc)
        ret = acc_select_end(acces_config)
        exit
     end if
     ret = acc_select_end(acces_config)
  end do
  ret = acc_select_end(acces_config)
  
  ! nombre d'attributs
  nb_att = desc%nbChamps
  
  ret = acc_select(acces_fic, trim(nom_elt), ACC_STRUCT)
  call cpsi_readStructure(acces_fic, desc, noms_att, types_att,        &
       vals_i, vals_d, vals_s, unites, att_def)
  ret = acc_select_end(acces_fic)

  
  ! liberation memoire
  
end subroutine cps_ihm_lireElt









!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Ecriture dans la zone d\'acces MADONA du fichier de configuration  !!
!! d'une description                                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_ecrireDesc(acces, fichier, rep_base, categ, id,     &
     nb_champs, noms_champs, types_champs, vd_i_champs,                &
     vd_d_champs, vd_s_champs, unites_champs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_ecrireDesc
!
!$Resume
!
!$Description
!  Ecriture dans la zone d\'acces MADONA du fichier de configuration
!  d'une description.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_ecrireDesc(acces, fichier, rep_base, categ, id,     &
!.         nb_champs, noms_champs, types_champs, vd_i_champs,                &
!.         vd_d_champs, vd_s_champs, unites_champs)
!.    integer :: acces, nb_champs
!.    character(LEN=*) :: fichier, rep_base, categ, id
!.    character(LEN=256), dimension(CPS_MAX_CHAMPS) :: noms_champs, vd_s_champs, unites_champs
!.    integer, dimension(CPS_MAX_CHAMPS) :: types_champs, vd_i_champs
!.    real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS) :: vd_d_champs
!
!$Arguments
!>E     acces          :<integer>                        accès MADONA ouvert sur le fichier de configuration
!>E     fichier        :<LEN=*>                          nom du fichier associéà la description
!>E     rep_base       :<LEN=*>                          répertoire de la base
!>E     categ          :<LEN=*>                          catégorie du fichier
!>E     id             :<LEN=*>                          id du fichier
!>E     nb_champs      :<integer>                        nombre d'attributs
!>E     noms_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   noms des attributs
!>E     types_champs   :<integer,DIM=(CPS_MAX_CHAMPS)>   types des attributs
!>E     vd_i_champs    :<integer,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type entier
!>E     vd_d_champs    :<PM_REEL,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type réel
!>E     vd_s_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type chaines de caractères
!>E     unites_champs  :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   unités des attributs de type réel
!
!$Common
!
!$Routines
!- cpsi_ihm_ecrireDesc
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces, nb_champs
  character(LEN=*), intent(in) :: fichier, rep_base, categ, id
  character(LEN=256), dimension(CPS_MAX_CHAMPS), intent(in) :: noms_champs, vd_s_champs, unites_champs
  integer, dimension(CPS_MAX_CHAMPS), intent(in) :: types_champs, vd_i_champs
  real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS), intent(in) :: vd_d_champs

  ! variables locales
  integer :: ret, i, nb_fic, ind
  logical :: trouve
  character(LEN=256) :: buff_tmp, fic_tmp, categ_tmp, id_tmp
  
  trouve = .false.

  ret = acc_exist(acces, "fichiers")
  if (ret/=1) then
     ret = acc_create(acces, "fichiers", ACC_TABL, "")
  end if
  nb_fic = acc_get_dim(acces, "fichiers")
  ret = acc_select(acces, "fichiers", ACC_TABL)
  ! recherche des descriptions associees a la categorie 'categ'
  do i=1, nb_fic
     ret = acc_set_index(acces, i)
     ret = acc_select(acces, ACC_INDEX, ACC_STRUCT)
     ret = acc_gets(acces, "fichier", fic_tmp)
     ret = acc_gets(acces, "id", id_tmp)
     ret = acc_gets(acces, "categorie", categ_tmp)
     if (trim(fic_tmp).eq.trim(fichier)) then
        ! le fichier est trouve
        trouve = .true.
        categ_tmp = trim(categ)
        id_tmp = trim(id)
     end if
     if (trim(categ_tmp).eq.trim(categ)) then
        ! la description courante est associee
        ! a la categorie 'categ'
        call cpsi_ihm_ecrireDesc(acces, trim(categ_tmp), trim(id_tmp), &
             trim(fic_tmp), nb_champs, noms_champs, types_champs,      &
             vd_i_champs, vd_d_champs, vd_s_champs, unites_champs,     &
             .true.)
     end if
     ret = acc_select_end(acces)
  end do

  ret = acc_select_end(acces)

  if (.not.trouve) then
     ! la description n'existe pas
     ! on la cree
     nb_fic = nb_fic+1
     write(buff_tmp,*) nb_fic
     ret = acc_create(acces, "fichiers["//trim(buff_tmp)//"]", ACC_STRUCT, "")
     ind = nb_fic

     ! selection de la description
     write(buff_tmp,*) ind
     ret = acc_select(acces, "fichiers["//trim(buff_tmp)//"]", ACC_STRUCT)
     call cpsi_ihm_ecrireDesc(acces, trim(categ), trim(id), trim(fichier),&
          nb_champs, noms_champs, types_champs, vd_i_champs, vd_d_champs, &
          vd_s_champs, unites_champs, trouve)
  end if

!  ret = acc_select_end(acces)

end subroutine cps_ihm_ecrireDesc








! Routine interne
subroutine cpsi_ihm_ecrireDesc(acces, categ, id, fichier, nb_champs,   &
     noms_champs, types_champs, vd_i_champs, vd_d_champs, vd_s_champs, &
     unites_champs, existe_deja)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ihm_ecrireDesc
!
!$Resume
!
!$Description
!  Routine interne.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ihm_ecrireDesc(acces, categ, id, fichier, nb_champs,   &
!.         noms_champs, types_champs, vd_i_champs, vd_d_champs, vd_s_champs, &
!.         unites_champs, existe_deja)
!.    integer :: acces, nb_champs
!.    character(LEN=*) :: fichier, categ, id
!.    character(LEN=256), dimension(CPS_MAX_CHAMPS) :: noms_champs, vd_s_champs, unites_champs
!.    integer, dimension(CPS_MAX_CHAMPS) :: types_champs, vd_i_champs
!.    real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS) :: vd_d_champs
!.    logical :: existe_deja
!
!$Arguments
!>E     acces          :<integer>                        accès MADONA ouvert su le fichier de configuration
!>E     categ          :<LEN=*>                          catégorie du fichier associé à la description
!>E     id             :<LEN=*>                          id du fichier
!>E     fichier        :<LEN=*>                          nom du fichier
!>E     nb_champs      :<integer>                        nombre d'attributs
!>E     noms_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   noms des attributs
!>E     types_champs   :<integer,DIM=(CPS_MAX_CHAMPS)>   types des attributs
!>E     vd_i_champs    :<integer,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type entier
!>E     vd_d_champs    :<PM_REEL,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type réels
!>E     vd_s_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type chaînes de caractères
!>E     unites_champs  :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   unités des attributs de type réel
!>E/S   existe_deja    :<logical>                        booléen indiquant si la description existe déjà ou non dans le fichier de configuration
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces, nb_champs
  character(LEN=*), intent(in) :: fichier, categ, id
  character(LEN=256), dimension(CPS_MAX_CHAMPS), intent(in) :: noms_champs, vd_s_champs, unites_champs
  integer, dimension(CPS_MAX_CHAMPS), intent(in) :: types_champs, vd_i_champs
  real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS), intent(in) :: vd_d_champs
  logical :: existe_deja

  ! variables locales
  integer :: ret, i, nb, k
  character(LEN=256) :: buff_tmp
  
  ! mise a jour des champs
  ret = acc_puts(acces, "categorie", trim(categ))
  ret = acc_puts(acces, "id", trim(id))
  ret = acc_puts(acces, "fichier", trim(fichier))
  ret = acc_puti(acces, "nb_champs", nb_champs)
  ! infos_champs
  if (.not.existe_deja) then
     ! creation du tableau infos_champs
     ret = acc_create(acces, "infos_champs", ACC_TABL, "")
  end if
  nb = acc_get_dim(acces, "infos_champs")
  do i=1, nb_champs
     write(buff_tmp,*) i
     ret = acc_exist(acces, "infos_champs["//trim(buff_tmp)//"]")
     if (ret.ne.1) then
        ! le champ n'existe pas, on le cree
        ret = acc_create(acces, "infos_champs["//trim(buff_tmp)//"]", ACC_STRUCT, "")
     end if
     ! selection du champ
     ret = acc_select(acces, "infos_champs["//trim(buff_tmp)//"]", ACC_STRUCT)
     ! mise a jour du champ
     ret = acc_puts(acces, "nom", trim(noms_champs(i)))
     ret = acc_puti(acces, "type_donnee", types_champs(i))
     select case (types_champs(i))
     case (CPS_ENTIER)
        ret = acc_puti(acces, "valeur_defaut", vd_i_champs(i))
        ret = acc_exist(acces, "unite")
        if (ret.eq.1) then
           ! on supprime le parametre 'unite'
           ret = acc_delete(acces, "unite")
        end if
     case (CPS_REEL)
        ret = acc_delete(acces, "valeur_defaut")
        ret = acc_putd(acces, "valeur_defaut", vd_d_champs(i), trim(unites_champs(i)))
        ret = acc_puts(acces, "unite", trim(unites_champs(i)))
     case (CPS_STRING)
        ret = acc_puts(acces, "valeur_defaut", trim(vd_s_champs(i)))
        ret = acc_exist(acces, "unite")
        if (ret.eq.1) then
           ! on supprime le parametre 'unite'
           ret = acc_delete(acces, "unite")
        end if
     end select
     ret = acc_select_end(acces)
  end do

  ! on efface les descriptions qui auraient ete supprimees
  k = nb_champs+1
  write(buff_tmp,*) k
  do i=k, nb
     ret = acc_exist(acces, "infos_champs["//trim(buff_tmp)//"]")
     if (ret.eq.1) then
        ret = acc_delete(acces, "infos_champs["//trim(buff_tmp)//"]")
     end if
  end do


end subroutine cpsi_ihm_ecrireDesc









! Renvoie la liste des sous repertoires d\'un repertoire
subroutine cps_ihm_getListeSousRep(rep, liste_sous_rep, nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getListeSousRep
!
!$Resume
!
!$Description
!  Renvoie la liste des sous repertoires d\'un repertoire.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getListeSousRep(rep, liste_sous_rep, nb)
!.    character(LEN=*) :: rep
!.    integer :: nb
!.    character(LEN=256), dimension(CPS_MAX_REP) :: liste_sous_rep
!
!$Arguments
!>E     rep             :<LEN=*>                       répertoire principal
!>S     liste_sous_rep  :<LEN=256,DIM=(CPS_MAX_REP)>   liste des sous-répertoire de 'rep'
!>S     nb              :<integer>                     nombre de sous-répertoires de 'rep'
!
!$Common
!
!$Routines
!- random_number
!- cps_file_unit
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_util
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

  use cps_util, only : cps_file_unit

  implicit none
#include "cps_ihm.h"
  ! arguments
  character(LEN=*), intent(in) :: rep
  integer, intent(out) :: nb
  character(LEN=256), dimension(CPS_MAX_REP), intent(out) :: liste_sous_rep

  ! Constante
  integer, parameter :: DERNIER_DIGIT = 10

  ! variables locales
  real :: rand
  character(LEN=256) :: fic_tmp, buff
  integer :: ios, file_unit, ier
  integer :: ret, system

  ! initialisation : on ajoute les repertoires . et ..
  liste_sous_rep(1) = "."
  nb = 1
  if (trim(rep).ne."/") then
     liste_sous_rep(2) = ".."
     nb = 2
  end if
  
  ! on execute la commande 'ls -p | grep /' et on 
  ! met le resultat dans un fichier temporaire
  fic_tmp = ""
  call random_number(rand)
  write(fic_tmp,*) rand
  fic_tmp(1:7) = fic_tmp(4:DERNIER_DIGIT)
  fic_tmp = trim(fic_tmp)//".tmp"  

  ! execution de la commande 'ls'
  ret = system("/bin/ls -pt "//trim(rep)//" | grep / > "//trim(fic_tmp))
  
  ! lecture du fichier resultat
!  file_unit = 12
  call cps_file_unit(file_unit,ier)
  if(ier.lt.0)then
     return
  endif
  open(file_unit, access='SEQUENTIAL', file=trim(fic_tmp), status='OLD')
  do
     read(file_unit, *, iostat=ios) buff
     if (ios.lt.0) then
        exit
     else
        nb = nb+1
        liste_sous_rep(nb) =  trim(buff)
     end if
  end do
  close(file_unit)
  
  ! effecement du fichier temporaire
  ret = system("/bin/rm -f "//trim(fic_tmp))

end subroutine cps_ihm_getListeSousRep



! Renvoie le repertoire parent d\'un repertoire
function cps_ihm_getRepParent(rep) result(repParent)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getRepParent
!
!$Resume
!
!$Description
!  Renvoie le repertoire parent d\'un repertoire.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  repParent = cps_ihm_getRepParent(rep)
!.    character(LEN=*) :: rep
!.    character(LEN=256) :: repParent
!
!$Arguments
!>E     rep        :<LEN=*>     répertoire principal
!>S     repParent  :<LEN=256>   répertoire parent de 'rep'
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
  ! argument
  character(LEN=*), intent(in) :: rep

  ! variable de sortie
  character(LEN=256) :: repParent
  
  ! variables internes
  integer :: ind, lg, ind_tmp
  character(LEN=256) :: buff, buff_tmp

  repParent = ""
  buff = ""
  buff_tmp = ""
  ind = 0
  ind_tmp = 0
  
  if (trim(rep).eq."/") then
     repParent = "/"
  else
     buff = trim(rep)
     lg = len(trim(buff))
     if (buff(lg:lg).eq."/") then
        buff(lg:lg) = ""
     end if
     do
        ind_tmp = index(trim(buff), "/")
        if (ind_tmp.gt.0) then
           buff_tmp = buff(ind_tmp+1:)
           buff = trim(buff_tmp)
           ind = ind+ind_tmp
        else
           exit
        end if
     end do
     if (ind.gt.0) then
        repParent = rep(1:ind-1)
     end if
  end if

end function cps_ihm_getRepParent



! Renvoie la liste et le nombre des fichiers contenu dans un fichier de description
subroutine cps_ihm_getListeFic(acces, listeFic, nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getListeFic
!
!$Resume
!
!$Description
!  Renvoie la liste et le nombre des fichiers contenu dans un fichier de
!  description.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getListeFic(acces, listeFic, nb)
!.    integer :: acces
!.    character(LEN=256), dimension(CPS_MAX_FICHIERS) :: listeFic
!.    integer :: nb
!
!$Arguments
!>E     acces     :<integer>                          accès MADONA ouvert sur un fichier de configuration
!>S     listeFic  :<LEN=256,DIM=(CPS_MAX_FICHIERS)>   liste des fichiers de la base
!>S     nb        :<integer>                          nombre de fichiers de la base
!
!$Common
!
!$Routines
!
!$Include
!#V
!- acces_F.h
!- cps_ihm.h
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
#include "acces_F.h"
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces
  character(LEN=256), dimension(CPS_MAX_FICHIERS), intent(out) :: listeFic
  integer, intent(out) :: nb
  
  ! variables locales
  integer :: ret, i

  nb = 0

  if (acces.lt.0) then
     ! acces non valide
     return
  end if
  
  nb = acc_get_dim(acces, "fichiers")
  ret = acc_select(acces, "fichiers", ACC_TABL)
  do i=1, nb
     ret = acc_set_index(acces, i)
     ret = acc_select(acces, ACC_INDEX, ACC_STRUCT)
     ret = acc_gets(acces, "fichier", listeFic(i))
     ret = acc_select_end(acces)
  end do
  ret = acc_select_end(acces)

end subroutine cps_ihm_getListeFic





! Met a jour tous les fichiers de la base
! locale par rapport aux descriptions du fichier
! de configuration de la base
subroutine cps_ihm_mettreAJourBase(acces_config, rep_base)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_mettreAJourBase
!
!$Resume
!
!$Description
!  Met a jour tous les fichiers de la base locale par rapport aux
!  descriptions du fichier de configuration de la base.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_mettreAJourBase(acces_config, rep_base)
!.    integer :: acces_config
!.    character(LEN=*) :: rep_base
!
!$Arguments
!>E     acces_config  :<integer>   accès MADONA ouvert sur un fichier de configuration
!>E     rep_base      :<LEN=*>     répertoire de la base
!
!$Common
!
!$Routines
!- cpsi_ihm_mettreAJourFichier
!- cps_ihm_ecrireFichier
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces_config
  character(LEN=*), intent(in) :: rep_base

  ! variables locales
  integer :: ret, nb_fic, i, j, acces, ret_exist
  character(LEN=256) :: nom_fic
  integer :: nb_champs
  character(LEN=256), dimension(CPS_MAX_CHAMPS) :: noms_champs, vd_s_champs, unites_champs
  integer, dimension(CPS_MAX_CHAMPS) :: types_champs, vd_i_champs
  real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS) :: vd_d_champs

  nom_fic = ""
  nb_fic = 0
  
  ! verification de l'existence du fichier
  ret_exist = acc_exist(acces_config, "fichiers")

  if (ret_exist.eq.1) then
     nb_fic = acc_get_dim(acces_config, "fichiers")
     ! selectionner les descriptions
     ret = acc_select(acces_config, "fichiers", ACC_TABL)
  endif

  do i=1, nb_fic
     ! selection de la description i
     ret = acc_set_index(acces_config, i)
     ret = acc_select(acces_config, ACC_INDEX, ACC_STRUCT)
     ! nom du fichier
     ret = acc_gets(acces_config, "fichier", nom_fic)
     ! champs de la categorie
     ret = acc_geti(acces_config, "nb_champs", nb_champs)
     ! selection des champs
     ret = acc_select(acces_config, "infos_champs", ACC_TABL)
     do j=1, nb_champs
        ret = acc_set_index(acces_config, j)
        ! selection du champ j
        ret = acc_select(acces_config, ACC_INDEX, ACC_STRUCT)
        ret = acc_gets(acces_config, "nom", noms_champs(j))
        ret = acc_geti(acces_config, "type_donnee", types_champs(j))
        select case (types_champs(j))
        case (CPS_ENTIER)
           ret = acc_geti(acces_config, "valeur_defaut", vd_i_champs(j))
        case (CPS_REEL)
           ret = acc_gets(acces_config, "unite", unites_champs(j))
           ret = acc_getd(acces_config, "valeur_defaut", vd_d_champs(j),&
                trim(unites_champs(j)))
        case (CPS_STRING)
           ret = acc_gets(acces_config, "valeur_defaut", vd_s_champs(j))
        end select
        ! Cas d'erreur sur la valeurs par défaut (par exemple si modifications
        ! manuelles)
        if (ret <0) &
             write(*,*) "Attention, erreur sur valeur_defaut, champ ", &
             trim(noms_champs(j))
        ! fin de selection du champ j
        ret = acc_select_end(acces_config)
     end do
     ! fin de selection des champs
     ret = acc_select_end(acces_config)
     ! ouverture d\'un acces MADONA sur le fichier
     acces = acc_load(trim(rep_base)//"/"//trim(nom_fic))
     if (acces.lt.0) then
        ! probleme d\'ouverture
        return
     end if
     ! mise a jour du fichier
     call cpsi_ihm_mettreAJourFichier(acces, nb_champs, noms_champs,   &
          types_champs, vd_i_champs, vd_d_champs, vd_s_champs,         &
          unites_champs)
     ! ecrire le fichier
     call cps_ihm_ecrireFichier(acces, trim(rep_base)//"/"//trim(nom_fic))
     ! fermeture du fichier
     ret = acc_close(acces)
     ! fin de la selection de la description i
     ret = acc_select_end(acces_config)
  end do
  ret = acc_select_end(acces_config)
  
end subroutine cps_ihm_mettreAJourBase



! fonction interne
subroutine cpsi_ihm_mettreAJourFichier(acces, nb_champs, noms_champs,   &
     types_champs, vd_i_champs, vd_d_champs, vd_s_champs, unites_champs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ihm_mettreAJourFichier
!
!$Resume
!
!$Description
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ihm_mettreAJourFichier(acces, nb_champs, noms_champs,   &
!.         types_champs, vd_i_champs, vd_d_champs, vd_s_champs, unites_champs)
!.    integer :: acces, nb_champs
!.    character(LEN=256), dimension(CPS_MAX_CHAMPS) :: noms_champs, vd_s_champs, unites_champs
!.    integer, dimension(CPS_MAX_CHAMPS) :: types_champs, vd_i_champs
!.    real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS) :: vd_d_champs
!
!$Arguments
!>E     acces          :<integer>                        accès MADONA ouvert sur un fichier de données
!>E     nb_champs      :<integer>                        nombre d'attribut de la catégorie à laquelle est associé le fichier
!>E     noms_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   noms des attributs
!>E     types_champs   :<integer,DIM=(CPS_MAX_CHAMPS)>   types des attributs
!>E     vd_i_champs    :<integer,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type entier
!>E     vd_d_champs    :<PM_REEL,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type réel
!>E     vd_s_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type chaînes de caractères
!>E     unites_champs  :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   unités des attributs de type réel
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none

#include "cps_ihm.h"

  ! arguments
  integer, intent(in) :: acces, nb_champs
  character(LEN=256), dimension(CPS_MAX_CHAMPS), intent(in) :: noms_champs, vd_s_champs, unites_champs
  integer, dimension(CPS_MAX_CHAMPS), intent(in) :: types_champs, vd_i_champs
  real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS), intent(in) :: vd_d_champs

  ! variables locales
  integer :: ret, nature, i
  character(LEN=256) :: libelle
  logical :: present0
  integer :: nature_champ
  character(LEN=256) :: nom_champ

  ret = acc_scan_reset(acces)
  ! selection de chaque structure du fichier
  do
     ret = acc_scan(acces, libelle, nature)
     if (nature.eq.0) then
        ! fin du fichier
        exit
     end if
     !elseif (nature.eq.ACC_STRUCT) then
     ! selection de la structure
     ret= acc_select(acces, trim(libelle), ACC_STRUCT)
     ! pour chaque champ de la description, on regarde
     ! s'il existe dans la structure
     ! s'il n'existe pas, on le cree avec la valeur par defaut
     do i=1, nb_champs
        ret = acc_exist(acces, trim(noms_champs(i)))
        if (ret.ne.1) then
           ! le champ n'existe pas, on le cree avec la valeur par   &
           ! defaut
           select case (types_champs(i))
           case (CPS_ENTIER)
              ret = acc_puti(acces, trim(noms_champs(i)),           &
                   vd_i_champs(i))
           case (CPS_REEL)
              ret = acc_putd(acces, trim(noms_champs(i)),           &
                   vd_d_champs(i), CPS_UNITE_UNDEF)
           case (CPS_STRING)
              ret = acc_puts(acces, trim(noms_champs(i)),           &
                   vd_s_champs(i))
           end select
        end if
     end do
     ! on supprime les champs de la structure qui ne sont pas dans 
     ! la description
     do
        ret = acc_scan(acces, nom_champ, nature_champ)
        if (nature_champ.eq.0) then
           ! fin de la structure
           exit
        end if
        !else
        present0 = cpsi_stringInTab(trim(nom_champ),              &
             noms_champs(1:nb_champs))
        if (.not.present0) then
           ! le champ n'existe pas dans la desription
           ! on le supprime du fichier
           ret = acc_delete(acces, trim(nom_champ))
        end if
        !end if
     end do
     ! fin de la selection de la structure
     ret = acc_select_end(acces)
  !end if
  
  end do
  
end subroutine cpsi_ihm_mettreAJourFichier






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Recuperation des champs d'une categorie dans le fichier de         !!
!! configuration                                                      !!
!! retourne CPS_OK si trouve                                          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getChampsCateg(acces_config, categ, nb_champs,        &
     noms_champs, types_champs, vd_i_champs, vd_d_champs, vd_s_champs, &
     unites_champs) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getChampsCateg
!
!$Resume
!
!$Description
!  Recuperation des champs d'une categorie dans le fichier de configuration.
!  retourne CPS_OK si trouve.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_ihm_getChampsCateg(acces_config, categ, nb_champs,        &
!.         noms_champs, types_champs, vd_i_champs, vd_d_champs, vd_s_champs, &
!.         unites_champs)
!.    integer :: acces_config
!.    character(LEN=*) :: categ
!.    integer :: nb_champs
!.    character(LEN=256), dimension(CPS_MAX_CHAMPS) :: noms_champs, vd_s_champs, unites_champs
!.    integer, dimension(CPS_MAX_CHAMPS) :: types_champs, vd_i_champs
!.    real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS) :: vd_d_champs
!.    integer :: trouve
!
!$Arguments
!>E     acces_config   :<integer>                        accès ouvert sur un fichier de configuration
!>E     categ          :<LEN=*>                          catégorie à lire
!>S     nb_champs      :<integer>                        nombre d'attributs de la catégorie
!>S     noms_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   noms des attributs
!>S     types_champs   :<integer,DIM=(CPS_MAX_CHAMPS)>   types des attributs
!>S     vd_i_champs    :<integer,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type entier
!>S     vd_d_champs    :<PM_REEL,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type réel
!>S     vd_s_champs    :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   valeurs par défaut des attributs de type chaînes de caractères
!>S     unites_champs  :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   unités des attributs de type réel
!>S     trouve         :<integer>                        CPS_OK si la description est trouvée, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces_config
  character(LEN=*), intent(in) :: categ
  integer, intent(out) :: nb_champs
  character(LEN=256), dimension(CPS_MAX_CHAMPS), intent(out) :: noms_champs, vd_s_champs, unites_champs
  integer, dimension(CPS_MAX_CHAMPS), intent(out) :: types_champs, vd_i_champs
  real(KIND=PM_REEL),  dimension(CPS_MAX_CHAMPS), intent(out) :: vd_d_champs

  ! retour
  integer :: trouve

  ! variables locales
  integer :: ret, nb_fic, i, j
  character(LEN=256) :: buff_tmp

  nb_champs = 0
  trouve = CPS_ERR_DEF
  nb_fic = acc_get_dim(acces_config, "fichiers")
  ! selection du tableau fichiers
  ret = acc_select(acces_config, "fichiers", ACC_TABL)
  do i=1, nb_fic
     ret = acc_set_index(acces_config, i)
     ! selection de la description i
     ret = acc_select(acces_config, ACC_INDEX, ACC_STRUCT)
     ret = acc_gets(acces_config, "categorie", buff_tmp)
     if (trim(buff_tmp).eq.trim(categ)) then
        ! la categorie est trouvee
        ret = acc_geti(acces_config, "nb_champs", nb_champs)
        ! selection du tableau infos_champs
        ret = acc_select(acces_config, "infos_champs", ACC_TABL)
        do j=1, nb_champs
           ret = acc_set_index(acces_config, j)
           ! selection du champ j
           ret = acc_select(acces_config, ACC_INDEX, ACC_STRUCT)
           ret = acc_gets(acces_config, "nom", noms_champs(j))
           ret = acc_geti(acces_config, "type_donnee", types_champs(j))
           select case (types_champs(j))
           case (CPS_ENTIER)
              ret = acc_geti(acces_config, "valeur_defaut",            &
                   vd_i_champs(j))
           case (CPS_REEL)
              ret = acc_gets(acces_config, "unite", unites_champs(j))
              ret = acc_getd(acces_config, "valeur_defaut",            &
                   vd_d_champs(j), trim(unites_champs(j)))
           case (CPS_STRING)
              ret = acc_gets(acces_config, "valeur_defaut",            &
                   vd_s_champs(j))
           end select
           ! Cas d'erreur sur la valeurs par défaut (par exemple si modifications
           ! manuelles)
           if (ret <0) &
                write(*,*) "Attention, erreur sur valeur_defaut, champ ", &
                trim(noms_champs(j))
           ! fin de la selection du champ j
           ret = acc_select_end(acces_config)
        end do
        ! fin de la selection du tableau infos_champs
        ret = acc_select_end(acces_config)
        ret = acc_select_end(acces_config)
        trouve = CPS_OK
        exit
     end if
     ret = acc_select_end(acces_config)
  end do
  ! fin de la selection du tableau fichiers
  ret = acc_select_end(acces_config)
  
end function cps_ihm_getChampsCateg


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Suppression d\'une description correspondant a un fichier dans un !!
!! fichier de configuration                                          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_supprimerDescConfig(acces_config, fichier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_supprimerDescConfig
!
!$Resume
!
!$Description
!  Suppression d\'une description correspondant a un fichier dans un
!  fichier de configuration.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_supprimerDescConfig(acces_config, fichier)
!.    integer :: acces_config
!.    character(LEN=*) :: fichier
!
!$Arguments
!>E     acces_config  :<integer>   accès MADONA ouvert sur un fichier de configuration
!>E     fichier       :<LEN=*>     nom du fichier à supprimer
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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

  use cps_utilisateur
  implicit none

#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces_config
  character(LEN=*), intent(in) :: fichier
  
  ! variables locales
  integer :: ret, i, nb_fic, ind
  character(LEN=256) :: buff_tmp
  
  ind = 0
  nb_fic = acc_get_dim(acces_config, "fichiers")
  
  ! selection du tableau "fichiers"
  ret = acc_select(acces_config, "fichiers", ACC_TABL)
  do i=1, nb_fic
     ret = acc_set_index(acces_config, i)
     ! selection de la description i
     ret = acc_select(acces_config, ACC_INDEX, ACC_STRUCT)
     ret = acc_gets(acces_config, "fichier", buff_tmp)
     if (trim(buff_tmp).eq.trim(fichier)) then
        ! la description est trouvee
        ind = i
        ret = acc_select_end(acces_config)
        exit
     end if
     ! fin de la selection i
     ret = acc_select_end(acces_config)
  end do
  ! fin de la selection du tableau "fichiers"
  ret = acc_select_end(acces_config)
  
  if (ind.gt.0) then
     ! suppression de la description
     write(buff_tmp,*) ind
     ret = acc_delete(acces_config, "fichiers["//trim(buff_tmp)//"]")
  end if
  
end subroutine cps_ihm_supprimerDescConfig



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Ajout d'un element dans un fichier MADONA sous forme d'une nouvelle!!
!! structure                                                         !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_ajouterElt(nom_elt, acces_fichier, nb_champs,       &
     noms_champs, types_champs, vd_i_champs, vd_d_champs, vd_s_champs, &
     unites_champs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_ajouterElt
!
!$Resume
!
!$Description
!  Ajout d'un element dans un fichier MADONA sous forme d'une nouvelle
!  structure.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_ajouterElt(nom_elt, acces_fichier, nb_champs,       &
!.         noms_champs, types_champs, vd_i_champs, vd_d_champs, vd_s_champs, &
!.         unites_champs)
!.    integer :: acces_fichier,nb_champs
!.    character(LEN=*) :: nom_elt
!.    character(LEN=256), dimension(CPS_MAX_ELTS) :: noms_champs, vd_s_champs, unites_champs
!.    integer, dimension(CPS_MAX_ELTS) :: types_champs, vd_i_champs
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ELTS) :: vd_d_champs
!
!$Arguments
!>E     nom_elt        :<LEN=*>                        nom de la structure à rajouter
!>E     acces_fichier  :<integer>                      accès MADONA ouvert sur le fichier
!>E     nb_champs      :<integer>                      nombre d'attributs
!>E     noms_champs    :<LEN=256,DIM=(CPS_MAX_ELTS)>   noms des attributs
!>E     types_champs   :<integer,DIM=(CPS_MAX_ELTS)>   types des attributs
!>E     vd_i_champs    :<integer,DIM=(CPS_MAX_ELTS)>   valeurs par défaut des attributs de type entier
!>E     vd_d_champs    :<PM_REEL,DIM=(CPS_MAX_ELTS)>   valeurs par défaut des attributs de type réel
!>E     vd_s_champs    :<LEN=256,DIM=(CPS_MAX_ELTS)>   valeurs par défaut des attributs de type chaînes de caractères
!>E     unites_champs  :<LEN=256,DIM=(CPS_MAX_ELTS)>   unités des attributs de type réel
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces_fichier,nb_champs
  character(LEN=*), intent(in) :: nom_elt
  character(LEN=256), dimension(CPS_MAX_ELTS), intent(in) :: noms_champs, vd_s_champs, unites_champs
  integer, dimension(CPS_MAX_ELTS), intent(in) :: types_champs, vd_i_champs
  real(KIND=PM_REEL), dimension(CPS_MAX_ELTS), intent(in) :: vd_d_champs

  ! variables locales
  integer :: ret, i
  
  ! creation de la strucuture 'cle'
  ret = acc_create(acces_fichier, trim(nom_elt), ACC_STRUCT, "")
  ! selection de la structure
  ret = acc_select(acces_fichier, trim(nom_elt), ACC_STRUCT)
  ! creation de chaque champ
  do i=1, nb_champs
     select case (types_champs(i))
     case (CPS_ENTIER)
        ret = acc_puti(acces_fichier, trim(noms_champs(i)),            &
             vd_i_champs(i))
     case (CPS_REEL)
        ret = acc_putd(acces_fichier, trim(noms_champs(i)),            &
             vd_d_champs(i), trim(unites_champs(i)))
     case (CPS_STRING)
        ret = acc_puts(acces_fichier, trim(noms_champs(i)),            &
             trim(vd_s_champs(i)))
     end select
  end do
  ! fin de la selection de la structure
  ret = acc_select_end(acces_fichier)

end subroutine cps_ihm_ajouterElt



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Suppression des caracteres blancs en debut de chaine               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cpsi_ihm_trimGauche(str) result(res)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ihm_trimGauche
!
!$Resume
!
!$Description
!  Suppression des caracteres blancs en debut de chaine.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  res = cpsi_ihm_trimGauche(str)
!.    character(LEN=*) :: str
!.    character(LEN=256) :: res
!
!$Arguments
!>E     str  :<LEN=*>     chaîe de caractères en entrée
!>S     res  :<LEN=256>   résultat
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
  ! arguments
  character(LEN=*), intent(in) :: str

  ! resultat
  character(LEN=256) :: res
  
  ! variables locales
  integer :: lg, i
  
  lg = len(str)
  res = trim(str)
  do i=1,lg
     if (str(i:i).ne." ") then
        exit
     end if
  end do
  res = str(i:lg)
  
end function cpsi_ihm_trimGauche



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Ecriture d'un element dans l'acces MADONA associe a un fichier de  !!
!! donnees                                                            !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_ecrireElt(nom_elt, acces_fichier, nom_att_change,   &
     nb_att, noms_att, types_att, vals_i, vals_d, vals_s, unites)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_ecrireElt
!
!$Resume
!
!$Description
!  Ecriture d'un element dans l'acces MADONA associe a un fichier de
!  donnees.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_ecrireElt(nom_elt, acces_fichier, nom_att_change,   &
!.         nb_att, noms_att, types_att, vals_i, vals_d, vals_s, unites)
!.    character(LEN=*) :: nom_elt, nom_att_change
!.    integer :: acces_fichier, nb_att
!.    character(LEN=256), dimension(CPS_MAX_CHAMPS) :: noms_att, vals_s
!.    character(LEN=256), dimension(CPS_MAX_CHAMPS) :: unites
!.    integer, dimension(CPS_MAX_CHAMPS) :: types_att, vals_i
!.    real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS) :: vals_d
!
!$Arguments
!>E     nom_elt         :<LEN=*>                          nom de la structure MADONA à écrire
!>E     acces_fichier   :<integer>                        accès MADONA ouvert sur le fichier de données
!>E     nom_att_change  :<LEN=*>                          nom de l'attribut modifié par rapport à l'état précédent
!>E     nb_att          :<integer>                        nombre d'attributs
!>E     noms_att        :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   noms des attributs
!>E     types_att       :<integer,DIM=(CPS_MAX_CHAMPS)>   types des attributs
!>E     vals_i          :<integer,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type entier
!>E     vals_d          :<PM_REEL,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type réel
!>E     vals_s          :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type chaînes de caractères
!>E     unites          :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   unités des attributs de type réel
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  character(LEN=*), intent(in) :: nom_elt, nom_att_change
  integer, intent(in) :: acces_fichier, nb_att
  character(LEN=256), dimension(CPS_MAX_CHAMPS), intent(in) :: noms_att, vals_s
  character(LEN=256), dimension(CPS_MAX_CHAMPS), intent(in) :: unites
  integer, dimension(CPS_MAX_CHAMPS), intent(in) :: types_att, vals_i
  real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS), intent(in) :: vals_d

  ! variables locales
  integer :: ret, i
  character(LEN=256), dimension(CPS_MAX_CHAMPS) :: unites_fic
  real(KIND=PM_REEL) :: tmp

  ! on regarde si l\'element existe ou non
  !ret = acc_exist()

  ! initialisation
  unites_fic(1:nb_att) = CPS_UNITE_UNDEF

  ! selection de la structure 'nom_elt'
  ret = acc_select(acces_fichier, trim(nom_elt), ACC_STRUCT)

  ! recuperer les valeurs reelles ecrites dans le fichier avec les unites
  do i=1, nb_att
     if (trim(noms_att(i)).eq.trim(nom_att_change)) then
        ! on affecte la bonne unite a l'element qui change
        unites_fic(i) = trim(unites(i))
     end if
     if (types_att(i).eq.CPS_REEL) then
        ret = acc_getd(acces_fichier, trim(noms_att(i)), tmp, trim(unites(i)))
        if (ret.eq.0) then
           ! la valeur est definie
           unites_fic(i) = trim(unites(i))
        end if
     end if
  end do



  ! ecriture

  do i=1, nb_att
     ! on efface la donnee
     ! dans le cas où la donnee est réelle, si dans le fichier
     ! elle possede l'unité undef, on ne peut pas changer d'unité
     ! car elles ne sont pas compatibles
     ret = acc_delete(acces_fichier, trim(noms_att(i)))
     select case (types_att(i))
     case (CPS_ENTIER)
        ret = acc_puti(acces_fichier, trim(noms_att(i)), vals_i(i))
     case (CPS_REEL)
        ! on efface d'abord la donnee a cause de l'unite undef qui est incompatible
        !ret = acc_delete(acces_fichier, trim(noms_att(i)))
        ret = acc_putd(acces_fichier, trim(noms_att(i)), vals_d(i),    &
             trim(unites_fic(i)))
     case (CPS_STRING)
        ret = acc_puts(acces_fichier, trim(noms_att(i)), trim(vals_s(i)))
     end select
  end do
  ! fin de la selection de la structure 'nom_elt'
  ret = acc_select_end(acces_fichier)
       
end subroutine cps_ihm_ecrireElt



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!           suppression d'un element dans un fichier MADONA          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_supprimerElt(acces, nom_elt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_supprimerElt
!
!$Resume
!
!$Description
!  Suppression d'un element dans un fichier MADONA.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_supprimerElt(acces, nom_elt)
!.    integer :: acces
!.    character(LEN=*) :: nom_elt
!
!$Arguments
!>E     acces    :<integer>   accès MADONA ouvert sur un fichier de données
!>E     nom_elt  :<LEN=*>     nom de la structure MADONA à supprimer
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  integer, intent(in) :: acces
  character(LEN=*), intent(in) :: nom_elt

  ! variables locales
  integer :: ret

  ! on regarde si l'element existe
  ret = acc_exist(acces, trim(nom_elt))
  if (ret.eq.1) then
     ret = acc_delete(acces, trim(nom_elt))
  end if

end subroutine cps_ihm_supprimerElt


subroutine cps_ihm_majArgs(args, option, valeur)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_majArgs
!
!$Resume
!
!$Description
!  Mise a jour de la ligne d'arguments pour l'execution d'un utilitaire.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_majArgs(args, option, valeur)
!.    character(LEN=*) :: args
!.    character(LEN=*) :: option, valeur
!
!$Arguments
!>E/S   args    :<LEN=*>   ligne d'arguments
!>E     option  :<LEN=*>   option à mettre à jour (avec '-')
!>E     valeur  :<LEN=*>   nouvelle valeur de l'option
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
  ! arguments
  character(LEN=*), intent(inout) :: args
  character(LEN=*), intent(in) :: option, valeur

  ! variables locales
  integer :: ind, lg_args, lg_option, lg_valeur, ind_apres
  character(LEN=256) :: args_avant, args_apres
  integer :: i
  logical :: derniere_option

  ! determiner si l'option est deja presente
  ind = index(trim(args), trim(option))
  if (ind.gt.0) then
     ! l'option est deja presente : on met a jour la valeur
     lg_args = len(trim(args))
     lg_option = len(trim(option))
     lg_valeur = len(trim(valeur))
     
     args_avant = args(1:ind-1+lg_option)
     
     ind_apres = ind+lg_option
     derniere_option = .true.

     do i=ind+lg_option, lg_args 
        if (args(i:i).eq."-") then
           ind_apres = i
           derniere_option = .false.
           exit
        end if
     end do

     args_apres = args(ind_apres:lg_args)
     if (derniere_option) then
        args = trim(args_avant)//" "//trim(valeur)
     else
        args = trim(args_avant)//" "//trim(valeur)//" "//trim(args_apres)
     end if
  else
     ! l'option n'est pas presente : on la rajoute a la fin
     args = trim(args)//" "//trim(option)//" "//trim(valeur)
  end if
end subroutine cps_ihm_majArgs


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Suppression d'une option et de sa valeur dans une chaine d'arguments !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_supprimerOption(args, option)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_supprimerOption
!
!$Resume
!
!$Description
!  Suppression d'une option et de sa valeur dans une chaine d'arguments.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_supprimerOption(args, option)
!.    character(LEN=*) :: args
!.    character(LEN=*) :: option
!
!$Arguments
!>E/S   args    :<LEN=*>   ligne d'arguments
!>E     option  :<LEN=*>   option à supprimer (avec '-')
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
  ! arguments
  character(LEN=*), intent(inout) :: args
  character(LEN=*), intent(in) :: option
  
  ! variables locales
  integer :: ind, lg_args, ind_apres
  character(LEN=256) :: args_avant, args_apres
  integer :: i
  logical :: derniere_option

  ! determiner si l'option est deja presente
  ind = index(trim(args), trim(option))
  if (ind.gt.0) then
     ! l'option est deja presente : on la supprime avec 
     ! sa valeur
     lg_args = len(trim(args))

     args_avant = args(1:ind-1)
     
     ind_apres = ind-1
     derniere_option = .true.

     do i=ind+1, lg_args 
        if (args(i:i).eq."-") then
           ind_apres = i
           derniere_option = .false.
           exit
        end if
     end do

     args_apres = args(ind_apres:lg_args)
     if (derniere_option) then
        args = trim(args_avant)
     else
        args = trim(args_avant)//" "//trim(args_apres)
     end if
  end if
  
  
end subroutine cps_ihm_supprimerOption




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Fournie tout le contenu d'une structure MADONA selon une categorie !!
!! cle correspond au nom de la structure                              !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_getListAttCateg(cle, categorie, nb_att, noms_att,   &
     types_att, vals_i, vals_d, vals_s, unites, att_def)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getListAttCateg
!
!$Resume
!
!$Description
!  Fournie tous le contenu d'une structure MADONA selon une categorie.
!  'cle' correspond au nom de la structure.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getListAttCateg(cle, categorie, nb_att, noms_att,   &
!.         types_att, vals_i, vals_d, vals_s, unites, att_def)
!.    character(LEN=*) :: cle, categorie
!.    integer :: nb_att
!.    character(LEN=256), dimension(CPS_MAX_CHAMPS) :: noms_att, vals_s, unites
!.    integer, dimension(CPS_MAX_CHAMPS) :: types_att, vals_i
!.    real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS) :: vals_d
!.    logical, dimension(CPS_MAX_CHAMPS) :: att_def
!
!$Arguments
!>E     cle        :<LEN=*>                          nom de la structure MADONA
!>E     categorie  :<LEN=*>                          catégorie
!>S     nb_att     :<integer>                        nombre d'attributs
!>S     noms_att   :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   noms des attributs
!>S     types_att  :<integer,DIM=(CPS_MAX_CHAMPS)>   types des attributs
!>S     vals_i     :<integer,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type entier
!>S     vals_d     :<PM_REEL,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type réel
!>S     vals_s     :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   valeurs des attributs de type chaînes de caractères
!>S     unites     :<LEN=256,DIM=(CPS_MAX_CHAMPS)>   unités des attributs de type réel
!>S     att_def    :<logical,DIM=(CPS_MAX_CHAMPS)>   tableau de booléens indiquant si les attributs sont définis ou non
!
!$Common
!
!$Routines
!- cps_getListAttCateg
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
  use cps_utilisateur
  implicit none
#include "cps_ihm.h"
  ! arguments
  character(LEN=*), intent(in) :: cle, categorie
  integer, intent(out) :: nb_att
  character(LEN=256), dimension(CPS_MAX_CHAMPS), intent(out) :: noms_att, vals_s, unites
  integer, dimension(CPS_MAX_CHAMPS), intent(out) :: types_att, vals_i
  real(KIND=PM_REEL), dimension(CPS_MAX_CHAMPS), intent(out) :: vals_d
  logical, dimension(CPS_MAX_CHAMPS), intent(out) :: att_def
  
  ! variables locales
  
  ! appel au module cps_utilisateur
  call cps_getListAttCateg(trim(cle), trim(categorie), nb_att,         &
       noms_att, types_att, vals_i, vals_d, vals_s,                    &
       unites, att_def)

end subroutine cps_ihm_getListAttCateg


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Renvoie tous les noms des structures MADONA disponibles pour une   !!
!! categorie                                                          !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_getListEltsCateg(categorie, nb_elts, liste_elts)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getListEltsCateg
!
!$Resume
!
!$Description
!  Renvoie tous les noms des structures MADONA disponibles pour une
!  categorie
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getListEltsCateg(categorie, nb_elts, liste_elts)
!.    character(LEN=*) :: categorie
!.    integer :: nb_elts
!.    character(LEN=256), dimension(CPS_MAX_ELTS) :: liste_elts
!
!$Arguments
!>E     categorie   :<LEN=*>                        catégorie pour laquelle on souhaite obtenir les éléments
!>S     nb_elts     :<integer>                      nombre d'éléments de la catégorie
!>S     liste_elts  :<LEN=256,DIM=(CPS_MAX_ELTS)>   liste des éléments de la catégorie
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_acces
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
  use cps_acces
  implicit none
#include "cps_ihm.h"
  ! arguments
  character(LEN=*), intent(in) :: categorie
  integer, intent(out) :: nb_elts
  character(LEN=256), dimension(CPS_MAX_ELTS), intent(out) :: liste_elts
  
  ! variables locales
  character(LEN=256), dimension(:), pointer :: liste_elts_tmp => NULL()
  integer :: trouve
  
  ! appel au module cps_utilisateur
  trouve = cps_getListEltsCateg(trim(categorie), liste_elts_tmp)
  nb_elts = size(liste_elts_tmp)
  if (nb_elts.gt.CPS_MAX_ELTS) then
     nb_elts = CPS_MAX_ELTS
  end if
  liste_elts(1:nb_elts) = liste_elts_tmp(1:nb_elts)
  
  ! liberation memoire
  if (associated(liste_elts_tmp)) then
     deallocate(liste_elts_tmp)
  end if
  
end subroutine cps_ihm_getListEltsCateg


subroutine cps_ihm_copyContenu(acces_config, rep_base, nom_fic)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_copyContenu
!
!$Resume
!  FA-ID 583 : copie les fichiers de ressources associes aux elements
!  d'une table.
!
!$Description
!  Cette routine copie les fichiers de ressources associes aux elements
!  d'une table possedant des attributs contenant "fichier" et/ou 
!  "repertoire" de la base de reference dans la base locale. Cette routine
!  est appelée lors de la copie de fichiers de reference dans la base 
!  locale.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_copyContenu(acces_config, rep_base, nom_fic)
!.    integer :: acces_config
!.    character(LEN=*) :: rep_base, nom_fic
!
!$Arguments
!>E     acces_config  :<integer>   acces MADONA ouvert sur le fichier de configuration de la base locale
!>E     rep_base      :<LEN=*>     repertoire de la base locale
!>E     nom_fic       :<LEN=*>     fichier à traiter
!
!$Common
!
!$Routines
!- cpsi_ihm_copyContenu
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- cps_acces
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
  use cps_acces
  implicit none
  ! arguments
  integer, intent(in) :: acces_config
  character(LEN=*), intent(in) :: rep_base, nom_fic

  ! variables locales
  character(LEN=CPS_MAXLG) :: nom_fic_tmp
  character(LEN=CPS_MAXLG) :: categ
  integer :: i, ret, nb_fic

  ! nombre de fichiers locaux
  nb_fic = acc_get_dim(acces_config, "fichiers")

  ! selection des fichiers
  ret = acc_select(acces_config, "fichiers", ACC_TABL)
  
  ! parcourir des structures
  boucle_fic : do i=1,nb_fic
     ! initialisation 
     nom_fic_tmp = ""
     ! selection de la structure courante
     ret = acc_set_index(acces_config, i)
     ret = acc_select(acces_config, ACC_INDEX, ACC_STRUCT)
     ! on recupere le nom du fichier
     ret = acc_gets(acces_config, "fichier", nom_fic_tmp)
     ! on recupere la catégorie
     ret = acc_gets(acces_config, "categorie", categ)
     
     
     ! S'il ne s'agit pas du fichier d'entrée, on passe au suivant
     if (trim(nom_fic_tmp) /= trim(nom_fic)) then
       ! fin de la selection de la structure
        ret = acc_select_end(acces_config)
        cycle boucle_fic
     endif

     ! copie des fichiers
     call cpsi_ihm_copyContenu(nom_fic, rep_base, categ)
     if ( MSP_ERREUR ) then
        ! Ajout d'un message a la pile
        ! Volontairement pas de return, on autorise la copie des autres fichiers
        call MSP_signaler_message(message="erreur lors de la copie de : " &
           //trim(nom_fic), &
           type = MSP_ENUM_ERREUR, &
           routine="cpsi_ihm_copyContenu")
     endif

     ! fin de la selection de la structure
     ret = acc_select_end(acces_config)

  end do boucle_fic
  
  ! fin de la selection
  ret = acc_select_end(acces_config)

end subroutine cps_ihm_copyContenu



subroutine cpsi_ihm_copyContenu(nom_fic, rep_base, categ)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ihm_copyContenu
!
!$Resume
!   Fonction de recopie de l'ensemble des fichiers pointés par un fichier 
!   de description.
!
!$Description
!   Fonction de recopie de l'ensemble des fichiers (ex : Atmosphere/Mars/data_emcd.xxx)
!   pointés par un fichier de description (ex : Atmosphere/atmosphere).
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ihm_copyContenu(nom_fic, rep_base, att_fic, att_rep)
!.    character(LEN=*) :: nom_fic, rep_base, att_fic, att_rep
!
!$Arguments
!>E     nom_fic   :<LEN=*>   Nom du fichier
!>E     rep_base  :<LEN=*>   Nom du repertoire de la base locale dans lequel
!                          est le fichier de description
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
!- cps_acces
!- cps_constantes
!- MSP_gestion_erreur
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
use cps_acces
use cps_constantes, only : CPS_MAXLG
use MSP_gestion_erreur

  implicit none
  ! arguments
  character(LEN=*), intent(in) :: nom_fic
  character(LEN=*), intent(in) :: rep_base 
  character(LEN=*), intent(in) :: categ
  
  ! Variables locales
  character(LEN=CPS_MAXLG) :: rep_base_src = ""
  logical :: fic_existe = .false.
  integer :: ind1, ind2
  integer :: system, ret = 0

  ! Verification que le fichier contient bien un répertoire non vide
  ind1 = index(nom_fic, "/", back=.false.)
  ind2 = index(nom_fic, "/", back=.true.)
  
  ! Traitement du cas où il s'agit juste d'un fichier 
  ! <=> "fichier"
  !      OU "/fichier"
  !      OU "./fichier"
  if ( ind1 <= 1 .or. (ind1 == 2 .and. nom_fic(1:1) == ".") ) then
     ! Aucun traitement
     return
  endif
  
  ! Autre cas possibles
  !     "repertoire/fichier"
  !     "./repertoire/fichier"
  !     "/repertoire/fichier"
  if ( ind2  == 2 .and. nom_fic(1:1) == ".") then
     rep_base_src = nom_fic(3:ind1-1)
  elseif ( ind2 == 1 ) then
     rep_base_src = nom_fic(2:ind1-1)
  else
     rep_base_src = nom_fic(1:ind1-1)
  endif

  ! Vérification que la catégorie ne soit ni données du bulletin A,
  ! ni activité solaire
  if ( categ == CPS_CATEG_BULLA_SURE ) then
     ! Message d'erreur
     call MSP_signaler_message(message= &
           "Attention, les données réelles du bulletin A ne doivent pas être stockées " &
           // "dans une base locale ! ", &
           type = MSP_ENUM_WARNING, &
           routine =  "cpsi_ihm_copyContenu")
  endif

  if ( categ == CPS_CATEG_BULLA_PRED ) then
     ! Message d'erreur
     call MSP_signaler_message(message= &
           "Attention, les données prédites du bulletin A ne doivent pas être stockées " &
           // "dans une base locale ! ", &
           type = MSP_ENUM_WARNING, &
           routine =  "cpsi_ihm_copyContenu")
  endif

  if ( categ == CPS_CATEG_ACSOL2_SURE ) then
     ! Message d'erreur
     call MSP_signaler_message(message= &
          "Attention, les données d'activité solaire réelles ne doivent pas " &
           // "être stockées dans une base locale ! ", &
           type = MSP_ENUM_WARNING, &
           routine =  "cpsi_ihm_copyContenu")
  endif

  if ( categ == CPS_CATEG_ACSOL2_PRED ) then
     ! Message d'erreur
     call MSP_signaler_message(message= &
          "Attention, les données d'activité solaire prédites ne doivent pas " &
           // "être stockées dans une base locale ! ", &
           type = MSP_ENUM_WARNING, &
           routine =  "cpsi_ihm_copyContenu")
  endif
  
  ! Test de présence du répertoire ou copier les données
  inquire(FILE=trim(rep_base), EXIST=fic_existe)
  if ( .not. fic_existe) then
     ! Message d'erreur
     call MSP_signaler_message(message="echec d'acces au repertoire de destination "// &
           trim(rep_base), &
           type = MSP_ENUM_ERREUR, &
           routine =  "cpsi_ihm_copyContenu")
     return
  endif

  ! Appel a la commande tar depuis le repertoire de la base
  ! On construit le chemin de l'archive (le test sert à définir si le chemin est local ou absolu.
  ! Puis archivage du fichier ou repertoire dans une archive dans le repertoire de destination
  ret = system("(cpsi_chemin=${PWD} && cd "// trim(rep_base_ref) //" ; " // &
    " cpsi_rep_dest="// trim(rep_base) // ";" // &
    " test ! -r "// trim(rep_base) // " && cpsi_rep_dest=$cpsi_chemin/"// trim(rep_base) // ";" // &
    " tar cf $cpsi_rep_dest/cpsi_ihm_copyRepEtFichier.tar " // trim(rep_base_src) // " )")
  if (ret /= 0 ) then
     call MSP_signaler_message(message="echec lors de l'archivage temporaire dans le repertoire" // &
           trim(rep_base), &
           type = MSP_ENUM_ERREUR, &
           routine =  "cpsi_ihm_copyContenu") 
     return
  endif

  ! Decompression de l'archive
  ! et suppression de l'archive temporaire créee
  ret = system("(cd "// trim(rep_base) //" && " // &
    " tar xf ./cpsi_ihm_copyRepEtFichier.tar && " // &
    " rm -f ./cpsi_ihm_copyRepEtFichier.tar )") 
  if ( ret /= 0 ) then
     call MSP_signaler_message(message="echec lors du désarchivage temporaire " // &
           trim(rep_base) // "/cpsi_ihm_copyRepEtFichier.tar", &
           type = MSP_ENUM_ERREUR, &
           routine =  "cpsi_ihm_copyContenu")
             
  endif
  
end subroutine cpsi_ihm_copyContenu



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Donne la liste des corps pour le calcul du mouvements des pôles    !!
!! UAI selon une certaine théorie                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_getCorpsPolesUAI(theorieUAI, liste_corps, nb_corps)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getCorpsPolesUAI
!
!$Resume
!
!$Description
!  Renvoie tous les codes des corps disponibles pour le calcul du 
!  mouvements des pôles UAI selon une certaine théorie.
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getCorpsPolesUAI(theorieUAI, liste_corps, nb_corps)
!.    character(LEN=*) :: theorieUAI
!.    integer, dimension(10000) :: liste_corps
!.    integer :: nb_corps
!
!$Arguments
!>E     theorieUAI   :<LEN=*>                 nom de la théorie UAI ('UAI2000' ou 'UAI91')
!>S     liste_corps  :<integer,DIM=(10000)>   liste des codes des corps
!>S     nb_corps     :<integer>               nombre de corps
!
!$Common
!
!$Routines
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_utilisateur
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

  use cps_utilisateur
  implicit none

#include "cps_ihm.h"

  ! Argument
  character(LEN=*), intent(in) :: theorieUAI
  integer, dimension(10000), intent(out) :: liste_corps
  integer, intent(out) :: nb_corps

  ! Variables locales
  integer :: trouve
  character(LEN=256), dimension(:), pointer :: liste => NULL()
  integer :: nb_liste, ii, ind, kk
  character(LEN=256) :: theo
  
  kk=1
  
  ! Appel au modèle cps_utilisateur
  trouve=cps_getListEltsCateg("mvts_poles_UAI", liste)
  nb_liste=size(liste)
  
  do ii= 1,nb_liste
     trouve=cps_requete("mvts_poles_UAI",trim(liste(ii)), "*", "theories_disponibles",theo)
     ind=index(trim(theo), trim(theorieUAI))
     if(ind.gt.0) then
        trouve=cps_requete("mvts_poles_UAI", trim(liste(ii)), "*", "code", liste_corps(kk))
        kk=kk+1
     endif
  enddo
  
  nb_corps = kk-1
  
  if(associated(liste)) deallocate(liste)

end subroutine cps_ihm_getCorpsPolesUAI

! Conversion d'un code repere a trois chiffres en un code repere MSPRO
subroutine eph_codageRepMSPRO(code, tbv_rep_out, bv_pla_out, date_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_codageRepMSPRO
!
!$Resume
!
!$Description
! V2.0
! Routine de conversion d'un code de repere a trois chiffres en elements
! decrivant un repere dans la MSPRO.
! Le code d'un repere a trois chiffre provient de l'AMLIB.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_codageRepMSPRO(code, tbv_rep_out, bv_pla_out, date_out)
!.    integer :: code
!.    integer :: tbv_rep_out, bv_pla_out, date_out
!
!$Arguments
!>E     code         :<integer>   Code repere COMPAS
!>S     tbv_rep_out  :<integer>   code MSPRO type de repere
!>S     bv_pla_out   :<integer>   code MSPRO planete du repere
!>S     date_out     :<integer>   code MSPRO date du repere
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mspro
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


  use mspro
  implicit none

  ! arguments
  integer, intent(in) :: code
  integer, intent(out) :: tbv_rep_out, bv_pla_out, date_out
  
  ! variables locales
  integer :: rep, pla, date
  
  ! Constantes
  integer, parameter :: cpsi_10 = 10
  integer, parameter :: cpsi_100 = 100
  integer, parameter :: cpsi_99 = 99

  ! Initialisations
  rep = code/cpsi_100
  date = (code-rep*cpsi_100)/cpsi_10
  pla = code-rep*cpsi_100-date*cpsi_10

  tbv_rep_out = 0
  bv_pla_out = 0
  date_out = 0

  ! date
  select case (date)
     case (1)
        date_out = pm_1janvier1950_00h00
     case (2)
        date_out = pm_1janvier2000_12h00
     case (3)
        date_out = pm_autre_date
     case (4)
        date_out = pm_autre_date
     case default
! call msp_signaler_message
        return
  end select

  ! planete
  if (pla < 1 .or. pla > 9) then
! call msp_signaler_message
     return
  endif

  bv_pla_out = pla * cpsi_100 + cpsi_99

  ! type de repere
  select case (rep)
     case(1)  ! Gamma/Equatorial Vrai 
        tbv_rep_out=pm_equa_vrai
        bv_pla_out = pm_pla_terre
     case(2)  ! Gamma/Equatorial Moyen
        tbv_rep_out=pm_equa_moy
     case(3)  ! Ecliptique 2000 uniquement
        date_out=pm_1janvier2000_12h00
        tbv_rep_out=pm_ecli_moy
        bv_pla_out = pm_pla_terre
     case(4) ! Planetocentrique vrai
        tbv_rep_out=pm_planeto_vrai
        bv_pla_out = pm_pla_terre
     case(5) ! Planetocentrique
        tbv_rep_out=pm_planeto_ref
     case(6)  ! Veis de la date (pas de mauvaise definition)
        tbv_rep_out=pm_veis
        bv_pla_out = pm_pla_terre
     case(7)  ! Planetocentrique inertiel de la date
        tbv_rep_out=pm_planeto_ref_iner
     case(8)  ! PQ / Equatorial UAI de la date
        tbv_rep_out=pm_equa_uai
     case default
! call msp_signaler_message
  end select
  
end subroutine eph_codageRepMSPRO


! Conversion d'un code repere MSPRO en code a trois chiffre
subroutine eph_decodageRepMSPRO(tbv_rep_in, bv_pla_in, date_in, code)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_decodageRepMSPRO
!
!$Resume
!
!$Description
! V2.0
! Routine de conversion des elements identifiant un repere dans la MSPRO en un code
! a trois chiffres identifiant le meme repere. Le codage sur trois chiffre pour identifier
! un repere provient de l'AMLIB.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_decodageRepMSPRO(tbv_rep_in, bv_pla_in, date_in, code)
!.    integer :: tbv_rep_in, bv_pla_in, date_in
!.    integer :: code
!
!$Arguments
!>E     tbv_rep_in  :<integer>   code MSPRO type de repere
!>E     bv_pla_in   :<integer>   code MSPRO planete du repere
!>E     date_in     :<integer>   code MSPRO date du repere
!>S     code        :<integer>   Code repere COMPAS
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mspro
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
  use mspro
  implicit none

  ! arguments
  integer, intent(in) :: tbv_rep_in, bv_pla_in, date_in
  integer, intent(out) :: code
  
  ! variables internes
  integer :: rep, date, pla

  ! Constantes 
  integer, parameter :: cpsi_100 = 100
  integer, parameter :: cpsi_10 = 10

  ! Initialisations
  code = 0

  ! planete : pla
  select case (bv_pla_in)
     case (pm_pla_mercure)
        pla = 1
     case (pm_pla_venus)
        pla = 2
     case (pm_pla_terre)
        pla = 3
     case (pm_pla_mars)
        pla = 4
     case (pm_pla_jupiter)
        pla = 5
     case (pm_pla_saturne)
        pla = 6
     case (pm_pla_uranus)
        pla = 7
     case (pm_pla_neptune)
        pla = 8
     case (pm_pla_pluton)
        pla = 9
     case default
        pla = 0
  end select

  ! date
  select case (date_in)
     case (pm_1janvier1950_00h00)
        date = 1
     case (pm_1janvier2000_12h00)
        date = 2
     case (pm_autre_date)
        date = 3
     case default
        date = 0
  end select
  
  ! type de repere : rep
  select case (tbv_rep_in)
     case (pm_equa_vrai)
        rep = 1
        ! planete = Terre
        pla = 3
     case (pm_equa_moy)
        rep = 2
     case (pm_planeto_vrai)
        rep = 4
        ! planete = Terre
        pla = 3
     case (pm_planeto_ref)
        rep = 5
     case (pm_planeto_ref_iner)
        rep = 7
     case (pm_topo)
        ! Pas de topocentrique !
        rep = 0
     case (pm_equa_uai)
        rep = 8
     case (pm_veis)
        rep = 6
        ! planete = Terre
        pla = 3
     case (pm_ecli_moy)
        rep = 3
        ! planete = Terre
        pla = 3
     case default
        rep = 0
  end select

  ! construction du code
  code = rep*cpsi_100 + date *cpsi_10 + pla

end subroutine eph_decodageRepMSPRO



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM CREATEPHEM             !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine eph_lentfic_pla77(lfn,lentete,numplac,numframe,tframe, &
          nbplanet,numpla,dureej,ndeg,tdeb,tfin, echtout, nomframeout)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_lentfic_pla77
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_lentfic_pla77(lfn,lentete,numplac,numframe,tframe, &
!.              nbplanet,numpla,dureej,ndeg,tdeb,tfin, echtout, nomframeout)
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
!.    character(LEN=*) :: echtout
!.    character(LEN=*) :: nomframeout
!
!$Arguments
!>E     lfn          :<integer>                    
!>S     lentete      :<integer>                    
!>S     numplac      :<integer>                    
!>S     numframe     :<integer>                    
!>S     tframe       :<PM_REEL>                    
!>S     nbplanet     :<integer>                    
!>S     numpla       :<integer,DIM=(NBPLANETMX)>   
!>S     dureej       :<PM_REEL>                    
!>S     ndeg         :<integer>                    
!>S     tdeb         :<PM_REEL>                    
!>S     tfin         :<PM_REEL>                    
!>S     echtout      :<LEN=*>                      
!>S     nomframeout  :<LEN=*>                      
!
!$Common
!
!$Routines
!- eph_lentfic_pla
!
!$Include
!
!$Module
!#V
!- eph_tcheb
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

        use eph_tcheb
        
  implicit none

  integer,parameter :: NBPLANETMX=15
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
      character(LEN=*)     , intent(out) :: echtout
      character(LEN=*)     , intent(out) :: nomframeout


      call  eph_lentfic_pla(lfn,lentete,numplac,numframe,tframe, &
          nbplanet,numpla,dureej,ndeg,tdeb,tfin, echtout, nomframeout)

    end subroutine eph_lentfic_pla77



subroutine cps_ihm_initConfigLocal(acces_config)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_initConfigLocal
!
!$Resume
!  FA-ID 582 : initialisation du fichier de configuration de la
!  base locale.
!
!$Description
!  Cette routine intialise le fichier de configuration de la base locale
!  lors de sa creation en ajoutant :
!  - le type de la base : base locale
!  - le tableau des fichiers, initialement vide
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_initConfigLocal(acces_config)
!.    integer :: acces_config
!
!$Arguments
!>E     acces_config  :<integer>   acces MADONA ouvert sur le fichier de configuration de la base locale
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- cps_acces
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
  use cps_acces
  implicit none
  ! arguments
  integer, intent(in) :: acces_config

  ! variables locales
  integer :: ret
  
  ! type de la base : base locale
  ret = acc_puts(acces_config, "type_base", "base_locale")
  ! liste des fichiers
  ret = acc_create(acces_config, "fichiers", ACC_TABL, "")

end subroutine cps_ihm_initConfigLocal



function cps_ihm_getFicElt(acces_fichier, nom_elt, rep_base) result(fichier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getFicElt
!
!$Resume
!  Retourne le chemin complet d'un eventuel fichier de ressources associé à l'élément.
!
!$Description
!  Cette fonction retourne le chemin complet d'un eventuel fichier de ressources
!  associé à l'élément indiqué. C'est le cas lorsque l'élément possède 
!  un attribut contenant la chaîne de caractères "fichier". S'il existe de plus un attribut
!  contenant la chaîne de caractères "repertoire", alors cet attribut est considéré
!  comme le répertoire dans la base contenant le fichier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  fichier = cps_ihm_getFicElt(acces_fichier, nom_elt, rep_base)
!.    integer :: acces_fichier
!.    character(LEN=*) :: nom_elt, rep_base
!.    character(LEN=256) :: fichier
!
!$Arguments
!>E     acces_fichier  :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_elt        :<LEN=*>     nom de la structure contenue dans le fichier de données
!>E     rep_base       :<LEN=*>     repertoire de la base
!>S     fichier        :<LEN=256>   chemin complet du fichier, s'il existe. S'il n'existe pas,
!                                   retourne ""
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- cps_acces
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
  use cps_acces
  implicit none
  ! arguments
  integer, intent(in) :: acces_fichier
  character(LEN=*), intent(in) :: nom_elt, rep_base

  ! resultat
  character(LEN=256) :: fichier

  ! variables interne
  character(LEN=256) :: libelle, fic, rep
  integer :: ret, ind, nature
  logical :: fic_exist

  ! initialisation
  fichier = ""
  fic = ""
  rep = ""
  
  ! selection de l'éléméent
  ret = acc_select(acces_fichier, trim(nom_elt), ACC_STRUCT)
  ! parcours des attributs pour determiner s'il existe un attribut contenant :
  ! - "fichier"
  ! - "repertoire"
  ret = acc_scan_reset(acces_fichier)
  nature = ACC_PARAM
  do while (nature/=0)
     ret = acc_scan(acces_fichier, libelle, nature)
     if (nature/=0) then
        ind = index(trim(libelle), "fichier")
        if (ind>0) then
           ! correspond à un fichier
           ret = acc_gets(acces_fichier, trim(libelle), fic)
        end if
        ind = index(trim(libelle), "repertoire")
        if (ind>0) then
           ! correspond à un repertoire
           ret = acc_gets(acces_fichier, trim(libelle), rep)
        end if
     end if
  end do
  if (trim(fic)/="") then
     ! il existe une ressource de type fichier associée
     ! on recupere la valeur du fichier
     if (trim(rep)/="") then
        ! il y a un repertoire indique
        fichier = trim(rep_base)//"/"//trim(rep)//"/"//trim(fic)
     else
        ! pas de repertoire specifie
        fichier = trim(rep_base)//"/"//trim(fic)
     end if
     ! on teste si le fichier existe physiquement
     inquire(file=trim(fichier), exist=fic_exist)
     if (.not.fic_exist) then
        ! le fichier n'existe pas physiquement
        fichier = ""
     end if
  end if
  ! fin de la selection de l'élément
  ret = acc_select_end(acces_fichier)

end function cps_ihm_getFicElt


function cps_ihm_contientFic(rep_base, nom_fic) result(fic_present)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_contientFic
!
!$Resume
!  Determine s'il existe un fichier associé aux éléments du fichier indiqué
!
!$Description
!  Cette fonction détermine s'il existe un fichier de ressources associé aux
!  éléments contenus dans le fichier. C'est le cas lorsque les éléments possède 
!  un attribut contenant la chaîne de caractères "fichier". S'il existe de plus un attribut
!  contenant la chaîne de caractères "repertoire", alors cet attribut est considéré
!  comme le répertoire dans la base contenant le fichier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  fic_present = cps_ihm_contientFic(rep_base, nom_fic)
!.    character(LEN=*) :: rep_base, nom_fic
!.    logical :: fic_present
!
!$Arguments
!>E     rep_base     :<LEN=*>     repertoire de la base
!>E     nom_fic      :<LEN=*>     nom du fichier concerné
!>S     fic_present  :<logical>   .true. s'il existe un fichier de ressources associé
!
!$Common
!
!$Routines
!- cps_ihm_getEltsFichier
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_acces
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
  use cps_acces
  implicit none
  ! arguments
  character(LEN=*), intent(in) :: rep_base, nom_fic
  
  ! resultat
  logical :: fic_present
  
#include "cps_ihm.h"
  ! variables internes
  integer :: acces, nb_elts, i, ret
  character(LEN=256), dimension(CPS_MAX_ELTS) :: noms_elts
  character(LEN=256) :: fic_tmp

  ! fonction
  character(LEN=256) :: cps_ihm_getFicElt

  ! initialisation
  fic_present = .false.
  fic_tmp = ""

  ! acces au fichier
  acces = acc_load(trim(rep_base)//"/"//trim(nom_fic))
  
  ! parcourt des elements pour determiner s'il existe des ressources à supprimer
  call cps_ihm_getEltsFichier(acces, nb_elts, noms_elts)
  i = 1
  do while ((i<=nb_elts) .and. (.not.fic_present))
     fic_tmp = cps_ihm_getFicElt(acces, trim(noms_elts(i)), trim(rep_base))
     if (trim(fic_tmp)/="") then
        fic_present = .true.
     end if
     i = i+1
  end do

  ! fermeture du fichier
  ret = acc_close(acces)

end function cps_ihm_contientFic


subroutine cps_ihm_supprimerContenu(rep_base, nom_fic)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_supprimerContenu
!
!$Resume
!  Supprime les ressources associées à un fichire de données
!
!$Description
!  Cette routine supprime des fichiers de ressources associées aux
!  elements contenus dans le fichier de données. C'est le cas lorsque les éléments possède 
!  un attribut contenant la chaîne de caractères "fichier". S'il existe de plus un attribut
!  contenant la chaîne de caractères "repertoire", alors cet attribut est considéré
!  comme le répertoire dans la base contenant le fichier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_supprimerContenu(rep_base, nom_fic)
!.    character(LEN=*) :: rep_base, nom_fic
!
!$Arguments
!>E     rep_base  :<LEN=*>   repertoire de la base
!>E     nom_fic   :<LEN=*>   nom du fichier
!
!$Common
!
!$Routines
!- cps_ihm_getEltsFichier
!
!$Include
!#V
!- cps_ihm.h
!#
!
!$Module
!#V
!- cps_acces
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
  use cps_acces
  implicit none
  ! arguments
  character(LEN=*), intent(in) :: rep_base, nom_fic
  
#include "cps_ihm.h"
  ! variables internes
  integer :: acces, nb_elts, i, ret
  character(LEN=256), dimension(CPS_MAX_ELTS) :: noms_elts
  character(LEN=256) :: fic_tmp

  ! fonction
  character(LEN=256) :: cps_ihm_getFicElt
  integer :: system

  ! initialisation
  fic_tmp = ""

  ! acces au fichier
  acces = acc_load(trim(rep_base)//"/"//trim(nom_fic))
  
  ! parcourt des elements pour determiner s'il existe des ressources à supprimer
  call cps_ihm_getEltsFichier(acces, nb_elts, noms_elts)
  i = 1
  do while (i<=nb_elts)
     fic_tmp = cps_ihm_getFicElt(acces, trim(noms_elts(i)), trim(rep_base))
     if (trim(fic_tmp)/="") then
        ! suppression du fichier
        ret = system("/bin/rm -f "//trim(fic_tmp))
     end if
     i = i+1
  end do

  ! fermeture du fichier
  ret = acc_close(acces)
  
end subroutine cps_ihm_supprimerContenu
