program ui_compasinfo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_compasinfo
!
!$Resume
!    Affichage des informations sur la base de données COMPAS
!
!$Description
!.    Liste des fichiers / corps / données / tables disponibles dans la base
!.    ou
!.    Sélection de corps ayant certaines propriétés
!
!     Cet utilitaire fournit les noms des constantes ou attributs disponibles
!     pour une table. Il permet donc de paramétrer l'utilitaire d'accès ui_compas
!     qui, lui, fournit les valeurs.
!
!  Usage
!
!   ui_compasinfo [ -llc ] [-lf [theorie]] [-lt1 | -lt2 | -lt [table]]
!.  [ -lc [ -nrang nrang -nbcorps nbcorps] -type [type] | -cc [codec] | -id identfichier ]
!.  [-h]
!
!
!  Arguments
!. Options demandées en sorties
!>[E] -llc         : description des règles de codage des corps
!>[E] -lf [theorie]: liste des théories disponibles ou des informations sur "theorie"
!>[E] -lt1         : liste des tables de niveau 1 disponibles
!>[E] -lt2         : liste des modèles de niveau 2
!>[E] -lt [table]  : contenu de la table "table" ou liste des tables de niveau 1 et 2
!>[E] -lc          : liste des corps dans la base
!>[E] -h           : aide en ligne
!
!. Options spécifiques à -lc
!>[E] -type [type] : sélectionne les corps de type "type" ou liste les types disponibles
!>[E] -cc [codec]  : sélectionne les corps de corps central "codec" ou liste les corps centraux 
!>[E] -id identfichier            : filtrage par identificateur de fichier
!>[E] [-nrang nrang] [-nbcorps nbcorps]: affiche les nbcorps (défaut 50) corps à partir 
!                                    du rang nrang (défaut 1)
!. toutes les options de -lc peuvent s'utiliser ensemble -lc -cc code -lc -type type ...
!
!$Auteur
!    F.VIVARES           (SchlumbergerSema)
!
!$Version
!  $Id: ui_compasinfo.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_compasinfo.F90,v $
!  Revision 1.12  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.11  2009/09/09 12:53:14  cmartel
!  FA-ID 1157 : Correction de constantes non nommées
!
!  Revision 1.10  2009/03/24 10:18:54  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!
!  Revision 1.9  2009/03/24 09:00:03  cml
!  DM-ID 1159 : Ajout d'initialisations de pointeurs manquantes
!
!  Revision 1.8  2008/11/07 14:35:58  cml
!  AQ : Correction de pointeurs non initialises
!
!  Revision 1.7  2008/11/07 14:01:34  cml
!  AQ : Renommage de la variable type en type_lu
!
!  Revision 1.6  2008/10/31 13:08:39  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.5  2008/04/28 13:01:22  vivaresf
!  FA-ID 664 : présentation des sorties et cartouches
!
!  Revision 1.4  2008/04/10 16:49:04  vivaresf
!  Version 2.4 : validation
!
!  Revision 1.3  2008/04/10 13:31:36  vivaresf
!  Version 2.4, validation et complément de commentaires
!
!  Revision 1.2  2008/04/10 10:57:31  vivaresf
!  Version 2.4 : correction des cartouches
!
!  Revision 1.1  2008/02/08 17:51:37  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.22  2007/11/19 07:38:12  sbd
!  FA-ID 803 mise a jour du cartouche
!  Revision 1.21  2007/11/13 16:54:48  sbd
!  FA-ID 803 correction du cartouche
!  Revision 1.20  2007/09/18 12:52:23  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.19  2007/07/12 13:55:22  vivaresf
!  DM-ID 666 : révision par rapport à la PM rev 1
!  Revision 1.18  2007/07/11 14:07:44  couturis
!  FA-ID 708: ajout de [-h] dans le cartouche + description
!  Revision 1.17  2007/07/11 13:12:53  couturis
!  FA-ID 666: remplacement de nb_corps par nbcorps
!  Revision 1.16  2007/07/05 17:14:34  vivaresf
!  FA-ID 664 : correction de la présentation
!  Revision 1.15  2007/06/15 11:12:40  vivaresf
!  FA-ID 667 : liste_id dynamique
!  Revision 1.14  2007/01/19 09:29:26  fabrec
!  FA-ID 666 : modification des options de ui_compasinfo
!  Revision 1.13  2007/01/18 15:57:57  fabrec
!  FA-ID 667 : rajout de systeme_solaire_asteroides
!  Revision 1.12  2007/01/18 14:51:54  fabrec
!  FA-ID 668 : ui_compasinfo
!  Revision 1.11  2006/12/01 10:00:57  vivaresf
!  Version 2-1p1
!  Revision 1.10  2006/11/21 12:08:42  vivaresf
!  Version 2.1 : passage Understand, variables inutilisées
!  Revision 1.9  2006/11/02 12:42:12  mle
!  passage understand
!  Revision 1.8  2006/10/23 12:55:49  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.7.2.1  2006/10/23 10:11:54  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.7  2006/10/06 08:51:56  mle
!  DM-ID 462 : version finale
!  Revision 1.6  2006/07/05 16:29:33  vpg
!  Modification des options de la commande ls : ajout de -t
!  Revision 1.5  2006/06/30 14:48:58  vpg
!  Mise a jour des cartouches
!  Revision 1.4  2006/06/16 17:32:01  vivaresf
!  Cartouches d'entete
!  Revision 1.3  2006/06/01 09:57:34  vivaresf
!  Routine dupliquee
!  Revision 1.2  2006/05/31 13:08:37  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.1.1.1  2006/02/06 15:48:20  vpg
!  ihm et utilitaires COMPAS V2-0
!  Revision 1.5  2005/12/09 14:46:23  vivaresf
!  Mise en forme des tests
!  Revision 1.4  2005/12/09 12:28:20  vivaresf
!  Dernieres validation
!  Revision 1.3  2005/12/08 18:24:28  vivaresf
!  Mise a jour des cartcouhes
!  Revision 1.2  2005/12/07 09:02:19  vpg
!  changement de la commande pour voir le contenu de la base : ls -lR au lieu de ls -l bdcompas*.conf
!  Revision 1.1.1.1  2005/12/07 07:23:10  vivaresf
!  Refonte de COMPAS
!  Revision 1.16  2005/03/07 15:31:46  vivaresf
!  Documentation des cartouches
!  Revision 1.15  2005/03/07 14:41:09  vivaresf
!  Version 1-5
!  Revision 1.14  2005/03/07 08:15:50  vivaresf
!  Version 1-5 : présentation de la documentation extraite des cartouches
!  Revision 1.13  2005/03/04 16:25:13  vivaresf
!  Livraison version 1-5 : correction des entêtes
!  Revision 1.12  2004/12/08 10:06:57  vivaresf
!  Presentation de l'aide en ligne
!  Revision 1.11  2004/12/07 14:07:38  tanguy
!  Modifs dans le cartouche : ordre et libelles des sections
!  Revision 1.10  2004/12/03 14:23:40  tanguy
!  Champs usage / arguments de l'entete
!  Revision 1.9  2004/12/03 10:52:54  tanguy
!  MAJ commentaires avec make_cartouchef90 ui_compasinfo.F90
!  Revision 1.8  2004/12/02 11:26:25  vivaresf
!  Version 1-4
!  Revision 1.6  2004/10/08 07:31:19  adminam
!  Version sans l'uilib
!  Revision 1.5  2004/03/29 15:06:40  vivaresf
!  DM_1
!  Revision 1.2.4.2  2004/03/29 14:53:37  vivaresf
!  Vresion V1_3 du CNES
!  Revision 1.2.4.1  2004/03/15 11:40:38  vivaresf
!  Evolutions suite a l'utilisation de COMPAS
!  Revision 1.1.1.1  2003/01/07 08:29:07  vivaresf
!  Bibliotheque de COnstantes et Modeles Associes au systeme Solaire
!
!$FinHistorique
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!     ui_compas
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include "cps_ihm.h"

  use mslib
  use mspro
  use msp_gestion_erreur
  use cps_utilisateur
  use ui_ioplus

  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_compasinfo.F90 69 2012-09-11 08:33:34Z ffsm $'


#include "formats.h"

  ! entrées
  character(LEN=256) :: theorie,identfichier, table, type
  integer :: nmin, nmax

  ! options 

  logical :: ccd=.false.
  logical :: listed=.false.
  logical :: typed=.false.
  logical :: nd = .false.
  logical :: idd = .false.
  logical :: lt1d = .false.
  logical :: lt2d = .false.
  logical :: ltd = .false.
  logical :: llcd = .false.
  logical :: nvd = .false.
  integer :: trouve=CPS_ERR_DEF
  integer :: codec = -1

  ! variables locales
  character(LEN=256) :: dirfcf
  character(len=30), dimension(MAXCORPS*2) :: nomsfr, nomsen, nomconst
  character(LEN=256), dimension(50) :: l_opt
  character(LEN=18),dimension(3) :: N2 =(/ "ephemerides       ","mvts_poles_UAI    ","modeles_atmosphere" /)
  character(LEN=CPS_MAXLG), dimension(:), pointer :: noms => NULL(), liste_id => NULL()
  character(LEN=256), dimension(CPS_MAX_ELTS) :: elements
  character(len=50) :: nom1, nom2, nom3, format0
  character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL(), listCateg => NULL()
  type(MSP_MESSAGE) :: messages
  type(cpsi_desc) :: desc
  integer :: lch_dirfcf, ii, ncorps, jj, indDeb, indFin
  integer :: ier, ierfin, iostat, noptions, ind, nvar, nb_elts, nb_id
  integer, dimension(:), pointer :: codes => NULL()
  integer, dimension(:), pointer :: codes1 => NULL(), codes2 => NULL()
  integer, dimension(MAXCORPS*2) :: codescorps
  logical::err,fini

  ! initialisation
  ier = AMv_rc_get ('compas_ui_fcf','compas','','dirfcf',dirfcf, &
       lch_dirfcf)
  call MSP_ajouter_fichier_message (trim(dirfcf)//"/CPSUI_erreur_fr")
  call cps_init_utilisateur()
  if (MSP_gen_messages("ui_compasinfo")) goto 999
  theorie=""
  identfichier=""
  type = ""
  ! valeurs par defaut:
  ! premier corps affiché
  nmin = 1
  ! nombre de corps affiché
  nmax = 50

  ! analyse des arguments
  !----------------------
  
  ! Lecture des paramètres d'entrée
  call ui_lire_options(noptions, l_opt)
  call ui_test_arguments ("-h", "", ierfin)
  
  ! -h ou pas de parametres
  if(ierfin.eq.1.or.noptions==0) then
     call ui_ecrire_help("ui_compasinfo")
     goto 999
  endif
  
  ! decodage
  call cpsi_lectOptInit(l_opt,llcd,listed,theorie,typed,type,nvar,&
     ccd,codec,idd,identfichier,nd,nmin,nmax,nvd,lt1d,lt2d,ltd,table)

  ! corps du programme
  !-------------------

  !-------------------------------------------------
  !description des règles de codage des corps (-llc)
  !-------------------------------------------------
  ! géré de la même manière que l'option -lc -n

  !----------------------------
  !information sur les théories
  !----------------------------

  call cpsi_listeInfoTheo(nvar,listed,nvd,theorie,fini)

  !---------------------------
  !liste des corps disponibles
  !---------------------------
  ! liste des corps disponibles triés par type (-lc -type)
  if (typed) then
     call cpsi_listeCorpsType (nvar,nvd,type,codes,ccd,noms)
  end if
  ! liste des corps disponibles tries par corps central (-lc -cc)
  if (ccd) then
     call cpsi_listeTousCodes(nvar,nvd,typed,codec,codes,noms)
  end if
  ! liste des corps disponibles tries par corps central et par
  ! type (-lc -cc code -lc -type type)
  if (typed .and. ccd) then
     call cpsi_listeCorpsTypeCode(codec,codes1,codes2,type,codes,noms,nvd)
  end if
  if (size(codes)>=1) then
     format0 = '(a20,I10)'
     indDeb = min(nmin, size(codes))
     indFin = min(nmax+nmin-1, size(codes))
  end if
  if(idd .and.(size(codes)>=1)) then
     ! on parcourt tous les fichiers pour retrouver l'id demandée
     call cps_getListFichiersCateg("corps", fichiers)
     do jj = 1, size(fichiers)
        trouve = cpsi_getDescFichier(fichiers(jj), desc)
        if (trim(desc%id) == trim(identfichier)) then
           call cpsi_trouveId(desc,nb_elts,elements,indDeb,indFin, &
     codes,noms,err)
           if(err) goto 999
        end if
     end do
  else if (size(codes)>=1) then
     do ii=indDeb, indFin
        trouve = cps_getAtt(codes(ii), "nom_id", noms(ii))
        if (MSP_ERREUR) goto 999
        write(*,format0) trim(noms(ii)), codes(ii)
     end do
  end if

  ! liste des corps disponibles tries par identificateur de fichier (-lc -id)
  if (idd) then
     if ((nvar==2).or.((nvar==3).and. nvd)) then
        !-lc -id [-nvd]
        call cpsi_getListId(liste_id)
        nb_id = size(liste_id)
        if(.not. nvd) write(*,1000) " Identificateurs de fichiers disponibles = ["
        do ii=1, nb_id
           write(*,1000) trim(liste_id(ii))
        enddo
        if(.not. nvd) write(*,1000) "]"

        goto 999
     else if((nvar==3).or.((nvar==4).and. nvd).or.(nd .and. .not. ccd .and. .not. typed)) then
        !-lc -id identfichier [-nvd]
        if(.not. nvd) write(*,'(a50,1x,a8)')"nom","code"
        call cps_getListFichiersCateg("corps", fichiers)
        ind =0
        do jj = 1, size(fichiers)
           !récupération de la description des fichiers
           trouve = cpsi_getDescFichier(fichiers(jj), desc)
           if (trim(desc%id) == trim(identfichier)) then
              call cpsi_listeCodesLimitee(desc,nb_elts,elements,ind,nmin,nmax,err)
              if(err) goto 999
           end if
        end do
        goto 999
     end if
  end if
  ! liste des corps disponibles tries par indice ou nombre 
  ! maximal de corps -lc -n, option seule
  if ( (nd .and. ((nvar==4) .or.(nvar==8).or.((nvar==5).and. nvd).or.((nvar==9).and.nvd))) .or.   llcd ) then
     if (llcd .and. (.not. nvd)) write(*,1000)"Par défaut, l'information est donnée pour les 50 premiers corps."
     ncorps=nmax
     call eph_lire_ephem_corps(nomsfr, nomsen, nomconst, codescorps, ncorps,nmin)
     format0 = '(a57,a20,a7)'
     if((ncorps.gt.0).and. .not. nvd) write(*,format0) "Corps disponibles   ", "constante   ","code"
     do ii=1, nmax
        nom1=nomsen(ii)
        nom2=nomsfr(ii)
        nom3=nomconst(ii)
        format0 = '(a57,a20,i7)'
        write(*,format0) trim(nom1)//" ("// trim(nom2)//") : ",trim(nom3)//" = ", codescorps(ii)
     end do
     goto 999
  end if

  !--------------------------------------
  ! liste des types de données disponibles
  !--------------------------------------
  ! liste de modèles de niveau 2  (-lt2)
  if(lt2d) then
     write(*,1004)trim(N2(1))," (N2)"
     write(*,1004)trim(N2(2))," (N2)"
     write(*,1004)trim(N2(3))," (N2)"
     goto 999 
  end if
  !liste de modèles de niveau 1  (-lt1) et de tous niveaux (-lt) 
  if (lt1d .or. (ltd.and. ((nvar==1).or.((nvar==2).and. nvd)) ) ) then
     !récupération des categories
     call cpsi_getListCateg(listCateg)
     !affichage des catégories
     call cpsi_afficheCateg(listCateg,N2,ltd,lt1d)
     goto 999
  end if
  !contenu d'une table de niveau 1  (-lt table)

  if (ltd) then
     call cpsi_listeInfoTable(nvd,table)
     goto 999
  end if
    
999 continue
  
  ! variables dynamiques
  if (associated(codes)) deallocate(codes, stat=iostat)
  if (associated(codes1)) deallocate(codes1, stat=iostat)
  if (associated(codes2)) deallocate(codes2, stat=iostat)
  if (associated(noms)) deallocate(noms, stat=iostat)
  if (associated(listCateg)) deallocate(listCateg, stat=iostat)
  if (associated(fichiers)) deallocate(fichiers, stat=iostat)

  ! Warning/erreur
  if (MSP_PROBLEME) then
     call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
     call MSP_afficher_message (message=messages,unit=0)
  endif
  
  ! fin
  call cps_close_utilisateur()

contains

subroutine cpsi_trouveId(desc,nb_elts,elements,indDeb,indFin, &
     codes,noms,err)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_trouveId
!
!$Resume
!  Affichage des noms des corps
!$Description
!  récupération des fichiers en fonctions du corps et de l'Id
!  une fois que l'id du fichier correspond a l'identificateur donné
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_trouveId(desc,nb_elts,elements,indDeb,indFin, &
!.         codes,noms,err)
!.    logical :: err
!.    type(cpsi_desc) :: desc
!.    integer :: nb_elts,indDeb,indFin
!.    character(LEN=*), dimension(:), pointer :: noms
!.    character(LEN=256), dimension(:) :: elements
!.    integer,dimension(:),pointer :: codes
!
!$Arguments
!>E/S   desc      :<cpsi_desc>         descripteur de fichier COMPAS
!>E/S   nb_elts   :<integer>           Nombre de champs dans le fichier
!>E     elements  :<LEN=256,DIM=(:)>   tableau des noms de champs
!>E/S   indDeb    :<integer>           | indices et début/fin de parcours du tableau
!>E/S   indFin    :<integer>           | (nrang, nrang+nbcorps) 
!>E/S   codes     :<integer,DIM=(:),pointer> codes à détecter  
!>E/S   noms      :<LEN=*,DIM=(:),pointer>   Noms des corps à afficher
!>S     err       :<logical>                   
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mslib
  use cps_utilisateur

  implicit none

#include "formats.h"

  !arguments
  logical, intent(out)::err
  type(cpsi_desc) :: desc
  integer,intent(inout)::nb_elts,indDeb,indFin
  character(LEN=*), dimension(:), pointer :: noms
  character(LEN=256), dimension(:),intent(in) :: elements
  integer,dimension(:),pointer::codes

  !variables locales
  integer :: ii,trouve,kk,ni,acces
  character(len=20)::format0='(a20,I10)'
  character(len=50)::niCar

  ! Initialisations
  err=.false.

  !on regarde si le corps est dans le fichier
  call cpsi_getAccesMadona(trim(desc%fichier),acces)
  call cps_ihm_getEltsFichier(acces, nb_elts, elements)
  do ii=indDeb, indFin
     trouve = cps_getAtt(codes(ii), "nom_id", noms(ii))
     if (MSP_ERREUR) then
        err=.true.
     else
        do kk = 1, nb_elts
           niCar = elements(kk)
           read(trim(niCar(6 :)),*) ni
           if (ni==codes(ii)) write(*,format0) trim(noms(ii)), codes(ii)
        end do
     end if
  end do
end subroutine cpsi_trouveId

subroutine cpsi_listeTousCodes(nvar,nvd,typed,codec,codes,noms)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_listeTousCodes
!
!$Resume
!  Ecriture des titres pour les corps
!$Description
!  Ecriture des titres pour les corps en fonction de leur type et/ou de leur
!  corps central et sélection de la liste
!  ou, si -lc cc, affichage des corps centraux possibles
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_listeTousCodes(nvar,nvd,typed,codec,codes,noms)
!.    integer :: nvar,codec
!.    logical :: nvd,typed
!.    integer, dimension(:), pointer :: codes 
!.    character(LEN=*), dimension(:), pointer :: noms 
!
!$Arguments
!>E     nvar   :<integer>         type de sortie          
!>E     nvd    :<logical>         mode non bavard
!>E     typed  :<logical>         satellite d'un corps central demandé 
!>E     codec  :<integer>         code corps central          
!>S   codes  :<integer,DIM=(:),pointer>   codes trouvés
!>S   noms   :<LEN=*,DIM=(:),pointer>     tableau des noms (non initialisés)
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
  
#include "formats.h"

  !arguments
  integer,intent(in)::nvar,codec
  logical, intent(in)::nvd,typed
  integer, dimension(:), pointer :: codes 
  character(LEN=*), dimension(:), pointer :: noms  

  if ((nvar==2).or.((nvar==3).and. nvd)) then
     !-lc -cc
     if (.not. nvd)  write(*,1000) "Liste des codes de corps centraux disponibles = "
     write(*,1000) " 10 (Soleil)"
     write(*,1000) "199 (Mercure)"
     write(*,1000) "299 (Venus)"
     write(*,1000) "399 (Terre)"
     write(*,1000) "499 (Mars)"
     write(*,1000) "599 (Jupiter)"
     write(*,1000) "699 (Saturne)"
     write(*,1000) "799 (Uranus)"
     write(*,1000) "899 (Neptune)"
     write(*,1000) "999 (Pluton)"
  else if (.not. typed) then
     !-lc -cc code
     call cps_getCorps("corpsc", codec, codes)
     if (size(codes)>=1) then
        if (.not. nvd)  write(*,'(a20,a10)') "nom", "code"
        allocate(noms(size(codes)))
     else
        write(*,1000)"Il n'y a pas de corps correspondant à la demande"
     end if
  end if
end subroutine cpsi_listeTousCodes

subroutine cpsi_listeCorpsType (nvar,nvd,type,codes,ccd,noms)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_listeCorpsType
!
!$Resume
!  liste les corps triés par type
!$Description
!  liste les corps triés par type  et écriture des titres
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_listeCorpsType (nvar,nvd,type,codes,ccd,noms)
!.    integer :: nvar
!.    logical :: nvd,ccd
!.    integer, dimension(:), pointer :: codes
!.    character(LEN=256) :: type
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms
!
!$Arguments
!>E     nvar   :<integer>         type de sortie                
!>E     nvd    :<logical>         mode non bavard  
!>E     type   :<LEN=256>         type demandé (corps, etroile, bary) 
!>S   codes  :<integer,DIM=(:),pointer>         codes trouvé
!>E     ccd    :<logical>                       corps d'un type demandé
!>S   noms   :<LEN=CPS_MAXLG,DIM=(:),pointer>   tableau des noms (non initialisé)
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mslib
  use cps_utilisateur

  implicit none

#include "formats.h"

  ! Arguments
  integer, intent(in) :: nvar
  logical, intent(in) :: nvd,ccd
  integer, dimension(:), pointer :: codes
  character(LEN=256),intent(in) :: type
  character(LEN=CPS_MAXLG), dimension(:), pointer :: noms
  
  ! Variables locales

  ! Calculs
  if ((nvar==2).or.((nvar==3).and. nvd)) then
     !-lc -type
     if (.not. nvd) then
        write(*,1000) " Types disponibles = [ corps etoile bary ]"
     else 
        write(*,1000) "corps etoile bary"
     end if
  else if (.not. ccd) then
     !-lc -type type
     call cps_getCorps("type", type, codes)
     if (size(codes)>=1) then
        if (.not. nvd)  write(*,'(a20,a10)') "nom", "code"
        allocate(noms(size(codes)))
     else
        write(*,1000)"Il n'y a pas de corps correspondant à la demande"
     end if
  end if
end subroutine cpsi_listeCorpsType

subroutine cpsi_listeCorpsTypeCode(codec,codes1,codes2,type,codes,noms,nvd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_listeCorpsTypeCode
!
!$Resume
!  liste les corps triés par type et par code
!$Description
!  liste les corps triés par type et par code
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_listeCorpsTypeCode(codec,codes1,codes2,type,codes,noms,nvd)
!.    integer :: codec
!.    character(len=*) :: type
!.    logical :: nvd
!.    integer, dimension(:), pointer :: codes,codes1, codes2
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms
!
!$Arguments
!>E     codec   :<integer>                    code corps central     
!>E/S   codes1  :<integer,DIM=(:),pointer>    codes des satellites de codec trouvés
!>E/S   codes2  :<integer,DIM=(:),pointer>    codes des corps d'un type
!>E     type    :<LEN=*>                      type     
!>E/S   codes   :<integer,DIM=(:),pointer>        tableau des codes (non rempli) 
!>E/S   noms    :<LEN=CPS_MAXLG,DIM=(:),pointer>  tableau des noms  (non rempli)
!>E     nvd     :<logical>                    mode non bavard
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

#include "formats.h"

  !arguments
  integer,intent(in)::codec
  character(len=*),intent(in)::type
  logical, intent(in)::nvd
  integer, dimension(:), pointer :: codes,codes1, codes2
  character(LEN=CPS_MAXLG), dimension(:), pointer :: noms

  call cps_getCorps("corpsc", codec, codes1)
  call cps_getCorps("type", type, codes2)     
  ! Intersection de listes de codes
  ! resultats dans codes
  call cpsi_intersecter(codes1,codes2, codes)
  if (size(codes)>=1) then
     if (.not. nvd)  write(*,'(a20,a10)') "nom", "code"
     allocate(noms(size(codes)))
  else
     write(*,1000)"Il n'y a pas de corps correspondant à la demande"
  end if

end subroutine cpsi_listeCorpsTypeCode

subroutine cpsi_afficheCateg(listCateg,N2,ltd,lt1d)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_afficheCateg
!
!$Resume
!  affiche les catégories disponibles pour un modèle
!$Description
!  affiche les catégories disponibles pour un modèle
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_afficheCateg(listCateg,N2,ltd,lt1d)
!.    character(LEN=18),dimension(3) :: N2
!.    character(LEN=*), dimension(:), pointer :: listCateg
!.    logical :: ltd,lt1d
!
!$Arguments
!>E/S   listCateg  :<LEN=*,DIM=(:),pointer>   liste des catégories
!>E     N2         :<LEN=18,DIM=(3)>          tableau des catégories de niveau 1 
!>E     ltd        :<logical>                 catégories n2 demandées
!>E     lt1d       :<logical>                 catégories n1 demandées
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

#include "formats.h"

  !arguments
  character(LEN=18),dimension(3),intent(in) :: N2
  character(LEN=*), dimension(:), pointer :: listCateg
  logical, intent(in)::ltd,lt1d

  ! Variables locales
  integer::ii

  ! Recherche des tables

  do ii = 1, size(listCateg)
     if((trim(listCateg(ii))/=trim(N2(1))).and.(trim(listCateg(ii))/=trim(N2(2))) &
          .and.(trim(listCateg(ii)))/=trim(N2(3)))  then
        if(ltd)  write(*,1004)trim(listCateg(ii))," (N1)"
        if(lt1d)  write(*,1004)trim(listCateg(ii))," (N1)"
     else
        if(ltd)  write(*,1004)trim(listCateg(ii))," (N2)"
     end if
  end do

end subroutine cpsi_afficheCateg
  
end program ui_compasinfo

subroutine ui_lire_fic3(fic, typec, codec)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_lire_fic3
!
!$Resume
!  lecture des options 
!$Description
!  lecture des options si -fic, -cc ou -type
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_lire_fic3(fic, typec, codec)
!.    character(len=*) :: fic, typec
!.    integer :: codec
!
!$Arguments
!>S     fic    :<LEN=*>     Fichier     
!>S     typec  :<LEN=*>     Type 
!>S     codec  :<integer>   Corpsc central
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

#include "formats.h"

  ! Arguments
  character(len=*), intent(out) ::fic, typec
  integer, intent(out) :: codec

  ! Variables locales
  integer::nb_arg,pos
  character(len=100)::argument, argument2
  logical :: ficok=.false.
  logical :: ccok=.false.
  logical :: typeok=.false.
    
  ! Fonctions
  integer::iargc

  ! Calculs
  nb_arg = iargc()
  
  do pos=1,nb_arg
     call getarg(pos,argument)
     
     if(trim(argument).eq."-fic") then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument2)
           fic=trim(argument2)
           ficok=.true.
        endif
     endif
     if(trim(argument).eq."-cc") then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument2)
           read(trim(argument2),*) codec
           ccok=.true.
        endif
     endif
     if(trim(argument).eq."-type") then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument2)
           read(trim(argument2),*) typec
           typeok=.true.
        endif
     endif
  enddo

end subroutine ui_lire_fic3

subroutine cpsi_lectOptInit(l_opt,llcd,listed,theorie,typed,type_lu, nvar,&
     ccd,codec,idd,identfichier,nd,nmin,nmax,nvd,lt1d,lt2d,ltd,table)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lectOptInit
!
!$Resume
!  lecture des options entrées et initialisation
!$Description
!  lecture des options entrées et initialisation
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_lectOptInit(l_opt,llcd,listed,theorie,typed,type_lu, nvar,&
!.         ccd,codec,idd,identfichier,nd,nmin,nmax,nvd,lt1d,lt2d,ltd,table)
!.    character(LEN=256), dimension(50) :: l_opt
!.    logical :: llcd,listed,typed,idd,nd,nvd,lt1d,lt2d,ltd,ccd
!.    integer :: codec,nmin,nmax,nvar
!.    character(LEN=256) :: theorie,type,table,identfichier
!
!$Arguments
!>E     l_opt         :<LEN=256,DIM=(50)>   tableau des paramètres d'appel
!>E/S   llcd          :<logical>            option -llc
!>E/S   listed        :<logical>            option -lf
!>E/S   theorie       :<LEN=256>            théorie
!>E/S   typed         :<logical>            option -type
!>E/S   type_lu       :<LEN=256>            valeur du type 
!>E/S   nvar          :<integer>            nombre de paramètre d'appel
!>E/S   ccd           :<logical>            option -cc
!>E/S   codec         :<integer>            code corps central
!>E/S   idd           :<logical>            option -id
!>E/S   identfichier  :<LEN=256>            identificateur
!>E/S   nd            :<logical>            options -nrang -nbcorps
!>E/S   nmin          :<integer>            rang min
!>E/S   nmax          :<integer>            nombre de corps
!>E/S   nvd           :<logical>            option -nv
!>E/S   lt1d          :<logical>            option -lt1
!>E/S   lt2d          :<logical>            option -lt2
!>E/S   ltd           :<logical>            option -lt
!>E/S   table         :<LEN=256>            table
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use msp_gestion_erreur
  implicit none

#include "formats.h"

  !arguments
  character(LEN=256), dimension(50),intent(in) :: l_opt
  logical,intent(inout)::llcd,listed,typed,idd,nd,nvd,lt1d,lt2d,ltd,ccd
  integer,intent(inout)::codec,nmin,nmax,nvar
  character(LEN=256),intent(inout)::theorie,type_lu,table,identfichier
  
  ! Constantes
  integer, parameter :: NB_MAX_CORPS=50

  !variables locales
  integer::iargc,ind
  

  !on regarde toutes les variables
  nvar=iargc()
  do ind = 1, 2*nvar
     select case (l_opt(ind))
     case ("-llc")
        llcd = .true.
     case ("-lf")
        listed = .true.
        theorie = l_opt(ind+1)
     case ("-lc") 
        !cette option n'est utilisable qu'associée à un critère de selection
        select case (l_opt(ind+2))
        case ("-type")
           typed=.true.
           type_lu =  l_opt(ind+3) 
        case("-cc")
           ccd=.true.
           if (nvar>2) then
              read(trim(l_opt(ind+3)),*) codec
           end if
        case("-id")
           idd = .true.
           identfichier = l_opt(ind+3)
        case default
           call MSP_signaler_message(cle_mes="CPS_OPT_LC", & 
                routine="cps_liste") 
        end select
     case("-nrang")
        nd = .true.
        read(trim(l_opt(ind+1)),*) nmin
     case("-nbcorps")
        read(trim(l_opt(ind+1)),*) nmax
     case("-lt1")
        lt1d = .true.
     case("-lt2")
        lt2d = .true.
     case("-lt")
        ltd = .true.
        table = trim(l_opt(ind+1))
     case("-nv")
        nvd = .true.
     end select
  end do 
  if(nmax>NB_MAX_CORPS) nmax=NB_MAX_CORPS
  if(nmin<1)  nmin=1

end subroutine cpsi_lectOptInit

subroutine cpsi_listeCodesLimitee(desc,nb_elts,elements,ind,nmin,nmax,err)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_listeCodesLimitee
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
!  call cpsi_listeCodesLimitee(desc,nb_elts,elements,ind,nmin,nmax,err)
!.    logical :: err
!.    type(cpsi_desc) :: desc
!.    integer :: nb_elts,nmin,nmax,ind
!.    character(LEN=256), dimension(CPS_MAX_ELTS) :: elements
!
!$Arguments
!>E/S   desc      :<cpsi_desc>                    
!>E/S   nb_elts   :<integer>                      
!>E     elements  :<LEN=256,DIM=(CPS_MAX_ELTS)>   
!>E/S   ind       :<integer>                      
!>E/S   nmin      :<integer>                      
!>E/S   nmax      :<integer>                      
!>S     err       :<logical>                      
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mslib
  use cps_acces
  implicit none

#include "formats.h"

  !arguments
  logical, intent(out)::err
  type(cpsi_desc) :: desc
  integer,intent(inout)::nb_elts,nmin,nmax,ind
  character(LEN=256), dimension(CPS_MAX_ELTS),intent(in) :: elements

  !variables locales
  integer :: ii,acces,ni
  character(len=50) :: niCar,nom_corps

  ! Initialisations
  err=.false.

  ! Recherche et affichage
  call cpsi_getAccesMadona(trim(desc%fichier),acces)
  call cps_ihm_getEltsFichier(acces, nb_elts, elements)
  do ii = 1, nb_elts
     niCar = elements(ii)
     read(trim(niCar(6 :)),*) ni
     call cps_codenom(ni, nom_corps)
     ind = ind+1
     if ((ind>=nmin).and.(ind<nmin+nmax)) write(*,'(a50,1x,i8)')trim(nom_corps), ni
     if (ind>=nmin+nmax) err=.true.
  end do

end subroutine cpsi_listeCodesLimitee

subroutine cpsi_listeInfoTable(nvd,table)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_listeInfoTable
!
!$Resume
!  liste les informations d'une table de niveau 1
!$Description
!  liste les informations d'une table de niveau 1
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_listeInfoTable(nvd,table)
!.    logical :: nvd
!.    character(len=*) :: table
!
!$Arguments
!>E     nvd    :<logical>   mode non bavard
!>E     table  :<LEN=*>     nom de la table
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

#include "formats.h"

  !arguments
  logical,intent(in)::nvd
  character(len=*),intent(in)::table

  !variables locales
  integer::trouve,ii,jj
  character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
  type(cpsi_desc)::desc
  character(len=50)::typeDesc

2000 format(3(a30, 1x)) 
2002 format(a60, 1x, a30)
  write(*,'(a93)')"************************************************************************************************"
  if (.not. nvd)  write(*,1004) "TABLE ", trim(table)
  if (.not. nvd) then
     write(*,'(a93)')"************************************************************************************************"
  end if
  if (.not. nvd)  write(*,2000)"nom","type","unité"
  if (.not. nvd) then
     write(*,'(a93)')"************************************************************************************************"
  end if
  !récupération des fichiers
  call cps_getListFichiersCateg(trim(table), fichiers)
  trouve = cpsi_getDescFichier(fichiers(1), desc)
  do ii=1, size(desc%infosChamps)
     select case (desc%infosChamps(ii)%type)
        case(0)
           typeDesc ="entier"
        case(1)
           typeDesc = "réel"
        case(2)
           typeDesc = "chaîne de caractères"
     end select
     write(*,2000) trim(desc%infosChamps(ii)%nom),trim(typeDesc), trim(desc%infosChamps(ii)%unite)
  end do
  write(*,'(a93)')"************************************************************************************************"
  if (.not. nvd) write(*,1000) "Fichiers associés :"
  if (.not. nvd) then
     write(*,'(a93)')"************************************************************************************************"
  end if
  if (.not. nvd) write(*,2002)"fichier","famille(id)"
  if (.not. nvd) then
     write(*,'(a93)')"************************************************************************************************"
  end if
  write(*,2002)trim(desc%fichier),trim(desc%id)
  do jj = 2, size(fichiers)
     !récupération de la description des fichiers
     trouve = cpsi_getDescFichier(fichiers(jj), desc)
     write(*,2002)trim(desc%fichier),trim(desc%id)
  end do
end subroutine cpsi_listeInfoTable

subroutine cpsi_listeInfoTheo(nvar,listed,nvd,theorie,fini)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_listeInfoTheo
!
!$Resume
!  donne les informations sur les theories
!$Description
!  donne les informations sur les theories
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_listeInfoTheo(nvar,listed,nvd,theorie,fini)
!.    integer :: nvar
!.    logical :: nvd,listed
!.    character(len=*) :: theorie
!.    logical :: fini
!
!$Arguments
!>E     nvar     :<integer>   type d'entré
!>E     listed   :<logical>   infos sur théorie nécessaires
!>E     nvd      :<logical>   mode non bavard
!>E     theorie  :<LEN=*>     théorie demandée
!>S     fini     :<logical>   retour
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

#include "formats.h"

  !arguments
  integer,intent(in)::nvar
  logical,intent(in)::nvd,listed
  character(len=*),intent(in)::theorie
  logical,intent(out)::fini

  !variables locales
  character(len=256)::affiche
  integer::ii,ios
  character(LEN=CPS_MAXLG), dimension(:), pointer :: listeTheories => NULL()
  character(LEN=8),dimension(4) :: infoTh =(/ "G       ","vlum    ","ua      ", &
       "coef_prs" /)
  character(len=50) :: unite
  real(kind=pm_reel):: valeur

  ! Initialisations
  nullify (listeTheories)
  fini=.false.

  ! Liste des theories (-lf)
  if (listed .and. ((nvar==1).or.((nvar==2) .and. nvd))) then
     call cps_getListTheories(listeTheories)
     affiche = listeTheories(1)
     do ii = 2, size(listeTheories)
         affiche = trim(affiche)//" "//trim(listeTheories(ii))
      end do
      if(.not. nvd) then
         write(*,1000) "Théories disponibles = [ "//trim(affiche)//" ]"
      else
         write(*,1000) trim(affiche)
      end if
      fini=.true.
   end if

  ! Information sur une théorie (-lf theorie)
  if (listed .and. ((nvar>2).or.((nvar==2) .and. .not.nvd))) then
     if (.not. nvd) write(*,1004) "Théorie ", trim(theorie)
     affiche = ""
     do ii=1, 4
        ios = cps_getCsteGenTh(theorie, trim(infoTh(ii)), valeur, unite)
        if (ios==CPS_OK) affiche = trim(affiche)//" "// trim(infoTh(ii))
     end do
     if (.not.nvd) then
        write(*,1000) "Informations disponibles = [ "//trim(affiche)//" ]"
     else
         write(*,1000)trim(affiche)
      end if
     fini=.true.
  end if
  if (associated(listeTheories)) deallocate(listeTheories)
end subroutine cpsi_listeInfoTheo












