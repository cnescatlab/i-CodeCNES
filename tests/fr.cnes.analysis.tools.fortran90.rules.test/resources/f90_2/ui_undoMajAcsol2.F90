!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!  Routines utiles pour les IHM COMPAS (administration et processus automatiques)
!
!$Version
!  $Id: ui_undoMajAcsol2.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_undoMajAcsol2.F90,v $
!  Revision 1.7  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.6  2009/03/24 10:19:08  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!
!  Revision 1.5  2009/03/24 08:59:05  cml
!  DM-ID 1159 : Ajout d'initialisations de pointeurs manquantes
!
!  Revision 1.4  2008/04/11 12:43:54  vivaresf
!  Version 2.4 AQ : correction des cartouches et suppression code commenté
!
!  Revision 1.3  2008/04/11 10:57:22  vivaresf
!  FA-ID 778 : suppression des variables inutilisées te des codes en commentaires
!
!  Revision 1.2  2008/04/11 10:09:30  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!
!  Revision 1.1  2008/02/08 17:51:28  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!
!  Revision 1.7  2006/10/23 12:56:47  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!
!  Revision 1.6.4.1  2006/10/23 10:12:33  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!
!  Revision 1.6  2006/06/16 17:32:09  vivaresf
!  Cartouches d'entete
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine cps_undoMajAcsol2 (jj, mm, aaaa)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_undoMajAcsol2
!
!$Resume
!
!$Description
!  V2-0
!  Ce programme efface les données sures et les predictions issues
!  du fichier ACSOL2 qui ont ete introduites à une date superieure
!  ou egale a la date fournie en entree.
!  La date est en jour julien 1950.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_undoMajAcsol2 (jj, mm, aaaa)
!.    integer :: jj, mm, aaaa
!
!$Arguments
!>E     jj    :<integer>   jour
!>E     mm    :<integer>   mois
!>E     aaaa  :<integer>   année
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
  use ui_ioplus

  implicit none
  ! arguments
  integer, intent(in) :: jj, mm, aaaa

  ! variables locales
  real(KIND=PM_REEL) :: date, sec
  !character(LEN=20), dimension(50) :: l_opt
  !integer :: noptions
  integer :: h, min
  character(LEN=256), dimension(:), pointer :: fichiers => NULL()
  integer :: nb_fic, i, acces, ret, nature
  character(LEN=256) :: libelle, fic
  type(tm_jour_sec) :: jour_sec
  type(tm_code_retour) :: code_retour

  ! conversion de la date en jj1950
  call md_calend_julien(aaaa, mm, jj, h, min, sec, jour_sec, code_retour)
  call md_joursec_jourfrac(jour_sec, date, code_retour)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! traitement des données sures !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  ! liste des fichiers : categorie CPS_CATEG_ACSOL2_SURE
  call cps_getListFichiersCateg(CPS_CATEG_ACSOL2_SURE, fichiers)
  nb_fic = size(fichiers)
  do i=1, nb_fic
     ! ouverture du fichier i de la categorie CPS_CATEG_ACSOL2_SURE
     call cpsi_getAccesMadona(trim(fichiers(i)), acces)
     ! parcours des elements
     ret = acc_scan_reset(acces)
     do
        ret = acc_scan(acces, libelle, nature)
        if (nature.eq.0) then
           ! fin du fichier
           exit
        elseif (nature.eq.ACC_STRUCT)then
           ! lecture d'une structure correspondant a un fichier de donnees
           ! lecture du nom du fichier
           ret = acc_gets(acces, trim(libelle)//".fichier", fic)
           ! on rajoute le repertoire de la base de reference
           fic = trim(rep_base_ref)//"/"//trim(fic)
           call cpsi_undoMajAcsol2(trim(fic), date, 1)
        end if
     end do
  end do
    
  ! liberation memoire
  if (associated(fichiers)) then
     deallocate(fichiers)
  end if
  

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! traitement des predictions      !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  ! liste des fichiers : categorie CPS_CATEG_ACSOL2_PRED
  call cps_getListFichiersCateg(CPS_CATEG_ACSOL2_PRED, fichiers)
  nb_fic = size(fichiers)
  do i=1, nb_fic
     ! ouverture du fichier i de la categorie CPS_CATEG_ACSOL2_PRED
     call cpsi_getAccesMadona(trim(fichiers(i)), acces)
     ! parcours des elements
     ret = acc_scan_reset(acces)
     do
        ret = acc_scan(acces, libelle, nature)
        if (nature.eq.0) then
           ! fin du fichier
           exit
        elseif (nature.eq.ACC_STRUCT)then
           ! lecture d'une structure correspondant a un fichier de donnees
           ! lecture du nom du fichier
           ret = acc_gets(acces, trim(libelle)//".fichier", fic)
           ! on rajoute le repertoire de la base de reference
           fic = trim(rep_base_ref)//"/"//trim(fic)
           call cpsi_undoMajAcsol2(trim(fic), date, 2)
        end if
     end do
  end do
    
  ! liberation memoire
  if (associated(fichiers)) then
     deallocate(fichiers)
  end if


end subroutine cps_undoMajAcsol2


subroutine cpsi_undoMajAcsol2(fic, date, type0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_undoMajAcsol2
!
!$Resume
!
!$Description
!  V2-0
!  Routine interne. Elle efface les structures MADONA dans un fichier de
!  donnees sures ou dans un fichier de donnees predites qui ont ete introduites
!  a une date superieure ou egale a la date specifiee en argument.
!  type0 = 1 : donnees sures
!  type0 = 2 : predictions
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_undoMajAcsol2(fic, date, type0)
!.    character(LEN=*) :: fic
!.    real(KIND=PM_REEL) :: date
!.    integer :: type0 
!
!$Arguments
!>E     fic    :<LEN=*>     fichier de données
!>E     date   :<PM_REEL>   les données introduites après cette date sont à supprimer
!>E     type0  :<integer>   1 pour les données sûres, 2 pour les prédictions
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

  ! arguments
  character(LEN=*), intent(in) :: fic
  real(KIND=PM_REEL), intent(in) :: date
  integer, intent(in) :: type0 ! 1=donnees sures ; 2=prdictions
  
  ! variables locales
  integer :: acces, ret, res
  real(KIND=PM_REEL) :: dd
  character(LEN=100) :: libelle
  integer :: nature
  logical :: modif
  character(LEN=10) :: type_date
  
  ! initialisation
  modif = .false.
  if (type0.eq.1) then
     ! donnees sures : date_jour
     type_date = ".date_jour"
  else
     ! predictions : date_pred
     type_date = ".date_pred"
  end if

  ! ouverture du fichier
  acces = acc_load(trim(fic))
  
  ! parcours des donnees du fichier
  ret = acc_scan_reset(acces)
  do
     ret = acc_scan(acces, libelle, nature)
     if (nature.eq.0) then
        ! fin du fichier
        exit
     elseif (nature.eq.ACC_STRUCT) then
        ! on regarde la date d'introduction de la structure : date_jour ou date_pred
        ret = acc_getd(acces, trim(libelle)//trim(type_date), dd, "")
        ! on compare a la date
        res = cpsi_compareReels(dd,date)
        if (res.ge.0) then
           ! la date d'introcdution de la structure est superieure a la date : on 
           ! supprime la structure
           ret = acc_delete(acces, trim(libelle))
           modif = .true.
        end if
     end if
  end do

  ! ecriture du fichier, s'il y a eut des modifications
  if (modif) then
     call cps_ihm_ecrireFichier(acces, trim(fic))
  end if

  ! fermeture du fichier
  ret = acc_close(acces)
  
end subroutine cpsi_undoMajAcsol2

