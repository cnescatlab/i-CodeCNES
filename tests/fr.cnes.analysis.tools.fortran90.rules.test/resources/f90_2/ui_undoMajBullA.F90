!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!  Routines utiles pour les IHM COMPAS (administration et processus automatiques)
!
!$Version
!  $Id: ui_undoMajBullA.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_undoMajBullA.F90,v $
!  Revision 1.5  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.4  2009/03/24 10:19:07  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!
!  Revision 1.3  2009/03/24 08:59:05  cml
!  DM-ID 1159 : Ajout d'initialisations de pointeurs manquantes
!
!  Revision 1.2  2008/04/11 12:43:55  vivaresf
!  Version 2.4 AQ : correction des cartouches et suppression code commenté
!
!  Revision 1.1  2008/02/08 17:51:29  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!
!  Revision 1.7  2006/10/23 12:56:52  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!
!  Revision 1.6.4.1  2006/10/23 10:12:39  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!
!  Revision 1.6  2006/06/16 17:32:10  vivaresf
!  Cartouches d'entete
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_undoMajBullA (jj, mm, aaaa)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_undoMajBullA
!
!$Resume
!
!$Description
!  V2-0
!  Cette routine efface les données sures et les predictions issues
!  du bulletin A de l'IERS qui ont ete introduites à une date superieure
!  ou egale a la date fournie en entree.
!  La date est en jour julien, de la meme reference que celle inscrite dans
!  COMPAS (53000...)
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_undoMajBullA (jj, mm, aaaa)
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
  use cps_utilisateur
  use ui_ioplus

  implicit none

  ! arguments
  integer, intent(in) :: jj, mm, aaaa
  
  ! variables locales
  real(KIND=PM_REEL) :: date, sec
  !character(LEN=20), dimension(50) :: l_opt
  !integer :: noptions
  integer :: res, ret
  character(LEN=256), dimension(:), pointer :: fichiers => NULL()
  integer :: nb_fic, i, acces, h, min
  real(KIND=PM_REEL) :: date_fic
  character(LEN=256) :: libelle, fic
  integer :: nature
  logical :: modif
  type(tm_jour_sec) :: jour_sec
  type(tm_code_retour) :: code_retour

  ! initialisation COMPAS
  !call cps_init_utilisateur()

  ! conversion de la date en MJD
  call md_calend_julien(aaaa, mm, jj, h, min, sec, jour_sec, code_retour)
  call md_joursec_jourfrac(jour_sec, date, code_retour)
  ! conversion date jj1950 en MJD
  date = date+33282.0_pm_reel

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! traitement des données sures !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  ! liste des fichiers : categorie CPS_CATEG_BULLA_SURE
  call cps_getListFichiersCateg(CPS_CATEG_BULLA_SURE, fichiers)
  nb_fic = size(fichiers)
  do i=1, nb_fic
     ! ouverture du fichier i de la categorie CPS_CATEG_BULLA_SURE
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
           call cpsi_undoMajBullA_sure(trim(fic), date)
        end if
     end do
  end do
    
  ! liberation memoire
  if (associated(fichiers)) then
     deallocate(fichiers)
  end if
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! traitements des predictions !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  ! liste des fichiers : categorie CPS_CATEG_BULLA_PRED
  call cps_getListFichiersCateg(CPS_CATEG_BULLA_PRED, fichiers)
  nb_fic = size(fichiers)
  do i=1, nb_fic
     modif = .false.
     ! ouverture du fichier i de la categorie CPS_CATEG_BULLA_PRED
     call cpsi_getAccesMadona(trim(fichiers(i)), acces)
     ! parcours des elements
     ret = acc_scan_reset(acces)
     do
        ret = acc_scan(acces, libelle, nature)
        if (nature.eq.0) then
           ! fin du fichier
           exit
        elseif (nature.eq.ACC_STRUCT)then
           ! lecture d'une structure correspondant a un fichier de donnee
           ! lecture de la date d'introduction dans la base
           ret = acc_getd(acces, trim(libelle)//".date", date_fic, "")
           ! on compare avec la date en entree
           res = cpsi_compareReels(date_fic, date)
           if (res.ge.0) then
              ! les données ont ete introduites a une date superieure
              ! on efface le fichier et la structure
              ret = acc_gets(acces, trim(libelle)//".fichier", fic)
              ! on rajoute le repertoire de la base de reference
              fic = trim(rep_base_ref)//"/"//trim(fic)
              ! suppression physique du fichier
              call system("/bin/rm -f "//trim(fic))
              ! suppression de la structure
              ret = acc_delete(acces, trim(libelle))
              modif = .true.
           end if
        end if
     end do
     if (modif) then
        ! le fichier a ete modifie : on enregistre les modifications
        call cps_ihm_ecrireFichier(acces, trim(fichiers(i)))
     end if
  end do
  
  ! liberation memoire
  if (associated(fichiers)) then
     deallocate(fichiers)
  end if

!999 continue

  ! fermeture COMPAS
  !call cps_close_utilisateur()

end subroutine cps_undoMajBullA


subroutine cpsi_undoMajBullA_sure(fic, date)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_undoMajBullA_sure
!
!$Resume
!
!$Description
!  V2-0
!  Routine interne. Elle efface les structures MADONA dans un fichier de
!  donnees sures qui ont ete introduites a une date superieure ou egale a
!  la date specifiee en argument.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_undoMajBullA_sure(fic, date)
!.    character(LEN=*) :: fic
!.    real(KIND=PM_REEL) :: date
!
!$Arguments
!>E     fic   :<LEN=*>     fichier de données
!>E     date  :<PM_REEL>   les données introduites après cette date sont à supprimer
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
  
  ! variables locales
  integer :: acces, ret, res
  real(KIND=PM_REEL) :: date_debut
  character(LEN=100) :: libelle
  integer :: nature
  logical :: modif
  
  ! initialisation
  modif = .false.

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
        ! on regarde la date d'introduction de la structure : date_debut
        ret = acc_getd(acces, trim(libelle)//".date_debut", date_debut, "")
        ! on compare a la date
        res = cpsi_compareReels(date_debut,date)
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

end subroutine cpsi_undoMajBullA_sure

