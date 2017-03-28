module ui_acsol2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ui_acsol2
!
!$Resume
!
!$Description
!  Module gerant la mise a jour automatique des donnees issues du fichier
!  ACSOL2 dans COMPAS.
!
!$Auteur
!  vpg
!
!$Version
!  $Id: ui_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_acsol2.F90,v $
!  Revision 1.7  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.6  2009/11/09 13:26:29  cmartel
!  DM-ID 842 : Ajout de routine de lecture ACTSOL et écriture ACSOL2
!
!  Revision 1.5  2009/03/24 10:19:05  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!  Revision 1.4  2009/03/24 08:59:03  cml
!  DM-ID 1159 : Ajout d'initialisations de pointeurs manquantes
!  Revision 1.3  2008/11/04 10:22:01  cml
!  AQ : Ajout de mots clefs manquants pour les read, renommage de la variable unit
!  Revision 1.2  2008/04/11 12:02:22  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches
!  Revision 1.1  2008/02/08 17:51:19  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.7  2006/10/23 12:55:24  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.6.4.1  2006/10/23 10:11:34  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.6  2006/06/16 17:31:59  vivaresf
!  Cartouches d'entete
!  Revision 1.5  2006/06/14 12:38:48  vivaresf
!  Suppression code mort
!  lfn alloues par cps_file_unit
!  Revision 1.4  2006/05/12 11:40:25  vpg
!  Remplissage des cartouches
!  Revision 1.3  2006/04/06 16:05:19  vpg
!  Correction de la lecture de la date de prediction
!  Revision 1.2  2006/03/20 16:12:54  vpg
!  Mise a jour des cartouches
!
!$FinHistorique
!
!$Usage
!  use ui_acsol2
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cpsi_lire_actsol
!- cpsi_ecrireAcsol2
!
!$Fonctions
!- cps_majAcsol2
!- cpsi_majPartieSure
!- cpsi_majPartiePred
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cps_majAcsol2 cpsi_majPartieSure cpsi_majPartiePred cpsi_lire_actsol cpsi_ecrireAcsol2
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cps_util
  use cps_utilisateur
  use cps_acsol2
  
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ui_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $'

 
  
contains
  
 subroutine cpsi_lire_actsol (factsol, tab_lignes, nb_lignes)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lire_actsol
!
!$Resume
!	Lecture des coefficients de flux et d'indice géomagnétique d'un fichier
!
!$Description
!   Lecture des coefficients de flux et d'indice géomagnétique d'un fichier
!
!$Auteur
!   Cédric MARTEL (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_lire_actsol (factsol, tab_lignes, nb_lignes)
!.    character(LEN=*) :: factsol
!.    integer :: nb_lignes
!.    type(ligne_acsol2), dimension(:), pointer :: tab_lignes
!
!$Arguments
!>E     factsol     :<LEN=*>                          fichier contenant les valeurs    
!>E/S   tab_lignes  :<ligne_acsol2,DIM=(:),pointer>   tableau des lignes de données
!>S     nb_lignes   :<integer>                        nombre de lignes dans le tableau
!
!$Common
!
!$Routines
!- cps_file_unit
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! LIRE ACTIVITE SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   implicit none

   ! Entrées/sorties
   character(LEN=*), intent(in) :: factsol
   integer, intent(out) :: nb_lignes
   type(ligne_acsol2), dimension(:), pointer :: tab_lignes
   
   ! Constantes
   character(LEN=3),parameter :: FORMAT_LIGNE='(a)'
   character(LEN=19),parameter :: FORMAT_LIGNE_DONNEES='(i5,3(f5.1),18(i3))'

   ! Variables locales
   character(LEN=CPS_MAXLG) :: buffer
   integer :: ii,unit_read,ierr

   ! Initialisation
   nullify(tab_lignes)
   nb_lignes = 0

   ! Lecture du fichier pour connaitre le nombre de lignes
   call cps_file_unit(unit_read,ierr)
   if(ierr.lt.0)then
      call MSP_signaler_message (cle_mes="CPS_ERR_LIRE_ACSOL2", &
         routine="cps_lireAcsol2_Brut")
      return
   endif
   
   open(unit=unit_read,file=factsol,status='old',&
     form='formatted', iostat=ierr)
   if ( ierr /= 0 ) then
      call MSP_signaler_message (cle_mes="CPS_ERR_LIRE_ACSOL2", &
         routine="cps_lireAcsol2_Brut")
      return
   endif    

   ! Lecture de toutes les lignes pour 
   ierr = 0
   ii = 0
   boucle : do while ( ierr == 0 )
      ! Lecture brute de la ligne
      read(unit_read,fmt=FORMAT_LIGNE,IOSTAT=ierr) buffer
      ! Suppression des lignes blanches
      if ( ierr == 0 .and. len_trim(buffer) /= 0 ) then
         ! Analyse de la ligne
         read(buffer,fmt=FORMAT_LIGNE_DONNEES, IOSTAT=ierr)

         ! Problème à la lecture ou fin de fichier
         if ( ierr > 0 ) then
            call MSP_signaler_message (cle_mes="CPS_ERR_LECT_ACTSOL")
            return
         else if ( ierr < 0 ) then
            exit boucle
         endif
      
         ii = ii + 1
      endif
   enddo boucle
   nb_lignes = ii
   close(unit_read)

   ! Allocation memoire
   allocate(tab_lignes(nb_lignes))
   
   ! Relectyure des lignes et remplissage des structures.
   open(unit=unit_read,file=factsol,status='old',form='formatted')
   do ii=1, nb_lignes
      ! Lecture brute de la ligne
      read(unit_read,fmt=FORMAT_LIGNE,IOSTAT=ierr) buffer
      ! Suppression des lignes blanches
      if ( ierr == 0 .and. len_trim(buffer) /= 0 ) then
      
         ! Lecture de la ligne
         read(buffer,fmt=FORMAT_LIGNE_DONNEES,IOSTAT=ierr) &
            tab_lignes(ii)%julsol, &
            tab_lignes(ii)%flux, tab_lignes(ii)%fluxm, tab_lignes(ii)%fluxa,&
            tab_lignes(ii)%iaamoy, tab_lignes(ii)%iaa(1:8), &
            tab_lignes(ii)%iapmoy, tab_lignes(ii)%iap(1:8)

         ! Il s'agit par défaut de valeur observées
         tab_lignes(ii)%indflu = 3
         ! Init des autres champs
         tab_lignes(ii)%fluxpr = 0.0_pm_reel
         tab_lignes(ii)%fluxmp = 0.0_pm_reel
         tab_lignes(ii)%indgeo = 0
         tab_lignes(ii)%iaampr = 0
         tab_lignes(ii)%iaapr(1:8) = 0
         tab_lignes(ii)%tmp(1:6) = 0

         ! En cas d'erreur on affiche un warning 
         ! et on renvoie le nombre partiel de lignes
         if ( ierr > 0 ) then
            call MSP_signaler_message (cle_mes="CPS_ERR_LECT_ACTSOL_001")
            nb_lignes = ii-1
            return
         endif
      endif         
   enddo
   
   close(unit_read)

   return
      
end subroutine cpsi_lire_actsol


subroutine cpsi_ecrireAcsol2(facsol2, tab_lignes, nb_lignes)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ecrireAcsol2
!
!$Resume
!  Ecriture d'un fichier ACSOL2
!
!$Description
!  Ecriture d'un fichier ACSOL2
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ecrireAcsol2(facsol2, tab_lignes, nb_lignes)
!.    character(LEN=*) :: facsol2
!.    integer :: nb_lignes
!.    type(ligne_acsol2), dimension(:), pointer :: tab_lignes
!
!$Arguments
!>E     facsol2     :<LEN=*>                         nom du fichier écrit
!>E/S   tab_lignes  :<ligne_acsol2,DIM=(:),pointer>  tableau contenant les données
!>E     nb_lignes   :<integer>                       nombre de lignes du tableau
!
!$Common
!
!$Routines
!- cps_file_unit
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- cps_accesMadona
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

   ! Variables locales
   use cps_accesMadona
   use cps_util


   implicit none

   ! Entrées/sorties
   character(LEN=*), intent(in) :: facsol2
   integer, intent(in) :: nb_lignes
   type(ligne_acsol2), dimension(:), pointer :: tab_lignes
    
   ! resultat
   integer :: ierr
   
   ! Variables locales
    integer :: ii, ier
    integer :: unit_write
    character(len=38), parameter :: FORMAT_LIGNE &
         = '(i5,2x,i1,5(1x,F6.2),2x,i1,33(1x,i3))'
    ! Format d'une ligne de commentaire
    character(len=38), parameter :: FORMAT_LIGNE_CMT = '(a,a)'

    ! Initialisation
    ierr = 0
    
    ! Numero d'unite disponible
    call cps_file_unit(unit_write,ier)
    if(ier.lt.0)then
       call MSP_signaler_message (cle_mes="CPS_ERR_LIRE_ACSOL2", &
            routine="cps_lireAcsol2_Brut")
       return
    endif
    
    ! TODO test de vérification de présence du fichier

    open(unit=unit_write,file=facsol2,status='new',&
         form='formatted', iostat=ierr)
    if ( ierr /= 0 ) then
       call MSP_signaler_message (cle_mes="CPS_ERR_LIRE_ACSOL2", &
            routine="cps_lireAcsol2_Brut")
       return
    endif       
    
    ! Lecture des lignes de commentaires
    ! ces lignes ne sont pas exploitées
    write(unit=unit_write, fmt=FORMAT_LIGNE_CMT) &
"    0**********************************************************************************************",&
"*******************************************************************************************"      
    write(unit=unit_write, fmt=FORMAT_LIGNE_CMT) &
"    0 DE 18000 A 18000 : PAS DE FLUX PRED., FLUX OBS. = FLUX AJUST.                                ",&
"                                                                                           "     
    write(unit=unit_write, fmt=FORMAT_LIGNE_CMT) &
"    0 DE 18000 A 18000 : FLUX PRED. = FLUX OBS. = FLUX AJUST.                                      ",&
"                                                                                           "    
    write(unit=unit_write, fmt=FORMAT_LIGNE_CMT) &
"    0 DE 12078 A ..... : PAS DE FLUX AJUST.                                                        ",&
"                                                                                           "    
    write(unit=unit_write, fmt=FORMAT_LIGNE_CMT) &
"    0 FLUX-OBS: 20493   AA: 20493   AP: 20484   PAA: 20501                                         ",&
"                                                                                           "   
    write(unit=unit_write, fmt=FORMAT_LIGNE_CMT) &
"    0     FLX O  FLX A  FLX M  FLP    FLP M     AAM  ***** COEFFICIENTS (AA) *****  APM  ***** COEF",&
"FICIENTS (AP) *****  AAPM * COEFFICIENTS (AA PREDITS) *                                    " 
    write(unit=unit_write, fmt=FORMAT_LIGNE_CMT) &
"    0----------------------------------------------------------------------------------------------",&
"-------------------------------------------------------------------------------------------"
   
    
    ! Détermination du nombre de lignes
    ierr = 0
    do ii = 1, nb_lignes
       ! On écrit chacune des lignes de données
       write(unit=unit_write, fmt=FORMAT_LIGNE) tab_lignes(ii)%julsol, &
            tab_lignes(ii)%indflu,tab_lignes(ii)%flux,tab_lignes(ii)%fluxa,           &
            tab_lignes(ii)%fluxm,tab_lignes(ii)%fluxpr,tab_lignes(ii)%fluxmp,         &
            tab_lignes(ii)%indgeo,tab_lignes(ii)%iaamoy, tab_lignes(ii)%iaa(1:8),     &
            tab_lignes(ii)%iapmoy, tab_lignes(ii)%iap(1:8),                      &
            tab_lignes(ii)%iaampr,tab_lignes(ii)%iaapr(1:8),tab_lignes(ii)%tmp(1:6)
       if ( ierr /= 0 ) then
          call MSP_signaler_message (cle_mes="CPS_ERR_ECRIRE_ACSOL2", &
             routine="cpsi_ecrireAcsol2")
          close (unit_write)
          return
       endif       
    end do

    ! Fermerture du fichiers
    close (unit_write)
    

end subroutine cpsi_ecrireAcsol2

  
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Fonction de mise a jour du fichier ACSOL2 dans la base COMPAS      !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function cps_majAcsol2(date_du_jour, fichier_brut) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_majAcsol2
!
!$Resume
!
!$Description
!  Fonction de mise a jour du fichier ACSOL2 dans la base COMPAS.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_majAcsol2(date_du_jour, fichier_brut)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    character(LEN=*) :: fichier_brut
!.    integer :: ok
!
!$Arguments
!>E     date_du_jour  :<PM_REEL>   date de mise à jour (unité jj1950)
!>E     fichier_brut  :<LEN=*>     chemin complet du fichier ACSOL2 brut
!>S     ok            :<integer>   CPS_OK si la mise à jour s'est correctement effectuée, CPE_ERR_DEF sinon
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use cps_accesMadona

    implicit none
    ! arguments
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    character(LEN=*), intent(in) :: fichier_brut
    
    !resultat
    integer :: ok

    !variables locales
    integer :: nb_lignes
    type(ligne_acsol2), dimension(:), pointer :: data => NULL()
   
    ! initialisation
    ok = CPS_ERR_DEF
    
    ! lecture du fichier brut
    call cps_lireAcsol2(trim(fichier_brut), data, nb_lignes)
    if ( MSP_gen_messages("cps_majAcsol2")) return

   
    ! mise a jour de la partie sure du fichier (donnees observees)
    ok = cpsi_majPartieSure(date_du_jour, data(1:nb_lignes))
    ! mise a jour des predictions
    ok = cpsi_majPartiePred(date_du_jour, data(1:nb_lignes))
    
    ! liberation memoire
    if (associated(data)) then
       deallocate(data)
    end if

  end function cps_majAcsol2





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Mise a jour de la partie sure du fichier ACSOL2 correspondant aux  !!
!! donnees observees                                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function cpsi_majPartieSure(date_du_jour, data) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_majPartieSure
!
!$Resume
!
!$Description
!  Mise a jour de la partie sure du fichier ACSOL2 correspondant aux
!  donnees observees.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cpsi_majPartieSure(date_du_jour, data)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    type(ligne_acsol2), dimension(:) :: data
!.    integer :: ok
!
!$Arguments
!>E     date_du_jour  :<PM_REEL>                date de mise à jour (unité jj1950)
!>E     data          :<ligne_acsol2,DIM=(:)>   lignes du fichier ACSOL2 brut
!>S     ok            :<integer>                CPS_OK si la mise à jour s'est correctement effectuée, CPE_ERR_DEF sinon
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cps_accesMadona

    implicit none


    ! arguments
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    type(ligne_acsol2), dimension(:), intent(in) :: data
    
    ! resultat
    integer :: ok

    ! variables locales
    real(KIND=PM_REEL) :: derniere_date, date
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    integer :: nb_fic, i, acces_fic, nature, acces, nb_lignes, res
    integer :: ind_debut, trouve, ret
    character(LEN=256) :: libelle, fichier, cle_fic, cle
    
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_REEL) :: sec
    character(LEN=4) :: buff_mm, buff_aaaa
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    
    ! fonction utilisee
    integer :: cps_ihm_creerFichier
    
    ! initialisation
    cle = ""
    ok = CPS_ERR_DEF
    
    ! recuperer la derniere date correspondant a une observation
    ! on commence par prendre le fichier le plus recent
    call cps_getListFichiersCateg("acsol2_sure", fichiers)
    nb_fic = size(fichiers)
    ! on prend le dernier (les fichiers sont classes par ordre chronologique)
    call cpsi_getAccesMadona(trim(fichiers(nb_fic)), acces_fic)
    if (acces_fic.lt.0) then
       ! erreur d'ouverture
       return
    end if
    ret = acc_scan_reset(acces_fic)
    do
       ret = acc_scan(acces_fic, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       else if (nature.eq.ACC_STRUCT) then
          ret = acc_gets(acces_fic, trim(libelle)//".fichier", fichier)
          fichier = trim(rep_base_ref)//"/"//trim(fichier)
          ok = CPS_OK
       end if
    end do

    if (ok.eq.CPS_OK) then
       ! 'fichier' correspond au fichier le plus recent
       ! la derniere date est la derniere de ce fichier
       call cpsi_getAccesMadona(trim(fichier), acces)
       if (acces.lt.0) then
          ! erreur d'ouverture
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.0) then
             ! fin du fichier
             exit
          else if (nature.eq.ACC_STRUCT) then
             ret = acc_getd(acces, trim(libelle)//".date", derniere_date, "")
          end if
       end do
    else
       derniere_date = 0.0
    end if
    
    ok = CPS_ERR_DEF

    ! recuperer la date a partir de laquelle on enregistre dans COMPAS
    ! les nouvelles observations
    nb_lignes = size(data)
    ! on initialise 'ind_debut' de façon a ce que si aucune date posterieure
    ! a 'derniere_date' n'est trouvee, on ne fait pas de mise a jour (cf. boucle
    ! des enregistrements des nouvelles observations). Un tel cas se presente lorsque
    ! le fichier brut n'a pas changé.
    res = cpsi_compareReels(derniere_date, dble(0))
    if (res.eq.0) then
       ind_debut = 1
    else
       ind_debut = nb_lignes+1
       do i=1, nb_lignes
          res = cpsi_compareReels(derniere_date, dble(data(i)%julsol))
          if (res.eq.-1) then
             ! la date 'data(i)%julsol' est plus posterieure a 'derniere_date'
             ind_debut = i
             exit
          end if
       end do
    end if
    
    ! enregistrement des nouvelles observations dans COMPAS
    
    do i=ind_debut, nb_lignes
       if (data(i)%indflu.eq.3) then
          date = dble(data(i)%julsol)
          ! 
          call md_jourfrac_joursec(date, jour_sec, code_retour)
          call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
          write(buff_mm,10) mm
          if (buff_mm(1:1).eq.' ') then
             buff_mm(1:1) = '0'
          end if
          write(buff_aaaa,20) aaaa
          
          ! on recupere le fichier où ecrire les donnees courantes
          trouve = cps_getFichierAcsol2Sure(date, acces, fichier)
          
          if (trouve.ne.CPS_OK) then
             ! le fichier n'existe pas : on le cree
             fichier = "Activite_solaire/acsol2_sure_"//trim(buff_mm)//"_"//trim(buff_aaaa)
             ! on le reference
             cle_fic = "mois_"//trim(buff_mm)//"_annee_"//trim(buff_aaaa)
             ret = acc_create(acces_fic, trim(cle_fic), ACC_STRUCT, "")
             ret = acc_puti(acces_fic, trim(cle_fic)//".mois", mm)
             ret = acc_puti(acces_fic, trim(cle_fic)//".annee", aaaa)
             ret = acc_puts(acces_fic, trim(cle_fic)//".fichier", trim(fichier))
             ! enregistrement
             call cps_ihm_ecrireFichier(acces_fic, trim(fichiers(nb_fic)))
             
             fichier = trim(rep_base_ref)//"/"//trim(fichier)
             ! creation du fichier
             ok = cps_ihm_creerFichier(trim(fichier))
             ! ouverture de l'acces MADONA sur ce dernier
             call cpsi_getAccesMadona(trim(fichier), acces)
          end if
       
          ! ecriture des donnees dans COMPAS
          cle = cpsi_getCleAcsol2(date)

          ret = acc_exist(acces, trim(cle))
          if (ret.ne.1) then
             ret = acc_create(acces, trim(cle), ACC_STRUCT, "")
          end if
          ! selection de la structure
          ret = acc_select(acces, trim(cle), ACC_STRUCT)
          
          ret = acc_putd(acces, "date", dble(data(i)%julsol), "")
          
          ret = acc_putd(acces, "flux", data(i)%flux, "sfu")
          ret = acc_putd(acces, "fluxm", data(i)%fluxm, "sfu")
          
          ret = acc_puti(acces, "iaamoy", data(i)%iaamoy)
          ret = acc_puti(acces, "iaa_1", data(i)%iaa(1))
          ret = acc_puti(acces, "iaa_2", data(i)%iaa(2))
          ret = acc_puti(acces, "iaa_3", data(i)%iaa(3))
          ret = acc_puti(acces, "iaa_4", data(i)%iaa(4))
          ret = acc_puti(acces, "iaa_5", data(i)%iaa(5))
          ret = acc_puti(acces, "iaa_6", data(i)%iaa(6))
          ret = acc_puti(acces, "iaa_7", data(i)%iaa(7))
          ret = acc_puti(acces, "iaa_8", data(i)%iaa(8))
          ret = acc_puti(acces, "iapmoy", data(i)%iapmoy)

          ret = acc_putd(acces, "date_jour", date_du_jour, "")

          ! fin de la selection
          ret = acc_select_end(acces)
          
          ! enregistrement
          call cps_ihm_ecrireFichier(acces, trim(fichier))
          
          ok = CPS_OK
       end if
       
    end do
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers)
    end if


10  format (i2)
20  format (i4)
    
  end function cpsi_majPartieSure





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      Mise a jour des predictions du fichier ACSOL2 dans COMPAS     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function cpsi_majPartiePred(date_du_jour, data) result (ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_majPartiePred
!
!$Resume
!
!$Description
!  Mise a jour des predictions du fichier ACSOL2 dans COMPAS.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cpsi_majPartiePred(date_du_jour, data)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    type(ligne_acsol2), dimension(:) :: data
!.    integer :: ok
!
!$Arguments
!>E     date_du_jour  :<PM_REEL>                date de mise à jour (unité jj1950)
!>E     data          :<ligne_acsol2,DIM=(:)>   lignes du fichier ACSOL2 brut
!>S     ok            :<integer>                CPS_OK si la mise à jour s'est correctement effectuée, CPE_ERR_DEF sinon
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
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    type(ligne_acsol2), dimension(:), intent(in) :: data

    ! resultat
    integer :: ok
    
    ! variables locales
    integer :: nb_lignes, i, res, acces, trouve, acces_fic, nb_fic, ret
    character(LEN=256) :: fichier, str_date_du_jour, cle, cle_fic
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
 
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_REEL) :: sec
    character(LEN=4) :: buff_jj, buff_mm, buff_aaaa
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    
    ! fonction utilisee
    integer :: cps_ihm_creerFichier
    
    ! initialisation
    ok = CPS_ERR_DEF

    nb_lignes = size(data)
    str_date_du_jour = ""
    
    call md_jourfrac_joursec(date_du_jour, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
    write(buff_jj,10)  jj
    if (buff_jj(1:1).eq.' ') then
       buff_jj(1:1) = '0'
    end if
    write(buff_mm,10)  mm
    if (buff_mm(1:1).eq.' ') then
       buff_mm(1:1) = '0'
    end if
    write(buff_aaaa,20)  aaaa
    str_date_du_jour = trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)



    ! mettre a jour les predictions
    do i=1, nb_lignes
       res = cpsi_compareReels(dble(data(i)%julsol), date_du_jour)
       ! on ne s'interesse qu'aux predictions posterieures a la date du jour
       if ((data(i)%indflu.eq.2).and.(res.eq.1)) then
          trouve = cps_getFichierAcsol2Pred(dble(data(i)%julsol), acces, fichier)
          
          call md_jourfrac_joursec(dble(data(i)%julsol), jour_sec, code_retour)
          call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
          write(buff_jj,10)  jj
          if (buff_jj(1:1).eq.' ') then
             buff_jj(1:1) = '0'
          end if
          write(buff_mm,10)  mm
          if (buff_mm(1:1).eq.' ') then
             buff_mm(1:1) = '0'
          end if
          write(buff_aaaa,20)  aaaa
          
          if (trouve.ne.CPS_OK) then
             ! le fichier n'existe pas : il faut le creer
             fichier = "Activite_solaire/acsol2_pred_"//trim(buff_mm)//"_"//trim(buff_aaaa)
             ! on le reference
             call cps_getListFichiersCateg("acsol2_pred", fichiers)
             nb_fic = size(fichiers)
             call cpsi_getAccesMadona(trim(fichiers(nb_fic)), acces_fic)
             if (acces_fic.lt.0) then
                ! erreur d'ouverture du fichier
                return
             end if
             cle_fic = "mois_"//trim(buff_mm)//"_annee_"//trim(buff_aaaa)
             ret = acc_create(acces_fic, trim(cle_fic), ACC_STRUCT, "")
             ret = acc_puti(acces_fic, trim(cle_fic)//".mois", mm)
             ret = acc_puti(acces_fic, trim(cle_fic)//".annee", aaaa)
             ret = acc_puts(acces_fic, trim(cle_fic)//".fichier", trim(fichier))
             ! enregistrement
             call cps_ihm_ecrireFichier(acces_fic, trim(fichiers(nb_fic)))
             
             fichier = trim(rep_base_ref)//"/"//trim(fichier)
             ! creation du fichier
             ret = cps_ihm_creerFichier(trim(fichier))
             ! acces Madona
             call cpsi_getAccesMadona(trim(fichier), acces)
             ! liberation memoire
             if (associated(fichiers)) then
                deallocate(fichiers)
             end if
          end if
          
          ! mise a jour des predictions
          cle = cpsi_getCleAcsol2(dble(data(i)%julsol))
          cle = trim(cle)//"_"//trim(str_date_du_jour)
          
          ! creation de la structure
          ret = acc_create(acces, trim(cle), ACC_STRUCT, "")
          ! selection de la structure
          ret = acc_select(acces, trim(cle), ACC_STRUCT)
          
          ret = acc_putd(acces, "date", dble(data(i)%julsol), "")
          ret = acc_putd(acces, "date_pred", date_du_jour, "")
          
          ret = acc_putd(acces, "fluxpr", data(i)%fluxpr, "sfu")
          ret = acc_putd(acces, "fluxmp", data(i)%fluxmp, "sfu")
          
          ret = acc_puti(acces, "iaampr", data(i)%iaampr)
          ret = acc_puti(acces, "iaapr_1", data(i)%iaapr(1))
          ret = acc_puti(acces, "iaapr_2", data(i)%iaapr(2))
          ret = acc_puti(acces, "iaapr_3", data(i)%iaapr(3))
          ret = acc_puti(acces, "iaapr_4", data(i)%iaapr(4))
          ret = acc_puti(acces, "iaapr_5", data(i)%iaapr(5))
          ret = acc_puti(acces, "iaapr_6", data(i)%iaapr(6))
          ret = acc_puti(acces, "iaapr_7", data(i)%iaapr(7))
          ret = acc_puti(acces, "iaapr_8", data(i)%iaapr(8))

          ! fin de la selection
          ret = acc_select_end(acces)

          ! enregistrement
          call cps_ihm_ecrireFichier(acces, trim(fichier))
          
          ok = CPS_OK
       end if
    end do


    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers)
    end if

10  format(i2)
20  format(i4)

  end function cpsi_majPartiePred



end module ui_acsol2
