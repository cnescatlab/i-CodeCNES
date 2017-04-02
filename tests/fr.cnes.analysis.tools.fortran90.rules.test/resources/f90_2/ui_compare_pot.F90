program ui_compare_pot

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_compare_pot
!
!$Resume
!  Comparaison de fichiers de potentiels
!
!$Description
!  La comparaison peut être entre deux fichiers de potentiel au format GRGS,
!  ou entre un fichier de potentiel au format  GRGS et un fichier de potentiel
!  au format MSDON.
!  On peut comparer des potentiels dont les coefficients n'ont pas le même degré
!  ou ordre maximal.
!
!
! Usage
!.  ui_compare_pot [fic1 | -msdon ficMSDON] fic2 
!>[E]  fic1       : <LEN=256>     nom du fichier à comparer au format GRGS
!>[E]  ficMSDON   : <LEN=256>     nom du fichier à comparer au format MSDON  
!>E    fic2       : <LEN=256>     nom du fichier de référence au format GRGS
!
!$Auteur
! Marie LARROQUE (ATOS ORIGIN)  
!
!$Version
!  $Id: ui_compare_pot.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_compare_pot.F90,v $
!  Revision 1.9  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.8  2009/03/24 10:18:52  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!
!  Revision 1.7  2009/03/24 09:00:02  cml
!  DM-ID 1159 : Ajout d'initialisations de pointeurs manquantes
!
!  Revision 1.6  2008/10/31 13:08:37  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.5  2008/04/28 13:01:27  vivaresf
!  FA-ID 664 : présentation des sorties et cartouches
!
!  Revision 1.4  2008/04/11 10:57:54  vivaresf
!  FA-ID 778 : suppression des variables inutilisées
!
!  Revision 1.3  2008/04/11 10:09:33  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!
!  Revision 1.2  2008/04/10 10:23:10  vivaresf
!  AQ version 2.4 :  mise à jour des cartouches
!
!  Revision 1.1  2008/02/08 17:51:36  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.3  2006/11/20 18:03:02  vivaresf
!  Version 2.1 / DM 462 : fonction de gestion des potentiels dans COMPAS_UI
!  Revision 1.2  2006/11/02 12:42:11  mle
!  passage understand
!  Revision 1.1  2006/10/06 08:58:50  mle
!  DM-462 : version finale
!  Revision 1.3  2006/09/26 07:42:32  mle
!  DM-ID : 462 petites modifications
!  Revision 1.2  2006/09/25 14:05:27  mle
!  Relecture des cartouches
!  DM-ID 462. Nouveaux utilitaires pour les potentiels. Version initiale
!
!$FinHistorique
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
  use msp_gestion_erreur
  use ui_io
  use cps_utilisateur
  use ui_potentiel_fct


  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_compare_pot.F90 69 2012-09-11 08:33:34Z ffsm $'


  ! Entrees/sorties
  
  character(LEN=256) :: ficMSDON
  character(LEN=256) :: fic1, fic2

  ! Variable locale
  character(LEN=256), dimension(50) :: l_opt
  integer :: ierfin, noptions
  real(KIND=PM_REEL), dimension(:), pointer :: zb => NULL()
  real(KIND=PM_REEL), dimension(:,:), pointer :: clmb => NULL()
  real(KIND=PM_REEL), dimension(:,:), pointer :: slmb => NULL()
  real(KIND=PM_REEL), dimension(:,:), pointer :: C1 => NULL()
  real(KIND=PM_REEL), dimension(:,:), pointer :: S1 => NULL()
  real(KIND=PM_REEL), dimension(:,:), pointer :: C2 => NULL()
  real(KIND=PM_REEL), dimension(:,:), pointer :: S2 => NULL()
  real(KIND=pm_reel) :: mu1, requa1, apla1, vrot1, mu2, &
		requa2, apla2, vrot2 
  integer :: degmax1, ordremax1, degmax2, ordremax2
 
  logical :: opt_msdon,err

  real(KIND=PM_REEL) :: jdeb
  logical :: denorm

  character(LEN=256) :: dirfcf
  integer :: lch_dirfcf
  type(MSP_MESSAGE) :: messages
  integer :: nvar, iargc, ier

  ! Lecture des paramètres

  ! initialisation
  !---------------
  ier = AMv_rc_get ('compas_ui_fcf','compas','','dirfcf',dirfcf, &
       lch_dirfcf)
  call MSP_ajouter_fichier_message (trim(dirfcf)//"/CPSUI_erreur_fr")
  call cps_init_utilisateur()
  if (MSP_gen_messages("ui_compare_pot")) goto 999

  opt_msdon = .false.
  
  jdeb = 0._pm_reel
  denorm = .false.
  vrot2 = 0._pm_reel

  ! analyse des arguments
  !----------------------

  ! aide
  ! lecture des arguments
  call ui_lire_options(noptions, l_opt)
  call ui_test_arguments ("-h", "", ierfin)
  nvar=iargc()

  ! -h ou pas de parametres
  if(ierfin.eq.1.or.nvar==0) then
     call ui_ecrire_help("ui_compare_pot")
     goto 999
  endif

  ! decodage
  call cpsi_lectureOptFic(noptions,l_opt,fic1,fic2,opt_msdon,ficMSDON,err)
  if(err) goto 999

  ! Comparaison des coefficients
  !-----------------------------

  ! on récupère les coefficients du fichier au format GRGS
  call cps_lirePotentielGRGS(fic1, jdeb, denorm, C1, S1, requa1,&
 			apla1, mu1, vrot1, degmax1, ordremax1)  
  if (MSP_gen_messages("ui_compare_pot")) goto 999
  if (MSP_PROBLEME) then
	call MSP_signaler_message(cle_mes="CPS_ERR_GRGS", partie_variable=trim(fic1))
	goto 999
  endif 
  !si le premier fichier a pu etre lu, 
  !on récupère les coeff. du second fichier
  if (opt_msdon) then
     !le ficchier est au format MSDON
     call cps_lirePotentielMSDON(ficMSDON, zb, clmb, slmb, mu2, &
          requa2, apla2, degmax2, ordremax2)
     apla2=1._pm_reel/apla2
     if (MSP_gen_messages("ui_compare_pot")) goto 999
     if (MSP_PROBLEME) then
        call MSP_signaler_message(cle_mes="CPS_ERR_MSDON", partie_variable=trim(ficMSDON))
        goto 999
     endif
     !recuperation de C et S correspondants
     allocate(C2(0:degmax2, 0:ordremax2))
     allocate(S2(0:degmax2, 0:ordremax2))
     call cpsi_conversionCS(clmb,slmb,ordremax2,degmax2,zb,C2,S2)
     fic2 = ficMSDON
  else
     !le ficchier est au format GRGS
     call cps_lirePotentielGRGS(fic2, jdeb, denorm, C2, S2, requa2,&
          apla2, mu2, vrot2, degmax2, ordremax2)
     if (MSP_gen_messages("ui_compas_pot")) then 
        call MSP_signaler_message(cle_mes="CPS_ERR_GRGS", partie_variable=trim(fic2))
        goto 999
     endif
  endif
  !si le deuxieme fichier a pu etre lu, on compare les coeff.
  call cps_comparePotentiels(C1, S1, mu1, requa1, apla1, vrot1, degmax1, ordremax1,&
       C2, S2, requa2, apla2, mu2, vrot2, degmax2, ordremax2, fic1, fic2)
  
  if (MSP_gen_messages("ui_compare_pot")) goto 999

999 continue
  
  ! affichage des erreurs
  if (MSP_PROBLEME) then
     call MSP_recuperer_message(message=messages, nb_mes=MSP_tous_messages)
     call MSP_afficher_message(message=messages, unit=0)
  endif
  
  ! fin
  call cps_close_utilisateur()

  ! liberation memoire
  if (associated(zb)) then
     deallocate(zb)
  end if
  if (associated(clmb)) then
     deallocate(clmb)
  end if
  if (associated(slmb)) then
     deallocate(slmb)
  end if
 if (associated(C1)) then
    deallocate(C1)
  end if
  if (associated(S1)) then
     deallocate(S1)
  end if
 if (associated(C2)) then
    deallocate(C2)
  end if
  if (associated(S2)) then
     deallocate(S2)
  end if


contains

  subroutine cpsi_lectureOptFic(noptions,l_opt,fic1,fic2,opt_msdon,ficMSDON,err)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lectureOptFic
!
!$Resume
!  décodage des options et paramètres d'entrée
!
!$Description
!  décodage des options 
!  détermination des formats de fichiers de potentiels d'entree
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_lectureOptFic(noptions,l_opt,fic1,fic2,opt_msdon,ficMSDON,err)
!.    character(len=256) :: fic1,fic2,ficMSDON
!.    logical :: opt_msdon
!.    character(LEN=256), dimension(50) :: l_opt
!.    integer :: noptions
!.    logical :: err
!
!$Arguments
!>E     noptions   :<integer>            nombre d'options
!>E     l_opt      :<LEN=256,DIM=(50)>   tableau des options
!>S     fic1       :<LEN=256>            fichier de ref.
!>S     fic2       :<LEN=256>            fichier à comparer (si format GRGS)
!>S     opt_msdon  :<logical>            format MSDON
!>S     ficMSDON   :<LEN=256>            fichier à comparer (si format MSDON)
!>S     err        :<logical>            code d'erreur
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

    ! arguments
    character(LEN=256), dimension(50), intent(in) :: l_opt
    integer, intent(in)::noptions
    logical,intent(out)::opt_msdon
    character(len=256),intent(out)::fic1,fic2,ficMSDON
    logical, intent(out)::err

    err=.false.

    !les deux fichiers sont au format GRGS
    if (noptions == 0) then
       fic1=trim(l_opt(1))
       fic2=trim(l_opt(2))
    else if (noptions == 1) then
       ! un des deux fichiers est au format MSDON
       if (l_opt(1)=="-msdon") then
          opt_msdon = .true.
          ficMSDON = trim(l_opt(2))
          fic1 = trim(l_opt(3))
       else
          fic1 = trim(l_opt(1))
          if (trim(l_opt(2)) /= "-msdon") then
             ! erreur : l'option n'est pas bien place
             call MSP_signaler_message(cle_mes="CPS_ERR_OPT", &
                  partie_variable="ui_compare_pot")
             err=.true.
          end if
          ficMSDON = trim(l_opt(3))
          opt_msdon = .true.
       endif
    else
       ! erreur  
       call MSP_signaler_message(cle_mes="CPS_ERR_OPT", &
            partie_variable="ui_compare_pot")
       err=.true.
    endif
end subroutine cpsi_lectureOptFic

subroutine cpsi_conversionCS(clmb,slmb,ordremax2,degmax2,zb,C2,S2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_conversionCS
!
!$Resume
! Conversion des coeff. C et S. si format MSDON
!$Description
! passage de clmb et slmb provenant de la lecture d'un fichier de potentiel au 
! format MSDON a C et S
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_conversionCS(clmb,slmb,C2,S2,ordremax2,degmax2,zb)
!.    integer :: ordremax2,degmax2
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: clmb, slmb, C2, S2
!.    real(KIND=PM_REEL), dimension(:), pointer :: zb
!
!$Arguments
!>E     clmb       :<PM_REEL,DIM=(:,:),pointer>   clmb tableau des C MSDON
!>E     slmb       :<PM_REEL,DIM=(:,:),pointer>   clmb tableau des S MSDON
!>E     ordremax2  :<integer>                     ordre max
!>E     degmax2    :<integer>                     degré max
!>E     zb         :<PM_REEL,DIM=(:),pointer>     
!>S     C2         :<PM_REEL,DIM=(:,:),pointer>   tableau des C GRGS
!>S     S2         :<PM_REEL,DIM=(:,:),pointer>   tableau des S GRGS
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
  implicit none

  !arguments
  integer, intent(in)::ordremax2,degmax2
  real(KIND=PM_REEL), dimension(:,:), pointer :: clmb, slmb, C2, S2
  real(KIND=PM_REEL), dimension(:), pointer :: zb

  !variables locales
  integer::indl,indm

  C2(:,:) = 0._pm_reel
  S2(:,:) = 0._pm_reel
  indm = 0
  do indl	= 1, degmax2
     C2(indl,indm) = zb(indl)
  enddo
  do indm = 1, ordremax2
     do indl	= 1, degmax2
        C2(indl,indm) = clmb(indl, indm)
        S2(indl,indm) = slmb(indl, indm)
     enddo
  enddo
end subroutine cpsi_conversionCS


end program ui_compare_pot
