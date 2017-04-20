program ui_pot_grgs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_pot_grgs
!
!$Resume
!  Conversion des fichiers potentiels au format GRGS
!
!$Description
!  Conversion des fichiers potentiels au format GRGS
!
!
! Usage
!.  ui_pot_grgs -msdon fich_msdon [-corps code_corps] -grgs fich_grgs
!>E    fich_msdon   : <LEN=256>      nom du fichier au format MSDON à convertir 
!>[E]  code_corps   : <integer>      code du corps correspondant au fichier
!>S    fich_grgs    : <LEN=256>      nom du fichier au format GRGS correspondant
!
!$Auteur
!  Marie LARROQUE
!
!$Version
!  $Id: ui_pot_grgs.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_pot_grgs.F90,v $
!  Revision 1.9  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.8  2009/03/24 10:18:54  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!
!  Revision 1.7  2009/03/24 09:00:04  cml
!  DM-ID 1159 : Ajout d'initialisations de pointeurs manquantes
!
!  Revision 1.6  2008/10/31 13:08:57  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.5  2008/04/28 13:00:45  vivaresf
!  FA-ID 664 : présentation des sorties et cartouches
!
!  Revision 1.4  2008/04/11 10:57:56  vivaresf
!  FA-ID 778 : suppression des variables inutilisées
!
!  Revision 1.3  2008/04/10 13:30:52  vivaresf
!  Version 2.4, validation
!
!  Revision 1.2  2008/04/10 12:47:16  vivaresf
!  Version 2.4 AQ : vérification des cartouches,
!  rajout du cps_init_utilisateur dans tous les cas
!
!  Revision 1.1  2008/02/08 17:51:41  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.7  2007/09/18 12:51:06  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.6  2006/11/21 07:37:28  vivaresf
!  Version 2.1 : couverture de test
!  Revision 1.5  2006/11/20 18:39:17  vivaresf
!  Version 2-1 : couverture de test
!  Revision 1.4  2006/11/20 18:03:03  vivaresf
!  Version 2.1 / DM 462 : fonction de gestion des potentiels dans COMPAS_UI
!  Revision 1.3  2006/11/02 12:42:32  mle
!  passage understand
!  Revision 1.2  2006/10/18 11:07:27  vivaresf
!  Conversion
!  Revision 1.1  2006/10/06 08:58:50  mle
!  DM-462 : version finale
!  Revision 1.3  2006/09/26 07:42:31  mle
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
  character(len=256) :: SVN_VER =  '$Id: ui_pot_grgs.F90 69 2012-09-11 08:33:34Z ffsm $'

  
#include "formats.h"

  ! Entrees/sorties
  
  character(LEN=256) :: fich_MSDON
  integer :: code_corps
  character(LEN=256) :: fich_GRGS
  
  ! Variable locale
  character(LEN=256), dimension(50) :: l_opt
  integer :: ierfin, noptions, i
  real(KIND=PM_REEL), dimension(:), pointer :: zb => NULL()
  real(KIND=PM_REEL), dimension(:,:), pointer :: clmb => NULL(), slmb => NULL()
  real(KIND=pm_reel) :: mu,requa,apla,vrot
  integer :: degmax, ordremax
  
  logical :: opt_msdon, opt_grgs
  
  real(KIND=PM_REEL) :: jdeb
  logical :: denorm
  real(KIND=PM_REEL), dimension(:,:),pointer :: C => NULL(), S => NULL()
  
  type(MSP_MESSAGE) :: messages

  character(LEN=256) :: dirfcf
  integer :: lch_dirfcf, ier

  ! Lecture des paramètre
  
  ! initialisation
  !---------------
  call cps_init_utilisateur()

  ier = AMv_rc_get ('compas_ui_fcf','compas','','../fcf',dirfcf, lch_dirfcf)
  call MSP_ajouter_fichier_message (trim(dirfcf)//"/CPSUI_erreur_fr")
  if (MSP_gen_messages("ui_pot_grgs")) goto 999

  opt_msdon = .false.
  opt_grgs = .false.
  ! par defaut, le corps est la Terre, code 399
  code_corps = 399
  
  jdeb = 0._pm_reel
  denorm = .false.
  
  ! analyse des arguments
  !-----------------------
  
  ! aide
  ! lecture des arguments
  call ui_lire_options(noptions, l_opt)
  call ui_test_arguments ("-h", "", ierfin)
  
  ! -h ou pas de parametres
  if(ierfin.eq.1.or.noptions == 0) then
     call ui_ecrire_help("ui_pot_grgs")
     goto 999
  endif
  
  ! decodage
  do i=1, noptions
     select case (l_opt(2*i-1))
     case ("-msdon")
	opt_msdon = .true.
       	fich_MSDON = trim(l_opt(2*i))
        
     case ("-grgs")
	opt_grgs = .true.
  	fich_GRGS = trim(l_opt(2*i))
        
     case ("-corps")
        read(trim(l_opt(2*i)),*) code_corps
        
     end select
  end do
  
  ! Vérification des données obligatoires.
  !---------------------------------------
  if (.not.opt_msdon) then
     ! erreur : il faut que le nom du fichier au format MSDON soit donné
     call msp_signaler_message(cle_mes="CPS_ERR_ARG", partie_variable="ui_pot_grgs")
     goto 999
  endif
  
  if(.not.opt_grgs) then
     ! erreur : il faut que le nom du fichier au format GRGS soit donné
     call msp_signaler_message(cle_mes="CPS_ERR_ARG", partie_variable="ui_pot_grgs")
     goto 999
  endif
  
  
  ! Conversion du fichier
  !----------------------
  
  ! on verifie que le fichier n'est pas deja au format GRGS
  ! si on peut lire le fichier avec la routine cps_lirePotentielGRGS c'est 
  ! que le fichier est deja au format GRGS
  
  call cps_lirePotentielGRGS(fich_MSDON, jdeb, denorm, C, S, requa, apla, &
       mu, vrot, degmax, ordremax)  
  
  if (MSP_gen_messages("ui_pot_grgs")) then
     call MSP_annuler_probleme()
     call MSP_effacer_message(MSP_tous_messages)

     !lecture du fichier au format MSDON
     call cps_lirePotentielMSDON(fich_MSDON, zb, clmb, slmb, mu, requa, &
          apla, degmax, ordremax)
     !ecriture du fichier au format GRGS
     call cps_ecrirePotentielGRGS(fich_GRGS, code_corps, zb, clmb, slmb, &
          mu, requa, apla, degmax, ordremax, fich_MSDON)
  else
     call MSP_annuler_probleme()
     call MSP_effacer_message(MSP_tous_messages)
     call MSP_signaler_message (cle_mes="CPS_ERR_MSDON", partie_variable=trim(fich_MSDON))
  endif
  
  if (MSP_gen_messages("ui_pot_grgs")) goto 999
  
999 continue
  
  call cps_close_utilisateur()

  ! affichage des erreurs
  if (MSP_PROBLEME) then
     call MSP_recuperer_message(message=messages, nb_mes=MSP_tous_messages)
     call MSP_afficher_message(message=messages, unit=0)
  else
     write(*,1000)"La conversion de fichier a été réalisée."
  endif
  
  ! liberation memoire
  if (associated(zb)) deallocate(zb)
  if (associated(clmb)) deallocate(clmb)
  if (associated(slmb)) deallocate(slmb)
  if (associated(C)) deallocate(C)
  if (associated(S)) deallocate(S)

end program ui_pot_grgs
