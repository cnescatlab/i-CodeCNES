program ui_conv_acsol2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_conv_acsol2
!
!$Resume
!  Convertit un fichier d'activité solaire ACTSOL en un fichier ACSOL2.
!
!$Description
!  Convertit un fichier d'activité solaire ACTSOL en un fichier ACSOL2.
!
! Usage
!.  ui_conv_acsol2 fic1 fic2 
!>E  fic1       : <LEN=256>     nom du fichier format ACTSOL à convertir
!>E  fic2       : <LEN=256>     nom du fichier format ACSOL2 après conversion
!
!$Auteur
!  Cédric MARTEL (ATOS ORIGIN)  
!
!$Version
!  $Id: ui_conv_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_conv_acsol2.F90,v $
!  Revision 1.3  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.2  2009/11/23 10:08:06  cmartel
!  AQ : Suppression de variables inutilisees
!
!  Revision 1.1  2009/11/16 12:59:58  cmartel
!  DM-ID 842 : Ajout d'un utilitaire de conversion ACTSOL en ACSOL2
!
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
   use ui_acsol2

   implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_conv_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $'


   ! Entrees/sorties
   character(LEN=256) :: fic1, fic2

   ! Variable locale
   character(LEN=256), dimension(50) :: l_opt
   integer :: noptions
 
   ! Analyse des arguements
   character(LEN=256) :: dirfcf
   integer :: lch_dirfcf
   type(MSP_MESSAGE) :: messages
   integer :: nvar, iargc, ier
   logical :: present

   ! Données lues puis écrites
   integer :: nb_lignes = 0
   type(ligne_acsol2), dimension(:), pointer :: tab_lignes => NULL()

   ! initialisation
   !---------------
   ier = AMv_rc_get ('compas_ui_fcf','compas','','dirfcf',dirfcf, &
        lch_dirfcf)
   call MSP_ajouter_fichier_message (trim(dirfcf)//"/CPSUI_erreur_fr")
   call cps_init_utilisateur()
   if (MSP_gen_messages("ui_conv_acsol2")) goto 999

   ! analyse des arguments
   !----------------------

   ! aide
   ! lecture des arguments
   call ui_lire_options(noptions, l_opt)
   call ui_test_arguments ("-h", "", ier)
   nvar=iargc()

   ! -h ou pas de parametres
   if(ier.eq.1.or.nvar /= 2) then
      call ui_ecrire_help("ui_conv_acsol2")
      goto 999
   endif

   ! Les deux fichiers sont les deux premiers arguments
   call getarg(1, fic1)
   call getarg(2, fic2)

   ! Vérification de la présence des deux fichiers renseignés
   inquire(file=fic1, exist=present)
   if ( .not. present ) then
	  call MSP_signaler_message(cle_mes="CPS_ERR_FICABS", &
         partie_variable=trim(fic1))
      goto 999
   endif
   inquire(file=fic2, exist=present)
   if ( present ) then
	  call MSP_signaler_message(cle_mes="CPS_ERR_WRITE", &
         partie_variable=trim(fic2))
      goto 999
   endif
      
   ! Traitement
   !------------

   ! Extraction des données ACTSOL
   call cpsi_lire_actsol(fic1, tab_lignes, nb_lignes)
   if ( MSP_gen_messages("ui_conv_acsol2 - lire ACTSOL") ) goto 999

   ! Ecriture des données au format ACSOL2
   call cpsi_ecrireAcsol2(fic2, tab_lignes, nb_lignes)
   if ( MSP_gen_messages("ui_conv_acsol2 - ecrire ACSOL2") ) goto 999

   ! affichage des erreurs
999 if (MSP_PROBLEME) then
      call MSP_recuperer_message(message=messages, nb_mes=MSP_tous_messages)
      call MSP_afficher_message(message=messages, unit=0)
   endif


   ! fin
   call cps_close_utilisateur()

end program ui_conv_acsol2
