program ui_polesUAI

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_polesUAI
!
!$Resume
! Utilitaire de calcul de mouvements des axes des pôles d'un corps donné.
!
!$Description
! Utilitaire de calcul de mouvements des axes des pôles d'un corps donné.
!
! Usage
!.    ui_polesUAI -ci code -jour jour -sec secondes -th theorie [-nv]
!
!
!>E  code       : <integer>          code du corps étudié
!>E  theorie    : <LEN=7>            théorie utilisée (UAI2000 ou UAI91)
!>E  jour       : <integer>          date JJ50, jour entier
!>E  secondes   : <pm_reel>          secondes dans le jour
!>[E]  -nv      :                    option permettant d'obtenir les 
!                                    résultats sans commentaire ni unité
!
!
!>S  alpha0     : <PM_REEL>          ascension droite (rad)
!>S  delta0     : <PM_REEL>          déclinaison (rad)
!>S  W          : <PM_REEL>          temps sidéral (rad)
!>S  dW         : <PM_REEL>          dérivée du temps sidéral (rad/s)
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Version
!  $Id: ui_polesUAI.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_polesUAI.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.17  2010/11/02 16:16:17  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.16  2010/11/02 14:24:04  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.15  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.14  2008/10/28 14:11:44  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.13  2008/10/03 07:22:06  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.12  2008/07/11 12:08:32  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.11  2008/07/04 12:04:08  huec
!  DM-ID 1058 : Gestion de la memoire, appel a cps_close_utilisateur
!
!  Revision 1.10  2008/04/04 18:00:31  vivaresf
!  Version 2.4 relecture des cartouches
!
!  Revision 1.9  2008/04/03 18:03:43  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.8  2007/11/13 14:39:36  vivaresf
!  DM-ID 698 : réels formmatés pour les portages à venir
!
!  Revision 1.7  2007/07/04 12:22:46  vivaresf
!  FA-ID 664 : majuscules pour la présentation
!
!  Revision 1.6  2007/01/19 15:09:54  fabrec
!  compas V2.2 : controle des cartouches
!
!  Revision 1.5  2006/11/20 08:13:23  vpg
!  Mise a jour des cartouches
!
!  Revision 1.4  2006/11/09 14:39:17  vivaresf
!  DM-ID 462 : cas d'erreur
!
!  Revision 1.3  2006/10/26 13:45:33  vpg
!  DM-ID 425 : passage de PSIMU sous Linux
!
!  Revision 1.2  2006/10/24 08:49:09  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!
!  Revision 1.1  2006/10/23 15:42:39  mle
!  DM-ID 462 : amelioration de l'acces a la description de la base COMPAS
!
!  Revision 1.7  2006/08/30 08:54:35  vivaresf
!  FA-ID 576 : unités pour les poles UAI
!
!  Revision 1.6  2006/05/23 09:32:58  vivaresf
!  DM-ID 387 (lot 5) : mise au point des utilitaires
!  - simplification du code
!  - suppression des variables inutilisées
!
!  Revision 1.5  2006/05/12 12:06:14  bouillaj
!  Amelioration qualite : complements sur les cartouches
!
!  Revision 1.4  2006/05/02 09:38:39  vpg
!  Suppression des variables non utilisees
!
!  Revision 1.3  2006/03/30 14:02:02  bouillaj
!  *** empty log message ***
!
!  Revision 1.2  2006/02/07 09:32:06  bouillaj
!  Redaction du cartouche
!
!
!
!#V
!>  code       : <integer>          code du corps étudié
!>  theorie    : <LEN=7>            théorie utilisée
!>  dateref    : <tm_jour_sec>      date étudié (jj 1950)
!>  jour       : <integer>          jour
!>  sec        : <pm_reel>          seconde
!>  alpha0     : <PM_REEL>          ascension droite (rad)
!>  delta0     : <PM_REEL>          déclinaison (rad)
!>  W          : <PM_REEL>          temps sidéral (rad)
!>  dW         : <PM_REEL>          dérivée du temps sidéral (rad/sec)
!>  messages   : <MSP_MESSAGE>      
!>  long       : <PM_REEL>          longitude (rad)
!>  haut       : <PM_REEL>          
!>  noptions   : <integer>          
!>  ii         : <integer>          
!>  iargc      : <integer>          
!>  ierfin     : <integer>          
!>  l_opt      : <LEN=20,DIM=(50)>  
!>  logjour    : <logical>          
!>  logsec     : <logical>          
!>  logtheo    : <logical>          
!>  logcode    : <logical>          
!#
!
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_getMouvementsPolesCorps
!- MSP_recuperer_message
!- MSP_afficher_message
!#
!
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_poles
!- cps_acces
!#
!
!#V
!#
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
  use cps_poles
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_polesUAI.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées/sorties 
  integer :: code
  character(LEN=7) :: theorie
  type(tm_jour_sec) :: dateref
  integer :: jour
  real(kind=pm_reel) :: secondes

  real(KIND=PM_REEL) :: alpha0, delta0, W, dW     

! variables locales

  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, ierfin
  character(len=20),dimension(50):: l_opt
  logical :: logjour, logsec, logtheo, logcode, lognv

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_polesUAI") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  logjour =.false.
  logsec =.false.
  logcode = .false.
  logtheo = .false.
  lognv = .false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_polesUAI")
         goto 999
      else
         ! Lecture des arguments : 
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_polesUAI")
            goto 999
         else
            do ii=1, noptions
               select case(l_opt(2*ii-1))
               case("-ci")
                  read(l_opt(2*ii), fmt='(i3)') code
                  logcode = .true.
               case("-sec") 
                  read(l_opt(2*ii), fmt=*) secondes
                  logsec=.true.
               case("-jour")
                  read(l_opt(2*ii), fmt=*) jour
                  logjour=.true.
               case("-th")
                  theorie=trim(l_opt(2*ii))
                  logtheo=.true.
               case("-nv")
                  lognv = .true.
               end select
            enddo
         endif
         
      endif

! corps du programme

! Initialisations preliminaires

      if(logjour.and.logsec.and.logtheo.and.logcode) then
         dateref%jour=jour
         dateref%sec=secondes
         
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         
         call cps_getMouvementsPolesCorps(code, theorie, dateref, &
              alpha0, delta0, W, dW)       

         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
       
         ! sortie écran
         if (.not. lognv) then
            write (*,1002) "Ascension droite         (rad)   = ", alpha0
            write (*,1002) "Déclinaison              (rad)   = ", delta0
            write (*,1002) "Temps sidéral            (rad)   = ", W
            write (*,1002) "Dérivée du temps sidéral (rad/s) = ", dW
         else
            write(*,1001) alpha0
            write(*,1001) delta0
            write(*,1001) W
            write(*,1001) dW
         end if
      else
         write(0,*) "Les arguments ne sont pas correctement remplis"

      endif
      call cps_close_utilisateur()

999 continue
       if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

1001 FORMAT(g21.12)
1002 FORMAT(a,g21.12)

  end program ui_polesUAI
