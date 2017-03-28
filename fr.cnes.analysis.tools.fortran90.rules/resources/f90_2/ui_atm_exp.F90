program ui_atm_exp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_exp
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère EXP
!
!$Description
! Utilitaire pour le fonctionnement du modèle d'atmosphère EXP.
! Il utilise la routine cps_atm_exponentiel de cps_atm_exp.F90 de COMPAS_BASE.
! Usage
!.     ui_atm_exp -haut haut -h0 h0 -ro0 ro0 -hs hs 
!         [-altmin altmin] [-altmax altmax] 
!         [-planete planete / -g g] [-m m] [-gamma gamma]
!>E     haut     :<pm_reel>   hauteur aréodésique (m)
!>E     h0       :<pm_reel>   hauteur de référence (m)
!>E     ro0      :<pm_reel>   masse volumique à la hauteur de référence (kg/m^3)
!>E     hs       :<pm_reel>   constante d'echelle de masse volumique/pression (m)
!>S     ro       :<pm_reel>   masse volumique (kg/m^3)
!>[E]   altmin   :<pm_reel>   altitude minimum (m) (0 km par défaut)
!>[E]   altmax   :<pm_reel>   altitude minimum (m) (1000 km par défaut)
!>[E]   planete  :<integer>   planète 
!.                            si planete est fourni, on utilise
!.                            planete, m et gamma
!.                            sinon on n'en utilise aucun.  
!>[E]   g        :<pm_reel>   accélération gravitationnelle locale 
!.                            inutilisée si planete renseignée
!>[E]   m        :<pm_reel>   masse molaire (kg.mol-1)
!>[E]   gamma    :<pm_reel>   capacité thermique
!>[S]   temp     :<pm_reel>   température approchée  (K) 
!>[S]   pres     :<pm_reel>   pression (Pa)
!>[S]   vsound   :<pm_reel>   vitesse du son (m/s)
!
!$Auteur
! Sandrine Baudrand(ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_exp.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_exp.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.17  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.16  2010/11/02 14:16:37  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.15  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.14  2010/10/04 11:08:33  ogarat
!  VERSION::FA-ID:1446:04/10/2010: Alignement des signes = dans l'affichage des résultats
!
!  Revision 1.13  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.12  2010/05/05 13:11:27  jlrobin
!  V2.9::FA-ID:1347:05/05/2010:probleme d'affichage d unite
!
!  Revision 1.11  2009/11/17 09:11:00  cmartel
!  DM-ID 1120 : Ajout de tests sur les entrees
!
!  Revision 1.10  2009/08/24 09:54:26  cml
!  DM-ID 1120 : Correction de la description du paramètre g
!
!  Revision 1.9  2009/08/21 09:56:23  cml
!  DM-ID 1120 : Prise en compte des nouvelles fonctionnalités du modèle
!
!  Revision 1.8  2008/10/28 14:11:39  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!  Revision 1.7  2008/10/03 07:22:02  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!  Revision 1.6  2008/07/11 12:08:13  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!  Revision 1.5  2008/07/04 12:23:59  huec
!  DM-ID 1058 : Gestion de la memoire, appel a cps_close_utilisateur
!  Revision 1.4  2008/04/03 18:02:54  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!  Revision 1.3  2007/11/13 14:39:31  vivaresf
!  DM-ID 698 : réels formmatés pour les portages à venir
!  Revision 1.2  2007/11/12 08:28:12  sbd
!  DM-ID 797 ajout commentaires sur parametres optionnels
!  Revision 1.1  2007/10/30 11:09:21  sbd
!  DM-ID 797 creation utilitaires pour terre mars et venus
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_acces
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_atmexp
!- MSP_recuperer_message
!- MSP_afficher_message
!
!$FinHistorique
!
!$Include
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
  use cps_atm_exp
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_exp.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées
  real(KIND=PM_REEL) :: haut,h0,ro0,hs
  real(KIND=PM_REEL) :: m, gamma, g
  integer :: planete
  real(KIND=PM_REEL) :: altmin, altmax

! sorties
  real(KIND=PM_REEL) :: temp,pres,ro, vsound 

! variables locales
  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, ierfin, ok
  character(len=60),dimension(50):: l_opt
  character(LEN=32) :: errmess
  character(LEN=256) :: unite
  real(kind=pm_reel) :: requa, mu
  ! Trace des données en entrées
  logical :: loghaut, logh0, logro0, loghs
  logical :: logplanete, logm, loggamma, loggrav
  logical :: logaltmin, logaltmax

! initialisation

  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_exp") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  ! Mise a zero des flags
  loghaut =.false.
  logh0 =.false.
  logro0 = .false.
  loghs = .false.
  logplanete =.false.
  logm =.false.
  loggamma =.false.
  loggrav=.false.
  logaltmin=.false.
  logaltmax=.false.
! Lecture des paramètres (en ligne)
  call ui_test_arguments ("-h", "", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_exp")
         goto 999
      else
         ! Lecture des arguments :
         ! -haut -h0 -ro0 -hs [-planete] [-m] [-gamma]
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_exp")
            goto 999
         else
            ! Lecture des options saisies en entrée
            do ii=1, noptions
               if (l_opt(2*ii-1)=="-haut") then
                  read(l_opt(2*ii), fmt=*) haut
                  loghaut = .true.
               elseif(l_opt(2*ii-1)=="-h0") then
                  read(l_opt(2*ii), fmt=*) h0
                  logh0=.true.
               elseif(l_opt(2*ii-1)=="-ro0") then
                 read(l_opt(2*ii), fmt=*) ro0
                  logro0=.true.
               elseif(l_opt(2*ii-1)=="-hs") then
                  read(l_opt(2*ii), fmt=*) hs
                  loghs=.true.
               elseif(l_opt(2*ii-1)=="-planete") then
                  read(l_opt(2*ii), fmt=*) planete
                  logplanete=.true.
              elseif(l_opt(2*ii-1)=="-g") then
                  read(l_opt(2*ii), fmt=*) g
                  loggrav=.true.
              elseif(l_opt(2*ii-1)=="-m") then
                  read(l_opt(2*ii), fmt=*) m
                  logm=.true.
              elseif(l_opt(2*ii-1)=="-gamma") then
                  read(l_opt(2*ii), fmt=*) gamma
                  loggamma=.true.
              elseif(l_opt(2*ii-1)=="-altmin") then
                  read(l_opt(2*ii), fmt=*) altmin
                  logaltmin=.true.
              elseif(l_opt(2*ii-1)=="-altmax") then
                  read(l_opt(2*ii), fmt=*) altmax
                  logaltmax=.true.
              endif
            enddo
         endif
         
      endif

! corps du programme

! Initialisations preliminaires

      ! Par défaut les bornes sont 0 et 1000 km
      if (.not. logaltmin) altmin = 0.0_pm_reel
      if (.not. logaltmax) altmax = 1000000.0_pm_reel

      if(logplanete) then
         ! Calcul de g pour la planete
         ! il faut extraire le mu et le rayon équatorial
         unite="m"
         ok= cps_getCsteTh(planete,"UAI1994", "mu", mu, unite)
         write(errmess,*) "corps ", planete
         if (ok/=cps_ok) then
            call MSP_signaler_message(cle_mes="CPS_ATM_ERR",&
               partie_variable=errmess)
            goto 999
         endif
         unite="m"
         ok=cps_getCsteTh(planete,"UAI1994","requa", requa, unite)
         if (ok/=cps_ok) then
            call MSP_signaler_message(cle_mes="CPS_ATM_ERR",&
               partie_variable=errmess)
            goto 999
         endif

         ! Calcul de g
         g=mu/(requa+h0)/(requa+h0)
         loggrav = .true.
      endif

! Test uniquement sur les arguments obligatoires

      if(loghaut.and.logh0.and.logro0.and.loghs) then
          
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         ! g, m et gamma sont optionnels : 
         ! soit on les fournit tous soit aucun des trois.
         ! Dans le cas où seuls 1 ou 2 des paramètres optionnels sont donnés
         ! on n'en utilise aucun.
         ! Dans le cas où juste m (ou/et gamma) est donné, on ne les prend pas en compte
         if(logm.and.loggamma.and.loggrav) then
               ! Calcul complet
               call cps_atm_exponentiel (haut,ro0,h0,hs,altmin, &
                    altmax, densite=ro, &
                    g=g, ml=m, gamma=gamma, &
                    pression=pres, vson=vsound, temp=temp)
               if ( MSP_ERREUR ) goto 999
               ! sortie écran
               write (*,'(a,e15.9)') "Température     (K)     = ", temp
               write (*,'(a,e15.9)') "Pression        (Pa)    = ", pres
               write (*,'(a,e15.9)') "Masse volumique (kg/m3) = ", ro
               write (*,'(a,e15.9)') "Vitesse du son  (m/s)   = ", vsound
	     else 
               ! Calcul de la densité seule
               call cps_atm_exponentiel (haut,ro0,h0,hs,altmin, &
                    altmax, densite=ro)
               if ( MSP_ERREUR ) goto 999
               ! sortie écran
               write (*,'(a,e15.9)') "Masse volumique (kg/m3) = ", ro

	     endif      
      else
         write(0,*) "Erreur : les arguments ne sont pas correctement remplis"
      endif

999 continue
       if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

    ! Fermeture de la base
    call cps_close_utilisateur()

  end program ui_atm_exp
