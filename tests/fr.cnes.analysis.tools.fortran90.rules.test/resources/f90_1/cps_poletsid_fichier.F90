module cps_poletsid_fichier

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_poletsid_fichier
!
!$Resume
!  Modèle d'attitude quelconque pour un corps du systême solaire
!
!$Description
!  Regroupe les méthodes de lecture d'un fichier modèle positions tabulées
!  du pôle et du temps sidéral, interpolation de ces valeurs, et recherche
!  dans la base des informations pour ce type de modèles.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Version
!  $Id: cps_poletsid_fichier.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_poletsid_fichier.F90,v $
!  Revision 1.11  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.10  2009/11/13 14:05:00  cmartel
!  AQ : Ajout de desallocation manquante
!
!  Revision 1.9  2008/10/28 13:17:24  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!
!  Revision 1.8  2008/10/15 15:02:57  tanguyy
!  DM-ID 1058 : les "use" sont sur 2 lignes pour passer en g95
!
!  Revision 1.7  2008/10/14 08:14:27  cml
!  AQ : Ajout d'un commentaire sur les sorties
!
!  Revision 1.6  2008/10/07 16:02:11  cml
!  DM-ID 1111 : Correction du nom de la structure
!
!  Revision 1.5  2008/10/01 12:43:56  cml
!  DM-ID 1111 : Amelioration des messages d erreur
!
!  Revision 1.4  2008/09/29 15:58:23  cml
!  DM-ID 1111 : Fractionnement de la fonction de lecture
!
!  Revision 1.3  2008/09/29 07:47:29  cml
!  DM-ID 1111 : Fractionnement de la fonction de lecture
!  Revision 1.2  2008/09/18 15:48:08  cml
!  DM-ID 1111 : Ajout de remarques pour utilisation des pointeurs et de la structure
!  Revision 1.1  2008/09/16 07:45:08  cml
!  DM-ID 1111 : Renommage des fonctions et modules
!  Revision 1.4  2008/09/09 15:36:21  cml
!  DM-ID 1111 : Ajout de la lecture du rayon equatorial et aplatissement
!  Revision 1.3  2008/09/08 07:56:29  cml
!  DM-ID 1111 : Ajout du nombre d elements aux signatures des routines
!  Revision 1.2  2008/08/26 08:31:29  cml
!  DM-ID 1111 : Suppression du traitement des noms de modele en double
!  Revision 1.1  2008/08/26 07:41:27  cml
!  DM-ID 1111 : Creation du module pour le traitement des fichiers poletsid
!
!$FinHistorique
!
!$Usage
!  use cps_poletsid_fichier
!
!$Structure
!
!: cps_struct_poletsid_fichier : Description tabulée du position du pôle et du temps sidéral.
!>     npoletsid   : <integer>                  Nombre de lignes      
!>     dates       : <pm_reel,DIM=(:),pointer>  Date en jours juliens fractionnaires
!>     alphas      : <pm_reel,DIM=(:),pointer>  Premier angle (rad) 
!>     deltas      : <pm_reel,DIM=(:),pointer>  Deuxième angle (rad)  définissant la position du pôle
!>     tsid        : <pm_reel,DIM=(:),pointer>  Temps sidéral (rad) 
!>     dtsid       : <pm_reel,DIM=(:),pointer>  Dérivée du temps sidéral (rad/s)
!
!$Global
!
!$Common
!
!$Routines
!- cps_lireFichierPoletsid
!- cpsi_preAnalyerFichier
!- cpsi_lectureValeursPoleTsid
!- cps_getModelesPoletsidCorps
!- cps_getListeCorpsPoletsid
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MSLIB
!- cps_utilisateur
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cps_lireFichierPoletsid cpsi_preAnalyerFichier cpsi_lectureValeursPoleTsid cps_getModelesPoletsidCorps
!.  cps_getListeCorpsPoletsid
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


use MSLIB, only : pm_reel

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_poletsid_fichier.F90 69 2012-09-11 08:33:34Z ffsm $'



type cps_struct_poletsid_fichier
! Structure de description tabulée du position du pôle et du temps sidéral en fonction
! du temps

     integer :: npoletsid  ! Dimension des tableaux suivants
     real(KIND=pm_reel), dimension(:),pointer :: dates => NULL() ! Tableau des dates ordonnées 
                                                                 ! (jour juliens 1950 fractionnaires)
     ! Position du pole à chaque pas de temps
     real(KIND=pm_reel), dimension(:),pointer :: alphas => NULL()! Tableau des angles alpha (rad) 
     real(KIND=pm_reel), dimension(:),pointer :: deltas => NULL()! Tableau des angles deltas (rad)
     ! Description du temsp sidéral
     real(KIND=pm_reel), dimension(:),pointer :: tsid => NULL()  ! Temps sidéral (rad)
     real(KIND=pm_reel), dimension(:),pointer :: dtsid => NULL() ! Dérivée du temps sidéral (rad / s)
     
end type cps_struct_poletsid_fichier

contains 




subroutine cps_lireFichierPoletsid(nom_fichier, tab_poletsid, &
       code_corps, datemin, datemax, requa, apla)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_lireFichierPoletsid
!
!$Resume
!  Routine de lecture d'un fichier vers une structure de données.
!
!$Description
!  Routine de lecture d'un fichier vers une structure de données
!  Si la datemin est donnée, alors la lecture commencera à la dernière date
!    telle que date <= datemin
!  Si la datemax est donnée, alors la lecture s'arrêtera à la première date 
!    telle que date >= datemax
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_lireFichierPoletsid(nom_fichier, tab_poletsid, &
!.           code_corps, [datemin], [datemax], [requa], [apla])
!.    character(LEN=*) :: nom_fichier
!.    real(KIND=pm_reel) :: datemin
!.    real(KIND=pm_reel) :: datemax
!.    type(cps_struct_poletsid_fichier) :: tab_poletsid
!.    integer :: code_corps
!.    real(KIND=pm_reel) :: requa
!.    real(KIND=pm_reel) :: apla
!
!$Arguments
!>E     nom_fichier   :<LEN=*>                     Nom du fichier à lire
!>S     tab_poletsid  :<cps_struct_poletsid_fichier>   Structure des positions du pole et du temps sidéral sur [datemin, datemax]
!>S     code_corps    :<integer>                   Code NAIF du corps pour ce fichier
!>[E]   datemin       :<pm_reel>                   Debut de l'intervalle sur lequel on veut les données (jj frac)
!>[E]   datemax       :<pm_reel>                   Fin de l'intervalle (jj frac) 
!>[S]   requa         :<pm_reel>                   Rayon équatorial lu (m)
!>[S]   apla          :<pm_reel>                   Aplatissement lu (sans unité)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_preAnalyerFichier
!- cpsi_lectureValeursPoleTsid
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
!#
!
!$Remarques
!   Attention, lors de la création de la structure avant l'appel à cette fonction, 
!   il faut s'assurer que les éléments dates, alphas, deltas, tsid, dtsid soient bien nuls.
!   Le cas échéant un dépassement mémoire peut survenir.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use cps_utilisateur

    implicit none
    
    !-------------------------------
    ! Paramètres d'entrée
    character(LEN=*), intent(in) :: nom_fichier

    ! Paramètres d'entrée optionnels
    real(KIND=pm_reel), optional, intent(in) :: datemin
    real(KIND=pm_reel), optional, intent(in) :: datemax
    
    ! Paramètres en sortie
    type(cps_struct_poletsid_fichier), intent(out) :: tab_poletsid
    integer, intent(out) :: code_corps

    ! Paramètres en sortie optionnels
    real(KIND=pm_reel), optional, intent(out) :: requa
    real(KIND=pm_reel), optional, intent(out) :: apla
    
    !-------------------------------
    ! Variables locales
    integer :: acces_mad               ! Numéro d'accès MADONA
    character (len=CPS_MAXLG), dimension(3) :: msp_mess ! Structure pour message d'erreur
    integer :: code_retour             ! Code de retour
    real(KIND=pm_reel) :: datemin_lue  ! Date minimum lue dans le header
    real(KIND=pm_reel) :: datemax_lue  ! Date maximum lue dans le headder
    ! Indice de la premiere ligne a lire (0 si datemin non specifiee)
    integer :: indice_prem_ligne
    integer :: nb_lignes_a_lire        ! Nombre de lignes a lire

    !-------------------------------
    ! L'algorithme est le suivant : 
    !  - Lecture du header et vérification a priori
    !    (lecture éventuelle du rayon équatorial et de l'aplatissement)
    !  - Si datemin présente, recherche de la première ligne valable
    !  - Si datemax présente, recherche du nombre d'élements à stocker
    !  - Allocation du vecteur final
    !  - Recopie des "nb_lignes_a_lire lignes" à partir de "indice_prem_ligne"
    !-------------------------------

    !-------------------------------
    ! Init de la structure de sortie
    tab_poletsid%npoletsid = 0
    if ( associated(tab_poletsid%dates ) )  deallocate(tab_poletsid%dates)  
    if ( associated(tab_poletsid%alphas ) ) deallocate(tab_poletsid%alphas) 
    if ( associated(tab_poletsid%deltas ) ) deallocate(tab_poletsid%deltas)
    if ( associated(tab_poletsid%tsid ) )   deallocate(tab_poletsid%tsid)
    if ( associated(tab_poletsid%dtsid ) )  deallocate(tab_poletsid%dtsid)
    
    ! ouvrir le fichier
    acces_mad = acc_open()
    if (acces_mad >= 0 ) then
       code_retour = acc_connect (acces_mad, nom_fichier, ACC_R)
    end if
    if (acces_mad < 0 .or. code_retour < 0) then
        call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
             routine="cps_lireFichierPoletsid", &
             partie_variable=trim(nom_fichier))
        return
    end if

    ! Lecture de l'entete
    code_retour = acc_read( acces_mad, ACC_HEADER )
    if ( code_retour .lt. 0 ) then
       ! Creation du message d'erreur
       write(msp_mess(1),*) code_retour
       write(msp_mess(2),*) "HEADER"
       write(msp_mess(3),*) trim(nom_fichier)
       call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
             routine="cps_lireFichierPoletsid", &
             partie_variable=msp_mess)
       return
    end if

    ! Analyse de l'entete :
    !  extraction du corps, de la theorie, du nombre de lignes, 
    !  de la date min et de la date max
    code_retour = acc_geti(acces_mad, "code_corps", code_corps)
    code_retour = acc_getd(acces_mad, "date_min", datemin_lue, "jj1950")
    code_retour = acc_getd(acces_mad, "date_max", datemax_lue, "jj1950")
    ! Extraction optionnnelle du rayon équatorial
    if (present(requa)) then
       requa = 0._pm_reel
       code_retour = acc_getd(acces_mad, "requa", requa, "m")
    endif

    if (present(apla)) then
       apla = 0._pm_reel
       code_retour = acc_getd(acces_mad, "apla", apla, "")
    endif

    ! Verification que les dates eventuellement imposées sont possibles
    if ( present(datemax) ) then
       if (datemax <= datemin_lue) then
          call MSP_signaler_message(cle_mes="CPS_ERR_DATES_NON_COUV_POLETSID", &
             routine="cps_lireFichierPoletsid", &
             partie_variable=trim(nom_fichier))
          return
       end if
    end if

    ! Analyse des cas, et vérification des dates
    if ( present(datemin) ) then

       if (datemin >= datemax_lue) then
          ! NOTE : Ici on aurait besoin d'un "ET PUIS" mais ca n'existe pas
          call MSP_signaler_message(cle_mes="CPS_ERR_DATES_NON_COUV_POLETSID", &
             routine="cps_lireFichierPoletsid", &
             partie_variable=trim(nom_fichier))
          return
       end if

       if (present (datemax) ) then

          if ( datemin >= datemax ) then 
             write(msp_mess(1),*) datemin
             write(msp_mess(2),*) datemax
             call MSP_signaler_message(cle_mes="CPS_ERR_DATES_POLETSID", &
                routine="cps_lireFichierPoletsid", &
                partie_variable=msp_mess)
             return
          end if

          call cpsi_preAnalyerFichier(nom_fichier, acces_mad, &
               indice_prem_ligne, nb_lignes_a_lire, datemin=datemin, datemax=datemax)
       else
          call cpsi_preAnalyerFichier(nom_fichier, acces_mad, &
               indice_prem_ligne, nb_lignes_a_lire, datemin=datemin )
       endif 
    else
       if (present (datemax) ) then
          call cpsi_preAnalyerFichier(nom_fichier, acces_mad, &
               indice_prem_ligne, nb_lignes_a_lire, datemax=datemax)
       else
          call cpsi_preAnalyerFichier(nom_fichier, acces_mad, &
               indice_prem_ligne, nb_lignes_a_lire)
       endif 
    endif

    ! Remontée d'une éventuelle erreur
    if ( MSP_gen_messages("cps_lireFichierPoletsid") )return

    ! Après lecture, on connaît le nombre de lignes dispo correspondant aux critères
    if ( nb_lignes_a_lire < 2) then
       if (.not. present(datemin) .and. .not.  present(datemax) ) then
          ! Erreur : au moins deux lignes doivent pouvoir etres lues !
          write(msp_mess(1),*) nb_lignes_a_lire
          write(msp_mess(2),*) trim(nom_fichier)
          call MSP_signaler_message(cle_mes="CPS_ERR_NB_LIGNES_POLETSID", &
                routine="cps_lireFichierPoletsid", &
                partie_variable=msp_mess)
          return     
       else
          ! Message d'erreur différent, l'intervalle n'est pas suffisamment
          ! couvert par le fichier
          call MSP_signaler_message(cle_mes="CPS_ERR_DATES_NON_COUV_POLETSID", &
                routine="cps_lireFichierPoletsid", &
                partie_variable=trim(nom_fichier))
          return     
       end if
    end if

    ! Remise a zero des compteurs et deconnexion
    code_retour = acc_deconnect(acces_mad, ACC_R)
    code_retour = acc_close(acces_mad)

    !----------------------------
    ! Lancement de la lecture
    call cpsi_lectureValeursPoleTsid(nom_fichier, indice_prem_ligne, nb_lignes_a_lire, &
          datemin_lue, datemax_lue, tab_poletsid)

    ! Remontée d'une éventuelle erreur
    if ( MSP_gen_messages("cps_lireFichierPoletsid") )return

end subroutine cps_lireFichierPoletsid


subroutine cpsi_preAnalyerFichier(nom_fichier, acces_mad, &
    indice_prem_ligne, nb_lignes_a_lire, datemin, datemax)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_preAnalyerFichier
!
!$Resume
!  Fonction interne d'analyse des lignes qui devrons être extraites
!  d'un fichier "pôle et temps sidéral".
!
!$Description
!  Fonction interne d'analyse des lignes qui devrons être extraites
!  d'un fichier "pôle et temps sidéral" en fonction d'éventuelles
!  dates minimu et maximum. On suppose que l'accès Madonna est déjà
!  positionné après lecture du HEADER du fichier colonne.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_preAnalyerFichier(nom_fichier, acces_mad, &
!.        indice_prem_ligne, nb_lignes_a_lire, [datemin], [datemax])
!.    character(LEN=*) :: nom_fichier
!.    integer :: acces_mad 
!.    integer :: indice_prem_ligne
!.    integer :: nb_lignes_a_lire 
!.    real(KIND=pm_reel) :: datemin
!.    real(KIND=pm_reel) :: datemax
!
!$Arguments
!>E     nom_fichier        :<LEN=*>     Nom du fichier ouvert
!>E     acces_mad          :<integer>   Accès MADONA à ce fichier
!>S     indice_prem_ligne  :<integer>   Indice de la première ligne à lire
!>S     nb_lignes_a_lire   :<integer>   Nombre total de ligne qui devront être lues
!>[E]   datemin            :<pm_reel>   Date minimum qui devra être couverte
!>[E]   datemax            :<pm_reel>   Date maximum qui devra être couverte
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
!- cps_utilisateur
!#
!
!$Remarques
!  Méthode interne à cps_lireFichierPoletsid. L'accès Madona suppose que l'entête
!  du fichier a déjà été traitée. L'utilisation de cette méthode en dehors de la
!  fonction de lecture est fortement déconseillée.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use cps_utilisateur

    implicit none
    
    !-------------------------------
    ! Paramètres d'entrée
    character(LEN=*), intent(in) :: nom_fichier
    integer, intent(in) :: acces_mad               ! Numéro d'accès MADONA

    ! Sorties ----------------------
    ! Indice de la premiere ligne a lire (0 si datemin non specifiee)
    integer, intent(out) :: indice_prem_ligne
    integer, intent(out) :: nb_lignes_a_lire        ! Nombre de lignes a lire

    ! Paramètres d'entrée optionnels
    real(KIND=pm_reel), optional, intent(in) :: datemin
    real(KIND=pm_reel), optional, intent(in) :: datemax
    
    !-------------------------------
    ! Variables locales
    character (len=CPS_MAXLG), dimension(3) :: msp_mess ! Structure pour message d'erreur
    integer :: code_retour             ! Code de retour
    integer :: code_read               ! Code de retour pour le acc_read
    real(KIND=pm_reel) :: date_cour    ! Derniere date lue
    ! Indice de la premiere ligne a lire (0 si datemin non specifiee)
    logical :: trouve                  ! Flag pour l'arrêt des boucles
    integer :: nb_lignes_lues          ! Nombre de lignes lues


    ! Initialisation du code_read
    code_read = ACC_EOF
    code_retour = 0

    ! --------------------------------------
    ! Si la date min est specifiee, on avance jusqu'a rencontrer la derniere date <= datemin
    nb_lignes_lues = 0
    indice_prem_ligne = 1
    date_cour = 0.
    ! Init : la valeur n'est pas trouvee
    trouve = .false.
    if ( present(datemin) ) then
       ! On avance jusqu'a passer la datemin
       ! NOTE DEV : on suppose que la date est > 0
       code_read = acc_read( acces_mad, ACC_NEXT )
       do while ( ( code_read .NE. ACC_EOF ) & 
           .and. ( .not. trouve ) )

           nb_lignes_lues = nb_lignes_lues +1

           ! Lecture de la date de la ligne
           code_retour = acc_getd(acces_mad, "date", date_cour, "jj1950" )
           ! Verification que la lecture de la colonne date est bien présente
           if ( code_retour < 0 ) then
              write(msp_mess(1),*) "date"
              write(msp_mess(2),*) trim(nom_fichier)
              call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                  routine="cpsi_preAnalyerFichier", &
                  partie_variable=msp_mess)   
              return 
           endif

           ! Test de la date courante
           if ( date_cour > datemin ) then
              trouve = .true.
           else
              ! Passage a la ligne suivante
              code_read = acc_read( acces_mad, ACC_NEXT )
           end if

       end do

       ! On a du trouve au moins une ligne, sinon erreur
       if (.not. trouve) then
          write(msp_mess(1),*) date_cour
          write(msp_mess(2),*) trim(nom_fichier)
          call MSP_signaler_message(cle_mes="CPS_ERR_DATEMIN_POLETSID", &
               routine="cpsi_preAnalyerFichier", &
               partie_variable=msp_mess)   
          return 
       end if

       ! On a avance au moins une fois, l'indice de la premiere ligne doit etre decrémenté
       indice_prem_ligne = nb_lignes_lues-1
    end if

    ! --------------------------------------
    ! Si la date max est specifiee, on avance jusqu'a rencontrer la derniere date >= datemax
    nb_lignes_a_lire = 0
    date_cour = 0.
    if ( present(datemax) ) then

       ! Que l'on soit passé dans la condition précédente ou non, le pointeur 
       ! sur les lignes du fichier doit etre deja positionné
       if ( .not. present(datemin) ) then

          ! Dans le cas ou l'option date min est présente, on a déjà avancé le curseur
          code_read = acc_read( acces_mad, ACC_NEXT )
          ! Filtre si aucune ligne n'est presente
          if ( code_read == ACC_EOF ) then
             call MSP_signaler_message(cle_mes="CPS_ERR_DATES_POLETSID", &
                  routine="cpsi_preAnalyerFichier", &
                  partie_variable=trim(nom_fichier))
             return
          end if
          
          ! Lecture de la date de la ligne
          code_retour = acc_getd(acces_mad, "date", date_cour, "jj1950" )
          
       end if

       ! La date courante est spécifiée sur la première valeur
       ! respectant alors la condition optionnelle datemin
       if ( date_cour >= datemax ) then
          ! Erreur : au moins deux lignes doivent pouvoir etres lues !
          call MSP_signaler_message(cle_mes="CPS_ERR_DATES_POLETSID", &
                  routine="cpsi_preAnalyerFichier", &
                  partie_variable=trim(nom_fichier))
          return          
       endif

       ! Sinon parcours jusqu'à dépasser la date
       trouve = .false.
       if ( present(datemin) ) then
          nb_lignes_a_lire = 1 ! Le curseur a deja avance d'un pas de trop
       else
          nb_lignes_a_lire = 0 ! Une seule lecture a ete effectuee
       end if

       ! Note: le code_read est deja OK a la premiere iteration
       do while ( ( code_read /= ACC_EOF ) & 
           .and. ( .not. trouve ) )
           
           ! Incrément du nombre de lignes valables
           nb_lignes_a_lire = nb_lignes_a_lire +1
           
           ! Lecture de la date de la ligne
           code_retour = acc_getd(acces_mad, "date", date_cour, "jj1950" )
           ! Verification que la lecture de la colonne date est bien présente
           if ( code_retour < 0 ) then
              write(msp_mess(1),*) "date"
              write(msp_mess(2),*) trim(nom_fichier)
              call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                  routine="cpsi_preAnalyerFichier", &
                  partie_variable=msp_mess)   
              return 
           endif

           ! Test d'arret de la recherche
           if (date_cour >= datemax) then
              trouve = .true.
           endif
           
           ! Ligne suivante
           code_read = acc_read( acces_mad, ACC_NEXT )
       end do

       ! Vérification que la condition "FIN de fichier"
       ! est remplie en même que le nombre de ligne au total
       if ( .not. trouve ) then 
          write(msp_mess(1),*) trim(nom_fichier)
          call MSP_signaler_message(cle_mes="CPS_ERR_DATES_NON_COUV_POLETSID", &
               routine="cpsi_preAnalyerFichier", &
               partie_variable=msp_mess)   
          return
       end if

       ! Pas de traitement particulier s'il on est arrivé à la fin
       ! nb_lignes_a_lire doit deja etre ok

    else
       ! Si l'option datemax n'était pas présente
       ! Alors le nombre de lignes a lire est calculée en allant jusqu'à la fin du fichier
       if ( present(datemin) ) then
          nb_lignes_a_lire = 2 ! Le curseur a deja avance d'un pas de trop
       else
          nb_lignes_a_lire = 0 ! La lecture n'a pas commencée
       end if

       ! Si l'option datemax n'était pas présente
       ! Alors le nombre de lignes a lire est calculée en allant jusqu'à la fin du fichier
       do while ( acc_read( acces_mad, ACC_NEXT ) /= ACC_EOF )
           
           ! Incrément du nombre de lignes valables
           nb_lignes_a_lire = nb_lignes_a_lire +1

       end do  
    end if
   
    ! Après lecture, on connaît le nombre de lignes dispo correspondant aux critères
    if ( nb_lignes_a_lire < 2) then
       if (.not. present(datemin) .and. .not.  present(datemax) ) then
          ! Erreur : au moins deux lignes doivent pouvoir etres lues !
          write(msp_mess(1),*) nb_lignes_a_lire
          write(msp_mess(2),*) trim(nom_fichier)
          call MSP_signaler_message(cle_mes="CPS_ERR_NB_LIGNES_POLETSID", &
                routine="cpsi_preAnalyerFichier", &
                partie_variable=msp_mess)
          return     
       else
          ! Message d'erreur différent, l'intervalle n'est pas suffisamment
          ! couvert par le fichier
          call MSP_signaler_message(cle_mes="CPS_ERR_DATES_NON_COUV_POLETSID", &
                routine="cpsi_preAnalyerFichier", &
                partie_variable=trim(nom_fichier))
          return     
       end if
    end if

end subroutine cpsi_preAnalyerFichier


subroutine cpsi_lectureValeursPoleTsid(nom_fichier, indice_prem_ligne, nb_lignes_a_lire, &
     date_min, date_max, tab_poletsid)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lectureValeursPoleTsid
!
!$Resume
!  Fonction interne de lecture et de remplissage d'une structure 
!  "pôle et temps sidéral"
!
!$Description
!  Fonction interne de lecture et de remplissage d'une structure 
!  "pôle et temps sidéral"
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PRIVEE
!
!$Usage
!  call cpsi_lectureValeursPoleTsid(nom_fichier, indice_prem_ligne, nb_lignes_a_lire, &
!.         date_min, date_max, tab_poletsid)
!.    character(len=*) :: nom_fichier
!.    integer :: indice_prem_ligne
!.    integer :: nb_lignes_a_lire
!.    real(KIND=pm_reel) :: date_min, date_max
!.    type(cps_struct_poletsid_fichier) :: tab_poletsid
!
!$Arguments
!>E     nom_fichier        :<LEN=*>                   Nom du fichier             
!>E     indice_prem_ligne  :<integer>                 Indice de la première ligne à lire
!>E     nb_lignes_a_lire   :<integer>                 Nombre de lignes à lire
!>E/S   date_min           :<pm_reel>                 Date minimum qui a permis de calculer le nombre de lignes  
!>E/S   date_max           :<pm_reel>                 Date maximum  
!>S     tab_poletsid       :<cps_struct_poletsid_fichier> Structure à remplir
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
!- cps_utilisateur
!#
!
!$Remarques
!  On suppose que les indices indice_prem_ligne et nb_lignes_a_lire sont
!  coherents avec le fichier. 
!  Les dates min et max sont fournies pour les vérifications.
!  On suppose que la structure tab_poletsid est déjà initialisée (pointeurs à null).
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Rq : on suppose qu'une premiere lecture a deja eu lieu, ce qui permet d'alleger les test de codes de retour

! Rq : On suppose que la structure a deja ete désallouée au dessus

use cps_utilisateur

implicit none

    ! ---------------------
    ! Entrées
    character(len=*), intent(in) :: nom_fichier
    integer, intent(in) :: indice_prem_ligne
    integer, intent(in) :: nb_lignes_a_lire
    real(KIND=pm_reel) :: date_min, date_max
    
    ! ---------------------
    ! Sorties
    type(cps_struct_poletsid_fichier), intent(out) :: tab_poletsid
    
    ! ---------------------
    ! Variables 
    integer :: ii             ! Indice de boucle
    integer :: code_retour    ! Code de retour en lecture
    integer :: acces_mad      ! Numéro d'accès MADONA
    integer :: nb_lignes_lues ! Nombre de lignes lues

    ! ------------------------------------------------
    ! Désallocation du tableau précédent
    if ( associated(tab_poletsid%dates ) )  deallocate(tab_poletsid%dates)  
    if ( associated(tab_poletsid%alphas ) ) deallocate(tab_poletsid%alphas) 
    if ( associated(tab_poletsid%deltas ) ) deallocate(tab_poletsid%deltas) 
    if ( associated(tab_poletsid%tsid ) )   deallocate(tab_poletsid%tsid) 
    if ( associated(tab_poletsid%dtsid ) )  deallocate(tab_poletsid%dtsid) 

    ! Allocation du tableau de la structure de sortie
    tab_poletsid%npoletsid = nb_lignes_a_lire
    allocate(tab_poletsid%dates(nb_lignes_a_lire) )
    allocate(tab_poletsid%alphas(nb_lignes_a_lire) )
    allocate(tab_poletsid%deltas(nb_lignes_a_lire) )
    allocate(tab_poletsid%tsid(nb_lignes_a_lire) )
    allocate(tab_poletsid%dtsid(nb_lignes_a_lire) )
    
    ! Initialisation des tableaux a zero 
    do ii = 1, nb_lignes_a_lire
       tab_poletsid%dates(ii) = 0._pm_reel
       tab_poletsid%alphas(ii) = 0._pm_reel
       tab_poletsid%deltas(ii) = 0._pm_reel
       tab_poletsid%tsid(ii) = 0._pm_reel
       tab_poletsid%dtsid(ii) = 0._pm_reel
    end do

    ! Remise a zero des compteurs et re-connexion
    acces_mad = acc_open()
    ! Le fichier a été ouvert lors de la pré-analyse. Pas besoin de tester
    code_retour = acc_connect (acces_mad, nom_fichier, ACC_R)

    ! ---------------------------------------------
    ! Avancée jusqu'à l'indice de la premère ligne
    nb_lignes_lues = 0

    do while ( nb_lignes_lues < indice_prem_ligne -1 )
       ! NOTE : On s'est assuré précédemment que l'on arriverait pas a la fin du fichier
       ! Donc pas de verification sur l'atteinte de fin de fichier
       code_retour = acc_read( acces_mad, ACC_NEXT ) 
       
       ! Incrémente le compteur
       nb_lignes_lues = nb_lignes_lues +1
    end do

    ! ---------------------------------------------
    ! Lecture des lignes à partir de la position courante jusqu'à atteindre 
    ! la fin du fichier
    ! (ou, si datemax est present, jusqu'au bon nombre de lignes voulues)
    nb_lignes_lues = 0
    do while ( nb_lignes_lues < nb_lignes_a_lire )

       ! NOTE : On s'est assuré précédemment que l'on arriverait pas a la fin du fichier
       ! Toutefois, on s'assure ne pas dépasser du tableau
       code_retour = acc_read( acces_mad, ACC_NEXT ) 
       if ( code_retour < 0 ) then
          call MSP_signaler_message(cle_mes="CPS_ERR_LECT_POLETSID", &
             routine="cpsi_lectureValeursPoleTsid", &
             partie_variable=trim(nom_fichier))                  
       endif

       ! Incrémente
       nb_lignes_lues = nb_lignes_lues +1

       ! Lecture de la date
       code_retour = acc_getd(acces_mad, "date", tab_poletsid%dates(nb_lignes_lues), "jj1950" )
       ! Lecture des angles alpha et delta
       code_retour = acc_getd(acces_mad, "alpha", tab_poletsid%alphas(nb_lignes_lues), "rad" )
       code_retour = acc_getd(acces_mad, "delta", tab_poletsid%deltas(nb_lignes_lues), "rad" )
       ! Lecture du temps sidéral et de sa dérivée
       code_retour = acc_getd(acces_mad, "tsid", tab_poletsid%tsid(nb_lignes_lues) , "rad")
       code_retour = acc_getd(acces_mad, "dtsid", tab_poletsid%dtsid(nb_lignes_lues), "rad/s" )
              
    end do

    ! Vérification de cohérence des dates du header et celle du fichier
    ! Note : Nécessaire seulement dans les cas ou les dates min et/ou max ne sont pas forcées
    if ( (tab_poletsid%dates(1) < date_min) &
       .or. (tab_poletsid%dates(nb_lignes_lues) > date_max) ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_DATES_HEADER_POLETSID", &
             routine="cpsi_lectureValeursPoleTsid", &
             partie_variable=trim(nom_fichier))   
       return
    end if

    ! Fermer l'accès au fichier     
    code_retour = acc_deconnect(acces_mad, ACC_R)
    code_retour = acc_close(acces_mad)

end subroutine cpsi_lectureValeursPoleTsid
 

subroutine cps_interpolPoletsid(tab_poletsid, date, alpha, delta, tsid, dtsid)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_interpolPoletsid
!
!$Resume
!  Routine d'interpolation linéaire pour la position des pôles et du temps sidéral.
!
!$Description
!  Routine d'interpolation linéaire. Aucune extrapolation n'est réalisée.
!  Si la date demandée est hors du créneau disponible, alors une erreur est générée.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_interpolPoletsid(tab_poletsid, date, alpha, delta, tsid, dtsid)
!.    type(cps_struct_poletsid_fichier) :: tab_poletsid 
!.    real(KIND=pm_reel) :: date 
!.    real(KIND=pm_reel) :: alpha 
!.    real(KIND=pm_reel) :: delta 
!.    real(KIND=pm_reel) :: tsid 
!.    real(KIND=pm_reel) :: dtsid 
!
!$Arguments
!>E     tab_poletsid  :<cps_struct_poletsid_fichier>   Struture regrouppant les poistions du pole et temps sideral 
!>E     date          :<pm_reel>                   Date utilisée pour l'interpolation (jj frac)    
!>S     alpha         :<pm_reel>                   Premier angle donnant la position du pole (rad)
!>S     delta         :<pm_reel>                   Deuxieme angle donnant la position du pole (rad)  
!>S     tsid          :<pm_reel>                   Temps sidéral (rad)  
!>S     dtsid         :<pm_reel>                   Dérivée du temps sidéral (rad/s)
!
!$Common
!
!$Routines
!- mu_inter_dim1_lin
!- mzipro_val_retour
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- parametre_mspro
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

use mspro, only : &
     mu_inter_dim1_lin, pm_warn_extrapol, &
     pm_warn_extrapol_borne_double, pm_avant

use cps_utilisateur

    implicit none

    !-------------------------------
    ! Paramètres d'entrée
    ! Struture regrouppant les poistions du pole et temps sideral 
    type(cps_struct_poletsid_fichier), intent(in) :: tab_poletsid  
    real(KIND=pm_reel), intent(in) :: date     ! Date utilisée pour l'interpolation (jj frac)

    ! Paramètres en sortie
    real(KIND=pm_reel), intent(out) :: alpha   ! Premier angle donnant la position du pole (rad)
    real(KIND=pm_reel), intent(out) :: delta   ! Deuxieme angle donnant la position du pole (rad)
    real(KIND=pm_reel), intent(out) :: tsid    ! Temps sidéral (rad)
    real(KIND=pm_reel), intent(out) :: dtsid   ! Dérivée du temps sidéral (rad/s)

    !-------------------------------
    ! Variables locales
    type(tm_code_retour) :: code_retour_mspro
    integer :: indice_depart   ! Indice dans le tableau a partir duquel se fait la recherche    
    integer :: dir_pt_double   ! Direction employée en cas de point double
    
    !-------------------------------
    ! Début du code

    ! Verification des parametres d'entree
    if ( date < tab_poletsid%dates(1) &
         .or. date > tab_poletsid%dates(tab_poletsid%npoletsid) ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_EXTRAPOL_POLETSID") 
       return
    endif

    ! Aucune info sur l'indice de départ
    indice_depart = 1
    ! Specification d'une direction dans le cas d'un point double. Par défaut -1.
    dir_pt_double = pm_avant
    ! Init du code de retour, les autres ne sont pas nécessaires
    code_retour_mspro%valeur = 0
    
    ! Interpolation pour l'angle alpha
    call mu_inter_dim1_lin(tab_poletsid%npoletsid, tab_poletsid%dates, &
         tab_poletsid%alphas, date, indice_depart, alpha, code_retour_mspro,.false., dir_pt_double)
    ! Traitement des codes de retour
    if ( code_retour_mspro%valeur /= 0 ) then 
       call MSP_signaler_message(ier_mslib=code_retour_mspro)
       if ( MSP_gen_messages("cps_interpolPoletsid") ) return
    endif

    ! Interpolation pour l'angle delta
    ! On reutilise l'indice trouve precedement pour optimisation (voir MSPRO)
    ! Les tableaux sont de même taille et correspondent au même vecteur de dates
    call mu_inter_dim1_lin(tab_poletsid%npoletsid, tab_poletsid%dates, &
         tab_poletsid%deltas, date, indice_depart, delta, code_retour_mspro,.false., dir_pt_double)
    ! Traitement des codes de retour
    if ( code_retour_mspro%valeur /= 0 ) then 
       call MSP_signaler_message(ier_mslib=code_retour_mspro)
       if ( MSP_gen_messages("cps_interpolPoletsid") ) return
    endif

    ! Interpolation pour tsid
    call mu_inter_dim1_lin(tab_poletsid%npoletsid, tab_poletsid%dates, &
         tab_poletsid%tsid, date, indice_depart, tsid, code_retour_mspro, .false., dir_pt_double)
    ! Traitement des codes de retour
    if ( code_retour_mspro%valeur /= 0 ) then 
       call MSP_signaler_message(ier_mslib=code_retour_mspro)
       if ( MSP_gen_messages("cps_interpolPoletsid") ) return
    endif


    ! Interpolation pour dtsid
    call mu_inter_dim1_lin(tab_poletsid%npoletsid, tab_poletsid%dates, &
         tab_poletsid%dtsid, date, indice_depart, dtsid, code_retour_mspro, .false., dir_pt_double)
    ! Traitement des codes de retour
    if ( code_retour_mspro%valeur /= 0 ) then 
       call MSP_signaler_message(ier_mslib=code_retour_mspro)
       if ( MSP_gen_messages("cps_interpolPoletsid") ) return
    endif


end subroutine cps_interpolPoletsid



subroutine cps_getModelesPoletsidCorps(code_corps, nb_modeles, liste_modeles)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getModelesPoletsidCorps
!
!$Resume
! Routine interrogeant la base en renovoyant la liste des modèles "poletsid" du corps.
!
!$Description
! Routine interrogeant la base en renovoyant la liste des modèles "poletsid" du corps.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getModelesPoletsidCorps(code_corps, nb_modeles, liste_modeles)
!.    integer :: code_corps 
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: liste_modeles 
!.    integer :: nb_modeles 
!
!$Arguments
!>E     code_corps     :<integer>                         Code NAIF du corps concerné
!>S     nb_modeles     :<integer>                         Nombre de modèles associés au corps       
!>E/S   liste_modeles  :<LEN=CPS_MAXLG,DIM=(:),pointer>   Liste des modèles disponibles pour ce corps      
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!
!$Include
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

    !-------------------------------
    ! Paramètres en entrée
    integer, intent(in) :: code_corps ! Code du corps concerné

    !-------------------------------
    ! Paramètres en sortie
    ! Liste des modèles disponibles pour ce corps
    character(LEN=CPS_MAXLG), dimension(:), pointer :: liste_modeles 
    integer, intent(out) :: nb_modeles      ! Nombre de modeles pour le corps

    !-------------------------------
    ! Variables locales
    integer :: ii               ! Indices de boucle
    integer :: acces            ! Acces MADONA a un fichier 
    ! Liste des fichiers à analyser
    character(LEN=CPS_MAXLG), dimension(:), pointer :: liste_fic_config => NULL()
    integer :: indice_corps    ! Indice du corps courant
    integer :: code_retour     ! Code retour de acc_get
    ! Pour le scan
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature
    character (LEN=CPS_MAXLG), dimension(3) :: msp_mess ! Structure pour message d'erreur
    
    !-------------------------------
    ! L'algorithme est le suivant :
    !  - Recherche des structures concernant ce corps 
    !    pour obtenir un nombre max.
    !  - Allocation du tableau final au nombre de modèles disponibles
    !  - Pour chaque modele dispo
    !     - Ajout au tableau
    !-------------------------------

    ! Init de la structure en sortie
    if (associated(liste_modeles))  deallocate(liste_modeles)

    ! Vérification de l'initialisation de la base
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getModelesPoletsidCorps", &
            partie_variable="cps_utilisateur")
       return
    end if

    ! Extraction de la liste des fichiers de ce type
    call cps_getListFichiersCateg(CPS_CATEG_POLETSID, liste_fic_config)

    ! 1er temps : extraction du nombre de corps traites sans suprr des doublons
    nb_modeles = 0
    do ii=1, cpsi_size_ptr(liste_fic_config)

       ! Lecture madona du, ou des fichiers
       call cpsi_getAccesMadona(liste_fic_config(ii), acces)
       if (acces < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getModelesPoletsidCorps", &
               partie_variable=trim(liste_fic_config(ii)))
          return
       end if

       ! Init du parcours
       code_retour = acc_scan_reset(acces)
       boucle1 : do
          ! Pour la structure courante
          code_retour = acc_scan(acces, libelle, nature)

          ! Si fin de parcours, sortie normale de la boucle
          if (nature.eq.CPS_FIN_SEQ) exit boucle1

          ! Si le champs n'est pas une structure, on passe au suivant
          if (nature /= ACC_STRUCT) then
             cycle boucle1
          endif

          ! Si il s'agit du meme corps que celui en parametres
          code_retour = acc_geti(acces, trim(libelle)//".code", indice_corps)
          if ( indice_corps == code_corps) then
             ! On a un modèle supplémentaire potentiel (avant suppr des doublons)
             nb_modeles = nb_modeles +1
          end if
       end do boucle1
      
    end do
 
    ! Cas ou aucun modèle n'est trouvé
    if ( nb_modeles == 0 ) return

    ! Allocation d'une première liste avec la limite haute
    allocate(liste_modeles(nb_modeles))
    do ii = 1, nb_modeles
       liste_modeles(ii) = ''
    end do
       
    ! 2ième temps : Relecture et recopie des valeurs
    nb_modeles = 0
    do ii=1, cpsi_size_ptr(liste_fic_config)
       ! Lecture madona du, ou des fichiers
       call cpsi_getAccesMadona(liste_fic_config(ii), acces)
       ! On a deja teste l'acces au fichier

       ! Init du parcours
       code_retour = acc_scan_reset(acces)
       boucle2 : do
          ! Pour la structure courante
          code_retour = acc_scan(acces, libelle, nature)

          ! Si fin de parcours, sortie normale de la boucle
          if (nature.eq.CPS_FIN_SEQ) exit boucle2
          
          ! Si le champs n'est pas une structure, on passe au suivant
          if (nature /= ACC_STRUCT) then
             cycle boucle2
          endif

          ! Si il s'agit du meme corps que celui en parametres
          code_retour = acc_geti(acces, trim(libelle)//".code", indice_corps)
          ! Le code a deja été lu, pas besoin de tester
          
          ! On ne traite pas un autre corps que celui recherché
          if ( indice_corps /= code_corps) then
             cycle boucle2
          endif
          
          ! Alors, on a un modèle suppl.
          ! Extraction du modèle
          nb_modeles = nb_modeles +1
          code_retour = acc_gets(acces, trim(libelle)//".modele_poletsid_fichier", &
               liste_modeles(nb_modeles) )
          
          ! Traitement du cas ou le modèle manque
          if ( code_retour == ACC_ERR ) then
            
             ! Creation du message d'erreur
             write(msp_mess(1),*) code_retour
             write(msp_mess(2),*) trim(libelle)//".modele_poletsid_fichier"
             write(msp_mess(3),*) trim(liste_fic_config(ii))
             call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                  routine="cps_lireFichierPoletsid", &
                  partie_variable=msp_mess)
             return
          end if 

       end do boucle2 ! Fin du parcours du fichier
       
    end do ! Fin du parcours de la liste des fichiers

    ! Désallocation des tableaux
    deallocate(liste_fic_config)

end subroutine cps_getModelesPoletsidCorps


  
subroutine cps_getListeCorpsPoletsid(nb_corps, liste_code_corps)
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListeCorpsPoletsid
!
!$Resume
!  Routine interrogeant la base en renovoyant la liste des corps disposant 
!  du modèle "poletsid"
!
!$Description
!  Routine interrogeant la base en renovoyant la liste des corps disposant 
!  du modèle "poletsid"
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListeCorpsPoletsid(nb_corps, liste_code_corps)
!.    integer :: nb_corps 
!.    integer, dimension(:), pointer :: liste_code_corps 
!
!$Arguments
!>S     nb_corps          :<integer>                   
!>E/S   liste_code_corps  :<integer,DIM=(:),pointer>   Liste des codes NAIF des corps  
!  - Allocation du tableau final au nombre de modèles dispo et différents
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
!#
!
!$Remarques
!  Pour des raisons algorithmiques, la taille du tableau après allocation
!  peut être supérieure au nombre de corps au final. Il ne faut utiliser que 
!  le nombre spécifié en sortie.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use cps_utilisateur

    implicit none
    
    !-------------------------------
    ! Paramètres en sortie 
    ! Liste des codes des corps disposant de ce modèle
    integer, intent(out) :: nb_corps        ! Nombre de corps pour le modele
    integer, dimension(:), pointer :: liste_code_corps  

    !-------------------------------
    ! Variables locales
    integer :: ii, jj           ! Indices de boucle
    integer :: acces            ! Acces MADONA a un fichier 
    ! Liste des fichiers à analyser
    character(LEN=CPS_MAXLG), dimension(:), pointer :: liste_fic_config => NULL()
    integer :: indice_corps    ! Indice du corps courant
    integer :: code_retour     ! Code retour de  acc_get
    ! Pour le scan
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature
    logical :: trouve
    ! Premiere liste obtenue
    integer, dimension(:), pointer :: liste_avec_doublons => NULL()

    ! Init de la structure en sortie
    if (associated(liste_code_corps)) deallocate(liste_code_corps)
    nb_corps = 0
    
    ! Vérification de l'initialisation de la base
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getListeCorpsPoletsid", &
            partie_variable="cps_utilisateur")
       return
    end if

    ! Extraction de la liste des fichiers de ce type
    call cps_getListFichiersCateg(CPS_CATEG_POLETSID, liste_fic_config)

    ! 1er temps : extraction du nombre de corps traites sans suppr des doublons
    nb_corps = 0
    do ii=1, cpsi_size_ptr(liste_fic_config)

       ! Lecture madona du, ou des fichiers
       call cpsi_getAccesMadona(liste_fic_config(ii), acces)
       if (acces < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getListeCorpsPoletsid", &
               partie_variable=trim(liste_fic_config(ii)))
          return
       end if

       ! Init du parcours
       code_retour = acc_scan_reset(acces)
       boucle1 : do
          ! Pour la structure courante
          code_retour = acc_scan(acces, libelle, nature)

          ! Si fin de parcours, sortie normale de la boucle
          if (nature.eq.CPS_FIN_SEQ) exit boucle1
          
          ! Si le champs n'est pas une structure, on passe au suivant
          if (nature /= ACC_STRUCT) then
             cycle boucle1
          endif


          ! Si c'est une structure extraction du code du corps correspondant
          code_retour = acc_geti(acces, trim(libelle)//".code", indice_corps)
          ! Si la structure ne contient pas de code : la base est corrompue
          if (code_retour /= 0) then
             call MSP_signaler_message(cle_mes="CPS_ERR_BASE_POLETSID", &
                  routine="cps_getListeCorpsPoletsid", &
                  partie_variable=trim(liste_fic_config(ii)))
             return
          endif

          ! Le code du corps existe, on peut incrémenter
          nb_corps = nb_corps +1

       end do boucle1
      
    end do

    ! Dans le cas ou aucun modele poletsid n'est présent
    if ( nb_corps == 0 ) return
    
    ! Allocation d'une première liste avec la limite haute
    allocate(liste_avec_doublons(nb_corps))
    do ii = 1, nb_corps
       liste_avec_doublons(ii) = 0
    end do

    ! 2ième temps : Relecture et recopie des valeurs
    nb_corps = 0
    do ii=1, cpsi_size_ptr(liste_fic_config)
       ! Lecture madona du, ou des fichiers
       call cpsi_getAccesMadona(liste_fic_config(ii), acces)
       ! On a deja teste l'acces au fichier

       ! Init du parcours
       code_retour = acc_scan_reset(acces)
       boucle2 : do
          ! Pour la structure courante
          code_retour = acc_scan(acces, libelle, nature)

          ! Si fin de parcours, sortie normale de la boucle
          if (nature == CPS_FIN_SEQ)  exit boucle2

          ! Si le champs n'est pas une structure, on passe au suivant
          if (nature /= ACC_STRUCT) then
             cycle boucle2
          endif

          
          ! Si c'est une structure extraction du code du corps correspondant
          code_retour = acc_geti(acces, trim(libelle)//".code", indice_corps)
          ! La lecture de ce champs a déjà été effectuée, inutile de tester 

          ! Verification que le code n'est pas deja present
          trouve = .false.
          jj = 1
          do while ( ( .not. trouve ) &
               .and. ( jj <= nb_corps) )
             ! Test de la présence du corps
             if ( liste_avec_doublons(jj) == indice_corps ) then
                trouve = .true.
             else
                jj = jj +1
             end if

          end do

          if ( .not. trouve) then
             ! On ajoute le corps a la liste
             nb_corps = nb_corps +1
             liste_avec_doublons(nb_corps) = indice_corps
          end if

       end do boucle2  ! Fin du parcours du fichier
       
    end do ! Fin du parcours de la liste des fichiers

    ! Allocation du tableau final
    allocate(liste_code_corps(nb_corps))
    liste_code_corps(1:nb_corps) = liste_avec_doublons(1:nb_corps)

    ! Désallocation des tableaux
    deallocate(liste_fic_config)
    deallocate(liste_avec_doublons)

  end subroutine cps_getListeCorpsPoletsid

end module cps_poletsid_fichier
