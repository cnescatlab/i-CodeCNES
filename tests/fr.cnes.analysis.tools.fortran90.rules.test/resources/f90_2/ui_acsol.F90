program ui_acsol

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_acsol
!
!$Resume
!  Utilitaire d'accès aux données issues du fichier ACSOL2
!
!$Description
!  Utilitaire d'accès aux données issues du fichier ACSOL2
!
!  Usage
!
!.  ui_acsol (-sure (-date d | -date_deb dd -date_fin df) | 
!.            -pred ([-date d] (-last | -date_pred dd )))
!.            [-all] [-nv] [-err] [-flux] [-fluxm] [-iaamoy] [-iaa] [-iapmoy]
!> -sure         : données sûres
!> -pred         : données prédites
!> -date d       : date à afficher  (JJ 1950)
!> -all          : toutes les données disponibles (sinon données demandées par
!                  les options suivantes)
!> -flux         : flux
!> -fluxm        : flux moyen
!> -iaamoy       : AA moyen
!> -iaa          : AA
!> -iapmoy       : AP moyen
!> -last         : dernière prédiction disponible
!> -date_pred dd : date de la prédiction souhaitée (JJ 1950)
!> -nv           : mode non bavard
!> -date_deb  dd : créneau de dates à afficher, début (JJ 1950)
!> -date_fin  df : créneau de dates à afficher, fin (JJ 1950)
!
!$Auteur
!
!$Version
!  $Id: ui_acsol.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_acsol.F90,v $
!  Revision 1.8  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.7  2008/10/31 13:08:36  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.6  2008/04/28 13:01:30  vivaresf
!  FA-ID 664 : présentation des sorties et cartouches
!
!  Revision 1.5  2008/04/11 10:57:54  vivaresf
!  FA-ID 778 : suppression des variables inutilisées
!
!  Revision 1.4  2008/04/11 10:09:32  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!
!  Revision 1.3  2008/04/10 16:48:55  vivaresf
!  Version 2.4 : validation
!
!  Revision 1.2  2008/04/10 10:19:46  vivaresf
!  FA-ID 664 : correction de la présentation, accents
!  AQ : mise à jour des cartouches
!
!  Revision 1.1  2008/02/08 17:51:35  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.9  2007/09/18 12:52:48  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.8  2006/10/24 08:57:13  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!  Revision 1.7  2006/06/30 14:48:57  vpg
!  Mise a jour des cartouches
!  Revision 1.6  2006/06/16 17:31:59  vivaresf
!  Cartouches d'entete
!  Revision 1.5  2006/05/31 13:08:36  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.4  2006/04/25 13:49:37  bouillaj
!  Mise a jour du cartouche
!  Revision 1.3  2006/04/06 16:04:50  vpg
!  Correction de l'affichage de l'indice AP moyen
!  Revision 1.2  2006/03/20 16:12:53  vpg
!  Mise a jour des cartouches
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
  use cps_utilisateur
  use ui_ioplus

  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_acsol.F90 69 2012-09-11 08:33:34Z ffsm $'

  
#include "formats.h"  

  logical :: opt_sure, opt_pred, opt_date, opt_all, opt_flux, opt_fluxm,&
       opt_iaamoy, opt_iaa, opt_iapmoy, opt_date_deb, opt_date_fin,     &
       opt_nv, opt_last, opt_date_pred
  integer :: ierfin, noptions, i, trouve
  character(LEN=20), dimension(50) :: l_opt
  
  real(KIND=PM_REEL) :: date, date_pred, date_deb, date_fin
  real(KIND=PM_REEL), dimension(2) :: data_flux
  integer, dimension(10) :: data_ia_sure
  integer, dimension(9) :: data_ia_pred
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE) :: tab_date_sure
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED) :: tab_date_pred
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE, 2) :: tab_data_sure_flux
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED, 2) :: tab_data_pred_flux
  integer, dimension(CPS_MAX_ACSOL2_SURE, 10) :: tab_data_sure_ia
  integer, dimension(CPS_MAX_ACSOL2_PRED, 9) :: tab_data_pred_ia
  integer :: nb_pred, type0, nb_data
  
  ! initialisation
  call cps_init_utilisateur()
  trouve = CPS_ERR_DEF
  nb_pred = 0
  type0 = 0

  opt_sure = .false.
  opt_pred = .false.
  opt_date = .false.
  opt_nv   = .false.
  ! par defaut : toutes les donnees
  opt_all    = .true.
  opt_flux   = .false.
  opt_fluxm  = .false.
  opt_iaamoy = .false.
  opt_iaa    = .false.
  opt_iapmoy = .false.

  opt_date_deb = .false.
  opt_date_fin = .false.
  
  opt_last = .false.
  opt_date_pred = .false.
  
  ! analyse des arguments
  ! aide
  call ui_test_arguments ("-h", "", ierfin)
  if(ierfin.eq.1) then
     call ui_ecrire_help("ui_acsol")
     goto 999
  else
     ! lecture des arguments
     call ui_lire_options(noptions, l_opt)
     
     if (noptions == 0) then
        ! aucun arguments
        call ui_ecrire_help("ui_acsol")
        goto 999
     else
        do i=1, noptions
           select case (l_opt(2*i-1))
           case ("-sure")
              opt_sure = .true.
              
           case ("-pred")
              opt_pred = .true.
              
           case ("-date")
              opt_date = .true.
              read(l_opt(2*i),*) date
              
           case ("-all")
              opt_all = .true.
              
           case ("-flux")
              opt_flux = .true.
              opt_all = .false.
              
           case ("-fluxm")
              opt_fluxm = .true.
              opt_all = .false.
              
           case ("-iaamoy")
              opt_iaamoy = .true.
              opt_all = .false.

           case ("-iaa")
              opt_iaa = .true.
              opt_all = .false.
              
           case ("-iapmoy")
              opt_iapmoy = .true.
              opt_all = .false.
              
           case ("-nv")
              opt_nv = .true.
              
           case ("-last")
              opt_last = .true.
              
           case ("-date_pred")
              opt_date_pred = .true.
              read(l_opt(2*i),*) date_pred

           case ("-date_deb")
              opt_date_deb = .true.
              read(l_opt(2*i),*) date_deb

           case ("-date_fin")
              opt_date_fin = .true.
              read(l_opt(2*i),*) date_fin

           end select
        end do
     end if
  end if
  
  if (opt_all) then
     opt_flux   = .true.
     opt_fluxm  = .true.
     opt_iaamoy = .true.
     opt_iaa    = .true.
     opt_iapmoy = .true.
  end if

  if (opt_sure) then
     ! donnees sures
     ! il faut qu'il y ait une date
     if ((.not.opt_date).and.(.not.opt_date_deb).and.(.not.opt_date_fin)) then
        write(0,1000) "Erreur : il faut spécifier une date!"
        goto 999
     end if
     if (opt_date) then
        ! une seule date
        type0 = 1
        trouve = cps_getAcsol2Sure(date, data_flux, data_ia_sure)
        if (trouve.ne.CPS_OK) then
           write(0,1000) "Erreur : donnée introuvable"
           goto 999
        end if
     else
        if (opt_date_deb.and.(.not.opt_date_fin)) then
           ! il manque 'date_fin'
           write(0,1000) "Erreur : il faut spécifier la date de fin!"
           goto 999
        else if (opt_date_fin.and.(.not.opt_date_deb)) then
           ! il manque 'date_deb'
           write(0,1000) "Erreur : il faut spécifier la date de debut!"
           goto 999
        else
           ! creneau de dates
           type0 = 2
           trouve = cps_getAcsol2SureCreneau(date_deb, date_fin, tab_date_sure, &
                tab_data_sure_flux, tab_data_sure_ia, nb_data)
           if (trouve.ne.CPS_OK) then
              write(0,1000) "Erreur : donnée introuvable"
              goto 999
           end if
        end if
     end if
  end if
     
  if (opt_pred) then
     opt_iapmoy = .false.
     ! donnees predites
     if (opt_last.and.(.not.opt_date)) then
        ! toutes les dernieres predictions
        type0 = 3
        call cps_getAcsol2DernieresPred(date_pred, tab_date_pred, tab_data_pred_flux, &
             tab_data_pred_ia, nb_pred)
        trouve = CPS_OK
     else if (opt_last.and.opt_date.and.(.not.opt_date_pred)) then
        ! les dernieres predictions effectuees pour une date particuliere
        type0 = 4
        trouve = cps_getAcsol2DernierePred(date, date_pred, data_flux, data_ia_pred)
     else if (opt_date.and.opt_date_pred) then
        ! les predictions effectuees a la date 'date_pred' pour la date 'date'
        type0 = 5
        trouve = cps_getAcsol2DatePred(date, date_pred, data_flux, data_ia_pred)
     else if ((.not.opt_date).and.opt_date_pred) then
        ! toutes les predictions effectuees a la date 'date_pred'
        type0 = 6
        trouve = cps_getAcsol2Pred(date_pred, tab_date_pred, tab_data_pred_flux,   &
             tab_data_pred_ia, nb_pred)        
     end if

     if (trouve.ne.CPS_OK) then
        write(0,1000) "Erreur : donnée introuvable"
        goto 999
     end if
  end if
  
  
  ! affichage des resultats

  select case (type0)
  case (1)
     if (.not.opt_nv) then
        write(*,1002) "Données sûres pour la date : ", date
     else
        write(*,1029) date
     end if
  case (2)
     if (.not.opt_nv) then
        write(*,1049) "Données sûres sur le créneau [ ", date_deb, " ; ", date_fin, " ]"
     else
        write(*,1029) date_deb
        write(*,1029) date_fin
     end if
  case (3)
     if (.not.opt_nv) then
        write(*,1000) "Toutes les dernières prédictions"
        write(*,1002) "Prédictions effectuées à la date ", date_pred
        write(*,1001) "Nombre de prédictions : ", nb_pred
     else
        write(*,1029) date_pred
        write(*,1057) nb_pred
     end if
  case (4)
     if (.not.opt_nv) then
        write(*,1002) "Dernières prédictions pour la date ", date
        write(*,1002) "Prédictions effectuées à la date ", date_pred
     else
        write(*,1029) date
        write(*,1029) date_pred
     end if
  case (5)
     if (.not.opt_nv) then
        write(*,1006) "Prédictions pour la date ", date, "effectuées à la date ", date_pred
     else
        write(*,1029) date
        write(*,1029) date_pred
     end if
  case (6)
     if (.not.opt_nv) then
        write(*,1002) "Toutes les prédictions effectuées à la date ", date_pred
        write(*,1001) "Nombre de prédictions : ", nb_pred
     else
        write(*,1029) date_pred
        write(*,1057) nb_pred
     end if
  end select


  ! affichage de la legende
  if (.not.opt_nv) then
     call cpsi_ui_afficherLegendeAcsol2(opt_flux, opt_fluxm, opt_iaamoy, opt_iaa,  &
          opt_iapmoy)
  end if

  
  if (type0.eq.1) then
    call cpsi_ui_afficherLigneAcsol2(opt_flux, opt_fluxm, opt_iaamoy, opt_iaa,     &
          opt_iapmoy, date, data_flux, data_ia_sure)
  end if
  
  if ((type0.eq.4).or.(type0.eq.5)) then
     call cpsi_ui_afficherLigneAcsol2(opt_flux, opt_fluxm, opt_iaamoy, opt_iaa,    &
          opt_iapmoy, date, data_flux, data_ia_pred)
  end if
  
  if (type0.eq.2) then
     do i=1,nb_data
        call cpsi_ui_afficherLigneAcsol2(opt_flux, opt_fluxm, opt_iaamoy, opt_iaa, &
             opt_iapmoy, tab_date_sure(i), tab_data_sure_flux(i,1:2),              &
             tab_data_sure_ia(i,1:10))
     end do
  end if
  
  if ((type0.eq.3).or.(type0.eq.6)) then
     do i=1,nb_pred
        call cpsi_ui_afficherLigneAcsol2(opt_flux, opt_fluxm, opt_iaamoy, opt_iaa, &
            opt_iapmoy,  tab_date_pred(i), tab_data_pred_flux(i,1:2),              &
            tab_data_pred_ia(i,1:9))
     end do
  end if


999 continue
  
 
  ! fin
  call cps_close_utilisateur()

! 100 format (8D23.16)

end program ui_acsol



subroutine cpsi_ui_afficherLegendeAcsol2(opt_flux, opt_fluxm, opt_iaamoy, opt_iaa, opt_iapmoy)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ui_afficherLegendeAcsol2
!
!$Resume
!  Affichage des titres des colonnes de données
!$Description
!  Affichage des titres des colonnes de données
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ui_afficherLegendeAcsol2(opt_flux, opt_fluxm, opt_iaamoy, opt_iaa, opt_iapmoy)
!.    logical :: opt_flux, opt_fluxm, opt_iaamoy, opt_iaa, opt_iapmoy
!
!$Arguments
!>E     opt_flux    :<logical>   colonne flux demandée
!>E     opt_fluxm   :<logical>   colonne flux moyen demandée
!>E     opt_iaamoy  :<logical>   colonne AA moyen demandée
!>E     opt_iaa     :<logical>   colonne AA demandée
!>E     opt_iapmoy  :<logical>   colonne AP moyen demandée
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

  ! arguments
  logical, intent(in) :: opt_flux, opt_fluxm, opt_iaamoy, opt_iaa, opt_iapmoy

  ! variables locales
  character(LEN=256) :: legende
  
  ! initialisation
  legende = "date"
  
  if (opt_flux) then
     legende = trim(legende)//"  flux (~sfu)"
  end if

  if (opt_fluxm) then
     legende = trim(legende)//"  flux moyen (~sfu)"
  end if

  if (opt_iaamoy) then
     legende = trim(legende)//"  indice AA moyen"
  end if
  
  if (opt_iaa) then
     legende = trim(legende)//"  indice AA trihoraire"
  end if

  if (opt_iapmoy) then
     legende = trim(legende)//"  indice AP moyen"
  end if

  ! ecriture
  write(*,1000) trim(legende)

end subroutine cpsi_ui_afficherLegendeAcsol2




subroutine cpsi_ui_afficherLigneAcsol2(opt_flux, opt_fluxm, opt_iaamoy, &
     opt_iaa, opt_iapmoy, date, data_flux, data_ia)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ui_afficherLigneAcsol2
!
!$Resume
!  Affichage des colonnes de données
!$Description
!  Affichage lignes à lignes des données
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ui_afficherLigneAcsol2(opt_flux, opt_fluxm, opt_iaamoy, &
!.         opt_iaa, opt_iapmoy, date, data_flux, data_ia)
!.    logical :: opt_flux, opt_fluxm, opt_iaamoy, opt_iaa, opt_iapmoy
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL), dimension(2) :: data_flux
!.    integer, dimension(10) :: data_ia
!
!$Arguments
!>E     opt_flux    :<logical>      colonne flux demandée      
!>E     opt_fluxm   :<logical>      colonne flux moyen demandée      
!>E     opt_iaamoy  :<logical>      colonne AA moyen demandée     
!>E     opt_iaa     :<logical>      colonne AA demandée      
!>E     opt_iapmoy  :<logical>      colonne AP moyen demandée      
!>E     date        :<PM_REEL>      date à afficher    
!>E     data_flux   :<PM_REEL,DIM=(2)>    tableau des flux
!>E     data_ia     :<integer,DIM=(10)>   tableau des indices (AA ou AP)
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSLIB
  implicit none

#include "formats.h"

  ! arguments
  logical, intent(in) :: opt_flux, opt_fluxm, opt_iaamoy, opt_iaa, opt_iapmoy
  real(KIND=PM_REEL), intent(in) :: date
  real(KIND=PM_REEL), dimension(2), intent(in) :: data_flux
  integer, dimension(10), intent(in) :: data_ia
  
  ! variables locales
  character(LEN=10000) :: ligne
  character(LEN=80) :: d, flux, fluxm, iaamoy, iaa, iapmoy
  
  ! initialisation
  ligne = ""
  
  write(d,1029) date
  ligne = trim(d)

  if (opt_flux) then
     write(flux,1029) data_flux(1)
     ligne = trim(ligne)//trim(flux)
  end if
  
  if (opt_fluxm) then
     write(fluxm,1029) data_flux(2)
     ligne = trim(ligne)//trim(fluxm)
  end if

  if (opt_iaamoy) then
     write(iaamoy,1057) data_ia(1)
     ligne = trim(ligne)//trim(iaamoy)
  end if
  
  if (opt_iaa) then
     write(iaa,1072) data_ia(2), data_ia(3), data_ia(4), data_ia(5),      &
          data_ia(6), data_ia(7), data_ia(8), data_ia(9)
     ligne = trim(ligne)//trim(iaa)
  end if

  if (opt_iapmoy) then
     write(iapmoy,1057) data_ia(10)
     ligne = trim(ligne)//trim(iapmoy)
  end if
  
  ! affichage de la ligne
  write(*,1000) trim(ligne)

end subroutine cpsi_ui_afficherLigneAcsol2
