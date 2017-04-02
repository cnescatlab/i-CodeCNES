program ui_polesTerre

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_polesTerre
!
!$Resume
!  Données des mouvements précis des pôles terrestres
!
!$Description
!  Données des mouvements précis des pôles terrestres.
!  Utilitaire pour les données issues du bulletin A
!
!
!  Usage
!
!.  ui_polesTerre (-sure (-date d | -date_deb dd -date_fin df) | 
!.                 -pred ([-date d] (-last | -date_pred dp )))
!.                [-all] [-nv] [-err] [-pm] [-astd] [-nstd] [-utc]
!
! options disponibles :  
!
!> -sure                  : données sûres
!> -pred                  : données prédites
!> -date d                : date demandée (MJD)
!> -last                  : dernières données prédites 
!> -date_pred  dp         : date de la prédiction (MJD)
!> -all                   : toutes les données disponibles (sinon données demandées par les options suivantes)
!> -err                   : colonnes d'erreur
!> -utc                   : UTC-UT1
!> -pm                    : PM_x + PM_y
!> -astd                  : dpsi + deps
!> -nstd                  : dX + dY
!> -nv                    : mode non bavard
!> -date_deb dd           : créneau de dates à afficher, début (MJD)
!> -date_fin df           : créneau de dates à afficher, fin   (MJD)
!
!$Auteur
!
!$Version
!  $Id: ui_polesTerre.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_polesTerre.F90,v $
!  Revision 1.7  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.6  2009/09/09 12:56:39  cmartel
!  FA-ID 1157 : Correction de constantes non nommées
!
!  Revision 1.5  2008/10/31 13:08:47  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.4  2008/04/11 10:57:56  vivaresf
!  FA-ID 778 : suppression des variables inutilisées
!
!  Revision 1.3  2008/04/11 10:09:36  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!
!  Revision 1.2  2008/04/10 12:40:49  vivaresf
!  Version 2.4 AQ : correction des cartouches
!  FA-ID 664 : correction de la présentation des sorties (accents)
!
!  Revision 1.1  2008/02/08 17:51:40  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.9  2007/09/19 09:36:50  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.8  2007/09/18 12:51:20  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.7  2006/10/24 08:57:14  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!  Revision 1.6  2006/06/30 14:48:58  vpg
!  Mise a jour des cartouches
!  Revision 1.5  2006/06/16 17:32:07  vivaresf
!  Cartouches d'entete
!  Revision 1.4  2006/05/31 13:08:40  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.3  2006/04/25 13:49:37  bouillaj
!  Mise a jour du cartouche
!  Revision 1.2  2006/03/20 16:12:56  vpg
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
  character(len=256) :: SVN_VER =  '$Id: ui_polesTerre.F90 69 2012-09-11 08:33:34Z ffsm $'

  
#include "formats.h"  

  ! Constantes
  integer, parameter :: dim_data = 15

  ! Variables locales
  logical :: opt_sure, opt_pred, opt_date, opt_all, opt_err, opt_utc,   &
       opt_pm, opt_astd, opt_nstd , opt_date_deb, opt_date_fin, opt_nv, &
       opt_last, opt_date_pred
  integer :: ierfin, noptions, i, trouve
  character(LEN=20), dimension(50) :: l_opt
  
  real(KIND=PM_REEL) :: date, date_pred, date_deb, date_fin
  real(KIND=PM_REEL), dimension(dim_data) :: data0
  real(KIND=PM_REEL), dimension(120, dim_data) :: tab_data_pred
  real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_SURE, dim_data) :: tab_data_sure
  integer :: nb_pred, type0, nb_data
  
  ! initialisation
  call cps_init_utilisateur()
  trouve = CPS_ERR_DEF
  nb_pred = 0
  type0 = 0

  opt_sure = .false.
  opt_pred = .false.
  opt_date = .false.
  opt_err  = .false.
  opt_nv   = .false.
  ! par defaut : toutes les donnees
  opt_all  = .true.
  opt_utc  = .false.
  opt_pm   = .false.
  opt_astd = .false.
  opt_nstd = .false.
  
  opt_date_deb = .false.
  opt_date_fin = .false.
  
  opt_last = .false.
  opt_date_pred = .false.
  
  ! analyse des arguments
  ! aide
  call ui_test_arguments ("-h", "", ierfin)
  if(ierfin.eq.1) then
     call ui_ecrire_help("ui_polesTerre")
     goto 999
  else
     ! lecture des arguments
     call ui_lire_options(noptions, l_opt)
     
     if (noptions == 0) then
        ! aucun arguments
        call ui_ecrire_help("ui_polesTerre")
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
              
           case ("-err")
              opt_err = .true.
              
           case ("-utc")
              opt_utc = .true.
              opt_all = .false.
              
           case ("-pm")
              opt_pm = .true.
              opt_all = .false.

           case ("-astd")
              opt_astd = .true.
              opt_all = .false.
              
           case ("-nstd")
              opt_nstd = .true.
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
     opt_utc  = .true.
     opt_pm   = .true.
     opt_astd = .true.
     opt_nstd = .true.
  end if

  if (opt_sure) then
     ! donnees sures
     ! il faut qu'il y ait une date
     if ((.not.opt_date).and.(.not.opt_date_deb).and.(.not.opt_date_fin)) then
        write(0,1000) "Il faut spécifier une date!"
        goto 999
     end if
     if (opt_date) then
        ! une seule date
        type0 = 1
        trouve = cps_getBullASure(date, data0)
        if (trouve.ne.CPS_OK) then
           write(0,1000) "Erreur : donnée non trouvée"
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
           trouve = cps_getBullASureCreneau(date_deb, date_fin, tab_data_sure, nb_data)
           if (trouve.ne.CPS_OK) then
              write(0,1000) "Erreur : donnée introuvable"
              goto 999
           end if
        end if
     end if
  end if
     
  if (opt_pred) then
     ! donnees predites
     if (opt_last.and.(.not.opt_date)) then
        ! toutes les dernieres prédictions
        type0 = 3
        call cps_getBullADernieresPred(date_pred, tab_data_pred, nb_pred)
        trouve = CPS_OK
     else if (opt_last.and.opt_date.and.(.not.opt_date_pred)) then
        ! les dernieres prédictions effectuees pour une date particuliere
        type0 = 4
        trouve = cps_getBullADernierePred(date, date_pred, data0)
     else if (opt_date.and.opt_date_pred) then
        ! les prédictions effectuees a la date 'date_pred' pour la date 'date'
        type0 = 5
        trouve = cps_getBullADatePred(date, date_pred, data0)
     else if ((.not.opt_date).and.opt_date_pred) then
        ! toutes les prédictions effectuees a la date 'date_pred'
        type0 = 6
        trouve = cps_getBullAPred(date_pred, tab_data_pred, nb_pred)        
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
        write(*,'(a,f10.1)') "Données sûres pour la date : ", date
     else
        write(*,'(f10.1)') date
     end if
  case (2)
     if (.not.opt_nv) then
        write(*,'(a,f10.1,a,f10.1,a)') "Données sûres sur le créneau [ ", date_deb, " ; ", date_fin, " ]"
     else
        write(*,'(f10.1)') date_deb
        write(*,'(f10.1)') date_fin
     end if
  case (3)
     if (.not.opt_nv) then
        write(*,1000) "Toutes les dernières prédictions"
        write(*,'(a,f10.1)') "Prédictions effectuées à la date ", date_pred
        write(*,1001) "Nombre de prédictions : ", nb_pred
     else
        write(*,'(f10.1)') date_pred
        write(*,1057) nb_pred
     end if
  case (4)
     if (.not.opt_nv) then
        write(*,'(a,f10.1)') "Dernières prédictions pour la date ", date
        write(*,'(a,f10.1)') "Prédictions effectuées à la date ", date_pred
     else
        write(*,'(f10.1)') date
        write(*,'(f10.1)') date_pred
     end if
  case (5)
     if (.not.opt_nv) then
        write(*,'(a,f10.1,a,f10.1)') "Prédictions pour la date ", date, " effectuées à la date ", date_pred
     else
        write(*,'(f10.1)') date
        write(*,'(f10.1)') date_pred
     end if
  case (6)
     if (.not.opt_nv) then
        write(*,'(a,f10.1)') "Toutes les prédictions effectuées à la date ", date_pred
        write(*,1001) "Nombre de prédictions : ", nb_pred
     else
        write(*,'(f10.1)') date_pred
        write(*,1057) nb_pred
     end if
  end select


  ! affichage de la legende
  if (.not.opt_nv) then
     call cpsi_ui_afficherLegende(opt_utc, opt_pm, opt_astd, opt_nstd, opt_err)
  end if

  
  if ((type0.eq.1).or.(type0.eq.4).or.(type0.eq.5)) then
     call cpsi_ui_afficherLigne(opt_utc, opt_pm, opt_astd, opt_nstd, opt_err, data0)
  end if
  
  
  if (type0.eq.2) then
     do i=1,nb_data
        call cpsi_ui_afficherLigne(opt_utc, opt_pm, opt_astd, opt_nstd, opt_err, tab_data_sure(i,1:dim_data))
     end do
  end if

  
  if ((type0.eq.3).or.(type0.eq.6)) then
     do i=1,nb_pred
        call cpsi_ui_afficherLigne(opt_utc, opt_pm, opt_astd, opt_nstd, opt_err, tab_data_pred(i,1:dim_data))
     end do
  end if


999 continue
  
 
  ! fin
  call cps_close_utilisateur()

end program ui_polesTerre



subroutine cpsi_ui_afficherLegende(opt_utc, opt_pm, opt_astd, opt_nstd, opt_err)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ui_afficherLegende
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ui_afficherLegende(opt_utc, opt_pm, opt_astd, opt_nstd, opt_err)
!.    logical :: opt_utc, opt_pm, opt_astd, opt_nstd, opt_err
!
!$Arguments
!>E     opt_utc   :<logical>   
!>E     opt_pm    :<logical>   
!>E     opt_astd  :<logical>   
!>E     opt_nstd  :<logical>   
!>E     opt_err   :<logical>   
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
  logical, intent(in) :: opt_utc, opt_pm, opt_astd, opt_nstd, opt_err

  ! variables locales
  character(LEN=256) :: legende
  
  ! initialisation
  legende = "   date"
  
  if (opt_utc) then
     legende = trim(legende)//"        UTC-UT1"
     if (opt_err) then
        legende = trim(legende)//"  UTC-UT1_err"
     end if
  end if

  if (opt_pm) then
     legende = trim(legende)//"                PM_x           PM_y"
     if (opt_err) then
        legende = trim(legende)//"  PM_x_err  PM_y_err"
     end if
  end if

  if (opt_astd) then
     legende = trim(legende)//"         dpsi       deps"
     if (opt_err) then
        legende = trim(legende)//"  dpsi_err  deps_err"
     end if
  end if
  
  if (opt_nstd) then
     legende = trim(legende)//"       dX      dY  "
     if (opt_err) then
        legende = trim(legende)//"  dX_err  dY_err"
     end if
  end if

  ! ecriture
  write(*,1000) trim(legende)

end subroutine cpsi_ui_afficherLegende




subroutine cpsi_ui_afficherLigne(opt_utc, opt_pm, opt_astd, opt_nstd, opt_err, data0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ui_afficherLigne
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ui_afficherLigne(opt_utc, opt_pm, opt_astd, opt_nstd, opt_err, data0)
!.    logical :: opt_utc, opt_pm, opt_astd, opt_nstd, opt_err
!.    real(KIND=PM_REEL), dimension(15) :: data0
!
!$Arguments
!>E     opt_utc   :<logical>            
!>E     opt_pm    :<logical>            
!>E     opt_astd  :<logical>            
!>E     opt_nstd  :<logical>            
!>E     opt_err   :<logical>            
!>E     data0     :<PM_REEL,DIM=(15)>   
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
  logical, intent(in) :: opt_utc, opt_pm, opt_astd, opt_nstd, opt_err
  real(KIND=PM_REEL), dimension(15), intent(in) :: data0
  
  ! variables locales
  character(LEN=512) :: ligne
  character(LEN=50) :: date, utc_ut1, utc_ut1_err, pm_x, pm_y, pm_x_err, pm_y_err, &
       dpsi, deps, dpsi_err, deps_err, dX, dY, dX_err, dY_err
  
  ! initialisation
  ligne = ""
  
  write(date,'(f10.1)') data0(1)
  ligne = trim(date)
  
  if (opt_utc) then
     write(utc_ut1,1029) data0(2)
     ligne = trim(ligne)//trim(utc_ut1)
     if (opt_err) then
        write(utc_ut1_err,1029) data0(3)
        ligne = trim(ligne)//trim(utc_ut1_err)
     end if
  end if
  
  if (opt_pm) then
     write(pm_x,1029) data0(4)
     write(pm_y,1029) data0(5)
     ligne = trim(ligne)//trim(pm_x)//trim(pm_y)
     if (opt_err) then
        write(pm_x_err,1029) data0(6)
        write(pm_y_err,1029) data0(7)
        ligne = trim(ligne)//trim(pm_x_err)//trim(pm_y_err)
     end if
  end if

  if (opt_astd) then
     write(dpsi,'(f10.4)') data0(8)
     write(deps,'(f10.4)') data0(9)
     ligne = trim(ligne)//trim(dpsi)//trim(deps)
     if (opt_err) then
        write(dpsi_err,1029) data0(10)
        write(deps_err,1029) data0(11)
        ligne = trim(ligne)//trim(dpsi_err)//trim(deps_err)
     end if
  end if
  
  if (opt_nstd) then
     write(dX,'(f10.4)') data0(12)
     write(dY,'(f10.4)') data0(13)
     ligne = trim(ligne)//trim(dX)//trim(dY)
     if (opt_err) then
        write(dX_err,1029) data0(14)
        write(dY_err,1029) data0(15)
        ligne = trim(ligne)//trim(dX_err)//trim(dY_err)
     end if
  end if

  ! affichage de la ligne
  write(*,1000) trim(ligne)

end subroutine cpsi_ui_afficherLigne
