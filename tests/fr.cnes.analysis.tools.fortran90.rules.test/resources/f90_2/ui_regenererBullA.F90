program ui_regenererBullA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_regenererBullA
!
!$Resume
!  Création du fichier bulletin A à partir des données COMPAS
!
!$Description
!  Création, dans le répertoire courant des fichiers
!.  - finals.all          (bulletin A ancien format)
!.  - finals2000A.all     (bulletin A nouveau format)
!  tels qu'ils étaient à une date donnée, à partir des informations
!  disponibles dans la base de données COMPAS.
!
! Usage
!. ui_regenererbullA [-h | -date date]
!>E date : date de référence (jj1950)
!> -h    : affichage de l'aide en ligne
!
!$Auteur
!   Vincent Philavong (ATOS Origin)
!
!$Version
!  $Id: ui_regenererBullA.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_regenererBullA.F90,v $
!  Revision 1.6  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.5  2008/10/31 13:09:01  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.4  2008/04/29 08:08:24  vivaresf
!  FA-ID 664 : formattage des cartouches
!
!  Revision 1.3  2008/04/28 13:00:44  vivaresf
!  FA-ID 664 : présentation des sorties et cartouches
!
!  Revision 1.2  2008/04/10 12:48:58  vivaresf
!  Version 2.4 AQ : vérification des arguments et des cartouches
!
!  Revision 1.1  2008/02/08 17:51:41  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.7  2007/09/18 12:50:55  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.6  2006/10/24 08:51:11  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!  Revision 1.5  2006/06/30 14:48:59  vpg
!  Mise a jour des cartouches
!  Revision 1.4  2006/06/16 17:32:08  vivaresf
!  Cartouches d'entete
!  Revision 1.3  2006/06/14 12:36:33  vivaresf
!  DM-ID 387 : Entete et documentation
!
!$FinHistorique
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use ui_bullA
  use ui_ioplus

  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_regenererBullA.F90 69 2012-09-11 08:33:34Z ffsm $'


#include "formats.h"

  logical :: opt_date
  real(KIND=PM_REEL) :: date
  character(LEN=256) :: fic, fic_2000A
  integer :: ok
  integer :: ierfin, noptions, i
  character(LEN=20), dimension(50) :: l_opt
  type(MSP_MESSAGE) :: messages

  ! initialisation
  ok = CPS_ERR_DEF
  date = 0.0_pm_reel
  fic = "finals.all"
  fic_2000A = "finals2000A.all"
  opt_date = .false.
  
  call cps_init_utilisateur()
  
  ! analyse des arguments
  ! aide
  call ui_test_arguments ("-h", "", ierfin)
  if(ierfin.eq.1) then
     call ui_ecrire_help("ui_regenererBullA")
     goto 999
  else
     ! lecture des arguments
     call ui_lire_options(noptions, l_opt)
     
     if (noptions == 0) then
        ! aucun arguments
        call ui_ecrire_help("ui_regenererBullA")
        goto 999
     else
        do i=1, noptions
           select case (l_opt(2*i-1))
           case ("-date")
              opt_date = .true.
              read(l_opt(2*i),*) date
           end select
        end do
     end if
     
  ! Erreur d'appel
     if (.not.opt_date) then
        write(0,1000) "Erreur : il manque la date!"
        goto 999
     end if
     
  ! Régéneration

     ok = cps_regenererBullA(date, trim(fic), trim(fic_2000A))
     if (ok.ne.CPS_OK) then
        write(0,1002) "Echec de la régénération du bulletin A à la date ",date
     else
        write(*,1054) "Régénération du bulletin A à la date ",date, " OK"
     end if
     
  end if
  
  
999 continue
  
  ! fermeture de COMPAS
  call cps_close_utilisateur()
  
  if (MSP_PROBLEME) then
     call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
     call MSP_afficher_message (message=messages,unit=0)
  endif
  
end program ui_regenererBullA
