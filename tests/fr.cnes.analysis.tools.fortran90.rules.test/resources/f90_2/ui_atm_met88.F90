program ui_atm_met88

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_met88
!
!$Resume
!   Programme de calcul du modèle d'atmosphère MET88
!$Description
!   Programme de calcul du modèle d'atmosphère MET88
!
! Usage
!.   ui_atm_met88  -alt alt -lat xlat -longi xlng -year iyr -month mon -day ida -hour ihr
!.                 -min min -index_geo igeo_ind -f10 f10 -f10b f10b -gi gi 
!. ou
!.   ui_atm_met88 -fic fichier_contenant_sequence_appel
! 
!
!>E z 		: real(4)	 altitude
!>E xlat 	: real(4)	 latitude
!>E xlng 	: real(4)	 longitude
!>E iyr 	: integer	 année
!>E mon 	: integer	 mois
!>E day 	: integer	 jour dans le mois
!>E ihr 	: integer	 heure
!>E min 	: integer	 minutes
!>E igeo_ind 	: integer	 indice géomagnétique
!>E f10 	: real(4)	 flux solaire 
!>E f10b 	: real(4)	 flux solaire moyen 
!>E gi  	: real(4)	 indice d'activité géomagnétique
!>S outdata : real(4),dimension(12)
!. outdata(1) : température exosphérique (K)
!. outdata(2) : température à l'altitude z
!. outdata(3) : masse volumique numéraire en N2 (/m3)
!. outdata(4) : masse volumique numéraire en O2 (/m3)
!. outdata(5) : masse volumique numéraire en O  (/m3)
!. outdata(6) : masse volumique numéraire en A  (/m3)
!. outdata(7) : masse volumique numéraire en He (/m3)
!. outdata(8) : masse volumique numéraire en H  (/m3)
!. outdata(9) : poids moléculaire moyen 
!. outdata(10): masse volumique totale          
!. outdata(11): log10 (masse volumique totale)
!. outdata(12): pression totale         (Pa)
!>S auxdata :  real(4),dimension(5)  
!. auxdata(1) : accélération de pesanteur (m/s²)
!. auxdata(2) : quotient des chaleurs spécifiques
!. auxdata(3) : pression donnée en hauteur (de colonne de liquide)
!. auxdata(4) : chaleur spécifique à pression constante
!. auxdata(5) : chaleur spécifique à volume constant
!
!$Auteur
!  Elise Aitier (ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_met88.F90 355 2013-02-14 12:16:41Z aadt $
!$Historique
!  $Log: ui_atm_met88.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.20  2010/11/03 10:50:58  mercadig
!  VERSION::FA-ID:1443:03/11/2010:Correction des formats
!
!  Revision 1.19  2010/11/02 16:16:17  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.18  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.17  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.16  2008/10/28 14:11:41  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.15  2008/10/03 07:22:03  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.14  2008/07/04 12:23:15  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.13  2008/04/04 18:00:30  vivaresf
!  Version 2.4 relecture des cartouches
!
!  Revision 1.12  2008/04/03 18:03:24  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.11  2008/03/18 16:10:38  vivaresf
!  Version 2.4 : correction de cartouche, définition des types
!
!  Revision 1.10  2007/11/21 16:49:57  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!
!  Revision 1.9  2007/10/31 17:59:03  sbd
!  DM-ID 797 modification en tete et variables appels
!
!  Revision 1.8  2007/10/30 11:10:55  sbd
!  DM-ID 797 modification de ui atm met88 pour lecture parametres dans fichier
!
!  Revision 1.7  2007/07/04 12:22:44  vivaresf
!  FA-ID 664 : majuscules pour la présentation
!
!  Revision 1.6  2007/01/25 09:38:53  vivaresf
!  Version 2.2a1 : vérification des cartouches
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
  use cps_atm_met88_mod
  use cps_acces
  
  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_met88.F90 355 2013-02-14 12:16:41Z aadt $'

  
! Arguments
! =========
  real(kind=4) :: z
  real(kind=4) :: xlat
  real(kind=4) :: xlng
  integer :: iyr   
  integer :: mon   
  integer :: ida   
  integer :: ihr   
  integer :: min
  integer :: igeo_ind
  real(kind=4) :: f10
  real(kind=4) :: f10b
  real(kind=4) :: gi
  real(kind=4),dimension(12) :: outdata
  real(kind=4),dimension(5)  :: auxdata
!  type(tm_code_retour)  :: code_retour

! Variables
! =========

  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, jj, ierfin
  character (len=256)            :: sLine
  character(len=60),dimension(50):: l_opt
  logical :: logalt, loglat, loglongi
  logical :: logyear, logmonth, logday, loghour, logmin
  logical :: loggeo_ind, logf10, logf10b, loggi

  integer :: numfich=11

! Initialisation
! ==============

  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_met88") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  logalt     = .false.
  loglat     = .false.
  loglongi   = .false.
  logyear    = .false.
  logmonth   = .false.
  logday     = .false.
  loghour    = .false.
  logmin     = .false.
  loggeo_ind = .false.
  logf10     = .false.
  logf10b    = .false.
  loggi      = .false.
  
  ! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)
  
  ! lecture effective
  if(ierfin.eq.1) then
     call ui_ecrire_help("ui_atm_met88")
     goto 999
  elseif(ierfin.eq.0) then
         call ui_lire_options(noptions, l_opt)
         open(unit=numfich, file=trim(l_opt(2)))
!        Reading arguments from *.don file (" " separator)
         read(unit=numfich,fmt='(A)') sLine
         sLine=trim(adjustl(sLine))
         do ii=1,24
           jj = 1
           do while (sLine(jj:jj) /= " ")
             jj = jj + 1
           end do
           l_opt(ii)=trim(adjustl(sLine(:jj-1)))
           sLine=trim(adjustl(sLine(jj+1:)))
         end do
         noptions=12
  else
     ! Lecture des arguments : 
     call ui_lire_options(noptions, l_opt)
     if (noptions == 0) then
        call ui_ecrire_help("ui_atm_met88")
        goto 999
    endif
  endif
        do ii=1, noptions
           
           if (l_opt(2*ii-1)=="-alt") then
              read(l_opt(2*ii), fmt=*) z
              logalt     = .true.
              
           elseif(l_opt(2*ii-1)=="-lat") then
              read(l_opt(2*ii), fmt=*) xlat 
              loglat     = .true.
              
           elseif(l_opt(2*ii-1)=="-longi") then
              read(l_opt(2*ii), fmt=*) xlng
              loglongi   = .true.
              
           elseif(l_opt(2*ii-1)=="-year") then
              read(l_opt(2*ii), fmt=*) iyr
              logyear    = .true.
              
           elseif(l_opt(2*ii-1)=="-month") then
              read(l_opt(2*ii), fmt=*) mon
              logmonth   = .true.
              
           elseif(l_opt(2*ii-1)=="-day") then
              read(l_opt(2*ii), fmt=*) ida
              logday     = .true.
              
           elseif(l_opt(2*ii-1)=="-hour") then
              read(l_opt(2*ii), fmt=*) ihr
              loghour    = .true.
              
           elseif(l_opt(2*ii-1)=="-min") then
              read(l_opt(2*ii), fmt=*) min
              logmin     = .true.
              
           elseif(l_opt(2*ii-1)=="-index_geo") then
              read(l_opt(2*ii), fmt=*) igeo_ind
              loggeo_ind = .true.
              
           elseif(l_opt(2*ii-1)=="-f10") then
              read(l_opt(2*ii), fmt=*) f10 
              logf10     = .true.
              
           elseif(l_opt(2*ii-1)=="-f10b") then
              read(l_opt(2*ii), fmt=*) f10b
              logf10b    = .true.
              
           elseif(l_opt(2*ii-1)=="-gi") then
              read(l_opt(2*ii), fmt=*) gi
              loggi      = .true.
              
           endif
        enddo
  
! Corps du programme
! =================

  ! Initialisations preliminaires

  if(logalt.and.loglat.and.loglongi.and.logyear.and.logmonth  &
       .and.logday.and.loghour.and.logmin.and.loggeo_ind.and. &
       logf10.and.logf10b.and.loggi ) then
     
     ! erreur sur l'init
     if (MSP_erreur) goto 999
     
     ! appel effectif
     call cps_atm_met88 ( z, xlat, xlng, iyr, mon, ida, ihr,  &
          min, igeo_ind, f10, f10b, gi, outdata, auxdata)
     
     ! erreur dans l'appel
     if (MSP_erreur) goto 999
        
         ! sortie écran
     write(*,'(a)')" "
     write(*,'(a,g21.12)')" Température exosphérique                (K)            = ", outdata (1)
     write(*,'(a,g21.12)')" Température à l'altitude z              (K)            = ", outdata (2)
     write(*,'(a,g21.12)')" Masse volumique en N2                   (/m3)          = ", outdata (3)
     write(*,'(a,g21.12)')" Masse volumique en O2                   (/m3)          = ", outdata (4)
     write(*,'(a,g21.12)')" Masse volumique en O                    (/m3)          = ", outdata (5)
     write(*,'(a,g21.12)')" Masse volumique en A                    (/m3)          = ", outdata (6)
     write(*,'(a,g21.12)')" Masse volumique en He                   (/m3)          = ", outdata (7)
     write(*,'(a,g21.12)')" Masse volumique en H                    (/m3)          = ", outdata (8)
     write(*,'(a,g21.12)')" Poids moléculaire moyen                                = ", outdata (9)
     write(*,'(a,g21.12)')" Masse volumique massique totale         (kg/m3)        = ", outdata (10)
     write(*,'(a,g21.12)')" Log10(masse volumique massique totale)                 = ", outdata (11)
     write(*,'(a,g21.12)')" Pression totale                         (Pa)           = ", outdata (12)
     write(*,'(a,g21.12)')" Accélération de pesanteur locale        (m.sec-2)      = ", auxdata (1)
     write(*,'(a,g21.12)')" Quotient de chaleur spécifique                         = ", auxdata (2)
     write(*,'(a,g21.12)')" Pression (exprimée en m)                (m)            = ", auxdata (3)
     write(*,'(a,g21.12)')" Chaleur spécifique à pression constante (m2.sec-2.k-1) = ", auxdata (4)
     write(*,'(a,g21.12)')" Chaleur spécifique à volume constant    (m2.sec-2.k-1) = ", auxdata (5)
     write(*,'(a)')" "
     
      else
         write(0,*) "Erreur : les arguments ne sont pas correctement renseignés."
         goto 999
      endif

999 continue
       if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

    call cps_close_utilisateur()

end program ui_atm_met88
