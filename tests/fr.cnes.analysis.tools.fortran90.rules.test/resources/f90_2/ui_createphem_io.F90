module ui_createphem_io
!***********************************************************************
!$<AM-V2.0>
!
!$Version
!  $Id: ui_createphem_io.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_createphem_io.F90,v $
!  Revision 1.7  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.6  2009/09/09 12:55:18  cmartel
!  FA-ID 1157 : Suppression de goto
!
!  Revision 1.5  2008/10/31 13:19:00  cml
!  FA-ID 1076 : Corrections mineurs dans les affichages des resultats des utilitaires
!
!  Revision 1.4  2008/04/11 12:13:00  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches
!
!  Revision 1.3  2008/04/11 10:57:34  vivaresf
!  FA-ID 778 : suppression des variables inutilisées
!  Revision 1.2  2008/04/11 10:09:22  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!  Revision 1.1  2008/02/08 17:51:21  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.4  2006/06/16 17:32:04  vivaresf
!  Cartouches d'entete
!  Revision 1.3  2006/06/14 12:38:13  vivaresf
!  DM-ID 387 : mise au point de CREATEPHEM
!
!$FinHistorique
!
!$<>
!***********************************************************************

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ui_createphem_io.F90 69 2012-09-11 08:33:34Z ffsm $'


      
  ! constantes (inexistante dans ephem de COMPAS_BASE)
  integer, private, parameter :: code_analy_vsop82 = 10
  integer, private, parameter :: code_naif_de200 = 20
  integer, private, parameter :: code_naif_de403 = 21
  integer, private, parameter :: code_naif_de405 = 22
  integer, private, parameter :: code_naif_de406 = 23
  integer, private, parameter :: code_bdl_vsop82 = 30
  integer, private, parameter :: code_kepler = 11

contains

      subroutine ecrire_entete_col(lfn,ktype,nomplac,code_rep,ctframe, &
           nbplanet,nompla,dureej,ndeg,ctdeb,ctfin,datecour,ier)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  ecrire_entete_col
!
!$Resume
!   Création de l'en-tête d'un fic. d'éphémérides de planètes cnes.
!
!$Description
!   Création de l'entête d'un fichier éphémérides planètes \
!   CNES présentées sous forme de coefficients de Tchebychev.
!
!$Auteur
!   Equipe interplanétaire
!
!$Mots-cles
!       Coefficients de Tchebychev
!
!$Usage
!  call ecrire_entete_col(lfn,ktype,nomplac,code_rep,ctframe, &
!.               nbplanet,nompla,dureej,ndeg,ctdeb,ctfin,datecour,ier)
!.    character (LEN=32), dimension(NBPLANETMX) :: nompla
!.    real (KIND=pm_reel) :: dureej
!.    integer :: lfn,ktype,nbplanet,ndeg
!.    integer :: code_rep 
!.    character (LEN=32) :: ctframe,ctdeb,ctfin,nomplac
!.    logical :: datecour
!.    integer :: ier
!
!$Arguments
!>E     lfn       :<integer>                   Numéro de tape du fichier
!>E     ktype     :<integer>                   Type d'éphémérides sources du fichier
!.                           10 --> éphémérides fichier bdl vsop82
!.                           20 --> éphémérides naif
!.                           30 --> éphémérides analytiques bdl vsop82
!>E     nomplac   :<LEN=32>                    Nom du corps central
!>E     code_rep  :<integer>                   Code repère
!>E     ctframe   :<LEN=32>                    Date définition du repère de sortie
!>E     nbplanet  :<integer>                   Nbre de coprs dans nompla
!.                    (limité à 15)
!>E     nompla    :<LEN=32,DIM=(NBPLANETMX)>   Liste des planètes dont on veut les
!                             éphémérides
!>E     dureej    :<pm_reel>                   Intervalle de validité des polynômes (j)
!>E     ndeg      :<integer>                   Degré des polynômes de Tchebychev
!>E     ctdeb     :<LEN=32>                    Date début du fichier
!>E     ctfin     :<LEN=32>                    Date fin du fichier
!>E     datecour  :<logical>                   OK si repère a la date courante
!>E/S   ier       :<integer>                   Code d'erreur
!.	ier	:   0	Pas d'erreur
!.	ier	:  -1	Nombre de planètes incohérent
!>E	iaccesw      = pointeur sur la zone de donnees "moyens d'acces"
!
!$Acces
!  PUBLIC
!
!$Remarques
!
!$Voir-Aussi
!
!$<>
!***********************************************************************

      use mslib
      use msp_gestion_erreur

      implicit none

!------------------------------
!	PARAMETRES
!------------------------------
      integer, parameter :: NBPLANETMX=15

      character (LEN=32), dimension(NBPLANETMX), intent(in)  :: nompla
      real (KIND=pm_reel), intent(in) :: dureej
      integer, intent(in) :: lfn,ktype,nbplanet,ndeg
      integer, intent(in) :: code_rep ! DM 387 : integration COMPAS V2-0
      character (LEN=32), intent(in)  :: ctframe,ctdeb,ctfin,nomplac
      logical, intent(in) :: datecour

!-------------------------------
!	SOUS-PROGRAMMES UTILISES
!-------------------------------
! Declaration manuelle des external

! Fin de la declaration manuelle
!-------------------------------

!-------------------------------

!-------------------------------
!	COMMONS
!-------------------------------

!-------------------------------
!	VARIABLES LOCALES
!-------------------------------

      integer :: iplanet,ier
      character (LEN=100) ::baratin
      character (LEN=3) :: nomframe ! DM 387

!-------------------------------
!	INITIALISATION
!-------------------------------
      ier = 0

!-------------------------------
!-------------------------------
! CORPS DU SOUS-PROGRAMME <centfic_pla>
!-------------------------------
!-------------------------------
      if (nbplanet.gt.nbplanetmx) then
         ier = -1
         call MSP_signaler_message(cle_mes='CREA_LIBENVINT',routine='centfic_pla', &
              partie_variable='Nombre de planètes incohérent')
         return
      endif

      write(lfn,111)

!-----------------------------------------------------------------------
!     Indication du type d'ephemerides
!-----------------------------------------------------------------------

      if (ktype.eq.code_analy_vsop82) then
       baratin = 'EPHEMERIDES ANALYTIQUE BDL VSOP82 TCHEBYCHEVISEES  = '
      elseif (ktype.eq.code_naif_de200) then
       baratin = 'EPHEMERIDES NAIF DE200 TCHEBYCHEVISEES             = '
      elseif (ktype.eq.code_naif_de403) then
       baratin = 'EPHEMERIDES NAIF DE403 TCHEBYCHEVISEES             = '
      elseif (ktype.eq.code_naif_de405) then
       baratin = 'EPHEMERIDES NAIF DE405 TCHEBYCHEVISEES             = '
      elseif (ktype.eq.code_naif_de406) then
       baratin = 'EPHEMERIDES NAIF DE406 TCHEBYCHEVISEES             = '
      elseif (ktype.eq.code_bdl_vsop82) then
       baratin = 'EPHEMERIDES FICHIER BDL VSOP82 TCHEBYCHEVISEES     = '
      elseif (ktype.eq.code_kepler) then
       baratin = 'EPHEMERIDES KEPLERIENS TCHEBYCHEVISEES             = '
      else
       baratin = '                                                   = '
      endif

      write(lfn,150) baratin,ktype

      write(lfn,110) &
      'NOM DU CORPS CENTRAL                               = ',trim(nomplac)

      write(nomframe,'(i3)') code_rep


      ! Repere
      baratin = ' au '//trim(ctframe)
      if(datecour) baratin=" date du bulletin"

      write(lfn,110)                                             &
      'NOM DU REPERE DE SORTIE                            = ', &
      trim(nomframe)//trim(baratin)

      !!!!!!!!!!!!!!!!!!!!!!!!!

      write(lfn,150) &
      'NOMBRE DE PLANETES DU FICHIER                      = ',nbplanet
 
      do iplanet=1,nbplanet
        write(lfn,110) &
      '                                                   = ',trim(nompla(iplanet))
      enddo

      write(lfn,140) &
      'DUREE VALIDITE POLYNOME TCHEBYCHEV (JOURS FRACT.)  = ',dureej

      write(lfn,150) &
      'DEGRE POLYNOME TCHEBYCHEV EN JOURS FRACTIONNAIRES  = ',ndeg

      write(lfn,110) &
      'ECHELLE DE TEMPS                                   = ','TE'

      write(lfn,120) &
      'DATE DEBUT DU FICHIER                              = ',ctdeb

      write(lfn,120) &
      'DATE FIN DU FICHIER (A LA DUREE VALIDITE PRES)     = ',ctfin

      write(lfn,111)

!-------------------------------
!	FORMATS
!-------------------------------
! 100	format(a)
110	format(a53,a)
111	format(100('*'))
120	format(a53,a)
! 130	format(a53,d20.12)
140	format(a53,d23.16)
150	format(a53,i3)

    end subroutine ecrire_entete_col

  

      subroutine ecrire_entete_mad(iaccesw,ktype,nomplac,code_rep,     &
           ctframe, nbplanet,nompla,mu,dureej,ndeg,ctdeb,ctfin,vrot,   &
           obliq, lref, angle_u, angle_v, datecour, ier, &
           textetyperep, textedaterep, texteplarep)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  ecrire_entete_mad
!
!$Auteur
!       Equipe interplanétaire
!
!$Resume
!	Entete des ephemerides de Tchebytchev MADONA
!
!$Description
!	CREATION FICHIER EPHEMERIDES PLANETES CNES PRESENTEES SOUS
!       FORME DE COEFFICIENTS DE TCHEBYCHEV AU FORMAT MADONA.
!
!$Usage
!  call ecrire_entete_mad(iaccesw,ktype,nomplac,code_rep,     &
!.               ctframe, nbplanet,nompla,mu,dureej,ndeg,ctdeb,ctfin,vrot,   &
!.               obliq, lref, angle_u, angle_v, datecour, ier, &
!.               textetyperep, textedaterep, texteplarep)
!.    integer :: iaccesw,ktype,nbplanet,ndeg
!.    character (LEN=*) :: nomplac
!.    integer :: code_rep 
!.    character (LEN=*) :: ctframe,ctdeb,ctfin
!.    character (LEN=*),dimension(nbplanetmx) :: nompla
!.    character (LEN=*) :: textetyperep, textedaterep, texteplarep
!.    real(KIND=PM_REEL) :: vrot, obliq, lref, angle_u, angle_v
!.    logical :: datecour
!.    integer :: ier
!.    real (KIND=pm_reel),DIMENSION(nbplanetmx) :: mu
!.    real (KIND=pm_reel) :: dureej
!
!$Arguments
!>E     iaccesw       :<integer>                    Accès au fichier MADONA               
!>E     ktype         :<integer>                    Type d'éphémérides sources du fichier
!.                           10 --> éphémérides fichier bdl vsop82
!.                           20 --> éphémérides naif
!.                           30 --> éphémérides analytiques bdl vsop82
!>E     nomplac       :<LEN=*>                      Nom du corps central
!>E     code_rep      :<integer>                    Code repère
!>E     ctframe       :<LEN=*>                      Date définition du repère de sortie
!>E     nbplanet      :<integer>                    Nbre de coprs dans nompla
!>E     nompla        :<LEN=*,DIM=(nbplanetmx)>     Liste des corps dont on veut les
!                             éphémérides
!>E     mu            :<pm_reel,DIM=(nbplanetmx)>   mu des corps
!>E     dureej        :<pm_reel>                    Intervalle de validité des polynômes (j)
!>E     ndeg          :<integer>                    Degré des polynômes de Tchebychev
!>E     ctdeb         :<LEN=*>                      Date début du fichier
!>E     ctfin         :<LEN=*>                      Date fin du fichier
!>E     vrot          :<PM_REEL>                    Vitesse de rotation du repère
!>E     obliq         :<PM_REEL>                    Obliquite
!>E     lref          :<PM_REEL>                    Longitude de reference du repere
!>E     angle_u       :<PM_REEL>                    angle Pole%u
!>E     angle_v       :<PM_REEL>                    angle Pole%v 
!>E     datecour      :<logical>                    OK si repère a la date courante
!>S     ier           :<integer>                    Code d'erreur
!.	ier	:   0	Pas d'erreur
!.	ier	:  -1	Nombre de planètes incohérent
!>E     textetyperep  :<LEN=*>                      Texte du champ rep 
!>E     textedaterep  :<LEN=*>                      Texte du champ ech_date
!>E     texteplarep   :<LEN=*>                      Texte du champ pla
!
!$Remarques
!
!$Voir-Aussi
!
!$Mots-cles
!
!$Acces
!  PUBLIC
!
!$<>
!*******************************************************************************
      use mslib
      use msp_gestion_erreur

      implicit none


!#include "acces_F.h"
      integer, parameter :: nbplanetmx=15

!     Arguments
      integer,            intent(in) :: iaccesw,ktype,nbplanet,ndeg
      character (LEN=*),  intent(in) :: nomplac
      integer,            intent(in) :: code_rep ! DM 387 : integration COMPAS V2-0
      character (LEN=*),  intent(in) :: ctframe,ctdeb,ctfin
      character (LEN=*),  intent(in),dimension(nbplanetmx) :: nompla
      character (LEN=*),  intent(in) :: textetyperep, textedaterep, texteplarep
      real(KIND=PM_REEL), intent(in) :: vrot, obliq, lref, angle_u, angle_v
      logical,            intent(in) :: datecour
      integer, intent(out) :: ier

      real (KIND=pm_reel),DIMENSION(nbplanetmx), intent(in) :: mu
      real (KIND=pm_reel), intent(in)  :: dureej

!     Variables locales
      integer :: iplanet
      integer :: iret,ichoix
      character (LEN=20) :: cnumero
      character (LEN=3)  :: cnumframe

      character (LEN=100) :: baratin


      ! Initialisations
      ier = 0

      ! Cas d'erreur
      if(nbplanet.gt.nbplanetmx)then
         ier = -1
         call MSP_signaler_message(routine='centfic_pla_mad',cle_mes='CREA_PLAMAX', &
              partie_variable='Nombre de planètes incohérent')
         return
      endif
  
      ! Ephemerides
      if(ktype.eq.code_analy_vsop82)then
         baratin = 'FICHIER EPHEMERIDES ANALYTIQUES BDL VSOP82 AVEC COEFF'&
              // 'ICIENTS TCHEBYCHEV RECALCULES'
      endif
      if(ktype.eq.code_naif_de200)then
         baratin = 'FICHIER EPHEMERIDES NAIF AVEC COEFFICIENTS TCHEBYCHEV'&
              // ' RECALCULES'
      endif
      if(ktype.eq.code_bdl_vsop82)then
         baratin = 'FICHIER EPHEMERIDES FICHIER BDL VSOP82 AVEC COEFFICIE'&
              // 'NTS TCHEBYCHEV RECALCULES'
      endif

      iret = acc_putcom(iaccesw,ACC_HEADER,1,baratin)

      ! Corps central
      iret = acc_puts(iaccesw,"Corps_central",nomplac)
      iret = acc_putcom(iaccesw,"Corps_central",1,"Corps central")
  
      ichoix = 1

      ! Code repere
      write(cnumframe,'(I3)') code_rep
      cnumero = "Repère numéro "//cnumframe(1:3)
      iret = acc_puti(iaccesw,"Repere",code_rep)
      iret = acc_putcom(iaccesw,"Repere",1,cnumero)
      iret = acc_puts(iaccesw,"Date_repere",ctframe)
      if (datecour) iret = acc_puts(iaccesw,"Date_repere","date du bulletin")
      
      ! infos de GS_AXES sur le repere
      
      ! Type de repère
      cnumero="Type de repère"
      iret = acc_puts(iaccesw, "rep", textetyperep)
      iret = acc_putcom(iaccesw,"rep",1,cnumero)
      ! Date de référence
      if (len_trim(textedaterep) >0) then
         cnumero="Date de référence du repère"
         iret = acc_puts(iaccesw, "ech_date", textedaterep)
         iret = acc_putcom(iaccesw, "ech_date",1, cnumero)
      endif
      ! Planète de référence
      if (len_trim(texteplarep) >0) then
         cnumero="Planète de référence du repère"
         iret = acc_puts(iaccesw, "pla", texteplarep)
         iret = acc_putcom(iaccesw,"pla",1,cnumero)
      endif
      
      ! vitesse de rotation
      iret = acc_putd(iaccesw, "vrot", vrot, "rad/s")
      ! obliquite
      iret = acc_putd(iaccesw, "obli", obliq, "deg")
      ! longitude  de reference
      iret = acc_putd(iaccesw, "lref", obliq, "deg")
      ! angle u du pole
      iret = acc_putd(iaccesw, "angle_u", obliq, "deg")
      ! angle v du pole
      iret = acc_putd(iaccesw, "angle_v", obliq, "deg")
      
      
      ! Corps d'interet
      iret = acc_create(iaccesw,"Planetes",ACC_TABL,"")
      iret = acc_select(iaccesw,"Planetes",ACC_TABL)
      
      do iplanet=1,nbplanet
         iret = acc_set_index(iaccesw,iplanet)
         iret = acc_puts(iaccesw,ACC_INDEX,nompla(iplanet))
      enddo
      iret = acc_select_end(iaccesw)
      iret = acc_putcom(iaccesw,"Planetes",1,"Liste des planètes")
      
      iret = acc_create(iaccesw,"Mu_planetes",ACC_TABL,"")
      iret = acc_select(iaccesw,"Mu_planetes",ACC_TABL)
      
      do iplanet=1,nbplanet
         iret = acc_set_index(iaccesw,iplanet)
         iret = acc_putd(iaccesw,ACC_INDEX,mu(iplanet),"m^3/s^2")
      enddo
      iret = acc_select_end(iaccesw)
      iret = acc_putcom(iaccesw,"Mu_planetes",1,"Constante de gravitation des planètes")
      
      
      ! Parametres de tchebytchevisation
      iret = acc_putd(iaccesw,"Pas_tchebycheff",dureej,"j")
      iret = acc_putcom(iaccesw,"Pas_tchebycheff",1,"Pas de calcul")
      
      iret = acc_puti(iaccesw,"Degre_tchebycheff",ndeg)
      iret = acc_putcom(iaccesw,"Degre_tchebycheff",1, &
           "Degré des polynômes")
      
      ! Dates
      iret = acc_puts(iaccesw,"Echelle_temps",'TE')
      iret = acc_putcom(iaccesw,"Echelle_temps",1, &
           "Echelle de temps utilisée pour les dates")
      
      iret = acc_puts(iaccesw,"Date_debut",ctdeb)
      iret = acc_putcom(iaccesw,"Date_debut",1, &
           "Date de début de tchebychevisation")
      
      iret = acc_puts(iaccesw,"Date_fin",ctfin)
      iret = acc_putcom(iaccesw,"Date_fin",1, &
           "Date de fin de tchebychevisation")
            
    end subroutine ecrire_entete_mad


 end module ui_createphem_io

