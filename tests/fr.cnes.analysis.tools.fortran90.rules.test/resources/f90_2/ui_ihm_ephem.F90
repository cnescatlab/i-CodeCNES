!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!	Routines utiles pour l'IHM CREATEPHEM
!
!$Version
!  $Id: ui_ihm_ephem.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_ihm_ephem.F90,v $
!  Revision 1.5  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.4  2008/11/07 14:35:31  cml
!  AQ : Correction de pointeurs non initialises
!
!  Revision 1.3  2008/04/11 12:42:40  vivaresf
!  Version 2.4 AQ : correction des cartouches
!
!  Revision 1.2  2008/04/11 10:57:35  vivaresf
!  FA-ID 778 : suppression des variables inutilisées
!
!  Revision 1.1  2008/02/08 17:51:25  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!
!  Revision 1.3  2006/10/23 12:56:35  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!
!  Revision 1.2.4.1  2006/10/23 10:12:23  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!
!  Revision 1.2  2006/06/16 17:32:06  vivaresf
!  Cartouches d'entete
!
!  Revision 1.1  2006/06/16 10:43:09  vivaresf
!  Deplacement de la routine pour utilisation en IHM
!
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine lentfic_pla_MAD(fic_ephem,ibid1,numplac,numframe,tframe &
           ,nbplanet,numpla,bid2,ibid2,tdeb,tfin,vrot,lref,pole_u,       &
           pole_v, obliq, datecour, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  lentfic_pla_MAD
!
!$Resume
!   Lecture de l'entete du fichier des ephemerides au format MADONA
!
!$Description
!   Lecture de l'entete du fichier des ephemerides au format MADONA
!
!$Auteur
!    SchlumbergerSema
!
!$Acces
!  PUBLIC
!
!$Usage
!  call lentfic_pla_MAD(fic_ephem,ibid1,numplac,numframe,tframe &
!.               ,nbplanet,numpla,bid2,ibid2,tdeb,tfin,vrot,lref,pole_u,       &
!.               pole_v, obliq, datecour, ier)
!.    character (LEN=*) :: fic_ephem
!.    integer :: numplac 
!.    integer :: numframe 
!.    integer :: nbplanet
!.    integer :: numpla(nbplanetmx)
!.    integer :: ibid1,ibid2
!.    real (KIND=pm_reel) :: bid2,tdeb,tfin
!.    real (KIND=pm_reel) :: tframe, vrot, lref, pole_u, pole_v, obliq
!.    logical :: datecour
!.    integer :: ier
!
!$Arguments
!>E     fic_ephem  :<LEN=*>                      
!>S     ibid1      :<integer>                    non utilisee
!>S     numplac    :<integer>                    Numero de la planete centrale
!>S     numframe   :<integer>                    Numero CNES du repere de sortie
!>S     tframe     :<pm_reel>                    Date repere de sortie
!>S     nbplanet   :<integer>                    Nombre de planetes a examiner
!>S     numpla     :<integer,DIM=(nbplanetmx)>   Numero des planetes a examiner
!>S     bid2       :<pm_reel>                    non utilisee
!>S     ibid2      :<integer>                    non utilisee
!>S     tdeb       :<pm_reel>                    date de début d'éphémérides                
!>S     tfin       :<pm_reel>                    date de fin d'éphémérides    
!>S     vrot       :<pm_reel>                    Vitesse de rotation de la planète du repère
!>S     lref       :<pm_reel>                    Longitude de reference du repère
!>S     pole_u     :<pm_reel>                    Pole%u du repere                 
!>S     pole_v     :<pm_reel>                    Pole%v du repere                   
!>S     obliq      :<pm_reel>                    Obliquité                 
!>S     datecour   :<logical>                    OK si repère à la date courante        
!>S     ier        :<integer>                    code erreur retour
!0 si OK
!
!$Remarques
!
!$Mots-cles
!   ephemerides, Tchebychev, MADONA
!
!$Voir-Aussi
!   mecaspa (MSP_lire_ephem_3corps)
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use mslib
      use EPHEM
      use msp_gestion_erreur

      implicit none

      integer, parameter :: nbplanetmx=15
!
! Parametres
!
      character (LEN=*), intent(in) :: fic_ephem
      integer, intent(out)  ::  numplac   ! numero de la planete centrale
      integer, intent(out) ::  numframe         
      integer, intent(out) ::  nbplanet
      ! Numero planetes (pas l'ancien numero CNES)  PTPT
      integer, intent(out) ::  numpla(nbplanetmx)
      integer, intent(out) ::  ibid1,ibid2
      ! date de debut et de fin au format Julien
      real (KIND=pm_reel), intent(out) :: bid2,tdeb,tfin
      real (KIND=pm_reel), intent(out) :: tframe, vrot, lref, pole_u, pole_v, obliq
      logical, intent(out) :: datecour
      integer, intent(out) :: ier
!
! Variables locales
!
      integer :: iacces
      integer :: iretour
      integer :: ipla
      character (LEN=30) :: nomplac
      character (LEN=30) :: nompla(nbplanetmx)
      character (LEN=25) :: ctdeb,ctfin
      ! DM 387 : integration COMPAS V2-0
      integer, dimension(:), pointer :: lcorps => NULL()
      character(LEN=32) :: ctframe

!     Acces au fichier de configuration

!
! Ouverture zone d'acces et connexion du fichier en lecture
!
      iacces = acc_open()
      iretour=acc_connect(iacces,fic_ephem,'r')
      iretour=acc_read(iacces, ACC_ALL)

! Recuperation des donnees      
      ier = 1
!-------------------------------------------------------------------------------
!	Nom du Corps central
!-------------------------------------------------------------------------------
      iretour=acc_gets(iacces, "Corps_central",nomplac)

!  Conversion nom corps central en numero CNES AMLIB
      call cps_getCorps("nom_fr", trim(nomplac), lcorps)
      if (MSP_gen_messages("lentfic_pla_MAD")) return
      
      if (size(lcorps).gt.0) then
         numplac = lcorps(1)
      end if
      if (associated(lcorps)) then
         deallocate(lcorps)
      end if
      
!-------------------------------------------------------------------------------
!	Planètes 
!-------------------------------------------------------------------------------
      nbplanet=acc_get_dim(iacces,"Planetes")
      iretour=acc_select(iacces, "Planetes", ACC_TABL)
      do ipla=1,nbplanet
          iretour=acc_set_index(iacces,ipla)
          iretour=acc_gets(iacces,ACC_INDEX,nompla(ipla))
      enddo
      iretour=acc_select_end(iacces)

!     Conversion des noms planetes en numero CNES
      do ipla=1,nbplanet
         call cps_getCorps("nom_fr", trim(nompla(ipla)), lcorps)
      if (MSP_gen_messages("lentfic_pla_MAD")) return
         if (size(lcorps).gt.0) then
            numpla(ipla) = lcorps(1)
         end if
         if (associated(lcorps)) then
            deallocate(lcorps)
         end if
      end do

!-------------------------------------------------------------------------------
!	Nom Repère Ephémerides
!-------------------------------------------------------------------------------

      iretour = acc_geti(iacces, "Repere", numframe)

!-------------------------------------------------------------------------------
!	DM 387 : date du repere
!-------------------------------------------------------------------------------
      ! tframe
      iretour = acc_gets(iacces, "Date_repere", ctframe)

      if(trim(ctframe).eq."date du bulletin") then
         datecour=.true.
      else
         datecour=.false.
         call eph_cdatejj50(ctframe,tframe)
         if (MSP_gen_messages("lentfic_pla_MAD")) return
      endif

!-------------------------------------------------------------------------------
!	DM 387 : infos de GS_AXES utiles pour le changement de repere
!                effectue dans ui_tephemtche avec GS_changer_repere()
!-------------------------------------------------------------------------------
      ! initialisation
      vrot = 0._pm_reel
      lref = 0._pm_reel
      pole_u = 0._pm_reel
      pole_v = 0._pm_reel
      obliq = 0._pm_reel
      
      ! vitesse de rotation
      iretour = acc_exist(iacces, "vrot")
      if (iretour.eq.1) then
         iretour = acc_getd(iacces, "vrot", vrot, "rad/s")
      end if
      
      ! longitude de reference
      iretour = acc_exist(iacces, "lref")
      if (iretour.eq.1) then
         iretour = acc_getd(iacces, "lref", lref, "deg")
      end if
      
      ! coordonnee u du pole
      iretour = acc_exist(iacces, "angle_u")
      if (iretour.eq.1) then
         iretour = acc_getd(iacces, "angle_u", pole_u, "deg")
      end if
      
      ! coordonnee v du pole
      iretour = acc_exist(iacces, "angle_v")
      if (iretour.eq.1) then
         iretour = acc_getd(iacces, "angle_v", pole_v, "deg")
      end if

      ! obliquite
      iretour = acc_exist(iacces, "obli")
      if (iretour.eq.1) then
         iretour = acc_getd(iacces, "obli", obliq, "deg")
      end if
      
      
!-------------------------------------------------------------------------------
!	date debut du fichier
!-------------------------------------------------------------------------------
      iretour=acc_gets(iacces, "Date_debut",ctdeb)
      call eph_cdatejj50(ctdeb,tdeb)
      if (MSP_gen_messages("lentfic_pla_MAD")) return

!-------------------------------------------------------------------------------
!	date fin du fichier
!-------------------------------------------------------------------------------
      iretour=acc_gets(iacces, "Date_fin",ctfin)
      call eph_cdatejj50(ctfin,tfin)
      if (MSP_gen_messages("lentfic_pla_MAD")) return

! 9999  continue

! Deconnexion fichier et fermeture zone
!
      iretour=acc_deconnect(iacces, 'r')
      iretour=acc_close(iacces)

      ier = 0

    end subroutine lentfic_pla_MAD
