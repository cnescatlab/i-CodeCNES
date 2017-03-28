module MSP_SEPARATION_IMP_DEF

!*******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_SEPARATION_IMP_DEF
!
!$Resume
!  Module contenant les informations de base aux modules liés aux séparations.
!
!$Description
!  Module contenant les informations de base aux modules liés aux séparations.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_SEPARATION_IMP_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_SEPARATION_IMP_DEF.F90,v $
!  Revision 1.19  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.18  2008/11/19 13:34:06  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.17  2008/10/24 08:02:33  huec
!  AQ : Indentation du code
!  Revision 1.16  2008/04/24 13:59:08  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.15  2007/02/02 08:28:26  tanguyy
!  DM-ID 659 : rajout d'un type de variation MSP_ENUM_COEFF_NULS pour gerer les init par defaut de structure AERO
!  Revision 1.14  2006/06/02 11:21:55  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!  Revision 1.13  2005/03/08 07:32:37  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.12  2005/02/01 08:59:22  fabrec
!  DM-ID 235 : typcf
!  Revision 1.11  2005/01/31 16:36:06  fabrec
!  DM-ID 235 : ajout de typcf
!  Revision 1.10  2005/01/10 12:48:10  vivaresf
!  FA_321
!  Revision 1.9.2.2  2005/01/07 14:21:00  vivaresf
!  Effacement
!  Revision 1.9.2.1  2005/01/07 10:49:42  vivaresf
!  FA-ID 321 : effacement propre de pard et aero avant re-affecation
!  Revision 1.9  2004/10/25 10:14:34  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.8  2004/10/22 08:24:28  vivaresf
!  Gestion des newaero et newprad pour affichage
!  Revision 1.7  2004/10/21 14:39:01  vivaresf
!  FA-ID 218 : fuite memoires sur les pointeurs
!  Revision 1.10  2004/10/20 16:45:00 jfb
!  - Ajout du champ flag_func dans la structure MSP_AERO pour la 
!    gestion des fuites mémoires 
!  - Ajout d'une procedure MSP_effacer_separation pour desallouer proprement
!    une structure separation
! - Ajout d'une procedure MSP_egaler_separation permettant la surcharge 
!   de l'operateur "=" entre deux lois de separation
!  Revision 1.9  2004/09/20 13:24:16  vivaresf
!  version V3-3-1
!  Revision 1.6  2004/06/22 09:32:21  vivaresf
!  DM_133
!  Revision 1.3.2.2  2004/06/17 11:01:21  vivaresf
!  DM-ID 133 : masse en delta ou en absolu, champs PRAD et AERO dans la separation
!  Revision 1.3.2.1  2004/06/08 10:25:41  vivaresf
!  DM-ID 133 : rajout de structures prad et aero dans la separation
!  Revision 1.3  2004/05/03 15:24:12  vivaresf
!  Intégration DM_83
!  Revision 1.2.2.1  2004/05/03 14:59:40  vivaresf
!  DM-ID 83,dates en jour / secondes avec origine MJD1950 ou MJD2000
!  Revision 1.2  2002/12/03 17:21:04  adm_ipsi
!   Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:09:36  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.4  2000/06/15 08:43:57  util_am
!  - Privatisation du contenu de la structure MSP_SEPARATION
!  - Ajout de la MSP_afficher_separation, MSP_consulter_separation MSP_modifier_separation
!  - Transfert de la routine MSP_creer_separation de MSP_SEPARATION_IMP_DEF.F90
!  - Ajout d'interface anglaise pour les routines et fonctions publiques
!  - Mise à jour des cartouches
!  Revision 1.3  1999/10/26 07:51:37  util_am
!  Ajout du paramètre st
!  Revision 1.2  1999/10/25 15:28:26  util_am
!  Ajout des surfaces de panneaux solaires
!  Revision 1.1.1.1  1999/07/13 08:37:56  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_SEPARATION_IMP_DEF
!
!$Structure
!
!: MSP_SEPARATION : définition d'une loi de séparation
!#V
!>     flag_func   : <logical,private>      
!>     typdat      : <integer,private>      type de date:
!.                           1 => date [Jours Juliens CNES]
!.                           2 => durée [s]
!>     date        : <tm_jour_sec,private>  date en jour/secondes
!>     duree       : <pm_reel,private>      duree (si typdate == 2)
!>     origdat     : <integer,private>      origine des dates (0=J50, 1=MJD2000)
!>     deltav      : <pm_reel,private>      incrément de vitesse [m/s]
!>     omega       : <pm_reel,private>      angle dans le plan de symétrie du véhicule [rad]
!>     omegap      : <pm_reel,private>      angle hors du plan de symétrie du véhicule [rad]
!>     forme       : <integer,private>      forme du véhicule après séparation (MSP_ENUM_SPHERE, MSP_ENUM_PLAQUE,
!>                           MSP_ENUM_CYLINDRE, MSP_ENUM_PARALLEPIPEDE)
!>     sx          : <pm_reel,private>      surface perpendiculaire à l'axe X du véhicule après séparation [m^2]
!>     sy          : <pm_reel,private>      surface perpendiculaire à l'axe Y du véhicule après séparation [m^2]
!>     sz          : <pm_reel,private>      surface perpendiculaire à l'axe Z du véhicule après séparation [m^2]
!>     st          : <pm_reel,private>      surface transverse de révolution  du véhicule après séparation [m^2]
!>     spx         : <pm_reel,private>      surface des panneaux solaires perpendiculaire à l'axe X du véhiculeaprès séparation [m^2]
!>     spy         : <pm_reel,private>      surface des panneaux solaires perpendiculaire à l'axe Y du véhicule après séparation [m^2]
!>     spz         : <pm_reel,private>      surface des panneaux solaires perpendiculaire à l'axe Z du véhicule après séparation [m^2]
!>     merg        : <pm_reel,private>      masse d'ergols utilisée ou masse totale restante [kg] 
!>     tymasse     : <integer,private>      Type de masse rentrée : 
!.                                        1 : masse d'ergols utilisée pour la séparation [defaut]
!.                                        2 : masse totale restante aprèsa séparation
!>     typcf       : <integer,private>      type de frottement aero apres separation
!>     newprad     : <logical,private>      Si false, prad ne doit pas etre pris en compte
!>     prad        : <MSP_PRAD,private>     Nouveaux coefficients de pression de radiation après séparation
!>     newaero     : <logical,private>      Si false, prad ne doit pas etre pris en compte
!>     aero        : <MSP_AERO,private>     Nouveaux coefficients aeronautique après séparation
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_separation
!- MSP_get_separation_data
!- MSP_set_separation_data
!- MSP_display_separation
!- MSP_effacer_separation
!- MSP_consulter_separation
!- MSP_modifier_separation
!- MSP_afficher_separation
!#V
!- egaler_separation
!#
!
!$Fonctions
!- MSP_creer_separation
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSPRO
!- MSP_AERO_DEF
!- MSP_PRAD_DEF
!- MSP_GESTION_ERREUR
!- MSP_VEHICULE_DEF
!#
!
!$Interface
!> msp_get_separation_data :  MSP_consulter_separation
!> assignment :               egaler_separation
!> msp_create_separation :    MSP_creer_separation
!> msp_display_separation :   MSP_afficher_separation
!> msp_set_separation_data :  MSP_modifier_separation
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!#V
!.  egaler_separation
!#
!.  MSP_creer_separation MSP_create_separation MSP_get_separation_data MSP_set_separation_data
!.  MSP_display_separation MSP_effacer_separation MSP_consulter_separation MSP_modifier_separation
!.  MSP_afficher_separation
!
!$<>
!******************************************************************************

   use MSLIB, only : pm_reel
   use MSPRO

   ! Separation permet redefinir un vehicule, d'ou besoin des constituants
   use MSP_AERO_DEF
   use MSP_PRAD_DEF


   ! TYPES DERIVES:

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_SEPARATION_IMP_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


   type MSP_SEPARATION
      private
      logical :: flag_func   

      integer            :: typdat
      type(tm_jour_sec)  :: date
      real(KIND=pm_reel) :: duree
      integer            :: origdat
      real(KIND=pm_reel) :: deltav
      real(KIND=pm_reel) :: omega
      real(KIND=pm_reel) :: omegap

      ! =%= structure MCI
      integer :: forme
      real(KIND=pm_reel) :: sx
      real(KIND=pm_reel) :: sy
      real(KIND=pm_reel) :: sz
      real(KIND=pm_reel) :: st
      real(KIND=pm_reel) :: spx
      real(KIND=pm_reel) :: spy
      real(KIND=pm_reel) :: spz
      real(KIND=pm_reel) :: merg
      integer            :: tymasse
      integer            :: typcf

      ! structure PRAD
      logical :: newprad
      type(MSP_PRAD) :: prad

      ! structure AERO
      logical :: newaero
      type(MSP_AERO) :: aero
   end type MSP_SEPARATION

   ! SOUS-PROGRAMMES ET FONCTIONS
   private :: egaler_separation

   interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!   str2=str1
!.    type(MSP_SEPARATION) :: str2
!.    type(MSP_SEPARATION) :: str1
!
!$Procedures
!#V
!- egaler_separation
!#
!
!$Remarques
!
!$Mots-cles
! SEPARATION EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure egaler_separation
   end interface


   interface MSP_create_separation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_separation
!
!$Resume
!  Creation of a separation law
!
!$Description
!  Creation of a separation law
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_create_separation (typdat,[date],[deltav],[omega],[omegap],[forme],&
!.            sx,sy,sz,st,spx,spy,spz,tymasse,typcf,merg,aero,prad,date_js,origdat)
!.    integer :: typdat
!.    real(KIND=pm_reel) :: date, deltav
!.    real(KIND=pm_reel) :: omega, omegap
!.    integer :: forme, tymasse, typcf
!.    real(KIND=pm_reel) :: sx, sy, sz, st, spx, spy, spz,merg
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!.    type(MSP_AERO) :: aero
!.    type(MSP_PRAD) :: prad
!.    type(MSP_SEPARATION) :: loi
!
!$Procedures
!- MSP_creer_separation
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_separation
   end interface
  
   interface MSP_get_separation_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_separation_data
!
!$Resume
!  Gets characteristics of a separation law
!
!$Description
!  Gets characteristics of a separation law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_separation_data (loi_sep, [typdat], [date], [deltav], [omega], [omegap], [forme], &
!.            sx, sy, sz, st, spx, spy, spz, tymasse, typcf, merg,aero,prad,newprad,newaero, date_js, origdat)
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: deltav
!.    real(KIND=PM_REEL) :: omega
!.    real(KIND=PM_REEL) :: omegap
!.    integer :: forme,tymasse,typcf
!.    real(KIND=PM_REEL) :: sx
!.    real(KIND=PM_REEL) :: sy
!.    real(KIND=PM_REEL) :: sz
!.    real(KIND=PM_REEL) :: st
!.    real(KIND=PM_REEL) :: spx
!.    real(KIND=PM_REEL) :: spy
!.    real(KIND=PM_REEL) :: spz
!.    real(KIND=PM_REEL) :: merg
!.    type (MSP_AERO) :: aero
!.    type (MSP_PRAD) :: prad
!.    logical :: newprad,newaero
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!
!$Procedures
!- MSP_consulter_separation
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_separation
   end interface
   
   interface MSP_set_separation_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_separation_data
!
!$Resume
!  Modifies characteristics of a separation law
!
!$Description
!  Modifies characteristics of a separation law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_separation_data(loi_sep, [typdat], [date], [deltav], [omega], &
!.            omegap, forme, sx, sy, sz, st, spx, spy, spz, tymasse, typcf, merg, &
!.            newprad,newaero,aero,prad, date_js,origdat)
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: deltav
!.    real(KIND=PM_REEL) :: omega
!.    real(KIND=PM_REEL) :: omegap
!.    integer :: forme, tymasse,typcf
!.    real(KIND=PM_REEL) :: sx
!.    real(KIND=PM_REEL) :: sy
!.    real(KIND=PM_REEL) :: sz
!.    real(KIND=PM_REEL) :: st
!.    real(KIND=PM_REEL) :: spx
!.    real(KIND=PM_REEL) :: spy
!.    real(KIND=PM_REEL) :: spz
!.    real(KIND=PM_REEL) :: merg
!.    type(MSP_AERO) :: aero
!.    type(MSP_PRAD) :: prad
!.    logical :: newprad,newaero
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!
!$Procedures
!- MSP_modifier_separation
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_separation
   end interface

   interface MSP_display_separation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_separation
!
!$Resume
!  Displays characteristics of a separation law
!
!$Description
!  Displays characteristics of a separation law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_separation(loi_sep, num)
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: num
!
!$Procedures
!- MSP_afficher_separation
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_separation
   end interface

   contains

    subroutine MSP_effacer_separation(str,nul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_separation
!
!$Resume
!	Routine permettant de désallouer proprement une structure separation
!
!$Description
!	Routine permettant de désallouer proprement une structure separation
!
!$Auteur
!       J. Becquaert
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_separation(str,[nul])
!.    type(MSP_SEPARATION) :: str
!.    logical :: nul
!
!$Arguments
!>E/S   str  :<MSP_SEPARATION>   
!>[E]   nul  :<logical>          =.true., on se contente des instructions NULLIFY (par défaut .false.)
!
!$Common
!
!$Routines
!- MSP_effacer_prad
!- MSP_effacer_aero
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SEPARATION EFFACER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      implicit none

      type(MSP_SEPARATION)::str
      logical, intent(in), optional :: nul

      logical :: nul_tmp

      if ( present (nul) ) then
         nul_tmp = nul
      else
         nul_tmp = .false.
      endif
        
      ! désallocation propre du début de la structure str
      str%typdat     = 2
      str%date%jour  = 0
      str%date%sec   = 0._PM_REEL
      str%duree      = 0._PM_REEL
      str%origdat    = 0
      str%deltav     = 0._PM_REEL
      str%omega      = 0._PM_REEL
      str%omegap     = 0._PM_REEL

      ! desallocation de la partie MCI de la structure str
      str%forme      = 0
      str%sx         = 0._PM_REEL
      str%sy         = 0._PM_REEL
      str%sz         = 0._PM_REEL
      str%st         = 0._PM_REEL
      str%spx        = 0._PM_REEL
      str%spy        = 0._PM_REEL
      str%spz        = 0._PM_REEL
      str%merg       = 0._PM_REEL
      str%tymasse    = 0
      str%typcf    = 0
       
      ! desallocation des parties PRAD et AERO de la structure str
      call MSP_effacer_prad(str%prad)
      call MSP_effacer_aero(str%aero,nul=nul_tmp)
      str%newaero=.false.
      str%newprad=.false.

    end subroutine MSP_effacer_separation

   subroutine egaler_separation (str2,str1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_separation
!
!$Resume
!	Routine definissant l'affectation de 2 structures separations
!
!$Description
!	Routine definissant l'affectation de 2 structures separations
!
!$Auteur
!       J. J. Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_separation (str2,str1)
!.    type(MSP_SEPARATION) :: str2
!.    type(MSP_SEPARATION) :: str1
!
!$Arguments
!>E/S   str2  :<MSP_SEPARATION>   
!>E     str1  :<MSP_SEPARATION>   
!
!$Common
!
!$Routines
!- MSP_effacer_separation
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SEPARATION EGALER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      implicit none

      type(MSP_SEPARATION), intent(INOUT) :: str2
      type(MSP_SEPARATION), intent(IN)  :: str1

      if ( MSP_gen_messages("egaler_separation") ) return

      str2%flag_func=.false.
      str2%typdat     = str1%typdat
      str2%date       = str1%date
      str2%duree      = str1%duree
      str2%origdat    = str1%origdat
      str2%deltav     = str1%deltav
      str2%omega      = str1%omega
      str2%omegap     = str1%omegap

      str2%forme      = str1%forme
      str2%sx         = str1%sx
      str2%sy         = str1%sy
      str2%sz         = str1%sz
      str2%st         = str1%st
      str2%spx        = str1%spx
      str2%spy        = str1%spy
      str2%spz        = str1%spz
      str2%merg       = str1%merg
      str2%tymasse    = str1%tymasse
      str2%typcf      = str1%typcf
      str2%newprad    = str1%newprad
      str2%prad       = str1%prad
      str2%newaero    = str1%newaero
      str2%aero       = str1%aero

     if ( str1%flag_func) then
        call MSP_effacer_separation(str1)
      end if

    end subroutine egaler_separation

   function MSP_creer_separation (typdat,date,deltav,omega,omegap,forme,&
        sx,sy,sz,st,spx,spy,spz,tymasse,typcf,merg,aero,prad,date_js,origdat) result(loi)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_separation
!
!$Resume
!  Fonction servant à créer une loi de type MSP_SEPARATION.
!
!$Description
!  Fonction servant à créer une loi de type MSP_SEPARATION.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_creer_separation (typdat,[date],[deltav],[omega],[omegap],[forme],&
!.            [sx],[sy],[sz],[st],[spx],[spy],[spz],[tymasse],[typcf],[merg],[aero],[prad],[date_js],[origdat])
!.    integer :: typdat
!.    real(KIND=pm_reel) :: date, deltav
!.    real(KIND=pm_reel) :: omega, omegap
!.    integer :: forme, tymasse, typcf
!.    real(KIND=pm_reel) :: sx, sy, sz, st, spx, spy, spz,merg
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!.    type(MSP_AERO) :: aero
!.    type(MSP_PRAD) :: prad
!.    type(MSP_SEPARATION) :: loi
!
!$Arguments
!>E     typdat   :<integer>          type de date:
!.                                  1 => date [Jours Juliens CNES]
!.                                  2 => durée [s]
!>[E]   date     :<pm_reel>          date [JJ CNES ou s]
!>[E]   deltav   :<pm_reel>          incrément de vitesse [m/s]
!>[E]   omega    :<pm_reel>          angle dans le plan de symétrie du véhicule [rad] [par défaut 0.]
!>[E]   omegap   :<pm_reel>          angle hors du plan de symétrie du véhicule [rad] [par défaut 0.]
!>[E]   forme    :<integer>          forme du véhicule après séparation (MSP_ENUM_SPHERE, MSP_ENUM_PLAQUE,
!>                                  MSP_ENUM_CYLINDRE, MSP_ENUM_PARALLEPIPEDE) [par défaut MSP_ENUM_SPHERE]
!>[E]   sx       :<pm_reel>          surface perpendiculaire à l'axe X du véhicule après séparation [m^2] [par défaut 0.]
!>[E]   sy       :<pm_reel>          surface perpendiculaire à l'axe Y du véhicule après séparation [m^2] [par défaut 0.]
!>[E]   sz       :<pm_reel>          surface perpendiculaire à l'axe Z du véhicule après séparation [m^2] [par défaut 0.]
!>[E]   st       :<pm_reel>          surface transverse de révolution  du véhicule après séparation [m^2] [par défaut 0.]
!>[E]   spx      :<pm_reel>          surface des panneaux solaires perpendiculaire à l'axe X du véhicule après séparation [m^2] [par défaut 0]
!>[E]   spy      :<pm_reel>          surface des panneaux solaires perpendiculaire à l'axe Y du véhicule après séparation [m^2] [par défaut 0]
!>[E]   spz      :<pm_reel>          surface des panneaux solaires perpendiculaire à l'axe Z du véhicule après séparation [m^2] [par défaut 0]
!>[E]   tymasse  :<integer>          Type de masse rentrée : 
!.                                        1 : masse d'ergols utilisée pour la séparation [defaut]
!.                                        2 : masse totale restante aprèsa séparation
!>[E]   typcf    :<integer>          type de frottement aero apres separation
!>[E]   merg     :<pm_reel>          masse d'ergol restante après séparation [kg] [par défaut 0.]
!>[E]   aero     :<MSP_AERO>         sous-structure contenant les données aérodynamiques
!                                    après séparation (si absente, newaero vaut false)
!>[E]   prad     :<MSP_PRAD>         sous-structure contenant les données relatives à la pression de radiation
!                                    solaire après séparation (si absente, newprad vaut false)
!>[E]   date_js  :<tm_jour_sec>      date en jour/secondes (si present, remplace date)
!>[E]   origdat  :<integer>          origine des dates (0=J50, 1=MJD2000) [par defaut 0]
!>S     loi      :<MSP_SEPARATION>   structure contenant les données de la séparation
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_jourfrac_joursec
!- MSP_effacer_prad
!- MSP_effacer_aero
!
!$Include
!
!$Module
!#V
!- MSP_GESTION_ERREUR
!- MSP_VEHICULE_DEF
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      use MSP_GESTION_ERREUR
      use MSP_VEHICULE_DEF

      implicit none

      ! Parametres
      integer, intent(IN)            :: typdat

      ! Parametres optionnels
      real(KIND=pm_reel), intent(IN), optional :: date, deltav
      real(KIND=pm_reel), intent(IN), optional :: omega, omegap
      integer, intent(IN), optional  :: forme, tymasse, typcf
      real(KIND=pm_reel), intent(IN), optional :: sx, sy, sz, st, spx, spy, spz,merg
      type(tm_jour_sec), intent(IN), optional  :: date_js
      integer, intent(IN), optional  :: origdat
      type(MSP_AERO), intent(IN), optional :: aero
      type(MSP_PRAD), intent(IN), optional :: prad

      ! Retour de la fonction
      type(MSP_SEPARATION) :: loi

      ! Variables locales
      type(tm_code_retour) :: code_retour

      ! Initialisations

      loi%date%jour = 0
      loi%date%sec = 0._PM_REEL
      loi%duree = 0._PM_REEL

      ! Corps da la routine
      loi%typdat = typdat

      if (loi%typdat == 2) then 
         if (present(date))then
            loi%duree = date
         else
            call MSP_signaler_message(cle_mes="MSP_dateabs", &
                 message="La variable date doit être renseignée",&
                 routine="MSP_creer_separation")
            return
         endif
      else if (loi%typdat == 1) then 
         if (present(date_js)) then
            loi%date=date_js
         elseif (present(date)) then
            call md_jourfrac_joursec(date, loi%date, code_retour)
         else
            call MSP_signaler_message(cle_mes="MSP_dateabs", &
                 message="Les variables date ou date_js doivent être renseignées",&
                 routine="MSP_creer_separation")
            return
         endif
      ! Autres cas (Erreurs corrigées plus tard)
      else
         if (present(date))    loi%duree = date
         if (present(date_js)) loi%date=date_js
      endif

      if (present(deltav)) then
         loi%deltav = deltav
      else
         call MSP_signaler_message(cle_mes="MSP_dateabs", &
              message="La variable deltav doit être renseignée",&
              routine="MSP_creer_separation")
         return
      endif

      if ( PRESENT(omega) ) then
         loi%omega  = omega
      else
         loi%omega  = 0._pm_reel
      endif

      if ( PRESENT(omegap) ) then
         loi%omegap  = omegap
      else
         loi%omegap  = 0._pm_reel
      endif

      if ( PRESENT(forme) ) then
         loi%forme  = forme
      else
         loi%forme  = MSP_ENUM_SPHERE
      endif

      if ( PRESENT(sx) ) then
         loi%sx  = sx
      else
         loi%sx  = 0._pm_reel
      endif

      if ( PRESENT(sy) ) then
         loi%sy  = sy
      else
         loi%sy  = 0._pm_reel
      endif

      if ( PRESENT(sz) ) then
         loi%sz  = sz
      else
         loi%sz  = 0._pm_reel
      endif

      if ( PRESENT(st) ) then
         loi%st  = st
      else
         loi%st  = 0._pm_reel
      endif

      if ( PRESENT(spx) ) then
         loi%spx  = spx
      else
         loi%spx  = 0._pm_reel
      endif

      if ( PRESENT(spy) ) then
         loi%spy  = spy
      else
         loi%spy  = 0._pm_reel
      endif

      if ( PRESENT(spz) ) then
         loi%spz  = spz
      else
         loi%spz  = 0._pm_reel
      endif

      if ( PRESENT(merg) ) then
         loi%merg  = merg
      else
         loi%merg  = 0._pm_reel
      endif

      if ( PRESENT(tymasse) ) then
         loi%tymasse  = tymasse
      else
         loi%tymasse  = 0
      endif

      if ( PRESENT(typcf) ) then
         loi%typcf  = typcf
      else
         loi%typcf  = 0
      endif

      if ( PRESENT(origdat) ) then
         loi%origdat  = origdat
      else
         loi%origdat  = 0
      endif

      call MSP_effacer_prad(loi%prad)
      loi%flag_func=.true.

      if ( present(prad) ) then
         loi%prad = prad
         loi%newprad=.true.
      else
         loi%newprad=.false.
         loi%prad = MSP_creer_prad ()
      endif

      call MSP_effacer_aero(loi%aero,nul=.true.)
      if ( PRESENT(aero) ) then
         loi%aero  = aero
         loi%newaero=.true.
      else
         loi%newaero=.false.
         ! init des coefs aero avec des coefficients nuls, exprimés dans le repère aérodynamique.
         loi%aero = MSP_creer_aero (type_coef=MSP_ENUM_COEFF_AERO_VITESSE,type_variation=MSP_ENUM_COEFF_NULS)
      endif

   end function MSP_creer_separation
      

   SUBROUTINE MSP_consulter_separation (loi_sep, typdat, date, deltav, omega, omegap, forme, &
        sx, sy, sz, st, spx, spy, spz, tymasse, typcf, merg,aero,prad,newprad,newaero, date_js, origdat)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_separation
!
!$Resume
!  Routine de consultation d'une loi de separation
!
!$Description
!  Routine de consultation d'une loi de separation
!
!$Auteur
!  J. J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_separation (loi_sep, [typdat], [date], [deltav], [omega], [omegap], [forme], &
!.            [sx], [sy], [sz], [st], [spx], [spy], [spz], [tymasse], [typcf], [merg],[aero],[prad],[newprad],[newaero], [date_js], [origdat])
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: deltav
!.    real(KIND=PM_REEL) :: omega
!.    real(KIND=PM_REEL) :: omegap
!.    integer :: forme,tymasse,typcf
!.    real(KIND=PM_REEL) :: sx
!.    real(KIND=PM_REEL) :: sy
!.    real(KIND=PM_REEL) :: sz
!.    real(KIND=PM_REEL) :: st
!.    real(KIND=PM_REEL) :: spx
!.    real(KIND=PM_REEL) :: spy
!.    real(KIND=PM_REEL) :: spz
!.    real(KIND=PM_REEL) :: merg
!.    type (MSP_AERO) :: aero
!.    type (MSP_PRAD) :: prad
!.    logical :: newprad,newaero
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!
!$Arguments
!>E     loi_sep  :<MSP_SEPARATION>   Loi de separation a consulter
!>[S]   typdat   :<integer>          type de date:
!.                                     1 => date [Jours Juliens CNES]
!.                                     2 => durée [s]
!>[S]   date     :<PM_REEL>          date [JJ CNES ou s]
!>[S]   deltav   :<PM_REEL>          incrément de vitesse [m/s]
!>[S]   omega    :<PM_REEL>          angle dans le plan de symétrie du véhicule [rad]
!>[S]   omegap   :<PM_REEL>          angle hors du plan de symétrie du véhicule [rad]
!>[S]   forme    :<integer>          forme du véhicule (MSP_ENUM_SPHERE, MSP_ENUM_PLAQUE,
!>                                      MSP_ENUM_CYLINDRE, MSP_ENUM_PARALLEPIPEDE)
!>[S]   sx       :<PM_REEL>          surface perpendiculaire à l'axe X du véhicule [m^2]
!>[S]   sy       :<PM_REEL>          surface perpendiculaire à l'axe Y du véhicule [m^2]
!>[S]   sz       :<PM_REEL>          surface perpendiculaire à l'axe Z du véhicule [m^2]
!>[S]   st       :<PM_REEL>          surface transverse de révolution du véhicule [m^2]
!>[S]   spx      :<PM_REEL>          surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2]
!>[S]   spy      :<PM_REEL>          surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2]
!>[S]   spz      :<PM_REEL>          surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2]
!>[S]   tymasse  :<integer>          Type de masse rentrée : 
!.                                        1 : masse d'ergols utilisée pour la séparation [defaut]
!.                                        2 : masse totale restante aprèsa séparation
!>[S]   typcf    :<integer>          type de frottement aero apres separation
!>[S]   merg     :<PM_REEL>          masse d'ergols restante après séparation [kg]
!>[S]   aero     :<MSP_AERO>         Coefficients de pression de radiation solaire aerodynamiques après séparation
!>[S]   prad     :<MSP_PRAD>         Coefficients aerodynamiques après séparation
!>[S]   newprad  :<logical>          Si false, la structure prad n'a pas ete initialisee
!>[S]   newaero  :<logical>          Si false, la structure aero n'a pas ete initialisee
!>[S]   date_js  :<tm_jour_sec>      date en jour/secondes
!>[S]   origdat  :<integer>          origine des dates (0=J50, 1=MJD2000)
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!
!$Include
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

     ! Entrée obligatoire
     type(MSP_SEPARATION), intent(IN) :: loi_sep

     ! Sorties possibles
     integer, intent(OUT), optional            :: typdat
     real(KIND=PM_REEL), intent(OUT), optional :: date
     real(KIND=PM_REEL), intent(OUT), optional :: deltav
     real(KIND=PM_REEL), intent(OUT), optional :: omega
     real(KIND=PM_REEL), intent(OUT), optional :: omegap
     integer, intent(OUT), optional :: forme,tymasse,typcf
     real(KIND=PM_REEL), intent(OUT), optional :: sx
     real(KIND=PM_REEL), intent(OUT), optional :: sy
     real(KIND=PM_REEL), intent(OUT), optional :: sz
     real(KIND=PM_REEL), intent(OUT), optional :: st
     real(KIND=PM_REEL), intent(OUT), optional :: spx
     real(KIND=PM_REEL), intent(OUT), optional :: spy
     real(KIND=PM_REEL), intent(OUT), optional :: spz
     real(KIND=PM_REEL), intent(OUT), optional :: merg
     type (MSP_AERO), intent(OUT), optional :: aero
     type (MSP_PRAD), intent(OUT), optional :: prad
     logical, intent(OUT), optional :: newprad,newaero
     type(tm_jour_sec) , intent(OUT), optional :: date_js
     integer, intent(OUT), optional  :: origdat

     ! Variables locales
     type(tm_code_retour) :: code_retour

     ! Instructions

     if (PRESENT(typdat)) typdat = loi_sep%typdat
     if (PRESENT(date)) then
        if (loi_sep%typdat == 1) then 
           call md_joursec_jourfrac(loi_sep%date, date, code_retour)
        else
           date = loi_sep%duree
        endif
     endif
     if (PRESENT(deltav))  deltav   = loi_sep%deltav
     if (PRESENT(omega))   omega    = loi_sep%omega
     if (PRESENT(omegap))  omegap   = loi_sep%omegap
     if (PRESENT(forme))   forme    = loi_sep%forme
     if (PRESENT(sx))      sx       = loi_sep%sx
     if (PRESENT(sy))      sy       = loi_sep%sy
     if (PRESENT(sz))      sz       = loi_sep%sz
     if (PRESENT(st))      st       = loi_sep%st
     if (PRESENT(spx))     spx      = loi_sep%spx
     if (PRESENT(spy))     spy      = loi_sep%spy
     if (PRESENT(spz))     spz      = loi_sep%spz
     if (PRESENT(tymasse)) tymasse  = loi_sep%tymasse
     if (PRESENT(typcf))   typcf    = loi_sep%typcf
     if (PRESENT(merg))    merg     = loi_sep%merg
     if (PRESENT(prad))    prad     = loi_sep%prad
     if (PRESENT(aero))    aero     = loi_sep%aero
     if (PRESENT(newprad)) newprad  = loi_sep%newprad
     if (PRESENT(newaero)) newaero  = loi_sep%newaero
     if (PRESENT(date_js)) date_js  = loi_sep%date
     if (PRESENT(origdat)) origdat  = loi_sep%origdat

   end SUBROUTINE MSP_consulter_separation

   SUBROUTINE MSP_modifier_separation(loi_sep, typdat, date, deltav, omega, &
        omegap, forme, sx, sy, sz, st, spx, spy, spz, tymasse, typcf, merg, &
        newprad,newaero,aero,prad, date_js,origdat)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_separation
!
!$Resume
!  Routine de modification d'une loi de separation
!
!$Description
!  Routine de modification d'une loi de separation
!
!$Auteur
!  J. J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_separation(loi_sep, [typdat], [date], [deltav], [omega], &
!.            [omegap], [forme], [sx], [sy], [sz], [st], [spx], [spy], [spz], [tymasse], [typcf], [merg], &
!.            [newprad],[newaero],[aero],[prad], [date_js],[origdat])
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: deltav
!.    real(KIND=PM_REEL) :: omega
!.    real(KIND=PM_REEL) :: omegap
!.    integer :: forme, tymasse,typcf
!.    real(KIND=PM_REEL) :: sx
!.    real(KIND=PM_REEL) :: sy
!.    real(KIND=PM_REEL) :: sz
!.    real(KIND=PM_REEL) :: st
!.    real(KIND=PM_REEL) :: spx
!.    real(KIND=PM_REEL) :: spy
!.    real(KIND=PM_REEL) :: spz
!.    real(KIND=PM_REEL) :: merg
!.    type(MSP_AERO) :: aero
!.    type(MSP_PRAD) :: prad
!.    logical :: newprad,newaero
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!
!$Arguments
!>E/S   loi_sep  :<MSP_SEPARATION>   Loi de séparation à modifier
!>[E]   typdat   :<integer>          type de date:
!.                                     1 => date [Jours Juliens CNES]
!.                                     2 => durée [s]
!>[E]   date     :<PM_REEL>          date [JJ CNES ou s]
!>[E]   deltav   :<PM_REEL>          incrément de vitesse [m/s]
!>[E]   omega    :<PM_REEL>          angle dans le plan de symétrie du véhicule [rad]
!>[E]   omegap   :<PM_REEL>          angle hors du plan de symétrie du véhicule [rad]
!>[E]   forme    :<integer>          forme du véhicule (MSP_ENUM_SPHERE, MSP_ENUM_PLAQUE,
!>                                      MSP_ENUM_CYLINDRE, MSP_ENUM_PARALLEPIPEDE)
!>[E]   sx       :<PM_REEL>          surface perpendiculaire à l'axe X du véhicule [m^2]
!>[E]   sy       :<PM_REEL>          surface perpendiculaire à l'axe Y du véhicule [m^2]
!>[E]   sz       :<PM_REEL>          surface perpendiculaire à l'axe Z du véhicule [m^2]
!>[E]   st       :<PM_REEL>          surface transverse de révolution du véhicule [m^2]
!>[E]   spx      :<PM_REEL>          surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2]
!>[E]   spy      :<PM_REEL>          surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2]
!>[E]   spz      :<PM_REEL>          surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2]
!>[E]   tymasse  :<integer>          Type de masse rentrée : 
!.                                        1 : masse d'ergols utilisée pour la séparation [defaut]
!.                                        2 : masse totale restante aprèsa séparation
!>[E]   typcf    :<integer>          type de frottement aero apres separation
!>[E]   merg     :<PM_REEL>          masse d'ergols restante après la separation [kg]
!>[E]   newprad  :<logical>          Si false, la structure prad n'a pas ete initialisee 
!>[E]   newaero  :<logical>          Si false, la structure aero n'a pas ete initialisee
!>[E]   aero     :<MSP_AERO>         Structure AERO à mettre dans la structure véhicule
!>[E]   prad     :<MSP_PRAD>         Structure PRAD à mettre dans la structure véhicule
!>[E]   date_js  :<tm_jour_sec>      date en jour/secondes (si présent, prépondérant sur date) 
!>[E]   origdat  :<integer>          origine des dates (0=J50, 1=MJD2000)
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_effacer_prad
!- MSP_effacer_aero
!
!$Include
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

     ! Entrée obligatoire
     type(MSP_SEPARATION), intent(INOUT)      :: loi_sep

     ! Entrées optionnelles
     integer, intent(IN), optional            :: typdat
     real(KIND=PM_REEL), intent(IN), optional :: date
     real(KIND=PM_REEL), intent(IN), optional :: deltav
     real(KIND=PM_REEL), intent(IN), optional :: omega
     real(KIND=PM_REEL), intent(IN), optional :: omegap
     integer, intent(IN), optional :: forme, tymasse,typcf
     real(KIND=PM_REEL), intent(IN), optional :: sx
     real(KIND=PM_REEL), intent(IN), optional :: sy
     real(KIND=PM_REEL), intent(IN), optional :: sz
     real(KIND=PM_REEL), intent(IN), optional :: st
     real(KIND=PM_REEL), intent(IN), optional :: spx
     real(KIND=PM_REEL), intent(IN), optional :: spy
     real(KIND=PM_REEL), intent(IN), optional :: spz
     real(KIND=PM_REEL), intent(IN), optional :: merg
     type(MSP_AERO),     intent(IN), optional :: aero
     type(MSP_PRAD),     intent(IN), optional :: prad
     logical, intent(IN), optional :: newprad,newaero
     type(tm_jour_sec),  intent(in), optional :: date_js
     integer, intent(in), optional            :: origdat

     ! Variables locales
     type(tm_code_retour) :: code_retour

     ! Instructions

     if (PRESENT(typdat)) loi_sep%typdat = typdat
     if (PRESENT(date_js)) then
        loi_sep%date = date_js
     elseif (PRESENT(date)) then
        if (loi_sep%typdat == 1) &
             call md_jourfrac_joursec(date,loi_sep%date,code_retour)
     endif
     if (PRESENT(date).and.loi_sep%typdat == 2) loi_sep%duree = date

     if (PRESENT(deltav))  loi_sep%deltav  = deltav
     if (PRESENT(omega))   loi_sep%omega   = omega
     if (PRESENT(omegap))  loi_sep%omegap  = omegap
     if (PRESENT(forme))   loi_sep%forme   = forme
     if (PRESENT(sx))      loi_sep%sx      = sx
     if (PRESENT(sy))      loi_sep%sy      = sy
     if (PRESENT(sz))      loi_sep%sz      = sz
     if (PRESENT(st))      loi_sep%st      = st
     if (PRESENT(spx))     loi_sep%spx     = spx
     if (PRESENT(spy))     loi_sep%spy     = spy
     if (PRESENT(spz))     loi_sep%spz     = spz
     if (PRESENT(tymasse)) loi_sep%tymasse = tymasse
     if (PRESENT(typcf))   loi_sep%typcf   = typcf
     if (PRESENT(merg))    loi_sep%merg    = merg
     if (PRESENT(prad)) then
        call MSP_effacer_prad(loi_sep%prad)
        loi_sep%prad    = prad
        loi_sep%newprad = .true.
     endif
     if (PRESENT(aero)) then
        call MSP_effacer_aero(loi_sep%aero)
        loi_sep%aero    = aero
        loi_sep%newaero = .true.
     endif
     if (PRESENT(newprad)) loi_sep%newprad = newprad
     if (PRESENT(newaero)) loi_sep%newaero = newaero
     if (PRESENT(origdat)) loi_sep%origdat = origdat

   end SUBROUTINE MSP_modifier_separation


   subroutine MSP_afficher_separation(loi_sep, num)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_separation
!
!$Resume
! Affichage dans un unité logique des caractéristiques d'une loi de séparation
!
!$Description
! Affichage dans un unité logique des caractéristiques d'une loi de séparation
!
!$Auteur
!  Jean-Jaqcues Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_separation(loi_sep, num)
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: num
!
!$Arguments
!>E     loi_sep  :<MSP_SEPARATION>   
!>E     num      :<integer>          
!
!$Common
!
!$Routines
!- MSP_afficher_prad
!- MSP_afficher_aero
!
!$Include
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

     type(MSP_SEPARATION), intent(IN) :: loi_sep
     integer, intent(IN) :: num

     ! variables locales
     character(len=8) :: oo

     ! Ecriture
     oo="MJD1950"
     if (loi_sep%origdat == 1) oo="MJD2000"

     write(num,'(a,i9)') "TYPDAT: ",loi_sep%typdat
     if (loi_sep%typdat == 1) then 
        write(num,'(a,a,i9,a,g21.12)') "DATE:   ",oo, loi_sep%date%jour, "-", loi_sep%date%sec
     else if (loi_sep%typdat == 2) then 
        write(num,'(a,g21.12)') "DATE:   ",loi_sep%duree
     end if

     write(num,'(a,g21.12)') "DELTAV:         ",loi_sep%deltav
     write(num,'(a,g21.12)') "OMEGA:          ",loi_sep%omega
     write(num,'(a,g21.12)') "OMEGAP:         ",loi_sep%omegap
     write(num,'(a,i9)') "FORME:          ",loi_sep%forme
     write(num,'(a,g21.12)') "SX:             ",loi_sep%sx
     write(num,'(a,g21.12)') "SY:             ",loi_sep%sy
     write(num,'(a,g21.12)') "SZ:             ",loi_sep%sz
     if (loi_sep%tymasse == 2 ) then
        write(num,'(a,g21.12)') "NOUVELLE MASSE: ",loi_sep%merg
     else
        write(num,'(a,g21.12)') "MERG CONSOMME:  ",loi_sep%merg
     endif
     if (loi_sep%newprad) call MSP_afficher_prad(loi_sep%prad,ilog=num)
     if (loi_sep%newaero) call MSP_afficher_aero(loi_sep%aero,ilog=num)

   end subroutine MSP_afficher_separation

end module MSP_SEPARATION_IMP_DEF
