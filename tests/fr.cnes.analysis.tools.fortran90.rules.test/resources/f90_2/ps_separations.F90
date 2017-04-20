module ps_separations

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_separations
!
!$Resume
!  Module gérant les données et le sous-programme nécessaires aux séparations.
!
!$Description
!  Module gérant les données et le sous-programme nécessaires aux séparations.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_separations.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_separations.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.31  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.30  2009/10/28 16:19:31  mercadig
!  DM-ID 1018: Renommage de la variable locale dv
!
!  Revision 1.29  2009/06/16 08:24:07  tanguyy
!  rappatriement dans le tronc commun ( cf FA-ID 1306)
!
!  Revision 1.28.2.2  2009/06/12 13:55:11  tanguyy
!  FA-ID 1306 (mise au propre du code)
!
!  Revision 1.28.2.1  2009/06/11 14:12:10  tanguyy
!  FA-ID 1306 : correction d'une fuite memoire : l'intégrateur de Cowell et Runge-Kutta n'était désalloué que pour le véhicule courant
!
!  Revision 1.28  2008/12/08 08:37:39  tanguyy
!  AQ : amelioration des liberations memoires
!
!  Revision 1.27  2008/12/02 16:55:12  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.26  2008/12/02 10:46:48  huec
!  DM-ID 1058 : Suppression de variables inutilisees
!  Revision 1.25  2008/11/26 08:57:12  tanguyy
!  DM-ID 733 : utilisation des structures MECASPA pour les coefs
!  Revision 1.24  2008/09/04 07:53:16  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.23  2007/09/24 15:06:21  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.22  2007/06/21 13:16:03  vivaresf
!  FA-ID 746 : validation
!  Revision 1.21  2007/06/20 12:33:33  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.20  2007/02/05 17:45:44  tanguyy
!  PSIMU V8.7a1
!  Revision 1.19  2007/02/02 13:31:36  tanguyy
!  DM-ID 659 : prise en compte des types de variation des coefs Aero
!  Revision 1.18  2006/12/05 15:09:46  fabrec
!  suppression de variables redondantes
!  Revision 1.17  2006/10/19 15:09:34  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.16.2.2  2006/10/17 09:54:29  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.16.2.1  2006/10/13 07:55:23  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.16  2006/05/30 09:33:13  tanguyy
!  DM-ID 232 : Cloture du FT (Nommage des arguments lors de l appel d une routine ou d une fonction en Fortran 90 ou d un objet GENESIS)
!  Revision 1.15.2.1  2006/05/30 09:32:22  tanguyy
!  DM-ID 232 : nommage des parametres optionnels
!  Revision 1.15  2006/03/15 13:25:21  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.14  2005/11/10 18:37:10  vivaresf
!  Mise à jour des cartouches
!  Revision 1.13  2005/01/28 10:42:26  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.12  2005/01/25 16:21:05  fabrec
!  DM-ID 175
!  Revision 1.11  2005/01/17 15:29:33  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.10  2004/12/10 16:59:18  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.9  2004/06/18 10:33:11  vivaresf
!  Mise a jour des entetes
!  Revision 1.8  2004/06/18 10:06:05  vivaresf
!  DM_133 : integration
!  Revision 1.7.2.1  2004/06/18 09:41:25  vivaresf
!  DM-ID 133, Redefinition possible du vehicule apres separation
!  - prise en compte des deux mode de definition de la masse (delta ou nouvelle)
!  Revision 1.7  2003/07/10 10:07:19  adm_ipsi
!  FA-ID 13 : Controle que la masse >0
!  Revision 1.9  2003/03/28 09:56:47  util_am
!  SLB - Version industrialisée
!  Revision 1.6  2002/12/20 16:40:59  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.5  2002/12/05 17:56:33  boschett
!  Changement du type de la variable 'fotmp' de la subroutine pseject de real à integer
!  Revision 1.4  2002/12/04 14:26:52  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.3  2002/12/03 18:16:07  boschett
!  Utilisation de la constante symbolique PS_ECRAN dans les appels d'écriture à l'écran
!  Revision 1.2  2002/11/26 17:04:57  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:35  laurent
!  Industrialisation PSIMU
!  Revision 1.8  2000/06/21 15:25:00  util_am
!  retrait de l'include domtrad_F.h
!  Revision 1.7  2000/05/29 09:12:07  util_am
!  Utilisation de Domtraduire
!  Revision 1.6  2000/04/17 10:58:18  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.5  2000/01/14 16:36:31  util_am
!  Correction sur le choix de la loi d'attitude quand la date de fin de la loi i-1 est égale à la date de début de la loi i+1 (spécialement pour les poussées et les séparations)
!  Revision 1.4  1999/10/27 11:24:33  util_am
!  Prise en compte des panneaux solaires dans le calcul des forces
!  Revision 1.3  1999/10/26 11:00:13  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:18  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_separations
!
!$Structure
!
!$Global
!
!>  scenar_sep        : <MSP_SCENARIO_LOI,DIM=(PS_NVMAX)>  scenario de separation
!>  nloi_scenar_sep   : <integer,DIM=(PS_NVMAX)>           nombre de lois de separation du scenario     
!>  iloi_scenar_sep   : <integer,DIM=(PS_NVMAX)>           indice de la loi de separation courante  
!$Common
!
!$Routines
!- pseject
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_bulletin
!- ps_caracteristiques
!- ps_attitude
!- ps_evenements
!- ps_ecriture
!- ps_variables
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MECASPA
   use ps_generalites
   use ps_bulletin
   use ps_caracteristiques
   use ps_attitude
   use ps_evenements
   use ps_ecriture
   use ps_variables

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_separations.F90 368 2013-02-19 14:43:59Z aadt $'

  
   type(MSP_SCENARIO_LOI), dimension(PS_NVMAX), save :: scenar_sep
   integer, dimension(PS_NVMAX) :: nloi_scenar_sep
   integer, dimension(PS_NVMAX) :: iloi_scenar_sep

   contains

      subroutine pseject (itsgn,tsign,t,date,par,iter)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pseject
!
!$Resume
!  Sous-programme calculant un changement de trajectoire du a une séparation.
!
!$Description
!  Sous-programme calculant un changement de trajectoire du a une séparation
!  (avec éventuellement une impulsion).
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pseject (itsgn,tsign,t,date,par,iter)
!.    integer :: itsgn,iter
!.    real (KIND=pm_reel) :: t,tsign
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel) :: par(6)
!
!$Arguments
!>E     itsgn  :<integer>            sens de l'intégration:
!.                                    1 => posigrade
!.                                   -1 => rétrograde
!>E     tsign  :<pm_reel>            sens de l'intégration:
!.                                    1 => posigrade
!.                                   -1 => rétrograde   
!>E     t      :<pm_reel,DIM=(IN)>   instant de séparation (s) 
!>E     date   :<tm_jour_sec>        date de séparation (Jours Juliens CNES)
!>E/S   par    :<pm_reel,DIM=(6)>    position/vitesse au debut puis en fin de séparation (m et m/s) 
!>E     iter   :<integer>            numéro de l'itération
!
!$Common
!
!$Routines
!- ps_maj_vecteur_etat
!- pswresu
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- MSP_consulter_scenario
!- MSP_consulter_separation
!- MSP_signaler_message
!- flush
!- MSP_effacer_vehicule
!- MSP_consulter_aero
!- psattit
!- MSP_effacer_prad
!- MSP_effacer_aero
!- MSP_effacer_separation
!
!$Include
!
!$Module
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      ! Arguments
      !==========
      integer, intent(IN) :: itsgn,iter
      real (KIND=pm_reel), intent(IN)    :: t,tsign
      type(tm_jour_sec), intent(in)      :: date
      real (KIND=pm_reel), intent(INOUT) :: par(6)

      ! Variables locales
      !==================
      real (KIND=pm_reel) :: dv(3),dv_RI(3),pgamav(3,3)
      real (KIND=pm_reel) :: delv,omega,omegap

      real (KIND=pm_reel) :: angle1,angle2,angle3,angi1,angi2,angi3

      integer :: type_coef, type_variation
      
      integer :: typa,repa,choix_loi

      type(MSP_SEPARATION) :: loi_sep
      
      integer            :: typdat
      real(KIND=pm_reel) :: deltav

      ! =%= structure MCI
      integer :: forme
      real(KIND=pm_reel) :: sx
      real(KIND=pm_reel) :: sy
      real(KIND=pm_reel) :: sz
      real(KIND=pm_reel) :: st
      real(KIND=pm_reel) :: spx
      real(KIND=pm_reel) :: spy
      real(KIND=pm_reel) :: spz
      integer            :: tymasse

      type(MSP_PRAD) :: sep_prad
      type(MSP_AERO) :: sep_aero
      type(MSP_MCI)  :: mci

      real (KIND=pm_reel) :: masse_sep, masse_totale


      ! Début du code
      !==============

      call MSP_effacer_aero(sep_aero,nul=.true.)
      call MSP_effacer_separation(loi_sep,nul=.true.)

!     Calcul et ecriture des variables selectionnees:
!     ----------------------------------------------
      if ( ilogeve /= 0 ) then
         if (itsgn > 0) then
            choix_loi = 1
         else
            choix_loi = 2
         endif
	 
         call ps_maj_vecteur_etat (t,date,par,choix_loi_atti=choix_loi)
         if ( MSP_gen_messages("pseject") ) return
         call pswresu (1,ilogeph,0,20,iter,t,&
              str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
              str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
              str_eve(iveh)%neve,str_eve(iveh)%aeve)
         if ( MSP_gen_messages("pseject") ) return
         if ( itsgn > 0 ) then
            call pswresu (2,ilogeve,0,20,iter,t,&
                 str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                 str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                 str_eve(iveh)%neve,str_eve(iveh)%aeve)
         else
            call pswresu (2,ilogeve,0,21,iter,t,&
                 str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                 str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                 str_eve(iveh)%neve,str_eve(iveh)%aeve)
         endif
         if ( MSP_gen_messages("pseject") ) return
      endif
      
      !   extraction de la structure vehicule, afin de connaître la masse avant separation
      call MSP_consulter_vehicule(str_car(iveh)%vehicule, mci=mci)
      if ( MSP_gen_messages("pseject") ) return

      call MSP_consulter_mci(mci=mci,mstr=masse_totale)
      if ( MSP_gen_messages("pseject") ) return

      !   extraction de la structure MSP_SEPARATION dans la structure scenario (MSP_SCENARIO_LOI)
      call MSP_consulter_scenario (scenar_sep(iveh), loi_sep=loi_sep)
      if ( MSP_gen_messages("pseject") ) return

      !  Lecture des donnees dans la structure loi de separation (MSP_SEPARATION)
      call MSP_consulter_separation (loi_sep, typdat=typdat, &
           deltav=deltav, omega=omega, &
           omegap=omegap, &
           forme=forme, sx=sx, &
           sy=sy, sz=sz, &
           st=st, spx=spx, &
           spy=spy, spz=spz, &
           tymasse=tymasse, &
           merg=masse_sep, aero=sep_aero,prad=sep_prad)
      if ( MSP_gen_messages("pseject") ) return



      if ((tymasse == 2) .and. &
          (masse_sep > 0._PM_REEL)) then
         ! masse apres separation
         ! - Contrôle que la masse > 0
         ! (la masse d'ergols reste inchangée)
         masse_totale = masse_sep 
         
      else if (tsign*masse_sep < masse_totale ) then
         ! type par defaut : delta de masse
         ! - Controle que la masse ne passe pas en negatif
         masse_totale = masse_totale - tsign*masse_sep
         
      else
         call MSP_signaler_message (cle_mes="PS_MASSE_001",routine="pseject")
         return
      endif


      if ( iscreen == 1 ) then
         write (PS_ECRAN,Domtraduire(nomdomaine,"PS_SEP")) iloi_scenar_sep(iveh)
         call flush (PS_ECRAN)
      endif

      ! nouvelle forme dans str_car
      mci  = MSP_creer_mci (forme=forme,sx=sx,&
           sy=sy,sz=sz,st=st,&
           spx=spx,spy=spy,spz=spz,&
           mstr=masse_totale)
         
      ! Effacement pour s'assurer que l'on n'écrasera pas cette structure
      if (str_car(iveh)%vehiculeinit) then
         call MSP_effacer_vehicule(str_car(iveh)%vehicule)
         str_car(iveh)%vehiculeinit = .false.
      else
         call MSP_effacer_vehicule(str_car(iveh)%vehicule, nul=.true.)
      endif

      str_car(iveh)%vehicule = MSP_creer_vehicule(aero=sep_aero,mci=mci,prad=sep_prad)
      ! le flag de création du véhicule est mis à true, pour que le véhicule soit désalloué après
      str_car(iveh)%vehiculeinit = .true.

      call MSP_consulter_aero(sep_aero,sref=str_car(iveh)%sref,type_coef=type_coef,&
           type_variation=type_variation)
      
      if (type_coef == MSP_ENUM_COEFF_AERO_VITESSE .and. &
           type_variation == MSP_ENUM_INCIDENCE_MACH) then
         !  Coefs variant en fonction de l'incidence et du mach, lecture dans un fichier
         !  -> des traitements particuliers sont effectués pour ce type de coefs
         !  et stocker le type dans une variable PSIMU permet de gagner en efficacité
         !  car il n'est pas nécessaire de "dépiler" la structure MSP_VEHICULE
         str_car(iveh)%typcf = PS_INCIDENCE_MACH
      else
         ! Coefs autres (aérodynamiques variant en fonction de l'altitude, constants, ou qui 
         ! suivent le modèle tabulé).
         str_car(iveh)%typcf = PS_COEFS_AUTRES
      end if

      ! nouveaux coef de pression de radiation solaire
      delv=deltav

!   * Deltav dans le repere vehicule:

      dv(1)=delv*cos(omegap)*cos(omega)
      dv(2)=delv*sin(omegap)
      dv(3)=delv*cos(omegap)*sin(omega)

!   * Calcul de l'attitude:

      if (itsgn > 0) then
         choix_loi = 2
      else
         choix_loi = 1
      endif
      
      call psattit (date,t,par(1),par(4),angle1,angle2,angle3,angi1,angi2,angi3,&
           pgamav,typa,repa,choix_loi=choix_loi)
      if ( MSP_gen_messages("pseject") ) return


!   * Deltav dans le repere RI de PSIMU:

      dv_RI(1)=pgamav(1,1)*dv(1)+pgamav(2,1)*dv(2)+pgamav(3,1)*dv(3)
      dv_RI(2)=pgamav(1,2)*dv(1)+pgamav(2,2)*dv(2)+pgamav(3,2)*dv(3)
      dv_RI(3)=pgamav(1,3)*dv(1)+pgamav(2,3)*dv(2)+pgamav(3,3)*dv(3)

!   * Nouveau vecteur vitesse dans Planeto:

      par(4)=par(4)+tsign*dv_RI(1)
      par(5)=par(5)+tsign*dv_RI(2)
      par(6)=par(6)+tsign*dv_RI(3)

      if ( ilogeve /= 0 ) then
      
        call ps_maj_vecteur_etat (t,date,par,choix_loi_atti=choix_loi)
         if ( MSP_gen_messages("pseject") ) return
         call pswresu (1,ilogeph,iscreen,21,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
              str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
              str_eve(iveh)%neve,str_eve(iveh)%aeve)
         if ( MSP_gen_messages("pseject") ) return
         if ( itsgn > 0 ) then
            call pswresu (2,ilogeve,0,21,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                 str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                 str_eve(iveh)%neve,str_eve(iveh)%aeve)
         else
            call pswresu (2,ilogeve,0,20,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                 str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                 str_eve(iveh)%neve,str_eve(iveh)%aeve)
         endif
         if ( MSP_gen_messages("pseject") ) return
      endif

      ! Libération de la mémoire
      call MSP_effacer_prad (sep_prad)
      if ( MSP_gen_messages("pseject") ) return
      call MSP_effacer_aero (sep_aero)
      if ( MSP_gen_messages("pseject") ) return
      call MSP_effacer_separation (loi_sep)
      if ( MSP_gen_messages("pseject") ) return

      end subroutine pseject

end module ps_separations
