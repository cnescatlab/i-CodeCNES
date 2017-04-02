!$<AM-V2.0>
!
!$Resume
!	Routines utiles pour l' IHM ACSOL2 de COMPAS
!
!$Version
!  $Id: ui_ihm_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_ihm_acsol2.F90,v $
!  Revision 1.3  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.2  2008/04/11 12:42:45  vivaresf
!  Version 2.4 AQ : correction des cartouches
!
!  Revision 1.1  2008/02/08 17:51:23  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!
!  Revision 1.4  2006/05/31 13:08:39  vivaresf
!  COMPAS 2.0 : validation
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function cps_ihm_getAcsol2Sure(date, data_flux, data_ia) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getAcsol2Sure
!
!$Resume
!
!$Description
!  Interface pour l'IHM acces.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_ihm_getAcsol2Sure(date, data_flux, data_ia)
!.    real(KIND=PM_REEL) :: date
!.    real(kind=PM_REEL), dimension(2) :: data_flux
!.    integer, dimension(10) :: data_ia
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>            date des données recherchées (unité jj1950)
!>S     data_flux  :<PM_REEL,DIM=(2)>    données liées aux flux
!>S     data_ia    :<integer,DIM=(10)>   données liées aux indices d'activité
!>S     trouve     :<integer>            CPS_OK si les données sont trouvées, CPS_ERR_DEF
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
  ! arguments
  real(KIND=PM_REEL), intent(in) :: date
  real(kind=PM_REEL), dimension(2), intent(out) :: data_flux
  integer, dimension(10), intent(out) :: data_ia
  
  ! resultat
  integer :: trouve
  
  ! initialisation
  trouve = CPS_ERR_DEF

  ! appel a COMPAS_BASE
  trouve = cps_getAcsol2Sure(date, data_flux, data_ia)
  
end function cps_ihm_getAcsol2Sure


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getAcsol2SureCreneau(date_deb, date_fin, tab_date,   &
     tab_data_flux, tab_data_ia, nb_data) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getAcsol2SureCreneau
!
!$Resume
!  Interface pour l'IHM acces cps_getAcsol2SureCreneau.
!$Description
!  Interface pour l'IHM acces cps_getAcsol2SureCreneau.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_ihm_getAcsol2SureCreneau(date_deb, date_fin, tab_date,   &
!.         tab_data_flux, tab_data_ia, nb_data)
!.    real(KIND=PM_REEL) :: date_deb, date_fin
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE) :: tab_date
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE,2) :: tab_data_flux
!.    integer, dimension(CPS_MAX_ACSOL2_SURE,10) :: tab_data_ia
!.    integer :: nb_data
!.    integer :: ok
!
!$Arguments
!>E     date_deb       :<PM_REEL>                                date de début du créneau (unité jj1950)
!>E     date_fin       :<PM_REEL>                                date de fin du créneau (unité jj1950)
!>S     tab_date       :<PM_REEL,DIM=(CPS_MAX_ACSOL2_SURE)>      tableau des dates des données
!>S     tab_data_flux  :<PM_REEL,DIM=(CPS_MAX_ACSOL2_SURE,2)>    tableau des données de flux
!>S     tab_data_ia    :<integer,DIM=(CPS_MAX_ACSOL2_SURE,10)>   tableau des données liées aux indices
!>S     nb_data        :<integer>                                nombre d'éléments des tableaux
!>S     ok             :<integer>                                CPS_OK si toutes les données du créneau sont trouvées
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
  ! arguments
  real(KIND=PM_REEL), intent(in) :: date_deb, date_fin
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE), intent(out) :: tab_date
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE,2), intent(out) :: tab_data_flux
  integer, dimension(CPS_MAX_ACSOL2_SURE,10), intent(out) :: tab_data_ia
  integer, intent(out) :: nb_data
  
  ! resultat
  integer :: ok
  
 ! initialisation
  ok = CPS_ERR_DEF
  nb_data = 0

  ! appel a COMPAS_BASE
  ok = cps_getAcsol2SureCreneau(date_deb, date_fin, tab_date,          &
       tab_data_flux, tab_data_ia, nb_data)
  
end function cps_ihm_getAcsol2SureCreneau


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_getAcsol2DernieresPred(date_pred, dates, data_flux, &
     data_ia, nb_pred)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getAcsol2DernieresPred
!
!$Resume
!  Interface pour l'IHM acces cps_getAcsol2DernieresPred
!$Description
!  Interface pour l'IHM acces cps_getAcsol2DernieresPred
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getAcsol2DernieresPred(date_pred, dates, data_flux, &
!.         data_ia, nb_pred)
!.    real(KIND=PM_REEL) :: date_pred
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED) :: dates
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED,2) :: data_flux
!.    integer, dimension(CPS_MAX_ACSOL2_PRED,9) :: data_ia
!.    integer :: nb_pred
!
!$Arguments
!>S     date_pred  :<PM_REEL>                               date à laquelle ont été effectuées les dernières prédictions (jj1950)
!>S     dates      :<PM_REEL,DIM=(CPS_MAX_ACSOL2_PRED)>     tableau des dates des données (jj1950)
!>S     data_flux  :<PM_REEL,DIM=(CPS_MAX_ACSOL2_PRED,2)>   tableau des données liées aux flux
!>S     data_ia    :<integer,DIM=(CPS_MAX_ACSOL2_PRED,9)>   tableau des données liées aux indices
!>S     nb_pred    :<integer>                               nombre d'éléments des tableaux
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
  ! arguments
  real(KIND=PM_REEL), intent(out) :: date_pred
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED), intent(out) :: dates
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED,2), intent(out) :: data_flux
  integer, dimension(CPS_MAX_ACSOL2_PRED,9),  intent(out) :: data_ia
  integer, intent(out) :: nb_pred
  
  ! initialisation
  nb_pred = 0

  ! appel a COMPAS_BASE
  call cps_getAcsol2DernieresPred(date_pred, dates, data_flux, data_ia,&
       nb_pred)
  
end subroutine cps_ihm_getAcsol2DernieresPred


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getAcsol2DatePred(date, date_pred, data_flux, data_ia) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getAcsol2DatePred
!
!$Resume
!  Interface pour l'IHM acces cps_getAcsol2DatePred
!$Description
!  Interface pour l'IHM acces cps_getAcsol2DatePred
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_ihm_getAcsol2DatePred(date, date_pred, data_flux, data_ia)
!.    real(KIND=PM_REEL) :: date, date_pred
!.    real(KIND=PM_REEL), dimension(2) :: data_flux
!.    integer, dimension(9) :: data_ia
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>           date des données recherchées (jj1950)
!>E     date_pred  :<PM_REEL>           date à laquelle a été faite la prédiction
!>S     data_flux  :<PM_REEL,DIM=(2)>   données des flux
!>S     data_ia    :<integer,DIM=(9)>   données liées aux indices
!>S     trouve     :<integer>           CPS_OK si les données sont trouvées
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
  ! arguments
  real(KIND=PM_REEL), intent(in) :: date, date_pred
  real(KIND=PM_REEL), dimension(2), intent(out) :: data_flux
  integer, dimension(9), intent(out) :: data_ia
  
  ! resultat
  integer :: trouve
  
  ! initialisation
  trouve = CPS_ERR_DEF
  
  ! appel a COMPAS_BASE
  trouve = cps_getAcsol2DatePred(date, date_pred, data_flux, data_ia) 

end function cps_ihm_getAcsol2DatePred


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getAcsol2Pred(date, dates, data_flux, data_ia, nb_pred) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getAcsol2Pred
!
!$Resume
!  Interface pour l'IHM acces de cps_getAcsol2Pred
!$Description
!  Interface pour l'IHM acces de cps_getAcsol2Pred
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_ihm_getAcsol2Pred(date, dates, data_flux, data_ia, nb_pred)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED) :: dates
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED,2) :: data_flux
!.    integer, dimension(50,9) :: data_ia
!.    integer :: nb_pred
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>                               date à laquelle ont été faites les prédictions
!>S     dates      :<PM_REEL,DIM=(CPS_MAX_ACSOL2_PRED)>     tableau des dates (jj1950)
!>S     data_flux  :<PM_REEL,DIM=(CPS_MAX_ACSOL2_PRED,2)>   tableau des flux
!>S     data_ia    :<integer,DIM=(50,9)>                    tableau des indices
!>S     nb_pred    :<integer>                               nombre d'éléments des tableaux
!>S     trouve     :<integer>                               CPS_OK si les donnéesont été trouvées, CPS_ERR_DEF sinon
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
  ! arguments
  real(KIND=PM_REEL), intent(in) :: date
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED), intent(out) :: dates
  real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_PRED,2), intent(out) :: data_flux
  integer, dimension(50,9), intent(out) :: data_ia
  integer, intent(out) :: nb_pred
  
  ! resultat
  integer :: trouve
  
  ! initialisation
  trouve = CPS_ERR_DEF
  nb_pred = 0

  ! appel a COMPAS_BASE
  trouve = cps_getAcsol2Pred(date, dates, data_flux, data_ia, nb_pred)

end function cps_ihm_getAcsol2Pred
  


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getAcsol2DernierePred(date, date_pred, data_flux, data_ia) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getAcsol2DernierePred
!
!$Resume
!  Interface pour l'IHM acces de cps_getAcsol2DernierePred
!$Description
!  Interface pour l'IHM acces de cps_getAcsol2DernierePred
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_ihm_getAcsol2DernierePred(date, date_pred, data_flux, data_ia)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: date_pred
!.    real(KIND=PM_REEL), dimension(2) :: data_flux
!.    integer, dimension(9) :: data_ia
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>           date des données recherchées (jj1950)
!>S     date_pred  :<PM_REEL>           date à laquelle a été faite la dernière prédiction concernant les données recherchées
!>S     data_flux  :<PM_REEL,DIM=(2)>   tableau des flux
!>S     data_ia    :<integer,DIM=(9)>   tableau des indices
!>S     trouve     :<integer>           CPS_Ok si les données ont été trouvées, CPS_ERR_DEF sinon
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
  ! arguments
  real(KIND=PM_REEL), intent(in) :: date
  real(KIND=PM_REEL), intent(out) :: date_pred
  real(KIND=PM_REEL), dimension(2), intent(out) :: data_flux
  integer, dimension(9), intent(out) :: data_ia
  
  ! resultat
  integer :: trouve
  
  ! initialisation
  trouve = CPS_ERR_DEF
  
  ! appel a COMPAS_BASE
  trouve = cps_getAcsol2DernierePred(date, date_pred, data_flux, data_ia)

end function cps_ihm_getAcsol2DernierePred

