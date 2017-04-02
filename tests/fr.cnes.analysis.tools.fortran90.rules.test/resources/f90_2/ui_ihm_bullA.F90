!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!	Routines utiles pour 'IHM bulletin A de COMPAS
!
!$Version
!  $Id: ui_ihm_bullA.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_ihm_bullA.F90,v $
!  Revision 1.4  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.3  2008/04/11 12:42:44  vivaresf
!  Version 2.4 AQ : correction des cartouches
!
!  Revision 1.2  2008/04/11 10:09:26  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!
!  Revision 1.1  2008/02/08 17:51:24  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!
!  Revision 1.3  2006/05/31 13:08:40  vivaresf
!  COMPAS 2.0 : validation
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getBullASure(date, data0) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getBullASure
!
!$Resume
!
!$Description
!  Interface pour l'IHM acces.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_ihm_getBullASure(date, data0)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL), dimension(15) :: data0
!.    integer :: ok
!
!$Arguments
!>E     date   :<PM_REEL>            
!>S     data0  :<PM_REEL,DIM=(15)>   
!>S     ok     :<integer>            
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
  real(KIND=PM_REEL), dimension(15), intent(out) :: data0
  
  ! resultat
  integer :: ok
  
  ! initialisation
  ok = CPS_ERR_DEF
  
  ! appel a COMPAS_BASE
  ok = cps_getBullASure(date, data0)
  
end function cps_ihm_getBullASure


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getBullASureCreneau(date_deb, date_fin, tab_data, nb_data) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getBullASureCreneau
!
!$Resume
!
!$Description
!  Interface pour l'IHM acces.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_ihm_getBullASureCreneau(date_deb, date_fin, tab_data, nb_data)
!.    real(KIND=PM_REEL) :: date_deb, date_fin
!.    real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_SURE, 15) :: tab_data
!.    integer :: nb_data
!.    integer :: ok
!
!$Arguments
!>E     date_deb  :<PM_REEL>                                
!>E     date_fin  :<PM_REEL>                                
!>S     tab_data  :<PM_REEL,DIM=(CPS_MAX_BULLA_SURE, 15)>   
!>S     nb_data   :<integer>                                
!>S     ok        :<integer>                                
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
  real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_SURE, 15), intent(out) :: tab_data
  integer, intent(out) :: nb_data
  
  ! resultat
  integer :: ok
  
  ! initialisation
  ok = CPS_ERR_DEF
  nb_data = 0
  
  ! appel a COMPAS_BASE
  ok = cps_getBullASureCreneau(date_deb, date_fin, tab_data, nb_data)

end function cps_ihm_getBullASureCreneau



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_getBullADernieresPred(date_pred, tab_data, nb_pred)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getBullADernieresPred
!
!$Resume
!
!$Description
!  Interface pour l'IHM acces.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getBullADernieresPred(date_pred, tab_data, nb_pred)
!.    real(KIND=PM_REEL) :: date_pred
!.    real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_PRED,15) :: tab_data
!.    integer :: nb_pred
!
!$Arguments
!>S     date_pred  :<PM_REEL>                               
!>S     tab_data   :<PM_REEL,DIM=(CPS_MAX_BULLA_PRED,15)>   
!>S     nb_pred    :<integer>                               
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
  real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_PRED,15), intent(out) :: tab_data
  integer, intent(out) :: nb_pred
  
  ! initialisation
  nb_pred = 0
  
  ! appel a COMPAS_BASE
  call cps_getBullADernieresPred(date_pred, tab_data, nb_pred)

end subroutine cps_ihm_getBullADernieresPred


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function  cps_ihm_getBullAPred(date, data_pred, nb_pred) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getBullAPred
!
!$Resume
!
!$Description
!  Interface pour l'IHM acces.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_ihm_getBullAPred(date, data_pred, nb_pred)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_PRED,15) :: data_pred
!.    integer :: nb_pred
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>                               
!>S     data_pred  :<PM_REEL,DIM=(CPS_MAX_BULLA_PRED,15)>   
!>S     nb_pred    :<integer>                               
!>S     trouve     :<integer>                               
!
!$Common
!
!$Routines
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
  ! arguments
  real(KIND=PM_REEL), intent(in) :: date
  real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_PRED,15), intent(out) :: data_pred
  integer, intent(out) :: nb_pred
  
  ! resultat
  integer :: trouve
  
  ! initialisation
  trouve = CPS_ERR_DEF
  
  ! appel a COMPAS_BASE
  trouve = cps_getBullAPred(date, data_pred, nb_pred)
  
end function cps_ihm_getBullAPred



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getBullADatePred(date, date_pred, data_pred) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getBullADatePred
!
!$Resume
!
!$Description
!  Interface pour l'IHM acces.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_ihm_getBullADatePred(date, date_pred, data_pred)
!.    real(KIND=PM_REEL) :: date, date_pred
!.    real(KIND=PM_REEL), dimension(15) :: data_pred
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>            
!>E     date_pred  :<PM_REEL>            
!>S     data_pred  :<PM_REEL,DIM=(15)>   
!>S     trouve     :<integer>            
!
!$Common
!
!$Routines
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
  ! arguments
  real(KIND=PM_REEL), intent(in) :: date, date_pred
  real(KIND=PM_REEL), dimension(15), intent(out) :: data_pred
  
  ! resultat
  integer :: trouve

  ! initialisation
  trouve = CPS_ERR_DEF

  ! appel a COMPAS_BASE
  trouve = cps_getBullADatePred(date, date_pred, data_pred)
  
end function cps_ihm_getBullADatePred



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Interface pour l'IHM acces                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function cps_ihm_getBullADernierePred(date, date_pred, data_pred) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getBullADernierePred
!
!$Resume
!
!$Description
!  Interface pour l'IHM acces.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_ihm_getBullADernierePred(date, date_pred, data_pred)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: date_pred
!.    real(KIND=PM_REEL), dimension(15) :: data_pred
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>            
!>S     date_pred  :<PM_REEL>            
!>S     data_pred  :<PM_REEL,DIM=(15)>   
!>S     trouve     :<integer>            
!
!$Common
!
!$Routines
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
  ! arguments
  real(KIND=PM_REEL), intent(in) :: date
  real(KIND=PM_REEL), intent(out) :: date_pred
  real(KIND=PM_REEL), dimension(15), intent(out) :: data_pred
    
  ! resultat
  integer :: trouve
  
  ! initialisation
  trouve = CPS_ERR_DEF
  
  ! appel a COMPAS_BASE
  trouve = cps_getBullADernierePred(date, date_pred, data_pred)

end function cps_ihm_getBullADernierePred

