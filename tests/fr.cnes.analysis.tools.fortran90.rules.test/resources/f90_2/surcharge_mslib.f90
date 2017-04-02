
module surcharge_mslib

! (C) Copyright CNES - MSLIB - 1997-2004

!************************************************************************
!
! But:  Definition des surcharges explicites des routines MSLIB. 
! ===
!
! Note d'utilisation:  Ce module est accessible a l'utilisateur via le module mslib.
! ==================
!
!$Historique
! ==========
!   + Version 6.5 : DM-ID 478 : integrateur de cowell, modification de l'interface
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/03/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

  use longueur_chaine_mslib     ! definition des longueurs de chaines de caracteres
  use type_mslib     ! definition des types

implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: surcharge_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


real(pm_reel), parameter :: seuil_comp = 1.e-9_pm_reel

!................................................................................................................

  character(len=pm_longueur_info_utilisateur), private, parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB surcharge_mslib.f90: derniere modification V6.13 >'

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: surcharge_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

!................................................................................................................
! definition de l'interface de la surcharge de l'addition pour des dates

interface assignment (=)
   module procedure mdi_joursec_egal_joursec 
end interface

interface operator (+)
   module procedure mdi_joursec_plus_sec
   module procedure mdi_sec_plus_joursec
   module procedure mdi_joursec_plus_joursec
end interface

interface operator (-)
   module procedure mdi_joursec_moins_sec
   module procedure mdi_joursec_moins_joursec
end interface

interface operator (<)
   module procedure mdi_joursec_inferieur
end interface

interface operator (>)
   module procedure mdi_joursec_superieur
end interface

interface operator (==)
   module procedure mdi_joursec_egal
end interface

interface operator (<=)
   module procedure mdi_joursec_inferieur_ou_egal
end interface

interface operator (>=)
   module procedure mdi_joursec_superieur_ou_egal
end interface

interface operator (/=)
   module procedure mdi_joursec_different
end interface


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CONTAINS


subroutine mdi_joursec_egal_joursec (jour_sec_out, jour_sec_in)

!************************************************************************
!
! But:  Transfert de dates de type tm_jour_sec en tm_jour_sec 
! ===   Cette procedure correspond a une affectation ("=")
!
!
! Arguments:
!>E     jour_sec_in   :<tm_jour_sec>   date à droite du signe =
!>S     jour_sec_out  :<tm_jour_sec>   date à gauche du signe /=
!************************************************************************

! Declarations
! ============

type(tm_jour_sec), intent(out) :: jour_sec_out   ! resultat
type(tm_jour_sec), intent(in)  :: jour_sec_in    ! operande

! Corps du sous programme
! =======================

jour_sec_out%jour  = jour_sec_in%jour
jour_sec_out%sec   = jour_sec_in%sec

end subroutine mdi_joursec_egal_joursec

function mdi_joursec_plus_sec(jour_sec,sec)

!************************************************************************
!
! But:  Addition d'une date de type tm_jour_sec a des secondes reelles
!       La date resultat de cette somme est normalisee:
!       les secondes sont dans l'intervalle [0, 86400[
!       Cette procedure correspond a une operation ("+")
!
! Arguments:
!>E      jour_sec  :<tm_jour_sec> a gauche du signe +
!>E      sec       :<pm_reel>  a droite du signe +
!>S      mdi_joursec_plus_sec :<tm_jour_sec>  résultat
!************************************************************************

! Declarations
! ============

type(tm_jour_sec) :: mdi_joursec_plus_sec   ! resultat
type(tm_jour_sec), intent(in) :: jour_sec      ! operande 1
real(pm_reel), intent(in)  :: sec              ! operande 2

! Autres declarations
! -------------------

type(tm_jour_sec) :: date_inter               ! date intermediaire
type(tm_jour_sec) :: date_inter_norm          ! date intermediaire normalisee

type(tm_code_retour) :: code_retour_local     ! code retour local

! Corps du sous programme
! =======================

! Somme entre les secondes de la date initiale et les secondes a rajouter
! .......................................................................

date_inter%jour = jour_sec%jour
date_inter%sec  = jour_sec%sec + sec

! Normalisation de la date somme des dates dans l'eventualite
! ou la somme des secondes ne serait pas dans l'intervalle 
! [0., 86400.[
! ...........................................................

call md_joursec_norme(date_inter, date_inter_norm, code_retour_local)
! pas de code de retour a tester ici

! Affectation pour retour par la fonction
! .......................................

mdi_joursec_plus_sec = date_inter_norm ! surcharge "=" pour tm_jour_sec

end function mdi_joursec_plus_sec

function mdi_sec_plus_joursec (sec,jour_sec)

!************************************************************************
!
! But:  Addition de sec secondes reelles a une date jour_sec definie par 
! ===   la structure tm_jour_sec, operation commutee de la precedente. 
!       La date resultat de cette somme est normalisee:
!       les secondes sont dans l'intervalle [0, 86400[
!       Cette procedure correspond a une operation ("+")
!
! Arguments:
!>E      sec       :<pm_reel>  a gauche du signe +
!>E      jour_sec  :<tm_jour_sec> a droite du signe +
!>S      mdi_sec_plus_joursec :<tm_jour_sec>  résultat
!************************************************************************

! Declarations
! ============

type(tm_jour_sec) :: mdi_sec_plus_joursec   ! resultat
real(pm_reel), intent(in)  :: sec             ! operande 1
type(tm_jour_sec), intent(in) :: jour_sec     ! operande 2

! Autres declarations
! -------------------

type(tm_jour_sec) :: date_inter               ! date intermediaire
type(tm_jour_sec) :: date_inter_norm          ! date intermediaire normalisee

type(tm_code_retour) :: code_retour_local     ! code retour local

! Corps du sous programme
! =======================

! Somme entre les secondes de la date initiale et les secondes a rajouter
! .......................................................................

date_inter%jour = jour_sec%jour
date_inter%sec  = jour_sec%sec + sec

! Normalisation de la date somme des dates dans l'eventualite
! ou la somme des secondes ne serait pas dans l'intervalle 
! [0., 86400.[
! ...........................................................

call md_joursec_norme(date_inter, date_inter_norm, code_retour_local)
! pas de code de retour a tester ici

! Affectation pour retour par la fonction
! .......................................

mdi_sec_plus_joursec = date_inter_norm ! surcharge "=" pour tm_jour_sec

end function mdi_sec_plus_joursec

function mdi_joursec_plus_joursec(jour_sec1,jour_sec2)

!************************************************************************
!
! But:  Addition de 2 dates de type tm_jour_sec . 
!       La date resultat de cette somme est normalisee:
!       les secondes sont dans l'intervalle [0, 86400[
!       Cette procedure correspond a une operation ("+")
!
! Arguments:
!>E      jour_sec1  :<tm_jour_sec> a gauche du signe +
!>E      jour_sec2  :<tm_jour_sec> a droite du signe +
!>S      mdi_joursec_plus_joursec :<tm_jour_sec>  résultat
!
!************************************************************************

! Declarations
! ============

type(tm_jour_sec) :: mdi_joursec_plus_joursec   ! resultat
type(tm_jour_sec), intent(in) :: jour_sec1      ! operande 1
type(tm_jour_sec), intent(in) :: jour_sec2      ! operande 2

! Autres declarations
! -------------------

type(tm_jour_sec) :: date_inter               ! date intermediaire
type(tm_jour_sec) :: date_inter_norm          ! date intermediaire normalisee

type(tm_code_retour) :: code_retour_local     ! code retour local

! Corps du sous programme
! =======================

! Somme entre les secondes de la date initiale et les secondes a rajouter
! .......................................................................

date_inter%jour = jour_sec1%jour + jour_sec2%jour
date_inter%sec  = jour_sec1%sec + jour_sec2%sec

! Normalisation de la date somme des dates dans l'eventualite
! ou la somme des secondes ne serait pas dans l'intervalle 
! [0., 86400.[
! ...........................................................

call md_joursec_norme(date_inter, date_inter_norm, code_retour_local)
! pas de code de retour a tester ici

! Affectation pour retour par la fonction
! .......................................

mdi_joursec_plus_joursec = date_inter_norm ! surcharge "=" pour tm_jour_sec

end function mdi_joursec_plus_joursec

function mdi_joursec_moins_sec (jour_sec,sec)

!************************************************************************
!
! But:  Soustraction de secondes (pm_reel) a une date definie par la structure tm_jour_sec
! ===   La date resultat de cette soustraction est normalisee:
!       les secondes sont dans l'intervalle [0, 86400[
!       Cette procedure correspond a une operation ("-") 
!
! Arguments:
!>E      jour_sec  :<tm_jour_sec> a gauche du signe +
!>E      sec  :<pm_reel> a droite du signe +
!>S      mdi_joursec_moins_sec :<tm_jour_sec>  résultat
!************************************************************************

! Declarations
! ============
type(tm_jour_sec) :: mdi_joursec_moins_sec      ! resultat
type(tm_jour_sec), intent(in) :: jour_sec         ! operande 1
real(pm_reel), intent(in)  :: sec                 ! operande 2

! Autres declarations
! -------------------

type(tm_jour_sec) :: date_inter                   ! date intermediaire
type(tm_jour_sec) :: date_inter_norm              ! date intermediaire normalisee 

! code de retour de md_joursec_norme
type(tm_code_retour) :: code_retour_local

! Corps du sous programme
! =======================

! Soustraction entre les secondes de la date initiale et les secondes a enlever
! .............................................................................

date_inter%jour = jour_sec%jour
date_inter%sec  = jour_sec%sec - sec

! Normalisation de la date difference des dates
! .............................................

call md_joursec_norme(date_inter, date_inter_norm, code_retour_local)
! pas de code de retour a tester ici

! Affectation pour retour par la fonction
! .......................................

mdi_joursec_moins_sec = date_inter_norm ! surcharge "=" pour tm_jour_sec

end function mdi_joursec_moins_sec

function mdi_joursec_moins_joursec (jour_sec1, jour_sec2)

!************************************************************************
!
! But:  Soustraction de deux dates definies par la structure tm_jour_sec 
! ===   Le resultat est donne sous la forme de secondes (pm_reel).
!       Le resultat de cette soustraction peut etre positif, nul ou negatif.
!       Avant toute soustraction de dates, les 2 dates sont normalisees
!       Cette procedure correspond a une operation ("-") 
!
! Arguments:
!>E      jour_sec1  :<tm_jour_sec> a gauche du signe +
!>E      jour_sec2  :<tm_jour_sec> a droite du signe +
!>S      mdi_joursec_moins_joursec :<pm_reel>  résultat
!
!************************************************************************

! Declarations
! ============

real(pm_reel)                 :: mdi_joursec_moins_joursec    ! resultat
type(tm_jour_sec), intent(in) :: jour_sec1                     ! operande 1
type(tm_jour_sec), intent(in) :: jour_sec2                   ! operande 2

! Autres declarations
! -------------------

type(tm_jour_sec)  :: jour_sec1_norm         ! date 1 normalisee
type(tm_jour_sec)  :: jour_sec2_norm       ! date 2 normalisee

type(tm_jour_sec) :: date_inter                ! date intermediaire
real(pm_reel) :: secondes                      ! nb de secondes

type(tm_code_retour) :: code_retour_local      ! code retour local

! Corps du sous programme
! =======================

! Normalisation des deux dates
! ............................

call md_joursec_norme(jour_sec1, jour_sec1_norm, code_retour_local)
! pas de code de retour a tester ici

call md_joursec_norme(jour_sec2, jour_sec2_norm, code_retour_local)
! pas de code de retour a tester ici

! Soustraction entre les deux dates, champ a champ
! ................................................

date_inter%jour = jour_sec1_norm%jour - jour_sec2_norm%jour
date_inter%sec  = jour_sec1_norm%sec  - jour_sec2_norm%sec

! Ecart en secondes entre les deux dates
! .......................................

secondes = real(date_inter%jour,kind=pm_reel)*86400.0_pm_reel + date_inter%sec

! Affectation pour retour par la fonction
! .......................................

mdi_joursec_moins_joursec = secondes    

end function mdi_joursec_moins_joursec


logical function mdi_joursec_inferieur(date1, date2) result(test)

!************************************************************************
!
! But:  
! ===  Test de comparaison (<) de deux dates de type tm_jour_sec
!
!
!$Usage
!  test = OSC_date_inferieure(date1, date2)
!.    type (tm_jour_sec) :: date1, date2
!
!$Arguments
!>E     date1  :<tm_jour_sec>   date à gauche du symbole <
!>E     date2  :<tm_jour_sec>   date à droite du symbole <
!>S     test   :<logical>       resultat logique
!
!************************************************************************
    
! Declarations
! ============    
type(tm_jour_sec), intent(in) :: date1, date2
    
! =======================

test = ( (date1%jour < date2%jour).or. &
     (date1%jour == date2%jour).and.&
     ( (date1%sec < date2%sec).and.&
     (abs(date1%sec-date2%sec) > seuil_comp ) ) )

end function mdi_joursec_inferieur


logical function mdi_joursec_superieur(date1, date2) result(test)

!************************************************************************
!
! But:  
! === Test de comparaison (>) de deux dates de type tm_jour_sec
!
!$Usage
!  test = OSC_date_superieure(date1, date2)
!.    type (tm_jour_sec) :: date1, date2
!
!$Arguments
!>E     date1  :<tm_jour_sec>   
!>E     date2  :<tm_jour_sec>   
!>S     test   :<logical>       résultat logique
!
!************************************************************************

    
! Declarations
! ============    
type(tm_jour_sec), intent(in) :: date1, date2
    
! =======================

test = ( (date1%jour > date2%jour).or. &
     (date1%jour == date2%jour).and.&
     ( (date1%sec > date2%sec).and.(abs(date1%sec-date2%sec) > seuil_comp ) ) )

end function mdi_joursec_superieur

logical function mdi_joursec_egal(date1, date2) result(test)

!************************************************************************
!
! But: 
! ===  Test d'égalité de deux dates de type tm_jour_sec
!
!
!$Usage
!  test = OSC_date_egale(date1, date2)
!.    type (tm_jour_sec) :: date1, date2
!
!$Arguments
!>E     date1  :<tm_jour_sec>   date à gauche du symbole ==
!>E     date2  :<tm_jour_sec>   date à droite du symbole ==
!>S     test   :<logical>       résultat logique
!
!************************************************************************ 
    
! Declarations
! ============    
type (tm_jour_sec), intent(in) :: date1, date2
! ============    

test = ( (date1%jour == date2%jour).and. &
     (abs(date1%sec-date2%sec) < seuil_comp ) )

end function mdi_joursec_egal


logical function mdi_joursec_inferieur_ou_egal(date1, date2) result(test)

!************************************************************************
!
! But: 
! ===  Test de comparaison (<=) de deux dates de type tm_jour_sec
!
!$Usage
!  test = mdi_joursec_inferieur_ou_egal (date1, date2)
!.    type (tm_jour_sec) :: date1, date2
!
!$Arguments
!>E     date1  :<tm_jour_sec>   date à gauche du signe <=
!>E     date2  :<tm_jour_sec>   date à droite du signe <=
!>S     test   :<logical>       résultat logique
!
!************************************************************************

! Declarations
! ============    
type (tm_jour_sec), intent(in) :: date1, date2
! ============    

!test = ( (mdi_joursec_inferieur(date1,date2)) .or.( mdi_joursec_egal(date1,date2) ))
test = ( (date1 < date2) .or. ( date1==date2 ))

end function mdi_joursec_inferieur_ou_egal


logical function mdi_joursec_superieur_ou_egal(date1, date2) result(test)

!************************************************************************
!
! But: 
! ===  Test de comparaison (>=) de deux dates de type tm_jour_sec
!
!$Usage
!  test = mdi_joursec_superieur_ou_egal(date1, date2)
!.    type (tm_jour_sec) :: date1, date2
!
!$Arguments
!>E     date1  :<tm_jour_sec>   =
!>E     date2  :<tm_jour_sec>   =
!>S     test   :<logical>       résultat logique
!************************************************************************

    
! Declarations
! ============    
type(tm_jour_sec), intent(in) :: date1, date2
! ============    

!test = ( (mdi_joursec_superieur(date1,date2)).or.(mdi_joursec_egal(date1,date2)) )
test = ( (date1 > date2) .or. (date1==date2) )

end function mdi_joursec_superieur_ou_egal


logical function mdi_joursec_different(date1, date2) result(test)

!************************************************************************
!
! But:
! ===  Test de comparaison (/=) de deux dates de type tm_jour_sec
!
!$Usage
!  test = mdi_joursec_different(date1, date2)
!.    type (tm_jour_sec) :: date1, date2
!
!$Arguments
!>E     date1  :<tm_jour_sec>   date à gauche du signe /=
!>E     date2  :<tm_jour_sec>   date à droite du signe /=
!>S     test   :<logical>       résultat logique
!************************************************************************
  
! Declarations
! ============        
type (tm_jour_sec), intent(in) :: date1, date2
! ============        

test = ( (date1%jour /= date2%jour).or.(abs(date1%sec-date2%sec) > seuil_comp ) )

end function mdi_joursec_different


end module surcharge_mslib
