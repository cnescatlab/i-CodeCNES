subroutine mr_mat_nuta (nuta, obli_moy, mat_nuta, code_retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Calcul de la MATrice de NUTAtion pour le passage du repere equatorial moyen au 
! ===   repere equatorial vrai pour la meme epoque.
!
!$Historique
! ==========
!   + Version 1.0 (SP 286 ed01 rev00): creation a partir de la routine MRMNUT de la MSLIB f77
!                         (Date: 10/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00) : modifications qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058  :Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_nuta), intent(in)                            :: nuta    !    nutation en longitude et en obliquite
real(pm_reel), intent(in)                            :: obli_moy!    obliquite moyenne
real(pm_reel), dimension(3,3), intent(out)           :: mat_nuta!     matrice de nutation
type(tm_code_retour), intent(out)                    :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)  ::   rcnuln,rsnuln!     cosinus et sinus de la nutation en longitude
real(pm_reel)  ::   rcobli,rsobli!     cosinus et sinus de l'obliquite moyenne
real(pm_reel)  ::   rcoblv,rsoblv!     cosinus et sinus de l'obliquite vrai

!     variables de travail
real(pm_reel)  ::   rscovn,rccovn
real(pm_reel)  ::   robliv

intrinsic cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_mat_nuta.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_mat_nuta.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

!====================
! matrice de nutation
!====================

rcnuln = cos( nuta%long )
rsnuln = sin( nuta%long )

rcobli = cos( obli_moy )
rsobli = sin( obli_moy )

robliv = nuta%obli + obli_moy

rcoblv = cos( robliv )
rsoblv = sin( robliv )

mat_nuta(1,1) = rcnuln
mat_nuta(1,2) = -rsnuln*rcobli
mat_nuta(1,3) = -rsnuln*rsobli

rccovn = rcoblv*rcnuln

mat_nuta(2,1) = rcoblv*rsnuln
mat_nuta(2,2) = rccovn*rcobli + rsoblv*rsobli
mat_nuta(2,3) = rccovn*rsobli - rsoblv*rcobli

rscovn = rsoblv*rcnuln

mat_nuta(3,1) = rsoblv*rsnuln
mat_nuta(3,2) = rscovn*rcobli - rcoblv*rsobli
mat_nuta(3,3) = rscovn*rsobli + rcoblv*rcobli

code_retour%routine = pm_num_mr_mat_nuta
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_mat_nuta
