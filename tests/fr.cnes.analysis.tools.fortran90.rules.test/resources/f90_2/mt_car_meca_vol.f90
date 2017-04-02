subroutine mt_car_meca_vol (r_equa, apla, pos_car, vit_car, pos_geod, vit_meca_vol, code_retour, jacob)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Passage des position-vitesse en coordonnees cartesiennes dans 
! ===  le repere  de reference en position-vitesse en coordonnees 
!      mecanique du vol.   
!   
! Note d'utilisation:  
! ===================   
!   Definition du repere pseudo topocentrique Nord: 
!   il correspond a un repere topocentrique Nord dont l'origine est la position courante 
!   du satellite.                 
!   Pour plus d'explications: se reporter a la documentation utilisateur
!                                     et a la note algorithmique.
!   
!$Historique
! ===========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 04/2002 - Realisation: Mickael Hazak et Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!                   FA-ID 624 : initialisation de la variable retour_jac_non_calc
!   + Version 5.9 : DM-ID 859 : utilisation de mu_matmul6
!                   (Date: 03/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_mspro

use int_rep_internes_mspro, only : mti_car_pseudo_topoN

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       
real(pm_reel), intent(in)                      :: r_equa        ! rayon equatorial
real(pm_reel),intent (in)                      :: apla          ! aplatissement 
real(pm_reel),dimension (3), intent(in)        :: pos_car       ! position cartesienne                                              
real(pm_reel),dimension (3), intent(in)        :: vit_car       ! vitesse cartesienne
type(tm_geodesique), intent(out)               :: pos_geod      ! position geodesique             
type(tm_vit_meca_vol), intent(out)             :: vit_meca_vol  ! vitesse mecanique du vol          
type(tm_code_retour), intent(out)              :: code_retour
real(pm_reel), dimension (6,6), intent(out), optional  :: jacob ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel) ::   eps100                          ! epsiolon de precision pour les reels

! concernant calcul ref ==> pseudo-topoN
real(pm_reel), dimension(3)   :: vit_pseudo_topoN  ! vitesse dans le repere pseudo "topocentrique" Nord
real(pm_reel)                 :: epsreq            ! epsilon test par rapport a r_equa
real(pm_reel), dimension(6,6) :: jacobcartopoN     ! jacobienne cartesien -> topo Nord      
integer                       :: retour            ! code retour

real(pm_reel)                 :: azimut    ! angle entre l'axe nord et la composante horizontale de la vitesse
real(pm_reel)                 :: normeV    ! norme du vecteur vitesse 
real(pm_reel)                 :: epsnormeV ! epsilon de test des composantes de la vitesse
type(tm_code_retour)          :: code_local    

real(pm_reel), dimension(3)   :: vit_norme         ! vitesse normee
real(pm_reel), dimension(6,6) :: jacobtopoNmecavol ! jacobienne topo Nord -> repere meca vol
real(pm_reel)                 :: vxy, vxy2, v2     ! intermediaires de calcul

! pour traitement du cas du jacobien non calculable 
! alors que les autres sorties le sont
logical :: jac_non_calc                            ! flag a oui si jacobienne non calculable
integer :: retour_jac_non_calc                     ! relais du code retour specifique a la jacobienne non calculable

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mt_car_meca_vol.f90: derniere modification V5.15 >'

!************************************************************************
!
! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK
!
! initialisation pour test
! ........................
eps100 = 100._pm_reel*epsilon(1._pm_reel)

jac_non_calc = .FALSE. ! pour traitement du cas aux poles avec calcul du jacobien
retour_jac_non_calc = pm_OK ! initialisation globale (en pratique inutilisée puisque
                            ! la valeur n'est utilisée que si jac_non_calc : 
                            ! l'initrialisation est la pour les outils de controle)

! Calcul de la position geodesique et de la vitesse dans le repere pseudo topocentrique Nord
! ==========================================================================================
if (present(jacob)) then

   ! calculs avec le jacobien de la transfo (car ==> topoN) 
   call mti_car_pseudo_topoN (r_equa, apla, pos_car, vit_car, pos_geod, vit_pseudo_topoN, retour, epsreq=epsreq, &
                              jacobcartopoN = jacobcartopoN)

else

   ! calculs sans le jacobien de la transfo (car ==> topoN) 
   call mti_car_pseudo_topoN (r_equa, apla, pos_car, vit_car, pos_geod, vit_pseudo_topoN, retour, epsreq=epsreq)

end if

if (retour /= pm_OK) then 
   code_retour%valeur = retour 
   if  (code_retour%valeur < pm_OK) then
      if (code_retour%valeur == pm_err_jac_non_calc_poles .OR. code_retour%valeur == pm_err_jac_non_calc_alt_neg )  then 
         jac_non_calc = .TRUE. ! autres sorties sont calculables meme si calcul impossible du jacobien
         retour_jac_non_calc = code_retour%valeur
      else        
         go to 6000 ! autres problemes: sortie necessaire
      end if
   end if
end if

! Calcul de la vitesse meca vol
! =============================

call mu_norme (vit_pseudo_topoN , normeV, code_local, vit_norme) ! test sur le code retour inutile car non physique

if (normeV < eps100)  then ! vitesse nulle: impossible de calculer les angles
   vit_meca_vol%norme = 0._pm_reel   
   vit_meca_vol%pente = 0._pm_reel   
   vit_meca_vol%azimut = 0._pm_reel   
   code_retour%valeur = pm_err_vit_nul
   go to 6000
end if

vit_meca_vol%norme = normeV
epsnormeV = eps100*normeV

vit_meca_vol%pente = asin(vit_norme(3)) ! resultat dans [-pi/2,pi/2], avec les bonnes conventions 
                                        ! positif si ascencion, negatif si rentree, nul si vitesse dans le plan horizontal.

if (sqrt(pos_car(1)**2+pos_car(2)**2) < epsreq)  then ! cas aux poles

   azimut=0._pm_reel ! valeur arbitraire 
   code_retour%valeur = pm_warn_pos_Oz_ref_meca_vol ! ecrase pm_warn_pos_Oz_ref (pour la longitude uniquement)

else ! code_retour deja teste au moment de l'appel de mt_car_geod.

   if ((abs(vit_pseudo_topoN(1)) < epsnormeV) .AND. (abs( vit_pseudo_topoN(2)) < epsnormeV)) then ! vitesse verticale pure

      azimut = 0._pm_reel ! valeur arbitraire
      code_retour%valeur = pm_warn_vit_vertic

   else

      call mu_angle2 ( vit_pseudo_topoN(1) , -vit_pseudo_topoN(2) , azimut , code_local ) 
      ! aucun test necessaire sur le code retour car vitesse NON verticale pure

   end if
end if

vit_meca_vol%azimut = azimut

! Calcul de la jacobienne
! =======================

! a ce stade des calculs: V est non nulle

if (present(jacob)) then

   if (jac_non_calc) then ! intialisation issue de l'appel a mti_car_pseudo_topoN
      code_retour%valeur = retour_jac_non_calc ! a pu etre ecrase dans les calculs intermediaires par un warning
      go to 6000
   end if

   ! si gamma = +/- pi/2, alors la vitesse est verticale pure
   if ( (abs(vit_pseudo_topoN(1))< epsnormeV) .AND. (abs(vit_pseudo_topoN(2)) < epsnormeV) ) then ! vitesse verticale pure

      code_retour%valeur= pm_err_jac_non_calc_vit_vertic
      go to 6000

   end if

   ! calcul du jacobien de la transformation topoN ==> meca vol
   ! ..........................................................

   vxy2  = vit_pseudo_topoN(1)**2+vit_pseudo_topoN(2)**2
   vxy   = sqrt(vit_pseudo_topoN(1)**2+vit_pseudo_topoN(2)**2)
   v2    = normeV * normeV

   jacobtopoNmecavol(:,:)= 0._pm_reel ! matrice contenant beaucoup de 0

   jacobtopoNmecavol(1,1)= 1._pm_reel
   jacobtopoNmecavol(2,2)= 1._pm_reel
   jacobtopoNmecavol(3,3)= 1._pm_reel

   jacobtopoNmecavol(4,4)= - (vit_pseudo_topoN(3)*vit_pseudo_topoN(1))/(vxy*v2) 
   jacobtopoNmecavol(4,5)= - (vit_pseudo_topoN(3)*vit_pseudo_topoN(2))/(vxy*v2) 
   jacobtopoNmecavol(4,6)= vxy/v2

   jacobtopoNmecavol(5,4)=  vit_pseudo_topoN(2)/vxy2
   jacobtopoNmecavol(5,5)= -vit_pseudo_topoN(1)/vxy2
   jacobtopoNmecavol(5,6)= 0._pm_reel

   jacobtopoNmecavol(6,4:6)= vit_pseudo_topoN(1:3)/normeV

   ! calcul de la jacobienne demandee
   ! ................................

   ! jacob transfo (car ==> meca vol) = jacob transfo (topoN ==> meca vol) * transfo (car ==> topoN)

   call mu_matmul6(jacobtopoNmecavol,jacobcartopoN,jacob,code_local)
   ! Pas d'erreur possible donc le code retour n est pas teste

end if

6000 continue

code_retour%routine = pm_num_mt_car_meca_vol
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' ' 

end subroutine mt_car_meca_vol
