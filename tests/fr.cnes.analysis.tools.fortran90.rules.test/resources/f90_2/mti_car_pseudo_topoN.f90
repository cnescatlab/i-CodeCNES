subroutine mti_car_pseudo_topoN (r_equa, apla, pos_car, vit_car, pos_geod, vit_pseudo_topoN, retour, &
                                 epsreq, jacobcartopoN)

! (C) Copyright CNES - MSPRO - 2001-2003
!************************************************************************
!
! But: Passage des coordonnees cartesiennes du repere de reference 
! ===  aux coordonnees dans le pseudo repere topocentrique Nord  
!
! Note d'utilisation:
! ==================
!  Pour plus d'explications: se reporter a la documentation utilisateur
!                                     et a la note algorithmique.
!   
!$Historique
! ===========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 01/2002 - Realisation: Mickael Hazak et Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE 1) : Utilisation du calcul de jacobienne de mt_car_geod et mt_car_geoc
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 5.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 5.4 : modification
!                     FA-ID 439 : remarques qualite
!                     (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!                   FA-ID 624 : initialisation de la variable retour_jac_non_calc
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib
use int_util_internes_mspro, only : mui_dot_product3

use parametre_interne_mspro
use type_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
real(pm_reel), intent(in)                             :: r_equa        ! rayon equatorial
real(pm_reel),intent (in)                             :: apla          ! aplatissement
real(pm_reel),dimension (3), intent(in)               :: pos_car       ! position cartesienne
real(pm_reel),dimension (3), intent(in)               :: vit_car       ! vitesse cartesienne

type(tm_geodesique), intent(out)                      :: pos_geod          ! position geodesique             
real(pm_reel),dimension (3), intent(out)              :: vit_pseudo_topoN  ! vitesse dans le repere topo Nord
integer, intent(out)                                  :: retour

real(pm_reel),                 intent(out), optional  :: epsreq        ! epsilon test par rapport a r_equa
real(pm_reel), dimension(6,6), intent(out), optional  :: jacobcartopoN ! jacobien de la transformation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel), dimension(3) ::  vecti,vectj,vectk ! composantes des vecteurs directeurs 
                                                  ! du repere pseudo "topocentrique" Nord 
real(pm_reel), dimension(3) ::  vtopoN            ! vitesse dans le repere pseudo topo Nord
real(pm_reel)               ::  eps100,eps_dist   ! epsilons de comparaison
type(tm_code_retour)        ::  code_local        ! code retour local
integer                     ::  mu_retour

! pour le calcul du jacobien
real(pm_reel)               :: latitude, longitude, hauteur ! parametres geodesiques
type(tm_geodesique)         :: pos_geod_non_nul             ! position geodesique si aplatissement non nul
type(tm_geocentrique)       :: pos_geoc                     ! position si aplatissement nul, equivalant a une
                                                               ! position geocentrique
real(pm_reel), dimension(6,6) ::  jacob_geoc                ! jacobienne intermediaire si aplatissement nul
real(pm_reel), dimension(3,3) ::  jacob_pos                 ! jacobienne positions
real(pm_reel)               :: jzzpmkzyp, kzxpmizzp, izypmjzxp ! intermediaires de calcul

! pour traitement du cas du jacobien non calculable 
! alors que les autres sorties le sont
logical :: jac_non_calc                            ! flag a oui si jacobienne non calculable
integer :: retour_jac_non_calc                     ! relais du code retour specifique a la jacobienne non calculable
intrinsic max, epsilon, abs, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mti_car_pseudo_topoN.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations 
! ................
retour = pm_OK

jac_non_calc = pm_i_non
retour_jac_non_calc = pm_OK ! initialisation globale (en pratique inutilisée puisque
                            ! la valeur n'est utilisée que si jac_non_calc : 
                            ! l'initrialisation est la pour les outils de controle)

eps100 = 100._pm_reel * epsilon(1._pm_reel) ! epsilon de comparaison pour les reels (proche de 1)

eps_dist= r_equa* max(1.e-9_pm_reel, 10._pm_reel*eps100) ! valeur identique a celle de mt_car_geod
if (present(epsreq)) epsreq = eps_dist

! controle des donnees d'entree
! .............................

if (apla >= 1._pm_reel) then    ! aplatissement superieur ou egal a 1
   retour = pm_err_apla_sup1
   go to 6000
end if

! Calcul des coordonnees geodesiques (traitement selon l'aplatissement)
! =====================================================================

if  ((apla >= 0._pm_reel).and.(apla < eps100  )) then ! cas ou l'aplatissement est nul: calcul direct des coordonnees geodesiques
   retour = pm_warn_apla_nul  

   if(sqrt(pos_car(1)**2+pos_car(2)**2)< eps_dist) then ! traitement des cas aux poles Nord et Sud 

      latitude  = sign(pm_pi_sur2,pos_car(3))                                                     
      longitude = 0._pm_reel  ! comme la longitude n'est pas definie aux poles on lui donne la valeur arbitraire 0
      hauteur = abs(pos_car(3)) - r_equa ! 0<apla<eps100 donc le terme apla est negligeable
      retour = pm_warn_pos_Oz_ref

   else ! cas hors poles

      if (present(jacobcartopoN)) then
         call mt_car_geoc (pos_car, pos_geoc, code_local, vit_car = vit_car, jacob = jacob_geoc) 
         ! le parametre vit_car n'est pas utilise. Mais il es necessaire pour l'appel avec jacob en option
         jacob_pos(:,:) = jacob_geoc(1:3,1:3)
      else
         call mt_car_geoc (pos_car, pos_geoc, code_local) 
      end if
      if (code_local%valeur /= pm_ok) then
         retour = code_local%valeur
         if (retour< pm_ok) go to 6000
      end if
      latitude = pos_geoc%lat
      longitude = pos_geoc%long
      hauteur = pos_geoc%dist - r_equa ! 0<apla<eps100 donc le terme apla est negligeable

   end if

else ! aplatissement non nul

   if (present(jacobcartopoN)) then
      call mt_car_geod (pos_car, r_equa, apla, pos_geod_non_nul, code_local, jacob = jacob_pos) 
   else
      call mt_car_geod (pos_car, r_equa, apla, pos_geod_non_nul, code_local) 
   end if
   if (code_local%valeur /= pm_ok) then
      retour = code_local%valeur
      if  (code_local%valeur < pm_OK) then
         if (code_local%valeur == pm_err_jac_non_calc_poles .OR. code_local%valeur == pm_err_jac_non_calc_alt_neg )  then 
            jac_non_calc = pm_i_oui ! autres sorties sont calculables meme si calcul impossible du jacobien
            retour_jac_non_calc = code_local%valeur
         else  
            go to 6000 ! autres problemes: sortie necessaire
         end if
      end if
   end if

   latitude  = pos_geod_non_nul%lat
   longitude = pos_geod_non_nul%long
   hauteur   = pos_geod_non_nul%haut

end if

! affectation des valeurs en sortie (non utilisables dans les calculs intermediaires)
pos_geod%lat  = latitude
pos_geod%long = longitude
pos_geod%haut = hauteur

! Calcul de la vitesse dans le  repere pseudo "topocentrique" Nord
! =================================================================

! calcul des composantes des vecteurs (u,v,w) constituant
! le repere pseudo "topocentrique" vecteurs exprimes dans le repere de reference.
! ..............................................................................
call mt_def_topo_N (latitude,longitude,vecti,vectj,vectk,code_local) 
! code retour toujours OK

! calcul de la vitesse dans le  repere pseudo "topocentrique" Nord  
! ................................................................
call mui_dot_product3 ( vecti , vit_car , vtopoN(1) , mu_retour )
! retour non teste car toujours positif
call mui_dot_product3 ( vectj , vit_car , vtopoN(2) , mu_retour )

call mui_dot_product3 ( vectk , vit_car , vtopoN(3) , mu_retour )

! affectation de la sortie (non utilisable dans les calculs)
vit_pseudo_topoN(1:3) = vtopoN(1:3)

! Calcul du jacobien analytique (pour tous les aplatissements)
!=============================================================

if (present(jacobcartopoN)) then

   if (jac_non_calc) then ! intialisation issue de l'appel a mt_car_geod
      retour = retour_jac_non_calc ! a pu etre ecrase dans les calculs intermediaires par un warning
      go to 6000
   end if

   ! test a faire pour tous les cas d'aplatissement
   if ((sqrt(pos_car(1)**2+pos_car(2)**2)) < eps_dist) then ! traitement des cas aux poles Nord et Sud 
      retour = pm_err_jac_non_calc_poles
      go to 6000
   end if

  ! premier bloc
  ! ------------

  jacobcartopoN(1:3,1:3)= jacob_pos(:,:)

  ! deuxieme bloc
  ! -------------

  jacobcartopoN(1:3,4:6) = 0._pm_reel

  ! troisieme bloc
  ! --------------

  jzzpmkzyp         = vectj(3)*vtopoN(3)-vectk(3)*vtopoN(2)

  jacobcartopoN(4,1:3)= - ( jacobcartopoN(1,1:3)*vtopoN(3) + jacobcartopoN(2,1:3)*jzzpmkzyp ) ! avec jacobcartopoN(2,3)=0

  kzxpmizzp         = vectk(3)*vtopoN(1)-vecti(3)*vtopoN(3)

  jacobcartopoN(5,1:3)= - ( jacobcartopoN(2,1:3)* kzxpmizzp) ! avec jacobcartopoN(2,3) = 0

  izypmjzxp         = vecti(3)*vtopoN(2)-vectj(3)*vtopoN(1)

  jacobcartopoN(6,1:3)=  jacobcartopoN(1,1:3)*vtopoN(1) - jacobcartopoN(2,1:3)*izypmjzxp ! avec jacobcartopoN(2,3) = 0

  ! quatrieme bloc
  ! --------------

  jacobcartopoN(4,4:6)= vecti(1:3)

  jacobcartopoN(5,4:6)= vectj(1:3)

  jacobcartopoN(6,4:6)= vectk(1:3)

end if

6000 continue

end subroutine mti_car_pseudo_topoN
