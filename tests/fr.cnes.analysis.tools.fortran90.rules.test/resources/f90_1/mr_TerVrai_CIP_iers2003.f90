subroutine mr_TerVrai_CIP_iers2003(jul1950,delta_tu1,pos_TerVrai,pos_CIP,code_retour,inertiel,vit_TerVrai,vit_CIP,jacob, &
     mat,vect_rot)

! (C) Copyright CNES - MSLIB - 2008

!************************************************************************
!
! But:  Passage du repere terrestre vrai au repere celeste intermediaire CIP a la meme date
!       d'après la définition de l'IERS2003
! ===
!
! $Remarques : ce code est couvert par la DV BIBMS n°18 (Code lié aux changements de repères de l'IERS 2003 (MSLIB90))
!              Plus d'informations sur ces routines est disponible dans la note algorithmique du thème R de la MSLIB90
!              -> BIBMS-SME-19-2025-ATOS
! 
!$Historique
! ==========
!   + Version 6.9 : DM-ID 1092 Création
!                   (Date: 07/2008 - Realisation: Atos origin)
!   + Version 6.10 : AQ : application de la DV 18
!                    FA-ID 1217 : Correction du vecteur rotation  
!                   (Date: 02/2009 - Realisation: Atos origin)
!   + Version 6.11 : FA-ID 1305 : Optimisation du calcul de t_tu1_j2000
!                   (Date: 09/2009 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!   + Version 6.13.1 : FA-ID 1473 : Correction du signe et de l'unité du vecteur rotation
!                      (Date : 04/2011 - Réalisation : Atos origin)
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======

use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use int_constantes, only : pm_deux_pi, pm_DM1092_TerVrai_CIP_cte1, pm_DM1092_TerVrai_CIP_cte2
use parametres_internes_mslib, only : pm_i_date_t2000

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jour,sec)                             
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),        dimension(3), intent(in)            :: pos_TerVrai ! vecteur position dans le repere terrestre vrai a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_CIP     ! vecteur position dans le repere celeste intermedaire a la date t
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel    ! indicateur vrai si calcul inertiel (sans vitesse d'entrainement)
real(pm_reel),        dimension(3), intent(in) , optional :: vit_TerVrai ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),        dimension(3), intent(out), optional :: vit_CIP     ! vecteur vitesse dans le repere celeste intermediare a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation
real(pm_reel),      dimension(3,3), intent(out), optional :: mat         ! matrice de rotation instantannee
real(pm_reel),      dimension(3),   intent(out), optional :: vect_rot    ! vecteur rotation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel), dimension(3,3) :: R                                           ! matrice de changement de repere
real(pm_reel), dimension(3,3) :: M_rot, M_rot_R                              ! matrices intermédiaires
real(pm_reel), dimension(3)   :: vit_CIP_inertiel,vect_rot2,vect_entrainement ! vecteur intermediaire
integer                       :: ii, jj
type(tm_code_retour)          :: code_retour_local                           ! code retour local
real(pm_reel)                 :: t_tu1_j2000,t_tu1_j2000_mod1                ! temps en TU1; patie fractionnaire de t_tu1_j2000
real(pm_reel)                 :: jul1950_frac
real(pm_reel)                 :: theta, theta_point                          ! position anglulaire, rotation
real(pm_reel), parameter      :: nb_sec_par_jour=86400_pm_reel               ! nombre de seconde en un jour
logical                       :: inertiel2                                            ! indicateur vrai si calcul inertiel
type(tm_jour_sec)             :: jul1950_tmp                                 ! date julienne en (jour,sec)

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mr_TerVrai_CIP_iers2003.f90: derniere modification V6.13.1 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_TerVrai_CIP_iers2003.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK
code_retour_local%valeur = pm_OK

! Verifications
! ===============

! test sur la coherence des entrees/sorties optionnelles si precisees
if ((present(vit_CIP)) .and. (.not. present(vit_TerVrai))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_TerVrai)) .and. (.not. present(vit_CIP))) then
   code_retour%valeur = pm_warn_para_option
end if

if (present(inertiel)) then
  inertiel2=inertiel
else
  inertiel2=.true.
end if

! Calculs
! =======

! Calcul de t_tu1_j2000 : temps TU1 en jour par rapport à J2000

jul1950_tmp%jour = jul1950%jour - int(pm_i_date_t2000)
jul1950_tmp%sec  = jul1950%sec  + delta_tu1

! Conversion de la date en jour frac
call md_joursec_jourfrac(jul1950_tmp,jul1950_frac,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   ! Probleme lors de la conversion de la date
   code_retour%valeur = pm_err_conv_date
   goto 6000
end if

t_tu1_j2000=jul1950_frac - (pm_i_date_t2000 - real(int(pm_i_date_t2000),kind=pm_reel) )

! - Calcul de theta(t_tu1) selon la methode de l'IERS
t_tu1_j2000_mod1=mod(t_tu1_j2000,1._pm_reel)
theta=pm_deux_pi*(t_tu1_j2000_mod1+pm_DM1092_TerVrai_CIP_cte1+pm_DM1092_TerVrai_CIP_cte2*t_tu1_j2000)
! On fait le modulo 2pi
theta = modulo(theta,pm_deux_pi)

! - Calcul de la matrice de changement de repere R=R(-theta)
call mri_R3_iers2003(-theta,R,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = pm_err_calc_mat
    go to 6000
end if
      
! - Calcul de pos_CIP=R*pos_TerVrai
call mu_mulvect3(R,pos_TerVrai,pos_CIP, code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = pm_err_calc_transfo
   go to 6000
end if

! - Calcul de vit_CIP
if (present(vit_CIP)) then
  if (inertiel2) then ! calcul inertiel
  
    ! vit_CIP=R*vit_TerVrai
    call mu_mulvect3(R,vit_TerVrai,vit_CIP, code_retour_local)
    if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = pm_err_calc_transfo
      go to 6000
    end if
    
  else ! calcul non-inertiel 
  
  ! vecteur intermediaire vit_: CIP_inertiel = R*vit_TerVrai
  call mu_mulvect3(R,vit_TerVrai,vit_CIP_inertiel, code_retour_local)
  if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = pm_err_calc_transfo
    go to 6000
  end if
  ! vecteur intermediaire de rotation : vect_rot = [0 0 theta_point]t
  vect_rot2(1)=0._pm_reel
  vect_rot2(2)=0._pm_reel
  !! on ramène la vitesse d'entrainement par rapport à la seconde
  !! elle était exprimée par rapport au jour !
  vect_rot2(3)=pm_deux_pi*(1._pm_reel+pm_DM1092_TerVrai_CIP_cte2)/nb_sec_par_jour
 
  ! vecteur intemediaire d'entrainement : vect_entrainement=vect_rot .prodvect. pos_CIP
  call mu_prod_vect (vect_rot2, pos_CIP, vect_entrainement, code_retour)

  if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = pm_err_calc_transfo
    go to 6000
  end if
  ! vit_CIP = vit_CIP_inertiel+vect_entrainement

  vit_CIP(1)=vit_CIP_inertiel(1)+vect_entrainement(1)
  vit_CIP(2)=vit_CIP_inertiel(2)+vect_entrainement(2)
  vit_CIP(3)=vit_CIP_inertiel(3)+vect_entrainement(3)
  end if
  
end if

! - Calcul de la Jacobienne jacob
if (present(jacob)) then
  if (inertiel2) then ! calcul inertiel
    !        [ R 0 ]
    !jacob = [ 0 R ]
    do ii=1,3
      do jj=1,3
        jacob(ii,jj) =    R(ii,jj)
	jacob(ii,jj+3) =  0._pm_reel
	jacob(ii+3,jj) =  0._pm_reel
        jacob(ii+3,jj+3) = R(ii,jj)
      end do
    end do
  else ! calcul non-inertiel
    ! matrice intermediaire associée au vecteur rotation
    theta_point=pm_deux_pi*(1._pm_reel+pm_DM1092_TerVrai_CIP_cte2)/nb_sec_par_jour
    !         [      0 -theta_point    0 ]
    ! M_rot = [ theta_point   0       0 ]
    !         [      0        0        0 ]
    M_rot(1,1)=0._pm_reel
    M_rot(1,2)=-theta_point
    M_rot(1,3)=0._pm_reel
    M_rot(2,1)=theta_point
    M_rot(2,2)=0._pm_reel
    M_rot(2,3)=0._pm_reel
    M_rot(3,1)=0._pm_reel
    M_rot(3,2)=0._pm_reel
    M_rot(3,3)=0._pm_reel
    call mu_matmul3(M_rot,R,M_rot_R,code_retour_local)
    if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = pm_err_calc_mat
      go to 6000
    end if
    !        [ R        0 ]
    !jacob = [ M_rot*R  R ]
    do ii=1,3
      do jj=1,3
        jacob(ii,jj)     = R(ii,jj)
	jacob(ii,jj+3)   = 0._pm_reel
	jacob(ii+3,jj)   = M_rot_R(ii,jj)
        jacob(ii+3,jj+3) = R(ii,jj)
      end do
    end do 
  end if
end if

! - Calcul de la matrice de rot instantanée
if (present(mat)) then
    ! matrice associée au vecteur rotation
    if (inertiel2) then ! calcul inertiel
      mat(1,1)=0._pm_reel
      mat(1,2)=0._pm_reel
      mat(1,3)=0._pm_reel
      mat(2,1)=0._pm_reel
      mat(2,2)=0._pm_reel
      mat(2,3)=0._pm_reel
      mat(3,1)=0._pm_reel
      mat(3,2)=0._pm_reel
      mat(3,3)=0._pm_reel
    else
      ! matrice associée au vecteur rotation
      theta_point=pm_deux_pi*(1._pm_reel+pm_DM1092_TerVrai_CIP_cte2)
      !       [      0  -theta_point    0 ]
      ! mat = [ theta_point   0       0 ]
      !       [      0        0        0 ]
      mat(1,1)=0._pm_reel
      mat(1,2)=-theta_point
      mat(1,3)=0._pm_reel
      mat(2,1)=theta_point
      mat(2,2)=0._pm_reel
      mat(2,3)=0._pm_reel
      mat(3,1)=0._pm_reel
      mat(3,2)=0._pm_reel
      mat(3,3)=0._pm_reel
    end if
end if

if (present(vect_rot)) then
   ! vecteur rotation
   if (inertiel2) then
      ! cas inertiel
      vect_rot(1) = 0._pm_reel
      vect_rot(2) = 0._pm_reel
      vect_rot(3) = 0._pm_reel
   else
      ! cas non inertiel
      theta_point=pm_deux_pi*(1._pm_reel+pm_DM1092_TerVrai_CIP_cte2)/nb_sec_par_jour
      vect_rot(1) = 0._pm_reel
      vect_rot(2) = 0._pm_reel
      vect_rot(3) = theta_point
   end if
end if

6000 continue

code_retour%routine = pm_num_mr_TerVrai_CIP_iers
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_TerVrai_CIP_iers2003
