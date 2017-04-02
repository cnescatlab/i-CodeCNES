subroutine mr_CIP_EME2000_iers2003(jul1950, delta_te, dX, dY, pos_CIP, pos_EME2000,code_retour,&
                                   inertiel, vit_CIP, vit_EME2000, jacob, mat,vect_rot)

! (C) Copyright CNES - MSLIB - 2008

!************************************************************************
!
! But:  Passage du repere celeste intermediaire CIP au repere equatorial moyen EME2000 a la meme date
!       d'après la définition de l'IERS2003
!
! $Remarques : ce code est couvert par la DV BIBMS n°18 (Code lié aux changements de repères de l'IERS 2003 (MSLIB90))
!              Plus d'informations sur ces routines est disponible dans la note algorithmique du thème R de la MSLIB90
!              -> BIBMS-SME-19-2025-ATOS
! 
! ===
!
!$Historique
! ==========
!   + Version 6.9 : DM-ID 1092 Création
!                   (Date: 07/2008 - Realisation: Atos origin)
!   + Version 6.10 : AQ : application de la DV 18
!                    FA-ID 1217 : Correction de la vitesse d'entrainement 
!                   (Date: 02/2009 - Realisation: Atos origin)
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

use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use int_rep_internes

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jours,sec)
real(pm_reel),                      intent(in)            :: delta_te    ! ecart entre l'echelle de temps TE (ou TT) et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),                      intent(in)            :: dX          ! Paramètre de correction sur la quantité X
real(pm_reel),                      intent(in)            :: dY          ! Paramètre de correction sur la quantité Y
real(pm_reel),        dimension(3), intent(in)            :: pos_CIP     ! vecteur position dans le repere celeste intermediaire
real(pm_reel),        dimension(3), intent(out)           :: pos_EME2000 ! vecteur position dans le repere equatorial moyen EME2000
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel    ! indicateur vrai si calcul inertiel (sans vitesse d'entrainement)
real(pm_reel),        dimension(3), intent(in) , optional :: vit_CIP     ! vecteur vitesse dans le repere celeste intermediaire
real(pm_reel),        dimension(3), intent(out), optional :: vit_EME2000 ! vecteur vitesse dans le repere equatorial moyen EME2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation
real(pm_reel),      dimension(3,3), intent(out), optional :: mat         ! matrice de rotation instantannee
real(pm_reel),      dimension(3),   intent(out), optional :: vect_rot    ! vecteur rotation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel)           :: X,X_tmp     ! 1er vecteur unitaire du CIP dans le GCRS
real(pm_reel)           :: Y,Y_tmp     ! 2eme vecteur unitaire du CIP dans le GCRS
real(pm_reel)           :: S           ! quantité positionnant le CEO
real(pm_reel)           :: DXt         ! derivé de X a la date t
real(pm_reel)           :: DYt          ! derivé de Y a la date t
real(pm_reel)           :: DS          ! derivé de S a la date t
real(pm_reel), dimension(3,3) :: Q                                                    ! matrice de changement de repere
real(pm_reel), dimension(3,3) :: Q_intermediaire, R, M_rot_1, M_rot_2                 ! matrices intermédiaires
real(pm_reel), dimension(3)   :: omega, vit_EME2000_inertiel,vit_EME2000_entrainement ! vecteurs intermediaires
integer                       :: ii, jj
type(tm_code_retour)          :: code_retour_local                                    ! code retour local
type(tm_jour_sec)             :: date_te, date_te_tmp                                 ! temps en temps TE
real(pm_reel)                 :: aa,Da                                                ! variable intermedaire
logical                       :: inertiel2

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mr_CIP_EME2000_iers2003.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_CIP_EME2000_iers2003.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK
code_retour_local%valeur = pm_OK

! Verifications
! ===============

! test sur la coherence des entrees/sorties optionnelles si precisees
if ((present(vit_EME2000)) .and. (.not. present(vit_CIP))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_CIP)) .and. (.not. present(vit_EME2000))) then
   code_retour%valeur = pm_warn_para_option
end if

if (present(inertiel)) then
  inertiel2=inertiel
else
  inertiel2=.true.
end if

! Calculs
! =======

! calcul du temps en temps TE
date_te_tmp%jour=jul1950%jour
date_te_tmp%sec=jul1950%sec+delta_te  
call md_joursec_norme (date_te_tmp, date_te, code_retour)
! pas de code de retour a tester ici

! - Calcul de X,Y,S,DXt,DYt,DS
call mri_XYS_iers2003(date_te,X_tmp,Y_tmp,S,DXt,DYt,DS,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = pm_err_paramXYS
    go to 6000
end if

! Correction de X et Y
X=X_tmp+dX
Y=Y_tmp+dY

! - Calcul de la matrice de changement de repère Q
! matrice intermediaire Q_intermediaire
! Q_intermediaire = [ 1-aX2   -aXY     X     ]
!                   [ -aXY   1-aY2     Y     ]
!                   [ -X      -Y   1-a(X2+Y2)]

aa=1._pm_reel/(1._pm_reel+sqrt(1._pm_reel-X*X-Y*Y))
Q_intermediaire(1,1)=1._pm_reel-aa*X*X
Q_intermediaire(1,2)=-aa*X*Y
Q_intermediaire(1,3)=X
Q_intermediaire(2,1)=-aa*X*Y
Q_intermediaire(2,2)=1._pm_reel-aa*Y*Y
Q_intermediaire(2,3)=Y
Q_intermediaire(3,1)=-X
Q_intermediaire(3,2)=-Y
Q_intermediaire(3,3)=1._pm_reel-aa*(X*X+Y*Y)

! matrice intermediaire R
! R=R3(S)
call mri_R3_iers2003(S,R,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
    code_retour%valeur = pm_err_calc_mat
    go to 6000
end if

! matrice Q=Q_intermediaire*R
call mu_matmul3(Q_intermediaire,R,Q,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
  code_retour%valeur = pm_err_calc_mat
  go to 6000
end if


! - Calcul de pos_EME2000=Q*pos_CIP

call mu_mulvect3(Q,pos_CIP,pos_EME2000, code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = pm_err_calc_transfo
   go to 6000
end if

! vecteur rotation intermediaire
Da =(aa*aa*(X*DXt+Y*DYt))/(sqrt(1._pm_reel-X*X-Y*Y))
! omega= [                -DYt-YaXDXt-aY2DYt -DaY3-XDS-YDaX2                       ]
!        [                XaYDYt+XDaY2+aX2DXt+DXt-YDS+DaX3                         ]
!        [-DaXY3a-a2XDYtY2-DaX3aY-2*a2X2DXtY-DS-DXtY+DSaY2+DSaX2+DaXY-a2DXtY3+aXDYt]
omega(1)=-DYt-Y*aa*X*DXt-aa*Y*Y*DYt -Da*Y*Y*Y-X*DS-Y*Da*X*X
omega(2)=X*aa*Y*DYt+X*Da*Y*Y+aa*X*X*DXt+DXt-Y*DS+Da*X*X*X
omega(3)=-Da*X*Y*Y*Y*aa-aa*aa*X*DYt*Y*Y-Da*X*X*X*aa*Y-2._pm_reel*aa*aa*X*X*DXt*Y-DS-DXt*Y+DS*aa*Y*Y+&
         DS*aa*X*X+Da*X*Y-aa*aa*DXt*Y*Y*Y+aa*X*DYt

! - Calcul de vit_EME2000

if (present(vit_EME2000)) then

   if (inertiel2) then ! calcul inertiel
  
      ! vit_EME2000=Q*vit_CIP
      call mu_mulvect3(Q,vit_CIP,vit_EME2000, code_retour_local)
      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = pm_err_calc_transfo
         go to 6000
      end if
  
   else ! calcul non-intertiel
  
      ! vitesse intermediaire inertiele
      call mu_mulvect3(Q,vit_CIP,vit_EME2000_inertiel, code_retour_local)
      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = pm_err_calc_transfo
         go to 6000
      end if

      ! vitesse intemediaire d'entrainement : vit_EME2000_entrainement = omega .prodvect. pos_EME2000
      call mu_prod_vect (omega, pos_EME2000, vit_EME2000_entrainement, code_retour)
      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = pm_err_calc_transfo
         go to 6000 
      end if
   
      ! vit_EME2000=vit_EME2000_inertiel+vit_EME2000_entrainement
      vit_EME2000(1)=vit_EME2000_inertiel(1)+vit_EME2000_entrainement(1)
      vit_EME2000(2)=vit_EME2000_inertiel(2)+vit_EME2000_entrainement(2)
      vit_EME2000(3)=vit_EME2000_inertiel(3)+vit_EME2000_entrainement(3)
     
   end if
end if

! - Calcul de la Jacobienne jacob

if (present(jacob)) then
   
   if (inertiel2) then ! calcul inertiel
     
      ! jacob = [ Q 0 ]
      !         [ 0 Q ]

      do ii=1,3
         do jj=1,3
            jacob(ii,jj)= Q(ii,jj)
            jacob(ii,jj+3) = 0._pm_reel
            jacob(ii+3,jj) = 0._pm_reel
            jacob(ii+3,jj+3) = Q(ii,jj)
         end do
      end do

   else ! calcul non-intertiel
   
      ! matrice intermediaire M1
      ! M_rot_1 = [     0     -omega(3)  omega(2) ]
      !           [  omega(3)    0      -omega(1) ]
      !           [ -omega(2) omega(1)      0     ]
      M_rot_1(1,1)=0._pm_reel
      M_rot_1(1,2)=-omega(3)
      M_rot_1(1,3)=omega(2)
      M_rot_1(2,1)=omega(3)
      M_rot_1(2,2)=0._pm_reel
      M_rot_1(2,3)=-omega(1)
      M_rot_1(3,1)=-omega(2)
      M_rot_1(3,2)=omega(1)
      M_rot_1(3,3)=0._pm_reel
      ! matrice intermediaire M_rot_2=Q*M_rot_1
      call mu_matmul3(Q,M_rot_1,M_rot_2,code_retour_local)
      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = pm_err_calc_mat
         go to 6000
      end if
      ! matrice jacobienne : 
      ! jacob = [    Q    0 ]
      !         [ M_rot_2 Q ]
      
      do ii=1,3
         do jj=1,3
            jacob(ii,jj)= Q(ii,jj)
            jacob(ii,jj+3) = 0._pm_reel
            jacob(ii+3,jj) = M_rot_2(ii,jj)
            jacob(ii+3,jj+3) = Q(ii,jj)
         end do
      end do
   end if
endif

! - Calcul de la matrice de rotation mat

if (present(mat)) then
   ! martice associée au vecteur rotation
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
      
   else ! calcul non-inertiel
      
      ! martice associée au vecteur rotation
      ! mat = [     0     -omega(3)  omega(2) ]
      !       [  omega(3)    0      -omega(1) ]
      !       [ -omega(2) omega(1)      0     ]
      mat(1,1)=0._pm_reel
      mat(1,2)=-omega(3)
      mat(1,3)=omega(2)
      mat(2,1)=omega(3)
      mat(2,2)=0._pm_reel
      mat(2,3)=-omega(1)
      mat(3,1)=-omega(2)
      mat(3,2)=omega(1)
      mat(3,3)=0._pm_reel
      
   end if
end if

if (present(vect_rot)) then
   ! Vecteur rotation
   if (inertiel2) then
      ! Calcul inertiel
      vect_rot(1) = 0._pm_reel
      vect_rot(2) = 0._pm_reel
      vect_rot(2) = 0._pm_reel
   else
      ! Calcul non inertiel
      vect_rot = omega(1)
      vect_rot = omega(2)
      vect_rot = omega(3)
   end if
end if

6000 continue

code_retour%routine = pm_num_mr_CIP_EME2000_iers
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_CIP_EME2000_iers2003
