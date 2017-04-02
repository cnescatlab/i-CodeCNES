subroutine mii_pb_lambert_jac (mu, pos_car, vit_car, duree, deriv, retour, encke)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But:  Calcul de la jacobienne associee au probleme de Lambert
! ===   
!
! Note d'utilisation: Sans objet 
! ==================
!
!$Historique
! ==========
!   + Version 6.4 : DM-ID 422 : Integrer l'ancienne procedure MPDLAM dans le 
!                   nouveau theme interplanetaire
!                   creation a partir de la routine MPDLAM de la MSLIB f77
!                   (Date: 04/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : FA-ID 731 : Problèmes de codes sur la MSLIB soulevés par g95
!                   (Date: 05/2007 - realisation Sandrine Avril - Atos origin)
!
!   + Version 6.7 : FA-ID 828 : suppression de la variable ierr non utilisée
!                   (Date: 10/2007 - realisation Sandrine Avril- Atos origin)
!
!   + Version 6.8 : FA-ID 859 : utilisation de transpose3 et matmul3
!                   (Date: 03/2008 - realisation Atos origin)
!
!
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

use int_utilitaires, only : mu_norme
use int_interplanetaire, only : mi_pb_deux_corps
use int_util_internes, only : mui_inverse_matrice

   use precision_mslib
   use type_mslib
   use valeur_code_retour_mslib

! Declarations
! ============
   implicit none
   intrinsic epsilon

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                         :: mu        ! constante d'attraction du corps central
real(pm_reel), dimension(3), intent(in)           :: pos_car   ! vecteur position en coordonnées cartésiennes
real(pm_reel), dimension(3), intent(in)          :: vit_car   ! vecteur vitesse en coordonnées cartésiennes
real(pm_reel), intent(in)                         :: duree     ! duree de transfert
real(pm_reel), dimension(6,8), intent(out)        :: deriv     ! matrice (6,8) des derivees partielles de Lambert
integer , intent(out)                             :: retour
real(pm_reel), intent(in) , optional             :: encke     ! approximation de solution de l'equation de kepler generalise a t ou parametre de encke

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

   real(pm_reel), dimension(6,6)   ::  rp, rp1     ! derivees partielles

!     parametres d'appel de mi_pb_deux_corps
   real(pm_reel), dimension(3)   :: pos_car_f
   real(pm_reel), dimension(3)   :: vit_car_f

   real(pm_reel), dimension(3,3) :: rc11,rc21,rc22,rc12  !  matrices extraites de rp
   real(pm_reel), dimension(3,3) :: rpinv  ! matrice inverse de rc12
   real(pm_reel), dimension(3,3) :: rptran  ! matrice transposee de rpinv

   real(pm_reel), dimension(3,3) :: rmatp1  ! matrice produit de rpinv et rc11
   real(pm_reel), dimension(3,3) :: rmatp2  ! matrice produit de rc22  et rpinv
   real(pm_reel), dimension(3,3) :: rmatr   ! matrice transposee de rmatp2
   
   real(pm_reel) :: rnorm   ! norme euclidienne de pos_car
   real(pm_reel) :: rnor3,rgan
   
   real(pm_reel), dimension(3,3) :: rv  ! vecteur vitesse
   
   real(pm_reel), dimension(3,3) :: rvp, rmatp3, rmatp4 ! matrice de travail
   real(pm_reel), dimension(6,8) :: rq ! matrice de travail
   real(pm_reel), dimension(3)   :: acc_t0       ! vecteur acceleration initial
   real(pm_reel), dimension(3)   :: acc_t1       ! vecteur acceleration final

   integer :: i,j   !     indices de boucle

!     variables de travail
   integer :: ip3,jp3
   real(pm_reel)  :: rpsi, rpsi_loc

   real(pm_reel)   ::  eps100            ! variable epsilon machine *100
   type(tm_code_retour)  :: code_retour_local
   integer :: retour_local

   character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
        '@(#) fichier mslib mii_pb_lambert_jac.f90: derniere modification V6.13 >'

! ne pas toucher a la ligne suivante
   character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mii_pb_lambert_jac.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

   retour = pm_ok

! autres initialisations

   eps100 = 100._pm_reel * epsilon(1._pm_reel)

! verification des parametres en entree
! ------------------------

! constante de la gravitation
   if (mu <= eps100) then
      if (mu < 0._pm_reel) then              ! constante de la gravitation negative
         retour = pm_err_mu_negatif
      else
         retour = pm_err_mu_nul  ! constante de la gravitation nulle
      end if
      go to 6000
   end if
   
   if (duree < 0._pm_reel) then
      retour = pm_err_duree_negatif
      go to 6000
   end if

   if (present(encke)) then
      rpsi = encke              ! si encke est precise par l'appelant
   else
      rpsi = 0._pm_reel         ! sinon 0
   end if

! derivees partielles du vecteur position-vitesse 

   call mi_pb_deux_corps ( mu, pos_car, vit_car, duree, rpsi_loc, pos_car_f, &
        vit_car_f, code_retour_local, encke = rpsi, deriv_t0 = rp1, deriv_t1 = rp, &
        acc_t0 = acc_t0, acc_t1 = acc_t1)
        rpsi = rpsi_loc
   if (code_retour_local%valeur /= pm_OK) then
      retour = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK) go to 6000
   end if
 
! initialisations

   do i = 1,3
      ip3 = i + 3
      do j = 1,3
         jp3 = j + 3
         rc11(i,j) = rp(i  ,j  )
         rc12(i,j) = rp(i  ,jp3)
         rc21(i,j) = rp(ip3,j  )
         rc22(i,j) = rp(ip3,jp3)
      enddo
   enddo

! Calcul de l'inverse d'une matrice
   call mui_inverse_matrice(rc12,rpinv,retour_local)
   if (retour_local /= pm_OK) then
      retour = retour_local           ! affectation du code retour
      if (retour < pm_OK) go to 6000              ! sortie si erreur
   end if

!  Calcul de la transpose d'une matrice
   call mu_transpose3(rpinv,rptran,code_retour_local)

!  Produit de 2 matrices
   call mu_matmul3(rpinv,rc11,rmatp1,code_retour_local)
   ! pas d'erreur possible donc pas de test dur code retour

!  Produit de 2 matrices
   call mu_matmul3(rc22,rpinv,rmatp2,code_retour_local)
   ! pas d'erreur possible donc pas de test dur code retour

!  Calcul de la transpose d'une matrice
   call mu_transpose3(rmatp2,rmatr,code_retour_local)

! derivees partielles par rapport aux positions

   do i = 1,3
      ip3 = i + 3
      do j = 1,3
         jp3 = j + 3
         rq (i  ,jp3) =   rpinv  (i,j)
         rq (ip3,j  ) = - rptran (i,j)
         rq (i,j)     = - rmatp1 (i,j)
         rq (ip3,jp3) =   rmatr  (i,j)
      enddo
   enddo

! derivees partielles par rapport aux dates

   call mu_norme(pos_car, rnorm, code_retour_local)
   if (rnorm < eps100)  then   ! vecteur position nul
      retour = pm_err_pos_nul
      go to 6000
   end if

   rnor3 = rnorm * rnorm * rnorm
   rgan  = mu / rnor3
   
   do i =1,3
      rv(i,1) = vit_car(i)
   enddo

!  Produit de 2 matrices
   call mu_matmul3(rmatp1,rv,rvp,code_retour_local)
   ! pas d'erreur possible donc pas de test dur code retour

   do i = 1,3
      rq(i,7) =   rvp(i,1) - rgan * pos_car(i)
      rq(i,8) = - rq(i,7)
   enddo

!  Produit de 2 matrices
   call mu_matmul3(rmatp2,rc11,rmatp3,code_retour_local)
   ! pas d'erreur possible donc pas de test dur code retour

   do i = 1,3
      do j = 1,3
         rmatp4(i,j) = - rc21(i,j) + rmatp3(i,j)
      enddo
   enddo

!  Produit de 2 matrices
   call mu_matmul3(rmatp4,rv,rvp,code_retour_local)
   ! pas d'erreur possible donc pas de test dur code retour

   do i = 1,3
      ip3 = i + 3
      rq (ip3,7) =   rvp (i,1)
      rq (ip3,8) = - rq  (ip3,7)
   enddo

   deriv(:,:) = rq(:,:)

6000 continue

end subroutine mii_pb_lambert_jac
