subroutine mi_pb_deux_corps ( mu, pos_car_t0, vit_car_t0, duree, sol_kep, pos_car_t1, vit_car_t1, code_retour, &
     encke, dist_t0, dist_t1, acc_t0, acc_t1, deriv_t0, deriv_t1, deriv_mu_t0, deriv_mu_t1)

! (c) copyright cnes - mslib - 2000

!************************************************************************
!
! but: probleme de deux corps 
! ===
!
! note d'utilisation: sans objet 
! ==================
!
!$Historique
! ==========
!   + version 6.3 : DM-ID 381 : integrer des routines du theme trajectoires interplanetaires
!                   creation a partir de la routine MPBODY de la MSLIB77
!                   (date: 09/2005 - realisation: Claire Fabre - Atos Origin)
!   + Version 6.4 : DM-ID 422 : Integrer l'ancienne procedure MPDLAM dans le 
!                   nouveau theme interplanetaire
!                   (Date: 04/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! modules
! =======

use int_utilitaires, only : mu_norme

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! declarations
! ============
implicit none

intrinsic present

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            :: mu           ! constante d'attraction du corps central
real(pm_reel), dimension(3), intent(in)              :: pos_car_t0   ! vecteur position en coordonnées cartésiennes à t0 initial
real(pm_reel), dimension(3), intent(in)              :: vit_car_t0   ! vecteur vitesse en coordonnées cartésiennes à t0 initial
real(pm_reel), intent(in)                            :: duree        ! duree de transfert
real(pm_reel), intent(out)                           :: sol_kep      ! solution de l'equation de kepler
real(pm_reel), dimension(3), intent(out)             :: pos_car_t1   ! vecteur position en coordonnées cartésiennes à t1 final
real(pm_reel), dimension(3), intent(out)             :: vit_car_t1   ! vecteur vitesse en coordonnées cartésiennes à t1 final
type(tm_code_retour), intent(out)                    :: code_retour
real(pm_reel), intent(in), optional                  :: encke        ! approximation de solution de l'equation de kepler generalise a t ou parametre de encke
real(pm_reel), intent(out), optional                 :: dist_t0      ! distance initiale
real(pm_reel), intent(out), optional                 :: dist_t1      ! distance finale
real(pm_reel), dimension(3), intent(out), optional   :: acc_t0       ! vecteur acceleration initial
real(pm_reel), dimension(3), intent(out), optional   :: acc_t1       ! vecteur acceleration final
real(pm_reel), dimension(6,6), intent(out), optional :: deriv_t0     ! derivees partielles initiales
real(pm_reel), dimension(6,6), intent(out), optional :: deriv_t1     ! derivees partielles finales
real(pm_reel), dimension(6), intent(out), optional   :: deriv_mu_t0  ! derivees partielles initiales par rapport a mu
real(pm_reel), dimension(6), intent(out), optional   :: deriv_mu_t1  ! derivees partielles finales par rapport a mu

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! autres declarations
! ===================

real(pm_reel)   :: dist0, dist1=0._pm_reel   ! distances initiale et finale
real(pm_reel), dimension(3)      :: acct0, acct1       ! vecteur acceleration initial et final
real(pm_reel)   :: rsigi,ralpha,runsri  !variables pour kepler
real(pm_reel)   :: rdtau,runsr=0._pm_reel          !variables pour kepler
real(pm_reel)  :: rpsip=0._pm_reel,rpsin=0._pm_reel !bornes positive et negative de psi
real(pm_reel)  :: rdtp=0._pm_reel,rdtn=0._pm_reel  !     bornes positive et negative de delta tau
integer :: imodif,imods   !     indicateur de mise a jour
!     variables de travail
real(pm_reel)  :: rps2,rdelta,rdeltb=0._pm_reel,rtemp
real(pm_reel)  :: ru,rgdm1,rpsi
integer :: ip3,jp3

!     series
real(pm_reel) :: rc53=0._pm_reel,rc4=0._pm_reel,rc3,rc2,rc1,rc0=0._pm_reel
real(pm_reel) :: rs3=0._pm_reel,rs2=0._pm_reel,rs1=0._pm_reel

!     fonctions f et g
real(pm_reel)  :: rfm1,rfd,rg
real(pm_reel)  :: r3,ri3

!     indices de boucles
integer :: i,j

logical          :: iter            ! logique pour les iterations
real(pm_reel)    ::  eps100         ! variable epsilon machine *100

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) fichier mslib mi_pb_deux_corps.f90: derniere modification V6.13 >'

! ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mi_pb_deux_corps.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_ok

! autres initialisations

eps100 = 100._pm_reel * epsilon(1._pm_reel)
iter = .true.

! verification des parametres en entree
! ------------------------

if (mu <= eps100) then
   if (mu < 0._pm_reel) then              ! constante de la gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else
      code_retour%valeur = pm_err_mu_nul  ! constante de la gravitation nulle
   end if
   go to 6000
end if

! test sur la coherence des entrees/sorties optionnelles si precisees

if (((present(deriv_t0)) .and. (.not. present(deriv_t1))) .or. &
    ((present(deriv_t1)) .and. (.not. present(deriv_t0))) ) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if
if (((present(deriv_mu_t0)) .and. (.not. present(deriv_t1))) .or. &
     ((present(deriv_mu_t1)) .and. (.not. present(deriv_t1))) ) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if (present(encke)) then
   rpsi = encke              ! si encke est precise par l'appelant
else
   rpsi = 0._pm_reel         ! sinon 0
end if

! initialisation pour la resolution de l'equation de kepler
! ------------------------

call mu_norme(pos_car_t0, dist0, code_retour)
if (dist0 < eps100)  then   ! vecteur position nul
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if

rsigi  = pos_car_t0(1) * vit_car_t0(1) + pos_car_t0(2) * vit_car_t0(2) + pos_car_t0(3) * vit_car_t0(3)
runsri = 1._pm_reel / dist0
ralpha = vit_car_t0(1) * vit_car_t0(1) + vit_car_t0(2) * vit_car_t0(2) + vit_car_t0(3) * vit_car_t0(3) &
     -2._pm_reel * mu * runsri

if ( duree < 0._pm_reel ) then
   rpsin = - 1.e+40_pm_reel
   rpsip =   0._pm_reel
   rdtn  =   rpsin
   rdtp  = - duree
elseif ( duree > 0._pm_reel ) then
   rpsip = + 1.e+40_pm_reel
   rpsin =   0._pm_reel
   rdtp  =   rpsip
   rdtn  = - duree
else
   rpsi  =   0._pm_reel
endif

if ((abs(duree) > eps100).and.((rpsi <= rpsin) &
     .or. (rpsi >= rpsip))) then
   rpsi = duree * runsri
   if ( (rpsi >= rpsip).or.(rpsi <= rpsin) ) then
      rpsi = duree
   endif
endif

! resolution de l'equation de kepler
! ------------------------

do while (iter)
   iter = .false.
   imodif = 0
   rps2   = rpsi * rpsi
   rdelta = ralpha * rps2
   if (abs(duree) > eps100) then
      rdeltb = 1._pm_reel / rdelta
   endif
   
   do while (abs(rdelta) > 1._pm_reel) ! iterations tant que abs(rdlta) > 1
      imodif = imodif + 1
      rdelta = rdelta * 0.25_pm_reel
   enddo

! calcul series rc53, rc4, rc3, rc1, rc0
! ------------------------

   rc53 = (1._pm_reel+(1._pm_reel+(1._pm_reel+(1._pm_reel+(1._pm_reel+(1._pm_reel+(1._pm_reel+ rdelta / 342._pm_reel) &
        *rdelta / 272._pm_reel)*rdelta / 210._pm_reel)*rdelta / 156._pm_reel) &
        *rdelta / 110._pm_reel)*rdelta / 72._pm_reel)*rdelta / 42._pm_reel) / 40._pm_reel
   rc4  = (1._pm_reel+(1._pm_reel+(1._pm_reel+(1._pm_reel+(1._pm_reel+(1._pm_reel+(1._pm_reel+ rdelta / 306._pm_reel) &
        *rdelta / 240._pm_reel)*rdelta / 182._pm_reel)*rdelta / 132._pm_reel) &
        *rdelta /  90._pm_reel)*rdelta / 56._pm_reel)*rdelta / 30._pm_reel) / 24._pm_reel
   rc3   = (0.5_pm_reel + rdelta * rc53 ) / 3._pm_reel
   rc2   =  0.5_pm_reel + rdelta * rc4
   rc1   =  1._pm_reel  + rdelta * rc3
   rc0   =  1._pm_reel  + rdelta * rc2
   
   imods = imodif
   do while (imodif > 0) ! iterations tant que imodif non nul
      rc1 = rc1 * rc0
      rc0 = 2._pm_reel * rc0 * rc0 - 1._pm_reel
      imodif = imodif - 1
   enddo

! mise a jour de rc2, rc3, rc4, rc53
! ------------------------

   if (imods > 0) then
      rc2  = (rc0 - 1._pm_reel) * rdeltb
      rc3  = (rc1 - 1._pm_reel) * rdeltb
      rc4  = (rc2 - 0.5_pm_reel) * rdeltb
      rc53 = (3._pm_reel* rc3 - 0.5_pm_reel) * rdeltb
   endif
   
! calcul des series rs1, rs2, rs3
! ------------------------

   rs1 = rc1 * rpsi
   rs2 = rc2 * rps2
   rs3 = rc3 * rps2 * rpsi

! equation de kepler
! ------------------------

   rg = dist0 * rs1 + rsigi * rs2
   rdtau = (rg + mu * rs3 ) - duree
   dist1 = abs(dist0 * rc0 + rsigi * rs1 + mu * rs2)
   runsr = 1._pm_reel / dist1

   if (rdtau < 0._pm_reel) then
      rpsin = rpsi
      rdtn = rdtau
      rpsi = rpsi - rdtau * runsr
   else
      if (rdtau > 0._pm_reel) then
         rpsip = rpsi
         rdtp = rdtau
         rpsi = rpsi - rdtau * runsr
      endif
   endif

   if ( ( abs(rdtau) > eps100) .and. &
        ( rpsi > rpsin .and. rpsi < rpsip)) then
      !          nouvelle iteration de la methode de newton
      iter = .true.
   else
      if ( abs(rdtau) > eps100 ) then

!     ... essai methode 1
         if((abs(duree) > eps100).and.( abs(rdtn) < abs(rdtp) ) ) then
            rpsi = rpsin * ( 1._pm_reel - ( 4._pm_reel * rdtn ) / duree )
         endif
         if ( (abs(duree) > eps100) .and. (abs(rdtp) < abs(rdtn)) ) then
            rpsi = rpsip * ( 1._pm_reel - ( 4._pm_reel * rdtp ) / duree )
         endif
         if ( ( rpsi > rpsin ).and.( rpsi < rpsip )) then
            !          nouvelle iteration de la methode de newton
            iter = .true.
         else
            
            !     ... essai methode 2
            if (duree > 0._pm_reel ) then
               rpsi = 2._pm_reel * rpsin
            endif
            if (duree < 0._pm_reel ) then
               rpsi = 2._pm_reel * rpsip
            endif
            if ((rpsi > rpsin ).and.(rpsi < rpsip)) then
               !          nouvelle iteration de la methode de newton
               iter = .true.
            else
               
               !           ... essai methode 3
               rtemp = rdtn - rdtp
               rtemp = rdtn / rtemp
               rpsi  = rpsi + ( rpsip - rpsin ) * rtemp
               if ( (rpsi > rpsin).and.(rpsi < rpsip )) then
                  !          nouvelle iteration de la methode de newton
                  iter = .true.
               else
                  
                  !     ... essai methode 4
                  rpsi = rpsin + ( rpsip - rpsin ) * 0.5_pm_reel
                  if ( (abs( rpsi - rpsin) > eps100).and.(abs( rpsi-rpsip ) > eps100)) then
                     !          nouvelle iteration de la methode de newton
                     iter = .true.
                  endif
               endif
            endif
         endif
      endif
   endif
enddo
sol_kep = rpsi

! coordonnees cartesiennes et acceleration
! ------------------------

rfm1  = - mu * rs2 * runsri
rfd   = - mu * rs1 * runsri * runsr
rg    =   duree - mu * rs3
rgdm1 = - mu * rs2 * runsr
r3    =   dist1 * dist1 * dist1
r3    =   1._pm_reel / r3
ri3   =   dist0 * dist0 * dist0
ri3   =   1._pm_reel / ri3

do i = 1,3
   pos_car_t1(i) = pos_car_t0(i) + rfm1 * pos_car_t0(i) + rg*vit_car_t0(i)
   vit_car_t1(i) = vit_car_t0(i) + rfd  * pos_car_t0(i) + rgdm1*vit_car_t0(i)
enddo

do i = 1,3
   acct0(i) = -mu*pos_car_t0(i) * ri3
   acct1(i) = -mu*pos_car_t1(i) * r3
enddo

! sorties optionnelles
! ------------------------

if (present(dist_t0)) then
   dist_t0 = dist0
endif
if (present(dist_t1)) then
   dist_t1 = dist1
endif
if (present(acc_t0)) then
   acc_t0(:) = acct0(:)
endif
if (present(acc_t1)) then
   acc_t1(:) = acct1(:)
endif

! derivees partielles
! ------------------------

if (present(deriv_t0) .or. present(deriv_t1) .or. present(deriv_mu_t0) .or. present(deriv_mu_t1)) then
   
   rps2 = rpsi * rpsi
   rps2 = rps2 * rps2 * rpsi
   ru   = rs2 * duree + mu * (rc4 - rc53 ) * rps2

!  initialisation pour matrice p1
   deriv_t1(1,1) = - (rfd * rs1 + rfm1 * runsri) * runsri
   deriv_t1(1,2) = -  rfd * rs2
   deriv_t1(2,1) = rfm1* rs1 * runsri
   deriv_t1(2,2) = rfm1* rs2

!  initialisation pour matrice p2
   deriv_t1(1,3) = deriv_t1(1,2 )
   deriv_t1(1,4) = - rgdm1* rs2
   deriv_t1(2,3) = deriv_t1(2,2)
   deriv_t1(2,4) = rg*rs2
      
!     ... initialisation pour matrice p3
   deriv_t1(3,1) = - rfd * ( rc0*runsri*runsr + runsr*runsr &
        +runsri*runsri )
   deriv_t1(3,2) = - (rfd * rs1 + rgdm1 * runsr ) * runsr
   deriv_t1(4,1) = - deriv_t1(1,1)
   deriv_t1(4,2) = - deriv_t1(1,2)

!  initialisation pour matrice p4
   deriv_t1(3,3) = deriv_t1(3,2)
   deriv_t1(3,4) = - rgdm1* rs1 * runsr
   deriv_t1(4,3) = - deriv_t1(1,2)
   deriv_t1(4,4) = - deriv_t1(1,4)

!  matrice pour la derivation des vitesses finale et initiale
!         par rapport a mu
   deriv_t1(1,5) = -   rs1 * runsr * runsri
   deriv_t1(2,5) =     rs2 * runsri
   deriv_t1(3,5) =     ru  * runsri - rs3
   deriv_t1(1,6) = -   deriv_t1(1,5)
   deriv_t1(2,6) =     rs2 * runsr
   deriv_t1(3,6) = -   ru  * runsr  + rs3

   if (present(deriv_mu_t0) .or. present(deriv_mu_t1)) then
      do i = 1,3
         ip3 =  i + 3
         deriv_mu_t1(i)   = - deriv_t1(2,5) * pos_car_t1(i) + deriv_t1(3,5) * vit_car_t1(i)
         deriv_mu_t1(ip3) =   deriv_t1(1,5) * pos_car_t1(i) + deriv_t1(2,5) * vit_car_t1(i) &
              + deriv_t1(3,5) * acc_t1(i)
         deriv_mu_t0(i)   = - deriv_t1(2,6) * pos_car_t0(i) + deriv_t1(3,6) * vit_car_t0(i)
         deriv_mu_t0(ip3) =   deriv_t1(1,6) * pos_car_t0(i) + deriv_t1(2,6) * vit_car_t0(i) &
              + deriv_t1(3,6) * acc_t0(i)
      enddo
   endif

!  matrices p1 p2 p3 p4
   do i = 1,3
      do j = 1,4
         ip3 = i + 3
         deriv_t0(j,i)   = deriv_t1(j,1) * pos_car_t0(i) + deriv_t1(j,2) * vit_car_t0(i)
         deriv_t0(j,ip3) = deriv_t1(j,3) * pos_car_t0(i) + deriv_t1(j,4) * vit_car_t0(i)
      enddo
   enddo

! matrice derivee du vecteur-position vitesse final
! par rapport au vecteur-position vitesse initial
! ------------------------
   do i = 1,3
      ip3 = i + 3
      do j = 1,3
         jp3 = j + 3
         deriv_t1(i,j)    = pos_car_t1(i) * deriv_t0(1,j)    +  deriv_t0(2,j)   * vit_car_t1(i)  + &
              ru * vit_car_t1(i) * acc_t0(j)
         deriv_t1(i,jp3)  = pos_car_t1(i) * deriv_t0(1,jp3)  +  deriv_t0(2,jp3) * vit_car_t1(i)  - &
              ru * vit_car_t1(i) * vit_car_t0(j)
         deriv_t1(ip3,j)  = pos_car_t1(i) * deriv_t0(3,j)   +   deriv_t0(4,j)   * vit_car_t1(i)  + &
              ru * acc_t1(i)  * acc_t0(j)
         deriv_t1(ip3,jp3)= pos_car_t1(i) * deriv_t0(3,jp3)  +  deriv_t0(4,jp3) * vit_car_t1(i)  - &
              ru * acc_t1(i) * vit_car_t0(j)
      enddo
      deriv_t1(i,i)    = deriv_t1(i,i)     + rfm1 + 1._pm_reel
      deriv_t1(i,ip3)  = deriv_t1(i,ip3)   + rg
      deriv_t1(ip3,i)  = deriv_t1(ip3,i)   + rfd
      deriv_t1(ip3,ip3)= deriv_t1(ip3,ip3) + rgdm1 + 1._pm_reel
   enddo

!     matrice derivee du vecteur-position vitesse initial
!     par rapport au vecteur-position vitesse final
! ------------------------
   do i = 1,3
      do j = 1,3
         ip3 = i + 3
         jp3 = j + 3
         deriv_t0(j,i)    =   deriv_t1(ip3,jp3)
         deriv_t0(j,ip3)  = - deriv_t1(i,jp3)
         deriv_t0(jp3,i)  = - deriv_t1(ip3,j)
         deriv_t0(jp3,ip3)=   deriv_t1(i,j)
      enddo
   enddo
   
endif

6000 continue

code_retour%routine = pm_num_mi_pb_deux_corps
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_ok) code_retour%message = ' '

end subroutine mi_pb_deux_corps
