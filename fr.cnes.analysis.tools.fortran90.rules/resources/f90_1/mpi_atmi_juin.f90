subroutine mpi_atmi_juin (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JUIN 
! ===
!
! Note d'utilisation:  Sans objet
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0  creation a partir d'une portion de la routine MCATMI 
!                         (Date: 12/2000 - Realisation: Veronique Lepine)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!VERSION:5.15:FA-ID:1398:30/09/2010:Ajout du marqueur de fin historique
!
!$FinHistorique
!
!************************************************************************

  ! Modules
  ! =======

  use mslib

  use parametre_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(:,:), intent(out)    ::  tbar  ! temperatures 
real(pm_reel), dimension(:,:), intent(out)    ::  zbar  ! altitudes
real(pm_reel), dimension(:,:), intent(out)    ::  z1    ! amplitude de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phi1  ! phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  t1    ! amplitude de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  phit1 ! phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(out)    ::  z2    ! amplitude de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phi2  ! phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  t2    ! amplitude de la temperature de l'onde 2
real(pm_reel), dimension(:,:), intent(out)    ::  phit2 ! phase de la temperature de l'onde 2
integer, intent(out)                          ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_80= (/ &
       264.30_pm_r,-219._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       241.10_pm_r,3470._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.50_pm_r,6825._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.30_pm_r,9932._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.50_pm_r,12916._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.70_pm_r,15830._pm_r,11._pm_r,221._pm_r,.62_pm_r,207._pm_r,3._pm_r,172._pm_r,.34_pm_r,346._pm_r, &
       192.80_pm_r,18682._pm_r,12._pm_r,219._pm_r,.95_pm_r,202._pm_r,3._pm_r,173._pm_r,.50_pm_r,347._pm_r, &
       189.40_pm_r,21481._pm_r,13._pm_r,217._pm_r,1.11_pm_r,192._pm_r,2._pm_r,175._pm_r,.58_pm_r,348._pm_r, &
       181.40_pm_r,24199._pm_r,15._pm_r,214._pm_r,.82_pm_r,173._pm_r,1._pm_r,179._pm_r,.43_pm_r,352._pm_r, &
       182.70_pm_r,26848._pm_r,15._pm_r,211._pm_r,.51_pm_r,117._pm_r,1._pm_r,181._pm_r,.13_pm_r,5._pm_r, &
       198.00_pm_r,29624._pm_r,15._pm_r,208._pm_r,.66_pm_r,68._pm_r,1._pm_r,177._pm_r,.14_pm_r,159._pm_r, &
       213.70_pm_r,32645._pm_r,14._pm_r,206._pm_r,.82_pm_r,44._pm_r,1._pm_r,174._pm_r,.24_pm_r,168._pm_r, &
       227.10_pm_r,35871._pm_r,12._pm_r,205._pm_r,.91_pm_r,24._pm_r,1._pm_r,173._pm_r,.23_pm_r,173._pm_r, &
       238.20_pm_r,39283._pm_r,11._pm_r,205._pm_r,.89_pm_r,28._pm_r,2._pm_r,175._pm_r,.13_pm_r,225._pm_r, &
       250.30_pm_r,42854._pm_r,10._pm_r,205._pm_r,1.06_pm_r,26._pm_r,2._pm_r,184._pm_r,.25_pm_r,256._pm_r, &
       262.30_pm_r,46615._pm_r,8._pm_r,206._pm_r,1.40_pm_r,16._pm_r,2._pm_r,197._pm_r,.41_pm_r,240._pm_r, &
       267.90_pm_r,50504._pm_r,6._pm_r,212._pm_r,1.83_pm_r,3._pm_r,3._pm_r,205._pm_r,.59_pm_r,216._pm_r, &
       265.40_pm_r,54416._pm_r,4._pm_r,237._pm_r,1.85_pm_r,348._pm_r,3._pm_r,206._pm_r,.58_pm_r,203._pm_r, &
       257.70_pm_r,58254._pm_r,4._pm_r,276._pm_r,1.70_pm_r,336._pm_r,4._pm_r,204._pm_r,.48_pm_r,188._pm_r, &
       248.40_pm_r,61957._pm_r,6._pm_r,295._pm_r,1.38_pm_r,323._pm_r,5._pm_r,202._pm_r,.29_pm_r,170._pm_r, &
       241.10_pm_r,65542._pm_r,7._pm_r,300._pm_r,1.03_pm_r,305._pm_r,5._pm_r,199._pm_r,.17_pm_r,111._pm_r, &
       235.30_pm_r,69028._pm_r,9._pm_r,298._pm_r,.83_pm_r,275._pm_r,5._pm_r,196._pm_r,.32_pm_r,51._pm_r, &
       230.60_pm_r,72439._pm_r,10._pm_r,294._pm_r,.85_pm_r,243._pm_r,4._pm_r,192._pm_r,.54_pm_r,35._pm_r, &
       228.80_pm_r,75799._pm_r,10._pm_r,287._pm_r,1.02_pm_r,223._pm_r,3._pm_r,187._pm_r,.71_pm_r,30._pm_r, &
       227.70_pm_r,79139._pm_r,11._pm_r,279._pm_r,1.16_pm_r,213._pm_r,2._pm_r,176._pm_r,.81_pm_r,28._pm_r, &
       227.80_pm_r,82463._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.20_pm_r,85789._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.60_pm_r,89039._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.00_pm_r,92150._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.10_pm_r,95182._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.90_pm_r,98212._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.30_pm_r,101316._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.90_pm_r,104579._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       236.90_pm_r,108097._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       261.80_pm_r,111991._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       320.50_pm_r,116585._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
       269.60_pm_r,-169._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       244.10_pm_r,3582._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.20_pm_r,6980._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.10_pm_r,10129._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.10_pm_r,13168._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.70_pm_r,16160._pm_r,15._pm_r,223._pm_r,1.06_pm_r,169._pm_r,2._pm_r,152._pm_r,.14_pm_r,49._pm_r, &
       199.80_pm_r,19107._pm_r,16._pm_r,218._pm_r,1.59_pm_r,164._pm_r,2._pm_r,145._pm_r,.25_pm_r,45._pm_r, &
       196.50_pm_r,22009._pm_r,18._pm_r,210._pm_r,1.88_pm_r,155._pm_r,2._pm_r,132._pm_r,.39_pm_r,42._pm_r, &
       189.00_pm_r,24834._pm_r,19._pm_r,203._pm_r,1.62_pm_r,136._pm_r,2._pm_r,115._pm_r,.48_pm_r,38._pm_r, &
       189.90_pm_r,27595._pm_r,19._pm_r,197._pm_r,1.24_pm_r,97._pm_r,2._pm_r,99._pm_r,.48_pm_r,33._pm_r, &
       202.40_pm_r,30457._pm_r,18._pm_r,193._pm_r,1.35_pm_r,53._pm_r,3._pm_r,85._pm_r,.42_pm_r,27._pm_r, &
       216.50_pm_r,33528._pm_r,17._pm_r,189._pm_r,1.64_pm_r,29._pm_r,3._pm_r,76._pm_r,.38_pm_r,20._pm_r, &
       229.70_pm_r,36793._pm_r,14._pm_r,187._pm_r,1.82_pm_r,14._pm_r,3._pm_r,68._pm_r,.33_pm_r,13._pm_r, &
       240.60_pm_r,40240._pm_r,11._pm_r,186._pm_r,1.91_pm_r,15._pm_r,4._pm_r,62._pm_r,.29_pm_r,11._pm_r, &
       252.00_pm_r,43842._pm_r,8._pm_r,182._pm_r,2.29_pm_r,15._pm_r,4._pm_r,57._pm_r,.27_pm_r,5._pm_r, &
       262.20_pm_r,47614._pm_r,5._pm_r,174._pm_r,2.92_pm_r,11._pm_r,4._pm_r,53._pm_r,.17_pm_r,345._pm_r, &
       266.00_pm_r,51488._pm_r,1._pm_r,91._pm_r,3.52_pm_r,5._pm_r,4._pm_r,51._pm_r,.16_pm_r,254._pm_r, &
       263.10_pm_r,55367._pm_r,5._pm_r,14._pm_r,3.15_pm_r,355._pm_r,4._pm_r,51._pm_r,.27_pm_r,220._pm_r, &
       256.30_pm_r,59176._pm_r,9._pm_r,3._pm_r,2.55_pm_r,344._pm_r,3._pm_r,53._pm_r,.31_pm_r,207._pm_r, &
       247.90_pm_r,62866._pm_r,12._pm_r,356._pm_r,1.87_pm_r,327._pm_r,3._pm_r,58._pm_r,.24_pm_r,187._pm_r, &
       240.40_pm_r,66443._pm_r,14._pm_r,350._pm_r,1.37_pm_r,299._pm_r,3._pm_r,63._pm_r,.17_pm_r,139._pm_r, &
       233.90_pm_r,69915._pm_r,15._pm_r,343._pm_r,1.35_pm_r,261._pm_r,3._pm_r,67._pm_r,.27_pm_r,85._pm_r, &
       228.40_pm_r,73300._pm_r,15._pm_r,334._pm_r,1.71_pm_r,236._pm_r,4._pm_r,68._pm_r,.44_pm_r,69._pm_r, &
       225.10_pm_r,76616._pm_r,14._pm_r,323._pm_r,2.10_pm_r,224._pm_r,4._pm_r,67._pm_r,.58_pm_r,62._pm_r, &
       223.60_pm_r,79892._pm_r,14._pm_r,310._pm_r,2.33_pm_r,217._pm_r,5._pm_r,66._pm_r,.66_pm_r,61._pm_r, &
       224.30_pm_r,83165._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.00_pm_r,86449._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.50_pm_r,89642._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.60_pm_r,92700._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.10_pm_r,95687._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.00_pm_r,98675._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.00_pm_r,101734._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.40_pm_r,104948._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       234.20_pm_r,108418._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.40_pm_r,112295._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       323.20_pm_r,116924._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
       274.90_pm_r,-99._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       250.00_pm_r,3736._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.70_pm_r,7213._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.20_pm_r,10420._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.50_pm_r,13523._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.50_pm_r,16608._pm_r,17._pm_r,224._pm_r,1.60_pm_r,142._pm_r,2._pm_r,354._pm_r,.34_pm_r,150._pm_r, &
       206.80_pm_r,19655._pm_r,17._pm_r,215._pm_r,2.20_pm_r,138._pm_r,1._pm_r,6._pm_r,.47_pm_r,145._pm_r, &
       204.80_pm_r,22671._pm_r,18._pm_r,204._pm_r,2.40_pm_r,129._pm_r,1._pm_r,38._pm_r,.53_pm_r,135._pm_r, &
       199.20_pm_r,25631._pm_r,19._pm_r,194._pm_r,1.90_pm_r,111._pm_r,1._pm_r,74._pm_r,.46_pm_r,113._pm_r, &
       198.50_pm_r,28535._pm_r,18._pm_r,187._pm_r,1.51_pm_r,63._pm_r,2._pm_r,80._pm_r,.41_pm_r,69._pm_r, &
       204.00_pm_r,31476._pm_r,16._pm_r,183._pm_r,2.14_pm_r,20._pm_r,2._pm_r,71._pm_r,.56_pm_r,33._pm_r, &
       214.40_pm_r,34538._pm_r,13._pm_r,180._pm_r,2.81_pm_r,3._pm_r,3._pm_r,58._pm_r,.71_pm_r,16._pm_r, &
       227.60_pm_r,37769._pm_r,8._pm_r,181._pm_r,2.97_pm_r,356._pm_r,4._pm_r,47._pm_r,.75_pm_r,8._pm_r, &
       239.40_pm_r,41195._pm_r,4._pm_r,185._pm_r,2.87_pm_r,0._pm_r,5._pm_r,39._pm_r,.58_pm_r,2._pm_r, &
       250.60_pm_r,44780._pm_r,0._pm_r,325._pm_r,3.09_pm_r,2._pm_r,5._pm_r,34._pm_r,.44_pm_r,360._pm_r, &
       258.40_pm_r,48517._pm_r,5._pm_r,358._pm_r,3.48_pm_r,357._pm_r,6._pm_r,31._pm_r,.30_pm_r,360._pm_r, &
       259.80_pm_r,52317._pm_r,10._pm_r,355._pm_r,3.73_pm_r,348._pm_r,6._pm_r,30._pm_r,.10_pm_r,6._pm_r, &
       257.90_pm_r,56110._pm_r,15._pm_r,351._pm_r,2.98_pm_r,336._pm_r,6._pm_r,30._pm_r,.04_pm_r,157._pm_r, &
       253.90_pm_r,59862._pm_r,19._pm_r,347._pm_r,2.25_pm_r,319._pm_r,6._pm_r,30._pm_r,.10_pm_r,156._pm_r, &
       248.00_pm_r,63537._pm_r,21._pm_r,342._pm_r,1.72_pm_r,297._pm_r,6._pm_r,32._pm_r,.11_pm_r,129._pm_r, &
       241.00_pm_r,67121._pm_r,23._pm_r,337._pm_r,1.44_pm_r,270._pm_r,6._pm_r,33._pm_r,.15_pm_r,79._pm_r, &
       234.00_pm_r,70597._pm_r,23._pm_r,332._pm_r,1.45_pm_r,245._pm_r,6._pm_r,35._pm_r,.28_pm_r,55._pm_r, &
       227.30_pm_r,73977._pm_r,23._pm_r,327._pm_r,1.59_pm_r,226._pm_r,7._pm_r,36._pm_r,.41_pm_r,45._pm_r, &
       222.00_pm_r,77260._pm_r,22._pm_r,320._pm_r,1.76_pm_r,215._pm_r,7._pm_r,37._pm_r,.51_pm_r,43._pm_r, &
       219.50_pm_r,80474._pm_r,21._pm_r,314._pm_r,1.82_pm_r,208._pm_r,8._pm_r,37._pm_r,.57_pm_r,41._pm_r, &
       219.70_pm_r,83692._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.30_pm_r,86913._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.40_pm_r,90017._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.80_pm_r,92996._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.90_pm_r,95920._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.90_pm_r,98850._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.40_pm_r,101848._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.20_pm_r,104988._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.90_pm_r,108386._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.40_pm_r,112228._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       327.10_pm_r,116902._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
       280.20_pm_r,-59._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       256.40_pm_r,3865._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       231.00_pm_r,7430._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.20_pm_r,10700._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.70_pm_r,13859._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.90_pm_r,17015._pm_r,9._pm_r,208._pm_r,1.37_pm_r,137._pm_r,3._pm_r,55._pm_r,.48_pm_r,195._pm_r, &
       213.20_pm_r,20149._pm_r,10._pm_r,195._pm_r,1.70_pm_r,130._pm_r,2._pm_r,68._pm_r,.60_pm_r,192._pm_r, &
       212.90_pm_r,23270._pm_r,11._pm_r,183._pm_r,1.63_pm_r,116._pm_r,2._pm_r,91._pm_r,.55_pm_r,185._pm_r, &
       209.50_pm_r,26366._pm_r,12._pm_r,173._pm_r,1.33_pm_r,82._pm_r,2._pm_r,109._pm_r,.33_pm_r,161._pm_r, &
       206.80_pm_r,29410._pm_r,11._pm_r,163._pm_r,1.66_pm_r,30._pm_r,2._pm_r,110._pm_r,.35_pm_r,68._pm_r, &
       207.60_pm_r,32439._pm_r,8._pm_r,152._pm_r,2.65_pm_r,3._pm_r,3._pm_r,95._pm_r,.84_pm_r,38._pm_r, &
       214.70_pm_r,35526._pm_r,5._pm_r,130._pm_r,3.24_pm_r,350._pm_r,4._pm_r,75._pm_r,1.20_pm_r,30._pm_r, &
       227.20_pm_r,38755._pm_r,3._pm_r,55._pm_r,3.11_pm_r,341._pm_r,5._pm_r,60._pm_r,1.26_pm_r,26._pm_r, &
       239.90_pm_r,42182._pm_r,6._pm_r,9._pm_r,2.54_pm_r,338._pm_r,7._pm_r,53._pm_r,.68_pm_r,26._pm_r, &
       249.80_pm_r,45768._pm_r,9._pm_r,357._pm_r,2.33_pm_r,336._pm_r,7._pm_r,51._pm_r,.20_pm_r,44._pm_r, &
       254.40_pm_r,49468._pm_r,12._pm_r,350._pm_r,2.39_pm_r,332._pm_r,7._pm_r,52._pm_r,.14_pm_r,148._pm_r, &
       253.80_pm_r,53194._pm_r,15._pm_r,345._pm_r,2.48_pm_r,326._pm_r,7._pm_r,54._pm_r,.19_pm_r,161._pm_r, &
       251.40_pm_r,56895._pm_r,18._pm_r,341._pm_r,1.88_pm_r,310._pm_r,7._pm_r,55._pm_r,.05_pm_r,90._pm_r, &
       247.00_pm_r,60548._pm_r,20._pm_r,336._pm_r,1.43_pm_r,290._pm_r,7._pm_r,55._pm_r,.15_pm_r,23._pm_r, &
       241.70_pm_r,64125._pm_r,21._pm_r,332._pm_r,1.16_pm_r,269._pm_r,8._pm_r,53._pm_r,.28_pm_r,21._pm_r, &
       235.70_pm_r,67623._pm_r,22._pm_r,328._pm_r,1.01_pm_r,250._pm_r,8._pm_r,52._pm_r,.40_pm_r,25._pm_r, &
       229.30_pm_r,71026._pm_r,22._pm_r,325._pm_r,.90_pm_r,235._pm_r,9._pm_r,50._pm_r,.49_pm_r,28._pm_r, &
       223.30_pm_r,74341._pm_r,22._pm_r,321._pm_r,.83_pm_r,223._pm_r,9._pm_r,48._pm_r,.57_pm_r,30._pm_r, &
       218.70_pm_r,77574._pm_r,22._pm_r,318._pm_r,.78_pm_r,213._pm_r,10._pm_r,47._pm_r,.61_pm_r,32._pm_r, &
       216.20_pm_r,80746._pm_r,21._pm_r,315._pm_r,.72_pm_r,205._pm_r,11._pm_r,45._pm_r,.62_pm_r,33._pm_r, &
       214.80_pm_r,83915._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.40_pm_r,87059._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.70_pm_r,90060._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.60_pm_r,92954._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.80_pm_r,95812._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.60_pm_r,98681._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.60_pm_r,101614._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.30_pm_r,104674._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.60_pm_r,107986._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       261.60_pm_r,111777._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       331.20_pm_r,116493._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
       286.30_pm_r,-32._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.00_pm_r,3982._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       236.10_pm_r,7628._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.70_pm_r,10957._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.70_pm_r,14137._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.00_pm_r,17291._pm_r,4._pm_r,180._pm_r,.97_pm_r,139._pm_r,4._pm_r,68._pm_r,.62_pm_r,241._pm_r, &
       215.40_pm_r,20441._pm_r,6._pm_r,169._pm_r,1.05_pm_r,128._pm_r,3._pm_r,70._pm_r,.68_pm_r,243._pm_r, &
       218.00_pm_r,23618._pm_r,7._pm_r,159._pm_r,.85_pm_r,99._pm_r,2._pm_r,71._pm_r,.52_pm_r,250._pm_r, &
       217.70_pm_r,26811._pm_r,7._pm_r,149._pm_r,.96_pm_r,41._pm_r,2._pm_r,68._pm_r,.19_pm_r,306._pm_r, &
       217.20_pm_r,29993._pm_r,6._pm_r,134._pm_r,1.69_pm_r,7._pm_r,2._pm_r,57._pm_r,.63_pm_r,33._pm_r, &
       219.10_pm_r,33184._pm_r,4._pm_r,103._pm_r,2.43_pm_r,352._pm_r,3._pm_r,49._pm_r,1.29_pm_r,41._pm_r, &
       225.60_pm_r,36437._pm_r,4._pm_r,48._pm_r,2.59_pm_r,341._pm_r,6._pm_r,46._pm_r,1.68_pm_r,43._pm_r, &
       237.10_pm_r,39816._pm_r,6._pm_r,15._pm_r,2.14_pm_r,325._pm_r,8._pm_r,45._pm_r,1.66_pm_r,43._pm_r, &
       249.90_pm_r,43391._pm_r,8._pm_r,359._pm_r,1.45_pm_r,308._pm_r,10._pm_r,46._pm_r,.88_pm_r,66._pm_r, &
       255.30_pm_r,47098._pm_r,9._pm_r,348._pm_r,1.11_pm_r,285._pm_r,11._pm_r,50._pm_r,.69_pm_r,127._pm_r, &
       253.70_pm_r,50829._pm_r,9._pm_r,339._pm_r,1.10_pm_r,258._pm_r,10._pm_r,57._pm_r,.90_pm_r,158._pm_r, &
       248.70_pm_r,54512._pm_r,10._pm_r,328._pm_r,1.28_pm_r,240._pm_r,10._pm_r,64._pm_r,.78_pm_r,171._pm_r, &
       243.30_pm_r,58113._pm_r,9._pm_r,315._pm_r,1.69_pm_r,218._pm_r,10._pm_r,67._pm_r,.22_pm_r,213._pm_r, &
       238.50_pm_r,61643._pm_r,9._pm_r,299._pm_r,1.80_pm_r,208._pm_r,10._pm_r,66._pm_r,.39_pm_r,318._pm_r, &
       232.40_pm_r,65091._pm_r,9._pm_r,283._pm_r,1.52_pm_r,202._pm_r,10._pm_r,61._pm_r,.69_pm_r,337._pm_r, &
       225.70_pm_r,68447._pm_r,10._pm_r,272._pm_r,1.08_pm_r,196._pm_r,10._pm_r,54._pm_r,.88_pm_r,345._pm_r, &
       220.80_pm_r,71713._pm_r,10._pm_r,265._pm_r,.58_pm_r,185._pm_r,11._pm_r,48._pm_r,.99_pm_r,352._pm_r, &
       216.40_pm_r,74917._pm_r,10._pm_r,262._pm_r,.21_pm_r,141._pm_r,11._pm_r,42._pm_r,1.05_pm_r,356._pm_r, &
       212.70_pm_r,78055._pm_r,10._pm_r,262._pm_r,.30_pm_r,62._pm_r,13._pm_r,37._pm_r,1.07_pm_r,359._pm_r, &
       210.90_pm_r,81144._pm_r,9._pm_r,263._pm_r,.47_pm_r,46._pm_r,14._pm_r,33._pm_r,1.04_pm_r,1._pm_r, &
       208.70_pm_r,84236._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.20_pm_r,87274._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.70_pm_r,90171._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.20_pm_r,92994._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.40_pm_r,95798._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.00_pm_r,98619._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.20_pm_r,101494._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.80_pm_r,104484._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.30_pm_r,107713._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       260.10_pm_r,111450._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       335.00_pm_r,116199._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
       292.00_pm_r,-34._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       268.20_pm_r,4070._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.10_pm_r,7816._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.70_pm_r,11222._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.60_pm_r,14389._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.60_pm_r,17454._pm_r,4._pm_r,152._pm_r,.10_pm_r,169._pm_r,3._pm_r,81._pm_r,.58_pm_r,259._pm_r, &
       213.90_pm_r,20553._pm_r,4._pm_r,152._pm_r,.07_pm_r,340._pm_r,2._pm_r,81._pm_r,.63_pm_r,265._pm_r, &
       219.30_pm_r,23728._pm_r,4._pm_r,152._pm_r,.41_pm_r,342._pm_r,1._pm_r,75._pm_r,.49_pm_r,281._pm_r, &
       223.00_pm_r,26967._pm_r,3._pm_r,149._pm_r,.82_pm_r,342._pm_r,1._pm_r,50._pm_r,.36_pm_r,336._pm_r, &
       226.10_pm_r,30256._pm_r,2._pm_r,137._pm_r,1.18_pm_r,340._pm_r,2._pm_r,33._pm_r,.68_pm_r,26._pm_r, &
       230.20_pm_r,33594._pm_r,1._pm_r,40._pm_r,1.33_pm_r,337._pm_r,3._pm_r,34._pm_r,1.13_pm_r,39._pm_r, &
       238.50_pm_r,37024._pm_r,2._pm_r,351._pm_r,1.12_pm_r,331._pm_r,5._pm_r,37._pm_r,1.34_pm_r,43._pm_r, &
       249.80_pm_r,40595._pm_r,3._pm_r,341._pm_r,.66_pm_r,309._pm_r,7._pm_r,39._pm_r,1.24_pm_r,44._pm_r, &
       258.80_pm_r,44328._pm_r,4._pm_r,334._pm_r,.36_pm_r,277._pm_r,8._pm_r,41._pm_r,.59_pm_r,69._pm_r, &
       261.50_pm_r,48144._pm_r,4._pm_r,326._pm_r,.55_pm_r,215._pm_r,8._pm_r,45._pm_r,.49_pm_r,128._pm_r, &
       256.90_pm_r,51944._pm_r,3._pm_r,308._pm_r,1.31_pm_r,191._pm_r,8._pm_r,50._pm_r,.57_pm_r,136._pm_r, &
       248.90_pm_r,55651._pm_r,3._pm_r,259._pm_r,2.32_pm_r,182._pm_r,9._pm_r,55._pm_r,.53_pm_r,102._pm_r, &
       241.40_pm_r,59240._pm_r,5._pm_r,213._pm_r,2.97_pm_r,177._pm_r,9._pm_r,56._pm_r,.45_pm_r,42._pm_r, &
       233.20_pm_r,62719._pm_r,9._pm_r,196._pm_r,2.94_pm_r,173._pm_r,10._pm_r,54._pm_r,.60_pm_r,4._pm_r, &
       223.60_pm_r,66063._pm_r,13._pm_r,189._pm_r,2.34_pm_r,168._pm_r,10._pm_r,49._pm_r,.70_pm_r,343._pm_r, &
       215.30_pm_r,69276._pm_r,16._pm_r,184._pm_r,1.55_pm_r,160._pm_r,11._pm_r,44._pm_r,.75_pm_r,326._pm_r, &
       210.50_pm_r,72389._pm_r,17._pm_r,181._pm_r,.86_pm_r,139._pm_r,11._pm_r,38._pm_r,.79_pm_r,312._pm_r, &
       207.30_pm_r,75449._pm_r,18._pm_r,178._pm_r,.59_pm_r,88._pm_r,11._pm_r,31._pm_r,.83_pm_r,301._pm_r, &
       205.50_pm_r,78469._pm_r,17._pm_r,176._pm_r,.75_pm_r,51._pm_r,11._pm_r,25._pm_r,.84_pm_r,295._pm_r, &
       205.00_pm_r,81468._pm_r,16._pm_r,172._pm_r,.93_pm_r,37._pm_r,11._pm_r,18._pm_r,.82_pm_r,290._pm_r, &
       203.10_pm_r,84478._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.60_pm_r,87421._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.50_pm_r,90241._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.60_pm_r,93016._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,95789._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.70_pm_r,98581._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.10_pm_r,101418._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.90_pm_r,104358._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.40_pm_r,107528._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       259.10_pm_r,111225._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       338.50_pm_r,116006._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
       296.90_pm_r,-46._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.00_pm_r,4140._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.80_pm_r,7980._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.40_pm_r,11457._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.90_pm_r,14604._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.50_pm_r,17569._pm_r,4._pm_r,140._pm_r,.49_pm_r,272._pm_r,1._pm_r,74._pm_r,.27_pm_r,270._pm_r, &
       212.00_pm_r,20612._pm_r,3._pm_r,151._pm_r,.66_pm_r,279._pm_r,1._pm_r,66._pm_r,.28_pm_r,285._pm_r, &
       219.30_pm_r,23773._pm_r,2._pm_r,170._pm_r,.73_pm_r,286._pm_r,1._pm_r,47._pm_r,.23_pm_r,316._pm_r, &
       224.60_pm_r,27024._pm_r,2._pm_r,198._pm_r,.75_pm_r,295._pm_r,1._pm_r,30._pm_r,.30_pm_r,4._pm_r, &
       229.70_pm_r,30350._pm_r,2._pm_r,227._pm_r,.67_pm_r,306._pm_r,2._pm_r,27._pm_r,.48_pm_r,30._pm_r, &
       237.90_pm_r,33769._pm_r,2._pm_r,249._pm_r,.48_pm_r,319._pm_r,2._pm_r,30._pm_r,.64_pm_r,40._pm_r, &
       247.30_pm_r,37325._pm_r,3._pm_r,260._pm_r,.23_pm_r,342._pm_r,3._pm_r,33._pm_r,.67_pm_r,44._pm_r, &
       256.80_pm_r,41013._pm_r,2._pm_r,264._pm_r,.14_pm_r,98._pm_r,4._pm_r,36._pm_r,.56_pm_r,44._pm_r, &
       264.00_pm_r,44833._pm_r,2._pm_r,263._pm_r,.23_pm_r,96._pm_r,5._pm_r,38._pm_r,.25_pm_r,83._pm_r, &
       265.20_pm_r,48714._pm_r,2._pm_r,256._pm_r,.39_pm_r,136._pm_r,5._pm_r,42._pm_r,.28_pm_r,122._pm_r, &
       259.60_pm_r,52561._pm_r,2._pm_r,223._pm_r,1.08_pm_r,155._pm_r,5._pm_r,47._pm_r,.32_pm_r,105._pm_r, &
       249.90_pm_r,56298._pm_r,4._pm_r,186._pm_r,2.10_pm_r,159._pm_r,6._pm_r,50._pm_r,.35_pm_r,63._pm_r, &
       239.50_pm_r,59880._pm_r,7._pm_r,173._pm_r,2.56_pm_r,159._pm_r,6._pm_r,49._pm_r,.28_pm_r,14._pm_r, &
       228.00_pm_r,63309._pm_r,10._pm_r,168._pm_r,2.38_pm_r,159._pm_r,6._pm_r,46._pm_r,.31_pm_r,327._pm_r, &
       215.70_pm_r,66554._pm_r,13._pm_r,166._pm_r,1.70_pm_r,158._pm_r,6._pm_r,41._pm_r,.35_pm_r,296._pm_r, &
       207.20_pm_r,69645._pm_r,15._pm_r,165._pm_r,.84_pm_r,155._pm_r,6._pm_r,36._pm_r,.40_pm_r,273._pm_r, &
       204.50_pm_r,72656._pm_r,16._pm_r,164._pm_r,.10_pm_r,108._pm_r,5._pm_r,32._pm_r,.45_pm_r,257._pm_r, &
       202.90_pm_r,75640._pm_r,16._pm_r,164._pm_r,.57_pm_r,349._pm_r,5._pm_r,27._pm_r,.51_pm_r,243._pm_r, &
       202.10_pm_r,78602._pm_r,14._pm_r,164._pm_r,.97_pm_r,345._pm_r,4._pm_r,21._pm_r,.54_pm_r,237._pm_r, &
       201.70_pm_r,81557._pm_r,13._pm_r,164._pm_r,1.18_pm_r,345._pm_r,4._pm_r,14._pm_r,.55_pm_r,233._pm_r, &
       199.10_pm_r,84510._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.00_pm_r,87387._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.50_pm_r,90167._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,92924._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,95688._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.90_pm_r,98472._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.80_pm_r,101295._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.50_pm_r,104215._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.90_pm_r,107362._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       259.60_pm_r,111051._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       342.10_pm_r,115871._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
       300.10_pm_r,-61._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.30_pm_r,4167._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.80_pm_r,8048._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.20_pm_r,11554._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.80_pm_r,14686._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.60_pm_r,17601._pm_r,2._pm_r,148._pm_r,.20_pm_r,288._pm_r,0._pm_r,82._pm_r,.05_pm_r,18._pm_r, &
       211.10_pm_r,20615._pm_r,2._pm_r,157._pm_r,.29_pm_r,285._pm_r,0._pm_r,67._pm_r,.09_pm_r,29._pm_r, &
       218.30_pm_r,23762._pm_r,1._pm_r,174._pm_r,.33_pm_r,283._pm_r,1._pm_r,55._pm_r,.14_pm_r,35._pm_r, &
       224.70_pm_r,27004._pm_r,1._pm_r,198._pm_r,.36_pm_r,281._pm_r,1._pm_r,49._pm_r,.19_pm_r,38._pm_r, &
       232.10_pm_r,30348._pm_r,1._pm_r,219._pm_r,.32_pm_r,275._pm_r,1._pm_r,46._pm_r,.22_pm_r,42._pm_r, &
       241.70_pm_r,33814._pm_r,2._pm_r,231._pm_r,.24_pm_r,266._pm_r,1._pm_r,46._pm_r,.22_pm_r,46._pm_r, &
       250.20_pm_r,37420._pm_r,2._pm_r,235._pm_r,.14_pm_r,242._pm_r,2._pm_r,46._pm_r,.19_pm_r,47._pm_r, &
       257.30_pm_r,41136._pm_r,2._pm_r,233._pm_r,.11_pm_r,170._pm_r,2._pm_r,46._pm_r,.14_pm_r,46._pm_r, &
       262.90_pm_r,44949._pm_r,2._pm_r,231._pm_r,.12_pm_r,58._pm_r,2._pm_r,49._pm_r,.13_pm_r,138._pm_r, &
       265.10_pm_r,48821._pm_r,2._pm_r,229._pm_r,.20_pm_r,83._pm_r,2._pm_r,57._pm_r,.29_pm_r,132._pm_r, &
       261.20_pm_r,52680._pm_r,2._pm_r,213._pm_r,.57_pm_r,143._pm_r,2._pm_r,70._pm_r,.44_pm_r,114._pm_r, &
       252.00_pm_r,56445._pm_r,3._pm_r,187._pm_r,1.34_pm_r,158._pm_r,3._pm_r,78._pm_r,.52_pm_r,98._pm_r, &
       239.70_pm_r,60046._pm_r,5._pm_r,174._pm_r,1.72_pm_r,161._pm_r,4._pm_r,80._pm_r,.34_pm_r,82._pm_r, &
       225.20_pm_r,63456._pm_r,7._pm_r,170._pm_r,1.67_pm_r,163._pm_r,4._pm_r,79._pm_r,.16_pm_r,37._pm_r, &
       212.60_pm_r,66654._pm_r,9._pm_r,168._pm_r,1.24_pm_r,164._pm_r,4._pm_r,76._pm_r,.21_pm_r,313._pm_r, &
       204.70_pm_r,69706._pm_r,11._pm_r,168._pm_r,.69_pm_r,167._pm_r,4._pm_r,71._pm_r,.39_pm_r,292._pm_r, &
       202.10_pm_r,72680._pm_r,11._pm_r,168._pm_r,.17_pm_r,181._pm_r,3._pm_r,64._pm_r,.54_pm_r,285._pm_r, &
       202.30_pm_r,75639._pm_r,11._pm_r,168._pm_r,.26_pm_r,331._pm_r,2._pm_r,50._pm_r,.65_pm_r,281._pm_r, &
       202.80_pm_r,78608._pm_r,11._pm_r,169._pm_r,.54_pm_r,337._pm_r,2._pm_r,26._pm_r,.71_pm_r,279._pm_r, &
       201.60_pm_r,81582._pm_r,10._pm_r,170._pm_r,.68_pm_r,338._pm_r,2._pm_r,354._pm_r,.70_pm_r,278._pm_r, &
       197.00_pm_r,84510._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.20_pm_r,87352._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,90123._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,92882._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,95652._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.20_pm_r,98440._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.20_pm_r,101269._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.30_pm_r,104199._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.30_pm_r,107364._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.20_pm_r,111086._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       346.30_pm_r,115961._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
       301.00_pm_r,-99._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.70_pm_r,4139._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.40_pm_r,8028._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.90_pm_r,11544._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.80_pm_r,14682._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.00_pm_r,17593._pm_r,1._pm_r,155._pm_r,.17_pm_r,273._pm_r,0._pm_r,203._pm_r,.10_pm_r,63._pm_r, &
       210.10_pm_r,20598._pm_r,1._pm_r,165._pm_r,.22_pm_r,267._pm_r,0._pm_r,151._pm_r,.14_pm_r,60._pm_r, &
       217.70_pm_r,23736._pm_r,1._pm_r,180._pm_r,.26_pm_r,262._pm_r,0._pm_r,99._pm_r,.17_pm_r,60._pm_r, &
       224.30_pm_r,26974._pm_r,1._pm_r,195._pm_r,.27_pm_r,255._pm_r,1._pm_r,82._pm_r,.17_pm_r,59._pm_r, &
       231.80_pm_r,30314._pm_r,2._pm_r,207._pm_r,.24_pm_r,247._pm_r,1._pm_r,76._pm_r,.17_pm_r,57._pm_r, &
       242.50_pm_r,33781._pm_r,2._pm_r,212._pm_r,.18_pm_r,232._pm_r,1._pm_r,72._pm_r,.14_pm_r,56._pm_r, &
       251.00_pm_r,37395._pm_r,2._pm_r,213._pm_r,.12_pm_r,202._pm_r,1._pm_r,70._pm_r,.10_pm_r,55._pm_r, &
       257.50_pm_r,41116._pm_r,2._pm_r,211._pm_r,.11_pm_r,146._pm_r,1._pm_r,69._pm_r,.06_pm_r,51._pm_r, &
       262.60_pm_r,44930._pm_r,2._pm_r,208._pm_r,.12_pm_r,54._pm_r,1._pm_r,73._pm_r,.12_pm_r,185._pm_r, &
       264.50_pm_r,48797._pm_r,2._pm_r,208._pm_r,.15_pm_r,16._pm_r,1._pm_r,85._pm_r,.23_pm_r,193._pm_r, &
       261.30_pm_r,52652._pm_r,2._pm_r,209._pm_r,.03_pm_r,225._pm_r,1._pm_r,99._pm_r,.20_pm_r,173._pm_r, &
       252.40_pm_r,56419._pm_r,2._pm_r,202._pm_r,.35_pm_r,185._pm_r,2._pm_r,104._pm_r,.25_pm_r,104._pm_r, &
       240.30_pm_r,60026._pm_r,3._pm_r,191._pm_r,.57_pm_r,173._pm_r,2._pm_r,100._pm_r,.37_pm_r,75._pm_r, &
       226.00_pm_r,63445._pm_r,4._pm_r,184._pm_r,.64_pm_r,171._pm_r,3._pm_r,93._pm_r,.37_pm_r,66._pm_r, &
       212.80_pm_r,66653._pm_r,5._pm_r,181._pm_r,.53_pm_r,172._pm_r,3._pm_r,87._pm_r,.23_pm_r,62._pm_r, &
       204.60_pm_r,69710._pm_r,6._pm_r,180._pm_r,.39_pm_r,177._pm_r,3._pm_r,83._pm_r,.04_pm_r,34._pm_r, &
       202.30_pm_r,72688._pm_r,6._pm_r,180._pm_r,.25_pm_r,195._pm_r,3._pm_r,81._pm_r,.14_pm_r,250._pm_r, &
       203.40_pm_r,75656._pm_r,7._pm_r,182._pm_r,.16_pm_r,227._pm_r,3._pm_r,81._pm_r,.30_pm_r,244._pm_r, &
       204.20_pm_r,78637._pm_r,6._pm_r,185._pm_r,.16_pm_r,257._pm_r,2._pm_r,82._pm_r,.39_pm_r,245._pm_r, &
       202.10_pm_r,81624._pm_r,6._pm_r,189._pm_r,.17_pm_r,277._pm_r,2._pm_r,86._pm_r,.42_pm_r,245._pm_r, &
       196.30_pm_r,84543._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.80_pm_r,87372._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.60_pm_r,90145._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,92909._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,95684._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.90_pm_r,98481._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.80_pm_r,101328._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.10_pm_r,104293._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.30_pm_r,107510._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       267.10_pm_r,111305._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       351.20_pm_r,116259._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
       301.20_pm_r,-110._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.10_pm_r,4132._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.50_pm_r,8024._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.10_pm_r,11541._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.20_pm_r,14683._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.50_pm_r,17600._pm_r,2._pm_r,168._pm_r,.22_pm_r,274._pm_r,1._pm_r,165._pm_r,.12_pm_r,22._pm_r, &
       210.80_pm_r,20610._pm_r,2._pm_r,177._pm_r,.29_pm_r,271._pm_r,1._pm_r,150._pm_r,.16_pm_r,24._pm_r, &
       218.60_pm_r,23757._pm_r,2._pm_r,189._pm_r,.32_pm_r,266._pm_r,0._pm_r,119._pm_r,.18_pm_r,23._pm_r, &
       225.00_pm_r,27005._pm_r,2._pm_r,200._pm_r,.33_pm_r,260._pm_r,0._pm_r,82._pm_r,.19_pm_r,23._pm_r, &
       231.80_pm_r,30349._pm_r,3._pm_r,208._pm_r,.28_pm_r,250._pm_r,1._pm_r,61._pm_r,.17_pm_r,26._pm_r, &
       241.30_pm_r,33809._pm_r,3._pm_r,212._pm_r,.21_pm_r,236._pm_r,1._pm_r,52._pm_r,.14_pm_r,27._pm_r, &
       250.00_pm_r,37410._pm_r,3._pm_r,213._pm_r,.13_pm_r,199._pm_r,1._pm_r,48._pm_r,.09_pm_r,33._pm_r, &
       257.50_pm_r,41125._pm_r,3._pm_r,211._pm_r,.14_pm_r,144._pm_r,1._pm_r,47._pm_r,.05_pm_r,45._pm_r, &
       263.40_pm_r,44944._pm_r,3._pm_r,208._pm_r,.13_pm_r,99._pm_r,1._pm_r,51._pm_r,.12_pm_r,167._pm_r, &
       265.20_pm_r,48820._pm_r,3._pm_r,204._pm_r,.18_pm_r,98._pm_r,1._pm_r,64._pm_r,.21_pm_r,162._pm_r, &
       260.40_pm_r,52673._pm_r,3._pm_r,198._pm_r,.26_pm_r,124._pm_r,1._pm_r,80._pm_r,.16_pm_r,152._pm_r, &
       251.50_pm_r,56428._pm_r,4._pm_r,191._pm_r,.42_pm_r,149._pm_r,1._pm_r,85._pm_r,.10_pm_r,51._pm_r, &
       240.70_pm_r,60034._pm_r,4._pm_r,186._pm_r,.48_pm_r,163._pm_r,1._pm_r,74._pm_r,.25_pm_r,13._pm_r, &
       226.90_pm_r,63464._pm_r,5._pm_r,184._pm_r,.50_pm_r,183._pm_r,2._pm_r,59._pm_r,.33_pm_r,10._pm_r, &
       214.60_pm_r,66690._pm_r,6._pm_r,186._pm_r,.55_pm_r,206._pm_r,2._pm_r,49._pm_r,.31_pm_r,17._pm_r, &
       206.60_pm_r,69771._pm_r,6._pm_r,190._pm_r,.64_pm_r,223._pm_r,2._pm_r,44._pm_r,.26_pm_r,30._pm_r, &
       203.60_pm_r,72770._pm_r,7._pm_r,195._pm_r,.74_pm_r,234._pm_r,3._pm_r,44._pm_r,.23_pm_r,52._pm_r, &
       202.50_pm_r,75743._pm_r,8._pm_r,200._pm_r,.83_pm_r,240._pm_r,3._pm_r,45._pm_r,.22_pm_r,70._pm_r, &
       202.60_pm_r,78711._pm_r,9._pm_r,206._pm_r,.87_pm_r,243._pm_r,3._pm_r,49._pm_r,.23_pm_r,81._pm_r, &
       200.50_pm_r,81680._pm_r,10._pm_r,210._pm_r,.86_pm_r,246._pm_r,4._pm_r,52._pm_r,.24_pm_r,91._pm_r, &
       195.10_pm_r,84576._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.90_pm_r,87391._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,90155._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.40_pm_r,92907._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,95669._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.40_pm_r,98465._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.10_pm_r,101332._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.30_pm_r,104345._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.60_pm_r,107644._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.10_pm_r,111544._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       356.80_pm_r,116596._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
       300.00_pm_r,-134._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.50_pm_r,4100._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.20_pm_r,7991._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.40_pm_r,11507._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.30_pm_r,14665._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.80_pm_r,17617._pm_r,3._pm_r,150._pm_r,.70_pm_r,274._pm_r,1._pm_r,165._pm_r,.29_pm_r,360._pm_r, &
       212.30_pm_r,20656._pm_r,3._pm_r,172._pm_r,.85_pm_r,274._pm_r,1._pm_r,157._pm_r,.39_pm_r,360._pm_r, &
       219.70_pm_r,23822._pm_r,3._pm_r,201._pm_r,.88_pm_r,274._pm_r,0._pm_r,123._pm_r,.44_pm_r,360._pm_r, &
       226.20_pm_r,27087._pm_r,3._pm_r,222._pm_r,.80_pm_r,274._pm_r,1._pm_r,41._pm_r,.46_pm_r,1._pm_r, &
       232.80_pm_r,30449._pm_r,4._pm_r,234._pm_r,.59_pm_r,272._pm_r,1._pm_r,19._pm_r,.43_pm_r,1._pm_r, &
       241.00_pm_r,33915._pm_r,5._pm_r,239._pm_r,.32_pm_r,266._pm_r,2._pm_r,13._pm_r,.35_pm_r,360._pm_r, &
       250.00_pm_r,37513._pm_r,5._pm_r,240._pm_r,.10_pm_r,223._pm_r,2._pm_r,10._pm_r,.25_pm_r,359._pm_r, &
       258.90_pm_r,41237._pm_r,5._pm_r,238._pm_r,.18_pm_r,137._pm_r,2._pm_r,9._pm_r,.14_pm_r,356._pm_r, &
       265.70_pm_r,45085._pm_r,5._pm_r,235._pm_r,.21_pm_r,166._pm_r,3._pm_r,9._pm_r,.03_pm_r,162._pm_r, &
       266.90_pm_r,48992._pm_r,5._pm_r,232._pm_r,.29_pm_r,196._pm_r,2._pm_r,10._pm_r,.16_pm_r,176._pm_r, &
       259.80_pm_r,52853._pm_r,5._pm_r,230._pm_r,.30_pm_r,206._pm_r,2._pm_r,10._pm_r,.18_pm_r,202._pm_r, &
       250.00_pm_r,56589._pm_r,6._pm_r,228._pm_r,.19_pm_r,201._pm_r,2._pm_r,5._pm_r,.23_pm_r,270._pm_r, &
       239.30_pm_r,60172._pm_r,6._pm_r,227._pm_r,.13_pm_r,178._pm_r,2._pm_r,353._pm_r,.33_pm_r,300._pm_r, &
       226.80_pm_r,63590._pm_r,6._pm_r,225._pm_r,.18_pm_r,196._pm_r,3._pm_r,345._pm_r,.35_pm_r,317._pm_r, &
       215.90_pm_r,66825._pm_r,7._pm_r,224._pm_r,.37_pm_r,217._pm_r,3._pm_r,342._pm_r,.25_pm_r,336._pm_r, &
       208.80_pm_r,69932._pm_r,7._pm_r,224._pm_r,.63_pm_r,223._pm_r,3._pm_r,343._pm_r,.17_pm_r,25._pm_r, &
       205.40_pm_r,72962._pm_r,8._pm_r,224._pm_r,.85_pm_r,226._pm_r,3._pm_r,348._pm_r,.24_pm_r,75._pm_r, &
       202.80_pm_r,75951._pm_r,10._pm_r,224._pm_r,1.03_pm_r,227._pm_r,3._pm_r,355._pm_r,.36_pm_r,92._pm_r, &
       201.10_pm_r,78906._pm_r,11._pm_r,225._pm_r,1.13_pm_r,228._pm_r,3._pm_r,6._pm_r,.43_pm_r,98._pm_r, &
       198.00_pm_r,81842._pm_r,13._pm_r,225._pm_r,1.15_pm_r,229._pm_r,3._pm_r,17._pm_r,.46_pm_r,101._pm_r, &
       192.20_pm_r,84698._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,87469._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.80_pm_r,90188._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.30_pm_r,92892._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.60_pm_r,95614._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.10_pm_r,98390._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.50_pm_r,101271._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.40_pm_r,104340._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       233.50_pm_r,107740._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       282.40_pm_r,111770._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       362.70_pm_r,116929._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
       296.70_pm_r,-106._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       275.70_pm_r,4087._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       250.80_pm_r,7945._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.20_pm_r,11432._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.40_pm_r,14609._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.30_pm_r,17631._pm_r,4._pm_r,163._pm_r,.92_pm_r,284._pm_r,2._pm_r,157._pm_r,.29_pm_r,352._pm_r, &
       214.40_pm_r,20718._pm_r,3._pm_r,186._pm_r,1.11_pm_r,284._pm_r,1._pm_r,151._pm_r,.38_pm_r,352._pm_r, &
       220.50_pm_r,23904._pm_r,4._pm_r,214._pm_r,1.12_pm_r,284._pm_r,1._pm_r,132._pm_r,.44_pm_r,352._pm_r, &
       227.10_pm_r,27179._pm_r,4._pm_r,234._pm_r,.98_pm_r,283._pm_r,1._pm_r,63._pm_r,.46_pm_r,350._pm_r, &
       233.70_pm_r,30553._pm_r,5._pm_r,244._pm_r,.67_pm_r,280._pm_r,1._pm_r,18._pm_r,.43_pm_r,349._pm_r, &
       242.90_pm_r,34038._pm_r,6._pm_r,248._pm_r,.31_pm_r,266._pm_r,1._pm_r,6._pm_r,.36_pm_r,346._pm_r, &
       252.50_pm_r,37668._pm_r,6._pm_r,247._pm_r,.15_pm_r,189._pm_r,2._pm_r,1._pm_r,.27_pm_r,340._pm_r, &
       262.00_pm_r,41433._pm_r,6._pm_r,244._pm_r,.29_pm_r,152._pm_r,2._pm_r,357._pm_r,.19_pm_r,331._pm_r, &
       268.60_pm_r,45327._pm_r,6._pm_r,240._pm_r,.35_pm_r,175._pm_r,2._pm_r,354._pm_r,.15_pm_r,308._pm_r, &
       268.10_pm_r,49265._pm_r,7._pm_r,236._pm_r,.46_pm_r,209._pm_r,3._pm_r,349._pm_r,.22_pm_r,302._pm_r, &
       260.70_pm_r,53141._pm_r,7._pm_r,235._pm_r,.58_pm_r,235._pm_r,3._pm_r,343._pm_r,.28_pm_r,297._pm_r, &
       250.20_pm_r,56886._pm_r,8._pm_r,236._pm_r,.60_pm_r,258._pm_r,3._pm_r,338._pm_r,.24_pm_r,283._pm_r, &
       238.60_pm_r,60464._pm_r,9._pm_r,239._pm_r,.49_pm_r,276._pm_r,3._pm_r,333._pm_r,.15_pm_r,257._pm_r, &
       226.40_pm_r,63872._pm_r,9._pm_r,242._pm_r,.37_pm_r,295._pm_r,3._pm_r,330._pm_r,.11_pm_r,238._pm_r, &
       216.40_pm_r,67108._pm_r,10._pm_r,244._pm_r,.24_pm_r,311._pm_r,3._pm_r,327._pm_r,.08_pm_r,264._pm_r, &
       209.10_pm_r,70224._pm_r,10._pm_r,245._pm_r,.13_pm_r,337._pm_r,3._pm_r,326._pm_r,.12_pm_r,310._pm_r, &
       204.10_pm_r,73247._pm_r,10._pm_r,246._pm_r,.07_pm_r,37._pm_r,4._pm_r,326._pm_r,.19_pm_r,328._pm_r, &
       200.10_pm_r,76208._pm_r,10._pm_r,246._pm_r,.11_pm_r,87._pm_r,4._pm_r,326._pm_r,.26_pm_r,332._pm_r, &
       196.70_pm_r,79112._pm_r,9._pm_r,245._pm_r,.15_pm_r,102._pm_r,4._pm_r,327._pm_r,.30_pm_r,333._pm_r, &
       192.20_pm_r,81969._pm_r,9._pm_r,245._pm_r,.17_pm_r,107._pm_r,5._pm_r,327._pm_r,.30_pm_r,333._pm_r, &
       185.90_pm_r,84734._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       180.70_pm_r,87412._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       177.60_pm_r,90035._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       177.90_pm_r,92652._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.00_pm_r,95304._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,98042._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.70_pm_r,100930._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.30_pm_r,104058._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.70_pm_r,107577._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       290.90_pm_r,111749._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       368.10_pm_r,117013._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
       290.20_pm_r,-43._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       271.40_pm_r,4068._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       245.40_pm_r,7851._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.90_pm_r,11279._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.20_pm_r,14479._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.00_pm_r,17606._pm_r,3._pm_r,210._pm_r,.35_pm_r,266._pm_r,2._pm_r,143._pm_r,.27_pm_r,336._pm_r, &
       217.70_pm_r,20766._pm_r,3._pm_r,218._pm_r,.43_pm_r,263._pm_r,1._pm_r,139._pm_r,.37_pm_r,336._pm_r, &
       221.90_pm_r,23984._pm_r,4._pm_r,225._pm_r,.46_pm_r,257._pm_r,1._pm_r,128._pm_r,.43_pm_r,335._pm_r, &
       228.10_pm_r,27275._pm_r,4._pm_r,229._pm_r,.44_pm_r,250._pm_r,0._pm_r,80._pm_r,.46_pm_r,334._pm_r, &
       235.60_pm_r,30671._pm_r,5._pm_r,231._pm_r,.36_pm_r,238._pm_r,1._pm_r,8._pm_r,.42_pm_r,333._pm_r, &
       245.10_pm_r,34186._pm_r,5._pm_r,231._pm_r,.28_pm_r,222._pm_r,1._pm_r,352._pm_r,.34_pm_r,330._pm_r, &
       255.50_pm_r,37854._pm_r,6._pm_r,230._pm_r,.24_pm_r,204._pm_r,2._pm_r,346._pm_r,.25_pm_r,327._pm_r, &
       266.00_pm_r,41670._pm_r,6._pm_r,228._pm_r,.20_pm_r,195._pm_r,2._pm_r,342._pm_r,.16_pm_r,317._pm_r, &
       272.20_pm_r,45621._pm_r,6._pm_r,228._pm_r,.43_pm_r,44._pm_r,2._pm_r,332._pm_r,.58_pm_r,277._pm_r, &
       269.80_pm_r,49598._pm_r,5._pm_r,230._pm_r,1.47_pm_r,41._pm_r,3._pm_r,309._pm_r,1.42_pm_r,274._pm_r, &
       262.40_pm_r,53496._pm_r,2._pm_r,239._pm_r,1.40_pm_r,39._pm_r,5._pm_r,295._pm_r,1.46_pm_r,274._pm_r, &
       251.80_pm_r,57266._pm_r,1._pm_r,263._pm_r,.15_pm_r,332._pm_r,7._pm_r,290._pm_r,.51_pm_r,274._pm_r, &
       240.50_pm_r,60868._pm_r,2._pm_r,251._pm_r,1.19_pm_r,230._pm_r,7._pm_r,290._pm_r,.41_pm_r,93._pm_r, &
       229.10_pm_r,64310._pm_r,4._pm_r,241._pm_r,1.33_pm_r,230._pm_r,6._pm_r,293._pm_r,.53_pm_r,89._pm_r, &
       217.90_pm_r,67580._pm_r,6._pm_r,239._pm_r,.56_pm_r,239._pm_r,6._pm_r,295._pm_r,.11_pm_r,350._pm_r, &
       208.10_pm_r,70701._pm_r,6._pm_r,241._pm_r,.71_pm_r,31._pm_r,6._pm_r,295._pm_r,.94_pm_r,284._pm_r, &
       199.80_pm_r,73685._pm_r,4._pm_r,254._pm_r,2.17_pm_r,41._pm_r,8._pm_r,291._pm_r,2.04_pm_r,280._pm_r, &
       191.90_pm_r,76556._pm_r,2._pm_r,343._pm_r,3.77_pm_r,43._pm_r,12._pm_r,287._pm_r,3.26_pm_r,278._pm_r, &
       184.90_pm_r,79309._pm_r,8._pm_r,29._pm_r,4.70_pm_r,43._pm_r,18._pm_r,284._pm_r,3.99_pm_r,278._pm_r, &
       179.10_pm_r,81970._pm_r,14._pm_r,35._pm_r,4.20_pm_r,43._pm_r,23._pm_r,283._pm_r,3.53_pm_r,278._pm_r, &
       174.70_pm_r,84563._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       171.20_pm_r,87092._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       169.00_pm_r,89579._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       170.80_pm_r,92077._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       176.20_pm_r,94638._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.40_pm_r,97324._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.00_pm_r,100214._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.40_pm_r,103410._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.90_pm_r,107061._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       298.60_pm_r,111379._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       372.90_pm_r,116728._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
       283.20_pm_r,-13._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       266.20_pm_r,4006._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       240.60_pm_r,7712._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.70_pm_r,11100._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.00_pm_r,14338._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.60_pm_r,17563._pm_r,2._pm_r,264._pm_r,.41_pm_r,194._pm_r,2._pm_r,137._pm_r,.21_pm_r,5._pm_r, &
       222.00_pm_r,20803._pm_r,3._pm_r,249._pm_r,.54_pm_r,195._pm_r,1._pm_r,124._pm_r,.30_pm_r,3._pm_r, &
       224.10_pm_r,24066._pm_r,3._pm_r,237._pm_r,.61_pm_r,199._pm_r,1._pm_r,102._pm_r,.35_pm_r,0._pm_r, &
       229.30_pm_r,27381._pm_r,4._pm_r,229._pm_r,.59_pm_r,202._pm_r,1._pm_r,72._pm_r,.38_pm_r,356._pm_r, &
       237.50_pm_r,30799._pm_r,5._pm_r,225._pm_r,.48_pm_r,209._pm_r,1._pm_r,48._pm_r,.35_pm_r,350._pm_r, &
       248.10_pm_r,34349._pm_r,5._pm_r,224._pm_r,.33_pm_r,221._pm_r,2._pm_r,33._pm_r,.29_pm_r,342._pm_r, &
       259.70_pm_r,38072._pm_r,6._pm_r,224._pm_r,.21_pm_r,238._pm_r,2._pm_r,23._pm_r,.22_pm_r,331._pm_r, &
       270.00_pm_r,41950._pm_r,6._pm_r,225._pm_r,.14_pm_r,260._pm_r,2._pm_r,16._pm_r,.17_pm_r,317._pm_r, &
       275.10_pm_r,45951._pm_r,6._pm_r,226._pm_r,.15_pm_r,235._pm_r,2._pm_r,10._pm_r,.16_pm_r,297._pm_r, &
       273.60_pm_r,49975._pm_r,6._pm_r,226._pm_r,.27_pm_r,208._pm_r,2._pm_r,4._pm_r,.15_pm_r,292._pm_r, &
       267.00_pm_r,53936._pm_r,7._pm_r,224._pm_r,.38_pm_r,208._pm_r,2._pm_r,359._pm_r,.12_pm_r,316._pm_r, &
       256.20_pm_r,57774._pm_r,7._pm_r,224._pm_r,.41_pm_r,220._pm_r,2._pm_r,358._pm_r,.17_pm_r,353._pm_r, &
       244.00_pm_r,61434._pm_r,8._pm_r,224._pm_r,.29_pm_r,236._pm_r,3._pm_r,357._pm_r,.18_pm_r,352._pm_r, &
       231.50_pm_r,64919._pm_r,8._pm_r,224._pm_r,.10_pm_r,259._pm_r,3._pm_r,356._pm_r,.17_pm_r,330._pm_r, &
       219.40_pm_r,68217._pm_r,8._pm_r,225._pm_r,.10_pm_r,44._pm_r,3._pm_r,353._pm_r,.20_pm_r,292._pm_r, &
       208.20_pm_r,71349._pm_r,8._pm_r,224._pm_r,.28_pm_r,63._pm_r,3._pm_r,347._pm_r,.27_pm_r,270._pm_r, &
       197.50_pm_r,74317._pm_r,7._pm_r,223._pm_r,.39_pm_r,68._pm_r,3._pm_r,339._pm_r,.34_pm_r,259._pm_r, &
       186.50_pm_r,77131._pm_r,7._pm_r,221._pm_r,.45_pm_r,71._pm_r,3._pm_r,330._pm_r,.37_pm_r,254._pm_r, &
       176.10_pm_r,79781._pm_r,6._pm_r,218._pm_r,.45_pm_r,72._pm_r,4._pm_r,322._pm_r,.37_pm_r,251._pm_r, &
       167.70_pm_r,82288._pm_r,6._pm_r,214._pm_r,.41_pm_r,73._pm_r,4._pm_r,314._pm_r,.33_pm_r,251._pm_r, &
       162.40_pm_r,84699._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       159.90_pm_r,87049._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       159.30_pm_r,89381._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       163.00_pm_r,91749._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       170.90_pm_r,94212._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.50_pm_r,96840._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.30_pm_r,99729._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.10_pm_r,102997._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       263.70_pm_r,106792._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       304.40_pm_r,111246._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       376.70_pm_r,116655._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
       281.50_pm_r,-32._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.40_pm_r,3942._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       237.30_pm_r,7594._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.60_pm_r,10969._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.30_pm_r,14249._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.20_pm_r,17540._pm_r,1._pm_r,254._pm_r,.26_pm_r,230._pm_r,1._pm_r,84._pm_r,.04_pm_r,337._pm_r, &
       226.10_pm_r,20845._pm_r,2._pm_r,247._pm_r,.35_pm_r,231._pm_r,1._pm_r,81._pm_r,.06_pm_r,329._pm_r, &
       227.10_pm_r,24160._pm_r,2._pm_r,243._pm_r,.40_pm_r,230._pm_r,1._pm_r,77._pm_r,.08_pm_r,330._pm_r, &
       231.70_pm_r,27513._pm_r,3._pm_r,240._pm_r,.39_pm_r,228._pm_r,1._pm_r,70._pm_r,.10_pm_r,323._pm_r, &
       240.50_pm_r,30971._pm_r,3._pm_r,238._pm_r,.32_pm_r,227._pm_r,1._pm_r,62._pm_r,.11_pm_r,322._pm_r, &
       251.50_pm_r,34569._pm_r,4._pm_r,237._pm_r,.23_pm_r,223._pm_r,1._pm_r,54._pm_r,.12_pm_r,318._pm_r, &
       263.40_pm_r,38344._pm_r,4._pm_r,235._pm_r,.15_pm_r,215._pm_r,1._pm_r,44._pm_r,.12_pm_r,312._pm_r, &
       274.00_pm_r,42278._pm_r,4._pm_r,234._pm_r,.10_pm_r,204._pm_r,1._pm_r,35._pm_r,.11_pm_r,315._pm_r, &
       278.90_pm_r,46335._pm_r,4._pm_r,233._pm_r,.15_pm_r,200._pm_r,1._pm_r,27._pm_r,.18_pm_r,352._pm_r, &
       277.00_pm_r,50412._pm_r,4._pm_r,231._pm_r,.18_pm_r,209._pm_r,2._pm_r,22._pm_r,.24_pm_r,12._pm_r, &
       270.10_pm_r,54420._pm_r,5._pm_r,231._pm_r,.30_pm_r,251._pm_r,2._pm_r,22._pm_r,.05_pm_r,77._pm_r, &
       259.70_pm_r,58305._pm_r,5._pm_r,235._pm_r,.62_pm_r,273._pm_r,2._pm_r,27._pm_r,.44_pm_r,185._pm_r, &
       247.70_pm_r,62018._pm_r,6._pm_r,242._pm_r,.74_pm_r,279._pm_r,1._pm_r,49._pm_r,.67_pm_r,189._pm_r, &
       235.00_pm_r,65556._pm_r,7._pm_r,247._pm_r,.69_pm_r,285._pm_r,1._pm_r,145._pm_r,.64_pm_r,188._pm_r, &
       221.90_pm_r,68898._pm_r,8._pm_r,252._pm_r,.48_pm_r,294._pm_r,1._pm_r,165._pm_r,.37_pm_r,175._pm_r, &
       208.60_pm_r,72055._pm_r,8._pm_r,255._pm_r,.25_pm_r,320._pm_r,2._pm_r,164._pm_r,.16_pm_r,104._pm_r, &
       194.60_pm_r,75004._pm_r,8._pm_r,257._pm_r,.18_pm_r,24._pm_r,2._pm_r,151._pm_r,.37_pm_r,45._pm_r, &
       180.20_pm_r,77753._pm_r,8._pm_r,258._pm_r,.28_pm_r,63._pm_r,2._pm_r,126._pm_r,.54_pm_r,34._pm_r, &
       167.30_pm_r,80288._pm_r,7._pm_r,258._pm_r,.36_pm_r,72._pm_r,2._pm_r,95._pm_r,.60_pm_r,30._pm_r, &
       156.90_pm_r,82650._pm_r,7._pm_r,259._pm_r,.35_pm_r,75._pm_r,2._pm_r,73._pm_r,.58_pm_r,28._pm_r, &
       150.70_pm_r,84880._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       149.00_pm_r,87057._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       150.20_pm_r,89241._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       155.60_pm_r,91485._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       165.80_pm_r,93854._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.50_pm_r,96426._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.50_pm_r,99313._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       237.20_pm_r,102657._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.40_pm_r,106601._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       308.10_pm_r,111171._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       379.60_pm_r,116614._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
       276.40_pm_r,-30._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       259.50_pm_r,3883._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       234.90_pm_r,7493._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.30_pm_r,10868._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.80_pm_r,14191._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.80_pm_r,17533._pm_r,1._pm_r,172._pm_r,.17_pm_r,287._pm_r,1._pm_r,53._pm_r,.09_pm_r,215._pm_r, &
       229.90_pm_r,20892._pm_r,1._pm_r,203._pm_r,.23_pm_r,280._pm_r,1._pm_r,56._pm_r,.14_pm_r,216._pm_r, &
       229.90_pm_r,24254._pm_r,1._pm_r,230._pm_r,.27_pm_r,267._pm_r,1._pm_r,60._pm_r,.17_pm_r,219._pm_r, &
       233.90_pm_r,27643._pm_r,1._pm_r,241._pm_r,.28_pm_r,252._pm_r,1._pm_r,68._pm_r,.19_pm_r,221._pm_r, &
       243.00_pm_r,31135._pm_r,2._pm_r,241._pm_r,.28_pm_r,232._pm_r,1._pm_r,82._pm_r,.19_pm_r,223._pm_r, &
       254.70_pm_r,34774._pm_r,2._pm_r,237._pm_r,.28_pm_r,213._pm_r,0._pm_r,111._pm_r,.16_pm_r,229._pm_r, &
       267.00_pm_r,38598._pm_r,2._pm_r,231._pm_r,.27_pm_r,198._pm_r,0._pm_r,152._pm_r,.12_pm_r,238._pm_r, &
       277.60_pm_r,42586._pm_r,3._pm_r,226._pm_r,.25_pm_r,187._pm_r,0._pm_r,181._pm_r,.08_pm_r,253._pm_r, &
       282.60_pm_r,46695._pm_r,3._pm_r,223._pm_r,.22_pm_r,215._pm_r,0._pm_r,193._pm_r,.03_pm_r,232._pm_r, &
       281.30_pm_r,50832._pm_r,3._pm_r,224._pm_r,.28_pm_r,246._pm_r,0._pm_r,193._pm_r,.02_pm_r,90._pm_r, &
       274.10_pm_r,54901._pm_r,4._pm_r,227._pm_r,.28_pm_r,251._pm_r,0._pm_r,194._pm_r,.08_pm_r,349._pm_r, &
       263.30_pm_r,58842._pm_r,4._pm_r,229._pm_r,.22_pm_r,231._pm_r,0._pm_r,233._pm_r,.20_pm_r,328._pm_r, &
       249.70_pm_r,62597._pm_r,4._pm_r,228._pm_r,.20_pm_r,202._pm_r,0._pm_r,289._pm_r,.19_pm_r,318._pm_r, &
       235.90_pm_r,66156._pm_r,5._pm_r,226._pm_r,.23_pm_r,203._pm_r,1._pm_r,296._pm_r,.07_pm_r,286._pm_r, &
       222.30_pm_r,69508._pm_r,5._pm_r,225._pm_r,.31_pm_r,227._pm_r,1._pm_r,286._pm_r,.14_pm_r,163._pm_r, &
       207.40_pm_r,72660._pm_r,5._pm_r,226._pm_r,.42_pm_r,241._pm_r,0._pm_r,250._pm_r,.32_pm_r,150._pm_r, &
       191.90_pm_r,75580._pm_r,6._pm_r,228._pm_r,.51_pm_r,250._pm_r,1._pm_r,187._pm_r,.43_pm_r,145._pm_r, &
       176.40_pm_r,78281._pm_r,7._pm_r,231._pm_r,.53_pm_r,253._pm_r,1._pm_r,165._pm_r,.48_pm_r,142._pm_r, &
       163.00_pm_r,80756._pm_r,8._pm_r,233._pm_r,.49_pm_r,256._pm_r,2._pm_r,156._pm_r,.47_pm_r,141._pm_r, &
       150.80_pm_r,83049._pm_r,8._pm_r,235._pm_r,.43_pm_r,258._pm_r,3._pm_r,152._pm_r,.41_pm_r,141._pm_r, &
       142.20_pm_r,85150._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       140.20_pm_r,87187._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       142.70_pm_r,89251._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       149.50_pm_r,91395._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       161.30_pm_r,93685._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.60_pm_r,96207._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.50_pm_r,99090._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       244.70_pm_r,102505._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       283.80_pm_r,106584._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       309.90_pm_r,111244._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       381.70_pm_r,116701._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/) 
  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
       271.30_pm_r,-14._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       256.70_pm_r,3839._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       233.10_pm_r,7414._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.80_pm_r,10793._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.70_pm_r,14154._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       231.40_pm_r,17536._pm_r,0._pm_r,99._pm_r,.07_pm_r,155._pm_r,1._pm_r,38._pm_r,.07_pm_r,200._pm_r, &
       232.40_pm_r,20933._pm_r,0._pm_r,144._pm_r,.09_pm_r,180._pm_r,1._pm_r,41._pm_r,.11_pm_r,202._pm_r, &
       232.60_pm_r,24335._pm_r,0._pm_r,173._pm_r,.14_pm_r,213._pm_r,1._pm_r,48._pm_r,.13_pm_r,202._pm_r, &
       236.00_pm_r,27761._pm_r,1._pm_r,198._pm_r,.22_pm_r,236._pm_r,0._pm_r,63._pm_r,.15_pm_r,200._pm_r, &
       244.40_pm_r,31280._pm_r,1._pm_r,218._pm_r,.30_pm_r,248._pm_r,0._pm_r,98._pm_r,.14_pm_r,200._pm_r, &
       255.80_pm_r,34936._pm_r,1._pm_r,231._pm_r,.34_pm_r,255._pm_r,0._pm_r,140._pm_r,.13_pm_r,203._pm_r, &
       268.60_pm_r,38780._pm_r,2._pm_r,238._pm_r,.35_pm_r,258._pm_r,0._pm_r,163._pm_r,.10_pm_r,203._pm_r, &
       279.80_pm_r,42795._pm_r,2._pm_r,242._pm_r,.30_pm_r,258._pm_r,1._pm_r,173._pm_r,.08_pm_r,203._pm_r, &
       285.70_pm_r,46944._pm_r,3._pm_r,244._pm_r,.19_pm_r,233._pm_r,1._pm_r,175._pm_r,.03_pm_r,112._pm_r, &
       285.00_pm_r,51130._pm_r,3._pm_r,239._pm_r,.26_pm_r,171._pm_r,1._pm_r,162._pm_r,.16_pm_r,72._pm_r, &
       277.40_pm_r,55251._pm_r,3._pm_r,230._pm_r,.38_pm_r,152._pm_r,1._pm_r,129._pm_r,.30_pm_r,61._pm_r, &
       265.80_pm_r,59235._pm_r,3._pm_r,219._pm_r,.34_pm_r,148._pm_r,1._pm_r,98._pm_r,.36_pm_r,52._pm_r, &
       251.00_pm_r,63017._pm_r,3._pm_r,214._pm_r,.13_pm_r,162._pm_r,1._pm_r,82._pm_r,.24_pm_r,44._pm_r, &
       236.30_pm_r,66589._pm_r,3._pm_r,213._pm_r,.11_pm_r,236._pm_r,1._pm_r,76._pm_r,.07_pm_r,56._pm_r, &
       222.20_pm_r,69943._pm_r,3._pm_r,215._pm_r,.17_pm_r,248._pm_r,1._pm_r,79._pm_r,.13_pm_r,189._pm_r, &
       206.40_pm_r,73089._pm_r,4._pm_r,217._pm_r,.24_pm_r,235._pm_r,1._pm_r,90._pm_r,.27_pm_r,193._pm_r, &
       189.90_pm_r,75987._pm_r,4._pm_r,218._pm_r,.27_pm_r,227._pm_r,1._pm_r,110._pm_r,.35_pm_r,190._pm_r, &
       174.00_pm_r,78656._pm_r,4._pm_r,219._pm_r,.28_pm_r,218._pm_r,2._pm_r,130._pm_r,.37_pm_r,189._pm_r, &
       160.20_pm_r,81091._pm_r,5._pm_r,218._pm_r,.25_pm_r,213._pm_r,2._pm_r,144._pm_r,.36_pm_r,188._pm_r, &
       147.00_pm_r,83341._pm_r,5._pm_r,218._pm_r,.21_pm_r,211._pm_r,2._pm_r,153._pm_r,.30_pm_r,189._pm_r, &
       136.70_pm_r,85360._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       134.50_pm_r,87307._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       137.90_pm_r,89293._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       145.50_pm_r,91371._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       158.20_pm_r,93607._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       178.20_pm_r,96095._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.90_pm_r,98973._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       250.20_pm_r,102438._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       290.40_pm_r,106617._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       310.40_pm_r,111332._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       382.90_pm_r,116793._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_juin = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_juin.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_juin,(/10,pm_nb_alt,pm_nb_lat/))
  retour = pm_OK

  tbar(:,:) = donnees(1,:,:)
  zbar(:,:) = donnees(2,:,:)
  z1(:,:)   = donnees(3,:,:)
  phi1(:,:) = donnees(4,:,:)
  t1(:,:)   = donnees(5,:,:)
  phit1(:,:)= donnees(6,:,:)
  z2(:,:)   = donnees(7,:,:)
  phi2(:,:) = donnees(8,:,:)
  t2(:,:)   = donnees(9,:,:)
  phit2(:,:)= donnees(10,:,:)

end subroutine mpi_atmi_juin
