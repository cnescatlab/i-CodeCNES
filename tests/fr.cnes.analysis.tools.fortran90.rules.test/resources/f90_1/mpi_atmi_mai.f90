subroutine mpi_atmi_mai (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de MAI 
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
       268.90_pm_r,  -187._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       241.20_pm_r,  3536._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.40_pm_r,  6898._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       211.70_pm_r, 10044._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       208.90_pm_r, 13114._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       204.90_pm_r, 16144._pm_r,   6._pm_r,195._pm_r, 1.24_pm_r,188._pm_r,   3._pm_r,146._pm_r,  .20_pm_r,317._pm_r, &
       200.10_pm_r, 19111._pm_r,   8._pm_r,192._pm_r, 1.78_pm_r,183._pm_r,   3._pm_r,147._pm_r,  .31_pm_r,315._pm_r, &
       194.30_pm_r, 22000._pm_r,  11._pm_r,188._pm_r, 2.06_pm_r,173._pm_r,   3._pm_r,150._pm_r,  .40_pm_r,311._pm_r, &
       187.40_pm_r, 24792._pm_r,  14._pm_r,184._pm_r, 1.76_pm_r,153._pm_r,   2._pm_r,156._pm_r,  .39_pm_r,308._pm_r, &
       194.80_pm_r, 27573._pm_r,  15._pm_r,177._pm_r, 1.42_pm_r,114._pm_r,   2._pm_r,166._pm_r,  .30_pm_r,300._pm_r, &
       206.20_pm_r, 30512._pm_r,  16._pm_r,169._pm_r, 1.72_pm_r, 79._pm_r,   1._pm_r,178._pm_r,  .20_pm_r,288._pm_r, &
       216.40_pm_r, 33609._pm_r,  15._pm_r,159._pm_r, 2.09_pm_r, 62._pm_r,   1._pm_r,188._pm_r,  .14_pm_r,266._pm_r, &
       227.00_pm_r, 36852._pm_r,  15._pm_r,147._pm_r, 2.13_pm_r, 54._pm_r,   1._pm_r,194._pm_r,  .11_pm_r,243._pm_r, &
       238.00_pm_r, 40259._pm_r,  15._pm_r,137._pm_r, 1.73_pm_r, 54._pm_r,   2._pm_r,199._pm_r,  .15_pm_r,240._pm_r, &
       250.40_pm_r, 43830._pm_r,  16._pm_r,128._pm_r, 1.52_pm_r, 58._pm_r,   2._pm_r,204._pm_r,  .19_pm_r,231._pm_r, &
       259.40_pm_r, 47576._pm_r,  17._pm_r,121._pm_r, 1.62_pm_r, 57._pm_r,   2._pm_r,207._pm_r,  .19_pm_r,215._pm_r, &
       260.10_pm_r, 51385._pm_r,  18._pm_r,113._pm_r, 1.89_pm_r, 52._pm_r,   2._pm_r,206._pm_r,  .17_pm_r,187._pm_r, &
       257.00_pm_r, 55173._pm_r,  19._pm_r,106._pm_r, 1.59_pm_r, 44._pm_r,   3._pm_r,203._pm_r,  .12_pm_r,137._pm_r, &
       251.20_pm_r, 58901._pm_r,  20._pm_r,101._pm_r, 1.14_pm_r, 31._pm_r,   3._pm_r,199._pm_r,  .13_pm_r, 94._pm_r, &
       243.90_pm_r, 62524._pm_r,  20._pm_r, 97._pm_r,  .66_pm_r,  1._pm_r,   2._pm_r,195._pm_r,  .14_pm_r, 67._pm_r, &
       238.50_pm_r, 66056._pm_r,  20._pm_r, 95._pm_r,  .64_pm_r,298._pm_r,   2._pm_r,191._pm_r,  .15_pm_r, 48._pm_r, &
       234.90_pm_r, 69519._pm_r,  18._pm_r, 95._pm_r, 1.12_pm_r,267._pm_r,   2._pm_r,189._pm_r,  .17_pm_r, 27._pm_r, &
       233.50_pm_r, 72948._pm_r,  17._pm_r, 97._pm_r, 1.62_pm_r,257._pm_r,   2._pm_r,187._pm_r,  .19_pm_r, 12._pm_r, &
       234.00_pm_r, 76372._pm_r,  14._pm_r,101._pm_r, 2.03_pm_r,253._pm_r,   2._pm_r,187._pm_r,  .21_pm_r,  1._pm_r, &
       231.80_pm_r, 79802._pm_r,  11._pm_r,109._pm_r, 2.23_pm_r,250._pm_r,   1._pm_r,190._pm_r,  .21_pm_r,357._pm_r, &
       225.10_pm_r, 83143._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       215.70_pm_r, 86370._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       204.70_pm_r, 89444._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       198.20_pm_r, 92395._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       196.10_pm_r, 95297._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       199.30_pm_r, 98225._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       207.10_pm_r,101264._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       220.70_pm_r,104501._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       240.30_pm_r,108043._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       269.40_pm_r,112023._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       335.10_pm_r,116790._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
       272.90_pm_r,   -96._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       245.20_pm_r,  3687._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       222.20_pm_r,  7100._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       214.20_pm_r, 10286._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       211.80_pm_r, 13397._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       208.50_pm_r, 16475._pm_r,   9._pm_r,205._pm_r, 2.14_pm_r,155._pm_r,   5._pm_r,150._pm_r,  .19_pm_r,348._pm_r, &
       204.90_pm_r, 19502._pm_r,  12._pm_r,190._pm_r, 3.14_pm_r,151._pm_r,   5._pm_r,149._pm_r,  .34_pm_r,342._pm_r, &
       201.10_pm_r, 22475._pm_r,  16._pm_r,178._pm_r, 3.76_pm_r,144._pm_r,   4._pm_r,147._pm_r,  .54_pm_r,337._pm_r, &
       195.90_pm_r, 25382._pm_r,  21._pm_r,168._pm_r, 3.49_pm_r,133._pm_r,   3._pm_r,145._pm_r,  .73_pm_r,332._pm_r, &
       198.60_pm_r, 28260._pm_r,  24._pm_r,160._pm_r, 2.66_pm_r,108._pm_r,   2._pm_r,142._pm_r,  .82_pm_r,328._pm_r, &
       206.30_pm_r, 31222._pm_r,  25._pm_r,153._pm_r, 2.38_pm_r, 70._pm_r,   1._pm_r,135._pm_r,  .83_pm_r,323._pm_r, &
       215.70_pm_r, 34313._pm_r,  25._pm_r,145._pm_r, 2.74_pm_r, 40._pm_r,   0._pm_r,330._pm_r,  .72_pm_r,319._pm_r, &
       226.80_pm_r, 37548._pm_r,  24._pm_r,135._pm_r, 3.02_pm_r, 23._pm_r,   1._pm_r,321._pm_r,  .55_pm_r,315._pm_r, &
       238.60_pm_r, 40959._pm_r,  22._pm_r,125._pm_r, 2.65_pm_r, 22._pm_r,   2._pm_r,320._pm_r,  .27_pm_r,319._pm_r, &
       250.80_pm_r, 44539._pm_r,  22._pm_r,116._pm_r, 2.43_pm_r, 25._pm_r,   2._pm_r,320._pm_r,  .11_pm_r,331._pm_r, &
       258.80_pm_r, 48282._pm_r,  22._pm_r,106._pm_r, 2.58_pm_r, 25._pm_r,   2._pm_r,321._pm_r,  .05_pm_r,311._pm_r, &
       258.80_pm_r, 52078._pm_r,  23._pm_r, 96._pm_r, 2.88_pm_r, 22._pm_r,   2._pm_r,319._pm_r,  .08_pm_r,245._pm_r, &
       255.80_pm_r, 55849._pm_r,  24._pm_r, 87._pm_r, 2.49_pm_r, 12._pm_r,   2._pm_r,315._pm_r,  .13_pm_r,221._pm_r, &
       250.50_pm_r, 59561._pm_r,  25._pm_r, 79._pm_r, 1.91_pm_r,359._pm_r,   2._pm_r,311._pm_r,  .14_pm_r,209._pm_r, &
       244.20_pm_r, 63181._pm_r,  25._pm_r, 74._pm_r, 1.35_pm_r,333._pm_r,   2._pm_r,306._pm_r,  .11_pm_r,199._pm_r, &
       238.60_pm_r, 66717._pm_r,  24._pm_r, 70._pm_r, 1.24_pm_r,292._pm_r,   2._pm_r,303._pm_r,  .06_pm_r,180._pm_r, &
       234.30_pm_r, 70177._pm_r,  22._pm_r, 68._pm_r, 1.66_pm_r,261._pm_r,   2._pm_r,302._pm_r,  .04_pm_r,104._pm_r, &
       231.40_pm_r, 73587._pm_r,  19._pm_r, 68._pm_r, 2.23_pm_r,247._pm_r,   2._pm_r,304._pm_r,  .08_pm_r, 60._pm_r, &
       230.00_pm_r, 76965._pm_r,  16._pm_r, 69._pm_r, 2.68_pm_r,241._pm_r,   2._pm_r,307._pm_r,  .13_pm_r, 49._pm_r, &
       227.10_pm_r, 80323._pm_r,  12._pm_r, 72._pm_r, 2.90_pm_r,237._pm_r,   2._pm_r,313._pm_r,  .15_pm_r, 46._pm_r, &
       221.20_pm_r, 83601._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       212.60_pm_r, 86783._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       202.10_pm_r, 89818._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       196.10_pm_r, 92736._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       194.20_pm_r, 95610._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       197.10_pm_r, 98511._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       204.50_pm_r,101515._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       217.70_pm_r,104711._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       238.00_pm_r,108212._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       270.30_pm_r,112180._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       337.30_pm_r,116983._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
       
       276.90_pm_r,   -54._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       251.00_pm_r,  3804._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       227.00_pm_r,  7297._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       216.10_pm_r, 10534._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       214.80_pm_r, 13683._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       212.80_pm_r, 16816._pm_r,   9._pm_r,201._pm_r, 2.58_pm_r,137._pm_r,   3._pm_r,162._pm_r,  .32_pm_r, 39._pm_r, &
       210.40_pm_r, 19914._pm_r,  12._pm_r,181._pm_r, 3.71_pm_r,134._pm_r,   3._pm_r,153._pm_r,  .46_pm_r, 30._pm_r, &
       209.10_pm_r, 22988._pm_r,  17._pm_r,165._pm_r, 4.45_pm_r,130._pm_r,   3._pm_r,140._pm_r,  .58_pm_r, 15._pm_r, &
       205.40_pm_r, 26024._pm_r,  22._pm_r,154._pm_r, 4.27_pm_r,122._pm_r,   2._pm_r,122._pm_r,  .69_pm_r,353._pm_r, &
       204.10_pm_r, 29018._pm_r,  27._pm_r,147._pm_r, 3.32_pm_r,105._pm_r,   1._pm_r, 91._pm_r,  .83_pm_r,332._pm_r, &
       207.70_pm_r, 32028._pm_r,  29._pm_r,140._pm_r, 2.46_pm_r, 68._pm_r,   1._pm_r, 30._pm_r,  .95_pm_r,314._pm_r, &
       215.80_pm_r, 35127._pm_r,  29._pm_r,133._pm_r, 2.71_pm_r, 25._pm_r,   2._pm_r,344._pm_r,  .94_pm_r,302._pm_r, &
       226.80_pm_r, 38362._pm_r,  27._pm_r,125._pm_r, 3.39_pm_r,  2._pm_r,   3._pm_r,326._pm_r,  .79_pm_r,293._pm_r, &
       238.10_pm_r, 41771._pm_r,  25._pm_r,115._pm_r, 3.25_pm_r,  3._pm_r,   4._pm_r,318._pm_r,  .34_pm_r,283._pm_r, &
       248.40_pm_r, 45331._pm_r,  23._pm_r,104._pm_r, 3.05_pm_r,  6._pm_r,   4._pm_r,316._pm_r,  .07_pm_r,297._pm_r, &
       254.30_pm_r, 49021._pm_r,  23._pm_r, 93._pm_r, 3.06_pm_r,  3._pm_r,   4._pm_r,317._pm_r,  .12_pm_r,350._pm_r, &
       254.20_pm_r, 52750._pm_r,  23._pm_r, 82._pm_r, 3.24_pm_r,353._pm_r,   4._pm_r,318._pm_r,  .34_pm_r,323._pm_r, &
       251.90_pm_r, 56457._pm_r,  23._pm_r, 71._pm_r, 3.07_pm_r,337._pm_r,   5._pm_r,318._pm_r,  .45_pm_r,312._pm_r, &
       248.80_pm_r, 60125._pm_r,  23._pm_r, 60._pm_r, 2.71_pm_r,321._pm_r,   6._pm_r,317._pm_r,  .47_pm_r,312._pm_r, &
       244.70_pm_r, 63739._pm_r,  22._pm_r, 51._pm_r, 2.27_pm_r,302._pm_r,   6._pm_r,317._pm_r,  .41_pm_r,322._pm_r, &
       239.50_pm_r, 67286._pm_r,  20._pm_r, 43._pm_r, 1.98_pm_r,276._pm_r,   7._pm_r,318._pm_r,  .33_pm_r,342._pm_r, &
       234.80_pm_r, 70757._pm_r,  18._pm_r, 38._pm_r, 2.06_pm_r,250._pm_r,   7._pm_r,320._pm_r,  .30_pm_r, 13._pm_r, &
       230.20_pm_r, 74164._pm_r,  15._pm_r, 33._pm_r, 2.37_pm_r,232._pm_r,   7._pm_r,324._pm_r,  .35_pm_r, 39._pm_r, &
       225.70_pm_r, 77496._pm_r,  12._pm_r, 29._pm_r, 2.66_pm_r,221._pm_r,   7._pm_r,328._pm_r,  .42_pm_r, 54._pm_r, &
       221.60_pm_r, 80770._pm_r,   8._pm_r, 25._pm_r, 2.79_pm_r,214._pm_r,   7._pm_r,333._pm_r,  .47_pm_r, 62._pm_r, &
       216.20_pm_r, 83977._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       208.20_pm_r, 87098._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       198.50_pm_r, 90075._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       193.10_pm_r, 92947._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       191.70_pm_r, 95784._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       194.20_pm_r, 98645._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.80_pm_r,101603._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       213.10_pm_r,104736._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       234.20_pm_r,108173._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       270.90_pm_r,112115._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       340.40_pm_r,116964._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
       280.90_pm_r,   -52._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       257.70_pm_r,  3887._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       232.70_pm_r,  7473._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       217.30_pm_r, 10764._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       216.10_pm_r, 13934._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       215.70_pm_r, 17098._pm_r,   5._pm_r,207._pm_r, 1.89_pm_r,126._pm_r,   1._pm_r,145._pm_r,  .37_pm_r, 54._pm_r, &
       214.90_pm_r, 20250._pm_r,   6._pm_r,176._pm_r, 2.60_pm_r,123._pm_r,   1._pm_r,107._pm_r,  .47_pm_r, 43._pm_r, &
       215.20_pm_r, 23401._pm_r,   9._pm_r,154._pm_r, 2.98_pm_r,117._pm_r,   1._pm_r, 78._pm_r,  .49_pm_r, 21._pm_r, &
       213.60_pm_r, 26542._pm_r,  13._pm_r,141._pm_r, 2.90_pm_r,107._pm_r,   2._pm_r, 54._pm_r,  .54_pm_r,346._pm_r, &
       212.70_pm_r, 29662._pm_r,  16._pm_r,131._pm_r, 2.39_pm_r, 88._pm_r,   2._pm_r, 27._pm_r,  .75_pm_r,312._pm_r, &
       214.60_pm_r, 32785._pm_r,  18._pm_r,123._pm_r, 2.00_pm_r, 52._pm_r,   2._pm_r,355._pm_r, 1.05_pm_r,291._pm_r, &
       221.40_pm_r, 35974._pm_r,  18._pm_r,113._pm_r, 2.22_pm_r, 14._pm_r,   3._pm_r,327._pm_r, 1.19_pm_r,276._pm_r, &
       232.10_pm_r, 39289._pm_r,  17._pm_r,102._pm_r, 2.59_pm_r,353._pm_r,   5._pm_r,308._pm_r, 1.11_pm_r,262._pm_r, &
       242.60_pm_r, 42770._pm_r,  16._pm_r, 89._pm_r, 2.50_pm_r,354._pm_r,   5._pm_r,297._pm_r,  .65_pm_r,241._pm_r, &
       249.80_pm_r, 46378._pm_r,  16._pm_r, 77._pm_r, 2.38_pm_r,353._pm_r,   6._pm_r,291._pm_r,  .33_pm_r,211._pm_r, &
       252.10_pm_r, 50060._pm_r,  17._pm_r, 65._pm_r, 2.42_pm_r,341._pm_r,   6._pm_r,288._pm_r,  .09_pm_r,173._pm_r, &
       249.80_pm_r, 53740._pm_r,  17._pm_r, 52._pm_r, 2.71_pm_r,323._pm_r,   6._pm_r,288._pm_r,  .19_pm_r,357._pm_r, &
       246.20_pm_r, 57373._pm_r,  17._pm_r, 38._pm_r, 2.74_pm_r,307._pm_r,   6._pm_r,291._pm_r,  .21_pm_r,350._pm_r, &
       241.60_pm_r, 60947._pm_r,  17._pm_r, 25._pm_r, 2.50_pm_r,292._pm_r,   6._pm_r,293._pm_r,  .19_pm_r,348._pm_r, &
       237.20_pm_r, 64454._pm_r,  16._pm_r, 13._pm_r, 2.09_pm_r,275._pm_r,   6._pm_r,295._pm_r,  .16_pm_r,346._pm_r, &
       232.70_pm_r, 67897._pm_r,  16._pm_r,  4._pm_r, 1.74_pm_r,253._pm_r,   6._pm_r,297._pm_r,  .12_pm_r,346._pm_r, &
       228.20_pm_r, 71270._pm_r,  15._pm_r,356._pm_r, 1.65_pm_r,227._pm_r,   6._pm_r,298._pm_r,  .08_pm_r,358._pm_r, &
       224.40_pm_r, 74585._pm_r,  13._pm_r,348._pm_r, 1.80_pm_r,206._pm_r,   6._pm_r,299._pm_r,  .06_pm_r, 27._pm_r, &
       221.20_pm_r, 77847._pm_r,  10._pm_r,341._pm_r, 1.98_pm_r,194._pm_r,   6._pm_r,300._pm_r,  .07_pm_r, 59._pm_r, &
       217.50_pm_r, 81064._pm_r,   8._pm_r,330._pm_r, 2.06_pm_r,186._pm_r,   6._pm_r,300._pm_r,  .08_pm_r, 67._pm_r, &
       211.50_pm_r, 84211._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       203.10_pm_r, 87261._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       194.40_pm_r, 90169._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.10_pm_r, 92992._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       189.20_pm_r, 95790._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       191.10_pm_r, 98613._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       196.80_pm_r,101521._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       207.70_pm_r,104587._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       229.20_pm_r,107944._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       270.50_pm_r,111841._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       343.70_pm_r,116732._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
       287.60_pm_r,   -40._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       264.20_pm_r,  4000._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       238.20_pm_r,  7677._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       218.50_pm_r, 11021._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       214.20_pm_r, 14188._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       214.00_pm_r, 17321._pm_r,   2._pm_r,213._pm_r,  .99_pm_r,112._pm_r,   1._pm_r, 97._pm_r,  .31_pm_r, 14._pm_r, &
       215.40_pm_r, 20463._pm_r,   2._pm_r,163._pm_r, 1.25_pm_r,105._pm_r,   1._pm_r, 70._pm_r,  .40_pm_r,  1._pm_r, &
       218.70_pm_r, 23645._pm_r,   3._pm_r,133._pm_r, 1.31_pm_r, 93._pm_r,   1._pm_r, 45._pm_r,  .44_pm_r,341._pm_r, &
       219.70_pm_r, 26856._pm_r,   5._pm_r,116._pm_r, 1.21_pm_r, 72._pm_r,   2._pm_r, 22._pm_r,  .50_pm_r,312._pm_r, &
       221.10_pm_r, 30082._pm_r,   6._pm_r,101._pm_r, 1.20_pm_r, 38._pm_r,   2._pm_r,356._pm_r,  .66_pm_r,281._pm_r, &
       225.60_pm_r, 33349._pm_r,   6._pm_r, 85._pm_r, 1.44_pm_r,  8._pm_r,   2._pm_r,327._pm_r,  .90_pm_r,259._pm_r, &
       232.80_pm_r, 36705._pm_r,   7._pm_r, 65._pm_r, 1.68_pm_r,350._pm_r,   3._pm_r,298._pm_r, 1.09_pm_r,244._pm_r, &
       242.90_pm_r, 40181._pm_r,   8._pm_r, 47._pm_r, 1.67_pm_r,339._pm_r,   4._pm_r,277._pm_r, 1.17_pm_r,230._pm_r, &
       253.60_pm_r, 43823._pm_r,   9._pm_r, 34._pm_r, 1.41_pm_r,345._pm_r,   5._pm_r,264._pm_r,  .85_pm_r,217._pm_r, &
       257.00_pm_r, 47570._pm_r,  10._pm_r, 26._pm_r, 1.19_pm_r,340._pm_r,   6._pm_r,256._pm_r,  .51_pm_r,202._pm_r, &
       253.80_pm_r, 51313._pm_r,  11._pm_r, 19._pm_r, 1.10_pm_r,315._pm_r,   6._pm_r,251._pm_r,  .26_pm_r,170._pm_r, &
       247.20_pm_r, 54987._pm_r,  12._pm_r, 10._pm_r, 1.33_pm_r,283._pm_r,   6._pm_r,248._pm_r,  .27_pm_r,121._pm_r, &
       240.10_pm_r, 58553._pm_r,  12._pm_r,  1._pm_r, 1.41_pm_r,260._pm_r,   6._pm_r,244._pm_r,  .38_pm_r,130._pm_r, &
       234.50_pm_r, 62028._pm_r,  11._pm_r,350._pm_r, 1.41_pm_r,241._pm_r,   6._pm_r,238._pm_r,  .40_pm_r,134._pm_r, &
       229.30_pm_r, 65423._pm_r,  10._pm_r,340._pm_r, 1.36_pm_r,226._pm_r,   5._pm_r,232._pm_r,  .32_pm_r,134._pm_r, &
       224.40_pm_r, 68746._pm_r,   9._pm_r,330._pm_r, 1.29_pm_r,212._pm_r,   5._pm_r,228._pm_r,  .18_pm_r,128._pm_r, &
       220.40_pm_r, 72002._pm_r,   8._pm_r,319._pm_r, 1.26_pm_r,199._pm_r,   5._pm_r,227._pm_r,  .05_pm_r, 90._pm_r, &
       217.10_pm_r, 75206._pm_r,   8._pm_r,308._pm_r, 1.25_pm_r,189._pm_r,   5._pm_r,227._pm_r,  .09_pm_r,347._pm_r, &
       214.80_pm_r, 78366._pm_r,   7._pm_r,295._pm_r, 1.23_pm_r,183._pm_r,   5._pm_r,229._pm_r,  .18_pm_r,337._pm_r, &
       212.00_pm_r, 81497._pm_r,   6._pm_r,280._pm_r, 1.18_pm_r,178._pm_r,   5._pm_r,232._pm_r,  .21_pm_r,332._pm_r, &
       206.40_pm_r, 84570._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       198.30_pm_r, 87540._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.70_pm_r, 90382._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.70_pm_r, 93164._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.20_pm_r, 95934._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.60_pm_r, 98728._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       193.20_pm_r,101592._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       202.70_pm_r,104596._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       224.00_pm_r,107875._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       269.20_pm_r,111719._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       346.90_pm_r,116642._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
       293.50_pm_r,   -53._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       269.90_pm_r,  4075._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       244.30_pm_r,  7842._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       221.40_pm_r, 11254._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       209.50_pm_r, 14411._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       208.50_pm_r, 17459._pm_r,   2._pm_r,162._pm_r,  .20_pm_r, 68._pm_r,   2._pm_r, 80._pm_r,  .22_pm_r,312._pm_r, &
       213.30_pm_r, 20545._pm_r,   2._pm_r,154._pm_r,  .31_pm_r, 33._pm_r,   1._pm_r, 68._pm_r,  .33_pm_r,302._pm_r, &
       219.50_pm_r, 23718._pm_r,   2._pm_r,141._pm_r,  .54_pm_r,  6._pm_r,   1._pm_r, 46._pm_r,  .42_pm_r,290._pm_r, &
       223.50_pm_r, 26962._pm_r,   1._pm_r,110._pm_r,  .88_pm_r,352._pm_r,   1._pm_r,  8._pm_r,  .51_pm_r,276._pm_r, &
       227.50_pm_r, 30263._pm_r,   1._pm_r, 35._pm_r, 1.23_pm_r,345._pm_r,   1._pm_r,321._pm_r,  .61_pm_r,260._pm_r, &
       233.60_pm_r, 33636._pm_r,   3._pm_r,  4._pm_r, 1.45_pm_r,341._pm_r,   2._pm_r,290._pm_r,  .71_pm_r,245._pm_r, &
       242.30_pm_r, 37121._pm_r,   5._pm_r,354._pm_r, 1.41_pm_r,341._pm_r,   3._pm_r,270._pm_r,  .77_pm_r,231._pm_r, &
       252.70_pm_r, 40742._pm_r,   7._pm_r,351._pm_r, 1.14_pm_r,342._pm_r,   4._pm_r,257._pm_r,  .80_pm_r,219._pm_r, &
       260.80_pm_r, 44509._pm_r,   8._pm_r,351._pm_r,  .96_pm_r,  1._pm_r,   4._pm_r,247._pm_r,  .66_pm_r,210._pm_r, &
       262.40_pm_r, 48347._pm_r,  10._pm_r,353._pm_r,  .77_pm_r, 14._pm_r,   5._pm_r,241._pm_r,  .53_pm_r,203._pm_r, &
       257.30_pm_r, 52157._pm_r,  10._pm_r,355._pm_r,  .14_pm_r, 18._pm_r,   6._pm_r,236._pm_r,  .36_pm_r,182._pm_r, &
       248.40_pm_r, 55864._pm_r,  10._pm_r,354._pm_r,  .88_pm_r,201._pm_r,   6._pm_r,231._pm_r,  .35_pm_r,121._pm_r, &
       240.00_pm_r, 59437._pm_r,   8._pm_r,348._pm_r, 1.61_pm_r,195._pm_r,   5._pm_r,225._pm_r,  .48_pm_r,103._pm_r, &
       232.40_pm_r, 62899._pm_r,   6._pm_r,337._pm_r, 1.85_pm_r,193._pm_r,   5._pm_r,219._pm_r,  .48_pm_r, 92._pm_r, &
       223.70_pm_r, 66237._pm_r,   4._pm_r,316._pm_r, 1.63_pm_r,190._pm_r,   5._pm_r,213._pm_r,  .33_pm_r, 76._pm_r, &
       216.40_pm_r, 69458._pm_r,   3._pm_r,285._pm_r, 1.21_pm_r,188._pm_r,   4._pm_r,211._pm_r,  .20_pm_r, 32._pm_r, &
       212.70_pm_r, 72596._pm_r,   3._pm_r,260._pm_r,  .77_pm_r,183._pm_r,   4._pm_r,213._pm_r,  .25_pm_r,336._pm_r, &
       209.90_pm_r, 75691._pm_r,   4._pm_r,246._pm_r,  .41_pm_r,171._pm_r,   4._pm_r,220._pm_r,  .39_pm_r,314._pm_r, &
       208.30_pm_r, 78750._pm_r,   4._pm_r,240._pm_r,  .20_pm_r,136._pm_r,   4._pm_r,230._pm_r,  .49_pm_r,307._pm_r, &
       206.40_pm_r, 81790._pm_r,   3._pm_r,237._pm_r,  .18_pm_r, 79._pm_r,   4._pm_r,240._pm_r,  .54_pm_r,305._pm_r, &
       201.80_pm_r, 84790._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       194.40_pm_r, 87693._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.10_pm_r, 90489._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.50_pm_r, 93246._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.30_pm_r, 96003._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.20_pm_r, 98783._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.60_pm_r,101620._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       199.20_pm_r,104580._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.80_pm_r,107799._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       267.80_pm_r,111598._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       349.70_pm_r,116545._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
       298.00_pm_r,   -68._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       274.70_pm_r,  4131._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       250.10_pm_r,  7979._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       224.40_pm_r, 11458._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       204.20_pm_r, 14600._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       202.40_pm_r, 17552._pm_r,   2._pm_r,132._pm_r,  .29_pm_r,266._pm_r,   1._pm_r, 67._pm_r,  .07_pm_r,295._pm_r, &
       211.10_pm_r, 20578._pm_r,   2._pm_r,143._pm_r,  .46_pm_r,288._pm_r,   1._pm_r, 62._pm_r,  .11_pm_r,270._pm_r, &
       219.20_pm_r, 23734._pm_r,   1._pm_r,164._pm_r,  .68_pm_r,306._pm_r,   1._pm_r, 57._pm_r,  .17_pm_r,254._pm_r, &
       225.20_pm_r, 26988._pm_r,   1._pm_r,259._pm_r,  .96_pm_r,320._pm_r,   1._pm_r, 51._pm_r,  .24_pm_r,240._pm_r, &
       230.90_pm_r, 30327._pm_r,   2._pm_r,308._pm_r, 1.22_pm_r,329._pm_r,   0._pm_r, 20._pm_r,  .32_pm_r,232._pm_r, &
       239.70_pm_r, 33768._pm_r,   4._pm_r,320._pm_r, 1.32_pm_r,336._pm_r,   1._pm_r,232._pm_r,  .38_pm_r,225._pm_r, &
       249.40_pm_r, 37353._pm_r,   6._pm_r,327._pm_r, 1.24_pm_r,343._pm_r,   1._pm_r,226._pm_r,  .41_pm_r,219._pm_r, &
       259.00_pm_r, 41073._pm_r,   7._pm_r,331._pm_r,  .97_pm_r,351._pm_r,   2._pm_r,223._pm_r,  .39_pm_r,212._pm_r, &
       265.60_pm_r, 44923._pm_r,   8._pm_r,335._pm_r,  .71_pm_r,  8._pm_r,   2._pm_r,220._pm_r,  .32_pm_r,215._pm_r, &
       265.60_pm_r, 48820._pm_r,   9._pm_r,339._pm_r,  .35_pm_r, 46._pm_r,   3._pm_r,220._pm_r,  .31_pm_r,218._pm_r, &
       260.20_pm_r, 52672._pm_r,   9._pm_r,341._pm_r,  .42_pm_r,150._pm_r,   3._pm_r,218._pm_r,  .30_pm_r,202._pm_r, &
       250.60_pm_r, 56419._pm_r,   8._pm_r,340._pm_r,  .94_pm_r,178._pm_r,   3._pm_r,214._pm_r,  .35_pm_r,167._pm_r, &
       239.60_pm_r, 60006._pm_r,   6._pm_r,334._pm_r, 1.20_pm_r,180._pm_r,   4._pm_r,207._pm_r,  .39_pm_r,139._pm_r, &
       228.30_pm_r, 63435._pm_r,   5._pm_r,325._pm_r, 1.19_pm_r,178._pm_r,   4._pm_r,198._pm_r,  .36_pm_r,124._pm_r, &
       216.90_pm_r, 66691._pm_r,   3._pm_r,311._pm_r, 1.00_pm_r,171._pm_r,   4._pm_r,192._pm_r,  .24_pm_r,112._pm_r, &
       209.40_pm_r, 69809._pm_r,   2._pm_r,292._pm_r,  .80_pm_r,156._pm_r,   4._pm_r,189._pm_r,  .07_pm_r, 74._pm_r, &
       206.80_pm_r, 72853._pm_r,   2._pm_r,270._pm_r,  .67_pm_r,137._pm_r,   4._pm_r,189._pm_r,  .14_pm_r,327._pm_r, &
       205.50_pm_r, 75872._pm_r,   1._pm_r,234._pm_r,  .64_pm_r,118._pm_r,   4._pm_r,193._pm_r,  .28_pm_r,311._pm_r, &
       205.40_pm_r, 78878._pm_r,   1._pm_r,175._pm_r,  .66_pm_r,104._pm_r,   4._pm_r,200._pm_r,  .38_pm_r,307._pm_r, &
       204.00_pm_r, 81886._pm_r,   2._pm_r,136._pm_r,  .65_pm_r, 96._pm_r,   3._pm_r,210._pm_r,  .42_pm_r,305._pm_r, &
       198.80_pm_r, 84843._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       191.90_pm_r, 87700._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.10_pm_r, 90473._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.40_pm_r, 93224._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.30_pm_r, 95983._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.90_pm_r, 98763._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       189.50_pm_r,101590._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       197.60_pm_r,104531._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       217.70_pm_r,107721._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       267.40_pm_r,111500._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       352.60_pm_r,116474._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
       300.80_pm_r,   -91._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       276.70_pm_r,  4145._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       253.30_pm_r,  8032._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       225.80_pm_r, 11546._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       201.00_pm_r, 14677._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       198.90_pm_r, 17572._pm_r,   2._pm_r,123._pm_r,  .22_pm_r,282._pm_r,   0._pm_r,298._pm_r,  .08_pm_r, 88._pm_r, &
       209.60_pm_r, 20562._pm_r,   1._pm_r,128._pm_r,  .35_pm_r,290._pm_r,   0._pm_r,357._pm_r,  .10_pm_r, 99._pm_r, &
       218.60_pm_r, 23702._pm_r,   1._pm_r,138._pm_r,  .45_pm_r,299._pm_r,   0._pm_r, 81._pm_r,  .10_pm_r,117._pm_r, &
       225.60_pm_r, 26953._pm_r,   0._pm_r,212._pm_r,  .55_pm_r,307._pm_r,   0._pm_r,105._pm_r,  .10_pm_r,141._pm_r, &
       233.20_pm_r, 30311._pm_r,   1._pm_r,297._pm_r,  .61_pm_r,314._pm_r,   0._pm_r,124._pm_r,  .13_pm_r,166._pm_r, &
       243.50_pm_r, 33798._pm_r,   2._pm_r,307._pm_r,  .59_pm_r,322._pm_r,   1._pm_r,141._pm_r,  .15_pm_r,188._pm_r, &
       253.00_pm_r, 37438._pm_r,   3._pm_r,313._pm_r,  .50_pm_r,329._pm_r,   1._pm_r,157._pm_r,  .16_pm_r,203._pm_r, &
       261.00_pm_r, 41202._pm_r,   3._pm_r,317._pm_r,  .36_pm_r,343._pm_r,   1._pm_r,170._pm_r,  .17_pm_r,216._pm_r, &
       266.20_pm_r, 45066._pm_r,   4._pm_r,322._pm_r,  .35_pm_r,360._pm_r,   1._pm_r,181._pm_r,  .14_pm_r,234._pm_r, &
       267.10_pm_r, 48977._pm_r,   4._pm_r,327._pm_r,  .31_pm_r,358._pm_r,   1._pm_r,189._pm_r,  .16_pm_r,225._pm_r, &
       262.10_pm_r, 52856._pm_r,   4._pm_r,328._pm_r,  .07_pm_r,324._pm_r,   1._pm_r,192._pm_r,  .24_pm_r,192._pm_r, &
       252.30_pm_r, 56630._pm_r,   4._pm_r,327._pm_r,  .39_pm_r,176._pm_r,   2._pm_r,190._pm_r,  .40_pm_r,178._pm_r, &
       239.30_pm_r, 60229._pm_r,   3._pm_r,321._pm_r,  .76_pm_r,161._pm_r,   3._pm_r,187._pm_r,  .43_pm_r,179._pm_r, &
       224.70_pm_r, 63631._pm_r,   2._pm_r,311._pm_r,  .92_pm_r,153._pm_r,   3._pm_r,186._pm_r,  .39_pm_r,186._pm_r, &
       212.70_pm_r, 66827._pm_r,   1._pm_r,283._pm_r,  .86_pm_r,145._pm_r,   4._pm_r,187._pm_r,  .31_pm_r,203._pm_r, &
       205.60_pm_r, 69886._pm_r,   1._pm_r,186._pm_r,  .71_pm_r,134._pm_r,   4._pm_r,190._pm_r,  .27_pm_r,232._pm_r, &
       204.10_pm_r, 72883._pm_r,   1._pm_r,151._pm_r,  .58_pm_r,118._pm_r,   4._pm_r,195._pm_r,  .31_pm_r,256._pm_r, &
       205.30_pm_r, 75878._pm_r,   2._pm_r,136._pm_r,  .52_pm_r,100._pm_r,   4._pm_r,201._pm_r,  .37_pm_r,270._pm_r, &
       207.10_pm_r, 78901._pm_r,   3._pm_r,125._pm_r,  .51_pm_r, 86._pm_r,   5._pm_r,208._pm_r,  .41_pm_r,278._pm_r, &
       205.20_pm_r, 81949._pm_r,   3._pm_r,116._pm_r,  .50_pm_r, 76._pm_r,   5._pm_r,214._pm_r,  .42_pm_r,281._pm_r, &
       198.00_pm_r, 84899._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.90_pm_r, 87734._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.20_pm_r, 90503._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.90_pm_r, 93261._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.90_pm_r, 96030._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.30_pm_r, 98819._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       189.80_pm_r,101653._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       197.90_pm_r,104599._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       218.20_pm_r,107799._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       268.90_pm_r,111595._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       355.80_pm_r,116608._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
       301.40_pm_r,  -114._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       277.10_pm_r,  4130._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       254.10_pm_r,  8027._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.70_pm_r, 11555._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.70_pm_r, 14690._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       198.00_pm_r, 17578._pm_r,   2._pm_r,134._pm_r,  .19_pm_r,267._pm_r,   0._pm_r,234._pm_r,  .12_pm_r, 88._pm_r, &
       208.80_pm_r, 20560._pm_r,   1._pm_r,140._pm_r,  .27_pm_r,272._pm_r,   0._pm_r,130._pm_r,  .15_pm_r, 94._pm_r, &
       218.10_pm_r, 23689._pm_r,   1._pm_r,153._pm_r,  .32_pm_r,277._pm_r,   0._pm_r,108._pm_r,  .16_pm_r,103._pm_r, &
       225.30_pm_r, 26931._pm_r,   1._pm_r,179._pm_r,  .36_pm_r,284._pm_r,   0._pm_r,110._pm_r,  .16_pm_r,117._pm_r, &
       233.10_pm_r, 30283._pm_r,   1._pm_r,221._pm_r,  .35_pm_r,292._pm_r,   1._pm_r,118._pm_r,  .14_pm_r,136._pm_r, &
       244.70_pm_r, 33774._pm_r,   1._pm_r,252._pm_r,  .31_pm_r,302._pm_r,   1._pm_r,127._pm_r,  .14_pm_r,163._pm_r, &
       254.20_pm_r, 37430._pm_r,   1._pm_r,267._pm_r,  .23_pm_r,317._pm_r,   1._pm_r,139._pm_r,  .14_pm_r,188._pm_r, &
       261.30_pm_r, 41203._pm_r,   2._pm_r,277._pm_r,  .16_pm_r,346._pm_r,   1._pm_r,151._pm_r,  .15_pm_r,212._pm_r, &
       266.30_pm_r, 45073._pm_r,   2._pm_r,287._pm_r,  .20_pm_r, 13._pm_r,   1._pm_r,161._pm_r,  .09_pm_r,225._pm_r, &
       267.10_pm_r, 48985._pm_r,   2._pm_r,298._pm_r,  .13_pm_r, 18._pm_r,   1._pm_r,168._pm_r,  .05_pm_r,259._pm_r, &
       262.60_pm_r, 52866._pm_r,   2._pm_r,303._pm_r,  .11_pm_r,177._pm_r,   1._pm_r,175._pm_r,  .06_pm_r,215._pm_r, &
       253.00_pm_r, 56642._pm_r,   1._pm_r,295._pm_r,  .39_pm_r,179._pm_r,   1._pm_r,182._pm_r,  .24_pm_r,189._pm_r, &
       239.90_pm_r, 60247._pm_r,   1._pm_r,263._pm_r,  .50_pm_r,164._pm_r,   2._pm_r,186._pm_r,  .39_pm_r,192._pm_r, &
       225.20_pm_r, 63660._pm_r,   1._pm_r,217._pm_r,  .46_pm_r,148._pm_r,   2._pm_r,187._pm_r,  .47_pm_r,193._pm_r, &
       212.60_pm_r, 66867._pm_r,   2._pm_r,191._pm_r,  .36_pm_r,122._pm_r,   3._pm_r,187._pm_r,  .45_pm_r,196._pm_r, &
       205.00_pm_r, 69932._pm_r,   2._pm_r,176._pm_r,  .34_pm_r, 88._pm_r,   3._pm_r,186._pm_r,  .38_pm_r,198._pm_r, &
       203.50_pm_r, 72930._pm_r,   2._pm_r,165._pm_r,  .42_pm_r, 60._pm_r,   4._pm_r,185._pm_r,  .29_pm_r,205._pm_r, &
       205.70_pm_r, 75929._pm_r,   3._pm_r,153._pm_r,  .53_pm_r, 46._pm_r,   4._pm_r,185._pm_r,  .23_pm_r,214._pm_r, &
       208.20_pm_r, 78962._pm_r,   3._pm_r,141._pm_r,  .61_pm_r, 38._pm_r,   4._pm_r,184._pm_r,  .20_pm_r,218._pm_r, &
       205.80_pm_r, 82020._pm_r,   3._pm_r,128._pm_r,  .64_pm_r, 35._pm_r,   4._pm_r,183._pm_r,  .16_pm_r,222._pm_r, &
       197.60_pm_r, 84965._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.60_pm_r, 87792._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.60_pm_r, 90564._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.30_pm_r, 93328._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.30_pm_r, 96102._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.00_pm_r, 98900._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       191.10_pm_r,101749._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.10_pm_r,104723._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       221.30_pm_r,107964._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       272.60_pm_r,111814._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       359.70_pm_r,116887._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
       301.30_pm_r,  -104._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       277.30_pm_r,  4140._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       253.70_pm_r,  8035._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.40_pm_r, 11557._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       201.30_pm_r, 14694._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       199.00_pm_r, 17592._pm_r,   3._pm_r,159._pm_r,  .26_pm_r,304._pm_r,   1._pm_r,120._pm_r,  .11_pm_r,  8._pm_r, &
       209.60_pm_r, 20582._pm_r,   2._pm_r,165._pm_r,  .35_pm_r,303._pm_r,   1._pm_r,105._pm_r,  .12_pm_r,  5._pm_r, &
       218.20_pm_r, 23717._pm_r,   2._pm_r,177._pm_r,  .40_pm_r,300._pm_r,   1._pm_r, 89._pm_r,  .10_pm_r,354._pm_r, &
       225.00_pm_r, 26962._pm_r,   2._pm_r,196._pm_r,  .43_pm_r,298._pm_r,   1._pm_r, 75._pm_r,  .08_pm_r,328._pm_r, &
       232.20_pm_r, 30308._pm_r,   2._pm_r,217._pm_r,  .39_pm_r,296._pm_r,   1._pm_r, 67._pm_r,  .07_pm_r,279._pm_r, &
       243.90_pm_r, 33789._pm_r,   2._pm_r,234._pm_r,  .30_pm_r,295._pm_r,   0._pm_r, 64._pm_r,  .10_pm_r,242._pm_r, &
       253.80_pm_r, 37439._pm_r,   2._pm_r,243._pm_r,  .19_pm_r,298._pm_r,   0._pm_r, 73._pm_r,  .12_pm_r,225._pm_r, &
       261.60_pm_r, 41212._pm_r,   2._pm_r,247._pm_r,  .07_pm_r,321._pm_r,   0._pm_r,128._pm_r,  .13_pm_r,214._pm_r, &
       266.80_pm_r, 45087._pm_r,   2._pm_r,251._pm_r,  .13_pm_r, 20._pm_r,   0._pm_r,175._pm_r,  .10_pm_r,200._pm_r, &
       267.10_pm_r, 49003._pm_r,   2._pm_r,256._pm_r,  .20_pm_r, 40._pm_r,   0._pm_r,182._pm_r,  .04_pm_r,187._pm_r, &
       260.60_pm_r, 52871._pm_r,   2._pm_r,259._pm_r,  .18_pm_r, 81._pm_r,   0._pm_r,179._pm_r,  .06_pm_r, 38._pm_r, &
       251.00_pm_r, 56622._pm_r,   1._pm_r,251._pm_r,  .32_pm_r,141._pm_r,   0._pm_r,156._pm_r,  .14_pm_r, 34._pm_r, &
       240.30_pm_r, 60220._pm_r,   1._pm_r,225._pm_r,  .49_pm_r,152._pm_r,   0._pm_r,103._pm_r,  .14_pm_r, 61._pm_r, &
       227.90_pm_r, 63653._pm_r,   2._pm_r,201._pm_r,  .55_pm_r,152._pm_r,   0._pm_r, 96._pm_r,  .15_pm_r,116._pm_r, &
       216.80_pm_r, 66904._pm_r,   2._pm_r,186._pm_r,  .49_pm_r,148._pm_r,   1._pm_r,113._pm_r,  .28_pm_r,149._pm_r, &
       209.10_pm_r, 70020._pm_r,   3._pm_r,177._pm_r,  .39_pm_r,137._pm_r,   1._pm_r,132._pm_r,  .43_pm_r,161._pm_r, &
       206.00_pm_r, 73055._pm_r,   3._pm_r,170._pm_r,  .32_pm_r,117._pm_r,   2._pm_r,145._pm_r,  .56_pm_r,167._pm_r, &
       205.80_pm_r, 76067._pm_r,   3._pm_r,164._pm_r,  .28_pm_r, 98._pm_r,   3._pm_r,153._pm_r,  .66_pm_r,170._pm_r, &
       206.40_pm_r, 79088._pm_r,   4._pm_r,158._pm_r,  .28_pm_r, 83._pm_r,   4._pm_r,158._pm_r,  .73_pm_r,171._pm_r, &
       203.60_pm_r, 82116._pm_r,   4._pm_r,151._pm_r,  .28_pm_r, 74._pm_r,   5._pm_r,161._pm_r,  .74_pm_r,171._pm_r, &
       196.20_pm_r, 85036._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.00_pm_r, 87851._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.00_pm_r, 90615._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.40_pm_r, 93367._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.70_pm_r, 96130._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.40_pm_r, 98926._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       193.00_pm_r,101792._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       203.70_pm_r,104806._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.60_pm_r,108116._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       278.30_pm_r,112054._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       364.10_pm_r,117205._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
       299.20_pm_r,  -124._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       276.60_pm_r,  4098._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       251.80_pm_r,  7972._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       225.30_pm_r, 11470._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       204.10_pm_r, 14618._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       202.40_pm_r, 17568._pm_r,   3._pm_r,146._pm_r,  .60_pm_r,280._pm_r,   1._pm_r,173._pm_r,  .34_pm_r, 18._pm_r, &
       211.40_pm_r, 20596._pm_r,   3._pm_r,162._pm_r,  .77_pm_r,280._pm_r,   1._pm_r,157._pm_r,  .44_pm_r, 16._pm_r, &
       219.60_pm_r, 23755._pm_r,   2._pm_r,187._pm_r,  .79_pm_r,282._pm_r,   1._pm_r,103._pm_r,  .45_pm_r, 15._pm_r, &
       226.20_pm_r, 27019._pm_r,   3._pm_r,214._pm_r,  .72_pm_r,284._pm_r,   1._pm_r, 51._pm_r,  .42_pm_r, 12._pm_r, &
       233.20_pm_r, 30384._pm_r,   3._pm_r,232._pm_r,  .55_pm_r,287._pm_r,   1._pm_r, 35._pm_r,  .32_pm_r,  9._pm_r, &
       242.90_pm_r, 33866._pm_r,   3._pm_r,241._pm_r,  .32_pm_r,292._pm_r,   2._pm_r, 29._pm_r,  .20_pm_r,  0._pm_r, &
       252.70_pm_r, 37498._pm_r,   4._pm_r,245._pm_r,  .11_pm_r,307._pm_r,   2._pm_r, 25._pm_r,  .08_pm_r,343._pm_r, &
       261.50_pm_r, 41262._pm_r,   4._pm_r,246._pm_r,  .06_pm_r, 99._pm_r,   2._pm_r, 23._pm_r,  .04_pm_r,236._pm_r, &
       267.90_pm_r, 45144._pm_r,   4._pm_r,245._pm_r,  .04_pm_r,187._pm_r,   2._pm_r, 22._pm_r,  .06_pm_r,198._pm_r, &
       268.70_pm_r, 49081._pm_r,   4._pm_r,245._pm_r,  .16_pm_r,263._pm_r,   2._pm_r, 24._pm_r,  .07_pm_r,146._pm_r, &
       259.70_pm_r, 52955._pm_r,   4._pm_r,247._pm_r,  .23_pm_r,279._pm_r,   2._pm_r, 29._pm_r,  .14_pm_r, 92._pm_r, &
       248.90_pm_r, 56680._pm_r,   4._pm_r,249._pm_r,  .14_pm_r,306._pm_r,   2._pm_r, 35._pm_r,  .23_pm_r, 70._pm_r, &
       239.80_pm_r, 60256._pm_r,   4._pm_r,250._pm_r,  .16_pm_r, 76._pm_r,   2._pm_r, 40._pm_r,  .18_pm_r, 71._pm_r, &
       230.30_pm_r, 63701._pm_r,   4._pm_r,249._pm_r,  .36_pm_r, 93._pm_r,   2._pm_r, 44._pm_r,  .10_pm_r,119._pm_r, &
       220.90_pm_r, 67001._pm_r,   3._pm_r,244._pm_r,  .47_pm_r, 98._pm_r,   2._pm_r, 48._pm_r,  .21_pm_r,181._pm_r, &
       213.20_pm_r, 70180._pm_r,   3._pm_r,234._pm_r,  .51_pm_r,101._pm_r,   2._pm_r, 57._pm_r,  .40_pm_r,194._pm_r, &
       208.20_pm_r, 73261._pm_r,   2._pm_r,218._pm_r,  .52_pm_r,104._pm_r,   1._pm_r, 76._pm_r,  .57_pm_r,197._pm_r, &
       205.10_pm_r, 76286._pm_r,   2._pm_r,198._pm_r,  .51_pm_r,106._pm_r,   1._pm_r,116._pm_r,  .68_pm_r,199._pm_r, &
       204.10_pm_r, 79278._pm_r,   2._pm_r,177._pm_r,  .50_pm_r,108._pm_r,   2._pm_r,154._pm_r,  .76_pm_r,200._pm_r, &
       200.70_pm_r, 82262._pm_r,   2._pm_r,161._pm_r,  .46_pm_r,109._pm_r,   3._pm_r,172._pm_r,  .77_pm_r,200._pm_r, &
       193.60_pm_r, 85142._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.50_pm_r, 87921._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.20_pm_r, 90646._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       183.70_pm_r, 93356._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.80_pm_r, 96081._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.10_pm_r, 98857._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       195.10_pm_r,101735._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       208.20_pm_r,104798._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       233.60_pm_r,108196._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       285.50_pm_r,112248._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       368.80_pm_r,117488._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
       294.60_pm_r,   -95._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       273.20_pm_r,  4065._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       247.40_pm_r,  7879._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       222.60_pm_r, 11322._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       209.10_pm_r, 14485._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       208.20_pm_r, 17525._pm_r,   4._pm_r,158._pm_r,  .89_pm_r,294._pm_r,   1._pm_r,179._pm_r,  .36_pm_r, 31._pm_r, &
       213.80_pm_r, 20613._pm_r,   3._pm_r,175._pm_r, 1.09_pm_r,294._pm_r,   1._pm_r,157._pm_r,  .47_pm_r, 29._pm_r, &
       219.70_pm_r, 23789._pm_r,   3._pm_r,203._pm_r, 1.08_pm_r,294._pm_r,   1._pm_r,106._pm_r,  .50_pm_r, 27._pm_r, &
       226.20_pm_r, 27051._pm_r,   3._pm_r,229._pm_r,  .91_pm_r,293._pm_r,   1._pm_r, 65._pm_r,  .49_pm_r, 24._pm_r, &
       233.30_pm_r, 30417._pm_r,   4._pm_r,244._pm_r,  .59_pm_r,291._pm_r,   2._pm_r, 49._pm_r,  .41_pm_r, 19._pm_r, &
       243.10_pm_r, 33900._pm_r,   4._pm_r,249._pm_r,  .23_pm_r,280._pm_r,   2._pm_r, 41._pm_r,  .29_pm_r, 11._pm_r, &
       253.50_pm_r, 37539._pm_r,   5._pm_r,249._pm_r,  .10_pm_r,173._pm_r,   2._pm_r, 37._pm_r,  .18_pm_r,357._pm_r, &
       263.50_pm_r, 41322._pm_r,   5._pm_r,246._pm_r,  .22_pm_r,148._pm_r,   3._pm_r, 33._pm_r,  .10_pm_r,332._pm_r, &
       270.20_pm_r, 45238._pm_r,   5._pm_r,243._pm_r,  .10_pm_r,191._pm_r,   3._pm_r, 31._pm_r,  .06_pm_r,342._pm_r, &
       269.90_pm_r, 49202._pm_r,   5._pm_r,244._pm_r,  .30_pm_r,282._pm_r,   3._pm_r, 31._pm_r,  .13_pm_r, 45._pm_r, &
       260.80_pm_r, 53091._pm_r,   5._pm_r,249._pm_r,  .50_pm_r,292._pm_r,   3._pm_r, 33._pm_r,  .21_pm_r, 55._pm_r, &
       249.60_pm_r, 56830._pm_r,   6._pm_r,254._pm_r,  .39_pm_r,298._pm_r,   3._pm_r, 35._pm_r,  .18_pm_r, 53._pm_r, &
       240.10_pm_r, 60413._pm_r,   6._pm_r,256._pm_r,  .08_pm_r,330._pm_r,   3._pm_r, 35._pm_r,  .02_pm_r, 49._pm_r, &
       231.10_pm_r, 63865._pm_r,   6._pm_r,257._pm_r,  .24_pm_r, 79._pm_r,   3._pm_r, 35._pm_r,  .12_pm_r,223._pm_r, &
       222.50_pm_r, 67183._pm_r,   5._pm_r,256._pm_r,  .45_pm_r, 77._pm_r,   3._pm_r, 35._pm_r,  .20_pm_r,218._pm_r, &
       214.60_pm_r, 70385._pm_r,   5._pm_r,257._pm_r,  .61_pm_r, 72._pm_r,   3._pm_r, 35._pm_r,  .25_pm_r,210._pm_r, &
       208.20_pm_r, 73478._pm_r,   4._pm_r,259._pm_r,  .69_pm_r, 66._pm_r,   2._pm_r, 36._pm_r,  .28_pm_r,204._pm_r, &
       203.50_pm_r, 76493._pm_r,   3._pm_r,265._pm_r,  .73_pm_r, 62._pm_r,   2._pm_r, 40._pm_r,  .29_pm_r,199._pm_r, &
       200.30_pm_r, 79448._pm_r,   2._pm_r,281._pm_r,  .75_pm_r, 60._pm_r,   2._pm_r, 46._pm_r,  .29_pm_r,195._pm_r, &
       195.40_pm_r, 82359._pm_r,   1._pm_r,324._pm_r,  .71_pm_r, 58._pm_r,   1._pm_r, 56._pm_r,  .28_pm_r,194._pm_r, &
       188.10_pm_r, 85161._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       182.20_pm_r, 87861._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       178.80_pm_r, 90504._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       178.80_pm_r, 93136._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       181.40_pm_r, 95798._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.90_pm_r, 98538._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       197.20_pm_r,101421._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       213.50_pm_r,104539._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       242.10_pm_r,108044._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       293.00_pm_r,112225._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       373.00_pm_r,117551._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)
       

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
       
       288.00_pm_r,   -38._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       267.90_pm_r,  4031._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       241.70_pm_r,  7762._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.90_pm_r, 11141._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       214.40_pm_r, 14320._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       214.60_pm_r, 17458._pm_r,   4._pm_r,201._pm_r,  .32_pm_r,293._pm_r,   1._pm_r, 99._pm_r,  .10_pm_r,313._pm_r, &
       216.70_pm_r, 20614._pm_r,   4._pm_r,210._pm_r,  .39_pm_r,293._pm_r,   1._pm_r, 94._pm_r,  .14_pm_r,315._pm_r, &
       219.80_pm_r, 23809._pm_r,   4._pm_r,219._pm_r,  .39_pm_r,290._pm_r,   1._pm_r, 85._pm_r,  .18_pm_r,316._pm_r, &
       225.20_pm_r, 27064._pm_r,   4._pm_r,226._pm_r,  .32_pm_r,286._pm_r,   1._pm_r, 70._pm_r,  .20_pm_r,319._pm_r, &
       232.30_pm_r, 30413._pm_r,   4._pm_r,231._pm_r,  .20_pm_r,279._pm_r,   1._pm_r, 51._pm_r,  .20_pm_r,323._pm_r, &
       243.70_pm_r, 33890._pm_r,   4._pm_r,233._pm_r,  .09_pm_r,249._pm_r,   1._pm_r, 33._pm_r,  .18_pm_r,326._pm_r, &
       256.20_pm_r, 37555._pm_r,   4._pm_r,232._pm_r,  .06_pm_r,180._pm_r,   1._pm_r, 21._pm_r,  .14_pm_r,333._pm_r, &
       267.20_pm_r, 41387._pm_r,   4._pm_r,231._pm_r,  .07_pm_r,172._pm_r,   1._pm_r, 15._pm_r,  .10_pm_r,343._pm_r, &
       273.20_pm_r, 45354._pm_r,   5._pm_r,231._pm_r,  .07_pm_r,250._pm_r,   1._pm_r, 14._pm_r,  .06_pm_r, 48._pm_r, &
       271.60_pm_r, 49352._pm_r,   5._pm_r,232._pm_r,  .13_pm_r,261._pm_r,   1._pm_r, 19._pm_r,  .12_pm_r,110._pm_r, &
       263.50_pm_r, 53273._pm_r,   5._pm_r,233._pm_r,  .11_pm_r,259._pm_r,   1._pm_r, 27._pm_r,  .13_pm_r,146._pm_r, &
       251.20_pm_r, 57048._pm_r,   5._pm_r,233._pm_r,  .02_pm_r,180._pm_r,   1._pm_r, 32._pm_r,  .14_pm_r,214._pm_r, &
       240.40_pm_r, 60640._pm_r,   5._pm_r,233._pm_r,  .06_pm_r, 39._pm_r,   1._pm_r, 22._pm_r,  .25_pm_r,266._pm_r, &
       231.70_pm_r, 64099._pm_r,   5._pm_r,234._pm_r,  .15_pm_r,  4._pm_r,   1._pm_r,355._pm_r,  .34_pm_r,291._pm_r, &
       222.70_pm_r, 67424._pm_r,   5._pm_r,237._pm_r,  .24_pm_r,344._pm_r,   1._pm_r,336._pm_r,  .37_pm_r,311._pm_r, &
       214.20_pm_r, 70625._pm_r,   5._pm_r,242._pm_r,  .32_pm_r,334._pm_r,   2._pm_r,331._pm_r,  .39_pm_r,329._pm_r, &
       206.60_pm_r, 73703._pm_r,   5._pm_r,249._pm_r,  .38_pm_r,328._pm_r,   2._pm_r,332._pm_r,  .41_pm_r,344._pm_r, &
       199.30_pm_r, 76678._pm_r,   5._pm_r,256._pm_r,  .42_pm_r,325._pm_r,   3._pm_r,336._pm_r,  .41_pm_r,354._pm_r, &
       192.60_pm_r, 79543._pm_r,   5._pm_r,262._pm_r,  .43_pm_r,322._pm_r,   4._pm_r,340._pm_r,  .42_pm_r,  2._pm_r, &
       185.80_pm_r, 82317._pm_r,   5._pm_r,268._pm_r,  .40_pm_r,320._pm_r,   4._pm_r,343._pm_r,  .39_pm_r,  6._pm_r, &
       179.00_pm_r, 84984._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       174.00_pm_r, 87558._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       171.10_pm_r, 90082._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       172.40_pm_r, 92608._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       177.10_pm_r, 95188._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.50_pm_r, 97882._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       199.30_pm_r,100767._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.70_pm_r,103946._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       251.50_pm_r,107572._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       299.80_pm_r,111885._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       376.80_pm_r,117282._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
       281.70_pm_r,   -20._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       262.00_pm_r,  3956._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       236.70_pm_r,  7603._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       220.80_pm_r, 10949._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.30_pm_r, 14168._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.70_pm_r, 17383._pm_r,   4._pm_r,283._pm_r,  .58_pm_r,174._pm_r,   3._pm_r, 86._pm_r,  .28_pm_r,298._pm_r, &
       220.40_pm_r, 20605._pm_r,   4._pm_r,268._pm_r,  .78_pm_r,174._pm_r,   2._pm_r, 79._pm_r,  .38_pm_r,297._pm_r, &
       221.50_pm_r, 23837._pm_r,   4._pm_r,249._pm_r,  .87_pm_r,175._pm_r,   2._pm_r, 67._pm_r,  .42_pm_r,296._pm_r, &
       225.60_pm_r, 27106._pm_r,   4._pm_r,233._pm_r,  .80_pm_r,176._pm_r,   1._pm_r, 50._pm_r,  .37_pm_r,293._pm_r, &
       232.30_pm_r, 30457._pm_r,   5._pm_r,223._pm_r,  .58_pm_r,178._pm_r,   1._pm_r, 31._pm_r,  .27_pm_r,290._pm_r, &
       244.30_pm_r, 33939._pm_r,   5._pm_r,218._pm_r,  .28_pm_r,186._pm_r,   1._pm_r, 17._pm_r,  .13_pm_r,283._pm_r, &
       257.10_pm_r, 37615._pm_r,   6._pm_r,218._pm_r,  .07_pm_r,262._pm_r,   1._pm_r, 13._pm_r,  .02_pm_r,246._pm_r, &
       268.50_pm_r, 41462._pm_r,   6._pm_r,219._pm_r,  .21_pm_r,333._pm_r,   1._pm_r, 14._pm_r,  .05_pm_r,107._pm_r, &
       275.00_pm_r, 45452._pm_r,   6._pm_r,223._pm_r,  .22_pm_r,313._pm_r,   1._pm_r, 17._pm_r,  .06_pm_r, 45._pm_r, &
       273.70_pm_r, 49477._pm_r,   6._pm_r,225._pm_r,  .26_pm_r,255._pm_r,   1._pm_r, 16._pm_r,  .15_pm_r,345._pm_r, &
       265.90_pm_r, 53431._pm_r,   6._pm_r,226._pm_r,  .42_pm_r,228._pm_r,   2._pm_r, 10._pm_r,  .21_pm_r,333._pm_r, &
       254.60_pm_r, 57247._pm_r,   7._pm_r,226._pm_r,  .41_pm_r,226._pm_r,   2._pm_r,  5._pm_r,  .11_pm_r,342._pm_r, &
       243.80_pm_r, 60891._pm_r,   7._pm_r,227._pm_r,  .14_pm_r,280._pm_r,   2._pm_r,  6._pm_r,  .11_pm_r, 95._pm_r, &
       234.00_pm_r, 64392._pm_r,   7._pm_r,229._pm_r,  .31_pm_r,  9._pm_r,   2._pm_r, 14._pm_r,  .21_pm_r,104._pm_r, &
       224.40_pm_r, 67746._pm_r,   7._pm_r,232._pm_r,  .55_pm_r, 22._pm_r,   2._pm_r, 24._pm_r,  .24_pm_r, 95._pm_r, &
       215.50_pm_r, 70968._pm_r,   6._pm_r,236._pm_r,  .69_pm_r, 29._pm_r,   2._pm_r, 32._pm_r,  .23_pm_r, 77._pm_r, &
       206.40_pm_r, 74056._pm_r,   5._pm_r,241._pm_r,  .75_pm_r, 32._pm_r,   2._pm_r, 37._pm_r,  .21_pm_r, 60._pm_r, &
       195.90_pm_r, 77006._pm_r,   4._pm_r,249._pm_r,  .74_pm_r, 34._pm_r,   3._pm_r, 39._pm_r,  .22_pm_r, 44._pm_r, &
       185.50_pm_r, 79793._pm_r,   3._pm_r,261._pm_r,  .70_pm_r, 35._pm_r,   3._pm_r, 39._pm_r,  .21_pm_r, 35._pm_r, &
       176.00_pm_r, 82438._pm_r,   2._pm_r,278._pm_r,  .62_pm_r, 36._pm_r,   3._pm_r, 38._pm_r,  .19_pm_r, 28._pm_r, &
       168.40_pm_r, 84947._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       164.10_pm_r, 87366._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       162.50_pm_r, 89751._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       165.50_pm_r, 92161._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       172.50_pm_r, 94653._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       183.90_pm_r, 97296._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       201.60_pm_r,100183._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.80_pm_r,103428._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       261.20_pm_r,107185._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       304.90_pm_r,111618._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       379.80_pm_r,117063._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
       
       277.50_pm_r,   -28._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       257.20_pm_r,  3879._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       232.80_pm_r,  7460._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       223.30_pm_r, 10793._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       223.30_pm_r, 14056._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       223.80_pm_r, 17330._pm_r,   5._pm_r,299._pm_r,  .61_pm_r,170._pm_r,   3._pm_r, 73._pm_r,  .21_pm_r,291._pm_r, &
       224.10_pm_r, 20610._pm_r,   5._pm_r,290._pm_r,  .84_pm_r,168._pm_r,   2._pm_r, 68._pm_r,  .27_pm_r,287._pm_r, &
       223.80_pm_r, 23887._pm_r,   4._pm_r,274._pm_r,  .95_pm_r,165._pm_r,   2._pm_r, 60._pm_r,  .31_pm_r,282._pm_r, &
       226.70_pm_r, 27180._pm_r,   4._pm_r,255._pm_r,  .88_pm_r,161._pm_r,   2._pm_r, 51._pm_r,  .27_pm_r,272._pm_r, &
       233.70_pm_r, 30548._pm_r,   4._pm_r,238._pm_r,  .66_pm_r,154._pm_r,   1._pm_r, 44._pm_r,  .20_pm_r,254._pm_r, &
       245.20_pm_r, 34048._pm_r,   4._pm_r,227._pm_r,  .37_pm_r,139._pm_r,   1._pm_r, 40._pm_r,  .14_pm_r,225._pm_r, &
       257.30_pm_r, 37731._pm_r,   4._pm_r,222._pm_r,  .18_pm_r, 98._pm_r,   1._pm_r, 43._pm_r,  .12_pm_r,187._pm_r, &
       269.00_pm_r, 41583._pm_r,   4._pm_r,221._pm_r,  .18_pm_r, 34._pm_r,   1._pm_r, 50._pm_r,  .10_pm_r,156._pm_r, &
       276.20_pm_r, 45585._pm_r,   4._pm_r,223._pm_r,  .13_pm_r,301._pm_r,   1._pm_r, 56._pm_r,  .10_pm_r, 78._pm_r, &
       275.40_pm_r, 49632._pm_r,   4._pm_r,226._pm_r,  .28_pm_r,249._pm_r,   1._pm_r, 55._pm_r,  .22_pm_r, 34._pm_r, &
       268.10_pm_r, 53615._pm_r,   4._pm_r,227._pm_r,  .37_pm_r,235._pm_r,   2._pm_r, 48._pm_r,  .24_pm_r, 15._pm_r, &
       257.30_pm_r, 57467._pm_r,   5._pm_r,228._pm_r,  .28_pm_r,243._pm_r,   2._pm_r, 41._pm_r,  .13_pm_r,333._pm_r, &
       246.10_pm_r, 61149._pm_r,   5._pm_r,230._pm_r,  .16_pm_r,333._pm_r,   2._pm_r, 36._pm_r,  .18_pm_r,247._pm_r, &
       236.10_pm_r, 64682._pm_r,   5._pm_r,234._pm_r,  .44_pm_r, 16._pm_r,   1._pm_r, 31._pm_r,  .33_pm_r,227._pm_r, &
       226.80_pm_r, 68068._pm_r,   4._pm_r,240._pm_r,  .71_pm_r, 28._pm_r,   1._pm_r, 23._pm_r,  .38_pm_r,220._pm_r, &
       217.50_pm_r, 71324._pm_r,   3._pm_r,251._pm_r,  .90_pm_r, 33._pm_r,   0._pm_r,350._pm_r,  .37_pm_r,216._pm_r, &
       206.60_pm_r, 74430._pm_r,   2._pm_r,274._pm_r,  .98_pm_r, 36._pm_r,   0._pm_r,249._pm_r,  .33_pm_r,216._pm_r, &
       193.50_pm_r, 77365._pm_r,   2._pm_r,318._pm_r,  .97_pm_r, 38._pm_r,   1._pm_r,231._pm_r,  .28_pm_r,215._pm_r, &
       180.80_pm_r, 80098._pm_r,   2._pm_r,353._pm_r,  .90_pm_r, 40._pm_r,   1._pm_r,225._pm_r,  .23_pm_r,213._pm_r, &
       168.60_pm_r, 82658._pm_r,   3._pm_r,  9._pm_r,  .76_pm_r, 40._pm_r,   2._pm_r,223._pm_r,  .17_pm_r,211._pm_r, &
       158.70_pm_r, 85018._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       154.40_pm_r, 87282._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       154.30_pm_r, 89535._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       159.00_pm_r, 91835._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       168.00_pm_r, 94243._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       182.30_pm_r, 96837._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       203.70_pm_r, 99725._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       234.10_pm_r,103038._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       270.40_pm_r,106925._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       307.70_pm_r,111456._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       382.10_pm_r,116924._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)
  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
       269.30_pm_r,    17._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       253.40_pm_r,  3834._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       230.20_pm_r,  7365._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       225.30_pm_r, 10691._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.20_pm_r, 13988._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       227.00_pm_r, 17306._pm_r,   4._pm_r,290._pm_r,  .12_pm_r,170._pm_r,   2._pm_r, 44._pm_r,  .12_pm_r,195._pm_r, &
       227.50_pm_r, 20635._pm_r,   4._pm_r,287._pm_r,  .23_pm_r,162._pm_r,   2._pm_r, 47._pm_r,  .18_pm_r,199._pm_r, &
       226.50_pm_r, 23956._pm_r,   3._pm_r,281._pm_r,  .37_pm_r,156._pm_r,   1._pm_r, 52._pm_r,  .24_pm_r,203._pm_r, &
       228.30_pm_r, 27280._pm_r,   3._pm_r,271._pm_r,  .50_pm_r,151._pm_r,   1._pm_r, 61._pm_r,  .27_pm_r,208._pm_r, &
       235.60_pm_r, 30676._pm_r,   3._pm_r,256._pm_r,  .58_pm_r,146._pm_r,   1._pm_r, 76._pm_r,  .27_pm_r,211._pm_r, &
       245.70_pm_r, 34195._pm_r,   3._pm_r,237._pm_r,  .59_pm_r,141._pm_r,   1._pm_r,102._pm_r,  .24_pm_r,216._pm_r, &
       258.30_pm_r, 37887._pm_r,   3._pm_r,219._pm_r,  .51_pm_r,135._pm_r,   1._pm_r,134._pm_r,  .19_pm_r,224._pm_r, &
       270.50_pm_r, 41758._pm_r,   3._pm_r,204._pm_r,  .37_pm_r,126._pm_r,   1._pm_r,159._pm_r,  .13_pm_r,236._pm_r, &
       277.10_pm_r, 45775._pm_r,   3._pm_r,197._pm_r,  .12_pm_r,165._pm_r,   1._pm_r,167._pm_r,  .01_pm_r, 11._pm_r, &
       277.90_pm_r, 49846._pm_r,   3._pm_r,200._pm_r,  .26_pm_r,260._pm_r,   1._pm_r,162._pm_r,  .05_pm_r, 37._pm_r, &
       270.90_pm_r, 53869._pm_r,   3._pm_r,209._pm_r,  .50_pm_r,270._pm_r,   1._pm_r,160._pm_r,  .03_pm_r,290._pm_r, &
       260.40_pm_r, 57763._pm_r,   4._pm_r,220._pm_r,  .57_pm_r,271._pm_r,   1._pm_r,173._pm_r,  .14_pm_r,243._pm_r, &
       248.40_pm_r, 61486._pm_r,   4._pm_r,228._pm_r,  .32_pm_r,296._pm_r,   1._pm_r,192._pm_r,  .17_pm_r,229._pm_r, &
       237.00_pm_r, 65041._pm_r,   4._pm_r,234._pm_r,  .30_pm_r, 14._pm_r,   1._pm_r,199._pm_r,  .17_pm_r,203._pm_r, &
       227.80_pm_r, 68441._pm_r,   3._pm_r,237._pm_r,  .62_pm_r, 44._pm_r,   1._pm_r,196._pm_r,  .18_pm_r,167._pm_r, &
       217.10_pm_r, 71705._pm_r,   2._pm_r,241._pm_r,  .91_pm_r, 53._pm_r,   1._pm_r,187._pm_r,  .23_pm_r,140._pm_r, &
       203.90_pm_r, 74786._pm_r,   1._pm_r,255._pm_r, 1.08_pm_r, 55._pm_r,   2._pm_r,175._pm_r,  .30_pm_r,127._pm_r, &
       189.90_pm_r, 77674._pm_r,   1._pm_r, 41._pm_r, 1.12_pm_r, 57._pm_r,   2._pm_r,165._pm_r,  .32_pm_r,120._pm_r, &
       177.10_pm_r, 80353._pm_r,   3._pm_r, 51._pm_r, 1.06_pm_r, 58._pm_r,   2._pm_r,156._pm_r,  .31_pm_r,117._pm_r, &
       163.40_pm_r, 82857._pm_r,   4._pm_r, 54._pm_r,  .92_pm_r, 59._pm_r,   3._pm_r,150._pm_r,  .28_pm_r,114._pm_r, &
       151.10_pm_r, 85103._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       146.40_pm_r, 87240._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       147.70_pm_r, 89385._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       153.60_pm_r, 91596._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       164.30_pm_r, 93936._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       181.00_pm_r, 96490._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       205.60_pm_r, 99379._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       240.50_pm_r,102754._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       278.10_pm_r,106753._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       308.70_pm_r,111353._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       383.80_pm_r,116827._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
       263.60_pm_r,    42._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       250.40_pm_r,  3794._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       228.60_pm_r,  7290._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       227.10_pm_r, 10616._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       228.50_pm_r, 13941._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       229.40_pm_r, 17293._pm_r,   2._pm_r,287._pm_r,  .08_pm_r,105._pm_r,   1._pm_r, 31._pm_r,  .08_pm_r,191._pm_r, &
       230.00_pm_r, 20658._pm_r,   2._pm_r,287._pm_r,  .10_pm_r,114._pm_r,   1._pm_r, 35._pm_r,  .13_pm_r,193._pm_r, &
       229.10_pm_r, 24018._pm_r,   2._pm_r,286._pm_r,  .11_pm_r,126._pm_r,   1._pm_r, 43._pm_r,  .19_pm_r,197._pm_r, &
       230.00_pm_r, 27374._pm_r,   2._pm_r,284._pm_r,  .11_pm_r,142._pm_r,   0._pm_r, 66._pm_r,  .22_pm_r,201._pm_r, &
       236.40_pm_r, 30790._pm_r,   2._pm_r,279._pm_r,  .13_pm_r,171._pm_r,   0._pm_r,136._pm_r,  .24_pm_r,203._pm_r, &
       245.20_pm_r, 34310._pm_r,   2._pm_r,272._pm_r,  .16_pm_r,187._pm_r,   1._pm_r,177._pm_r,  .23_pm_r,209._pm_r, &
       258.00_pm_r, 37996._pm_r,   2._pm_r,263._pm_r,  .19_pm_r,197._pm_r,   1._pm_r,190._pm_r,  .18_pm_r,214._pm_r, &
       270.90_pm_r, 41868._pm_r,   2._pm_r,255._pm_r,  .22_pm_r,202._pm_r,   1._pm_r,196._pm_r,  .12_pm_r,222._pm_r, &
       278.40_pm_r, 45897._pm_r,   2._pm_r,249._pm_r,  .22_pm_r,207._pm_r,   1._pm_r,193._pm_r,  .11_pm_r,105._pm_r, &
       280.60_pm_r, 49997._pm_r,   2._pm_r,244._pm_r,  .28_pm_r,229._pm_r,   1._pm_r,177._pm_r,  .30_pm_r, 96._pm_r, &
       273.90_pm_r, 54064._pm_r,   3._pm_r,244._pm_r,  .35_pm_r,258._pm_r,   1._pm_r,150._pm_r,  .42_pm_r, 83._pm_r, &
       263.00_pm_r, 58001._pm_r,   3._pm_r,248._pm_r,  .43_pm_r,281._pm_r,   1._pm_r,124._pm_r,  .48_pm_r, 53._pm_r, &
       250.10_pm_r, 61757._pm_r,   4._pm_r,254._pm_r,  .34_pm_r,301._pm_r,   2._pm_r, 99._pm_r,  .45_pm_r, 11._pm_r, &
       237.30_pm_r, 65327._pm_r,   4._pm_r,259._pm_r,  .23_pm_r,326._pm_r,   2._pm_r, 75._pm_r,  .49_pm_r,332._pm_r, &
       227.90_pm_r, 68731._pm_r,   4._pm_r,263._pm_r,  .15_pm_r, 14._pm_r,   1._pm_r, 44._pm_r,  .59_pm_r,293._pm_r, &
       216.40_pm_r, 71991._pm_r,   4._pm_r,265._pm_r,  .17_pm_r, 65._pm_r,   1._pm_r,355._pm_r,  .75_pm_r,268._pm_r, &
       201.80_pm_r, 75051._pm_r,   4._pm_r,265._pm_r,  .23_pm_r, 95._pm_r,   2._pm_r,302._pm_r,  .89_pm_r,253._pm_r, &
       187.40_pm_r, 77904._pm_r,   3._pm_r,263._pm_r,  .26_pm_r,106._pm_r,   3._pm_r,277._pm_r,  .95_pm_r,245._pm_r, &
       174.50_pm_r, 80546._pm_r,   3._pm_r,260._pm_r,  .27_pm_r,114._pm_r,   4._pm_r,265._pm_r,  .92_pm_r,240._pm_r, &
       160.00_pm_r, 83012._pm_r,   3._pm_r,255._pm_r,  .25_pm_r,117._pm_r,   5._pm_r,258._pm_r,  .81_pm_r,238._pm_r, &
       146.10_pm_r, 85183._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       141.30_pm_r, 87238._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       143.40_pm_r, 89315._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       150.10_pm_r, 91467._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       161.80_pm_r, 93763._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       180.00_pm_r, 96290._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       206.80_pm_r, 99178._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       245.10_pm_r,102598._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       283.20_pm_r,106676._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       308.80_pm_r,111315._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       384.80_pm_r,116788._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_mai = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_mai.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_mai,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_mai
