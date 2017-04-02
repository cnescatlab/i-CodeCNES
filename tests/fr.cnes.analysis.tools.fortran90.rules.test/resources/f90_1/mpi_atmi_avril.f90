subroutine mpi_atmi_avril (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de AVRIL 
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
integer, intent(out)                                           ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_80= (/ &
       264.00_pm_r,  -196._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       243.00_pm_r,  3505._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       221.90_pm_r,  6898._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       218.40_pm_r, 10111._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       218.20_pm_r, 13298._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       215.90_pm_r, 16477._pm_r,  10._pm_r,244._pm_r, 1.22_pm_r,179._pm_r,   1._pm_r, 53._pm_r,  .09_pm_r,  7._pm_r, &
       212.60_pm_r, 19617._pm_r,  11._pm_r,234._pm_r, 1.64_pm_r,176._pm_r,   1._pm_r, 48._pm_r,  .11_pm_r, 10._pm_r, &
       208.00_pm_r, 22697._pm_r,  12._pm_r,223._pm_r, 1.75_pm_r,170._pm_r,   1._pm_r, 43._pm_r,  .12_pm_r, 21._pm_r, &
       202.80_pm_r, 25703._pm_r,  13._pm_r,215._pm_r, 1.29_pm_r,156._pm_r,   1._pm_r, 41._pm_r,  .10_pm_r, 37._pm_r, &
       207.50_pm_r, 28695._pm_r,  14._pm_r,209._pm_r,  .73_pm_r,104._pm_r,   2._pm_r, 42._pm_r,  .06_pm_r, 75._pm_r, &
       212.70_pm_r, 31775._pm_r,  13._pm_r,206._pm_r, 1.24_pm_r, 43._pm_r,   2._pm_r, 44._pm_r,  .07_pm_r,153._pm_r, &
       220.10_pm_r, 34941._pm_r,  10._pm_r,204._pm_r, 1.94_pm_r, 26._pm_r,   1._pm_r, 49._pm_r,  .13_pm_r,189._pm_r, &
       231.30_pm_r, 38241._pm_r,   7._pm_r,205._pm_r, 2.23_pm_r, 19._pm_r,   1._pm_r, 54._pm_r,  .18_pm_r,207._pm_r, &
       243.10_pm_r, 41718._pm_r,   4._pm_r,205._pm_r, 1.88_pm_r, 33._pm_r,   1._pm_r, 59._pm_r,  .11_pm_r,203._pm_r, &
       254.40_pm_r, 45359._pm_r,   2._pm_r,183._pm_r, 1.76_pm_r, 47._pm_r,   1._pm_r, 66._pm_r,  .11_pm_r,170._pm_r, &
       258.90_pm_r, 49130._pm_r,   2._pm_r, 94._pm_r, 1.85_pm_r, 48._pm_r,   1._pm_r, 80._pm_r,  .23_pm_r,172._pm_r, &
       256.20_pm_r, 52905._pm_r,   4._pm_r, 64._pm_r, 1.95_pm_r, 36._pm_r,   1._pm_r,107._pm_r,  .43_pm_r,185._pm_r, &
       253.00_pm_r, 56633._pm_r,   7._pm_r, 49._pm_r, 1.69_pm_r, 15._pm_r,   1._pm_r,141._pm_r,  .52_pm_r,199._pm_r, &
       249.10_pm_r, 60313._pm_r,   8._pm_r, 38._pm_r, 1.49_pm_r,350._pm_r,   2._pm_r,164._pm_r,  .55_pm_r,204._pm_r, &
       242.60_pm_r, 63914._pm_r,  10._pm_r, 26._pm_r, 1.44_pm_r,324._pm_r,   2._pm_r,175._pm_r,  .47_pm_r,202._pm_r, &
       236.00_pm_r, 67421._pm_r,  10._pm_r, 15._pm_r, 1.56_pm_r,301._pm_r,   3._pm_r,180._pm_r,  .36_pm_r,193._pm_r, &
       231.40_pm_r, 70839._pm_r,  11._pm_r,  2._pm_r, 1.77_pm_r,285._pm_r,   3._pm_r,181._pm_r,  .26_pm_r,178._pm_r, &
       229.70_pm_r, 74213._pm_r,  12._pm_r,348._pm_r, 2.03_pm_r,275._pm_r,   4._pm_r,180._pm_r,  .20_pm_r,153._pm_r, &
       228.60_pm_r, 77573._pm_r,  13._pm_r,334._pm_r, 2.19_pm_r,269._pm_r,   4._pm_r,177._pm_r,  .20_pm_r,130._pm_r, &
       221.80_pm_r, 80914._pm_r,  14._pm_r,322._pm_r, 2.21_pm_r,265._pm_r,   4._pm_r,174._pm_r,  .21_pm_r,113._pm_r, &
       206.90_pm_r, 84034._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       193.00_pm_r, 86930._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.10_pm_r, 89675._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       181.30_pm_r, 92350._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       183.30_pm_r, 95034._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.60_pm_r, 97802._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       203.40_pm_r,100747._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       223.00_pm_r,103972._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       249.20_pm_r,107600._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       283.50_pm_r,111762._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       357.30_pm_r,116810._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
       269.70_pm_r,  -156._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       246.40_pm_r,  3613._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       224.50_pm_r,  7051._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       220.00_pm_r, 10297._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.90_pm_r, 13509._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       217.60_pm_r, 16714._pm_r,  13._pm_r,238._pm_r, 1.39_pm_r,163._pm_r,   2._pm_r, 13._pm_r,  .20_pm_r, 80._pm_r, &
       214.70_pm_r, 19880._pm_r,  14._pm_r,229._pm_r, 1.80_pm_r,156._pm_r,   2._pm_r, 23._pm_r,  .27_pm_r, 77._pm_r, &
       211.70_pm_r, 23003._pm_r,  15._pm_r,218._pm_r, 1.88_pm_r,141._pm_r,   2._pm_r, 32._pm_r,  .29_pm_r, 70._pm_r, &
       208.10_pm_r, 26075._pm_r,  15._pm_r,208._pm_r, 1.68_pm_r,111._pm_r,   2._pm_r, 37._pm_r,  .25_pm_r, 52._pm_r, &
       210.50_pm_r, 29132._pm_r,  14._pm_r,199._pm_r, 1.95_pm_r, 66._pm_r,   3._pm_r, 37._pm_r,  .21_pm_r, 12._pm_r, &
       214.20_pm_r, 32242._pm_r,  11._pm_r,190._pm_r, 2.89_pm_r, 39._pm_r,   3._pm_r, 32._pm_r,  .29_pm_r,333._pm_r, &
       220.90_pm_r, 35425._pm_r,   7._pm_r,175._pm_r, 3.59_pm_r, 26._pm_r,   3._pm_r, 23._pm_r,  .40_pm_r,316._pm_r, &
       231.90_pm_r, 38734._pm_r,   3._pm_r,124._pm_r, 3.62_pm_r, 18._pm_r,   3._pm_r, 13._pm_r,  .46_pm_r,309._pm_r, &
       244.60_pm_r, 42228._pm_r,   5._pm_r, 60._pm_r, 2.82_pm_r, 27._pm_r,   4._pm_r,  5._pm_r,  .29_pm_r,286._pm_r, &
       255.00_pm_r, 45887._pm_r,   9._pm_r, 47._pm_r, 2.49_pm_r, 34._pm_r,   4._pm_r,359._pm_r,  .24_pm_r,250._pm_r, &
       257.20_pm_r, 49650._pm_r,  12._pm_r, 42._pm_r, 2.51_pm_r, 26._pm_r,   3._pm_r,354._pm_r,  .18_pm_r,238._pm_r, &
       252.30_pm_r, 53386._pm_r,  16._pm_r, 37._pm_r, 2.75_pm_r,  8._pm_r,   3._pm_r,352._pm_r,  .03_pm_r,333._pm_r, &
       248.50_pm_r, 57052._pm_r,  19._pm_r, 29._pm_r, 2.58_pm_r,345._pm_r,   4._pm_r,353._pm_r,  .27_pm_r, 27._pm_r, &
       244.00_pm_r, 60661._pm_r,  21._pm_r, 21._pm_r, 2.42_pm_r,324._pm_r,   4._pm_r,358._pm_r,  .44_pm_r, 34._pm_r, &
       238.40_pm_r, 64193._pm_r,  23._pm_r, 13._pm_r, 2.23_pm_r,307._pm_r,   5._pm_r,  4._pm_r,  .51_pm_r, 39._pm_r, &
       233.00_pm_r, 67646._pm_r,  24._pm_r,  6._pm_r, 2.06_pm_r,290._pm_r,   5._pm_r,  9._pm_r,  .54_pm_r, 45._pm_r, &
       229.30_pm_r, 71028._pm_r,  24._pm_r,359._pm_r, 1.99_pm_r,275._pm_r,   6._pm_r, 14._pm_r,  .53_pm_r, 51._pm_r, &
       227.20_pm_r, 74370._pm_r,  24._pm_r,352._pm_r, 1.99_pm_r,262._pm_r,   6._pm_r, 18._pm_r,  .52_pm_r, 56._pm_r, &
       225.40_pm_r, 77686._pm_r,  24._pm_r,345._pm_r, 1.99_pm_r,254._pm_r,   7._pm_r, 22._pm_r,  .51_pm_r, 60._pm_r, &
       218.90_pm_r, 80974._pm_r,  24._pm_r,338._pm_r, 1.92_pm_r,248._pm_r,   8._pm_r, 26._pm_r,  .48_pm_r, 63._pm_r, &
       205.60_pm_r, 84067._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       192.80_pm_r, 86958._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.20_pm_r, 89706._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       181.30_pm_r, 92384._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       183.10_pm_r, 95067._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       189.60_pm_r, 97827._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       201.60_pm_r,100754._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       220.30_pm_r,103946._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       247.10_pm_r,107537._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       284.60_pm_r,111690._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       358.60_pm_r,116770._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/) 

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
       275.40_pm_r,  -133._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       252.20_pm_r,  3723._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       228.90_pm_r,  7238._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       220.40_pm_r, 10521._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       220.10_pm_r, 13740._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       218.40_pm_r, 16954._pm_r,  15._pm_r,225._pm_r, 1.11_pm_r,120._pm_r,   3._pm_r, 21._pm_r,  .20_pm_r,122._pm_r, &
       216.90_pm_r, 20139._pm_r,  14._pm_r,218._pm_r, 1.58_pm_r,107._pm_r,   3._pm_r, 28._pm_r,  .25_pm_r,119._pm_r, &
       217.40_pm_r, 23321._pm_r,  13._pm_r,208._pm_r, 2.09_pm_r, 89._pm_r,   3._pm_r, 37._pm_r,  .24_pm_r,112._pm_r, &
       215.10_pm_r, 26490._pm_r,  11._pm_r,193._pm_r, 2.67_pm_r, 69._pm_r,   3._pm_r, 43._pm_r,  .16_pm_r, 90._pm_r, &
       214.80_pm_r, 29633._pm_r,   9._pm_r,172._pm_r, 3.31_pm_r, 51._pm_r,   3._pm_r, 43._pm_r,  .14_pm_r, 10._pm_r, &
       219.20_pm_r, 32805._pm_r,   7._pm_r,135._pm_r, 3.83_pm_r, 35._pm_r,   3._pm_r, 38._pm_r,  .33_pm_r,333._pm_r, &
       226.60_pm_r, 36069._pm_r,   8._pm_r, 88._pm_r, 3.90_pm_r, 23._pm_r,   3._pm_r, 27._pm_r,  .51_pm_r,322._pm_r, &
       236.60_pm_r, 39455._pm_r,  11._pm_r, 59._pm_r, 3.51_pm_r, 12._pm_r,   4._pm_r, 15._pm_r,  .62_pm_r,316._pm_r, &
       248.00_pm_r, 43010._pm_r,  14._pm_r, 46._pm_r, 2.61_pm_r, 15._pm_r,   4._pm_r,  6._pm_r,  .35_pm_r,301._pm_r, &
       255.50_pm_r, 46700._pm_r,  17._pm_r, 40._pm_r, 2.22_pm_r, 14._pm_r,   4._pm_r,  2._pm_r,  .11_pm_r,229._pm_r, &
       254.40_pm_r, 50443._pm_r,  20._pm_r, 35._pm_r, 2.39_pm_r,360._pm_r,   4._pm_r,  2._pm_r,  .21_pm_r,146._pm_r, &
       247.90_pm_r, 54124._pm_r,  23._pm_r, 28._pm_r, 3.02_pm_r,341._pm_r,   4._pm_r,  7._pm_r,  .28_pm_r,109._pm_r, &
       243.60_pm_r, 57722._pm_r,  26._pm_r, 20._pm_r, 3.06_pm_r,323._pm_r,   4._pm_r, 12._pm_r,  .23_pm_r, 48._pm_r, &
       238.40_pm_r, 61254._pm_r,  28._pm_r, 12._pm_r, 2.84_pm_r,309._pm_r,   4._pm_r, 13._pm_r,  .35_pm_r, 15._pm_r, &
       233.70_pm_r, 64709._pm_r,  30._pm_r,  5._pm_r, 2.36_pm_r,296._pm_r,   5._pm_r, 13._pm_r,  .43_pm_r,  5._pm_r, &
       229.70_pm_r, 68103._pm_r,  31._pm_r,359._pm_r, 1.86_pm_r,281._pm_r,   5._pm_r, 12._pm_r,  .46_pm_r,  1._pm_r, &
       226.50_pm_r, 71442._pm_r,  31._pm_r,355._pm_r, 1.45_pm_r,262._pm_r,   6._pm_r, 10._pm_r,  .46_pm_r,  1._pm_r, &
       223.20_pm_r, 74736._pm_r,  30._pm_r,351._pm_r, 1.26_pm_r,240._pm_r,   7._pm_r, 10._pm_r,  .44_pm_r,  3._pm_r, &
       219.70_pm_r, 77977._pm_r,  29._pm_r,348._pm_r, 1.21_pm_r,222._pm_r,   7._pm_r,  9._pm_r,  .42_pm_r,  5._pm_r, &
       213.80_pm_r, 81170._pm_r,  28._pm_r,346._pm_r, 1.20_pm_r,209._pm_r,   8._pm_r,  9._pm_r,  .38_pm_r,  6._pm_r, &
       203.20_pm_r, 84214._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       192.50_pm_r, 87097._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.50_pm_r, 89848._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       181.40_pm_r, 92532._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       182.90_pm_r, 95217._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.40_pm_r, 97968._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       199.10_pm_r,100869._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       216.30_pm_r,104013._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       243.30_pm_r,107545._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       285.20_pm_r,111672._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       360.20_pm_r,116790._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
       281.10_pm_r,   -78._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       259.60_pm_r,  3877._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       234.80_pm_r,  7493._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.80_pm_r, 10817._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       217.70_pm_r, 14017._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       217.30_pm_r, 17203._pm_r,  11._pm_r,217._pm_r, 1.00_pm_r, 94._pm_r,   2._pm_r, 34._pm_r,  .05_pm_r,133._pm_r, &
       217.50_pm_r, 20385._pm_r,  10._pm_r,209._pm_r, 1.48_pm_r, 80._pm_r,   2._pm_r, 36._pm_r,  .03_pm_r,108._pm_r, &
       219.60_pm_r, 23588._pm_r,   8._pm_r,196._pm_r, 2.01_pm_r, 64._pm_r,   2._pm_r, 36._pm_r,  .06_pm_r,345._pm_r, &
       219.50_pm_r, 26805._pm_r,   6._pm_r,174._pm_r, 2.62_pm_r, 49._pm_r,   2._pm_r, 32._pm_r,  .17_pm_r,334._pm_r, &
       220.70_pm_r, 30025._pm_r,   4._pm_r,128._pm_r, 3.17_pm_r, 34._pm_r,   3._pm_r, 25._pm_r,  .31_pm_r,330._pm_r, &
       225.90_pm_r, 33289._pm_r,   6._pm_r, 73._pm_r, 3.47_pm_r, 22._pm_r,   3._pm_r, 16._pm_r,  .43_pm_r,328._pm_r, &
       234.60_pm_r, 36661._pm_r,  10._pm_r, 47._pm_r, 3.30_pm_r, 10._pm_r,   3._pm_r,  8._pm_r,  .49_pm_r,326._pm_r, &
       244.90_pm_r, 40168._pm_r,  13._pm_r, 34._pm_r, 2.75_pm_r,357._pm_r,   4._pm_r,  0._pm_r,  .49_pm_r,323._pm_r, &
       254.40_pm_r, 43830._pm_r,  16._pm_r, 27._pm_r, 1.81_pm_r,359._pm_r,   4._pm_r,355._pm_r,  .27_pm_r,295._pm_r, &
       258.80_pm_r, 47593._pm_r,  18._pm_r, 23._pm_r, 1.37_pm_r,  1._pm_r,   4._pm_r,351._pm_r,  .21_pm_r,217._pm_r, &
       255.30_pm_r, 51364._pm_r,  20._pm_r, 21._pm_r, 1.40_pm_r,346._pm_r,   4._pm_r,350._pm_r,  .30_pm_r,161._pm_r, &
       247.70_pm_r, 55050._pm_r,  22._pm_r, 16._pm_r, 1.92_pm_r,323._pm_r,   4._pm_r,355._pm_r,  .43_pm_r,111._pm_r, &
       240.40_pm_r, 58623._pm_r,  23._pm_r,  9._pm_r, 2.21_pm_r,299._pm_r,   4._pm_r,  6._pm_r,  .55_pm_r, 77._pm_r, &
       232.80_pm_r, 62089._pm_r,  24._pm_r,  1._pm_r, 2.29_pm_r,284._pm_r,   4._pm_r, 17._pm_r,  .64_pm_r, 62._pm_r, &
       226.90_pm_r, 65451._pm_r,  24._pm_r,354._pm_r, 2.07_pm_r,274._pm_r,   5._pm_r, 24._pm_r,  .60_pm_r, 56._pm_r, &
       223.00_pm_r, 68746._pm_r,  25._pm_r,347._pm_r, 1.71_pm_r,263._pm_r,   6._pm_r, 29._pm_r,  .50_pm_r, 52._pm_r, &
       220.10_pm_r, 71990._pm_r,  25._pm_r,342._pm_r, 1.35_pm_r,250._pm_r,   6._pm_r, 31._pm_r,  .37_pm_r, 50._pm_r, &
       217.60_pm_r, 75196._pm_r,  25._pm_r,338._pm_r, 1.09_pm_r,236._pm_r,   7._pm_r, 32._pm_r,  .26_pm_r, 49._pm_r, &
       215.20_pm_r, 78367._pm_r,  24._pm_r,335._pm_r,  .94_pm_r,220._pm_r,   7._pm_r, 33._pm_r,  .17_pm_r, 49._pm_r, &
       210.40_pm_r, 81501._pm_r,  24._pm_r,332._pm_r,  .85_pm_r,208._pm_r,   7._pm_r, 33._pm_r,  .12_pm_r, 43._pm_r, &
       201.60_pm_r, 84513._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       192.10_pm_r, 87386._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.80_pm_r, 90138._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       182.00_pm_r, 92832._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       183.00_pm_r, 95524._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.30_pm_r, 98272._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       196.30_pm_r,101146._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       211.50_pm_r,104236._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       238.10_pm_r,107691._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       284.60_pm_r,111770._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       362.00_pm_r,116920._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/) 

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
       288.90_pm_r,   -20._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       266.60_pm_r,  4046._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       240.70_pm_r,  7760._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       220.10_pm_r, 11133._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       213.50_pm_r, 14307._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       212.90_pm_r, 17425._pm_r,   4._pm_r,207._pm_r,  .44_pm_r, 97._pm_r,   2._pm_r, 82._pm_r,  .04_pm_r,247._pm_r, &
       215.50_pm_r, 20559._pm_r,   4._pm_r,197._pm_r,  .61_pm_r, 75._pm_r,   2._pm_r, 82._pm_r,  .07_pm_r,278._pm_r, &
       220.20_pm_r, 23753._pm_r,   3._pm_r,185._pm_r,  .86_pm_r, 48._pm_r,   2._pm_r, 79._pm_r,  .13_pm_r,299._pm_r, &
       222.50_pm_r, 26996._pm_r,   2._pm_r,162._pm_r, 1.25_pm_r, 28._pm_r,   1._pm_r, 72._pm_r,  .21_pm_r,311._pm_r, &
       225.70_pm_r, 30276._pm_r,   1._pm_r, 89._pm_r, 1.66_pm_r, 14._pm_r,   1._pm_r, 57._pm_r,  .31_pm_r,320._pm_r, &
       232.40_pm_r, 33626._pm_r,   3._pm_r, 34._pm_r, 1.90_pm_r,  4._pm_r,   1._pm_r, 36._pm_r,  .37_pm_r,326._pm_r, &
       241.20_pm_r, 37094._pm_r,   6._pm_r, 17._pm_r, 1.82_pm_r,354._pm_r,   2._pm_r, 18._pm_r,  .39_pm_r,332._pm_r, &
       251.30_pm_r, 40697._pm_r,   8._pm_r,  9._pm_r, 1.45_pm_r,343._pm_r,   2._pm_r,  8._pm_r,  .38_pm_r,338._pm_r, &
       260.40_pm_r, 44450._pm_r,   9._pm_r,  5._pm_r,  .77_pm_r,349._pm_r,   3._pm_r,  3._pm_r,  .15_pm_r,336._pm_r, &
       264.30_pm_r, 48299._pm_r,  10._pm_r,  4._pm_r,  .42_pm_r,355._pm_r,   3._pm_r,  3._pm_r,  .14_pm_r,144._pm_r, &
       258.80_pm_r, 52136._pm_r,  11._pm_r,  3._pm_r,  .45_pm_r,316._pm_r,   2._pm_r, 10._pm_r,  .38_pm_r,132._pm_r, &
       248.40_pm_r, 55854._pm_r,  11._pm_r,358._pm_r,  .97_pm_r,290._pm_r,   2._pm_r, 28._pm_r,  .54_pm_r,115._pm_r, &
       239.20_pm_r, 59422._pm_r,  12._pm_r,350._pm_r, 1.36_pm_r,269._pm_r,   2._pm_r, 46._pm_r,  .52_pm_r, 92._pm_r, &
       231.30_pm_r, 62868._pm_r,  12._pm_r,340._pm_r, 1.51_pm_r,258._pm_r,   3._pm_r, 54._pm_r,  .49_pm_r, 73._pm_r, &
       224.60_pm_r, 66205._pm_r,  12._pm_r,329._pm_r, 1.39_pm_r,250._pm_r,   4._pm_r, 56._pm_r,  .43_pm_r, 60._pm_r, &
       219.40_pm_r, 69456._pm_r,  13._pm_r,321._pm_r, 1.18_pm_r,242._pm_r,   4._pm_r, 56._pm_r,  .35_pm_r, 47._pm_r, &
       216.10_pm_r, 72643._pm_r,  13._pm_r,314._pm_r,  .95_pm_r,232._pm_r,   5._pm_r, 55._pm_r,  .28_pm_r, 38._pm_r, &
       213.70_pm_r, 75790._pm_r,  13._pm_r,308._pm_r,  .77_pm_r,220._pm_r,   5._pm_r, 53._pm_r,  .23_pm_r, 23._pm_r, &
       211.50_pm_r, 78902._pm_r,  13._pm_r,304._pm_r,  .67_pm_r,208._pm_r,   5._pm_r, 51._pm_r,  .20_pm_r, 10._pm_r, &
       207.60_pm_r, 81984._pm_r,  13._pm_r,300._pm_r,  .59_pm_r,197._pm_r,   6._pm_r, 49._pm_r,  .17_pm_r,  0._pm_r, &
       200.00_pm_r, 84966._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       191.60_pm_r, 87826._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.20_pm_r, 90580._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       183.00_pm_r, 93287._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       183.50_pm_r, 95994._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.50_pm_r, 98745._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       193.80_pm_r,101597._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       206.80_pm_r,104635._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       232.40_pm_r,108010._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       282.70_pm_r,112026._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       363.40_pm_r,117191._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
       295.00_pm_r,   -28._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       271.90_pm_r,  4125._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       246.40_pm_r,  7922._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       222.50_pm_r, 11358._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       208.30_pm_r, 14514._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       206.90_pm_r, 17539._pm_r,   3._pm_r,178._pm_r,  .17_pm_r,157._pm_r,   1._pm_r, 99._pm_r,  .08_pm_r,103._pm_r, &
       212.60_pm_r, 20608._pm_r,   3._pm_r,176._pm_r,  .12_pm_r,158._pm_r,   1._pm_r, 99._pm_r,  .09_pm_r,103._pm_r, &
       219.70_pm_r, 23778._pm_r,   3._pm_r,176._pm_r,  .04_pm_r,328._pm_r,   1._pm_r, 99._pm_r,  .06_pm_r, 99._pm_r, &
       224.50_pm_r, 27031._pm_r,   3._pm_r,178._pm_r,  .26_pm_r,333._pm_r,   1._pm_r, 99._pm_r,  .02_pm_r, 63._pm_r, &
       229.30_pm_r, 30353._pm_r,   2._pm_r,185._pm_r,  .50_pm_r,331._pm_r,   1._pm_r, 97._pm_r,  .05_pm_r,323._pm_r, &
       236.90_pm_r, 33761._pm_r,   2._pm_r,204._pm_r,  .67_pm_r,328._pm_r,   1._pm_r, 92._pm_r,  .12_pm_r,310._pm_r, &
       245.90_pm_r, 37297._pm_r,   1._pm_r,244._pm_r,  .69_pm_r,323._pm_r,   1._pm_r, 83._pm_r,  .16_pm_r,311._pm_r, &
       255.70_pm_r, 40966._pm_r,   2._pm_r,274._pm_r,  .57_pm_r,313._pm_r,   1._pm_r, 66._pm_r,  .19_pm_r,316._pm_r, &
       264.00_pm_r, 44779._pm_r,   2._pm_r,285._pm_r,  .27_pm_r,329._pm_r,   1._pm_r, 47._pm_r,  .13_pm_r,283._pm_r, &
       266.80_pm_r, 48672._pm_r,   3._pm_r,290._pm_r,  .20_pm_r,322._pm_r,   0._pm_r, 34._pm_r,  .16_pm_r,236._pm_r, &
       260.20_pm_r, 52537._pm_r,   3._pm_r,291._pm_r,  .36_pm_r,283._pm_r,   0._pm_r, 18._pm_r,  .15_pm_r,222._pm_r, &
       248.60_pm_r, 56269._pm_r,   4._pm_r,288._pm_r,  .67_pm_r,269._pm_r,   0._pm_r,349._pm_r,  .01_pm_r, 63._pm_r, &
       239.40_pm_r, 59836._pm_r,   5._pm_r,281._pm_r,  .77_pm_r,251._pm_r,   0._pm_r, 19._pm_r,  .26_pm_r, 32._pm_r, &
       232.50_pm_r, 63293._pm_r,   6._pm_r,274._pm_r,  .74_pm_r,239._pm_r,   1._pm_r, 28._pm_r,  .41_pm_r, 33._pm_r, &
       224.40_pm_r, 66639._pm_r,   6._pm_r,268._pm_r,  .62_pm_r,230._pm_r,   1._pm_r, 31._pm_r,  .43_pm_r, 38._pm_r, &
       217.20_pm_r, 69872._pm_r,   7._pm_r,264._pm_r,  .47_pm_r,219._pm_r,   2._pm_r, 34._pm_r,  .39_pm_r, 43._pm_r, &
       213.30_pm_r, 73022._pm_r,   7._pm_r,260._pm_r,  .32_pm_r,206._pm_r,   3._pm_r, 37._pm_r,  .34_pm_r, 52._pm_r, &
       210.50_pm_r, 76125._pm_r,   7._pm_r,258._pm_r,  .23_pm_r,186._pm_r,   3._pm_r, 40._pm_r,  .29_pm_r, 61._pm_r, &
       208.20_pm_r, 79188._pm_r,   8._pm_r,255._pm_r,  .19_pm_r,158._pm_r,   3._pm_r, 43._pm_r,  .25_pm_r, 70._pm_r, &
       204.70_pm_r, 82221._pm_r,   7._pm_r,253._pm_r,  .18_pm_r,136._pm_r,   4._pm_r, 46._pm_r,  .22_pm_r, 77._pm_r, &
       198.30_pm_r, 85174._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       191.10_pm_r, 88020._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.80_pm_r, 90776._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.30_pm_r, 93501._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.50_pm_r, 96227._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.50_pm_r, 98988._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       191.90_pm_r,101828._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       203.10_pm_r,104827._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       227.20_pm_r,108132._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       280.00_pm_r,112083._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       364.60_pm_r,117251._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
       299.00_pm_r,   -61._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       275.70_pm_r,  4152._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       251.30_pm_r,  8016._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       225.10_pm_r, 11509._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       203.60_pm_r, 14652._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       201.20_pm_r, 17589._pm_r,   3._pm_r,174._pm_r,  .08_pm_r,247._pm_r,   0._pm_r,344._pm_r,  .11_pm_r,105._pm_r, &
       210.00_pm_r, 20597._pm_r,   3._pm_r,177._pm_r,  .13_pm_r,252._pm_r,   0._pm_r, 12._pm_r,  .15_pm_r,110._pm_r, &
       219.10_pm_r, 23744._pm_r,   3._pm_r,182._pm_r,  .19_pm_r,253._pm_r,   0._pm_r, 48._pm_r,  .16_pm_r,112._pm_r, &
       225.50_pm_r, 26999._pm_r,   3._pm_r,188._pm_r,  .27_pm_r,255._pm_r,   1._pm_r, 70._pm_r,  .13_pm_r,117._pm_r, &
       231.80_pm_r, 30348._pm_r,   3._pm_r,195._pm_r,  .35_pm_r,254._pm_r,   1._pm_r, 82._pm_r,  .09_pm_r,133._pm_r, &
       240.40_pm_r, 33801._pm_r,   4._pm_r,203._pm_r,  .41_pm_r,252._pm_r,   1._pm_r, 89._pm_r,  .04_pm_r,180._pm_r, &
       250.50_pm_r, 37398._pm_r,   4._pm_r,210._pm_r,  .46_pm_r,247._pm_r,   1._pm_r, 93._pm_r,  .07_pm_r,257._pm_r, &
       260.70_pm_r, 41140._pm_r,   5._pm_r,215._pm_r,  .49_pm_r,242._pm_r,   1._pm_r, 94._pm_r,  .11_pm_r,275._pm_r, &
       267.40_pm_r, 45014._pm_r,   5._pm_r,217._pm_r,  .35_pm_r,231._pm_r,   0._pm_r, 96._pm_r,  .15_pm_r,270._pm_r, &
       267.80_pm_r, 48940._pm_r,   6._pm_r,218._pm_r,  .32_pm_r,234._pm_r,   0._pm_r,127._pm_r,  .19_pm_r,267._pm_r, &
       261.30_pm_r, 52818._pm_r,   6._pm_r,220._pm_r,  .36_pm_r,256._pm_r,   0._pm_r,264._pm_r,  .24_pm_r,275._pm_r, &
       250.30_pm_r, 56571._pm_r,   7._pm_r,224._pm_r,  .46_pm_r,277._pm_r,   1._pm_r,274._pm_r,  .28_pm_r,288._pm_r, &
       239.70_pm_r, 60155._pm_r,   7._pm_r,229._pm_r,  .43_pm_r,277._pm_r,   1._pm_r,283._pm_r,  .29_pm_r,306._pm_r, &
       230.40_pm_r, 63600._pm_r,   7._pm_r,232._pm_r,  .29_pm_r,260._pm_r,   1._pm_r,292._pm_r,  .26_pm_r,320._pm_r, &
       221.10_pm_r, 66903._pm_r,   8._pm_r,232._pm_r,  .23_pm_r,208._pm_r,   2._pm_r,299._pm_r,  .22_pm_r,340._pm_r, &
       214.20_pm_r, 70089._pm_r,   8._pm_r,230._pm_r,  .38_pm_r,166._pm_r,   2._pm_r,306._pm_r,  .19_pm_r,357._pm_r, &
       210.50_pm_r, 73195._pm_r,   8._pm_r,225._pm_r,  .58_pm_r,155._pm_r,   2._pm_r,313._pm_r,  .17_pm_r, 24._pm_r, &
       208.10_pm_r, 76260._pm_r,   9._pm_r,219._pm_r,  .73_pm_r,151._pm_r,   2._pm_r,320._pm_r,  .18_pm_r, 43._pm_r, &
       207.40_pm_r, 79298._pm_r,   9._pm_r,212._pm_r,  .83_pm_r,149._pm_r,   2._pm_r,327._pm_r,  .19_pm_r, 53._pm_r, &
       204.60_pm_r, 82332._pm_r,  10._pm_r,205._pm_r,  .85_pm_r,147._pm_r,   2._pm_r,334._pm_r,  .18_pm_r, 61._pm_r, &
       197.80_pm_r, 85277._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.70_pm_r, 88112._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.50_pm_r, 90874._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.70_pm_r, 93616._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.70_pm_r, 96365._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.00_pm_r, 99141._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.80_pm_r,101978._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.80_pm_r,104953._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       223.60_pm_r,108213._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       277.60_pm_r,112117._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       365.70_pm_r,117283._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/) 

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
       301.30_pm_r,   -99._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       276.90_pm_r,  4142._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       253.60_pm_r,  8034._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.30_pm_r, 11554._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.60_pm_r, 14685._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       198.00_pm_r, 17570._pm_r,   3._pm_r,165._pm_r,  .19_pm_r,312._pm_r,   1._pm_r,289._pm_r,  .16_pm_r,101._pm_r, &
       208.70_pm_r, 20546._pm_r,   2._pm_r,170._pm_r,  .24_pm_r,302._pm_r,   0._pm_r,296._pm_r,  .21_pm_r,102._pm_r, &
       218.30_pm_r, 23678._pm_r,   2._pm_r,177._pm_r,  .26_pm_r,285._pm_r,   0._pm_r, 58._pm_r,  .22_pm_r,103._pm_r, &
       225.20_pm_r, 26925._pm_r,   2._pm_r,187._pm_r,  .29_pm_r,263._pm_r,   0._pm_r, 94._pm_r,  .20_pm_r,105._pm_r, &
       232.90_pm_r, 30278._pm_r,   3._pm_r,197._pm_r,  .35_pm_r,240._pm_r,   1._pm_r, 99._pm_r,  .14_pm_r,109._pm_r, &
       243.50_pm_r, 33761._pm_r,   3._pm_r,203._pm_r,  .43_pm_r,225._pm_r,   1._pm_r,101._pm_r,  .06_pm_r,128._pm_r, &
       254.40_pm_r, 37411._pm_r,   4._pm_r,207._pm_r,  .52_pm_r,218._pm_r,   1._pm_r,104._pm_r,  .05_pm_r,241._pm_r, &
       264.10_pm_r, 41206._pm_r,   4._pm_r,208._pm_r,  .56_pm_r,214._pm_r,   1._pm_r,110._pm_r,  .13_pm_r,261._pm_r, &
       269.80_pm_r, 45123._pm_r,   5._pm_r,209._pm_r,  .43_pm_r,224._pm_r,   1._pm_r,125._pm_r,  .19_pm_r,254._pm_r, &
       269.70_pm_r, 49079._pm_r,   6._pm_r,212._pm_r,  .30_pm_r,261._pm_r,   0._pm_r,161._pm_r,  .19_pm_r,243._pm_r, &
       263.00_pm_r, 52984._pm_r,   6._pm_r,216._pm_r,  .27_pm_r,304._pm_r,   1._pm_r,187._pm_r,  .10_pm_r,237._pm_r, &
       251.40_pm_r, 56758._pm_r,   6._pm_r,219._pm_r,  .20_pm_r,345._pm_r,   1._pm_r,191._pm_r,  .05_pm_r, 72._pm_r, &
       239.00_pm_r, 60346._pm_r,   6._pm_r,219._pm_r,  .11_pm_r,107._pm_r,   0._pm_r,183._pm_r,  .09_pm_r, 32._pm_r, &
       227.50_pm_r, 63764._pm_r,   6._pm_r,216._pm_r,  .30_pm_r,136._pm_r,   0._pm_r,177._pm_r,  .08_pm_r,  0._pm_r, &
       217.90_pm_r, 67021._pm_r,   6._pm_r,211._pm_r,  .40_pm_r,142._pm_r,   0._pm_r,190._pm_r,  .09_pm_r,286._pm_r, &
       211.70_pm_r, 70164._pm_r,   6._pm_r,206._pm_r,  .43_pm_r,146._pm_r,   0._pm_r,224._pm_r,  .19_pm_r,253._pm_r, &
       209.70_pm_r, 73247._pm_r,   6._pm_r,201._pm_r,  .42_pm_r,149._pm_r,   1._pm_r,236._pm_r,  .31_pm_r,244._pm_r, &
       209.60_pm_r, 76316._pm_r,   7._pm_r,197._pm_r,  .41_pm_r,151._pm_r,   1._pm_r,239._pm_r,  .39_pm_r,241._pm_r, &
       211.00_pm_r, 79395._pm_r,   7._pm_r,193._pm_r,  .40_pm_r,153._pm_r,   2._pm_r,239._pm_r,  .46_pm_r,239._pm_r, &
       208.00_pm_r, 82497._pm_r,   8._pm_r,191._pm_r,  .37_pm_r,154._pm_r,   3._pm_r,239._pm_r,  .48_pm_r,238._pm_r, &
       198.70_pm_r, 85462._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.70_pm_r, 88293._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.20_pm_r, 91062._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.80_pm_r, 93819._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.80_pm_r, 96586._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.60_pm_r, 99376._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.70_pm_r,102219._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.00_pm_r,105188._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       222.10_pm_r,108434._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       276.70_pm_r,112319._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       367.00_pm_r,117490._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
       301.60_pm_r,  -126._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       277.10_pm_r,  4120._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       254.20_pm_r,  8017._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.90_pm_r, 11547._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.20_pm_r, 14680._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       197.10_pm_r, 17558._pm_r,   2._pm_r,153._pm_r,  .13_pm_r,299._pm_r,   1._pm_r,288._pm_r,  .16_pm_r, 94._pm_r, &
       207.90_pm_r, 20526._pm_r,   2._pm_r,156._pm_r,  .17_pm_r,287._pm_r,   0._pm_r,298._pm_r,  .20_pm_r, 96._pm_r, &
       217.90_pm_r, 23646._pm_r,   2._pm_r,163._pm_r,  .20_pm_r,269._pm_r,   0._pm_r,350._pm_r,  .20_pm_r, 96._pm_r, &
       225.00_pm_r, 26882._pm_r,   2._pm_r,174._pm_r,  .26_pm_r,249._pm_r,   0._pm_r, 69._pm_r,  .16_pm_r,101._pm_r, &
       232.40_pm_r, 30225._pm_r,   2._pm_r,188._pm_r,  .32_pm_r,234._pm_r,   0._pm_r, 86._pm_r,  .10_pm_r,110._pm_r, &
       244.50_pm_r, 33711._pm_r,   2._pm_r,198._pm_r,  .38_pm_r,223._pm_r,   0._pm_r, 96._pm_r,  .04_pm_r,166._pm_r, &
       255.90_pm_r, 37378._pm_r,   3._pm_r,203._pm_r,  .42_pm_r,216._pm_r,   0._pm_r,108._pm_r,  .09_pm_r,238._pm_r, &
       265.20_pm_r, 41190._pm_r,   3._pm_r,205._pm_r,  .44_pm_r,211._pm_r,   0._pm_r,135._pm_r,  .16_pm_r,248._pm_r, &
       270.60_pm_r, 45118._pm_r,   4._pm_r,206._pm_r,  .38_pm_r,219._pm_r,   0._pm_r,182._pm_r,  .22_pm_r,249._pm_r, &
       270.20_pm_r, 49084._pm_r,   5._pm_r,208._pm_r,  .30_pm_r,244._pm_r,   1._pm_r,206._pm_r,  .19_pm_r,245._pm_r, &
       263.90_pm_r, 52996._pm_r,   5._pm_r,211._pm_r,  .28_pm_r,275._pm_r,   1._pm_r,209._pm_r,  .04_pm_r,210._pm_r, &
       252.50_pm_r, 56778._pm_r,   5._pm_r,215._pm_r,  .25_pm_r,307._pm_r,   1._pm_r,196._pm_r,  .14_pm_r, 98._pm_r, &
       239.50_pm_r, 60371._pm_r,   5._pm_r,217._pm_r,  .18_pm_r,328._pm_r,   1._pm_r,178._pm_r,  .11_pm_r,120._pm_r, &
       226.90_pm_r, 63789._pm_r,   5._pm_r,217._pm_r,  .18_pm_r,  0._pm_r,   1._pm_r,176._pm_r,  .14_pm_r,201._pm_r, &
       216.60_pm_r, 67039._pm_r,   4._pm_r,217._pm_r,  .24_pm_r,  8._pm_r,   1._pm_r,191._pm_r,  .34_pm_r,230._pm_r, &
       210.10_pm_r, 70172._pm_r,   4._pm_r,217._pm_r,  .30_pm_r,  9._pm_r,   2._pm_r,207._pm_r,  .54_pm_r,237._pm_r, &
       208.40_pm_r, 73242._pm_r,   4._pm_r,219._pm_r,  .37_pm_r,  9._pm_r,   2._pm_r,218._pm_r,  .72_pm_r,241._pm_r, &
       209.40_pm_r, 76303._pm_r,   3._pm_r,222._pm_r,  .43_pm_r,  5._pm_r,   3._pm_r,225._pm_r,  .89_pm_r,242._pm_r, &
       212.00_pm_r, 79388._pm_r,   3._pm_r,227._pm_r,  .47_pm_r,  5._pm_r,   5._pm_r,229._pm_r, 1.00_pm_r,243._pm_r, &
       209.10_pm_r, 82503._pm_r,   3._pm_r,233._pm_r,  .48_pm_r,  5._pm_r,   6._pm_r,232._pm_r, 1.03_pm_r,243._pm_r, &
       198.90_pm_r, 85472._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.70_pm_r, 88301._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.60_pm_r, 91074._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.30_pm_r, 93838._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.30_pm_r, 96613._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.10_pm_r, 99411._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       191.30_pm_r,102262._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.80_pm_r,105241._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       223.00_pm_r,108501._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       277.90_pm_r,112403._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       368.60_pm_r,117596._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
       301.00_pm_r,  -114._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       277.20_pm_r,  4127._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       253.30_pm_r,  8018._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.30_pm_r, 11536._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       201.00_pm_r, 14670._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       198.30_pm_r, 17560._pm_r,   2._pm_r,156._pm_r,  .26_pm_r,342._pm_r,   0._pm_r,298._pm_r,  .11_pm_r, 52._pm_r, &
       208.40_pm_r, 20537._pm_r,   2._pm_r,156._pm_r,  .30_pm_r,334._pm_r,   0._pm_r, 19._pm_r,  .12_pm_r, 48._pm_r, &
       217.30_pm_r, 23658._pm_r,   2._pm_r,158._pm_r,  .26_pm_r,315._pm_r,   0._pm_r, 32._pm_r,  .10_pm_r, 41._pm_r, &
       224.30_pm_r, 26891._pm_r,   1._pm_r,168._pm_r,  .25_pm_r,279._pm_r,   0._pm_r, 32._pm_r,  .06_pm_r, 24._pm_r, &
       231.60_pm_r, 30228._pm_r,   1._pm_r,186._pm_r,  .33_pm_r,243._pm_r,   1._pm_r, 28._pm_r,  .04_pm_r,300._pm_r, &
       244.10_pm_r, 33705._pm_r,   2._pm_r,199._pm_r,  .45_pm_r,225._pm_r,   0._pm_r, 17._pm_r,  .09_pm_r,257._pm_r, &
       255.30_pm_r, 37368._pm_r,   3._pm_r,205._pm_r,  .54_pm_r,216._pm_r,   0._pm_r,357._pm_r,  .13_pm_r,243._pm_r, &
       264.50_pm_r, 41173._pm_r,   3._pm_r,207._pm_r,  .56_pm_r,212._pm_r,   0._pm_r,321._pm_r,  .15_pm_r,234._pm_r, &
       270.10_pm_r, 45094._pm_r,   4._pm_r,208._pm_r,  .41_pm_r,215._pm_r,   0._pm_r,285._pm_r,  .17_pm_r,236._pm_r, &
       269.80_pm_r, 49055._pm_r,   5._pm_r,210._pm_r,  .25_pm_r,242._pm_r,   1._pm_r,272._pm_r,  .11_pm_r,265._pm_r, &
       262.30_pm_r, 52956._pm_r,   5._pm_r,213._pm_r,  .21_pm_r,279._pm_r,   1._pm_r,280._pm_r,  .15_pm_r,  6._pm_r, &
       250.70_pm_r, 56719._pm_r,   5._pm_r,216._pm_r,  .20_pm_r,280._pm_r,   1._pm_r,309._pm_r,  .34_pm_r, 32._pm_r, &
       239.00_pm_r, 60301._pm_r,   5._pm_r,218._pm_r,  .15_pm_r,244._pm_r,   1._pm_r,346._pm_r,  .32_pm_r, 49._pm_r, &
       228.40_pm_r, 63726._pm_r,   5._pm_r,219._pm_r,  .13_pm_r,214._pm_r,   1._pm_r,  6._pm_r,  .19_pm_r, 78._pm_r, &
       219.00_pm_r, 66996._pm_r,   5._pm_r,218._pm_r,  .08_pm_r,191._pm_r,   1._pm_r, 18._pm_r,  .18_pm_r,164._pm_r, &
       211.90_pm_r, 70150._pm_r,   5._pm_r,218._pm_r,  .02_pm_r,117._pm_r,   1._pm_r, 27._pm_r,  .40_pm_r,196._pm_r, &
       208.50_pm_r, 73223._pm_r,   5._pm_r,218._pm_r,  .08_pm_r, 36._pm_r,   0._pm_r,189._pm_r,  .61_pm_r,205._pm_r, &
       208.30_pm_r, 76272._pm_r,   5._pm_r,218._pm_r,  .15_pm_r, 27._pm_r,   1._pm_r,204._pm_r,  .80_pm_r,208._pm_r, &
       209.30_pm_r, 79331._pm_r,   5._pm_r,219._pm_r,  .19_pm_r, 25._pm_r,   3._pm_r,207._pm_r,  .91_pm_r,210._pm_r, &
       206.10_pm_r, 82404._pm_r,   5._pm_r,220._pm_r,  .21_pm_r, 22._pm_r,   4._pm_r,208._pm_r,  .95_pm_r,212._pm_r, &
       197.40_pm_r, 85347._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       190.30_pm_r, 88169._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.10_pm_r, 90936._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.60_pm_r, 93690._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.80_pm_r, 96455._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.10_pm_r, 99248._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       192.30_pm_r,102106._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       202.80_pm_r,105108._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.20_pm_r,108407._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       281.30_pm_r,112361._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       370.80_pm_r,117597._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
       298.30_pm_r,  -131._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       275.60_pm_r,  4076._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       250.30_pm_r,  7932._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       224.80_pm_r, 11415._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       204.40_pm_r, 14562._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       202.10_pm_r, 17513._pm_r,   3._pm_r,148._pm_r,  .38_pm_r,303._pm_r,   1._pm_r,200._pm_r,  .28_pm_r, 41._pm_r, &
       210.10_pm_r, 20529._pm_r,   2._pm_r,155._pm_r,  .49_pm_r,298._pm_r,   0._pm_r,157._pm_r,  .36_pm_r, 38._pm_r, &
       218.00_pm_r, 23666._pm_r,   2._pm_r,170._pm_r,  .51_pm_r,291._pm_r,   1._pm_r, 65._pm_r,  .35_pm_r, 35._pm_r, &
       224.80_pm_r, 26907._pm_r,   2._pm_r,193._pm_r,  .49_pm_r,279._pm_r,   1._pm_r, 47._pm_r,  .31_pm_r, 27._pm_r, &
       232.80_pm_r, 30258._pm_r,   2._pm_r,212._pm_r,  .43_pm_r,261._pm_r,   1._pm_r, 40._pm_r,  .21_pm_r, 15._pm_r, &
       243.00_pm_r, 33737._pm_r,   3._pm_r,221._pm_r,  .40_pm_r,238._pm_r,   2._pm_r, 34._pm_r,  .11_pm_r,345._pm_r, &
       253.30_pm_r, 37375._pm_r,   3._pm_r,222._pm_r,  .41_pm_r,217._pm_r,   2._pm_r, 29._pm_r,  .10_pm_r,276._pm_r, &
       263.00_pm_r, 41154._pm_r,   4._pm_r,220._pm_r,  .42_pm_r,204._pm_r,   1._pm_r, 24._pm_r,  .14_pm_r,236._pm_r, &
       269.40_pm_r, 45061._pm_r,   4._pm_r,218._pm_r,  .32_pm_r,204._pm_r,   1._pm_r, 20._pm_r,  .12_pm_r,227._pm_r, &
       269.10_pm_r, 49012._pm_r,   5._pm_r,217._pm_r,  .16_pm_r,227._pm_r,   1._pm_r, 17._pm_r,  .04_pm_r,256._pm_r, &
       260.10_pm_r, 52891._pm_r,   5._pm_r,219._pm_r,  .13_pm_r,299._pm_r,   1._pm_r, 16._pm_r,  .11_pm_r, 18._pm_r, &
       247.90_pm_r, 56616._pm_r,   5._pm_r,221._pm_r,  .17_pm_r,339._pm_r,   1._pm_r, 17._pm_r,  .18_pm_r, 29._pm_r, &
       238.20_pm_r, 60169._pm_r,   5._pm_r,223._pm_r,  .10_pm_r,348._pm_r,   2._pm_r, 21._pm_r,  .10_pm_r, 72._pm_r, &
       230.90_pm_r, 63605._pm_r,   4._pm_r,224._pm_r,  .04_pm_r, 45._pm_r,   2._pm_r, 26._pm_r,  .16_pm_r,158._pm_r, &
       224.00_pm_r, 66934._pm_r,   4._pm_r,223._pm_r,  .07_pm_r,111._pm_r,   1._pm_r, 35._pm_r,  .32_pm_r,178._pm_r, &
       217.10_pm_r, 70166._pm_r,   4._pm_r,222._pm_r,  .10_pm_r,114._pm_r,   1._pm_r, 57._pm_r,  .46_pm_r,186._pm_r, &
       211.80_pm_r, 73303._pm_r,   4._pm_r,220._pm_r,  .11_pm_r,111._pm_r,   1._pm_r,119._pm_r,  .58_pm_r,189._pm_r, &
       209.00_pm_r, 76382._pm_r,   4._pm_r,218._pm_r,  .13_pm_r,113._pm_r,   1._pm_r,161._pm_r,  .66_pm_r,191._pm_r, &
       208.40_pm_r, 79438._pm_r,   4._pm_r,215._pm_r,  .13_pm_r,112._pm_r,   2._pm_r,175._pm_r,  .70_pm_r,193._pm_r, &
       204.50_pm_r, 82488._pm_r,   4._pm_r,213._pm_r,  .13_pm_r,113._pm_r,   3._pm_r,181._pm_r,  .70_pm_r,194._pm_r, &
       195.90_pm_r, 85408._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       188.70_pm_r, 88209._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.10_pm_r, 90949._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.40_pm_r, 93670._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.00_pm_r, 96403._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       187.60_pm_r, 99178._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       193.60_pm_r,102040._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       206.00_pm_r,105074._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       231.30_pm_r,108436._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       286.30_pm_r,112472._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       373.20_pm_r,117762._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
       
       293.00_pm_r,   -96._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       270.70_pm_r,  4034._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       244.80_pm_r,  7811._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       221.50_pm_r, 11228._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       209.60_pm_r, 14386._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       208.20_pm_r, 17433._pm_r,   6._pm_r,166._pm_r,  .73_pm_r,318._pm_r,   1._pm_r,173._pm_r,  .43_pm_r, 36._pm_r, &
       212.40_pm_r, 20510._pm_r,   4._pm_r,174._pm_r,  .97_pm_r,318._pm_r,   1._pm_r,116._pm_r,  .54_pm_r, 35._pm_r, &
       218.20_pm_r, 23663._pm_r,   3._pm_r,189._pm_r, 1.09_pm_r,317._pm_r,   1._pm_r, 67._pm_r,  .56_pm_r, 33._pm_r, &
       224.30_pm_r, 26900._pm_r,   3._pm_r,218._pm_r, 1.06_pm_r,315._pm_r,   2._pm_r, 52._pm_r,  .48_pm_r, 31._pm_r, &
       231.70_pm_r, 30240._pm_r,   3._pm_r,247._pm_r,  .86_pm_r,311._pm_r,   2._pm_r, 46._pm_r,  .31_pm_r, 25._pm_r, &
       241.90_pm_r, 33703._pm_r,   4._pm_r,262._pm_r,  .55_pm_r,303._pm_r,   3._pm_r, 43._pm_r,  .12_pm_r,358._pm_r, &
       252.50_pm_r, 37327._pm_r,   4._pm_r,266._pm_r,  .25_pm_r,272._pm_r,   3._pm_r, 41._pm_r,  .12_pm_r,253._pm_r, &
       262.60_pm_r, 41096._pm_r,   4._pm_r,264._pm_r,  .25_pm_r,195._pm_r,   2._pm_r, 38._pm_r,  .24_pm_r,234._pm_r, &
       269.60_pm_r, 45000._pm_r,   4._pm_r,258._pm_r,  .35_pm_r,185._pm_r,   2._pm_r, 35._pm_r,  .23_pm_r,251._pm_r, &
       269.30_pm_r, 48955._pm_r,   5._pm_r,252._pm_r,  .35_pm_r,191._pm_r,   2._pm_r, 25._pm_r,  .34_pm_r,272._pm_r, &
       260.50_pm_r, 52837._pm_r,   5._pm_r,247._pm_r,  .26_pm_r,192._pm_r,   2._pm_r,  6._pm_r,  .40_pm_r,279._pm_r, &
       248.80_pm_r, 56569._pm_r,   5._pm_r,244._pm_r,  .10_pm_r,169._pm_r,   2._pm_r,349._pm_r,  .27_pm_r,280._pm_r, &
       239.60_pm_r, 60140._pm_r,   5._pm_r,243._pm_r,  .13_pm_r,100._pm_r,   2._pm_r,343._pm_r,  .12_pm_r,179._pm_r, &
       233.20_pm_r, 63602._pm_r,   5._pm_r,242._pm_r,  .17_pm_r, 58._pm_r,   1._pm_r,346._pm_r,  .34_pm_r,147._pm_r, &
       226.60_pm_r, 66967._pm_r,   4._pm_r,244._pm_r,  .23_pm_r, 19._pm_r,   1._pm_r,359._pm_r,  .51_pm_r,150._pm_r, &
       220.30_pm_r, 70240._pm_r,   4._pm_r,249._pm_r,  .34_pm_r,354._pm_r,   0._pm_r, 71._pm_r,  .61_pm_r,157._pm_r, &
       215.10_pm_r, 73425._pm_r,   4._pm_r,256._pm_r,  .45_pm_r,342._pm_r,   1._pm_r,139._pm_r,  .68_pm_r,163._pm_r, &
       210.50_pm_r, 76543._pm_r,   4._pm_r,266._pm_r,  .53_pm_r,336._pm_r,   2._pm_r,152._pm_r,  .72_pm_r,168._pm_r, &
       207.00_pm_r, 79598._pm_r,   5._pm_r,275._pm_r,  .58_pm_r,331._pm_r,   3._pm_r,159._pm_r,  .73_pm_r,172._pm_r, &
       201.40_pm_r, 82606._pm_r,   5._pm_r,283._pm_r,  .58_pm_r,330._pm_r,   4._pm_r,162._pm_r,  .69_pm_r,174._pm_r, &
       192.60_pm_r, 85481._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.40_pm_r, 88235._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       181.30_pm_r, 90920._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       180.60_pm_r, 93584._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       182.20_pm_r, 96266._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       186.50_pm_r, 99009._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       195.10_pm_r,101873._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       210.10_pm_r,104949._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       238.10_pm_r,108394._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       292.10_pm_r,112531._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       375.50_pm_r,117878._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
       286.10_pm_r,   -49._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       264.00_pm_r,  3978._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       238.10_pm_r,  7653._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.00_pm_r, 11000._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       214.80_pm_r, 14175._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       214.40_pm_r, 17317._pm_r,   6._pm_r,212._pm_r,  .18_pm_r,149._pm_r,   3._pm_r,118._pm_r,  .53_pm_r,  1._pm_r, &
       215.30_pm_r, 20461._pm_r,   6._pm_r,210._pm_r,  .09_pm_r, 78._pm_r,   3._pm_r,100._pm_r,  .66_pm_r,359._pm_r, &
       218.00_pm_r, 23632._pm_r,   6._pm_r,211._pm_r,  .41_pm_r,  7._pm_r,   3._pm_r, 76._pm_r,  .67_pm_r,356._pm_r, &
       222.40_pm_r, 26853._pm_r,   5._pm_r,216._pm_r,  .91_pm_r,358._pm_r,   3._pm_r, 57._pm_r,  .53_pm_r,349._pm_r, &
       229.10_pm_r, 30158._pm_r,   4._pm_r,233._pm_r, 1.35_pm_r,355._pm_r,   3._pm_r, 45._pm_r,  .31_pm_r,329._pm_r, &
       240.70_pm_r, 33591._pm_r,   3._pm_r,266._pm_r, 1.53_pm_r,354._pm_r,   3._pm_r, 39._pm_r,  .20_pm_r,267._pm_r, &
       253.30_pm_r, 37213._pm_r,   4._pm_r,298._pm_r, 1.37_pm_r,353._pm_r,   3._pm_r, 36._pm_r,  .30_pm_r,225._pm_r, &
       264.10_pm_r, 41001._pm_r,   5._pm_r,314._pm_r,  .92_pm_r,353._pm_r,   2._pm_r, 36._pm_r,  .36_pm_r,212._pm_r, &
       270.60_pm_r, 44924._pm_r,   6._pm_r,319._pm_r,  .27_pm_r,344._pm_r,   2._pm_r, 35._pm_r,  .21_pm_r,229._pm_r, &
       270.70_pm_r, 48896._pm_r,   6._pm_r,317._pm_r,  .40_pm_r,205._pm_r,   2._pm_r, 30._pm_r,  .16_pm_r,277._pm_r, &
       262.70_pm_r, 52806._pm_r,   6._pm_r,309._pm_r,  .85_pm_r,202._pm_r,   2._pm_r, 20._pm_r,  .18_pm_r,310._pm_r, &
       251.10_pm_r, 56572._pm_r,   5._pm_r,295._pm_r,  .96_pm_r,205._pm_r,   2._pm_r, 12._pm_r,  .13_pm_r,321._pm_r, &
       241.60_pm_r, 60174._pm_r,   6._pm_r,280._pm_r,  .88_pm_r,196._pm_r,   2._pm_r, 11._pm_r,  .04_pm_r, 40._pm_r, &
       235.00_pm_r, 63664._pm_r,   6._pm_r,269._pm_r,  .62_pm_r,190._pm_r,   2._pm_r, 14._pm_r,  .09_pm_r,111._pm_r, &
       228.20_pm_r, 67054._pm_r,   6._pm_r,262._pm_r,  .27_pm_r,186._pm_r,   2._pm_r, 18._pm_r,  .11_pm_r,143._pm_r, &
       221.80_pm_r, 70351._pm_r,   6._pm_r,261._pm_r,  .09_pm_r,360._pm_r,   2._pm_r, 22._pm_r,  .11_pm_r,170._pm_r, &
       215.80_pm_r, 73553._pm_r,   6._pm_r,265._pm_r,  .38_pm_r,359._pm_r,   1._pm_r, 24._pm_r,  .14_pm_r,196._pm_r, &
       209.30_pm_r, 76669._pm_r,   6._pm_r,272._pm_r,  .59_pm_r,360._pm_r,   1._pm_r, 24._pm_r,  .16_pm_r,210._pm_r, &
       202.80_pm_r, 79684._pm_r,   6._pm_r,281._pm_r,  .69_pm_r,358._pm_r,   1._pm_r, 22._pm_r,  .18_pm_r,216._pm_r, &
       195.50_pm_r, 82610._pm_r,   6._pm_r,291._pm_r,  .72_pm_r,358._pm_r,   1._pm_r, 16._pm_r,  .18_pm_r,218._pm_r, &
       187.00_pm_r, 85404._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       180.10_pm_r, 88080._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       175.90_pm_r, 90683._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       175.70_pm_r, 93268._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       178.80_pm_r, 95885._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       185.20_pm_r, 98591._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       196.90_pm_r,101456._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       215.10_pm_r,104581._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       245.90_pm_r,108126._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       297.40_pm_r,112370._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       377.40_pm_r,117761._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
       280.40_pm_r,   -43._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       257.70_pm_r,  3893._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       233.00_pm_r,  7481._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       219.60_pm_r, 10792._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       218.70_pm_r, 13997._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       218.70_pm_r, 17201._pm_r,  10._pm_r,271._pm_r, 1.94_pm_r,163._pm_r,   5._pm_r, 93._pm_r,  .51_pm_r,319._pm_r, &
       218.60_pm_r, 20402._pm_r,   9._pm_r,252._pm_r, 2.32_pm_r,159._pm_r,   4._pm_r, 84._pm_r,  .69_pm_r,318._pm_r, &
       219.20_pm_r, 23606._pm_r,   9._pm_r,231._pm_r, 2.08_pm_r,151._pm_r,   4._pm_r, 70._pm_r,  .77_pm_r,314._pm_r, &
       221.60_pm_r, 26829._pm_r,  10._pm_r,216._pm_r, 1.33_pm_r,124._pm_r,   3._pm_r, 53._pm_r,  .71_pm_r,309._pm_r, &
       227.20_pm_r, 30112._pm_r,   9._pm_r,208._pm_r, 1.23_pm_r, 55._pm_r,   3._pm_r, 37._pm_r,  .53_pm_r,298._pm_r, &
       238.70_pm_r, 33516._pm_r,   6._pm_r,205._pm_r, 2.10_pm_r, 21._pm_r,   3._pm_r, 26._pm_r,  .33_pm_r,273._pm_r, &
       251.40_pm_r, 37110._pm_r,   3._pm_r,217._pm_r, 2.61_pm_r, 10._pm_r,   3._pm_r, 20._pm_r,  .25_pm_r,229._pm_r, &
       263.30_pm_r, 40876._pm_r,   2._pm_r,315._pm_r, 2.49_pm_r,  4._pm_r,   2._pm_r, 18._pm_r,  .28_pm_r,200._pm_r, &
       271.00_pm_r, 44798._pm_r,   5._pm_r,342._pm_r, 1.53_pm_r,350._pm_r,   2._pm_r, 18._pm_r,  .11_pm_r,193._pm_r, &
       270.80_pm_r, 48774._pm_r,   6._pm_r,339._pm_r,  .88_pm_r,293._pm_r,   2._pm_r, 16._pm_r,  .17_pm_r,321._pm_r, &
       263.50_pm_r, 52689._pm_r,   7._pm_r,326._pm_r, 1.26_pm_r,242._pm_r,   2._pm_r,  7._pm_r,  .46_pm_r,325._pm_r, &
       252.80_pm_r, 56475._pm_r,   7._pm_r,309._pm_r, 1.37_pm_r,227._pm_r,   3._pm_r,356._pm_r,  .69_pm_r,324._pm_r, &
       243.10_pm_r, 60100._pm_r,   7._pm_r,294._pm_r, 1.12_pm_r,207._pm_r,   4._pm_r,349._pm_r,  .56_pm_r,324._pm_r, &
       236.40_pm_r, 63610._pm_r,   7._pm_r,282._pm_r,  .84_pm_r,192._pm_r,   5._pm_r,345._pm_r,  .32_pm_r,331._pm_r, &
       230.20_pm_r, 67024._pm_r,   7._pm_r,273._pm_r,  .54_pm_r,186._pm_r,   5._pm_r,345._pm_r,  .11_pm_r, 29._pm_r, &
       224.10_pm_r, 70352._pm_r,   7._pm_r,269._pm_r,  .25_pm_r,198._pm_r,   5._pm_r,348._pm_r,  .27_pm_r,106._pm_r, &
       217.70_pm_r, 73586._pm_r,   7._pm_r,268._pm_r,  .15_pm_r,277._pm_r,   4._pm_r,354._pm_r,  .46_pm_r,117._pm_r, &
       209.00_pm_r, 76716._pm_r,   8._pm_r,269._pm_r,  .30_pm_r,312._pm_r,   4._pm_r,  3._pm_r,  .57_pm_r,119._pm_r, &
       200.00_pm_r, 79706._pm_r,   8._pm_r,272._pm_r,  .40_pm_r,319._pm_r,   4._pm_r, 15._pm_r,  .62_pm_r,122._pm_r, &
       190.40_pm_r, 82572._pm_r,   8._pm_r,275._pm_r,  .44_pm_r,321._pm_r,   4._pm_r, 29._pm_r,  .60_pm_r,122._pm_r, &
       180.50_pm_r, 85274._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       173.40_pm_r, 87847._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       169.60_pm_r, 90352._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       170.50_pm_r, 92850._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       175.30_pm_r, 95400._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       184.10_pm_r, 98066._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       198.90_pm_r,100935._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       220.90_pm_r,104116._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       253.90_pm_r,107767._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       301.20_pm_r,112109._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       378.80_pm_r,117523._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
       273.00_pm_r,    26._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       252.40_pm_r,  3865._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       229.00_pm_r,  7383._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       221.00_pm_r, 10671._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       221.30_pm_r, 13903._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       221.20_pm_r, 17145._pm_r,  12._pm_r,280._pm_r, 2.26_pm_r,169._pm_r,   6._pm_r, 59._pm_r,  .55_pm_r,245._pm_r, &
       221.10_pm_r, 20382._pm_r,  11._pm_r,261._pm_r, 2.95_pm_r,166._pm_r,   5._pm_r, 57._pm_r,  .72_pm_r,244._pm_r, &
       221.50_pm_r, 23622._pm_r,  11._pm_r,237._pm_r, 3.13_pm_r,160._pm_r,   4._pm_r, 56._pm_r,  .80_pm_r,242._pm_r, &
       222.20_pm_r, 26868._pm_r,  12._pm_r,217._pm_r, 2.56_pm_r,148._pm_r,   2._pm_r, 53._pm_r,  .69_pm_r,239._pm_r, &
       226.30_pm_r, 30149._pm_r,  13._pm_r,204._pm_r, 1.68_pm_r,118._pm_r,   2._pm_r, 51._pm_r,  .44_pm_r,232._pm_r, &
       235.60_pm_r, 33524._pm_r,  13._pm_r,195._pm_r, 1.59_pm_r, 63._pm_r,   1._pm_r, 54._pm_r,  .17_pm_r,211._pm_r, &
       247.10_pm_r, 37061._pm_r,  10._pm_r,187._pm_r, 2.12_pm_r, 32._pm_r,   1._pm_r, 60._pm_r,  .11_pm_r,111._pm_r, &
       259.30_pm_r, 40765._pm_r,   7._pm_r,180._pm_r, 2.36_pm_r, 17._pm_r,   1._pm_r, 64._pm_r,  .19_pm_r, 74._pm_r, &
       268.50_pm_r, 44640._pm_r,   4._pm_r,172._pm_r, 1.91_pm_r,  1._pm_r,   2._pm_r, 63._pm_r,  .18_pm_r, 45._pm_r, &
       270.60_pm_r, 48596._pm_r,   2._pm_r,177._pm_r, 1.43_pm_r,329._pm_r,   2._pm_r, 59._pm_r,  .20_pm_r, 15._pm_r, &
       263.80_pm_r, 52514._pm_r,   2._pm_r,252._pm_r, 1.33_pm_r,290._pm_r,   2._pm_r, 50._pm_r,  .28_pm_r,346._pm_r, &
       252.90_pm_r, 56302._pm_r,   3._pm_r,266._pm_r, 1.19_pm_r,264._pm_r,   2._pm_r, 37._pm_r,  .44_pm_r,328._pm_r, &
       243.90_pm_r, 59935._pm_r,   5._pm_r,261._pm_r,  .76_pm_r,233._pm_r,   2._pm_r, 21._pm_r,  .50_pm_r,313._pm_r, &
       236.90_pm_r, 63455._pm_r,   5._pm_r,254._pm_r,  .57_pm_r,187._pm_r,   3._pm_r,  7._pm_r,  .45_pm_r,303._pm_r, &
       231.10_pm_r, 66880._pm_r,   5._pm_r,245._pm_r,  .57_pm_r,150._pm_r,   3._pm_r,357._pm_r,  .30_pm_r,294._pm_r, &
       225.70_pm_r, 70227._pm_r,   5._pm_r,236._pm_r,  .57_pm_r,125._pm_r,   3._pm_r,351._pm_r,  .13_pm_r,274._pm_r, &
       219.00_pm_r, 73483._pm_r,   5._pm_r,228._pm_r,  .55_pm_r,106._pm_r,   3._pm_r,350._pm_r,  .11_pm_r,172._pm_r, &
       210.30_pm_r, 76632._pm_r,   4._pm_r,220._pm_r,  .51_pm_r, 91._pm_r,   3._pm_r,351._pm_r,  .21_pm_r,149._pm_r, &
       200.20_pm_r, 79637._pm_r,   4._pm_r,212._pm_r,  .49_pm_r, 81._pm_r,   3._pm_r,355._pm_r,  .28_pm_r,138._pm_r, &
       187.90_pm_r, 82498._pm_r,   3._pm_r,204._pm_r,  .45_pm_r, 73._pm_r,   2._pm_r,  2._pm_r,  .30_pm_r,135._pm_r, &
       174.60_pm_r, 85120._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       166.50_pm_r, 87584._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       163.60_pm_r, 89991._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       165.60_pm_r, 92407._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       172.00_pm_r, 94895._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       183.20_pm_r, 97526._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       200.90_pm_r,100401._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.60_pm_r,103636._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       261.10_pm_r,107388._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       303.10_pm_r,111805._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       379.70_pm_r,117222._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
       261.90_pm_r,   179._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       247.70_pm_r,  3900._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       226.10_pm_r,  7359._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       221.90_pm_r, 10631._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       222.80_pm_r, 13878._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       222.70_pm_r, 17141._pm_r,   8._pm_r,267._pm_r, 1.26_pm_r,173._pm_r,   4._pm_r, 43._pm_r,  .45_pm_r,211._pm_r, &
       223.00_pm_r, 20403._pm_r,   8._pm_r,251._pm_r, 1.77_pm_r,169._pm_r,   3._pm_r, 46._pm_r,  .62_pm_r,209._pm_r, &
       224.50_pm_r, 23681._pm_r,   9._pm_r,232._pm_r, 2.10_pm_r,163._pm_r,   2._pm_r, 54._pm_r,  .72_pm_r,207._pm_r, &
       223.90_pm_r, 26965._pm_r,  10._pm_r,215._pm_r, 2.10_pm_r,155._pm_r,   1._pm_r, 79._pm_r,  .70_pm_r,204._pm_r, &
       225.50_pm_r, 30253._pm_r,  12._pm_r,202._pm_r, 1.79_pm_r,141._pm_r,   1._pm_r,126._pm_r,  .56_pm_r,200._pm_r, &
       232.00_pm_r, 33596._pm_r,  13._pm_r,192._pm_r, 1.35_pm_r,116._pm_r,   1._pm_r,153._pm_r,  .35_pm_r,192._pm_r, &
       242.60_pm_r, 37071._pm_r,  13._pm_r,185._pm_r, 1.09_pm_r, 78._pm_r,   2._pm_r,159._pm_r,  .14_pm_r,172._pm_r, &
       254.70_pm_r, 40710._pm_r,  12._pm_r,178._pm_r, 1.14_pm_r, 40._pm_r,   2._pm_r,158._pm_r,  .05_pm_r, 68._pm_r, &
       264.90_pm_r, 44522._pm_r,  10._pm_r,174._pm_r, 1.15_pm_r, 10._pm_r,   2._pm_r,155._pm_r,  .12_pm_r, 24._pm_r, &
       269.70_pm_r, 48444._pm_r,   9._pm_r,174._pm_r, 1.11_pm_r,341._pm_r,   2._pm_r,150._pm_r,  .11_pm_r, 15._pm_r, &
       264.80_pm_r, 52366._pm_r,   7._pm_r,179._pm_r, 1.02_pm_r,313._pm_r,   2._pm_r,146._pm_r,  .11_pm_r,  8._pm_r, &
       254.40_pm_r, 56174._pm_r,   7._pm_r,189._pm_r,  .77_pm_r,283._pm_r,   1._pm_r,141._pm_r,  .16_pm_r,  4._pm_r, &
       244.30_pm_r, 59822._pm_r,   7._pm_r,197._pm_r,  .46_pm_r,259._pm_r,   1._pm_r,134._pm_r,  .21_pm_r,352._pm_r, &
       235.90_pm_r, 63338._pm_r,   7._pm_r,199._pm_r,  .24_pm_r,203._pm_r,   1._pm_r,121._pm_r,  .22_pm_r,351._pm_r, &
       230.90_pm_r, 66753._pm_r,   7._pm_r,198._pm_r,  .33_pm_r,134._pm_r,   1._pm_r,104._pm_r,  .20_pm_r,350._pm_r, &
       225.70_pm_r, 70100._pm_r,   8._pm_r,193._pm_r,  .49_pm_r,111._pm_r,   1._pm_r, 84._pm_r,  .15_pm_r,350._pm_r, &
       218.20_pm_r, 73351._pm_r,   8._pm_r,187._pm_r,  .59_pm_r, 98._pm_r,   1._pm_r, 70._pm_r,  .09_pm_r,355._pm_r, &
       209.20_pm_r, 76484._pm_r,   8._pm_r,180._pm_r,  .65_pm_r, 90._pm_r,   1._pm_r, 64._pm_r,  .04_pm_r,  7._pm_r, &
       199.50_pm_r, 79475._pm_r,   8._pm_r,173._pm_r,  .66_pm_r, 85._pm_r,   1._pm_r, 62._pm_r,  .01_pm_r, 76._pm_r, &
       185.90_pm_r, 82328._pm_r,   8._pm_r,166._pm_r,  .61_pm_r, 82._pm_r,   1._pm_r, 64._pm_r,  .02_pm_r,146._pm_r, &
       169.60_pm_r, 84878._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       160.60_pm_r, 87247._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       158.70_pm_r, 89574._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       161.70_pm_r, 91924._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       169.50_pm_r, 94363._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       182.60_pm_r, 96968._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       202.70_pm_r, 99849._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       231.50_pm_r,103133._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       266.60_pm_r,106967._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       303.30_pm_r,111430._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       380.30_pm_r,116838._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
       253.20_pm_r,   324._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       244.20_pm_r,  3954._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       224.20_pm_r,  7373._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       222.10_pm_r, 10630._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       223.30_pm_r, 13881._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       223.50_pm_r, 17154._pm_r,   3._pm_r,233._pm_r,  .52_pm_r,213._pm_r,   2._pm_r, 38._pm_r,  .20_pm_r,190._pm_r, &
       224.30_pm_r, 20430._pm_r,   4._pm_r,228._pm_r,  .72_pm_r,206._pm_r,   1._pm_r, 46._pm_r,  .28_pm_r,193._pm_r, &
       227.30_pm_r, 23742._pm_r,   5._pm_r,221._pm_r,  .86_pm_r,196._pm_r,   1._pm_r, 62._pm_r,  .32_pm_r,197._pm_r, &
       225.50_pm_r, 27061._pm_r,   6._pm_r,215._pm_r,  .90_pm_r,181._pm_r,   1._pm_r, 96._pm_r,  .31_pm_r,202._pm_r, &
       225.20_pm_r, 30359._pm_r,   7._pm_r,207._pm_r,  .87_pm_r,160._pm_r,   1._pm_r,140._pm_r,  .27_pm_r,211._pm_r, &
       230.20_pm_r, 33685._pm_r,   8._pm_r,199._pm_r,  .81_pm_r,135._pm_r,   1._pm_r,167._pm_r,  .21_pm_r,223._pm_r, &
       240.20_pm_r, 37129._pm_r,   8._pm_r,191._pm_r,  .75_pm_r,107._pm_r,   1._pm_r,182._pm_r,  .14_pm_r,244._pm_r, &
       251.80_pm_r, 40729._pm_r,   8._pm_r,183._pm_r,  .70_pm_r, 80._pm_r,   1._pm_r,192._pm_r,  .11_pm_r,265._pm_r, &
       262.50_pm_r, 44500._pm_r,   8._pm_r,178._pm_r,  .47_pm_r, 51._pm_r,   1._pm_r,198._pm_r,  .04_pm_r,247._pm_r, &
       268.80_pm_r, 48397._pm_r,   7._pm_r,175._pm_r,  .28_pm_r,354._pm_r,   1._pm_r,198._pm_r,  .07_pm_r,180._pm_r, &
       265.10_pm_r, 52316._pm_r,   7._pm_r,178._pm_r,  .43_pm_r,290._pm_r,   1._pm_r,196._pm_r,  .19_pm_r,187._pm_r, &
       255.00_pm_r, 56131._pm_r,   7._pm_r,185._pm_r,  .60_pm_r,272._pm_r,   2._pm_r,195._pm_r,  .38_pm_r,194._pm_r, &
       243.90_pm_r, 59782._pm_r,   7._pm_r,192._pm_r,  .52_pm_r,285._pm_r,   2._pm_r,195._pm_r,  .44_pm_r,196._pm_r, &
       234.80_pm_r, 63286._pm_r,   7._pm_r,198._pm_r,  .43_pm_r,308._pm_r,   3._pm_r,196._pm_r,  .38_pm_r,198._pm_r, &
       230.10_pm_r, 66687._pm_r,   6._pm_r,202._pm_r,  .40_pm_r,342._pm_r,   3._pm_r,196._pm_r,  .22_pm_r,203._pm_r, &
       225.40_pm_r, 70027._pm_r,   6._pm_r,205._pm_r,  .44_pm_r,  8._pm_r,   4._pm_r,197._pm_r,  .03_pm_r,270._pm_r, &
       217.50_pm_r, 73271._pm_r,   5._pm_r,206._pm_r,  .53_pm_r, 20._pm_r,   3._pm_r,198._pm_r,  .18_pm_r,  5._pm_r, &
       208.60_pm_r, 76394._pm_r,   4._pm_r,206._pm_r,  .57_pm_r, 26._pm_r,   3._pm_r,199._pm_r,  .32_pm_r,  9._pm_r, &
       199.20_pm_r, 79377._pm_r,   3._pm_r,206._pm_r,  .57_pm_r, 30._pm_r,   3._pm_r,201._pm_r,  .40_pm_r,  9._pm_r, &
       184.70_pm_r, 82227._pm_r,   3._pm_r,204._pm_r,  .52_pm_r, 32._pm_r,   2._pm_r,205._pm_r,  .40_pm_r, 12._pm_r, &
       166.40_pm_r, 84727._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       156.80_pm_r, 87034._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       155.60_pm_r, 89309._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       159.30_pm_r, 91617._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       168.00_pm_r, 94025._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       182.30_pm_r, 96615._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       203.90_pm_r, 99500._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       234.90_pm_r,102819._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       270.00_pm_r,106706._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       302.80_pm_r,111192._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
       380.60_pm_r,116587._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_avril = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_avril.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_avril,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_avril
