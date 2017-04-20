subroutine mpi_atmi_septembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de SEPTEMBRE 
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
       265.50_pm_r,  -422._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       240.20_pm_r,  3269._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       218.40_pm_r,  6616._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.80_pm_r,  9691._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       196.10_pm_r, 12602._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       193.60_pm_r, 15452._pm_r,  13._pm_r,270._pm_r, 2.66_pm_r,169._pm_r,   2._pm_r,232._pm_r,  .24_pm_r, 24._pm_r, & 
       194.40_pm_r, 18289._pm_r,  12._pm_r,246._pm_r, 4.79_pm_r,143._pm_r,   1._pm_r,242._pm_r,  .44_pm_r, 28._pm_r, & 
       197.70_pm_r, 21162._pm_r,  10._pm_r,199._pm_r, 7.04_pm_r,107._pm_r,   1._pm_r,274._pm_r,  .67_pm_r, 37._pm_r, & 
       198.50_pm_r, 24053._pm_r,  12._pm_r,144._pm_r, 7.28_pm_r, 81._pm_r,   1._pm_r,345._pm_r,  .49_pm_r, 32._pm_r, & 
       215.90_pm_r, 27077._pm_r,  19._pm_r,112._pm_r, 7.34_pm_r, 67._pm_r,   1._pm_r,  2._pm_r,  .28_pm_r, 21._pm_r, & 
       240.60_pm_r, 30414._pm_r,  27._pm_r, 94._pm_r, 7.27_pm_r, 57._pm_r,   2._pm_r,  4._pm_r,  .16_pm_r,349._pm_r, & 
       259.50_pm_r, 34089._pm_r,  35._pm_r, 83._pm_r, 6.73_pm_r, 49._pm_r,   2._pm_r,359._pm_r,  .17_pm_r,296._pm_r, & 
       271.50_pm_r, 37980._pm_r,  43._pm_r, 76._pm_r, 5.55_pm_r, 40._pm_r,   2._pm_r,349._pm_r,  .29_pm_r,272._pm_r, & 
       275.30_pm_r, 41987._pm_r,  48._pm_r, 70._pm_r, 3.60_pm_r, 26._pm_r,   2._pm_r,335._pm_r,  .35_pm_r,282._pm_r, & 
       277.90_pm_r, 46038._pm_r,  50._pm_r, 66._pm_r, 2.86_pm_r,348._pm_r,   2._pm_r,327._pm_r,  .31_pm_r,299._pm_r, & 
       277.80_pm_r, 50113._pm_r,  50._pm_r, 61._pm_r, 3.93_pm_r,317._pm_r,   3._pm_r,325._pm_r,  .15_pm_r,335._pm_r, & 
       272.60_pm_r, 54150._pm_r,  48._pm_r, 53._pm_r, 5.21_pm_r,306._pm_r,   3._pm_r,327._pm_r,  .15_pm_r,106._pm_r, & 
       258.70_pm_r, 58038._pm_r,  45._pm_r, 44._pm_r, 5.51_pm_r,291._pm_r,   3._pm_r,328._pm_r,  .34_pm_r,159._pm_r, & 
       244.90_pm_r, 61728._pm_r,  42._pm_r, 34._pm_r, 5.34_pm_r,279._pm_r,   2._pm_r,322._pm_r,  .51_pm_r,170._pm_r, & 
       234.80_pm_r, 65233._pm_r,  39._pm_r, 25._pm_r, 4.73_pm_r,269._pm_r,   1._pm_r,304._pm_r,  .57_pm_r,172._pm_r, & 
       227.80_pm_r, 68621._pm_r,  36._pm_r, 16._pm_r, 3.95_pm_r,258._pm_r,   1._pm_r,263._pm_r,  .55_pm_r,171._pm_r, & 
       222.20_pm_r, 71914._pm_r,  33._pm_r,  9._pm_r, 3.23_pm_r,246._pm_r,   1._pm_r,222._pm_r,  .49_pm_r,165._pm_r, & 
       217.70_pm_r, 75135._pm_r,  31._pm_r,  3._pm_r, 2.66_pm_r,233._pm_r,   2._pm_r,201._pm_r,  .43_pm_r,158._pm_r, & 
       213.80_pm_r, 78295._pm_r,  28._pm_r,358._pm_r, 2.26_pm_r,221._pm_r,   2._pm_r,189._pm_r,  .37_pm_r,152._pm_r, & 
       206.30_pm_r, 81399._pm_r,  26._pm_r,353._pm_r, 1.94_pm_r,210._pm_r,   2._pm_r,181._pm_r,  .32_pm_r,145._pm_r, & 
       194.20_pm_r, 84314._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       183.30_pm_r, 87053._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       176.40_pm_r, 89671._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       175.20_pm_r, 92246._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       179.00_pm_r, 94853._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       188.10_pm_r, 97570._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       203.20_pm_r,100494._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.70_pm_r,103738._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       254.90_pm_r,107432._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       290.80_pm_r,111698._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       367.90_pm_r,116894._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
       270.00_pm_r,  -230._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       243.60_pm_r,  3520._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       220.70_pm_r,  6910._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       205.10_pm_r, 10020._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       199.90_pm_r, 12977._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       198.60_pm_r, 15891._pm_r,  33._pm_r,233._pm_r, 4.92_pm_r,124._pm_r,   8._pm_r,307._pm_r,  .74_pm_r,104._pm_r, & 
       199.70_pm_r, 18806._pm_r,  30._pm_r,217._pm_r, 8.27_pm_r,111._pm_r,   6._pm_r,313._pm_r, 1.18_pm_r,100._pm_r, & 
       202.30_pm_r, 21748._pm_r,  27._pm_r,188._pm_r,10.76_pm_r, 94._pm_r,   5._pm_r,326._pm_r, 1.28_pm_r, 92._pm_r, & 
       207.10_pm_r, 24736._pm_r,  29._pm_r,155._pm_r,10.75_pm_r, 78._pm_r,   4._pm_r,345._pm_r,  .90_pm_r, 79._pm_r, & 
       222.70_pm_r, 27878._pm_r,  33._pm_r,128._pm_r,10.02_pm_r, 62._pm_r,   5._pm_r,357._pm_r,  .49_pm_r, 51._pm_r, & 
       242.20_pm_r, 31278._pm_r,  40._pm_r,109._pm_r, 9.18_pm_r, 47._pm_r,   5._pm_r,360._pm_r,  .40_pm_r,351._pm_r, & 
       257.40_pm_r, 34947._pm_r,  46._pm_r, 94._pm_r, 8.01_pm_r, 32._pm_r,   6._pm_r,356._pm_r,  .59_pm_r,314._pm_r, & 
       267.20_pm_r, 38791._pm_r,  50._pm_r, 82._pm_r, 6.40_pm_r, 13._pm_r,   6._pm_r,349._pm_r,  .81_pm_r,299._pm_r, & 
       270.20_pm_r, 42731._pm_r,  52._pm_r, 74._pm_r, 4.50_pm_r,347._pm_r,   7._pm_r,341._pm_r,  .84_pm_r,295._pm_r, & 
       271.10_pm_r, 46697._pm_r,  51._pm_r, 67._pm_r, 4.81_pm_r,317._pm_r,   8._pm_r,335._pm_r,  .82_pm_r,288._pm_r, & 
       268.30_pm_r, 50651._pm_r,  48._pm_r, 58._pm_r, 6.33_pm_r,308._pm_r,   9._pm_r,329._pm_r,  .68_pm_r,272._pm_r, & 
       261.70_pm_r, 54537._pm_r,  46._pm_r, 46._pm_r, 7.56_pm_r,312._pm_r,   9._pm_r,324._pm_r,  .50_pm_r,233._pm_r, & 
       249.90_pm_r, 58281._pm_r,  45._pm_r, 33._pm_r, 7.10_pm_r,303._pm_r,   9._pm_r,319._pm_r,  .58_pm_r,197._pm_r, & 
       239.20_pm_r, 61863._pm_r,  46._pm_r, 20._pm_r, 5.97_pm_r,294._pm_r,   8._pm_r,314._pm_r,  .69_pm_r,180._pm_r, & 
       232.00_pm_r, 65310._pm_r,  46._pm_r, 11._pm_r, 4.63_pm_r,281._pm_r,   8._pm_r,309._pm_r,  .68_pm_r,173._pm_r, & 
       227.10_pm_r, 68671._pm_r,  46._pm_r,  3._pm_r, 3.53_pm_r,263._pm_r,   7._pm_r,304._pm_r,  .60_pm_r,171._pm_r, & 
       222.90_pm_r, 71965._pm_r,  44._pm_r,358._pm_r, 2.94_pm_r,241._pm_r,   6._pm_r,298._pm_r,  .49_pm_r,172._pm_r, & 
       219.00_pm_r, 75202._pm_r,  42._pm_r,353._pm_r, 2.79_pm_r,221._pm_r,   6._pm_r,293._pm_r,  .38_pm_r,172._pm_r, & 
       214.60_pm_r, 78379._pm_r,  39._pm_r,350._pm_r, 2.79_pm_r,206._pm_r,   6._pm_r,289._pm_r,  .28_pm_r,173._pm_r, & 
       206.90_pm_r, 81489._pm_r,  36._pm_r,346._pm_r, 2.72_pm_r,197._pm_r,   6._pm_r,286._pm_r,  .20_pm_r,174._pm_r, & 
       195.10_pm_r, 84418._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       184.50_pm_r, 87175._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       177.40_pm_r, 89810._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       175.90_pm_r, 92399._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       179.30_pm_r, 95014._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.50_pm_r, 97729._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.60_pm_r,100639._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       223.00_pm_r,103851._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       252.60_pm_r,107506._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       291.80_pm_r,111762._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       368.60_pm_r,116985._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
       274.50_pm_r,   -63._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       249.30_pm_r,  3764._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.00_pm_r,  7230._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       210.30_pm_r, 10411._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       207.40_pm_r, 13463._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       207.20_pm_r, 16498._pm_r,  45._pm_r,220._pm_r, 7.27_pm_r, 94._pm_r,  13._pm_r,315._pm_r, 1.23_pm_r,121._pm_r, & 
       208.10_pm_r, 19537._pm_r,  39._pm_r,204._pm_r,10.43_pm_r, 89._pm_r,  11._pm_r,318._pm_r, 1.60_pm_r,121._pm_r, & 
       210.50_pm_r, 22601._pm_r,  34._pm_r,179._pm_r,12.19_pm_r, 82._pm_r,   8._pm_r,323._pm_r, 1.54_pm_r,121._pm_r, & 
       215.70_pm_r, 25716._pm_r,  35._pm_r,149._pm_r,11.57_pm_r, 72._pm_r,   7._pm_r,329._pm_r, 1.01_pm_r,122._pm_r, & 
       225.50_pm_r, 28946._pm_r,  40._pm_r,127._pm_r, 9.36_pm_r, 56._pm_r,   6._pm_r,333._pm_r,  .27_pm_r,130._pm_r, & 
       237.70_pm_r, 32334._pm_r,  43._pm_r,111._pm_r, 7.21_pm_r, 32._pm_r,   6._pm_r,331._pm_r,  .43_pm_r,292._pm_r, & 
       249.10_pm_r, 35904._pm_r,  43._pm_r, 98._pm_r, 6.04_pm_r,360._pm_r,   7._pm_r,326._pm_r,  .91_pm_r,295._pm_r, & 
       258.30_pm_r, 39620._pm_r,  41._pm_r, 87._pm_r, 5.86_pm_r,328._pm_r,   8._pm_r,320._pm_r, 1.16_pm_r,293._pm_r, & 
       262.80_pm_r, 43442._pm_r,  36._pm_r, 77._pm_r, 5.30_pm_r,309._pm_r,   9._pm_r,316._pm_r,  .95_pm_r,290._pm_r, & 
       263.40_pm_r, 47300._pm_r,  31._pm_r, 67._pm_r, 5.53_pm_r,301._pm_r,  10._pm_r,312._pm_r,  .77_pm_r,271._pm_r, & 
       258.10_pm_r, 51124._pm_r,  27._pm_r, 52._pm_r, 6.41_pm_r,302._pm_r,  11._pm_r,308._pm_r,  .70_pm_r,243._pm_r, & 
       250.40_pm_r, 54850._pm_r,  26._pm_r, 30._pm_r, 7.50_pm_r,309._pm_r,  11._pm_r,303._pm_r,  .72_pm_r,220._pm_r, & 
       242.00_pm_r, 58453._pm_r,  29._pm_r,  9._pm_r, 6.77_pm_r,302._pm_r,  11._pm_r,298._pm_r,  .59_pm_r,199._pm_r, & 
       235.80_pm_r, 61951._pm_r,  33._pm_r,354._pm_r, 5.39_pm_r,294._pm_r,  11._pm_r,294._pm_r,  .48_pm_r,184._pm_r, & 
       231.60_pm_r, 65370._pm_r,  37._pm_r,344._pm_r, 3.86_pm_r,283._pm_r,  11._pm_r,291._pm_r,  .35_pm_r,175._pm_r, & 
       228.60_pm_r, 68741._pm_r,  39._pm_r,338._pm_r, 2.64_pm_r,263._pm_r,  11._pm_r,289._pm_r,  .23_pm_r,178._pm_r, & 
       225.50_pm_r, 72067._pm_r,  39._pm_r,333._pm_r, 2.08_pm_r,233._pm_r,  11._pm_r,288._pm_r,  .12_pm_r,204._pm_r, & 
       221.60_pm_r, 75344._pm_r,  38._pm_r,329._pm_r, 2.08_pm_r,206._pm_r,  11._pm_r,287._pm_r,  .11_pm_r,260._pm_r, & 
       215.50_pm_r, 78547._pm_r,  36._pm_r,325._pm_r, 2.25_pm_r,189._pm_r,  11._pm_r,287._pm_r,  .14_pm_r,290._pm_r, & 
       207.20_pm_r, 81659._pm_r,  33._pm_r,321._pm_r, 2.27_pm_r,180._pm_r,  11._pm_r,287._pm_r,  .17_pm_r,303._pm_r, & 
       196.10_pm_r, 84601._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.00_pm_r, 87382._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       179.00_pm_r, 90042._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       177.10_pm_r, 92654._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       179.80_pm_r, 95285._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.80_pm_r, 98002._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       199.30_pm_r,100892._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       218.90_pm_r,104058._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       248.60_pm_r,107650._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       292.40_pm_r,111877._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       369.70_pm_r,117131._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
       279.00_pm_r,   -51._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       255.60_pm_r,  3859._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       230.50_pm_r,  7414._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       216.80_pm_r, 10686._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       215.60_pm_r, 13848._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       215.90_pm_r, 17009._pm_r,  33._pm_r,212._pm_r, 5.91_pm_r, 84._pm_r,   7._pm_r,312._pm_r,  .55_pm_r,129._pm_r, & 
       216.90_pm_r, 20175._pm_r,  28._pm_r,196._pm_r, 7.95_pm_r, 80._pm_r,   6._pm_r,313._pm_r,  .69_pm_r,129._pm_r, & 
       219.70_pm_r, 23373._pm_r,  25._pm_r,169._pm_r, 8.87_pm_r, 74._pm_r,   5._pm_r,313._pm_r,  .68_pm_r,133._pm_r, & 
       222.10_pm_r, 26607._pm_r,  26._pm_r,140._pm_r, 8.31_pm_r, 65._pm_r,   4._pm_r,312._pm_r,  .50_pm_r,140._pm_r, & 
       225.10_pm_r, 29879._pm_r,  29._pm_r,118._pm_r, 6.61_pm_r, 48._pm_r,   4._pm_r,311._pm_r,  .20_pm_r,162._pm_r, & 
       232.30_pm_r, 33223._pm_r,  31._pm_r,103._pm_r, 4.91_pm_r, 16._pm_r,   4._pm_r,308._pm_r,  .20_pm_r,283._pm_r, & 
       241.40_pm_r, 36694._pm_r,  30._pm_r, 91._pm_r, 4.65_pm_r,336._pm_r,   4._pm_r,307._pm_r,  .44_pm_r,306._pm_r, & 
       250.60_pm_r, 40295._pm_r,  26._pm_r, 79._pm_r, 5.22_pm_r,308._pm_r,   5._pm_r,308._pm_r,  .60_pm_r,316._pm_r, & 
       257.50_pm_r, 44024._pm_r,  21._pm_r, 65._pm_r, 4.53_pm_r,298._pm_r,   6._pm_r,310._pm_r,  .46_pm_r,325._pm_r, & 
       258.60_pm_r, 47811._pm_r,  18._pm_r, 50._pm_r, 4.08_pm_r,297._pm_r,   6._pm_r,311._pm_r,  .16_pm_r,311._pm_r, & 
       253.50_pm_r, 51565._pm_r,  17._pm_r, 30._pm_r, 4.29_pm_r,301._pm_r,   6._pm_r,309._pm_r,  .30_pm_r,191._pm_r, & 
       245.40_pm_r, 55222._pm_r,  19._pm_r,  9._pm_r, 5.00_pm_r,305._pm_r,   6._pm_r,303._pm_r,  .68_pm_r,193._pm_r, & 
       238.20_pm_r, 58758._pm_r,  22._pm_r,352._pm_r, 4.51_pm_r,297._pm_r,   6._pm_r,294._pm_r,  .64_pm_r,204._pm_r, & 
       234.10_pm_r, 62216._pm_r,  26._pm_r,341._pm_r, 3.62_pm_r,288._pm_r,   6._pm_r,286._pm_r,  .56_pm_r,219._pm_r, & 
       230.60_pm_r, 65619._pm_r,  29._pm_r,333._pm_r, 2.62_pm_r,276._pm_r,   7._pm_r,280._pm_r,  .48_pm_r,238._pm_r, & 
       227.30_pm_r, 68972._pm_r,  30._pm_r,327._pm_r, 1.78_pm_r,259._pm_r,   7._pm_r,277._pm_r,  .45_pm_r,259._pm_r, & 
       224.30_pm_r, 72279._pm_r,  30._pm_r,323._pm_r, 1.31_pm_r,231._pm_r,   8._pm_r,276._pm_r,  .45_pm_r,276._pm_r, & 
       219.10_pm_r, 75531._pm_r,  30._pm_r,320._pm_r, 1.22_pm_r,202._pm_r,   8._pm_r,277._pm_r,  .46_pm_r,288._pm_r, & 
       212.60_pm_r, 78688._pm_r,  29._pm_r,317._pm_r, 1.27_pm_r,183._pm_r,   9._pm_r,278._pm_r,  .47_pm_r,296._pm_r, & 
       205.40_pm_r, 81756._pm_r,  28._pm_r,315._pm_r, 1.29_pm_r,172._pm_r,  10._pm_r,279._pm_r,  .44_pm_r,301._pm_r, & 
       196.40_pm_r, 84694._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.60_pm_r, 87495._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       180.90_pm_r, 90185._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       179.00_pm_r, 92828._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       180.80_pm_r, 95482._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.20_pm_r, 98206._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       196.90_pm_r,101077._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       214.00_pm_r,104190._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       243.00_pm_r,107701._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       291.60_pm_r,111875._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       370.70_pm_r,117151._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
       285.40_pm_r,   -38._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       262.10_pm_r,  3969._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       236.40_pm_r,  7618._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       220.80_pm_r, 10965._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       217.00_pm_r, 14170._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       216.90_pm_r, 17345._pm_r,  17._pm_r,205._pm_r, 3.13_pm_r, 83._pm_r,   3._pm_r,347._pm_r,  .16_pm_r,166._pm_r, & 
       218.70_pm_r, 20532._pm_r,  15._pm_r,188._pm_r, 4.03_pm_r, 78._pm_r,   3._pm_r,346._pm_r,  .22_pm_r,175._pm_r, & 
       222.10_pm_r, 23763._pm_r,  13._pm_r,163._pm_r, 4.28_pm_r, 69._pm_r,   2._pm_r,344._pm_r,  .25_pm_r,186._pm_r, & 
       224.20_pm_r, 27030._pm_r,  14._pm_r,137._pm_r, 3.88_pm_r, 54._pm_r,   2._pm_r,339._pm_r,  .24_pm_r,199._pm_r, & 
       226.10_pm_r, 30326._pm_r,  14._pm_r,115._pm_r, 3.28_pm_r, 27._pm_r,   2._pm_r,331._pm_r,  .21_pm_r,221._pm_r, & 
       232.50_pm_r, 33676._pm_r,  14._pm_r, 97._pm_r, 3.20_pm_r,349._pm_r,   2._pm_r,324._pm_r,  .15_pm_r,266._pm_r, & 
       241.20_pm_r, 37147._pm_r,  12._pm_r, 76._pm_r, 3.66_pm_r,320._pm_r,   2._pm_r,321._pm_r,  .19_pm_r,325._pm_r, & 
       250.60_pm_r, 40745._pm_r,  10._pm_r, 47._pm_r, 3.93_pm_r,301._pm_r,   2._pm_r,324._pm_r,  .32_pm_r,356._pm_r, & 
       258.80_pm_r, 44481._pm_r,   9._pm_r, 15._pm_r, 3.08_pm_r,297._pm_r,   3._pm_r,332._pm_r,  .37_pm_r, 26._pm_r, & 
       260.40_pm_r, 48291._pm_r,  11._pm_r,354._pm_r, 2.40_pm_r,300._pm_r,   3._pm_r,342._pm_r,  .35_pm_r, 72._pm_r, & 
       254.50_pm_r, 52066._pm_r,  13._pm_r,343._pm_r, 2.11_pm_r,304._pm_r,   3._pm_r,352._pm_r,  .43_pm_r,125._pm_r, & 
       244.80_pm_r, 55727._pm_r,  16._pm_r,336._pm_r, 2.10_pm_r,306._pm_r,   2._pm_r,  0._pm_r,  .64_pm_r,166._pm_r, & 
       236.60_pm_r, 59246._pm_r,  18._pm_r,330._pm_r, 1.68_pm_r,286._pm_r,   1._pm_r,  3._pm_r,  .56_pm_r,188._pm_r, & 
       231.60_pm_r, 62673._pm_r,  19._pm_r,325._pm_r, 1.41_pm_r,265._pm_r,   1._pm_r,339._pm_r,  .50_pm_r,210._pm_r, & 
       227.70_pm_r, 66035._pm_r,  20._pm_r,320._pm_r, 1.23_pm_r,249._pm_r,   1._pm_r,266._pm_r,  .45_pm_r,228._pm_r, & 
       224.00_pm_r, 69343._pm_r,  21._pm_r,315._pm_r, 1.08_pm_r,239._pm_r,   1._pm_r,251._pm_r,  .41_pm_r,242._pm_r, & 
       220.20_pm_r, 72596._pm_r,  21._pm_r,311._pm_r,  .92_pm_r,232._pm_r,   2._pm_r,250._pm_r,  .36_pm_r,252._pm_r, & 
       215.30_pm_r, 75788._pm_r,  21._pm_r,308._pm_r,  .77_pm_r,228._pm_r,   2._pm_r,251._pm_r,  .30_pm_r,261._pm_r, & 
       210.00_pm_r, 78900._pm_r,  21._pm_r,305._pm_r,  .65_pm_r,224._pm_r,   3._pm_r,253._pm_r,  .28_pm_r,268._pm_r, & 
       204.20_pm_r, 81940._pm_r,  21._pm_r,303._pm_r,  .53_pm_r,223._pm_r,   3._pm_r,255._pm_r,  .24_pm_r,275._pm_r, & 
       196.60_pm_r, 84875._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       188.90_pm_r, 87692._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       182.90_pm_r, 90409._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       181.20_pm_r, 93086._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       182.20_pm_r, 95769._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.10_pm_r, 98507._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       194.60_pm_r,101361._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       209.10_pm_r,104423._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       236.80_pm_r,107849._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       289.20_pm_r,111951._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       371.40_pm_r,117234._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
       290.90_pm_r,   -24._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       268.70_pm_r,  4076._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       243.50_pm_r,  7829._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       223.00_pm_r, 11246._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       211.40_pm_r, 14429._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       210.70_pm_r, 17507._pm_r,   7._pm_r,194._pm_r, 1.28_pm_r, 82._pm_r,   1._pm_r, 15._pm_r,  .05_pm_r,171._pm_r, & 
       215.70_pm_r, 20629._pm_r,   7._pm_r,178._pm_r, 1.47_pm_r, 75._pm_r,   1._pm_r, 16._pm_r,  .08_pm_r,176._pm_r, & 
       220.40_pm_r, 23825._pm_r,   6._pm_r,160._pm_r, 1.28_pm_r, 59._pm_r,   1._pm_r, 19._pm_r,  .12_pm_r,178._pm_r, & 
       224.70_pm_r, 27082._pm_r,   6._pm_r,146._pm_r,  .98_pm_r, 21._pm_r,   1._pm_r, 24._pm_r,  .15_pm_r,176._pm_r, & 
       229.20_pm_r, 30405._pm_r,   5._pm_r,137._pm_r, 1.29_pm_r,329._pm_r,   1._pm_r, 33._pm_r,  .15_pm_r,172._pm_r, & 
       236.50_pm_r, 33810._pm_r,   2._pm_r,142._pm_r, 2.02_pm_r,303._pm_r,   1._pm_r, 47._pm_r,  .13_pm_r,164._pm_r, & 
       245.20_pm_r, 37338._pm_r,   2._pm_r,263._pm_r, 2.52_pm_r,290._pm_r,   1._pm_r, 63._pm_r,  .08_pm_r,128._pm_r, & 
       255.20_pm_r, 40998._pm_r,   6._pm_r,278._pm_r, 2.62_pm_r,279._pm_r,   1._pm_r, 68._pm_r,  .11_pm_r, 66._pm_r, & 
       263.40_pm_r, 44804._pm_r,   9._pm_r,278._pm_r, 1.86_pm_r,281._pm_r,   1._pm_r, 64._pm_r,  .16_pm_r, 48._pm_r, & 
       264.30_pm_r, 48676._pm_r,  11._pm_r,280._pm_r, 1.14_pm_r,293._pm_r,   1._pm_r, 61._pm_r,  .19_pm_r, 50._pm_r, & 
       257.80_pm_r, 52503._pm_r,  12._pm_r,282._pm_r,  .62_pm_r,312._pm_r,   1._pm_r, 60._pm_r,  .18_pm_r, 66._pm_r, & 
       246.70_pm_r, 56205._pm_r,  13._pm_r,284._pm_r,  .25_pm_r,311._pm_r,   2._pm_r, 63._pm_r,  .15_pm_r, 90._pm_r, & 
       236.40_pm_r, 59737._pm_r,  13._pm_r,282._pm_r,  .54_pm_r,197._pm_r,   2._pm_r, 66._pm_r,  .12_pm_r, 90._pm_r, & 
       229.60_pm_r, 63148._pm_r,  13._pm_r,277._pm_r,  .91_pm_r,191._pm_r,   2._pm_r, 67._pm_r,  .06_pm_r, 95._pm_r, & 
       224.10_pm_r, 66469._pm_r,  13._pm_r,271._pm_r,  .96_pm_r,192._pm_r,   2._pm_r, 68._pm_r,  .03_pm_r,197._pm_r, & 
       219.40_pm_r, 69716._pm_r,  13._pm_r,265._pm_r,  .85_pm_r,197._pm_r,   2._pm_r, 69._pm_r,  .09_pm_r,234._pm_r, & 
       215.70_pm_r, 72901._pm_r,  14._pm_r,261._pm_r,  .66_pm_r,206._pm_r,   2._pm_r, 71._pm_r,  .15_pm_r,239._pm_r, & 
       211.70_pm_r, 76032._pm_r,  15._pm_r,259._pm_r,  .51_pm_r,219._pm_r,   1._pm_r, 73._pm_r,  .18_pm_r,239._pm_r, & 
       207.90_pm_r, 79103._pm_r,  15._pm_r,257._pm_r,  .41_pm_r,234._pm_r,   1._pm_r, 76._pm_r,  .20_pm_r,240._pm_r, & 
       203.30_pm_r, 82123._pm_r,  16._pm_r,257._pm_r,  .34_pm_r,249._pm_r,   1._pm_r, 82._pm_r,  .20_pm_r,240._pm_r, & 
       196.60_pm_r, 85053._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       189.80_pm_r, 87878._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       184.70_pm_r, 90618._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       183.40_pm_r, 93328._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       183.90_pm_r, 96043._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.50_pm_r, 98798._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       192.70_pm_r,101644._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       205.20_pm_r,104664._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       230.90_pm_r,108013._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       285.90_pm_r,112040._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       371.80_pm_r,117315._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
       295.80_pm_r,   -26._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       274.20_pm_r,  4153._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       249.90_pm_r,  7995._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.40_pm_r, 11473._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       205.40_pm_r, 14624._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       204.40_pm_r, 17600._pm_r,   4._pm_r,176._pm_r,  .24_pm_r, 31._pm_r,   1._pm_r, 29._pm_r,  .11_pm_r, 41._pm_r, & 
       212.60_pm_r, 20654._pm_r,   3._pm_r,173._pm_r,  .25_pm_r,  7._pm_r,   1._pm_r, 32._pm_r,  .15_pm_r, 48._pm_r, & 
       218.40_pm_r, 23812._pm_r,   3._pm_r,175._pm_r,  .28_pm_r,316._pm_r,   1._pm_r, 37._pm_r,  .17_pm_r, 57._pm_r, & 
       224.10_pm_r, 27049._pm_r,   3._pm_r,185._pm_r,  .52_pm_r,281._pm_r,   1._pm_r, 42._pm_r,  .18_pm_r, 71._pm_r, & 
       230.90_pm_r, 30380._pm_r,   3._pm_r,205._pm_r,  .85_pm_r,266._pm_r,   2._pm_r, 47._pm_r,  .18_pm_r, 83._pm_r, & 
       240.30_pm_r, 33826._pm_r,   4._pm_r,223._pm_r, 1.13_pm_r,257._pm_r,   2._pm_r, 53._pm_r,  .16_pm_r,104._pm_r, & 
       250.50_pm_r, 37422._pm_r,   6._pm_r,233._pm_r, 1.29_pm_r,251._pm_r,   2._pm_r, 59._pm_r,  .14_pm_r,124._pm_r, & 
       260.50_pm_r, 41161._pm_r,   7._pm_r,237._pm_r, 1.28_pm_r,245._pm_r,   2._pm_r, 65._pm_r,  .13_pm_r,148._pm_r, & 
       267.50_pm_r, 45036._pm_r,   9._pm_r,239._pm_r,  .89_pm_r,251._pm_r,   2._pm_r, 69._pm_r,  .09_pm_r,232._pm_r, & 
       267.50_pm_r, 48961._pm_r,  10._pm_r,240._pm_r,  .51_pm_r,267._pm_r,   2._pm_r, 66._pm_r,  .27_pm_r,274._pm_r, & 
       260.30_pm_r, 52829._pm_r,  10._pm_r,242._pm_r,  .17_pm_r,291._pm_r,   1._pm_r, 56._pm_r,  .31_pm_r,275._pm_r, & 
       248.20_pm_r, 56560._pm_r,  10._pm_r,242._pm_r,  .20_pm_r,101._pm_r,   1._pm_r, 46._pm_r,  .14_pm_r,231._pm_r, & 
       236.30_pm_r, 60101._pm_r,  10._pm_r,240._pm_r,  .47_pm_r,129._pm_r,   1._pm_r, 61._pm_r,  .28_pm_r,145._pm_r, & 
       228.10_pm_r, 63501._pm_r,  10._pm_r,235._pm_r,  .55_pm_r,134._pm_r,   1._pm_r, 87._pm_r,  .37_pm_r,131._pm_r, & 
       221.40_pm_r, 66790._pm_r,  10._pm_r,231._pm_r,  .45_pm_r,136._pm_r,   2._pm_r, 99._pm_r,  .27_pm_r,124._pm_r, & 
       215.70_pm_r, 69992._pm_r,  10._pm_r,228._pm_r,  .25_pm_r,135._pm_r,   2._pm_r,101._pm_r,  .09_pm_r, 90._pm_r, & 
       211.40_pm_r, 73116._pm_r,  10._pm_r,227._pm_r,  .04_pm_r,120._pm_r,   2._pm_r, 98._pm_r,  .17_pm_r,333._pm_r, & 
       208.40_pm_r, 76191._pm_r,  10._pm_r,227._pm_r,  .14_pm_r,326._pm_r,   2._pm_r, 87._pm_r,  .34_pm_r,322._pm_r, & 
       206.40_pm_r, 79228._pm_r,  10._pm_r,229._pm_r,  .27_pm_r,322._pm_r,   1._pm_r, 63._pm_r,  .47_pm_r,320._pm_r, & 
       202.90_pm_r, 82236._pm_r,  10._pm_r,232._pm_r,  .34_pm_r,322._pm_r,   1._pm_r, 30._pm_r,  .52_pm_r,318._pm_r, & 
       196.60_pm_r, 85163._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       190.30_pm_r, 87990._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.20_pm_r, 90747._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       185.40_pm_r, 93485._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       185.50_pm_r, 96230._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.10_pm_r, 99005._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       191.60_pm_r,101849._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.40_pm_r,104841._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.40_pm_r,108137._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       282.60_pm_r,112101._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       372.10_pm_r,117361._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
       299.10_pm_r,   -46._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       276.10_pm_r,  4173._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       252.50_pm_r,  8051._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.10_pm_r, 11554._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.60_pm_r, 14691._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.50_pm_r, 17620._pm_r,   3._pm_r,178._pm_r,  .18_pm_r,316._pm_r,   1._pm_r,  2._pm_r,  .10_pm_r, 42._pm_r, & 
       211.10_pm_r, 20643._pm_r,   2._pm_r,183._pm_r,  .23_pm_r,306._pm_r,   1._pm_r,  9._pm_r,  .14_pm_r, 44._pm_r, & 
       217.00_pm_r, 23780._pm_r,   2._pm_r,191._pm_r,  .23_pm_r,289._pm_r,   1._pm_r, 16._pm_r,  .15_pm_r, 48._pm_r, & 
       223.90_pm_r, 27005._pm_r,   2._pm_r,200._pm_r,  .25_pm_r,265._pm_r,   1._pm_r, 21._pm_r,  .15_pm_r, 55._pm_r, & 
       231.10_pm_r, 30336._pm_r,   3._pm_r,207._pm_r,  .31_pm_r,242._pm_r,   2._pm_r, 26._pm_r,  .13_pm_r, 64._pm_r, & 
       241.60_pm_r, 33791._pm_r,   3._pm_r,211._pm_r,  .39_pm_r,226._pm_r,   2._pm_r, 30._pm_r,  .10_pm_r, 81._pm_r, & 
       252.30_pm_r, 37411._pm_r,   4._pm_r,213._pm_r,  .46_pm_r,218._pm_r,   2._pm_r, 34._pm_r,  .07_pm_r,117._pm_r, & 
       262.20_pm_r, 41176._pm_r,   4._pm_r,213._pm_r,  .51_pm_r,215._pm_r,   2._pm_r, 38._pm_r,  .08_pm_r,152._pm_r, & 
       268.60_pm_r, 45070._pm_r,   5._pm_r,215._pm_r,  .54_pm_r,232._pm_r,   2._pm_r, 41._pm_r,  .06_pm_r,187._pm_r, & 
       268.70_pm_r, 49011._pm_r,   6._pm_r,218._pm_r,  .59_pm_r,247._pm_r,   2._pm_r, 41._pm_r,  .04_pm_r,240._pm_r, & 
       262.50_pm_r, 52905._pm_r,   7._pm_r,222._pm_r,  .41_pm_r,257._pm_r,   2._pm_r, 40._pm_r,  .05_pm_r,235._pm_r, & 
       250.90_pm_r, 56672._pm_r,   7._pm_r,224._pm_r,  .10_pm_r, 39._pm_r,   1._pm_r, 41._pm_r,  .15_pm_r,199._pm_r, & 
       238.30_pm_r, 60248._pm_r,   6._pm_r,222._pm_r,  .52_pm_r, 76._pm_r,   1._pm_r, 48._pm_r,  .28_pm_r,198._pm_r, & 
       228.10_pm_r, 63664._pm_r,   6._pm_r,216._pm_r,  .77_pm_r, 79._pm_r,   1._pm_r, 68._pm_r,  .35_pm_r,202._pm_r, & 
       219.10_pm_r, 66935._pm_r,   5._pm_r,206._pm_r,  .81_pm_r, 81._pm_r,   0._pm_r,120._pm_r,  .35_pm_r,214._pm_r, & 
       212.00_pm_r, 70091._pm_r,   4._pm_r,193._pm_r,  .73_pm_r, 83._pm_r,   1._pm_r,176._pm_r,  .34_pm_r,228._pm_r, & 
       208.20_pm_r, 73164._pm_r,   4._pm_r,180._pm_r,  .64_pm_r, 86._pm_r,   1._pm_r,203._pm_r,  .32_pm_r,244._pm_r, & 
       206.10_pm_r, 76198._pm_r,   4._pm_r,168._pm_r,  .54_pm_r, 89._pm_r,   1._pm_r,219._pm_r,  .34_pm_r,256._pm_r, & 
       205.60_pm_r, 79211._pm_r,   4._pm_r,158._pm_r,  .47_pm_r, 92._pm_r,   2._pm_r,231._pm_r,  .36_pm_r,266._pm_r, & 
       203.10_pm_r, 82219._pm_r,   5._pm_r,151._pm_r,  .41_pm_r, 95._pm_r,   2._pm_r,239._pm_r,  .36_pm_r,270._pm_r, & 
       196.80_pm_r, 85147._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       190.60_pm_r, 87976._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.20_pm_r, 90744._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.80_pm_r, 93501._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.80_pm_r, 96267._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.70_pm_r, 99058._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       191.20_pm_r,101906._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.10_pm_r,104886._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.10_pm_r,108156._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       280.60_pm_r,112085._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       372.40_pm_r,117335._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
       300.20_pm_r,  -100._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       276.40_pm_r,  4130._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       253.20_pm_r,  8015._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.60_pm_r, 11527._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.50_pm_r, 14668._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.00_pm_r, 17593._pm_r,   2._pm_r,177._pm_r,  .16_pm_r,290._pm_r,   1._pm_r,342._pm_r,  .10_pm_r, 35._pm_r, & 
       210.40_pm_r, 20609._pm_r,   2._pm_r,185._pm_r,  .21_pm_r,287._pm_r,   1._pm_r,357._pm_r,  .14_pm_r, 36._pm_r, & 
       216.20_pm_r, 23733._pm_r,   2._pm_r,194._pm_r,  .23_pm_r,279._pm_r,   1._pm_r,  9._pm_r,  .16_pm_r, 40._pm_r, & 
       223.40_pm_r, 26944._pm_r,   2._pm_r,204._pm_r,  .23_pm_r,268._pm_r,   1._pm_r, 18._pm_r,  .15_pm_r, 42._pm_r, & 
       230.80_pm_r, 30268._pm_r,   2._pm_r,211._pm_r,  .23_pm_r,252._pm_r,   1._pm_r, 24._pm_r,  .13_pm_r, 48._pm_r, & 
       240.90_pm_r, 33718._pm_r,   2._pm_r,215._pm_r,  .23_pm_r,232._pm_r,   1._pm_r, 29._pm_r,  .09_pm_r, 58._pm_r, & 
       251.90_pm_r, 37328._pm_r,   3._pm_r,215._pm_r,  .24_pm_r,217._pm_r,   1._pm_r, 33._pm_r,  .05_pm_r, 84._pm_r, & 
       262.30_pm_r, 41089._pm_r,   3._pm_r,215._pm_r,  .27_pm_r,207._pm_r,   1._pm_r, 35._pm_r,  .04_pm_r,153._pm_r, & 
       268.90_pm_r, 44982._pm_r,   4._pm_r,215._pm_r,  .36_pm_r,235._pm_r,   1._pm_r, 39._pm_r,  .10_pm_r,159._pm_r, & 
       269.20_pm_r, 48926._pm_r,   4._pm_r,221._pm_r,  .60_pm_r,255._pm_r,   1._pm_r, 45._pm_r,  .17_pm_r,144._pm_r, & 
       263.30_pm_r, 52827._pm_r,   5._pm_r,228._pm_r,  .62_pm_r,264._pm_r,   1._pm_r, 53._pm_r,  .15_pm_r,148._pm_r, & 
       252.20_pm_r, 56605._pm_r,   5._pm_r,233._pm_r,  .23_pm_r,275._pm_r,   1._pm_r, 59._pm_r,  .11_pm_r,225._pm_r, & 
       239.60_pm_r, 60198._pm_r,   5._pm_r,234._pm_r,  .35_pm_r, 86._pm_r,   1._pm_r, 58._pm_r,  .27_pm_r,256._pm_r, & 
       228.20_pm_r, 63628._pm_r,   5._pm_r,230._pm_r,  .70_pm_r, 90._pm_r,   0._pm_r, 54._pm_r,  .40_pm_r,248._pm_r, & 
       218.90_pm_r, 66905._pm_r,   4._pm_r,220._pm_r,  .81_pm_r, 94._pm_r,   0._pm_r,227._pm_r,  .46_pm_r,234._pm_r, & 
       211.90_pm_r, 70067._pm_r,   3._pm_r,204._pm_r,  .76_pm_r, 98._pm_r,   1._pm_r,220._pm_r,  .54_pm_r,215._pm_r, & 
       207.80_pm_r, 73145._pm_r,   3._pm_r,185._pm_r,  .66_pm_r,104._pm_r,   2._pm_r,212._pm_r,  .62_pm_r,203._pm_r, & 
       205.90_pm_r, 76179._pm_r,   4._pm_r,168._pm_r,  .57_pm_r,109._pm_r,   2._pm_r,205._pm_r,  .73_pm_r,194._pm_r, & 
       206.20_pm_r, 79194._pm_r,   4._pm_r,157._pm_r,  .51_pm_r,116._pm_r,   3._pm_r,201._pm_r,  .80_pm_r,190._pm_r, & 
       204.00_pm_r, 82216._pm_r,   5._pm_r,150._pm_r,  .44_pm_r,121._pm_r,   4._pm_r,197._pm_r,  .84_pm_r,187._pm_r, & 
       197.10_pm_r, 85150._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       190.80_pm_r, 87978._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.60_pm_r, 90751._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.30_pm_r, 93515._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.30_pm_r, 96290._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       188.10_pm_r, 99088._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       191.40_pm_r,101940._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.20_pm_r,104923._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.00_pm_r,108193._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       280.70_pm_r,112123._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       373.20_pm_r,117380._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
       301.20_pm_r,  -115._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       277.00_pm_r,  4126._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       253.40_pm_r,  8016._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.80_pm_r, 11531._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.80_pm_r, 14675._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       201.30_pm_r, 17603._pm_r,   2._pm_r,168._pm_r,  .15_pm_r,309._pm_r,   0._pm_r,179._pm_r,  .09_pm_r, 19._pm_r, & 
       210.70_pm_r, 20621._pm_r,   2._pm_r,173._pm_r,  .19_pm_r,304._pm_r,   0._pm_r,138._pm_r,  .12_pm_r, 17._pm_r, & 
       216.10_pm_r, 23747._pm_r,   2._pm_r,180._pm_r,  .18_pm_r,293._pm_r,   0._pm_r, 44._pm_r,  .12_pm_r, 19._pm_r, & 
       223.00_pm_r, 26958._pm_r,   2._pm_r,188._pm_r,  .18_pm_r,275._pm_r,   0._pm_r, 31._pm_r,  .11_pm_r, 22._pm_r, & 
       230.90_pm_r, 30282._pm_r,   2._pm_r,195._pm_r,  .18_pm_r,246._pm_r,   1._pm_r, 29._pm_r,  .08_pm_r, 26._pm_r, & 
       240.10_pm_r, 33727._pm_r,   2._pm_r,200._pm_r,  .21_pm_r,222._pm_r,   1._pm_r, 29._pm_r,  .04_pm_r, 36._pm_r, & 
       250.60_pm_r, 37323._pm_r,   3._pm_r,202._pm_r,  .24_pm_r,209._pm_r,   1._pm_r, 30._pm_r,  .01_pm_r,101._pm_r, & 
       260.80_pm_r, 41066._pm_r,   3._pm_r,202._pm_r,  .26_pm_r,202._pm_r,   1._pm_r, 32._pm_r,  .03_pm_r,171._pm_r, & 
       267.50_pm_r, 44942._pm_r,   3._pm_r,204._pm_r,  .31_pm_r,224._pm_r,   1._pm_r, 38._pm_r,  .07_pm_r,169._pm_r, & 
       268.10_pm_r, 48871._pm_r,   4._pm_r,209._pm_r,  .52_pm_r,249._pm_r,   0._pm_r, 47._pm_r,  .07_pm_r,180._pm_r, & 
       262.20_pm_r, 52759._pm_r,   4._pm_r,218._pm_r,  .70_pm_r,262._pm_r,   0._pm_r, 55._pm_r,  .09_pm_r,218._pm_r, & 
       251.10_pm_r, 56526._pm_r,   5._pm_r,226._pm_r,  .63_pm_r,271._pm_r,   0._pm_r, 56._pm_r,  .16_pm_r,244._pm_r, & 
       239.90_pm_r, 60117._pm_r,   6._pm_r,231._pm_r,  .22_pm_r,285._pm_r,   0._pm_r,270._pm_r,  .18_pm_r,243._pm_r, & 
       230.30_pm_r, 63561._pm_r,   6._pm_r,231._pm_r,  .21_pm_r, 81._pm_r,   0._pm_r,245._pm_r,  .17_pm_r,231._pm_r, & 
       221.20_pm_r, 66864._pm_r,   5._pm_r,227._pm_r,  .52_pm_r, 96._pm_r,   1._pm_r,234._pm_r,  .17_pm_r,210._pm_r, & 
       214.30_pm_r, 70052._pm_r,   5._pm_r,218._pm_r,  .74_pm_r,104._pm_r,   1._pm_r,224._pm_r,  .18_pm_r,187._pm_r, & 
       210.00_pm_r, 73155._pm_r,   4._pm_r,203._pm_r,  .90_pm_r,108._pm_r,   1._pm_r,212._pm_r,  .21_pm_r,168._pm_r, & 
       207.10_pm_r, 76208._pm_r,   4._pm_r,184._pm_r, 1.02_pm_r,112._pm_r,   1._pm_r,200._pm_r,  .24_pm_r,158._pm_r, & 
       206.70_pm_r, 79232._pm_r,   5._pm_r,168._pm_r, 1.08_pm_r,113._pm_r,   2._pm_r,190._pm_r,  .27_pm_r,152._pm_r, & 
       203.90_pm_r, 82258._pm_r,   6._pm_r,156._pm_r, 1.08_pm_r,115._pm_r,   2._pm_r,182._pm_r,  .27_pm_r,149._pm_r, & 
       196.90_pm_r, 85188._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       190.40_pm_r, 88012._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.10_pm_r, 90779._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.60_pm_r, 93534._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.80_pm_r, 96300._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       188.00_pm_r, 99092._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       192.00_pm_r,101948._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.60_pm_r,104946._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.20_pm_r,108243._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       283.00_pm_r,112209._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       374.30_pm_r,117491._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
       300.80_pm_r,  -119._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       277.20_pm_r,  4119._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       253.20_pm_r,  8008._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       226.20_pm_r, 11522._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       204.20_pm_r, 14678._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       203.00_pm_r, 17630._pm_r,   3._pm_r,153._pm_r,  .39_pm_r,281._pm_r,   1._pm_r,171._pm_r,  .32_pm_r,355._pm_r, & 
       212.10_pm_r, 20671._pm_r,   3._pm_r,164._pm_r,  .49_pm_r,282._pm_r,   1._pm_r,168._pm_r,  .42_pm_r,355._pm_r, & 
       217.20_pm_r, 23815._pm_r,   2._pm_r,180._pm_r,  .50_pm_r,280._pm_r,   0._pm_r, 80._pm_r,  .47_pm_r,354._pm_r, & 
       223.50_pm_r, 27039._pm_r,   2._pm_r,197._pm_r,  .43_pm_r,278._pm_r,   1._pm_r,360._pm_r,  .45_pm_r,354._pm_r, & 
       230.90_pm_r, 30367._pm_r,   3._pm_r,209._pm_r,  .30_pm_r,272._pm_r,   1._pm_r,357._pm_r,  .39_pm_r,353._pm_r, & 
       239.20_pm_r, 33807._pm_r,   3._pm_r,215._pm_r,  .16_pm_r,252._pm_r,   2._pm_r,355._pm_r,  .28_pm_r,352._pm_r, & 
       248.80_pm_r, 37382._pm_r,   3._pm_r,215._pm_r,  .11_pm_r,188._pm_r,   2._pm_r,355._pm_r,  .16_pm_r,349._pm_r, & 
       258.70_pm_r, 41097._pm_r,   3._pm_r,213._pm_r,  .17_pm_r,159._pm_r,   2._pm_r,354._pm_r,  .05_pm_r,338._pm_r, & 
       266.00_pm_r, 44946._pm_r,   3._pm_r,209._pm_r,  .18_pm_r,177._pm_r,   2._pm_r,355._pm_r,  .09_pm_r,126._pm_r, & 
       267.00_pm_r, 48857._pm_r,   4._pm_r,208._pm_r,  .21_pm_r,223._pm_r,   2._pm_r,359._pm_r,  .14_pm_r,120._pm_r, & 
       259.10_pm_r, 52714._pm_r,   4._pm_r,212._pm_r,  .36_pm_r,263._pm_r,   2._pm_r,  2._pm_r,  .03_pm_r,117._pm_r, & 
       249.40_pm_r, 56439._pm_r,   4._pm_r,220._pm_r,  .56_pm_r,277._pm_r,   2._pm_r,  0._pm_r,  .20_pm_r,307._pm_r, & 
       241.10_pm_r, 60029._pm_r,   5._pm_r,229._pm_r,  .52_pm_r,278._pm_r,   2._pm_r,352._pm_r,  .31_pm_r,304._pm_r, & 
       232.10_pm_r, 63498._pm_r,   5._pm_r,234._pm_r,  .35_pm_r,267._pm_r,   3._pm_r,345._pm_r,  .26_pm_r,307._pm_r, & 
       223.50_pm_r, 66830._pm_r,   6._pm_r,235._pm_r,  .18_pm_r,218._pm_r,   3._pm_r,343._pm_r,  .10_pm_r,342._pm_r, & 
       216.40_pm_r, 70051._pm_r,   6._pm_r,232._pm_r,  .33_pm_r,155._pm_r,   3._pm_r,345._pm_r,  .18_pm_r, 90._pm_r, & 
       211.40_pm_r, 73180._pm_r,   6._pm_r,226._pm_r,  .56_pm_r,140._pm_r,   3._pm_r,354._pm_r,  .41_pm_r,103._pm_r, & 
       207.80_pm_r, 76249._pm_r,   6._pm_r,216._pm_r,  .74_pm_r,136._pm_r,   3._pm_r,  8._pm_r,  .59_pm_r,107._pm_r, & 
       206.20_pm_r, 79275._pm_r,   6._pm_r,205._pm_r,  .85_pm_r,134._pm_r,   3._pm_r, 29._pm_r,  .71_pm_r,107._pm_r, & 
       202.60_pm_r, 82285._pm_r,   7._pm_r,195._pm_r,  .89_pm_r,133._pm_r,   3._pm_r, 49._pm_r,  .76_pm_r,108._pm_r, & 
       195.70_pm_r, 85198._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       189.20_pm_r, 88007._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       185.50_pm_r, 90752._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       184.70_pm_r, 93479._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       185.20_pm_r, 96216._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       187.40_pm_r, 98990._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       193.10_pm_r,101847._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       205.20_pm_r,104872._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       230.60_pm_r,108221._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       287.10_pm_r,112255._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       375.80_pm_r,117576._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
       299.00_pm_r,  -111._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       276.00_pm_r,  4102._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       251.30_pm_r,  7965._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.60_pm_r, 11459._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       207.20_pm_r, 14630._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       206.70_pm_r, 17636._pm_r,   4._pm_r,165._pm_r,  .47_pm_r,294._pm_r,   1._pm_r,187._pm_r,  .35_pm_r,  3._pm_r, & 
       214.50_pm_r, 20721._pm_r,   4._pm_r,175._pm_r,  .61_pm_r,294._pm_r,   0._pm_r,195._pm_r,  .47_pm_r,  1._pm_r, & 
       218.70_pm_r, 23893._pm_r,   3._pm_r,189._pm_r,  .65_pm_r,291._pm_r,   1._pm_r,351._pm_r,  .54_pm_r,  1._pm_r, & 
       223.70_pm_r, 27129._pm_r,   3._pm_r,205._pm_r,  .61_pm_r,289._pm_r,   1._pm_r,357._pm_r,  .56_pm_r,359._pm_r, & 
       230.00_pm_r, 30452._pm_r,   4._pm_r,218._pm_r,  .47_pm_r,284._pm_r,   2._pm_r,357._pm_r,  .50_pm_r,357._pm_r, & 
       238.10_pm_r, 33876._pm_r,   4._pm_r,225._pm_r,  .29_pm_r,273._pm_r,   3._pm_r,357._pm_r,  .39_pm_r,356._pm_r, & 
       247.20_pm_r, 37432._pm_r,   4._pm_r,228._pm_r,  .15_pm_r,247._pm_r,   3._pm_r,357._pm_r,  .26_pm_r,353._pm_r, & 
       256.30_pm_r, 41116._pm_r,   4._pm_r,228._pm_r,  .10_pm_r,192._pm_r,   4._pm_r,356._pm_r,  .14_pm_r,352._pm_r, & 
       264.00_pm_r, 44933._pm_r,   4._pm_r,226._pm_r,  .10_pm_r,135._pm_r,   4._pm_r,357._pm_r,  .06_pm_r, 49._pm_r, & 
       266.30_pm_r, 48823._pm_r,   4._pm_r,224._pm_r,  .10_pm_r, 93._pm_r,   4._pm_r,358._pm_r,  .10_pm_r, 87._pm_r, & 
       258.80_pm_r, 52675._pm_r,   4._pm_r,224._pm_r,  .09_pm_r,301._pm_r,   4._pm_r,  1._pm_r,  .11_pm_r, 98._pm_r, & 
       247.70_pm_r, 56386._pm_r,   4._pm_r,228._pm_r,  .45_pm_r,277._pm_r,   4._pm_r,  3._pm_r,  .10_pm_r,118._pm_r, & 
       239.10_pm_r, 59946._pm_r,   5._pm_r,235._pm_r,  .69_pm_r,271._pm_r,   4._pm_r,  5._pm_r,  .11_pm_r,146._pm_r, & 
       231.90_pm_r, 63397._pm_r,   6._pm_r,241._pm_r,  .73_pm_r,265._pm_r,   3._pm_r,  7._pm_r,  .15_pm_r,140._pm_r, & 
       224.00_pm_r, 66734._pm_r,   7._pm_r,244._pm_r,  .58_pm_r,258._pm_r,   3._pm_r, 11._pm_r,  .23_pm_r,122._pm_r, & 
       217.10_pm_r, 69963._pm_r,   8._pm_r,245._pm_r,  .37_pm_r,244._pm_r,   3._pm_r, 18._pm_r,  .31_pm_r,112._pm_r, & 
       212.60_pm_r, 73107._pm_r,   8._pm_r,244._pm_r,  .21_pm_r,204._pm_r,   3._pm_r, 27._pm_r,  .39_pm_r,107._pm_r, & 
       209.40_pm_r, 76197._pm_r,   8._pm_r,242._pm_r,  .25_pm_r,155._pm_r,   3._pm_r, 38._pm_r,  .47_pm_r,104._pm_r, & 
       206.60_pm_r, 79242._pm_r,   8._pm_r,239._pm_r,  .33_pm_r,133._pm_r,   4._pm_r, 48._pm_r,  .50_pm_r,101._pm_r, & 
       201.70_pm_r, 82247._pm_r,   8._pm_r,235._pm_r,  .39_pm_r,126._pm_r,   4._pm_r, 56._pm_r,  .50_pm_r,100._pm_r, & 
       193.80_pm_r, 85138._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.80_pm_r, 87914._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       182.40_pm_r, 90617._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       181.40_pm_r, 93295._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       182.70_pm_r, 95986._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       186.50_pm_r, 98732._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       194.50_pm_r,101591._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       208.90_pm_r,104653._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       236.70_pm_r,108076._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       292.10_pm_r,112201._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       377.20_pm_r,117564._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
       293.80_pm_r,   -66._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       271.60_pm_r,  4073._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       246.00_pm_r,  7862._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       223.50_pm_r, 11299._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       212.70_pm_r, 14492._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       212.50_pm_r, 17593._pm_r,   4._pm_r,206._pm_r,  .12_pm_r, 76._pm_r,   1._pm_r,349._pm_r,  .11_pm_r, 18._pm_r, & 
       217.00_pm_r, 20738._pm_r,   4._pm_r,203._pm_r,  .14_pm_r, 78._pm_r,   1._pm_r,355._pm_r,  .16_pm_r, 18._pm_r, & 
       220.00_pm_r, 23938._pm_r,   4._pm_r,201._pm_r,  .11_pm_r, 75._pm_r,   1._pm_r,360._pm_r,  .20_pm_r, 19._pm_r, & 
       224.00_pm_r, 27186._pm_r,   4._pm_r,200._pm_r,  .06_pm_r, 59._pm_r,   2._pm_r,  4._pm_r,  .22_pm_r, 22._pm_r, & 
       230.00_pm_r, 30510._pm_r,   4._pm_r,200._pm_r,  .05_pm_r,319._pm_r,   2._pm_r,  8._pm_r,  .22_pm_r, 25._pm_r, & 
       237.30_pm_r, 33928._pm_r,   4._pm_r,202._pm_r,  .13_pm_r,299._pm_r,   2._pm_r, 11._pm_r,  .21_pm_r, 31._pm_r, & 
       246.90_pm_r, 37475._pm_r,   4._pm_r,205._pm_r,  .17_pm_r,301._pm_r,   2._pm_r, 13._pm_r,  .19_pm_r, 39._pm_r, & 
       256.70_pm_r, 41161._pm_r,   4._pm_r,210._pm_r,  .19_pm_r,313._pm_r,   3._pm_r, 16._pm_r,  .17_pm_r, 45._pm_r, & 
       263.60_pm_r, 44977._pm_r,   3._pm_r,214._pm_r,  .27_pm_r,  4._pm_r,   3._pm_r, 19._pm_r,  .11_pm_r, 52._pm_r, & 
       265.40_pm_r, 48858._pm_r,   3._pm_r,217._pm_r,  .51_pm_r, 23._pm_r,   3._pm_r, 20._pm_r,  .05_pm_r, 53._pm_r, & 
       257.70_pm_r, 52695._pm_r,   2._pm_r,223._pm_r,  .51_pm_r, 18._pm_r,   3._pm_r, 20._pm_r,  .03_pm_r, 53._pm_r, & 
       246.70_pm_r, 56390._pm_r,   2._pm_r,237._pm_r,  .34_pm_r,323._pm_r,   3._pm_r, 21._pm_r,  .08_pm_r, 76._pm_r, & 
       237.30_pm_r, 59931._pm_r,   2._pm_r,249._pm_r,  .53_pm_r,268._pm_r,   3._pm_r, 24._pm_r,  .16_pm_r, 79._pm_r, & 
       229.20_pm_r, 63349._pm_r,   3._pm_r,253._pm_r,  .66_pm_r,257._pm_r,   3._pm_r, 28._pm_r,  .20_pm_r, 87._pm_r, & 
       221.90_pm_r, 66650._pm_r,   4._pm_r,254._pm_r,  .55_pm_r,259._pm_r,   3._pm_r, 33._pm_r,  .18_pm_r,103._pm_r, & 
       216.00_pm_r, 69856._pm_r,   5._pm_r,255._pm_r,  .32_pm_r,277._pm_r,   3._pm_r, 37._pm_r,  .17_pm_r,123._pm_r, & 
       212.00_pm_r, 72988._pm_r,   5._pm_r,258._pm_r,  .22_pm_r,329._pm_r,   3._pm_r, 41._pm_r,  .18_pm_r,145._pm_r, & 
       208.70_pm_r, 76070._pm_r,   5._pm_r,263._pm_r,  .34_pm_r, 12._pm_r,   3._pm_r, 46._pm_r,  .19_pm_r,159._pm_r, & 
       205.20_pm_r, 79100._pm_r,   5._pm_r,270._pm_r,  .46_pm_r, 25._pm_r,   3._pm_r, 50._pm_r,  .21_pm_r,172._pm_r, & 
       199.60_pm_r, 82081._pm_r,   4._pm_r,278._pm_r,  .53_pm_r, 29._pm_r,   3._pm_r, 55._pm_r,  .21_pm_r,175._pm_r, & 
       190.70_pm_r, 84929._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       182.90_pm_r, 87649._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       178.00_pm_r, 90287._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       177.30_pm_r, 92900._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       179.80_pm_r, 95536._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       185.40_pm_r, 98250._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       196.30_pm_r,101112._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       213.60_pm_r,104221._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       243.90_pm_r,107737._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       296.80_pm_r,111959._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       378.20_pm_r,117355._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
       286.30_pm_r,   -54._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       265.40_pm_r,  3981._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       240.30_pm_r,  7679._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       222.20_pm_r, 11062._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       218.40_pm_r, 14284._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       218.60_pm_r, 17481._pm_r,   4._pm_r,235._pm_r,  .31_pm_r,117._pm_r,   4._pm_r, 36._pm_r,  .46_pm_r,258._pm_r, & 
       219.90_pm_r, 20691._pm_r,   4._pm_r,227._pm_r,  .43_pm_r,119._pm_r,   3._pm_r, 27._pm_r,  .59_pm_r,253._pm_r, & 
       220.90_pm_r, 23917._pm_r,   4._pm_r,216._pm_r,  .52_pm_r,121._pm_r,   2._pm_r, 12._pm_r,  .63_pm_r,245._pm_r, & 
       224.10_pm_r, 27172._pm_r,   4._pm_r,203._pm_r,  .55_pm_r,122._pm_r,   2._pm_r,353._pm_r,  .55_pm_r,231._pm_r, & 
       229.70_pm_r, 30493._pm_r,   4._pm_r,192._pm_r,  .49_pm_r,123._pm_r,   2._pm_r,334._pm_r,  .43_pm_r,203._pm_r, & 
       237.30_pm_r, 33909._pm_r,   4._pm_r,183._pm_r,  .38_pm_r,118._pm_r,   1._pm_r,319._pm_r,  .42_pm_r,160._pm_r, & 
       246.30_pm_r, 37452._pm_r,   4._pm_r,177._pm_r,  .25_pm_r,101._pm_r,   0._pm_r,312._pm_r,  .49_pm_r,129._pm_r, & 
       255.70_pm_r, 41126._pm_r,   4._pm_r,173._pm_r,  .22_pm_r, 59._pm_r,   0._pm_r,105._pm_r,  .56_pm_r,109._pm_r, & 
       263.00_pm_r, 44932._pm_r,   4._pm_r,169._pm_r,  .31_pm_r, 36._pm_r,   1._pm_r,102._pm_r,  .49_pm_r, 93._pm_r, & 
       264.60_pm_r, 48803._pm_r,   4._pm_r,164._pm_r,  .37_pm_r, 21._pm_r,   2._pm_r, 97._pm_r,  .51_pm_r, 86._pm_r, & 
       257.30_pm_r, 52630._pm_r,   3._pm_r,160._pm_r,  .32_pm_r,339._pm_r,   3._pm_r, 95._pm_r,  .51_pm_r, 93._pm_r, & 
       246.90_pm_r, 56324._pm_r,   3._pm_r,167._pm_r,  .54_pm_r,282._pm_r,   3._pm_r, 96._pm_r,  .45_pm_r,110._pm_r, & 
       237.80_pm_r, 59870._pm_r,   3._pm_r,187._pm_r,  .67_pm_r,276._pm_r,   4._pm_r, 98._pm_r,  .28_pm_r,119._pm_r, & 
       229.00_pm_r, 63290._pm_r,   3._pm_r,207._pm_r,  .58_pm_r,281._pm_r,   4._pm_r,100._pm_r,  .16_pm_r,132._pm_r, & 
       221.60_pm_r, 66585._pm_r,   3._pm_r,222._pm_r,  .39_pm_r,309._pm_r,   4._pm_r,101._pm_r,  .07_pm_r,130._pm_r, & 
       216.00_pm_r, 69789._pm_r,   3._pm_r,232._pm_r,  .40_pm_r,  1._pm_r,   4._pm_r,102._pm_r,  .05_pm_r, 96._pm_r, & 
       211.20_pm_r, 72916._pm_r,   2._pm_r,243._pm_r,  .62_pm_r, 29._pm_r,   4._pm_r,101._pm_r,  .08_pm_r, 56._pm_r, & 
       206.60_pm_r, 75977._pm_r,   1._pm_r,267._pm_r,  .84_pm_r, 38._pm_r,   5._pm_r,100._pm_r,  .11_pm_r, 45._pm_r, & 
       202.20_pm_r, 78967._pm_r,   1._pm_r,338._pm_r,  .97_pm_r, 43._pm_r,   5._pm_r, 98._pm_r,  .13_pm_r, 41._pm_r, & 
       195.60_pm_r, 81897._pm_r,   2._pm_r, 17._pm_r, 1.02_pm_r, 45._pm_r,   5._pm_r, 96._pm_r,  .14_pm_r, 39._pm_r, & 
       185.90_pm_r, 84677._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       177.70_pm_r, 87320._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       172.90_pm_r, 89879._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       172.90_pm_r, 92420._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       176.80_pm_r, 94998._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       184.70_pm_r, 97681._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       198.30_pm_r,100550._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       219.10_pm_r,103711._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       251.40_pm_r,107327._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       300.20_pm_r,111639._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       378.90_pm_r,117049._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
       281.70_pm_r,   -73._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       260.50_pm_r,  3889._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       235.80_pm_r,  7516._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       222.60_pm_r, 10865._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       222.10_pm_r, 14115._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       222.30_pm_r, 17370._pm_r,   2._pm_r,207._pm_r,  .46_pm_r,269._pm_r,   5._pm_r, 40._pm_r,  .63_pm_r,265._pm_r, & 
       222.00_pm_r, 20623._pm_r,   3._pm_r,219._pm_r,  .52_pm_r,260._pm_r,   5._pm_r, 30._pm_r,  .88_pm_r,259._pm_r, & 
       221.70_pm_r, 23869._pm_r,   3._pm_r,226._pm_r,  .45_pm_r,241._pm_r,   4._pm_r, 15._pm_r, 1.03_pm_r,250._pm_r, & 
       223.60_pm_r, 27126._pm_r,   4._pm_r,225._pm_r,  .37_pm_r,191._pm_r,   3._pm_r,350._pm_r, 1.06_pm_r,237._pm_r, & 
       229.10_pm_r, 30439._pm_r,   4._pm_r,217._pm_r,  .59_pm_r,141._pm_r,   2._pm_r,319._pm_r,  .97_pm_r,218._pm_r, & 
       236.30_pm_r, 33845._pm_r,   5._pm_r,203._pm_r,  .93_pm_r,122._pm_r,   2._pm_r,286._pm_r,  .88_pm_r,193._pm_r, & 
       243.80_pm_r, 37363._pm_r,   5._pm_r,184._pm_r, 1.13_pm_r,111._pm_r,   2._pm_r,254._pm_r,  .83_pm_r,166._pm_r, & 
       252.50_pm_r, 40993._pm_r,   5._pm_r,167._pm_r, 1.13_pm_r,100._pm_r,   2._pm_r,224._pm_r,  .80_pm_r,141._pm_r, & 
       260.90_pm_r, 44760._pm_r,   6._pm_r,153._pm_r,  .85_pm_r, 79._pm_r,   3._pm_r,201._pm_r,  .62_pm_r,129._pm_r, & 
       263.50_pm_r, 48607._pm_r,   6._pm_r,143._pm_r,  .55_pm_r, 51._pm_r,   3._pm_r,185._pm_r,  .50_pm_r,115._pm_r, & 
       257.60_pm_r, 52429._pm_r,   6._pm_r,138._pm_r,  .34_pm_r,344._pm_r,   3._pm_r,173._pm_r,  .42_pm_r,102._pm_r, & 
       248.20_pm_r, 56135._pm_r,   5._pm_r,140._pm_r,  .64_pm_r,284._pm_r,   3._pm_r,163._pm_r,  .33_pm_r, 88._pm_r, & 
       240.70_pm_r, 59714._pm_r,   4._pm_r,149._pm_r,  .79_pm_r,281._pm_r,   3._pm_r,155._pm_r,  .32_pm_r, 54._pm_r, & 
       232.50_pm_r, 63181._pm_r,   4._pm_r,161._pm_r,  .69_pm_r,288._pm_r,   3._pm_r,147._pm_r,  .35_pm_r, 29._pm_r, & 
       225.30_pm_r, 66529._pm_r,   3._pm_r,172._pm_r,  .45_pm_r,310._pm_r,   3._pm_r,138._pm_r,  .36_pm_r, 15._pm_r, & 
       218.50_pm_r, 69781._pm_r,   3._pm_r,175._pm_r,  .37_pm_r,  3._pm_r,   3._pm_r,129._pm_r,  .35_pm_r,  8._pm_r, & 
       211.70_pm_r, 72929._pm_r,   2._pm_r,166._pm_r,  .56_pm_r, 39._pm_r,   2._pm_r,119._pm_r,  .34_pm_r,  4._pm_r, & 
       205.30_pm_r, 75984._pm_r,   2._pm_r,139._pm_r,  .75_pm_r, 51._pm_r,   2._pm_r,107._pm_r,  .31_pm_r,  2._pm_r, & 
       199.90_pm_r, 78947._pm_r,   2._pm_r,108._pm_r,  .88_pm_r, 56._pm_r,   2._pm_r, 95._pm_r,  .28_pm_r,360._pm_r, & 
       192.10_pm_r, 81837._pm_r,   3._pm_r, 90._pm_r,  .91_pm_r, 59._pm_r,   2._pm_r, 85._pm_r,  .26_pm_r,358._pm_r, & 
       180.80_pm_r, 84546._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       172.20_pm_r, 87103._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       168.00_pm_r, 89584._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       168.80_pm_r, 92056._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       174.20_pm_r, 94583._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       184.20_pm_r, 97238._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       200.40_pm_r,100116._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.50_pm_r,103331._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       258.00_pm_r,107041._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       301.60_pm_r,111420._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       379.20_pm_r,116824._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
       275.80_pm_r,   -54._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       256.80_pm_r,  3835._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       232.50_pm_r,  7408._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       223.40_pm_r, 10737._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.00_pm_r, 14004._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       223.80_pm_r, 17285._pm_r,   1._pm_r,177._pm_r,  .68_pm_r,273._pm_r,   4._pm_r, 39._pm_r,  .42_pm_r,264._pm_r, & 
       222.80_pm_r, 20555._pm_r,   2._pm_r,218._pm_r,  .84_pm_r,268._pm_r,   4._pm_r, 31._pm_r,  .61_pm_r,258._pm_r, & 
       221.80_pm_r, 23808._pm_r,   3._pm_r,237._pm_r,  .80_pm_r,259._pm_r,   3._pm_r, 18._pm_r,  .77_pm_r,250._pm_r, & 
       222.90_pm_r, 27060._pm_r,   4._pm_r,241._pm_r,  .55_pm_r,238._pm_r,   2._pm_r,356._pm_r,  .85_pm_r,240._pm_r, & 
       227.40_pm_r, 30357._pm_r,   4._pm_r,237._pm_r,  .41_pm_r,172._pm_r,   2._pm_r,324._pm_r,  .83_pm_r,226._pm_r, & 
       232.70_pm_r, 33724._pm_r,   4._pm_r,226._pm_r,  .80_pm_r,127._pm_r,   2._pm_r,289._pm_r,  .80_pm_r,208._pm_r, & 
       239.60_pm_r, 37183._pm_r,   4._pm_r,205._pm_r, 1.17_pm_r,112._pm_r,   2._pm_r,260._pm_r,  .71_pm_r,186._pm_r, & 
       248.40_pm_r, 40752._pm_r,   4._pm_r,179._pm_r, 1.31_pm_r,101._pm_r,   3._pm_r,237._pm_r,  .64_pm_r,162._pm_r, & 
       257.60_pm_r, 44463._pm_r,   5._pm_r,157._pm_r, 1.03_pm_r, 82._pm_r,   3._pm_r,220._pm_r,  .53_pm_r,153._pm_r, & 
       262.40_pm_r, 48276._pm_r,   5._pm_r,142._pm_r,  .69_pm_r, 48._pm_r,   3._pm_r,208._pm_r,  .41_pm_r,147._pm_r, & 
       259.00_pm_r, 52101._pm_r,   4._pm_r,132._pm_r,  .58_pm_r,357._pm_r,   3._pm_r,201._pm_r,  .23_pm_r,136._pm_r, & 
       251.10_pm_r, 55839._pm_r,   4._pm_r,127._pm_r,  .63_pm_r,315._pm_r,   3._pm_r,198._pm_r,  .07_pm_r, 39._pm_r, & 
       243.90_pm_r, 59462._pm_r,   3._pm_r,127._pm_r,  .51_pm_r,299._pm_r,   3._pm_r,198._pm_r,  .30_pm_r, 13._pm_r, & 
       236.50_pm_r, 62982._pm_r,   2._pm_r,132._pm_r,  .32_pm_r,278._pm_r,   3._pm_r,199._pm_r,  .49_pm_r, 12._pm_r, & 
       228.20_pm_r, 66382._pm_r,   2._pm_r,139._pm_r,  .17_pm_r,225._pm_r,   2._pm_r,201._pm_r,  .60_pm_r, 17._pm_r, & 
       220.00_pm_r, 69665._pm_r,   2._pm_r,144._pm_r,  .25_pm_r,159._pm_r,   1._pm_r,202._pm_r,  .68_pm_r, 22._pm_r, & 
       212.90_pm_r, 72832._pm_r,   3._pm_r,145._pm_r,  .40_pm_r,139._pm_r,   0._pm_r, 36._pm_r,  .73_pm_r, 28._pm_r, & 
       206.70_pm_r, 75905._pm_r,   3._pm_r,143._pm_r,  .51_pm_r,133._pm_r,   1._pm_r, 31._pm_r,  .75_pm_r, 32._pm_r, & 
       201.10_pm_r, 78889._pm_r,   4._pm_r,141._pm_r,  .57_pm_r,129._pm_r,   2._pm_r, 32._pm_r,  .73_pm_r, 34._pm_r, & 
       191.40_pm_r, 81795._pm_r,   5._pm_r,138._pm_r,  .57_pm_r,126._pm_r,   3._pm_r, 33._pm_r,  .68_pm_r, 36._pm_r, & 
       177.10_pm_r, 84457._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       167.50_pm_r, 86938._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       164.00_pm_r, 89353._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       165.70_pm_r, 91770._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       172.20_pm_r, 94258._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       183.90_pm_r, 96894._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       202.20_pm_r, 99781._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       229.00_pm_r,103042._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       263.10_pm_r,106828._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       301.50_pm_r,111247._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       379.30_pm_r,116634._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
       268.80_pm_r,   -32._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       254.00_pm_r,  3784._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       230.20_pm_r,  7318._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.40_pm_r, 10636._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       225.30_pm_r, 13918._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       224.50_pm_r, 17213._pm_r,   0._pm_r, 71._pm_r,  .47_pm_r,256._pm_r,   2._pm_r, 23._pm_r,  .09_pm_r,220._pm_r, & 
       223.10_pm_r, 20492._pm_r,   1._pm_r,258._pm_r,  .62_pm_r,256._pm_r,   1._pm_r, 21._pm_r,  .14_pm_r,216._pm_r, & 
       221.60_pm_r, 23745._pm_r,   2._pm_r,256._pm_r,  .64_pm_r,255._pm_r,   1._pm_r, 18._pm_r,  .19_pm_r,213._pm_r, & 
       222.10_pm_r, 26990._pm_r,   2._pm_r,256._pm_r,  .48_pm_r,254._pm_r,   1._pm_r, 14._pm_r,  .23_pm_r,209._pm_r, & 
       226.00_pm_r, 30270._pm_r,   3._pm_r,255._pm_r,  .19_pm_r,250._pm_r,   1._pm_r,  4._pm_r,  .26_pm_r,205._pm_r, & 
       230.60_pm_r, 33611._pm_r,   3._pm_r,255._pm_r,  .17_pm_r, 87._pm_r,   0._pm_r,326._pm_r,  .28_pm_r,199._pm_r, & 
       237.40_pm_r, 37037._pm_r,   2._pm_r,253._pm_r,  .50_pm_r, 81._pm_r,   0._pm_r,225._pm_r,  .26_pm_r,192._pm_r, & 
       246.10_pm_r, 40574._pm_r,   1._pm_r,248._pm_r,  .73_pm_r, 80._pm_r,   1._pm_r,205._pm_r,  .23_pm_r,185._pm_r, & 
       255.30_pm_r, 44250._pm_r,   1._pm_r,235._pm_r,  .57_pm_r, 64._pm_r,   1._pm_r,199._pm_r,  .18_pm_r,191._pm_r, & 
       261.30_pm_r, 48036._pm_r,   0._pm_r, 27._pm_r,  .47_pm_r, 18._pm_r,   1._pm_r,201._pm_r,  .14_pm_r,234._pm_r, & 
       260.20_pm_r, 51862._pm_r,   1._pm_r,  4._pm_r,  .58_pm_r,340._pm_r,   1._pm_r,211._pm_r,  .22_pm_r,278._pm_r, & 
       254.50_pm_r, 55634._pm_r,   2._pm_r,348._pm_r,  .60_pm_r,322._pm_r,   1._pm_r,227._pm_r,  .35_pm_r,290._pm_r, & 
       247.70_pm_r, 59310._pm_r,   2._pm_r,341._pm_r,  .28_pm_r,324._pm_r,   2._pm_r,241._pm_r,  .27_pm_r,298._pm_r, & 
       240.50_pm_r, 62888._pm_r,   3._pm_r,341._pm_r,  .10_pm_r, 61._pm_r,   2._pm_r,251._pm_r,  .16_pm_r,330._pm_r, & 
       230.90_pm_r, 66340._pm_r,   3._pm_r,349._pm_r,  .34_pm_r, 89._pm_r,   2._pm_r,257._pm_r,  .21_pm_r, 43._pm_r, & 
       220.90_pm_r, 69650._pm_r,   3._pm_r,  3._pm_r,  .53_pm_r, 86._pm_r,   1._pm_r,264._pm_r,  .40_pm_r, 63._pm_r, & 
       212.80_pm_r, 72822._pm_r,   3._pm_r, 21._pm_r,  .68_pm_r, 81._pm_r,   1._pm_r,283._pm_r,  .58_pm_r, 70._pm_r, & 
       205.90_pm_r, 75890._pm_r,   4._pm_r, 37._pm_r,  .79_pm_r, 78._pm_r,   1._pm_r, 31._pm_r,  .72_pm_r, 71._pm_r, & 
       200.00_pm_r, 78860._pm_r,   5._pm_r, 46._pm_r,  .82_pm_r, 75._pm_r,   2._pm_r, 59._pm_r,  .77_pm_r, 73._pm_r, & 
       189.40_pm_r, 81747._pm_r,   6._pm_r, 52._pm_r,  .78_pm_r, 74._pm_r,   3._pm_r, 65._pm_r,  .75_pm_r, 74._pm_r, & 
       174.10_pm_r, 84364._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       164.40_pm_r, 86795._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       161.40_pm_r, 89166._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       163.60_pm_r, 91548._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       171.00_pm_r, 94012._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       183.80_pm_r, 96637._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       203.50_pm_r, 99531._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       232.00_pm_r,102825._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       266.00_pm_r,106657._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       300.80_pm_r,111095._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, & 
       379.30_pm_r,116465._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_septembre = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_septembre.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_septembre,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_septembre
