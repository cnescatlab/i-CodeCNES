subroutine mpi_atmi_aout (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de AOUT 
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
       263.10_pm_r,-320._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       239.80_pm_r,3351._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.50_pm_r,6689._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.00_pm_r,9750._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.00_pm_r,12648._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.90_pm_r,15470._pm_r,9._pm_r,242._pm_r,1.17_pm_r,206._pm_r,6._pm_r,166._pm_r,.47_pm_r,13._pm_r, &
       188.30_pm_r,18246._pm_r,11._pm_r,233._pm_r,2.42_pm_r,191._pm_r,5._pm_r,161._pm_r,.97_pm_r,5._pm_r, &
       186.20_pm_r,20992._pm_r,15._pm_r,216._pm_r,4.21_pm_r,166._pm_r,3._pm_r,148._pm_r,1.56_pm_r,354._pm_r, &
       179.00_pm_r,23660._pm_r,18._pm_r,200._pm_r,3.55_pm_r,143._pm_r,1._pm_r,112._pm_r,1.03_pm_r,340._pm_r, &
       192.70_pm_r,26363._pm_r,20._pm_r,188._pm_r,2.90_pm_r,122._pm_r,1._pm_r,42._pm_r,.64_pm_r,315._pm_r, &
       215.70_pm_r,29348._pm_r,22._pm_r,178._pm_r,2.69_pm_r,104._pm_r,1._pm_r,348._pm_r,.52_pm_r,285._pm_r, &
       234.90_pm_r,32658._pm_r,23._pm_r,168._pm_r,2.56_pm_r,89._pm_r,1._pm_r,316._pm_r,.52_pm_r,262._pm_r, &
       249.80_pm_r,36206._pm_r,23._pm_r,159._pm_r,2.38_pm_r,73._pm_r,2._pm_r,295._pm_r,.52_pm_r,245._pm_r, &
       259.00_pm_r,39937._pm_r,23._pm_r,150._pm_r,2.64_pm_r,48._pm_r,2._pm_r,282._pm_r,.36_pm_r,237._pm_r, &
       267.10_pm_r,43788._pm_r,22._pm_r,141._pm_r,2.89_pm_r,24._pm_r,3._pm_r,275._pm_r,.18_pm_r,236._pm_r, &
       273.60_pm_r,47752._pm_r,19._pm_r,131._pm_r,2.79_pm_r,355._pm_r,3._pm_r,273._pm_r,.09_pm_r,241._pm_r, &
       275.50_pm_r,51778._pm_r,16._pm_r,125._pm_r,2.96_pm_r,312._pm_r,3._pm_r,271._pm_r,.15_pm_r,228._pm_r, &
       267.80_pm_r,55763._pm_r,11._pm_r,130._pm_r,3.59_pm_r,279._pm_r,3._pm_r,266._pm_r,.28_pm_r,217._pm_r, &
       255.50_pm_r,59601._pm_r,8._pm_r,160._pm_r,4.27_pm_r,261._pm_r,3._pm_r,260._pm_r,.34_pm_r,214._pm_r, &
       243.40_pm_r,63248._pm_r,10._pm_r,202._pm_r,4.41_pm_r,251._pm_r,4._pm_r,255._pm_r,.31_pm_r,214._pm_r, &
       233.20_pm_r,66740._pm_r,15._pm_r,219._pm_r,4.14_pm_r,243._pm_r,4._pm_r,251._pm_r,.21_pm_r,215._pm_r, &
       226.40_pm_r,70101._pm_r,20._pm_r,225._pm_r,3.66_pm_r,235._pm_r,4._pm_r,250._pm_r,.07_pm_r,219._pm_r, &
       222.50_pm_r,73386._pm_r,25._pm_r,226._pm_r,3.19_pm_r,228._pm_r,4._pm_r,250._pm_r,.06_pm_r,31._pm_r, &
       219.90_pm_r,76624._pm_r,30._pm_r,226._pm_r,2.80_pm_r,221._pm_r,4._pm_r,251._pm_r,.18_pm_r,40._pm_r, &
       217.50_pm_r,79827._pm_r,33._pm_r,225._pm_r,2.43_pm_r,214._pm_r,4._pm_r,253._pm_r,.25_pm_r,40._pm_r, &
       214.80_pm_r,82989._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.10_pm_r,86108._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.10_pm_r,89099._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.10_pm_r,91973._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.80_pm_r,94805._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.70_pm_r,97675._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.70_pm_r,100668._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.00_pm_r,103882._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       241.90_pm_r,107431._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       273.20_pm_r,111454._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       338.70_pm_r,116282._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
       268.10_pm_r,-178._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.80_pm_r,3560._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.80_pm_r,6953._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.70_pm_r,10060._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       198.90_pm_r,13007._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.00_pm_r,15896._pm_r,16._pm_r,254._pm_r,3.07_pm_r,181._pm_r,13._pm_r,156._pm_r,1.59_pm_r,354._pm_r, &
       194.70_pm_r,18755._pm_r,18._pm_r,235._pm_r,5.11_pm_r,170._pm_r,10._pm_r,151._pm_r,2.75_pm_r,349._pm_r, &
       194.20_pm_r,21604._pm_r,22._pm_r,213._pm_r,6.34_pm_r,153._pm_r,5._pm_r,138._pm_r,3.45_pm_r,342._pm_r, &
       191.50_pm_r,24420._pm_r,26._pm_r,195._pm_r,5.31_pm_r,127._pm_r,2._pm_r,86._pm_r,2.62_pm_r,329._pm_r, &
       203.70_pm_r,27301._pm_r,28._pm_r,180._pm_r,4.66_pm_r,96._pm_r,3._pm_r,358._pm_r,1.63_pm_r,307._pm_r, &
       223.90_pm_r,30426._pm_r,28._pm_r,166._pm_r,4.83_pm_r,71._pm_r,4._pm_r,329._pm_r,1.14_pm_r,271._pm_r, &
       241.30_pm_r,33842._pm_r,27._pm_r,151._pm_r,5.02_pm_r,55._pm_r,4._pm_r,308._pm_r,1.10_pm_r,233._pm_r, &
       254.70_pm_r,37473._pm_r,26._pm_r,135._pm_r,4.88_pm_r,41._pm_r,5._pm_r,287._pm_r,1.21_pm_r,208._pm_r, &
       262.20_pm_r,41262._pm_r,26._pm_r,120._pm_r,4.49_pm_r,27._pm_r,5._pm_r,272._pm_r,.64_pm_r,203._pm_r, &
       268.00_pm_r,45145._pm_r,26._pm_r,106._pm_r,4.01_pm_r,5._pm_r,5._pm_r,267._pm_r,.27_pm_r,250._pm_r, &
       270.50_pm_r,49094._pm_r,23._pm_r,94._pm_r,4.06_pm_r,331._pm_r,6._pm_r,267._pm_r,.46_pm_r,274._pm_r, &
       267.60_pm_r,53042._pm_r,19._pm_r,81._pm_r,5.06_pm_r,300._pm_r,7._pm_r,266._pm_r,.75_pm_r,241._pm_r, &
       257.80_pm_r,56892._pm_r,13._pm_r,64._pm_r,5.75_pm_r,280._pm_r,8._pm_r,258._pm_r,1.10_pm_r,201._pm_r, &
       246.10_pm_r,60585._pm_r,6._pm_r,23._pm_r,6.07_pm_r,265._pm_r,9._pm_r,247._pm_r,1.45_pm_r,185._pm_r, &
       236.30_pm_r,64112._pm_r,7._pm_r,304._pm_r,5.85_pm_r,253._pm_r,10._pm_r,235._pm_r,1.58_pm_r,176._pm_r, &
       229.00_pm_r,67520._pm_r,14._pm_r,274._pm_r,5.34_pm_r,242._pm_r,11._pm_r,224._pm_r,1.51_pm_r,169._pm_r, &
       224.20_pm_r,70834._pm_r,20._pm_r,261._pm_r,4.78_pm_r,230._pm_r,12._pm_r,216._pm_r,1.36_pm_r,163._pm_r, &
       221.40_pm_r,74096._pm_r,26._pm_r,252._pm_r,4.34_pm_r,219._pm_r,13._pm_r,209._pm_r,1.20_pm_r,155._pm_r, &
       219.20_pm_r,77323._pm_r,31._pm_r,245._pm_r,4.00_pm_r,210._pm_r,14._pm_r,203._pm_r,1.06_pm_r,148._pm_r, &
       216.80_pm_r,80517._pm_r,35._pm_r,240._pm_r,3.63_pm_r,203._pm_r,15._pm_r,199._pm_r,.92_pm_r,141._pm_r, &
       213.60_pm_r,83668._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.30_pm_r,86766._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.40_pm_r,89731._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.80_pm_r,92583._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.30_pm_r,95397._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.00_pm_r,98246._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.30_pm_r,101211._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.20_pm_r,104387._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       239.50_pm_r,107895._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       273.70_pm_r,111903._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       340.80_pm_r,116764._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
       273.10_pm_r,-31._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.10_pm_r,3785._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.80_pm_r,7248._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.40_pm_r,10421._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.40_pm_r,13459._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.10_pm_r,16473._pm_r,19._pm_r,261._pm_r,4.33_pm_r,151._pm_r,16._pm_r,137._pm_r,2.77_pm_r,334._pm_r, &
       204.30_pm_r,19469._pm_r,17._pm_r,237._pm_r,6.13_pm_r,143._pm_r,12._pm_r,131._pm_r,4.05_pm_r,330._pm_r, &
       204.90_pm_r,22465._pm_r,18._pm_r,206._pm_r,6.75_pm_r,129._pm_r,6._pm_r,113._pm_r,4.60_pm_r,323._pm_r, &
       204.50_pm_r,25460._pm_r,21._pm_r,179._pm_r,6.03_pm_r,106._pm_r,3._pm_r,19._pm_r,3.93_pm_r,312._pm_r, &
       212.60_pm_r,28506._pm_r,22._pm_r,158._pm_r,5.49_pm_r,73._pm_r,6._pm_r,330._pm_r,2.54_pm_r,290._pm_r, &
       227.50_pm_r,31722._pm_r,22._pm_r,137._pm_r,6.05_pm_r,43._pm_r,8._pm_r,311._pm_r,1.86_pm_r,242._pm_r, &
       241.80_pm_r,35166._pm_r,22._pm_r,113._pm_r,6.59_pm_r,25._pm_r,8._pm_r,291._pm_r,2.24_pm_r,202._pm_r, &
       253.70_pm_r,38795._pm_r,24._pm_r,89._pm_r,6.49_pm_r,12._pm_r,9._pm_r,266._pm_r,2.75_pm_r,181._pm_r, &
       260.40_pm_r,42564._pm_r,26._pm_r,70._pm_r,5.28_pm_r,360._pm_r,9._pm_r,243._pm_r,2.08_pm_r,172._pm_r, &
       264.20_pm_r,46409._pm_r,28._pm_r,56._pm_r,4.16_pm_r,334._pm_r,10._pm_r,229._pm_r,1.52_pm_r,178._pm_r, &
       262.70_pm_r,50273._pm_r,28._pm_r,43._pm_r,4.54_pm_r,299._pm_r,12._pm_r,223._pm_r,1.44_pm_r,199._pm_r, &
       255.70_pm_r,54077._pm_r,25._pm_r,28._pm_r,6.00_pm_r,279._pm_r,14._pm_r,220._pm_r,1.80_pm_r,214._pm_r, &
       245.80_pm_r,57748._pm_r,23._pm_r,6._pm_r,6.46_pm_r,267._pm_r,17._pm_r,219._pm_r,1.67_pm_r,203._pm_r, &
       237.10_pm_r,61284._pm_r,23._pm_r,342._pm_r,6.19_pm_r,258._pm_r,19._pm_r,216._pm_r,1.42_pm_r,188._pm_r, &
       231.40_pm_r,64711._pm_r,24._pm_r,322._pm_r,5.36_pm_r,249._pm_r,21._pm_r,213._pm_r,1.19_pm_r,169._pm_r, &
       227.70_pm_r,68072._pm_r,27._pm_r,307._pm_r,4.35_pm_r,238._pm_r,22._pm_r,209._pm_r,1.08_pm_r,148._pm_r, &
       224.90_pm_r,71386._pm_r,29._pm_r,296._pm_r,3.47_pm_r,224._pm_r,22._pm_r,205._pm_r,1.10_pm_r,130._pm_r, &
       222.30_pm_r,74661._pm_r,30._pm_r,287._pm_r,2.88_pm_r,207._pm_r,22._pm_r,201._pm_r,1.15_pm_r,116._pm_r, &
       219.50_pm_r,77898._pm_r,30._pm_r,279._pm_r,2.54_pm_r,191._pm_r,22._pm_r,197._pm_r,1.19_pm_r,108._pm_r, &
       216.20_pm_r,81091._pm_r,30._pm_r,273._pm_r,2.29_pm_r,178._pm_r,22._pm_r,192._pm_r,1.17_pm_r,102._pm_r, &
       211.70_pm_r,84228._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.50_pm_r,87293._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.90_pm_r,90218._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.80_pm_r,93039._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.60_pm_r,95828._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.80_pm_r,98650._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.20_pm_r,101577._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.90_pm_r,104697._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.50_pm_r,108142._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       273.80_pm_r,112117._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       343.70_pm_r,117016._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
       278.10_pm_r,2._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       255.20_pm_r,3902._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.80_pm_r,7449._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.50_pm_r,10706._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.30_pm_r,13849._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.80_pm_r,16985._pm_r,13._pm_r,263._pm_r,3.44_pm_r,130._pm_r,14._pm_r,126._pm_r,3.16_pm_r,328._pm_r, &
       213.40_pm_r,20111._pm_r,9._pm_r,238._pm_r,4.49_pm_r,122._pm_r,9._pm_r,113._pm_r,4.33_pm_r,324._pm_r, &
       214.70_pm_r,23246._pm_r,8._pm_r,192._pm_r,4.73_pm_r,110._pm_r,4._pm_r,65._pm_r,4.83_pm_r,318._pm_r, &
       216.20_pm_r,26400._pm_r,10._pm_r,152._pm_r,4.38_pm_r,88._pm_r,7._pm_r,350._pm_r,4.47_pm_r,307._pm_r, &
       219.60_pm_r,29589._pm_r,13._pm_r,124._pm_r,4.31_pm_r,56._pm_r,11._pm_r,326._pm_r,3.45_pm_r,287._pm_r, &
       227.60_pm_r,32856._pm_r,15._pm_r,98._pm_r,4.93_pm_r,26._pm_r,14._pm_r,311._pm_r,2.68_pm_r,246._pm_r, &
       237.80_pm_r,36266._pm_r,18._pm_r,73._pm_r,5.42_pm_r,7._pm_r,15._pm_r,295._pm_r,3.06_pm_r,205._pm_r, &
       248.00_pm_r,39821._pm_r,21._pm_r,52._pm_r,5.21_pm_r,352._pm_r,14._pm_r,276._pm_r,3.77_pm_r,181._pm_r, &
       255.40_pm_r,43514._pm_r,25._pm_r,38._pm_r,3.82_pm_r,336._pm_r,14._pm_r,256._pm_r,2.94_pm_r,167._pm_r, &
       257.90_pm_r,47279._pm_r,26._pm_r,28._pm_r,2.92_pm_r,294._pm_r,15._pm_r,241._pm_r,2.17_pm_r,161._pm_r, &
       253.80_pm_r,51031._pm_r,24._pm_r,18._pm_r,4.02_pm_r,258._pm_r,16._pm_r,231._pm_r,1.74_pm_r,178._pm_r, &
       245.60_pm_r,54693._pm_r,21._pm_r,2._pm_r,5.41_pm_r,248._pm_r,18._pm_r,226._pm_r,2.07_pm_r,207._pm_r, &
       237.60_pm_r,58228._pm_r,19._pm_r,340._pm_r,5.38_pm_r,245._pm_r,21._pm_r,223._pm_r,2.24_pm_r,203._pm_r, &
       232.30_pm_r,61668._pm_r,20._pm_r,318._pm_r,4.70_pm_r,243._pm_r,24._pm_r,220._pm_r,2.02_pm_r,196._pm_r, &
       228.50_pm_r,65042._pm_r,22._pm_r,302._pm_r,3.72_pm_r,241._pm_r,26._pm_r,217._pm_r,1.63_pm_r,184._pm_r, &
       225.20_pm_r,68364._pm_r,25._pm_r,292._pm_r,2.74_pm_r,238._pm_r,28._pm_r,214._pm_r,1.34_pm_r,162._pm_r, &
       222.80_pm_r,71644._pm_r,27._pm_r,286._pm_r,1.91_pm_r,232._pm_r,29._pm_r,211._pm_r,1.30_pm_r,137._pm_r, &
       220.00_pm_r,74889._pm_r,28._pm_r,282._pm_r,1.28_pm_r,224._pm_r,29._pm_r,207._pm_r,1.41_pm_r,119._pm_r, &
       216.60_pm_r,78085._pm_r,29._pm_r,279._pm_r,.83_pm_r,211._pm_r,29._pm_r,202._pm_r,1.52_pm_r,109._pm_r, &
       213.10_pm_r,81233._pm_r,29._pm_r,277._pm_r,.55_pm_r,192._pm_r,29._pm_r,198._pm_r,1.53_pm_r,102._pm_r, &
       208.20_pm_r,84325._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.70_pm_r,87336._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.10_pm_r,90211._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.80_pm_r,93001._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.10_pm_r,95767._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.50_pm_r,98562._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.80_pm_r,101449._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.90_pm_r,104506._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.40_pm_r,107874._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       272.60_pm_r,111797._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       346.60_pm_r,116728._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
       285.10_pm_r,-6._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       261.00_pm_r,3991._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.40_pm_r,7625._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.10_pm_r,10959._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.20_pm_r,14153._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.90_pm_r,17314._pm_r,6._pm_r,248._pm_r,1.58_pm_r,118._pm_r,8._pm_r,126._pm_r,2.18_pm_r,330._pm_r, &
       217.30_pm_r,20485._pm_r,5._pm_r,223._pm_r,1.97_pm_r,108._pm_r,4._pm_r,107._pm_r,2.94_pm_r,325._pm_r, &
       220.10_pm_r,23689._pm_r,4._pm_r,183._pm_r,2.08_pm_r,90._pm_r,3._pm_r,28._pm_r,3.33_pm_r,317._pm_r, &
       222.80_pm_r,26930._pm_r,4._pm_r,138._pm_r,2.15_pm_r,64._pm_r,6._pm_r,337._pm_r,3.24_pm_r,305._pm_r, &
       225.10_pm_r,30210._pm_r,5._pm_r,100._pm_r,2.53_pm_r,36._pm_r,10._pm_r,319._pm_r,2.83_pm_r,284._pm_r, &
       230.30_pm_r,33538._pm_r,8._pm_r,69._pm_r,3.01_pm_r,14._pm_r,13._pm_r,306._pm_r,2.52_pm_r,250._pm_r, &
       238.30_pm_r,36970._pm_r,10._pm_r,46._pm_r,3.12_pm_r,359._pm_r,14._pm_r,291._pm_r,2.80_pm_r,214._pm_r, &
       247.80_pm_r,40526._pm_r,13._pm_r,31._pm_r,2.71_pm_r,345._pm_r,15._pm_r,273._pm_r,3.41_pm_r,189._pm_r, &
       256.30_pm_r,44224._pm_r,15._pm_r,20._pm_r,2.03_pm_r,313._pm_r,15._pm_r,256._pm_r,2.62_pm_r,170._pm_r, &
       258.10_pm_r,47999._pm_r,16._pm_r,10._pm_r,2.16_pm_r,261._pm_r,15._pm_r,244._pm_r,1.84_pm_r,151._pm_r, &
       252.10_pm_r,51739._pm_r,14._pm_r,357._pm_r,3.20_pm_r,232._pm_r,15._pm_r,236._pm_r,1.16_pm_r,156._pm_r, &
       241.70_pm_r,55360._pm_r,11._pm_r,335._pm_r,3.92_pm_r,220._pm_r,16._pm_r,231._pm_r,1.30_pm_r,209._pm_r, &
       233.00_pm_r,58831._pm_r,10._pm_r,304._pm_r,3.60_pm_r,213._pm_r,18._pm_r,228._pm_r,2.07_pm_r,205._pm_r, &
       228.10_pm_r,62206._pm_r,10._pm_r,276._pm_r,2.94_pm_r,208._pm_r,21._pm_r,224._pm_r,2.25_pm_r,198._pm_r, &
       224.20_pm_r,65517._pm_r,12._pm_r,260._pm_r,2.20_pm_r,206._pm_r,24._pm_r,220._pm_r,1.98_pm_r,186._pm_r, &
       220.90_pm_r,68777._pm_r,14._pm_r,251._pm_r,1.55_pm_r,208._pm_r,26._pm_r,216._pm_r,1.63_pm_r,165._pm_r, &
       218.80_pm_r,71996._pm_r,16._pm_r,246._pm_r,1.05_pm_r,213._pm_r,27._pm_r,212._pm_r,1.52_pm_r,138._pm_r, &
       215.70_pm_r,75181._pm_r,17._pm_r,244._pm_r,.69_pm_r,224._pm_r,27._pm_r,207._pm_r,1.67_pm_r,117._pm_r, &
       212.10_pm_r,78311._pm_r,17._pm_r,244._pm_r,.50_pm_r,242._pm_r,27._pm_r,202._pm_r,1.83_pm_r,105._pm_r, &
       208.90_pm_r,81392._pm_r,18._pm_r,244._pm_r,.38_pm_r,263._pm_r,27._pm_r,196._pm_r,1.87_pm_r,99._pm_r, &
       204.20_pm_r,84427._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.00_pm_r,87377._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.60_pm_r,90205._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.40_pm_r,92969._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.00_pm_r,95719._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.70_pm_r,98497._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.70_pm_r,101351._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.00_pm_r,104352._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.00_pm_r,107641._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       270.50_pm_r,111502._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       349.30_pm_r,116454._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
       290.90_pm_r,-27._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       267.60_pm_r,4065._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.80_pm_r,7804._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.90_pm_r,11216._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.40_pm_r,14398._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.70_pm_r,17475._pm_r,3._pm_r,199._pm_r,.21_pm_r,103._pm_r,5._pm_r,129._pm_r,1.33_pm_r,334._pm_r, &
       215.70_pm_r,20597._pm_r,3._pm_r,191._pm_r,.30_pm_r,60._pm_r,3._pm_r,111._pm_r,1.72_pm_r,328._pm_r, &
       220.30_pm_r,23791._pm_r,2._pm_r,182._pm_r,.57_pm_r,29._pm_r,2._pm_r,39._pm_r,1.81_pm_r,318._pm_r, &
       224.90_pm_r,27047._pm_r,1._pm_r,163._pm_r,.94_pm_r,13._pm_r,3._pm_r,341._pm_r,1.66_pm_r,302._pm_r, &
       228.70_pm_r,30371._pm_r,1._pm_r,34._pm_r,1.35_pm_r,5._pm_r,5._pm_r,319._pm_r,1.46_pm_r,273._pm_r, &
       234.20_pm_r,33756._pm_r,3._pm_r,11._pm_r,1.58_pm_r,358._pm_r,6._pm_r,300._pm_r,1.55_pm_r,234._pm_r, &
       242.20_pm_r,37243._pm_r,5._pm_r,4._pm_r,1.49_pm_r,351._pm_r,7._pm_r,278._pm_r,1.95_pm_r,203._pm_r, &
       252.20_pm_r,40859._pm_r,7._pm_r,359._pm_r,1.12_pm_r,338._pm_r,8._pm_r,253._pm_r,2.39_pm_r,182._pm_r, &
       261.00_pm_r,44625._pm_r,8._pm_r,352._pm_r,1.04_pm_r,288._pm_r,9._pm_r,233._pm_r,1.91_pm_r,168._pm_r, &
       262.50_pm_r,48467._pm_r,8._pm_r,340._pm_r,1.50_pm_r,246._pm_r,10._pm_r,220._pm_r,1.34_pm_r,161._pm_r, &
       256.30_pm_r,52270._pm_r,8._pm_r,322._pm_r,1.99_pm_r,215._pm_r,11._pm_r,213._pm_r,.79_pm_r,172._pm_r, &
       245.10_pm_r,55948._pm_r,7._pm_r,298._pm_r,2.47_pm_r,188._pm_r,12._pm_r,212._pm_r,.64_pm_r,221._pm_r, &
       234.30_pm_r,59453._pm_r,6._pm_r,265._pm_r,2.58_pm_r,172._pm_r,13._pm_r,211._pm_r,.70_pm_r,200._pm_r, &
       226.30_pm_r,62826._pm_r,7._pm_r,232._pm_r,2.32_pm_r,164._pm_r,14._pm_r,210._pm_r,.75_pm_r,177._pm_r, &
       220.90_pm_r,66098._pm_r,8._pm_r,212._pm_r,1.71_pm_r,164._pm_r,15._pm_r,207._pm_r,.85_pm_r,156._pm_r, &
       217.10_pm_r,69306._pm_r,10._pm_r,204._pm_r,1.01_pm_r,177._pm_r,15._pm_r,202._pm_r,.98_pm_r,140._pm_r, &
       214.70_pm_r,72467._pm_r,11._pm_r,203._pm_r,.60_pm_r,221._pm_r,16._pm_r,197._pm_r,1.12_pm_r,130._pm_r, &
       211.70_pm_r,75591._pm_r,11._pm_r,206._pm_r,.77_pm_r,269._pm_r,17._pm_r,191._pm_r,1.23_pm_r,124._pm_r, &
       208.20_pm_r,78665._pm_r,12._pm_r,212._pm_r,1.05_pm_r,286._pm_r,17._pm_r,186._pm_r,1.27_pm_r,120._pm_r, &
       205.00_pm_r,81690._pm_r,12._pm_r,220._pm_r,1.20_pm_r,292._pm_r,18._pm_r,180._pm_r,1.23_pm_r,118._pm_r, &
       200.70_pm_r,84674._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.10_pm_r,87573._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.80_pm_r,90364._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.90_pm_r,93115._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.70_pm_r,95863._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,98635._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.50_pm_r,101468._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.10_pm_r,104427._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.50_pm_r,107653._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       268.30_pm_r,111461._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       351.50_pm_r,116424._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
       295.70_pm_r,-27._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       273.80_pm_r,4148._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.60_pm_r,7986._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.40_pm_r,11461._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.40_pm_r,14612._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.60_pm_r,17590._pm_r,2._pm_r,153._pm_r,.41_pm_r,293._pm_r,2._pm_r,130._pm_r,.55_pm_r,346._pm_r, &
       213.10_pm_r,20649._pm_r,1._pm_r,171._pm_r,.59_pm_r,300._pm_r,2._pm_r,112._pm_r,.68_pm_r,340._pm_r, &
       219.00_pm_r,23816._pm_r,1._pm_r,213._pm_r,.69_pm_r,309._pm_r,1._pm_r,73._pm_r,.64_pm_r,330._pm_r, &
       224.30_pm_r,27060._pm_r,1._pm_r,264._pm_r,.74_pm_r,318._pm_r,1._pm_r,29._pm_r,.49_pm_r,311._pm_r, &
       229.40_pm_r,30381._pm_r,2._pm_r,291._pm_r,.75_pm_r,328._pm_r,1._pm_r,359._pm_r,.38_pm_r,263._pm_r, &
       237.50_pm_r,33795._pm_r,3._pm_r,304._pm_r,.66_pm_r,338._pm_r,1._pm_r,328._pm_r,.54_pm_r,212._pm_r, &
       246.90_pm_r,37345._pm_r,4._pm_r,312._pm_r,.48_pm_r,344._pm_r,1._pm_r,262._pm_r,.82_pm_r,188._pm_r, &
       257.10_pm_r,41031._pm_r,4._pm_r,316._pm_r,.25_pm_r,346._pm_r,2._pm_r,210._pm_r,1.04_pm_r,174._pm_r, &
       265.50_pm_r,44866._pm_r,4._pm_r,314._pm_r,.38_pm_r,255._pm_r,3._pm_r,192._pm_r,.86_pm_r,170._pm_r, &
       266.90_pm_r,48773._pm_r,5._pm_r,304._pm_r,.82_pm_r,226._pm_r,4._pm_r,188._pm_r,.65_pm_r,186._pm_r, &
       260.50_pm_r,52640._pm_r,5._pm_r,286._pm_r,1.20_pm_r,196._pm_r,5._pm_r,191._pm_r,.56_pm_r,224._pm_r, &
       248.50_pm_r,56374._pm_r,5._pm_r,261._pm_r,1.74_pm_r,165._pm_r,5._pm_r,197._pm_r,.60_pm_r,259._pm_r, &
       236.50_pm_r,59920._pm_r,5._pm_r,227._pm_r,1.99_pm_r,149._pm_r,6._pm_r,203._pm_r,.31_pm_r,263._pm_r, &
       227.20_pm_r,63316._pm_r,6._pm_r,198._pm_r,1.89_pm_r,143._pm_r,6._pm_r,206._pm_r,.05_pm_r,233._pm_r, &
       219.60_pm_r,66584._pm_r,8._pm_r,183._pm_r,1.42_pm_r,146._pm_r,6._pm_r,205._pm_r,.14_pm_r,134._pm_r, &
       214.10_pm_r,69759._pm_r,9._pm_r,177._pm_r,.86_pm_r,160._pm_r,6._pm_r,202._pm_r,.24_pm_r,138._pm_r, &
       211.10_pm_r,72871._pm_r,10._pm_r,177._pm_r,.54_pm_r,205._pm_r,6._pm_r,199._pm_r,.35_pm_r,147._pm_r, &
       208.40_pm_r,75945._pm_r,11._pm_r,181._pm_r,.69_pm_r,250._pm_r,7._pm_r,195._pm_r,.43_pm_r,152._pm_r, &
       205.20_pm_r,78975._pm_r,11._pm_r,187._pm_r,.94_pm_r,267._pm_r,7._pm_r,191._pm_r,.48_pm_r,155._pm_r, &
       202.20_pm_r,81958._pm_r,11._pm_r,195._pm_r,1.08_pm_r,274._pm_r,8._pm_r,188._pm_r,.49_pm_r,156._pm_r, &
       198.10_pm_r,84903._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.20_pm_r,87766._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.20_pm_r,90541._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.20_pm_r,93291._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.10_pm_r,96047._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,98825._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.60_pm_r,101652._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.40_pm_r,104592._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.00_pm_r,107786._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       267.20_pm_r,111564._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       353.60_pm_r,116541._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
       299.00_pm_r,-60._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.00_pm_r,4158._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.30_pm_r,8033._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.70_pm_r,11532._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.00_pm_r,14669._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.30_pm_r,17607._pm_r,2._pm_r,167._pm_r,.15_pm_r,295._pm_r,1._pm_r,72._pm_r,.10_pm_r,22._pm_r, &
       211.90_pm_r,20642._pm_r,2._pm_r,174._pm_r,.21_pm_r,298._pm_r,1._pm_r,62._pm_r,.12_pm_r,22._pm_r, &
       217.40_pm_r,23787._pm_r,2._pm_r,184._pm_r,.24_pm_r,299._pm_r,1._pm_r,55._pm_r,.11_pm_r,23._pm_r, &
       223.30_pm_r,27011._pm_r,2._pm_r,197._pm_r,.25_pm_r,302._pm_r,1._pm_r,51._pm_r,.07_pm_r,25._pm_r, &
       229.70_pm_r,30329._pm_r,1._pm_r,211._pm_r,.24_pm_r,305._pm_r,1._pm_r,51._pm_r,.02_pm_r,108._pm_r, &
       238.60_pm_r,33752._pm_r,1._pm_r,224._pm_r,.20_pm_r,309._pm_r,1._pm_r,55._pm_r,.10_pm_r,165._pm_r, &
       248.60_pm_r,37321._pm_r,2._pm_r,233._pm_r,.13_pm_r,315._pm_r,1._pm_r,66._pm_r,.19_pm_r,172._pm_r, &
       258.70_pm_r,41032._pm_r,2._pm_r,239._pm_r,.06_pm_r,329._pm_r,1._pm_r,87._pm_r,.27_pm_r,170._pm_r, &
       266.00_pm_r,44881._pm_r,2._pm_r,241._pm_r,.14_pm_r,249._pm_r,1._pm_r,107._pm_r,.23_pm_r,168._pm_r, &
       267.90_pm_r,48797._pm_r,2._pm_r,240._pm_r,.33_pm_r,221._pm_r,1._pm_r,120._pm_r,.18_pm_r,183._pm_r, &
       263.10_pm_r,52691._pm_r,2._pm_r,231._pm_r,.47_pm_r,185._pm_r,1._pm_r,131._pm_r,.18_pm_r,226._pm_r, &
       252.50_pm_r,56475._pm_r,3._pm_r,216._pm_r,.73_pm_r,150._pm_r,1._pm_r,146._pm_r,.26_pm_r,239._pm_r, &
       239.90_pm_r,60077._pm_r,3._pm_r,195._pm_r,.86_pm_r,132._pm_r,1._pm_r,162._pm_r,.29_pm_r,208._pm_r, &
       228.60_pm_r,63509._pm_r,4._pm_r,179._pm_r,.80_pm_r,126._pm_r,2._pm_r,170._pm_r,.40_pm_r,177._pm_r, &
       218.80_pm_r,66780._pm_r,5._pm_r,169._pm_r,.56_pm_r,133._pm_r,3._pm_r,169._pm_r,.57_pm_r,160._pm_r, &
       211.90_pm_r,69933._pm_r,5._pm_r,166._pm_r,.32_pm_r,164._pm_r,3._pm_r,165._pm_r,.74_pm_r,151._pm_r, &
       208.10_pm_r,73006._pm_r,6._pm_r,168._pm_r,.35_pm_r,221._pm_r,5._pm_r,161._pm_r,.90_pm_r,146._pm_r, &
       205.10_pm_r,76033._pm_r,6._pm_r,174._pm_r,.54_pm_r,246._pm_r,6._pm_r,157._pm_r,1.02_pm_r,143._pm_r, &
       202.90_pm_r,79016._pm_r,6._pm_r,182._pm_r,.70_pm_r,254._pm_r,8._pm_r,154._pm_r,1.09_pm_r,142._pm_r, &
       200.70_pm_r,81973._pm_r,7._pm_r,192._pm_r,.78_pm_r,257._pm_r,9._pm_r,152._pm_r,1.08_pm_r,141._pm_r, &
       196.70_pm_r,84894._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.40_pm_r,87738._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.40_pm_r,90511._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.90_pm_r,93270._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.90_pm_r,96038._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,98827._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.80_pm_r,101661._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.50_pm_r,104605._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.90_pm_r,107798._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       267.90_pm_r,111583._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       356.10_pm_r,116588._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
       300.10_pm_r,-100._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.10_pm_r,4127._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.80_pm_r,8007._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.10_pm_r,11513._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.00_pm_r,14654._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.00_pm_r,17591._pm_r,2._pm_r,169._pm_r,.11_pm_r,265._pm_r,0._pm_r,14._pm_r,.06_pm_r,45._pm_r, &
       211.20_pm_r,20619._pm_r,2._pm_r,174._pm_r,.14_pm_r,262._pm_r,0._pm_r,30._pm_r,.08_pm_r,45._pm_r, &
       216.40_pm_r,23751._pm_r,2._pm_r,181._pm_r,.17_pm_r,260._pm_r,0._pm_r,39._pm_r,.09_pm_r,49._pm_r, &
       222.40_pm_r,26959._pm_r,2._pm_r,189._pm_r,.17_pm_r,260._pm_r,1._pm_r,45._pm_r,.08_pm_r,60._pm_r, &
       228.90_pm_r,30266._pm_r,2._pm_r,196._pm_r,.15_pm_r,258._pm_r,1._pm_r,50._pm_r,.06_pm_r,76._pm_r, &
       238.60_pm_r,33685._pm_r,2._pm_r,201._pm_r,.10_pm_r,259._pm_r,1._pm_r,56._pm_r,.05_pm_r,112._pm_r, &
       248.80_pm_r,37253._pm_r,2._pm_r,203._pm_r,.04_pm_r,278._pm_r,1._pm_r,64._pm_r,.07_pm_r,153._pm_r, &
       258.30_pm_r,40960._pm_r,2._pm_r,203._pm_r,.04_pm_r,56._pm_r,1._pm_r,76._pm_r,.10_pm_r,169._pm_r, &
       265.30_pm_r,44799._pm_r,2._pm_r,203._pm_r,.06_pm_r,305._pm_r,1._pm_r,89._pm_r,.11_pm_r,129._pm_r, &
       267.40_pm_r,48705._pm_r,2._pm_r,207._pm_r,.15_pm_r,259._pm_r,1._pm_r,95._pm_r,.18_pm_r,93._pm_r, &
       264.00_pm_r,52599._pm_r,2._pm_r,209._pm_r,.18_pm_r,193._pm_r,1._pm_r,96._pm_r,.15_pm_r,62._pm_r, &
       254.10_pm_r,56397._pm_r,2._pm_r,202._pm_r,.47_pm_r,140._pm_r,1._pm_r,96._pm_r,.13_pm_r,312._pm_r, &
       241.90_pm_r,60023._pm_r,3._pm_r,186._pm_r,.67_pm_r,128._pm_r,1._pm_r,104._pm_r,.29_pm_r,254._pm_r, &
       230.50_pm_r,63483._pm_r,3._pm_r,173._pm_r,.64_pm_r,132._pm_r,1._pm_r,141._pm_r,.43_pm_r,220._pm_r, &
       220.50_pm_r,66781._pm_r,4._pm_r,167._pm_r,.48_pm_r,161._pm_r,1._pm_r,172._pm_r,.62_pm_r,193._pm_r, &
       212.70_pm_r,69955._pm_r,5._pm_r,170._pm_r,.51_pm_r,207._pm_r,2._pm_r,176._pm_r,.85_pm_r,176._pm_r, &
       207.70_pm_r,73035._pm_r,5._pm_r,178._pm_r,.76_pm_r,235._pm_r,4._pm_r,172._pm_r,1.10_pm_r,165._pm_r, &
       204.50_pm_r,76059._pm_r,6._pm_r,189._pm_r,1.03_pm_r,245._pm_r,5._pm_r,168._pm_r,1.33_pm_r,160._pm_r, &
       203.20_pm_r,79045._pm_r,7._pm_r,200._pm_r,1.21_pm_r,250._pm_r,7._pm_r,164._pm_r,1.47_pm_r,157._pm_r, &
       201.00_pm_r,82014._pm_r,8._pm_r,209._pm_r,1.29_pm_r,252._pm_r,9._pm_r,161._pm_r,1.51_pm_r,155._pm_r, &
       196.40_pm_r,84930._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.10_pm_r,87766._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.70_pm_r,90541._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,93306._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,96081._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.90_pm_r,98878._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.90_pm_r,101725._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.20_pm_r,104690._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.40_pm_r,107917._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       270.90_pm_r,111746._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       359.20_pm_r,116799._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
       301.30_pm_r,-106._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.60_pm_r,4133._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.20_pm_r,8019._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.60_pm_r,11531._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.30_pm_r,14677._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.10_pm_r,17616._pm_r,2._pm_r,173._pm_r,.17_pm_r,278._pm_r,0._pm_r,242._pm_r,.06_pm_r,38._pm_r, &
       211.30_pm_r,20644._pm_r,2._pm_r,182._pm_r,.22_pm_r,275._pm_r,0._pm_r,263._pm_r,.07_pm_r,36._pm_r, &
       216.80_pm_r,23780._pm_r,2._pm_r,193._pm_r,.23_pm_r,270._pm_r,0._pm_r,324._pm_r,.07_pm_r,41._pm_r, &
       222.70_pm_r,26995._pm_r,2._pm_r,203._pm_r,.22_pm_r,262._pm_r,0._pm_r,8._pm_r,.06_pm_r,48._pm_r, &
       229.40_pm_r,30305._pm_r,2._pm_r,210._pm_r,.18_pm_r,250._pm_r,0._pm_r,25._pm_r,.05_pm_r,69._pm_r, &
       237.90_pm_r,33723._pm_r,2._pm_r,213._pm_r,.13_pm_r,228._pm_r,0._pm_r,40._pm_r,.05_pm_r,114._pm_r, &
       247.30_pm_r,37278._pm_r,2._pm_r,213._pm_r,.09_pm_r,195._pm_r,0._pm_r,61._pm_r,.07_pm_r,145._pm_r, &
       256.70_pm_r,40967._pm_r,3._pm_r,211._pm_r,.08_pm_r,155._pm_r,0._pm_r,88._pm_r,.10_pm_r,151._pm_r, &
       264.00_pm_r,44787._pm_r,3._pm_r,210._pm_r,.02_pm_r,234._pm_r,0._pm_r,104._pm_r,.13_pm_r,117._pm_r, &
       266.50_pm_r,48677._pm_r,3._pm_r,212._pm_r,.15_pm_r,291._pm_r,1._pm_r,104._pm_r,.23_pm_r,99._pm_r, &
       262.20_pm_r,52555._pm_r,3._pm_r,217._pm_r,.12_pm_r,300._pm_r,1._pm_r,101._pm_r,.22_pm_r,95._pm_r, &
       253.00_pm_r,56335._pm_r,3._pm_r,218._pm_r,.14_pm_r,117._pm_r,1._pm_r,101._pm_r,.05_pm_r,139._pm_r, &
       242.70_pm_r,59964._pm_r,3._pm_r,208._pm_r,.40_pm_r,132._pm_r,1._pm_r,109._pm_r,.21_pm_r,230._pm_r, &
       231.50_pm_r,63440._pm_r,3._pm_r,196._pm_r,.52_pm_r,154._pm_r,1._pm_r,128._pm_r,.31_pm_r,219._pm_r, &
       222.10_pm_r,66756._pm_r,4._pm_r,191._pm_r,.63_pm_r,185._pm_r,1._pm_r,151._pm_r,.37_pm_r,192._pm_r, &
       214.70_pm_r,69954._pm_r,5._pm_r,192._pm_r,.84_pm_r,209._pm_r,2._pm_r,160._pm_r,.47_pm_r,167._pm_r, &
       209.40_pm_r,73056._pm_r,6._pm_r,198._pm_r,1.11_pm_r,224._pm_r,3._pm_r,159._pm_r,.61_pm_r,151._pm_r, &
       206.00_pm_r,76097._pm_r,8._pm_r,205._pm_r,1.35_pm_r,231._pm_r,4._pm_r,156._pm_r,.74_pm_r,142._pm_r, &
       204.60_pm_r,79099._pm_r,10._pm_r,211._pm_r,1.50_pm_r,235._pm_r,5._pm_r,152._pm_r,.83_pm_r,138._pm_r, &
       201.80_pm_r,82087._pm_r,12._pm_r,215._pm_r,1.54_pm_r,237._pm_r,6._pm_r,148._pm_r,.85_pm_r,135._pm_r, &
       196.00_pm_r,85000._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.50_pm_r,87825._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.20_pm_r,90594._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.50_pm_r,93348._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,96111._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.10_pm_r,98904._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.40_pm_r,101764._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.20_pm_r,104763._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.00_pm_r,108049._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.20_pm_r,111957._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       363.00_pm_r,117081._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
       300.70_pm_r,-119._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.40_pm_r,4119._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.50_pm_r,8012._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.60_pm_r,11532._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.00_pm_r,14696._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.70_pm_r,17660._pm_r,2._pm_r,148._pm_r,.54_pm_r,261._pm_r,1._pm_r,181._pm_r,.26_pm_r,360._pm_r, &
       212.40_pm_r,20708._pm_r,2._pm_r,170._pm_r,.68_pm_r,261._pm_r,0._pm_r,184._pm_r,.35_pm_r,358._pm_r, &
       217.80_pm_r,23859._pm_r,3._pm_r,195._pm_r,.72_pm_r,259._pm_r,0._pm_r,346._pm_r,.39_pm_r,358._pm_r, &
       223.40_pm_r,27088._pm_r,3._pm_r,212._pm_r,.68_pm_r,257._pm_r,1._pm_r,355._pm_r,.40_pm_r,357._pm_r, &
       229.80_pm_r,30407._pm_r,4._pm_r,221._pm_r,.52_pm_r,252._pm_r,1._pm_r,356._pm_r,.35_pm_r,357._pm_r, &
       238.20_pm_r,33829._pm_r,4._pm_r,225._pm_r,.33_pm_r,241._pm_r,2._pm_r,356._pm_r,.25_pm_r,355._pm_r, &
       246.40_pm_r,37381._pm_r,5._pm_r,226._pm_r,.19_pm_r,211._pm_r,2._pm_r,356._pm_r,.14_pm_r,354._pm_r, &
       255.60_pm_r,41051._pm_r,5._pm_r,224._pm_r,.16_pm_r,162._pm_r,2._pm_r,356._pm_r,.03_pm_r,360._pm_r, &
       263.90_pm_r,44865._pm_r,5._pm_r,222._pm_r,.17_pm_r,190._pm_r,2._pm_r,356._pm_r,.05_pm_r,135._pm_r, &
       264.80_pm_r,48745._pm_r,5._pm_r,221._pm_r,.29_pm_r,232._pm_r,2._pm_r,358._pm_r,.08_pm_r,120._pm_r, &
       258.20_pm_r,52578._pm_r,6._pm_r,222._pm_r,.24_pm_r,220._pm_r,2._pm_r,0._pm_r,.02_pm_r,53._pm_r, &
       250.20_pm_r,56303._pm_r,6._pm_r,220._pm_r,.35_pm_r,127._pm_r,2._pm_r,358._pm_r,.16_pm_r,308._pm_r, &
       242.30_pm_r,59910._pm_r,6._pm_r,212._pm_r,.66_pm_r,115._pm_r,2._pm_r,352._pm_r,.21_pm_r,290._pm_r, &
       232.10_pm_r,63387._pm_r,6._pm_r,203._pm_r,.66_pm_r,126._pm_r,2._pm_r,346._pm_r,.12_pm_r,265._pm_r, &
       222.70_pm_r,66712._pm_r,6._pm_r,196._pm_r,.55_pm_r,170._pm_r,2._pm_r,345._pm_r,.14_pm_r,145._pm_r, &
       215.60_pm_r,69921._pm_r,7._pm_r,196._pm_r,.84_pm_r,217._pm_r,2._pm_r,351._pm_r,.39_pm_r,127._pm_r, &
       211.00_pm_r,73041._pm_r,9._pm_r,202._pm_r,1.32_pm_r,234._pm_r,2._pm_r,11._pm_r,.62_pm_r,121._pm_r, &
       207.60_pm_r,76106._pm_r,11._pm_r,209._pm_r,1.75_pm_r,240._pm_r,2._pm_r,49._pm_r,.81_pm_r,119._pm_r, &
       205.20_pm_r,79127._pm_r,13._pm_r,216._pm_r,2.01_pm_r,244._pm_r,2._pm_r,79._pm_r,.91_pm_r,118._pm_r, &
       201.20_pm_r,82117._pm_r,16._pm_r,221._pm_r,2.11_pm_r,245._pm_r,4._pm_r,93._pm_r,.93_pm_r,117._pm_r, &
       194.40_pm_r,85010._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.30_pm_r,87805._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.70_pm_r,90539._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.80_pm_r,93254._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.70_pm_r,95981._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.50_pm_r,98752._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.00_pm_r,101618._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.20_pm_r,104659._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       231.50_pm_r,108026._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       283.00_pm_r,112041._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       367.10_pm_r,117247._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
       299.30_pm_r,-110._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.10_pm_r,4113._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.80_pm_r,7995._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.00_pm_r,11511._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.90_pm_r,14697._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.80_pm_r,17709._pm_r,3._pm_r,154._pm_r,.75_pm_r,259._pm_r,1._pm_r,182._pm_r,.33_pm_r,0._pm_r, &
       214.50_pm_r,20794._pm_r,3._pm_r,180._pm_r,.95_pm_r,258._pm_r,1._pm_r,184._pm_r,.45_pm_r,359._pm_r, &
       219.60_pm_r,23973._pm_r,3._pm_r,204._pm_r,1.01_pm_r,256._pm_r,0._pm_r,276._pm_r,.52_pm_r,358._pm_r, &
       225.20_pm_r,27227._pm_r,5._pm_r,219._pm_r,.95_pm_r,253._pm_r,1._pm_r,353._pm_r,.55_pm_r,356._pm_r, &
       230.90_pm_r,30568._pm_r,6._pm_r,226._pm_r,.76_pm_r,248._pm_r,2._pm_r,354._pm_r,.52_pm_r,353._pm_r, &
       238.30_pm_r,34000._pm_r,7._pm_r,228._pm_r,.51_pm_r,237._pm_r,2._pm_r,353._pm_r,.44_pm_r,349._pm_r, &
       247.00_pm_r,37555._pm_r,7._pm_r,228._pm_r,.31_pm_r,214._pm_r,3._pm_r,352._pm_r,.33_pm_r,342._pm_r, &
       256.30_pm_r,41238._pm_r,7._pm_r,227._pm_r,.25_pm_r,180._pm_r,3._pm_r,350._pm_r,.22_pm_r,329._pm_r, &
       263.30_pm_r,45050._pm_r,8._pm_r,225._pm_r,.23_pm_r,199._pm_r,4._pm_r,348._pm_r,.12_pm_r,321._pm_r, &
       263.90_pm_r,48918._pm_r,8._pm_r,225._pm_r,.29_pm_r,241._pm_r,4._pm_r,347._pm_r,.07_pm_r,312._pm_r, &
       256.80_pm_r,52735._pm_r,8._pm_r,226._pm_r,.17_pm_r,238._pm_r,4._pm_r,345._pm_r,.17_pm_r,285._pm_r, &
       248.00_pm_r,56434._pm_r,8._pm_r,225._pm_r,.32_pm_r,114._pm_r,4._pm_r,340._pm_r,.37_pm_r,278._pm_r, &
       239.80_pm_r,60006._pm_r,8._pm_r,220._pm_r,.60_pm_r,114._pm_r,4._pm_r,333._pm_r,.40_pm_r,271._pm_r, &
       230.30_pm_r,63452._pm_r,8._pm_r,214._pm_r,.52_pm_r,130._pm_r,4._pm_r,327._pm_r,.21_pm_r,258._pm_r, &
       221.60_pm_r,66757._pm_r,8._pm_r,210._pm_r,.40_pm_r,188._pm_r,4._pm_r,326._pm_r,.18_pm_r,128._pm_r, &
       215.10_pm_r,69954._pm_r,9._pm_r,211._pm_r,.75_pm_r,236._pm_r,4._pm_r,330._pm_r,.55_pm_r,108._pm_r, &
       210.90_pm_r,73071._pm_r,10._pm_r,216._pm_r,1.22_pm_r,251._pm_r,3._pm_r,343._pm_r,.88_pm_r,105._pm_r, &
       207.30_pm_r,76134._pm_r,12._pm_r,222._pm_r,1.63_pm_r,256._pm_r,3._pm_r,11._pm_r,1.14_pm_r,103._pm_r, &
       203.50_pm_r,79143._pm_r,14._pm_r,228._pm_r,1.86_pm_r,259._pm_r,3._pm_r,45._pm_r,1.29_pm_r,103._pm_r, &
       197.90_pm_r,82096._pm_r,17._pm_r,233._pm_r,1.93_pm_r,260._pm_r,5._pm_r,66._pm_r,1.32_pm_r,102._pm_r, &
       190.10_pm_r,84932._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.70_pm_r,87659._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.80_pm_r,90319._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.20_pm_r,92961._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.40_pm_r,95626._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.30_pm_r,98360._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.70_pm_r,101228._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.20_pm_r,104317._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       239.40_pm_r,107784._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       290.50_pm_r,111922._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       371.00_pm_r,117210._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
       295.10_pm_r,-79._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.30_pm_r,4089._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.10_pm_r,7921._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.80_pm_r,11397._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.00_pm_r,14609._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.40_pm_r,17709._pm_r,4._pm_r,183._pm_r,.59_pm_r,276._pm_r,1._pm_r,181._pm_r,.23_pm_r,34._pm_r, &
       217.50_pm_r,20857._pm_r,4._pm_r,199._pm_r,.75_pm_r,276._pm_r,0._pm_r,149._pm_r,.31_pm_r,31._pm_r, &
       221.50_pm_r,24072._pm_r,4._pm_r,214._pm_r,.83_pm_r,276._pm_r,1._pm_r,77._pm_r,.37_pm_r,25._pm_r, &
       226.30_pm_r,27348._pm_r,5._pm_r,227._pm_r,.77_pm_r,274._pm_r,1._pm_r,46._pm_r,.39_pm_r,17._pm_r, &
       232.50_pm_r,30708._pm_r,6._pm_r,234._pm_r,.61_pm_r,272._pm_r,1._pm_r,33._pm_r,.37_pm_r,5._pm_r, &
       240.20_pm_r,34165._pm_r,6._pm_r,238._pm_r,.39_pm_r,264._pm_r,2._pm_r,24._pm_r,.32_pm_r,351._pm_r, &
       250.10_pm_r,37757._pm_r,7._pm_r,239._pm_r,.23_pm_r,247._pm_r,2._pm_r,16._pm_r,.28_pm_r,336._pm_r, &
       259.70_pm_r,41489._pm_r,7._pm_r,239._pm_r,.16_pm_r,218._pm_r,2._pm_r,10._pm_r,.24_pm_r,318._pm_r, &
       265.30_pm_r,45341._pm_r,7._pm_r,238._pm_r,.31_pm_r,234._pm_r,3._pm_r,4._pm_r,.15_pm_r,306._pm_r, &
       264.50_pm_r,49228._pm_r,8._pm_r,240._pm_r,.53_pm_r,267._pm_r,3._pm_r,1._pm_r,.10_pm_r,287._pm_r, &
       256.80_pm_r,53046._pm_r,8._pm_r,243._pm_r,.44_pm_r,288._pm_r,3._pm_r,356._pm_r,.23_pm_r,286._pm_r, &
       247.60_pm_r,56742._pm_r,9._pm_r,245._pm_r,.38_pm_r,43._pm_r,3._pm_r,347._pm_r,.55_pm_r,291._pm_r, &
       238.30_pm_r,60300._pm_r,8._pm_r,245._pm_r,.84_pm_r,73._pm_r,4._pm_r,335._pm_r,.70_pm_r,296._pm_r, &
       228.40_pm_r,63720._pm_r,6._pm_r,243._pm_r,.95_pm_r,75._pm_r,5._pm_r,327._pm_r,.62_pm_r,301._pm_r, &
       220.30_pm_r,67001._pm_r,5._pm_r,241._pm_r,.73_pm_r,66._pm_r,5._pm_r,324._pm_r,.41_pm_r,314._pm_r, &
       213.80_pm_r,70180._pm_r,4._pm_r,242._pm_r,.44_pm_r,36._pm_r,6._pm_r,325._pm_r,.20_pm_r,357._pm_r, &
       208.00_pm_r,73266._pm_r,4._pm_r,249._pm_r,.47_pm_r,339._pm_r,6._pm_r,327._pm_r,.28_pm_r,62._pm_r, &
       202.30_pm_r,76273._pm_r,4._pm_r,260._pm_r,.70_pm_r,313._pm_r,6._pm_r,333._pm_r,.46_pm_r,81._pm_r, &
       196.60_pm_r,79191._pm_r,5._pm_r,270._pm_r,.89_pm_r,306._pm_r,5._pm_r,340._pm_r,.56_pm_r,87._pm_r, &
       190.00_pm_r,82030._pm_r,6._pm_r,277._pm_r,.97_pm_r,302._pm_r,5._pm_r,349._pm_r,.61_pm_r,90._pm_r, &
       182.40_pm_r,84751._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       176.30_pm_r,87366._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       172.70_pm_r,89916._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       173.10_pm_r,92458._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       177.10_pm_r,95043._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.70_pm_r,97730._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.40_pm_r,100595._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.20_pm_r,103738._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       248.40_pm_r,107320._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       297.40_pm_r,111588._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       374.20_pm_r,116942._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
       287.20_pm_r,-46._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       268.90_pm_r,4021._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.60_pm_r,7770._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.40_pm_r,11192._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.40_pm_r,14438._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.60_pm_r,17649._pm_r,4._pm_r,205._pm_r,.29_pm_r,330._pm_r,1._pm_r,80._pm_r,.06_pm_r,333._pm_r, &
       221.60_pm_r,20878._pm_r,4._pm_r,211._pm_r,.38_pm_r,326._pm_r,1._pm_r,73._pm_r,.10_pm_r,332._pm_r, &
       223.70_pm_r,24137._pm_r,4._pm_r,220._pm_r,.43_pm_r,321._pm_r,1._pm_r,63._pm_r,.13_pm_r,329._pm_r, &
       228.30_pm_r,27442._pm_r,4._pm_r,230._pm_r,.41_pm_r,311._pm_r,1._pm_r,48._pm_r,.17_pm_r,327._pm_r, &
       235.60_pm_r,30840._pm_r,4._pm_r,238._pm_r,.36_pm_r,295._pm_r,1._pm_r,33._pm_r,.18_pm_r,324._pm_r, &
       243.90_pm_r,34348._pm_r,4._pm_r,243._pm_r,.33_pm_r,274._pm_r,1._pm_r,19._pm_r,.19_pm_r,323._pm_r, &
       252.80_pm_r,37987._pm_r,5._pm_r,245._pm_r,.32_pm_r,254._pm_r,1._pm_r,8._pm_r,.18_pm_r,319._pm_r, &
       261.80_pm_r,41753._pm_r,5._pm_r,245._pm_r,.32_pm_r,241._pm_r,1._pm_r,0._pm_r,.16_pm_r,317._pm_r, &
       267.70_pm_r,45637._pm_r,6._pm_r,244._pm_r,.29_pm_r,230._pm_r,2._pm_r,354._pm_r,.14_pm_r,301._pm_r, &
       267.30_pm_r,49562._pm_r,6._pm_r,243._pm_r,.31_pm_r,239._pm_r,2._pm_r,348._pm_r,.13_pm_r,301._pm_r, &
       259.50_pm_r,53421._pm_r,6._pm_r,243._pm_r,.26_pm_r,254._pm_r,2._pm_r,344._pm_r,.15_pm_r,305._pm_r, &
       249.60_pm_r,57152._pm_r,7._pm_r,244._pm_r,.13_pm_r,272._pm_r,2._pm_r,340._pm_r,.20_pm_r,309._pm_r, &
       239.60_pm_r,60732._pm_r,7._pm_r,245._pm_r,.11_pm_r,327._pm_r,2._pm_r,336._pm_r,.16_pm_r,314._pm_r, &
       229.70_pm_r,64170._pm_r,7._pm_r,247._pm_r,.19_pm_r,316._pm_r,3._pm_r,335._pm_r,.08_pm_r,338._pm_r, &
       220.90_pm_r,67467._pm_r,7._pm_r,250._pm_r,.35_pm_r,300._pm_r,3._pm_r,337._pm_r,.10_pm_r,75._pm_r, &
       212.20_pm_r,70641._pm_r,8._pm_r,253._pm_r,.54_pm_r,290._pm_r,2._pm_r,342._pm_r,.22_pm_r,95._pm_r, &
       203.70_pm_r,73684._pm_r,8._pm_r,257._pm_r,.70_pm_r,287._pm_r,2._pm_r,351._pm_r,.31_pm_r,99._pm_r, &
       194.90_pm_r,76605._pm_r,9._pm_r,260._pm_r,.80_pm_r,285._pm_r,2._pm_r,3._pm_r,.37_pm_r,100._pm_r, &
       187.20_pm_r,79397._pm_r,10._pm_r,263._pm_r,.83_pm_r,284._pm_r,2._pm_r,18._pm_r,.39_pm_r,102._pm_r, &
       179.70_pm_r,82085._pm_r,12._pm_r,265._pm_r,.81_pm_r,284._pm_r,2._pm_r,32._pm_r,.39_pm_r,102._pm_r, &
       172.40_pm_r,84655._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       167.20_pm_r,87127._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       164.60_pm_r,89548._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       166.40_pm_r,91979._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       172.40_pm_r,94479._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.90_pm_r,97113._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.10_pm_r,99975._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.60_pm_r,103177._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       257.80_pm_r,106881._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       302.80_pm_r,111271._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       376.70_pm_r,116668._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
       284.00_pm_r,-46._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       264.70_pm_r,3964._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       239.40_pm_r,7647._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.50_pm_r,11037._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.70_pm_r,14312._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.50_pm_r,17595._pm_r,4._pm_r,192._pm_r,.46_pm_r,331._pm_r,2._pm_r,65._pm_r,.16_pm_r,279._pm_r, &
       225.10_pm_r,20887._pm_r,4._pm_r,201._pm_r,.61_pm_r,330._pm_r,1._pm_r,58._pm_r,.22_pm_r,280._pm_r, &
       225.80_pm_r,24186._pm_r,3._pm_r,216._pm_r,.67_pm_r,329._pm_r,1._pm_r,45._pm_r,.27_pm_r,281._pm_r, &
       229.80_pm_r,27516._pm_r,3._pm_r,234._pm_r,.60_pm_r,327._pm_r,1._pm_r,21._pm_r,.28_pm_r,283._pm_r, &
       237.20_pm_r,30935._pm_r,3._pm_r,250._pm_r,.44_pm_r,322._pm_r,1._pm_r,356._pm_r,.25_pm_r,284._pm_r, &
       246.30_pm_r,34472._pm_r,3._pm_r,259._pm_r,.25_pm_r,310._pm_r,1._pm_r,339._pm_r,.20_pm_r,285._pm_r, &
       255.40_pm_r,38149._pm_r,3._pm_r,261._pm_r,.13_pm_r,272._pm_r,1._pm_r,329._pm_r,.15_pm_r,290._pm_r, &
       264.60_pm_r,41955._pm_r,4._pm_r,261._pm_r,.13_pm_r,231._pm_r,1._pm_r,325._pm_r,.10_pm_r,299._pm_r, &
       270.30_pm_r,45882._pm_r,4._pm_r,258._pm_r,.17_pm_r,219._pm_r,2._pm_r,325._pm_r,.10_pm_r,345._pm_r, &
       269.10_pm_r,49838._pm_r,4._pm_r,256._pm_r,.17_pm_r,234._pm_r,2._pm_r,328._pm_r,.21_pm_r,346._pm_r, &
       262.80_pm_r,53735._pm_r,4._pm_r,256._pm_r,.02_pm_r,63._pm_r,2._pm_r,329._pm_r,.33_pm_r,329._pm_r, &
       254.10_pm_r,57524._pm_r,4._pm_r,256._pm_r,.35_pm_r,69._pm_r,3._pm_r,327._pm_r,.41_pm_r,308._pm_r, &
       244.50_pm_r,61174._pm_r,3._pm_r,258._pm_r,.36_pm_r,57._pm_r,3._pm_r,322._pm_r,.31_pm_r,266._pm_r, &
       234.30_pm_r,64683._pm_r,3._pm_r,263._pm_r,.18_pm_r,348._pm_r,3._pm_r,313._pm_r,.40_pm_r,210._pm_r, &
       223.50_pm_r,68034._pm_r,3._pm_r,268._pm_r,.58_pm_r,281._pm_r,3._pm_r,299._pm_r,.66_pm_r,184._pm_r, &
       211.80_pm_r,71226._pm_r,5._pm_r,270._pm_r,1.08_pm_r,272._pm_r,2._pm_r,276._pm_r,.87_pm_r,173._pm_r, &
       199.30_pm_r,74233._pm_r,6._pm_r,270._pm_r,1.45_pm_r,268._pm_r,2._pm_r,242._pm_r,.99_pm_r,167._pm_r, &
       187.60_pm_r,77068._pm_r,9._pm_r,269._pm_r,1.64_pm_r,267._pm_r,3._pm_r,215._pm_r,1.03_pm_r,164._pm_r, &
       178.40_pm_r,79740._pm_r,11._pm_r,269._pm_r,1.70_pm_r,267._pm_r,4._pm_r,198._pm_r,1.00_pm_r,162._pm_r, &
       169.90_pm_r,82288._pm_r,14._pm_r,268._pm_r,1.60_pm_r,266._pm_r,5._pm_r,189._pm_r,.91_pm_r,160._pm_r, &
       162.40_pm_r,84705._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       158.10_pm_r,87031._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       156.90_pm_r,89327._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       160.20_pm_r,91654._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       168.10_pm_r,94073._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.20_pm_r,96660._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.80_pm_r,99518._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.00_pm_r,102780._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       266.60_pm_r,106603._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       306.00_pm_r,111093._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       378.30_pm_r,116508._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
       279.50_pm_r,-40._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       261.50_pm_r,3911._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       236.40_pm_r,7546._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.90_pm_r,10922._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.70_pm_r,14227._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.40_pm_r,17554._pm_r,3._pm_r,176._pm_r,.43_pm_r,318._pm_r,1._pm_r,50._pm_r,.05_pm_r,8._pm_r, &
       227.80_pm_r,20887._pm_r,2._pm_r,187._pm_r,.57_pm_r,317._pm_r,1._pm_r,45._pm_r,.08_pm_r,0._pm_r, &
       227.60_pm_r,24218._pm_r,2._pm_r,207._pm_r,.61_pm_r,315._pm_r,1._pm_r,37._pm_r,.09_pm_r,344._pm_r, &
       230.90_pm_r,27569._pm_r,2._pm_r,233._pm_r,.54_pm_r,313._pm_r,1._pm_r,29._pm_r,.10_pm_r,327._pm_r, &
       238.30_pm_r,31004._pm_r,2._pm_r,251._pm_r,.37_pm_r,310._pm_r,1._pm_r,19._pm_r,.11_pm_r,308._pm_r, &
       247.70_pm_r,34558._pm_r,2._pm_r,259._pm_r,.19_pm_r,299._pm_r,1._pm_r,9._pm_r,.13_pm_r,296._pm_r, &
       257.50_pm_r,38261._pm_r,3._pm_r,261._pm_r,.07_pm_r,258._pm_r,1._pm_r,358._pm_r,.13_pm_r,288._pm_r, &
       266.90_pm_r,42099._pm_r,3._pm_r,260._pm_r,.06_pm_r,195._pm_r,1._pm_r,348._pm_r,.13_pm_r,283._pm_r, &
       272.80_pm_r,46059._pm_r,3._pm_r,258._pm_r,.02_pm_r,212._pm_r,1._pm_r,340._pm_r,.13_pm_r,289._pm_r, &
       272.20_pm_r,50056._pm_r,3._pm_r,260._pm_r,.12_pm_r,333._pm_r,1._pm_r,333._pm_r,.28_pm_r,308._pm_r, &
       266.30_pm_r,54002._pm_r,3._pm_r,266._pm_r,.27_pm_r,340._pm_r,2._pm_r,326._pm_r,.36_pm_r,306._pm_r, &
       258.70_pm_r,57849._pm_r,3._pm_r,274._pm_r,.31_pm_r,345._pm_r,2._pm_r,321._pm_r,.28_pm_r,281._pm_r, &
       249.70_pm_r,61571._pm_r,3._pm_r,281._pm_r,.19_pm_r,344._pm_r,2._pm_r,313._pm_r,.24_pm_r,213._pm_r, &
       238.40_pm_r,65152._pm_r,3._pm_r,283._pm_r,.01_pm_r,135._pm_r,2._pm_r,303._pm_r,.34_pm_r,185._pm_r, &
       224.90_pm_r,68542._pm_r,3._pm_r,280._pm_r,.24_pm_r,166._pm_r,2._pm_r,291._pm_r,.34_pm_r,179._pm_r, &
       210.90_pm_r,71737._pm_r,3._pm_r,271._pm_r,.43_pm_r,169._pm_r,2._pm_r,277._pm_r,.28_pm_r,180._pm_r, &
       197.50_pm_r,74723._pm_r,3._pm_r,256._pm_r,.55_pm_r,169._pm_r,2._pm_r,267._pm_r,.19_pm_r,187._pm_r, &
       184.90_pm_r,77526._pm_r,3._pm_r,240._pm_r,.59_pm_r,171._pm_r,2._pm_r,260._pm_r,.12_pm_r,201._pm_r, &
       174.70_pm_r,80149._pm_r,3._pm_r,227._pm_r,.57_pm_r,171._pm_r,2._pm_r,257._pm_r,.09_pm_r,223._pm_r, &
       164.40_pm_r,82636._pm_r,4._pm_r,217._pm_r,.52_pm_r,171._pm_r,2._pm_r,256._pm_r,.07_pm_r,249._pm_r, &
       155.00_pm_r,84940._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       150.60_pm_r,87145._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       150.60_pm_r,89340._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       155.20_pm_r,91582._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       164.50_pm_r,93936._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.80_pm_r,96485._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.50_pm_r,99340._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.70_pm_r,102655._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.00_pm_r,106581._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       307.20_pm_r,111141._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       379.40_pm_r,116557._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/) 
  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
       273.50_pm_r,-29._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       258.70_pm_r,3855._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       234.00_pm_r,7451._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.90_pm_r,10823._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.20_pm_r,14159._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.60_pm_r,17519._pm_r,1._pm_r,212._pm_r,.09_pm_r,74._pm_r,0._pm_r,13._pm_r,.05_pm_r,34._pm_r, &
       229.80_pm_r,20884._pm_r,1._pm_r,208._pm_r,.10_pm_r,61._pm_r,0._pm_r,24._pm_r,.06_pm_r,39._pm_r, &
       229.30_pm_r,24243._pm_r,1._pm_r,205._pm_r,.12_pm_r,28._pm_r,0._pm_r,33._pm_r,.06_pm_r,63._pm_r, &
       232.00_pm_r,27615._pm_r,1._pm_r,209._pm_r,.16_pm_r,353._pm_r,0._pm_r,43._pm_r,.05_pm_r,112._pm_r, &
       238.80_pm_r,31061._pm_r,1._pm_r,230._pm_r,.24_pm_r,332._pm_r,0._pm_r,60._pm_r,.09_pm_r,144._pm_r, &
       247.90_pm_r,34620._pm_r,1._pm_r,264._pm_r,.29_pm_r,322._pm_r,0._pm_r,85._pm_r,.13_pm_r,162._pm_r, &
       258.10_pm_r,38328._pm_r,1._pm_r,285._pm_r,.31_pm_r,317._pm_r,0._pm_r,113._pm_r,.15_pm_r,169._pm_r, &
       267.90_pm_r,42178._pm_r,1._pm_r,294._pm_r,.27_pm_r,315._pm_r,1._pm_r,132._pm_r,.15_pm_r,172._pm_r, &
       274.70_pm_r,46159._pm_r,2._pm_r,295._pm_r,.14_pm_r,278._pm_r,1._pm_r,132._pm_r,.14_pm_r,86._pm_r, &
       275.30_pm_r,50192._pm_r,2._pm_r,292._pm_r,.21_pm_r,273._pm_r,1._pm_r,115._pm_r,.28_pm_r,69._pm_r, &
       269.70_pm_r,54186._pm_r,2._pm_r,297._pm_r,.62_pm_r,330._pm_r,1._pm_r,106._pm_r,.29_pm_r,109._pm_r, &
       262.20_pm_r,58084._pm_r,4._pm_r,314._pm_r,1.46_pm_r,349._pm_r,2._pm_r,118._pm_r,.56_pm_r,169._pm_r, &
       253.20_pm_r,61857._pm_r,6._pm_r,329._pm_r,1.61_pm_r,358._pm_r,2._pm_r,141._pm_r,.77_pm_r,197._pm_r, &
       241.50_pm_r,65486._pm_r,7._pm_r,338._pm_r,1.09_pm_r,10._pm_r,3._pm_r,163._pm_r,.80_pm_r,227._pm_r, &
       226.70_pm_r,68912._pm_r,8._pm_r,343._pm_r,.34_pm_r,84._pm_r,3._pm_r,183._pm_r,.85_pm_r,264._pm_r, &
       212.10_pm_r,72128._pm_r,7._pm_r,345._pm_r,1.12_pm_r,158._pm_r,3._pm_r,207._pm_r,1.07_pm_r,291._pm_r, &
       198.40_pm_r,75130._pm_r,5._pm_r,346._pm_r,1.92_pm_r,167._pm_r,4._pm_r,235._pm_r,1.28_pm_r,307._pm_r, &
       185.60_pm_r,77946._pm_r,2._pm_r,340._pm_r,2.37_pm_r,171._pm_r,5._pm_r,259._pm_r,1.37_pm_r,315._pm_r, &
       174.90_pm_r,80576._pm_r,2._pm_r,181._pm_r,2.47_pm_r,172._pm_r,6._pm_r,276._pm_r,1.33_pm_r,319._pm_r, &
       162.90_pm_r,83061._pm_r,6._pm_r,175._pm_r,2.33_pm_r,173._pm_r,7._pm_r,286._pm_r,1.21_pm_r,322._pm_r, &
       150.80_pm_r,85301._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       145.70_pm_r,87429._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       146.60_pm_r,89557._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       152.00_pm_r,91745._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       162.10_pm_r,94058._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       178.80_pm_r,96580._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.60_pm_r,99434._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       239.60_pm_r,102786._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       278.80_pm_r,106782._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       307.30_pm_r,111383._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       380.00_pm_r,116792._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_aout = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_aout.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_aout,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_aout
