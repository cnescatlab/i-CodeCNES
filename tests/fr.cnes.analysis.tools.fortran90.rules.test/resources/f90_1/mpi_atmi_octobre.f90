subroutine mpi_atmi_octobre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de OCTOBRE 
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
       267.00_pm_r,-221._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.60_pm_r,3499._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.20_pm_r,6877._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.70_pm_r,9985._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.70_pm_r,12966._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.70_pm_r,15938._pm_r,21._pm_r,224._pm_r,2.89_pm_r,147._pm_r,4._pm_r,122._pm_r,.65_pm_r,308._pm_r, &
       207.20_pm_r,18943._pm_r,22._pm_r,211._pm_r,4.34_pm_r,142._pm_r,2._pm_r,119._pm_r,.92_pm_r,309._pm_r, &
       212.70_pm_r,22020._pm_r,25._pm_r,196._pm_r,4.91_pm_r,133._pm_r,1._pm_r,104._pm_r,.86_pm_r,311._pm_r, &
       217.00_pm_r,25157._pm_r,28._pm_r,183._pm_r,4.16_pm_r,121._pm_r,1._pm_r,28._pm_r,.50_pm_r,317._pm_r, &
       233.60_pm_r,28452._pm_r,31._pm_r,174._pm_r,3.07_pm_r,103._pm_r,1._pm_r,1._pm_r,.16_pm_r,356._pm_r, &
       255.00_pm_r,32024._pm_r,31._pm_r,167._pm_r,2.25_pm_r,76._pm_r,1._pm_r,11._pm_r,.25_pm_r,95._pm_r, &
       272.80_pm_r,35899._pm_r,31._pm_r,162._pm_r,1.90_pm_r,41._pm_r,1._pm_r,39._pm_r,.41_pm_r,108._pm_r, &
       283.80_pm_r,39980._pm_r,29._pm_r,158._pm_r,1.95_pm_r,9._pm_r,1._pm_r,65._pm_r,.46_pm_r,114._pm_r, &
       284.20_pm_r,44143._pm_r,26._pm_r,155._pm_r,2.31_pm_r,354._pm_r,2._pm_r,80._pm_r,.31_pm_r,127._pm_r, &
       281.60_pm_r,48291._pm_r,22._pm_r,153._pm_r,2.78_pm_r,349._pm_r,2._pm_r,87._pm_r,.09_pm_r,186._pm_r, &
       273.30_pm_r,52358._pm_r,18._pm_r,149._pm_r,3.11_pm_r,347._pm_r,2._pm_r,88._pm_r,.22_pm_r,275._pm_r, &
       263.50_pm_r,56290._pm_r,14._pm_r,144._pm_r,3.05_pm_r,345._pm_r,1._pm_r,85._pm_r,.26_pm_r,288._pm_r, &
       249.80_pm_r,60044._pm_r,10._pm_r,139._pm_r,2.31_pm_r,326._pm_r,1._pm_r,77._pm_r,.17_pm_r,302._pm_r, &
       238.40_pm_r,63618._pm_r,7._pm_r,140._pm_r,1.72_pm_r,301._pm_r,1._pm_r,68._pm_r,.14_pm_r,12._pm_r, &
       232.20_pm_r,67059._pm_r,5._pm_r,154._pm_r,1.43_pm_r,272._pm_r,1._pm_r,61._pm_r,.29_pm_r,42._pm_r, &
       226.20_pm_r,70420._pm_r,5._pm_r,176._pm_r,1.31_pm_r,247._pm_r,2._pm_r,57._pm_r,.46_pm_r,48._pm_r, &
       217.60_pm_r,73670._pm_r,6._pm_r,191._pm_r,1.24_pm_r,228._pm_r,3._pm_r,54._pm_r,.59_pm_r,47._pm_r, &
       208.20_pm_r,76791._pm_r,8._pm_r,197._pm_r,1.18_pm_r,214._pm_r,4._pm_r,52._pm_r,.68_pm_r,46._pm_r, &
       198.50_pm_r,79766._pm_r,10._pm_r,199._pm_r,1.09_pm_r,204._pm_r,5._pm_r,51._pm_r,.69_pm_r,46._pm_r, &
       184.40_pm_r,82605._pm_r,11._pm_r,199._pm_r,.97_pm_r,196._pm_r,6._pm_r,50._pm_r,.65_pm_r,45._pm_r, &
       167.10_pm_r,85106._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       158.00_pm_r,87430._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       157.00_pm_r,89726._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       161.50_pm_r,92061._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       170.50_pm_r,94502._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.20_pm_r,97133._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.10_pm_r,100072._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       239.20_pm_r,103456._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       273.80_pm_r,107408._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       309.70_pm_r,111965._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       395.20_pm_r,117550._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
       271.50_pm_r,-39._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       244.90_pm_r,3731._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.00_pm_r,7141._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.50_pm_r,10284._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.10_pm_r,13319._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.00_pm_r,16359._pm_r,35._pm_r,217._pm_r,5.21_pm_r,137._pm_r,7._pm_r,102._pm_r,.97_pm_r,284._pm_r, &
       210.80_pm_r,19421._pm_r,38._pm_r,203._pm_r,7.27_pm_r,134._pm_r,5._pm_r,100._pm_r,1.15_pm_r,290._pm_r, &
       217.10_pm_r,22558._pm_r,42._pm_r,189._pm_r,7.80_pm_r,128._pm_r,4._pm_r,94._pm_r,.88_pm_r,305._pm_r, &
       221.80_pm_r,25766._pm_r,48._pm_r,177._pm_r,6.35_pm_r,118._pm_r,3._pm_r,80._pm_r,.50_pm_r,3._pm_r, &
       234.40_pm_r,29102._pm_r,51._pm_r,169._pm_r,4.18_pm_r,98._pm_r,4._pm_r,72._pm_r,.95_pm_r,62._pm_r, &
       251.00_pm_r,32652._pm_r,52._pm_r,164._pm_r,2.81_pm_r,55._pm_r,6._pm_r,71._pm_r,1.51_pm_r,76._pm_r, &
       265.40_pm_r,36442._pm_r,49._pm_r,160._pm_r,3.20_pm_r,9._pm_r,8._pm_r,74._pm_r,1.78_pm_r,82._pm_r, &
       275.20_pm_r,40404._pm_r,44._pm_r,158._pm_r,4.06_pm_r,346._pm_r,11._pm_r,77._pm_r,1.74_pm_r,87._pm_r, &
       277.20_pm_r,44454._pm_r,37._pm_r,158._pm_r,4.71_pm_r,340._pm_r,13._pm_r,79._pm_r,1.04_pm_r,97._pm_r, &
       275.10_pm_r,48504._pm_r,30._pm_r,158._pm_r,5.35_pm_r,338._pm_r,14._pm_r,81._pm_r,.40_pm_r,130._pm_r, &
       267.20_pm_r,52478._pm_r,22._pm_r,158._pm_r,5.57_pm_r,337._pm_r,14._pm_r,82._pm_r,.28_pm_r,213._pm_r, &
       258.10_pm_r,56326._pm_r,14._pm_r,158._pm_r,4.97_pm_r,338._pm_r,13._pm_r,83._pm_r,.20_pm_r,257._pm_r, &
       247.10_pm_r,60022._pm_r,8._pm_r,161._pm_r,3.10_pm_r,327._pm_r,13._pm_r,83._pm_r,.16_pm_r,304._pm_r, &
       238.10_pm_r,63575._pm_r,5._pm_r,176._pm_r,1.65_pm_r,301._pm_r,13._pm_r,82._pm_r,.19_pm_r,328._pm_r, &
       232.60_pm_r,67018._pm_r,5._pm_r,197._pm_r,1.24_pm_r,255._pm_r,13._pm_r,81._pm_r,.20_pm_r,321._pm_r, &
       227.00_pm_r,70387._pm_r,7._pm_r,208._pm_r,1.39_pm_r,228._pm_r,13._pm_r,80._pm_r,.22_pm_r,304._pm_r, &
       219.10_pm_r,73653._pm_r,9._pm_r,211._pm_r,1.51_pm_r,218._pm_r,12._pm_r,79._pm_r,.25_pm_r,289._pm_r, &
       209.80_pm_r,76798._pm_r,11._pm_r,212._pm_r,1.48_pm_r,214._pm_r,12._pm_r,78._pm_r,.28_pm_r,278._pm_r, &
       199.90_pm_r,79795._pm_r,13._pm_r,212._pm_r,1.36_pm_r,213._pm_r,12._pm_r,77._pm_r,.29_pm_r,272._pm_r, &
       186.40_pm_r,82653._pm_r,15._pm_r,212._pm_r,1.19_pm_r,212._pm_r,11._pm_r,77._pm_r,.27_pm_r,269._pm_r, &
       170.40_pm_r,85206._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       161.60_pm_r,87589._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       159.90_pm_r,89934._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       163.70_pm_r,92307._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       171.80_pm_r,94774._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.30_pm_r,97419._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.50_pm_r,100348._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.80_pm_r,103695._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       270.90_pm_r,107595._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       310.70_pm_r,112140._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       394.60_pm_r,117737._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
       276.00_pm_r,23._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       250.10_pm_r,3867._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.30_pm_r,7349._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.30_pm_r,10561._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.30_pm_r,13678._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.00_pm_r,16810._pm_r,36._pm_r,211._pm_r,5.52_pm_r,130._pm_r,7._pm_r,73._pm_r,.62_pm_r,272._pm_r, &
       216.30_pm_r,19954._pm_r,38._pm_r,196._pm_r,7.36_pm_r,126._pm_r,6._pm_r,70._pm_r,.59_pm_r,278._pm_r, &
       224.10_pm_r,23184._pm_r,43._pm_r,182._pm_r,7.92_pm_r,121._pm_r,6._pm_r,66._pm_r,.23_pm_r,312._pm_r, &
       226.90_pm_r,26488._pm_r,49._pm_r,170._pm_r,6.56_pm_r,111._pm_r,6._pm_r,65._pm_r,.58_pm_r,67._pm_r, &
       231.80_pm_r,29842._pm_r,52._pm_r,162._pm_r,4.33_pm_r,88._pm_r,8._pm_r,67._pm_r,1.35_pm_r,78._pm_r, &
       242.60_pm_r,33309._pm_r,52._pm_r,156._pm_r,3.33_pm_r,38._pm_r,10._pm_r,70._pm_r,1.92_pm_r,81._pm_r, &
       253.70_pm_r,36947._pm_r,48._pm_r,153._pm_r,4.40_pm_r,358._pm_r,13._pm_r,73._pm_r,2.13_pm_r,83._pm_r, &
       263.90_pm_r,40736._pm_r,41._pm_r,150._pm_r,5.49_pm_r,341._pm_r,16._pm_r,75._pm_r,1.95_pm_r,86._pm_r, &
       269.70_pm_r,44651._pm_r,32._pm_r,147._pm_r,5.91_pm_r,337._pm_r,18._pm_r,76._pm_r,.94_pm_r,83._pm_r, &
       269.20_pm_r,48605._pm_r,24._pm_r,144._pm_r,6.24_pm_r,335._pm_r,19._pm_r,76._pm_r,.15_pm_r,344._pm_r, &
       261.80_pm_r,52497._pm_r,15._pm_r,139._pm_r,6.02_pm_r,331._pm_r,18._pm_r,75._pm_r,.59_pm_r,280._pm_r, &
       252.70_pm_r,56266._pm_r,7._pm_r,127._pm_r,4.93_pm_r,325._pm_r,17._pm_r,74._pm_r,.47_pm_r,271._pm_r, &
       244.50_pm_r,59903._pm_r,2._pm_r,90._pm_r,2.91_pm_r,306._pm_r,17._pm_r,74._pm_r,.25_pm_r,208._pm_r, &
       238.20_pm_r,63438._pm_r,2._pm_r,313._pm_r,1.75_pm_r,269._pm_r,17._pm_r,76._pm_r,.38_pm_r,157._pm_r, &
       232.90_pm_r,66886._pm_r,4._pm_r,276._pm_r,1.60_pm_r,233._pm_r,17._pm_r,78._pm_r,.44_pm_r,142._pm_r, &
       227.60_pm_r,70260._pm_r,5._pm_r,256._pm_r,1.61_pm_r,215._pm_r,17._pm_r,79._pm_r,.38_pm_r,135._pm_r, &
       220.80_pm_r,73544._pm_r,7._pm_r,243._pm_r,1.49_pm_r,209._pm_r,18._pm_r,81._pm_r,.27_pm_r,130._pm_r, &
       211.90_pm_r,76718._pm_r,9._pm_r,235._pm_r,1.29_pm_r,206._pm_r,18._pm_r,81._pm_r,.16_pm_r,120._pm_r, &
       201.60_pm_r,79745._pm_r,10._pm_r,230._pm_r,1.06_pm_r,205._pm_r,18._pm_r,82._pm_r,.10_pm_r,102._pm_r, &
       188.90_pm_r,82623._pm_r,12._pm_r,227._pm_r,.84_pm_r,204._pm_r,18._pm_r,82._pm_r,.06_pm_r,72._pm_r, &
       175.20_pm_r,85251._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       167.10_pm_r,87722._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       164.50_pm_r,90142._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       167.20_pm_r,92576._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       173.90_pm_r,95087._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.40_pm_r,97751._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.20_pm_r,100666._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.70_pm_r,103960._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       265.90_pm_r,107780._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       311.40_pm_r,112292._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       393.50_pm_r,117897._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
       280.50_pm_r,-27._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       256.60_pm_r,3901._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       232.10_pm_r,7475._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.60_pm_r,10771._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.20_pm_r,13966._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.70_pm_r,17167._pm_r,23._pm_r,207._pm_r,3.66_pm_r,124._pm_r,6._pm_r,81._pm_r,.59_pm_r,256._pm_r, &
       219.90_pm_r,20375._pm_r,24._pm_r,192._pm_r,4.81_pm_r,120._pm_r,5._pm_r,82._pm_r,.62_pm_r,257._pm_r, &
       223.90_pm_r,23628._pm_r,27._pm_r,177._pm_r,5.20_pm_r,114._pm_r,4._pm_r,83._pm_r,.40_pm_r,259._pm_r, &
       225.10_pm_r,26917._pm_r,30._pm_r,164._pm_r,4.65_pm_r,102._pm_r,4._pm_r,83._pm_r,.08_pm_r,47._pm_r, &
       227.20_pm_r,30225._pm_r,32._pm_r,154._pm_r,3.59_pm_r,77._pm_r,4._pm_r,81._pm_r,.68_pm_r,72._pm_r, &
       236.20_pm_r,33610._pm_r,32._pm_r,145._pm_r,3.18_pm_r,35._pm_r,6._pm_r,79._pm_r,1.17_pm_r,74._pm_r, &
       247.10_pm_r,37152._pm_r,29._pm_r,138._pm_r,3.86_pm_r,2._pm_r,8._pm_r,78._pm_r,1.38_pm_r,76._pm_r, &
       258.10_pm_r,40849._pm_r,25._pm_r,129._pm_r,4.54_pm_r,345._pm_r,10._pm_r,78._pm_r,1.29_pm_r,77._pm_r, &
       266.10_pm_r,44698._pm_r,19._pm_r,118._pm_r,4.80_pm_r,339._pm_r,11._pm_r,78._pm_r,.59_pm_r,71._pm_r, &
       266.20_pm_r,48606._pm_r,14._pm_r,101._pm_r,4.85_pm_r,332._pm_r,11._pm_r,77._pm_r,.09_pm_r,301._pm_r, &
       259.70_pm_r,52460._pm_r,11._pm_r,74._pm_r,4.29_pm_r,321._pm_r,11._pm_r,77._pm_r,.43_pm_r,254._pm_r, &
       250.70_pm_r,56201._pm_r,9._pm_r,43._pm_r,3.20_pm_r,301._pm_r,10._pm_r,78._pm_r,.45_pm_r,238._pm_r, &
       242.80_pm_r,59810._pm_r,8._pm_r,18._pm_r,2.26_pm_r,268._pm_r,10._pm_r,79._pm_r,.37_pm_r,211._pm_r, &
       237.40_pm_r,63327._pm_r,7._pm_r,358._pm_r,1.94_pm_r,236._pm_r,9._pm_r,82._pm_r,.31_pm_r,187._pm_r, &
       232.00_pm_r,66764._pm_r,6._pm_r,337._pm_r,1.67_pm_r,218._pm_r,9._pm_r,85._pm_r,.27_pm_r,172._pm_r, &
       226.10_pm_r,70121._pm_r,5._pm_r,315._pm_r,1.26_pm_r,206._pm_r,9._pm_r,87._pm_r,.21_pm_r,165._pm_r, &
       219.50_pm_r,73385._pm_r,4._pm_r,296._pm_r,.80_pm_r,197._pm_r,10._pm_r,88._pm_r,.15_pm_r,161._pm_r, &
       210.70_pm_r,76541._pm_r,4._pm_r,284._pm_r,.42_pm_r,181._pm_r,10._pm_r,89._pm_r,.10_pm_r,160._pm_r, &
       201.50_pm_r,79554._pm_r,4._pm_r,279._pm_r,.19_pm_r,139._pm_r,10._pm_r,90._pm_r,.07_pm_r,153._pm_r, &
       191.40_pm_r,82439._pm_r,4._pm_r,279._pm_r,.19_pm_r,82._pm_r,10._pm_r,90._pm_r,.03_pm_r,149._pm_r, &
       180.80_pm_r,85146._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       173.60_pm_r,87720._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       170.20_pm_r,90232._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       171.60_pm_r,92742._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       176.60_pm_r,95308._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.00_pm_r,98000._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.60_pm_r,100902._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.80_pm_r,104133._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       259.10_pm_r,107854._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       310.60_pm_r,112306._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       392.20_pm_r,117910._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
       286.40_pm_r,-29._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       263.50_pm_r,3997._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       238.00_pm_r,7668._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.20_pm_r,11029._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.00_pm_r,14237._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.90_pm_r,17412._pm_r,11._pm_r,199._pm_r,1.78_pm_r,123._pm_r,4._pm_r,87._pm_r,.64_pm_r,260._pm_r, &
       218.90_pm_r,20601._pm_r,12._pm_r,185._pm_r,2.28_pm_r,117._pm_r,3._pm_r,89._pm_r,.78_pm_r,263._pm_r, &
       222.20_pm_r,23834._pm_r,14._pm_r,171._pm_r,2.43_pm_r,107._pm_r,2._pm_r,92._pm_r,.75_pm_r,267._pm_r, &
       224.00_pm_r,27101._pm_r,15._pm_r,159._pm_r,2.25_pm_r,89._pm_r,1._pm_r,94._pm_r,.54_pm_r,276._pm_r, &
       226.70_pm_r,30398._pm_r,16._pm_r,147._pm_r,2.05_pm_r,59._pm_r,1._pm_r,76._pm_r,.25_pm_r,315._pm_r, &
       235.80_pm_r,33776._pm_r,15._pm_r,136._pm_r,2.22_pm_r,24._pm_r,1._pm_r,48._pm_r,.33_pm_r,41._pm_r, &
       247.20_pm_r,37316._pm_r,13._pm_r,124._pm_r,2.63_pm_r,0._pm_r,1._pm_r,51._pm_r,.56_pm_r,62._pm_r, &
       258.20_pm_r,41015._pm_r,11._pm_r,108._pm_r,2.93_pm_r,345._pm_r,2._pm_r,57._pm_r,.64_pm_r,71._pm_r, &
       266.30_pm_r,44864._pm_r,9._pm_r,86._pm_r,2.86_pm_r,334._pm_r,3._pm_r,61._pm_r,.36_pm_r,74._pm_r, &
       267.40_pm_r,48780._pm_r,8._pm_r,60._pm_r,2.62_pm_r,318._pm_r,3._pm_r,63._pm_r,.14_pm_r,115._pm_r, &
       260.10_pm_r,52647._pm_r,7._pm_r,34._pm_r,2.16_pm_r,293._pm_r,3._pm_r,67._pm_r,.19_pm_r,186._pm_r, &
       249.10_pm_r,56380._pm_r,7._pm_r,12._pm_r,1.80_pm_r,254._pm_r,3._pm_r,72._pm_r,.24_pm_r,208._pm_r, &
       241.00_pm_r,59963._pm_r,5._pm_r,351._pm_r,1.67_pm_r,224._pm_r,3._pm_r,75._pm_r,.19_pm_r,227._pm_r, &
       235.50_pm_r,63453._pm_r,4._pm_r,326._pm_r,1.47_pm_r,205._pm_r,3._pm_r,77._pm_r,.12_pm_r,256._pm_r, &
       229.20_pm_r,66855._pm_r,3._pm_r,297._pm_r,1.03_pm_r,196._pm_r,2._pm_r,76._pm_r,.04_pm_r,333._pm_r, &
       223.00_pm_r,70168._pm_r,3._pm_r,276._pm_r,.46_pm_r,191._pm_r,3._pm_r,74._pm_r,.12_pm_r,42._pm_r, &
       216.90_pm_r,73387._pm_r,3._pm_r,271._pm_r,.08_pm_r,352._pm_r,3._pm_r,72._pm_r,.21_pm_r,57._pm_r, &
       210.30_pm_r,76518._pm_r,3._pm_r,279._pm_r,.50_pm_r,2._pm_r,3._pm_r,71._pm_r,.27_pm_r,62._pm_r, &
       203.70_pm_r,79546._pm_r,3._pm_r,296._pm_r,.77_pm_r,3._pm_r,4._pm_r,70._pm_r,.31_pm_r,66._pm_r, &
       195.90_pm_r,82482._pm_r,4._pm_r,312._pm_r,.87_pm_r,3._pm_r,4._pm_r,69._pm_r,.30_pm_r,69._pm_r, &
       186.90_pm_r,85275._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       180.00_pm_r,87945._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       176.10_pm_r,90549._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       176.40_pm_r,93141._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.70_pm_r,95769._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.80_pm_r,98493._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.00_pm_r,101384._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.80_pm_r,104551._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       251.50_pm_r,108165._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       308.10_pm_r,112534._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       390.80_pm_r,118119._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
       292.10_pm_r,-26._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       269.80_pm_r,4091._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       244.30_pm_r,7857._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.90_pm_r,11280._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.00_pm_r,14459._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.20_pm_r,17530._pm_r,5._pm_r,175._pm_r,.45_pm_r,124._pm_r,2._pm_r,77._pm_r,.37_pm_r,251._pm_r, &
       215.10_pm_r,20642._pm_r,5._pm_r,168._pm_r,.50_pm_r,115._pm_r,1._pm_r,80._pm_r,.46_pm_r,250._pm_r, &
       220.10_pm_r,23831._pm_r,6._pm_r,162._pm_r,.44_pm_r,94._pm_r,1._pm_r,90._pm_r,.47_pm_r,249._pm_r, &
       224.50_pm_r,27086._pm_r,6._pm_r,156._pm_r,.41_pm_r,51._pm_r,0._pm_r,167._pm_r,.40_pm_r,249._pm_r, &
       230.00_pm_r,30413._pm_r,5._pm_r,151._pm_r,.59_pm_r,8._pm_r,1._pm_r,224._pm_r,.24_pm_r,248._pm_r, &
       239.00_pm_r,33842._pm_r,4._pm_r,145._pm_r,.86_pm_r,346._pm_r,1._pm_r,230._pm_r,.06_pm_r,235._pm_r, &
       249.20_pm_r,37419._pm_r,3._pm_r,138._pm_r,1.05_pm_r,333._pm_r,1._pm_r,228._pm_r,.10_pm_r,76._pm_r, &
       259.70_pm_r,41142._pm_r,1._pm_r,127._pm_r,1.11_pm_r,322._pm_r,1._pm_r,218._pm_r,.20_pm_r,71._pm_r, &
       267.90_pm_r,45014._pm_r,0._pm_r,14._pm_r,1.03_pm_r,311._pm_r,0._pm_r,188._pm_r,.21_pm_r,82._pm_r, &
       268.50_pm_r,48951._pm_r,2._pm_r,313._pm_r,.80_pm_r,289._pm_r,1._pm_r,139._pm_r,.30_pm_r,96._pm_r, &
       260.20_pm_r,52826._pm_r,2._pm_r,293._pm_r,.72_pm_r,236._pm_r,1._pm_r,120._pm_r,.30_pm_r,103._pm_r, &
       248.60_pm_r,56555._pm_r,3._pm_r,266._pm_r,1.15_pm_r,194._pm_r,1._pm_r,114._pm_r,.15_pm_r,90._pm_r, &
       239.60_pm_r,60124._pm_r,4._pm_r,236._pm_r,1.30_pm_r,183._pm_r,1._pm_r,109._pm_r,.16_pm_r,343._pm_r, &
       233.00_pm_r,63586._pm_r,5._pm_r,218._pm_r,1.15_pm_r,177._pm_r,1._pm_r,93._pm_r,.32_pm_r,347._pm_r, &
       226.10_pm_r,66947._pm_r,6._pm_r,208._pm_r,.76_pm_r,174._pm_r,1._pm_r,63._pm_r,.45_pm_r,6._pm_r, &
       219.40_pm_r,70211._pm_r,7._pm_r,204._pm_r,.30_pm_r,170._pm_r,2._pm_r,45._pm_r,.57_pm_r,25._pm_r, &
       213.80_pm_r,73380._pm_r,7._pm_r,204._pm_r,.12_pm_r,358._pm_r,3._pm_r,40._pm_r,.73_pm_r,37._pm_r, &
       209.00_pm_r,76477._pm_r,6._pm_r,206._pm_r,.44_pm_r,352._pm_r,4._pm_r,40._pm_r,.85_pm_r,45._pm_r, &
       205.50_pm_r,79509._pm_r,6._pm_r,210._pm_r,.65_pm_r,351._pm_r,5._pm_r,42._pm_r,.92_pm_r,49._pm_r, &
       200.00_pm_r,82494._pm_r,5._pm_r,218._pm_r,.74_pm_r,351._pm_r,7._pm_r,44._pm_r,.91_pm_r,51._pm_r, &
       191.70_pm_r,85354._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.00_pm_r,88098._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.30_pm_r,90780._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       180.90_pm_r,93446._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.70_pm_r,96134._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.60_pm_r,98889._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.60_pm_r,101770._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.30_pm_r,104881._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.90_pm_r,108395._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       303.80_pm_r,112667._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       389.00_pm_r,118216._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
       296.50_pm_r,-34._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.30_pm_r,4151._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       250.10_pm_r,7996._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.60_pm_r,11476._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.90_pm_r,14625._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.50_pm_r,17590._pm_r,3._pm_r,151._pm_r,.12_pm_r,261._pm_r,0._pm_r,330._pm_r,.13_pm_r,115._pm_r, &
       211.90_pm_r,20632._pm_r,2._pm_r,156._pm_r,.21_pm_r,262._pm_r,0._pm_r,356._pm_r,.17_pm_r,118._pm_r, &
       218.20_pm_r,23783._pm_r,2._pm_r,165._pm_r,.30_pm_r,263._pm_r,0._pm_r,56._pm_r,.18_pm_r,125._pm_r, &
       224.70_pm_r,27023._pm_r,2._pm_r,178._pm_r,.40_pm_r,263._pm_r,0._pm_r,93._pm_r,.16_pm_r,133._pm_r, &
       232.60_pm_r,30371._pm_r,2._pm_r,194._pm_r,.50_pm_r,263._pm_r,1._pm_r,109._pm_r,.13_pm_r,148._pm_r, &
       242.40_pm_r,33845._pm_r,3._pm_r,209._pm_r,.57_pm_r,261._pm_r,1._pm_r,119._pm_r,.09_pm_r,174._pm_r, &
       252.40_pm_r,37472._pm_r,3._pm_r,220._pm_r,.59_pm_r,259._pm_r,1._pm_r,127._pm_r,.07_pm_r,225._pm_r, &
       262.00_pm_r,41237._pm_r,4._pm_r,227._pm_r,.59_pm_r,255._pm_r,1._pm_r,135._pm_r,.09_pm_r,270._pm_r, &
       268.80_pm_r,45131._pm_r,5._pm_r,232._pm_r,.56_pm_r,262._pm_r,1._pm_r,141._pm_r,.12_pm_r,302._pm_r, &
       268.90_pm_r,49076._pm_r,6._pm_r,237._pm_r,.49_pm_r,274._pm_r,0._pm_r,144._pm_r,.19_pm_r,328._pm_r, &
       260.10_pm_r,52954._pm_r,6._pm_r,240._pm_r,.26_pm_r,267._pm_r,0._pm_r,106._pm_r,.16_pm_r,339._pm_r, &
       248.00_pm_r,56679._pm_r,6._pm_r,239._pm_r,.30_pm_r,147._pm_r,0._pm_r,21._pm_r,.02_pm_r,63._pm_r, &
       237.80_pm_r,60230._pm_r,6._pm_r,232._pm_r,.67_pm_r,136._pm_r,0._pm_r,64._pm_r,.09_pm_r,131._pm_r, &
       230.10_pm_r,63657._pm_r,6._pm_r,222._pm_r,.82_pm_r,130._pm_r,0._pm_r,91._pm_r,.12_pm_r,90._pm_r, &
       222.70_pm_r,66970._pm_r,6._pm_r,211._pm_r,.76_pm_r,119._pm_r,1._pm_r,72._pm_r,.25_pm_r,35._pm_r, &
       215.60_pm_r,70181._pm_r,6._pm_r,201._pm_r,.64_pm_r,103._pm_r,1._pm_r,48._pm_r,.48_pm_r,20._pm_r, &
       209.80_pm_r,73293._pm_r,6._pm_r,193._pm_r,.53_pm_r,80._pm_r,2._pm_r,32._pm_r,.70_pm_r,13._pm_r, &
       207.00_pm_r,76342._pm_r,5._pm_r,186._pm_r,.54_pm_r,59._pm_r,3._pm_r,24._pm_r,.87_pm_r,11._pm_r, &
       206.40_pm_r,79367._pm_r,5._pm_r,179._pm_r,.56_pm_r,45._pm_r,4._pm_r,19._pm_r,.99_pm_r,9._pm_r, &
       202.70_pm_r,82385._pm_r,4._pm_r,171._pm_r,.58_pm_r,36._pm_r,6._pm_r,16._pm_r,1.02_pm_r,8._pm_r, &
       194.80_pm_r,85286._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.20_pm_r,88075._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.00_pm_r,90810._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.40_pm_r,93531._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.20_pm_r,96267._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.20_pm_r,99048._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.60_pm_r,101923._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.70_pm_r,104986._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       237.40_pm_r,108415._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       298.90_pm_r,112593._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       387.20_pm_r,118096._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
       299.40_pm_r,-60._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.20_pm_r,4162._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.70_pm_r,8041._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.40_pm_r,11548._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.60_pm_r,14680._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.20_pm_r,17590._pm_r,2._pm_r,155._pm_r,.16_pm_r,301._pm_r,1._pm_r,261._pm_r,.15_pm_r,82._pm_r, &
       210.50_pm_r,20597._pm_r,2._pm_r,161._pm_r,.22_pm_r,288._pm_r,0._pm_r,260._pm_r,.19_pm_r,81._pm_r, &
       217.10_pm_r,23729._pm_r,2._pm_r,171._pm_r,.25_pm_r,271._pm_r,0._pm_r,264._pm_r,.19_pm_r,78._pm_r, &
       224.30_pm_r,26959._pm_r,2._pm_r,185._pm_r,.32_pm_r,253._pm_r,0._pm_r,72._pm_r,.18_pm_r,76._pm_r, &
       232.20_pm_r,30302._pm_r,2._pm_r,198._pm_r,.40_pm_r,241._pm_r,0._pm_r,72._pm_r,.13_pm_r,67._pm_r, &
       244.00_pm_r,33783._pm_r,3._pm_r,207._pm_r,.46_pm_r,234._pm_r,1._pm_r,69._pm_r,.05_pm_r,49._pm_r, &
       254.70_pm_r,37441._pm_r,3._pm_r,213._pm_r,.49_pm_r,233._pm_r,1._pm_r,64._pm_r,.05_pm_r,302._pm_r, &
       263.40_pm_r,41233._pm_r,4._pm_r,217._pm_r,.50_pm_r,236._pm_r,1._pm_r,55._pm_r,.12_pm_r,272._pm_r, &
       268.80_pm_r,45137._pm_r,5._pm_r,221._pm_r,.57_pm_r,253._pm_r,0._pm_r,34._pm_r,.13_pm_r,285._pm_r, &
       268.70_pm_r,49080._pm_r,5._pm_r,227._pm_r,.66_pm_r,264._pm_r,0._pm_r,6._pm_r,.18_pm_r,341._pm_r, &
       261.50_pm_r,52966._pm_r,6._pm_r,233._pm_r,.47_pm_r,269._pm_r,1._pm_r,0._pm_r,.22_pm_r,357._pm_r, &
       250.20_pm_r,56719._pm_r,6._pm_r,235._pm_r,.15_pm_r,72._pm_r,1._pm_r,359._pm_r,.10_pm_r,332._pm_r, &
       238.40_pm_r,60293._pm_r,6._pm_r,231._pm_r,.77_pm_r,85._pm_r,1._pm_r,350._pm_r,.16_pm_r,239._pm_r, &
       227.60_pm_r,63707._pm_r,5._pm_r,220._pm_r,1.22_pm_r,87._pm_r,1._pm_r,333._pm_r,.21_pm_r,244._pm_r, &
       218.10_pm_r,66967._pm_r,4._pm_r,196._pm_r,1.41_pm_r,89._pm_r,1._pm_r,319._pm_r,.18_pm_r,291._pm_r, &
       210.90_pm_r,70107._pm_r,4._pm_r,161._pm_r,1.47_pm_r,91._pm_r,1._pm_r,320._pm_r,.31_pm_r,338._pm_r, &
       207.50_pm_r,73167._pm_r,5._pm_r,136._pm_r,1.47_pm_r,93._pm_r,2._pm_r,329._pm_r,.51_pm_r,355._pm_r, &
       207.20_pm_r,76200._pm_r,7._pm_r,124._pm_r,1.47_pm_r,95._pm_r,3._pm_r,338._pm_r,.69_pm_r,0._pm_r, &
       208.20_pm_r,79244._pm_r,9._pm_r,117._pm_r,1.45_pm_r,96._pm_r,4._pm_r,345._pm_r,.82_pm_r,3._pm_r, &
       205.10_pm_r,82301._pm_r,10._pm_r,113._pm_r,1.38_pm_r,97._pm_r,5._pm_r,350._pm_r,.87_pm_r,4._pm_r, &
       196.50_pm_r,85230._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.70_pm_r,88039._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,90801._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.60_pm_r,93555._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.80_pm_r,96320._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.50_pm_r,99117._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.10_pm_r,101985._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.40_pm_r,105011._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       232.40_pm_r,108377._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       294.40_pm_r,112479._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       385.20_pm_r,117935._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
       300.50_pm_r,-109._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.50_pm_r,4124._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.40_pm_r,8012._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.90_pm_r,11528._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.20_pm_r,14661._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.60_pm_r,17563._pm_r,2._pm_r,159._pm_r,.17_pm_r,297._pm_r,1._pm_r,272._pm_r,.13_pm_r,74._pm_r, &
       210.00_pm_r,20565._pm_r,2._pm_r,166._pm_r,.23_pm_r,288._pm_r,1._pm_r,280._pm_r,.17_pm_r,73._pm_r, &
       216.40_pm_r,23688._pm_r,1._pm_r,179._pm_r,.27_pm_r,274._pm_r,0._pm_r,305._pm_r,.16_pm_r,72._pm_r, &
       224.00_pm_r,26907._pm_r,2._pm_r,195._pm_r,.32_pm_r,261._pm_r,0._pm_r,2._pm_r,.13_pm_r,67._pm_r, &
       232.80_pm_r,30251._pm_r,2._pm_r,209._pm_r,.36_pm_r,251._pm_r,0._pm_r,33._pm_r,.08_pm_r,58._pm_r, &
       244.30_pm_r,33738._pm_r,2._pm_r,218._pm_r,.37_pm_r,244._pm_r,0._pm_r,38._pm_r,.02_pm_r,333._pm_r, &
       254.90_pm_r,37396._pm_r,3._pm_r,223._pm_r,.36_pm_r,242._pm_r,0._pm_r,31._pm_r,.09_pm_r,277._pm_r, &
       263.80_pm_r,41190._pm_r,3._pm_r,226._pm_r,.31_pm_r,248._pm_r,0._pm_r,8._pm_r,.15_pm_r,270._pm_r, &
       269.20_pm_r,45099._pm_r,4._pm_r,231._pm_r,.35_pm_r,270._pm_r,0._pm_r,330._pm_r,.17_pm_r,268._pm_r, &
       269.10_pm_r,49046._pm_r,4._pm_r,237._pm_r,.41_pm_r,281._pm_r,1._pm_r,309._pm_r,.07_pm_r,278._pm_r, &
       262.80_pm_r,52943._pm_r,5._pm_r,242._pm_r,.29_pm_r,283._pm_r,1._pm_r,307._pm_r,.08_pm_r,62._pm_r, &
       251.50_pm_r,56711._pm_r,5._pm_r,244._pm_r,.08_pm_r,113._pm_r,1._pm_r,316._pm_r,.22_pm_r,82._pm_r, &
       238.40_pm_r,60292._pm_r,5._pm_r,243._pm_r,.52_pm_r,99._pm_r,1._pm_r,332._pm_r,.14_pm_r,92._pm_r, &
       225.90_pm_r,63698._pm_r,4._pm_r,236._pm_r,.89_pm_r,96._pm_r,0._pm_r,359._pm_r,.03_pm_r,198._pm_r, &
       215.70_pm_r,66936._pm_r,3._pm_r,216._pm_r,1.16_pm_r,94._pm_r,0._pm_r,26._pm_r,.20_pm_r,266._pm_r, &
       209.50_pm_r,70058._pm_r,2._pm_r,166._pm_r,1.34_pm_r,92._pm_r,0._pm_r,340._pm_r,.36_pm_r,270._pm_r, &
       208.00_pm_r,73119._pm_r,3._pm_r,126._pm_r,1.49_pm_r,90._pm_r,0._pm_r,279._pm_r,.51_pm_r,272._pm_r, &
       209.20_pm_r,76175._pm_r,5._pm_r,111._pm_r,1.62_pm_r,89._pm_r,1._pm_r,278._pm_r,.63_pm_r,274._pm_r, &
       211.90_pm_r,79256._pm_r,8._pm_r,105._pm_r,1.70_pm_r,88._pm_r,2._pm_r,279._pm_r,.72_pm_r,274._pm_r, &
       208.80_pm_r,82367._pm_r,10._pm_r,101._pm_r,1.69_pm_r,88._pm_r,2._pm_r,280._pm_r,.73_pm_r,275._pm_r, &
       198.10_pm_r,85326._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.10_pm_r,88142._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.50_pm_r,90910._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,93674._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,96448._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.40_pm_r,99248._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.00_pm_r,102108._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.30_pm_r,105111._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.40_pm_r,108438._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       291.30_pm_r,112490._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       383.10_pm_r,117908._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
       301.30_pm_r,-101._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.90_pm_r,4140._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.30_pm_r,8029._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.70_pm_r,11542._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.40_pm_r,14675._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.90_pm_r,17581._pm_r,3._pm_r,165._pm_r,.29_pm_r,336._pm_r,1._pm_r,282._pm_r,.18_pm_r,62._pm_r, &
       210.20_pm_r,20584._pm_r,2._pm_r,168._pm_r,.37_pm_r,327._pm_r,1._pm_r,307._pm_r,.22_pm_r,60._pm_r, &
       216.70_pm_r,23710._pm_r,2._pm_r,176._pm_r,.39_pm_r,315._pm_r,0._pm_r,348._pm_r,.21_pm_r,60._pm_r, &
       224.10_pm_r,26934._pm_r,2._pm_r,192._pm_r,.40_pm_r,297._pm_r,1._pm_r,14._pm_r,.17_pm_r,55._pm_r, &
       232.90_pm_r,30281._pm_r,2._pm_r,214._pm_r,.43_pm_r,278._pm_r,1._pm_r,23._pm_r,.08_pm_r,47._pm_r, &
       243.00_pm_r,33763._pm_r,2._pm_r,230._pm_r,.46_pm_r,265._pm_r,1._pm_r,24._pm_r,.03_pm_r,315._pm_r, &
       253.30_pm_r,37401._pm_r,3._pm_r,238._pm_r,.47_pm_r,261._pm_r,1._pm_r,18._pm_r,.09_pm_r,265._pm_r, &
       262.70_pm_r,41179._pm_r,3._pm_r,243._pm_r,.45_pm_r,264._pm_r,1._pm_r,7._pm_r,.13_pm_r,259._pm_r, &
       268.50_pm_r,45077._pm_r,4._pm_r,247._pm_r,.47_pm_r,275._pm_r,1._pm_r,352._pm_r,.13_pm_r,263._pm_r, &
       268.60_pm_r,49017._pm_r,5._pm_r,252._pm_r,.56_pm_r,277._pm_r,1._pm_r,337._pm_r,.15_pm_r,276._pm_r, &
       262.20_pm_r,52908._pm_r,5._pm_r,256._pm_r,.52_pm_r,280._pm_r,1._pm_r,326._pm_r,.11_pm_r,293._pm_r, &
       250.70_pm_r,56672._pm_r,6._pm_r,259._pm_r,.23_pm_r,304._pm_r,1._pm_r,326._pm_r,.05_pm_r,34._pm_r, &
       238.30_pm_r,60248._pm_r,6._pm_r,261._pm_r,.34_pm_r,49._pm_r,1._pm_r,333._pm_r,.15_pm_r,98._pm_r, &
       227.00_pm_r,63657._pm_r,5._pm_r,264._pm_r,.67_pm_r,70._pm_r,1._pm_r,347._pm_r,.21_pm_r,124._pm_r, &
       217.50_pm_r,66907._pm_r,4._pm_r,266._pm_r,.88_pm_r,81._pm_r,1._pm_r,5._pm_r,.24_pm_r,151._pm_r, &
       211.50_pm_r,70045._pm_r,3._pm_r,266._pm_r,.99_pm_r,91._pm_r,0._pm_r,48._pm_r,.30_pm_r,176._pm_r, &
       209.70_pm_r,73125._pm_r,1._pm_r,254._pm_r,1.06_pm_r,97._pm_r,0._pm_r,163._pm_r,.37_pm_r,192._pm_r, &
       209.80_pm_r,76195._pm_r,1._pm_r,134._pm_r,1.11_pm_r,102._pm_r,1._pm_r,184._pm_r,.44_pm_r,201._pm_r, &
       211.20_pm_r,79278._pm_r,2._pm_r,113._pm_r,1.14_pm_r,105._pm_r,2._pm_r,192._pm_r,.49_pm_r,206._pm_r, &
       207.80_pm_r,82384._pm_r,4._pm_r,110._pm_r,1.12_pm_r,107._pm_r,2._pm_r,197._pm_r,.50_pm_r,209._pm_r, &
       197.90_pm_r,85339._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.00_pm_r,88155._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.10_pm_r,90919._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.80_pm_r,93675._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.90_pm_r,96442._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.90_pm_r,99235._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.50_pm_r,102087._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.70_pm_r,105081._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.60_pm_r,108397._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       290.20_pm_r,112432._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       381.40_pm_r,117828._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
       300.50_pm_r,-96._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.50_pm_r,4134._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.10_pm_r,8010._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.40_pm_r,11511._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.20_pm_r,14653._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.00_pm_r,17591._pm_r,4._pm_r,150._pm_r,.46_pm_r,302._pm_r,1._pm_r,222._pm_r,.40_pm_r,31._pm_r, &
       211.40_pm_r,20618._pm_r,3._pm_r,156._pm_r,.66_pm_r,303._pm_r,0._pm_r,241._pm_r,.50_pm_r,30._pm_r, &
       217.70_pm_r,23761._pm_r,2._pm_r,170._pm_r,.79_pm_r,304._pm_r,0._pm_r,358._pm_r,.49_pm_r,27._pm_r, &
       224.40_pm_r,26995._pm_r,2._pm_r,201._pm_r,.89_pm_r,306._pm_r,1._pm_r,14._pm_r,.41_pm_r,22._pm_r, &
       231.80_pm_r,30338._pm_r,2._pm_r,245._pm_r,.93_pm_r,308._pm_r,2._pm_r,15._pm_r,.27_pm_r,12._pm_r, &
       240.30_pm_r,33792._pm_r,3._pm_r,271._pm_r,.90_pm_r,310._pm_r,2._pm_r,13._pm_r,.12_pm_r,336._pm_r, &
       250.10_pm_r,37385._pm_r,4._pm_r,284._pm_r,.82_pm_r,314._pm_r,2._pm_r,9._pm_r,.12_pm_r,261._pm_r, &
       260.00_pm_r,41119._pm_r,5._pm_r,291._pm_r,.74_pm_r,318._pm_r,2._pm_r,3._pm_r,.19_pm_r,234._pm_r, &
       266.70_pm_r,44984._pm_r,6._pm_r,295._pm_r,.64_pm_r,315._pm_r,2._pm_r,358._pm_r,.16_pm_r,197._pm_r, &
       267.20_pm_r,48901._pm_r,7._pm_r,298._pm_r,.65_pm_r,307._pm_r,1._pm_r,358._pm_r,.14_pm_r,155._pm_r, &
       260.80_pm_r,52772._pm_r,8._pm_r,299._pm_r,.60_pm_r,311._pm_r,1._pm_r,2._pm_r,.04_pm_r,90._pm_r, &
       249.80_pm_r,56517._pm_r,8._pm_r,301._pm_r,.42_pm_r,339._pm_r,1._pm_r,360._pm_r,.23_pm_r,326._pm_r, &
       239.10_pm_r,60093._pm_r,8._pm_r,304._pm_r,.34_pm_r,34._pm_r,2._pm_r,350._pm_r,.35_pm_r,316._pm_r, &
       230.10_pm_r,63531._pm_r,8._pm_r,307._pm_r,.41_pm_r,80._pm_r,2._pm_r,342._pm_r,.27_pm_r,304._pm_r, &
       221.20_pm_r,66832._pm_r,8._pm_r,309._pm_r,.46_pm_r,122._pm_r,2._pm_r,337._pm_r,.10_pm_r,233._pm_r, &
       214.50_pm_r,70020._pm_r,7._pm_r,308._pm_r,.62_pm_r,153._pm_r,2._pm_r,335._pm_r,.33_pm_r,160._pm_r, &
       211.30_pm_r,73134._pm_r,6._pm_r,303._pm_r,.81_pm_r,169._pm_r,1._pm_r,336._pm_r,.61_pm_r,150._pm_r, &
       209.10_pm_r,76212._pm_r,5._pm_r,292._pm_r,1.00_pm_r,178._pm_r,0._pm_r,2._pm_r,.84_pm_r,147._pm_r, &
       208.60_pm_r,79267._pm_r,5._pm_r,275._pm_r,1.10_pm_r,183._pm_r,1._pm_r,138._pm_r,.98_pm_r,147._pm_r, &
       205.10_pm_r,82321._pm_r,5._pm_r,257._pm_r,1.14_pm_r,186._pm_r,3._pm_r,142._pm_r,1.03_pm_r,146._pm_r, &
       197.10_pm_r,85260._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.90_pm_r,88078._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.20_pm_r,90832._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.60_pm_r,93571._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.70_pm_r,96320._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.40_pm_r,99098._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.80_pm_r,101947._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.70_pm_r,104951._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.00_pm_r,108284._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       290.80_pm_r,112335._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       380.00_pm_r,117722._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
       297.30_pm_r,-75._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       273.40_pm_r,4106._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       248.20_pm_r,7927._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       223.60_pm_r,11384._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.30_pm_r,14541._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.40_pm_r,17549._pm_r,6._pm_r,160._pm_r,.74_pm_r,305._pm_r,1._pm_r,275._pm_r,.34_pm_r,43._pm_r, &
       213.20_pm_r,20621._pm_r,5._pm_r,169._pm_r,.99_pm_r,312._pm_r,1._pm_r,312._pm_r,.44_pm_r,42._pm_r, &
       218.60_pm_r,23784._pm_r,3._pm_r,183._pm_r,1.12_pm_r,323._pm_r,1._pm_r,352._pm_r,.42_pm_r,40._pm_r, &
       223.30_pm_r,27018._pm_r,2._pm_r,208._pm_r,1.21_pm_r,337._pm_r,1._pm_r,8._pm_r,.34_pm_r,37._pm_r, &
       228.90_pm_r,30330._pm_r,2._pm_r,263._pm_r,1.29_pm_r,352._pm_r,2._pm_r,14._pm_r,.19_pm_r,29._pm_r, &
       236.70_pm_r,33736._pm_r,2._pm_r,319._pm_r,1.36_pm_r,5._pm_r,2._pm_r,14._pm_r,.05_pm_r,331._pm_r, &
       245.80_pm_r,37271._pm_r,4._pm_r,342._pm_r,1.37_pm_r,14._pm_r,2._pm_r,11._pm_r,.12_pm_r,245._pm_r, &
       255.60_pm_r,40938._pm_r,6._pm_r,353._pm_r,1.29_pm_r,17._pm_r,2._pm_r,6._pm_r,.17_pm_r,234._pm_r, &
       264.10_pm_r,44750._pm_r,7._pm_r,358._pm_r,.98_pm_r,12._pm_r,2._pm_r,1._pm_r,.13_pm_r,227._pm_r, &
       266.90_pm_r,48645._pm_r,8._pm_r,359._pm_r,.70_pm_r,357._pm_r,2._pm_r,355._pm_r,.16_pm_r,235._pm_r, &
       260.40_pm_r,52513._pm_r,9._pm_r,359._pm_r,.56_pm_r,349._pm_r,2._pm_r,345._pm_r,.22_pm_r,258._pm_r, &
       248.60_pm_r,56246._pm_r,10._pm_r,358._pm_r,.46_pm_r,4._pm_r,2._pm_r,331._pm_r,.32_pm_r,276._pm_r, &
       238.80_pm_r,59809._pm_r,11._pm_r,359._pm_r,.34_pm_r,13._pm_r,2._pm_r,322._pm_r,.24_pm_r,297._pm_r, &
       231.90_pm_r,63258._pm_r,11._pm_r,359._pm_r,.04_pm_r,324._pm_r,2._pm_r,321._pm_r,.10_pm_r,331._pm_r, &
       224.00_pm_r,66596._pm_r,11._pm_r,358._pm_r,.45_pm_r,219._pm_r,2._pm_r,324._pm_r,.14_pm_r,86._pm_r, &
       217.00_pm_r,69823._pm_r,10._pm_r,354._pm_r,.96_pm_r,218._pm_r,2._pm_r,330._pm_r,.33_pm_r,110._pm_r, &
       213.40_pm_r,72972._pm_r,9._pm_r,346._pm_r,1.38_pm_r,218._pm_r,2._pm_r,344._pm_r,.50_pm_r,118._pm_r, &
       210.90_pm_r,76079._pm_r,8._pm_r,333._pm_r,1.72_pm_r,218._pm_r,1._pm_r,17._pm_r,.63_pm_r,121._pm_r, &
       208.80_pm_r,79150._pm_r,7._pm_r,312._pm_r,1.91_pm_r,218._pm_r,1._pm_r,66._pm_r,.70_pm_r,123._pm_r, &
       204.70_pm_r,82192._pm_r,7._pm_r,289._pm_r,1.94_pm_r,219._pm_r,2._pm_r,92._pm_r,.71_pm_r,123._pm_r, &
       197.20_pm_r,85133._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.80_pm_r,87956._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       185.20_pm_r,90700._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.00_pm_r,93417._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.50_pm_r,96142._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.20_pm_r,98907._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.20_pm_r,101761._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.40_pm_r,104794._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       233.40_pm_r,108173._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       292.50_pm_r,112267._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       378.60_pm_r,117651._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
       291.10_pm_r,-32._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       267.70_pm_r,4059._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.00_pm_r,7790._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.80_pm_r,11177._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.60_pm_r,14350._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.10_pm_r,17452._pm_r,5._pm_r,206._pm_r,.16_pm_r,247._pm_r,2._pm_r,3._pm_r,.24_pm_r,316._pm_r, &
       215.20_pm_r,20580._pm_r,5._pm_r,207._pm_r,.13_pm_r,99._pm_r,2._pm_r,356._pm_r,.30_pm_r,311._pm_r, &
       218.50_pm_r,23757._pm_r,5._pm_r,201._pm_r,.71_pm_r,81._pm_r,3._pm_r,348._pm_r,.32_pm_r,305._pm_r, &
       221.40_pm_r,26977._pm_r,4._pm_r,182._pm_r,1.50_pm_r,77._pm_r,3._pm_r,342._pm_r,.28_pm_r,291._pm_r, &
       225.70_pm_r,30249._pm_r,4._pm_r,144._pm_r,2.34_pm_r,74._pm_r,3._pm_r,336._pm_r,.21_pm_r,266._pm_r, &
       232.80_pm_r,33602._pm_r,7._pm_r,111._pm_r,2.94_pm_r,70._pm_r,3._pm_r,331._pm_r,.18_pm_r,225._pm_r, &
       241.80_pm_r,37079._pm_r,10._pm_r,94._pm_r,3.12_pm_r,65._pm_r,3._pm_r,327._pm_r,.20_pm_r,193._pm_r, &
       251.70_pm_r,40689._pm_r,14._pm_r,84._pm_r,2.87_pm_r,57._pm_r,3._pm_r,323._pm_r,.20_pm_r,174._pm_r, &
       260.80_pm_r,44449._pm_r,18._pm_r,78._pm_r,2.24_pm_r,50._pm_r,3._pm_r,322._pm_r,.08_pm_r,155._pm_r, &
       264.60_pm_r,48304._pm_r,20._pm_r,74._pm_r,1.61_pm_r,42._pm_r,3._pm_r,322._pm_r,.05_pm_r,0._pm_r, &
       258.90_pm_r,52145._pm_r,22._pm_r,71._pm_r,1.16_pm_r,21._pm_r,3._pm_r,323._pm_r,.13_pm_r,351._pm_r, &
       248.50_pm_r,55865._pm_r,23._pm_r,67._pm_r,1.15_pm_r,351._pm_r,3._pm_r,325._pm_r,.14_pm_r,352._pm_r, &
       239.00_pm_r,59431._pm_r,23._pm_r,62._pm_r,1.51_pm_r,327._pm_r,3._pm_r,328._pm_r,.12_pm_r,15._pm_r, &
       230.90_pm_r,62872._pm_r,22._pm_r,56._pm_r,1.64_pm_r,311._pm_r,3._pm_r,330._pm_r,.12_pm_r,31._pm_r, &
       223.90_pm_r,66200._pm_r,21._pm_r,50._pm_r,1.56_pm_r,294._pm_r,3._pm_r,332._pm_r,.12_pm_r,7._pm_r, &
       218.70_pm_r,69441._pm_r,20._pm_r,45._pm_r,1.48_pm_r,275._pm_r,3._pm_r,334._pm_r,.14_pm_r,343._pm_r, &
       215.50_pm_r,72617._pm_r,19._pm_r,41._pm_r,1.52_pm_r,256._pm_r,4._pm_r,334._pm_r,.17_pm_r,327._pm_r, &
       213.10_pm_r,75756._pm_r,17._pm_r,37._pm_r,1.64_pm_r,242._pm_r,4._pm_r,333._pm_r,.21_pm_r,317._pm_r, &
       210.90_pm_r,78860._pm_r,14._pm_r,34._pm_r,1.73_pm_r,233._pm_r,4._pm_r,332._pm_r,.23_pm_r,314._pm_r, &
       206.60_pm_r,81933._pm_r,12._pm_r,30._pm_r,1.73_pm_r,228._pm_r,5._pm_r,330._pm_r,.24_pm_r,312._pm_r, &
       198.30_pm_r,84894._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.90_pm_r,87724._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.00_pm_r,90458._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.60_pm_r,93153._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.50_pm_r,95856._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.40_pm_r,98612._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.40_pm_r,101481._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.50_pm_r,104560._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       238.40_pm_r,108008._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       294.40_pm_r,112159._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       377.30_pm_r,117535._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
       283.70_pm_r,-58._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       261.30_pm_r,3928._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       236.60_pm_r,7569._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.20_pm_r,10910._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       217.10_pm_r,14108._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.90_pm_r,17285._pm_r,6._pm_r,285._pm_r,1.62_pm_r,200._pm_r,5._pm_r,29._pm_r,.88_pm_r,309._pm_r, &
       217.30_pm_r,20464._pm_r,7._pm_r,261._pm_r,2.25_pm_r,181._pm_r,5._pm_r,12._pm_r,1.16_pm_r,304._pm_r, &
       217.80_pm_r,23649._pm_r,8._pm_r,230._pm_r,3.07_pm_r,156._pm_r,6._pm_r,355._pm_r,1.27_pm_r,298._pm_r, &
       219.20_pm_r,26848._pm_r,10._pm_r,197._pm_r,4.31_pm_r,134._pm_r,7._pm_r,342._pm_r,1.17_pm_r,286._pm_r, &
       222.40_pm_r,30080._pm_r,14._pm_r,166._pm_r,5.79_pm_r,117._pm_r,8._pm_r,332._pm_r,.92_pm_r,265._pm_r, &
       228.20_pm_r,33375._pm_r,21._pm_r,144._pm_r,6.94_pm_r,105._pm_r,8._pm_r,323._pm_r,.75_pm_r,228._pm_r, &
       237.00_pm_r,36782._pm_r,29._pm_r,130._pm_r,7.11_pm_r,95._pm_r,8._pm_r,316._pm_r,.77_pm_r,191._pm_r, &
       246.90_pm_r,40321._pm_r,37._pm_r,120._pm_r,6.35_pm_r,84._pm_r,7._pm_r,309._pm_r,.79_pm_r,166._pm_r, &
       255.80_pm_r,44009._pm_r,44._pm_r,113._pm_r,4.89_pm_r,72._pm_r,6._pm_r,305._pm_r,.63_pm_r,144._pm_r, &
       259.90_pm_r,47791._pm_r,48._pm_r,107._pm_r,3.57_pm_r,58._pm_r,5._pm_r,303._pm_r,.46_pm_r,129._pm_r, &
       256.30_pm_r,51578._pm_r,50._pm_r,103._pm_r,2.56_pm_r,32._pm_r,5._pm_r,303._pm_r,.33_pm_r,115._pm_r, &
       248.10_pm_r,55276._pm_r,51._pm_r,99._pm_r,2.43_pm_r,352._pm_r,4._pm_r,305._pm_r,.30_pm_r,100._pm_r, &
       239.80_pm_r,58847._pm_r,48._pm_r,95._pm_r,3.18_pm_r,325._pm_r,4._pm_r,309._pm_r,.37_pm_r,73._pm_r, &
       231.50_pm_r,62300._pm_r,45._pm_r,91._pm_r,3.54_pm_r,312._pm_r,4._pm_r,318._pm_r,.39_pm_r,54._pm_r, &
       225.40_pm_r,65641._pm_r,41._pm_r,87._pm_r,3.30_pm_r,302._pm_r,4._pm_r,326._pm_r,.33_pm_r,31._pm_r, &
       221.40_pm_r,68911._pm_r,37._pm_r,83._pm_r,2.78_pm_r,292._pm_r,4._pm_r,331._pm_r,.31_pm_r,357._pm_r, &
       218.50_pm_r,72131._pm_r,34._pm_r,81._pm_r,2.25_pm_r,280._pm_r,4._pm_r,332._pm_r,.34_pm_r,327._pm_r, &
       216.10_pm_r,75313._pm_r,31._pm_r,79._pm_r,1.88_pm_r,265._pm_r,5._pm_r,331._pm_r,.43_pm_r,310._pm_r, &
       213.80_pm_r,78461._pm_r,28._pm_r,79._pm_r,1.64_pm_r,251._pm_r,6._pm_r,328._pm_r,.47_pm_r,301._pm_r, &
       208.80_pm_r,81574._pm_r,26._pm_r,81._pm_r,1.48_pm_r,239._pm_r,6._pm_r,325._pm_r,.50_pm_r,298._pm_r, &
       199.40_pm_r,84556._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.90_pm_r,87391._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.10_pm_r,90115._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.30_pm_r,92792._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.90_pm_r,95477._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.00_pm_r,98231._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       198.30_pm_r,101125._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.40_pm_r,104260._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       244.00_pm_r,107789._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       295.30_pm_r,111995._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       376.10_pm_r,117352._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
       277.50_pm_r,-92._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       255.80_pm_r,3805._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       232.00_pm_r,7370._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.70_pm_r,10678._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.00_pm_r,13898._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.50_pm_r,17118._pm_r,8._pm_r,309._pm_r,2.57_pm_r,224._pm_r,7._pm_r,21._pm_r,.86_pm_r,318._pm_r, &
       218.30_pm_r,20324._pm_r,9._pm_r,278._pm_r,3.45_pm_r,206._pm_r,8._pm_r,11._pm_r,1.19_pm_r,312._pm_r, &
       216.90_pm_r,23510._pm_r,11._pm_r,245._pm_r,4.54_pm_r,180._pm_r,9._pm_r,360._pm_r,1.43_pm_r,302._pm_r, &
       216.30_pm_r,26680._pm_r,14._pm_r,213._pm_r,6.36_pm_r,155._pm_r,10._pm_r,349._pm_r,1.48_pm_r,289._pm_r, &
       218.40_pm_r,29861._pm_r,21._pm_r,183._pm_r,8.76_pm_r,135._pm_r,11._pm_r,339._pm_r,1.38_pm_r,269._pm_r, &
       223.40_pm_r,33093._pm_r,31._pm_r,161._pm_r,10.81_pm_r,122._pm_r,12._pm_r,329._pm_r,1.29_pm_r,242._pm_r, &
       230.30_pm_r,36416._pm_r,45._pm_r,147._pm_r,11.34_pm_r,113._pm_r,11._pm_r,320._pm_r,1.26_pm_r,215._pm_r, &
       239.30_pm_r,39850._pm_r,58._pm_r,137._pm_r,10.20_pm_r,103._pm_r,11._pm_r,311._pm_r,1.23_pm_r,191._pm_r, &
       249.50_pm_r,43435._pm_r,68._pm_r,130._pm_r,7.24_pm_r,90._pm_r,10._pm_r,303._pm_r,1.18_pm_r,167._pm_r, &
       256.40_pm_r,47142._pm_r,74._pm_r,125._pm_r,4.83_pm_r,69._pm_r,8._pm_r,296._pm_r,1.19_pm_r,150._pm_r, &
       255.00_pm_r,50897._pm_r,76._pm_r,121._pm_r,3.54_pm_r,35._pm_r,7._pm_r,289._pm_r,1.14_pm_r,140._pm_r, &
       247.90_pm_r,54582._pm_r,75._pm_r,117._pm_r,3.55_pm_r,356._pm_r,5._pm_r,281._pm_r,.93_pm_r,133._pm_r, &
       242.40_pm_r,58172._pm_r,71._pm_r,114._pm_r,4.18_pm_r,331._pm_r,4._pm_r,276._pm_r,.62_pm_r,113._pm_r, &
       236.30_pm_r,61679._pm_r,66._pm_r,111._pm_r,4.48_pm_r,317._pm_r,4._pm_r,275._pm_r,.44_pm_r,73._pm_r, &
       231.00_pm_r,65097._pm_r,60._pm_r,109._pm_r,4.20_pm_r,307._pm_r,3._pm_r,284._pm_r,.50_pm_r,36._pm_r, &
       226.60_pm_r,68448._pm_r,54._pm_r,108._pm_r,3.63_pm_r,298._pm_r,3._pm_r,298._pm_r,.61_pm_r,18._pm_r, &
       223.10_pm_r,71739._pm_r,50._pm_r,107._pm_r,3.02_pm_r,288._pm_r,4._pm_r,314._pm_r,.70_pm_r,9._pm_r, &
       219.80_pm_r,74984._pm_r,46._pm_r,108._pm_r,2.51_pm_r,278._pm_r,4._pm_r,325._pm_r,.75_pm_r,5._pm_r, &
       216.30_pm_r,78174._pm_r,42._pm_r,109._pm_r,2.14_pm_r,268._pm_r,5._pm_r,333._pm_r,.77_pm_r,3._pm_r, &
       210.40_pm_r,81317._pm_r,40._pm_r,110._pm_r,1.83_pm_r,259._pm_r,6._pm_r,338._pm_r,.75_pm_r,0._pm_r, &
       200.10_pm_r,84312._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.70_pm_r,87148._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.40_pm_r,89861._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       180.40_pm_r,92523._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.80_pm_r,95199._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.10_pm_r,97957._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.40_pm_r,100880._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.30_pm_r,104073._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.10_pm_r,107678._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       294.90_pm_r,111924._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       374.90_pm_r,117250._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
       268.50_pm_r,-36._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       251.00_pm_r,3758._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       228.00_pm_r,7255._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.70_pm_r,10532._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       221.00_pm_r,13757._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.40_pm_r,16983._pm_r,8._pm_r,327._pm_r,2.60_pm_r,229._pm_r,7._pm_r,7._pm_r,.49_pm_r,350._pm_r, &
       217.00_pm_r,20180._pm_r,8._pm_r,296._pm_r,3.62_pm_r,213._pm_r,7._pm_r,4._pm_r,.65_pm_r,339._pm_r, &
       214.60_pm_r,23339._pm_r,10._pm_r,258._pm_r,4.85_pm_r,190._pm_r,8._pm_r,0._pm_r,.79_pm_r,321._pm_r, &
       213.60_pm_r,26472._pm_r,14._pm_r,223._pm_r,6.53_pm_r,167._pm_r,9._pm_r,354._pm_r,.90_pm_r,298._pm_r, &
       215.30_pm_r,29611._pm_r,21._pm_r,193._pm_r,8.71_pm_r,147._pm_r,10._pm_r,346._pm_r,1.07_pm_r,271._pm_r, &
       216.70_pm_r,32775._pm_r,32._pm_r,172._pm_r,10.97_pm_r,134._pm_r,10._pm_r,336._pm_r,1.34_pm_r,250._pm_r, &
       221.30_pm_r,35979._pm_r,45._pm_r,157._pm_r,11.75_pm_r,123._pm_r,10._pm_r,324._pm_r,1.52_pm_r,233._pm_r, &
       230.50_pm_r,39280._pm_r,59._pm_r,147._pm_r,10.55_pm_r,112._pm_r,10._pm_r,311._pm_r,1.48_pm_r,217._pm_r, &
       242.80_pm_r,42751._pm_r,70._pm_r,139._pm_r,7.83_pm_r,97._pm_r,10._pm_r,299._pm_r,1.33_pm_r,195._pm_r, &
       253.10_pm_r,46384._pm_r,76._pm_r,134._pm_r,5.78_pm_r,75._pm_r,9._pm_r,288._pm_r,1.32_pm_r,177._pm_r, &
       255.40_pm_r,50121._pm_r,78._pm_r,129._pm_r,4.26_pm_r,42._pm_r,8._pm_r,277._pm_r,1.16_pm_r,164._pm_r, &
       250.60_pm_r,53829._pm_r,77._pm_r,125._pm_r,3.97_pm_r,351._pm_r,8._pm_r,268._pm_r,.68_pm_r,147._pm_r, &
       246.60_pm_r,57468._pm_r,71._pm_r,122._pm_r,5.05_pm_r,318._pm_r,7._pm_r,265._pm_r,.33_pm_r,77._pm_r, &
       242.00_pm_r,61049._pm_r,63._pm_r,121._pm_r,5.81_pm_r,303._pm_r,7._pm_r,269._pm_r,.60_pm_r,24._pm_r, &
       236.00_pm_r,64548._pm_r,54._pm_r,122._pm_r,5.69_pm_r,293._pm_r,7._pm_r,277._pm_r,.80_pm_r,12._pm_r, &
       230.20_pm_r,67962._pm_r,47._pm_r,124._pm_r,5.04_pm_r,286._pm_r,7._pm_r,288._pm_r,.83_pm_r,8._pm_r, &
       225.90_pm_r,71298._pm_r,41._pm_r,128._pm_r,4.23_pm_r,278._pm_r,7._pm_r,298._pm_r,.79_pm_r,5._pm_r, &
       223.40_pm_r,74588._pm_r,36._pm_r,133._pm_r,3.52_pm_r,270._pm_r,7._pm_r,306._pm_r,.71_pm_r,4._pm_r, &
       221.50_pm_r,77847._pm_r,33._pm_r,139._pm_r,2.96_pm_r,261._pm_r,8._pm_r,312._pm_r,.63_pm_r,4._pm_r, &
       214.90_pm_r,81078._pm_r,31._pm_r,145._pm_r,2.49_pm_r,253._pm_r,9._pm_r,316._pm_r,.54_pm_r,3._pm_r, &
       201.60_pm_r,84110._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.50_pm_r,86944._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.90_pm_r,89647._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.90_pm_r,92299._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.90_pm_r,94971._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.40_pm_r,97737._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.10_pm_r,100688._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.40_pm_r,103931._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.70_pm_r,107596._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       293.50_pm_r,111859._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       373.80_pm_r,117151._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/) 
  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
       261.60_pm_r,23._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       246.60_pm_r,3732._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.90_pm_r,7174._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.90_pm_r,10427._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.90_pm_r,13652._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.50_pm_r,16871._pm_r,7._pm_r,330._pm_r,1.40_pm_r,226._pm_r,3._pm_r,358._pm_r,.08_pm_r,45._pm_r, &
       215.10_pm_r,20047._pm_r,7._pm_r,310._pm_r,2.05_pm_r,210._pm_r,3._pm_r,359._pm_r,.09_pm_r,18._pm_r, &
       212.10_pm_r,23173._pm_r,6._pm_r,278._pm_r,2.90_pm_r,189._pm_r,4._pm_r,359._pm_r,.13_pm_r,331._pm_r, &
       210.20_pm_r,26263._pm_r,7._pm_r,237._pm_r,4.01_pm_r,169._pm_r,4._pm_r,356._pm_r,.25_pm_r,299._pm_r, &
       211.10_pm_r,29346._pm_r,11._pm_r,200._pm_r,5.29_pm_r,152._pm_r,4._pm_r,350._pm_r,.40_pm_r,281._pm_r, &
       210.60_pm_r,32434._pm_r,18._pm_r,176._pm_r,6.64_pm_r,139._pm_r,4._pm_r,341._pm_r,.54_pm_r,268._pm_r, &
       214.30_pm_r,35539._pm_r,26._pm_r,161._pm_r,7.06_pm_r,128._pm_r,4._pm_r,330._pm_r,.60_pm_r,255._pm_r, &
       224.20_pm_r,38742._pm_r,34._pm_r,151._pm_r,6.19_pm_r,118._pm_r,5._pm_r,319._pm_r,.55_pm_r,236._pm_r, &
       237.90_pm_r,42129._pm_r,41._pm_r,144._pm_r,4.41_pm_r,102._pm_r,5._pm_r,310._pm_r,.47_pm_r,222._pm_r, &
       250.60_pm_r,45705._pm_r,44._pm_r,138._pm_r,3.35_pm_r,78._pm_r,5._pm_r,303._pm_r,.30_pm_r,222._pm_r, &
       256.30_pm_r,49430._pm_r,46._pm_r,133._pm_r,2.76_pm_r,51._pm_r,5._pm_r,300._pm_r,.10_pm_r,273._pm_r, &
       254.30_pm_r,53172._pm_r,45._pm_r,128._pm_r,2.18_pm_r,20._pm_r,5._pm_r,301._pm_r,.24_pm_r,2._pm_r, &
       251.90_pm_r,56879._pm_r,44._pm_r,125._pm_r,1.89_pm_r,343._pm_r,5._pm_r,305._pm_r,.27_pm_r,48._pm_r, &
       248.00_pm_r,60544._pm_r,41._pm_r,124._pm_r,2.01_pm_r,315._pm_r,5._pm_r,310._pm_r,.37_pm_r,85._pm_r, &
       241.30_pm_r,64127._pm_r,38._pm_r,124._pm_r,2.11_pm_r,300._pm_r,4._pm_r,314._pm_r,.55_pm_r,111._pm_r, &
       233.90_pm_r,67608._pm_r,35._pm_r,125._pm_r,2.09_pm_r,290._pm_r,3._pm_r,319._pm_r,.77_pm_r,126._pm_r, &
       228.50_pm_r,70988._pm_r,32._pm_r,126._pm_r,2.00_pm_r,284._pm_r,2._pm_r,325._pm_r,1.03_pm_r,133._pm_r, &
       225.90_pm_r,74314._pm_r,30._pm_r,128._pm_r,1.89_pm_r,280._pm_r,1._pm_r,6._pm_r,1.25_pm_r,137._pm_r, &
       224.50_pm_r,77614._pm_r,28._pm_r,131._pm_r,1.78_pm_r,276._pm_r,2._pm_r,127._pm_r,1.39_pm_r,139._pm_r, &
       217.30_pm_r,80891._pm_r,26._pm_r,135._pm_r,1.62_pm_r,273._pm_r,4._pm_r,134._pm_r,1.43_pm_r,141._pm_r, &
       202.40_pm_r,83942._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.30_pm_r,86775._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.50_pm_r,89472._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       179.70_pm_r,92119._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.20_pm_r,94789._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.50_pm_r,97563._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.00_pm_r,100534._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.00_pm_r,103810._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       254.70_pm_r,107512._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       292.00_pm_r,111778._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       373.00_pm_r,117041._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_octobre = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_octobre.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_octobre,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_octobre
