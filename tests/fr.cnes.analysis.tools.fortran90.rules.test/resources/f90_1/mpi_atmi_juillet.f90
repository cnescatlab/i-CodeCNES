subroutine mpi_atmi_juillet (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de JUILLET 
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
       265.20_pm_r,-354._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       240.50_pm_r,3337._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.50_pm_r,6687._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.40_pm_r,9767._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.30_pm_r,12684._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.50_pm_r,15521._pm_r,4._pm_r,288._pm_r,1.53_pm_r,207._pm_r,4._pm_r,185._pm_r,.42_pm_r,3._pm_r, &
       187.90_pm_r,18299._pm_r,5._pm_r,256._pm_r,2.34_pm_r,205._pm_r,3._pm_r,185._pm_r,.75_pm_r,6._pm_r, &
       185.00_pm_r,21031._pm_r,8._pm_r,234._pm_r,2.46_pm_r,198._pm_r,2._pm_r,182._pm_r,1.03_pm_r,11._pm_r, &
       178.00_pm_r,23689._pm_r,10._pm_r,223._pm_r,1.13_pm_r,158._pm_r,1._pm_r,163._pm_r,.63_pm_r,14._pm_r, &
       185.10_pm_r,26330._pm_r,9._pm_r,215._pm_r,1.52_pm_r,70._pm_r,0._pm_r,101._pm_r,.21_pm_r,33._pm_r, &
       204.40_pm_r,29173._pm_r,7._pm_r,205._pm_r,2.51_pm_r,52._pm_r,1._pm_r,94._pm_r,.14_pm_r,150._pm_r, &
       222.30_pm_r,32305._pm_r,3._pm_r,176._pm_r,3.04_pm_r,47._pm_r,1._pm_r,121._pm_r,.30_pm_r,177._pm_r, &
       236.90_pm_r,35667._pm_r,4._pm_r,93._pm_r,3.17_pm_r,45._pm_r,1._pm_r,147._pm_r,.40_pm_r,187._pm_r, &
       246.60_pm_r,39212._pm_r,7._pm_r,69._pm_r,2.43_pm_r,54._pm_r,2._pm_r,159._pm_r,.25_pm_r,180._pm_r, &
       256.00_pm_r,42889._pm_r,10._pm_r,66._pm_r,1.95_pm_r,65._pm_r,2._pm_r,160._pm_r,.11_pm_r,146._pm_r, &
       266.00_pm_r,46714._pm_r,13._pm_r,66._pm_r,1.84_pm_r,66._pm_r,2._pm_r,159._pm_r,.02_pm_r,63._pm_r, &
       273.10_pm_r,50665._pm_r,16._pm_r,65._pm_r,1.99_pm_r,52._pm_r,2._pm_r,161._pm_r,.14_pm_r,270._pm_r, &
       270.80_pm_r,54657._pm_r,18._pm_r,62._pm_r,1.96_pm_r,35._pm_r,2._pm_r,174._pm_r,.37_pm_r,257._pm_r, &
       261.80_pm_r,58565._pm_r,21._pm_r,58._pm_r,1.80_pm_r,20._pm_r,2._pm_r,194._pm_r,.52_pm_r,252._pm_r, &
       250.40_pm_r,62313._pm_r,23._pm_r,53._pm_r,1.51_pm_r,6._pm_r,3._pm_r,210._pm_r,.56_pm_r,250._pm_r, &
       239.80_pm_r,65903._pm_r,24._pm_r,49._pm_r,1.18_pm_r,349._pm_r,3._pm_r,219._pm_r,.51_pm_r,247._pm_r, &
       231.90_pm_r,69354._pm_r,24._pm_r,46._pm_r,.93_pm_r,325._pm_r,4._pm_r,224._pm_r,.42_pm_r,242._pm_r, &
       226.30_pm_r,72709._pm_r,24._pm_r,43._pm_r,.87_pm_r,296._pm_r,4._pm_r,226._pm_r,.35_pm_r,237._pm_r, &
       222.70_pm_r,75990._pm_r,24._pm_r,40._pm_r,.96_pm_r,274._pm_r,5._pm_r,226._pm_r,.25_pm_r,231._pm_r, &
       221.40_pm_r,79226._pm_r,23._pm_r,37._pm_r,1.04_pm_r,261._pm_r,5._pm_r,226._pm_r,.19_pm_r,223._pm_r, &
       223.30_pm_r,82468._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.90_pm_r,85749._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.00_pm_r,88975._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.20_pm_r,92062._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.50_pm_r,95069._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.30_pm_r,98075._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       209.10_pm_r,101157._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.20_pm_r,104406._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       236.90_pm_r,107918._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.80_pm_r,111822._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       321.10_pm_r,116430._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
       270.00_pm_r,-321._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       243.00_pm_r,3425._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.40_pm_r,6809._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.80_pm_r,9921._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.30_pm_r,12886._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.20_pm_r,15793._pm_r,13._pm_r,240._pm_r,2.23_pm_r,193._pm_r,4._pm_r,188._pm_r,.45_pm_r,343._pm_r, &
       195.00_pm_r,18665._pm_r,16._pm_r,229._pm_r,3.10_pm_r,185._pm_r,4._pm_r,193._pm_r,.66_pm_r,347._pm_r, &
       192.50_pm_r,21503._pm_r,19._pm_r,217._pm_r,3.09_pm_r,162._pm_r,3._pm_r,201._pm_r,.64_pm_r,358._pm_r, &
       186.70_pm_r,24278._pm_r,20._pm_r,206._pm_r,2.84_pm_r,109._pm_r,2._pm_r,205._pm_r,.37_pm_r,35._pm_r, &
       192.30_pm_r,27040._pm_r,18._pm_r,192._pm_r,4.06_pm_r,68._pm_r,2._pm_r,192._pm_r,.40_pm_r,107._pm_r, &
       208.50_pm_r,29964._pm_r,14._pm_r,172._pm_r,5.33_pm_r,51._pm_r,2._pm_r,172._pm_r,.63_pm_r,133._pm_r, &
       225.20_pm_r,33146._pm_r,12._pm_r,135._pm_r,5.89_pm_r,43._pm_r,3._pm_r,161._pm_r,.75_pm_r,145._pm_r, &
       239.90_pm_r,36550._pm_r,14._pm_r,96._pm_r,5.79_pm_r,36._pm_r,4._pm_r,158._pm_r,.77_pm_r,153._pm_r, &
       249.90_pm_r,40142._pm_r,19._pm_r,75._pm_r,4.73_pm_r,34._pm_r,5._pm_r,158._pm_r,.32_pm_r,164._pm_r, &
       258.90_pm_r,43864._pm_r,24._pm_r,65._pm_r,3.77_pm_r,33._pm_r,5._pm_r,159._pm_r,.19_pm_r,278._pm_r, &
       266.60_pm_r,47718._pm_r,28._pm_r,60._pm_r,3.07_pm_r,30._pm_r,5._pm_r,163._pm_r,.44_pm_r,291._pm_r, &
       269.70_pm_r,51651._pm_r,31._pm_r,55._pm_r,2.59_pm_r,21._pm_r,4._pm_r,172._pm_r,.55_pm_r,271._pm_r, &
       265.50_pm_r,55576._pm_r,34._pm_r,52._pm_r,1.96_pm_r,360._pm_r,5._pm_r,182._pm_r,.62_pm_r,238._pm_r, &
       256.70_pm_r,59406._pm_r,35._pm_r,48._pm_r,1.52_pm_r,332._pm_r,5._pm_r,190._pm_r,.73_pm_r,217._pm_r, &
       246.30_pm_r,63086._pm_r,35._pm_r,45._pm_r,1.28_pm_r,303._pm_r,6._pm_r,193._pm_r,.80_pm_r,206._pm_r, &
       237.00_pm_r,66626._pm_r,34._pm_r,42._pm_r,1.21_pm_r,273._pm_r,8._pm_r,194._pm_r,.80_pm_r,197._pm_r, &
       229.60_pm_r,70039._pm_r,33._pm_r,40._pm_r,1.28_pm_r,248._pm_r,9._pm_r,194._pm_r,.76_pm_r,189._pm_r, &
       224.00_pm_r,73360._pm_r,31._pm_r,39._pm_r,1.45_pm_r,229._pm_r,10._pm_r,193._pm_r,.72_pm_r,182._pm_r, &
       220.30_pm_r,76608._pm_r,29._pm_r,39._pm_r,1.61_pm_r,218._pm_r,11._pm_r,192._pm_r,.68_pm_r,177._pm_r, &
       219.00_pm_r,79809._pm_r,26._pm_r,39._pm_r,1.68_pm_r,210._pm_r,12._pm_r,191._pm_r,.62_pm_r,171._pm_r, &
       221.20_pm_r,83024._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.30_pm_r,86276._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.20_pm_r,89450._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.10_pm_r,92487._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.70_pm_r,95452._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.60_pm_r,98419._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.00_pm_r,101460._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       216.80_pm_r,104661._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       234.20_pm_r,108126._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       263.10_pm_r,112010._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       323.80_pm_r,116651._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
       274.80_pm_r,-124._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.00_pm_r,3704._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.30_pm_r,7162._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.00_pm_r,10336._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.30_pm_r,13385._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.30_pm_r,16407._pm_r,21._pm_r,234._pm_r,3.05_pm_r,159._pm_r,0._pm_r,210._pm_r,.54_pm_r,244._pm_r, &
       203.10_pm_r,19398._pm_r,22._pm_r,220._pm_r,4.13_pm_r,148._pm_r,1._pm_r,232._pm_r,.89_pm_r,229._pm_r, &
       201.30_pm_r,22359._pm_r,24._pm_r,205._pm_r,4.61_pm_r,126._pm_r,3._pm_r,224._pm_r,1.29_pm_r,210._pm_r, &
       197.10_pm_r,25275._pm_r,24._pm_r,188._pm_r,4.90_pm_r,91._pm_r,5._pm_r,214._pm_r,1.58_pm_r,191._pm_r, &
       200.70_pm_r,28178._pm_r,22._pm_r,170._pm_r,6.01_pm_r,58._pm_r,7._pm_r,203._pm_r,1.67_pm_r,173._pm_r, &
       211.50_pm_r,31189._pm_r,19._pm_r,144._pm_r,7.49_pm_r,39._pm_r,9._pm_r,194._pm_r,1.66_pm_r,160._pm_r, &
       225.30_pm_r,34390._pm_r,18._pm_r,107._pm_r,8.10_pm_r,28._pm_r,11._pm_r,186._pm_r,1.52_pm_r,149._pm_r, &
       239.80_pm_r,37793._pm_r,23._pm_r,76._pm_r,7.68_pm_r,20._pm_r,13._pm_r,180._pm_r,1.26_pm_r,139._pm_r, &
       250.30_pm_r,41388._pm_r,29._pm_r,59._pm_r,6.28_pm_r,17._pm_r,13._pm_r,176._pm_r,.52_pm_r,121._pm_r, &
       258.80_pm_r,45114._pm_r,36._pm_r,50._pm_r,5.07_pm_r,13._pm_r,14._pm_r,175._pm_r,.10_pm_r,270._pm_r, &
       263.70_pm_r,48948._pm_r,41._pm_r,44._pm_r,4.11_pm_r,6._pm_r,14._pm_r,177._pm_r,.73_pm_r,262._pm_r, &
       262.10_pm_r,52805._pm_r,45._pm_r,39._pm_r,3.22_pm_r,351._pm_r,14._pm_r,183._pm_r,1.33_pm_r,251._pm_r, &
       256.80_pm_r,56606._pm_r,47._pm_r,35._pm_r,2.16_pm_r,320._pm_r,15._pm_r,190._pm_r,1.35_pm_r,245._pm_r, &
       249.80_pm_r,60319._pm_r,47._pm_r,32._pm_r,1.85_pm_r,279._pm_r,16._pm_r,195._pm_r,1.22_pm_r,236._pm_r, &
       242.20_pm_r,63920._pm_r,45._pm_r,29._pm_r,1.92_pm_r,248._pm_r,18._pm_r,198._pm_r,1.00_pm_r,222._pm_r, &
       235.20_pm_r,67417._pm_r,43._pm_r,27._pm_r,1.99_pm_r,226._pm_r,19._pm_r,199._pm_r,.84_pm_r,199._pm_r, &
       228.70_pm_r,70811._pm_r,40._pm_r,27._pm_r,2.03_pm_r,207._pm_r,20._pm_r,198._pm_r,.85_pm_r,171._pm_r, &
       223.00_pm_r,74119._pm_r,37._pm_r,27._pm_r,2.11_pm_r,192._pm_r,21._pm_r,196._pm_r,1.00_pm_r,151._pm_r, &
       219.00_pm_r,77352._pm_r,34._pm_r,29._pm_r,2.18_pm_r,181._pm_r,22._pm_r,193._pm_r,1.14_pm_r,140._pm_r, &
       217.30_pm_r,80533._pm_r,31._pm_r,32._pm_r,2.16_pm_r,173._pm_r,23._pm_r,189._pm_r,1.20_pm_r,133._pm_r, &
       218.10_pm_r,83724._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.20_pm_r,86927._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.40_pm_r,90016._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       198.70_pm_r,92980._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.80_pm_r,95887._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.90_pm_r,98801._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.50_pm_r,101784._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.80_pm_r,104916._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.90_pm_r,108310._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.90_pm_r,112155._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       327.60_pm_r,116838._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
       279.60_pm_r,-49._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       255.30_pm_r,3863._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.30_pm_r,7407._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.10_pm_r,10657._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.20_pm_r,13797._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.80_pm_r,16927._pm_r,14._pm_r,207._pm_r,3.32_pm_r,147._pm_r,4._pm_r,30._pm_r,.76_pm_r,228._pm_r, &
       211.30_pm_r,20030._pm_r,17._pm_r,190._pm_r,4.20_pm_r,135._pm_r,2._pm_r,21._pm_r,1.25_pm_r,221._pm_r, &
       212.30_pm_r,23134._pm_r,20._pm_r,173._pm_r,4.52_pm_r,113._pm_r,1._pm_r,291._pm_r,1.76_pm_r,213._pm_r, &
       210.20_pm_r,26230._pm_r,23._pm_r,157._pm_r,5.02_pm_r,81._pm_r,3._pm_r,220._pm_r,2.15_pm_r,204._pm_r, &
       210.00_pm_r,29303._pm_r,24._pm_r,137._pm_r,6.59_pm_r,49._pm_r,6._pm_r,209._pm_r,2.27_pm_r,195._pm_r, &
       215.00_pm_r,32407._pm_r,25._pm_r,111._pm_r,8.48_pm_r,29._pm_r,9._pm_r,202._pm_r,2.06_pm_r,184._pm_r, &
       225.00_pm_r,35627._pm_r,28._pm_r,83._pm_r,9.11_pm_r,16._pm_r,12._pm_r,197._pm_r,1.66_pm_r,168._pm_r, &
       238.00_pm_r,39013._pm_r,34._pm_r,62._pm_r,8.24_pm_r,5._pm_r,14._pm_r,191._pm_r,1.31_pm_r,144._pm_r, &
       249.70_pm_r,42592._pm_r,41._pm_r,49._pm_r,6.08_pm_r,359._pm_r,14._pm_r,185._pm_r,.94_pm_r,94._pm_r, &
       256.90_pm_r,46304._pm_r,45._pm_r,42._pm_r,4.46_pm_r,351._pm_r,14._pm_r,180._pm_r,.72_pm_r,62._pm_r, &
       257.80_pm_r,50081._pm_r,49._pm_r,36._pm_r,3.35_pm_r,338._pm_r,13._pm_r,179._pm_r,.23_pm_r,299._pm_r, &
       253.30_pm_r,53830._pm_r,51._pm_r,32._pm_r,2.63_pm_r,317._pm_r,14._pm_r,183._pm_r,1.51_pm_r,247._pm_r, &
       247.10_pm_r,57494._pm_r,50._pm_r,28._pm_r,2.44_pm_r,273._pm_r,15._pm_r,192._pm_r,1.96_pm_r,241._pm_r, &
       240.90_pm_r,61069._pm_r,48._pm_r,24._pm_r,2.70_pm_r,244._pm_r,17._pm_r,199._pm_r,2.00_pm_r,236._pm_r, &
       235.10_pm_r,64552._pm_r,45._pm_r,22._pm_r,2.57_pm_r,226._pm_r,20._pm_r,204._pm_r,1.67_pm_r,230._pm_r, &
       229.60_pm_r,67957._pm_r,41._pm_r,20._pm_r,2.16_pm_r,207._pm_r,22._pm_r,206._pm_r,1.18_pm_r,218._pm_r, &
       224.60_pm_r,71280._pm_r,39._pm_r,21._pm_r,1.80_pm_r,182._pm_r,23._pm_r,206._pm_r,.76_pm_r,192._pm_r, &
       220.10_pm_r,74537._pm_r,36._pm_r,23._pm_r,1.75_pm_r,155._pm_r,24._pm_r,204._pm_r,.64_pm_r,150._pm_r, &
       216.50_pm_r,77732._pm_r,35._pm_r,27._pm_r,1.89_pm_r,136._pm_r,24._pm_r,202._pm_r,.78_pm_r,121._pm_r, &
       214.60_pm_r,80877._pm_r,35._pm_r,31._pm_r,1.98_pm_r,125._pm_r,24._pm_r,199._pm_r,.91_pm_r,107._pm_r, &
       213.70_pm_r,84026._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.70_pm_r,87156._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.10_pm_r,90148._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.90_pm_r,93033._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.00_pm_r,95880._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.90_pm_r,98738._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.00_pm_r,101661._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.10_pm_r,104715._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.50_pm_r,108025._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       261.60_pm_r,111816._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       331.60_pm_r,116535._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
       286.10_pm_r,-24._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       261.30_pm_r,3983._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.10_pm_r,7617._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.30_pm_r,10943._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.80_pm_r,14128._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.70_pm_r,17280._pm_r,4._pm_r,214._pm_r,2.55_pm_r,146._pm_r,3._pm_r,62._pm_r,.70_pm_r,277._pm_r, &
       215.30_pm_r,20427._pm_r,7._pm_r,178._pm_r,2.96_pm_r,134._pm_r,2._pm_r,43._pm_r,.98_pm_r,264._pm_r, &
       218.60_pm_r,23607._pm_r,10._pm_r,157._pm_r,2.84_pm_r,109._pm_r,1._pm_r,338._pm_r,1.22_pm_r,245._pm_r, &
       219.70_pm_r,26817._pm_r,12._pm_r,138._pm_r,3.19_pm_r,68._pm_r,2._pm_r,261._pm_r,1.48_pm_r,225._pm_r, &
       220.40_pm_r,30039._pm_r,13._pm_r,112._pm_r,4.79_pm_r,34._pm_r,4._pm_r,236._pm_r,1.73_pm_r,205._pm_r, &
       223.70_pm_r,33287._pm_r,16._pm_r,80._pm_r,6.66_pm_r,16._pm_r,6._pm_r,220._pm_r,1.88_pm_r,187._pm_r, &
       231.40_pm_r,36617._pm_r,22._pm_r,53._pm_r,7.33_pm_r,4._pm_r,8._pm_r,208._pm_r,1.81_pm_r,169._pm_r, &
       243.00_pm_r,40084._pm_r,29._pm_r,37._pm_r,6.52_pm_r,353._pm_r,10._pm_r,197._pm_r,1.63_pm_r,147._pm_r, &
       254.90_pm_r,43739._pm_r,35._pm_r,27._pm_r,4.47_pm_r,341._pm_r,11._pm_r,188._pm_r,1.18_pm_r,108._pm_r, &
       259.90_pm_r,47517._pm_r,38._pm_r,20._pm_r,3.11_pm_r,322._pm_r,11._pm_r,180._pm_r,.95_pm_r,75._pm_r, &
       255.60_pm_r,51299._pm_r,40._pm_r,15._pm_r,2.53_pm_r,291._pm_r,11._pm_r,175._pm_r,.40_pm_r,28._pm_r, &
       246.30_pm_r,54980._pm_r,39._pm_r,9._pm_r,2.52_pm_r,261._pm_r,10._pm_r,178._pm_r,.91_pm_r,269._pm_r, &
       238.40_pm_r,58525._pm_r,37._pm_r,4._pm_r,3.12_pm_r,233._pm_r,11._pm_r,186._pm_r,1.27_pm_r,258._pm_r, &
       232.50_pm_r,61973._pm_r,34._pm_r,359._pm_r,3.29_pm_r,217._pm_r,11._pm_r,195._pm_r,1.31_pm_r,251._pm_r, &
       226.30_pm_r,65332._pm_r,30._pm_r,354._pm_r,2.84_pm_r,206._pm_r,13._pm_r,202._pm_r,1.11_pm_r,242._pm_r, &
       220.50_pm_r,68604._pm_r,27._pm_r,351._pm_r,2.07_pm_r,191._pm_r,14._pm_r,205._pm_r,.81_pm_r,227._pm_r, &
       217.20_pm_r,71807._pm_r,24._pm_r,350._pm_r,1.42_pm_r,167._pm_r,15._pm_r,206._pm_r,.60_pm_r,204._pm_r, &
       214.10_pm_r,74967._pm_r,22._pm_r,351._pm_r,1.20_pm_r,131._pm_r,16._pm_r,205._pm_r,.54_pm_r,175._pm_r, &
       210.80_pm_r,78075._pm_r,22._pm_r,355._pm_r,1.30_pm_r,105._pm_r,16._pm_r,203._pm_r,.57_pm_r,155._pm_r, &
       209.30_pm_r,81139._pm_r,21._pm_r,0._pm_r,1.43_pm_r,90._pm_r,17._pm_r,201._pm_r,.59_pm_r,143._pm_r, &
       207.80_pm_r,84213._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.80_pm_r,87246._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.50_pm_r,90141._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.80_pm_r,92958._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.90_pm_r,95756._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.60_pm_r,98570._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.90_pm_r,101440._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.70_pm_r,104426._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.20_pm_r,107654._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       259.80_pm_r,111387._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       335.30_pm_r,116135._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
       291.20_pm_r,-29._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       267.80_pm_r,4067._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.80_pm_r,7807._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       222.40_pm_r,11216._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       211.00_pm_r,14391._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       210.00_pm_r,17461._pm_r,2._pm_r,178._pm_r,1.09_pm_r,161._pm_r,3._pm_r,91._pm_r,.75_pm_r,298._pm_r, &
       214.50_pm_r,20569._pm_r,3._pm_r,167._pm_r,1.00_pm_r,148._pm_r,2._pm_r,76._pm_r,.91_pm_r,289._pm_r, &
       219.50_pm_r,23750._pm_r,4._pm_r,157._pm_r,.67_pm_r,95._pm_r,1._pm_r,38._pm_r,.87_pm_r,272._pm_r, &
       223.80_pm_r,26995._pm_r,4._pm_r,140._pm_r,1.35_pm_r,32._pm_r,1._pm_r,305._pm_r,.80_pm_r,242._pm_r, &
       227.30_pm_r,30301._pm_r,4._pm_r,95._pm_r,2.64_pm_r,13._pm_r,2._pm_r,253._pm_r,.92_pm_r,205._pm_r, &
       231.50_pm_r,33656._pm_r,6._pm_r,45._pm_r,3.75_pm_r,4._pm_r,3._pm_r,222._pm_r,1.17_pm_r,178._pm_r, &
       239.70_pm_r,37103._pm_r,11._pm_r,23._pm_r,3.99_pm_r,357._pm_r,4._pm_r,200._pm_r,1.30_pm_r,159._pm_r, &
       251.00_pm_r,40692._pm_r,16._pm_r,13._pm_r,3.39_pm_r,347._pm_r,5._pm_r,184._pm_r,1.25_pm_r,142._pm_r, &
       260.40_pm_r,44445._pm_r,20._pm_r,7._pm_r,2.23_pm_r,331._pm_r,6._pm_r,174._pm_r,.80_pm_r,117._pm_r, &
       263.20_pm_r,48286._pm_r,21._pm_r,1._pm_r,1.71_pm_r,298._pm_r,7._pm_r,166._pm_r,.43_pm_r,81._pm_r, &
       257.60_pm_r,52105._pm_r,22._pm_r,355._pm_r,1.81_pm_r,261._pm_r,7._pm_r,164._pm_r,.21_pm_r,347._pm_r, &
       247.00_pm_r,55806._pm_r,21._pm_r,348._pm_r,2.09_pm_r,231._pm_r,6._pm_r,167._pm_r,.55_pm_r,273._pm_r, &
       236.90_pm_r,59345._pm_r,19._pm_r,340._pm_r,2.55_pm_r,208._pm_r,6._pm_r,175._pm_r,.57_pm_r,266._pm_r, &
       227.90_pm_r,62750._pm_r,16._pm_r,331._pm_r,2.61_pm_r,195._pm_r,6._pm_r,182._pm_r,.49_pm_r,260._pm_r, &
       220.10_pm_r,66027._pm_r,14._pm_r,321._pm_r,2.18_pm_r,185._pm_r,6._pm_r,187._pm_r,.37_pm_r,251._pm_r, &
       214.20_pm_r,69207._pm_r,12._pm_r,313._pm_r,1.55_pm_r,174._pm_r,7._pm_r,191._pm_r,.26_pm_r,235._pm_r, &
       210.80_pm_r,72317._pm_r,10._pm_r,308._pm_r,.97_pm_r,154._pm_r,7._pm_r,192._pm_r,.19_pm_r,213._pm_r, &
       208.00_pm_r,75385._pm_r,9._pm_r,306._pm_r,.67_pm_r,118._pm_r,7._pm_r,192._pm_r,.17_pm_r,188._pm_r, &
       205.40_pm_r,78411._pm_r,8._pm_r,309._pm_r,.69_pm_r,84._pm_r,7._pm_r,192._pm_r,.17_pm_r,170._pm_r, &
       204.30_pm_r,81403._pm_r,8._pm_r,316._pm_r,.78_pm_r,65._pm_r,8._pm_r,191._pm_r,.17_pm_r,158._pm_r, &
       202.50_pm_r,84405._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.50_pm_r,87348._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.50_pm_r,90169._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.40_pm_r,92942._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.10_pm_r,95712._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.50_pm_r,98501._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.00_pm_r,101336._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.70_pm_r,104274._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       215.20_pm_r,107442._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       258.40_pm_r,111133._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       338.60_pm_r,115906._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
       295.90_pm_r,-35._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       273.70_pm_r,4141._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.70_pm_r,7978._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.50_pm_r,11455._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.30_pm_r,14606._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.20_pm_r,17580._pm_r,2._pm_r,146._pm_r,.37_pm_r,239._pm_r,2._pm_r,77._pm_r,.37_pm_r,305._pm_r, &
       212.70_pm_r,20633._pm_r,2._pm_r,163._pm_r,.39_pm_r,266._pm_r,2._pm_r,61._pm_r,.43_pm_r,298._pm_r, &
       219.30_pm_r,23800._pm_r,2._pm_r,180._pm_r,.48_pm_r,311._pm_r,1._pm_r,39._pm_r,.38_pm_r,284._pm_r, &
       224.40_pm_r,27049._pm_r,1._pm_r,210._pm_r,.80_pm_r,339._pm_r,1._pm_r,16._pm_r,.30_pm_r,255._pm_r, &
       229.50_pm_r,30373._pm_r,1._pm_r,306._pm_r,1.21_pm_r,351._pm_r,1._pm_r,356._pm_r,.30_pm_r,209._pm_r, &
       236.50_pm_r,33781._pm_r,3._pm_r,338._pm_r,1.47_pm_r,356._pm_r,0._pm_r,326._pm_r,.41_pm_r,176._pm_r, &
       245.20_pm_r,37309._pm_r,5._pm_r,346._pm_r,1.42_pm_r,356._pm_r,0._pm_r,175._pm_r,.49_pm_r,156._pm_r, &
       254.90_pm_r,40968._pm_r,7._pm_r,348._pm_r,1.07_pm_r,351._pm_r,1._pm_r,158._pm_r,.49_pm_r,139._pm_r, &
       263.10_pm_r,44769._pm_r,8._pm_r,348._pm_r,.63_pm_r,330._pm_r,2._pm_r,148._pm_r,.30_pm_r,111._pm_r, &
       265.70_pm_r,48648._pm_r,9._pm_r,344._pm_r,.60_pm_r,281._pm_r,2._pm_r,138._pm_r,.21_pm_r,62._pm_r, &
       260.40_pm_r,52506._pm_r,9._pm_r,338._pm_r,.75_pm_r,238._pm_r,2._pm_r,131._pm_r,.11_pm_r,27._pm_r, &
       249.30_pm_r,56246._pm_r,8._pm_r,330._pm_r,1.05_pm_r,199._pm_r,2._pm_r,131._pm_r,.17_pm_r,225._pm_r, &
       237.90_pm_r,59809._pm_r,7._pm_r,320._pm_r,1.47_pm_r,180._pm_r,2._pm_r,143._pm_r,.32_pm_r,215._pm_r, &
       227.40_pm_r,63218._pm_r,5._pm_r,306._pm_r,1.53_pm_r,171._pm_r,2._pm_r,157._pm_r,.39_pm_r,216._pm_r, &
       217.20_pm_r,66471._pm_r,4._pm_r,284._pm_r,1.22_pm_r,167._pm_r,2._pm_r,169._pm_r,.37_pm_r,224._pm_r, &
       209.80_pm_r,69596._pm_r,3._pm_r,262._pm_r,.73_pm_r,164._pm_r,3._pm_r,178._pm_r,.31_pm_r,239._pm_r, &
       206.60_pm_r,72643._pm_r,3._pm_r,250._pm_r,.24_pm_r,158._pm_r,3._pm_r,186._pm_r,.27_pm_r,256._pm_r, &
       204.50_pm_r,75654._pm_r,3._pm_r,249._pm_r,.15_pm_r,356._pm_r,3._pm_r,194._pm_r,.26_pm_r,274._pm_r, &
       202.00_pm_r,78632._pm_r,3._pm_r,256._pm_r,.41_pm_r,349._pm_r,3._pm_r,201._pm_r,.26_pm_r,285._pm_r, &
       200.50_pm_r,81572._pm_r,3._pm_r,269._pm_r,.55_pm_r,347._pm_r,3._pm_r,209._pm_r,.26_pm_r,295._pm_r, &
       198.50_pm_r,84516._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       193.10_pm_r,87397._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.60_pm_r,90179._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,92936._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.60_pm_r,95699._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.80_pm_r,98482._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.80_pm_r,101304._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       195.20_pm_r,104222._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.60_pm_r,107366._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       258.70_pm_r,111046._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       341.90_pm_r,115853._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
       299.40_pm_r,-59._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.10_pm_r,4162._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.50_pm_r,8040._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.90_pm_r,11541._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.00_pm_r,14680._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.90_pm_r,17615._pm_r,1._pm_r,143._pm_r,.16_pm_r,299._pm_r,1._pm_r,20._pm_r,.04_pm_r,23._pm_r, &
       211.40_pm_r,20641._pm_r,1._pm_r,149._pm_r,.24_pm_r,309._pm_r,1._pm_r,20._pm_r,.03_pm_r,27._pm_r, &
       218.00_pm_r,23789._pm_r,1._pm_r,161._pm_r,.30_pm_r,318._pm_r,1._pm_r,20._pm_r,.02_pm_r,50._pm_r, &
       223.80_pm_r,27022._pm_r,0._pm_r,229._pm_r,.35_pm_r,328._pm_r,1._pm_r,22._pm_r,.03_pm_r,141._pm_r, &
       230.30_pm_r,30347._pm_r,1._pm_r,315._pm_r,.38_pm_r,336._pm_r,1._pm_r,24._pm_r,.06_pm_r,170._pm_r, &
       238.50_pm_r,33776._pm_r,1._pm_r,328._pm_r,.37_pm_r,344._pm_r,1._pm_r,28._pm_r,.10_pm_r,174._pm_r, &
       247.00_pm_r,37333._pm_r,2._pm_r,334._pm_r,.32_pm_r,349._pm_r,1._pm_r,34._pm_r,.12_pm_r,175._pm_r, &
       255.40_pm_r,41010._pm_r,2._pm_r,338._pm_r,.23_pm_r,356._pm_r,1._pm_r,44._pm_r,.13_pm_r,175._pm_r, &
       262.30_pm_r,44806._pm_r,2._pm_r,341._pm_r,.16_pm_r,15._pm_r,1._pm_r,57._pm_r,.08_pm_r,153._pm_r, &
       265.40_pm_r,48675._pm_r,2._pm_r,344._pm_r,.09_pm_r,52._pm_r,1._pm_r,63._pm_r,.05_pm_r,58._pm_r, &
       262.10_pm_r,52544._pm_r,2._pm_r,347._pm_r,.17_pm_r,144._pm_r,1._pm_r,58._pm_r,.08_pm_r,358._pm_r, &
       252.50_pm_r,56319._pm_r,2._pm_r,349._pm_r,.43_pm_r,159._pm_r,1._pm_r,48._pm_r,.13_pm_r,275._pm_r, &
       240.50_pm_r,59927._pm_r,1._pm_r,353._pm_r,.56_pm_r,165._pm_r,0._pm_r,31._pm_r,.27_pm_r,245._pm_r, &
       227.80_pm_r,63360._pm_r,0._pm_r,14._pm_r,.54_pm_r,166._pm_r,0._pm_r,302._pm_r,.40_pm_r,238._pm_r, &
       216.50_pm_r,66607._pm_r,0._pm_r,149._pm_r,.39_pm_r,168._pm_r,1._pm_r,254._pm_r,.46_pm_r,238._pm_r, &
       208.80_pm_r,69720._pm_r,1._pm_r,159._pm_r,.20_pm_r,173._pm_r,2._pm_r,247._pm_r,.49_pm_r,240._pm_r, &
       205.30_pm_r,72749._pm_r,1._pm_r,162._pm_r,.02_pm_r,229._pm_r,2._pm_r,245._pm_r,.51_pm_r,244._pm_r, &
       203.30_pm_r,75740._pm_r,1._pm_r,164._pm_r,.13_pm_r,335._pm_r,3._pm_r,245._pm_r,.51_pm_r,247._pm_r, &
       201.70_pm_r,78705._pm_r,1._pm_r,166._pm_r,.22_pm_r,341._pm_r,4._pm_r,246._pm_r,.51_pm_r,249._pm_r, &
       199.90_pm_r,81647._pm_r,0._pm_r,172._pm_r,.28_pm_r,341._pm_r,4._pm_r,247._pm_r,.48_pm_r,251._pm_r, &
       196.70_pm_r,84566._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.50_pm_r,87413._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.40_pm_r,90186._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,92946._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.00_pm_r,95715._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.20_pm_r,98504._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       189.10_pm_r,101332._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       196.00_pm_r,104260._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       214.80_pm_r,107419._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       261.10_pm_r,111129._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       345.60_pm_r,115988._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
       300.30_pm_r,-89._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.30_pm_r,4141._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.90_pm_r,8023._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.30_pm_r,11531._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.00_pm_r,14673._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.30_pm_r,17605._pm_r,1._pm_r,155._pm_r,.13_pm_r,301._pm_r,0._pm_r,346._pm_r,.09_pm_r,22._pm_r, &
       210.30_pm_r,20622._pm_r,1._pm_r,163._pm_r,.17_pm_r,303._pm_r,1._pm_r,356._pm_r,.12_pm_r,24._pm_r, &
       217.30_pm_r,23758._pm_r,1._pm_r,180._pm_r,.19_pm_r,303._pm_r,1._pm_r,4._pm_r,.12_pm_r,27._pm_r, &
       223.10_pm_r,26982._pm_r,0._pm_r,213._pm_r,.20_pm_r,307._pm_r,1._pm_r,10._pm_r,.12_pm_r,31._pm_r, &
       229.40_pm_r,30297._pm_r,0._pm_r,253._pm_r,.18_pm_r,312._pm_r,1._pm_r,16._pm_r,.09_pm_r,43._pm_r, &
       239.30_pm_r,33724._pm_r,1._pm_r,277._pm_r,.15_pm_r,323._pm_r,1._pm_r,21._pm_r,.06_pm_r,72._pm_r, &
       248.10_pm_r,37290._pm_r,1._pm_r,291._pm_r,.13_pm_r,342._pm_r,1._pm_r,28._pm_r,.07_pm_r,117._pm_r, &
       255.30_pm_r,40969._pm_r,1._pm_r,301._pm_r,.12_pm_r,0._pm_r,1._pm_r,36._pm_r,.09_pm_r,139._pm_r, &
       261.10_pm_r,44754._pm_r,1._pm_r,314._pm_r,.19_pm_r,52._pm_r,1._pm_r,45._pm_r,.06_pm_r,132._pm_r, &
       264.10_pm_r,48607._pm_r,1._pm_r,338._pm_r,.32_pm_r,83._pm_r,1._pm_r,51._pm_r,.02_pm_r,0._pm_r, &
       262.40_pm_r,52469._pm_r,1._pm_r,15._pm_r,.32_pm_r,104._pm_r,1._pm_r,51._pm_r,.05_pm_r,302._pm_r, &
       254.00_pm_r,56255._pm_r,1._pm_r,48._pm_r,.25_pm_r,168._pm_r,1._pm_r,48._pm_r,.05_pm_r,217._pm_r, &
       242.20_pm_r,59885._pm_r,0._pm_r,93._pm_r,.38_pm_r,220._pm_r,1._pm_r,46._pm_r,.16_pm_r,173._pm_r, &
       229.50_pm_r,63340._pm_r,1._pm_r,189._pm_r,.50_pm_r,230._pm_r,1._pm_r,46._pm_r,.23_pm_r,170._pm_r, &
       217.40_pm_r,66603._pm_r,1._pm_r,210._pm_r,.46_pm_r,226._pm_r,0._pm_r,52._pm_r,.23_pm_r,171._pm_r, &
       208.90_pm_r,69724._pm_r,2._pm_r,214._pm_r,.36_pm_r,212._pm_r,0._pm_r,75._pm_r,.19_pm_r,183._pm_r, &
       204.90_pm_r,72754._pm_r,2._pm_r,212._pm_r,.29_pm_r,183._pm_r,0._pm_r,150._pm_r,.16_pm_r,196._pm_r, &
       203.00_pm_r,75744._pm_r,3._pm_r,208._pm_r,.31_pm_r,158._pm_r,0._pm_r,186._pm_r,.14_pm_r,214._pm_r, &
       201.90_pm_r,78712._pm_r,3._pm_r,202._pm_r,.35_pm_r,142._pm_r,0._pm_r,196._pm_r,.14_pm_r,226._pm_r, &
       199.80_pm_r,81664._pm_r,4._pm_r,197._pm_r,.37_pm_r,133._pm_r,0._pm_r,202._pm_r,.13_pm_r,238._pm_r, &
       195.80_pm_r,84569._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       191.00_pm_r,87403._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.70_pm_r,90179._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,92944._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.30_pm_r,95718._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.90_pm_r,98515._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.70_pm_r,101360._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       198.60_pm_r,104320._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       218.70_pm_r,107530._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       265.80_pm_r,111309._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       350.30_pm_r,116243._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
       301.10_pm_r,-104._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       276.60_pm_r,4133._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.10_pm_r,8019._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.70_pm_r,11531._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.40_pm_r,14678._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       201.80_pm_r,17616._pm_r,2._pm_r,176._pm_r,.17_pm_r,279._pm_r,0._pm_r,144._pm_r,.09_pm_r,338._pm_r, &
       211.10_pm_r,20639._pm_r,2._pm_r,185._pm_r,.22_pm_r,281._pm_r,0._pm_r,130._pm_r,.11_pm_r,333._pm_r, &
       217.90_pm_r,23783._pm_r,2._pm_r,196._pm_r,.24_pm_r,282._pm_r,0._pm_r,12._pm_r,.11_pm_r,328._pm_r, &
       223.80_pm_r,27016._pm_r,2._pm_r,207._pm_r,.23_pm_r,285._pm_r,0._pm_r,337._pm_r,.10_pm_r,317._pm_r, &
       230.00_pm_r,30338._pm_r,2._pm_r,217._pm_r,.19_pm_r,290._pm_r,0._pm_r,327._pm_r,.07_pm_r,302._pm_r, &
       238.20_pm_r,33763._pm_r,2._pm_r,224._pm_r,.13_pm_r,302._pm_r,0._pm_r,321._pm_r,.04_pm_r,270._pm_r, &
       246.40_pm_r,37314._pm_r,2._pm_r,228._pm_r,.08_pm_r,332._pm_r,0._pm_r,314._pm_r,.04_pm_r,192._pm_r, &
       254.30_pm_r,40978._pm_r,2._pm_r,231._pm_r,.07_pm_r,25._pm_r,0._pm_r,309._pm_r,.06_pm_r,149._pm_r, &
       261.10_pm_r,44756._pm_r,2._pm_r,230._pm_r,.13_pm_r,74._pm_r,0._pm_r,311._pm_r,.10_pm_r,110._pm_r, &
       264.40_pm_r,48608._pm_r,2._pm_r,224._pm_r,.25_pm_r,105._pm_r,0._pm_r,0._pm_r,.16_pm_r,104._pm_r, &
       261.00_pm_r,52461._pm_r,2._pm_r,208._pm_r,.32_pm_r,127._pm_r,0._pm_r,92._pm_r,.17_pm_r,139._pm_r, &
       253.10_pm_r,56231._pm_r,2._pm_r,193._pm_r,.37_pm_r,160._pm_r,0._pm_r,136._pm_r,.27_pm_r,187._pm_r, &
       242.60_pm_r,59863._pm_r,2._pm_r,190._pm_r,.38_pm_r,192._pm_r,1._pm_r,169._pm_r,.31_pm_r,207._pm_r, &
       228.90_pm_r,63321._pm_r,3._pm_r,192._pm_r,.47_pm_r,210._pm_r,1._pm_r,184._pm_r,.23_pm_r,221._pm_r, &
       217.60_pm_r,66584._pm_r,4._pm_r,197._pm_r,.53_pm_r,215._pm_r,1._pm_r,192._pm_r,.06_pm_r,265._pm_r, &
       209.90_pm_r,69713._pm_r,4._pm_r,200._pm_r,.55_pm_r,214._pm_r,1._pm_r,193._pm_r,.19_pm_r,18._pm_r, &
       205.80_pm_r,72753._pm_r,5._pm_r,202._pm_r,.57_pm_r,211._pm_r,1._pm_r,185._pm_r,.40_pm_r,30._pm_r, &
       203.60_pm_r,75750._pm_r,6._pm_r,203._pm_r,.59_pm_r,207._pm_r,0._pm_r,102._pm_r,.55_pm_r,32._pm_r, &
       202.70_pm_r,78724._pm_r,7._pm_r,203._pm_r,.57_pm_r,204._pm_r,1._pm_r,49._pm_r,.65_pm_r,34._pm_r, &
       200.30_pm_r,81687._pm_r,8._pm_r,203._pm_r,.54_pm_r,203._pm_r,2._pm_r,42._pm_r,.69_pm_r,35._pm_r, &
       195.20_pm_r,84585._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       190.20_pm_r,87405._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.10_pm_r,90171._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.40_pm_r,92924._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,95686._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       188.20_pm_r,98480._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       192.80_pm_r,101344._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.50_pm_r,104349._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.70_pm_r,107637._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       272.70_pm_r,111519._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       355.70_pm_r,116550._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
       300.40_pm_r,-124._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.30_pm_r,4111._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       253.50_pm_r,8003._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       226.60_pm_r,11523._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.10_pm_r,14688._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       203.70_pm_r,17653._pm_r,2._pm_r,143._pm_r,.65_pm_r,257._pm_r,1._pm_r,152._pm_r,.27_pm_r,341._pm_r, &
       212.60_pm_r,20702._pm_r,2._pm_r,171._pm_r,.80_pm_r,258._pm_r,1._pm_r,145._pm_r,.35_pm_r,340._pm_r, &
       219.00_pm_r,23865._pm_r,3._pm_r,200._pm_r,.83_pm_r,258._pm_r,0._pm_r,90._pm_r,.39_pm_r,338._pm_r, &
       224.60_pm_r,27112._pm_r,3._pm_r,218._pm_r,.76_pm_r,259._pm_r,1._pm_r,353._pm_r,.40_pm_r,337._pm_r, &
       230.40_pm_r,30444._pm_r,4._pm_r,227._pm_r,.56_pm_r,261._pm_r,1._pm_r,344._pm_r,.35_pm_r,335._pm_r, &
       238.70_pm_r,33875._pm_r,5._pm_r,232._pm_r,.29_pm_r,262._pm_r,2._pm_r,341._pm_r,.26_pm_r,332._pm_r, &
       246.50_pm_r,37432._pm_r,5._pm_r,233._pm_r,.05_pm_r,236._pm_r,2._pm_r,339._pm_r,.16_pm_r,329._pm_r, &
       255.20_pm_r,41100._pm_r,5._pm_r,232._pm_r,.14_pm_r,111._pm_r,2._pm_r,338._pm_r,.07_pm_r,326._pm_r, &
       263.50_pm_r,44906._pm_r,5._pm_r,229._pm_r,.25_pm_r,131._pm_r,2._pm_r,338._pm_r,.02_pm_r,117._pm_r, &
       264.70_pm_r,48781._pm_r,5._pm_r,223._pm_r,.40_pm_r,148._pm_r,2._pm_r,340._pm_r,.14_pm_r,124._pm_r, &
       258.90_pm_r,52619._pm_r,5._pm_r,216._pm_r,.51_pm_r,156._pm_r,2._pm_r,344._pm_r,.17_pm_r,143._pm_r, &
       250.20_pm_r,56353._pm_r,5._pm_r,209._pm_r,.53_pm_r,164._pm_r,2._pm_r,343._pm_r,.20_pm_r,198._pm_r, &
       239.90_pm_r,59943._pm_r,6._pm_r,205._pm_r,.38_pm_r,177._pm_r,1._pm_r,331._pm_r,.26_pm_r,239._pm_r, &
       227.50_pm_r,63370._pm_r,6._pm_r,204._pm_r,.26_pm_r,205._pm_r,2._pm_r,315._pm_r,.28_pm_r,260._pm_r, &
       216.60_pm_r,66615._pm_r,7._pm_r,205._pm_r,.22_pm_r,244._pm_r,2._pm_r,307._pm_r,.22_pm_r,289._pm_r, &
       209.70_pm_r,69734._pm_r,7._pm_r,207._pm_r,.25_pm_r,275._pm_r,2._pm_r,308._pm_r,.20_pm_r,340._pm_r, &
       206.70_pm_r,72781._pm_r,7._pm_r,210._pm_r,.29_pm_r,290._pm_r,2._pm_r,315._pm_r,.28_pm_r,14._pm_r, &
       204.70_pm_r,75793._pm_r,7._pm_r,214._pm_r,.33_pm_r,297._pm_r,3._pm_r,326._pm_r,.39_pm_r,29._pm_r, &
       202.70_pm_r,78776._pm_r,7._pm_r,218._pm_r,.34_pm_r,301._pm_r,3._pm_r,338._pm_r,.46_pm_r,34._pm_r, &
       198.90_pm_r,81730._pm_r,7._pm_r,222._pm_r,.34_pm_r,306._pm_r,3._pm_r,348._pm_r,.48_pm_r,38._pm_r, &
       192.80_pm_r,84598._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.40_pm_r,87377._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.00_pm_r,90099._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       183.30_pm_r,92806._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.60_pm_r,95527._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       187.80_pm_r,98300._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       194.90_pm_r,101175._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       207.40_pm_r,104233._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       232.40_pm_r,107618._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       281.00_pm_r,111628._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       361.40_pm_r,116766._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
       298.50_pm_r,-116._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       277.10_pm_r,4101._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       252.90_pm_r,7984._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.10_pm_r,11501._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       208.20_pm_r,14690._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       206.90_pm_r,17706._pm_r,2._pm_r,139._pm_r,.88_pm_r,253._pm_r,2._pm_r,157._pm_r,.35_pm_r,336._pm_r, &
       214.60_pm_r,20792._pm_r,2._pm_r,182._pm_r,1.08_pm_r,253._pm_r,1._pm_r,157._pm_r,.47_pm_r,337._pm_r, &
       220.50_pm_r,23979._pm_r,3._pm_r,213._pm_r,1.11_pm_r,252._pm_r,0._pm_r,161._pm_r,.54_pm_r,336._pm_r, &
       226.40_pm_r,27249._pm_r,4._pm_r,227._pm_r,1.02_pm_r,252._pm_r,1._pm_r,334._pm_r,.57_pm_r,336._pm_r, &
       232.00_pm_r,30607._pm_r,6._pm_r,232._pm_r,.76_pm_r,249._pm_r,1._pm_r,335._pm_r,.54_pm_r,335._pm_r, &
       240.20_pm_r,34060._pm_r,6._pm_r,234._pm_r,.43_pm_r,240._pm_r,2._pm_r,334._pm_r,.44_pm_r,333._pm_r, &
       249.00_pm_r,37644._pm_r,7._pm_r,234._pm_r,.20_pm_r,202._pm_r,3._pm_r,334._pm_r,.33_pm_r,329._pm_r, &
       258.30_pm_r,41356._pm_r,7._pm_r,232._pm_r,.23_pm_r,150._pm_r,3._pm_r,333._pm_r,.21_pm_r,323._pm_r, &
       265.00_pm_r,45195._pm_r,7._pm_r,228._pm_r,.34_pm_r,159._pm_r,3._pm_r,332._pm_r,.08_pm_r,279._pm_r, &
       265.20_pm_r,49085._pm_r,7._pm_r,224._pm_r,.47_pm_r,171._pm_r,3._pm_r,329._pm_r,.14_pm_r,212._pm_r, &
       258.60_pm_r,52924._pm_r,8._pm_r,220._pm_r,.55_pm_r,185._pm_r,3._pm_r,325._pm_r,.23_pm_r,225._pm_r, &
       248.90_pm_r,56645._pm_r,9._pm_r,218._pm_r,.55_pm_r,200._pm_r,3._pm_r,318._pm_r,.35_pm_r,258._pm_r, &
       237.80_pm_r,60210._pm_r,9._pm_r,217._pm_r,.46_pm_r,224._pm_r,4._pm_r,311._pm_r,.42_pm_r,281._pm_r, &
       225.40_pm_r,63605._pm_r,10._pm_r,218._pm_r,.37_pm_r,257._pm_r,4._pm_r,308._pm_r,.44_pm_r,297._pm_r, &
       215.40_pm_r,66826._pm_r,10._pm_r,221._pm_r,.37_pm_r,295._pm_r,5._pm_r,307._pm_r,.39_pm_r,311._pm_r, &
       209.00_pm_r,69932._pm_r,10._pm_r,224._pm_r,.45_pm_r,323._pm_r,5._pm_r,309._pm_r,.31_pm_r,329._pm_r, &
       205.70_pm_r,72967._pm_r,10._pm_r,228._pm_r,.56_pm_r,340._pm_r,6._pm_r,311._pm_r,.28_pm_r,352._pm_r, &
       202.40_pm_r,75958._pm_r,10._pm_r,233._pm_r,.65_pm_r,347._pm_r,6._pm_r,314._pm_r,.27_pm_r,12._pm_r, &
       198.60_pm_r,78895._pm_r,9._pm_r,238._pm_r,.69_pm_r,352._pm_r,6._pm_r,317._pm_r,.27_pm_r,27._pm_r, &
       193.50_pm_r,81778._pm_r,9._pm_r,244._pm_r,.67_pm_r,354._pm_r,6._pm_r,321._pm_r,.27_pm_r,34._pm_r, &
       186.90_pm_r,84560._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       181.30_pm_r,87249._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       178.00_pm_r,89879._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       178.00_pm_r,92499._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       180.80_pm_r,95151._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       186.70_pm_r,97884._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       197.00_pm_r,100764._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.40_pm_r,103878._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       241.40_pm_r,107379._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       289.60_pm_r,111531._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       366.70_pm_r,116771._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
       293.30_pm_r,-66._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       274.10_pm_r,4087._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       249.00_pm_r,7916._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.90_pm_r,11393._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       213.50_pm_r,14609._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       212.80_pm_r,17716._pm_r,2._pm_r,196._pm_r,.59_pm_r,257._pm_r,2._pm_r,170._pm_r,.28_pm_r,358._pm_r, &
       217.80_pm_r,20868._pm_r,3._pm_r,214._pm_r,.75_pm_r,257._pm_r,1._pm_r,166._pm_r,.38_pm_r,357._pm_r, &
       222.40_pm_r,24091._pm_r,4._pm_r,226._pm_r,.78_pm_r,256._pm_r,1._pm_r,155._pm_r,.45_pm_r,356._pm_r, &
       228.00_pm_r,27386._pm_r,5._pm_r,232._pm_r,.71_pm_r,253._pm_r,0._pm_r,36._pm_r,.48_pm_r,353._pm_r, &
       234.60_pm_r,30773._pm_r,6._pm_r,235._pm_r,.53_pm_r,247._pm_r,1._pm_r,3._pm_r,.46_pm_r,349._pm_r, &
       243.10_pm_r,34268._pm_r,6._pm_r,236._pm_r,.33_pm_r,230._pm_r,2._pm_r,357._pm_r,.39_pm_r,345._pm_r, &
       252.80_pm_r,37901._pm_r,7._pm_r,234._pm_r,.23_pm_r,195._pm_r,2._pm_r,353._pm_r,.31_pm_r,336._pm_r, &
       262.40_pm_r,41672._pm_r,7._pm_r,232._pm_r,.25_pm_r,168._pm_r,2._pm_r,349._pm_r,.24_pm_r,324._pm_r, &
       268.40_pm_r,45567._pm_r,7._pm_r,229._pm_r,.38_pm_r,183._pm_r,3._pm_r,345._pm_r,.15_pm_r,298._pm_r, &
       267.40_pm_r,49498._pm_r,8._pm_r,226._pm_r,.53_pm_r,200._pm_r,3._pm_r,342._pm_r,.13_pm_r,279._pm_r, &
       259.90_pm_r,53362._pm_r,8._pm_r,224._pm_r,.60_pm_r,210._pm_r,3._pm_r,338._pm_r,.15_pm_r,285._pm_r, &
       249.80_pm_r,57099._pm_r,9._pm_r,223._pm_r,.47_pm_r,225._pm_r,3._pm_r,335._pm_r,.23_pm_r,310._pm_r, &
       238.30_pm_r,60672._pm_r,10._pm_r,224._pm_r,.29_pm_r,259._pm_r,3._pm_r,333._pm_r,.31_pm_r,316._pm_r, &
       225.80_pm_r,64074._pm_r,10._pm_r,226._pm_r,.33_pm_r,310._pm_r,4._pm_r,331._pm_r,.33_pm_r,316._pm_r, &
       215.80_pm_r,67302._pm_r,10._pm_r,230._pm_r,.48_pm_r,328._pm_r,4._pm_r,329._pm_r,.32_pm_r,309._pm_r, &
       208.20_pm_r,70407._pm_r,10._pm_r,234._pm_r,.59_pm_r,333._pm_r,5._pm_r,326._pm_r,.28_pm_r,300._pm_r, &
       202.20_pm_r,73411._pm_r,10._pm_r,240._pm_r,.67_pm_r,336._pm_r,5._pm_r,324._pm_r,.24_pm_r,292._pm_r, &
       195.80_pm_r,76328._pm_r,10._pm_r,246._pm_r,.69_pm_r,335._pm_r,5._pm_r,322._pm_r,.22_pm_r,283._pm_r, &
       189.10_pm_r,79143._pm_r,10._pm_r,252._pm_r,.68_pm_r,336._pm_r,6._pm_r,320._pm_r,.20_pm_r,276._pm_r, &
       182.60_pm_r,81865._pm_r,10._pm_r,257._pm_r,.64_pm_r,336._pm_r,6._pm_r,318._pm_r,.18_pm_r,273._pm_r, &
       176.50_pm_r,84492._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       172.00_pm_r,87036._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       169.50_pm_r,89532._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       170.80_pm_r,92035._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       176.00_pm_r,94595._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       184.90_pm_r,97275._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       199.00_pm_r,100153._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.10_pm_r,103330._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       251.50_pm_r,106960._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       297.60_pm_r,111258._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       371.30_pm_r,116583._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
       285.50_pm_r,-25._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       269.30_pm_r,4033._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       244.00_pm_r,7787._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.50_pm_r,11213._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       219.90_pm_r,14463._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       220.20_pm_r,17682._pm_r,3._pm_r,217._pm_r,.24_pm_r,283._pm_r,1._pm_r,205._pm_r,.26_pm_r,62._pm_r, &
       222.30_pm_r,20920._pm_r,3._pm_r,223._pm_r,.32_pm_r,280._pm_r,1._pm_r,185._pm_r,.36_pm_r,59._pm_r, &
       224.90_pm_r,24192._pm_r,3._pm_r,231._pm_r,.37_pm_r,275._pm_r,1._pm_r,137._pm_r,.43_pm_r,57._pm_r, &
       230.20_pm_r,27520._pm_r,4._pm_r,236._pm_r,.38_pm_r,268._pm_r,1._pm_r,96._pm_r,.44_pm_r,53._pm_r, &
       238.40_pm_r,30951._pm_r,4._pm_r,239._pm_r,.34_pm_r,257._pm_r,1._pm_r,79._pm_r,.37_pm_r,47._pm_r, &
       247.50_pm_r,34506._pm_r,5._pm_r,240._pm_r,.30_pm_r,240._pm_r,2._pm_r,70._pm_r,.28_pm_r,35._pm_r, &
       257.50_pm_r,38206._pm_r,5._pm_r,240._pm_r,.27_pm_r,221._pm_r,2._pm_r,64._pm_r,.18_pm_r,12._pm_r, &
       266.70_pm_r,42044._pm_r,6._pm_r,238._pm_r,.27_pm_r,204._pm_r,2._pm_r,58._pm_r,.14_pm_r,331._pm_r, &
       271.80_pm_r,45995._pm_r,6._pm_r,235._pm_r,.36_pm_r,203._pm_r,2._pm_r,54._pm_r,.11_pm_r,288._pm_r, &
       270.80_pm_r,49975._pm_r,7._pm_r,233._pm_r,.49_pm_r,210._pm_r,2._pm_r,51._pm_r,.09_pm_r,264._pm_r, &
       263.70_pm_r,53891._pm_r,7._pm_r,231._pm_r,.51_pm_r,217._pm_r,2._pm_r,49._pm_r,.08_pm_r,303._pm_r, &
       253.80_pm_r,57685._pm_r,8._pm_r,230._pm_r,.41_pm_r,226._pm_r,2._pm_r,43._pm_r,.20_pm_r,340._pm_r, &
       242.00_pm_r,61314._pm_r,8._pm_r,230._pm_r,.24_pm_r,243._pm_r,2._pm_r,34._pm_r,.31_pm_r,334._pm_r, &
       229.40_pm_r,64768._pm_r,9._pm_r,231._pm_r,.17_pm_r,270._pm_r,2._pm_r,24._pm_r,.34_pm_r,327._pm_r, &
       218.20_pm_r,68041._pm_r,9._pm_r,232._pm_r,.20_pm_r,282._pm_r,3._pm_r,15._pm_r,.30_pm_r,318._pm_r, &
       207.40_pm_r,71161._pm_r,9._pm_r,234._pm_r,.22_pm_r,279._pm_r,3._pm_r,8._pm_r,.23_pm_r,305._pm_r, &
       196.60_pm_r,74117._pm_r,9._pm_r,235._pm_r,.25_pm_r,273._pm_r,3._pm_r,3._pm_r,.18_pm_r,286._pm_r, &
       185.90_pm_r,76922._pm_r,10._pm_r,236._pm_r,.26_pm_r,268._pm_r,3._pm_r,358._pm_r,.15_pm_r,266._pm_r, &
       176.40_pm_r,79567._pm_r,10._pm_r,237._pm_r,.25_pm_r,264._pm_r,3._pm_r,354._pm_r,.14_pm_r,253._pm_r, &
       168.70_pm_r,82084._pm_r,10._pm_r,238._pm_r,.24_pm_r,263._pm_r,3._pm_r,351._pm_r,.13_pm_r,242._pm_r, &
       163.80_pm_r,84515._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       161.00_pm_r,86884._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       159.90_pm_r,89226._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       163.10_pm_r,91599._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       170.60_pm_r,94062._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       182.80_pm_r,96682._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       200.90_pm_r,99556._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.50_pm_r,102802._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       262.20_pm_r,106573._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       303.80_pm_r,111010._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       374.90_pm_r,116394._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
       283.80_pm_r,-38._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       265.80_pm_r,3978._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       240.40_pm_r,7677._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.30_pm_r,11080._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       224.40_pm_r,14366._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       225.50_pm_r,17660._pm_r,3._pm_r,190._pm_r,.45_pm_r,318._pm_r,0._pm_r,166._pm_r,.10_pm_r,90._pm_r, &
       226.60_pm_r,20970._pm_r,2._pm_r,206._pm_r,.61_pm_r,316._pm_r,0._pm_r,136._pm_r,.14_pm_r,94._pm_r, &
       227.80_pm_r,24295._pm_r,2._pm_r,230._pm_r,.67_pm_r,313._pm_r,1._pm_r,120._pm_r,.15_pm_r,96._pm_r, &
       232.50_pm_r,27660._pm_r,3._pm_r,252._pm_r,.61_pm_r,308._pm_r,1._pm_r,114._pm_r,.14_pm_r,102._pm_r, &
       241.00_pm_r,31126._pm_r,3._pm_r,264._pm_r,.44_pm_r,298._pm_r,1._pm_r,112._pm_r,.12_pm_r,110._pm_r, &
       251.30_pm_r,34727._pm_r,4._pm_r,267._pm_r,.27_pm_r,272._pm_r,1._pm_r,113._pm_r,.08_pm_r,130._pm_r, &
       262.10_pm_r,38490._pm_r,4._pm_r,266._pm_r,.21_pm_r,225._pm_r,1._pm_r,115._pm_r,.05_pm_r,158._pm_r, &
       271.60_pm_r,42397._pm_r,4._pm_r,262._pm_r,.26_pm_r,191._pm_r,1._pm_r,118._pm_r,.05_pm_r,191._pm_r, &
       276.30_pm_r,46419._pm_r,4._pm_r,256._pm_r,.27_pm_r,178._pm_r,1._pm_r,121._pm_r,.03_pm_r,189._pm_r, &
       274.80_pm_r,50461._pm_r,4._pm_r,252._pm_r,.18_pm_r,171._pm_r,1._pm_r,121._pm_r,.01_pm_r,90._pm_r, &
       268.70_pm_r,54443._pm_r,4._pm_r,249._pm_r,.11_pm_r,234._pm_r,1._pm_r,120._pm_r,.03_pm_r,39._pm_r, &
       258.70_pm_r,58311._pm_r,5._pm_r,250._pm_r,.33_pm_r,270._pm_r,1._pm_r,118._pm_r,.02_pm_r,154._pm_r, &
       246.40_pm_r,62007._pm_r,5._pm_r,252._pm_r,.45_pm_r,264._pm_r,1._pm_r,123._pm_r,.12_pm_r,207._pm_r, &
       233.30_pm_r,65524._pm_r,6._pm_r,253._pm_r,.43_pm_r,255._pm_r,1._pm_r,134._pm_r,.22_pm_r,210._pm_r, &
       220.00_pm_r,68841._pm_r,6._pm_r,253._pm_r,.34_pm_r,241._pm_r,1._pm_r,150._pm_r,.30_pm_r,213._pm_r, &
       205.90_pm_r,71965._pm_r,7._pm_r,251._pm_r,.24_pm_r,215._pm_r,2._pm_r,164._pm_r,.32_pm_r,214._pm_r, &
       191.50_pm_r,74870._pm_r,7._pm_r,249._pm_r,.22_pm_r,180._pm_r,2._pm_r,174._pm_r,.31_pm_r,216._pm_r, &
       177.70_pm_r,77577._pm_r,7._pm_r,246._pm_r,.23_pm_r,157._pm_r,2._pm_r,181._pm_r,.28_pm_r,215._pm_r, &
       166.00_pm_r,80084._pm_r,7._pm_r,244._pm_r,.23_pm_r,146._pm_r,3._pm_r,186._pm_r,.25_pm_r,216._pm_r, &
       156.90_pm_r,82433._pm_r,7._pm_r,241._pm_r,.22_pm_r,141._pm_r,3._pm_r,190._pm_r,.22_pm_r,219._pm_r, &
       151.80_pm_r,84679._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       150.20_pm_r,86876._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       150.90_pm_r,89073._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       155.90_pm_r,91324._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       165.40_pm_r,93691._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       180.60_pm_r,96255._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       202.90_pm_r,99122._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.10_pm_r,102439._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       272.80_pm_r,106353._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       307.70_pm_r,110910._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       377.50_pm_r,116326._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
       279.80_pm_r,-31._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       263.00_pm_r,3933._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       237.60_pm_r,7588._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.40_pm_r,10983._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       227.90_pm_r,14308._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.10_pm_r,17654._pm_r,2._pm_r,139._pm_r,.40_pm_r,287._pm_r,0._pm_r,65._pm_r,.02_pm_r,76._pm_r, &
       230.00_pm_r,21016._pm_r,1._pm_r,158._pm_r,.55_pm_r,285._pm_r,0._pm_r,68._pm_r,.02_pm_r,104._pm_r, &
       230.50_pm_r,24384._pm_r,1._pm_r,206._pm_r,.63_pm_r,281._pm_r,0._pm_r,74._pm_r,.03_pm_r,135._pm_r, &
       234.90_pm_r,27785._pm_r,2._pm_r,241._pm_r,.60_pm_r,275._pm_r,0._pm_r,85._pm_r,.05_pm_r,169._pm_r, &
       243.50_pm_r,31288._pm_r,2._pm_r,251._pm_r,.50_pm_r,266._pm_r,0._pm_r,101._pm_r,.07_pm_r,182._pm_r, &
       254.30_pm_r,34927._pm_r,3._pm_r,253._pm_r,.36_pm_r,251._pm_r,0._pm_r,119._pm_r,.08_pm_r,188._pm_r, &
       265.60_pm_r,38739._pm_r,3._pm_r,251._pm_r,.26_pm_r,227._pm_r,0._pm_r,135._pm_r,.08_pm_r,189._pm_r, &
       275.40_pm_r,42700._pm_r,4._pm_r,248._pm_r,.22_pm_r,202._pm_r,1._pm_r,146._pm_r,.07_pm_r,189._pm_r, &
       280.50_pm_r,46779._pm_r,4._pm_r,245._pm_r,.23_pm_r,205._pm_r,1._pm_r,153._pm_r,.08_pm_r,189._pm_r, &
       279.70_pm_r,50886._pm_r,4._pm_r,242._pm_r,.30_pm_r,222._pm_r,1._pm_r,162._pm_r,.13_pm_r,203._pm_r, &
       273.10_pm_r,54936._pm_r,5._pm_r,241._pm_r,.35_pm_r,243._pm_r,1._pm_r,172._pm_r,.14_pm_r,212._pm_r, &
       263.10_pm_r,58867._pm_r,5._pm_r,242._pm_r,.31_pm_r,268._pm_r,1._pm_r,180._pm_r,.15_pm_r,218._pm_r, &
       250.20_pm_r,62623._pm_r,5._pm_r,245._pm_r,.16_pm_r,312._pm_r,1._pm_r,185._pm_r,.11_pm_r,205._pm_r, &
       236.00_pm_r,66188._pm_r,5._pm_r,246._pm_r,.12_pm_r,50._pm_r,1._pm_r,187._pm_r,.12_pm_r,187._pm_r, &
       220.70_pm_r,69529._pm_r,5._pm_r,245._pm_r,.28_pm_r,103._pm_r,2._pm_r,186._pm_r,.17_pm_r,179._pm_r, &
       204.90_pm_r,72651._pm_r,5._pm_r,240._pm_r,.46_pm_r,120._pm_r,2._pm_r,185._pm_r,.21_pm_r,180._pm_r, &
       189.20_pm_r,75532._pm_r,4._pm_r,231._pm_r,.56_pm_r,128._pm_r,2._pm_r,184._pm_r,.23_pm_r,181._pm_r, &
       174.00_pm_r,78195._pm_r,4._pm_r,220._pm_r,.60_pm_r,133._pm_r,3._pm_r,184._pm_r,.24_pm_r,181._pm_r, &
       161.40_pm_r,80639._pm_r,5._pm_r,209._pm_r,.57_pm_r,136._pm_r,3._pm_r,184._pm_r,.22_pm_r,182._pm_r, &
       150.50_pm_r,82915._pm_r,5._pm_r,200._pm_r,.49_pm_r,137._pm_r,3._pm_r,183._pm_r,.19_pm_r,182._pm_r, &
       143.20_pm_r,85030._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       141.40_pm_r,87088._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       143.50_pm_r,89166._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       149.90_pm_r,91317._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       160.90_pm_r,93607._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       178.60_pm_r,96120._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       204.70_pm_r,98980._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       242.20_pm_r,102362._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       282.10_pm_r,106407._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       309.50_pm_r,111054._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       379.30_pm_r,116482._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/) 
  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
       274.30_pm_r,-12._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       260.40_pm_r,3891._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       235.90_pm_r,7513._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       229.60_pm_r,10910._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       230.70_pm_r,14270._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       231.60_pm_r,17655._pm_r,1._pm_r,85._pm_r,.24_pm_r,244._pm_r,0._pm_r,67._pm_r,.08_pm_r,292._pm_r, &
       232.50_pm_r,21053._pm_r,1._pm_r,93._pm_r,.35_pm_r,250._pm_r,0._pm_r,47._pm_r,.10_pm_r,294._pm_r, &
       233.00_pm_r,24459._pm_r,0._pm_r,124._pm_r,.43_pm_r,255._pm_r,0._pm_r,18._pm_r,.10_pm_r,291._pm_r, &
       236.80_pm_r,27894._pm_r,1._pm_r,222._pm_r,.47_pm_r,263._pm_r,0._pm_r,353._pm_r,.07_pm_r,278._pm_r, &
       244.60_pm_r,31420._pm_r,1._pm_r,249._pm_r,.46_pm_r,271._pm_r,0._pm_r,340._pm_r,.03_pm_r,243._pm_r, &
       255.40_pm_r,35075._pm_r,2._pm_r,259._pm_r,.40_pm_r,280._pm_r,0._pm_r,335._pm_r,.04_pm_r,166._pm_r, &
       267.20_pm_r,38906._pm_r,2._pm_r,265._pm_r,.31_pm_r,289._pm_r,0._pm_r,334._pm_r,.07_pm_r,152._pm_r, &
       277.50_pm_r,42893._pm_r,3._pm_r,269._pm_r,.20_pm_r,297._pm_r,0._pm_r,345._pm_r,.09_pm_r,148._pm_r, &
       283.80_pm_r,47011._pm_r,3._pm_r,270._pm_r,.08_pm_r,266._pm_r,0._pm_r,177._pm_r,.07_pm_r,184._pm_r, &
       283.70_pm_r,51174._pm_r,3._pm_r,269._pm_r,.09_pm_r,212._pm_r,0._pm_r,187._pm_r,.04_pm_r,207._pm_r, &
       276.80_pm_r,55282._pm_r,3._pm_r,266._pm_r,.09_pm_r,214._pm_r,0._pm_r,212._pm_r,.07_pm_r,287._pm_r, &
       265.80_pm_r,59261._pm_r,3._pm_r,266._pm_r,.06_pm_r,329._pm_r,0._pm_r,264._pm_r,.19_pm_r,317._pm_r, &
       252.20_pm_r,63052._pm_r,3._pm_r,268._pm_r,.12_pm_r,5._pm_r,1._pm_r,299._pm_r,.21_pm_r,333._pm_r, &
       237.30_pm_r,66643._pm_r,3._pm_r,272._pm_r,.13_pm_r,4._pm_r,1._pm_r,316._pm_r,.17_pm_r,3._pm_r, &
       221.20_pm_r,69996._pm_r,3._pm_r,275._pm_r,.08_pm_r,342._pm_r,1._pm_r,334._pm_r,.19_pm_r,60._pm_r, &
       204.70_pm_r,73121._pm_r,3._pm_r,276._pm_r,.07_pm_r,278._pm_r,1._pm_r,1._pm_r,.30_pm_r,90._pm_r, &
       188.30_pm_r,75994._pm_r,3._pm_r,275._pm_r,.11_pm_r,236._pm_r,1._pm_r,36._pm_r,.38_pm_r,99._pm_r, &
       172.70_pm_r,78642._pm_r,3._pm_r,273._pm_r,.13_pm_r,228._pm_r,1._pm_r,62._pm_r,.41_pm_r,103._pm_r, &
       159.50_pm_r,81062._pm_r,3._pm_r,270._pm_r,.14_pm_r,226._pm_r,2._pm_r,75._pm_r,.37_pm_r,107._pm_r, &
       147.20_pm_r,83308._pm_r,4._pm_r,268._pm_r,.13_pm_r,225._pm_r,2._pm_r,82._pm_r,.33_pm_r,110._pm_r, &
       137.90_pm_r,85345._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       135.80_pm_r,87313._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       138.70_pm_r,89314._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       145.90_pm_r,91401._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       157.80_pm_r,93637._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       177.10_pm_r,96115._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       205.90_pm_r,98969._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       247.30_pm_r,102395._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       288.70_pm_r,106535._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       310.00_pm_r,111239._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r, &
       380.40_pm_r,116668._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r,0._pm_r,0._pm_r,.00_pm_r,0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_juillet = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_juillet.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_juillet,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_juillet
