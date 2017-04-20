subroutine mpi_atmi_novembre (tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2, retour )

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But: Lecture du fichier ATMospherique pour l'Interplation correspondant au mois de NOVEMBRE 
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
264.20_pm_r,  -388._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
246.40_pm_r,  3339._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
223.40_pm_r,  6768._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
214.80_pm_r,  9966._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
221.80_pm_r, 13153._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
228.90_pm_r, 16458._pm_r,   8._pm_r,234._pm_r, 2.84_pm_r,135._pm_r,   2._pm_r,212._pm_r,  .12_pm_r,332._pm_r, &
233.70_pm_r, 19846._pm_r,   9._pm_r,202._pm_r, 3.32_pm_r,131._pm_r,   2._pm_r,218._pm_r,  .14_pm_r,326._pm_r, &
235.80_pm_r, 23289._pm_r,  11._pm_r,178._pm_r, 2.63_pm_r,123._pm_r,   2._pm_r,226._pm_r,  .12_pm_r,326._pm_r, &
236.60_pm_r, 26742._pm_r,  12._pm_r,166._pm_r, 1.21_pm_r, 88._pm_r,   2._pm_r,231._pm_r,  .05_pm_r,323._pm_r, &
248.10_pm_r, 30284._pm_r,  11._pm_r,160._pm_r, 1.66_pm_r,  2._pm_r,   2._pm_r,231._pm_r,  .04_pm_r,187._pm_r, &
260.50_pm_r, 34010._pm_r,   8._pm_r,156._pm_r, 3.14_pm_r,342._pm_r,   2._pm_r,228._pm_r,  .11_pm_r,175._pm_r, &
272.50_pm_r, 37915._pm_r,   3._pm_r,152._pm_r, 3.94_pm_r,335._pm_r,   2._pm_r,222._pm_r,  .17_pm_r,175._pm_r, &
282.10_pm_r, 41979._pm_r,   3._pm_r,334._pm_r, 3.89_pm_r,330._pm_r,   2._pm_r,216._pm_r,  .19_pm_r,174._pm_r, &
284.90_pm_r, 46136._pm_r,   8._pm_r,330._pm_r, 2.56_pm_r,324._pm_r,   2._pm_r,213._pm_r,  .19_pm_r,201._pm_r, &
283.20_pm_r, 50303._pm_r,  11._pm_r,327._pm_r, 1.18_pm_r,311._pm_r,   2._pm_r,213._pm_r,  .21_pm_r,219._pm_r, &
274.50_pm_r, 54391._pm_r,  12._pm_r,325._pm_r,  .49_pm_r,267._pm_r,   3._pm_r,214._pm_r,  .17_pm_r,224._pm_r, &
263.90_pm_r, 58336._pm_r,  12._pm_r,322._pm_r,  .47_pm_r,268._pm_r,   3._pm_r,215._pm_r,  .13_pm_r,225._pm_r, &
250.00_pm_r, 62096._pm_r,  12._pm_r,319._pm_r,  .46_pm_r,241._pm_r,   3._pm_r,215._pm_r,  .06_pm_r,209._pm_r, &
236.80_pm_r, 65661._pm_r,  12._pm_r,316._pm_r,  .45_pm_r,220._pm_r,   3._pm_r,214._pm_r,  .03_pm_r,162._pm_r, &
227.40_pm_r, 69058._pm_r,  12._pm_r,313._pm_r,  .42_pm_r,197._pm_r,   3._pm_r,213._pm_r,  .04_pm_r,144._pm_r, &
216.00_pm_r, 72313._pm_r,  12._pm_r,310._pm_r,  .41_pm_r,174._pm_r,   3._pm_r,212._pm_r,  .05_pm_r,169._pm_r, &
201.60_pm_r, 75368._pm_r,  11._pm_r,309._pm_r,  .43_pm_r,157._pm_r,   3._pm_r,211._pm_r,  .08_pm_r,176._pm_r, &
187.30_pm_r, 78219._pm_r,  11._pm_r,307._pm_r,  .42_pm_r,144._pm_r,   3._pm_r,210._pm_r,  .09_pm_r,180._pm_r, &
174.60_pm_r, 80860._pm_r,  10._pm_r,307._pm_r,  .41_pm_r,135._pm_r,   4._pm_r,209._pm_r,  .10_pm_r,183._pm_r, &
160.30_pm_r, 83329._pm_r,  10._pm_r,306._pm_r,  .36_pm_r,132._pm_r,   4._pm_r,208._pm_r,  .08_pm_r,187._pm_r, &
146.80_pm_r, 85507._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
142.70_pm_r, 87577._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
145.70_pm_r, 89679._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
153.50_pm_r, 91870._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
166.30_pm_r, 94219._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.80_pm_r, 96823._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
215.90_pm_r, 99823._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
254.90_pm_r,103387._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
289.30_pm_r,107591._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
319.50_pm_r,112337._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
410.90_pm_r,118142._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_70= (/ &
269.60_pm_r,  -235._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
247.70_pm_r,  3542._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.10_pm_r,  6995._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
216.50_pm_r, 10219._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
222.40_pm_r, 13424._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
228.00_pm_r, 16726._pm_r,  15._pm_r,233._pm_r, 4.73_pm_r,119._pm_r,   3._pm_r,212._pm_r,  .21_pm_r,  1._pm_r, &
232.00_pm_r, 20094._pm_r,  13._pm_r,201._pm_r, 5.60_pm_r,115._pm_r,   3._pm_r,215._pm_r,  .27_pm_r, 13._pm_r, &
234.40_pm_r, 23512._pm_r,  15._pm_r,171._pm_r, 4.71_pm_r,106._pm_r,   2._pm_r,217._pm_r,  .32_pm_r, 36._pm_r, &
235.90_pm_r, 26951._pm_r,  18._pm_r,154._pm_r, 2.67_pm_r, 77._pm_r,   2._pm_r,214._pm_r,  .38_pm_r, 61._pm_r, &
245.60_pm_r, 30470._pm_r,  16._pm_r,144._pm_r, 2.73_pm_r,  3._pm_r,   1._pm_r,199._pm_r,  .45_pm_r, 82._pm_r, &
257.60_pm_r, 34153._pm_r,  12._pm_r,134._pm_r, 4.89_pm_r,334._pm_r,   1._pm_r,169._pm_r,  .52_pm_r, 97._pm_r, &
269.30_pm_r, 38016._pm_r,   4._pm_r,103._pm_r, 6.23_pm_r,324._pm_r,   2._pm_r,146._pm_r,  .54_pm_r,107._pm_r, &
278.80_pm_r, 42032._pm_r,   7._pm_r,343._pm_r, 6.26_pm_r,318._pm_r,   3._pm_r,135._pm_r,  .50_pm_r,113._pm_r, &
281.90_pm_r, 46145._pm_r,  14._pm_r,329._pm_r, 4.21_pm_r,315._pm_r,   3._pm_r,132._pm_r,  .24_pm_r,124._pm_r, &
279.80_pm_r, 50265._pm_r,  18._pm_r,325._pm_r, 2.04_pm_r,308._pm_r,   3._pm_r,133._pm_r,  .09_pm_r,190._pm_r, &
271.20_pm_r, 54302._pm_r,  20._pm_r,323._pm_r,  .68_pm_r,277._pm_r,   3._pm_r,135._pm_r,  .12_pm_r,236._pm_r, &
260.60_pm_r, 58200._pm_r,  20._pm_r,321._pm_r,  .57_pm_r,256._pm_r,   3._pm_r,138._pm_r,  .09_pm_r,202._pm_r, &
247.90_pm_r, 61920._pm_r,  20._pm_r,318._pm_r,  .67_pm_r,213._pm_r,   3._pm_r,139._pm_r,  .13_pm_r,122._pm_r, &
236.10_pm_r, 65465._pm_r,  20._pm_r,316._pm_r,  .76_pm_r,196._pm_r,   4._pm_r,136._pm_r,  .24_pm_r, 95._pm_r, &
227.10_pm_r, 68855._pm_r,  19._pm_r,313._pm_r,  .68_pm_r,189._pm_r,   4._pm_r,131._pm_r,  .33_pm_r, 82._pm_r, &
216.90_pm_r, 72112._pm_r,  19._pm_r,311._pm_r,  .52_pm_r,185._pm_r,   4._pm_r,126._pm_r,  .39_pm_r, 75._pm_r, &
204.00_pm_r, 75192._pm_r,  19._pm_r,309._pm_r,  .32_pm_r,182._pm_r,   5._pm_r,120._pm_r,  .41_pm_r, 70._pm_r, &
190.20_pm_r, 78082._pm_r,  18._pm_r,309._pm_r,  .18_pm_r,175._pm_r,   5._pm_r,114._pm_r,  .39_pm_r, 67._pm_r, &
177.60_pm_r, 80767._pm_r,  18._pm_r,308._pm_r,  .07_pm_r,170._pm_r,   5._pm_r,110._pm_r,  .36_pm_r, 62._pm_r, &
163.90_pm_r, 83279._pm_r,  18._pm_r,308._pm_r,  .01_pm_r, 90._pm_r,   6._pm_r,106._pm_r,  .30_pm_r, 61._pm_r, &
151.50_pm_r, 85529._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
147.40_pm_r, 87676._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
149.70_pm_r, 89843._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
156.60_pm_r, 92087._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
168.30_pm_r, 94476._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.20_pm_r, 97101._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
213.80_pm_r,100089._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
249.80_pm_r,103599._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
285.40_pm_r,107729._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
320.50_pm_r,112458._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
408.90_pm_r,118260._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_60= (/ &
275.00_pm_r,   -97._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
252.20_pm_r,  3755._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
228.80_pm_r,  7270._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.70_pm_r, 10540._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
221.80_pm_r, 13759._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.50_pm_r, 17037._pm_r,  16._pm_r,221._pm_r, 4.66_pm_r,106._pm_r,   2._pm_r,222._pm_r,  .12_pm_r,111._pm_r, &
228.20_pm_r, 20360._pm_r,  14._pm_r,193._pm_r, 5.68_pm_r,101._pm_r,   2._pm_r,215._pm_r,  .22_pm_r,108._pm_r, &
229.70_pm_r, 23713._pm_r,  16._pm_r,163._pm_r, 5.19_pm_r, 92._pm_r,   2._pm_r,204._pm_r,  .35_pm_r,106._pm_r, &
232.80_pm_r, 27094._pm_r,  18._pm_r,142._pm_r, 3.44_pm_r, 68._pm_r,   2._pm_r,186._pm_r,  .47_pm_r,105._pm_r, &
240.40_pm_r, 30557._pm_r,  18._pm_r,129._pm_r, 2.86_pm_r,  8._pm_r,   2._pm_r,166._pm_r,  .56_pm_r,103._pm_r, &
252.80_pm_r, 34163._pm_r,  14._pm_r,117._pm_r, 4.51_pm_r,330._pm_r,   3._pm_r,150._pm_r,  .61_pm_r,103._pm_r, &
264.40_pm_r, 37956._pm_r,   8._pm_r, 92._pm_r, 5.78_pm_r,317._pm_r,   3._pm_r,139._pm_r,  .60_pm_r,103._pm_r, &
274.50_pm_r, 41901._pm_r,   6._pm_r, 13._pm_r, 5.83_pm_r,310._pm_r,   4._pm_r,132._pm_r,  .56_pm_r,105._pm_r, &
278.90_pm_r, 45962._pm_r,  11._pm_r,337._pm_r, 3.95_pm_r,307._pm_r,   5._pm_r,129._pm_r,  .28_pm_r,116._pm_r, &
276.30_pm_r, 50036._pm_r,  15._pm_r,328._pm_r, 1.98_pm_r,303._pm_r,   5._pm_r,129._pm_r,  .18_pm_r,161._pm_r, &
267.80_pm_r, 54022._pm_r,  17._pm_r,325._pm_r,  .68_pm_r,277._pm_r,   5._pm_r,131._pm_r,  .19_pm_r,163._pm_r, &
256.80_pm_r, 57868._pm_r,  17._pm_r,322._pm_r,  .63_pm_r,245._pm_r,   6._pm_r,132._pm_r,  .24_pm_r,118._pm_r, &
245.20_pm_r, 61539._pm_r,  17._pm_r,319._pm_r,  .81_pm_r,205._pm_r,   6._pm_r,129._pm_r,  .41_pm_r, 87._pm_r, &
235.30_pm_r, 65059._pm_r,  16._pm_r,315._pm_r,  .97_pm_r,189._pm_r,   6._pm_r,125._pm_r,  .52_pm_r, 78._pm_r, &
226.30_pm_r, 68436._pm_r,  15._pm_r,311._pm_r,  .97_pm_r,182._pm_r,   7._pm_r,120._pm_r,  .54_pm_r, 74._pm_r, &
217.60_pm_r, 71689._pm_r,  14._pm_r,307._pm_r,  .85_pm_r,177._pm_r,   7._pm_r,116._pm_r,  .48_pm_r, 73._pm_r, &
207.10_pm_r, 74799._pm_r,  14._pm_r,303._pm_r,  .69_pm_r,175._pm_r,   8._pm_r,112._pm_r,  .39_pm_r, 73._pm_r, &
194.30_pm_r, 77744._pm_r,  13._pm_r,300._pm_r,  .55_pm_r,172._pm_r,   8._pm_r,110._pm_r,  .30_pm_r, 75._pm_r, &
181.80_pm_r, 80489._pm_r,  13._pm_r,297._pm_r,  .41_pm_r,170._pm_r,   9._pm_r,109._pm_r,  .24_pm_r, 74._pm_r, &
169.20_pm_r, 83063._pm_r,  12._pm_r,295._pm_r,  .31_pm_r,167._pm_r,   9._pm_r,108._pm_r,  .18_pm_r, 74._pm_r, &
158.80_pm_r, 85425._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
154.90_pm_r, 87692._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
155.80_pm_r, 89960._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
161.40_pm_r, 92286._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
171.30_pm_r, 94735._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.80_pm_r, 97391._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
210.70_pm_r,100364._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
242.60_pm_r,103797._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
279.20_pm_r,107819._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
321.00_pm_r,112508._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
406.30_pm_r,118300._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)
 
  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_50= (/ &
280.40_pm_r,   -52._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
258.90_pm_r,  3892._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
234.40_pm_r,  7500._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
220.50_pm_r, 10826._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
220.00_pm_r, 14048._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
220.90_pm_r, 17279._pm_r,  13._pm_r,211._pm_r, 3.30_pm_r, 95._pm_r,   2._pm_r,200._pm_r,  .03_pm_r,350._pm_r, &
222.40_pm_r, 20523._pm_r,  11._pm_r,187._pm_r, 4.03_pm_r, 91._pm_r,   2._pm_r,199._pm_r,  .03_pm_r,101._pm_r, &
226.20_pm_r, 23810._pm_r,  12._pm_r,158._pm_r, 3.69_pm_r, 83._pm_r,   2._pm_r,196._pm_r,  .12_pm_r,135._pm_r, &
228.50_pm_r, 27139._pm_r,  13._pm_r,138._pm_r, 2.54_pm_r, 61._pm_r,   2._pm_r,189._pm_r,  .26_pm_r,140._pm_r, &
234.30_pm_r, 30522._pm_r,  13._pm_r,125._pm_r, 1.91_pm_r,  4._pm_r,   2._pm_r,180._pm_r,  .39_pm_r,140._pm_r, &
247.60_pm_r, 34043._pm_r,  10._pm_r,115._pm_r, 2.88_pm_r,321._pm_r,   3._pm_r,171._pm_r,  .49_pm_r,137._pm_r, &
260.90_pm_r, 37773._pm_r,   6._pm_r,102._pm_r, 3.70_pm_r,306._pm_r,   3._pm_r,164._pm_r,  .52_pm_r,132._pm_r, &
271.90_pm_r, 41675._pm_r,   2._pm_r, 30._pm_r, 3.73_pm_r,297._pm_r,   4._pm_r,158._pm_r,  .51_pm_r,127._pm_r, &
276.80_pm_r, 45702._pm_r,   5._pm_r,320._pm_r, 2.54_pm_r,294._pm_r,   5._pm_r,154._pm_r,  .35_pm_r,130._pm_r, &
274.10_pm_r, 49744._pm_r,   8._pm_r,309._pm_r, 1.36_pm_r,286._pm_r,   5._pm_r,152._pm_r,  .27_pm_r,147._pm_r, &
265.90_pm_r, 53701._pm_r,   9._pm_r,304._pm_r,  .64_pm_r,258._pm_r,   5._pm_r,152._pm_r,  .21_pm_r,152._pm_r, &
254.80_pm_r, 57519._pm_r,   9._pm_r,299._pm_r,  .60_pm_r,230._pm_r,   6._pm_r,152._pm_r,  .19_pm_r,121._pm_r, &
244.10_pm_r, 61167._pm_r,   9._pm_r,294._pm_r,  .72_pm_r,190._pm_r,   6._pm_r,149._pm_r,  .26_pm_r, 73._pm_r, &
234.60_pm_r, 64674._pm_r,   9._pm_r,287._pm_r,  .82_pm_r,172._pm_r,   6._pm_r,145._pm_r,  .33_pm_r, 58._pm_r, &
225.30_pm_r, 68039._pm_r,   8._pm_r,280._pm_r,  .77_pm_r,162._pm_r,   6._pm_r,140._pm_r,  .31_pm_r, 52._pm_r, &
216.80_pm_r, 71278._pm_r,   8._pm_r,273._pm_r,  .64_pm_r,157._pm_r,   6._pm_r,136._pm_r,  .24_pm_r, 48._pm_r, &
207.80_pm_r, 74387._pm_r,   7._pm_r,267._pm_r,  .48_pm_r,152._pm_r,   6._pm_r,133._pm_r,  .15_pm_r, 45._pm_r, &
197.40_pm_r, 77358._pm_r,   7._pm_r,263._pm_r,  .34_pm_r,148._pm_r,   6._pm_r,131._pm_r,  .08_pm_r, 43._pm_r, &
187.00_pm_r, 80167._pm_r,   7._pm_r,260._pm_r,  .23_pm_r,142._pm_r,   6._pm_r,131._pm_r,  .04_pm_r, 28._pm_r, &
176.90_pm_r, 82834._pm_r,   7._pm_r,258._pm_r,  .16_pm_r,135._pm_r,   6._pm_r,130._pm_r,  .01_pm_r,  0._pm_r, &
168.30_pm_r, 85340._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
164.00_pm_r, 87754._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
163.40_pm_r, 90146._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
167.30_pm_r, 92574._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
175.00_pm_r, 95097._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.70_pm_r, 97789._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
207.10_pm_r,100745._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
234.70_pm_r,104093._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
271.10_pm_r,107986._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
320.10_pm_r,112608._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
403.10_pm_r,118377._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_40= (/ &
287.20_pm_r,    -9._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
265.60_pm_r,  4038._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
240.00_pm_r,  7739._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
221.00_pm_r, 11114._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
215.50_pm_r, 14309._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
215.00_pm_r, 17458._pm_r,   6._pm_r,186._pm_r, 1.04_pm_r, 84._pm_r,   2._pm_r,133._pm_r,  .34_pm_r,261._pm_r, &
217.20_pm_r, 20619._pm_r,   6._pm_r,170._pm_r, 1.19_pm_r, 79._pm_r,   2._pm_r,149._pm_r,  .42_pm_r,256._pm_r, &
221.80_pm_r, 23837._pm_r,   6._pm_r,154._pm_r, 1.01_pm_r, 68._pm_r,   2._pm_r,170._pm_r,  .42_pm_r,249._pm_r, &
224.80_pm_r, 27107._pm_r,   6._pm_r,142._pm_r,  .67_pm_r, 33._pm_r,   2._pm_r,186._pm_r,  .34_pm_r,234._pm_r, &
231.00_pm_r, 30440._pm_r,   5._pm_r,136._pm_r,  .76_pm_r,330._pm_r,   2._pm_r,192._pm_r,  .25_pm_r,200._pm_r, &
244.30_pm_r, 33911._pm_r,   4._pm_r,136._pm_r, 1.17_pm_r,302._pm_r,   3._pm_r,190._pm_r,  .26_pm_r,152._pm_r, &
258.20_pm_r, 37597._pm_r,   2._pm_r,156._pm_r, 1.38_pm_r,290._pm_r,   3._pm_r,183._pm_r,  .34_pm_r,126._pm_r, &
270.00_pm_r, 41465._pm_r,   2._pm_r,219._pm_r, 1.30_pm_r,281._pm_r,   3._pm_r,174._pm_r,  .39_pm_r,113._pm_r, &
275.40_pm_r, 45468._pm_r,   3._pm_r,248._pm_r,  .86_pm_r,279._pm_r,   4._pm_r,168._pm_r,  .29_pm_r,130._pm_r, &
273.20_pm_r, 49493._pm_r,   4._pm_r,256._pm_r,  .43_pm_r,288._pm_r,   4._pm_r,166._pm_r,  .33_pm_r,175._pm_r, &
264.70_pm_r, 53435._pm_r,   4._pm_r,259._pm_r,  .10_pm_r,336._pm_r,   4._pm_r,169._pm_r,  .37_pm_r,196._pm_r, &
252.60_pm_r, 57229._pm_r,   4._pm_r,260._pm_r,  .20_pm_r,101._pm_r,   5._pm_r,171._pm_r,  .17_pm_r,201._pm_r, &
241.70_pm_r, 60842._pm_r,   3._pm_r,253._pm_r,  .55_pm_r,127._pm_r,   5._pm_r,170._pm_r,  .26_pm_r, 38._pm_r, &
233.40_pm_r, 64323._pm_r,   3._pm_r,239._pm_r,  .66_pm_r,129._pm_r,   4._pm_r,164._pm_r,  .56_pm_r, 36._pm_r, &
224.50_pm_r, 67674._pm_r,   3._pm_r,221._pm_r,  .57_pm_r,128._pm_r,   4._pm_r,153._pm_r,  .68_pm_r, 38._pm_r, &
216.10_pm_r, 70902._pm_r,   3._pm_r,206._pm_r,  .38_pm_r,117._pm_r,   4._pm_r,138._pm_r,  .67_pm_r, 42._pm_r, &
208.60_pm_r, 74009._pm_r,   3._pm_r,197._pm_r,  .21_pm_r, 94._pm_r,   4._pm_r,123._pm_r,  .61_pm_r, 45._pm_r, &
201.20_pm_r, 77013._pm_r,   3._pm_r,193._pm_r,  .17_pm_r, 40._pm_r,   4._pm_r,111._pm_r,  .55_pm_r, 49._pm_r, &
194.50_pm_r, 79906._pm_r,   2._pm_r,192._pm_r,  .20_pm_r, 12._pm_r,   4._pm_r,103._pm_r,  .47_pm_r, 52._pm_r, &
186.90_pm_r, 82708._pm_r,   2._pm_r,193._pm_r,  .22_pm_r,360._pm_r,   5._pm_r, 97._pm_r,  .41_pm_r, 54._pm_r, &
178.70_pm_r, 85371._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
173.40_pm_r, 87934._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
171.50_pm_r, 90457._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
173.60_pm_r, 92992._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
178.90_pm_r, 95594._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
188.60_pm_r, 98324._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
203.60_pm_r,101263._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
226.80_pm_r,104527._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
262.00_pm_r,108286._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
317.10_pm_r,112813._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
399.80_pm_r,118541._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_30= (/ &
292.90_pm_r,   -18._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
271.40_pm_r,  4116._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
245.70_pm_r,  7904._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
222.50_pm_r, 11335._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
209.40_pm_r, 14499._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
208.00_pm_r, 17542._pm_r,   3._pm_r,134._pm_r,  .18_pm_r,269._pm_r,   1._pm_r,100._pm_r,  .21_pm_r,225._pm_r, &
213.00_pm_r, 20623._pm_r,   3._pm_r,139._pm_r,  .25_pm_r,266._pm_r,   1._pm_r,118._pm_r,  .27_pm_r,224._pm_r, &
219.30_pm_r, 23792._pm_r,   2._pm_r,146._pm_r,  .29_pm_r,266._pm_r,   1._pm_r,144._pm_r,  .27_pm_r,221._pm_r, &
224.20_pm_r, 27038._pm_r,   2._pm_r,156._pm_r,  .32_pm_r,265._pm_r,   1._pm_r,164._pm_r,  .24_pm_r,216._pm_r, &
231.40_pm_r, 30372._pm_r,   2._pm_r,168._pm_r,  .30_pm_r,262._pm_r,   1._pm_r,174._pm_r,  .18_pm_r,203._pm_r, &
243.40_pm_r, 33843._pm_r,   2._pm_r,179._pm_r,  .25_pm_r,257._pm_r,   2._pm_r,177._pm_r,  .12_pm_r,178._pm_r, &
255.60_pm_r, 37501._pm_r,   2._pm_r,186._pm_r,  .17_pm_r,248._pm_r,   2._pm_r,175._pm_r,  .11_pm_r,133._pm_r, &
266.60_pm_r, 41323._pm_r,   2._pm_r,190._pm_r,  .10_pm_r,225._pm_r,   2._pm_r,170._pm_r,  .13_pm_r,105._pm_r, &
273.10_pm_r, 45284._pm_r,   3._pm_r,191._pm_r,  .03_pm_r,218._pm_r,   2._pm_r,165._pm_r,  .11_pm_r,106._pm_r, &
272.50_pm_r, 49289._pm_r,   3._pm_r,191._pm_r,  .02_pm_r, 63._pm_r,   2._pm_r,162._pm_r,  .06_pm_r,151._pm_r, &
262.90_pm_r, 53212._pm_r,   3._pm_r,189._pm_r,  .09_pm_r, 99._pm_r,   2._pm_r,163._pm_r,  .09_pm_r,191._pm_r, &
251.00_pm_r, 56978._pm_r,   3._pm_r,184._pm_r,  .21_pm_r,114._pm_r,   2._pm_r,165._pm_r,  .12_pm_r,178._pm_r, &
241.10_pm_r, 60579._pm_r,   3._pm_r,177._pm_r,  .28_pm_r,111._pm_r,   2._pm_r,164._pm_r,  .13_pm_r,116._pm_r, &
231.70_pm_r, 64042._pm_r,   3._pm_r,170._pm_r,  .21_pm_r,102._pm_r,   2._pm_r,158._pm_r,  .22_pm_r, 78._pm_r, &
223.00_pm_r, 67369._pm_r,   3._pm_r,165._pm_r,  .09_pm_r, 54._pm_r,   2._pm_r,149._pm_r,  .32_pm_r, 63._pm_r, &
215.20_pm_r, 70579._pm_r,   3._pm_r,165._pm_r,  .17_pm_r,334._pm_r,   3._pm_r,136._pm_r,  .41_pm_r, 53._pm_r, &
208.80_pm_r, 73680._pm_r,   2._pm_r,168._pm_r,  .34_pm_r,319._pm_r,   3._pm_r,122._pm_r,  .47_pm_r, 47._pm_r, &
204.30_pm_r, 76704._pm_r,   2._pm_r,177._pm_r,  .45_pm_r,315._pm_r,   3._pm_r,108._pm_r,  .50_pm_r, 43._pm_r, &
201.10_pm_r, 79671._pm_r,   1._pm_r,197._pm_r,  .51_pm_r,314._pm_r,   3._pm_r, 96._pm_r,  .51_pm_r, 40._pm_r, &
195.70_pm_r, 82594._pm_r,   1._pm_r,229._pm_r,  .52_pm_r,313._pm_r,   4._pm_r, 86._pm_r,  .49_pm_r, 39._pm_r, &
187.40_pm_r, 85386._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
181.40_pm_r, 88071._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
178.70_pm_r, 90708._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
179.40_pm_r, 93343._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
182.40_pm_r, 96017._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.20_pm_r, 98782._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.30_pm_r,101705._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
219.60_pm_r,104891._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
252.80_pm_r,108523._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
312.10_pm_r,112936._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
396.30_pm_r,118604._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_20= (/ &
297.20_pm_r,   -29._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
275.20_pm_r,  4168._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
250.50_pm_r,  8022._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
224.60_pm_r, 11505._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
203.80_pm_r, 14646._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
202.00_pm_r, 17591._pm_r,   3._pm_r,113._pm_r,  .45_pm_r,247._pm_r,   0._pm_r,293._pm_r,  .24_pm_r,129._pm_r, &
210.50_pm_r, 20610._pm_r,   2._pm_r,127._pm_r,  .55_pm_r,244._pm_r,   0._pm_r,158._pm_r,  .31_pm_r,128._pm_r, &
217.80_pm_r, 23749._pm_r,   2._pm_r,148._pm_r,  .53_pm_r,243._pm_r,   1._pm_r,136._pm_r,  .30_pm_r,129._pm_r, &
223.70_pm_r, 26981._pm_r,   2._pm_r,169._pm_r,  .43_pm_r,239._pm_r,   1._pm_r,133._pm_r,  .27_pm_r,129._pm_r, &
232.00_pm_r, 30317._pm_r,   2._pm_r,180._pm_r,  .25_pm_r,236._pm_r,   1._pm_r,133._pm_r,  .20_pm_r,131._pm_r, &
243.70_pm_r, 33795._pm_r,   2._pm_r,185._pm_r,  .06_pm_r,231._pm_r,   2._pm_r,132._pm_r,  .11_pm_r,131._pm_r, &
255.00_pm_r, 37452._pm_r,   2._pm_r,184._pm_r,  .10_pm_r, 39._pm_r,   2._pm_r,132._pm_r,  .03_pm_r,121._pm_r, &
264.60_pm_r, 41257._pm_r,   2._pm_r,181._pm_r,  .22_pm_r, 30._pm_r,   2._pm_r,132._pm_r,  .03_pm_r,342._pm_r, &
270.30_pm_r, 45180._pm_r,   2._pm_r,178._pm_r,  .25_pm_r,  2._pm_r,   2._pm_r,132._pm_r,  .04_pm_r,225._pm_r, &
270.50_pm_r, 49149._pm_r,   2._pm_r,182._pm_r,  .33_pm_r,329._pm_r,   2._pm_r,136._pm_r,  .14_pm_r,216._pm_r, &
261.10_pm_r, 53045._pm_r,   1._pm_r,200._pm_r,  .39_pm_r,324._pm_r,   2._pm_r,143._pm_r,  .16_pm_r,207._pm_r, &
250.00_pm_r, 56789._pm_r,   1._pm_r,226._pm_r,  .32_pm_r,351._pm_r,   2._pm_r,148._pm_r,  .08_pm_r,173._pm_r, &
240.50_pm_r, 60378._pm_r,   1._pm_r,258._pm_r,  .36_pm_r, 32._pm_r,   2._pm_r,146._pm_r,  .14_pm_r, 72._pm_r, &
230.90_pm_r, 63833._pm_r,   0._pm_r,341._pm_r,  .39_pm_r, 55._pm_r,   2._pm_r,138._pm_r,  .22_pm_r, 63._pm_r, &
221.60_pm_r, 67143._pm_r,   1._pm_r, 34._pm_r,  .35_pm_r, 68._pm_r,   2._pm_r,129._pm_r,  .21_pm_r, 65._pm_r, &
214.00_pm_r, 70332._pm_r,   1._pm_r, 50._pm_r,  .27_pm_r, 86._pm_r,   2._pm_r,123._pm_r,  .15_pm_r, 74._pm_r, &
209.10_pm_r, 73427._pm_r,   1._pm_r, 60._pm_r,  .19_pm_r,110._pm_r,   2._pm_r,121._pm_r,  .08_pm_r, 98._pm_r, &
206.20_pm_r, 76467._pm_r,   1._pm_r, 69._pm_r,  .17_pm_r,140._pm_r,   3._pm_r,121._pm_r,  .06_pm_r,162._pm_r, &
205.30_pm_r, 79477._pm_r,   2._pm_r, 79._pm_r,  .19_pm_r,161._pm_r,   3._pm_r,123._pm_r,  .10_pm_r,183._pm_r, &
201.30_pm_r, 82478._pm_r,   2._pm_r, 89._pm_r,  .18_pm_r,174._pm_r,   3._pm_r,126._pm_r,  .12_pm_r,194._pm_r, &
192.90_pm_r, 85350._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.60_pm_r, 88113._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
184.00_pm_r, 90830._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
183.80_pm_r, 93538._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
185.30_pm_r, 96270._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.50_pm_r, 99060._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
197.30_pm_r,101966._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
213.30_pm_r,105083._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
244.30_pm_r,108602._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
305.90_pm_r,112895._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
392.50_pm_r,118490._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/) 

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_moins_10= (/ &
299.90_pm_r,   -70._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
276.30_pm_r,  4157._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
252.80_pm_r,  8038._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.50_pm_r, 11546._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.40_pm_r, 14670._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
198.60_pm_r, 17559._pm_r,   3._pm_r,139._pm_r,  .32_pm_r,264._pm_r,   0._pm_r,233._pm_r,  .20_pm_r,102._pm_r, &
209.40_pm_r, 20546._pm_r,   2._pm_r,150._pm_r,  .40_pm_r,269._pm_r,   0._pm_r,162._pm_r,  .26_pm_r,103._pm_r, &
216.80_pm_r, 23669._pm_r,   2._pm_r,164._pm_r,  .39_pm_r,275._pm_r,   1._pm_r,128._pm_r,  .27_pm_r,105._pm_r, &
223.20_pm_r, 26890._pm_r,   2._pm_r,179._pm_r,  .37_pm_r,287._pm_r,   1._pm_r,119._pm_r,  .26_pm_r,107._pm_r, &
231.30_pm_r, 30215._pm_r,   2._pm_r,194._pm_r,  .31_pm_r,305._pm_r,   1._pm_r,116._pm_r,  .20_pm_r,109._pm_r, &
244.30_pm_r, 33692._pm_r,   2._pm_r,208._pm_r,  .28_pm_r,331._pm_r,   2._pm_r,115._pm_r,  .12_pm_r,108._pm_r, &
255.00_pm_r, 37355._pm_r,   1._pm_r,222._pm_r,  .29_pm_r,350._pm_r,   2._pm_r,115._pm_r,  .04_pm_r, 94._pm_r, &
263.20_pm_r, 41150._pm_r,   1._pm_r,240._pm_r,  .32_pm_r,360._pm_r,   2._pm_r,114._pm_r,  .04_pm_r,333._pm_r, &
267.90_pm_r, 45044._pm_r,   1._pm_r,267._pm_r,  .29_pm_r,355._pm_r,   2._pm_r,113._pm_r,  .03_pm_r,287._pm_r, &
267.60_pm_r, 48972._pm_r,   1._pm_r,289._pm_r,  .29_pm_r,329._pm_r,   2._pm_r,115._pm_r,  .10_pm_r,183._pm_r, &
261.20_pm_r, 52848._pm_r,   2._pm_r,298._pm_r,  .34_pm_r,316._pm_r,   2._pm_r,122._pm_r,  .19_pm_r,180._pm_r, &
252.40_pm_r, 56612._pm_r,   2._pm_r,304._pm_r,  .31_pm_r,328._pm_r,   2._pm_r,130._pm_r,  .22_pm_r,192._pm_r, &
242.10_pm_r, 60233._pm_r,   2._pm_r,311._pm_r,  .22_pm_r, 21._pm_r,   2._pm_r,137._pm_r,  .12_pm_r,217._pm_r, &
230.00_pm_r, 63695._pm_r,   2._pm_r,320._pm_r,  .35_pm_r, 76._pm_r,   2._pm_r,141._pm_r,  .07_pm_r,274._pm_r, &
219.00_pm_r, 66978._pm_r,   2._pm_r,337._pm_r,  .57_pm_r, 99._pm_r,   2._pm_r,142._pm_r,  .05_pm_r,335._pm_r, &
211.30_pm_r, 70127._pm_r,   1._pm_r, 10._pm_r,  .77_pm_r,110._pm_r,   2._pm_r,140._pm_r,  .07_pm_r, 36._pm_r, &
208.10_pm_r, 73194._pm_r,   2._pm_r, 57._pm_r,  .94_pm_r,115._pm_r,   2._pm_r,136._pm_r,  .10_pm_r, 63._pm_r, &
207.80_pm_r, 76237._pm_r,   3._pm_r, 85._pm_r, 1.07_pm_r,118._pm_r,   2._pm_r,131._pm_r,  .14_pm_r, 71._pm_r, &
208.50_pm_r, 79288._pm_r,   4._pm_r, 97._pm_r, 1.14_pm_r,120._pm_r,   2._pm_r,126._pm_r,  .17_pm_r, 78._pm_r, &
204.70_pm_r, 82346._pm_r,   6._pm_r,104._pm_r, 1.14_pm_r,121._pm_r,   2._pm_r,121._pm_r,  .17_pm_r, 82._pm_r, &
195.60_pm_r, 85261._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.10_pm_r, 88059._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.80_pm_r, 90816._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.40_pm_r, 93566._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.00_pm_r, 96332._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.20_pm_r, 99134._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
194.80_pm_r,102021._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
208.10_pm_r,105080._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
237.10_pm_r,108502._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
299.50_pm_r,112685._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
388.30_pm_r,118202._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_0= (/ &
300.50_pm_r,  -111._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
276.70_pm_r,  4123._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
253.60_pm_r,  8013._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
226.00_pm_r, 11532._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.00_pm_r, 14657._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
197.90_pm_r, 17537._pm_r,   2._pm_r,140._pm_r,  .28_pm_r,271._pm_r,   0._pm_r,251._pm_r,  .16_pm_r, 79._pm_r, &
208.60_pm_r, 20516._pm_r,   2._pm_r,148._pm_r,  .37_pm_r,276._pm_r,   0._pm_r, 97._pm_r,  .21_pm_r, 82._pm_r, &
216.20_pm_r, 23630._pm_r,   2._pm_r,161._pm_r,  .38_pm_r,286._pm_r,   1._pm_r, 90._pm_r,  .22_pm_r, 82._pm_r, &
223.20_pm_r, 26847._pm_r,   1._pm_r,178._pm_r,  .39_pm_r,300._pm_r,   1._pm_r, 89._pm_r,  .20_pm_r, 84._pm_r, &
232.10_pm_r, 30177._pm_r,   1._pm_r,204._pm_r,  .39_pm_r,317._pm_r,   1._pm_r, 89._pm_r,  .15_pm_r, 82._pm_r, &
244.80_pm_r, 33662._pm_r,   1._pm_r,242._pm_r,  .39_pm_r,334._pm_r,   1._pm_r, 88._pm_r,  .08_pm_r, 76._pm_r, &
255.10_pm_r, 37325._pm_r,   1._pm_r,282._pm_r,  .39_pm_r,348._pm_r,   1._pm_r, 87._pm_r,  .04_pm_r, 30._pm_r, &
262.70_pm_r, 41115._pm_r,   1._pm_r,308._pm_r,  .40_pm_r,357._pm_r,   1._pm_r, 84._pm_r,  .06_pm_r,329._pm_r, &
266.90_pm_r, 44999._pm_r,   2._pm_r,322._pm_r,  .43_pm_r,353._pm_r,   1._pm_r, 81._pm_r,  .10_pm_r,279._pm_r, &
267.00_pm_r, 48914._pm_r,   2._pm_r,328._pm_r,  .36_pm_r,344._pm_r,   1._pm_r, 80._pm_r,  .22_pm_r,254._pm_r, &
262.40_pm_r, 52792._pm_r,   3._pm_r,329._pm_r,  .16_pm_r,346._pm_r,   1._pm_r, 86._pm_r,  .30_pm_r,237._pm_r, &
253.50_pm_r, 56571._pm_r,   3._pm_r,328._pm_r,  .19_pm_r,126._pm_r,   1._pm_r,110._pm_r,  .30_pm_r,217._pm_r, &
241.60_pm_r, 60196._pm_r,   3._pm_r,328._pm_r,  .45_pm_r,127._pm_r,   1._pm_r,137._pm_r,  .21_pm_r,173._pm_r, &
227.80_pm_r, 63642._pm_r,   2._pm_r,332._pm_r,  .63_pm_r,129._pm_r,   1._pm_r,144._pm_r,  .29_pm_r,131._pm_r, &
215.60_pm_r, 66893._pm_r,   1._pm_r,344._pm_r,  .70_pm_r,129._pm_r,   2._pm_r,142._pm_r,  .40_pm_r,118._pm_r, &
208.50_pm_r, 70008._pm_r,   1._pm_r, 26._pm_r,  .72_pm_r,129._pm_r,   2._pm_r,137._pm_r,  .48_pm_r,114._pm_r, &
207.30_pm_r, 73057._pm_r,   1._pm_r, 86._pm_r,  .73_pm_r,129._pm_r,   3._pm_r,131._pm_r,  .54_pm_r,113._pm_r, &
209.70_pm_r, 76110._pm_r,   2._pm_r,106._pm_r,  .75_pm_r,130._pm_r,   3._pm_r,127._pm_r,  .58_pm_r,114._pm_r, &
212.30_pm_r, 79199._pm_r,   4._pm_r,114._pm_r,  .73_pm_r,130._pm_r,   4._pm_r,123._pm_r,  .61_pm_r,114._pm_r, &
208.40_pm_r, 82311._pm_r,   5._pm_r,118._pm_r,  .70_pm_r,130._pm_r,   5._pm_r,120._pm_r,  .60_pm_r,115._pm_r, &
197.40_pm_r, 85259._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.70_pm_r, 88066._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.50_pm_r, 90832._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.30_pm_r, 93595._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.40_pm_r, 96370._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
188.50_pm_r, 99171._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
192.60_pm_r,102036._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
204.10_pm_r,105048._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
231.60_pm_r,108398._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
294.10_pm_r,112493._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
384.10_pm_r,117935._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_10= (/ &
301.00_pm_r,  -100._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
277.00_pm_r,  4139._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
253.20_pm_r,  8028._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.70_pm_r, 11541._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.50_pm_r, 14667._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
198.30_pm_r, 17553._pm_r,   3._pm_r,142._pm_r,  .33_pm_r,313._pm_r,   1._pm_r,286._pm_r,  .26_pm_r, 67._pm_r, &
208.80_pm_r, 20533._pm_r,   3._pm_r,144._pm_r,  .50_pm_r,320._pm_r,   0._pm_r,335._pm_r,  .32_pm_r, 71._pm_r, &
216.60_pm_r, 23651._pm_r,   2._pm_r,144._pm_r,  .61_pm_r,326._pm_r,   1._pm_r, 33._pm_r,  .30_pm_r, 76._pm_r, &
223.50_pm_r, 26872._pm_r,   1._pm_r,138._pm_r,  .71_pm_r,333._pm_r,   1._pm_r, 53._pm_r,  .25_pm_r, 83._pm_r, &
232.20_pm_r, 30207._pm_r,   0._pm_r, 19._pm_r,  .78_pm_r,339._pm_r,   1._pm_r, 62._pm_r,  .17_pm_r, 96._pm_r, &
243.60_pm_r, 33688._pm_r,   2._pm_r,350._pm_r,  .79_pm_r,344._pm_r,   1._pm_r, 68._pm_r,  .09_pm_r,119._pm_r, &
254.00_pm_r, 37337._pm_r,   3._pm_r,348._pm_r,  .75_pm_r,347._pm_r,   1._pm_r, 71._pm_r,  .04_pm_r,169._pm_r, &
262.50_pm_r, 41119._pm_r,   4._pm_r,348._pm_r,  .68_pm_r,349._pm_r,   1._pm_r, 72._pm_r,  .04_pm_r,270._pm_r, &
266.90_pm_r, 45000._pm_r,   5._pm_r,348._pm_r,  .67_pm_r,347._pm_r,   1._pm_r, 71._pm_r,  .02_pm_r,315._pm_r, &
267.00_pm_r, 48914._pm_r,   6._pm_r,348._pm_r,  .53_pm_r,338._pm_r,   1._pm_r, 70._pm_r,  .03_pm_r,252._pm_r, &
261.80_pm_r, 52790._pm_r,   6._pm_r,346._pm_r,  .19_pm_r,307._pm_r,   1._pm_r, 72._pm_r,  .14_pm_r,238._pm_r, &
252.70_pm_r, 56565._pm_r,   6._pm_r,345._pm_r,  .43_pm_r,175._pm_r,   1._pm_r, 76._pm_r,  .28_pm_r,240._pm_r, &
241.10_pm_r, 60181._pm_r,   5._pm_r,344._pm_r,  .79_pm_r,163._pm_r,   1._pm_r, 85._pm_r,  .23_pm_r,249._pm_r, &
227.60_pm_r, 63618._pm_r,   4._pm_r,345._pm_r,  .95_pm_r,159._pm_r,   0._pm_r,103._pm_r,  .10_pm_r,240._pm_r, &
216.40_pm_r, 66862._pm_r,   2._pm_r,350._pm_r,  .92_pm_r,157._pm_r,   0._pm_r,117._pm_r,  .13_pm_r,114._pm_r, &
209.70_pm_r, 69977._pm_r,   1._pm_r,  4._pm_r,  .80_pm_r,157._pm_r,   1._pm_r,112._pm_r,  .32_pm_r,104._pm_r, &
208.50_pm_r, 73035._pm_r,   1._pm_r, 75._pm_r,  .67_pm_r,157._pm_r,   1._pm_r,107._pm_r,  .50_pm_r,102._pm_r, &
209.80_pm_r, 76095._pm_r,   1._pm_r,129._pm_r,  .56_pm_r,157._pm_r,   2._pm_r,105._pm_r,  .65_pm_r,102._pm_r, &
211.60_pm_r, 79184._pm_r,   2._pm_r,141._pm_r,  .48_pm_r,157._pm_r,   3._pm_r,104._pm_r,  .74_pm_r,101._pm_r, &
208.00_pm_r, 82298._pm_r,   2._pm_r,145._pm_r,  .41_pm_r,158._pm_r,   4._pm_r,103._pm_r,  .77_pm_r,102._pm_r, &
197.60_pm_r, 85252._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.70_pm_r, 88063._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.10_pm_r, 90824._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.90_pm_r, 93581._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.90_pm_r, 96350._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.70_pm_r, 99141._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
191.20_pm_r,101989._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.90_pm_r,104972._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
228.30_pm_r,108278._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
290.30_pm_r,112316._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
379.80_pm_r,117696._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_20= (/ &
299.70_pm_r,   -99._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
275.70_pm_r,  4120._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
250.90_pm_r,  7981._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
224.30_pm_r, 11465._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
202.80_pm_r, 14596._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.20_pm_r, 17526._pm_r,   6._pm_r,137._pm_r,  .87_pm_r,292._pm_r,   1._pm_r,253._pm_r,  .50_pm_r, 43._pm_r, &
210.20_pm_r, 20538._pm_r,   5._pm_r,142._pm_r, 1.15_pm_r,307._pm_r,   1._pm_r,284._pm_r,  .58_pm_r, 48._pm_r, &
217.20_pm_r, 23671._pm_r,   3._pm_r,146._pm_r, 1.32_pm_r,326._pm_r,   1._pm_r,355._pm_r,  .50_pm_r, 57._pm_r, &
223.00_pm_r, 26893._pm_r,   1._pm_r,116._pm_r, 1.60_pm_r,345._pm_r,   1._pm_r, 29._pm_r,  .37_pm_r, 77._pm_r, &
229.90_pm_r, 30208._pm_r,   2._pm_r, 10._pm_r, 1.91_pm_r,  0._pm_r,   1._pm_r, 48._pm_r,  .28_pm_r,116._pm_r, &
240.40_pm_r, 33648._pm_r,   5._pm_r,  7._pm_r, 2.08_pm_r,  9._pm_r,   1._pm_r, 65._pm_r,  .29_pm_r,151._pm_r, &
251.20_pm_r, 37252._pm_r,   8._pm_r,  9._pm_r, 1.99_pm_r, 13._pm_r,   1._pm_r, 81._pm_r,  .26_pm_r,165._pm_r, &
261.20_pm_r, 41002._pm_r,  11._pm_r, 10._pm_r, 1.65_pm_r, 13._pm_r,   2._pm_r, 93._pm_r,  .14_pm_r,159._pm_r, &
267.10_pm_r, 44879._pm_r,  13._pm_r,  9._pm_r, 1.24_pm_r,  2._pm_r,   2._pm_r, 93._pm_r,  .16_pm_r, 57._pm_r, &
266.20_pm_r, 48791._pm_r,  14._pm_r,  8._pm_r,  .76_pm_r,346._pm_r,   2._pm_r, 85._pm_r,  .22_pm_r, 21._pm_r, &
260.10_pm_r, 52648._pm_r,  15._pm_r,  7._pm_r,  .25_pm_r,244._pm_r,   2._pm_r, 75._pm_r,  .26_pm_r,315._pm_r, &
250.70_pm_r, 56394._pm_r,  14._pm_r,  6._pm_r, 1.21_pm_r,187._pm_r,   1._pm_r, 62._pm_r,  .60_pm_r,273._pm_r, &
240.30_pm_r, 59987._pm_r,  12._pm_r,  5._pm_r, 1.82_pm_r,189._pm_r,   1._pm_r, 23._pm_r,  .62_pm_r,272._pm_r, &
229.80_pm_r, 63432._pm_r,   9._pm_r,  4._pm_r, 1.90_pm_r,193._pm_r,   1._pm_r,325._pm_r,  .39_pm_r,274._pm_r, &
219.50_pm_r, 66717._pm_r,   6._pm_r,359._pm_r, 1.52_pm_r,201._pm_r,   1._pm_r,313._pm_r,  .01_pm_r,333._pm_r, &
212.80_pm_r, 69879._pm_r,   5._pm_r,348._pm_r, 1.02_pm_r,216._pm_r,   1._pm_r,326._pm_r,  .41_pm_r, 90._pm_r, &
210.80_pm_r, 72977._pm_r,   4._pm_r,334._pm_r,  .72_pm_r,249._pm_r,   1._pm_r, 30._pm_r,  .77_pm_r, 92._pm_r, &
209.90_pm_r, 76058._pm_r,   5._pm_r,323._pm_r,  .74_pm_r,285._pm_r,   2._pm_r, 69._pm_r, 1.05_pm_r, 92._pm_r, &
210.00_pm_r, 79130._pm_r,   6._pm_r,318._pm_r,  .88_pm_r,304._pm_r,   4._pm_r, 80._pm_r, 1.22_pm_r, 93._pm_r, &
206.70_pm_r, 82207._pm_r,   7._pm_r,317._pm_r,  .97_pm_r,314._pm_r,   5._pm_r, 85._pm_r, 1.27_pm_r, 92._pm_r, &
198.10_pm_r, 85168._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
190.30_pm_r, 87995._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.80_pm_r, 90756._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.30_pm_r, 93504._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.40_pm_r, 96263._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.30_pm_r, 99045._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
190.80_pm_r,101886._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.60_pm_r,104864._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
227.50_pm_r,108162._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
288.30_pm_r,112176._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
375.90_pm_r,117509._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_30= (/ &
295.20_pm_r,   -59._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
270.60_pm_r,  4086._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
245.20_pm_r,  7865._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
222.00_pm_r, 11288._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
207.70_pm_r, 14436._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
206.30_pm_r, 17451._pm_r,   9._pm_r,170._pm_r,  .84_pm_r,340._pm_r,   2._pm_r,318._pm_r,  .26_pm_r, 32._pm_r, &
211.80_pm_r, 20510._pm_r,   7._pm_r,170._pm_r, 1.29_pm_r,357._pm_r,   2._pm_r,330._pm_r,  .26_pm_r, 45._pm_r, &
217.10_pm_r, 23654._pm_r,   5._pm_r,163._pm_r, 1.79_pm_r, 15._pm_r,   2._pm_r,339._pm_r,  .22_pm_r, 86._pm_r, &
221.00_pm_r, 26861._pm_r,   3._pm_r,127._pm_r, 2.39_pm_r, 27._pm_r,   2._pm_r,348._pm_r,  .32_pm_r,132._pm_r, &
226.40_pm_r, 30136._pm_r,   5._pm_r, 74._pm_r, 2.87_pm_r, 34._pm_r,   1._pm_r,360._pm_r,  .49_pm_r,147._pm_r, &
234.60_pm_r, 33507._pm_r,   9._pm_r, 56._pm_r, 3.03_pm_r, 37._pm_r,   1._pm_r, 36._pm_r,  .59_pm_r,146._pm_r, &
244.70_pm_r, 37018._pm_r,  13._pm_r, 50._pm_r, 2.71_pm_r, 35._pm_r,   1._pm_r, 94._pm_r,  .56_pm_r,133._pm_r, &
255.40_pm_r, 40678._pm_r,  16._pm_r, 46._pm_r, 2.06_pm_r, 24._pm_r,   2._pm_r,105._pm_r,  .53_pm_r,102._pm_r, &
263.30_pm_r, 44485._pm_r,  18._pm_r, 41._pm_r, 1.57_pm_r,358._pm_r,   3._pm_r, 98._pm_r,  .67_pm_r, 69._pm_r, &
264.60_pm_r, 48358._pm_r,  20._pm_r, 36._pm_r, 1.36_pm_r,327._pm_r,   3._pm_r, 86._pm_r,  .74_pm_r, 46._pm_r, &
258.70_pm_r, 52195._pm_r,  20._pm_r, 31._pm_r, 1.14_pm_r,284._pm_r,   4._pm_r, 74._pm_r,  .72_pm_r, 11._pm_r, &
249.10_pm_r, 55919._pm_r,  19._pm_r, 27._pm_r, 1.46_pm_r,230._pm_r,   4._pm_r, 59._pm_r,  .87_pm_r,326._pm_r, &
240.00_pm_r, 59496._pm_r,  16._pm_r, 24._pm_r, 2.09_pm_r,221._pm_r,   4._pm_r, 41._pm_r,  .96_pm_r,319._pm_r, &
232.20_pm_r, 62955._pm_r,  13._pm_r, 20._pm_r, 2.17_pm_r,221._pm_r,   5._pm_r, 24._pm_r,  .79_pm_r,318._pm_r, &
224.20_pm_r, 66295._pm_r,  10._pm_r, 14._pm_r, 1.76_pm_r,227._pm_r,   5._pm_r, 16._pm_r,  .41_pm_r,327._pm_r, &
217.70_pm_r, 69529._pm_r,   9._pm_r,  5._pm_r, 1.24_pm_r,243._pm_r,   5._pm_r, 14._pm_r,  .15_pm_r, 52._pm_r, &
214.80_pm_r, 72693._pm_r,   8._pm_r,355._pm_r,  .92_pm_r,274._pm_r,   6._pm_r, 18._pm_r,  .47_pm_r,106._pm_r, &
212.70_pm_r, 75824._pm_r,   9._pm_r,347._pm_r,  .96_pm_r,307._pm_r,   6._pm_r, 27._pm_r,  .75_pm_r,113._pm_r, &
211.40_pm_r, 78927._pm_r,  10._pm_r,343._pm_r, 1.11_pm_r,324._pm_r,   6._pm_r, 40._pm_r,  .94_pm_r,116._pm_r, &
208.10_pm_r, 82015._pm_r,  12._pm_r,341._pm_r, 1.21_pm_r,332._pm_r,   6._pm_r, 53._pm_r, 1.02_pm_r,116._pm_r, &
200.50_pm_r, 85011._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
192.40_pm_r, 87876._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.40_pm_r, 90653._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.20_pm_r, 93403._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
186.20_pm_r, 96157._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.80_pm_r, 98941._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
192.00_pm_r,101793._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
203.60_pm_r,104794._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
229.20_pm_r,108119._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
287.60_pm_r,112140._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
372.40_pm_r,117435._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_40= (/ &
288.30_pm_r,   -27._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
263.50_pm_r,  4012._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
238.10_pm_r,  7684._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
219.20_pm_r, 11031._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
213.00_pm_r, 14195._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
212.40_pm_r, 17305._pm_r,   8._pm_r,228._pm_r, 1.27_pm_r,138._pm_r,   5._pm_r, 18._pm_r,  .78_pm_r,273._pm_r, &
214.10_pm_r, 20426._pm_r,   8._pm_r,210._pm_r, 2.05_pm_r,127._pm_r,   5._pm_r,  2._pm_r, 1.03_pm_r,265._pm_r, &
216.50_pm_r, 23582._pm_r,   9._pm_r,185._pm_r, 2.92_pm_r,116._pm_r,   4._pm_r,342._pm_r, 1.11_pm_r,251._pm_r, &
217.40_pm_r, 26758._pm_r,  11._pm_r,161._pm_r, 3.68_pm_r,106._pm_r,   4._pm_r,321._pm_r, 1.06_pm_r,228._pm_r, &
220.50_pm_r, 29962._pm_r,  15._pm_r,141._pm_r, 4.08_pm_r, 96._pm_r,   4._pm_r,301._pm_r, 1.03_pm_r,196._pm_r, &
227.50_pm_r, 33238._pm_r,  19._pm_r,127._pm_r, 3.92_pm_r, 85._pm_r,   4._pm_r,280._pm_r, 1.14_pm_r,162._pm_r, &
236.30_pm_r, 36636._pm_r,  23._pm_r,117._pm_r, 3.21_pm_r, 71._pm_r,   3._pm_r,251._pm_r, 1.31_pm_r,135._pm_r, &
246.90_pm_r, 40168._pm_r,  25._pm_r,110._pm_r, 2.28_pm_r, 46._pm_r,   2._pm_r,203._pm_r, 1.46_pm_r,112._pm_r, &
257.50_pm_r, 43870._pm_r,  26._pm_r,103._pm_r, 2.05_pm_r, 18._pm_r,   3._pm_r,154._pm_r, 1.41_pm_r, 93._pm_r, &
260.90_pm_r, 47675._pm_r,  26._pm_r, 96._pm_r, 2.34_pm_r,  1._pm_r,   4._pm_r,126._pm_r, 1.25_pm_r, 76._pm_r, &
256.80_pm_r, 51471._pm_r,  26._pm_r, 89._pm_r, 2.34_pm_r,351._pm_r,   5._pm_r,109._pm_r,  .90_pm_r, 50._pm_r, &
248.70_pm_r, 55179._pm_r,  25._pm_r, 82._pm_r, 1.77_pm_r,336._pm_r,   5._pm_r, 97._pm_r,  .71_pm_r,355._pm_r, &
239.90_pm_r, 58753._pm_r,  24._pm_r, 77._pm_r, 1.84_pm_r,293._pm_r,   5._pm_r, 84._pm_r,  .93_pm_r,334._pm_r, &
233.10_pm_r, 62216._pm_r,  21._pm_r, 74._pm_r, 2.24_pm_r,274._pm_r,   4._pm_r, 66._pm_r,  .95_pm_r,327._pm_r, &
227.80_pm_r, 65589._pm_r,  18._pm_r, 71._pm_r, 2.30_pm_r,271._pm_r,   4._pm_r, 49._pm_r,  .76_pm_r,329._pm_r, &
223.30_pm_r, 68892._pm_r,  15._pm_r, 66._pm_r, 2.11_pm_r,276._pm_r,   5._pm_r, 38._pm_r,  .48_pm_r,339._pm_r, &
220.10_pm_r, 72137._pm_r,  13._pm_r, 58._pm_r, 1.88_pm_r,286._pm_r,   5._pm_r, 34._pm_r,  .28_pm_r, 19._pm_r, &
217.60_pm_r, 75343._pm_r,  11._pm_r, 48._pm_r, 1.72_pm_r,297._pm_r,   5._pm_r, 35._pm_r,  .32_pm_r, 70._pm_r, &
215.80_pm_r, 78514._pm_r,  11._pm_r, 35._pm_r, 1.64_pm_r,307._pm_r,   6._pm_r, 39._pm_r,  .44_pm_r, 91._pm_r, &
212.30_pm_r, 81661._pm_r,  11._pm_r, 23._pm_r, 1.53_pm_r,314._pm_r,   6._pm_r, 45._pm_r,  .52_pm_r, 97._pm_r, &
205.00_pm_r, 84721._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
196.00_pm_r, 87648._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.10_pm_r, 90462._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.10_pm_r, 93229._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
187.10_pm_r, 95993._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.40_pm_r, 98792._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
195.00_pm_r,101675._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
207.50_pm_r,104726._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
232.90_pm_r,108108._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
287.40_pm_r,112158._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
369.30_pm_r,117416._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_50= (/ &
281.40_pm_r,   -69._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
256.80_pm_r,  3867._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
232.60_pm_r,  7446._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.70_pm_r, 10747._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
216.70_pm_r, 13931._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
216.10_pm_r, 17100._pm_r,  12._pm_r,276._pm_r, 3.45_pm_r,183._pm_r,   8._pm_r, 34._pm_r, 1.44_pm_r,300._pm_r, &
215.50_pm_r, 20260._pm_r,  13._pm_r,248._pm_r, 5.14_pm_r,176._pm_r,   8._pm_r, 16._pm_r, 1.92_pm_r,294._pm_r, &
215.20_pm_r, 23414._pm_r,  18._pm_r,219._pm_r, 6.66_pm_r,168._pm_r,   9._pm_r,356._pm_r, 2.08_pm_r,284._pm_r, &
213.90_pm_r, 26557._pm_r,  25._pm_r,199._pm_r, 7.64_pm_r,159._pm_r,  10._pm_r,339._pm_r, 1.88_pm_r,267._pm_r, &
215.00_pm_r, 29695._pm_r,  34._pm_r,185._pm_r, 7.85_pm_r,147._pm_r,  10._pm_r,325._pm_r, 1.60_pm_r,234._pm_r, &
219.30_pm_r, 32870._pm_r,  43._pm_r,174._pm_r, 7.25_pm_r,134._pm_r,  10._pm_r,312._pm_r, 1.73_pm_r,194._pm_r, &
227.50_pm_r, 36140._pm_r,  50._pm_r,166._pm_r, 5.82_pm_r,118._pm_r,   8._pm_r,298._pm_r, 2.05_pm_r,166._pm_r, &
238.20_pm_r, 39546._pm_r,  54._pm_r,160._pm_r, 4.14_pm_r, 98._pm_r,   6._pm_r,278._pm_r, 2.21_pm_r,146._pm_r, &
248.90_pm_r, 43119._pm_r,  56._pm_r,155._pm_r, 3.23_pm_r, 82._pm_r,   4._pm_r,248._pm_r, 2.06_pm_r,127._pm_r, &
255.80_pm_r, 46819._pm_r,  57._pm_r,150._pm_r, 3.32_pm_r, 68._pm_r,   3._pm_r,200._pm_r, 1.87_pm_r,112._pm_r, &
256.20_pm_r, 50576._pm_r,  57._pm_r,145._pm_r, 3.93_pm_r, 54._pm_r,   4._pm_r,160._pm_r, 1.37_pm_r,101._pm_r, &
250.40_pm_r, 54292._pm_r,  57._pm_r,138._pm_r, 4.55_pm_r, 40._pm_r,   5._pm_r,144._pm_r,  .50_pm_r, 85._pm_r, &
243.10_pm_r, 57903._pm_r,  55._pm_r,132._pm_r, 4.33_pm_r, 13._pm_r,   4._pm_r,139._pm_r,  .45_pm_r,338._pm_r, &
236.50_pm_r, 61414._pm_r,  51._pm_r,127._pm_r, 4.38_pm_r,350._pm_r,   3._pm_r,137._pm_r,  .89_pm_r,322._pm_r, &
231.90_pm_r, 64841._pm_r,  46._pm_r,122._pm_r, 4.36_pm_r,333._pm_r,   2._pm_r,134._pm_r,  .99_pm_r,323._pm_r, &
228.20_pm_r, 68211._pm_r,  40._pm_r,119._pm_r, 4.22_pm_r,321._pm_r,   1._pm_r,108._pm_r,  .89_pm_r,332._pm_r, &
225.10_pm_r, 71528._pm_r,  34._pm_r,115._pm_r, 4.00_pm_r,312._pm_r,   1._pm_r, 19._pm_r,  .73_pm_r,349._pm_r, &
222.60_pm_r, 74806._pm_r,  29._pm_r,113._pm_r, 3.79_pm_r,304._pm_r,   2._pm_r,  9._pm_r,  .65_pm_r, 11._pm_r, &
220.50_pm_r, 78052._pm_r,  24._pm_r,111._pm_r, 3.56_pm_r,299._pm_r,   3._pm_r, 13._pm_r,  .64_pm_r, 29._pm_r, &
217.00_pm_r, 81265._pm_r,  19._pm_r,110._pm_r, 3.26_pm_r,294._pm_r,   4._pm_r, 18._pm_r,  .64_pm_r, 42._pm_r, &
209.80_pm_r, 84393._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.50_pm_r, 87394._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
192.20_pm_r, 90261._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
189.10_pm_r, 93061._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
188.90_pm_r, 95849._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
191.90_pm_r, 98678._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
199.00_pm_r,101605._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
212.50_pm_r,104723._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
237.50_pm_r,108179._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
286.80_pm_r,112264._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
366.50_pm_r,117478._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_60= (/ &
272.30_pm_r,   -80._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
251.10_pm_r,  3745._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
228.20_pm_r,  7247._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.90_pm_r, 10514._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.20_pm_r, 13708._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
217.00_pm_r, 16897._pm_r,  16._pm_r,279._pm_r, 4.62_pm_r,209._pm_r,  11._pm_r, 29._pm_r, 1.22_pm_r,324._pm_r, &
215.20_pm_r, 20063._pm_r,  20._pm_r,255._pm_r, 7.05_pm_r,202._pm_r,  12._pm_r, 19._pm_r, 1.70_pm_r,319._pm_r, &
212.80_pm_r, 23197._pm_r,  28._pm_r,234._pm_r, 9.45_pm_r,193._pm_r,  13._pm_r,  8._pm_r, 1.95_pm_r,310._pm_r, &
209.40_pm_r, 26289._pm_r,  40._pm_r,218._pm_r,10.90_pm_r,181._pm_r,  14._pm_r,358._pm_r, 1.80_pm_r,296._pm_r, &
209.40_pm_r, 29351._pm_r,  53._pm_r,206._pm_r,11.50_pm_r,168._pm_r,  15._pm_r,350._pm_r, 1.50_pm_r,267._pm_r, &
213.70_pm_r, 32445._pm_r,  66._pm_r,195._pm_r,11.20_pm_r,153._pm_r,  15._pm_r,342._pm_r, 1.52_pm_r,228._pm_r, &
221.60_pm_r, 35632._pm_r,  77._pm_r,186._pm_r, 9.90_pm_r,138._pm_r,  14._pm_r,334._pm_r, 1.80_pm_r,199._pm_r, &
231.80_pm_r, 38947._pm_r,  85._pm_r,179._pm_r, 8.08_pm_r,121._pm_r,  11._pm_r,325._pm_r, 1.97_pm_r,182._pm_r, &
243.20_pm_r, 42431._pm_r,  90._pm_r,173._pm_r, 6.59_pm_r,110._pm_r,   9._pm_r,317._pm_r, 1.84_pm_r,167._pm_r, &
252.90_pm_r, 46064._pm_r,  94._pm_r,168._pm_r, 6.20_pm_r, 96._pm_r,   7._pm_r,308._pm_r, 1.62_pm_r,154._pm_r, &
256.60_pm_r, 49805._pm_r,  96._pm_r,162._pm_r, 6.62_pm_r, 77._pm_r,   5._pm_r,298._pm_r, 1.29_pm_r,145._pm_r, &
252.10_pm_r, 53538._pm_r,  95._pm_r,156._pm_r, 7.53_pm_r, 54._pm_r,   4._pm_r,288._pm_r,  .78_pm_r,140._pm_r, &
246.50_pm_r, 57188._pm_r,  91._pm_r,150._pm_r, 7.62_pm_r, 28._pm_r,   3._pm_r,281._pm_r,  .28_pm_r,116._pm_r, &
241.50_pm_r, 60762._pm_r,  84._pm_r,144._pm_r, 7.81_pm_r,  8._pm_r,   3._pm_r,285._pm_r,  .28_pm_r, 19._pm_r, &
237.40_pm_r, 64266._pm_r,  75._pm_r,139._pm_r, 7.58_pm_r,354._pm_r,   3._pm_r,297._pm_r,  .51_pm_r,359._pm_r, &
233.30_pm_r, 67713._pm_r,  66._pm_r,135._pm_r, 7.03_pm_r,342._pm_r,   3._pm_r,309._pm_r,  .60_pm_r,359._pm_r, &
230.10_pm_r, 71104._pm_r,  57._pm_r,131._pm_r, 6.36_pm_r,331._pm_r,   4._pm_r,319._pm_r,  .58_pm_r,360._pm_r, &
227.10_pm_r, 74454._pm_r,  48._pm_r,128._pm_r, 5.80_pm_r,320._pm_r,   5._pm_r,326._pm_r,  .52_pm_r,  0._pm_r, &
223.90_pm_r, 77752._pm_r,  40._pm_r,127._pm_r, 5.32_pm_r,312._pm_r,   5._pm_r,330._pm_r,  .45_pm_r,  3._pm_r, &
220.40_pm_r, 81009._pm_r,  33._pm_r,126._pm_r, 4.82_pm_r,305._pm_r,   6._pm_r,334._pm_r,  .37_pm_r,  3._pm_r, &
213.90_pm_r, 84190._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
204.80_pm_r, 87260._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
195.80_pm_r, 90185._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
191.50_pm_r, 93027._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
191.40_pm_r, 95849._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
194.90_pm_r, 98715._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
203.30_pm_r,101696._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
217.80_pm_r,104885._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
241.90_pm_r,108415._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
285.20_pm_r,112524._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
363.90_pm_r,117687._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_70= (/ &
261.30_pm_r,    -3._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
246.60_pm_r,  3706._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.00_pm_r,  7149._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
218.10_pm_r, 10385._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
217.30_pm_r, 13564._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
215.20_pm_r, 16733._pm_r,  13._pm_r,287._pm_r, 5.27_pm_r,224._pm_r,   8._pm_r, 18._pm_r,  .62_pm_r,  9._pm_r, &
212.30_pm_r, 19864._pm_r,  19._pm_r,260._pm_r, 7.78_pm_r,218._pm_r,   9._pm_r, 17._pm_r,  .88_pm_r,  4._pm_r, &
209.40_pm_r, 22952._pm_r,  29._pm_r,241._pm_r, 9.94_pm_r,208._pm_r,  11._pm_r, 15._pm_r, 1.02_pm_r,355._pm_r, &
204.80_pm_r, 25986._pm_r,  42._pm_r,228._pm_r,10.99_pm_r,196._pm_r,  12._pm_r, 12._pm_r,  .96_pm_r,340._pm_r, &
204.40_pm_r, 28976._pm_r,  56._pm_r,217._pm_r,10.96_pm_r,178._pm_r,  13._pm_r,  8._pm_r,  .80_pm_r,310._pm_r, &
208.40_pm_r, 31995._pm_r,  67._pm_r,207._pm_r,10.80_pm_r,159._pm_r,  13._pm_r,  3._pm_r,  .85_pm_r,270._pm_r, &
215.60_pm_r, 35099._pm_r,  77._pm_r,197._pm_r,10.31_pm_r,140._pm_r,  13._pm_r,357._pm_r, 1.06_pm_r,241._pm_r, &
225.90_pm_r, 38326._pm_r,  84._pm_r,188._pm_r, 9.17_pm_r,125._pm_r,  12._pm_r,351._pm_r, 1.20_pm_r,224._pm_r, &
240.00_pm_r, 41742._pm_r,  89._pm_r,181._pm_r, 7.24_pm_r,118._pm_r,  11._pm_r,344._pm_r, 1.20_pm_r,203._pm_r, &
253.30_pm_r, 45352._pm_r,  93._pm_r,176._pm_r, 6.14_pm_r,106._pm_r,   9._pm_r,339._pm_r, 1.17_pm_r,188._pm_r, &
260.00_pm_r, 49124._pm_r,  95._pm_r,170._pm_r, 6.06_pm_r, 82._pm_r,   8._pm_r,334._pm_r,  .93_pm_r,182._pm_r, &
256.70_pm_r, 52916._pm_r,  94._pm_r,165._pm_r, 7.19_pm_r, 54._pm_r,   7._pm_r,330._pm_r,  .44_pm_r,189._pm_r, &
251.20_pm_r, 56635._pm_r,  88._pm_r,159._pm_r, 7.50_pm_r, 32._pm_r,   7._pm_r,328._pm_r,  .11_pm_r,301._pm_r, &
244.70_pm_r, 60269._pm_r,  81._pm_r,153._pm_r, 7.56_pm_r, 16._pm_r,   7._pm_r,329._pm_r,  .40_pm_r,353._pm_r, &
238.10_pm_r, 63801._pm_r,  73._pm_r,148._pm_r, 7.08_pm_r,  4._pm_r,   8._pm_r,331._pm_r,  .55_pm_r,  5._pm_r, &
232.90_pm_r, 67250._pm_r,  64._pm_r,144._pm_r, 6.34_pm_r,354._pm_r,   9._pm_r,335._pm_r,  .58_pm_r, 16._pm_r, &
229.40_pm_r, 70633._pm_r,  57._pm_r,140._pm_r, 5.57_pm_r,344._pm_r,   9._pm_r,339._pm_r,  .54_pm_r, 29._pm_r, &
227.40_pm_r, 73976._pm_r,  49._pm_r,137._pm_r, 4.97_pm_r,333._pm_r,  10._pm_r,342._pm_r,  .51_pm_r, 45._pm_r, &
226.50_pm_r, 77298._pm_r,  43._pm_r,135._pm_r, 4.50_pm_r,324._pm_r,  10._pm_r,346._pm_r,  .49_pm_r, 57._pm_r, &
224.10_pm_r, 80609._pm_r,  36._pm_r,134._pm_r, 4.05_pm_r,316._pm_r,  10._pm_r,350._pm_r,  .47_pm_r, 67._pm_r, &
217.70_pm_r, 83844._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
208.50_pm_r, 86971._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
199.00_pm_r, 89948._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
194.00_pm_r, 92829._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
193.90_pm_r, 95686._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
197.90_pm_r, 98589._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
207.20_pm_r,101619._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
222.30_pm_r,104871._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
245.10_pm_r,108460._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
283.10_pm_r,112578._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
361.80_pm_r,117690._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_r), dimension(10*pm_nb_alt), parameter :: donnees_lat_80= (/ &
255.70_pm_r,    53._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
243.80_pm_r,  3699._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
222.90_pm_r,  7105._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
217.00_pm_r, 10316._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
215.70_pm_r, 13474._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
212.50_pm_r, 16611._pm_r,   7._pm_r,283._pm_r, 3.42_pm_r,232._pm_r,   4._pm_r, 12._pm_r,  .21_pm_r, 98._pm_r, &
208.90_pm_r, 19697._pm_r,  12._pm_r,259._pm_r, 4.97_pm_r,226._pm_r,   4._pm_r, 18._pm_r,  .28_pm_r, 92._pm_r, &
206.20_pm_r, 22738._pm_r,  19._pm_r,244._pm_r, 6.24_pm_r,217._pm_r,   4._pm_r, 23._pm_r,  .31_pm_r, 79._pm_r, &
200.50_pm_r, 25719._pm_r,  28._pm_r,233._pm_r, 6.73_pm_r,204._pm_r,   5._pm_r, 27._pm_r,  .27_pm_r, 54._pm_r, &
199.60_pm_r, 28640._pm_r,  36._pm_r,223._pm_r, 6.44_pm_r,184._pm_r,   5._pm_r, 27._pm_r,  .25_pm_r,  9._pm_r, &
203.40_pm_r, 31587._pm_r,  42._pm_r,213._pm_r, 6.37_pm_r,161._pm_r,   5._pm_r, 24._pm_r,  .34_pm_r,330._pm_r, &
210.40_pm_r, 34615._pm_r,  47._pm_r,203._pm_r, 6.36_pm_r,141._pm_r,   5._pm_r, 19._pm_r,  .42_pm_r,306._pm_r, &
221.30_pm_r, 37769._pm_r,  51._pm_r,194._pm_r, 5.90_pm_r,126._pm_r,   6._pm_r, 13._pm_r,  .44_pm_r,286._pm_r, &
237.30_pm_r, 41132._pm_r,  54._pm_r,186._pm_r, 4.44_pm_r,117._pm_r,   6._pm_r,  8._pm_r,  .20_pm_r,257._pm_r, &
252.80_pm_r, 44718._pm_r,  55._pm_r,180._pm_r, 3.52_pm_r, 97._pm_r,   5._pm_r,  6._pm_r,  .14_pm_r,201._pm_r, &
261.60_pm_r, 48501._pm_r,  55._pm_r,175._pm_r, 3.68_pm_r, 67._pm_r,   5._pm_r,  6._pm_r,  .19_pm_r,206._pm_r, &
259.10_pm_r, 52322._pm_r,  52._pm_r,169._pm_r, 4.66_pm_r, 43._pm_r,   5._pm_r,  3._pm_r,  .35_pm_r,226._pm_r, &
254.10_pm_r, 56082._pm_r,  48._pm_r,164._pm_r, 4.58_pm_r, 29._pm_r,   4._pm_r,357._pm_r,  .50_pm_r,234._pm_r, &
246.80_pm_r, 59754._pm_r,  43._pm_r,158._pm_r, 4.21_pm_r, 17._pm_r,   4._pm_r,348._pm_r,  .52_pm_r,238._pm_r, &
238.90_pm_r, 63309._pm_r,  38._pm_r,153._pm_r, 3.65_pm_r,  7._pm_r,   4._pm_r,337._pm_r,  .44_pm_r,243._pm_r, &
233.10_pm_r, 66765._pm_r,  34._pm_r,149._pm_r, 3.07_pm_r,357._pm_r,   4._pm_r,329._pm_r,  .32_pm_r,256._pm_r, &
229.40_pm_r, 70148._pm_r,  30._pm_r,146._pm_r, 2.58_pm_r,348._pm_r,   4._pm_r,324._pm_r,  .21_pm_r,281._pm_r, &
227.90_pm_r, 73494._pm_r,  27._pm_r,144._pm_r, 2.21_pm_r,337._pm_r,   4._pm_r,323._pm_r,  .19_pm_r,325._pm_r, &
228.30_pm_r, 76834._pm_r,  24._pm_r,143._pm_r, 1.95_pm_r,327._pm_r,   5._pm_r,325._pm_r,  .25_pm_r,354._pm_r, &
226.60_pm_r, 80182._pm_r,  21._pm_r,143._pm_r, 1.73_pm_r,318._pm_r,   5._pm_r,327._pm_r,  .29_pm_r,  8._pm_r, &
220.40_pm_r, 83453._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
211.10_pm_r, 86618._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
201.20_pm_r, 89631._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
195.90_pm_r, 92539._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
195.70_pm_r, 95422._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
200.10_pm_r, 98352._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
209.90_pm_r,101418._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
225.20_pm_r,104711._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
246.80_pm_r,108338._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
281.10_pm_r,112453._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r, &
360.30_pm_r,117528._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r,   0._pm_r,  0._pm_r,  .00_pm_r,  0._pm_r/)

  real(pm_reel), dimension(10*pm_nb_lat*pm_nb_alt), parameter :: donnees_novembre = &
       (/donnees_lat_moins_80(:),donnees_lat_moins_70(:),donnees_lat_moins_60(:),donnees_lat_moins_50(:),&
       donnees_lat_moins_40(:),donnees_lat_moins_30(:),donnees_lat_moins_20(:),donnees_lat_moins_10(:),&
       donnees_lat_0(:),donnees_lat_10(:),donnees_lat_20(:),donnees_lat_30(:),donnees_lat_40(:),donnees_lat_50(:),&
       donnees_lat_60(:),donnees_lat_70(:),donnees_lat_80(:)/)

real(pm_reel),dimension(10,pm_nb_alt,pm_nb_lat)::donnees

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_novembre.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
 donnees=reshape(donnees_novembre,(/10,pm_nb_alt,pm_nb_lat/))
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

end subroutine mpi_atmi_novembre
