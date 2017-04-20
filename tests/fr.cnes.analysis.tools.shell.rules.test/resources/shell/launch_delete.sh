# Script : Launch_delete.sh
# Author: Arnaud Poulain
# Date : 25/08/2014
############################################################################################
DayToDelete=$(date -d "1 month ago" "+%Y%m%d")
echo "$(date)  -------  Lauch of TMArchive   --------"
bash /GFS_SAGA/DPCCTools/TarTools/Delete_TMArchive.sh $DayToDelete
echo "$(date) -------- Lauch of Daily Nominal  ------"
bash /GFS_SAGA/DPCCTools/TarTools/Delete_daily_nominal.sh $DayToDelete
echo "$(date) -------- Lauch of Daily Backup   ------"
bash /GFS_SAGA/DPCCTools/TarTools/Delete_daily_backup.sh $DayToDelete

