#!/bin/ksh

# shell de lancement de la version 3.2 de PFD qui int�gre CASTLE
# pour les crontabs de risques de collision

echo " "
date
echo " "
echo "Shell de calcul de risques de collision automatique - V2.0"
echo "********************************************************"
echo " "

if [ `uname -r | cut -c3-3` -ne 1 ] ; then
  echo "ce script fonctionne en solaris 10"
  exit
fi


# --------------------------------------------
## INITIALISATIONS
# --------------------------------------------

echo " "
echo " ------------------------------ "
echo "        INITIALISATIONS "
echo " ------------------------------ "
echo " "

## satellites CNES � traiter
#
echo "Choix de la famille � traiter : 1=spot-helios / 2= mini-micro / 3=geo / 4=ISS "
read choix

if [ $choix -eq 1 ] ; then
  #  listenum="123 45 654 242 75 "
  #du bla bla 
  listenum="123 456 789 "
  echo "===> calcul des risques pour S2, S4, S5 "
#  listenum="789"
#  echo "===> calcul des risques pour S5"
  rep=spot_helios
  seuilDis=10.0

elif [ $choix -eq 2 ] ; then
  # listenum="456 456 78 67 46 27 42 53 12 78"
  #  ajout de blabla  
  listenum="987 654 321 741 852 963 147 258"
  echo "===> calcul des risques pour DMT, COR, CAL, PAR, JA1, JA2, SMS, PIC"
#  listenum="987"
# echo "===> calcul des risques pour DMT"
  rep=minimicro
  seuilDis=10.0
  
elif [ $choix -eq 3 ] ; then
  listenum="369 159"
  echo "===> calcul des risques pour TC2C TC2D"
  rep=geo
  seuilDis=50.0
  
elif [ $choix -eq 4 ] ; then
  listenum="951"
  echo "===> calcul des risques pour ISS"
  rep=iss
  seuilDis=30.0
  
else
  echo "===> Choix non valide"
  stop 0
fi



## repertoires de travail et de donn�es
#
dirbdtle=/users/debrisdon/BD_TLE/NORAD

dirtmp=/users/debrisdon/tmp/crontabs/lanceCollision/$$
mkdir -p $dirtmp

# version corrig�e de la v3.2.1 d'un bug pour castle .. non officiel ..
direxepfd=/opt/ms/PFDARC/V3.3.1

dircollision=/users/debrisdon/COLLISIONS/$rep

dircron=/users/debrisdon/crontabs/shells

\rm $dircollision/ARC/arc*.out


############################################################################
##
## PRETRAITEMENT
##
## Lancement de TRICICLE : suppression des tle decayed
##
############################################################################
echo " "
date
echo " "
echo " ------------------------------ "
echo "        PRETRAITEMENT "
echo " ------------------------------ "
echo " "

date +%d_%m_%y | sed -e 's,_, ,g' | awk '{print $1,$2,$3+2000}' | read jj mm aaaa

echo " "
cd $dircollision/TRICICLE
echo "- lancement de TRICICLE ..."
export DIRTRICICLE=$direxepfd/TRICICLE
export PATH=$DIRTRICICLE:$PATH

# - mise � jour du fichier de conf : date courante
date
echo 1 > meca.tmp
echo 2 >> meca.tmp
echo $jj $mm $aaaa 00 00 00 000 >> meca.tmp
echo 0 >> meca.tmp
/users/debrisdon/MECA/meca_shell < meca.tmp > mecaout.tmp
grep "date julienne  cnes" LISTCONVDAT | awk '{print $5,$5-15.0}' | read datejul datejulanc
grep "twolines" LISTCONVDAT | awk '{print $8}' | read numjour
sed -e 's,XXXXDATEANC,'$datejulanc',g'  $dircollision/TRICICLE/data_tricicle/OPT_Routine_modele > $dircollision/TRICICLE/data_tricicle/OPT_Routine

# - r�cup�ration du fichier de TLE du jour 
cp $dirbdtle/liste_tout_coo.2l ./data_tricicle/COO/liste_tout.2l


# - run de tricicle
$DIRTRICICLE/tricicle.exe -i OPT_Routine > tricicle.out

############################################################################
##
## CALCUL DES RAPPROCHEMENTS (crit�re g�om�trique : distance)
##
## Lancement de ARC
##
############################################################################

echo " "
echo " --------------------------------------------- "
echo "        CALCULS DES RAPPROCHEMENTS "
echo " --------------------------------------------- "
echo " "
date

cd $dirtmp
echo "- lancement de ARC ..."
echo " - pr�parations ..." 
export DIRARC=$direxepfd/ARC
. $DIRARC/gen/configure -nogen > $dirtmp/configure.out

# - r�cup�ration du fichier de TLE de TRICICLE
cp $dircollision/TRICICLE/resul_tricicle/pourARC_ficCOOretenus $dircollision/ARC/data_arc/TLE/

# on supprime les tle des satellites que l'on va traiter
# et on fait un fichier s�par� pour les satellites CNES
\rm $dircollision/ARC/data_arc/TLE/TLEcnes
cp $dircollision/TRICICLE/resul_tricicle/pourARC_ficCOOretenus $dircollision/ARC/data_arc/TLE/pourARC_ficCOOretenus_sansCNES

for num in $listenum ; 
do
 grep ^"1 "$num $dircollision/TRICICLE/resul_tricicle/pourARC_ficCOOretenus > tle.tmp
 grep ^"2 "$num $dircollision/TRICICLE/resul_tricicle/pourARC_ficCOOretenus >> tle.tmp
 cat tle.tmp >> $dircollision/ARC/data_arc/TLE/TLEcnes

 grep -v ^"1 "$num $dircollision/ARC/data_arc/TLE/pourARC_ficCOOretenus_sansCNES | grep -v ^"2 "$num > coo.tmp
 mv coo.tmp $dircollision/ARC/data_arc/TLE/pourARC_ficCOOretenus_sansCNES
done

# cas de l'ISS : il faut aussi supprimer les progress et soyuz !
if [ $choix -eq 4 ] ; then

 grep PROGRESS-M $dirbdtle/liste_tout_coo.2l | awk '{print $1}' | sed -e 's,-,,g' | cut -c 3-11 > liste.tmp
 grep SOYUZ-TMA $dirbdtle/liste_tout_coo.2l | awk '{print $1}' | sed -e 's,-,,g' | cut -c 3-11 >> liste.tmp

 wc -l liste.tmp | awk '{print $1}' | read nbliste

 kk=1
 while [ $kk -le $nbliste ] ; 
 do

  head -$kk liste.tmp | tail -1 | read numcospar
  grep $numcospar $dirbdtle/liste_tout_coo.2l | awk '{print $2}' | sed -e 's,U,,g' | read num

  grep -v ^"1 "$num $dircollision/ARC/data_arc/TLE/pourARC_ficCOOretenus_sansCNES | grep -v ^"2 "$num > coo.tmp
  mv coo.tmp $dircollision/ARC/data_arc/TLE/pourARC_ficCOOretenus_sansCNES

  kk=$((kk+1))
 done

fi


# - mise � jour du fichier de conf
sed -e 's,XXXXDATEJUL,'$datejul',g' -e 's,XXXXDATECAL,'$aaaa$mm$jj',g' -e 's,XXXXDIS,'$seuilDis',g' $dircollision/ARC/data_arc/param/ARC_CONF_Routine_modele > $dircollision/ARC/data_arc/param/ARC_CONF_Routine


# - run de arc
cd $dircollision/ARC
echo " - arc.exe ..." 
$DIRARC/arc.exe ARC_CONF_Routine 1> arc.out 2> arc_err.out


# - extraction des r�sultats
echo " - extraction des r�sultats ..." 
ficresarc=$dircollision/ARC/resul_arc/ARC_RES_Routine_$aaaa$mm$jj
ficresume=$dirtmp/resume_arc


cd $dirtmp
grep "* Intervalle de danger" $ficresarc | sed -e 's,\/, ,g' -e 's,W,,g' -e 's,E,,g' | awk '{print $5,$6,$7}' > objet.tmp
grep "Date     :" $ficresarc | awk '{print $3}' > datejul.tmp
grep "Distance :" $ficresarc | awk '{print $3}' > dist.tmp

grep "Position relative" $ficresarc | awk '{print $4,$5,$6}' > tnw.tmp
$dircron/extraitBplane.awk $ficresarc > bplane.tmp

grep "Position (G50)" $ficresarc | awk '{print $3,$4,$5}' > d1.tmp
grep "Vitesse  (G50)" $ficresarc | awk '{print $3,$4,$5}' > d2.tmp
paste d1.tmp d2.tmp | awk '(NR%2==1) {print $0}'> posPg50.tmp
paste d1.tmp d2.tmp | awk '(NR%2==0) {print $0}' > posSg50.tmp

echo "//objetPrimaire objetSecondaire NumOccurrence datejul distmin C1 C2 T N W Xp Yp Zp VXp VYp VZp Xs Ys Zs VXs VYs VZs" > $ficresume
paste objet.tmp datejul.tmp dist.tmp bplane.tmp tnw.tmp posPg50.tmp posSg50.tmp >> $ficresume

############################################################################
##
## CALCULS DES PROBA
##
############################################################################

echo " "
echo " --------------------------------------------- "
echo "        CALCULS DES PROBABILITES "
echo " --------------------------------------------- "
echo " "
date

# compteurs
cpt=0
cpt2=0
cpt3=0
cpt4=0
cpt8=0

cd $dirtmp
# fichier de r�sultat final
#
ficsynthese=$dircollision/ARC/resul_pfdV3.2/$aaaa/synthese_$jj$mm
mkdir -p $dircollision/ARC/resul_pfdV3.2/$aaaa
echo "// Primaire Secondaire NumOccurrence TCAjul distmin C1 C2 proba copule  tpropD rayP rayS Xp Yp Zp Vxp VYp VZp Xs Ys Zs Vxs VYs VZs" > $ficsynthese

## Rapprochements � traiter
grep -v "objet" $ficresume > $dirtmp/alertes.tmp
nl=`wc -l $dirtmp/alertes.tmp | awk '{print $1}'`
echo " "
echo "--> " $nl " rapprochements � traiter"

## factorisation de la preparation des fichiers

cp $dirbdtle/ssr/rayons_RCSmax.2l $dirtmp

# - pour  meca_shell 
echo 1 > $dirtmp/mecain_modele.tmp
echo 5 >> $dirtmp/mecain_modele.tmp
echo XXXDATEDEB >> $dirtmp/mecain_modele.tmp
echo 0 >> $dirtmp/mecain_modele.tmp


## Boucle sur les rapprochements
i=1
date | sed -e 's,:, ,g' | awk '{print $4*3600.0+$5*60.0+$6}' | read tdeb

while [ $i -le $nl ] ; do

cd $dirtmp
 # - r�cup�ration du rapprochement n� i
 head -$i $dirtmp/alertes.tmp | tail -1 | \
 	awk '{print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$20,$21,$22}' | \
	read numsat numdebcourt numocc  datealerte dist C1 C2 T N W  xp yp zp vxp vyp vzp xs ys zs vxs vys vzs
 numdeblong=`echo $((numdebcourt+100000)) | cut -c 2-6`
 echo " " 
 echo "*********** Rapprochement n� " $i " entre " $numsat " et l'objet " $numdebcourt
 echo "" >> $dircollision/ARC/arc_cov.out
 echo "*********** Rapprochement n� " $i " entre " $numsat " et l'objet " $numdebcourt >> $dircollision/ARC/arc_cov.out
 echo "" >> $dircollision/ARC/arc_cov_err.out
 echo "*********** Rapprochement n� " $i " entre " $numsat " et l'objet " $numdebcourt >> $dircollision/ARC/arc_cov_err.out

 # - rayons des objets
 if [ `grep $numdeblong $dirtmp/rayons_RCSmax.2l | wc -l` -eq 0 ] ; then
	rayondeb=10.0
 else
 	grep $numdeblong $dirtmp/rayons_RCSmax.2l | awk '{print $2}' | read rayondeb
 fi
 
 grep $numsat $dirbdtle/ssr/Rayons_Collisions.txt | awk '{print $2}' | read rayonsat
 echo "rayon sat =" $rayonsat "(m) ** rayon deb=" $rayondeb "(m)"
 
 
 # - dur�e de propagation (date du risque - date du TLE utilis�)  
  grep ^"1 "$numdeblong $dircollision/ARC/data_arc/TLE/pourARC_ficCOOretenus_sansCNES | \
  	awk '{print $4}' | read datetledeb

  sed -e 's,XXXDATEDEB,'$datetledeb',g' $dirtmp/mecain_modele.tmp > $dirtmp/mecain.tmp
  /users/debrisdon/MECA/meca_shell < $dirtmp/mecain.tmp > $dirtmp/mecaout.tmp
  
  grep "date jul cnes" $dirtmp/LISTCONVDAT | awk '{printf "%6.2f \n", '$datealerte'-$5+2}' > $dirtmp/tprop.tmp
  head -1 $dirtmp/tprop.tmp  > $dirtmp/tprop2.tmp 
  echo 6.0 >> $dirtmp/tprop2.tmp
  sort -n $dirtmp/tprop2.tmp -o $dirtmp/tprop.tmp
  tail -1 $dirtmp/tprop.tmp  | read tpropagdeb

  echo "	---> " $numdeblong " : " $tpropagdeb "j de propagation" 

# - date de d�but de l'historique : 1 an ou 2 ans si peu de TLE
 echo $numdeblong | cut -c 1-3 | read xxx
 echo $numdeblong | cut -c 4-5 | read yy
 grep ${numdeblong}U $dirbdtle/bd_objet/$xxx/$yy/$yy \
 	| awk '($4>=('$datetledeb'-1000)) {print $4}' | wc -l \
	| awk '{print $1}' | read nbtle 
  if [ $nbtle -lt 300 ] ; then
    histo=800
    cpt8=$((cpt8+1))
    tpropmax=$((tpropagdeb+5))
  else
    histo=365
    tpropmax=$((tpropagdeb+2))
  fi

  echo  $nbtle " TLE sur 1 an - historique pris � " $histo "jours"

# date autour du TCA
echo $datealerte | awk '{printf "%14.8f %d", $1-10.0/1400.0, $1-'$histo'}' | read datejul datedeb

 # - Calcul des probabilit�s
sed -e 's,XXXXDATEJUL,'$datejul',g'  -e 's,XXXXDIS,'$seuilDis',g' \
	-e 's,XXXNUMOBJP,'$numsat',g' -e 's,XXXNUMDEB,'$numdeblong',g' \
	-e 's,XXXT2,'$tpropmax',g' -e 's,XXXDATEDEB,'$datedeb',g' \
	-e 's,XXXRAYSAT,'$rayonsat',g' -e 's,XXXRAYDEB,'$rayondeb',g' \
	$dircollision/ARC/data_arc/param/ARC_CONF_RoutineCov_modele > $dircollision/ARC/data_arc/param/ARC_CONF_RoutineCov

echo " - arc.exe ..." 
cd $dircollision/ARC
$DIRARC/arc.exe ARC_CONF_RoutineCov 1>> arc_cov.out 2>> arc_cov_err.out
date | sed -e 's,:, ,g' | awk '{print $4*3600.0+$5*60.0+$6}' | read tfin

# - extraction des r�sultats
ficresarc=$dircollision/ARC/resul_arc/ARC_RES_RoutineCov_tmp


cd $dirtmp
if [ `grep "Probabilit� Castle" $ficresarc | wc -l` -eq 1 ];  then
   grep "Probabilit� Castle" $ficresarc | awk '{print $4}'  | read proba
   grep "Densit� de copule" $ficresarc | awk '{print $5}' | read copule
else 
  proba=-10
  copule=-10
fi

if [ `grep "Probabilit� approch�e" $ficresarc | tail -1 | wc -l` -eq 1 ];  then
  grep "Probabilit� approch�e" $ficresarc | tail -1 | awk '{print $4}' | read proba
  copule=-50
fi

echo $numsat $numdebcourt $numocc $datealerte $dist $C1 $C2 \
	$proba $copule $tpropagdeb \
	$rayonsat $raysondeb $xp $yp $zp $vxp $vyp $vzp $xs $ys $zs $vxs $vys $vzs \
	>> $ficsynthese

echo "proba= " $proba
echo "copule= " $copule" (si -50 => proba secours)"

echo "C1=" $C1 " **  C2= "$C2

if [ `grep Temps  $ficresarc | wc -l` -eq 0 ] ; then
  echo "probleme � l'execution : run non fini proprement" 
  cpt2=$((cpt2+1))
else 
  grep "Temps d'ex�cution" $ficresarc | awk '{print "cpu arc =",$5}'
fi

echo $tfin $tdeb | awk '{print($1-$2)}' | read dt
tdeb=$tfin

echo "Temp d'ex�cution r�el (sec)" $dt
if [ $dt -gt 240 ]; then
cpt4=$((cpt4+1))
fi

taille=`du -ks /users/debrisdon/corefiles/* | awk '{print $1}' | tail -1`
if [ $taille -ne 0 ]; then
  echo "coredump g�n�r�"
  \rm /users/debrisdon/corefiles/*
  cpt=$((cpt+1))
fi


# rapprochement suivant
 i=$((i+1))

done 

############################################################################
##
## Stat
##
############################################################################
echo " "
echo " -----------------------"
echo "      Statistiques "
echo " ----------------------- "
echo " "

echo $cpt "cores g�n�r�s sur " $nl " cas"
echo $cpt2 "runs non finis proprement "
echo $cpt4 "runs plus longs que 4 min "

awk '($8==-2) {print $1}' $ficsynthese | wc -l | awk '{print $1}' | read cpt3
echo $cpt3 "cas  � proba -2"

awk '($8==-10) {print $1}' $ficsynthese | wc -l | awk '{print $1}' | read cpt4
echo $cpt4 "cas avec plantage"


awk '($9==-50) {print $1}' $ficsynthese | wc -l | awk '{print $1}' | read cpt5
echo $cpt5 "cas avec calcul castle non valide"
echo $cpt8 "cas avec moins de 150 TLE/an"

cd $dirtmp
echo "nombre de proba castle > 1E-8 et proba castle max (hors proba secours)"
grep -v "P" $ficsynthese  | awk '($9>0.0) {print $0}' > res
scilab -f ${dircron}_V3.2/stat.sci -nw 1> aa.tmp 2>>aa.tmp
cat stat.dat

############################################################################
##
## MENAGE
##
############################################################################
echo " "
echo " -----------------------"
echo "     FIN : MENAGE "
echo " ----------------------- "
echo " "
date

cd $dircollision/ARC
\rm -rf $dirtmp
\rm resul_arc/ARC_RES_Routine_* resul_arc/ARC_TLE_REJET_Routine_*

