#!/bin/ksh
export PATH=~/bin:~/$(uname)/bin:$PATH

#######################################################################
##
## Shell de mise à jour des BD TLE NORAD et GRAVES
## situées sous debrisdon
## a partir des données COO et Spacetrack
## 
## V2.0 : mixage des majbd COO et Spacetrack en une seule BD
## V3.0 : modif suite à la perte des donnees spacetrack depuis HTTPS (11/04/2011)
#######################################################################

echo " ********************************************************************** "
echo "   Shell de mise à jour de la base de donnée TLE sur debrisdon "
echo "   			V3.0 du 06/05/2011 "
echo " ********************************************************************** "
date

#########################################################################
##
## INITIALISATIONS
##
#########################################################################


# Choix de la base de données à traiter
echo " "
echo " Choix de la BD à mettre à jour : 1=NORAD, 2=GRAVES "
read choix
echo " "

## répertoire temporaire
dirtmp=$HOME/tmp/crontabs/majbd2l_$choix/$$
mkdir -p $dirtmp
cd $dirtmp

# Récupération de la date du jour courant
echo 1 > meca.tmp
echo 2 >> meca.tmp
aa=$(date +"%Y")
date +"%d %m %Y %H %M %S 000" >> meca.tmp
echo 0 >> meca.tmp
meca_shell < meca.tmp > mecaout.tmp
grep twolines LISTCONVDAT | awk '{print $8,$10}' | read numjour dateacq
echo "@ "$dateacq > dateacq.tmp
echo " -> date d'acquisition : " $numjour $dateacq
echo " "


#########################################################################
##
## Cas de la BD GRAVES : données COO
##
#########################################################################
if [ choix -eq 2 ] ; then

# --------------------------------------------------------------
# 0 - initialisations
# --------------------------------------------------------------
fictle=liste_tout_graves.2l
dirbdtle=$HOME/BD_TLE/GRAVES
mkdir -p $dirbdtle/bd_jour_coo/$aa

echo " ===> Base de donnée GRAVES sous " $dirbdtle
echo " "

# --------------------------------------------------------------
# 1 - récupération du fichier COO du jour
# --------------------------------------------------------------
echo " - récupération fichier COO ..."
echo " "

echo "user xxxx" > cmd.tmp
echo "cd twolines" >> cmd.tmp
echo "get " $fictle >> cmd.tmp
echo "bye" >> cmd.tmp

ftp -n coo1 < cmd.tmp

# vérification que le fichier est nouveau
flagnew=0
diff $fictle $dirbdtle/$fictle | wc -l | awk '{print $1}' | read flagnew

if [ $flagnew -eq 0 ] ; then
  echo "  ---> Fichier identique au précédent : pas de traitement à faire"
  echo " "
else
  cp $fictle $dirbdtle/
fi


# --------------------------------------------------------------
# 2 - archivage du fichier du jour tel quel
# --------------------------------------------------------------
echo " - archivage fichier COO dans la BD jour ..."
echo " "
cp $fictle $dirbdtle/bd_jour_coo/$aa/$numjour.2l

# --------------------------------------------------------------
# 3 - archivage des TLE par objet avec la date d'acquisition
# --------------------------------------------------------------
if [ $flagnew -ne 0 ] ; then

date
echo " - répartition des TLE par objet dans la BD ..."
echo " "


grep ^1 $fictle > l1.tmp
grep ^2 $fictle > l2.tmp
wc -l l1.tmp | awk '{print $1}' | read nbobj
echo " -> " $nbobj " TLE à traiter"

awk '{print $2}' l2.tmp | cut -c1-3 > xxx.tmp
awk '{print $2}' l2.tmp | cut -c4-5 > yy.tmp
paste xxx.tmp yy.tmp > xxx_yy.tmp


i=1
cpt=0
while [ $i -le $nbobj ] ; do

 head -$i l1.tmp | tail -1 > tle.tmp
 head -$i l2.tmp | tail -1 >> tle.tmp

 head -$i xxx_yy.tmp | tail -1 | read n1 n2

 mkdir -p $dirbdtle/bd_objet/$n1/$n2
 
 
 grep ^1 tle.tmp | read chaine
 grep "$chaine" $dirbdtle/bd_objet/$n1/$n2/$n2 | wc -l | read flagdoublon
 if [ $flagdoublon -eq 0 ] ; then
   cat dateacq.tmp tle.tmp >> $dirbdtle/bd_objet/$n1/$n2/$n2
   cpt=$((cpt+1))
 fi

 i=$((i+1))

done

echo ""
echo "---> " $cpt " nouveaux TLE ajoutés"
echo ""

fi

#########################################################################
##
## Cas de la BD NORAD : données COO , plus de donnees spacetrack depuis HTTPS (11/04/2011)
##
#########################################################################
elif [ choix -eq 1 ] ; then

# --------------------------------------------------------------
# 0 - initialisations
# --------------------------------------------------------------
dirbdtle=$HOME/BD_TLE/NORAD
mkdir -p $dirbdtle/bd_jour_coo/$aa
mkdir -p $dirbdtle/bd_jour_spacetrack/$aa

echo " ===> Base de donnée NORAD sous " $dirbdtle
echo " "

# --------------------------------------------------------------
# 1 - récupération du fichier COO du jour
# --------------------------------------------------------------
echo " - récupération fichier COO ..."
echo " "

echo "user xxxxx" > cmd.tmp
echo "cd twolines" >> cmd.tmp
echo "get liste_tout.2l" >> cmd.tmp
echo "bye" >> cmd.tmp

ftp -n coo1 < cmd.tmp
cp liste_tout.2l $dirbdtle/liste_tout_coo.2l

\rm  $dirbdtle/liste_tout.2l
ln -s $dirbdtle/liste_tout_coo.2l $dirbdtle/liste_tout.2l

# archivage
cp $dirbdtle/liste_tout_coo.2l $dirbdtle/bd_jour_coo/$aa/$numjour.2l

chmod 644 $dirbdtle/bd_jour_coo/$aa/$numjour.2l
  
# --------------------------------------------------------------
# 3 - archivage des TLE par objet avec la date d'acquisition
#  et les infos ssr en entete depuis le fichier COO
# --------------------------------------------------------------
echo " "
date
echo " - répartition des TLEspacetrack  par objet dans la BD ..."
echo " "

# preparations
#
fictle=$dirbdtle/bd_jour_coo/$aa/$numjour.2l
grep ^1 $fictle > l1.tmp
grep ^2 $fictle > l2.tmp
wc -l l1.tmp | awk '{print $1}' | read nbobj
echo "---> " $nbobj " TLE à traiter ..."
echo ""

awk '{print $2}' l2.tmp | cut -c1-3 > xxx.tmp
awk '{print $2}' l2.tmp | cut -c4-5 > yy.tmp
paste xxx.tmp yy.tmp > xxx_yy.tmp

#
# parcour du fichier coo pour recuperer les nouveaux TLE de chaque objet
#
i=1
cpt=0

while [ $i -le $nbobj ] ; do

## Initalisations
 head -$i l1.tmp | tail -1 > tle.tmp
 head -$i l2.tmp | tail -1 >> tle.tmp
  
 head -$i xxx_yy.tmp | tail -1 | read n1 n2

 mkdir -p $dirbdtle/bd_objet/$n1/$n2

 
## Base de données par objet : vérification que le TLE n'existe pas déjà
 grep ^1 tle.tmp | read chaine
 grep "$chaine" $dirbdtle/bd_objet/$n1/$n2/$n2 | wc -l | read flagdoublon
 grep -n "$chaine" $fictle | awk -F":" '{print $1}' | read ligne_tle
 ligne_entete1=$((${ligne_tle}-3))
 ligne_entete2=$((${ligne_tle}-2))
 ligne_entete3=$((${ligne_tle}-1))
 sed "$ligne_entete3,$ligne_entete3!d" $fictle >  dateacq.tmp

 if [ $flagdoublon -eq 0 ] ; then
   cat dateacq.tmp tle.tmp >> $dirbdtle/bd_objet/$n1/$n2/$n2
   #Mise à jour du fichier entete
   sed "$ligne_entete1,$ligne_entete2!d" $fictle >  $dirbdtle/bd_objet/$n1/$n2/entete.2l_maj
   tail -1 $dirbdtle/bd_objet/$n1/$n2/entete.2l_maj | grep "Pas d'information pour le corps" | wc -l | read flagRCS
   
   if [ $flagRCS -eq 1 ] ; then
     sed "2,2!d" $dirbdtle/bd_objet/$n1/$n2/entete.2l_maj > $dirbdtle/bd_objet/$n1/$n2/entete.2l_maj2
     echo "  RCS : Aucune valeur disponible" >> $dirbdtle/bd_objet/$n1/$n2/entete.2l_maj2
     mv $dirbdtle/bd_objet/$n1/$n2/entete.2l_maj2 $dirbdtle/bd_objet/$n1/$n2/entete.2l_maj    
   fi
   #On ecrase systematiquement l'entete courant par l'entete COO dans bd_objet/$n1/$n2/
   mv  $dirbdtle/bd_objet/$n1/$n2/entete.2l_maj $dirbdtle/bd_objet/$n1/$n2/entete2l.tmp
      
   #On actualise le fichier RCS.txt si il existe et si la valeur est different de la derniere du fichier
   #Auparavant la date etait celle du fichier RCS, dorenavant c'est la date du fichier COO...
   if [ -s $dirbdtle/bd_objet/$n1/$n2/RCS.txt ] ; then   
     tail -1 $dirbdtle/bd_objet/$n1/$n2/RCS.txt | awk '{print $2}' | read old_RCS
     grep "RCS" $dirbdtle/bd_objet/$n1/$n2/entete2l.tmp | awk '{print $8}' | read new_RCS  
     if [ old_RCS -ne new_RCS ] ; then
       echo 1 > meca.tmp2
       echo 5 >> meca.tmp2
       echo $aa$numjour.0 >> meca.tmp2
       echo 0 >> meca.tmp2
       $HOME/MECA/meca_shell < meca.tmp2 > mecaout2.tmp
       grep "date jul cnes" mecaout2.tmp | awk '{print $9}' | read jj2
       echo $jj2 $new_RCS >> $dirbdtle/bd_objet/$n1/$n2/RCS.txt
       \rm mecaout2.tmp meca.tmp2
     fi
   fi        
   cpt=$((cpt+1))
 fi

 if [ $i%500 -eq 0 ] ; then
   echo $i "objets traités"
 fi     

## TLE suivant
 i=$((i+1))

done

echo ""
echo "---> " $cpt " nouveaux TLE ajoutés"
echo ""

# --------------------------------------------------------------
# 4 - fabrication du fichier TLE du jour avec les infos ssr en entete
# --------------------------------------------------------------
date
echo " - extraction du catalogue courant ..."
echo " "

# Récupération de la date du jour courant
aa=$(date +"%Y")
echo 1 > meca.tmp
echo 2 >> meca.tmp
date +"%d %m %Y %H %M %S 000" >> meca.tmp
echo 0 >> meca.tmp
$HOME/MECA/meca_shell < meca.tmp > mecaout.tmp
grep twolines LISTCONVDAT | awk '{print $8,$10}' | read numjour dateacq
echo "@ "$dateacq > dateacq.tmp


#
# filtre sur date d'ancienneté à moins 30 jours
#
grep "date julienne  cnes" LISTCONVDAT | awk '{print $5-30}' | read dateacqM30
echo 1 > meca.tmp
echo 4 >> meca.tmp
echo $dateacqM30 >> meca.tmp
echo 0 >> meca.tmp
$HOME/MECA/meca_shell < meca.tmp > mecaout.tmp
grep twolines LISTCONVDAT | awk '{print $10}' | sed -e 's,\.000000000,,g' | read dateacqM30
echo "dateacqM30 : $dateacqM30"

#
# boucle sur les objets à traiter
#
cpt=0
i=0
for rep in $dirbdtle/bd_objet/* ; 
do 
#  echo "- traitement de " $rep " ..."

  listexx=`ls $rep | sed -e 's,\/, ,g'`
#  echo "=>" $listexx
  for xx in $listexx ; 
  do

	file=$rep/$xx/$xx
	
	# recuperation du dernier TLE
	tail -3 $file > tle.tmp 
	tail -1 tle.tmp | awk '{print $2}' | read numobjlong

	# filtre sur l'ancienneté de la date d'acquisition
	head -1 tle.tmp | awk '((int($2)-'$dateacqM30')>=0) {print $2}' | wc -l | read flagdate 
	
	if [ $flagdate -eq 1 ] ; then
	 
 	 # si non disponibilité des données ssr : 
 	 if [ ! -e $rep/$xx/entete2l.tmp ] ; then
    	   echo "  Pas d'information pour le corps "$numobjlong" !" > entete2l.tmp
    	   echo "  RCS : Aucune valeur disponible" >> entete2l.tmp
   	   mv entete2l.tmp $rep/$xx 
 	 fi
	 
	 # test si objet decayed
	 if [ `grep Decayed $rep/$xx/entete2l.tmp | wc -l` -eq 0 ] ; then
	   echo " " >> liste_tout_spacetrack.2l
	   cat $rep/$xx/entete2l.tmp tle.tmp >> liste_tout_spacetrack.2l
	   cpt=$((cpt+1))
	 else
	   echo $numobjlong "  : objet decayed " >> extraire_listetout.rejet
	 fi
	 
 	else
	 echo $numobjlong "  : date cohérente non trouvée " >> extraire_listetout.rejet
	fi
      
        i=$((i+1))
	if [ $i%500 -eq 0 ] ; then
		echo $i" objets traités -> "$cpt" objets retenus"
	fi

  done
done


echo " ---> " $cpt "  objets dans le catalogue du jour " 


cp liste_tout_spacetrack.2l $dirbdtle
cp extraire_listetout.rejet $dirbdtle

cp $dirbdtle/liste_tout_spacetrack.2l $dirbdtle/bd_jour_spacetrack/$aa/$numjour.2l


#########################################################################
##
## Choix invalide
##
#########################################################################
else
  echo " "
  echo " CHOIX INVALIDE !"
  echo " "
fi  

#########################################################################
##
## MENAGE
##
#########################################################################
cd
\rm -rf $dirtmp
date
