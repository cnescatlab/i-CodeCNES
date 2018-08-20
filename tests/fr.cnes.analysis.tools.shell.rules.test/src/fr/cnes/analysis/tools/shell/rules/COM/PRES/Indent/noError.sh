#!/bin/bash
echo "------------------------------------------"
echo "COM.PRES.INDENT"
echo "Fichier OK de TU"
echo "La indentation du code source est bonne"
echo "------------------------------------------"

[[ $? != 0 ]] && { echo >&2 the required \"message\" command is not in your PATH; exit 1; }

code_erreur_nbargs=-128

# Définition de la variable qui contiendra le code retour (en particulier pour les cas d'erreur avec valeurs negatives)
r_ma_fonction_affine=

help() {
  cat <<EOF

    Usage :
            # $0 {-i} {-byfile} station 'fichier/expr_reguliere_grep a chercher' {[-|=|+]date_recherche}

    OPTIONS :
        -i : ne prend pas en compte la casse des caracteres
             lors de la recherche.

        -byfile : le tri s'effectue par partition, par fichier et enfin par date. Cela permet de
                          retrouver rapidement tous les derniers niveaux de sauvegarde pour un fichier.

        date_recherche : date au format 20040813 pour le 13 aout 2004
                         +date : recherche un fichier partir de la date indiquee
                         -date : recherche un fichier avant la date indiquee
                         =date : recherche un fichier le jour de la date indiquee

EOF
}

# ------------------------------------------------------------------------------------
#    Definition d'une fonction affine ( y = ax + b ) qui doit comporter 3 arguments 
# ------------------------------------------------------------------------------------
ma_fonction_affine () 
{ 
   if [ $# -ne 3 ]
   then
      r_ma_fonction_affine=$code_erreur_nbargs
   else 

#      printf "interface appel fonction ok : p1=%s p2=%s p3=%s\n" $1 $2 $3

      # calcul : y = ax + b
      let y=$1*$2+$3 
#     printf "y=%s\n" $y
      r_ma_fonction_affine=$y
   fi
}


# ------------------------------------------------------------------------------------
#    Definition d'une fonction qui affiche les resultats obtenus 
# ------------------------------------------------------------------------------------
affiche_resultat ()
   if [ $# -ne 2 ]
   then
      printf "Erreur grave dans affiche_resultat : nombre d'arguments incorrects\n"
   else 
      p1=$1
      p2=$2
      if [ $p2 -ge 0 ]
      then
         printf "execution de 'ma_fonction_affine' avec chaine de calcul : %s resultat = %s \n" $p1 $2
      else
         printf "erreur d'execution de 'ma_fonction_affine' avec chaine de calcul : %s code retour = %s\n" $p1 $p2
         printf "   ===>Erreur grave dans ma_fonction_affine : nombre d'arguments incorrects<===\n"
      fi
   fi

a_trouver=$(($RANDOM % 100))
 
echo "entrez un nombre compris entre 0 et 100"
read i
while [ $i -ne $a_trouver ] 
do
        if [ $i -lt $a_trouver ] ; then
                echo "trop petit, entrez un nombre compris entre 0 et 100"
        else
                echo "trop grand, entrez un nombre compris entre 0 et 100"
        fi
        read i
done
echo "bravo, le nombre etait en effet $a_trouver"

# - Traitement du fichier des mouvements, impression de chaque 
# - mouvement avec son type et calcul du solde bancaire
cat mvts.txt | awk 'BEGIN { print "Start AWK command" }
                    /^A/ {print "A---", NR }
                    /^B/ {print "B---", NR }
                    /^C/ {print "C---", NR }
                    /^D/ {print "D---", NR }
                    /^E/ {print "E---", NR }
                    /^F/ {print "F---", NR }
                    /^![A-F]/ {print "Default---", NR }
                    END { print "End AWK command" }'

echo
echo "String operations using \"expr \$string : \" construct"
echo "==================================================="
echo

a=1234zipper5FLIPPER43231

echo "The string being operated upon is \"`expr "$a" : '\(.*\)'`\"."
#     Escaped parentheses grouping operator.            ==  ==

#       ***************************
#+          Escaped parentheses
#+           match a substring
#       ***************************


#  If no escaped parentheses...
#+ then 'expr' converts the string operand to an integer.

echo "Length of \"$a\" is `expr "$a" : '.*'`."   # Length of string

echo "Number of digits at the beginning of \"$a\" is `expr "$a" : '[0-9]*'`."

# ------------------------------------------------------------------------- #

echo

echo "The digits at the beginning of \"$a\" are `expr "$a" : '\([0-9]*\)'`."
#                                                             ==      ==
echo "The first 7 characters of \"$a\" are `expr "$a" : '\(.......\)'`."
#         =====                                          ==       ==
# Again, escaped parentheses force a substring match.
#
echo "The last 7 characters of \"$a\" are `expr "$a" : '.*\(.......\)'`."
#         ====                  end of string operator  ^^
#  (actually means skip over one or more of any characters until specified
#+  substring)

echo


INIT_TAB_AWK=""
# Parameter to initialize awk script.
count_case=0
FILE_PARSE=$1

E_PARAMERR=65

usage()
{
    echo "Usage: letter-count.sh file letters" 2>&1
    # For example:   ./letter-count2.sh filename.txt a b c
    exit $E_PARAMERR  # Not enough arguments passed to script.
}

if [ ! -f "$1" ] ; then
    echo "$1: No such file." 2>&1
    usage                 # Print usage message and exit.
fi 

if [ -z "$2" ] ; then
    echo "$2: No letters specified." 2>&1
    usage
fi 

shift                      # Letters specified.
for letter in `echo $@`    # For each one . . .
do
  INIT_TAB_AWK="$INIT_TAB_AWK tab_search[${count_case}] = \"$letter\"; final_tab[${count_case}] = 0; " 
  # Pass as parameter to awk script below.
  count_case=`expr $count_case + 1`
done

# DEBUG:
# echo $INIT_TAB_AWK;

cat $FILE_PARSE |
# Pipe the target file to the following awk script.

# ----------------------------------------------------------------------------------
# Earlier version of script used:
# awk -v tab_search=0 -v final_tab=0 -v tab=0 -v nb_letter=0 -v chara=0 -v chara2=0 \

awk \
"BEGIN { $INIT_TAB_AWK } \
{ split(\$0, tab, \"\"); \
for (chara in tab) \
{ for (chara2 in tab_search) \
{ if (tab_search[chara2] == tab[chara]) { final_tab[chara2]++ } } } } \
END { for (chara in final_tab) \
{ print tab_search[chara] \" => \" final_tab[chara] } }"
# ----------------------------------------------------------------------------------
#  Nothing all that complicated, just . . .
#+ for-loops, if-tests, and a couple of specialized functions.


# -----------------------------------------------------------------------
# Every file has an inode, a record that holds its physical address info.
# -----------------------------------------------------------------------

echo; echo -n "Are you absolutely sure you want to delete \"$1\" (y/n)? "
# The '-v' option to 'rm' also asks this.
read answer
case "$answer" in
  [nN]) echo "Changed your mind, huh?"
      exit $E_CHANGED_MIND
      ;;
  *)    echo "Deleting file \"$1\".";;
esac

awk '
{nodecount()}  # Execute the user-defined nodecount() for each row to update the node count
$0~/<clusternode /,/<\/clusternode>/{ print } # Print the rows between "<clusternode " and  "</clusternode>"
function nodecount() #Function definition for nodecount
{if($0~/clusternode name/){count+=1}
}
END{print "Node Count="count}' $1 #Print the count value in the END pattern which is executed once


