#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.CASESWITCH"
echo "Fichier KO de TU"
echo "------------------------------------------"

# --- Extraction des valeurs des parametres
echo "What do you prefer: beach or moutain"
read res
case "$res" in
 "beach")
   echo "Let's go surfing"
 ;;
 "mountain" )
  echo "Let's go skiing"
 ;;
esac

caseFunction ()
{
  print "1) sub menu"
  print "2) nothing"

  while true; do
    read SELECT
    case $SELECT in
      1)    print "a) option a"
            print "b) option b"
            while read SELECT2;do
              case $SELECT2 in
                "a") print "A selected";;
                "b") print "B selected";;
              esac
              break 2
            done;;
      2) print "2 selected";;
    esac
    break
  done
}

# --- Extraction des valeurs des parametres
echo "What do you prefer: town or country"
read res
case "$res" in
 "town")
   echo "Let's go to the cinema"
 ;;
 "country" )
  echo "Let's go for a walk"
 ;;
esac
