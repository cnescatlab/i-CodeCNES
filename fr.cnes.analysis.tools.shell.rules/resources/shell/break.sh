#! /bin/bash
# pause2.sh : appuyer sur une touche pour continuer un script

echo "Appuyer la touche <Entrée> pour continuer..."
read touche
case $touche in
*)	echo "Reprise du script..."
	;;
esac
