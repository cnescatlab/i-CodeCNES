Fichier Fortran 77 pour test de programme d'analyse de code.
Edité par Pierre Marighetto.
Version 1 (05/02/2013).

-----------------------------------------------------------------------------------------------------------------------

Ce dossier comporte 3 fichiers :

	- README.txt    		Le présent document qui décrit le contenu du dossier
							et de chacun des fichiers.
	
	- programme_test.f		Un fichier Fortran codé exclusivement en F77 permettant
							de tester différentes règles du RNC ains que les règles
							communes.
					
	- no_comment.f			Certaines règles de codage sont en rapport avec la présence 
							de commentaire. Afin de vérifier ces règles, no_comment.f est
							un fichier ne comportant aucun commentaire. Une description de
							ce fichier est disponible dans la suite de ce document.
							
-----------------------------------------------------------------------------------------------------------------------

Description de no_comment.f :

- COMMON /PARTAGE/A sert à vérifier Don.CommonIncl

- FUNCTION EXT() est la fonction utilisée dans programme_test.f pour vérifier Tr.TypeFonctionExt

-----------------------------------------------------------------------------------------------------------------------
