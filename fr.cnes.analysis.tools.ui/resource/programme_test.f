0! Fichier de test pour évaluer les outils d'analyse de code Fortran.

! Le but de ce "programme" est d'observer quelles règles du RNC vérifient 
! chaque outil sélectionné sur Internet. Il sera utilisé par la suite comme
! fichier de test pour le pugin Eclipse.  

! Chaque partie de ce code enfreindra une règle sélectionnée. Afin de 
! distinguer les outils adaptables à F90 de ceux restreints à F77, ce 
! programme sera uniquement écrit en F77 (syntaxe libre).


        PROGRAM TEST

c Declaration de variables pour la suite des vérifications
c Verification Don.Separee : chaque variable doit être déclarée séparement
      	INTEGER I,J

      	INTEGER L 

c Vérification de Pr.CartStd : toutes déclaration de variable doit être commentée
c INTEGER L n'a pas été commenté
      	INTEGER K

c Vérification de Don.Utilisee : une variable déclarée doit être utilisée
c Vérification de Tr.Residus
	    INTEGER USELESS

c Variable pour vérification de Don.TablePrincipe
c Vérification de Tr.ParamTableau
c Vérification de Don.Dimension
        INTEGER A
	    DIMENSION A(10,10)

c Variables pour vérification de Tr.ComparaisonStrict
	    REAL U
		REAL V

c Variables pour vérification de Tr.BoucleSortie
		INTEGER INDICE
		INTEGER INDICE_MAX	

c Variables pour vérification de Tr.Parenthèses
		INTEGER FA
		INTEGER FB
		INTEGER SA
		INTEGER SB
		REAL RES

c Variables pour vérification de Tr.MelangeType
		REAL OPER1
		REAL OPER2
		DOUBLE PRECISION RESUL
		DOUBLE PRECISION OPER3

c Vérification de Id.MotsClés : n'utiliser aucun mot clé de Fortran pour une variable
		INTEGER DATA

c Vérification de Don.TypNorme
		INTEGER*4 I4
		REAL*4 R4

c Vérification de Don.DoublePrecision
		DOUBLE PRECISION CONST
		PARAMETER (CONST = 3.141592654)

c Vérification de Don.CommonBlanc
c Vérification de Don.CommonGroup
		COMMON	        OPER1,I,OPER3,J
		COMMON /PARTAGE/A

c Variable pour Tr.IndiceEntier
		REAL REAL_INDEX

c Vérification de Tr.Equivalence
		REAL C(2)
		REAL D(2)
		EQUIVALENCE (C(1), D(1)), (C(2), D(2)) 

c Variables pour vérification de Tr.Intrinsèques
		REAL ARGUM
		REAL VALMOD

c Variable pour vérification de Don.Hollerith		
		DOUBLE COMPLEX X(2)
      
		
		
c Initialisation des variables
      	I = 0
      	L = 1
      	J = 0
		U = 1.2
		V = 1.3
		INDICE = 1
		INDICE_MAX = 10
		FA = 8
		FB = 15
		SA = 150
		SB = 210
		RES = 0		
		OPER1 = 1.1
		OPER2 = 2.2
		OPER3 = 3.3
		RESUL = 0.0
		DATA = 0 
		I4 = 0
		R4 = 1.0
		REAL_INDEX = 0
		ARGUM = 5.0
		VALMOD = 2.5

c Vérification de Don.Hollerith
		DATA X /16Habcdefghijklmnop, 16Hqrstuvwxyz012345/
		
		
		
c Vérification de Org.Duplication : redondance de code
c Vérification de Pr.Aeration : le code doit être aéré
		DO I = 1,10
			J= J+ 1
			K =K -1
			L=2*L
		END DO

c Vérification de Pr.Indentation : le code doit être indenté
		DO I = 1,10
		J = J + 1
		K = K - 1
		L = 2 * L
		END DO

c Vérification Pr.CommIndent : les commentaires doivent être indenté comme le code
		DO I = 1,2
			WRITE(*,*) 'Pr.CommIndent'
		END DO

c Vérification Don.Declaration : toutes les variables utilisées sont déclarées
c Vérification Don.Typage : toutes les variables doivent être explicitement typées
		DO M = 1,2
			WRITE(*,*) 'Don.Declaration'
			WRITE(*,*) 'Don.Typage'
		END DO

c Vérification Don.Homonymie
c Vérification Don.Initialisation : les données doivent être initialisées avant utilisation
		DO M = 1,2
			WRITE(*,*) 'Don.Homonymie'
		END DO

c Vérification de Don.TablePrincipe
		DO I = 1,10
			DO J = 1,10
				A(I,J) = I*J
			END DO
		END DO


c Vérification de Tr.ComparaisonStrict
		IF (U .EQ. V) THEN
			WRITE(*,*) 'Tr.ComparaisonStrict'
		END IF

		IF (U .NE. V) THEN
			WRITE(*,*) 'Tr.ComparaisonStrict'
		END IF

c Vérification de Tr.Choix
		IF (U .EQ. V) THEN
			GOTO 1000
		END IF

		IF (U .NE.V) THEN
			GOTO 2000
		END IF

1000	WRITE(*,*) 'Tr.Choix'
2000	WRITE(*,*) 'Tr.Choix'

c Vérification de Tr.BoucleSortie
		DO WHILE (INDICE .LE. INDICE_MAX)
			IF (INDICE.EQ.5) THEN
				GOTO 3000
			END IF
			INDICE = INDICE + 1
		END DO

c Vérification de Tr.ModifCondSortie
3000	INDICE = 1
		DO WHILE (INDICE .LE. INDICE_MAX)
			IF (INDICE .LE. 5) THEN
				INDICE_MAX = INDICE_MAX + 1
			END IF
			INDICE = INDICE + 1
		END DO

c Vérification de Tr.Residus
		L = L + 1
c WRITE(*,*) 'Code mort'
4000	WRITE(*,*) 'Tr.Residus'	

c Vérification de Tr.Parenthèses
		RES = FA / SA + FB / SB

c Vérification de Tr.Booleen
		IF ( (FA.NE.FB) .AND. (SA.NE.SB) .AND. (U.NE.V) ) THEN
			WRITE(*,*) 'TR.Booleen'
		END IF

c Vérification de Tr.DoubleNeg
		IF (.NOT. (FA.NE.FA)) THEN
			WRITE(*,*) 'Tr.DoubleNeg'
		END IF

c Vérification de Tr.MelangeType
		RESUL = OPER1 + OPER2 + OPER3

c Vérification de Tr.ComparConst
		IF (INDICE_MAX .LE. INDICE) THEN
			WRITE(*,*) 'Tr.ComparConst'
		END IF

c Vérification de Int.ExistenceFichier
		open(11, file = 'README.txt')
		close(11)

c Vérification de Int.CheminFichier
		open(12, file = '../test_script/README.txt')

c Vérification de Int.FichierFermeture : seul le premier fichier est fermé 
c Aucune fermeture de fichier n'est appliquée à l'unité 12	

c Vérification de Int.GrouperES
		WRITE(*,*) I
		WRITE(*,*) J

c Vérification de Qa.Factorisation 
		RES = (3 * FA * FB) + (2 * FB)

c Vérification de Id.MotsClés
		DATA = 4

c Vérification de Tr.IfArithmétique
		IF (FA - FB) 5000,6000,7000
5000	WRITE(*,*) 'Tr.IfArithmétique'
6000	WRITE(*,*) 'Tr.IfArithmétique'
7000	WRITE(*,*) 'Tr.IfArithmétique'

c Vérification de Tr.IfElse
		IF (FA .LT. 0) THEN
			WRITE(*,*) 'Tr.IfElse'
		ELSE IF (FA .GT. 0) THEN
			WRITE(*,*) 'Tr.IfElse'
		ELSE IF (FA .EQ. 0) THEN
			WRITE(*,*) 'Tr.IfElse'
		END IF

c Vérification de Tr.IndiceEntier
		DO REAL_INDEX = 0.1,1.0,0.1
 			U = REAL_INDEX
		END DO

c Vérification de Tr.DoImbriqués
		DO 8000, I = 1,10
			DO 8000, J = 1,10
				A(I,J) = I + J
8000	CONTINUE

c Vérification de Tr.Pause
		PAUSE

c Vérification de Tr.Intrinsèques
		RES = AMOD (ARGUM,VALMOD)

c Vérification de Int.NuméroLogique
		WRITE(2,*) RES

c Vérification de Tr.Parametres
		CALL SUB(3,NO_PARAM,K)

c Vérification de Int.UnitéGénérique
		WRITE(*,*) 'Int.UnitéGraphique'

c Vérification de Tr.TypeFonctionExt
		J = EXT

		END PROGRAM



c Vérification de Tr.IntrinNom
		FUNCTION SIN (OPER)

c Vérification de Tr.Function : le type de OPER et de retour de la fonction SIN ne sont pas déclarés
c Vérification de Tr.ParSortie : initialiser la valeur de retour d'une fonction avant de l'attribuer	
		SIN = OPER

		ENTRY COS
		RETURN
		
		END FUNCTION



		SUBROUTINE SUB(OPER1, OPER2, OPER3)

c Vérification de Tr.OrdreParFormel
		INTEGER OPER1
		INTEGER OPER3
		INTEGER OPER2

		OPER2 = OPER2 + OPER1

c Vérification de Tr.ParSortie : les paramètres de sortie d'une procédure doivent être initialisés avant traitement
		OPER3 = 2 * OPER1

c Vérification de Tr.SubReturn : on ne peut utiliser RETURN(i)
c Vérification de Tr.FonctionSortie : une fonction n'a qu'une seule sortie
		RETURN
		RETURN 1

		END SUBROUTINE SUB



c Vérification de Tr.FunctionPar
		FUNCTION NO_PARAM
		
c Commentaire utilisé pour augmenter le taux de commentaires

		NO_PARAM = 5

		END FUNCTION NO_PARAM
