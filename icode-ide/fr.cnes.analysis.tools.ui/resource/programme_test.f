0! Fichier de test pour �valuer les outils d'analyse de code Fortran.

! Le but de ce "programme" est d'observer quelles r�gles du RNC v�rifient 
! chaque outil s�lectionn� sur Internet. Il sera utilis� par la suite comme
! fichier de test pour le pugin Eclipse.  

! Chaque partie de ce code enfreindra une r�gle s�lectionn�e. Afin de 
! distinguer les outils adaptables � F90 de ceux restreints � F77, ce 
! programme sera uniquement �crit en F77 (syntaxe libre).


        PROGRAM TEST

c Declaration de variables pour la suite des v�rifications
c Verification Don.Separee : chaque variable doit �tre d�clar�e s�parement
      	INTEGER I,J

      	INTEGER L 

c V�rification de Pr.CartStd : toutes d�claration de variable doit �tre comment�e
c INTEGER L n'a pas �t� comment�
      	INTEGER K

c V�rification de Don.Utilisee : une variable d�clar�e doit �tre utilis�e
c V�rification de Tr.Residus
	    INTEGER USELESS

c Variable pour v�rification de Don.TablePrincipe
c V�rification de Tr.ParamTableau
c V�rification de Don.Dimension
        INTEGER A
	    DIMENSION A(10,10)

c Variables pour v�rification de Tr.ComparaisonStrict
	    REAL U
		REAL V

c Variables pour v�rification de Tr.BoucleSortie
		INTEGER INDICE
		INTEGER INDICE_MAX	

c Variables pour v�rification de Tr.Parenth�ses
		INTEGER FA
		INTEGER FB
		INTEGER SA
		INTEGER SB
		REAL RES

c Variables pour v�rification de Tr.MelangeType
		REAL OPER1
		REAL OPER2
		DOUBLE PRECISION RESUL
		DOUBLE PRECISION OPER3

c V�rification de Id.MotsCl�s : n'utiliser aucun mot cl� de Fortran pour une variable
		INTEGER DATA

c V�rification de Don.TypNorme
		INTEGER*4 I4
		REAL*4 R4

c V�rification de Don.DoublePrecision
		DOUBLE PRECISION CONST
		PARAMETER (CONST = 3.141592654)

c V�rification de Don.CommonBlanc
c V�rification de Don.CommonGroup
		COMMON	        OPER1,I,OPER3,J
		COMMON /PARTAGE/A

c Variable pour Tr.IndiceEntier
		REAL REAL_INDEX

c V�rification de Tr.Equivalence
		REAL C(2)
		REAL D(2)
		EQUIVALENCE (C(1), D(1)), (C(2), D(2)) 

c Variables pour v�rification de Tr.Intrins�ques
		REAL ARGUM
		REAL VALMOD

c Variable pour v�rification de Don.Hollerith		
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

c V�rification de Don.Hollerith
		DATA X /16Habcdefghijklmnop, 16Hqrstuvwxyz012345/
		
		
		
c V�rification de Org.Duplication : redondance de code
c V�rification de Pr.Aeration : le code doit �tre a�r�
		DO I = 1,10
			J= J+ 1
			K =K -1
			L=2*L
		END DO

c V�rification de Pr.Indentation : le code doit �tre indent�
		DO I = 1,10
		J = J + 1
		K = K - 1
		L = 2 * L
		END DO

c V�rification Pr.CommIndent : les commentaires doivent �tre indent� comme le code
		DO I = 1,2
			WRITE(*,*) 'Pr.CommIndent'
		END DO

c V�rification Don.Declaration : toutes les variables utilis�es sont d�clar�es
c V�rification Don.Typage : toutes les variables doivent �tre explicitement typ�es
		DO M = 1,2
			WRITE(*,*) 'Don.Declaration'
			WRITE(*,*) 'Don.Typage'
		END DO

c V�rification Don.Homonymie
c V�rification Don.Initialisation : les donn�es doivent �tre initialis�es avant utilisation
		DO M = 1,2
			WRITE(*,*) 'Don.Homonymie'
		END DO

c V�rification de Don.TablePrincipe
		DO I = 1,10
			DO J = 1,10
				A(I,J) = I*J
			END DO
		END DO


c V�rification de Tr.ComparaisonStrict
		IF (U .EQ. V) THEN
			WRITE(*,*) 'Tr.ComparaisonStrict'
		END IF

		IF (U .NE. V) THEN
			WRITE(*,*) 'Tr.ComparaisonStrict'
		END IF

c V�rification de Tr.Choix
		IF (U .EQ. V) THEN
			GOTO 1000
		END IF

		IF (U .NE.V) THEN
			GOTO 2000
		END IF

1000	WRITE(*,*) 'Tr.Choix'
2000	WRITE(*,*) 'Tr.Choix'

c V�rification de Tr.BoucleSortie
		DO WHILE (INDICE .LE. INDICE_MAX)
			IF (INDICE.EQ.5) THEN
				GOTO 3000
			END IF
			INDICE = INDICE + 1
		END DO

c V�rification de Tr.ModifCondSortie
3000	INDICE = 1
		DO WHILE (INDICE .LE. INDICE_MAX)
			IF (INDICE .LE. 5) THEN
				INDICE_MAX = INDICE_MAX + 1
			END IF
			INDICE = INDICE + 1
		END DO

c V�rification de Tr.Residus
		L = L + 1
c WRITE(*,*) 'Code mort'
4000	WRITE(*,*) 'Tr.Residus'	

c V�rification de Tr.Parenth�ses
		RES = FA / SA + FB / SB

c V�rification de Tr.Booleen
		IF ( (FA.NE.FB) .AND. (SA.NE.SB) .AND. (U.NE.V) ) THEN
			WRITE(*,*) 'TR.Booleen'
		END IF

c V�rification de Tr.DoubleNeg
		IF (.NOT. (FA.NE.FA)) THEN
			WRITE(*,*) 'Tr.DoubleNeg'
		END IF

c V�rification de Tr.MelangeType
		RESUL = OPER1 + OPER2 + OPER3

c V�rification de Tr.ComparConst
		IF (INDICE_MAX .LE. INDICE) THEN
			WRITE(*,*) 'Tr.ComparConst'
		END IF

c V�rification de Int.ExistenceFichier
		open(11, file = 'README.txt')
		close(11)

c V�rification de Int.CheminFichier
		open(12, file = '../test_script/README.txt')

c V�rification de Int.FichierFermeture : seul le premier fichier est ferm� 
c Aucune fermeture de fichier n'est appliqu�e � l'unit� 12	

c V�rification de Int.GrouperES
		WRITE(*,*) I
		WRITE(*,*) J

c V�rification de Qa.Factorisation 
		RES = (3 * FA * FB) + (2 * FB)

c V�rification de Id.MotsCl�s
		DATA = 4

c V�rification de Tr.IfArithm�tique
		IF (FA - FB) 5000,6000,7000
5000	WRITE(*,*) 'Tr.IfArithm�tique'
6000	WRITE(*,*) 'Tr.IfArithm�tique'
7000	WRITE(*,*) 'Tr.IfArithm�tique'

c V�rification de Tr.IfElse
		IF (FA .LT. 0) THEN
			WRITE(*,*) 'Tr.IfElse'
		ELSE IF (FA .GT. 0) THEN
			WRITE(*,*) 'Tr.IfElse'
		ELSE IF (FA .EQ. 0) THEN
			WRITE(*,*) 'Tr.IfElse'
		END IF

c V�rification de Tr.IndiceEntier
		DO REAL_INDEX = 0.1,1.0,0.1
 			U = REAL_INDEX
		END DO

c V�rification de Tr.DoImbriqu�s
		DO 8000, I = 1,10
			DO 8000, J = 1,10
				A(I,J) = I + J
8000	CONTINUE

c V�rification de Tr.Pause
		PAUSE

c V�rification de Tr.Intrins�ques
		RES = AMOD (ARGUM,VALMOD)

c V�rification de Int.Num�roLogique
		WRITE(2,*) RES

c V�rification de Tr.Parametres
		CALL SUB(3,NO_PARAM,K)

c V�rification de Int.Unit�G�n�rique
		WRITE(*,*) 'Int.Unit�Graphique'

c V�rification de Tr.TypeFonctionExt
		J = EXT

		END PROGRAM



c V�rification de Tr.IntrinNom
		FUNCTION SIN (OPER)

c V�rification de Tr.Function : le type de OPER et de retour de la fonction SIN ne sont pas d�clar�s
c V�rification de Tr.ParSortie : initialiser la valeur de retour d'une fonction avant de l'attribuer	
		SIN = OPER

		ENTRY COS
		RETURN
		
		END FUNCTION



		SUBROUTINE SUB(OPER1, OPER2, OPER3)

c V�rification de Tr.OrdreParFormel
		INTEGER OPER1
		INTEGER OPER3
		INTEGER OPER2

		OPER2 = OPER2 + OPER1

c V�rification de Tr.ParSortie : les param�tres de sortie d'une proc�dure doivent �tre initialis�s avant traitement
		OPER3 = 2 * OPER1

c V�rification de Tr.SubReturn : on ne peut utiliser RETURN(i)
c V�rification de Tr.FonctionSortie : une fonction n'a qu'une seule sortie
		RETURN
		RETURN 1

		END SUBROUTINE SUB



c V�rification de Tr.FunctionPar
		FUNCTION NO_PARAM
		
c Commentaire utilis� pour augmenter le taux de commentaires

		NO_PARAM = 5

		END FUNCTION NO_PARAM
