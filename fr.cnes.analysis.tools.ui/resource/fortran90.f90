! Fichier de test pour évaluer les outils d'analyse de code Fortran.

! Le but de ce "programme" est d'observer quelles règles du RNC vérifient 
! chaque outil sélectionné sur Internet. Il sera utilisé par la suite comme
! fichier de test pour le pugin Eclipse. 

! Ce fichier permet de tester les règles particulières impossibles
! à vérifier pour F90 et impossible à vérifier avec un fichier F77.

PROGRAM TEST

! Déclaration des variables pour la suite des vérifications
! Variable pour vérification de Don.AllocDynBord
REAL, ALLOCATABLE, DIMENSION(:) :: A,B

! Vérification de Org.SousType
INTEGER,PARAMETER :: DOUBLE = selected_real_kind(15)

! Variable pour vérification de Tr.OrdreChoix
INTEGER :: NUM

! Variables pour vérification de Pr.SuiteAligne
! Vérification de Don.SousTypeRéel
REAL D,E,F,G, H

! Vérification de Don.SpecSousType
INTEGER,PARAMETER :: DISTANCE = 10

! Variables pour vérification de Don.LimitePointer
INTEGER,POINTER :: P
INTEGER,TARGET :: N

! Vérification de Don.ConstFlottant
REAL(DOUBLE) :: x1 = 0.1

! Variable pour vérification de Don.Associated
INTEGER,POINTER :: Q

! Variables pour vérification de Tr.TableauAffect
INTEGER :: V(4)
INTEGER :: TAB(4)
! Variables pour vérification de Don.Equivalence
COMPLEX :: COMPL(2) 
REAL :: TEMP(4) 

! Vérification de Don.Equivalence
EQUIVALENCE  (TEMP(1), COMPL(1))

! Vérification de Don.AllocDynBord
! Vérification de Don.AllocEchec
! Vérification de Err.Allocate
ALLOCATE(A(4))

! Initialisation des variables
NUM = 5
A(1) = 2
A(2) = 4
A(3) = 5
A(4) = 7
D = 75
E = 65
F = 92
G = 156

! Vérification de Don.AllocLiberation : toute mémoire allouée dynamiquement doit
! être libérée par la suite.
! On regarde si cette règle est vérifiée en ne libérant pas la mémoire.

! Vérification de Tr.OrdreChoix
SELECT CASE (NUM) 
	CASE (-5 : -1)
		WRITE(*,*) 'Tr.OrdreChoix'
	CASE (1 : 10 )
		WRITE(*,*) 'Tr.OrdreChoix'
	CASE ( : -6)
		WRITE(*,*) 'Tr.OrdreChoix'
        CASE (0)
		WRITE(*,*) 'Tr.OrdreChoix'
END SELECT

! Vérification de Pr.Instruction
WRITE(*,*) 'Pr.Instruction' ; WRITE(*,*) 'Pr.Instruction' ;

! Vérification de Pr.SuiteAligne
H = (G * F) + (G / (D - E)) &
    + (F / (G + F * E)) &
    - D

! Vérification de Id.Homonymie
CALL INCR(NUM)

! Vérification de Don.LimitePointer
P => N

! Vérification de Don.Associated
PRINT*, ASSOCIATED(Q)

! Vérification de Tr.TableauComplet
A(1:4) = 2 * A(1:4)

A(1:3) = A(1:3) + A(2:4)

! Vérification de Tr.TableauDepend : on vérifie la présence d'un commentaire avant
! Vérification de Tr.TableauAffect
V = (/ 1, 2, 2, 3 /)
TAB(V) = (/ 1, 2, 3, 4 /)

! Vérification de Tr.IFLogique
IF (D == 0) E = 7

! Vérification de Err.OpenRead
! Vérification de Int.ParamOpen
OPEN(11, FILE = 'README.txt')

! Vérification de Int.FormatFlottant
WRITE(6,*) x1

! Vérification de Don.AllocErreur
IF (.TRUE.) THEN
	ALLOCATE(B(1))
	B(2) = 0
END IF

! Vérification de Dyn.Ressources
DEALLOCATE(B)

END



! Vérification de Tr.RecursifBord
INTEGER RECURSIVE FUNCTION FONCTION_REC(NUM) RESULT (RES)

! Vérification de Pr.OperRelationnel
IF (NUM .EQ. 0) RETURN
RES = NUM * FONCTION_REC(NUM - 1)

! Vérification de Pr.UnitéNommée
END FUNCTION



! Vérification de Org.SousProgramme
! Vérification de Id.Homonymie
SUBROUTINE INCR(J)  

! Vérification de Tr.Intent
! On ne spécifie pas le INTENT pour J

I = I + 1     
RETURN

END



! Vérification de Don.Init
INTEGER FUNCTION FUN(A,X)

! Vérification de Tr.ParamTableau : déclarer la dimension des tableaux explicitement
INTEGER, INTENT(IN) :: A

! Vérification de Tr.ParamOptionnel
INTEGER, OPTIONAL, INTENT(IN) :: X

INTEGER :: I, S = 0   
DO I= 1, A
	S = S + I
END DO
FUN = S

! Vérification de Tr.Entry
ENTRY FUNK
RETURN

END FUNCTION FUN
