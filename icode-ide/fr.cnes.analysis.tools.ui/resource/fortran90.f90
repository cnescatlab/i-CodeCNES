! Fichier de test pour �valuer les outils d'analyse de code Fortran.

! Le but de ce "programme" est d'observer quelles r�gles du RNC v�rifient 
! chaque outil s�lectionn� sur Internet. Il sera utilis� par la suite comme
! fichier de test pour le pugin Eclipse. 

! Ce fichier permet de tester les r�gles particuli�res impossibles
! � v�rifier pour F90 et impossible � v�rifier avec un fichier F77.

PROGRAM TEST

    ! D�claration des variables pour la suite des v�rifications
    ! Variable pour v�rification de Don.AllocDynBord
    REAL, ALLOCATABLE, DIMENSION(:) :: A, B

    ! V�rification de Org.SousType
    INTEGER, PARAMETER :: DOUBLE = selected_real_kind(15)

    ! Variable pour v�rification de Tr.OrdreChoix
    INTEGER :: NUM

    ! Variables pour v�rification de Pr.SuiteAligne
    ! V�rification de Don.SousTypeR�el
    REAL D, E, F, G, H

    ! V�rification de Don.SpecSousType
    INTEGER, PARAMETER :: DISTANCE = 10

    ! Variables pour v�rification de Don.LimitePointer
    INTEGER, POINTER :: P
    INTEGER, TARGET :: N

    ! V�rification de Don.ConstFlottant
    REAL(DOUBLE) :: x1 = 0.1

    ! Variable pour v�rification de Don.Associated
    INTEGER, POINTER :: Q

    ! Variables pour v�rification de Tr.TableauAffect
    INTEGER :: V(4)
    INTEGER :: TAB(4)
    ! Variables pour v�rification de Don.Equivalence
    COMPLEX :: COMPL(2)
    REAL :: TEMP(4)

    ! V�rification de Don.Equivalence
    EQUIVALENCE  (TEMP(1), COMPL(1))

    ! V�rification de Don.AllocDynBord
    ! V�rification de Don.AllocEchec
    ! V�rification de Err.Allocate
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

    ! V�rification de Don.AllocLiberation : toute m�moire allou�e dynamiquement doit
    ! �tre lib�r�e par la suite.
    ! On regarde si cette r�gle est v�rifi�e en ne lib�rant pas la m�moire.

    ! V�rification de Tr.OrdreChoix
    SELECT CASE (NUM)
    CASE (-5:-1)
        WRITE(*, *) 'Tr.OrdreChoix'
    CASE (1:10)
        WRITE(*, *) 'Tr.OrdreChoix'
    CASE (:-6)
        WRITE(*, *) 'Tr.OrdreChoix'
    CASE (0)
        WRITE(*, *) 'Tr.OrdreChoix'
    END SELECT

    ! V�rification de Pr.Instruction
    WRITE(*, *) 'Pr.Instruction' ; WRITE(*, *) 'Pr.Instruction' ;

    ! V�rification de Pr.SuiteAligne
    H = (G * F) + (G / (D - E)) &
            + (F / (G + F * E)) &
            - D

    ! V�rification de Id.Homonymie
    CALL INCR(NUM)

    ! V�rification de Don.LimitePointer
    P => N

    ! V�rification de Don.Associated
    PRINT*, ASSOCIATED(Q)

    ! V�rification de Tr.TableauComplet
    A(1:4) = 2 * A(1:4)

    A(1:3) = A(1:3) + A(2:4)

    ! V�rification de Tr.TableauDepend : on v�rifie la pr�sence d'un commentaire avant
    ! V�rification de Tr.TableauAffect
    V = (/ 1, 2, 2, 3 /)
    TAB(V) = (/ 1, 2, 3, 4 /)

    ! V�rification de Tr.IFLogique
    IF (D == 0) E = 7

    ! V�rification de Err.OpenRead
    ! V�rification de Int.ParamOpen
    OPEN(11, FILE = 'README.txt')

    ! V�rification de Int.FormatFlottant
    WRITE(6, *) x1

    ! V�rification de Don.AllocErreur
    IF (.TRUE.) THEN
        ALLOCATE(B(1))
        B(2) = 0
    END IF

    ! V�rification de Dyn.Ressources
    DEALLOCATE(B)

END


! V�rification de Tr.RecursifBord
INTEGER RECURSIVE FUNCTION FONCTION_REC(NUM) RESULT (RES)

    ! V�rification de Pr.OperRelationnel
    IF (NUM .EQ. 0) RETURN
    RES = NUM * FONCTION_REC(NUM - 1)

    ! V�rification de Pr.Unit�Nomm�e
END FUNCTION


! V�rification de Org.SousProgramme
! V�rification de Id.Homonymie
SUBROUTINE INCR(J)

    ! V�rification de Tr.Intent
    ! On ne sp�cifie pas le INTENT pour J

    I = I + 1
    RETURN

END


! V�rification de Don.Init
INTEGER FUNCTION FUN(A, X)

    ! V�rification de Tr.ParamTableau : d�clarer la dimension des tableaux explicitement
    INTEGER, INTENT(IN) :: A

    ! V�rification de Tr.ParamOptionnel
    INTEGER, OPTIONAL, INTENT(IN) :: X

    INTEGER :: I, S = 0
    DO I = 1, A
        S = S + I
    END DO
    FUN = S

    ! V�rification de Tr.Entry
    ENTRY FUNK
    RETURN

END FUNCTION FUN
