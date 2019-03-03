*> \brief \b DSYEQUB
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at 
*            http://www.netlib.org/lapack/explore-html/ 
*
*> \htmlonly
*> Download DSYEQUB + dependencies 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dsyequb.f"> 
*> [TGZ]</a> 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dsyequb.f"> 
*> [ZIP]</a> 
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dsyequb.f"> 
*> [TXT]</a>
*> \endhtmlonly 
*
*  Definition:
*  ===========
*
*       SUBROUTINE DSYEQUB( UPLO, N, A, LDA, S, SCOND, AMAX, WORK, INFO )
* 
*       .. Scalar Arguments ..
*       INTEGER            INFO, LDA, N
*       DOUBLE PRECISION   AMAX, SCOND
*       CHARACTER          UPLO
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), S( * ), WORK( * )
*       ..
*  
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DSYEQUB computes row and column scalings intended to equilibrate a
*> symmetric matrix A and reduce its condition number
*> (with respect to the two-norm).  S contains the scale factors,
*> S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
*> elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
*> choice of S puts the condition number of B within a factor N of the
*> smallest possible condition number over all possible diagonal
*> scalings.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the details of the factorization are stored
*>          as an upper or lower triangular matrix.
*>          = 'U':  Upper triangular, form is A = U*D*U**T;
*>          = 'L':  Lower triangular, form is A = L*D*L**T.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          The N-by-N symmetric matrix whose scaling
*>          factors are to be computed.  Only the diagonal elements of A
*>          are referenced.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[out] S
*> \verbatim
*>          S is DOUBLE PRECISION array, dimension (N)
*>          If INFO = 0, S contains the scale factors for A.
*> \endverbatim
*>
*> \param[out] SCOND
*> \verbatim
*>          SCOND is DOUBLE PRECISION
*>          If INFO = 0, S contains the ratio of the smallest S(i) to
*>          the largest S(i).  If SCOND >= 0.1 and AMAX is neither too
*>          large nor too small, it is not worth scaling by S.
*> \endverbatim
*>
*> \param[out] AMAX
*> \verbatim
*>          AMAX is DOUBLE PRECISION
*>          Absolute value of largest matrix element.  If AMAX is very
*>          close to overflow or very close to underflow, the matrix
*>          should be scaled.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (3*N)
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*>          > 0:  if INFO = i, the i-th diagonal element is nonpositive.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee 
*> \author Univ. of California Berkeley 
*> \author Univ. of Colorado Denver 
*> \author NAG Ltd. 
*
*> \date November 2011
*
*> \ingroup doubleSYcomputational
*
*> \par References:
*  ================
*>
*>  Livne, O.E. and Golub, G.H., "Scaling by Binormalization", \n
*>  Numerical Algorithms, vol. 35, no. 1, pp. 97-120, January 2004. \n
*>  DOI 10.1023/B:NUMA.0000016606.32820.69 \n
*>  Tech report version: http://ruready.utah.edu/archive/papers/bin.pdf
*>
*  =====================================================================
      SUBROUTINE DSYEQUB( UPLO, N, A, LDA, S, SCOND, AMAX, WORK, INFO )
*
*  -- LAPACK computational routine (version 3.4.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2011
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   AMAX, SCOND
      CHARACTER          UPLO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), S( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            MAX_ITER
      PARAMETER          ( MAX_ITER = 100 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, ITER
      DOUBLE PRECISION   AVG, STD, TOL, C0, C1, C2, T, U, SI, D, BASE,
     $                   SMIN, SMAX, SMLNUM, BIGNUM, SCALE, SUMSQ
      LOGICAL            UP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      LOGICAL            LSAME
      EXTERNAL           DLAMCH, LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test input parameters.
*
      INFO = 0
      IF ( .NOT. ( LSAME( UPLO, 'U' ) .OR. LSAME( UPLO, 'L' ) ) ) THEN
        INFO = -1
      ELSE IF ( N .LT. 0 ) THEN
        INFO = -2
      ELSE IF ( LDA .LT. MAX( 1, N ) ) THEN
        INFO = -4
      END IF
      IF ( INFO .NE. 0 ) THEN
        CALL XERBLA( 'DSYEQUB', -INFO )
        RETURN
      END IF

      UP = LSAME( UPLO, 'U' )
      AMAX = ZERO
*
*     Quick return if possible.
*
      IF ( N .EQ. 0 ) THEN
        SCOND = ONE
        RETURN
      END IF

      DO I = 1, N
        S( I ) = ZERO
      END DO

      AMAX = ZERO
      IF ( UP ) THEN
         DO J = 1, N
            DO I = 1, J-1
               S( I ) = MAX( S( I ), ABS( A( I, J ) ) )
               S( J ) = MAX( S( J ), ABS( A( I, J ) ) )
               AMAX = MAX( AMAX, ABS( A(I, J) ) )
            END DO
            S( J ) = MAX( S( J ), ABS( A( J, J ) ) )
            AMAX = MAX( AMAX, ABS( A( J, J ) ) )
         END DO
      ELSE
         DO J = 1, N
            S( J ) = MAX( S( J ), ABS( A( J, J ) ) )
            AMAX = MAX( AMAX, ABS( A( J, J ) ) )
            DO I = J+1, N
               S( I ) = MAX( S( I ), ABS( A( I, J ) ) )
               S( J ) = MAX( S( J ), ABS( A( I, J ) ) )
               AMAX = MAX( AMAX, ABS( A( I, J ) ) )
            END DO
         END DO
      END IF
      DO J = 1, N
         S( J ) = 1.0D+0 / S( J )
      END DO

      TOL = ONE / SQRT(2.0D0 * N)

      DO ITER = 1, MAX_ITER
         SCALE = 0.0D+0
         SUMSQ = 0.0D+0
*       BETA = |A|S
        DO I = 1, N
           WORK(I) = ZERO
        END DO
        IF ( UP ) THEN
           DO J = 1, N
              DO I = 1, J-1
                 T = ABS( A( I, J ) )
                 WORK( I ) = WORK( I ) + ABS( A( I, J ) ) * S( J )
                 WORK( J ) = WORK( J ) + ABS( A( I, J ) ) * S( I )
              END DO
              WORK( J ) = WORK( J ) + ABS( A( J, J ) ) * S( J )
           END DO
        ELSE
           DO J = 1, N
              WORK( J ) = WORK( J ) + ABS( A( J, J ) ) * S( J )
              DO I = J+1, N
                 T = ABS( A( I, J ) )
                 WORK( I ) = WORK( I ) + ABS( A( I, J ) ) * S( J )
                 WORK( J ) = WORK( J ) + ABS( A( I, J ) ) * S( I )
              END DO
           END DO
        END IF

*       avg = s^T beta / n
        AVG = 0.0D+0
        DO I = 1, N
          AVG = AVG + S( I )*WORK( I )
        END DO
        AVG = AVG / N

        STD = 0.0D+0
        DO I = 2*N+1, 3*N
           WORK( I ) = S( I-2*N ) * WORK( I-2*N ) - AVG
        END DO
        CALL DLASSQ( N, WORK( 2*N+1 ), 1, SCALE, SUMSQ )
        STD = SCALE * SQRT( SUMSQ / N )

        IF ( STD .LT. TOL * AVG ) GOTO 999

        DO I = 1, N
          T = ABS( A( I, I ) )
          SI = S( I )
          C2 = ( N-1 ) * T
          C1 = ( N-2 ) * ( WORK( I ) - T*SI )
          C0 = -(T*SI)*SI + 2*WORK( I )*SI - N*AVG
          D = C1*C1 - 4*C0*C2

          IF ( D .LE. 0 ) THEN
            INFO = -1
            RETURN
          END IF
          SI = -2*C0 / ( C1 + SQRT( D ) )

          D = SI - S( I )
          U = ZERO
          IF ( UP ) THEN
            DO J = 1, I
              T = ABS( A( J, I ) )
              U = U + S( J )*T
              WORK( J ) = WORK( J ) + D*T
            END DO
            DO J = I+1,N
              T = ABS( A( I, J ) )
              U = U + S( J )*T
              WORK( J ) = WORK( J ) + D*T
            END DO
          ELSE
            DO J = 1, I
              T = ABS( A( I, J ) )
              U = U + S( J )*T
              WORK( J ) = WORK( J ) + D*T
            END DO
            DO J = I+1,N
              T = ABS( A( J, I ) )
              U = U + S( J )*T
              WORK( J ) = WORK( J ) + D*T
            END DO
          END IF

          AVG = AVG + ( U + WORK( I ) ) * D / N
          S( I ) = SI

        END DO

      END DO

 999  CONTINUE

      SMLNUM = DLAMCH( 'SAFEMIN' )
      BIGNUM = ONE / SMLNUM
      SMIN = BIGNUM
      SMAX = ZERO
      T = ONE / SQRT(AVG)
      BASE = DLAMCH( 'B' )
      U = ONE / LOG( BASE )
      DO I = 1, N
        S( I ) = BASE ** INT( U * LOG( S( I ) * T ) )
        SMIN = MIN( SMIN, S( I ) )
        SMAX = MAX( SMAX, S( I ) )
      END DO
      SCOND = MAX( SMIN, SMLNUM ) / MIN( SMAX, BIGNUM )
*
      END
