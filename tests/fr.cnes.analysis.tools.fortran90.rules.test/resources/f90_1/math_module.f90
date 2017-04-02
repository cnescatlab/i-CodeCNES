! math_module.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.12 $
!              Date: july 2009
! Last modification: $Date: 2011-11-02 14:57:49 $
!
! (c) Noveltis
!
! --------


! math -- Module
!
! * Purpose
!   This module contains parameters, sub-routines and functions for classic
!   mathematical formula and tools.
!
!
! * Sub-routines and functions
!     * barycentre_real
!     * barycen_iter_real
!     * barycentre_cplx
!     * barycen_iter_cplx
!     * dichotomd
!     * dichotom
!     * inttabdble
!     * inttabdble_open
!     * inttab
!     * inttab2
!     * getCubicSplineCoeff
!     * intspline
!     * intspline_open
!     * intlinear
!     * intpolynome
!     * polyappli
!     * grnd
!     * prnd
!     * correlation
!     * correlmax1D
!     * swap
!     * u2toi4
!     * jacobi
!     * eigsrt
!     * matmult
!     * moving_average
!     * inversmat
!     * search_extremum
!     * phasemono
!     * smoothcosine
!     * smootherf
!     * smoothlinear
!     * windowing
!     * windowingcplx
!     * cplxtomodarg
!     * modargtocplx
!     * cplxtorealimag
!     * realimagtocplx
!     * modargtorealimag
!     * realimagtomodarg
!     * clock
!     * grnd_init
!     * extract_linear_argument
!     * extract_lin_barycentre
!     * extract_poly_barycentre
!     * factorial
!     * factorial_recursive
!     * fwhm_real
!     * fwhm_cplx
!
!---------------------------------------------------------
!  18.09.2012 (JD) : moving_average added
!---------------------------------------------------------

module math_module

  use precision_type
  use constantes_type
  use statistique_module

  implicit none

  private :: &
       doubleCplxSwap_,      &
       doubleRealArraySwap_, &
       doubleCplxArraySwap_

  public ::                 &
       barycentre_real,     &
       barycen_iter_real,   &
       barycentre_cplx,     &
       barycen_iter_cplx,   &
       dichotomd,           &
       dichotom,            &
       inttabdble,          &
       inttabdble_open,     &
       inttab,              &
       inttab2,             &
       getCubicSplineCoeff, &
       intspline,           &
       intspline_open,      &
       intlinear,           &
       intpolynome,         &
       polyappli,           &
       grnd,                &
       prnd,                &
       correlation,         &
       correlmax1D,         &
       swap,                &
       u2toi4,              &
       jacobi,              &
       eigsrt,              &
       matmult,             &
       inversmat,           &
       search_extremum,     &
       phasemono,           &
       smoothcosine,        &
       smootherf,           &
       smoothlinear,        &
       windowing,           &
       windowingcplx,       &
       cplxtomodarg,        &
       modargtocplx,        &
       cplxtorealimag,      &
       realimagtocplx,      &
       modargtorealimag,    &
       realimagtomodarg,    &
       clock,               &
       grnd_init,           &
       extract_linear_argument,&
       extract_lin_barycentre, &
       extract_poly_barycentre,&
       factorial,              &
       factorial_recursive,    &
       fwhm_real,              &
       fwhm_cplx


  ! swap -- Public
  !
  ! * Purpose
  !   Swap data between to variables.
  !
  ! * Inputs/outputs
  !     * a, b:  The two variables to swap. These variables must be of the same
  !              type. Accepting types are:
  !         * double precision complex
  !         * double precision complex array
  !         * double precision real array
  !

  interface swap
     module procedure doubleCplxSwap_, doubleCplxArraySwap_, doubleRealArraySwap_
  end interface

contains

  ! u2toi4 -- Public
  !
  ! * Purpose
  !
  ! Convert short unsigned integers (2 bytes) in long signed integers (4
  ! bytes)
  !
  ! * Inputs
  !     * itab2: array of short integers
  !     * n    : array size
  !
  ! * Outputs
  !     * itab4: array of long integers
  !

  subroutine u2toi4(itab2, n, itab4)

    integer(kind=LONG),  intent(in)  :: n
    integer(kind=SHORT), intent(in)  :: itab2(n)

    integer(kind=LONG),  intent(out) :: itab4(n)

    integer(kind=LONG)  :: i

    do i=1,n
      if (itab2(i) < 0) then
        itab4(i) = itab2(i) + 65536
      else
        itab4(i) = itab2(i)
      end if
    enddo

    return

  end subroutine u2toi4


  subroutine barycen_iter_real( NSample,dx,x,y,Ip,Iteration, Barycentre )

    integer(kind=LONG), intent(in)                      :: NSample
    real(kind=DOUBLE),  intent(in)                      :: dx
    real(kind=DOUBLE),  intent(in),dimension(1:NSample) :: x
    real(kind=DOUBLE),  intent(in),dimension(1:NSample) :: y
    integer(kind=LONG), intent(in)                      :: Ip
    integer(kind=LONG), intent(in)                      :: Iteration
    real(kind=DOUBLE),  intent(inout)                   :: Barycentre
    integer(kind=LONG)                                  :: It
    integer(kind=LONG)                                  :: Margin
    real(kind=DOUBLE), dimension(:), allocatable        :: x_tmp
    real(kind=DOUBLE), dimension(:), allocatable        :: y_tmp
    real(kind=DOUBLE)                                   :: Barycentre_tmp
    integer(kind=LONG)                                  :: NSample_tmp
!
    Barycentre = 0_DOUBLE
    call barycentre_real( NSample,dx,x,y,Ip,Barycentre_tmp )
    write(*,*) 'Barycentre_tmp',Barycentre_tmp
    Margin = 2*idnint(dabs(Barycentre_tmp)/dx)
    NSample_tmp = NSample - 2*Margin
    if( (1+Margin .gt. NSample)  .or. (NSample-Margin .lt. 1) ) then
       write(*,*) 'Barycentre Iterarive Fatal Error',Margin
       call exit(1)
    end if
    allocate( x_tmp(NSample) )
    allocate( y_tmp(NSample_tmp) )
    do It = 1, Iteration
       Barycentre = Barycentre + Barycentre_tmp
       x_tmp(1:NSample) = x(1:NSample) &
                        - Barycentre
       call intspline( NSample,     x_tmp,       y,    &
                       NSample_tmp, x(1+Margin), y_tmp )
       call barycentre_real( NSample_tmp, dx, x(1+Margin),&
                             y_tmp, Ip, Barycentre_tmp    )
       write(*,*) 'Barycentre_tmp',Barycentre_tmp
    end do
    if( dabs(Barycentre_tmp) .gt. 1.d-06 ) then
       write(*,*) 'WARNING POOR Barycentre convergence',Barycentre_tmp
    end if
    Barycentre = Barycentre + Barycentre_tmp
!
    deallocate( x_tmp )
    deallocate( y_tmp )
    return
  end subroutine barycen_iter_real

  subroutine barycentre_real( NSample,dx,x,y,Ip,Barycentre )

    integer(kind=LONG), intent(in)                      :: NSample
    real(kind=DOUBLE),  intent(in)                      :: dx
    real(kind=DOUBLE),  intent(in),dimension(1:NSample) :: x
    real(kind=DOUBLE),  intent(in),dimension(1:NSample) :: y
    integer(kind=LONG), intent(in)                      :: Ip
    real(kind=DOUBLE),  intent(inout)                   :: Barycentre
    real(kind=DOUBLE)                                   :: Cog
    real(kind=DOUBLE)                                   :: CogNorm
!
    if( Ip == 1 ) then
       Cog     = sum( x(1:NSample)*y(1:NSample) ) * dx
       CogNorm = sum( y(1:NSample) ) * dx
    else
       Cog     = sum( x(1:NSample)*y(1:NSample)**Ip ) * dx
       CogNorm = sum( y(1:NSample)**Ip ) * dx
    end if
    Barycentre = Cog / CogNorm
!
    return
  end subroutine barycentre_real

  subroutine barycen_iter_cplx( NSample,dx,x,y,Ip,Iteration, Barycentre )

    integer(kind=LONG),  intent(in)                      :: NSample
    real(kind=DOUBLE),   intent(in)                      :: dx
    real(kind=DOUBLE),   intent(in),dimension(1:NSample) :: x
    complex(kind=DOUBLE),intent(in),dimension(1:NSample) :: y
    integer(kind=LONG),  intent(in)                      :: Ip
    integer(kind=LONG),  intent(in)                      :: Iteration
    real(kind=DOUBLE),   intent(inout)                   :: Barycentre
    integer(kind=LONG)                                   :: It
    integer(kind=LONG)                                   :: Margin
    real(kind=DOUBLE),   dimension(:), allocatable       :: x_tmp
    complex(kind=DOUBLE),dimension(:), allocatable       :: y_tmp
    real(kind=DOUBLE)   ,dimension(:), allocatable       :: y_tmp_R
    real(kind=DOUBLE)   ,dimension(:), allocatable       :: y_tmp_I
    real(kind=DOUBLE)   ,dimension(:), allocatable       :: y_R
    real(kind=DOUBLE)   ,dimension(:), allocatable       :: y_I
    real(kind=DOUBLE)                                    :: Barycentre_tmp
    integer(kind=LONG)                                   :: NSample_tmp
!
    Barycentre = 0_DOUBLE
    call barycentre_cplx( NSample,dx,x,y,Ip,Barycentre_tmp )
    write(*,*) 'Barycentre_tmp',Barycentre_tmp
    Margin = 2*idnint(dabs(Barycentre_tmp)/dx)
    NSample_tmp = NSample - 2*Margin
    if( (1+Margin .gt. NSample)  .or. (NSample-Margin .lt. 1) ) then
       write(*,*) 'Barycentre Iterarive Fatal Error',Margin
       call exit(1)
    end if
    allocate( y_R(NSample) )
    allocate( y_I(NSample) )
    allocate( x_tmp(NSample) )
    allocate( y_tmp(NSample_tmp) )
    allocate( y_tmp_R(NSample_tmp) )
    allocate( y_tmp_I(NSample_tmp) )
    call cplxtorealimag( y, y_R, y_I, NSample )
    do It = 1, Iteration
       Barycentre = Barycentre + Barycentre_tmp
       x_tmp(1:NSample) = x(1:NSample) &
                        - Barycentre
       call intspline( NSample,     x_tmp,       y_R,    &
                       NSample_tmp, x(1+Margin), y_tmp_R )
       call intspline( NSample,     x_tmp,       y_I,    &
                       NSample_tmp, x(1+Margin), y_tmp_I )
       call realimagtocplx( y_tmp, y_tmp_R, y_tmp_I, NSample_tmp )
       call barycentre_cplx( NSample_tmp, dx, x(1+Margin),&
                             y_tmp, Ip, Barycentre_tmp    )
       write(*,*) 'Barycentre_tmp',Barycentre_tmp
    end do
    if( dabs(Barycentre_tmp) .gt. 1.d-06 ) then
       write(*,*) 'WARNING POOR Barycentre convergence',Barycentre_tmp
    end if
    Barycentre = Barycentre + Barycentre_tmp
!
    deallocate( x_tmp )
    deallocate( y_tmp )
    deallocate( y_tmp_R )
    deallocate( y_tmp_I )
    deallocate( y_R )
    deallocate( y_I )
    return
  end subroutine barycen_iter_cplx

  subroutine barycentre_cplx( NSample,dx,x,y,Ip,Barycentre )

    integer(kind=LONG),  intent(in)                      :: NSample
    real(kind=DOUBLE),   intent(in)                      :: dx
    real(kind=DOUBLE),   intent(in),dimension(1:NSample) :: x
    complex(kind=DOUBLE),intent(in),dimension(1:NSample) :: y
    integer(kind=LONG),  intent(in)                      :: Ip
    real(kind=DOUBLE),   intent(inout)                   :: Barycentre
    real(kind=DOUBLE)                                    :: Cog
    real(kind=DOUBLE)                                    :: CogNorm
    complex(kind=DOUBLE)                                 :: Cog_cplx
    complex(kind=DOUBLE)                                 :: CogNorm_cplx
!
    if( Ip == 1 ) then
       Cog_cplx     = sum( x(1:NSample)*y(1:NSample) ) * dx
       CogNorm_cplx = sum( y(1:NSample) ) * dx
       Barycentre   = dreal(Cog_cplx / CogNorm_cplx)
    else
       Cog     = sum( x(1:NSample)*dreal(y(1:NSample)&
                                        *dconjg(y(1:NSample))) ) * dx
       CogNorm = sum( dreal(y(1:NSample)*dconjg(y(1:NSample))) ) * dx
       Barycentre = Cog / CogNorm
    end if
!
    return
  end subroutine barycentre_cplx

  ! dichotomd -- Public
  !
  ! * Purpose
  !   Searching for the position of a point in an array by using the
  !   dichotomy technique. The array is monotonous growing.
  !
  ! * Inputs
  !     * point: the point whose position has to be found in tab
  !     * tab: available array
  !
  ! * Inputs/outputs
  !     * ir1, ir2: as outputs they are the abscissa index of the array elements
  !                 surrounding and including the point
  !

  subroutine dichotomd (point, tab, ir1, ir2)

    integer, intent(inout) :: ir1
    integer, intent(inout) :: ir2

    real(kind=DOUBLE),                     intent(in) :: point
    real(kind=DOUBLE), dimension(ir1:ir2), intent(in) :: tab

    integer :: ir

    do while ((ir2 - ir1) > 1)
       ir = int((ir2 + ir1)/2)
       if (point > tab(ir)) then
          ir1 = ir
       else
          ir2 = ir
       end if
    end do
    if (point <= tab(ir1)) then
       ir2 = ir1
    end if
    if (point >= tab(ir2)) then
       ir1 = ir2
    end if

    return
  end subroutine dichotomd

  ! dichotom -- Public
  !
  ! * Purpose
  !   Searching for the position of a point in an array by using the
  !   dichotomy technique. The array is monotonous growing.
  !
  ! * Inputs
  !     * point: the point whose position has to be found in tab
  !     * tab: available array
  !
  ! * Inputs/outputs
  !     * ir1, ir2: as outputs they are the abscissa index of the array elements
  !                 surrounding and including the point
  !

  subroutine dichotom (point, tab, ir1, ir2)

    integer(kind=LONG), intent(inout)                 :: ir1
    integer(kind=LONG), intent(inout)                 :: ir2

    real(kind=SIMPLE),                     intent(in) :: point
    real(kind=SIMPLE), dimension(ir1:ir2), intent(in) :: tab

    integer(kind=LONG)                                :: ir

    do while ((ir2 - ir1) > 1)
       ir = int((ir2 + ir1)/2)
       if (point > tab(ir)) then
          ir1 = ir
       else
          ir2 = ir
       end if
    end do
    if (point <= tab(ir1)) then
       ir2 = ir1
    end if
    if (point >= tab(ir2)) then
       ir1 = ir2
    end if

    return
  end subroutine dichotom


  ! inttabdble -- Public
  !
  ! * Purpose
  !   Interpolation on growing monotonous real double precision abscissa.
  !
  ! * Inputs
  !     * yi: Input function ordinate
  !     * xi: Input function abscissa
  !     * ni: Dimension of xi and yi
  !     * xj: Function abscissa on which yi is interpolated
  !     * nj: Dimension of xj
  !
  ! * Outputs
  !     * yj: Ouput function ordinate
  !

  subroutine inttabdble (yi, xi, ni, yj, xj, nj)

    integer(kind=LONG), intent(in)                    :: ni
    integer(kind=LONG), intent(in)                    :: nj
    real(kind=DOUBLE), dimension(ni),     intent(in)  :: yi
    real(kind=DOUBLE), dimension(ni),     intent(in)  :: xi
    real(kind=DOUBLE), dimension(nj + 1), intent(in)  :: xj

    real(kind=DOUBLE), dimension(nj), intent(inout)   :: yj


    real(kind=DOUBLE) :: alpha
    integer :: i, j, im1

    ! if necessary, processing of left over edges
    j = 1
    do while ( (j <= nj) .and. (xj(j) <= xi(1)) )
       yj(j) = yi(1)
       j = j + 1
    end do

    ! point array processing
    do  i = 2, ni
       im1 = i - 1
       alpha = (yi(i) - yi(im1))/(xi(i) - xi(im1))
       do while ((xj(j) < xi(i)) .and. (j <= nj))
          yj(j) = yi(im1) + (xj(j) - xi(im1))*alpha
          j = j + 1
       end do
    end do

    ! processing of right over edges
    if (j <= nj) then 
      yj(j:nj) = yi(ni)
    end if

    return
  end subroutine inttabdble


  ! inttabdble_open -- Public
  !
  ! * Purpose
  !   Interpolation on growing monotonous real double precision abscissa.
  !
  ! * Inputs
  !     * yi: Input function ordinate
  !     * xi: Input function abscissa
  !     * ni: Dimension of xi and yi
  !     * xj: Function abscissa on which yi is interpolated
  !     * nj: Dimension of xj
  !
  ! * Outputs
  !     * yj: Ouput function ordinate
  !

  subroutine inttabdble_open (yi, xi, ni, yj, xj, nj)

    integer(kind=LONG), intent(in)                    :: ni
    integer(kind=LONG), intent(in)                    :: nj
    real(kind=DOUBLE), dimension(ni),     intent(in)  :: yi
    real(kind=DOUBLE), dimension(ni),     intent(in)  :: xi
    real(kind=DOUBLE), dimension(nj + 1), intent(in)  :: xj

    real(kind=DOUBLE), dimension(nj), intent(inout)   :: yj


    real(kind=DOUBLE) :: alpha
    integer :: i, j, im1

    ! if necessary, processing of left over edges
    j = 1
    do while ( (j <= nj) .and. (xj(j) <= xi(1)) )
       yj(j) = 0.0d+00
       j = j + 1
    end do

    ! point array processing
    do  i = 2, ni
       im1 = i - 1
       alpha = (yi(i) - yi(im1))/(xi(i) - xi(im1))
       do while ((xj(j) < xi(i)) .and. (j <= nj))
          yj(j) = yi(im1) + (xj(j) - xi(im1))*alpha
          j = j + 1
       end do
    end do

    ! processing of right over edges
    if (j <= nj) then 
      yj(j:nj) = 0.0d+00
    end if

    return
  end subroutine inttabdble_open


  ! inttab -- Public
  !
  ! * Purpose
  !   Interpolation on growing monotonous real single precision abscissa.
  !
  ! * Inputs
  !     * yi: Input function ordinate
  !     * xi: Input function abscissa
  !     * ni: Dimension of xi and yi
  !     * xj: Function abscissa on which yi is interpolated
  !     * nj: Dimension of xj
  !
  ! * Outputs
  !     * yj: Ouput function ordinate
  !

  subroutine inttab (yi, xi, ni, yj, xj, nj)

    integer(kind=LONG), intent(in)                    :: ni
    integer(kind=LONG), intent(in)                    :: nj
    real(kind=SIMPLE), dimension(ni),     intent(in)  :: yi
    real(kind=SIMPLE), dimension(ni),     intent(in)  :: xi
    real(kind=SIMPLE), dimension(nj + 1), intent(in)  :: xj

    real(kind=SIMPLE), dimension(nj), intent(inout)   :: yj


    real(kind=SIMPLE)  :: alpha
    integer(kind=LONG) :: i, j, im1

    ! if necessary, processing of left over edges
    j = 1
    do while ( (j <= nj) .and. (xj(j) <= xi(1)) )
       yj(j) = yi(1)
       j = j + 1
    end do

    ! point array processing
    do  i = 2, ni
       im1 = i - 1
       alpha = (yi(i) - yi(im1))/(xi(i) - xi(im1))
       do while ((xj(j) < xi(i)) .and. (j <= nj))
          yj(j) = yi(im1) + (xj(j) - xi(im1))*alpha
          j = j + 1
       end do
    end do

    ! processing of right over edges
    if (j <= nj) then 
      yj(j:nj) = yi(ni)
    end if

    return
  end subroutine inttab
!

  subroutine inttab2 (yi, xi, ni, yj, xj, nj)

    integer(kind=LONG),                    intent(in)  :: ni
    integer(kind=LONG),                    intent(in)  :: nj
    real(kind=SIMPLE),  dimension(ni),     intent(in)  :: yi
    real(kind=DOUBLE),  dimension(ni),     intent(in)  :: xi
    real(kind=DOUBLE),  dimension(nj + 1), intent(in)  :: xj

    real(kind=SIMPLE),  dimension(nj),     intent(out) :: yj

    real(kind=SIMPLE)  :: alpha
    integer(kind=LONG) :: i, j, im1


    ! if necessary, processing of left over edges
    j = 1
    do while ((j <= nj) .and. (xj(j) <= xi(1)))
       yj(j) = yi(1)
       j = j + 1
    end do

    ! point array processing
    do  i = 2, ni
       im1 = i - 1
       alpha = (yi(i) - yi(im1))/real(xi(i) - xi(im1))
       do while ((j <= nj) .and. (xj(j) < xi(i)))
          yj(j) = yi(im1) + real(xj(j) - xi(im1))*alpha
          j = j + 1
       end do
    end do

    ! processing of right over edges
    if (j <= nj) then 
       yj(j:nj) = yi(ni)
    end if

    return
  end subroutine inttab2



  ! getCubicSplineCoeff -- Public
  !
  ! * Purpose
  !   Computation of the coefficients for the cubic spline interpolation.
  !   It is called by intspline.
  !
  ! * Inputs
  !     * x
  !     * y
  !     * ndim: dimension of x and y; it must be greater than two.
  !
  ! * Inputs/Outputs
  !     * cb, cc, cd: cubic spline interpolation coefficients
  !

  subroutine getCubicSplineCoeff (ndim, x, y, cb, cc, cd)

    integer,                       intent(in) :: ndim
    real(kind=DOUBLE), dimension(ndim), intent(in) :: x, y

    real(kind=DOUBLE), dimension(ndim), intent(inout) :: cb, cc, cd

    integer :: i, ib
    integer :: nm1, nm2
    real(kind=DOUBLE) :: t

    if( ndim < 2 ) then
      write(0,*) ' number of points must be > 1 '
      call exit(1)
    end if
    nm1 = ndim
    nm2 = ndim - 1
    if (ndim == 2) then
       cb(1) = (y(2) - y(1))/(x(2) - x(1))
       cc(1) = 0.0
       cd(1) = 0.0
       cb(2) = cb(1)
       cc(2) = 0.0
       cd(2) = 0.0
    end if

    cd(1) = x(2) - x(1)
    cc(2) = (y(2) - y(1))/cd(1)

    do i = 2, nm2
       cd(i) = x(i + 1) - x(i)
       cb(i) = 2.0*(cd(i - 1) + cd(i))
       cc(i + 1) = (y(i + 1) - y(i))/cd(i)
       cc(i) = cc(i + 1) - cc(i)
    enddo
    cb(1) = -cd(1)
    cb(nm1) = -cd(nm2)
    cc(1) = 0.0
    cc(nm1) = 0.0
    if (ndim > 3) then
       cc(1) = cc(3)/(x(4) - x(2)) - cc(2)/(x(3) - x(1))
       cc(1) = cc(1)*cd(1)*cd(1)/(x(4) - x(1))
       cc(nm1) = -cc(nm1)*cd(nm2)*cd(nm2)/(x(nm1) - x(nm1 - 3))
    endif
    do i = 2, nm1
       t = cd(i - 1)/cb(i - 1)
       cb(i) = cb(i) - t*cd(i - 1)
       cc(i) = cc(i) - t*cc(i - 1)
    end do
    cc(nm1) = cc(nm1)/cb(nm1)
    do ib = nm2, 1, -1
       cc(ib) = (cc(ib) - cd(ib)*cc(ib + 1))/cb(ib)
    end do
    cb(nm1) = (y(nm1) - y(nm2))/cd(nm2) + cd(nm2)*(cc(nm2) + 2.0*cc(nm1))
    do i = 1, nm2
       cb(i) = (y(i + 1) - y(i))/cd(i) - cd(i)*(cc(i + 1) + 2.0*cc(i))
       cd(i) = (cc(i + 1) - cc(i))/cd(i)
       cc(i) = 3.0*cc(i)
    end do
    cc(nm1) = 3.0*cc(nm1)
    cd(nm1) = cd(nm2)

    return
  end subroutine getCubicSplineCoeff


  ! intspline -- Public
  !
  ! * Purpose
  !   Cubic spline interpolation.
  !
  ! * Inputs
  !     * x: input function abscissa
  !     * y: input function ordinate
  !     * ndim: dimension of x and y; it must be greater than two.
  !
  ! * Inputs/Outputs
  !     * cb, cc, cd: cubic spline interpolation coefficients
  !
  ! * Outputs
  !     * xx: output function abscissa
  !     * yy: output function ordinate
  !     * kdim: dimension of xx and yy
  !

  subroutine intspline (ndim, x, y, kdim, xx, yy)

    integer(kind=LONG),                 intent(in)    :: ndim
    real(kind=DOUBLE), dimension(ndim), intent(in)    :: x, y
    integer(kind=LONG),                 intent(in)    :: kdim
    real(kind=DOUBLE), dimension(kdim), intent(in)    :: xx

    real(kind=DOUBLE), dimension(kdim), intent(inout) :: yy
    real(kind=DOUBLE), dimension(:)   , allocatable   :: cb, cc, cd

    integer(kind=LONG)                                :: i, j, k, l
    real(kind=DOUBLE)                                 :: dx

    allocate( cb(ndim) )
    allocate( cc(ndim) )
    allocate( cd(ndim) )

    ! Extrapolation management.
    i = 1
    j = kdim
    if (xx(1) < x(1))then
       do while ((xx(i) < x(1)) .and. (i < kdim))
          yy(i) = y(1)
          i = i + 1
       end do
    end if
    if (xx(kdim) > x(ndim))then
       do while ((xx(j) > x(ndim)) .and. (j > 1))
          yy(j) = y(ndim)
          j = j - 1
       end do
    end if

    call getCubicSplineCoeff (ndim, x, y, cb, cc, cd)

    ! Compute interpolation between xx(i) and xx(j).
    k = 1
    do l = i, j
       do while (xx(l) > x(k + 1))
          k = k + 1
       end do
       dx = xx(l)-x(k)
       yy(l) = y(k)+dx*(cb(k)+dx*(cc(k)+dx*cd(k)))
    end do

    deallocate( cb )
    deallocate( cc )
    deallocate( cd )

    return
  end subroutine intspline


  ! intspline_open -- Public
  !
  ! * Purpose
  !   Cubic spline interpolation without extrapolation.
  !
  ! * Inputs
  !     * x: input function abscissa
  !     * y: input function ordinate
  !     * ndim: dimension of x and y; it must be greater than two.
  !
  ! * Inputs/Outputs
  !     * cb, cc, cd: cubic spline interpolation coefficients
  !
  ! * Outputs
  !     * xx: output function abscissa
  !     * yy: output function ordinate
  !     * kdim: dimension of xx and yy
  !

  subroutine intspline_open (ndim, x, y, kdim, xx, yy)

    integer(kind=LONG),                 intent(in)    :: ndim
    real(kind=DOUBLE), dimension(ndim), intent(in)    :: x, y
    integer(kind=LONG),                 intent(in)    :: kdim
    real(kind=DOUBLE), dimension(kdim), intent(in)    :: xx

    real(kind=DOUBLE), dimension(kdim), intent(inout) :: yy
    real(kind=DOUBLE), dimension(:)   , allocatable   :: cb, cc, cd

    integer(kind=LONG)                                :: i, j, k, l
    real(kind=DOUBLE)                                 :: dx

    allocate( cb(ndim) )
    allocate( cc(ndim) )
    allocate( cd(ndim) )

    ! Extrapolation management.
    i = 1
    j = kdim
    if (xx(1) < x(1))then
       do while ((xx(i) < x(1)) .and. (i < kdim))
          yy(i) = 0_DOUBLE
          i = i + 1
       end do
    end if
    if (xx(kdim) > x(ndim))then
       do while ((xx(j) > x(ndim)) .and. (j > 1))
          yy(j) = 0_DOUBLE
          j = j - 1
       end do
    end if

    call getCubicSplineCoeff (ndim, x, y, cb, cc, cd)

    ! Compute interpolation between xx(i) and xx(j).
    k = 1
    do l = i, j
       do while (xx(l) > x(k + 1))
          k = k + 1
       end do
       dx = xx(l)-x(k)
       yy(l) = y(k)+dx*(cb(k)+dx*(cc(k)+dx*cd(k)))
    end do

    deallocate( cb )
    deallocate( cc )
    deallocate( cd )

    return
  end subroutine intspline_open
!
!
  subroutine intlinear( NSample_In, &
                        x_In,       &
                        y_In,       &
                        Wgt,        &
                        NSample_Out,&
                        x_Out,      &
                        y_Out,      &
                        rms         )
   implicit none
!  
!     fonctions externe
      !$PRAGMA C(crvlinearleastsqr)
      integer(kind=LONG)                                 :: crvlinearleastsqr
      integer(kind=LONG),intent(in)                          :: NSample_In
      real(kind=DOUBLE) ,intent(in)   ,dimension(NSample_In) :: x_In
      real(kind=DOUBLE) ,intent(in)   ,dimension(NSample_In) :: y_In
      real(kind=DOUBLE) ,intent(in)   ,dimension(NSample_In) :: Wgt
      integer(kind=LONG),intent(in)                          :: NSample_Out
      real(kind=DOUBLE) ,intent(in)   ,dimension(NSample_Out):: x_Out
      real(kind=DOUBLE) ,intent(inout),dimension(NSample_Out):: y_Out
      real(kind=DOUBLE) ,intent(out)                         :: rms
      integer(kind=LONG)                                     :: ires
      real(kind=DOUBLE)                                      :: A0
      real(kind=DOUBLE)                                      :: A1
!
!     calcul des coefficients du polynome pour le module
      ires = crvlinearleastsqr( %VAL(NSample_In),&
                                x_In,            &
                                y_In,            &
                                Wgt,             &
                                A0,              &
                                A1,              &
                                rms              ) 
      if( ires == 0 ) then
!
!        application des coefficients du polynome
         write(*,*) ' A0,A1,rms',A0,A1,rms
         y_Out(1:NSample_Out) = A0 + ( A1 * x_Out(1:NSample_Out) )
      else
         write (*,*) 'crvlinearleastsqr abort :',ires
         call exit(1)
      end if
!
    return
  end subroutine intlinear
!
!
  subroutine intpolynome( deg_polynom,&
                          NSample_In, &
                          x_In,       &
                          y_In,       &
                          Wgt,        &
                          NSample_Out,&
                          x_Out,      &
                          y_Out,      &
                          y_Out_Mean, &
                          rms,        &
                          ErrorCode )
   implicit none
   !  
   ! external function
   !$PRAGMA C(crvpolyLeastsqr)
   !$PRAGMA C(crvlinearleastsqr)
   external                                               :: crvpolyLeastsqr
   external                                               :: crvlinearLeastsqr
   integer(kind=LONG)                                     :: crvpolyLeastsqr
   integer(kind=LONG)                                     :: crvlinearleastsqr
   integer(kind=LONG),intent(in)                          :: deg_polynom
   integer(kind=LONG),intent(in)                          :: NSample_In
   real(kind=DOUBLE) ,intent(in)   ,dimension(NSample_In) :: x_In
   real(kind=DOUBLE) ,intent(in)   ,dimension(NSample_In) :: y_In
   real(kind=DOUBLE) ,intent(in)   ,dimension(NSample_In) :: Wgt
   integer(kind=LONG),intent(in)                          :: NSample_Out
   real(kind=DOUBLE) ,intent(in)   ,dimension(NSample_Out):: x_Out
   real(kind=DOUBLE) ,intent(inout),dimension(NSample_Out):: y_Out
   real(kind=DOUBLE) ,intent(out)                         :: y_Out_Mean
   real(kind=DOUBLE) ,intent(out)                         :: rms
   integer(kind=LONG),intent(out)                         :: ErrorCode
   integer(kind=LONG)                                     :: ires
   real(kind=DOUBLE) ,dimension(:) ,allocatable           :: dcoeffs
   real(kind=DOUBLE) ,dimension(1)                        :: x_Out_Mean
   real(kind=DOUBLE) ,dimension(1)                        :: y_Out_MeanTab
   !
   allocate( dcoeffs(0:deg_polynom) )
   ErrorCode = 0
   !
   ! calculates coefficients of the polynomial function
   call average(NSample_Out,x_Out,x_Out_Mean(1))
   if( NSample_In > 1 ) then
      if( deg_polynom > 1 ) then
         ires = crvpolyLeastsqr( %VAL(NSample_In), &
                                 %VAL(deg_polynom),&
                                 x_In, y_In, Wgt,  &
                                 dcoeffs, rms      ) 
         if( ires == 0 ) then
         !
         !  apply coefficients of the polynomial function
            call polyappli( NSample_Out, x_Out, dcoeffs, &
                            deg_polynom, y_Out           )
            call polyappli( 1, x_Out_Mean, dcoeffs, &
                            deg_polynom, y_Out_MeanTab   )
            y_Out_Mean = y_Out_MeanTab(1)
         else
            write (0,*) 'crvpolyLeastsqr abort :', ires
            ErrorCode = 1
         end if
      else if( deg_polynom == 1 ) then
         ires = crvlinearleastsqr( %VAL(NSample_In),&
                                   x_In,            &
                                   y_In,            &
                                   Wgt,             &
                                   dcoeffs(0),      &
                                   dcoeffs(1),      &
                                   rms              ) 
         if( ires == 0 ) then
!
!           application des coefficients du polynome
            call polyappli( NSample_Out, x_Out, dcoeffs, &
                            deg_polynom, y_Out           )
            call polyappli( 1, x_Out_Mean, dcoeffs, &
                            deg_polynom, y_Out_MeanTab   )
            y_Out_Mean = y_Out_MeanTab(1)
         else
            write (*,*) 'crvlinearleastsqr abort :',ires
            call exit(1)
         end if
      else if( deg_polynom == 0 ) then
         call average( NSample_In, y_In, y_Out_Mean )
         y_Out(1:NSample_Out) = y_Out_Mean
      else
         write (0,*) 'intpolynome abort bad deg_polynom:', deg_polynom
         ErrorCode = 1
      end if
   else
      if ( deg_polynom == 0 ) then
         y_Out(1) = y_In(1)
         y_Out_Mean = y_Out(1)
      else
         write (0,*) 'intpolynome abort bad deg_polynom:', deg_polynom
         ErrorCode = 1
      end if
   end if

!
   deallocate( dcoeffs )
   return
 end subroutine intpolynome
!
!
  subroutine polyappli( n,x,d,deg,y )
      integer(kind=LONG), intent(in)                        :: n
      integer(kind=LONG), intent(in)                        :: deg
      real(kind=DOUBLE) , intent(in)   , dimension(n)       :: x
      real(kind=DOUBLE) , intent(in)   , dimension(0:deg)   :: d
      real(kind=DOUBLE) , intent(inout), dimension(n)       :: y
      real(kind=DOUBLE)                                     :: p
      integer(kind=LONG)                                    :: i
      integer(kind=LONG)                                    :: nd
!
      do i = 1, n
        p = d(deg)
        do nd = deg-1, 0, -1
          p = (p*x(i)) + d(nd)
        end do
        y(i) = p
      end do
!
    return
  end subroutine polyappli
!
!
  ! grnd -- Public
  !
  ! * Purpose
  !   Generator of gaussian random numbers; normalised half-width.
  !
  ! * Inputs
  !     * init
  !
  
  function grnd (init) result (return_grnd)

    integer, intent(in) :: init

    integer, parameter :: ldim = 5001

    real(kind=DOUBLE), dimension(ldim + 2), save ::  x
    real(kind=DOUBLE), dimension(ldim + 2), save ::  erf

    integer      :: ldimp
    integer      :: l
    integer      :: ir1, ir2
    real(kind=DOUBLE) :: return_grnd
    real(kind=DOUBLE) :: y
    real(kind=DOUBLE) :: rerf
    real(kind=DOUBLE) :: sqpiinv
    real(kind=DOUBLE) :: dx

    ! Initialisation of the erf array
    if (init == 1) then
       ldimp = int(ldim/2)
       dx = 10.0_DOUBLE/(ldim - 1)
       sqpiinv = dsqrt(1./twoPi)
       rerf = 0
       x(1) = -(ldimp+1)*dx
       x(ldim + 2) = (ldimp + 1)*dx
       do l = 1, ldim
          x(l + 1) = (l - ldimp - 1)*dx
          rerf = rerf + sqpiinv*dexp(-0.5*(x(l + 1))**2)*dx
          erf(l + 1) = rerf
       end do
       erf(1) = 0.
       erf(ldim + 2) = 1.
       return_grnd = 0.d+00
    else
       ! Generation of gaussian random numbers with a normalised half-width
       y = prnd()
       ir1 = 1 + 1
       ir2 = ldim + 1
       call dichotomd (y, erf(2), ir1, ir2)
       return_grnd = x(ir1) + (x(ir1 + 1) - x(ir1))           &
            *(y - erf(ir1))/(erf(ir1 + 1) - erf(ir1))
    end if

    return
  end function grnd


  ! prnd -- Public
  !
  ! * Purpose
  !   Pseudo random generator.
  !
  
  function prnd () result (return_prnd)

    integer(kind=LONG), save :: seed = 895
    real(kind=DOUBLE)        :: return_prnd

    seed = seed*125
    seed = seed - (seed/2796203)*2796203
    return_prnd = seed/2796203.0

    return
  end function prnd


  ! correlation -- Public
  !
  ! * Purpose
  !   Computation of the correlation coefficient CorrelCoef between x and y.
  !
  ! * Description
  !   CorrelCoef is the ratio between the sum of the products of deviations from
  !   the mean value and the square root of the sum of the products of quadratic
  !   deviations from the mean value.
  !
  ! * Inputs
  !     * x, y:       Input functions
  !     * dim:        Dimension of x and y
  !
  ! * Outputs
  !     * CorrelCoef: Correlation coefficient between x and y 
  !

  subroutine correlation (dim, x, y, CorrelCoef)

    integer(kind=LONG),                           intent(in) :: dim
    real(kind=DOUBLE), dimension(dim),            intent(in) :: x, y

    real(kind=DOUBLE), intent(inout)                         :: CorrelCoef

    integer(kind=LONG)                                       :: n
    real(kind=DOUBLE) :: xm, ym
    real(kind=DOUBLE) :: pdmv, xdmv, ydmv
    real(kind=DOUBLE) :: pqdm, xqdm, yqdm

    ! Computation of the mean values xm and ym of x and y respectively.
    xm = 0.0_DOUBLE
    ym = 0.0_DOUBLE
    do n = 1, dim
       xm = xm + x(n)
       ym = ym + y(n)
    end do
    xm = xm/dfloat(dim)
    ym = ym/dfloat(dim)


    ! Computation of the components of CorrelCoef.
    pdmv = 0.0_DOUBLE
    xqdm = 0.0_DOUBLE
    yqdm = 0.0_DOUBLE

    do n = 1, dim

       ! Deviations from the mean value xdmv and ydmv.
       xdmv = x(n) - xm
       ydmv = y(n) - ym

       ! Sum of the quadratic deviations from the mean value xqdm and yqdm.
       xqdm = xqdm + xdmv**2
       yqdm = yqdm + ydmv**2

       ! Sum of the products of the deviations from the mean value pdmv.
       pdmv = pdmv + ( xdmv*ydmv )
    end do

    ! Products of sums of quadratic deviations from the mean value pqdm.
    pqdm = xqdm*yqdm

    ! Computation of the correlation coefficient.
    if (pqdm <= 0.0_DOUBLE) then
       CorrelCoef = 1.0_DOUBLE
    else
       CorrelCoef = pdmv/dsqrt(pqdm)
    end if

    return
  end subroutine correlation


  subroutine correlmax1D( ndec, x, rc, rcmaxparab, &
                          xmax, n1, n2, Flag_Error )
    integer(kind=LONG) ,intent(in)                        :: ndec
    real(kind=DOUBLE)  ,intent(in) ,dimension(1:ndec)     :: x
    real(kind=DOUBLE)  ,intent(in) ,dimension(1:ndec)     :: rc
    real(kind=DOUBLE)  ,intent(out)                       :: rcmaxparab
    real(kind=DOUBLE)  ,intent(out)                       :: xmax
    integer(kind=LONG) ,intent(out)                       :: n1
    integer(kind=LONG) ,intent(out)                       :: n2
    integer(kind=LONG) ,intent(out)                       :: Flag_Error
    integer(kind=LONG)                                    :: nd
    integer(kind=LONG)                                    :: ndm
    real(kind=DOUBLE)                                     :: rcmax
    integer(kind=LONG)                                    :: ndmax
    real(kind=DOUBLE)                                     :: x1
    real(kind=DOUBLE)                                     :: x2
    real(kind=DOUBLE)                                     :: x3
    real(kind=DOUBLE)                                     :: y1
    real(kind=DOUBLE)                                     :: y2
    real(kind=DOUBLE)                                     :: y3
    real(kind=DOUBLE)                                     :: a
    real(kind=DOUBLE)                                     :: b
    real(kind=DOUBLE)                                     :: c
!
!   Maximum sample
    rcmax = -1.d+00
    ndm   = 0
    do nd = 1, ndec
       if( rcmax < rc(nd) ) then
          rcmax = rc(nd)
          ndm   = nd
       end if
    end do
!
!   if the maximum is at the edge
    ndmax = ndm
    if( ndm.eq.1 ) ndm = 2
    if( ndm.eq.ndec ) ndm = ndec-1
!
!   Parabole coefficients
    x1 = x(ndm-1)
    x2 = x(ndm)
    x3 = x(ndm+1)
    y1 = rc(ndm-1)
    y2 = rc(ndm)
    y3 = rc(ndm+1)
!
    a = -(y1-y2)*(x1+x2)/(x3-x1)/(x1*x1-x2*x2) + (y3-y2)/(x3-x2)/(x3-x1)
    b = (y1-y2)/(x1-x2)-a*(x1+x2)
    c = y1-b*x1-a*x1*x1
!
!   Maximun abscissa and value
    xmax = -b/(2.d+00*a)
    if( (xmax < x1) .or. (xmax > x3) ) then
       write(*,*) '(xmax < x1) .or. (xmax > x3)',&
                  x1,x3,xmax
       Flag_Error = 0
       if( y1 > y3 ) then
          xmax=x1
          rcmaxparab=y1
       else
          xmax=x3
          rcmaxparab=y3
       end if
    else
       Flag_Error = 1
       rcmaxparab = a*xmax**2+b*xmax+c
       if( rcmaxparab < rcmax ) then
          write(*,*) 'rcmaxparab < rcmax',rcmaxparab,rcmax
          Flag_Error = 0
       end if
    end if
!
    if( xmax.lt.x2 ) then 
       n1 = ndm-1
       n2 = ndm
    else
       n1 = ndm
       n2 = ndm+1
    end if
!
    return
  end subroutine correlmax1D



  ! doubleCplxSwap_ -- Private
  !
  ! * Purpose
  !   Swap data between to double precision complex.
  !
  ! * Inputs/outputs
  !     * a, b:  The two numbers to swap.
  !

  subroutine doubleCplxSwap_ (a, b)

    complex(kind=DOUBLECPLX), intent(inout) :: a, b
    complex(kind=DOUBLECPLX)                :: gah

    gah = a
    a = b
    b = gah

    return
  end subroutine doubleCplxSwap_


  ! doubleRealArraySwap_ -- Private
  !
  ! * Purpose
  !   Swap data between to double precision real arrays of the same size.
  !
  ! * Inputs/outputs
  !     * a, b:  The two arrays to swap.
  !

  subroutine doubleRealArraySwap_ (a, b)

    real(kind=DOUBLE), dimension(:), intent(inout) :: a, b
    real(kind=DOUBLE), dimension(size(a))          :: gah

    gah = a
    a = b
    b = gah

    return
  end subroutine doubleRealArraySwap_


  ! doubleCplxArraySwap_ -- Private
  !
  ! * Purpose
  !   Swap data between to double precision complex arrays of the same size.
  !
  ! * Inputs/outputs
  !     * a, b:  The two arrays to swap.
  !

  subroutine doubleCplxArraySwap_ (a, b)

    complex(kind=DOUBLECPLX), dimension(:), intent(inout) :: a, b
    complex(kind=DOUBLECPLX), dimension(size(a))          :: gah

    gah = a
    a = b
    b = gah

    return
  end subroutine doubleCplxArraySwap_
!
!
  subroutine jacobi( a,n,np,d,v,nrot )
    integer(kind=LONG), intent(in)                          :: np
    integer(kind=LONG), intent(in)                          :: n
    real(kind=DOUBLE) , intent(inout), dimension(np,np)     :: a
    real(kind=DOUBLE) , intent(inout), dimension(np,np)     :: v
    real(kind=DOUBLE) , intent(inout), dimension(np)        :: d
    integer(kind=LONG), intent(out)                         :: nrot
    integer(kind=LONG)                                      :: ip
    integer(kind=LONG)                                      :: iq
    integer(kind=LONG)                                      :: i
    integer(kind=LONG)                                      :: j
    real(kind=DOUBLE)                                       :: sm
    real(kind=DOUBLE)                                       :: tresh
    real(kind=DOUBLE)                                       :: g
    real(kind=DOUBLE)                                       :: h
    real(kind=DOUBLE)                                       :: t
    real(kind=DOUBLE)                                       :: theta
    real(kind=DOUBLE)                                       :: c
    real(kind=DOUBLE)                                       :: s
    real(kind=DOUBLE)                                       :: tau
    real(kind=DOUBLE)                , dimension(np)        :: b
    real(kind=DOUBLE)                , dimension(np)        :: z
      do ip = 1, n
        do iq = 1, n
          v(ip,iq) = 0.d+00
        end do
        v(ip,ip) = 1.d+00
      end do
      do ip = 1, n
        b(ip) = a(ip,ip)
        d(ip) = b(ip)
        z(ip) = 0.d+00
      end do
      nrot = 0
      do i = 1, 50
        sm = 0.d+00
        do ip = 1, n-1
          do iq = ip+1, n
            sm = sm+abs(a(ip,iq))
          end do
        end do
        if(sm.eq.0.)return
        if(i.lt.4)then
          tresh = 0.2d+00*sm/n**2
        else
          tresh = 0.d+00
        end if
        do ip = 1, n-1
          do iq = ip+1, n
            g = 100.d+00*abs(a(ip,iq))
            if((i.gt.4).and.(abs(d(ip))+g.eq.abs(d(ip)))&
                       .and.(abs(d(iq))+g.eq.abs(d(iq))))then
              a(ip,iq) = 0.d+00
            else if(abs(a(ip,iq)).gt.tresh)then
              h = d(iq)-d(ip)
              if(abs(h)+g.eq.abs(h))then
                t = a(ip,iq)/h
              else
                theta = 0.5*h/a(ip,iq)
                t =1./(abs(theta)+sqrt(1.+theta**2))
                if(theta.lt.0.) t=-t
              end if
              c = 1./sqrt(1+t**2)
              s = t*c
              tau = s/(1.+c)
              h = t*a(ip,iq)
              z(ip) = z(ip)-h
              z(iq) = z(iq)+h
              d(ip) = d(ip)-h
              d(iq) = d(iq)+h
              a(ip,iq) = 0.d+00
              do j = 1, ip-1
                g = a(j,ip)
                h = a(j,iq)
                a(j,ip) = g-s*(h+g*tau)
                a(j,iq) = h+s*(g-h*tau)
              end do
              do j = ip+1, iq-1
                g = a(ip,j)
                h = a(j,iq)
                a(ip,j) = g-s*(h+g*tau)
                a(j,iq) = h+s*(g-h*tau)
              end do
              do j = iq+1, n
                g = a(ip,j)
                h = a(iq,j)
                a(ip,j) = g-s*(h+g*tau)
                a(iq,j) = h+s*(g-h*tau)
              end do
              do j = 1, n
                g = v(j,ip)
                h = v(j,iq)
                v(j,ip) = g-s*(h+g*tau)
                v(j,iq) = h+s*(g-h*tau)
              end do
              nrot = nrot+1
            end if
          end do
        end do
        do ip = 1, n
          b(ip) = b(ip)+z(ip)
          d(ip) = b(ip)
          z(ip) = 0.d+00
        end do
      end do
    return
  end subroutine jacobi
!
!
  subroutine eigsrt( d,v,n,np )
    integer(kind=LONG), intent(in)                          :: np
    integer(kind=LONG), intent(in)                          :: n
    real(kind=DOUBLE) , intent(inout), dimension(np,np)     :: v
    real(kind=DOUBLE) , intent(inout), dimension(np)        :: d
    integer(kind=LONG)                                      :: i
    integer(kind=LONG)                                      :: j
    integer(kind=LONG)                                      :: k
    real(kind=DOUBLE)                                       :: p
!
      do i = 1, n-1
        k = i
        p = d(i)
        do j = i+1, n
          if(d(j).ge.p)then
            k = j
            p = d(j)
          end if
        end do
        if(k.ne.i)then
          d(k) = d(i)
          d(i) = p
          do j = 1, n
            p = v(j,i)
            v(j,i) = v(j,k)
            v(j,k) = p
          end do
        end if
      end do
!
    return
  end subroutine eigsrt
!
!
  subroutine matmult( ldmx,mdmx,ndmx,ld,md,nd,mat1,mat2,mat )
    integer(kind=LONG), intent(in)                          :: ldmx
    integer(kind=LONG), intent(in)                          :: mdmx
    integer(kind=LONG), intent(in)                          :: ndmx
    integer(kind=LONG), intent(in)                          :: ld
    integer(kind=LONG), intent(in)                          :: md
    integer(kind=LONG), intent(in)                          :: nd
    real(kind=DOUBLE) , intent(in)   , dimension(ldmx,mdmx) :: mat1
    real(kind=DOUBLE) , intent(in)   , dimension(mdmx,ndmx) :: mat2
    real(kind=DOUBLE) , intent(inout), dimension(ldmx,ndmx) :: mat
!
    mat( 1:ld,1:nd ) = matmul( mat1( 1:ld,1:md ), mat2( 1:md,1:nd )  )
    return
  end subroutine matmult
!
!!
!!
!> moving_average -- Public
!!
!! * Purpose
!!
!!     Mean calculation.   
!!
!! * Description
!!
!!     Compute the centred moving average of data on a window of 2*SlideFilter+1 width 
!!     
!! * Inputs 
!!
!!     - Tab             : data set to smooth - Tab(1:Nsample)
!!     - Nsample         : number of sample of the data set
!!     - SlideFilter     : half width of the moving average window
!!
!! * Inputs/outputs
!!
!! * Outputs
!!     
!!     - meanTab       : smoothed data set - MeanTab(1:Nsample)

  subroutine moving_average(Tab,Nsample,SlideFilter,MeanTab)

    real(kind=DOUBLE),  intent(in) :: Tab(1:Nsample)
    integer(kind=LONG), intent(in) :: Nsample
    integer(kind=LONG), intent(in) :: SlideFilter
    real(kind=DOUBLE),  intent(out) :: MeanTab(1:Nsample)
    integer(kind=LONG) :: ic
    integer(kind=LONG) :: SlideFilteri
    integer(kind=LONG) :: jcd
    integer(kind=LONG) :: jcf
    integer(kind=LONG) :: npt
    integer(kind=LONG) :: jc
    !
    ! first boundary
    do ic=1,SlideFilter
       SlideFilteri=ic-1
       jcd=ic-SlideFilteri
       jcf=ic+SlideFilteri
       npt=jcf-jcd+1
       MeanTab(ic)= 0_DOUBLE
       do jc=jcd,jcf
          MeanTab(ic)=MeanTab(ic)+Tab(jc)
       end do
       MeanTab(ic)=MeanTab(ic)/dfloat(npt)
    end do
    ! full window
    npt=2*SlideFilter+1
    do ic=1+SlideFilter,Nsample-SlideFilter
       jcd=ic-SlideFilter
       jcf=ic+SlideFilter
       MeanTab(ic)= 0_DOUBLE
       do jc=jcd,jcf
          MeanTab(ic)=MeanTab(ic)+Tab(jc)
       end do
       MeanTab(ic)=MeanTab(ic)/dfloat(npt)
    end do
    ! last boundary
    SlideFilteri=SlideFilter
    do ic=Nsample-SlideFilter+1,Nsample
       SlideFilteri=SlideFilteri-1
       jcd=ic-SlideFilteri
       jcf=ic+SlideFilteri
       npt=jcf-jcd+1
       MeanTab(ic)= 0_DOUBLE
       do jc=jcd,jcf
          MeanTab(ic)=MeanTab(ic)+Tab(jc)
       end do
       MeanTab(ic)=MeanTab(ic)/dfloat(npt)
    end do
    return
  end subroutine moving_average
!
  subroutine inversmat( ndmx,nd,mat,invmat,wr,z )
    integer(kind=LONG), intent(in)                          :: ndmx
    integer(kind=LONG), intent(in)                          :: nd
    real(kind=DOUBLE) , intent(in) , dimension(ndmx,ndmx)   :: mat
!
    real(kind=DOUBLE) , intent(inout), dimension(ndmx,ndmx) :: invmat
    real(kind=DOUBLE) , intent(inout), dimension(ndmx,ndmx) :: z
    real(kind=DOUBLE) , intent(inout), dimension(ndmx)      :: wr
!
    integer(kind=LONG)                                      :: i
    integer(kind=LONG)                                      :: j
    integer(kind=LONG)                                      :: l
    integer(kind=LONG)                                      :: nrot
    real(kind=DOUBLE)                , dimension(ndmx,ndmx) :: wrk
    real(kind=DOUBLE)                , dimension(ndmx,ndmx) :: zt
    real(kind=DOUBLE)                , dimension(ndmx)      :: wrinv
    real(kind=DOUBLE)                , dimension(ndmx,ndmx) :: zl
    real(kind=DOUBLE)                , dimension(ndmx,ndmx) :: id
!
      do i = 1, ndmx
        do j = 1, ndmx
          wrk(i,j) = mat(i,j)
        end do
      end do
      call jacobi( wrk,nd,ndmx,wr,z,nrot )
      call eigsrt( wr,z,nd,ndmx )

      do i = 1, nd
        if (wr(i).lt.0.) then
          write (*,*) 'valeur propre negative.'
          write (*,*) (wr(l),l=1,nd)
          wr(i) = 0.d0
        end if
        if ((wr(i).lt.1.d-18).and.(wr(i).ge.0)) then
          write (*,*) 'valeur propre nulle.'
          write (*,*) (wr(l),l=1,nd)
          wr(i) = 0.d0
        end if
      end do
!
!     calcul de la transposee
      do i = 1, nd
        do j = 1, nd
          zt(j,i) = z(i,j)
        end do
      end do
!
      do j = 1, nd
        if (wr(j).ne.0) then
          wrinv(j) = (1.d0/wr(j))
        end if
        if (wr(j).eq.0) then
          wrinv(j) = 0.d0
        end if
      end do

      do i = 1, nd
        do j = 1, nd
          zl(i,j) = z(i,j)*wrinv(j)
        end do
      end do

      call matmult( ndmx,ndmx,ndmx,nd,nd,nd,zl,zt,invmat )

      call matmult( ndmx,ndmx,ndmx,nd,nd,nd,mat,invmat,id )
!     write(*,*) 'verif: diagonale de mat.invmat.'
!     write(*,*) (id(i,i),i=1,nd)

    return
  end subroutine inversmat
!
!
  subroutine search_extremum( Vector,nlgp,ncgp,indx,indy,&
                              xsom,ysom,vsom,precisext,  &
                              preciscorr,courb,ier       )
  implicit none
! objet : calcul de la position (xsom,ysom) du sommet d'un paraboloide
!    a*x**2+b*y**2+c*x*y+d*x+e*y+f passant par les points Mj(Xj,Yj), ce
!    nombre de point est parametrable.
!    la qualite de determination de l'extremum est egalement calcule
!    les lignes de la matrice sont reperees par le premier indice
!    les colonnes de la matrice sont reperees par le deuxieme indice
     integer(kind=LONG) ,intent(in)                        :: nlgp
     integer(kind=LONG) ,intent(in)                        :: ncgp
     real(kind=DOUBLE)  ,intent(in),dimension(nlgp*ncgp)   :: Vector
     integer(kind=LONG) ,intent(in),dimension(nlgp)        :: indx
     integer(kind=LONG) ,intent(in),dimension(ncgp)        :: indy
     real(kind=DOUBLE)  ,dimension(:,:),allocatable        :: M
     real(kind=DOUBLE)  ,intent(out)                       :: xsom
     real(kind=DOUBLE)  ,intent(out)                       :: ysom
     real(kind=DOUBLE)  ,intent(out)                       :: vsom
     real(kind=DOUBLE)  ,intent(out)                       :: precisext
     real(kind=DOUBLE)  ,intent(out)                       :: preciscorr
     real(kind=DOUBLE)  ,intent(out)                       :: courb
     integer(kind=LONG) ,intent(out)                       :: ier
     integer(kind=LONG)                                    :: ncoef
     integer(kind=LONG)                                    :: n
     integer(kind=LONG)                                    :: nl
     integer(kind=LONG)                                    :: nc
     real(kind=DOUBLE)  ,dimension(:)  ,allocatable        :: x
     real(kind=DOUBLE)  ,dimension(:)  ,allocatable        :: y
     real(kind=DOUBLE)  ,dimension(:,:),allocatable        :: h
     real(kind=DOUBLE)  ,dimension(:,:),allocatable        :: ht
     real(kind=DOUBLE)  ,dimension(:,:),allocatable        :: work1
     real(kind=DOUBLE)  ,dimension(:,:),allocatable        :: invwork1
     real(kind=DOUBLE)  ,dimension(:,:),allocatable        :: coefparabo
     real(kind=DOUBLE)  ,dimension(:)  ,allocatable        :: wr
     real(kind=DOUBLE)  ,dimension(:,:),allocatable        :: z
     real(kind=DOUBLE)  ,dimension(:,:),allocatable        :: work2
     real(kind=DOUBLE)                                     :: a
     real(kind=DOUBLE)                                     :: b
     real(kind=DOUBLE)                                     :: c
     real(kind=DOUBLE)                                     :: d
     real(kind=DOUBLE)                                     :: e
     real(kind=DOUBLE)                                     :: f
     real(kind=DOUBLE)                                     :: det
     real(kind=DOUBLE)                                     :: epsilon
     real(kind=DOUBLE)                                     :: l1
     real(kind=DOUBLE)                                     :: l2
     real(kind=DOUBLE)                                     :: sum
! 1- construction des vecteurs x et y a partir des (nlgp*ncgp) points
!    la description se fait a partir du point inferieur gauche vers 
!    la droite et ligne par ligne. Les indices des points de mesure
!    sont suppose etre dans l'ordre croissant dans indx et indy.
!
     ncoef = 6
     allocate( M(nlgp*ncgp,1) )
     M(1:nlgp*ncgp,1) = Vector(1:nlgp*ncgp)
     allocate( x(nlgp*ncgp) )
     allocate( y(nlgp*ncgp) )
     allocate( h(nlgp*ncgp,ncoef) )
     allocate( ht(ncoef,nlgp*ncgp) )
     allocate( work1(ncoef,ncoef) )
     allocate( invwork1(ncoef,ncoef) )
     allocate( coefparabo(ncoef,1) )
     allocate( wr(ncoef) )
     allocate( z(ncoef,ncoef) )
     allocate( work2(ncoef,nlgp*ncgp) )
!
     n = 0
     do nl = 1, nlgp
        do nc = 1, ncgp
           n = n + 1
           y(n) = indy(nc)
           x(n) = indx(nl)
        end do 
     end do
!
! 2- contruction de la matrice h pour les (nlgp*ncgp) lignes
     do n = 1, nlgp*ncgp 
        h(n,1) = x(n)*x(n) 
        h(n,2) = y(n)*y(n)
        h(n,3) = x(n)*y(n) 
        h(n,4) = x(n) 
        h(n,5) = y(n)
        h(n,6) = 1.d0
     end do
!
! 3- calcul des coefficients du paraboloide au sens des Moindres Carre
!
! 3.1- calcul de la matrice transposee de h, ht
     do n = 1, nlgp*ncgp
        do nc = 1, ncoef
           ht(nc,n) = h(n,nc)
        end do
     end do
!
! 3.2- calcul de ht*h
     call matmult( ncoef,nlgp*ncgp,ncoef, &
                   ncoef,nlgp*ncgp,ncoef, &
                   ht,h,work1 )
!
! 3.3- calcul de la matrice inverse de work1
     call inversmat( ncoef,ncoef,work1,invwork1,wr,z )
!
! 3.4- invwork1*ht
     call matmult( ncoef,ncoef,nlgp*ncgp, &
                   ncoef,ncoef,nlgp*ncgp, &
                   invwork1,ht,work2 )
!
! 3.5- work2*m
     call matmult( ncoef,nlgp*ncgp,1, &
                   ncoef,nlgp*ncgp,1,     &
                   work2,M,coefparabo )      
!
     a = coefparabo(1,1)
     b = coefparabo(2,1)
     c = coefparabo(3,1)
     d = coefparabo(4,1)
     e = coefparabo(5,1)
     f = coefparabo(6,1)
!
! 4- determination de la position du sommet

     det = c**2 -(2.d0*a*2.d0*b)
     if( dabs(det) >= 0.1e-30 ) then
        xsom = ((2.d0*b*d)-(c*e))/det      
        ysom = ((2.d0*a*e)-(c*d))/det
        ier  = 1
! 4.1   position du maximum en dehors de la grille
        if( (dabs(xsom) > int(nlgp/2)).or. &
            (dabs(ysom) > int(ncgp/2)) ) then
           xsom = 0.
           ysom = 0.
           ier  = 0
           write (*,*) 'maximum en dehors de la grille!'
        end if
     else
        xsom = 0.
        ysom = 0.
        ier  = 0
     end if
!
! 5  valeur du sommet
     vsom=a*xsom**2.d0+b*ysom**2.d0+c*xsom*ysom+d*xsom+e*ysom+f
!
! 6  precision sur la determination de l'extremum    
     l1 = (a + b + dsqrt((a-b)**2.d0 + c**2.d0))/2.d0
     l2 = (a + b - dsqrt((a-b)**2.d0 + c**2.d0))/2.d0
     sum = 0
     do n = 1, nlgp*ncgp
        sum = sum + (M(n,1) - ( a*x(n)*x(n) +b*y(n)*y(n) +c*x(n)*y(n) &
                               +d*x(n) +e*y(n)+ f) )**2.d0
     end do     
     epsilon = dsqrt(sum/(nlgp*ncgp))
     if( (epsilon/dsqrt(dabs(l1))) > (epsilon/dsqrt(dabs(l2))) ) then
        precisext = 3.d0*epsilon/dsqrt(dabs(l1))
     else
        precisext = 3.d0*epsilon/dsqrt(dabs(l2)) 
     end if 
     if( (1.d0/dsqrt(dabs(l1))) > (1.d0/dsqrt(dabs(l2))) ) then
        courb = 1.d0/dsqrt(dabs(l1))
     else
        courb = 1.d0/dsqrt(dabs(l2))
     end if
!
! 7  precision de la correlation, moyenne quadratique des 
!    residus d'ajustement du polynome
     preciscorr = epsilon
!
     deallocate( M )
     deallocate( x )
     deallocate( y )
     deallocate( h )
     deallocate( ht )
     deallocate( work1 )
     deallocate( invwork1 )
     deallocate( coefparabo )
     deallocate( wr )
     deallocate( z )
     deallocate( work2 )
     return
  end subroutine search_extremum
!
!
  subroutine phasemono( phase , Npt )
  implicit none
    integer(kind=LONG) ,intent(in)                   :: Npt
    real(kind=DOUBLE)  ,intent(inout),dimension(Npt) :: phase
    integer(kind=LONG)                               :: npi
    real(kind=DOUBLE)                                :: dphi
    integer(kind=LONG)                               :: i
!
    npi = 0
    dphi = phase(2)-phase(1)
    if (dphi.gt.Pi) then
       npi = npi-1
       dphi = dphi-twoPi
    else
       if (dphi.lt.(-Pi)) then
          npi = npi+1
          dphi = dphi+twoPi
       end if
    end if
    phase(2) = phase(1)+dphi
!
    do i = 3, Npt
       phase(i) = phase(i)+dfloat(npi)*twoPi
       dphi = phase(i)-phase(i-1)
       if (dphi.gt.Pi) then
          npi = npi-1
          dphi = dphi-twoPi
       else if (dphi.lt.(-Pi)) then
          npi = npi+1
          dphi = dphi+twoPi
       end if
       phase(i) = phase(i-1)+dphi
    end do
!
    return
  end subroutine phasemono
!
!
  subroutine smoothcosine( NSample, Edge, x, y )
  implicit none
    integer(kind=LONG), intent(in)                       :: NSample
    character(len=*)  , intent(in)                       :: Edge
    real(kind=DOUBLE) , dimension(NSample), intent(in)   :: x
    real(kind=DOUBLE) , dimension(NSample), intent(inout):: y
    integer(kind=LONG)                                   :: Ns
    real(kind=DOUBLE)                                    :: Arg
    real(kind=DOUBLE)                                    :: Arg0
    real(kind=DOUBLE)                                    :: Weight
!
    Arg0 = Pi / ( x(NSample) - x(1) )
    if(      Edge == 'LEFT'  ) then
       write(*,*) 'Edge ',Edge
       do Ns = 1, NSample
          Arg = Arg0 * ( x(Ns) - x(1) )
          Weight = 0.5d+00 * ( 1.d+00 - dcos( Arg ) )
          y(Ns) = Weight * y(Ns)
       end do
    else if( Edge == 'RIGHT' ) then
       write(*,*) 'Edge ',Edge
       do Ns = 1, NSample
          Arg = Arg0 * ( x(Ns) - x(1) )
          Weight = 0.5d+00 * ( 1.d+00 - dcos( Arg ) )
          y(NSample-Ns+1) = Weight * y(NSample-Ns+1)
       end do
    else
       write(*,*) 'Edge smoothing Error ',Edge
       call exit(1)
    end if
!
    return
  end subroutine smoothcosine
!
!
  subroutine smootherf( NSample, Edge, dx, x, y )
  implicit none
    integer(kind=LONG), intent(in)                       :: NSample
    character(len=*)  , intent(in)                       :: Edge
    real(kind=DOUBLE) , intent(in)                       :: dx
    real(kind=DOUBLE) , dimension(NSample), intent(in)   :: x
    real(kind=DOUBLE) , dimension(NSample), intent(inout):: y
    integer(kind=LONG)                                   :: Ns
    integer(kind=LONG)                                   :: NSamplep
    real(kind=DOUBLE)                                    :: Erf
    real(kind=DOUBLE)                                    :: sigma
    real(kind=DOUBLE)                                    :: ln2
    real(kind=DOUBLE)                                    :: g
    real(kind=DOUBLE)                                    :: coef
    real(kind=DOUBLE)                                    :: coeff
    real(kind=DOUBLE)                                    :: x0
!
    NSamplep = int(NSample/2)
    ln2   = log(2.d+00)
    g     = dble(NSamplep)*dx
    sigma = 3.d+00*ln2/Pi/g
    coef  = ((sigma*Pi)**2)/ln2
    coeff = dsqrt(Pi/ln2)*sigma*dx
    x0    = x(NSamplep+1)
    Erf   = 0.d+00
    if(       Edge == 'LEFT' ) then
       do Ns = 1, NSample 
          Erf = Erf + coeff * dexp( -coef * (x(Ns)-x0)**2)
          y(Ns) = Erf * y(Ns)
       end do
    else if( Edge == 'RIGHT' ) then
       do Ns = 1, NSample
          Erf = Erf + coeff * dexp( -coef * (x(Ns)-x0)**2)
          y(NSample-Ns+1) = Erf * y(NSample-Ns+1)
       end do
    else
       write(*,*) 'Edge smoothing Error ',Edge
       call exit(1)
    end if
!
    return
  end subroutine smootherf
!
!
  subroutine smoothlinear( NSample, Edge, x, y )
  implicit none
    integer(kind=LONG), intent(in)                       :: NSample
    character(len=*)  , intent(in)                       :: Edge
    real(kind=DOUBLE) , dimension(NSample), intent(in)   :: x
    real(kind=DOUBLE) , dimension(NSample), intent(inout):: y
    integer(kind=LONG)                                   :: Ns
    integer(kind=LONG)                                   :: NSamplep
    real(kind=DOUBLE)                                    :: Lin
    real(kind=DOUBLE)                                    :: x1
    real(kind=DOUBLE)                                    :: Deltax
!
    NSamplep = int(NSample/2)
    x1       = x(1)
    Deltax   = (x(NSample)-x(1))
    if(       Edge == 'LEFT' ) then
       do Ns = 1, NSample 
          Lin = (x(Ns)-x1) / Deltax
          y(Ns) = Lin * y(Ns)
       end do
    else if( Edge == 'RIGHT' ) then
       do Ns = 1, NSample
          Lin = (x(Ns)-x1) / Deltax
          y(NSample-Ns+1) = Lin * y(NSample-Ns+1)
       end do
    else
       write(*,*) 'Edge smoothing Error ',Edge
       call exit(1)
    end if
!
    return
  end subroutine smoothlinear
!
!
  subroutine windowing( npti,porte,ldimp,dx )
  implicit none
!     amortissement des bords d'une porte reelle par produit
!     des bords de la porte par l'integrale partielle d'une gaussienne
!     definie dans l'espace des frequences. 
!     ce qui revient a convoluer par la gaussienne une porte 
!     reduite aux deux extremitees d'une demi largeur de gaussienne
!     sigma : demi largeur a mi hauteur de la gaussienne dans l'espace des 
!     frequences (norme 1 pour la frequence nulle) en cm-1
!     dx : pas d'echantillonnage de la porte en cm
!     la gaussienne de convolution a un domaine d'application limite a
!     2*ldimp+1
!     ce domaine ldimp correspond a 4 demi largeur a mi hauteur 

      integer(kind=LONG), intent(in)                       :: npti
      integer(kind=LONG), intent(in)                       :: ldimp
      real(kind=DOUBLE) , intent(in)                       :: dx
      real(kind=DOUBLE) , dimension(npti), intent(inout)   :: porte
      integer(kind=LONG)   :: ldim,l,m
      real(kind=DOUBLE)    :: sigma,ln2,glarge,coef,coeff
      real(kind=DOUBLE)    :: erf,x
!
!     initialisation des coefficients de la fonction d'amortissement
      ln2=log(2.d+00)
!     nombre d'echantillons impliques
      ldim=2*ldimp+1
      if(ldim > npti) then
         write(*,*) ' windowing Error',ldim,npti
         call exit(1)
      end if
!     demi largeur du domaine d'application de l'amortissement
      glarge=dble(ldimp)*dx
!     demi largeur de la gaussienne
      sigma=4.d+00*ln2/Pi/glarge
      coef=((sigma*Pi)**2)/ln2
      coeff=dsqrt(Pi/ln2)*sigma*dx
!     ponderation des bords de la porte
      ldim=2*ldimp+1
      erf=0.d+00
      do l=1,ldim
         x=dx*dble(l-ldimp-1)
         erf=erf+coeff*dexp(-coef*x**2)
         m=npti-l+1
         porte(l)=porte(l)*erf
         porte(m)=porte(m)*erf
      end do
!
      return
  end subroutine windowing
!
!
  subroutine windowingcplx( npti,porte,ldimp,dx )
  implicit none
!     amortissement des bords d'une porte complexe par produit
!     des bords de la porte par l'integrale partielle d'une gaussienne
!     definie dans l'espace des frequences. 
!     ce qui revient a convoluer par la gaussienne une porte 
!     reduite aux deux extremitees d'une demi largeur de gaussienne
!     sigma : demi largeur a mi hauteur de la gaussienne dans l'espace des 
!     frequences (norme 1 pour la frequence nulle) en cm-1
!     dx : pas d'echantillonnage de la porte en cm
!     la gaussienne de convolution a un domaine d'application limite a
!     2*ldimp+1
!     ce domaine ldimp correspond a 4 demi largeur a mi hauteur 

      integer(kind=LONG), intent(in)                       :: npti
      integer(kind=LONG), intent(in)                       :: ldimp
      real(kind=DOUBLE) , intent(in)                       :: dx
      complex(kind=DOUBLE), dimension(npti), intent(inout) :: porte
      integer(kind=LONG)   :: ldim,l,m
      real(kind=DOUBLE)    :: sigma,ln2,glarge,coef,coeff
      real(kind=DOUBLE)    :: erf,x
!
!     initialisation des coefficients de la fonction d'amortissement
      ln2=log(2.d+00)
!     nombre d'echantillons impliques
      ldim=2*ldimp+1
      if(ldim > npti) then
         write(0,*) ' erreur amortissement',ldim,npti
         call exit(1)
      end if
!     demi largeur du domaine d'application de l'amortissement
      glarge=dble(ldimp)*dx
!     demi largeur de la gaussienne
      sigma=4.d+00*ln2/Pi/glarge
      coef=((sigma*Pi)**2)/ln2
      coeff=dsqrt(Pi/ln2)*sigma*dx
!     ponderation des bords de la porte
      ldim=2*ldimp+1
      erf=0.d+00
      do l=1,ldim
         x=dx*dble(l-ldimp-1)
         erf=erf+coeff*dexp(-coef*x**2)
         m=npti-l+1
         porte(l)=porte(l)*erf
         porte(m)=porte(m)*erf
      end do
!
      return
  end subroutine windowingcplx
!
!
  subroutine modargtocplx( c, mod, arg, npti )
  implicit none
      complex(kind=DOUBLE),intent(inout),dimension(1:npti)  :: c
      real(kind=DOUBLE)   ,intent(in) ,dimension(1:npti)    :: mod
      real(kind=DOUBLE)   ,intent(in) ,dimension(1:npti)    :: arg
      integer(kind=LONG)  ,intent(in)                       :: npti
!
      c(1:npti) = dcmplx( mod(1:npti)*dcos(arg(1:npti)) &
                         ,mod(1:npti)*dsin(arg(1:npti)) )
!
      return
  end subroutine modargtocplx
!
!
  subroutine cplxtomodarg( c, mod, arg, npti  )
  implicit none
      complex(kind=DOUBLE),intent(in) ,dimension(1:npti)    :: c
      real(kind=DOUBLE)   ,intent(inout),dimension(1:npti)  :: mod
      real(kind=DOUBLE)   ,intent(inout),dimension(1:npti)  :: arg
      integer(kind=LONG)  ,intent(in)                       :: npti
!
      mod(1:npti) = dsqrt( dreal(c(1:npti)*dconjg(c(1:npti))) )
      arg(1:npti) = datan2( dimag(c(1:npti)),dreal(c(1:npti)) )
!
      return
  end subroutine cplxtomodarg
!
!
  subroutine cplxtorealimag( c, r, i, npti  )
  implicit none
      complex(kind=DOUBLE),intent(in) ,dimension(1:npti)    :: c
      real(kind=DOUBLE)   ,intent(inout),dimension(1:npti)  :: r
      real(kind=DOUBLE)   ,intent(inout),dimension(1:npti)  :: i
      integer(kind=LONG)  ,intent(in)                       :: npti
!
      r(1:npti) = dreal( c(1:npti) )
      i(1:npti) = dimag( c(1:npti) )
!
      return
  end subroutine cplxtorealimag
!
!
  subroutine realimagtocplx( c, r, i, npti  )
  implicit none
      complex(kind=DOUBLE),intent(inout),dimension(1:npti)  :: c
      real(kind=DOUBLE)   ,intent(in) ,dimension(1:npti)    :: r
      real(kind=DOUBLE)   ,intent(in) ,dimension(1:npti)    :: i
      integer(kind=LONG)  ,intent(in)                       :: npti
!
      c(1:npti) = dcmplx( r(1:npti),i(1:npti) )
!
      return
  end subroutine realimagtocplx
!
!
  subroutine modargtorealimag( mod, arg, r, i, npti )
  implicit none
      real(kind=DOUBLE)   ,intent(inout),dimension(1:npti)  :: r
      real(kind=DOUBLE)   ,intent(inout),dimension(1:npti)  :: i
      real(kind=DOUBLE)   ,intent(in) ,dimension(1:npti)    :: mod
      real(kind=DOUBLE)   ,intent(in) ,dimension(1:npti)    :: arg
      integer(kind=LONG)  ,intent(in)                       :: npti
!
      r(1:npti) = mod(1:npti)*dcos(arg(1:npti))
      i(1:npti) = mod(1:npti)*dsin(arg(1:npti))
!
      return
  end subroutine modargtorealimag
!
!
  subroutine realimagtomodarg( mod, arg, r, i, npti )
  implicit none
      real(kind=DOUBLE)   ,intent(in) ,dimension(1:npti)    :: r
      real(kind=DOUBLE)   ,intent(in) ,dimension(1:npti)    :: i
      real(kind=DOUBLE)   ,intent(inout),dimension(1:npti)  :: mod
      real(kind=DOUBLE)   ,intent(inout),dimension(1:npti)  :: arg
      integer(kind=LONG)  ,intent(in)                       :: npti
!
      mod(1:npti) = dsqrt( r(1:npti)**2 + i(1:npti)**2 )
      arg(1:npti) = datan2( i(1:npti), r(1:npti) )
!
      return
  end subroutine realimagtomodarg
!
!
  subroutine clock( Date, hh, mm, ss )
  implicit none
      character(len=60)  ,intent(in)                        :: Date
      integer(kind=LONG) ,intent(out)                       :: hh
      integer(kind=LONG) ,intent(out)                       :: mm
      integer(kind=LONG) ,intent(out)                       :: ss
!
      read(Date,fmt='(11x,3(i2,1x))') hh,mm,ss
!
      return
  end subroutine clock
!
!
  subroutine grnd_init( Date )
  implicit none
      character(len=60)  ,intent(in)                        :: Date
      integer(kind=LONG)                                    :: hh
      integer(kind=LONG)                                    :: mm
      integer(kind=LONG)                                    :: ss
      integer(kind=LONG)                                    :: ks
      integer(kind=LONG)                                    :: kk
      real(kind=DOUBLE)                                     :: br
!
      call clock(Date,hh,mm,ss)
      ks = mm*ss+1
      do kk = 1, ks
        br = prnd()
      end do
      br = grnd(1)
!
      return
  end subroutine grnd_init
!
!
   subroutine extract_linear_argument( NsOpd, Opd, NsFit, SMod, SArg )
   implicit none
   !$PRAGMA C( crvlinearleastsqr )
     integer(kind=LONG)                                    :: crvlinearleastsqr
     integer(kind=LONG)  ,intent(in)                       :: NsOpd
     real(kind=DOUBLE)   ,intent(in),dimension(1:NsOpd)    :: Opd
     integer(kind=LONG)  ,intent(in)                       :: NsFit
     real(kind=DOUBLE)   ,intent(in)   ,dimension(1:NsOpd) :: SMod
     real(kind=DOUBLE)   ,intent(inout),dimension(1:NsOpd) :: SArg
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: XFit
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: YFit
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: Pds
     integer(kind=LONG)                                    :: Ns0
     real(kind=DOUBLE)                                     :: A0
     real(kind=DOUBLE)                                     :: A1
     real(kind=DOUBLE)                                     :: rms
     integer(kind=LONG)                                    :: ires
     integer(kind=LONG)                                    :: ErrCode
!
     allocate( XFit(2*NsFit+1),  stat=ErrCode )
     allocate( YFit(2*NsFit+1),  stat=ErrCode )
     allocate( Pds(2*NsFit+1),   stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation extract_linear_argument Error'
        write(0,*) 'extract_linear_argument : fatal error'
        call exit(1)
     end if
!
!    region du fit lineaire
     Ns0 = int(NsOpd/2)+1
     XFit(1:2*NsFit+1) = Opd(Ns0-NsFit:Ns0+NsFit)
     YFit(1:2*NsFit+1) = SArg(Ns0-NsFit:Ns0+NsFit)
     Pds(1:2*NsFit+1)  = SMod(Ns0-NsFit:Ns0+NsFit)
!
!    linearisation de la region du fit
     ires = crvlinearleastsqr( %VAL(2*NsFit+1), &
                               XFit,            &
                               YFit,            &
                               Pds,             &
!
                               A0,              &
                               A1,              &
                               rms )
     if( ires == 0 ) then
!
!       extraction de la composante lineaire
        write(*,*) ' A0,A1,rms',A0,A1,rms
        SArg(1:NsOpd) = SArg(1:NsOpd) - (A1*Opd(1:NsOpd))
     else
        write(0,*) 'Warnning linear fit abort'
        call exit(1)
     end if
!
     deallocate( XFit )
     deallocate( YFit )
     deallocate( Pds )
!
     return
   end subroutine extract_linear_argument
!
!
   subroutine extract_lin_barycentre( NsOpd, Opd, NsFit, SMod, SArg, WnShift )
   implicit none
   !$PRAGMA C( crvlinearleastsqr )
     integer(kind=LONG)                                    :: crvlinearleastsqr
     integer(kind=LONG)  ,intent(in)                       :: NsOpd
     real(kind=DOUBLE)   ,intent(in),dimension(1:NsOpd)    :: Opd
     integer(kind=LONG)  ,intent(in)                       :: NsFit
     real(kind=DOUBLE)   ,intent(in)   ,dimension(1:NsOpd) :: SMod
     real(kind=DOUBLE)   ,intent(in)   ,dimension(1:NsOpd) :: SArg
     real(kind=DOUBLE)   ,intent(inout)                    :: WnShift
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: XFit
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: YFit
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: Pds
     integer(kind=LONG)                                    :: Ns0
     real(kind=DOUBLE)                                     :: A0
     real(kind=DOUBLE)                                     :: A1
     real(kind=DOUBLE)                                     :: rms
     integer(kind=LONG)                                    :: ires
     integer(kind=LONG)                                    :: ErrCode
!
     allocate( XFit(2*NsFit+1),  stat=ErrCode )
     allocate( YFit(2*NsFit+1),  stat=ErrCode )
     allocate( Pds(2*NsFit+1),   stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation extract_lin_barycentre Error'
        write(0,*) 'extract_lin_barycentre : fatal error'
        call exit(1)
     end if
!
!    region du fit lineaire
     Ns0 = int(NsOpd/2)+1
     XFit(1:2*NsFit+1) = Opd(Ns0-NsFit:Ns0+NsFit)
     YFit(1:2*NsFit+1) = SArg(Ns0-NsFit:Ns0+NsFit)
     Pds(1:2*NsFit+1)  = SMod(Ns0-NsFit:Ns0+NsFit)
!
!    linearisation de la region du fit
     ires = crvlinearleastsqr( %VAL(2*NsFit+1), &
                               XFit,            &
                               YFit,            &
                               Pds,             &
!
                               A0,              &
                               A1,              &
                               rms )
     if( ires == 0 ) then
!
!       compute spectral shift
        WnShift = A1 / twoPi
     else
        write(0,*) ' Warnning linear fit abort'
        call exit(1)
     end if
!
     deallocate( XFit )
     deallocate( YFit )
     deallocate( Pds )
!
     return
   end subroutine extract_lin_barycentre
!
!
   subroutine extract_poly_barycentre( NsOpd, Opd, deg_polynom, &
                                       SMod, SArg, WnShift )
   implicit none
   !$PRAGMA C( crvpolyLeastsqr )
     integer(kind=LONG)                                    :: crvpolyLeastsqr
     integer(kind=LONG)  ,intent(in)                       :: NsOpd
     real(kind=DOUBLE)   ,intent(in),dimension(1:NsOpd)    :: Opd
     integer(kind=LONG)  ,intent(in)                       :: deg_polynom
     real(kind=DOUBLE)   ,intent(in)   ,dimension(1:NsOpd) :: SMod
     real(kind=DOUBLE)   ,intent(in)   ,dimension(1:NsOpd) :: SArg
     real(kind=DOUBLE)   ,intent(inout)                    :: WnShift
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: XFit
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: YFit
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: Wgt
     real(kind=DOUBLE)   ,dimension(:) ,allocatable        :: A
     real(kind=DOUBLE)                                     :: rms
     integer(kind=LONG)                                    :: ires
     integer(kind=LONG)                                    :: ErrCode
     integer(kind=LONG)                                    :: ioalloc
!
!
     ioalloc = 0
     allocate( A(0:deg_polynom),  stat=ErrCode  )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( XFit(NsOpd),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( YFit(NsOpd),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Wgt(NsOpd),   stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
!
     if ( ioalloc > 0) then
        write(0,*) 'allocation extract_linear_argument Error'
        write(0,*) 'extract_linear_argument : fatal error'
        call exit(1)
     end if
!
!    data to be filtered
     XFit(1:NsOpd) = Opd(1:NsOpd)
     YFit(1:NsOpd) = SArg(1:NsOpd)
     Wgt(1:NsOpd)  = SMod(1:NsOpd)
!
!    linearisation de la region du fit
     ires = crvpolyLeastsqr( %VAL(NsOpd),      &
                             %VAL(deg_polynom),&
                             XFit,             &
                             YFit,             &
                             Wgt,              &
                             A(0:deg_polynom), &
                             rms )
     if( ires == 0 ) then
!
!       compute spectral shift
        WnShift = A(1) / twoPi
     else
        write(0,*) ' Warnning PolyLeastsqr fit abort'
        call exit(1)
     end if
!
     deallocate( XFit )
     deallocate( YFit )
     deallocate( Wgt )
     deallocate( A )
!
     return
   end subroutine extract_poly_barycentre
!
!
   subroutine factorial( N, Fact )
   implicit none
   integer(kind=LONG) ,intent(in)          :: N
   integer(kind=LONG) ,intent(out)         :: Fact
   integer(kind=LONG)                      :: i
!
     Fact = product( (/(i,i=1,N)/) )
!
     return
   end subroutine factorial
!
!
   subroutine factorial_recursive( N, Fact )
   implicit none
   integer(kind=LONG) ,intent(in)          :: N
   integer(kind=LONG) ,intent(inout)       :: Fact
!
     if( N == 0 ) then
        Fact = 1
     else if( N == 1 ) then
        Fact = 1
     else
        Fact = N * Fact
     end if
!
     return
   end subroutine factorial_recursive
!!
!!
!> fwhm_real -- Public
!!
!! * Purpose
!!
!!     Computation of the Full Width at Half Maximum value 
!!
!! * Description
!!
!!   This subroutine allows the Full Width at Half Maximum value ;
!!   its computation requires the following steps:
!!    - The first steps are the definition of the NsIsrfp+1 pivot point, the 
!!      oversampling and required tables allocation.
!!    - Then Cartesian coordinates along x-line are computed for each wave 
!!      number relativly to the pivot point
!!    - ISRf real part is extracted  
!!    - Cartesian coordinates are oversampled by a factor 10
!!    - ISRF real part is also oversampled by cubic spline 
!!    - The ISRF maximum is determined, as well as the ISRF value at half maximum 
!!    - At last, before all table deallocation, the last steps concern are the 
!!      computation of the width at half maximum value thanks to interpolation
!!
!! * Inputs
!!
!!    - NsIsrf : Isrf sample number
!!    - Isrf   : Instrumentatl spectral response function - real
!!    - dWn    : wavenumber sampling step
!!
!! * Inputs/outputs
!!
!!    - Fwhm   : Full Width at Half Maximum value
!!
!! * Outputs
!!
!! * References
!!
   subroutine fwhm_real( NsIsrf ,Isrf ,dWn ,Fwhm )
   implicit none
     integer(kind=LONG) ,intent(in)                     :: NsIsrf
     real(kind=DOUBLE)  ,intent(in) ,dimension(NsIsrf)  :: Isrf
     real(kind=DOUBLE)  ,intent(in)                     :: dWn
     real(kind=DOUBLE)  ,intent(inout)                  :: Fwhm
     integer(kind=LONG)                                 :: ErrCode
     integer(kind=LONG)                                 :: NsIsrfp
     integer(kind=LONG)                                 :: Ns
     integer(kind=LONG) ,parameter                      :: Nsurech=10
     real(kind=DOUBLE)  ,dimension(:) ,allocatable      :: crp
     real(kind=DOUBLE)  ,dimension(:) ,allocatable      :: crs
     real(kind=DOUBLE)  ,dimension(:) ,allocatable      :: cip
     real(kind=DOUBLE)  ,dimension(:) ,allocatable      :: cis
     real(kind=DOUBLE)                                  :: dWnc
     integer(kind=LONG)                                 :: NsIsrfc
     integer(kind=LONG)                                 :: NsIsrfcp
     real(kind=DOUBLE)                                  :: rmax
     real(kind=DOUBLE)                                  :: rmaxs2
     integer(kind=LONG)                                 :: Ns1
     integer(kind=LONG)                                 :: Ns2
     real(kind=DOUBLE)                                  :: Wn1
     real(kind=DOUBLE)                                  :: Wn2
!
!    NsIsrfp+1 pivot point
     NsIsrfp  = int(NsIsrf/2)
!
!    surechantillonnage
     NsIsrfc  = (NsIsrf-1)*Nsurech+1
     NsIsrfcp = int(NsIsrfc/2)
!
!    allocation
     allocate( crp(NsIsrf),   stat=ErrCode )
     allocate( cip(NsIsrf),   stat=ErrCode )
     allocate( crs(NsIsrfc),  stat=ErrCode )
     allocate( cis(NsIsrfc),  stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Error'
        write(0,*) 'fwhm_real : fatal error'
        call exit(1)
     end if
!
!    Cartesian coordinates along x-line computed for each wave 
!    number relativly to the pivot point
     cip(1:NsIsrf) = dWn * (/ (dble(Ns-NsIsrfp-1),Ns=1,NsIsrf) /)
!
!    extraction of the ISRf real part 
     crp(1:NsIsrf) = Isrf(1:NsIsrf)
!
!    Cartesian coordinates oversampled by a factor 10
     dWnc = dWn/dble(Nsurech)
     cis(1:NsIsrfc) = dWnc * (/ (dble(Ns-NsIsrfcp-1),Ns=1,NsIsrfc) /)
!
!    ISRF real part oversampled by cubic spline 
     call intspline( NsIsrf,cip,crp,NsIsrfc,cis,crs )
!
!    ISRF maximum 
     rmax = maxval( crs(1:NsIsrfc) )
!
!    ISRF value at half maximum 
     rmaxs2 = rmax/2.d+00
!    width at half maximum value thanks to interpolation
     do Ns = 1, NsIsrfc-1
        if( (crs(Ns).le.rmaxs2) .and. (crs(Ns+1).gt.rmaxs2) ) Ns1=Ns
        if( (crs(Ns).ge.rmaxs2) .and. (crs(Ns+1).lt.rmaxs2) ) Ns2=Ns
     end do
!    interpolation
     Wn1 = dWnc*(rmaxs2-crs(Ns1))/(crs(Ns1+1)-crs(Ns1))+cis(Ns1)
     Wn2 = dWnc*(rmaxs2-crs(Ns2))/(crs(Ns2+1)-crs(Ns2))+cis(Ns2)
     Fwhm = Wn2 - Wn1
!
!    deallocation
     deallocate(crp)
     deallocate(cip)
     deallocate(crs)
     deallocate(cis)
     return
   end subroutine fwhm_real
!
!


!!
!!
!> fwhm_cplx -- Public
!!
!! * Purpose
!!
!!     Computation of the Full Width at Half Maximum value 
!!
!! * Description
!!
!!   This subroutine allows the Full Width at Half Maximum value ; 
!!   its computationrequires the following steps:
!!    - The first steps are the definition of the NsIsrfp+1 pivot point, the 
!!      oversampling and required tables allocation.
!!    - Then Cartesian coordinates along x-line are computed for each wave 
!!      number relativly to the pivot point
!!    - ISRf real part is extracted  
!!    - Cartesian coordinates are oversampled by a factor 10
!!    - ISRF real part is also oversampled by cubic splin 
!!    - The ISRF maximum is determined, as well as the ISRF value at half maximum 
!!    - At last, before all table deallocation, the last steps concern are the 
!!      computation of the width at half maximum value thanks to interpolation
!!
!! * Inputs
!!
!!    - NsIsrf : Isrf sample number
!!    - Isrf   : Instrumentatl spectral response function - cplx
!!    - dWn    : wavenumber sampling step
!!
!! * Inputs/outputs
!!
!!    - Fwhm   : Full Width at Half Maximum value
!!
!! * Outputs
!!
!! * References
!!

   subroutine fwhm_cplx( NsIsrf ,Isrf ,dWn ,Fwhm )
   implicit none
     integer(kind=LONG)  ,intent(in)                     :: NsIsrf
     complex(kind=DOUBLE),intent(in) ,dimension(NsIsrf)  :: Isrf
     real(kind=DOUBLE)   ,intent(in)                     :: dWn
     real(kind=DOUBLE)   ,intent(inout)                  :: Fwhm
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: NsIsrfp
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)  ,parameter                      :: Nsurech=10
     real(kind=DOUBLE)   ,dimension(:) ,allocatable      :: crp
     real(kind=DOUBLE)   ,dimension(:) ,allocatable      :: crs
     real(kind=DOUBLE)   ,dimension(:) ,allocatable      :: cip
     real(kind=DOUBLE)   ,dimension(:) ,allocatable      :: cis
     real(kind=DOUBLE)                                   :: dWnc
     integer(kind=LONG)                                  :: NsIsrfc
     integer(kind=LONG)                                  :: NsIsrfcp
     real(kind=DOUBLE)                                   :: rmax
     real(kind=DOUBLE)                                   :: rmaxs2
     integer(kind=LONG)                                  :: Ns1
     integer(kind=LONG)                                  :: Ns2
     real(kind=DOUBLE)                                   :: Wn1
     real(kind=DOUBLE)                                   :: Wn2
!
!    point pivot NsIsrfp+1
     NsIsrfp  = int(NsIsrf/2)
!
!    surechantillonnage
     NsIsrfc  = (NsIsrf-1)*Nsurech+1
     NsIsrfcp = int(NsIsrfc/2)
!
!    allocation
     allocate( crp(NsIsrf),   stat=ErrCode )
     allocate( cip(NsIsrf),   stat=ErrCode )
     allocate( crs(NsIsrfc),  stat=ErrCode )
     allocate( cis(NsIsrfc),  stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Error'
        write(0,*) 'fwhm_cplx : fatal error'
        call exit(1)
     end if
!
!    calcul des abscisses en nombre d'onde relatives au point pivot
     cip(1:NsIsrf) = dWn * (/ (dble(Ns-NsIsrfp-1),Ns=1,NsIsrf) /)
!
!    extraction de la partie reelle de la fonction d'instrument
     crp(1:NsIsrf) = dreal( Isrf(1:NsIsrf) )
!    calcul des abscisses surechantillonnees d'un facteur 10
     dWnc = dWn/dble(Nsurech)
     cis(1:NsIsrfc) = dWnc * (/ (dble(Ns-NsIsrfcp-1),Ns=1,NsIsrfc) /)
!
!    surechantillonnage par spline cubique de la partie reelle des ISRF
     call intspline( NsIsrf,cip,crp,NsIsrfc,cis,crs )
!
!    recherche du maximum de l'ISRF
     rmax = maxval( crs(1:NsIsrfc) )
!    valeur de l'ISRF a mi-hauteur
     rmaxs2 = rmax/2.d+00
!    recherche de la largeur a mi hauteur
     do Ns = 1, NsIsrfc-1
        if( (crs(Ns).le.rmaxs2) .and. (crs(Ns+1).gt.rmaxs2) ) Ns1=Ns
        if( (crs(Ns).ge.rmaxs2) .and. (crs(Ns+1).lt.rmaxs2) ) Ns2=Ns
     end do
!    recherche de la largeur par interpolation
     Wn1 = dWnc*(rmaxs2-crs(Ns1))/(crs(Ns1+1)-crs(Ns1))+cis(Ns1)
     Wn2 = dWnc*(rmaxs2-crs(Ns2))/(crs(Ns2+1)-crs(Ns2))+cis(Ns2)
     Fwhm = Wn2 - Wn1
!
!    deallocation
     deallocate(crp)
     deallocate(cip)
     deallocate(crs)
     deallocate(cis)
!
     return
   end subroutine fwhm_cplx
!
!
end module math_module
