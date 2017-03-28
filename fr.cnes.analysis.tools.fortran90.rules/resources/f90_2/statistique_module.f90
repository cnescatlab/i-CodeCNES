! statistique_module.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.2 $
! Last modification: $Date: 2010-02-04 16:37:26 $
!
! Statistics -- Module
!
! * Purpose
!   Module for Statistics computations
!
module statistique_module
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: average         &
            ,average_pond    &
            ,std             &
            ,std_pond        &
            ,rms             &
            ,rms_pond        &
            ,median          &
            ,median_pond     &
            ,statistics      &
            ,statistics_pond
!
!
   contains
!
!
   subroutine average( NSample,X,XAvg )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
!
      real(kind=DOUBLE) , intent(out)                     :: XAvg
!
      integer(kind=LONG)                                  :: Npt
!
      Npt  = max(1,NSample)
      XAvg = sum( X(1:NSample) ) / dble(Npt)
!
      return
   end subroutine average
!
!
   subroutine average_pond( NSample,X,Wgt,XAvg )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: Wgt
!
      real(kind=DOUBLE) , intent(out)                     :: XAvg
!
      integer(kind=LONG)                                  :: Npt
!
      Npt  = max(1,NSample)
      XAvg = sum( X(1:NSample)*Wgt(1:NSample) ) / sum(Wgt(1:NSample))
!
      return
   end subroutine average_pond
!
!
   subroutine std(NSample,X,XAvg,XStd  )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
      real(kind=DOUBLE) , intent(in)                      :: XAvg
!
      real(kind=DOUBLE) , intent(out)                     :: XStd
!
      integer(kind=LONG)                                  :: Npt
!
      Npt  = max(1,NSample-1)
      XStd = dsqrt( sum( (X(1:NSample)-XAvg)**2 ) /dble(Npt) )
!
      return
   end subroutine std
!
!
   subroutine std_pond(NSample,X,Wgt,XAvg,XStd  )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: Wgt
      real(kind=DOUBLE) , intent(in)                      :: XAvg
!
      real(kind=DOUBLE) , intent(out)                     :: XStd
!
      XStd = dsqrt( sum( ((X(1:NSample)*Wgt(1:NSample))-XAvg)**2 ) &
                  / sum(Wgt(1:NSample)) )
!
      return
   end subroutine std_pond
!
!
   subroutine rms( NSample,X,XRms )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
!
      real(kind=DOUBLE) , intent(out)                     :: XRms
!
      integer(kind=LONG)                                  :: Npt
!
      Npt  = max(1,NSample)
      XRms = dsqrt( sum( (X(1:NSample))**2 ) /dble(Npt) )
!
      return
   end subroutine rms
!
!
   subroutine rms_pond( NSample,X,Wgt,XRms )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: Wgt
!
      real(kind=DOUBLE) , intent(out)                     :: XRms
!
      XRms = dsqrt( sum( ((X(1:NSample)*Wgt(1:NSample)))**2 ) &
                  / sum(Wgt(1:NSample)) )
!
      return
   end subroutine rms_pond
!
!
   subroutine median( NSample,X,XMed )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
!
      real(kind=DOUBLE) , intent(out)                     :: XMed
!
      integer(kind=LONG)                                  :: Npt
      integer(kind=LONG)                                  :: Npt2
      integer(kind=LONG)                                  :: Npt2odd
!
      Npt  = max(1,NSample)
      Npt2 = int(Npt/2)
      Npt2odd = (1 + Npt)/2
      if (Npt < 3) then
        if (Npt == 1) then
          XMed = X(1)
        else
          XMed = 0.5*sum(X(1:Npt))
        end if
      else
        if (Npt == 2*Npt2) then
          XMed = 0.5*(X(Npt2) + X(Npt2+1))
        else
          XMed = X(Npt2odd)
        end if
      end if
      
!
      return
   end subroutine median
!
!
   subroutine median_pond( NSample,X,Wgt,XMed )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: Wgt
!
      real(kind=DOUBLE) , intent(out)                     :: XMed
!
      integer(kind=LONG)                                  :: Npt
      integer(kind=LONG)                                  :: Npt2
      integer(kind=LONG)                                  :: Npt2odd
!
      Npt  = max(1,NSample)
      Npt2 = int(Npt/2)
      Npt2odd = (1 + Npt)/2
      if (Npt < 3) then
        if (Npt == 1) then
          XMed = X(1)*Wgt(1)
        else
          XMed = 0.5*sum(X(1:Npt)*Wgt(1:Npt))
        end if
      else
        if (Npt == 2*Npt2) then
          XMed = 0.5*( (X(Npt2)*Wgt(Npt2)) + (X(Npt2+1)*Wgt(Npt2+1)) )
        else
          XMed = X(Npt2odd)*Wgt(Npt2odd)
        end if
      end if
!
      return
   end subroutine median_pond
!
!
   subroutine statistics( NSample    &
                         ,X          &
!
                         ,XAvg       &
                         ,XStd       &
                         ,XMin       &
                         ,XMax       &
                         ,NsMin      &
                         ,NsMax      )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
!
      real(kind=DOUBLE) , intent(out)                     :: XAvg
      real(kind=DOUBLE) , intent(out)                     :: XStd
      real(kind=DOUBLE) , intent(out)                     :: XMin
      real(kind=DOUBLE) , intent(out)                     :: XMax
      integer(kind=LONG), intent(out)                     :: NsMin
      integer(kind=LONG), intent(out)                     :: NsMax
!
      integer(kind=LONG), dimension(:), allocatable       :: Pos
!
      allocate( Pos(1) )
!
!     moyenne
      call average( NSample,X,XAvg )
!
!     standard deviation
      call std( NSample,X,XAvg,XStd )
!
!     minimum valeur et position
      XMin = minval( X(1:NSample) )
      Pos  = minloc( X(1:NSample) )
      NsMin = Pos(1)
!
!     maximum valeur et position
      XMax = maxval( X(1:NSample) )
      Pos  = maxloc( X(1:NSample) )
      NsMax = Pos(1)
!
      deallocate( Pos )
      return
   end subroutine statistics
!
!
   subroutine statistics_pond( NSample    &
                              ,X          &
                              ,Wgt        &
!
                              ,XAvg       &
                              ,XStd       &
                              ,XMin       &
                              ,XMax       &
                              ,NsMin      &
                              ,NsMax      )
!
      integer(kind=LONG), intent(in)                      :: NSample
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: X
      real(kind=DOUBLE) , intent(in), dimension(NSample)  :: Wgt
!
      real(kind=DOUBLE) , intent(out)                     :: XAvg
      real(kind=DOUBLE) , intent(out)                     :: XStd
      real(kind=DOUBLE) , intent(out)                     :: XMin
      real(kind=DOUBLE) , intent(out)                     :: XMax
      integer(kind=LONG), intent(out)                     :: NsMin
      integer(kind=LONG), intent(out)                     :: NsMax
!
      integer(kind=LONG), dimension(:), allocatable       :: Pos
!
      allocate( Pos(1) )
!
!     moyenne
      call average_pond( NSample,X,Wgt,XAvg )
!
!     standard deviation
      call std_pond( NSample,X,Wgt,XAvg,XStd )
!
!     minimum valeur et position
      XMin = minval( X(1:NSample)*Wgt(1:NSample) )
      Pos  = minloc( X(1:NSample)*Wgt(1:NSample) )
      NsMin = Pos(1)
!
!     maximum valeur et position
      XMax = maxval( X(1:NSample)*Wgt(1:NSample) )
      Pos  = maxloc( X(1:NSample)*Wgt(1:NSample) )
      NsMax = Pos(1)
!
      deallocate( Pos )
      return
   end subroutine statistics_pond
!
!
end module statistique_module
