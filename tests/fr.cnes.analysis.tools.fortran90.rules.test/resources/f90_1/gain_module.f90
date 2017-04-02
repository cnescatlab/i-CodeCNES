! gain_module.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: august 2010
!           Version: $Revision: 1.5 $
! Last modification: $Date: 2011-11-02 14:57:49 $
!
! interferometer gain -- Module
!
! * Purpose
!   Module for interferogram gain
!
module gain_module
   use precision_type
   use error_type
   use constantes_type
   use gain_type
   use interferogram_type
   use math_module
   use statistique_module
!
   implicit none
!
!
   public ::                   &
             gain_init,        &
             interf_gainappli, &
             interf_gaincompens
!
!
   contains
!
!
!> gain_init -- Public
!!
!! * Purpose
!!
!!     Gain parameter initialisation
!!
!! * Description
!!
!!
!! * Inputs
!!
!!     - File_Header : File header of the gain parameters
!!     - File_Param  : File of the gain parameters
!!
!! * Inputs/outputs
!!
!!
!! * Outputs
!!
!!     - Gain : type_Gain / type for declaration and allocation of gain
!!
!! * References

!!
!!
   subroutine gain_init( File_Header,&
                         File_Param, &
                         Gain        )
   implicit none
     character(len=*)        ,intent(in)                 :: File_Header
     character(len=*)        ,intent(in)                 :: File_Param
     type(type_Gain)         ,intent(out)                :: Gain
!
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: NC
     integer(kind=LONG)                                  :: NL
     integer(kind=LONG)                                  :: C
     integer(kind=LONG)                                  :: L
     integer(kind=LONG)                                  :: PN
     integer(kind=LONG)                                  :: SUB_PN
     integer(kind=LONG)                                  :: C_SUB
     integer(kind=LONG)                                  :: L_SUB
     integer(kind=LONG)                                  :: N_Pixel
     integer(kind=LONG)                                  :: PosMin
     integer(kind=LONG)                                  :: PosMax
     real(kind=DOUBLE)      ,dimension(:),allocatable    :: Average_PN
     real(kind=DOUBLE)      ,dimension(:),allocatable    :: Variance_PN
     real(kind=DOUBLE)      ,dimension(:),allocatable    :: X
     integer(kind=LONG)     ,dimension(:),allocatable    :: Pos
     real(kind=DOUBLE)                                   :: Variance_PN_Avg
     real(kind=DOUBLE)                                   :: Diff_Min
           
!
     iFile = 10
!
     iPos = 1
     write(*,'(a)') File_Header(1:len_trim(File_Header))
     open(unit=iFile, file=File_Header, status='old',err=999)
     Gain%filename = File_Header
     read(iFile,'(a)',err=999) Gain%Variance_Choice
     read(iFile,*,err=999) Gain%NbCol
     read(iFile,*,err=999) Gain%NbLin
     read(iFile,*,err=999) Gain%Step
     read(iFile,*,err=999) Gain%Sensitivity_Avg
     read(iFile,*,err=999) Gain%NbCol_PN
     read(iFile,*,err=999) Gain%NbLin_PN
     read(iFile,*,err=999) Gain%Nb_SUB_PN
     read(iFile,*,err=999) Gain%NbCol_SUB_PN
     read(iFile,*,err=999) Gain%NbLin_SUB_PN
     close(unit=iFile)
     write(*,*) 'Gain%NbCol       ',Gain%NbCol
     write(*,*) 'Gain%NbLin       ',Gain%NbLin
     write(*,*) 'Gain%NbCol_PN    ',Gain%NbCol_PN
     write(*,*) 'Gain%NbLin_PN    ',Gain%NbLin_PN
     write(*,*) 'Gain%Nb_SUB_PN   ',Gain%Nb_SUB_PN
     write(*,*) 'Gain%NbCol_SUB_PN',Gain%NbCol_SUB_PN
     write(*,*) 'Gain%NbLin_SUB_PN',Gain%NbLin_SUB_PN
!
     call alloc_Gain( Gain )
!
     if( Gain%Variance_Choice == 'XXX' ) then
!
!      No gain variation
       Gain%PRNU(1:Gain%NbCol,1:Gain%NbLin) = 1.
       Gain%Gain_PN( 1:Gain%NbCol_PN,1:Gain%NbLin_PN ) = 1.
       Gain%Gain_SUB_PN( 1:Gain%NbCol_SUB_PN   &
                        ,1:Gain%NbLin_SUB_PN   &
                        ,1:Gain%Nb_SUB_PN ) = 1.
       Gain%Gain_Avg_SUB_PN(1:Gain%Nb_SUB_PN) = 1.
       Gain%Gain_Std_SUB_PN(1:Gain%Nb_SUB_PN) = 0.
       Gain%Gain_Min_SUB_PN(1:Gain%Nb_SUB_PN) = 1.
       Gain%Gain_Max_SUB_PN(1:Gain%Nb_SUB_PN) = 1.
       Gain%Gain_Avg_PN = 1.
       Gain%Gain_Std_PN = 0.
       Gain%Gain_Min_PN = 1.
       Gain%Gain_Max_PN = 1.
     else
!
!      compute gain variation
       write(*,'(a)') File_Param(1:len_trim(File_Param))
       open(unit=iFile, file=File_Param, status='old',err=999)
       iPos = 2
       do NL = 1, Gain%NbLin
          read(iFile,*,err=999) (Gain%PRNU(NC,NL),NC = 1, Gain%NbCol)
       end do
       close(unit=iFile)
!
       Gain%PRNU(1:Gain%NbCol,1:Gain%NbLin) =             &
                   Gain%PRNU(1:Gain%NbCol,1:Gain%NbLin) + 1
!
!      allocation
       allocate( X(Gain%NbCol*Gain%NbLin) )
       allocate( Average_PN(Gain%NbCol*Gain%NbLin) )
       allocate( Variance_PN(Gain%NbCol*Gain%NbLin) )
       allocate( Pos(1) )
!
!      compute average inside pixel map
       PN = 0
       do NL = 1, Gain%NbLin-Gain%NbLin_PN+1
       do NC = 1, Gain%NbCol-Gain%NbCol_PN+1
          PN = PN + 1
          Average_PN(PN) = sum( Gain%PRNU(NC:NC+Gain%NbCol_PN-1,  &
                                          NL:NL+Gain%NbLin_PN-1) )&
                         / dble(Gain%NbCol_PN*Gain%NbLin_PN)
       end do
       end do
!
!      compute variances inside pixel map
       PN = 0
       do NL = 1, Gain%NbLin-Gain%NbLin_PN+1
       do NC = 1, Gain%NbCol-Gain%NbCol_PN+1
          PN = PN + 1
          Variance_PN(PN) = dsqrt( sum( ( Gain%PRNU(NC:NC+Gain%NbCol_PN-1,  &
                                                    NL:NL+Gain%NbLin_PN-1)  &
                                         -Average_PN(PN) )**2 ) )           &
                          / dble(Gain%NbCol_PN*Gain%NbLin_PN)
       end do
       end do
       N_Pixel = PN
       Variance_PN_Avg = dsqrt( sum( Variance_PN(1:N_Pixel) )**2 ) &
                       / dble(N_Pixel)
       if(      Gain%Variance_Choice == 'MIN' ) then
!
!         search for the pixel map with the variance minimum
          Pos = minloc( Variance_PN(1:N_Pixel) )
          PN = Pos(1)
       else if( Gain%Variance_Choice == 'MAX' ) then
!
!         search for the pixel map with the variance maximum
          Pos = maxloc( Variance_PN(1:N_Pixel) )
          PN = Pos(1)
       else if( Gain%Variance_Choice == 'AVG' ) then
!
!         search for the first pixel map with the variance average
          Diff_Min = 1.d+20
          PN = 0
          do Ns = 1, N_Pixel
             if( dabs( Variance_PN(Ns)-Variance_PN_Avg ) &
                 < Diff_Min ) then
                Diff_Min = dabs( Variance_PN(Ns)-Variance_PN_Avg )
                PN = Ns
             end if
          end do
       else
          write(*,*) 'Variance_Choice Error : ',Gain%Variance_Choice
          go to 999
       end if
       if( PN == 0 ) then
          write(*,*) 'Search for Pixel map number Error'
          go to 999
       end if
!
!      convert PN number in Columns and Lines
       Gain%LinStart_PN = int( (PN-1)/(Gain%NbCol-Gain%NbCol_PN+1) ) + 1
       Gain%LinEnd_PN   = Gain%LinStart_PN + Gain%NbLin_PN - 1
       Gain%ColStart_PN = PN - ( (Gain%LinStart_PN-1)       &
                                *(Gain%NbCol-Gain%NbCol_PN+1) )
       Gain%ColEnd_PN   = Gain%ColStart_PN + Gain%NbCol_PN - 1
       write(*,*) 'N_Pixel         ',N_Pixel
       write(*,*) 'PN              ',PN
       write(*,*) 'PN Col Start End',Gain%ColStart_PN,Gain%ColEnd_PN
       write(*,*) 'PN Lin Start End',Gain%LinStart_PN,Gain%LinEnd_PN
!
!      extract gain pixel map
       Gain%Gain_PN(1:Gain%NbCol_PN,1:Gain%NbLin_PN) = &
            Gain%PRNU(Gain%ColStart_PN:Gain%ColEnd_PN, &
                      Gain%LinStart_PN:Gain%LinEnd_PN)
!
!      comptute Pixel gain statistics
       Ns = 0
       do NL = 1, Gain%NbLin_PN
          do NC = 1, Gain%NbCol_PN
             Ns = Ns + 1
             X(Ns) = Gain%Gain_PN(NC,NL)
          end do
       end do
       call statistics( Gain%NbCol_PN*Gain%NbLin_PN,&
                        X,                          &
!
                        Gain%Gain_Avg_PN,           &
                        Gain%Gain_Std_PN,           &
                        Gain%Gain_Min_PN,           &
                        Gain%Gain_Max_PN,           &
                        PosMin,                     &
                        PosMax                      )
       write(*,*) 'Pixel Stat Avg',Gain%Gain_Avg_PN
       write(*,*) 'Pixel Stat Std',Gain%Gain_Std_PN
       write(*,*) 'Pixel Stat Min',Gain%Gain_Min_PN
       write(*,*) 'Pixel Stat Max',Gain%Gain_Max_PN
       write(*,*) 'Pixel Stat Min',PosMin
       write(*,*) 'Pixel Stat Max',PosMax
!
!      decompose pixel map into sub pixels map
       C_SUB = int(Gain%NbCol_PN / Gain%NbCol_SUB_PN)
       L_SUB = int(Gain%NbLin_PN / Gain%NbLin_SUB_PN)
       do SUB_PN = 1, Gain%Nb_SUB_PN
          L = int((SUB_PN-1)/C_SUB) + 1
          C = SUB_PN - ( L-1)*C_SUB
          Gain%LinStart_SUB_PN(SUB_PN) = 1 + ( Gain%NbLin_SUB_PN * (L-1) )
          Gain%ColStart_SUB_PN(SUB_PN) = 1 + ( Gain%NbCol_SUB_PN * (C-1) )
          Gain%ColStart_SUB_PN(SUB_PN) = Gain%ColStart_SUB_PN(SUB_PN) &
                                       + Gain%ColStart_PN - 1
          Gain%LinStart_SUB_PN(SUB_PN) = Gain%LinStart_SUB_PN(SUB_PN) &
                                       + Gain%LinStart_PN - 1
          Gain%ColEnd_SUB_PN(SUB_PN)   = Gain%ColStart_SUB_PN(SUB_PN) &
                                       + Gain%NbCol_SUB_PN - 1
          Gain%LinEnd_SUB_PN(SUB_PN)   = Gain%LinStart_SUB_PN(SUB_PN) &
                                       + Gain%NbLin_SUB_PN - 1
          write(*,*) 'SUB_PN            ',SUB_PN
          write(*,*) 'Col Start End SUB_PN   ',Gain%ColStart_SUB_PN(SUB_PN),&
                                               Gain%ColEnd_SUB_PN(SUB_PN)
          write(*,*) 'Lin Start End SUB_PN   ',Gain%LinStart_SUB_PN(SUB_PN),&
                                               Gain%LinEnd_SUB_PN(SUB_PN)
!
          Gain%Gain_SUB_PN( 1:Gain%NbCol_SUB_PN            &
                           ,1:Gain%NbLin_SUB_PN,SUB_PN ) = &
                 Gain%PRNU( Gain%ColStart_SUB_PN(SUB_PN)   &
                           :Gain%ColEnd_SUB_PN(SUB_PN)     &
                           ,Gain%LinStart_SUB_PN(SUB_PN)   &
                           :Gain%LinEnd_SUB_PN(SUB_PN) )
!
!         comptute Pixel gain statistics
          Ns = 0
          do NL = 1, Gain%NbLin_SUB_PN
             do NC = 1, Gain%NbCol_SUB_PN
                Ns = Ns + 1
                X(Ns) = Gain%Gain_SUB_PN(NC,NL,SUB_PN)
             end do
          end do
          call statistics( Gain%NbCol_SUB_PN*Gain%NbLin_SUB_PN,&
                           X,                                  &
!
                           Gain%Gain_Avg_SUB_PN(SUB_PN),       &
                           Gain%Gain_Std_SUB_PN(SUB_PN),       &
                           Gain%Gain_Min_SUB_PN(SUB_PN),       &
                           Gain%Gain_Max_SUB_PN(SUB_PN),       &
                           PosMin,                             &
                           PosMax                              )
          write(*,*) 'SUB_Pixel Stat Avg',Gain%Gain_Avg_SUB_PN(SUB_PN)
          write(*,*) 'SUB_Pixel Stat Std',Gain%Gain_Std_SUB_PN(SUB_PN)
          write(*,*) 'SUB_Pixel Stat Min',Gain%Gain_Min_SUB_PN(SUB_PN)
          write(*,*) 'SUB_Pixel Stat Max',Gain%Gain_Max_SUB_PN(SUB_PN)
          write(*,*) 'SUB_Pixel Stat Min',PosMin
          write(*,*) 'SUB_Pixel Stat Max',PosMax
       end do
!
!      deallocation
       deallocate( X )
       deallocate( Pos )
       deallocate( Average_PN )
       deallocate( Variance_PN )
     end if
     return
 999 write(*,*) 'gain_init Fatal Error',iPos
     call exit(1)
   end subroutine gain_init
!
!
!>  interf_gainappli -- Public
!!
!! * Purpose
!!
!!     apply gain 
!!
!! * Description
!!
!!
!! * Inputs
!!
!!     - Gain   : type_Gain / type for declaration and allocation of gain
!!     - SUB_PN : sub pixel number
!!
!! * Inputs/outputs
!!
!!     - Interf : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Outputs
!!
!!
!! * References
!!
!!
   subroutine interf_gainappli( Gain, SUB_PN, Interf )
   implicit none
     type(type_Gain)         ,intent(in)                :: Gain
     integer(kind=LONG)      ,intent(in)                :: SUB_PN
     type(type_Interferogram),intent(inout)             :: Interf
! 
     if( Interf%Type == 'R' ) then
        Interf%Real_Part(1:Interf%N_Sample) =             &
                      Interf%Real_Part(1:Interf%N_Sample) &
                    * Gain%Gain_Avg_SUB_PN(SUB_PN)
     else if( Interf%Type == 'C' ) then
        Interf%Complex(1:Interf%N_Sample) =             &
                       Interf%Complex(1:Interf%N_Sample)&
                     * Gain%Gain_Avg_SUB_PN(SUB_PN)
     else
        write(*,*) 'Interferogram Type ERROR'
        go to 999
     end if
!
     return
 999 write(*,*) 'interf_gainappli Fatal Error'
     call exit(1)
   end subroutine interf_gainappli
!
!
!>  interf_gaincompens -- Public
!!
!! * Purpose
!!
!!     Gain compensation 
!!
!! * Description
!!
!!
!! * Inputs
!!
!!     - Gain   : type_Gain / type for declaration and allocation of gain
!!
!! * Inputs/outputs
!!
!!     - Interf : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Outputs
!!
!!
!! * References
!!
!!
   subroutine interf_gaincompens( Gain, Interf )
   implicit none
     type(type_Gain)         ,intent(in)                :: Gain
     type(type_Interferogram),intent(inout)             :: Interf
!
     if( Interf%Type == 'R' ) then
        Interf%Real_Part(1:Interf%N_Sample) =             &
                      Interf%Real_Part(1:Interf%N_Sample) &
                    / Gain%Gain_Avg_PN
     else if( Interf%Type == 'C' ) then
        Interf%Complex(1:Interf%N_Sample) =             &
                       Interf%Complex(1:Interf%N_Sample)&
                     / Gain%Gain_Avg_PN
     else
        write(*,*) 'Interferogram Type ERROR'
        go to 999
     end if
!
     return
 999 write(*,*) 'interf_gaincompens Fatal Error'
     call exit(1)
   end subroutine interf_gaincompens
!
!
end module gain_module
