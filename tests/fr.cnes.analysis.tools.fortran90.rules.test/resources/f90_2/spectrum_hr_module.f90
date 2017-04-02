!!#  spectrum_hr_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: january 2010
!!#            Version: $Revision: 1.9 $
!!#  Last modification: $Date: 2012-02-08 10:19:16 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> spectrum High Resolution -- Module
!!
!! * Purpose
!!
!!    Module for spectrum HR reading
!!
!! * Description
!!      
!!   This module allows the reading of atmospheric or blackbody spectrum and their allocation 
!!   to the dedicated type
!!
!! * Sub-routines and functions
!!
!! -  spectrum_hr_asc_at : reading of an atmospheric high resolution spectrum in ascii format
!! -  spectrum_hr_asc_bb : reading of a blackbody high resolution spectrum in ascii format
!! -  spectrum_hr_bin_at : reading of an atmospheric high resolution spectrum in binary format
!! -  spectrum_hr_bin_bb : reading of a blackbody high resolution spectrum in binary format
!!
!! * References
!!

module spectrum_hr_module
   use precision_type
   use error_type
   use constantes_type
   use spectrum_type
   use math_module
   use plancklib_module
!
   implicit none
!
!
   public :: spectrum_hr_asc_at, &
             spectrum_hr_asc_bb, &
             spectrum_hr_bin_at, &
             spectrum_hr_bin_bb
!
   contains
!
!

!> spectrum_hr_asc_at -- Public
!!
!! * Purpose
!!
!!     Reading of an atmospheric high resolution spectrum file in ascii format
!!
!! * Description
!! 
!!     This subroutine allows to read an atmospheric high resolution spectrum which is given
!!     in ascii format. Taking into account a convolution spectral margin in the determination 
!!     of the first and last wavenumber, the radiance spectrum is then allocated to the 
!!     dedicated type_spectrum.
!!
!! * Inputs
!!
!!     - WnS_in           : first wavenumber 
!!     - WnE_in           : last wavenumber 
!!     - Margin           : convolution spectral margin
!!     - File_Spectrum_hr : atmospheric high resolution spectrum ascii filename
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_hr : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!

   subroutine spectrum_hr_asc_at( WnS_in, WnE_in, Margin,       &
                                  File_Spectrum_hr, Spectrum_hr )
   implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS_in
     real(kind=DOUBLE)       ,intent(in)                 :: WnE_in
     real(kind=DOUBLE)       ,intent(in)                 :: Margin
     character(len=*)        ,intent(in)                 :: File_Spectrum_hr
     type(type_Spectrum)     ,intent(out)                :: Spectrum_hr
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     real(kind=DOUBLE)                                   :: WnS
     real(kind=DOUBLE)                                   :: WnE
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: n
     real(kind=DOUBLE)                                   :: Wn_L
     real(kind=DOUBLE)                                   :: Rad_L
     real(kind=DOUBLE)                                   :: TB_L
!
     iFile = 10
     iPos  = 1
     open(iFile, file=File_Spectrum_hr, err=999)
     iPos  = 2
     WnS = (WnS_in - Margin) / 100.d+00
     WnE = (WnE_in + Margin) / 100.d+00
     read(iFile,*,err=999) Spectrum_hr%N_Sample
     read(iFile,*,err=999) Spectrum_hr%dWn
     Spectrum_hr%Type = 'R'
     iPos  = 3
     write(*,*) 'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,*) 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
     write(*,*) 'Spectrum_hr%Type  : ',Spectrum_hr%Type
     write(*,*) 'WnS',WnS
     write(*,*) 'WnE',WnE
     call alloc_Spectrum( Spectrum_hr )
     Ns = 0
     do n = 1, Spectrum_hr%N_Sample
        read(iFile,*,err=999) Wn_L,Rad_L,TB_L
        if( (WnS <= Wn_L) .and. (Wn_L <= WnE) ) then
           Ns = Ns + 1
           Spectrum_hr%Wn(Ns)        = Wn_L  * 100.d+00
           Spectrum_hr%Real_Part(Ns) = Rad_L / 100.d+00
        end if
     end do
     Spectrum_hr%dWn = Spectrum_hr%dWn * 100.d+00
     Spectrum_hr%N_Sample = Ns
     Spectrum_hr%Wn_First = Spectrum_hr%Wn(1)
     Spectrum_hr%Wn_Last  = Spectrum_hr%Wn(Spectrum_hr%N_Sample)
     Spectrum_hr%Ns_First = idnint(Spectrum_hr%Wn_First/Spectrum_hr%dWn) +1
     Spectrum_hr%Ns_Last  = idnint(Spectrum_hr%Wn_Last/Spectrum_hr%dWn) +1
     Spectrum_hr%WnMax    = Spectrum_hr%Wn_Last * 1.2d+00
     write(*,*) 'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,*) 'Spectrum_hr%Wn_First',Spectrum_hr%Wn_First
     write(*,*) 'Spectrum_hr%Wn_Last ',Spectrum_hr%Wn_Last
     write(*,*) 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
     write(*,*) 'Spectrum_hr%WnMax   ',Spectrum_hr%WnMax
     close(iFile)
!
     if( (WnS_in > 0) .and. (WnS_in < Spectrum_hr%Wn_First) ) then
        write(*,*) ' WARNNING Not enough HR samples WnSart :',WnS_in
     end if
     if( (Spectrum_hr%Wn_Last < WnE_in) .and. (WnE_in < 1.d+20) ) then
        write(*,*) ' WARNNING Not enough HR samples WnEnd  :',WnE_in
     end if
!
     return
 999 write(*,*) 'Reading sp_hr Error',iPos
     close(iFile)
     call exit(1)
   end subroutine spectrum_hr_asc_at
!
!

!> spectrum_hr_asc_bb -- Public
!!
!! * Purpose
!!
!!     Reading of an blackbody high resolution spectrum file in ascii format
!!
!! * Description
!! 
!!     This subroutine allows to read a blackbody high resolution spectrum which is given in 
!!     ascii format. Taking into account a convolution spectral margin in the determination of 
!!     the first and last wavenumber (in m-1), the radiance spectrum (in watt/m**2/stradian/m-1) 
!!     is computed thanks to the plank law and is then allocated to the dedicated type_spectrum.
!!
!! * Inputs
!!
!!     - WnS_in           : first wavenumber 
!!     - WnE_in           : last wavenumber 
!!     - Margin           : convolution spectral margin
!!     - T                : blackbody temperature
!!     - File_Spectrum_hr : atmospheric high resolution spectrum ascii filename
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_hr : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!

  subroutine spectrum_hr_asc_bb( WnS_in, WnE_in, Margin, T,    &
                                 File_Spectrum_hr, Spectrum_hr )
   implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS_in
     real(kind=DOUBLE)       ,intent(in)                 :: WnE_in
     real(kind=DOUBLE)       ,intent(in)                 :: Margin
     real(kind=DOUBLE)       ,intent(in)                 :: T
     character(len=*)        ,intent(in)                 :: File_Spectrum_hr
     type(type_Spectrum)     ,intent(out)                :: Spectrum_hr
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     real(kind=DOUBLE)                                   :: WnS
     real(kind=DOUBLE)                                   :: WnE
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: n
     real(kind=DOUBLE)                                   :: Wn_L
     real(kind=DOUBLE)                                   :: Rad_L
     real(kind=DOUBLE)                                   :: TB_L
!
     iFile = 10
     iPos  = 1
     open(iFile, file=File_Spectrum_hr, err=999)
     iPos  = 2
     WnS = (WnS_in - Margin) / 100.d+00
     WnE = (WnE_in + Margin) / 100.d+00
     read(iFile,*,err=999) Spectrum_hr%N_Sample
     read(iFile,*,err=999) Spectrum_hr%dWn
     Spectrum_hr%Type = 'R'
     iPos  = 3
     write(*,*) 'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,*) 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
     write(*,*) 'Spectrum_hr%Type  : ',Spectrum_hr%Type
     write(*,*) 'WnS',WnS
     write(*,*) 'WnE',WnE
     call alloc_Spectrum( Spectrum_hr )
     Ns = 0
     do n = 1, Spectrum_hr%N_Sample
        read(iFile,*,err=999) Wn_L,Rad_L,TB_L
        if( (WnS <= Wn_L) .and. (Wn_L <= WnE) ) then
           Ns = Ns + 1
           Spectrum_hr%Wn(Ns)        = Wn_L  * 100.d+00
           call plkdirect( T, Spectrum_hr%Wn(Ns), Spectrum_hr%Real_Part(Ns) )
        end if
     end do
     Spectrum_hr%dWn = Spectrum_hr%dWn * 100.d+00
     Spectrum_hr%N_Sample = Ns
     Spectrum_hr%Wn_First = Spectrum_hr%Wn(1)
     Spectrum_hr%Wn_Last  = Spectrum_hr%Wn(Spectrum_hr%N_Sample)
     Spectrum_hr%Ns_First = idnint(Spectrum_hr%Wn_First/Spectrum_hr%dWn) +1
     Spectrum_hr%Ns_Last  = idnint(Spectrum_hr%Wn_Last/Spectrum_hr%dWn) +1
     Spectrum_hr%WnMax    = Spectrum_hr%Wn_Last * 1.2d+00
     write(*,*) 'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,*) 'Spectrum_hr%Wn_First',Spectrum_hr%Wn_First
     write(*,*) 'Spectrum_hr%Wn_Last ',Spectrum_hr%Wn_Last
     write(*,*) 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
     write(*,*) 'Spectrum_hr%WnMax   ',Spectrum_hr%WnMax
     close(iFile)
     if( WnS_in < Spectrum_hr%Wn_First ) then
        write(*,*) ' WARNNING Not enough HR samples WnSart :',WnS_in
     end if
     if( Spectrum_hr%Wn_Last < WnE_in ) then
        write(*,*) ' WARNNING Not enough HR samples WnEnd  :',WnE_in
     end if
!
     return
 999 write(*,*) 'Reading sp_hr Error',iPos
     close(iFile)
     call exit(1)
   end subroutine spectrum_hr_asc_bb
!
!

!> spectrum_hr_bin_at -- Public
!!
!! * Purpose
!!
!!     Reading of an atmospheric high resolution spectrum file in binary format
!!
!! * Description
!! 
!!     This subroutine allows to read an atmospheric high resolution spectrum which is given
!!     in binary format. Taking into account a convolution spectral margin in the determination 
!!     of the spectral band limits, the radiance spectrum is then allocated to the 
!!     dedicated type_spectrum.
!!
!! * Inputs
!!
!!     - WnS_in           : first wavenumber 
!!     - WnE_in           : last wavenumber 
!!     - Margin           : convolution spectral margin
!!     - File_Spectrum_hr : atmospheric high resolution spectrum binary filename
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_hr : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!

   subroutine spectrum_hr_bin_at( WnS_in, WnE_in, Margin,       &
                                  File_Spectrum_hr, Spectrum_hr )
   implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS_in
     real(kind=DOUBLE)       ,intent(in)                 :: WnE_in
     real(kind=DOUBLE)       ,intent(in)                 :: Margin
     character(len=*)        ,intent(in)                 :: File_Spectrum_hr
     type(type_Spectrum)     ,intent(inout)              :: Spectrum_hr
     type(type_Spectrum)                                 :: Spectrum_hr_in
     real(kind=DOUBLE)                                   :: WnS
     real(kind=DOUBLE)                                   :: WnE
     integer(kind=LONG)                                  :: NsDel
!
!    reading input spectrum
     Spectrum_hr_in%TopLevelSp = Spectrum_hr%TopLevelSp
     call read_spectrum_hr( File_Spectrum_hr, Spectrum_hr_in)
     if( (Spectrum_hr_in%Type /= 'R') .and. (Spectrum_hr_in%Type /= 'C') ) then
        write(*,*) 'Wrong spectrum type must be R or C : ',Spectrum_hr_in%Type
        write(*,*) 'spectrum_hr_bin_at Fatal Error'
        call exit(1)
     end if
!
!    spectral band limits
     WnS = (WnS_in - Margin)
     WnE = (WnE_in + Margin)
     write(*,*) 'Spectrum_hr_in%N_Sample',Spectrum_hr_in%N_Sample
     write(*,*) 'Spectrum_hr_in%dWn     ',Spectrum_hr_in%dWn
     write(*,*) 'Spectrum_hr_in%Type  : ',Spectrum_hr_in%Type
     write(*,*) 'WnS',WnS
     write(*,*) 'WnE',WnE
     Spectrum_hr%Type     = Spectrum_hr_in%Type
     Spectrum_hr%dWn      = Spectrum_hr_in%dWn
     Spectrum_hr%Ns_First = idnint( WnS / Spectrum_hr%dWn ) + 1
     Spectrum_hr%Ns_Last  = idnint( WnE / Spectrum_hr%dWn ) + 1
     Spectrum_hr%N_Sample = Spectrum_hr%Ns_Last - Spectrum_hr%Ns_First + 1
     Spectrum_hr%Wn_First = Spectrum_hr%dWn * dble( Spectrum_hr%Ns_First-1 )
     Spectrum_hr%Wn_Last  = Spectrum_hr%dWn * dble( Spectrum_hr%Ns_Last-1 )
     Spectrum_hr%WnMax    = Spectrum_hr_in%WnMax
!
     write(*,*) 'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,*) 'Spectrum_hr%Wn_First',Spectrum_hr%Wn_First
     write(*,*) 'Spectrum_hr%Wn_Last ',Spectrum_hr%Wn_Last
     write(*,*) 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
     write(*,*) 'Spectrum_hr%WnMax   ',Spectrum_hr%WnMax
!
!    coherence check
     if( Spectrum_hr%N_Sample > Spectrum_hr_in%N_Sample ) then
        write(*,*) 'Wrong sample number',Spectrum_hr_in%N_Sample,&
                                         Spectrum_hr%N_Sample
        write(*,*) 'spectrum_hr_bin_at Fatal Error'
        call exit(1)
     end if
     call alloc_Spectrum( Spectrum_hr )
     NsDel = Spectrum_hr%Ns_First - Spectrum_hr_in%Ns_First
     if( (NsDel < 0) .or. &
         (NsDel+Spectrum_hr%N_Sample > Spectrum_hr_in%N_Sample) ) then
        write(*,*) ' Wrong limits number',NsDel
        write(*,*) 'spectrum_hr_bin_at Fatal Error'
        call exit(1)
     end if
!
!    spectrum transfert
     Spectrum_hr%Wn(1:Spectrum_hr%N_Sample) = &
                 Spectrum_hr_in%Wn(NsDel+1:NsDel+Spectrum_hr%N_Sample)
     if( Spectrum_hr%Type == 'R' ) then
        Spectrum_hr%Real_Part(1:Spectrum_hr%N_Sample) = &
                 Spectrum_hr_in%Real_Part(NsDel+1:NsDel+Spectrum_hr%N_Sample)
     else if( Spectrum_hr%Type == 'C' ) then
        Spectrum_hr%Complex(1:Spectrum_hr%N_Sample) = &
                 Spectrum_hr_in%Complex(NsDel+1:NsDel+Spectrum_hr%N_Sample)
     else
        write(*,*) ' Wrong Spectrum_hr%Type must be R or C ',Spectrum_hr%Type
        go to 999
     end if
!
     if( (WnS_in > 0) .and. (WnS_in < Spectrum_hr%Wn_First) ) then
        write(*,*) ' WARNNING Not enough HR samples WnSart :',WnS_in
     end if
     if( (Spectrum_hr%Wn_Last < WnE_in) .and. (WnE_in < 1.d+20) ) then
        write(*,*) ' WARNNING Not enough HR samples WnEnd  :',WnE_in
     end if
!
     call dalloc_Spectrum( Spectrum_hr_in )
     return
 999 write(*,*) 'extraction sp_hr_bin at Fatal Error'
     call exit(1)
  end subroutine spectrum_hr_bin_at
!
!

!> spectrum_hr_bin_bb -- Public
!!
!! * Purpose
!!
!!     Reading of an blackbody high resolution spectrum file in binary format
!!
!! * Description
!! 
!!     This subroutine allows to read a blackbody high resolution spectrum which is given in 
!!     binary format. Taking into account a convolution spectral margin in the determination of 
!!     the first and last wavenumber (in m-1), the radiance spectrum (in watt/m**2/stradian/m-1) 
!!     is computed thanks to the plank law and is then allocated to the dedicated type_spectrum.
!!
!! * Inputs
!!
!!     - WnS_in           : first wavenumber 
!!     - WnE_in           : last wavenumber 
!!     - Margin           : convolution spectral margin
!!     - T                : blackbody temperature
!!     - File_Spectrum_hr : atmospheric high resolution spectrum binary filename
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_hr : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!

   subroutine spectrum_hr_bin_bb( WnS_in, WnE_in, Margin, T,    &
                                  File_Spectrum_hr, Spectrum_hr )
   implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS_in
     real(kind=DOUBLE)       ,intent(in)                 :: WnE_in
     real(kind=DOUBLE)       ,intent(in)                 :: Margin
     real(kind=DOUBLE)       ,intent(in)                 :: T
     character(len=*)        ,intent(in)                 :: File_Spectrum_hr
     type(type_Spectrum)     ,intent(inout)              :: Spectrum_hr
     type(type_Spectrum)                                 :: Spectrum_hr_in
     real(kind=DOUBLE)                                   :: WnS
     real(kind=DOUBLE)                                   :: WnE
     real(kind=DOUBLE)                                   :: Rad
     integer(kind=LONG)                                  :: NsDel
     integer(kind=LONG)                                  :: Ns
!
!    reading input spectrum
     Spectrum_hr_in%TopLevelSp = Spectrum_hr%TopLevelSp
     call read_spectrum_hr( File_Spectrum_hr, Spectrum_hr_in)
     if( (Spectrum_hr_in%Type /= 'R') .and. (Spectrum_hr_in%Type /= 'C') ) then
        write(*,*) 'Wrong spectrum type must be R or C',Spectrum_hr_in%Type
        write(*,*) 'spectrum_hr_bin_at Fatal Error'
        call exit(1)
     end if
!
!    spectral band limits
     WnS = (WnS_in - Margin)
     WnE = (WnE_in + Margin)
     write(*,*) 'Spectrum_hr_in%N_Sample',Spectrum_hr_in%N_Sample
     write(*,*) 'Spectrum_hr_in%dWn     ',Spectrum_hr_in%dWn
     write(*,*) 'Spectrum_hr_in%Type  : ',Spectrum_hr_in%Type
     write(*,*) 'WnS',WnS
     write(*,*) 'WnE',WnE
     Spectrum_hr%Type     = Spectrum_hr_in%Type
     Spectrum_hr%dWn      = Spectrum_hr_in%dWn
     Spectrum_hr%Ns_First = idnint( WnS / Spectrum_hr%dWn ) + 1
     Spectrum_hr%Ns_Last  = idnint( WnE / Spectrum_hr%dWn ) + 1
     Spectrum_hr%N_Sample = Spectrum_hr%Ns_Last - Spectrum_hr%Ns_First + 1
     Spectrum_hr%Wn_First = Spectrum_hr%dWn * dble( Spectrum_hr%Ns_First-1 )
     Spectrum_hr%Wn_Last  = Spectrum_hr%dWn * dble( Spectrum_hr%Ns_Last-1 )
     Spectrum_hr%WnMax    = Spectrum_hr_in%WnMax
!
!    coherence check
     if( Spectrum_hr%N_Sample > Spectrum_hr_in%N_Sample ) then
        write(*,*) 'Wrong sample number',Spectrum_hr_in%N_Sample,&
                                         Spectrum_hr%N_Sample
        write(*,*) 'spectrum_hr_bin_bb Fatal Error'
        call exit(1)
     end if
     call alloc_Spectrum( Spectrum_hr )
     NsDel = Spectrum_hr%Ns_First - Spectrum_hr_in%Ns_First
     if( (NsDel < 0) .or. &
         (NsDel+Spectrum_hr%N_Sample > Spectrum_hr_in%N_Sample) ) then
        write(*,*) ' Wrong limits number',NsDel
        write(*,*) 'spectrum_hr_bin_bb Fatal Error'
        call exit(1)
     end if
!
!    spectrum transfert
     Spectrum_hr%Wn(1:Spectrum_hr%N_Sample) = &
                 Spectrum_hr_in%Wn(NsDel+1:NsDel+Spectrum_hr%N_Sample)
     if( T /= 0.0d+00 ) then
        if( Spectrum_hr%Type == 'R' ) then
           do Ns = 1, Spectrum_hr%N_Sample
              call plkdirect( T, Spectrum_hr%Wn(Ns), Rad )
              Spectrum_hr%Real_Part(Ns) = Rad
           end do
        else if( Spectrum_hr%Type == 'C' ) then
           do Ns = 1, Spectrum_hr%N_Sample
              call plkdirect( T, Spectrum_hr%Wn(Ns), Rad )
              Spectrum_hr%Complex(Ns) = dcmplx( Rad, 0_DOUBLE )
           end do
        else
           write(*,*) ' Wrong Spectrum_hr%Type ',Spectrum_hr%Type
           go to 999
        end if
     else
        if( Spectrum_hr%Type == 'R' ) then
           Spectrum_hr%Real_Part(1:Spectrum_hr%N_Sample) = 0_DOUBLE
        else if( Spectrum_hr%Type == 'C' ) then
           Spectrum_hr%Complex(1:Spectrum_hr%N_Sample) = &
                                         dcmplx( 0_DOUBLE, 0_DOUBLE )
        else
           write(*,*) ' Wrong Spectrum_hr%Type ',Spectrum_hr%Type
           go to 999
        end if
     end if
     write(*,*) 'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,*) 'Spectrum_hr%Wn_First',Spectrum_hr%Wn_First
     write(*,*) 'Spectrum_hr%Wn_Last ',Spectrum_hr%Wn_Last
     write(*,*) 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
     write(*,*) 'Spectrum_hr%WnMax   ',Spectrum_hr%WnMax
!
     if( (WnS_in > 0) .and. (WnS_in < Spectrum_hr%Wn_First) ) then
        write(*,*) ' WARNNING Not enough HR samples WnSart :',WnS_in
     end if
     if( (Spectrum_hr%Wn_Last < WnE_in) .and. (WnE_in < 1.d+20) ) then
        write(*,*) ' WARNNING Not enough HR samples WnEnd  :',WnE_in
     end if
!
     call dalloc_Spectrum( Spectrum_hr_in )
     return
 999 write(*,*) 'extraction sp_hr_bin bb Fatal Error'
     call exit(1)
  end subroutine spectrum_hr_bin_bb
!
!
   subroutine spectrum_hr_bin_lz( WnS_in, WnE_in, Margin,       &
                                  WnLaser_EW, Laser_Intensity,  &
                                  File_Spectrum_hr, Spectrum_hr )
   implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS_in
     real(kind=DOUBLE)       ,intent(in)                 :: WnE_in
     real(kind=DOUBLE)       ,intent(in)                 :: Margin
     real(kind=DOUBLE)       ,intent(in)                 :: WnLaser_EW
     real(kind=DOUBLE)       ,intent(in)                 :: Laser_Intensity
     character(len=*)        ,intent(in)                 :: File_Spectrum_hr
     type(type_Spectrum)     ,intent(inout)              :: Spectrum_hr
     type(type_Spectrum)                                 :: Spectrum_hr_in
     real(kind=DOUBLE)                                   :: WnS
     real(kind=DOUBLE)                                   :: WnE
     integer(kind=LONG)                                  :: NsDel
     integer(kind=LONG)                                  :: Ns
!
!    reading input spectrum
     Spectrum_hr_in%TopLevelSp = Spectrum_hr%TopLevelSp
     call read_spectrum_hr( File_Spectrum_hr, Spectrum_hr_in)
     if( (Spectrum_hr_in%Type /= 'R') .and. (Spectrum_hr_in%Type /= 'C') ) then
        write(*,*) 'Wrong spectrum type must be R or C',Spectrum_hr_in%Type
        write(*,*) 'spectrum_hr_bin_lz Fatal Error'
        call exit(1)
     end if
!
!    spectral band limits
     WnS = (WnS_in - Margin)
     WnE = (WnE_in + Margin)
     write(*,*) 'Spectrum_hr_in%N_Sample',Spectrum_hr_in%N_Sample
     write(*,*) 'Spectrum_hr_in%dWn     ',Spectrum_hr_in%dWn
     write(*,*) 'Spectrum_hr_in%Type  : ',Spectrum_hr_in%Type
     write(*,*) 'WnS',WnS
     write(*,*) 'WnE',WnE
     Spectrum_hr%Type     = Spectrum_hr_in%Type
     Spectrum_hr%dWn      = Spectrum_hr_in%dWn
     Spectrum_hr%Ns_First = idnint( WnS / Spectrum_hr%dWn ) + 1
     Spectrum_hr%Ns_Last  = idnint( WnE / Spectrum_hr%dWn ) + 1
     Spectrum_hr%N_Sample = Spectrum_hr%Ns_Last - Spectrum_hr%Ns_First + 1
     Spectrum_hr%Wn_First = Spectrum_hr%dWn * dble( Spectrum_hr%Ns_First-1 )
     Spectrum_hr%Wn_Last  = Spectrum_hr%dWn * dble( Spectrum_hr%Ns_Last-1 )
     Spectrum_hr%WnMax    = Spectrum_hr_in%WnMax
!
!    coherence check
     if( Spectrum_hr%N_Sample > Spectrum_hr_in%N_Sample ) then
        write(*,*) 'Wrong sample number',Spectrum_hr_in%N_Sample,&
                                         Spectrum_hr%N_Sample
        write(*,*) 'spectrum_hr_bin_lz Fatal Error'
        call exit(1)
     end if
     call alloc_Spectrum( Spectrum_hr )
     NsDel = Spectrum_hr%Ns_First - Spectrum_hr_in%Ns_First
     if( (NsDel < 0) .or. &
         (NsDel+Spectrum_hr%N_Sample > Spectrum_hr_in%N_Sample) ) then
        write(*,*) ' Wrong limits number',NsDel
        write(*,*) 'spectrum_hr_bin_lz Fatal Error'
        call exit(1)
     end if
!
!    spectrum transfert
     Spectrum_hr%Wn(1:Spectrum_hr%N_Sample) = &
                 Spectrum_hr_in%Wn(NsDel+1:NsDel+Spectrum_hr%N_Sample)
!
!    Laser line position
     Ns = idnint( (WnLaser_EW-Spectrum_hr%Wn_First)/Spectrum_hr%dWn ) + 1
     if( Spectrum_hr%Type == 'R' ) then
        Spectrum_hr%Real_Part(1:Spectrum_hr%N_Sample) = 0_DOUBLE
        Spectrum_hr%Real_Part(Ns) = Laser_Intensity
     else if( Spectrum_hr%Type == 'C' ) then
        Spectrum_hr%Complex(1:Spectrum_hr%N_Sample) = &
                                    dcmplx( 0_DOUBLE, 0_DOUBLE )
         Spectrum_hr%Complex(Ns) = &
                dcmplx( Laser_Intensity , 0_DOUBLE )
    else
        write(*,*) ' Wrong Spectrum_hr%Type ',Spectrum_hr%Type
        go to 999
     end if
     write(*,*) 'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,*) 'Spectrum_hr%Wn_First',Spectrum_hr%Wn_First
     write(*,*) 'Spectrum_hr%Wn_Last ',Spectrum_hr%Wn_Last
     write(*,*) 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
     write(*,*) 'Spectrum_hr%WnMax   ',Spectrum_hr%WnMax
!
     if( (WnS_in > 0) .and. (WnS_in < Spectrum_hr%Wn_First) ) then
        write(*,*) ' WARNNING Not enough HR samples WnSart :',WnS_in
     end if
     if( (Spectrum_hr%Wn_Last < WnE_in) .and. (WnE_in < 1.d+20) ) then
        write(*,*) ' WARNNING Not enough HR samples WnEnd  :',WnE_in
     end if
!
     call dalloc_Spectrum( Spectrum_hr_in )
     return
 999 write(*,*) 'extraction sp_hr_bin lz Fatal Error'
     call exit(1)
  end subroutine spectrum_hr_bin_lz
!
!
   subroutine spectrum_hr_bin_ft( WnS_in, WnE_in, Margin,       &
                                  Flat_Intensity,               &
                                  File_Spectrum_hr, Spectrum_hr )
   implicit none
     real(kind=DOUBLE)       ,intent(in)                 :: WnS_in
     real(kind=DOUBLE)       ,intent(in)                 :: WnE_in
     real(kind=DOUBLE)       ,intent(in)                 :: Margin
     real(kind=DOUBLE)       ,intent(in)                 :: Flat_Intensity
     character(len=*)        ,intent(in)                 :: File_Spectrum_hr
     type(type_Spectrum)     ,intent(inout)              :: Spectrum_hr
     type(type_Spectrum)                                 :: Spectrum_hr_in
     real(kind=DOUBLE)                                   :: WnS
     real(kind=DOUBLE)                                   :: WnE
     integer(kind=LONG)                                  :: NsDel
!
!    reading input spectrum
     Spectrum_hr_in%TopLevelSp = Spectrum_hr%TopLevelSp
     call read_spectrum_hr( File_Spectrum_hr, Spectrum_hr_in)
     if( (Spectrum_hr_in%Type /= 'R') .and. (Spectrum_hr_in%Type /= 'C') ) then
        write(*,*) 'Wrong spectrum type must be R or C',Spectrum_hr_in%Type
        write(*,*) 'spectrum_hr_bin_ft Fatal Error'
        call exit(1)
     end if
!
!    spectral band limits
     WnS = (WnS_in - Margin)
     WnE = (WnE_in + Margin)
     write(*,*) 'Spectrum_hr_in%N_Sample',Spectrum_hr_in%N_Sample
     write(*,*) 'Spectrum_hr_in%dWn     ',Spectrum_hr_in%dWn
     write(*,*) 'Spectrum_hr_in%Type  : ',Spectrum_hr_in%Type
     write(*,*) 'WnS',WnS
     write(*,*) 'WnE',WnE
     Spectrum_hr%Type     = Spectrum_hr_in%Type
     Spectrum_hr%dWn      = Spectrum_hr_in%dWn
     Spectrum_hr%Ns_First = idnint( WnS / Spectrum_hr%dWn ) + 1
     Spectrum_hr%Ns_Last  = idnint( WnE / Spectrum_hr%dWn ) + 1
     Spectrum_hr%N_Sample = Spectrum_hr%Ns_Last - Spectrum_hr%Ns_First + 1
     Spectrum_hr%Wn_First = Spectrum_hr%dWn * dble( Spectrum_hr%Ns_First-1 )
     Spectrum_hr%Wn_Last  = Spectrum_hr%dWn * dble( Spectrum_hr%Ns_Last-1 )
     Spectrum_hr%WnMax    = Spectrum_hr_in%WnMax
!
!    coherence check
     if( Spectrum_hr%N_Sample > Spectrum_hr_in%N_Sample ) then
        write(*,*) 'Wrong sample number',Spectrum_hr_in%N_Sample,&
                                         Spectrum_hr%N_Sample
        write(*,*) 'spectrum_hr_bin_ft Fatal Error'
        call exit(1)
     end if
     call alloc_Spectrum( Spectrum_hr )
     NsDel = Spectrum_hr%Ns_First - Spectrum_hr_in%Ns_First
     if( (NsDel < 0) .or. &
         (NsDel+Spectrum_hr%N_Sample > Spectrum_hr_in%N_Sample) ) then
        write(*,*) ' Wrong limits number',NsDel
        write(*,*) 'spectrum_hr_bin_ft Fatal Error'
        call exit(1)
     end if
!
!    spectrum basis transfert
     Spectrum_hr%Wn(1:Spectrum_hr%N_Sample) = &
                 Spectrum_hr_in%Wn(NsDel+1:NsDel+Spectrum_hr%N_Sample)
!
!    Flat spectrum computation
     if( Spectrum_hr%Type == 'R' ) then
        Spectrum_hr%Real_Part(1:Spectrum_hr%N_Sample) = Flat_Intensity
!
     else if( Spectrum_hr%Type == 'C' ) then
        Spectrum_hr%Complex(1:Spectrum_hr%N_Sample) = &
                                    dcmplx( Flat_Intensity, 0_DOUBLE )
!
    else
        write(*,*) ' Wrong Spectrum_hr%Type ',Spectrum_hr%Type
        go to 999
     end if
     write(*,*) 'Spectrum_hr%N_Sample',Spectrum_hr%N_Sample
     write(*,*) 'Spectrum_hr%Wn_First',Spectrum_hr%Wn_First
     write(*,*) 'Spectrum_hr%Wn_Last ',Spectrum_hr%Wn_Last
     write(*,*) 'Spectrum_hr%dWn     ',Spectrum_hr%dWn
     write(*,*) 'Spectrum_hr%WnMax   ',Spectrum_hr%WnMax
!
     if( (WnS_in > 0) .and. (WnS_in < Spectrum_hr%Wn_First) ) then
        write(*,*) ' WARNNING Not enough HR samples WnSart :',WnS_in
     end if
     if( (Spectrum_hr%Wn_Last < WnE_in) .and. (WnE_in < 1.d+20) ) then
        write(*,*) ' WARNNING Not enough HR samples WnEnd  :',WnE_in
     end if
!
     call dalloc_Spectrum( Spectrum_hr_in )
     return
 999 write(*,*) 'extraction sp_hr_bin ft Fatal Error'
     call exit(1)
  end subroutine spectrum_hr_bin_ft
!
!
subroutine read_spectrum_hr( File_Spectrum_hr, Spectrum_hr )
     implicit none
     character(len=*)        ,intent(in)    :: File_Spectrum_hr
     type(type_Spectrum)     ,intent(inout) :: Spectrum_hr
     integer(kind=LONG)                                  :: ErrCode
!
!    reading input spectrum
     if ( File_Spectrum_hr(len_trim(File_Spectrum_hr)-2:len_trim(File_Spectrum_hr)) == ".nc" ) then
       Spectrum_hr%TopLevelSp%header%filename = File_Spectrum_hr(1:len_trim(File_Spectrum_hr))
       call readspectrum_netcdf( Spectrum_hr, ErrCode )
       if( ErrCode /= 0 ) then
         write(*,'(a,a)') 'readspectrum Error : ',&
            trim(Spectrum_hr%TopLevelSp%header%filename)
         go to 999
       end if
     else
        Spectrum_hr%filename = File_Spectrum_hr(1:len_trim(File_Spectrum_hr))
        call readspectrum( Spectrum_hr, ErrCode )
       if( ErrCode /= 0 ) then
         write(*,'(a,a)') 'readspectrum Error : ',&
            trim(Spectrum_hr%filename)
         go to 999
       end if
     end if

     return
 999 write(*,*) ' read_spectrum_hr Fatal Error'
     call exit(1)
end subroutine read_spectrum_hr
!
end module spectrum_hr_module
