!!# spectral_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009
!!#           Version: $Revision: 1.11 $
!!# Last modification: $Date: 2011-11-02 14:57:49 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> spectral_module -- Module
!!
!! * Purpose
!!
!!     Module for spectral performance computations
!!
!! * Description
!!      
!!     This module allows to compute spectral performances: it computes spectral errors 
!!     between a nominal case and a perturbed one, and spectral performances are given 
!!     in term of spectral shift error, spectral resolution error, shape error or spectrum 
!!     noise. The spectral analysis can be performed at four levels (1a, 1b, 1c and 1d).
!!
!! * Sub-routines and functions
!!
!!     - anal_ils_spectral  : ILS spectral performance analyse.
!!     - perfo_ils_spectral : Computation of ILS spectral performances 
!!     - anal_srf_spectral  : SRF spectral performance analyse
!!     - shapeindex         : Shape index computation 
!!
!! * References
!!

module spectral_module
   use precision_type
   use error_type
   use constantes_type
   use srf_param_type

   use spectrum_type
   use ils_perfo_param_type
   use ils_perfo_type
   use ils_perfo_stats_type
   use srf_type
   use math_module
   use statistique_module
   use plancklib_module
   use srf_computation_module
   use fft_module
!
   implicit none
!
!
   public :: ils_spectral_stats,      &
             perfo_ils_init,          &
             ils_perfo_param_transfer,&
             anal_ils_spectral,       &
             perfo_ils_spectral,      &
             anal_srf_spectral,       &
             shapeindex           
!
!
   contains
!
!
  subroutine ils_spectral_stats( Nb_Spectrum,    &
                                 Spectrum,       &
                                 Nb_Spectrum_ref,&
                                 Spectrum_ref,   &
                                 Ils_Perfo_Param,&
                                 Ils_Perfo_Stats )
  implicit none
   integer(kind=LONG)  ,intent(in)                              :: Nb_Spectrum
   integer(kind=LONG)  ,intent(in)                              :: Nb_Spectrum_ref
   type(type_Spectrum) ,intent(inout),dimension(Nb_Spectrum)    :: Spectrum
   type(type_Spectrum) ,intent(inout),dimension(Nb_Spectrum_ref):: Spectrum_ref
   type(type_Ils_Perfo_Param),intent(in)                        :: Ils_Perfo_Param
   type(type_Ils_Perfo_Stats)                                   :: Ils_Perfo_Stats
   integer(kind=LONG)                                           :: iPos
   integer(kind=LONG)                                           :: Ns
   integer(kind=LONG)                                           :: Nb
   type(type_Ils_Perfo),dimension(:),allocatable                :: Ils_Perfo
   type(type_Ils_Perfo)                                         :: Ils_Perfo_ref
   real(kind=DOUBLE),dimension(:),allocatable                   :: Ils
   real(kind=DOUBLE),dimension(:),allocatable                   :: Ils_ref
   real(kind=DOUBLE),dimension(:),allocatable                   :: X
!
   iPos = 1
   allocate( Ils_Perfo(Nb_Spectrum) )
!
   if( Nb_Spectrum_ref == 1 ) then
      call ils_perfo_param_transfer( Ils_Perfo_Param, Ils_Perfo_ref )
      call perfo_ils_spectral( Spectrum_ref(1),&
                               Ils_Perfo_ref   )
   else
      if( Nb_Spectrum_ref /= Nb_Spectrum ) then
         write(*,*) ' Nb_Spectrum_ref must be equal to Nb_Spectrum'
         go to 999
      end if
   end if
   do Nb = 1, Nb_Spectrum
      if( Nb_Spectrum_ref /= 1 ) then
         call ils_perfo_param_transfer( Ils_Perfo_Param, Ils_Perfo_ref )
         call perfo_ils_spectral( Spectrum_ref(Nb),&
                                  Ils_Perfo_ref    )
      end if
      call ils_perfo_param_transfer( Ils_Perfo_Param, Ils_Perfo(Nb) )
      call perfo_ils_spectral( Spectrum(Nb), &
                               Ils_Perfo(Nb) )
!
!     Shift Error
      Ils_Perfo(Nb)%ErShift = ( Ils_Perfo(Nb)%Shift-Ils_Perfo_ref%Shift ) &
                            / Ils_Perfo(Nb)%WnIls
!
!     Resolution Error
      Ils_Perfo(Nb)%ErFWhm = ( Ils_Perfo(Nb)%FWhm-Ils_Perfo_ref%FWhm ) &
                           / Ils_Perfo_ref%FWhm
!
!     Shape Error Index
      allocate( Ils_ref(Ils_Perfo(Nb)%Ns_Ils) )
      allocate( Ils(Ils_Perfo(Nb)%Ns_Ils) )
      Ils_ref(1:Ils_Perfo(Nb)%Ns_Ils) = &
                            dreal( Ils_Perfo_ref%Ils(1:Ils_Perfo(Nb)%Ns_Ils) )
      Ils(1:Ils_Perfo(Nb)%Ns_Ils)     = &
                            dreal( Ils_Perfo(Nb)%Ils(1:Ils_Perfo(Nb)%Ns_Ils) )
      Ils_Perfo(Nb)%ShapeDiff_R(1:Ils_Perfo(Nb)%Ns_Ils) = &
              ( Ils(1:Ils_Perfo(Nb)%Ns_Ils) - Ils_ref(1:Ils_Perfo(Nb)%Ns_Ils) ) &
              * Ils_Perfo(Nb)%dWn
!
      call shapeindex( Ils_Perfo(Nb)%Ns_Ils, Ils_ref, Ils,     &
                       Ils_Perfo(Nb)%dWn, Ils_Perfo(Nb)%Shape_R )
!
      Ils_ref(1:Ils_Perfo(Nb)%Ns_Ils) = &
                            dimag( Ils_Perfo_ref%Ils(1:Ils_Perfo(Nb)%Ns_Ils) )
      Ils(1:Ils_Perfo(Nb)%Ns_Ils)     = &
                            dimag( Ils_Perfo(Nb)%Ils(1:Ils_Perfo(Nb)%Ns_Ils) )
      Ils_Perfo(Nb)%ShapeDiff_I(1:Ils_Perfo(Nb)%Ns_Ils) = &
              ( Ils(1:Ils_Perfo(Nb)%Ns_Ils) - Ils_ref(1:Ils_Perfo(Nb)%Ns_Ils) ) &
              * Ils_Perfo(Nb)%dWn
!
      call shapeindex( Ils_Perfo(Nb)%Ns_Ils, Ils_ref, Ils,      &
                       Ils_Perfo(Nb)%dWn, Ils_Perfo(Nb)%Shape_I )
      deallocate( Ils_ref )
      deallocate( Ils )
      call dalloc_Spectrum( Spectrum(Nb) )
      if( Nb_Spectrum_ref /= 1 ) then
         call dalloc_Spectrum( Spectrum_ref(Nb) )
      end if
   end do
   if( Nb_Spectrum_ref == 1 ) then
      call dalloc_Spectrum( Spectrum_ref(1) )
   end if
!
!  compute ILS statistics
   allocate( X(Nb_Spectrum) )
!
   Ils_Perfo_Stats%WnIls     = Ils_Perfo_ref%WnIls
   Ils_Perfo_Stats%Ip        = Ils_Perfo_ref%Ip
   Ils_Perfo_Stats%Iteration = Ils_Perfo_ref%Iteration
   Ils_Perfo_Stats%OSFactor  = Ils_Perfo_ref%OSFactor
   Ils_Perfo_Stats%Ns_Ils    = Ils_Perfo_ref%Ns_Ils
   Ils_Perfo_Stats%SDWn      = Ils_Perfo_ref%SDWn
   Ils_Perfo_Stats%dWn       = Ils_Perfo_ref%dWn
   call alloc_Ils_Perfo_Stats( Ils_Perfo_Stats )
   Ils_Perfo_Stats%Wn(1:Ils_Perfo_Stats%Ns_Ils) = &
                        Ils_Perfo_ref%Wn(1:Ils_Perfo_ref%Ns_Ils)
   call dalloc_Ils_Perfo( Ils_Perfo_ref )
!
!  Spectral Shift
   do Nb = 1, Nb_Spectrum
      X(Nb) = Ils_Perfo(Nb)%Shift
   end do
   call statistics( Nb_Spectrum,                &
                    X,                          &
!
                    Ils_Perfo_Stats%Shift_Avg,  &
                    Ils_Perfo_Stats%Shift_Std,  &
                    Ils_Perfo_Stats%Shift_Min,  &
                    Ils_Perfo_Stats%Shift_Max,  &
                    Ils_Perfo_Stats%Shift_NsMin,&
                    Ils_Perfo_Stats%Shift_NsMax )
!
!  Spectral Shift Error
   do Nb = 1, Nb_Spectrum
      X(Nb) = Ils_Perfo(Nb)%ErShift
   end do
   call statistics( Nb_Spectrum,                  &
                    X,                            &
!
                    Ils_Perfo_Stats%ErShift_Avg,  &
                    Ils_Perfo_Stats%ErShift_Std,  &
                    Ils_Perfo_Stats%ErShift_Min,  &
                    Ils_Perfo_Stats%ErShift_Max,  &
                    Ils_Perfo_Stats%ErShift_NsMin,&
                    Ils_Perfo_Stats%ErShift_NsMax )
!
!  Spectral Resolution
   do Nb = 1, Nb_Spectrum
      X(Nb) = Ils_Perfo(Nb)%FWhm
   end do
   call statistics( Nb_Spectrum,               &
                    X,                         &
!
                    Ils_Perfo_Stats%FWhm_Avg,  &
                    Ils_Perfo_Stats%FWhm_Std,  &
                    Ils_Perfo_Stats%FWhm_Min,  &
                    Ils_Perfo_Stats%FWhm_Max,  &
                    Ils_Perfo_Stats%FWhm_NsMin,&
                    Ils_Perfo_Stats%FWhm_NsMax )
!
!  Spectral Resolution Error
   do Nb = 1, Nb_Spectrum
      X(Nb) = Ils_Perfo(Nb)%ErFWhm
   end do
   call statistics( Nb_Spectrum,                 &
                    X,                           &
!
                    Ils_Perfo_Stats%ErFWhm_Avg,  &
                    Ils_Perfo_Stats%ErFWhm_Std,  &
                    Ils_Perfo_Stats%ErFWhm_Min,  &
                    Ils_Perfo_Stats%ErFWhm_Max,  &
                    Ils_Perfo_Stats%ErFWhm_NsMin,&
                    Ils_Perfo_Stats%ErFWhm_NsMax )
!
!  Shape Index Error Real Part
   do Nb = 1, Nb_Spectrum
      X(Nb) = Ils_Perfo(Nb)%Shape_R
   end do
   call statistics( Nb_Spectrum,                        &
                    X,                                  &
!
                    Ils_Perfo_Stats%Shape_Index_R_Avg,  &
                    Ils_Perfo_Stats%Shape_Index_R_Std,  &
                    Ils_Perfo_Stats%Shape_Index_R_Min,  &
                    Ils_Perfo_Stats%Shape_Index_R_Max,  &
                    Ils_Perfo_Stats%Shape_Index_R_NsMin,&
                    Ils_Perfo_Stats%Shape_Index_R_NsMax )
!
!  Shape Index Error Imaginary Part
   do Nb = 1, Nb_Spectrum
      X(Nb) = Ils_Perfo(Nb)%Shape_I
   end do
   call statistics( Nb_Spectrum,                        &
                    X,                                  &
!
                    Ils_Perfo_Stats%Shape_Index_I_Avg,  &
                    Ils_Perfo_Stats%Shape_Index_I_Std,  &
                    Ils_Perfo_Stats%Shape_Index_I_Min,  &
                    Ils_Perfo_Stats%Shape_Index_I_Max,  &
                    Ils_Perfo_Stats%Shape_Index_I_NsMin,&
                    Ils_Perfo_Stats%Shape_Index_I_NsMax )
!
!  loop on the spectral dimension
   do Ns = 1, Ils_Perfo_ref%Ns_Ils
!
!     Ils Shape Real Part
      do Nb = 1, Nb_Spectrum
         X(Nb) = dreal(Ils_Perfo(Nb)%Ils(Ns))
      end do
      call statistics( Nb_Spectrum,                      &
                       X,                                &
!
                       Ils_Perfo_Stats%Shape_R_Avg(Ns),  &
                       Ils_Perfo_Stats%Shape_R_Std(Ns),  &
                       Ils_Perfo_Stats%Shape_R_Min(Ns),  &
                       Ils_Perfo_Stats%Shape_R_Max(Ns),  &
                       Ils_Perfo_Stats%Shape_R_NsMin(Ns),&
                       Ils_Perfo_Stats%Shape_R_NsMax(Ns) )
!
!     Ils Shape Imaginary Part
      do Nb = 1, Nb_Spectrum
         X(Nb) = dimag(Ils_Perfo(Nb)%Ils(Ns))
      end do
      call statistics( Nb_Spectrum,                      &
                       X,                                &
!
                       Ils_Perfo_Stats%Shape_I_Avg(Ns),  &
                       Ils_Perfo_Stats%Shape_I_Std(Ns),  &
                       Ils_Perfo_Stats%Shape_I_Min(Ns),  &
                       Ils_Perfo_Stats%Shape_I_Max(Ns),  &
                       Ils_Perfo_Stats%Shape_I_NsMin(Ns),&
                       Ils_Perfo_Stats%Shape_I_NsMax(Ns) )
!
!     Ils Shape Error Real Part
      do Nb = 1, Nb_Spectrum
         X(Nb) = Ils_Perfo(Nb)%ShapeDiff_R(Ns)
      end do
      call statistics( Nb_Spectrum,                          &
                       X,                                    &
!
                       Ils_Perfo_Stats%ShapeDiff_R_Avg(Ns),  &
                       Ils_Perfo_Stats%ShapeDiff_R_Std(Ns),  &
                       Ils_Perfo_Stats%ShapeDiff_R_Min(Ns),  &
                       Ils_Perfo_Stats%ShapeDiff_R_Max(Ns),  &
                       Ils_Perfo_Stats%ShapeDiff_R_NsMin(Ns),&
                       Ils_Perfo_Stats%ShapeDiff_R_NsMax(Ns) )
!
!     Ils Shape Error Imaginary Part
      do Nb = 1, Nb_Spectrum
         X(Nb) = Ils_Perfo(Nb)%ShapeDiff_I(Ns)
      end do
      call statistics( Nb_Spectrum,                          &
                       X,                                    &
!
                       Ils_Perfo_Stats%ShapeDiff_I_Avg(Ns),  &
                       Ils_Perfo_Stats%ShapeDiff_I_Std(Ns),  &
                       Ils_Perfo_Stats%ShapeDiff_I_Min(Ns),  &
                       Ils_Perfo_Stats%ShapeDiff_I_Max(Ns),  &
                       Ils_Perfo_Stats%ShapeDiff_I_NsMin(Ns),&
                       Ils_Perfo_Stats%ShapeDiff_I_NsMax(Ns) )
   end do
!
   do Nb = 1, Nb_Spectrum
      call dalloc_Ils_Perfo( Ils_Perfo(Nb) )
   end do
!
   return
999  write(*,*) 'ils_spectral_statsError',iPos
     call exit(1)
  end subroutine ils_spectral_stats
!
!
  subroutine perfo_ils_init( File_S_Perfo, Ils_Perfo_Param )
  implicit none
     character(len=*)          ,intent(in)               :: File_S_Perfo
     type(type_Ils_Perfo_Param),intent(inout)            :: Ils_Perfo_Param
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
!
     iFile = 10
     iPos = 1
     open(iFile, file=File_S_Perfo, status='old', err=999)
     write(*,'(a,a)') 'File_S_Perfo ',File_S_Perfo(1:len_trim(File_S_Perfo))
     iPos = 2
     read(iFile,*,err=999) Ils_Perfo_Param%WnIls
     write(*,*) 'Ils_Perfo_Param%WnIls     ',Ils_Perfo_Param%WnIls
     read(iFile,*,err=999) Ils_Perfo_Param%Ip
     write(*,*) 'Ils_Perfo_Param%Ip        ',Ils_Perfo_Param%Ip
     read(iFile,*,err=999) Ils_Perfo_Param%Iteration
     write(*,*) 'Ils_Perfo_Param%Iteration ',Ils_Perfo_Param%Iteration
     read(iFile,*,err=999) Ils_Perfo_Param%OSFactor
     write(*,*) 'Ils_Perfo_Param%OSFactor  ',Ils_Perfo_Param%OSFactor
     read(iFile,*,err=999) Ils_Perfo_Param%SDWn
     write(*,*) 'Ils_Perfo_Param%SDWn      ',Ils_Perfo_Param%SDWn
     close(iFile)
     return
999  write(*,*) 'perfo_ils_init parameter reading Error',iPos
     call exit(1)
  end subroutine perfo_ils_init
!
!
   subroutine ils_perfo_param_transfer( Ils_Perfo_Param, Ils_Perfo )
   implicit none
     type(type_Ils_Perfo_Param),intent(in)            :: Ils_Perfo_Param
     type(type_Ils_Perfo)      ,intent(inout)         :: Ils_Perfo
!
     Ils_Perfo%WnIls     = Ils_Perfo_Param%WnIls
     Ils_Perfo%Ip        = Ils_Perfo_Param%Ip
     Ils_Perfo%Iteration = Ils_Perfo_Param%Iteration
     Ils_Perfo%OSFactor  = Ils_Perfo_Param%OSFactor
     Ils_Perfo%SDWn      = Ils_Perfo_Param%SDWn
!    
   return
   end subroutine ils_perfo_param_transfer
!!
!!
!> anal_ils_spectral -- Public
!!
!! * Purpose
!!
!!     ILS spectral analyse.
!!
!! * Description
!!
!!     This performance estimation module derives ils spectral analyse between a reference 
!!     case and a perturbed one.
!!     After a spectral coherence control between the reference case and the perturbed one,   
!!     for the both case (reference and perturbed), ils spectral performances are computed;
!!     at last, the shift error, the resolution error and the shape error index are determined.
!!
!! * Inputs 
!!
!!     - file_plot       : character / name of writing file with statistical results
!!     - Spectrum        : type_Spectrum / type for declaration and allocation of spectrum 
!!     - Spectrum_ref    : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!!     -  Ils_Perfo      : type_Ils_Perfo / type for declaration and allocation of 
!!                         spectral performance parameters 
!!     -  Ils_Perfo_ref  : type_Ils_Perfo / type for declaration and allocation of 
!!                         spectral performance parameters 
!!
!! * Outputs
!!
!! * References
!!

   subroutine anal_ils_spectral( File_S_Perfo, &
                                 file_plot,    &
                                 Spectrum,     &
                                 Spectrum_ref, &
                                 Ils_Perfo,    &
                                 Ils_Perfo_ref )
   implicit none
     character(len=*)         ,intent(in)                   :: File_S_Perfo
     character(len=*)         ,intent(in)                   :: file_plot
     type(type_Spectrum)      ,intent(in)                   :: Spectrum
     type(type_Spectrum)      ,intent(in)                   :: Spectrum_ref
     type(type_Ils_Perfo)     ,intent(inout)                :: Ils_Perfo
     type(type_Ils_Perfo)     ,intent(inout)                :: Ils_Perfo_ref
     type(type_Ils_Perfo_Param)                             :: Ils_Perfo_Param
     real(kind=DOUBLE)        ,dimension(:) ,allocatable    :: Ils_ref
     real(kind=DOUBLE)        ,dimension(:) ,allocatable    :: Ils
     integer(kind=LONG)                                     :: Ns
     integer(kind=LONG)                                     :: ifile_plot
!
     ifile_plot = 10
!
     call perfo_ils_init( File_S_Perfo, Ils_Perfo_Param )
     call ils_perfo_param_transfer( Ils_Perfo_Param, Ils_Perfo )
     call ils_perfo_param_transfer( Ils_Perfo_Param, Ils_Perfo_ref )
!
!    coherence control
     if( (Spectrum%Ns_First /= Spectrum_ref%Ns_First) .or.  &
         (Spectrum%Ns_Last  /= Spectrum_ref%Ns_Last )  ) then
        write(*,*) 'Spectral coherence Error',              &
                   Spectrum%Ns_First,Spectrum_ref%Ns_First, &
                   Spectrum%Ns_Last,Spectrum_ref%Ns_Last
        write(*,*) 'anal_ils_spectral Fatal Error'
        call exit(1)
     end if
     if( Spectrum%Type /= Spectrum_ref%Type ) then
        write(*,*) 'Spectrum Type Error'
        write(*,*) 'anal_ils_spectral Fatal Error'
        call exit(1)
     end if
     call perfo_ils_spectral( Spectrum_ref, &
                              Ils_Perfo_ref )
     call perfo_ils_spectral( Spectrum, &
                              Ils_Perfo )
     if( Ils_Perfo%Ns_Ils /= Ils_Perfo_ref%Ns_Ils ) then
        write(*,*) 'Ils_Perfo Ns_Ils Error'
        write(*,*) 'anal_ils_spectral Fatal Error'
        call exit(1)
     end if
!
!    Shift Error
     Ils_Perfo%ErShift = ( Ils_Perfo%Shift-Ils_Perfo_ref%Shift ) &
                         / Ils_Perfo%WnIls
!
!    Resolution Error
     Ils_Perfo%ErFWhm = ( Ils_Perfo%FWhm-Ils_Perfo_ref%FWhm ) &
                        / Ils_Perfo_ref%FWhm
!
!    Shape Error Index
     allocate( Ils_ref(Ils_Perfo%Ns_Ils) )
     allocate( Ils(Ils_Perfo%Ns_Ils) )
     Ils_ref(1:Ils_Perfo%Ns_Ils) = &
                           dreal( Ils_Perfo_ref%Ils(1:Ils_Perfo%Ns_Ils) )
     Ils(1:Ils_Perfo%Ns_Ils)     = &
                           dreal( Ils_Perfo%Ils(1:Ils_Perfo%Ns_Ils) )
     Ils_Perfo%ShapeDiff_R(1:Ils_Perfo%Ns_Ils) = &
             ( Ils(1:Ils_Perfo%Ns_Ils) - Ils_ref(1:Ils_Perfo%Ns_Ils) ) &
             * Ils_Perfo%dWn
!
     call shapeindex( Ils_Perfo%Ns_Ils, Ils_ref, Ils,  &
                      Ils_Perfo%dWn, Ils_Perfo%Shape_R )
!
     Ils_ref(1:Ils_Perfo%Ns_Ils) = &
                           dimag( Ils_Perfo_ref%Ils(1:Ils_Perfo%Ns_Ils) )
     Ils(1:Ils_Perfo%Ns_Ils)     = &
                           dimag( Ils_Perfo%Ils(1:Ils_Perfo%Ns_Ils) )
     Ils_Perfo%ShapeDiff_I(1:Ils_Perfo%Ns_Ils) = &
             ( Ils(1:Ils_Perfo%Ns_Ils) - Ils_ref(1:Ils_Perfo%Ns_Ils) ) &
             * Ils_Perfo%dWn
!
     call shapeindex( Ils_Perfo%Ns_Ils, Ils_ref, Ils,  &
                      Ils_Perfo%dWn, Ils_Perfo%Shape_I )
!
     write(*,fmt='(a)') file_plot(1:len_trim(file_plot))
     open(ifile_plot,file=file_plot,err=999)
     write(ifile_plot,fmt='(a)',err=999) &
     '# WnIls; WnShift0; WnShift; ErShift; FWhm0; FWhm; ErFWhm; Shape_R; Shape_I'
     write(ifile_plot,fmt='(a,f12.3,3e12.4,2f12.4,3e12.4)',err=999) &
     '#',Ils_Perfo%WnIls,Ils_Perfo_ref%Shift,Ils_Perfo%Shift,&
         Ils_Perfo%ErShift,Ils_Perfo_ref%FWhm,Ils_Perfo%FWhm,  &
         Ils_Perfo%ErFWhm, Ils_Perfo%Shape_R,Ils_Perfo%Shape_I
     write(ifile_plot,fmt='(a)',err=999) '# Wn Ils_ref Ils Shape_R Shape_I'
     do Ns = 1, Ils_Perfo%Ns_Ils
        write(ifile_plot,9999,err=999)               &
                         Ils_Perfo%Wn(Ns)/100.d+00,  &
                         Ils_Perfo_ref%Ils(Ns),      &
                         Ils_Perfo%Ils(Ns),          &
                         Ils_Perfo%ShapeDiff_R(Ns),  &
                         Ils_Perfo%ShapeDiff_I(Ns)
     end do
     close(ifile_plot)
!
     deallocate( Ils_ref )
     deallocate( Ils )
     return
 999 write(*,fmt='(a)') 'anal_ils_spectral sortie en erreur'
     call exit(1)
9999 format (1h ,30e21.12)
   end subroutine anal_ils_spectral
!
!

!!
!!
!> perfo_ils_spectral -- Public
!!
!! * Purpose
!!
!!     Computation of ils spectral performances 
!!
!! * Description
!!
!!     This performance estimation module derives ils spectral performances. 
!!     The following steps are required:
!!       - the first step consists in the definition of the FFT size, and the pivot point.
!!       - then, the required tables are allocated for the symetrised usefull spectrum and 
!!         for the interferogram;
!!       - the usefull spectrum symetrisation is done, and the real interferogram is computed. 
!!       - the interferogram is over sampled and zero padding is applied.
!!       - thanks to a Fourier transform, the over sampled spectrum is computed and its 
!!         barycentre is determined in order to define the spectral shift.
!!       - ILS centred spectral basis and spectral basis centred on barycentre, both are then 
!!         defined;
!!       - the next step consist in real and imaginary parts extraction, interpolation and 
!!         ILS normalisation.
!!       - Then the same operations are repeated on the ILS: the ILS fine barycentre is 
!!         computed, the spectral basis centred on this new barycentre is defined: that allows 
!!         to compute a fine spectral shift as the sum of the spectrum barycentre and the
!!         ILS Barycentre minus the ILS central wavenumber. 
!!       - then, the next step consist in real and imaginary parts extraction, interpolation and 
!!         ILS normalisation. As well, the ILS real and imaginary part are extracted and 
!!         interpolated, and the ILS is normalised.
!!       - At last, in order to minimize the troncation errors, the same operations are repeated
!!         one more time : a very fine barycentre is computed, a new spectral basis centred on 
!!         this new barycentre is defined, and the very fine spectral shift is then determined.
!!       - The full width at half maximum of the ILS is finally computed. 
!!
!! * Inputs 
!!
!!     - Spectrum  : type_Spectrum / type for declaration and allocation of spectrum 
!!
!! * Inputs/outputs
!!
!!     - Ils_Perfo : type_Ils_Perfo / type for declaration and allocation of 
!!                   spectral performance parameters 
!!
!! * Outputs
!!
!! * References
!!

   subroutine perfo_ils_spectral( Spectrum, &
                                  Ils_Perfo )
   implicit none
     type(type_Spectrum)      ,intent(in)                     :: Spectrum
     type(type_Ils_Perfo)     ,intent(inout)                  :: Ils_Perfo
     integer(kind=LONG)                                       :: NSample
     integer(kind=LONG)                                       :: NsFft
     integer(kind=LONG)                                       :: NsFftp
     integer(kind=LONG)                                       :: ios
     complex(kind=DOUBLE)     ,dimension(:) ,allocatable      :: Sp_Sym_C
     real(kind=DOUBLE)        ,dimension(:) ,allocatable      :: Sp_Sym_R
     integer(kind=LONG)                                       :: Sens
     real(kind=DOUBLE)                                        :: dOpd
     real(kind=DOUBLE)        ,dimension(:) ,allocatable      :: Interf
     integer(kind=LONG)                                       :: NsDel
     integer(kind=LONG)                                       :: OSNsFft
     integer(kind=LONG)                                       :: OSNsFftp
     real(kind=DOUBLE)        ,dimension(:) ,allocatable      :: OSInterf
     real(kind=DOUBLE)                                        :: OSdWn
     complex(kind=DOUBLE)     ,dimension(:) ,allocatable      :: OSSp
     complex(kind=DOUBLE)     ,dimension(:) ,allocatable      :: tmp
     real(kind=DOUBLE)        ,dimension(:) ,allocatable      :: OSWn
     integer(kind=LONG)                                       :: Ns
     integer(kind=LONG)                                       :: NsPivot
     integer(kind=LONG)                                       :: Nslaser
     integer(kind=LONG)                                       :: OSNsfirst
     integer(kind=LONG)                                       :: OSNslast
     real(kind=DOUBLE)                                        :: Barycentre
     real(kind=DOUBLE)        ,dimension(:) ,allocatable      :: OSWn_Cal
     real(kind=DOUBLE)        ,dimension(:) ,allocatable      :: OSSp_C
     real(kind=DOUBLE)        ,dimension(:) ,allocatable      :: Ils_R
     real(kind=DOUBLE)        ,dimension(:) ,allocatable      :: Ils_I
     complex(kind=DOUBLE)                                     :: IlsNorm
!
!    FFT size 
     NSample = 4 * int(1.3 * Spectrum%Ns_Last)
     call tabule_NsFft( NSample, &
                        NsFft,   &
                        ios      )
     if( ios /= 0 ) then
        write(*,*) 'tabule_NsFft Error'
        write(*,*) 'perfo_ils_spectral Fatal Error'
        call exit(1)
     end if
!
!    Pivot point
     NsFftp = int(NsFft/2)
!
!    Allocation
     allocate( Interf(NsFft+1) )
!
!    Usefull spectrum symetrisation
     if( Spectrum%Type == 'C' ) then
        allocate( Sp_Sym_C(NsFft+1) )
        Sp_Sym_C(1:NsFft+1) = dcmplx(0.d+00,0.d+00)
        Sp_Sym_C(NsFftp+1+(Spectrum%Ns_First-1): &
                 NsFftp+1+(Spectrum%Ns_Last-1)) =&
             Spectrum%Complex(1:Spectrum%N_Sample)
        Sp_Sym_C(NsFftp+1-(Spectrum%Ns_Last-1):   &
                 NsFftp+1-(Spectrum%Ns_First-1)) =&
              dconjg(Spectrum%Complex(Spectrum%N_Sample:1:-1))
!
!       Real interferogram
        dOpd = 1.d+00 / ( 2*Spectrum%dWn*dble(NsFftp) )
        Sens = +1
        call fft_c2r( NsFft, Sens, Sp_Sym_C, Spectrum%dWn, dOpd, Interf )
        deallocate( Sp_Sym_C )
     else if( Spectrum%Type == 'R' ) then
        allocate( Sp_Sym_R(NsFft+1) )
        Sp_Sym_R(1:NsFft+1) = 0.d+00
        Sp_Sym_R(NsFftp+1+(Spectrum%Ns_First-1): &
                 NsFftp+1+(Spectrum%Ns_Last-1)) =&
             Spectrum%Real_Part(1:Spectrum%N_Sample)
        Sp_Sym_R(NsFftp+1-(Spectrum%Ns_Last-1):   &
                 NsFftp+1-(Spectrum%Ns_First-1)) =&
             Spectrum%Real_Part(Spectrum%N_Sample:1:-1)
!
!       Real interferogram
        dOpd = 1.d+00 / ( 2*Spectrum%dWn*dble(NsFftp) )
        Sens = +1
        call fft_r2r( NsFft, Sens, Sp_Sym_R, Spectrum%dWn, dOpd, Interf )
        deallocate( Sp_Sym_R )
     else if( Spectrum%Type == 'RI' ) then
        allocate( Sp_Sym_C(NsFft+1) )
        Sp_Sym_C(1:NsFft+1) = dcmplx(0.d+00,0.d+00)
        Sp_Sym_C(NsFftp+1+(Spectrum%Ns_First-1): &
                 NsFftp+1+(Spectrum%Ns_Last-1)) =&
             dcmplx( Spectrum%Real_Part(1:Spectrum%N_Sample),&
                     Spectrum%Imag_Part(1:Spectrum%N_Sample) )
        Sp_Sym_C(NsFftp+1-(Spectrum%Ns_Last-1):   &
                 NsFftp+1-(Spectrum%Ns_First-1)) =&
             dcmplx( Spectrum%Real_Part(Spectrum%N_Sample:1:-1),&
                    -Spectrum%Imag_Part(Spectrum%N_Sample:1:-1) )
!
!       Real interferogram
        dOpd = 1.d+00 / ( 2*Spectrum%dWn*dble(NsFftp) )
        Sens = +1
        call fft_c2r( NsFft, Sens, Sp_Sym_C, Spectrum%dWn, dOpd, Interf )
        deallocate( Sp_Sym_C )
     else
        write(*,*) 'Wrong Spectrum Type must be R or C'
        call exit(1)
     end if
!
!    Interferogram over sampling
     OSNsFft = 2 * ( Ils_Perfo%OSFactor * NsFftp )
     OSNsFftp = int(OSNsFft/2)
     NsDel = OSNsFftp - NsFftp
     if( NsDel < 1 ) then
        write(*,*) 'NsDel Error',NsDel
        write(*,*) 'perfo_ils_spectral Fatal Error'
        call exit(1)
     end if
!
!    zero padded interferogram
     allocate( OSInterf(OSNsFft+1) )
     allocate( OSSp(OSNsFft+1) )
     OSInterf(1:OSNsFft+1) = 0.d+00
     OSInterf(NsDel+1:NsDel+NsFft) = Interf(1:NsFft)
!
!    Over Sampled Spectrum
     OSdWn = Spectrum%dWn / dble(Ils_Perfo%OSFactor)
     Sens = -1
     call fft_r2c( OSNsFft, Sens, OSInterf, dOpd, Spectrum%dWn, OSSp )
!
!    barycentre
     allocate( OSWn(OSNsFftp+1) )
     OSWn(1:OSNsFftp+1) = (/ (dble(Ns-1),Ns=1,OSNsFftp+1) /) * OSdWn &
                        - Ils_Perfo%WnIls
     Nslaser = idnint( Ils_Perfo%WnIls / Spectrum%dWn ) + 1
     Ils_Perfo%dWn = OSdWn
     Ils_Perfo%Ns_Ils  = 2 * idnint( Ils_Perfo%SDWn / Ils_Perfo%dWn ) + 1
     OSNsfirst = (Nslaser-1) * Ils_Perfo%OSFactor + 1 &
               - 2*int(Ils_Perfo%Ns_Ils/2)
     OSNslast  = (Nslaser-1) * Ils_Perfo%OSFactor + 1 &
               + 2*int(Ils_Perfo%Ns_Ils/2)
     call barycen_iter_cplx( OSNslast-OSNsfirst+1,    &
                             Ils_Perfo%dWn,           &
                             OSWn(OSNsfirst),         &
                             OSSp(OSNsFftp+OSNsfirst),&
                             Ils_Perfo%Ip,            &
                             Ils_Perfo%Iteration,     &
                             Barycentre               )

     write(*,*) 'Spectral_module : Barycentre',Barycentre
!
!    Spectral perfo allocation
     call alloc_Ils_Perfo( Ils_Perfo )
!
!    Spectral shift
     Ils_Perfo%Shift = -Barycentre
     write(*,*) 'Spectral_module : Shift',Ils_Perfo%Shift
!
!    ILS centred spectral basis
     NsPivot = int(Ils_Perfo%Ns_Ils/2)+1
     Ils_Perfo%Wn(1:Ils_Perfo%Ns_Ils) = &
               (/ (dble(Ns-NsPivot),Ns=1,Ils_Perfo%Ns_Ils) /) &
               * Ils_Perfo%dWn
!
!    Spectral basis centered on barycentre
     allocate( OSWn_Cal(OSNsFftp+1) )
     OSWn_Cal(1:OSNsFftp+1) = OSWn(1:OSNsFftp+1) - Barycentre
!
!    Real and Imaginary part extraction and interpolation
     allocate( OSSp_C(OSNsFftp+1) )
     allocate( Ils_R(Ils_Perfo%Ns_Ils) )
     allocate( Ils_I(Ils_Perfo%Ns_Ils) )
     OSSp_C(1:OSNsFftp+1) = dreal(OSSp(OSNsFftp+1:OSNsFft+1))
     call intspline( OSNsFftp+1, OSWn_Cal, OSSp_C,         &
                     Ils_Perfo%Ns_Ils, Ils_Perfo%Wn, Ils_R )
     OSSp_C(1:OSNsFftp+1) = dimag(OSSp(OSNsFftp+1:OSNsFft+1))
     call intspline( OSNsFftp+1, OSWn_Cal, OSSp_C,         &
                     Ils_Perfo%Ns_Ils, Ils_Perfo%Wn, Ils_I )
!
!    Ils mirroring
!
!    Spectrum mirroring
     allocate( tmp(Ils_Perfo%Ns_Ils) )
     tmp(1:Ils_Perfo%Ns_Ils)   = Ils_R(Ils_Perfo%Ns_Ils:1:-1)
     Ils_R(1:Ils_Perfo%Ns_Ils) = tmp(1:Ils_Perfo%Ns_Ils)
     tmp(1:Ils_Perfo%Ns_Ils)   = Ils_I(Ils_Perfo%Ns_Ils:1:-1)
     Ils_I(1:Ils_Perfo%Ns_Ils) = tmp(1:Ils_Perfo%Ns_Ils)
     deallocate(tmp)
     call realimagtocplx( Ils_Perfo%Ils, Ils_R, Ils_I, Ils_Perfo%Ns_Ils )
!
!    Ils normalisation
     IlsNorm = dcmplx(0.d+00,0.d+00)
     IlsNorm = IlsNorm + Ils_Perfo%Ils(1)                &
                       * Ils_Perfo%dWn/2.d+00
     IlsNorm = IlsNorm + Ils_Perfo%Ils(Ils_Perfo%Ns_Ils) &
                       * Ils_Perfo%dWn/2.d+00
     IlsNorm = IlsNorm + sum( Ils_Perfo%Ils(2:Ils_Perfo%Ns_Ils-1) )&
                       * Ils_Perfo%dWn
     Ils_Perfo%Ils(1:Ils_Perfo%Ns_Ils) = &
                   Ils_Perfo%Ils(1:Ils_Perfo%Ns_Ils) / IlsNorm
!
!    Width at Half Maximum
     call fwhm_cplx( Ils_Perfo%Ns_Ils, Ils_Perfo%Ils, &
                    Ils_Perfo%dWn, Ils_Perfo%FWhm )
     write(*,*) 'Ils_Perfo%FWhm',Ils_Perfo%FWhm
!
     deallocate( Ils_R )
     deallocate( Ils_I )
     deallocate( OSSp_C )
     deallocate( OSWn_Cal )
     deallocate( OSWn )
     deallocate( OSSp )
     deallocate( OSInterf )
     deallocate( Interf )
     return
   end subroutine perfo_ils_spectral
!
!

!!
!!
!> anal_srf_spectral -- Public
!!
!! * Purpose
!!
!!     SRF spectral performance analyse
!!     
!!
!! * Description
!!
!!     This performance estimation subroutine derives srf spectral performances thanks  
!!     the comparison between a nominal case and a perturbed one.
!!     First, for each spectral band, the srf sampling coherence is ckecked between the 
!!     nominal and the perturbed cases. Then, the shape index is computed.
!!
!! * Inputs 
!!
!!     - file_plot    : character / name of writing file with statistical results
!!     - NBand        : integer / spectral band number
!!     - Level        : character / flag for definition of analysed level
!!     - Srf_nom      : type_Srf / type for declaration and allocation of srf
!!     - Srf_per      : type_Srf / type for declaration and allocation of srf
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!! * References
!!

   subroutine anal_srf_spectral( file_plot, &
                                 Nband,     &
                                 Level,     &
                                 Srf_nom,   &
                                 Srf_per    )
   implicit none
     character(len=*)    ,intent(in)                         :: file_plot
     integer(kind=LONG)  ,intent(in)                         :: Nband
     character(len=*)    ,intent(in)                         :: Level
     type(type_Srf)      ,intent(in),dimension(Nband)        :: Srf_nom
     type(type_Srf)      ,intent(in),dimension(Nband)        :: Srf_per
     integer(kind=LONG)                                      :: iFile
     integer(kind=LONG)                                      :: iPos
     integer(kind=LONG)                                      :: PN
     integer(kind=LONG)                                      :: SB
     integer(kind=LONG)                                      :: NF
     real(kind=DOUBLE)                                       :: ShapeId
     integer(kind=LONG)                                      :: NsSDWn1a
     integer(kind=LONG)                                      :: Ns1a_n_0
     integer(kind=LONG)                                      :: Ns1a_p_0
     integer(kind=LONG)                                      :: NsSDWn1b
     integer(kind=LONG)                                      :: Ns1b_n_0
     integer(kind=LONG)                                      :: Ns1b_p_0
     integer(kind=LONG)                                      :: NsSDWn1c
     integer(kind=LONG)                                      :: Ns1c_n_0
     integer(kind=LONG)                                      :: Ns1c_p_0
!
     iFile = 100
     iPos = 1
     open(unit=iFile, file=file_plot, err=999)
     iPos = 2
     write(iFile,'(a,a)',err=999) '# Wn; Shift_nom; Shift_per; Resol_nom; ',&
                                  'Resol_per; ErShift; ErResol, ErShape'
     do SB = 1, Nband
!
!      Srf sampling coherence
       if( Srf_nom(SB)%NsSDWn1a /= Srf_per(SB)%NsSDWn1a ) then
         NsSDWn1a = min( Srf_nom(SB)%NsSDWn1a, Srf_per(SB)%NsSDWn1a )
         if( NsSDWn1a == Srf_nom(SB)%NsSDWn1a ) then
           Ns1a_n_0 = 1
           Ns1a_p_0 = int(Srf_per(SB)%NsSDWn1a/2)   &
                    - int(Srf_nom(SB)%NsSDWn1a/2) + 1
         else
           Ns1a_p_0 = 1
           Ns1a_n_0 = int(Srf_nom(SB)%NsSDWn1a/2)   &
                    - int(Srf_per(SB)%NsSDWn1a/2) + 1
         end if
       else
         NsSDWn1a = Srf_nom(SB)%NsSDWn1a
         Ns1a_n_0 = 1
         Ns1a_p_0 = 1
       end if
       if( Srf_nom(SB)%NsSDWn1b /= Srf_per(SB)%NsSDWn1b ) then
         NsSDWn1b = min( Srf_nom(SB)%NsSDWn1b, Srf_per(SB)%NsSDWn1b )
         if( NsSDWn1b == Srf_nom(SB)%NsSDWn1b ) then
           Ns1b_n_0 = 1
           Ns1b_p_0 = int(Srf_per(SB)%NsSDWn1b/2)   &
                    - int(Srf_nom(SB)%NsSDWn1b/2) + 1
         else
           Ns1b_p_0 = 1
           Ns1b_n_0 = int(Srf_nom(SB)%NsSDWn1b/2)   &
                    - int(Srf_per(SB)%NsSDWn1b/2) + 1
         end if
       else
         NsSDWn1b = Srf_nom(SB)%NsSDWn1b
         Ns1b_n_0 = 1
         Ns1b_p_0 = 1
       end if
       if( Srf_nom(SB)%NsSDWn1c /= Srf_per(SB)%NsSDWn1c ) then
         NsSDWn1c = min( Srf_nom(SB)%NsSDWn1c, Srf_per(SB)%NsSDWn1c )
         if( NsSDWn1c == Srf_nom(SB)%NsSDWn1c ) then
           Ns1c_n_0 = 1
           Ns1c_p_0 = int(Srf_per(SB)%NsSDWn1c/2)   &
                    - int(Srf_nom(SB)%NsSDWn1c/2) + 1
         else
           Ns1c_p_0 = 1
           Ns1c_n_0 = int(Srf_nom(SB)%NsSDWn1c/2)   &
                    - int(Srf_per(SB)%NsSDWn1c/2) + 1
         end if
       else
         NsSDWn1c = Srf_nom(SB)%NsSDWn1c
         Ns1c_n_0 = 1
         Ns1c_p_0 = 1
       end if
!       write(*,*) 'NsSDWn1a',NsSDWn1a
!       write(*,*) 'Ns1a_n_0',Ns1a_n_0
!       write(*,*) 'Ns1a_p_0',Ns1a_p_0
!       write(*,*) 'NsSDWn1b',NsSDWn1b
!       write(*,*) 'Ns1b_n_0',Ns1b_n_0
!       write(*,*) 'Ns1b_p_0',Ns1b_p_0
!       write(*,*) 'NsSDWn1c',NsSDWn1c
!       write(*,*) 'Ns1c_n_0',Ns1c_n_0
!       write(*,*) 'Ns1c_p_0',Ns1c_p_0
!
       PN = Srf_nom(SB)%NsPixel+1
       do NF = 1, Srf_nom(SB)%NsWn0
          if( Level == 'L1a' ) then
             call shapeindex( NsSDWn1a,                       &
                              Srf_nom(SB)%L1a(Ns1a_n_0,NF,PN),&
                              Srf_per(SB)%L1a(Ns1a_p_0,NF,PN),&
                              Srf_nom(SB)%dWn1a, ShapeId      )
             write(iFile,'(f10.2,4f10.6,3e12.4)',err=999)  &
                         Srf_nom(SB)%Wn0(NF)/100.,         &
                         Srf_nom(SB)%WnShift1a(NF,PN)/100.,&
                         Srf_per(SB)%WnShift1a(NF,PN)/100.,&
                         Srf_nom(SB)%FWhm1a(NF,PN)/100.,   &
                         Srf_per(SB)%FWhm1a(NF,PN)/100.,   &
                         ( Srf_per(SB)%WnShift1a(NF,PN)    &
                          -Srf_nom(SB)%WnShift1a(NF,PN))   &
                         /Srf_nom(SB)%Wn0(NF),             &
                         ( Srf_per(SB)%FWhm1a(NF,PN)       &
                          -Srf_nom(SB)%FWhm1a(NF,PN))      &
                         /Srf_nom(SB)%FWhm1a(NF,PN),       &
                         ShapeId
          else if( Level == 'L1b' ) then
             call shapeindex( NsSDWn1b,                       &
                              Srf_nom(SB)%L1b(Ns1b_n_0,NF,PN),&
                              Srf_per(SB)%L1b(Ns1b_p_0,NF,PN),&
                              Srf_nom(SB)%dWn1b, ShapeId      )
             write(iFile,'(f10.2,4f10.6,3e12.4)',err=999)  &
                         Srf_nom(SB)%Wn0(NF)/100.,         &
                         Srf_nom(SB)%WnShift1b(NF,PN)/100.,&
                         Srf_per(SB)%WnShift1b(NF,PN)/100.,&
                         Srf_nom(SB)%FWhm1b(NF,PN)/100.,   &
                         Srf_per(SB)%FWhm1b(NF,PN)/100.,   &
                         ( Srf_per(SB)%WnShift1b(NF,PN)    &
                          -Srf_nom(SB)%WnShift1b(NF,PN))   &
                         /Srf_nom(SB)%Wn0(NF),             &
                         ( Srf_per(SB)%FWhm1b(NF,PN)       &
                          -Srf_nom(SB)%FWhm1b(NF,PN))      &
                         /Srf_nom(SB)%FWhm1b(NF,PN),       &
                         ShapeId
          else if( Level == 'L1c' ) then
             call shapeindex( NsSDWn1c,                      &
                              Srf_nom(SB)%L1c(Ns1c_n_0,1,1), &
                              Srf_per(SB)%L1c(Ns1c_p_0,NF,1),&
                              Srf_nom(SB)%dWn1c, ShapeId     )
             write(iFile,'(f10.2,4f10.6,3e12.4)',err=999)  &
                         Srf_nom(SB)%Wn0(NF)/100.,         &
                         Srf_nom(SB)%WnShift1c(1,1)/100.,  &
                         Srf_per(SB)%WnShift1c(NF,1)/100., &
                         Srf_nom(SB)%FWhm1c(1,1)/100.,     &
                         Srf_per(SB)%FWhm1c(NF,1)/100.,    &
                         ( Srf_per(SB)%WnShift1c(NF,1)     &
                          -Srf_nom(SB)%WnShift1c(1,1))     &
                         /Srf_nom(SB)%Wn0(NF),             &
                         ( Srf_per(SB)%FWhm1c(NF,1)        &
                          -Srf_nom(SB)%FWhm1c(1,1))        &
                         /Srf_nom(SB)%FWhm1c(1,1),         &
                         ShapeId
          else if( Level == 'L1d' ) then
             call shapeindex( NsSDWn1c,                      &
                              Srf_nom(SB)%L1c(Ns1c_n_0,1,1), &
                              Srf_per(SB)%L1c(Ns1c_p_0,NF,1),&
                              Srf_nom(SB)%dWn1c, ShapeId     )
             write(iFile,'(f10.2,4f10.6,3e12.4)',err=999)  &
                         Srf_nom(SB)%Wn0(NF)/100.,         &
                         Srf_nom(SB)%WnShift1c(1,1)/100.,  &
                         Srf_per(SB)%WnShift1c(NF,1)/100., &
                         Srf_nom(SB)%FWhm1c(1,1)/100.,     &
                         Srf_per(SB)%FWhm1c(NF,1)/100.,    &
                         ( Srf_per(SB)%WnShift1c(NF,1)     &
                          -Srf_nom(SB)%WnShift1c(1,1))     &
                         /Srf_nom(SB)%Wn0(NF),             &
                         ( Srf_per(SB)%FWhm1c(NF,1)        &
                          -Srf_nom(SB)%FWhm1c(1,1))        &
                         /Srf_nom(SB)%FWhm1c(1,1),         &
                         ShapeId
          else
             write(*,*) ' Level Error',Level
             write(*,*) 'anal_srf_spectral Fatal Error'
             go to 999
          end if
       end do
     end do
     close(unit=iFile)
!
     return
 999 write(*,*) 'writing results error',iPos
     write(*,*) 'anal_srf_spectral Fatal Error'
     call exit(1)
   end subroutine anal_srf_spectral
!
!

!!
!!
!> shapeindex -- Public
!!
!! * Purpose
!!
!!     Shape index computation
!!
!! * Description
!!
!!     This subroutine computes the shape index between a reference spectral response 
!!     function and an analysed one.
!!
!! * Inputs 
!!
!!     - ndim  : dimension 
!!     - y_ref : reference spectral response function
!!     - y     : analysed spectral response function
!!     - dx    : wavenumber sampling
!!
!! * Inputs/outputs
!!
!!     - ShapeId : shape index
!!
!! * Outputs
!!
!! * References
!!

   subroutine shapeindex( ndim, y_ref, y, dx, ShapeId )
   implicit none
     integer(kind=LONG),intent(in)                  :: ndim
     real(kind=DOUBLE) ,intent(in) ,dimension(ndim) :: y_ref
     real(kind=DOUBLE) ,intent(in) ,dimension(ndim) :: y
     real(kind=DOUBLE) ,intent(in)                  :: dx
     real(kind=DOUBLE) ,intent(inout)               :: ShapeId
!
!    shape index 
     ShapeId = sum( dabs( y(1:ndim) - y_ref(1:ndim) ) ) * dx
!
     return
   end subroutine shapeindex
!
!
end module spectral_module
