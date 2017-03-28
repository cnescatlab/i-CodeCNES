! plot_module.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.20 $
! Last modification: $Date: 2012-02-08 10:19:14 $
!
! Graphe -- Module
!
! * Purpose
!   Module for graph outputs
!
module plot_module
   use precision_type
   use error_type
   use constantes_type
   use interferogram_type
   use zpd_type
   use spectrum_type
   use diagonal_noise_type
   use matrix_noise_type
   use radiometry_error_type
   use radiometry_stats_type
   use ils_perfo_stats_type
   use srf_perfo_type
   use srf_param_type
   use psf_type
   use ccm_type
   use saf_type
   use srf_type
   use apf_type
   use general_type
   use gain_type
   use forman_type
   use phase_mertz_type
   use srf_moments_type
   use dc_component_type
   use math_module
   use plancklib_module
!
   implicit none
!
!
   public :: plot_interf,                &
             plot_interf_rpd,            &
             plot_rpd_top,               &
             plot_spectre,               &
             plot_noise_diagonal,        &
             plot_radiometry_error,      &
             plot_radiometry_stats,      &
             plot_ils_perfo_stats,       &
             plot_srf_perfo,             &
             plot_moments,               &
             plot_apf,                   &
             plot_saf,                   &
             plot_fct,                   &
             plot_spectral,              &
             plot_spectral_tas,          &
             plot_l1c_perturb,           &
             plot_ccm,                   &
             plot_zpd,                   &
             plot_psf,                   &
             plot_gain,                  &
             plot_forman,                &
             plot_phase_mertz,           &
             plot_dc_component,          &
             plot_dc_component_1
!
!
   contains
!
!
   subroutine plot_interf( File_Plot, &
                           Zpd,       &
                           Interf     )
!
   implicit none
   character(len=500)      ,intent(in)                  :: File_Plot
   type(type_Zpd)          ,intent(in)                  :: Zpd
   type(type_Interferogram),intent(in)                  :: Interf
   real(kind=DOUBLE)                                    :: Opd0
   real(kind=DOUBLE)                                    :: Time0
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: iFile
!
     Opd0  = Interf%Opd(Zpd%NZpd)
     Time0 = Interf%Time(Zpd%NZpd)
!
     iFile = 10
     open(iFile,file=File_Plot,err=999)
     if( Interf%Type == 'C' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),C,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,&
                                        Interf%Time(Ns)-Time0, &
                                        Interf%Opd(Ns)-Opd0,   &
                                        Interf%Complex(Ns)
        end do
     else if( Interf%Type == 'RI' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),RI,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,&
                                        Interf%Time(Ns)-Time0,&
                                        Interf%Opd(Ns)-Opd0,&
                                        Interf%Real_Part(Ns),&
                                        Interf%Imag_Part(Ns)
        end do
     else if( Interf%Type == 'MA' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),MA,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,&
                                        Interf%Time(Ns)-Time0,&
                                        Interf%Opd(Ns)-Opd0,&
                                        Interf%Modulus(Ns),&
                                        Interf%Argument(Ns)
        end do
     else if( Interf%Type == 'R' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),R,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,&
                                        Interf%Time(Ns)-Time0,&
                                        Interf%Opd(Ns)-Opd0,&
                                        Interf%Real_Part(Ns)
        end do
     else if( Interf%Type == 'I' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),I,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,&
                                        Interf%Time(Ns)-Time0,&
                                        Interf%Opd(Ns)-Opd0,&
                                        Interf%Imag_Part(Ns)
        end do
     else if( Interf%Type == 'M' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),M,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,&
                                        Interf%Time(Ns)-Time0,&
                                        Interf%Opd(Ns)-Opd0,&
                                        Interf%Modulus(Ns)
        end do
     else if( Interf%Type == 'A' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),A,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,&
                                        Interf%Time(Ns)-Time0,&
                                        Interf%Opd(Ns)-Opd0,&
                                        Interf%Argument(Ns)
        end do
     else
        write(*,*) 'Interf%Type Error',Interf%Type
        go to 999
     end if
     close(iFile)
     return
 999 write(0,*) 'plot_interf write Error : '&
                ,File_Plot(1:len_trim(File_Plot))
     call exit(1)
   end subroutine plot_interf
!
!
   subroutine plot_interf_rpd( File_Plot, &
                               Interf     )
!
   implicit none
   character(len=500)      ,intent(in)                  :: File_Plot
   type(type_Interferogram),intent(in)                  :: Interf
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: iFile
!
     iFile = 10
     open(iFile,file=File_Plot,err=999)
     if( Interf%Type == 'RI' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),RI,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,Interf%Time(Ns),  &
                                        Interf%Opd(Ns),      &
                                        Interf%Real_Part(Ns),&
                                        Interf%Imag_Part(Ns)
        end do
     else if( Interf%Type == 'R' ) then
        write(iFile,'(a)') '#Ns,Time(Ns),Opd(Ns),R,Interf(Ns)'
        do Ns = 1, Interf%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,Interf%Time(Ns),Interf%Opd(Ns),&
                                        Interf%Real_Part(Ns)
        end do
     else
        write(*,*) 'Interf%Type Error',Interf%Type
        go to 999
     end if
     close(iFile)
     return
 999 write(0,*) 'plot_interf write Error : '&
                ,File_Plot(1:len_trim(File_Plot))
     call exit(1)
   end subroutine plot_interf_rpd
!
!
   subroutine plot_rpd_top( File_Plot, &
                            Interf     )
!
   implicit none
   character(len=500)      ,intent(in)                  :: File_Plot
   type(type_Interferogram),intent(in)                  :: Interf
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: iFile
!
     iFile = 10
     open(iFile,file=File_Plot,err=999)
     write(iFile,'(a)') '#Ns,Time_Top(Ns),Opd_Top(Ns)'
     do Ns = 1, Interf%N_Top
        write(iFile,'(i10,4e20.10)') Ns,Interf%Time_Top(Ns),&
                                        Interf%Opd_Top(Ns)
     end do
     close(iFile)
     return
 999 write(0,*) 'plot_interf write Error : '&
                ,File_Plot(1:len_trim(File_Plot))
     call exit(1)
   end subroutine plot_rpd_top
!
!
   subroutine plot_spectre( File_Plot, &
                            Spectrum     )
!
   implicit none
   character(len=500)      ,intent(in)                  :: File_Plot
   type(type_Spectrum)     ,intent(in)                  :: Spectrum
   integer(kind=LONG)                                   :: Ns
   real(kind=DOUBLE)                                    :: TBr
   real(kind=DOUBLE)                                    :: TBi
   integer(kind=LONG)                                   :: iFile
!
     iFile = 10
     open(iFile,file=File_Plot,err=999)
     if( Spectrum%Type == 'C' ) then
        write(iFile,'(a)') '#Ns,Wn(Ns),C,Spectrum(Ns)'
        do Ns = 1, Spectrum%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,Spectrum%Wn(Ns)/100.,&
                                        Spectrum%Complex(Ns)*100.
        end do
     else if( Spectrum%Type == 'RI' ) then
        write(iFile,'(a)') '#Ns,Wn(Ns),R_Spectrum(Ns),TBR I_Spectrum(Ns),TBI'
        do Ns = 1, Spectrum%N_Sample
           call plkinverse( TBr, Spectrum%Wn(Ns), Spectrum%Real_Part(Ns) )
           call plkinverse( TBi, Spectrum%Wn(Ns), Spectrum%Imag_Part(Ns) )
           write(iFile,'(i10,5e20.10)') Ns,Spectrum%Wn(Ns)/100.,    &
                                        Spectrum%Real_Part(Ns)*100.,&
                                        TBr,                        &
                                        Spectrum%Imag_Part(Ns)*100.,&
                                        TBi
        end do
     else if( Spectrum%Type == 'MA' ) then
        write(iFile,'(a)') '#Ns,Wn(Ns),MA,Spectrum(Ns)'
        do Ns = 1, Spectrum%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,Spectrum%Wn(Ns)/100.,  &
                                        Spectrum%Modulus(Ns)*100.,&
                                        Spectrum%Argument(Ns)
        end do
     else if( Spectrum%Type == 'R' ) then
        write(iFile,'(a)') '#Ns,Wn(Ns),R,Spectrum(Ns),TBR'
        do Ns = 1, Spectrum%N_Sample
           call plkinverse( TBr, Spectrum%Wn(Ns), Spectrum%Real_Part(Ns) )
           write(iFile,'(i10,4e20.10)') Ns,Spectrum%Wn(Ns)/100.,    &
                                        Spectrum%Real_Part(Ns)*100.,&
                                        TBr
        end do
     else if( Spectrum%Type == 'I' ) then
        write(iFile,'(a)') '#Ns,Wn(Ns),I,Spectrum(Ns),TBI'
        do Ns = 1, Spectrum%N_Sample
           call plkinverse( TBi, Spectrum%Wn(Ns), Spectrum%Imag_Part(Ns) )
           write(iFile,'(i10,4e20.10)') Ns,Spectrum%Wn(Ns)/100.,    &
                                        Spectrum%Imag_Part(Ns)*100.,&
                                        TBi
        end do
     else if( Spectrum%Type == 'M' ) then
        write(iFile,'(a)') '#Ns,Wn(Ns),M,Spectrum(Ns)'
        do Ns = 1, Spectrum%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,Spectrum%Wn(Ns)/100.,&
                                        Spectrum%Modulus(Ns)*100.
        end do
     else if( Spectrum%Type == 'A' ) then
        write(iFile,'(a)') '#Ns,Wn(Ns),A,Spectrum(Ns)'
        do Ns = 1, Spectrum%N_Sample
           write(iFile,'(i10,4e20.10)') Ns,Spectrum%Wn(Ns)/100., &
                                        Spectrum%Argument(Ns)
        end do
     else
        write(*,*) 'Spectrum%Type Error',Spectrum%Type
        go to 999
     end if
     close(iFile)
     return
 999 write(0,*) 'plot_spectre write Error : '&
                ,File_Plot(1:len_trim(File_Plot))
     call exit(1)
   end subroutine plot_spectre
!
!
   subroutine plot_noise_matrix( Matrix_Noise )
   implicit none
   type(type_Matrix_Noise)                                  :: Matrix_Noise
   integer(kind=LONG)                                       :: Ns
   integer(kind=LONG)                                       :: Nss
   integer(kind=LONG)                                       :: Index
   integer(kind=LONG)                                       :: iFile
   real(kind=DOUBLE)                                        :: Zero
!
     Zero = 0_DOUBLE
     iFile = 10
     open(iFile,file=trim(Matrix_Noise%filename),err=999)
     write(iFile,*) '#Wn Wn Covariance (R,I) Correlation (R,I)'
     write(iFile,*) '#  Wn in cm-1'
     write(iFile,*) '#  Covariance in [(W/(m2.strd.cm-1))**2]'
     write(iFile,*) '#  Correlation in [-1:1]'
     if( trim(Matrix_Noise%Type) == 'RI' ) then
       if(  trim(Matrix_Noise%Matrix_Type) == 'CovarCorrel' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   Matrix_Noise%Covariance_R(index)*10000_DOUBLE,&
                   Matrix_Noise%Covariance_I(index)*10000_DOUBLE,&
                   Matrix_Noise%Correlation_R(index),&
                   Matrix_Noise%Correlation_I(index)
           end do
           write(iFile,*) 
         end do
       else if(  trim(Matrix_Noise%Matrix_Type) == 'Covariance' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   Matrix_Noise%Covariance_R(index)*10000_DOUBLE,&
                   Matrix_Noise%Covariance_I(index)*10000_DOUBLE,&
                   0_DOUBLE,&
                   0_DOUBLE
           end do
           write(iFile,*) 
         end do
       else if(  trim(Matrix_Noise%Matrix_Type) == 'Correlation' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   0_DOUBLE,                         &
                   0_DOUBLE,                         &
                   Matrix_Noise%Correlation_R(index),&
                   Matrix_Noise%Correlation_I(index)
           end do
           write(iFile,*) 
         end do
       end if
     else if( trim(Matrix_Noise%Type) == 'R' ) then
       if(  trim(Matrix_Noise%Matrix_Type) == 'CovarCorrel' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   Matrix_Noise%Covariance_R(index)*10000_DOUBLE,&
                   0_DOUBLE,&
                   Matrix_Noise%Correlation_R(index),&
                   0_DOUBLE
           end do
           write(iFile,*) 
         end do
       else if(  trim(Matrix_Noise%Matrix_Type) == 'Covariance' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   Matrix_Noise%Covariance_R(index)*10000_DOUBLE,&
                   0_DOUBLE,&
                   0_DOUBLE,&
                   0_DOUBLE
           end do
           write(iFile,*) 
         end do
       else if(  trim(Matrix_Noise%Matrix_Type) == 'Correlation' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   0_DOUBLE,                         &
                   0_DOUBLE,                         &
                   Matrix_Noise%Correlation_R(index),&
                   0_DOUBLE
           end do
           write(iFile,*) 
         end do
       end if
     else if( trim(Matrix_Noise%Type) == 'I' ) then
       if(  trim(Matrix_Noise%Matrix_Type) == 'CovarCorrel' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   0_DOUBLE,&
                   Matrix_Noise%Covariance_I(index)*10000_DOUBLE,&
                   0_DOUBLE,&
                   Matrix_Noise%Correlation_I(index)
           end do
           write(iFile,*) 
         end do
       else if(  trim(Matrix_Noise%Matrix_Type) == 'Covariance' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   0_DOUBLE,&
                   Matrix_Noise%Covariance_I(index)*10000_DOUBLE,&
                   0_DOUBLE,&
                   0_DOUBLE
           end do
           write(iFile,*) 
         end do
       else if(  trim(Matrix_Noise%Matrix_Type) == 'Correlation' ) then
         do Ns = 1, Matrix_Noise%N_Sample
           do Nss = 1, Matrix_Noise%N_Sample
             if ( Nss <= Ns ) then
                index = (Nss-1)*Nss/2+Ns
             else
                index = (Ns-1)*Ns/2+Nss
             end if
             write(iFile,'(2f10.2,4e20.10)')         &
                   Matrix_Noise%Wn(Ns)/100_DOUBLE,   &
                   Matrix_Noise%Wn(Nss)/100_DOUBLE,  &
                   0_DOUBLE,                         &
                   0_DOUBLE,                         &
                   0_DOUBLE,&
                   Matrix_Noise%Correlation_I(index)
           end do
           write(iFile,*) 
         end do
       end if
     end if
     close(iFile)
     return
 999 write(0,*) 'plot_noise_matrix write Error : '&
                ,trim(Matrix_Noise%filename)
     call exit(1)
   end subroutine plot_noise_matrix
!
!
   subroutine plot_noise_diagonal( Diagonal_Noise )
   implicit none
   type(type_Diagonal_Noise)                                :: Diagonal_Noise
   integer(kind=LONG)                                       :: Ns
   integer(kind=LONG)                                       :: iFile
   real(kind=DOUBLE)                                        :: Zero
!
     Zero = 0_DOUBLE
     iFile = 10
     open(iFile,file=trim(Diagonal_Noise%filename),err=999)
     if( trim(Diagonal_Noise%Type) == 'RI' ) then
        do Ns = 1, Diagonal_Noise%N_Sample
           write(iFile,9999,err=999)                                   &
                          Diagonal_Noise%Wn(Ns)/100. ,                 &
                          Diagonal_Noise%Spectrum_Avg_R(Ns)*100.,      &
                          Diagonal_Noise%NoiseNedl_R(Ns)*100.,         &
                          Diagonal_Noise%NoiseNedt_R(Ns),              &
                          Diagonal_Noise%DriftT_R(Ns),                 &
                          Diagonal_Noise%SmoothSpectrum_Avg_R(Ns)*100.,&
                          Diagonal_Noise%SmoothNoiseNedl_R(Ns)*100.,   &
                          Diagonal_Noise%SmoothNoiseNedt_R(Ns),        &
                          Diagonal_Noise%SmoothDriftT_R(Ns),           &
                          Diagonal_Noise%Spectrum_Avg_I(Ns)*100.,      &
                          Diagonal_Noise%NoiseNedl_I(Ns)*100.,         &
                          Diagonal_Noise%NoiseNedt_I(Ns),              &
                          Diagonal_Noise%DriftT_I(Ns),                 &
                          Diagonal_Noise%SmoothSpectrum_Avg_I(Ns)*100.,&
                          Diagonal_Noise%SmoothNoiseNedl_I(Ns)*100.,   &
                          Diagonal_Noise%SmoothNoiseNedt_I(Ns),        &
                          Diagonal_Noise%SmoothDriftT_I(Ns)
        end do
     else if( trim(Diagonal_Noise%Type) == 'R'  ) then
     write(iFile,fmt='(a)',err=999) &
      '#Wn; Real (S_Avg; Nedl; Nedt; Drift; Smooth(S_Avg; Nedl; Nedt; Drift;))'
        do Ns = 1, Diagonal_Noise%N_Sample
           write(iFile,9999,err=999)                                   &
                          Diagonal_Noise%Wn(Ns)/100. ,                 &
                          Diagonal_Noise%Spectrum_Avg_R(Ns)*100.,      &
                          Diagonal_Noise%NoiseNedl_R(Ns)*100.,         &
                          Diagonal_Noise%NoiseNedt_R(Ns),              &
                          Diagonal_Noise%DriftT_R(Ns),                 &
                          Diagonal_Noise%SmoothSpectrum_Avg_R(Ns)*100.,&
                          Diagonal_Noise%SmoothNoiseNedl_R(Ns)*100.,   &
                          Diagonal_Noise%SmoothNoiseNedt_R(Ns),        &
                          Diagonal_Noise%SmoothDriftT_R(Ns)
        end do
     else if( trim(Diagonal_Noise%Type) == 'I'  ) then
     write(iFile,fmt='(a)',err=999) &
      '#Wn; Imag (S_Avg; Nedl; Nedt; Drift; Smooth(S_Avg; Nedl; Nedt; Drift;))'
        do Ns = 1, Diagonal_Noise%N_Sample
           write(iFile,9999,err=999)                                   &
                          Diagonal_Noise%Wn(Ns)/100. ,                 &
                          Diagonal_Noise%Spectrum_Avg_I(Ns)*100.,      &
                          Diagonal_Noise%NoiseNedl_I(Ns)*100.,         &
                          Diagonal_Noise%NoiseNedt_I(Ns),              &
                          Diagonal_Noise%DriftT_I(Ns),                 &
                          Diagonal_Noise%SmoothSpectrum_Avg_I(Ns)*100.,&
                          Diagonal_Noise%SmoothNoiseNedl_I(Ns)*100.,   &
                          Diagonal_Noise%SmoothNoiseNedt_I(Ns),        &
                          Diagonal_Noise%SmoothDriftT_I(Ns)
        end do
     end if
     close(iFile)
     return
 999 write(0,*) 'plot_noise_diagonal write Error : '&
                ,trim(Diagonal_Noise%filename)
     call exit(1)
9999 format (1h ,30e21.12)
   end subroutine plot_noise_diagonal
!
!
   subroutine plot_radiometry_error( Spectrum_Error )
   implicit none
   type(type_Radiometry_Error) ,intent(in)                  :: Spectrum_Error
   integer(kind=LONG)                                       :: Ns
   integer(kind=LONG)                                       :: iFile
   real(kind=DOUBLE)                                        :: Zero
!
     Zero = 0_DOUBLE
     iFile = 10
     open(iFile,file=Spectrum_Error%filename,err=999)
     write(iFile,fmt='(a)',err=999) &
      '#Wn; S_Real; S_Imag;S0_Real; S0_Imag; Nedt_R; Nedt_I; TB-TB0; TB0; TB; Nedl_R; Nedl_I '     
     if( trim(Spectrum_Error%Type) == 'RI' ) then
        do Ns = 1, Spectrum_Error%N_Sample
           write(iFile,9999,err=999)                     &
                          Spectrum_Error%Wn(Ns)/100. ,   &
                          Spectrum_Error%S_R(Ns)*100.,   &
                          Spectrum_Error%S_I(Ns)*100.,   &
                          Spectrum_Error%S0_R(Ns)*100.,  &
                          Spectrum_Error%S0_I(Ns)*100.,  &
                          Spectrum_Error%Nedt_R(Ns),     &
                          Spectrum_Error%Nedt_I(Ns),     &
                          Spectrum_Error%TB(Ns)-         &
                          Spectrum_Error%TB0(Ns),        &
                          Spectrum_Error%TB0(Ns),        &
                          Spectrum_Error%TB(Ns),         &
                          Spectrum_Error%Nedl_R(Ns)*100.,&
                          Spectrum_Error%Nedl_I(Ns)*100.
        end do
     else if( trim(Spectrum_Error%Type) == 'R' ) then
        do Ns = 1, Spectrum_Error%N_Sample
           write(iFile,9999,err=999)                     &
                          Spectrum_Error%Wn(Ns)/100. ,   &
                          Spectrum_Error%S_R(Ns)*100.,   &
                          Zero,                          &
                          Spectrum_Error%S0_R(Ns)*100.,  &
                          Zero,                          &
                          Spectrum_Error%Nedt_R(Ns),     &
                          Zero,                          &
                          Spectrum_Error%TB(Ns)-         &
                          Spectrum_Error%TB0(Ns),        &
                          Spectrum_Error%TB0(Ns),        &
                          Spectrum_Error%TB(Ns),         &
                          Spectrum_Error%Nedl_R(Ns)*100.,&
                          Zero
        end do
     else if( trim(Spectrum_Error%Type) == 'I' ) then
        do Ns = 1, Spectrum_Error%N_Sample
           write(iFile,9999,err=999)                     &
                          Spectrum_Error%Wn(Ns)/100. ,   &
                          Zero,                          &
                          Spectrum_Error%S_I(Ns)*100.,   &
                          Zero,                          &
                          Spectrum_Error%S0_I(Ns)*100.,  &
                          Zero,                          &
                          Spectrum_Error%Nedt_I(Ns),     &
                          Spectrum_Error%TB(Ns)-         &
                          Spectrum_Error%TB0(Ns),        &
                          Spectrum_Error%TB0(Ns),        &
                          Spectrum_Error%TB(Ns),         &
                          Zero,                          &
                          Spectrum_Error%Nedl_I(Ns)*100.
        end do
     end if
     if( trim(Spectrum_Error%Type) == 'RI' .or. &
         trim(Spectrum_Error%Type) == 'R'  ) then
        write(iFile,'(a16,4e12.4,2i6)',err=999) '# Nedt Real Part' &
            ,Spectrum_Error%Nedt_R_Avg,Spectrum_Error%Nedt_R_Std   &
            ,Spectrum_Error%Nedt_R_Min,Spectrum_Error%Nedt_R_Max   &
            ,Spectrum_Error%Nedt_R_NsMin,Spectrum_Error%Nedt_R_NsMax
        write(iFile,'(a16,4e12.4,2i6)',err=999) '# Nedl Real Part' &
            ,Spectrum_Error%Nedl_R_Avg*100.,Spectrum_Error%Nedl_R_Std*100.&
            ,Spectrum_Error%Nedl_R_Min*100.,Spectrum_Error%Nedl_R_Max*100.&
            ,Spectrum_Error%Nedl_R_NsMin,Spectrum_Error%Nedl_R_NsMax
     end if
     if( trim(Spectrum_Error%Type) == 'RI' .or. &
         trim(Spectrum_Error%Type) == 'I'  ) then
     write(iFile,'(a16,4e12.4,2i6)',err=999) '# Nedt Imag Part' &
         ,Spectrum_Error%Nedt_I_Avg,Spectrum_Error%Nedt_I_Std   &
         ,Spectrum_Error%Nedt_I_Min,Spectrum_Error%Nedt_I_Max   &
         ,Spectrum_Error%Nedt_I_NsMin,Spectrum_Error%Nedt_I_NsMax
     write(iFile,'(a16,4e12.4,2i6)',err=999) '# Nedl Imag Part' &
         ,Spectrum_Error%Nedl_I_Avg*100.,Spectrum_Error%Nedl_I_Std*100.&
         ,Spectrum_Error%Nedl_I_Min*100.,Spectrum_Error%Nedl_I_Max*100.&
         ,Spectrum_Error%Nedl_I_NsMin,Spectrum_Error%Nedl_I_NsMax
     end if
     if( trim(Spectrum_Error%Type) == 'RI' .or. &
         trim(Spectrum_Error%Type) == 'R'  ) then
        do Ns = 1, Spectrum_Error%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Nedt Real Part', &
                               (Spectrum_Error%Wn_Class(Ns)             &
                               +Spectrum_Error%Wn_Class(Ns+1))/200.d+00,&
                                Spectrum_Error%Nedt_R_Class_Avg(Ns),    &
                                Spectrum_Error%Nedt_R_Class_Std(Ns),    &
                                Spectrum_Error%Nedt_R_Class_Min(Ns),    &
                                Spectrum_Error%Nedt_R_Class_Max(Ns),    &
                                Spectrum_Error%Nedt_R_Class_NsMin(Ns),  &
                                Spectrum_Error%Nedt_R_Class_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Error%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Nedl Real Part', &
                               (Spectrum_Error%Wn_Class(Ns)              &
                               +Spectrum_Error%Wn_Class(Ns+1))/200.d+00, &
                                Spectrum_Error%Nedl_R_Class_Avg(Ns)*100.,&
                                Spectrum_Error%Nedl_R_Class_Std(Ns)*100.,&
                                Spectrum_Error%Nedl_R_Class_Min(Ns)*100.,&
                                Spectrum_Error%Nedl_R_Class_Max(Ns)*100.,&
                                Spectrum_Error%Nedl_R_Class_NsMin(Ns),   &
                                Spectrum_Error%Nedl_R_Class_NsMax(Ns)
        end do
     end if
     if( trim(Spectrum_Error%Type) == 'RI' .or. &
         trim(Spectrum_Error%Type) == 'I'  ) then
        do Ns = 1, Spectrum_Error%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Nedt Real Part', &
                               (Spectrum_Error%Wn_Class(Ns)             &
                               +Spectrum_Error%Wn_Class(Ns+1))/200.d+00,&
                                Spectrum_Error%Nedt_I_Class_Avg(Ns),    &
                                Spectrum_Error%Nedt_I_Class_Std(Ns),    &
                                Spectrum_Error%Nedt_I_Class_Min(Ns),    &
                                Spectrum_Error%Nedt_I_Class_Max(Ns),    &
                                Spectrum_Error%Nedt_I_Class_NsMin(Ns),  &
                                Spectrum_Error%Nedt_I_Class_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Error%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Nedl Real Part', &
                               (Spectrum_Error%Wn_Class(Ns)              &
                               +Spectrum_Error%Wn_Class(Ns+1))/200.d+00, &
                                Spectrum_Error%Nedl_I_Class_Avg(Ns)*100.,&
                                Spectrum_Error%Nedl_I_Class_Std(Ns)*100.,&
                                Spectrum_Error%Nedl_I_Class_Min(Ns)*100.,&
                                Spectrum_Error%Nedl_I_Class_Max(Ns)*100.,&
                                Spectrum_Error%Nedl_I_Class_NsMin(Ns),   &
                                Spectrum_Error%Nedl_I_Class_NsMax(Ns)
        end do
     end if
     close(iFile)
     return
 999 write(0,*) 'plot_radiometry write Error : '&
                ,trim(Spectrum_Error%filename)
     call exit(1)
9999 format (1h ,30e21.12)
   end subroutine plot_radiometry_error
!
!
   subroutine plot_radiometry_stats( Spectrum_Stats )
   implicit none
   type(type_Radiometry_Stats) ,intent(in)                  :: Spectrum_Stats
   integer(kind=LONG)                                       :: Ns
   integer(kind=LONG)                                       :: iFile
   real(kind=DOUBLE)                                        :: Zero
!
     Zero = 0_DOUBLE
     iFile = 10
     open(iFile,file=Spectrum_Stats%filename,err=999)
     write(iFile,fmt='(a)',err=999) &
      '#Wn; Nedl (Real Imag) Nedt (Real Imag) (Avg Std Min Max NsMin NsMax) '     
     if( trim(Spectrum_Stats%Type) == 'RI' ) then
        do Ns = 1, Spectrum_Stats%N_Sample
           write(iFile,'(f10.3,4(4e12.4,2i6))',err=999)      &
                          Spectrum_Stats%Wn(Ns)/100. ,       &
                          Spectrum_Stats%Nedl_R_Avg(Ns)*100.,&
                          Spectrum_Stats%Nedl_R_Std(Ns)*100.,&
                          Spectrum_Stats%Nedl_R_Min(Ns)*100.,&
                          Spectrum_Stats%Nedl_R_Max(Ns)*100.,&
                          Spectrum_Stats%Nedl_R_NsMin(Ns),   &
                          Spectrum_Stats%Nedl_R_NsMax(Ns),   &
                          Spectrum_Stats%Nedl_I_Avg(Ns)*100.,&
                          Spectrum_Stats%Nedl_I_Std(Ns)*100.,&
                          Spectrum_Stats%Nedl_I_Min(Ns)*100.,&
                          Spectrum_Stats%Nedl_I_Max(Ns)*100.,&
                          Spectrum_Stats%Nedl_I_NsMin(Ns),   &
                          Spectrum_Stats%Nedl_I_NsMax(Ns),   &
                          Spectrum_Stats%Nedt_R_Avg(Ns),&
                          Spectrum_Stats%Nedt_R_Std(Ns),&
                          Spectrum_Stats%Nedt_R_Min(Ns),&
                          Spectrum_Stats%Nedt_R_Max(Ns),&
                          Spectrum_Stats%Nedt_R_NsMin(Ns),&
                          Spectrum_Stats%Nedt_R_NsMax(Ns),&
                          Spectrum_Stats%Nedt_I_Avg(Ns),&
                          Spectrum_Stats%Nedt_I_Std(Ns),&
                          Spectrum_Stats%Nedt_I_Min(Ns),&
                          Spectrum_Stats%Nedt_I_Max(Ns),&
                          Spectrum_Stats%Nedt_I_NsMin(Ns),&
                          Spectrum_Stats%Nedt_I_NsMax(Ns)
        end do
     else if( trim(Spectrum_Stats%Type) == 'R' ) then
        do Ns = 1, Spectrum_Stats%N_Sample
           write(iFile,'(f10.3,4(4e12.4,2i6))',err=999)      &
                          Spectrum_Stats%Wn(Ns)/100. ,       &
                          Spectrum_Stats%Nedl_R_Avg(Ns)*100.,&
                          Spectrum_Stats%Nedl_R_Std(Ns)*100.,&
                          Spectrum_Stats%Nedl_R_Min(Ns)*100.,&
                          Spectrum_Stats%Nedl_R_Max(Ns)*100.,&
                          Spectrum_Stats%Nedl_R_NsMin(Ns),   &
                          Spectrum_Stats%Nedl_R_NsMax(Ns),   &
                          Zero,                              &
                          Zero,                              &
                          Zero,                              &
                          Zero,                              &
                          0,                                 &
                          0,                                 &
                          Spectrum_Stats%Nedt_R_Avg(Ns),&
                          Spectrum_Stats%Nedt_R_Std(Ns),&
                          Spectrum_Stats%Nedt_R_Min(Ns),&
                          Spectrum_Stats%Nedt_R_Max(Ns),&
                          Spectrum_Stats%Nedt_R_NsMin(Ns),&
                          Spectrum_Stats%Nedt_R_NsMax(Ns),&
                          Zero,                              &
                          Zero,                              &
                          Zero,                              &
                          Zero,                              &
                          0,                                 &
                          0
        end do
     else if( trim(Spectrum_Stats%Type) == 'I' ) then
        do Ns = 1, Spectrum_Stats%N_Sample
           write(iFile,'(f10.3,4(4e12.4,2i6))',err=999)      &
                          Spectrum_Stats%Wn(Ns)/100. ,       &
                          Zero,                              &
                          Zero,                              &
                          Zero,                              &
                          Zero,                              &
                          0,                                 &
                          0,                                 &
                          Spectrum_Stats%Nedl_I_Avg(Ns)*100.,&
                          Spectrum_Stats%Nedl_I_Std(Ns)*100.,&
                          Spectrum_Stats%Nedl_I_Min(Ns)*100.,&
                          Spectrum_Stats%Nedl_I_Max(Ns)*100.,&
                          Spectrum_Stats%Nedl_I_NsMin(Ns),   &
                          Spectrum_Stats%Nedl_I_NsMax(Ns),   &
                          Zero,                              &
                          Zero,                              &
                          Zero,                              &
                          Zero,                              &
                          0,                                 &
                          0,                                 &
                          Spectrum_Stats%Nedt_I_Avg(Ns),&
                          Spectrum_Stats%Nedt_I_Std(Ns),&
                          Spectrum_Stats%Nedt_I_Min(Ns),&
                          Spectrum_Stats%Nedt_I_Max(Ns),&
                          Spectrum_Stats%Nedt_I_NsMin(Ns),&
                          Spectrum_Stats%Nedt_I_NsMax(Ns)
        end do
     end if
     if( trim(Spectrum_Stats%Type) == 'RI' .or. &
         trim(Spectrum_Stats%Type) == 'R'  ) then
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Avg Nedt Real Part', &
                            (Spectrum_Stats%Wn_Class(Ns)             &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,&
                             Spectrum_Stats%Nedt_R_Class_Avg_Avg(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Avg_Std(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Avg_Min(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Avg_Max(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Avg_NsMin(Ns),  &
                             Spectrum_Stats%Nedt_R_Class_Avg_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Std Nedt Real Part', &
                            (Spectrum_Stats%Wn_Class(Ns)             &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,&
                             Spectrum_Stats%Nedt_R_Class_Std_Avg(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Std_Std(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Std_Min(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Std_Max(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Std_NsMin(Ns),  &
                             Spectrum_Stats%Nedt_R_Class_Std_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Min Nedt Real Part', &
                            (Spectrum_Stats%Wn_Class(Ns)             &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,&
                             Spectrum_Stats%Nedt_R_Class_Min_Avg(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Min_Std(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Min_Min(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Min_Max(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Min_NsMin(Ns),  &
                             Spectrum_Stats%Nedt_R_Class_Min_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Max Nedt Real Part', &
                            (Spectrum_Stats%Wn_Class(Ns)             &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,&
                             Spectrum_Stats%Nedt_R_Class_Max_Avg(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Max_Std(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Max_Min(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Max_Max(Ns),    &
                             Spectrum_Stats%Nedt_R_Class_Max_NsMin(Ns),  &
                             Spectrum_Stats%Nedt_R_Class_Max_NsMax(Ns)
        end do
     end if
     if( trim(Spectrum_Stats%Type) == 'RI' .or. &
         trim(Spectrum_Stats%Type) == 'I'  ) then
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Avg Nedt Imag Part', &
                            (Spectrum_Stats%Wn_Class(Ns)             &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,&
                             Spectrum_Stats%Nedt_I_Class_Avg_Avg(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Avg_Std(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Avg_Min(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Avg_Max(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Avg_NsMin(Ns),  &
                             Spectrum_Stats%Nedt_I_Class_Avg_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Std Nedt Imag Part', &
                            (Spectrum_Stats%Wn_Class(Ns)             &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,&
                             Spectrum_Stats%Nedt_I_Class_Std_Avg(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Std_Std(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Std_Min(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Std_Max(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Std_NsMin(Ns),  &
                             Spectrum_Stats%Nedt_I_Class_Std_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Min Nedt Imag Part', &
                            (Spectrum_Stats%Wn_Class(Ns)             &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,&
                             Spectrum_Stats%Nedt_I_Class_Min_Avg(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Min_Std(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Min_Min(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Min_Max(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Min_NsMin(Ns),  &
                             Spectrum_Stats%Nedt_I_Class_Min_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Max Nedt Imag Part', &
                            (Spectrum_Stats%Wn_Class(Ns)             &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,&
                             Spectrum_Stats%Nedt_I_Class_Max_Avg(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Max_Std(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Max_Min(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Max_Max(Ns),    &
                             Spectrum_Stats%Nedt_I_Class_Max_NsMin(Ns),  &
                             Spectrum_Stats%Nedt_I_Class_Max_NsMax(Ns)
        end do
     end if
     if( trim(Spectrum_Stats%Type) == 'RI' .or. &
         trim(Spectrum_Stats%Type) == 'R'  ) then
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Avg Nedl Real Part', &
                            (Spectrum_Stats%Wn_Class(Ns)                      &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,         &
                             Spectrum_Stats%Nedl_R_Class_Avg_Avg(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Avg_Std(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Avg_Min(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Avg_Max(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Avg_NsMin(Ns),       &
                             Spectrum_Stats%Nedl_R_Class_Avg_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Std Nedl Real Part', &
                            (Spectrum_Stats%Wn_Class(Ns)                      &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,         &
                             Spectrum_Stats%Nedl_R_Class_Std_Avg(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Std_Std(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Std_Min(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Std_Max(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Std_NsMin(Ns),       &
                             Spectrum_Stats%Nedl_R_Class_Std_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Min Nedl Real Part', &
                            (Spectrum_Stats%Wn_Class(Ns)                      &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,         &
                             Spectrum_Stats%Nedl_R_Class_Min_Avg(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Min_Std(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Min_Min(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Min_Max(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Min_NsMin(Ns),       &
                             Spectrum_Stats%Nedl_R_Class_Min_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Max Nedl Real Part', &
                            (Spectrum_Stats%Wn_Class(Ns)                      &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,         &
                             Spectrum_Stats%Nedl_R_Class_Max_Avg(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Max_Std(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Max_Min(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Max_Max(Ns)*100.,    &
                             Spectrum_Stats%Nedl_R_Class_Max_NsMin(Ns),       &
                             Spectrum_Stats%Nedl_R_Class_Max_NsMax(Ns)
        end do
     end if
     if( trim(Spectrum_Stats%Type) == 'RI' .or. &
         trim(Spectrum_Stats%Type) == 'I'  ) then
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Avg Nedl Imag Part', &
                            (Spectrum_Stats%Wn_Class(Ns)                      &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,         &
                             Spectrum_Stats%Nedl_I_Class_Avg_Avg(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Avg_Std(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Avg_Min(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Avg_Max(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Avg_NsMin(Ns),       &
                             Spectrum_Stats%Nedl_I_Class_Avg_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Std Nedl Imag Part', &
                            (Spectrum_Stats%Wn_Class(Ns)                      &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,         &
                             Spectrum_Stats%Nedl_I_Class_Std_Avg(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Std_Std(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Std_Min(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Std_Max(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Std_NsMin(Ns),       &
                             Spectrum_Stats%Nedl_I_Class_Std_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Min Nedl Imag Part', &
                            (Spectrum_Stats%Wn_Class(Ns)                      &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,         &
                             Spectrum_Stats%Nedl_I_Class_Min_Avg(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Min_Std(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Min_Min(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Min_Max(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Min_NsMin(Ns),       &
                             Spectrum_Stats%Nedl_I_Class_Min_NsMax(Ns)
        end do
        do Ns = 1, Spectrum_Stats%Nb_Wn_Class
           write(iFile,'(a16,f10.3,4e12.4,2i6)',err=999) '# Max Nedl Imag Part', &
                            (Spectrum_Stats%Wn_Class(Ns)                      &
                            +Spectrum_Stats%Wn_Class(Ns+1))/200.d+00,         &
                             Spectrum_Stats%Nedl_I_Class_Max_Avg(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Max_Std(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Max_Min(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Max_Max(Ns)*100.,    &
                             Spectrum_Stats%Nedl_I_Class_Max_NsMin(Ns),       &
                             Spectrum_Stats%Nedl_I_Class_Max_NsMax(Ns)
        end do
     end if
     close(iFile)
     return
 999 write(0,*) 'plot_radiometry_stats write Error : '&
                ,trim(Spectrum_Stats%filename)
     call exit(1)
   end subroutine plot_radiometry_stats
!
!
   subroutine plot_ils_perfo_stats( Ils_Perfo_Stats )
   implicit none
   type(type_Ils_Perfo_Stats)                           :: Ils_Perfo_Stats
   integer(kind=LONG)                                   :: iFile
   integer(kind=LONG)                                   :: Ns
!
     iFile = 10
     open(iFile,file=Ils_Perfo_Stats%filename,err=999)
     write(iFile,'(a)',err=999) '# WnIls Shift ErShift FWhm ErFWhm Shape_Index_R Shape_Index_I'
     write(iFile,'(a,7e16.8)',err=999) '#   Avg',   &
                 Ils_Perfo_Stats%WnIls/100.,        &
                 Ils_Perfo_Stats%Shift_Avg/100.,    &
                 Ils_Perfo_Stats%ErShift_Avg,       &
                 Ils_Perfo_Stats%FWhm_Avg/100.,     &
                 Ils_Perfo_Stats%ErFWhm_Avg,        &
                 Ils_Perfo_Stats%Shape_Index_R_Avg, &
                 Ils_Perfo_Stats%Shape_Index_I_Avg
     write(iFile,'(a,7e16.8)',err=999) '#   Std',   &
                 Ils_Perfo_Stats%WnIls/100.,        &
                 Ils_Perfo_Stats%Shift_Std/100.,    &
                 Ils_Perfo_Stats%ErShift_Std,       &
                 Ils_Perfo_Stats%FWhm_Std/100.,     &
                 Ils_Perfo_Stats%ErFWhm_Std,        &
                 Ils_Perfo_Stats%Shape_Index_R_Std, &
                 Ils_Perfo_Stats%Shape_Index_I_Std
     write(iFile,'(a,7e16.8)',err=999) '#   Min',   &
                 Ils_Perfo_Stats%WnIls/100.,        &
                 Ils_Perfo_Stats%Shift_Min/100.,    &
                 Ils_Perfo_Stats%ErShift_Min,       &
                 Ils_Perfo_Stats%FWhm_Min/100.,     &
                 Ils_Perfo_Stats%ErFWhm_Min,        &
                 Ils_Perfo_Stats%Shape_Index_R_Min, &
                 Ils_Perfo_Stats%Shape_Index_I_Min
     write(iFile,'(a,7e16.8)',err=999) '#   Max',   &
                 Ils_Perfo_Stats%WnIls/100.,        &
                 Ils_Perfo_Stats%Shift_Max/100.,    &
                 Ils_Perfo_Stats%ErShift_Max,       &
                 Ils_Perfo_Stats%FWhm_Max/100.,     &
                 Ils_Perfo_Stats%ErFWhm_Max,        &
                 Ils_Perfo_Stats%Shape_Index_R_Max, &
                 Ils_Perfo_Stats%Shape_Index_I_Max
     write(iFile,'(a,e16.8,6i16)',err=999) '# NsMin', &
                 Ils_Perfo_Stats%WnIls/100.,          &
                 Ils_Perfo_Stats%Shift_NsMin,         &
                 Ils_Perfo_Stats%ErShift_NsMin,       &
                 Ils_Perfo_Stats%FWhm_NsMin,          &
                 Ils_Perfo_Stats%ErFWhm_NsMin,        &
                 Ils_Perfo_Stats%Shape_Index_R_NsMin,&
                 Ils_Perfo_Stats%Shape_Index_I_NsMin
     write(iFile,'(a,e16.8,6i16)',err=999) '# NsMax', &
                 Ils_Perfo_Stats%WnIls/100.,          &
                 Ils_Perfo_Stats%Shift_NsMax,         &
                 Ils_Perfo_Stats%ErShift_NsMax,       &
                 Ils_Perfo_Stats%FWhm_NsMax,          &
                 Ils_Perfo_Stats%ErFWhm_NsMax,        &
                 Ils_Perfo_Stats%Shape_Index_R_NsMax, &
                 Ils_Perfo_Stats%Shape_Index_I_NsMax
!
!    plot Ils Shape and Shape Error
     write(iFile,'(a)',err=999) '# WnIls Shape RI ErShape RI (Avg,Std,Min,Max) '
     do Ns = 1, Ils_Perfo_Stats%Ns_Ils
        write(iFile,'(17e16.8)',err=999)           &
                   Ils_Perfo_Stats%Wn(Ns)/100.,    &
                   Ils_Perfo_Stats%Shape_R_Avg(Ns),&
                   Ils_Perfo_Stats%Shape_R_Std(Ns),&
                   Ils_Perfo_Stats%Shape_R_Min(Ns),&
                   Ils_Perfo_Stats%Shape_R_Max(Ns),&
                   Ils_Perfo_Stats%Shape_I_Avg(Ns),&
                   Ils_Perfo_Stats%Shape_I_Std(Ns),&
                   Ils_Perfo_Stats%Shape_I_Min(Ns),&
                   Ils_Perfo_Stats%Shape_I_Max(Ns),&
                   Ils_Perfo_Stats%ShapeDiff_R_Avg(Ns),&
                   Ils_Perfo_Stats%ShapeDiff_R_Std(Ns),&
                   Ils_Perfo_Stats%ShapeDiff_R_Min(Ns),&
                   Ils_Perfo_Stats%ShapeDiff_R_Max(Ns),&
                   Ils_Perfo_Stats%ShapeDiff_I_Avg(Ns),&
                   Ils_Perfo_Stats%ShapeDiff_I_Std(Ns),&
                   Ils_Perfo_Stats%ShapeDiff_I_Min(Ns),&
                   Ils_Perfo_Stats%ShapeDiff_I_Max(Ns)
     end do
     close(iFile)
     return
 999 write(0,*) 'plot_ils_perfo_stats write Error : '&
                ,trim(Ils_Perfo_Stats%filename)
     call exit(1)
   end subroutine plot_ils_perfo_stats
!
!
   subroutine plot_srf_perfo( Srf_Perfo )
   type(type_Srf_Perfo)                                 :: Srf_Perfo
   integer(kind=LONG)                                   :: iFile
!!   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: NF
   integer(kind=LONG)                                   :: PN
!
     iFile = 10
     open(iFile,file=trim(Srf_Perfo%filename), err=999)
     write(iFile,'(a)',err=999) '# Wn ErShift ErFWhm ErShape'
     PN = Srf_Perfo%NsPixel+1
     do NF = 1, Srf_Perfo%NsWn0
        write(iFile,'(f10.2,9e20.10)',err=999) &
                  Srf_Perfo%Wn0(NF)/100_DOUBLE,&
                  Srf_Perfo%ErShift1a(NF,PN),  &
                  Srf_Perfo%ErFWhm1a(NF,PN),   &
                  Srf_Perfo%ShapeId1a(NF,PN),  &
                  Srf_Perfo%ErShift1b(NF,PN),  &
                  Srf_Perfo%ErFWhm1b(NF,PN),   &
                  Srf_Perfo%ShapeId1b(NF,PN),  &
                  Srf_Perfo%ErShift1c(NF,PN),  &
                  Srf_Perfo%ErFWhm1c(NF,PN),   &
                  Srf_Perfo%ShapeId1c(NF,PN)
     end do
     close(iFile)
     return
 999 write(0,*) 'plot_srf_perfo write Error : '&
                ,trim(Srf_Perfo%filename)
     call exit(1)
   end subroutine plot_srf_perfo
!
!
!
!
   subroutine plot_moments( File_Plot_, &
                            Spe_Moments     )
!
   implicit none
   character(len=*)      ,intent(in)                    :: File_Plot_
   type(type_Srf_Moments),intent(in)                    :: Spe_Moments
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: iFile
   integer(kind=LONG)                                   :: Nth
   character(len=2)                                     :: N_Der
   character(len=500)                                   :: File_Plot
!
     iFile = 10
     do Nth = 0, Spe_Moments%N_Moments
        write(N_Der,'(i2.2)') Nth
        File_Plot = File_Plot_(1:len_trim(File_Plot_)) &
                  //'Moments'//N_Der//'.plt'
        open(iFile,file=File_Plot,err=999)
        write(iFile,'(a)') '#Ns; Wn; Moments; Spe_Derive; (Real Imaginary)'
        do Ns = 1, Spe_Moments%NsWn0
           write(iFile,'(i10,5e20.10)') Ns,Spe_Moments%Wn0(Ns)/100., &
                                        Spe_Moments%Moments(Ns,Nth), &
                                        Spe_Moments%Derive0(Ns,Nth)
        end do
        close(iFile)
     end do
     return
 999 write(0,*) 'plot_moments write Error : '&
                ,File_Plot(1:len_trim(File_Plot))
     call exit(1)
   end subroutine plot_moments
!
!
   subroutine plot_apf( Files_Results_,&
                        SB, Apf )
!
   implicit none
   character(len=500)  ,intent(in)                      :: Files_Results_
   integer(kind=LONG)  ,intent(in)                      :: SB
   type(type_Apf)      ,intent(in)                      :: Apf
   character(len=500)                                   :: File_Plot
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: PN
   integer(kind=LONG)                                   :: NF
   integer(kind=LONG)                                   :: iFile
   character(len=1)                                     :: Band
!
     iFile = 10
!
!    Apodisation Functions
     write(Band,'(i1.1)') SB
     File_Plot = Files_Results_(1:len_trim(Files_Results_))&
                   //'SB'//Band//'.asc'
     open(unit=iFile, file=File_Plot, status='unknown', err=999)
     write(iFile,'(a,i4,a)') '#',Apf%NsWn0,' Wave Numbers'
     write(iFile,'(a,30e21.14)') '#',(Apf%Wn0(NF),NF=1,Apf%NsWn0)
     write(iFile,'(a)') '# Opd; N x (Apf_Mod; Apf_Arg;)'
     if ( Apf%NsPixel == 1 ) then
        PN = Apf%NsPixel
     else
        PN = Apf%NsPixel+1 ! pixels
     end if
     do Ns = 1, Apf%NsOpd
        write(iFile,'(f12.9,30e21.14)')    &
                        Apf%Opd(Ns),       &
                       (Apf%Mod(Ns,NF,PN), &
                        Apf%Arg(Ns,NF,PN), &
                        NF=1,Apf%NsWn0 )
     end do  ! fin de boucle sur les opd
     close(unit=iFile)
     return
 999 write(0,'(a)') 'plot_apf write Error : '&
                ,File_Plot(1:len_trim(File_Plot))
     call exit(1)
   end subroutine plot_apf
!
!
   subroutine plot_saf( Files_Results_,&
                        PN, SB, Saf )
!
   implicit none
   character(len=500)  ,intent(in)                      :: Files_Results_
   integer(kind=LONG)  ,intent(in)                      :: PN
   integer(kind=LONG)  ,intent(in)                      :: SB
   type(type_Saf)      ,intent(in)                      :: Saf
   character(len=500)                                   :: File_Plot
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: NF
   integer(kind=LONG)                                   :: SUBPN
   integer(kind=LONG)                                   :: iFile
   character(len=1)                                     :: Band
   character(len=4)                                     :: Pixel
   character(len=4)                                     :: Sub_Pixel
!
     iFile = 10
!
!    Apodisation Functions
     write(Band,'(i1.1)') SB
     write(Pixel,'(i4.4)') PN
     File_Plot = Files_Results_(1:len_trim(Files_Results_))&
                   //'PN'//Pixel//'_SB'//Band//'.asc'
     open(unit=iFile, file=File_Plot, status='unknown', err=999)
     write(iFile,'(a,i4,a)') '#',Saf%NsWn0,' Wave Numbers'
     write(iFile,'(a,50e22.14)') '#',(Saf%Wn0(NF),NF=1,Saf%NsWn0)
     write(iFile,'(a)') '# Opd; N x (Saf_Mod; Saf_Arg;)'
     if ( Saf%NsPixel == 1 ) then
        SUBPN = Saf%NsPixel
     else
        SUBPN = Saf%NsPixel+1 ! pixels
     end if
     do Ns = 1, Saf%NsOpd
        write(iFile,'(f12.9,50e22.14)')      &
                        Saf%Opd(Ns),         &
                       (Saf%Mod(Ns,NF,SUBPN),&
                        Saf%Arg(Ns,NF,SUBPN),&
                        NF=1,Saf%NsWn0 )
     end do  ! fin de boucle sur les opd
     close(unit=iFile)
     if( Saf%NsPixel /= 1 ) then
        do SUBPN = 1, Saf%NsPixel ! sub pixel loop
           write(Sub_Pixel,'(i4.4)') SUBPN
           File_Plot = Files_Results_(1:len_trim(Files_Results_))&
                   //'PN'//Pixel//'_SUBPN'//Sub_Pixel//'_SB'//Band//'.asc'
           open(unit=iFile, file=File_Plot, status='unknown', err=999)
           write(iFile,'(a,i4,a)') '#',Saf%NsWn0,' Wave Numbers'
           write(iFile,'(a,50e22.14)') '#',(Saf%Wn0(NF),NF=1,Saf%NsWn0)
           write(iFile,'(a)') '# Opd; N x (Saf_Mod; Saf_Arg;)'
           do Ns = 1, Saf%NsOpd
              write(iFile,'(f12.9,50e22.14)')    &
                              Saf%Opd(Ns),       &
                             (Saf%Mod(Ns,NF,SUBPN), &
                              Saf%Arg(Ns,NF,SUBPN), &
                              NF=1,Saf%NsWn0 )
           end do  ! end opd loop
           close(unit=iFile)
        end do  ! end sub pn loop
     end if
     return
 999 write(0,'(a)') 'plot_Saff write Error : '&
                ,File_Plot(1:len_trim(File_Plot))
     call exit(1)
   end subroutine plot_saf
!
!
   subroutine plot_fct( Files_Results_,&
                        SB, Cnv_Fct )
!
   implicit none
   character(len=500)  ,intent(in)                      :: Files_Results_
   integer(kind=LONG)  ,intent(in)                      :: SB
   type(type_Cnv_Fct)  ,intent(in)                      :: Cnv_Fct
   character(len=500)                                   :: File_Plot
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: iFile
   character(len=1)                                     :: Band
!
     iFile = 10
!
!    Apodisation Functions Interferogramm space
     write(Band,'(i1.1)') SB
     File_Plot = Files_Results_(1:len_trim(Files_Results_))&
                  //'IfSpace'//'_SB'//Band//'.asc'
     open(unit=iFile, file=File_Plot, status='unknown', err=999)
     write(iFile,'(a)') '# Opd (cm) ; Fct_TF;)'
     do Ns = 1, Cnv_Fct%Ns_Opd
        write(iFile,'(f12.7,50e22.14)')      &
                        Cnv_Fct%Opd(Ns)*100.,&
                        Cnv_Fct%Fct_TF(Ns)
     end do  ! fin de boucle sur les opd
     close(unit=iFile)
!
!    Apodisation Functions Interferogramm space
     write(Band,'(i1.1)') SB
     File_Plot = Files_Results_(1:len_trim(Files_Results_))&
                  //'SpSpace'//'_SB'//Band//'.asc'
     open(unit=iFile, file=File_Plot, status='unknown', err=999)
     write(iFile,'(a)') '# Wn (cm-1) ; Fct;)'
     do Ns = 1, Cnv_Fct%N_Sample
        write(iFile,'(f12.7,50e22.14)')     &
                        Cnv_Fct%Wn(Ns)/100.,&
                        Cnv_Fct%Fct(Ns)
     end do  ! fin de boucle sur les opd
     close(unit=iFile)
     return
 999 write(0,'(a)') 'plot_fct write Error : '&
                ,File_Plot(1:len_trim(File_Plot))
     call exit(1)
   end subroutine plot_fct
!
!
   subroutine plot_spectral( Files_Results_,    &
                             Srf_Param, Saf_Rpd,&
                             Saf, Sas, Apf, Srf )
!
   implicit none
   character(len=500)  ,intent(in)                       :: Files_Results_
   type(type_Srf_Param),intent(in)                       :: Srf_Param
   type(type_Saf)      ,intent(in)                       :: Saf_Rpd
   type(type_Saf)      ,intent(in),dimension(Srf_Param%Nband):: Saf
   type(type_Saf)      ,intent(in),dimension(Srf_Param%Nband):: Sas
   type(type_Srf)      ,intent(in),dimension(Srf_Param%Nband):: Srf
   type(type_Apf)      ,intent(in),dimension(Srf_Param%Nband):: Apf
   character(len=500)                                   :: Files_Plots
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: PN
   integer(kind=LONG)                                   :: NF
   integer(kind=LONG)                                   :: SB
   integer(kind=LONG)                                   :: iFile
   character(len=1)                                     :: Band
!
     iFile = 10
!
!    Self Apodisation Functions
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                     //'Saf_Rpd.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     write(iFile,'(a)') &
     '# Opd; 2( Saf_Mod; Saf_Arg; Sas_Mod; Sas_Arg;)'
     do PN = 1, Saf_Rpd%NsPixel ! boucle sur les pixels
        do Ns = 1, Saf_Rpd%NsOpd
           write(iFile,'(f12.9,10e20.10)') &
                    Saf_Rpd%Opd(Ns),       &
                    Saf_Rpd%Mod(Ns,1,PN),  &
                    Saf_Rpd%Arg(Ns,1,PN)
        end do
     end do  ! fin de boucle sur les pixels
     close(unit=iFile)
!
!    Self Apodisation Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                     //Band//'_Saf.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '# Opd; 2( Saf_Mod; Saf_Arg; Sas_Mod; Sas_Arg;)'
       do PN = 1, Saf(SB)%NsPixel ! boucle sur les pixels
         do NF = 1, Saf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Saf(SB)%NsWn0 ) then
             do Ns = 1, Saf(SB)%NsOpd
               write(iFile,'(f12.9,20e20.10)') &
                        Saf(SB)%Opd(Ns),       &
                        Saf(SB)%Mod(Ns,1,PN),  &
                        Saf(SB)%Arg(Ns,1,PN),  &
                        Sas(SB)%Mod(Ns,1,PN),  &
                        Sas(SB)%Arg(Ns,1,PN),  &
                        Saf(SB)%Mod(Ns,NF/2,PN),  &
                        Saf(SB)%Arg(Ns,NF/2,PN),  &
                        Sas(SB)%Mod(Ns,NF/2,PN),  &
                        Sas(SB)%Arg(Ns,NF/2,PN),  &
                        Saf(SB)%Mod(Ns,NF,PN), &
                        Saf(SB)%Arg(Ns,NF,PN), &
                        Sas(SB)%Mod(Ns,NF,PN), &
                        Sas(SB)%Arg(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
       end do  ! fin de boucle sur les pixels
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Agregated Self Apodisation Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Saf_agreg.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '# Opd; 2( Saf_Mod; Saf_Arg; Sas_Mod; Sas_Arg;)'
       PN = Saf(SB)%NsPixel+1 ! boucle sur les pixels
         do NF = 1, Saf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Saf(SB)%NsWn0 ) then
             do Ns = 1, Saf(SB)%NsOpd
               write(iFile,'(f12.9,20e20.10)') &
                        Saf(SB)%Opd(Ns),       &
                        Saf(SB)%Mod(Ns,1,PN),  &
                        Saf(SB)%Arg(Ns,1,PN),  &
                        Sas(SB)%Mod(Ns,1,PN),  &
                        Sas(SB)%Arg(Ns,1,PN),  &
                        Saf(SB)%Mod(Ns,NF/2,PN),  &
                        Saf(SB)%Arg(Ns,NF/2,PN),  &
                        Sas(SB)%Mod(Ns,NF/2,PN),  &
                        Sas(SB)%Arg(Ns,NF/2,PN),  &
                        Saf(SB)%Mod(Ns,NF,PN), &
                        Saf(SB)%Arg(Ns,NF,PN), &
                        Sas(SB)%Mod(Ns,NF,PN), &
                        Sas(SB)%Arg(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Apodisation Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Apf.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '# Opd; 2( Apf_Mod; Apf_Arg;)'
       do PN = 1, Apf(SB)%NsPixel ! boucle sur les pixels
         do NF = 1, Apf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Apf(SB)%NsWn0 ) then
             do Ns = 1, Apf(SB)%NsOpd
               write(iFile,'(f12.9,20e20.10)') &
                        Apf(SB)%Opd(Ns),       &
                        Apf(SB)%Mod(Ns,1,PN),  &
                        Apf(SB)%Arg(Ns,1,PN),  &
                        Apf(SB)%Mod(Ns,NF/2,PN), &
                        Apf(SB)%Arg(Ns,NF/2,PN), &
                        Apf(SB)%Mod(Ns,NF,PN), &
                        Apf(SB)%Arg(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
       end do  ! fin de boucle sur les pixels
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Agregated Apodisation Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Apf_agreg.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '# Opd; 2( Apf_Mod; Apf_Arg;)'
       PN = Apf(SB)%NsPixel+1 ! boucle sur les pixels
         do NF = 1, Apf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Apf(SB)%NsWn0 ) then
             do Ns = 1, Apf(SB)%NsOpd
               write(iFile,'(f12.9,20e20.10)') &
                        Apf(SB)%Opd(Ns),       &
                        Apf(SB)%Mod(Ns,1,PN),  &
                        Apf(SB)%Arg(Ns,1,PN),  &
                        Apf(SB)%Mod(Ns,NF/2,PN), &
                        Apf(SB)%Arg(Ns,NF/2,PN), &
                        Apf(SB)%Mod(Ns,NF,PN), &
                        Apf(SB)%Arg(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    SRF 1c In the Fourier Space
     SB = Srf_Param%Nband
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Gtf.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     write(iFile,'(a)') '# Opd; Gtf'
     do Ns = 1, Saf(SB)%NsOpd
        write(iFile,'(f12.9,10e20.10)') &
                        Saf(SB)%Opd(Ns),&
                        Srf(SB)%L1cTF(Ns)
     end do
     close(unit=iFile)
!
!    Spectral Response Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Srf.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '#SDWn1a; L1a; L1a; SDWn1b; L1b; L1b; SDWn1c; L1c'
       do PN = 1, Srf(SB)%NsPixel ! boucle sur les pixels
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Srf(SB)%NsWn0 ) then
             do Ns = 1, Srf(SB)%NsSDWn1a
               write(iFile,'(3(f12.3,3e20.10))') &
                           Srf(SB)%SDWn1a(Ns),   &
                           Srf(SB)%L1a(Ns,1,PN), &
                           Srf(SB)%L1a(Ns,NF/2,PN),&
                           Srf(SB)%L1a(Ns,NF,PN),&
                           Srf(SB)%SDWn1b(Ns),   &
                           Srf(SB)%L1b(Ns,1,PN), &
                           Srf(SB)%L1b(Ns,NF/2,PN),&
                           Srf(SB)%L1b(Ns,NF,PN),&
                           Srf(SB)%SDWn1c(Ns),   &
                           Srf(SB)%L1c(Ns,1,1)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
       end do  ! fin de boucle sur les pixels
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Agregated Spectral Response Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Srf_agreg.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '#SDWn1a; L1a; L1a; SDWn1b; L1b; L1b; SDWn1c; L1c'
       PN = Srf(SB)%NsPixel+1 ! pixel aggregated
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Srf(SB)%NsWn0 ) then
             do Ns = 1, Srf(SB)%NsSDWn1a
               write(iFile,'(3(f12.3,3e20.10))') &
                           Srf(SB)%SDWn1a(Ns),   &
                           Srf(SB)%L1a(Ns,1,PN), &
                           Srf(SB)%L1a(Ns,NF/2,PN),&
                           Srf(SB)%L1a(Ns,NF,PN),&
                           Srf(SB)%SDWn1b(Ns),   &
                           Srf(SB)%L1b(Ns,1,PN), &
                           Srf(SB)%L1b(Ns,NF/2,PN),&
                           Srf(SB)%L1b(Ns,NF,PN),&
                           Srf(SB)%SDWn1c(Ns),   &
                           Srf(SB)%L1c(Ns,1,1)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Spectral Performances
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Perf.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(2(a))') &
       '#Wn0; Arg0; Contrast; FieldAngle; WnShift1a; FWhm1a;',&
                            ' WnShift1b; FWhm1b; FWhm1c; Nrf1c'
       do PN = 1, Srf(SB)%NsPixel ! boucle sur les pixels
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           write(iFile,fmt='(f10.2,3e20.10,5(f12.4,f12.4))')&
                             Srf(SB)%Wn0(NF),               &
                             Sas(SB)%Arg0(NF,PN),           &
                             Sas(SB)%Contrast(NF,PN),       &
                             Sas(SB)%FieldMeanAngle(NF,PN), &
                             Srf(SB)%WnShift1a(NF,PN),      &
                             Srf(SB)%FWhm1a(NF,PN),         &
                             Srf(SB)%WnShift1b(NF,PN),      &
                             Srf(SB)%FWhm1b(NF,PN),         &
                             Srf(SB)%FWhm1c(1,1),           &
                             Apf(SB)%Nrf(NF,PN)
         end do  ! fin de boucle sur les nombres d onde
       end do  ! fin de boucle sur les pixels
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Agregated Spectral Performances
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Perf_agreg.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(2(a))') &
       '#Wn0; Arg0; Contrast; FieldAngle; WnShift1a; FWhm1a;',&
                            ' WnShift1b; FWhm1b; FWhm1c; Nrf1c'
       PN = Srf(SB)%NsPixel+1 ! boucle sur les pixels
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           write(iFile,fmt='(f10.2,3e20.10,5(f12.4,f12.4))')&
                             Srf(SB)%Wn0(NF),               &
                             Sas(SB)%Arg0(NF,PN),           &
                             Sas(SB)%Contrast(NF,PN),       &
                             Sas(SB)%FieldMeanAngle(NF,PN), &
                             Srf(SB)%WnShift1a(NF,PN),      &
                             Srf(SB)%FWhm1a(NF,PN),         &
                             Srf(SB)%WnShift1b(NF,PN),      &
                             Srf(SB)%FWhm1b(NF,PN),         &
                             Srf(SB)%FWhm1c(1,1),           &
                             Apf(SB)%Nrf(NF,PN)
         end do  ! fin de boucle sur les nombres d onde
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
     return
 999 write(0,*) 'plot_spectral write Error : '&
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_spectral
!
!
   subroutine plot_spectral_tas( Files_Results_,    &
                                 Srf_Param, Saf_Rpd,&
                                 Saf, Sas, Apf, Srf )
!
   implicit none
   character(len=500)  ,intent(in)                       :: Files_Results_
   type(type_Srf_Param),intent(in)                       :: Srf_Param
   type(type_Saf)      ,intent(in)                       :: Saf_Rpd
   type(type_Saf)      ,intent(in),dimension(Srf_Param%Nband):: Saf
   type(type_Saf)      ,intent(in),dimension(Srf_Param%Nband):: Sas
   type(type_Srf)      ,intent(in),dimension(Srf_Param%Nband):: Srf
   type(type_Apf)      ,intent(in),dimension(Srf_Param%Nband):: Apf
   character(len=500)                                   :: Files_Plots
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: PN
   integer(kind=LONG)                                   :: NF
   integer(kind=LONG)                                   :: SB
   integer(kind=LONG)                                   :: iFile
   character(len=1)                                     :: Band
!
     iFile = 10
!
!    Self Apodisation Functions
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                     //'Saf_Rpd.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     write(iFile,'(a)') &
     '# Opd; 2( Saf_Mod; Saf_Arg; Sas_Mod; Sas_Arg;)'
     do PN = 1, Saf_Rpd%NsPixel ! boucle sur les pixels
        do Ns = 1, Saf_Rpd%NsOpd
           write(iFile,'(f12.9,10e20.10)') &
                    Saf_Rpd%Opd(Ns),       &
                    Saf_Rpd%Mod(Ns,1,PN),  &
                    Saf_Rpd%Arg(Ns,1,PN)
        end do
     end do  ! fin de boucle sur les pixels
     close(unit=iFile)
!
!    Self Apodisation Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                     //Band//'_Saf.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '# Opd; 2( Saf_Mod; Saf_Arg; Sas_Mod; Sas_Arg;)'
       do PN = 1, Saf(SB)%NsPixel ! boucle sur les pixels
         do NF = 1, Saf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Saf(SB)%NsWn0-1 ) then
             do Ns = 1, Saf(SB)%NsOpd
               write(iFile,'(f12.9,10e20.10)') &
                        Saf(SB)%Opd(Ns),       &
                        Saf(SB)%Mod(Ns,2,PN),  &
                        Saf(SB)%Arg(Ns,2,PN),  &
                        Sas(SB)%Mod(Ns,2,PN),  &
                        Sas(SB)%Arg(Ns,2,PN),  &
                        Saf(SB)%Mod(Ns,NF,PN), &
                        Saf(SB)%Arg(Ns,NF,PN), &
                        Sas(SB)%Mod(Ns,NF,PN), &
                        Sas(SB)%Arg(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
       end do  ! fin de boucle sur les pixels
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Agregated Self Apodisation Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Saf_agreg.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '# Opd; 2( Saf_Mod; Saf_Arg; Sas_Mod; Sas_Arg;)'
       PN = Saf(SB)%NsPixel+1 ! boucle sur les pixels
         do NF = 1, Saf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Saf(SB)%NsWn0-1 ) then
             do Ns = 1, Saf(SB)%NsOpd
               write(iFile,'(f12.9,10e20.10)') &
                        Saf(SB)%Opd(Ns),       &
                        Saf(SB)%Mod(Ns,2,PN),  &
                        Saf(SB)%Arg(Ns,2,PN),  &
                        Sas(SB)%Mod(Ns,2,PN),  &
                        Sas(SB)%Arg(Ns,2,PN),  &
                        Saf(SB)%Mod(Ns,NF,PN), &
                        Saf(SB)%Arg(Ns,NF,PN), &
                        Sas(SB)%Mod(Ns,NF,PN), &
                        Sas(SB)%Arg(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Apodisation Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Apf.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '# Opd; 2( Apf_Mod; Apf_Arg;)'
       do PN = 1, Apf(SB)%NsPixel ! boucle sur les pixels
         do NF = 1, Apf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Apf(SB)%NsWn0-1 ) then
             do Ns = 1, Apf(SB)%NsOpd
               write(iFile,'(f12.9,10e20.10)') &
                        Apf(SB)%Opd(Ns),       &
                        Apf(SB)%Mod(Ns,2,PN),  &
                        Apf(SB)%Arg(Ns,2,PN),  &
                        Apf(SB)%Mod(Ns,NF,PN), &
                        Apf(SB)%Arg(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
       end do  ! fin de boucle sur les pixels
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Agregated Apodisation Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Apf_agreg.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '# Opd; 2( Apf_Mod; Apf_Arg;)'
       PN = Apf(SB)%NsPixel+1 ! boucle sur les pixels
         do NF = 1, Apf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Apf(SB)%NsWn0-1 ) then
             do Ns = 1, Apf(SB)%NsOpd
               write(iFile,'(f12.9,10e20.10)') &
                        Apf(SB)%Opd(Ns),       &
                        Apf(SB)%Mod(Ns,2,PN),  &
                        Apf(SB)%Arg(Ns,2,PN),  &
                        Apf(SB)%Mod(Ns,NF,PN), &
                        Apf(SB)%Arg(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    SRF 1c In the Fourier Space
     SB = Srf_Param%Nband
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Gtf.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     write(iFile,'(a)') '# Opd; Gtf'
     do Ns = 1, Saf(SB)%NsOpd
        write(iFile,'(f12.9,10e20.10)') &
                        Saf(SB)%Opd(Ns),&
                        Srf(SB)%L1cTF(Ns)
     end do
     close(unit=iFile)
!
!    Spectral Response Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Srf.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '#SDWn1a; L1a; L1a; SDWn1b; L1b; L1b; SDWn1c; L1c'
       do PN = 1, Srf(SB)%NsPixel ! boucle sur les pixels
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Srf(SB)%NsWn0-1 ) then
             do Ns = 1, Srf(SB)%NsSDWn1a
               write(iFile,'(3(f12.3,2e20.10))') &
                           Srf(SB)%SDWn1a(Ns),   &
                           Srf(SB)%L1a(Ns,2,PN), &
                           Srf(SB)%L1a(Ns,NF,PN),&
                           Srf(SB)%SDWn1b(Ns),   &
                           Srf(SB)%L1b(Ns,2,PN), &
                           Srf(SB)%L1b(Ns,NF,PN),&
                           Srf(SB)%SDWn1c(Ns),   &
                           Srf(SB)%L1c(Ns,1,1)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
       end do  ! fin de boucle sur les pixels
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Agregated Spectral Response Functions
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Srf_agreg.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(a)') &
       '#SDWn1a; L1a; L1a; SDWn1b; L1b; L1b; SDWn1c; L1c'
       PN = Srf(SB)%NsPixel+1 ! pixel aggregated
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Srf(SB)%NsWn0-1 ) then
             do Ns = 1, Srf(SB)%NsSDWn1a
               write(iFile,'(3(f12.3,2e20.10))') &
                           Srf(SB)%SDWn1a(Ns),   &
                           Srf(SB)%L1a(Ns,2,PN), &
                           Srf(SB)%L1a(Ns,NF,PN),&
                           Srf(SB)%SDWn1b(Ns),   &
                           Srf(SB)%L1b(Ns,2,PN), &
                           Srf(SB)%L1b(Ns,NF,PN),&
                           Srf(SB)%SDWn1c(Ns),   &
                           Srf(SB)%L1c(Ns,1,1)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Spectral Performances
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Perf.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(2(a))') &
       '#Wn0; Arg0; Contrast; FieldAngle; WnShift1a; FWhm1a;',&
                            ' WnShift1b; FWhm1b; FWhm1c; Nrf1c'
       do PN = 1, Srf(SB)%NsPixel ! boucle sur les pixels
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           write(iFile,fmt='(f10.2,3e20.10,5(f12.4,f12.4))')&
                             Srf(SB)%Wn0(NF),               &
                             Sas(SB)%Arg0(NF,PN),           &
                             Sas(SB)%Contrast(NF,PN),       &
                             Sas(SB)%FieldMeanAngle(NF,PN), &
                             Srf(SB)%WnShift1a(NF,PN),      &
                             Srf(SB)%FWhm1a(NF,PN),         &
                             Srf(SB)%WnShift1b(NF,PN),      &
                             Srf(SB)%FWhm1b(NF,PN),         &
                             Srf(SB)%FWhm1c(1,1),           &
                             Apf(SB)%Nrf(NF,PN)
         end do  ! fin de boucle sur les nombres d onde
       end do  ! fin de boucle sur les pixels
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
!
!    Agregated Spectral Performances
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Perf_agreg.asc'
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       write(iFile,'(2(a))') &
       '#Wn0; Arg0; Contrast; FieldAngle; WnShift1a; FWhm1a;',&
                            ' WnShift1b; FWhm1b; FWhm1c; Nrf1c'
       PN = Srf(SB)%NsPixel+1 ! boucle sur les pixels
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           write(iFile,fmt='(f10.2,3e20.10,5(f12.4,f12.4))')&
                             Srf(SB)%Wn0(NF),               &
                             Sas(SB)%Arg0(NF,PN),           &
                             Sas(SB)%Contrast(NF,PN),       &
                             Sas(SB)%FieldMeanAngle(NF,PN), &
                             Srf(SB)%WnShift1a(NF,PN),      &
                             Srf(SB)%FWhm1a(NF,PN),         &
                             Srf(SB)%WnShift1b(NF,PN),      &
                             Srf(SB)%FWhm1b(NF,PN),         &
                             Srf(SB)%FWhm1c(1,1),           &
                             Apf(SB)%Nrf(NF,PN)
         end do  ! fin de boucle sur les nombres d onde
     end do  ! fin de boucle sur les bandes
     close(unit=iFile)
     return
 999 write(0,*) 'plot_spectral_tas write Error : '&
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_spectral_tas
!
!
   subroutine plot_l1c_perturb( Files_Results_, Srf_Param, Srf )
!
   implicit none
   character(len=*)  ,intent(in)                        :: Files_Results_
   type(type_Srf_Param),intent(in)                      :: Srf_Param
   type(type_Srf)      ,intent(in),dimension(Srf_Param%Nband):: Srf
   character(len=500)                                   :: Files_Plots
   integer(kind=LONG)                                   :: SB
   integer(kind=LONG)                                   :: PN
   integer(kind=LONG)                                   :: NF
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: iFile
   integer(kind=LONG)                                   :: iPos
   character(len=1)                                     :: Band
!
     iFile = 10
!
!    Spectral L1C perturb Response Functions
     do SB = 1, Srf_Param%Nband ! Band loop
       write(Band,'(i1.1)') Srf_Param%SB_List(SB)
       Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //Band//'_Srf1c_Perturb.asc'
       iPos  = 1
       open(unit=iFile, file=Files_Plots, status='unknown', err=999)
       iPos  = 2
       write(iFile,'(a)') '#SDWn1c; 2( L1c )'
       do PN = 1, 1 ! boucle sur les pixels
         do NF = 1, Srf(SB)%NsWn0 ! boucle sur les nombres d onde
           if( NF == Srf(SB)%NsWn0 ) then
             do Ns = 1, Srf(SB)%NsSDWn1c
               write(iFile,'(3(f12.3,2e20.10))') &
                           Srf(SB)%SDWn1c(Ns),   &
                           Srf(SB)%L1c(Ns,1,PN), &
                           Srf(SB)%L1c(Ns,NF,PN)
             end do
           end if
         end do  ! fin de boucle sur les nombres d onde
       end do  ! end pixel loop
     end do  ! end band loop
     close(unit=iFile)
     return
 999 write(0,*) 'plot_l1c_Perturb write Error : '&
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_l1c_perturb
!
!
   subroutine plot_ccm( Files_Results_, &
                        Ccm,            &
                        Ccm_Rpd         )
!
   implicit none
   character(len=500)  ,intent(in)                      :: Files_Results_
   type(type_Ccm)      ,intent(in)                      :: Ccm
   type(type_Ccm)      ,intent(in)                      :: Ccm_Rpd
   character(len=500)                                   :: Files_Plots
   integer(kind=LONG)                                   :: Ns
   integer(kind=LONG)                                   :: iFile
   integer(kind=LONG)                                   :: iPos
!
     iFile = 10
     iPos  = 1
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Ccm.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 2
     write(iFile,'(a)',err=999) &
       '# Ccm_ApexX; Ccm_ApexY; Ccm_ApexZ'
     do Ns = 1, Ccm%NsCcm
           write(iFile,'(3e20.10)',err=999)&
                        Ccm%ApexX(Ns),     &
                        Ccm%ApexY(Ns),     &
                        Ccm%ApexZ(Ns)
     end do
     close(unit=iFile)
!
     iPos  = 3
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Ccm_Rpd.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 4
     write(iFile,'(a)',err=999) &
       '# Ccm_ApexX; Ccm_ApexY; Ccm_ApexZ'
     do Ns = 1, Ccm%NsCcm
           write(iFile,'(3e20.10)',err=999)&
                        Ccm_Rpd%ApexX(Ns), &
                        Ccm_Rpd%ApexY(Ns), &
                        Ccm_Rpd%ApexZ(Ns)
     end do
     close(unit=iFile)
!
     return
 999 write(0,*) 'plot_ccm write Error : ',iPos      &
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_ccm
!
!
   subroutine plot_zpd( Files_Results_, &
                        Zpd_CS,         &
                        Zpd_BB,         &
                        Zpd_EW          )
!
   implicit none
   character(len=500)  ,intent(in)                           :: Files_Results_
   type(type_Zpd)      ,intent(in)                           :: Zpd_CS
   type(type_Zpd)      ,intent(in)                           :: Zpd_BB
   type(type_Zpd)      ,intent(in)                           :: Zpd_EW
   character(len=500)                                        :: Files_Plots
   integer(kind=LONG)                                        :: Ns
   integer(kind=LONG)                                        :: iFile
   integer(kind=LONG)                                        :: iPos
!
     iFile = 10
     iPos  = 1
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                 //'_Zpd_Mod.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 2
     write(iFile,'(a)',err=999) '# Wn; CS BB EW (Mod_Connes; Mod_Nzpd)'
     do Ns = 1, Zpd_CS%N_Sample
        write(iFile,'(7e20.10)',err=999)    &
                      Zpd_CS%Wn(Ns)/100.,   &
                      Zpd_CS%Mod_Connes(Ns),&
                      Zpd_CS%Mod(Ns),       &
                      Zpd_BB%Mod_Connes(Ns),&
                      Zpd_BB%Mod(Ns),       &
                      Zpd_EW%Mod_Connes(Ns),&
                      Zpd_EW%Mod(Ns)
     end do
     close(unit=iFile)
!
     iPos  = 3
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'_Zpd_Arg.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 4
     write(iFile,'(a)',err=999) '# Wn; CS BB EW (Arg_Connes; Arg_Nzpd); Arg_Model'
     do Ns = 1, Zpd_CS%N_Sample
        write(iFile,'(8e20.10)',err=999)    &
                      Zpd_CS%Wn(Ns)/100.,   &
                      Zpd_CS%Arg_Connes(Ns),&
                      Zpd_CS%Arg(Ns),       &
                      Zpd_BB%Arg_Connes(Ns),&
                      Zpd_BB%Arg(Ns),       &
                      Zpd_EW%Arg_Connes(Ns),&
                      Zpd_EW%Arg(Ns),       &
                      Zpd_CS%Phase_Model(Ns)
     end do
     close(unit=iFile)
!
     iPos  = 5
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                 //'_Zpd_Dist.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 6
     write(iFile,'(a40,3i10)',err=999) '# CS BB EW NZpd_Barycentre   ',   &
                                                 Zpd_CS%NZpd_Barycentre,&
                                                 Zpd_BB%NZpd_Barycentre,&
                                                 Zpd_EW%NZpd_Barycentre
     write(iFile,'(a40,3i10)',err=999) '# CS BB EW NZpd_Connes   ',   &
                                                 Zpd_CS%NZpd_Connes,&
                                                 Zpd_BB%NZpd_Connes,&
                                                 Zpd_EW%NZpd_Connes
     write(iFile,'(a40,3i10)',err=999) '# CS BB EW NZpd_NZpd ',&
                                                 Zpd_CS%NZpd,&
                                                 Zpd_BB%NZpd,&
                                                 Zpd_EW%NZpd
     write(iFile,'(a40,3f10.6)',err=999) '# CS BB EW NZpd_Offset_Barycentre',&
                                                 Zpd_CS%Offset_Barycentre, &
                                                 Zpd_BB%Offset_Barycentre, &
                                                 Zpd_EW%Offset_Barycentre
     write(iFile,'(a40,3f10.6)',err=999) '# CS BB EW NZpd_Offset_Connes',&
                                                 Zpd_CS%Offset_Connes, &
                                                 Zpd_BB%Offset_Connes, &
                                                 Zpd_EW%Offset_Connes
     write(iFile,'(a40,3f10.6)',err=999) '# CS BB EW NZpd_Qual_Index_Connes',&
                                                 Zpd_CS%Qual_Index_Connes, &
                                                 Zpd_BB%Qual_Index_Connes, &
                                                 Zpd_EW%Qual_Index_Connes
     write(iFile,'(a40,3f10.6)',err=999) '# CS BB EW NZpd_Qual_Index',&
                                                 Zpd_CS%Qual_Index, &
                                                 Zpd_BB%Qual_Index, &
                                                 Zpd_EW%Qual_Index
     write(iFile,'(a)',err=999) '# Nstep; CS BB EW (Dist)'
     do Ns = 1, 2*Zpd_CS%NstepSrdFT+1
        write(iFile,'(i4,3e20.10)',err=999)   &
                      Ns-Zpd_CS%NstepSrdFT-1, &
                      Zpd_CS%Dist(Ns),        &
                      Zpd_BB%Dist(Ns),        &
                      Zpd_EW%Dist(Ns)
     end do
     close(unit=iFile)
!
     return
 999 write(0,*) 'plot_zpd write Error : ',iPos      &
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_zpd
!
!
   subroutine plot_psf( Files_Results_, &
                        Psf_Ref,        &
                        Psf_Rpd,        &
                        Psf_Mes         )
!
   implicit none
   character(len=500)  ,intent(in)                      :: Files_Results_
   type(type_Psf)      ,intent(in)                      :: Psf_Ref
   type(type_Psf)      ,intent(in)                      :: Psf_Rpd
   type(type_Psf)      ,intent(in)                      :: Psf_Mes
   character(len=500)                                   :: Files_Plots
   integer(kind=LONG)                                   :: Nl
   integer(kind=LONG)                                   :: Nc
   integer(kind=LONG)                                   :: iFile
   integer(kind=LONG)                                   :: iPos
!
     iFile = 10
     iPos  = 1
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Psf_Ref.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 2
     write(iFile,'(a)',err=999) &
       '# Psf_Ref_Y; Psf_Ref_Z; Psf_Ref_Wgt(Y,Z))'
     do Nl = 1, Psf_Ref%NbLin
        do Nc = 1, Psf_Ref%NbCol
           write(iFile,'(3e20.10)',err=999)&
                        Psf_Ref%Y(Nc),     &
                        Psf_Ref%Z(Nl),     &
                        Psf_Ref%Wgt(Nc,Nl,1)
        end do
     end do
     close(unit=iFile)
!
     iPos  = 3
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Psf_Rpd.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 4
     write(iFile,'(a)',err=999) &
       '# Psf_Rpd_Y; Psf_Rpd_Z; Psf_Rpd_Wgt(Y,Z))'
     do Nl = 1, Psf_Rpd%NbLin
        do Nc = 1, Psf_Rpd%NbCol
           write(iFile,'(3e20.10)',err=999)&
                        Psf_Rpd%Y(Nc),     &
                        Psf_Rpd%Z(Nl),     &
                        Psf_Rpd%Wgt(Nc,Nl,1)
        end do
     end do
     close(unit=iFile)
!
     iPos  = 5
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Psf_Mes.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 6
     write(iFile,'(a)',err=999) &
       '# Psf_Mes_Y; Psf_Mes_Z; Psf_Mes_Wgt(Y,Z))'
     do Nl = 1, Psf_Mes%NbLin
        do Nc = 1, Psf_Mes%NbCol
           write(iFile,'(3e20.10)',err=999)&
                        Psf_Mes%Y(Nc),     &
                        Psf_Mes%Z(Nl),     &
                        Psf_Mes%Wgt(Nc,Nl,1)
        end do
     end do
     close(unit=iFile)
!
     return
 999 write(0,*) 'plot_psf write Error : ',iPos      &
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_psf
!
!
   subroutine plot_gain( Files_Results_, &
                         SB,             &
                         Gain            )
!
   implicit none
   character(len=500)  ,intent(in)                      :: Files_Results_
   integer(kind=LONG)  ,intent(in)                      :: SB
   type(type_Gain)     ,intent(in)                      :: Gain
   character(len=500)                                   :: Files_Plots
   integer(kind=LONG)                                   :: NC
   integer(kind=LONG)                                   :: NL
   integer(kind=LONG)                                   :: SUB_PN
   integer(kind=LONG)                                   :: iFile
   integer(kind=LONG)                                   :: iPos
   character(len=1)                                     :: Band
!
     iFile = 10
     iPos  = 1
     write(Band,'(i1.1)') SB
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'SB'//Band//'_Gain_PN.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 2
     write(iFile,'(a,4i4)',err=999) &
           '# Column, Line, Gain PN, Col Start/End, Lin Start/End',&
           Gain%ColStart_PN,Gain%ColEnd_PN,Gain%LinStart_PN,Gain%LinEnd_PN
     do NL = 1, Gain%NbLin_PN
        do NC = 1, Gain%NbCol_PN
           write(iFile,'(2i4,f8.4)',err=999)      &
                        NC, NL, Gain%Gain_PN(NC,NL)
        end do
        write(iFile,'(2i4,f8.4)',err=999)
     end do
     close(unit=iFile)
!
     iPos  = 3
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'SB'//Band//'_Gain_SUB_PN.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 4
     write(iFile,'(a)',err=999) '# Column Line Gain SUB_PN'
     do SUB_PN = 1, Gain%Nb_SUB_PN
        do NL = 1, Gain%NbLin_SUB_PN
           do NC = 1, Gain%NbCol_SUB_PN
              write(iFile,'(2i4,f8.4,i4)',err=999) &
                 NC, NL, Gain%Gain_SUB_PN(NC,NL,SUB_PN), SUB_PN
           end do
           write(iFile,'(2i4,f8.4,i4)',err=999)
        end do
        write(iFile,'(2i4,f8.4,i4)',err=999)
        write(iFile,'(2i4,f8.4,i4)',err=999)
     end do
     close(unit=iFile)
!
     iPos  = 5
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'SB'//Band//'_Gain_Stat.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 6
     write(iFile,'(a)',err=999) &
       '# Gain AVG, STD, MIN, MAX'
     write(iFile,'(a4, 4f8.4)',err=999) 'PN  ',&
                        Gain%Gain_Avg_PN,      &
                        Gain%Gain_Std_PN,      &
                        Gain%Gain_Min_PN,      &
                        Gain%Gain_Max_PN
     do SUB_PN = 1, Gain%Nb_SUB_PN
        write(iFile,'(i4, 4f8.4)',err=999) SUB_PN,   &
                        Gain%Gain_Avg_SUB_PN(SUB_PN),&
                        Gain%Gain_Std_SUB_PN(SUB_PN),&
                        Gain%Gain_Min_SUB_PN(SUB_PN),&
                        Gain%Gain_Max_SUB_PN(SUB_PN)
     end do
     close(unit=iFile)
!
     return
 999 write(0,*) 'plot_gain write Error : ',iPos      &
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_gain
!
!
   subroutine plot_forman( Files_Results_, &
                           Forman          )
   implicit none
   character(len=*)    ,intent(in)                    :: Files_Results_
   type(type_Forman)   ,intent(in)                    :: Forman
   character(len=500)                                 :: Files_Plots
   integer(kind=LONG)                                 :: iFile
   integer(kind=LONG)                                 :: iPos
   integer(kind=LONG)                                 :: Ns
   real(kind=DOUBLE)                                  :: Opd
   real(kind=DOUBLE)                                  :: Wn
!
     iFile = 10
     iPos  = 1
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Sp_Usefull.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 2
     do Ns = 1, Forman%N_Usefull_Sample
        write(iFile,'(3e20.10)',err=999) Forman%Wn_Usefull(Ns)/100.,&
                     dsqrt(  dreal(Forman%Sp_Usefull(Ns)            &
                           *dconjg(Forman%Sp_Usefull(Ns)) ) ),      &
                     datan2( dimag(Forman%Sp_Usefull(Ns)),          &
                             dreal(Forman%Sp_Usefull(Ns)) )
     end do
     close(unit=iFile)
!
     iPos  = 10
     Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                   //'Sp_Reduced.asc'
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 20
     do Ns = 1, Forman%Ns_Fft+1
        Wn = dble(Ns-int(Forman%Ns_Fft/2)-1) * Forman%dWn
        write(iFile,'(3e20.10)',err=999) Wn/100.,                 &
                  dsqrt( dreal( Forman%Sp_Reduced(Ns)             &
                               *dconjg(Forman%Sp_Reduced(Ns)) ) ),&
                  datan2( dimag(Forman%Sp_Reduced(Ns)),           &
                          dreal(Forman%Sp_Reduced(Ns)) )
     end do
     close(unit=iFile)
     if( Forman%Type == 'R' ) then
!
        iPos  = 100
        Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                      //'Kernel_R.asc'
        open(unit=iFile, file=Files_Plots, status='unknown', err=999)
        iPos  = 200
        do Ns = 1, Forman%Ns_Fft+1
           Opd = dble(Ns-int(Forman%Ns_Fft/2)-1) * Forman%dOpd
           write(iFile,'(2e20.10)',err=999) Opd,Forman%Kernel_R(Ns)
        end do
     else
!
        iPos  = 100
        Files_Plots = Files_Results_(1:len_trim(Files_Results_))&
                      //'Kernel_C.asc'
        open(unit=iFile, file=Files_Plots, status='unknown', err=999)
        iPos  = 200
        do Ns = 1, Forman%Ns_Fft+1
           Opd = dble(Ns-int(Forman%Ns_Fft/2)-1) * Forman%dOpd
           write(iFile,'(3e20.10)',err=999) Opd,Forman%Kernel_C(Ns)
        end do
     end if
     close(unit=iFile)
!
     return
 999 write(0,*) 'plot_forman write Error : ',iPos   &
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_forman
!
!
   subroutine plot_phase_mertz( Files_Plots,&
                                Phase_Mertz )
   implicit none
   character(len=*)      ,intent(in)                    :: Files_Plots
   type(type_Phase_Mertz),intent(in)                    :: Phase_Mertz
   integer(kind=LONG)                                   :: iFile
   integer(kind=LONG)                                   :: iPos
   integer(kind=LONG)                                   :: Ns
!
     iFile = 10
     iPos  = 1
     open(unit=iFile, file=Files_Plots, status='unknown', err=999)
     iPos  = 2
     write(iFile,'(a)',err=999)  '# Ns Wn SMod SArg'
     iPos  = 3
     do Ns = 1, Phase_Mertz%N_Sample
        write(iFile,'(i10,f10.2,2e12.4)',err=999)         &
                          Ns,                             &
                          Phase_Mertz%Wn(Ns)/100.,&
                          Phase_Mertz%Sp_Mod(Ns)*100.,    &
                          Phase_Mertz%Sp_Arg(Ns)
     end do
     close(unit=iFile)
!
     return
 999 write(0,*) 'plot_phase_mertz write Error : ',iPos   &
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_phase_mertz
!
!
   subroutine plot_dc_component( Files_Plots, &
                                 Spectrum,    &
                                 Dc_Component )
!
   implicit none
     character(len=500)        ,intent(in)              :: Files_Plots
     type(type_Spectrum)       ,intent(in)              :: Spectrum
     type(type_Dc_Component)   ,intent(in)              :: Dc_Component
     integer(kind=LONG)                                 :: iFile
     integer(kind=LONG)                                 :: iPos
     integer(kind=LONG)                                 :: Seg
!
     iFile = 10
     iPos = 1
     open(iFile,file=Files_Plots, err=999)
     write(iFile,'(a,e15.6)',err=999) '# Spectral band integration :',&
                    sum(Spectrum%Real_Part(1:Spectrum%N_Sample))      &
                    /Spectrum%N_Sample
     iPos = 2
     write(iFile,'(a,e15.6)',err=999) '# Dc_Component  DC_Cal      :',&
                    Dc_Component%DC_Cal
     iPos = 3
     write(iFile,'(a)',err=999) '# Dc_Component per Segment:'
     do Seg = 1, Dc_Component%N_Segment
        iPos = 3+Seg
        write(iFile,'(i5,2e15.6)',err=999)      &
                    Seg, Dc_Component%Opd(Seg), & 
                    Dc_Component%Segment_Cal(Seg)
     end do
     close(iFile)
!
     return
 999 write(0,*) 'plot_dc_component write Error : ',iPos      &
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_dc_component
!
!
   subroutine plot_dc_component_1( Files_Plots, &
                                   Dc_Component )
!
   implicit none
     character(len=500)        ,intent(in)              :: Files_Plots
     type(type_Dc_Component)   ,intent(in)              :: Dc_Component
     integer(kind=LONG)                                 :: iFile
     integer(kind=LONG)                                 :: iPos
     integer(kind=LONG)                                 :: Seg
!
     iFile = 10
     iPos = 1
     open(iFile,file=Files_Plots, err=999)
     iPos = 2
     write(iFile,'(a,e15.6)',err=999) '# Dc_Component  DC_Cal      :',&
                    Dc_Component%DC_Cal
     iPos = 3
     write(iFile,'(a)',err=999) '# Dc_Component per Segment:'
     do Seg = 1, Dc_Component%N_Segment
        iPos = 3+Seg
        write(iFile,'(i5,2e15.6)',err=999)      &
                    Seg, Dc_Component%Opd(Seg), &
                    Dc_Component%Segment_Cal(Seg)
     end do
     close(iFile)
!
     return
 999 write(0,*) 'plot_dc_component_1 write Error : ',iPos      &
                ,Files_Plots(1:len_trim(Files_Plots))
     call exit(1)
   end subroutine plot_dc_component_1
!
!
end module plot_module
