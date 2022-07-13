program reference_lab_3
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1,F2, F3
   type(SourceLine), pointer :: InitialString  => Null()   ! Первоначальный код.
   type(SourceLine), pointer :: DeletedString  => Null()  

   F1 = "../data/source.f90"
   F2 = "../data/source2.f90"
   F3 = "source.f90.diff"
   
   InitialString => Read_Source_Code(F1)
   DeletedString => Read_Source_Code(F2)

   if (Associated(InitialString)) then
      if(Associated(DeletedString)) &
         call Diff_Str(InitialString, DeletedString)
 !     call Output_Source_Code(F3, InitialCode)

      call Output_Source_Code(F3, InitialString)
   end if

end program reference_lab_3
