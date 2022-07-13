! Copyright 2015 Fyodorov S. A.


program reference_lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F3
   type(SourceLine), pointer :: InitialCode  => Null()   ! Первоначальный код.

   F1 = "../data/source.f90"
   F3 = "source.f90.diff"
   
   InitialCode => Read_Source_Code(F1)

   if (Associated(InitialCode)) then
      call Diff_Codes(InitialCode, 4, 8, 1)

      call Output_Source_Code(F3, InitialCode)
   end if

end program reference_lab_2
