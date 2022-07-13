program exercise_8_28
   use Environment
   use Matr_IO
   use Matr_calculate 

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                :: N = 0, M = 0
   real(R_), allocatable   :: I(:, :)
   real(R_)                :: summ = 0, sumNormM, sumNormK

   call ReadP(input_file, N, M, I)
   
   call OutputP(output_file ,I)

   
   call mNormaMat(I, sumNormM)
   call iNormaMat(I, summ)
   call kNormaMat(I,   sumNormK)
   
   call Output(output_file, sumNormM, sumNormK, summ)
end program exercise_8_28
