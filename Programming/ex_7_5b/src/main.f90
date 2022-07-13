program exercise_7_5b
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0
   real(R_), allocatable   :: A(:)

   open (file=input_file, newunit=In)
      read (In, *) M
      allocate (A(M))
      read (In, *) A
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//M//"f6.2)") A
   close (Out)
 
   
   call SortNegatives(A(2::2))
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(/"//M//"f6.2)") A
   close (Out)

contains
  subroutine SortNegatives(A)
      real(R_), intent(inout) :: A(:)
      real(R_) :: tmp
      integer  :: i, MinInd,N, k

      do i = 1, Size(A) 
       MinInd = MinLoc(A, 1) + i-1
         if(A(i) > A(MinInd)) & 
            A([i, MinInd]) = A([MinInd, i])
      end do
   end subroutine SortNegatives
end program exercise_7_5b
