program exercise_7_28
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: C(:, :), B(:, :)
   real(R_)                :: max_sum = 0
   
   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (C(N, M))
      read (In, *) (C(i, :), i = 1, N)
   close (In)
   allocate(B(N,M))
  

 open (file=output_file, encoding=E_, newunit=Out)
      write (Out, *)   
      write (Out, '('//M//'f6.2)') (C(i, :), i = 1, N)
   close (Out)


   B =  RevD(C)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *)   
      write (Out, '('//M//'f6.2)') (B(i, :), i = 1, N)
   close (Out)

contains
    ! Чистая подпрограмма в регулярном стиле.
   pure function RevD(C) result(B)
      real(R_), intent(in)    :: C(:, :)
      real(R_), allocatable :: B(:,:)
      real(R_)  value_I, value_J 
      integer  i, j, N_min
      allocate(B(N,M))


      do i = 1, N
         B(i, [i, N-i+1]) = B(i, [N-i+1, i])
      end do

   end function RevD
end program exercise_7_28
