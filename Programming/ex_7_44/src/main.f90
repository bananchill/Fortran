program exercise_7_44
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: C(:, :), max_val_col (:)
   real(R_)                :: max_val = 0

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (C(N, M), max_val_col(M))
      read (In, *) (C(i, :), i = 1, N)
   close (In)

   ! Вывод данных.
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (C(i, :), i = 1, N)
   close (Out)
  
   max_val_col = MaxVal(C, dim = 1)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *)  
      write (Out, '('//M//'f6.2)')  max_val_col 
   close (Out)

end program exercise_7_44
