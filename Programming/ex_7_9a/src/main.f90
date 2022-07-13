program exercise_7_9a
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: Z(:, :), Sums(:)
   real(R_)                :: s = 0

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N, N))
      read (In, *) (Z(i, :), i = 1, N)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') (Z(i, :), i = 1, N)
   close (Out)


   s = Norm2(Z) 

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, T5, "= ", f9.6)') "Sum", s
   close (Out)
end program exercise_7_9a
