program exercise_5
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N, i=0
   real(R_), allocatable   :: A(:), B(:), C(:), D(:), F(:)

  open (file=input_file,newunit=In)
      read (In, *) N
      allocate(A(N), B(N), C(N), D(N), F(N))
      read (In, *) A
      read (In, *) B
      read (In, *) C
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '("A =", f0.1, "| ","B =",   f0.1,"| ", "C =", f0.1)') A, B, C
   close (Out)

   D = Min(A, B, C)
   F = Max(A, B, C)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '("D =", f0.1, "| ", T8,"F =",   f0.1)') D,F
   close (Out)
	
end program exercise_5
