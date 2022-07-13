program exercise_1 
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0
   real(R_)                   :: x = 0, ln_x = 0
   
   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) x
   close (In)

   if(x >= 0) then 
      ln_x = x
   else 
      ln_x= -x 
   end if
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *) "y = |x|:", ln_x 
   close (Out)
   
end program exercise_1
