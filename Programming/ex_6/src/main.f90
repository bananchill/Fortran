program exercise_6_3
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real                    :: y_n, y_n1, a, y
   real, parameter         :: eps = 1e-4

   open (file=input_file, newunit=In)
      read (In, *) a
   close (In)

  y_n = a  
  y = sqrt(a)   
  do
     y_n1 = y_n
     y_n = y_n - ((y_n**2 - a) / (2 * y_n))  
     if(abs(y_n1 - y_n) < eps) &
        exit
   end do
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", f0.8/))') 'sqrt(a) = ', y 
      write (Out, '(4(a, T16, "= ", f0.8/))') 'y_n =', y_n 
      write (Out, '(4(a, T16, "= ", f0.8/))') 'a = ', a 
  close (Out)

end program exercise_6_3

