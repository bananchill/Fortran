program exercise_1 
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 0, i = 0 , m = 3 , n = 4
   real(R_)                   :: x = 0, ln_x
   real(4), allocatable       :: Items(:)
   
   allocate(Items(n))
   
  pen (file=input_file, encoding=E_, newunit=In)
      read (In, *) x
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "x", x
   close (Out)

   Items(1) = x / 4+x
   do i = 2, 3
      Items(i) = (Items(i-1) * (x**m )) / (m * ((4 + x)**m))  !Подсчет слaгаемых
      m = m+2
   end do
   
   Items(4) = 0.693147 + 2 * Sum(Items(1:3)) !Подсчет окончательного значения для log(2+a)

 do i = 1, 4 
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, fmt) "Items", Items(i) 
      ! Проверка:
      write (Out, fmt) "error", log(2+x) - Items(i)
   close (Out)
 end do 

end program exercise_1
