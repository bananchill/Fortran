program exercise_4_2a
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, Nx = 0, Ny = 0, i = 0, j = 0
   real(R_)                :: x1 = 0, x2 = 0, dx = 0, y1 = 0, y2 = 0, dy = 0
   real(R_), allocatable   :: X(:), Y(:), F(:)

   open (file=input_file,newunit=In)
      read (In, *) x1, x2, dx
      read (In, *) y1, y2, dy
   close (In)

   open (file=output_file, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "x1", x1, "x2", x2, "dx", dx
      write (Out, '(3(a, T4, "= ", f0.4/))') "y1", y1, "y2", y2, "dy", dy
   close (Out)
   
   Nx = NInt((x2-x1) / dx) + 1
   Ny = NInt((y2-y1) / dy) + 1
  
   allocate (X(Nx), Y(Ny), F(Nx*Ny))
 
   call TabF(x1, y1, dx, dy, X, Y, F)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '("   x", T8, "|", T13, "y", T17, "|", T22, "f")')
      write (Out, '(f0.1, T8, "| ", f0.1, T17, "| ", f0.1)') ((X(i), Y(j), F(j + (i-1)*Ny), j = 1, Ny), i = 1, Nx)
   close (Out)

contains
    pure subroutine TabF(x1, y1, dx, dy, X, Y, F)
      real(R_)    x1, y1, dx, dy, X(:), Y(:), F(:)
      intent(in)  x1, y1, dx, dy
      intent(out) X, Y, F
      integer     i

      X = [(x1 + dx*(i-1), i = 1, Nx)]
   
      Y = [(y1 + dy*(i-1), i = 1, Ny)]
      

       ! Формирование F = [F(x1,y1), F(x1,y2), F(x1,y3), ..., F(x2,y1), F(x2,y2), F(x2,y3), ...].
      ! F(1) == F(x1, y1), F(2) == F(x1, y2), .... F(Ny) = F(x1, yNy), ..
      F = [(Sin(X(i)+Y) / (Cos(X(i)+Y))**2 ,i = 1, Nx)]
   end subroutine TabF
end program exercise_4_2a
