program exercise_3
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_), allocatable   :: B(:), A(:) 

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (A(N))

      read (In, *) A
   close (In)
   allocate(B(N/3))
   !P = ProdImp(B)
   ! Чистая функция в регулярном стиле.
   B = ProdImp(A)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(f0.4, T8, "| ", f0.4, T17, "| ", f0.4)") B
   close (Out)

contains
   
   ! Чистая функция в императивном стиле.
   pure function ProdImp(A) result(B)
      real(R_)     A(:)
      intent(in)  A
      integer     i, k
      integer                ::	Prod
      real(R_), allocatable  :: B(:)
	
      k = 2
      allocate(B(k))
     
      Prod = 1
      do i = 2, N, 3
      B(Prod) = A(i - 1) + A(i) + A(i + 1)      
      Prod= Prod+1
      end do
   end function ProdImp
end program exercise_3
