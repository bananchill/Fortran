module Matr_IO
   use Environment
contains
  subroutine ReadP(input_file, N, M, I)
      character(*), intent(in) :: input_file
      integer, intent(out)    :: N, M
      real(R_), intent(out), allocatable    :: I(:,:)
      
      integer :: In = 0
      integer j

      open (file=input_file, newunit=In)
         read (In, *) N, M
         allocate(I(N,M)) 
         read (In, *)(I(j,:), j = 1, N)
      close (In)
   end subroutine ReadP
  

   subroutine OutputP(output_file, I)
      character(*), intent(in) :: output_file
      real(R_), intent(in)     :: I(:, :)

      integer :: Out = 0
      integer    j
   
      open (file=output_file, encoding=E_, newunit=Out)
         write(out, '('//Ubound(I,1)//'f6.2)') (I(:, j), j = 1, Ubound(I,2))
         write(out, *)
      close (Out)
   end subroutine OutputP
   
   ! Вывод значений интеграла для разных p.
   
   subroutine Output(output_file, sumNormM, sumNormK, summ)
      character(*), intent(in) :: output_file
      real(R_), intent(in)     :: sumNormM,sumNormK, summ
      integer :: Out = 0
   
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, *) 'sumNormM =', sumNormM 
         write (Out, *) 'sumNormK =', sumNormK 
         write (Out, *) 'sumNormI =', summ
      close (Out)
   end subroutine Output
end module Matr_IO
