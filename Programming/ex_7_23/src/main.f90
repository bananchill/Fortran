program exercise_7_23
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, j = 1, countAct = 1 
   real(R_), allocatable   :: C(:, :)
   real(R_)                :: max_sum = 0 
   real(R_), allocatable    :: Sum_pos(:, :)
   integer, allocatable    :: Index_Sum_pos(:,:), Indexes(: ,:)
   logical, allocatable    :: Mask(:)

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (C(N, M), Index_Sum_pos(N*M, 2))
      read (In, *) (C(i, :), i = 1, N)
   close (In)
   allocate(Mask(N**2), source = .false.)
   


   open (file=output_file, encoding=E_, newunit=Out)
     write (Out, '('//M//'f6.2)') (C(i, :), i = 1, N)
     write (Out, *)
   close (Out)
   
   allocate(Sum_pos(1:N, 2))

   Index_Sum_pos(:,1) = [((i, i = 1, N-1), j = 1, M-1)]
   Index_Sum_pos(:,2) = [((j, i = 1, N-1), j = 1, M-1)]
   
   Sum_pos = C(:, 1:N-1)+ C(:, 2:N)
   Sum_pos = Sum_pos(1:N-1,:)+ Sum_pos(2:N, :)
   max_sum = MaxVal(Sum_pos)
   Mask = [max_sum == Sum_pos ]
   
   allocate(Indexes(Count(Mask), 2))
   
   Indexes(:, 1) = Pack(Index_Sum_pos(:,1), Mask)
   Indexes(:, 2) = Pack(Index_Sum_pos(:,2), Mask)
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, * )  max_sum
      write (Out, '('// Ubound(Sum_pos, 1) //'f6.2)' )(Sum_pos(i,:), i = 1, Ubound( Sum_pos,1 ) )
      write (Out, '(2i3)' )(Indexes(i,:), i = 1, Ubound( Indexes,1 ) )
   close (Out)

end program exercise_7_23
