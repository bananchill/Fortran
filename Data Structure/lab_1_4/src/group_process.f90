module Group_Process
   use Environment
   use Group_IO

   implicit none

contains
   pure recursive subroutine Sort_class_list(Group, i,N)
      type(student), intent(inout)  :: Group(:)
      integer, intent(in)           ::  N, i
      
      type(student)  :: tmp_Stud
     
      call Sort_class_list_helper(Group, i, i-1)
      if(i <  N) &
         call Sort_class_list(Group, i+1, N) 
   end subroutine Sort_class_list

   pure recursive subroutine Sort_class_list_helper(Group,current ,i)
      type(student), intent(inout)  :: Group(:)
      integer, intent(in)           ::  i, current
      
      type(student)  :: tmp_Stud

      if(Group(current)%Surname > Group(i)%Surname .or. (Group(current)%Surname== Group(i)%Surname &
         .and. Group(current)%Initials > Group(i)%Initials)) & 
         Group([current, i]) = Group([i, current])

      if(i > 1) & 
         call Sort_class_list_helper(Group, current ,i-1)
   end subroutine Sort_class_list_helper
end module group_process
