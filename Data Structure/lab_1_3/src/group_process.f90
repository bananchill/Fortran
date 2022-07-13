module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
   pure subroutine Sort_class_list(Group)
      type(student), intent(inout)  :: Group(:)

      integer        :: i, j
      type(student)  :: tmp_stud

      do i = 2, Size(Group)  
         tmp_stud = Group(i)
         j=i-1
         do while (j>=1 .and. (Group(j)%Surname > tmp_stud%Surname .or. (Group(j)%Surname == tmp_stud%Surname &
               .and. Group(j)%Initials > tmp_stud%Initials)))
            Group(j+1) = Group(j)
            j=j-1
         end do
         Group(j+1) = tmp_stud
      end do

   end subroutine Sort_class_list

end module group_process
