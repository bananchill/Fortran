program reference_lab_1_5
   use Environment
   use Fullname_Process 
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   type(student), pointer  :: Group_List => Null()
   integer(I_)               :: N = 0

input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   call Read_class_list(input_file,Group_List ,N)

   if (Associated(Group_List)) then
      !call Output_class_list(output_file, Group_List, "Исходный список:", "rewind")
      call Sort_fullnames(Group_List, Group_List)
      call Output_class_list(output_file, Group_List, "Отсортированный список класса" ,"append")
   end if
end program reference_lab_1_5
