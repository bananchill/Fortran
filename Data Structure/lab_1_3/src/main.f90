program reference_lab_1_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   
   type(student)              :: Group(STUD_AMOUNT)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   call Sort_class_list(Group)

   call Output_class_list(output_file, Group, "Отсортированный список:", "append")

end program reference_lab_1_3
