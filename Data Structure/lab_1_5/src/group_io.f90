module Group_IO
   use Environment

   implicit none
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5
   integer, parameter :: MARKS_AMOUNT  = 5
   integer              :: i = 0


real(R_)                                        :: start, finish

   ! Структура данных для хранения данных о студенте.
   type student
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
      type(student), pointer              :: next                 => Null()
   end type student
contains
   ! Чтение списка класса: фамилии, инициалы, полы и оценки.
   subroutine Read_class_list(Input_File,Group_List ,N)
      type(student), pointer, intent(inout)     :: Group_list
      character(*), intent(in)   :: Input_File
      integer  In
      integer, intent(inout)     :: N

 call cpu_time(start)
      open (file=Input_File, encoding=E_, newunit=In)
         Group_list => Read_student(In)
      close (In)
      N = i
   end subroutine Read_class_list

   ! Чтение следующего студента.
   recursive function Read_student(In) result(Stud)
      type(student), pointer  :: Stud
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (Stud)
      format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
      read (In, format, iostat=IO) stud%Surname, stud%Initials
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
         i = i + 1
          Stud%next => Read_student(In)
      else
         deallocate (Stud)
         nullify (Stud)
      end if
   end function Read_student


   subroutine Output_class_list(Output_File, Class_List, List_Name,Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(student), intent(in)  :: Class_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      write (out, '(/a)') List_Name   
      call Output_student(Out, Class_List)

           call cpu_time(finish)
     write (Out, '(f6.4)') finish - start
      close (Out)

   end subroutine Output_class_list

   recursive subroutine Output_student(Out, Stud)
      integer, intent(in)        :: Out
      type(student), intent(in)  :: Stud
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
      write (Out, format, iostat=IO) Stud%Surname, Stud%Initials
      call Handle_IO_status(IO, "writing student")
      if (Associated(Stud%next)) &
         call Output_student(Out, Stud%next)
         
   end subroutine Output_student
end module Group_IO 
