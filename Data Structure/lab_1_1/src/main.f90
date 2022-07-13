program reference_lab_1_1
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 10000, SURNAME_LEN = 15, INITIALS_LEN = 5, std_amount = 2 
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_) !CH__"\u1052" 

   character(:), allocatable  :: input_file, output_file, format

   ! Массивы фамилий, инициалов, полов, оценок и средних оценов и временные
   ! переменные для обменов при сортировке.
   character(SURNAME_LEN, kind=CH_)                :: tmpSurname = ""
   character(SURNAME_LEN, kind=CH_)                :: Class_Surnames(STUD_AMOUNT) = ""
   
   character(INITIALS_LEN, kind=CH_)               :: tmpInitials = ""
   character(INITIALS_LEN, kind=CH_)               :: Class_Initials(STUD_AMOUNT) = ""

   integer :: In, Out, IO, i, j
   logical :: Swap
   real(R_)                                        :: start, finish

   call cpu_time(start)

   input_file = "../data/class.txt"
   output_file = "output.txt"
   ! Чтение списка класса: фамилии, инициалы, полы, оценки и средний.
   open (file=input_file, encoding=E_, newunit=In)
      format = '(a, 1x, a)'
      read (In, format, iostat=IO) (Class_Surnames(i), Class_Initials(i), i = 1, STUD_AMOUNT)
   close (In)
   
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading class list."
      case(1:)
         write (Out, '(a)') "Error while reading class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
   end select

   ! Вывод списка класса.
   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Class_Surnames(i), Class_Initials(i), i = 1, STUD_AMOUNT)
   close (Out)
   
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing class list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
   end select

   
   do i = 1, STUD_AMOUNT
      tmpSurname = Class_Surnames(i)
      tmpInitials = Class_Initials(i)
      do j = i-1,1, -1 
         Swap = .false.
            if ( tmpSurname < Class_Surnames(j)) then
               Swap = .true.
            else if (tmpSurname== Class_Surnames(j) .and. tmpInitials < Class_Initials(j)) then
               Swap = .true.
            else
              !Если вытащили элемент больше - останавливаемся 
               exit
            end if

         if (Swap) then
           Class_Surnames(j+1)   = Class_Surnames(j)
           Class_Initials(j+1)   = Class_Initials(j)

         end if
      end do
      
      Class_Surnames(j+1) = tmpSurname
      Class_Initials(j+1) = tmpInitials

   end do

   ! Вывод отсортированного списка юношей со средним баллом.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Отсортированный класс:"
      write (Out, format, iostat=IO) &
         (Class_Surnames(i), Class_Initials(i), i = 1, STUD_AMOUNT)
  call cpu_time(finish)
      write (out, '(f6.4)') finish - start
   close (Out)
   
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted boys list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   end select




end program reference_lab_1_1
