program lab_1_2
   use Environment
   implicit none
   integer(I_), parameter              :: SURNAME_LEN = 15, INITIALS_LEN = 5, LIST_LEN = 100
   character(:), allocatable           :: input_file, output_file
   character(KIND = CH_)               :: surnames(SURNAME_LEN, LIST_LEN) = "", &
                                          initials(INITIALS_LEN,LIST_LEN) = ""
 
   real(R_)                                        :: start, finish
                                       input_file = "../data/class.txt"
   output_file = "output.txt"

   call cpu_time(start)

   call read_fullname_list(input_file, surnames, initials)
   !call output_fullname_list(output_file, surnames, initials, "Исходный список", "rewind")
   call sort_fullnames(surnames, initials)
   call output_fullname_list(output_file, surnames, initials, "Отсортированный список", "append")

contains
   subroutine read_fullname_list(input_file, surnames, initials)
      character(*)                  input_File
      character(kind=CH_)           surnames(:,:), initials(:, :)

      integer(I_)                   In, i, IO
      character(:), allocatable  :: format

      open (file=input_file, encoding=E_, newunit=In)
         format = '('//SURNAME_LEN// 'a1, 1x,' //INITIALS_LEN// 'a1)'
         read (In, format, iostat = IO) (surnames(:, i), initials(:, i), i = 1, LIST_LEN)
         call Handle_IO_status(IO, "reading positions list")
      close (In)
   end subroutine read_fullname_list

   subroutine output_fullname_list(output_file, surnames, initials, list_name, position)
      character(*)         output_File, position, list_name
      character(kind=CH_)  surnames(:, :), initials(:,:)

      integer                          :: Out, i, IO
      character(:), allocatable        :: format
  
      open (file=output_file, encoding=E_, newunit=Out)
         format = '(' //SURNAME_LEN// 'a1, 1x,' //INITIALS_LEN// 'a1)' 
         write (out, '(a, ":")') list_name
         write (Out, format, iostat=IO) (surnames(:, i), initials(:, i), i = 1, LIST_LEN)
         call Handle_IO_status(IO, "writing "//list_name)
   call cpu_time(finish)
   write (Out, '(f6.4)')  finish - start
      close (Out)
      
   end subroutine output_fullname_list
   
   pure subroutine sort_fullnames(surnames, initials)
      character(kind=CH_), intent(inout)  :: surnames(:,:), initials(:,:)
      integer                                i, j

      do i = 2, LIST_LEN
         j = i - 1
         do while (j >= 1 .and. compare_fullnames(surnames(:, j), initials(:, j), surnames(:, i), initials(:, i)))
            j = j - 1
         enddo
         surnames(:, j+1:i) = cshift(surnames(:, j+1:i), -1, dim = 2)
         initials(:, j+1:i) = cshift(initials(:, j+1:i), -1, dim = 2)
      enddo

   end subroutine sort_fullnames
   pure logical function compare_fullnames(surname1, initials1, surname2, initials2)
      character(kind=CH_), intent(in)  :: surname1(:), initials1(:), surname2(:), initials2(:)

      compare_fullnames = .false.
      if (str1_more(surname1, surname2)) then
         compare_fullnames = .true.
      elseif (All(surname1 == surname2) .and. str1_more(initials1, initials2)) then
         compare_fullnames = .true.
      endif

   end function compare_fullnames

   pure logical function str1_more(str1, str2)
      character(kind=CH_), intent(in)  :: str1(:), str2(:)
      integer                             i

      i = 1
      do while (str1(i) == str2(i) .and. i < Min(size(str1), size(str2))-1)
         i = i+1
      enddo
      str1_more = str1(i) > str2(i)

   end function str1_more
end program lab_1_2
