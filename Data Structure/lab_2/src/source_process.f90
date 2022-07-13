module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
   
   recursive subroutine Diff_Codes(InitialCode, first, last, counter)
      type(SourceLine), pointer , intent(inout)  :: InitialCode
      type(SourceLine), pointer ::tmp 
      integer, intent(in)  :: first, last, counter
      if(Associated(InitialCode)) then
          if (counter >= first .and. counter <= last ) then  
                    tmp => InitialCode
                    InitialCode => InitialCode%next
                    deallocate(tmp)
                    call diff_codes(initialcode, first, last, counter+1)
               else 
             call diff_codes(initialcode%next, first, last, counter+1)
         end if
      end if
   end subroutine Diff_Codes

end module Source_process
