module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
  ! проход всех элементов
  recursive subroutine Diff_Str(CurrentString, DeleteList) 
      type(SourceLine),pointer  :: CurrentString
      type(SourceLine), pointer :: DeleteList
      
      if(Associated(DeleteList)) then
             call delete_str(CurrentString, DeleteList)
             call Diff_Str(CurrentString, DeleteList%next)
       end if
   end subroutine Diff_Str
  
   recursive subroutine Delete_Str(current, current_delete_str)
    type(SourceLine), pointer  :: current                ! текущий элемент спсика 
    type(SourceLine), pointer  :: current_delete_str     ! элемент списка для удаления из основного списка 
    type(SourceLine), pointer  :: remove_str
  
    if(Associated(current)) then
      if(current_delete_str%String == current%String) then ! проверка на свопадение элементов
          remove_str => current                          ! создаем ссылку на текцщий элемент
          current => current%next                        ! сдвиг элемента
          deallocate(remove_str)                            ! удаление  элемент
          call Delete_Str(current, current_delete_str)
       else
          call Delete_Str(current%next, current_delete_str) 
       end if
    end if
   end subroutine Delete_Str

end module Source_process
