module Fullname_Process    
   use Environment
   use Group_IO         
   implicit none           

contains                   
   pure recursive subroutine Sort_fullnames(List, lastSorted)
      type(student), pointer ::  List, lastSorted, toPlace
      if (Associated(lastSorted%next)) then
         if (lastSorted%next%surname//lastSorted%next%initials < lastSorted%surname//lastSorted%initials) then
            toPlace => lastSorted%next      
            lastSorted%next => lastSorted%next%next
            call Paste(List, toPlace)
            call Sort_fullnames(List, lastSorted)
         else              
            call Sort_fullnames(List, lastSorted%next)
         endif
      endif
   end subroutine Sort_fullnames   
    
   pure recursive subroutine Paste(current, itemToInsert)
      type(student), pointer :: current, itemToInsert
      if (itemToInsert%surname//itemToInsert%initials < current%surname//current%initials) then
         itemToInsert%next => current    
         current           => itemToInsert
      else
         call Paste(current%next, itemToInsert)
      endif
   end subroutine Paste    
          
end module Fullname_Process
