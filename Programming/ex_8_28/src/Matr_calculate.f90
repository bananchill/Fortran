module Matr_calculate 
   use Environment

   implicit none

contains
    subroutine mNormaMat(I, sumNormM)
      real(R_), intent(inout)   :: I(:, :), sumNormM
      real(R_)   summ(Ubound(I,2)-1), AbsMass(Ubound(I, 1), Ubound(I, 2))
      integer j,k
      sumNormM = 0 
      AbsMass = abs(I)
      do j=1,Ubound(AbsMass,2)
            summ(:) = AbsMass(:,j) + summ(:)
      end do
      print *, summ
      do j = 1, Size(summ)
         if(summ(j) > sumNormM) &
            sumNormM = summ(j)
      end do
   end subroutine mNormaMat
   
  pure subroutine kNormaMat(I, sumNormK)
      real(R_), intent(out)   :: I(:, :), sumNormK
      real(R_) summ, AbsMass(Ubound(I,1), Ubound(I, 2))
      integer j, k
      
      AbsMass = abs(I)
      do j=1, Ubound(I,2)
         summ = 0
         do k = 1,Ubound(I, 1)
            summ = summ + AbsMass(k, j)
         end do
         if(summ > sumNormK ) &
            sumNormK = summ
      end do
   end subroutine kNormaMat
   
   pure subroutine iNormaMat(I, summ)
      real(R_), intent(out)   :: I(:, :), summ
      real(R_)                I_POW(Ubound(I,1), Ubound(I,2))
      integer                :: j, k
      
      I_POW = I**2
      do j=1,Ubound(I, 2)
         do k=1,Ubound(I,1)
            summ = summ + I_POW(k,j)
         end do 
      end do 
      summ = sqrt(summ)

   end subroutine iNormaMat
   
end module Matr_calculate 
