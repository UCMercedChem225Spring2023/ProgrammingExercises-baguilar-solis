      Program prgm_04_01

!     This program will carry out matrix-matrix multiplication for two
!     square matrices. The user will input the leading dimension and the
!     matrices will be filled with random numbers. The CPU time for the
!     operation will be recorded.

!     Variable Declarations  

      implicit None
      Integer::NDim
      Real::Time_Start,Time_End
      Real,Dimension(:,:),Allocatable::Matrix1,Matrix2,Matrix3
      Character(Len=64)::Character_Temp
!
!     Format Statement

1000 Format(1x,'Dimension=',I6,' Time: ',F10.4,' s.')

!     Begin by reading the dimension from the command line
!
      Call Get_Command_Argument(1,Character_Temp)
      Read(Character_Temp,*) NDim

!     Allocate sizes of matrices, using dimension given by the user. 
!     Fill Matrix1 and Matrix2 with random numbers 

      Allocate(Matrix1(NDim,NDim),Matrix2(NDim,NDim))
      Call random_number(Matrix1)
      Call random_number(Matrix2)

!
!     Compute matrix product
!     
      Call cpu_time(Time_Start)
      Matrix3 = MatMul(Matrix1, Matrix2)
      Call cpu_time(Time_End)

      Write(*,1000) NDim,Time_End-Time_Start

      End Program
