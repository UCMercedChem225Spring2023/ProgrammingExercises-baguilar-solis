      Program prgm_01_03
!
!     This program reads two 3x3 matrices from user-provided input files. After the
!     files are opened and read, they are closed and then printed. The
!     products of both matrices is also computed and printed. 
!
!
      implicit none
      integer,parameter::inFileUnitA=10,inFileUnitB=11
      integer::errorFlagA,errorFlagB,i,j,k
      real,dimension(3,3)::matrixInA,matrixInB,matrixProduct
      character(len=128)::fileNameA,fileNameB
!
!
!     Start by asking the user for the name of the data files.
!
      write(*,*)' What is the name of the input data files?'
      read(*,*) fileNameA
      read(*,*) fileNameB
!
!     Open the data files and read matrixInA & matrixInB from their
!     respective file.
!
      open(unit=inFileUnitA,file=TRIM(fileNameA),status='old',  &
        iostat=errorFlagA)
      open(unit=inFileUnitB,file=TRIM(fileNameB),status='old', &
        iostat=errorFlagB)
      if(errorFlagA.ne.0 .OR. errorFlagB.ne.0) then
        write(*,*)' There was a problem opening the input files.'
        goto 999
      endIf
      
      do i = 1,3
        read(inFileUnitA,*) matrixInA(1,i),matrixInA(2,i),matrixInA(3,i)
        read(inFileUnitB,*) matrixInB(1,i),matrixInB(2,i),matrixInB(3,i)
      endDo

      close(inFileUnitA)
      close(inFileUnitB)
!
!     Call the subroutine PrintMatrix to print matrixInA.
!
      call PrintMatrix3x3(matrixInA)
      call PrintMatrix3x3(matrixInB)

!     Compute and print product of matrices
     
      matrixProduct = 0

      do i = 1,3
       do j = 1,3
        do k = 1,3      
         matrixProduct(i,j) = matrixProduct(i,j) + matrixInA(k,i) &
           * matrixInB(j,k) 
        endDo
       endDo
      endDo
        
1000 format(3(2x,f5.1))
      write(*,*) 'Printing matrix product'
      write(*,1000) transpose(matrixProduct)

  999 continue
      End Program prgm_01_03


      Subroutine PrintMatrix3x3(matrix)
!
!     This subroutine prints a 3x3 real matrix. The output is written to StdOut.
!
      implicit none
      real,dimension(3,3),intent(in)::matrix
      integer::i
!
!     Format statements.
!
 1000 format(3(2x,f5.1))
!
!     Do the printing job.
!
      write(*,*)' Printing Matrix'
!
       
      write(*,1000) matrix
       
!
!
      return
      End Subroutine PrintMatrix3x3
