!=========================================
!	This program makes the initial data
!	files from raw eta or phi files
!=========================================
!	declaring data files
!=========================================
       DIMENSION NS(1000)
       DIMENSION TH(1000,999)
       INTEGER NEV,NS
       REAL TH
       INTEGER c,N
!=========================================			
         OPEN (UNIT=1,FILE='Setot.dat')
         OPEN (UNIT=11,FILE='eta.dat')
         OPEN (UNIT=12,FILE='mul.dat')
         OPEN (UNIT=13,FILE='totmul.dat')
!=========================================   
!	reading data files
!=========================================      
         N=48  ! Enter event no.
         c=0
         DO 5 I = 1,N
         READ(1,*) NEV,NS(I)
         DO 8 J = 1,NS(I)
         READ(1,*)NA,TH(I,J)
         c=c+1
8        CONTINUE
5        CONTINUE
!=========================================
!	writing output
!=========================================
        DO  i=1,N
        write(12,*)NS(I)
        DO  J=1,NS(I)
        write(11,*)TH(I,J)
        ENDDO
        ENDDO
        write(13,*) c
!=========================================
!	End program
!=========================================
         STOP
         END
