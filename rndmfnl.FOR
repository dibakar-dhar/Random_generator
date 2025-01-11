!=============================================
! This program randomly shuffles the data
! *** remenmber before running this file  ***  
! *** you have run the "randinit" program ***
!=============================================
!	data variables and necessary files
!==========================================
      DIMENSION ANGLE(9999),KCD(999)
      OPEN(15,FILE='mul.dat')
      OPEN(7,FILE='eta.dat')
      OPEN (13,FILE='totmul.dat')
      OPEN(16,FILE='Smtot.dat')
!==========================================
      NEVNT=48.0 ! Enter event no.
      READ(15,*) (KCD(I),I=1,NEVNT)
      READ(13,*) NANG
      READ(7,*)(ANGLE(I),I=1,NANG)
      write(*,*) "Data reading complete."
!==========================================      
      J=NANG/2
      DO 12 IJ=1,10000 ! shuffel
      DO 10 I=1,J
11    R=RAND()
!==========================================
        K=IFIX(R*6549) 
!==========================================
! make this the "num" of (R*num) has same 
! same number of digits as in the totmul 
!==========================================
        IF(K.LE.J.OR.K.GT.NANG)GO TO 11
        A=ANGLE(I)
        ANGLE(I)=ANGLE(K)
        ANGLE(K)=A
10      CONTINUE
12      CONTINUE
		write(*,*) "Randomization complete."
        J1=1
        J2=KCD(1)
        DO 15 II=1,NEVNT
        WRITE(16,22)II,KCD(II)
22		format(I4,4X,I4)
        L=0.0
        DO I=J1,J2
        L=L+1
        WRITE(16,33)L,ANGLE(I)
33		format(I4,4X,f11.8)
        ENDDO
        J1=J1+KCD(II)
        J2=J1+KCD(II+1)-1
15      CONTINUE
		write(*,*) "Program complete."
!=========================================
!	End program
!=========================================
        STOP
        END

