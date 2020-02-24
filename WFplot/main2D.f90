!------------------------------------------------------------------------------
! $Header: /home/cvs/phsht/WFplot/main2D.f90,v 1.1 2014/01/26 14:21:30 phukhh Exp $
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! $Log: main2D.f90,v $
! Revision 1.1  2014/01/26 14:21:30  phukhh
! *** empty log message ***
!
! Revision 1.4  2008/07/07 13:07:22  phsht
! automatic ALLOCATion of menory based on file size;
! added color and BW option
!
! Revision 1.3  2008/06/13 16:46:25  phsht
! included dynamic memory allocation
!
!------------------------------------------------------------------------------

PROGRAM WFplot
  IMPLICIT NONE

  DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE::  WF

  DOUBLE PRECISION NORM,D13,FACTOR,SIZE
  DOUBLE PRECISION O1,O2,O3,X,Y
  DOUBLE PRECISION Z1,Z2,Z3,U1,U2,U3,X1,X2,X3,Y1,Y2,Y3
  DOUBLE PRECISION LX,BSIZE,BCENTER,PS_SCALE,SCALE
  REAL BWI,RI,CR,CG,CB, dummy
  INTEGER SQUARESIZE,N,I,J,K,L,NR,SMALL,EDGED,LARGE, IErr, NMAX
  CHARACTER*200 FILE,PS_FILE
  INTEGER PS_UNIT, CHOICE
  LOGICAL SET_COL
  PARAMETER(PS_UNIT=21,NMAX=1000000000)
  COMMON /COMM_CENTPROJ/ Z1,Z2,Z3,U1,U2,U3,X1,X2,X3,Y1,Y2,Y3,PS_SCALE

  !----------------------------------------------------------------------------
  PRINT*,"WFplot $Revision: 1.1 $"
  !----------------------------------------------------------------------------

  WRITE(*,*) "FILENAME OF WAVEFUNCTION="
  READ(*,'(A)') FILE
  OPEN(12,FILE=FILE,ERR=1000)

  I=0
80 READ(12,*,END=90,ERR=1002) dummy
  I=I+1
  GOTO 80
90 CLOSE(12)
  PRINT*,"WFplot: found", I, " wave function amplitudes."
  SQUARESIZE= INT(I ** (1.0/2.0))+1
  WRITE(*,*) "LINEAR SQUARE SIZE=", SQUARESIZE

  IF (SQUARESIZE**2.LT.I) THEN
     PRINT*,"ASSUMING ", SQUARESIZE**2, " data values --- CONTINUING"
  ELSE IF (SQUARESIZE**2.GT.I) THEN
     SQUARESIZE=SQUARESIZE-1
     PRINT*,"ASSUMING ", SQUARESIZE**2, " data values --- CONTINUING"
  ENDIF
  IF (SQUARESIZE.LT.1) STOP 'WRONG SQUARESIZE --- STOPPING'

  ALLOCATE(WF(SQUARESIZE**2), STAT = IErr)
  !PRINT*, "DBG: IErr=", IErr
  IF( IErr.NE.0 ) THEN
     PRINT*,"main: error in ALLOCATE() with IErr=",IErr
     STOP
  ELSE
     PRINT*,"main: memory for ", SQUARESIZE, "^2 wavefunction values allocated."
  ENDIF

  PRINT*, "Choose: COLOR [0], B/W [1]"
  READ(*,*) CHOICE

  !----------------------------------------------------------------------------

  N=SQUARESIZE**2
  NORM=0.D0
  OPEN(12,FILE=FILE,ERR=1000)
  DO I=1,N
     READ(12,*,END=1001,ERR=1002) WF(I)
     ! print *,i,wf(i)
     WF(I)=WF(I)**2
     NORM=NORM+WF(I)
  ENDDO
  
  CLOSE(12)
  !print *,'read file ready'

  !NORMALIZATION -> MEAN IS 1
  NORM=NORM/DBLE(N)
  DO I=1,N
     WF(I)=WF(I)/NORM
  ENDDO

  !----------------------------------------------------------------------------

  LX=SQUARESIZE
  PS_FILE=FILE
  PS_SCALE=100.D0
  
  CALL SET_Z(Z1,Z2,Z3,LX)
  CALL SET_PLANE(Z1,Z2,Z3,U1,U2,U3,X1,X2,X3,Y1,Y2,Y3)
  
  BCENTER=(1.d0+LX)/2.D0
  BSIZE=LX/2.D0+0.5
  CALL BOX_1(BCENTER,BSIZE,PS_UNIT,PS_FILE)
  
  CALL PS_D_LINE(PS_UNIT,10)
  
  FACTOR=1000.
  scale=11.4
  NR=0
  D13=1.D0/3.D0
  SMALL=0
  EDGED=0
  LARGE=0
!  DO J=SQUARESIZE,1,-1
  DO J=1,1,-1
     PRINT *,'--- working on plane',J

     ! setting the color

     RI=1.-REAL(J-0.999)/REAL(SQUARESIZE)
     SELECT CASE(CHOICE)
     CASE(0)
        CALL RGB_SET(RI,CR,CG,CB)
     CASE(1)
        BWI= 1+3.0*RI*(RI-1.0)
        CR= BWI; CG= BWI; CB= BWI
        PRINT*,"DBG: J,BWI", J,BWI
     END SELECT
     CALL PS_COLOR(PS_UNIT,CR,CG,CB)

     !CALL DRAW_CUBE(SQUARESIZE,J,0,0.5D0,.FALSE.,PS_UNIT,PS_FILE)
     !CALL DRAW_CUBE(SQUARESIZE,J,0,0.5D0,.FALSE.,PS_UNIT)
     SET_COL=.FALSE.
     DO I=1,SQUARESIZE
        DO K=1,SQUARESIZE
           
           NR=NR+1
           IF (WF(NR).GT.1 .and. SET_COL) &
                call ps_color(PS_UNIT,cR,cG,cB)
           
           IF (WF(NR).GT. FACTOR) THEN
              LARGE=LARGE+1
              SIZE=WF(NR)**D13/scale
              CALL DRAW_CUBE(I,J,K,SIZE,.TRUE.,PS_UNIT)
              SET_COL=.TRUE.
           ELSE IF (WF(NR).GT. DSQRT(FACTOR)) THEN
              EDGED=EDGED+1
              SIZE=WF(NR)**D13/scale
              CALL DRAW_CUBE(I,J,K,SIZE,.TRUE.,PS_UNIT)
              SET_COL=.TRUE.
           ELSE IF (WF(NR).GT. 1            ) THEN ! >MEAN
              SMALL=SMALL+1
              SIZE=WF(NR)**D13/scale
              CALL DRAW_CUBE(I,J,K,SIZE,.FALSE.,PS_UNIT)
              SET_COL=.FALSE.
           ENDIF
           
        ENDDO
     ENDDO
  ENDDO
  
  CALL BOX_2(BCENTER,BSIZE,PS_UNIT,PS_FILE)
  WRITE(*,19) N,SMALL,EDGED,LARGE
19 FORMAT(I7,' SITES:',/, I7,' SMALL',I7,' EDGED AND ',I7,' LARGE CUBES')
  
  STOP

1000 write(*,*) 'WFplot: cannot open ',file
  stop
1001 write(*,*) 'WFplot: unexpected early end of ',file
  stop
1002 write(*,*) 'WFplot: error in ',file
  stop
END PROGRAM WFplot
