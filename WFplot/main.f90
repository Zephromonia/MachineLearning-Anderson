!------------------------------------------------------------------------------
! $Header: /home/cvs/phsht/WFplot/main.f90,v 1.6 2019/09/18 10:14:16 phsht Exp $
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! $Log: main.f90,v $
! Revision 1.6  2019/09/18 10:14:16  phsht
! removed some of the PRINT messages
!
! Revision 1.5  2019/09/18 09:56:03  phsht
! added maqkefile.GF to work with standard gfortran;
! added FRAME input;
! switched COLOR to 1 and B/W to 0
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
  INTEGER CUBESIZE,N,I,J,K,L,NR,SMALL,EDGED,LARGE, IErr, NMAX
  CHARACTER*200 FILE,PS_FILE
  INTEGER PS_UNIT, CHOICE, FRAME
  LOGICAL SET_COL, LOG
  PARAMETER(PS_UNIT=21,NMAX=1000000000,LOG=.FALSE.)
  COMMON /COMM_CENTPROJ/ Z1,Z2,Z3,U1,U2,U3,X1,X2,X3,Y1,Y2,Y3,PS_SCALE

  !----------------------------------------------------------------------------
  PRINT*,"WFplot $Revision: 1.6 $"
  !----------------------------------------------------------------------------

  WRITE(*,*) "FILENAME OF WAVEFUNCTION="
  READ(*,'(A)') FILE
  OPEN(12,FILE=FILE,ERR=1000)

  I=0
80 READ(12,*,END=90,ERR=1002) dummy
  I=I+1
  GOTO 80
90 CLOSE(12)

  IF(LOG) PRINT*,"WFplot: found", I, " wave function amplitues."
  CUBESIZE= INT(I ** (1.0/3.0))+1
  WRITE(*,*) "LINEAR CUBE SIZE=", CUBESIZE

  IF (CUBESIZE**3.LT.I) THEN
     PRINT*,"ASSUMING ", CUBESIZE**3, " data values --- CONTINUING"
  ELSE IF (CUBESIZE**3.GT.I) THEN
     CUBESIZE=CUBESIZE-1
     PRINT*,"ASSUMING ", CUBESIZE**3, " data values --- CONTINUING"
  ENDIF
  IF (CUBESIZE.LT.1) STOP 'WRONG CUBESIZE --- STOPPING'

  ALLOCATE(WF(CUBESIZE**3), STAT = IErr)
  !PRINT*, "DBG: IErr=", IErr
  IF( IErr.NE.0 ) THEN
     PRINT*,"main: error in ALLOCATE() with IErr=",IErr
     STOP
  ELSE
     PRINT*,"main: memory for ", CUBESIZE, "^3 wavefunction values allocated."
  ENDIF

  PRINT*, "Choose: B/W [0], COLOR [1]"
  READ(*,*) CHOICE

  PRINT*, "Choose: NO FRAME [0], FRAME [1]"
  READ(*,*) FRAME

  !----------------------------------------------------------------------------

  N=CUBESIZE**3
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

  LX=CUBESIZE
  PS_FILE=FILE
  PS_SCALE=100.D0
  
  CALL SET_Z(Z1,Z2,Z3,LX)
  CALL SET_PLANE(Z1,Z2,Z3,U1,U2,U3,X1,X2,X3,Y1,Y2,Y3)
  
  BCENTER=(1.d0+LX)/2.D0
  BSIZE=LX/2.D0+0.5

  CALL BOX_0(BCENTER,BSIZE,PS_UNIT,PS_FILE)
  
  IF(FRAME==1) CALL BOX_1(BCENTER,BSIZE,PS_UNIT,PS_FILE)
  
  CALL PS_D_LINE(PS_UNIT,10)
  
  FACTOR=1000.
  scale=11.4
  NR=0
  D13=1.D0/3.D0
  SMALL=0
  EDGED=0
  LARGE=0
  DO J=CUBESIZE,1,-1
     IF(LOG) PRINT *,'--- working on plane',J

     ! setting the color

     RI=1.-REAL(J-0.999)/REAL(CUBESIZE)
     SELECT CASE(CHOICE)
     CASE(1)
        CALL RGB_SET(RI,CR,CG,CB)
     CASE(0)
        BWI= 1+3.0*RI*(RI-1.0)
        CR= BWI; CG= BWI; CB= BWI
        !PRINT*,"DBG: J,BWI", J,BWI
     END SELECT
     CALL PS_COLOR(PS_UNIT,CR,CG,CB)

     !CALL DRAW_CUBE(CUBESIZE,J,0,0.5D0,.FALSE.,PS_UNIT,PS_FILE)
     IF(FRAME==1) CALL DRAW_CUBE(CUBESIZE,J,0,0.5D0,.FALSE.,PS_UNIT)
     SET_COL=.FALSE.
     DO I=1,CUBESIZE
        DO K=1,CUBESIZE
           
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
  
  IF(FRAME.EQ.1) CALL BOX_2(BCENTER,BSIZE,PS_UNIT,PS_FILE)
  
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
