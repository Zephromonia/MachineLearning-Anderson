
!------------------------------------------------------------------------------
! $Header: /home/cvs/phsht/WFplot/util.f90,v 1.3 2019/09/18 10:14:16 phsht Exp $
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! $Log: util.f90,v $
! Revision 1.3  2019/09/18 10:14:16  phsht
! removed some of the PRINT messages
!
! Revision 1.2  2019/09/18 09:56:03  phsht
! added maqkefile.GF to work with standard gfortran;
! added FRAME input;
! switched COLOR to 1 and B/W to 0
!
! Revision 1.1  2008/06/13 16:46:37  phsht
! German -> English
!
!------------------------------------------------------------------------------

SUBROUTINE RGB_SET(V,R,G,B)
  IMPLICIT NONE
  REAL V,R,G,B,V6
  INTEGER IV
  V6=V*6.
  IV=INT(V6)
  IF      (IV.EQ.0) THEN
     R=0.95
     G=0.
     B=0.95*MOD(V6,1.)
  ELSE IF (IV.EQ.1) THEN
     R=0.95*(1.-MOD(V6,1.))
     G=0.
     B=0.95
  ELSE IF (IV.EQ.2) THEN
     R=0.
     G=0.95*MOD(V6,1.)
     B=0.95
  ELSE IF (IV.EQ.3) THEN
     R=0.
     G=0.95
     B=0.95*(1.-MOD(V6,1.))
  ELSE IF (IV.EQ.4) THEN
     R=0.95*MOD(V6,1.)
     G=0.95
     B=0.
  ELSE
     R=0.95
     G=0.95*(1.-MOD(V6,1.))
     B=0.
  ENDIF
  ! PRINT *,V,IV,R,G,B
END SUBROUTINE RGB_SET

SUBROUTINE DRAW_CUBE(IX,IY,IZ,SIZE,EDGED,PS_UNIT)
  IMPLICIT NONE
  DOUBLE PRECISION X,Y,Z,SIZE
  INTEGER PS_UNIT,I,IP(8,2),IX,IY,IZ
  LOGICAL EDGED
  
  X=DBLE(IX)
  Y=DBLE(IY)
  Z=DBLE(IZ)
  
  CALL CENTPROJ(X-SIZE,Y-SIZE,Z-SIZE,IP(1,1),IP(1,2))
  CALL CENTPROJ(X+SIZE,Y-SIZE,Z-SIZE,IP(2,1),IP(2,2))
  CALL CENTPROJ(X+SIZE,Y+SIZE,Z-SIZE,IP(3,1),IP(3,2))
  CALL CENTPROJ(X+SIZE,Y+SIZE,Z+SIZE,IP(4,1),IP(4,2))
  CALL CENTPROJ(X-SIZE,Y+SIZE,Z+SIZE,IP(5,1),IP(5,2))
  CALL CENTPROJ(X-SIZE,Y-SIZE,Z+SIZE,IP(6,1),IP(6,2))
  IP(7,1)=IP(1,1)
  IP(7,2)=IP(1,2)
  CALL ps_poly(PS_UNIT,IP(1,1),IP(1,2),7,.TRUE.)
  
  IF (.NOT.EDGED) RETURN
  call ps_color(PS_UNIT,0.,0.,0.)
  CALL ps_poly(PS_UNIT,IP(1,1),IP(1,2),7,.FALSE.)
  
  CALL CENTPROJ(X+SIZE,Y-SIZE,Z+SIZE,IP(7,1),IP(7,2))
  call ps_line(PS_UNIT,IP(2,1),IP(2,2),IP(7,1),IP(7,2))
  call ps_line(PS_UNIT,IP(4,1),IP(4,2),IP(7,1),IP(7,2))
  call ps_line(PS_UNIT,IP(6,1),IP(6,2),IP(7,1),IP(7,2))
END SUBROUTINE DRAW_CUBE

SUBROUTINE BOX_0(C,SIZE,PS_UNIT,PS_FILE)
  IMPLICIT NONE
  DOUBLE PRECISION C,SIZE
  INTEGER BBL(2),BBH(2)
  INTEGER PS_UNIT,I,IP(8,2)
  CHARACTER*(*) PS_FILE
  
  CALL CENTPROJ(C+SIZE,C-SIZE,C-SIZE,IP(1,1),IP(1,2))
  CALL CENTPROJ(C+SIZE,C+SIZE,C-SIZE,IP(2,1),IP(2,2))
  CALL CENTPROJ(C-SIZE,C+SIZE,C-SIZE,IP(3,1),IP(3,2))
  CALL CENTPROJ(C-SIZE,C-SIZE,C-SIZE,IP(4,1),IP(4,2))
  CALL CENTPROJ(C+SIZE,C-SIZE,C+SIZE,IP(5,1),IP(5,2))
  CALL CENTPROJ(C+SIZE,C+SIZE,C+SIZE,IP(6,1),IP(6,2))
  CALL CENTPROJ(C-SIZE,C+SIZE,C+SIZE,IP(7,1),IP(7,2))
  CALL CENTPROJ(C-SIZE,C-SIZE,C+SIZE,IP(8,1),IP(8,2))
  BBL(1)=IP(1,1)
  BBL(2)=IP(1,2)
  BBH(1)=IP(1,1)
  BBH(2)=IP(1,2)
  DO I=2,8
     BBL(1)=MIN(IP(I,1),BBL(1))
     BBL(2)=MIN(IP(I,2),BBL(2))
     BBH(1)=MAX(IP(I,1),BBH(1))
     BBH(2)=MAX(IP(I,2),BBH(2))
  ENDDO
  CALL ps_open(PS_UNIT,PS_FILE,bbL,bbH)
!!$  call ps_color(PS_UNIT,0.,0.,0.)
!!$  CALL PS_D_LINE(PS_UNIT,8)
!!$  call ps_line(PS_UNIT,IP(3,1),IP(3,2),IP(2,1),IP(2,2))
!!$  call ps_line(PS_UNIT,IP(3,1),IP(3,2),IP(4,1),IP(4,2))
!!$  call ps_line(PS_UNIT,IP(3,1),IP(3,2),IP(7,1),IP(7,2))
END SUBROUTINE BOX_0

SUBROUTINE BOX_1(C,SIZE,PS_UNIT,PS_FILE)
  IMPLICIT NONE
  DOUBLE PRECISION C,SIZE
  INTEGER BBL(2),BBH(2)
  INTEGER PS_UNIT,I,IP(8,2)
  CHARACTER*(*) PS_FILE
  
  CALL CENTPROJ(C+SIZE,C-SIZE,C-SIZE,IP(1,1),IP(1,2))
  CALL CENTPROJ(C+SIZE,C+SIZE,C-SIZE,IP(2,1),IP(2,2))
  CALL CENTPROJ(C-SIZE,C+SIZE,C-SIZE,IP(3,1),IP(3,2))
  CALL CENTPROJ(C-SIZE,C-SIZE,C-SIZE,IP(4,1),IP(4,2))
  CALL CENTPROJ(C+SIZE,C-SIZE,C+SIZE,IP(5,1),IP(5,2))
  CALL CENTPROJ(C+SIZE,C+SIZE,C+SIZE,IP(6,1),IP(6,2))
  CALL CENTPROJ(C-SIZE,C+SIZE,C+SIZE,IP(7,1),IP(7,2))
  CALL CENTPROJ(C-SIZE,C-SIZE,C+SIZE,IP(8,1),IP(8,2))
  BBL(1)=IP(1,1)
  BBL(2)=IP(1,2)
  BBH(1)=IP(1,1)
  BBH(2)=IP(1,2)
  DO I=2,8
     BBL(1)=MIN(IP(I,1),BBL(1))
     BBL(2)=MIN(IP(I,2),BBL(2))
     BBH(1)=MAX(IP(I,1),BBH(1))
     BBH(2)=MAX(IP(I,2),BBH(2))
  ENDDO
!!$  CALL ps_open(PS_UNIT,PS_FILE,bbL,bbH)
  call ps_color(PS_UNIT,0.,0.,0.)
  CALL PS_D_LINE(PS_UNIT,8)
  call ps_line(PS_UNIT,IP(3,1),IP(3,2),IP(2,1),IP(2,2))
  call ps_line(PS_UNIT,IP(3,1),IP(3,2),IP(4,1),IP(4,2))
  call ps_line(PS_UNIT,IP(3,1),IP(3,2),IP(7,1),IP(7,2))
END SUBROUTINE BOX_1

SUBROUTINE BOX_2(C,SIZE,PS_UNIT,PS_FILE)
  IMPLICIT NONE
  DOUBLE PRECISION C,SIZE
  INTEGER PS_UNIT,I,IP(8,2)
  CHARACTER*(*) PS_FILE
  
  CALL CENTPROJ(C+SIZE,C+SIZE,C-SIZE,IP(1,1),IP(1,2))
  CALL CENTPROJ(C+SIZE,C-SIZE,C-SIZE,IP(2,1),IP(2,2))
  CALL CENTPROJ(C-SIZE,C-SIZE,C-SIZE,IP(3,1),IP(3,2))
  CALL CENTPROJ(C-SIZE,C-SIZE,C+SIZE,IP(4,1),IP(4,2))
  CALL CENTPROJ(C-SIZE,C+SIZE,C+SIZE,IP(5,1),IP(5,2))
  CALL CENTPROJ(C+SIZE,C+SIZE,C+SIZE,IP(6,1),IP(6,2))
  CALL CENTPROJ(C+SIZE,C-SIZE,C+SIZE,IP(7,1),IP(7,2))
  IP(8,1)=IP(4,1)
  IP(8,2)=IP(4,2)
  
  call ps_color(PS_UNIT,0.,0.,0.)
  CALL PS_D_LINE(PS_UNIT,14)
  CALL ps_poly(PS_UNIT,IP(1,1),IP(1,2),8,.FALSE.)
  call ps_line(PS_UNIT,IP(1,1),IP(1,2),IP(6,1),IP(6,2))
  call ps_line(PS_UNIT,IP(2,1),IP(2,2),IP(7,1),IP(7,2))
  
  CALL ps_close(PS_UNIT)
END SUBROUTINE BOX_2

SUBROUTINE CENTPROJ(O1,O2,O3,IX,IY)
  IMPLICIT NONE
  DOUBLE PRECISION O1,O2,O3,X,Y
  INTEGER IX,IY
  DOUBLE PRECISION Z1,Z2,Z3,U1,U2,U3,X1,X2,X3,Y1,Y2,Y3,PS_SCALE
  COMMON /COMM_CENTPROJ/ Z1,Z2,Z3,U1,U2,U3,X1,X2,X3,Y1,Y2,Y3,PS_SCALE
  
  X=-((-o3*u2+o2*u3)*y1 + (o3*u1-o1*u3)*y2  + (-o2*u1+o1*u2)*y3 &
       +(-o3*y2+u3*y2+o2*y3-u2*y3)*z1 + (o3*y1-u3*y1-o1*y3+u1*y3)*z2 &
       +(u2*y1-o2*y1+o1*y2-u1*y2)*z3)/ &
       ((o2*x3-o3*x2)*y1 + (o3*x1-o1*x3)*y2 + (o1*x2-o2*x1)*y3 &
       +(x3*y2-x2*y3)*z1 + (x1*y3-x3*y1)*z2 + (x2*y1-x1*y2)*z3) 
  Y=-((o2*u3-o3*u2)*x1 + (o3*u1-o1*u3)*x2 + (o1*u2-o2*u1)*x3 &
       +(-o3*x2+u3*x2+o2*x3-u2*x3)*z1 + (o3*x1-u3*x1-o1*x3+u1*x3)*z2 &
       +(u2*x1-o2*x1+o1*x2-u1*x2)*z3)/ &
       ((o3*x2-o2*x3)*y1 + (o1*x3-o3*x1)*y2 + (o2*x1-o1*x2)*y3 &
       +(x2*y3-x3*y2)*z1 + (x3*y1-x1*y3)*z2 + (x1*y2-x2*y1)*z3) 
  
  IX=INT(PS_SCALE*X)
  IY=INT(PS_SCALE*Y)
  
  !PRINT *,'O ',O1,O2,O3
  !PRINT *,X,Y
END SUBROUTINE CENTPROJ

SUBROUTINE SET_Z(Z1,Z2,Z3,LX)
  IMPLICIT NONE
  DOUBLE PRECISION Z1,Z2,Z3,RX,RY,RZ,LX
  !PRINT *,'RX,RY,RZ'
  !READ(*,*) RX,RY,RZ
  RX=2.
  RY=-3.5
  RZ=3.3
  Z1=RX*LX
  Z2=RY*LX
  Z3=RZ*LX
  !PRINT *,'Z ',Z1,Z2,Z3
END SUBROUTINE SET_Z

SUBROUTINE SET_PLANE(Z1,Z2,Z3,U1,U2,U3,X1,X2,X3,Y1,Y2,Y3)
  IMPLICIT NONE
  DOUBLE PRECISION Z1,Z2,Z3
  DOUBLE PRECISION U1,U2,U3,X1,X2,X3,Y1,Y2,Y3
  DOUBLE PRECISION NORM
  !u=-z
  NORM=DSQRT(Z1*Z1+Z2*Z2+Z3*Z3)
  U1=-Z1/NORM
  U2=-Z2/NORM
  U3=-Z3/NORM
  !x perpendicular to u and Ez, normalized
  X1= U2
  X2=-U1
  X3=0.d0
  NORM=DSQRT(X1*X1+X2*X2+X3*X3)
  X1=X1/NORM
  X2=X2/NORM
  X3=X3/NORM
  !y perpendicular to x and u, normalized
  Y1=X2*U3-X3*U2
  Y2=X3*U1-X1*U3
  Y3=X1*U2-X2*U1
  NORM=DSQRT(Y1*Y1+Y2*Y2+Y3*Y3)
  Y1=Y1/NORM
  Y2=Y2/NORM
  Y3=Y3/NORM
  !PRINT *,'X ',X1,X2,X3
  !PRINT *,'Y ',Y1,Y2,Y3
  !PRINT *,'U ',U1,U2,U3
END SUBROUTINE SET_PLANE

!------------ open the PS file-----------
subroutine ps_open(iunit,file,bbl,bbh)
  implicit none
  integer iunit,len
  character*(*) file
  character*200 psfile
  INTEGER bbl(2),bbh(2)
  REAL SCALE,BXL,BXH,BYL,BYH,BX,BY
  SCALE=0.05
  BXL=SCALE*REAL(BBL(1))
  BYL=SCALE*REAL(BBL(2))
  BXH=SCALE*REAL(BBH(1))
  BYH=SCALE*REAL(BBH(2))
  BX=0.05*(BXH-BXL)
  BY=0.05*(BYH-BYL)
  BXL=BXL-BX
  BXH=BXH+BX
  BYL=BYL-BY
  BYH=BYH+BY
  
  !print *,'psopen() ',file,IUNIT
  len=index(file,' ')
  psfile=file(1:len-1)//'.eps'
  open(iunit,file=psfile(1:len+4),err=999)
  write(iunit,10) BXL,BYL,BXH,BYH,SCALE,SCALE
10 format( '%!PS-Adobe-2.0 EPSF-2.0' &
        /'%%Creator: FraMi' &
        /'%%DocumentFonts: Helvetica' &
        /'%%BoundingBox: ',4(F9.3,1X), &
        /'%%EndComments' &
        /'/m {moveto} bind def' &
        /'/l {lineto} bind def' &
        /'/s {setlinewidth} bind def' &
        /'/e {stroke} bind def' &
        /'/c {setrgbcolor} bind def' &
        /'/f {eofill} bind def' &
        /'0 0 translate' &
        /,2(F5.3,1X),'scale' &
        /'0 setgray' &
        /'newpath')      
  return
999 print *,'cannot open ',psfile
  stop
end subroutine ps_open

!------------close PS file-----------
subroutine ps_close(iunit)
  implicit none
  integer iunit
  write(iunit,10)
10 format('e' &
        /'showpage')
  close(iunit)
end subroutine ps_close

!------------set line width-----------
subroutine ps_D_line(iunit,width)
  implicit none
  integer iunit,width
  write(iunit,10) width
10 format(i3,' s')
end subroutine ps_D_line

!------------draw line-----------
subroutine ps_line(iunit,x0,y0,x1,y1)
  implicit none
  integer iunit,x0,y0,x1,y1
  write(iunit,10) x0,y0,x1,y1
10 format(2(i6,1x),'m ',2(i6,1x),'l e')
end subroutine ps_line

!------------ draw poly-----------
subroutine ps_poly(iunit,x,y,n,FILL)
  implicit none
  integer iunit,x(*),y(*),n,i,width
  LOGICAL FILL
  write(iunit,10) x(1),y(1)
  do i=2,n
     if (i.eq.n) then
        IF (FILL) THEN
           write(iunit,13) x(i),y(i)
        ELSE
           write(iunit,12) x(i),y(i)
        ENDIF
     else
        write(iunit,11) x(i),y(i)
     endif
  ENDDO
10 format(2(i6,1x),'m ',$)
11 format(2(i6,1x),'l ',$)
12 format(2(i6,1x),'l e')
13 format(2(i6,1x),'l f e')
end subroutine ps_poly

!------------ change color-----------
subroutine ps_color(iunit,r,g,b)
  implicit none
  integer iunit
  real r,g,b
  IF (R.EQ.0. .AND. G.EQ.0. .AND. B.EQ.0.) THEN
     write(iunit,11)  
  ELSE
     if(r.gt.0.999)r=.999
     if(g.gt.0.999)g=.999
     if(b.gt.0.999)b=.999
     write(iunit,10) r,g,b
  ENDIF
10 format(3(f4.3,1x),'c')
11 format('0 0 0 c')
end subroutine ps_color
