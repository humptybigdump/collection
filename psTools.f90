 !
!> graphic tools to produce postscript pictures. They can be converted PS -> EPS by Ghostview (freeware) and then (with Acrobat Distiller) to PDF
      module psTools
      use inData
      implicit none
	  real(8) :: scales(2)           ! scales(1) for slope.ps; scales(2) for forces.ps

      contains

	  !----------------------------------------------------------------
	  subroutine psPrepareGeometryFile(title,lay)
	  implicit none
	  character(80),intent(in) :: title
	  type(layer),intent(in) ::  lay
	  real(8) :: maxDistance
        integer :: junit
	  junit = 1
        maxDistance = abs(lay%xx(1,1) - lay%xx(lay%nxx,1))
        call psSetScale(junit,maxDistance)
        open(unit=junit, file=trim(title)//'-Geometry.ps',err=99,status='REPLACE')
	  call psWriteHead(junit)
	  return
   99   stop 'I cannot open the file *-Geometry.ps'
	  end subroutine psPrepareGeometryFile
	  !----------------------------------------------------------------

	  subroutine psPrepareForcesFile(title,slices,n)
	  implicit none
	  character(80),intent(in) :: title
	  integer,intent(in) :: n
	  type(slice),intent(in) :: slices(n)
	  real(8) :: maxDistance
      integer :: junit
	  junit = 2
	  maxDistance = abs( sum(slices(:)%weight) )
        call psSetScale(junit,maxDistance)
        open(unit=junit, file=trim(title) // '-Forces.ps',err=99,status='REPLACE')
	  call psWriteHead(junit)
      return
   99 stop 'I cannot open the file *-Geometry.ps'
      end subroutine psPrepareForcesFile
	  !----------------------------------------------------------------

!> 1 unit in postscript is 0.353 mm (=1/72 in) on paper. It is
!> assumed A4 paper, portrait, and an effective plot width of
!> 14 cm, i.e. 400 units. Equal scaled in x and y directions.
      subroutine psSetScale(junit,maxDistance)
      implicit none
      integer,intent(in) :: junit
      real(8),intent(in) :: maxDistance
      real(8) :: md
      md = abs(maxDistance)
      if(md<1.0d-8) md=1
	  scales(junit) = 400.d0/md
      end subroutine psSetScale
      !----------------------------------------------------------------

      subroutine psWriteHead(junit)
      implicit none
      integer, intent(in) :: junit
	  real(8) :: s
	  s = scales(junit)
	  if(abs(s)<1.0d-8) stop 'Error in psWriteHead: scale is zero'
      write(junit,*) '%!PS-Adobe-3.0 EPSF-3.0'
      write(junit,*) '%%Creator: [generally the program that generated the postscript]'
      write(junit,*) '%%Title: [descriptive name or just the file name]'
      write(junit,*) '%%CreationDate: [date the file was created]'
      write(junit,*) '%%DocumentData: Clean7Bit'
      write(junit,*) '%%Origin: [eg: 0 0]'
      write(junit,*) '%%BoundingBox: xmin ymin xmax ymax'
      write(junit,*) '%%LanguageLevel: 2 [could be 1 2 or 3]'
      write(junit,*) '%%Pages: 1'
      write(junit,*) '%%Page: 1 1'
      write(junit,*) 100, 100, 'translate'                                 ! 1 unit in postscript is 0.353 mm (=1/72 in)
      write(junit,*) s, s, 'scale'                                         ! equal scaled in x and y directions
      end subroutine psWriteHead
      !----------------------------------------------------------------


      subroutine psDrawPoints(junit,points,npoints,orgb,olineWidth,  ocontinuous)
      implicit none
      integer, intent(in) :: junit,npoints
      real(8),intent(in) :: points(nPoints,2)
      real(8),optional :: orgb(3), olineWidth
      integer,optional :: ocontinuous
      real(8) :: rgb(3),linewidth, s
      integer :: i,continuous

      s =   scales(junit)
      if( .not. present(orgb) ) then
         rgb = (/1.0d0,0.0d0,0.0d0/)
      else
         rgb = orgb
      end if
      if( .not. present(olineWidth) ) then
         lineWidth=1.0d0
      else
         lineWidth =olineWidth
      end if
      if( .not. present(ocontinuous) ) then
         continuous = 0
      else
         continuous = ocontinuous
      end if

      do i=1,npoints
         write(junit,*) 'newpath'
         write(junit,'(5f10.3,1x,A10)') points(i,1), points(i,2), lineWidth/s  ,0.0d0, 360.d0, ' arc'
         write(junit,*) 'gsave'
         write(junit,*) 'grestore'
         write(junit,*) rgb, 'setrgbcolor'
         write(junit,*) lineWidth/s , ' setlinewidth'                                 ! the line with must be scaled tood
         write(junit,*) '[',lineWidth/s, continuous*lineWidth/s , '] 0 setdash'       ! dashed line
         write(junit,*) 'stroke'
      end do
      end subroutine psDrawPoints
      !----------------------------------------------------------------

      subroutine psDrawLines(junit,points,n,orgb,olineWidth,ocontinuous)
      implicit none
      real(8),optional:: orgb(3),olineWidth
      integer,optional :: ocontinuous
      integer, intent(in) :: junit,n
      real(8),intent(in) :: points(n ,2)
      real(8) ::  rgb(3), lineWidth, headSize, s
      integer ::  continuous
      integer :: i

      s = scales(junit)

      if( .not. present(orgb) ) then
         rgb = (/1.0d0,0.0d0,0.0d0/)
      else
         rgb = orgb
      end if
      if( .not. present(olineWidth) ) then
         lineWidth=1.0d0
      else
         lineWidth =olineWidth
      end if
      if( .not. present(ocontinuous) ) then
         continuous = 0
      else
         continuous = ocontinuous
      end if

      write(junit,*) 'newpath'
      write(junit,*) points(1,1) ,points(1,2), ' moveto'
      do i=2, n
        write(junit,*) points(i,1), points(i,2), ' lineto'
      end do
      write(junit,*)  rgb, 'setrgbcolor'                                 ! blue in RGB
      write(junit,*) lineWidth/s   , ' setlinewidth'                       ! the line with must be scaled too
      write(junit,*) '[',lineWidth/s   , continuous*lineWidth/s  ,    '] 0 setdash'   ! dashed line
      write(junit,*) 'stroke'

      end subroutine psDrawLines
      !----------------------------------------------------------------

      subroutine psArrow(junit,pt1,pt2,oheadSize,orgb,olineWidth, ocontinuous)
      use globalBlock
      implicit none
      real(8),optional:: orgb(3),olineWidth,oheadSize
      integer,optional :: ocontinuous
      integer, intent(in) :: junit
      real(8),intent(in) :: pt1(2),pt2(2)
      real(8) ::  rgb(3), lineWidth, headSize
      integer ::  continuous
      real(8) :: tb(2), pt3(2),pt4(2),points(3,2)
      integer :: i,n

      headSize = 5.0d0/scales(junit)
      if(  present(oheadSize) )  headSize = oheadSize;
      points(1,:) = pt1 ;  points(2,:) = pt2
      call psDrawLines(junit,points(1:2,:),2,orgb,olineWidth, ocontinuous)
      tb=normalized(pt2-pt1)
      pt3  = pt2 + headSize*rotateAlphaCCW(-tb,-30.0d0   )
      pt4  = pt2 + headSize*rotateAlphaCCW(-tb,30.0d0   )
      points(1,:) = pt3 ; points(2,:) = pt2 ; points(3,:) = pt4
      call psDrawLines(junit,points,3,orgb,olineWidth,  ocontinuous)

      end subroutine psArrow
      !----------------------------------------------------------------


      subroutine psDrawText(junit,point,text, orgb,ofontSize)
      implicit none
      integer, intent(in) :: junit
      real(8), intent(in) :: point(2)
      integer, optional :: ofontSize
      real(8), optional::orgb(3)
      character (256),intent(in) :: text
      integer :: fontSize
      real(8) :: rgb(3),s

      s = scales(junit )
      rgb = [0.0d0,0.0d0,0.0d0] ; if(  present(orgb) ) rgb = orgb
      fontsize= 9; if(  present(ofontSize) )  fontsize = ofontsize

      write(junit,*) '/Helvetica findfont'
      write(junit,*) fontSize/s , ' scalefont'
      write(junit,*) 'setfont'
      write(junit,'(3g16.8,A)') rgb, 'setrgbcolor'                                   ! black
      write(junit,*) 'newpath'
      write(junit,*) point(1) ,point(2), 'moveto'                                  ! position of text
      write(junit,'(3A)')   '(', trim(text),')  show'

      end subroutine psDrawText
      !----------------------------------------------------------------


      subroutine psDrawSliceForces(junit,start,s,islice, orgb,ofontSize,oheadSize)
      implicit none
      integer, intent(in) :: junit,islice
      type(slice),intent(in) :: s
      real(8),intent(inout) :: start(2)
      integer, optional ::ofontSize
      real(8),optional :: orgb(3),oheadSize
      integer, parameter :: nforces=4
      real(8) :: forces(nforces,2),ptEnd(2),rgb(3),midpt(2)
      integer :: i,fontsize
      character(3) :: tags(nforces)
      character(256) :: text, aux

      forces(1,:) = s%weight*[0.0d0,-1.0d0]
      forces(2,:) = -s%ER*s%ebR
      forces(3,:) = s%cohesion*s%tangents(1,:)
      forces(4,:) = s%Q*s%qb

      tags(:) = ['W  ','-Er','C  ', 'Q  ']

      do i=1,nforces
            ptEnd = start + forces(i,:)
            call psArrow(junit,start,ptEnd,  oheadSize, orgb)
          midpt = (start + ptEnd)/2.0d0
          write(aux,'(I4)')  iSlice
          text = trim(tags(i)) // adjustl(aux)
            call psDrawText(junit,midpt,text, orgb,ofontSize)
            start = ptEnd
      end do

      end subroutine psDrawSliceForces
      !----------------------------------------------------------------

      subroutine psDrawPolygonOfForces(junit,slices,nSlices, orgb,ofontSize,oheadSize)
      implicit none
      integer, intent(in) :: junit,nSlices
      type(slice),intent(in) :: slices(nSlices)
      integer, optional ::ofontSize
      real(8),optional :: orgb(3),oheadSize
      real(8) :: start(2), dx,point(2)
      character(256) :: text
      integer :: i

      dx = sum(slices(:)%weight)
      if(Abs(dx)<1.0d-8) dx=1

      start = [dx/2,dx]
      point = start + [-0.4d0*dx/2, 0.1d0*dx]
      write(text,'(A,g16.8)') 'Factor of safety = ',slices(1)%F
      call psDrawText(junit,point,text, [1.0d0,0.0d0,0.0d0],11)

      point = point + [0.0d0, -0.05d0*dx]
	  call psArrow(junit,point,point+[100,0],  oheadSize, orgb)
	  write(text,'(A4)') '100'
	  call psDrawText(junit,point+[100,0],text, [1.0d0,0.0d0,0.0d0],11)

      do i=1,nSlices
         call psDrawSliceForces(junit,start,slices(i),i, orgb, ofontSize,oheadSize)
          start=start + slices(i)%EL*slices(i)%ebL  + slices(i)%weight*[0.0d0,-1.0d0]
      enddo

      end subroutine psDrawPolygonOfForces
      !----------------------------------------------------------------

      subroutine psDrawSlope(jUnit,layers,nLayers,slices,nSlices,o)
      implicit none
      integer, intent(in) :: nLayers,nSlices,jUnit
      type(layer),intent(in) :: layers(nLayers)
      type(slice),intent(in) :: slices(nSlices)
      type(controls),intent(in) :: o
      real(8) :: rgb(3),line(2,2),xs,ys
      integer :: n,i

      n = layers(1)%nxx
      rgb = [0,1,0]
      call psDrawLines(junit,layers(1)%xx(1:n,:),n, rgb)
      rgb = [1,0,0]
      call psDrawPoints(junit,o%xx,1)

      rgb = [0,0,1]
      do i=1,nSlices
        xs = slices(i)%points(3,1)
        ys = (slices(i)%points(3,2) + slices(i)%points(2,2))/2
        line(1,:) = [xs - slices(i)%ebR(1),ys-slices(i)%ebR(2) ]
        line(2,:) = [xs,ys]
        call psDrawLines(junit,line,2,rgb)
        call psDrawLines(junit,slices(i)%points,4)
      end do

      end subroutine psDrawSlope
      !----------------------------------------------------------------

      end module psTools

