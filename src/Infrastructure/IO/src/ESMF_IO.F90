! $Id: ESMF_IO.F90,v 1.1 2004/04/27 20:41:31 slswift Exp $
!-------------------------------------------------------------------------
!
! ESMF IO module
!
! This code covered by the GNU public license.  See licence file for details.
! NCAR, 2002.
!

!-------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the read/write functions for the
! ESMF data types. 
!
! See the ESMF Developers Guide document for more details.
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!
!
!-------------------------------------------------------------------------

! module definition

module ESMF_IOMod

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_IOWritemod
!
! !USES:
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_FieldMod
      use ESMF_FieldCommMod
      use ESMF_DELayoutMod    ! ESMF layout class
      use ESMF_ArrayMod
      use ESMF_ArraySpecMod
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_GridTypesMod   ! ESMF basic grid types and primitives
      use wrf_data
      implicit none

! !PUBLIC TYPES:
      private
    
          
   
! !DESCRIPTION:
!     The following routines apply to general I/O characteristics.

! !PUBLIC MEMBER TYPES:

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_IOWrite
!     public ESMF_IORead
      
!EOP

!-------------------------------------------------------------------------

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_IOWrite

! !PRIVATE MEMBER FUNCTIONS:

!        module procedure ESMF_IOWriteBundle
         module procedure ESMF_IOWriteField
!        module procedure ESMF_IOWriteArray
! !DESCRIPTION:
!     This interface provides a single entry point for the {\tt ESMF\_IOWrite} 
!     methods.

! !INTERFACE:
!     interface ESMF_IORead

! !PRIVATE MEMBER FUNCTIONS:

!        module procedure ESMF_IOReadBundle
!        module procedure ESMF_IOReadField
!        module procedure ESMF_IOReadArray
! !DESCRIPTION:
!     This interface provides a single entry point for the {\tt ESMF\_IORead} 
!     methods.

!EOP
      end interface
!
!------------------------------------------------------------------------------

       contains

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_IOWriteField - write out an {\tt ESMF_Field}
!
! !INTERFACE:
      subroutine ESMF_IOWriteField(field, iospec, rc)
!
! !PARAMETERS:
      type (ESMF_Field), intent(in) :: field 
      type (ESMF_IOSpec) :: iospec
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!   Call the {\tt IOFormat} dependent routine to output an {\tt ESMF_Field}.

!
! !REQUIREMENTS: 

!EOP

      include 'mpif.h'

      type (ESMF_Grid) :: grid
      type (ESMF_IOFileFormat) :: fileformat
      character(len=ESMF_MAXSTR) :: filename
      type(ESMF_DELayout) :: layoutTop
      type(ESMF_DistGrid) :: distgrid
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Array) :: array_temp
      type(ESMF_Field) :: field_temp
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: u2
      character(len=ESMF_MAXSTR) :: fieldname
      character(len=ESMF_MAXSTR) :: distname

! Temporary prototype variables.
      include 'wrf_status_codes.h'
      include 'netcdf.inc'
      integer Comm
      integer IOComm
      character (80) SysDepInfo
      integer     :: DataHandle
      integer Status
      
      integer Dom
      character*3 MemOrd
      character*3 Stagger
      character*31, dimension(3) :: DimNames
      character (19) Date
      character (19) Date2
      integer , Dimension(2) :: Dom2S,Dom2E,Mem2S,Mem2E,Pat2S,Pat2E
      
      integer, parameter ::    pad = 3  
      integer, parameter ::    jds=1       , jde=6      , &
           ids=1       , ide=9
      integer, parameter ::    jms=jds-pad , jme=jde+pad , &
           ims=ids-pad , ime=ide+pad
      integer, parameter ::    jps=jds     , jpe=jde    , &
           ips=ids     , ipe=ide

      integer span(2)
      integer starts(2,1)
      
      Date = '2000-09-18_16:42:01'
      Date2 = '2000-09-18_16:52:01'
      
! Initialize the output stream.
      call ext_ncd_ioinit(Status)
      print *,'After call ext_ncd_ioinit, Status =',Status
      

      call ESMF_IOSpecGet(iospec=iospec, filename=filename, iofileformat=fileformat, rc=Status)

! This is the only option for file format right now.
!!$  if (fileformat%iofileformat /= ESMF_IO_FILEFORMAT_NETCDF%iofileformat) then
!!$     rc = ESMF_Failure
!!$     return
!!$  endif

!!$filename = 'foo.nc'

! For now we set the communicator to be MPI_COMM_WORLD.  Eventually,
! we will pull it from the DE.  
! We are also not taking advantage of the ability to have a different
! communicator just for IO.  So we set the IO communicator to that of
! the field DE.
      Comm = MPI_COMM_WORLD
      IOComm = Comm
      
  
      Stagger = ''
      SysDepInfo = 'sys info'
      
      field_temp = field ! work on a copy of the field rather than the original

! Until I know how to map the raw ESMF DE layout onto the raw WRF
! layout, do a Gather. 
      call ESMF_FieldAllGather(field_temp, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, u2, status)
      call ESMF_FieldGetName( field_temp, fieldname, Status)
      call ESMF_FieldGetGrid( field_temp, grid, Status)

  
      print*,'!!!!!!!!!!!!!!!!!!!!!!! ext_ncd_open_for_write_begin'

! To write multiple times into the same file, the I/O stream will
! probably need a separate initialize routine.  Upon initialization,
! 'DataHandle' will be carried around, perhaps in ESMF_IOSpec, to
! reaccess the file.
      call ext_ncd_open_for_write_begin( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
      print *, ' ext_ncd_open_for_write_begin Status = ',Status,DataHandle
      
      Dom = 0
  
! Possibly add this as an ESMF_IOSpec item.
! Hardwire it for now.
! The default should probably be based off of Grid coord_order
      MemOrd = "XY"

! All of the domain and halo dimension parameters and names to be
! pulled from the ESMF_Grid.
!!$  call ESMF_GridGetDistGrid(distgrid, grid%ptr, 'cell_center', Status)
      call ESMF_DistGridGet(grid%ptr%distgrids(1), globalCellCountPerDim=span, &
                            globalStartPerDEPerDim=starts, rc=Status)

      Dom2S = starts(:,1)
      Dom2E = Dom2S + span
      Mem2S = Dom2S - pad
      Mem2E = Dom2E + pad
      Pat2S = Dom2S
      Pat2E = Dom2E
      
      DimNames(1) = 'X'
      DimNames(2) = 'Y'
      DimNames(3) = ''

! There needs to be a block of code or function that converts the
! ESMF variable types into the corresponding WRF types. 
! i.e. ESMF_KIND_R4 => WRF_REAL
! This call does 'trains' the output library but does not write out any data.
      call ext_ncd_write_field(DataHandle,Date,fieldname,u2,WRF_REAL,Comm,IOComm,Dom,&
           &'XY',Stagger,DimNames,Dom2S,Dom2E,Mem2S,Mem2E,Pat2S,Pat2E,Status)
      print *,'             dry run : ext_ncd_write_field Status = ',Status
  
      call ext_ncd_open_for_write_commit(DataHandle, Status)
      print *, '             ext_ncd_open_for_write_commit Status = ', Status,DataHandle
  
! This call does output the data.
      call ext_ncd_write_field(DataHandle,Date,fieldname,u2,WRF_REAL,Comm,IOComm,Dom,&
           &'XY',Stagger,DimNames,Dom2S,Dom2E,Mem2S,Mem2E,Pat2S,Pat2E,Status)
      print *,'              first write: ext_ncd_write_field Status = ',Status
  
! For writing multiple times to the same file, these calls will have
! to be placed in a separate close routine to release the handle on
! the IO stream.
      call ext_ncd_ioclose( DataHandle, Status)
      print *, '             After ext_ncd_ioclose, Status = ',Status
      call ext_ncd_ioexit(Status)
      print *,'              After ext_ncd_ioexit, Status = ',Status
      
    end subroutine ESMF_IOWriteField
    
  end module ESMF_IOMod


