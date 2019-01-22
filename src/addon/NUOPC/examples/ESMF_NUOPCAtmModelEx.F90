! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!

!BOE
! \label{sec:atmexample}
!
! In this section we'll look at code for an example NUOPC Model cap.
! The example shows the basic structure of a NUOPC Model cap for a fictitious
! atmosphere model called ATM. It is slightly simpler than a ``real'' cap,
! but has enough detail to show the basic coding structures.
! Each section of the example cap code will be broken down and described separately.
!
! \textbf{Finding More NUOPC Code Examples}
! \\
! In addition to the example code in this section, the
! \htmladdnormallink{NUOPC Prototypes Subversion}{https://sourceforge.net/p/esmfcontrib/svn/HEAD/tree/NUOPC/tags/ESMF\_7\_0\_0/}
! repository contains many small example applications that are helpful
! for understanding the architecture of NUOPC applications and showing
! example uses of the NUOPC API.  These example applications can be
! compiled and executed on your system.
!
! A good starting point is the
! \htmladdnormallink{SingleModelProto application}{https://sourceforge.net/p/esmfcontrib/svn/HEAD/tree/NUOPC/tags/ESMF\_7\_0\_0/SingleModelProto},
! which includes a single Model with a Driver and the
! \htmladdnormallink{AtmOcnProto application}{https://sourceforge.net/p/esmfcontrib/svn/HEAD/tree/NUOPC/tags/ESMF\_7\_0\_0/AtmOcnProto}
! which includes two Models, a Connector, and a Driver.
!
! \subsection{Module Imports}
! \label{sec:atmexample_moduleimports}
! The required NUOPC subroutines in the cap are grouped into a Fortran module,
! here called ATM.  All NUOPC Model caps will import the {\tt ESMF}, {\tt NUOPC}, and
! {\tt NUOPC\_Model} modules.  Typically, other {\tt use} statements will appear
! as well to import subroutines and variables from your model code. The module
! exposes only a single public entry point (subroutine) called {\tt SetServices}.
! This should be true for all NUOPC Model caps.
!EOE 

!BOC
module ATM

  !-----------------------------------------------------------------------------
  ! Basic NUOPC Model cap for ATM component (a fictitious atmosphere model).
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance
  
  implicit none
  
  private
  
  public :: SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

!EOC
!BOE
!\subsection{SetServices}
!\label{sec:atmexample_setservices}
! Every NUOPC Component must include a {\tt SetServices} subroutine similar to the one
! shown below.  All NUOPC {\tt SetServices} routines have the same parameter list
! and should do several things:
! \begin{itemize}
! \item indicate the generic component being specialized,
! \item register entry points for execution phases, and
! \item register any specialization points.
! \end{itemize}
!
! In the example code, the call to {\tt NUOPC\_CompDerive} indicates that
! this component derives from (and specializes) the generic {\tt NUOPC\_Model}
! component.  In other words, this is a {\tt NUOPC\_Model} component customized
! for a specific model.
!
! The calls to {\tt NUOPC\_CompSetEntryPoint} register
! subroutines that are implemented in the cap.  These are initialization phases
! that are not provided by the generic NUOPC Model.
! The {\tt phaseLabelList} parameter lists a NUOPC-defined label from the \emph{Initialize Phase
! Definition}.  NUOPC defines explicitly what happens in each phase of model
! initialization and these labels uniquely define each phase.  For example, 
! {\tt "IPDv03p1"} stands for ``Initialize Phase Definition version 03 phase 1''. The
! value for the parameter {\tt userRoutine} is the name of the subroutine that should
! be executed for the phase (e.g., {\tt InitializeP1}).   This subroutine
! appears later on in the cap and the name of the registered subroutine is entirely
! up to you.

! At this point, don't worry too much about what happens during each phase, just
! know that some phases are not provided by NUOPC and so must be written
! by you.  In the example code:
! \begin{itemize}
! \item phase IPDv03p1 maps to subroutine {\tt InitializeP1}, and
! \item phase IPDv03p3 maps to subroutine {\tt InitializeP2}.
! \end{itemize}
!
! In addition to providing subroutines for entire phases, sometimes \emph{part} of
! a phase can be specialized.  The call to {\tt NUOPC\_CompSpecialize} shows how
! to register a subroutine for a particular ``specialization point.'' In this
! case the name of the specialization point is held in the variable
! {\tt model\_label\_Advance} and the subroutine providing the implementation is
! {\tt ModelAdvance}.
!
! The Advance specialization point is called by NUOPC whenever it needs
! your model to take a single timestep forward.  Basically, this means
! you'll need to add a call inside the specialization subroutine to your
! model's timestepping subroutine.
!EOE

!BOC  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
!EOC

!BOE
!\subsection{Checking Return Codes}
!\label{sec:atmexample_retcodes}
! Essentially all ESMF and NUOPC methods have an optional integer
! return code parameter.  You are highly encouraged to call
! {\tt ESMF\_LogFoundError} after every ESMF/NUOPC call in order to
! check the return code and record any errors in the log files
! that ESMF generates during the run.  Including the {\tt line} and
! {\tt file} parameters will help to located where errors occur in the code.
! These parameter values are typically filled in by the pre-processor.
!
!\subsection{Initialize Phase - Advertise Fields}
!\label{sec:atmexample_advertisefields}
! In this section we see the implementation of the {\tt InitializeP1} subroutine, which
! is registered for the initialize phase with label IPDv03p1.
! The full list of initialization phases,
! how they are ordered, and what happens during each phase is described in the
! \htmladdnormallink{NUOPC Reference Manual}{http://www.earthsystemmodeling.org/esmf\_releases/non_public/ESMF\_7\_0\_0/NUOPC\_refdoc/node3.html\#IPD}.
!
! For now you should notice a few things:
! \begin{itemize}
! \item All phase subroutines are standard ESMF methods with the same parameter list:
!   \begin{itemize}
!   \item {\tt model} - a reference to the component itself ({\tt ESMF\_GridComp})
!   \item {\tt importState} - a container ({\tt ESMF\_State}) for input fields
!   \item {\tt exportState} - a container ({\tt ESMF\_State}) for output fields
!   \item {\tt clock} - keeps track of model time ({\tt ESMF\_Clock})
!   \item {\tt rc} - an {\tt integer} return code
!   \end{itemize}
! \item If the subroutine succeeds, it should return {\tt ESMF\_SUCCESS} in
!  the return code.  Otherwise it should return an error code. The
!  list of pre-defined ESMF error codes is provided in the
!  \htmladdnormallink{ESMF Reference Manual}{http://www.earthsystemmodeling.org/esmf\_releases/public/last/ESMF\_refdoc/node9.html\#SECTION09030000000000000000}.
!\end{itemize}

! The purpose of this phase is for your model to \textbf{advertise its import and
! export fields}.  This means that your model announces which model variables
! it is capable of exporting (e.g., an atmosphere might export air pressure at sea level)
! and which model variables it requires (e.g., an atmosphere might require
! sea surface temperature as a boundary condition).  The reason there is an
! explicit \textbf{advertise} phase is because NUOPC dynamically matches fields among
! all the models participating in a coupled simulation during runtime. So, we
! need to collect the list of possible input and output fields from all the
! models during their initialization.
!
! As shown in the code below, to advertise a field you call
! {\tt NUOPC\_Advertise} with the following parameters:
! \begin{itemize}
! \item either the {\tt importState} or {\tt exportState} object
! \item the standard name of the field, based on the
! \htmladdnormallink{CF conventions}{http://cfconventions.org/standard-names.html}
! \item an optional name for the field, which can be used to
!  retrieve it later from its {\tt ESMF\_State}; if ommited
!  the standard name will be used as the field name
! \item a return code
!\end{itemize}
!
! The example code below advertises one import field with the standard
! name {\tt "sea\_surface\_temperature"}, and two export fields with standard
! names {\tt "air\_pressure\_at\_sea\_level"} and {\tt "surface\_net\_downward\_shortwave\_flux"}.
!
! \textbf{Advertising a Field does NOT allocate memory}
! \\
! Note that NUOPC does not allocate memory for fields during the
! advertise phase or when {\tt NUOPC\_Advertise} is called.
! Instead, this is simply a way for models to communicate the
! standard names of fields.  During a later phase, only those fields that
! are \emph{connected} (e.g., a field exported from one model that is
! imported by another) need to have memory allocated.
! Also, since ESMF will accept pointers to pre-allocated memory, it is usually not
! necessary to change how memory is allocated for your model's variables.

!EOE

!BOC
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sst", & 
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", & 
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", & 
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
!EOC

!BOE
!\subsection{Initialize Phase - Realize Fields}
!\label{sec:atmexample_realizefields}
!
!
! The following code fragment shows the {\tt InitializeP2} subroutine, which
! was registered for phase IPDv03p3.   During this phase, fields that
! were previously advertised should now be \textbf{realized}.  Realizing a field
! means that an {\tt ESMF\_Field} object is created and it is added to the appropriate
! {\tt ESMF\_State}, either import or export.
!
! In order to create an {\tt ESMF\_Field}, you'll first need to create one of the
! ESMF geometric types, {\tt ESMF\_Grid}, {\tt ESMF\_Mesh}, or {\tt ESMF\_LocStream}.
! For 2D and 3D logically rectangular grids (such as a lat-lon grid), the
! typical choice is {\tt ESMF\_Grid}.  For unstructured grids, use an {\tt ESMF\_Mesh}.
!
! Describing your model's grid (physical discretization) in the
! ESMF representation is  one of the most important parts of creating
! a NUOPC cap. The ESMF geometric types are described in detail in the ESMF Reference Manual:
! \begin{itemize}
! \item \htmladdnormallink{ESMF\_Grid}{http://www.earthsystemmodeling.org/esmf\_releases/public/last/ESMF\_refdoc/node5.html\#SECTION05080000000000000000}
! - logically rectangular grids
! \item \htmladdnormallink{ESMF\_Mesh}{http://www.earthsystemmodeling.org/esmf\_releases/public/last/ESMF\_refdoc/node5.html\#SECTION050100000000000000000}
! - unstructured grids
! \item \htmladdnormallink{ESMF\_LocStream}{http://www.earthsystemmodeling.org/esmf\_releases/public/last/ESMF\_refdoc/node5.html\#SECTION05090000000000000000}
! - a set of observational points
! \end{itemize}
!
! For the sake a simplicity, a 10x100 Cartesian grid is created in the code below
! and assigned to the variable {\tt gridIn}.
!
! An {\tt ESMF\_Field} is created by by passing in the field
! name (should be the same as advertised), the grid, and the data type of the
! field to {\tt ESMF\_FieldCreate}.
!
! Fields are put into import or export States by calling {\tt NUOPC\_Realize}.  
! The example code realizes three fields in total, one import and two export, 
! and all three share the same grid.

!EOE

  !-----------------------------------------------------------------------------
!BOC
  subroutine InitializeP2(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    
    rc = ESMF_SUCCESS
    
    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/10, 100/), &
      minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
      maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
!EOC

!BOE
! \subsection{Model Advance Specialization}
! \label{sec:atmexample_advancespec}
!
! As described in the section \ref{sec:atmexample_setservices},
! the subroutine {\tt ModelAdvance} (shown below) has been
! registered to the \emph{specialization point} with the label
! {\tt model\_label\_Advance} in the {\tt SetServices} subroutine. This
! specialization point subroutine is called within the generic {\tt NUOPC\_Model}
! run phase in order to request that your model take a timestep
! forward.  The code to do this is model dependent, so it does not appear
! in the subroutine below.
!
! \textbf{Each NUOPC component maintains its own clock} (an {\tt ESMF\_Clock} object).
! The clock is used here to indicate the current model time and the
! timestep size. When the subroutine finishes, your model should be
! moved ahead in time from the current time by one timestep.  NUOPC will
! automatically advance the clock for you, so there is no explicit call
! to do that here.
!
! Since there is no actual model for us to advance in this example,
! the code below simply prints the current time and stop time (current time + timestep)
! to standard out.
!
! With respect to specialization point subroutines in general, note that:
! \begin{itemize}
! \item All specialization point subroutines rely on the ESMF attachable
!    methods capability, and therefore all share the same parameter list:
! \begin{itemize}
!    \item a pointer to the component ({\tt ESMF\_GridComp})
!    \item an {\tt integer} return code
! \end{itemize}
! \item Because the import/export states and the clock do not come in through
!   the parameter list, they must be accessed via a call to {\tt NUOPC\_ModelGet}
!   as shown in the code below.
!\end{itemize}
!EOE

  !-----------------------------------------------------------------------------

!BOC
  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

end module
!EOC

! A driver set up to mirror fields
! in ATM component above
module DRIVER

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
       driver_routine_SS             => SetServices, &
       driver_label_SetModelServices => label_SetModelServices

  use NUOPC_Connector, only: cplSS => SetServices

  use ATM, only: atmSS => SetServices

  implicit none

  private

  integer, parameter            :: stepCount = 5
  real(ESMF_KIND_R8), parameter :: stepTime  = 30.D0  ! step time [s]


  public :: SetServices

  !-----------------------------------------------------------------------------
contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(drvr, rc)
    type(ESMF_GridComp)  :: drvr
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(drvr, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call NUOPC_CompSetInternalEntryPoint(drvr, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv05p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call NUOPC_CompSetInternalEntryPoint(drvr, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv05p6"/), userRoutine=RealizeMirroredFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call NUOPC_CompSetInternalEntryPoint(drvr, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv05p8"/), userRoutine=TimestampMirroredFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(drvr, specLabel=driver_label_SetModelServices, &
         specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(drvr, rc)
    type(ESMF_GridComp)  :: drvr
    integer, intent(out) :: rc

    ! local variables
    integer                       :: localrc
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock

    rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetAutoAdd(.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call NUOPC_CompAttributeSet(drvr, name="CompLabel", &
         value="Driver", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call NUOPC_DriverAddComp(drvr, "ATM", atmSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    ! Add connectors for field mirroring
    call NUOPC_DriverAddComp(drvr, srcCompLabel="Driver", &
         dstCompLabel="ATM", compSetServicesRoutine=cplSS, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call NUOPC_DriverAddComp(drvr, srcCompLabel="ATM", &
         dstCompLabel="Driver", compSetServicesRoutine=cplSS, &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    ! set the driver clock
    call ESMF_TimeSet(startTime, s = 0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeSet(stopTime, s_r8 = stepTime * stepCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeStep, s_r8 = stepTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

    internalClock = ESMF_ClockCreate(name="Driver Clock", &
         timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSet(drvr, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

  end subroutine SetModelServices

  subroutine InitializeP1(drvr, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: drvr
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_GridComp) :: comp
    type(ESMF_State)   :: compImport, compExport

    rc = ESMF_SUCCESS

    call NUOPC_DriverGetComp(drvr, compLabel="ATM", &
         comp=comp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_GridCompGet(comp, importState=compImport, &
         exportState=compExport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call NUOPC_SetAttribute(compImport, "FieldTransferPolicy", &
         "transferAll", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    call NUOPC_SetAttribute(compExport, "FieldTransferPolicy", &
         "transferAll", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    ! set driver's own import/export to accept field transfers
    call NUOPC_SetAttribute(importState, "FieldTransferPolicy", &
         "transferAll", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    call NUOPC_SetAttribute(exportState, "FieldTransferPolicy", &
         "transferAll", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

  end subroutine InitializeP1

  subroutine RealizeMirroredFields(drvr, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: drvr
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    !print *, "Inside RealizeMirroredFields"

    ! realize mirrored fields that have accepted grid from other comp
    call MirrorFieldsInState(importState, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return ! bail out

    call MirrorFieldsInState(exportState, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return ! bail out

  end subroutine RealizeMirroredFields

  subroutine MirrorFieldsInState(state, rc)
    type(ESMF_State), intent(in) :: state
    integer, intent(out) :: rc

    integer                :: i, itemCount, stat
    character(ESMF_MAXSTR) :: transferGeom
    character(ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)       :: field

    type(ESMF_Grid)        :: grid
    type(ESMF_DistGrid)    :: distgrid
    integer, allocatable   :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer                :: dimCount
    character(len=80)      :: valueString, attrString
    logical                :: isPresent
    type(ESMF_AttPack)     :: attpack
    integer, pointer       :: ungriddedLBound(:), ungriddedUBound(:)

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return  ! bail out

    allocate(itemNameList(itemCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
         msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) &
         return  ! bail out

    allocate(itemTypeList(itemCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) &
         return  ! bail out

    call ESMF_StateGet(state, itemNameList=itemNameList, &
         itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    ! WARNING: does not currently deal with nested states or field bundles
    do i=lbound(itemNameList,1), ubound(itemNameList,1)
       if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then

          ! TODO: condition on NUOPC_IsConnected first
          ! NUOPC_IsConnected(state, fieldName=fieldNameList(i))

          call ESMF_StateGet(state, &
               itemNameList(i), field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out

          call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
               value=transferGeom, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out

          if (trim(transferGeom)=="accept") then

             call ESMF_LogWrite("Completing mirrored field: "//itemNameList(i), &
                  ESMF_LOGMSG_INFO, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) &
                  return  ! bail out

             nullify(ungriddedLBound)
             nullify(ungriddedUBound)

             call ESMF_AttributeGetAttPack(field, attpack=attpack, &
                  convention="NUOPC", purpose="Instance", isPresent=isPresent, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) &
                  return  ! bail out
             if (.not. isPresent) then
                ! attpack not present
                call ESMF_LogWrite("Field level attpack NOT present!", &
                     ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                     line=__LINE__, file=__FILE__)) &
                     return  ! bail out
             else
                ! retrieve ungridded dimension bounds and mirror
                ! match those as well
                call ESMF_AttributeGet(field, name="UngriddedLBound", &
                     attpack=attpack, itemCount=itemCount, isPresent=isPresent, &
                     attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                     line=__LINE__, file=__FILE__)) &
                     return  ! bail out

                if (isPresent .and. itemCount > 0) then
                   allocate(ungriddedLBound(itemCount),stat=stat)
                   if (ESMF_LogFoundAllocError(statusToCheck=stat, &
                        msg="Allocation of internal ungriddedLBound failed.", &
                        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
                        return  ! bail out

                   call ESMF_AttributeGet(field, &
                        name="UngriddedLBound", valueList=ungriddedLBound, &
                        convention="NUOPC", purpose="Instance", &
                        attnestflag=ESMF_ATTNEST_ON, rc=rc)
                   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out

                   !print *, "UNGRIDDED LBOUND = ", ungriddedLBound
                endif

                call ESMF_AttributeGet(field, name="UngriddedUBound", &
                     attpack=attpack, itemCount=itemCount, isPresent=isPresent, &
                     attnestflag=ESMF_ATTNEST_ON, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                     line=__LINE__, file=__FILE__)) &
                     return  ! bail out

                if (isPresent .and. itemCount > 0) then
                   allocate(ungriddedUBound(itemCount),stat=stat)
                   if (ESMF_LogFoundAllocError(statusToCheck=stat, &
                        msg="Allocation of internal ungriddedUBound failed.", &
                        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
                        return  ! bail out

                   call ESMF_AttributeGet(field, &
                        name="UngriddedUBound", valueList=ungriddedUBound, &
                        convention="NUOPC", purpose="Instance", &
                        attnestflag=ESMF_ATTNEST_ON, rc=rc)
                   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out

                   !print *, "UNGRIDDED UBOUND = ", ungriddedUBound
                endif
             endif

             if (associated(ungriddedLBound) .and. &
                  associated(ungriddedUBound)) then
                call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
                     ungriddedLBound=ungriddedLBound, &
                     ungriddedUBound=ungriddedUBound, &
                     rc=rc)
                deallocate(ungriddedLBound)
                deallocate(ungriddedUBound)
             else
                call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
             endif

             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
          else
             !print *, "NOT COMPLETING FIELD: ", itemNameList(i), trim(transferGeom)
             call ESMF_LogWrite("CANNOT complete mirrored field: "//itemNameList(i), &
                  ESMF_LOGMSG_INFO, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
          end if

       end if
    end do

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine MirrorFieldsInState


  subroutine TimestampMirroredFields(drvr, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: drvr
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    !print *, "Inside TimestampMirroredFields"

    ! timestamp mirrored fields

    ! no Clock is passed into the driver from the application level
    ! -> query the driver for its internal clock
    call ESMF_GridCompGet(drvr, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call NUOPC_SetTimestamp(importState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call NUOPC_SetTimestamp(exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(drvr, name="InitializeDataComplete", &
         value="true", convention="NUOPC",  purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine TimestampMirroredFields



end module




    program ESMF_NUOPCAtmModelEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod
    use DRIVER, only: driverSS => SetServices

    implicit none

    ! Local variables
    type(ESMF_GridComp) :: drvr
    type(ESMF_State) :: importState, exportState
    integer :: rc
    integer :: finalrc
    integer :: result = 0     ! all pass
    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

    finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_NUOPCAtmModelEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    call ESMF_Initialize(defaultlogfilename="NUOPCAtmModelEx.Log", &
                     defaultCalKind=ESMF_CALKIND_GREGORIAN, &
                     logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

!-------------------------------------------------------------------------
    print *, "NUOPC DRIVER + ATM Model example run"

    importState = ESMF_StateCreate(name="Driver Import State", &
       stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
    exportState = ESMF_StateCreate(name="Driver Export State", &
       stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    drvr = ESMF_GridCompCreate(name="Driver", rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridCompSetServices(drvr, userRoutine=driverSS, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridCompInitialize(drvr, phase=0, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridCompInitialize(drvr, importState=importState, &
         exportState=exportState, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridCompRun(drvr, importState=importState, &
         exportState=exportState, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridCompFinalize(drvr, importState=importState, &
         exportState=exportState, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridCompDestroy(drvr, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


!-------------------------------------------------------------------------


    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

    call ESMF_Finalize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_NUOPCAtmModelEx.F90"
    else
        print *, "FAIL: ESMF_NUOPCAtmModelEx.F90"
    end if

    end program ESMF_NUOPCAtmModelEx
