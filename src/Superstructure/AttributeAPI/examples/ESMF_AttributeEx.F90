! $Id: ESMF_AttributeEx.F90,v 1.2 2008/10/20 22:14:09 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_AttributeEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Demonstrate Attribute usage}
!
! This example illustrates the use of the ESMF\_Attribute class to create 
! Attribute hierarchies using Attribute packages.  A gridded Component
! is used in conjunction with two States, a FieldBundle, and various realistic
! Fields to create an Attribute hierarchy and copy it from one State to another.  
! Attributes packages are created on the Component and Fields, and the 
! standard Attributes in each package are used in the Attribute hierarchy.
! The Attribute package of the Component is customized to contain a number
! of Attributes with different types and lists, this is used to demonstrate
! the capabilities of removing and getting default Attributes.
!EOE


!  !PROGRAM: ESMF\_Attribute - Examples of Attribute usage.
!
!  !DESCRIPTION: 
!
! This program shows examples of Attribute usage


!BOC
      ! Use ESMF framework module
      use ESMF_Mod
      implicit none

      ! Local variables  
      integer                 :: rc, finalrc, petCount, localPet
      type(ESMF_VM)           :: vm
      type(ESMF_Field)        :: DPEDT,DTDT,DUDT,DVDT,PHIS,QTR,CNV,CONVCPT,&
                                 CONVKE,CONVPHI
      type(ESMF_FieldBundle)  :: fbundle
      type(ESMF_State)        :: importState, exportState
      type(ESMF_GridComp)     :: gridcomp
      character(ESMF_MAXSTR)  :: name1,name2,name3,name4,name5,name6,name7, &
                                 name8, name9, name10, value1,value2,value3, &
                                 value4,value5,value6,value7,value8,value9, &
                                 value10,conv,purp
      
      character(ESMF_MAXSTR), dimension(12) :: attpackListTNames
      integer(kind=4)                       :: inI4
      integer(kind=4), dimension(3)         :: inI4l
      integer(kind=8)                       :: inI8
      integer(kind=8), dimension(3)         :: inI8l
      real(kind=4)                          :: inR4
      real(kind=4), dimension(3)            :: inR4l
      real(kind=8)                          :: inR8
      real(kind=8), dimension(3)            :: inR8l
      character(ESMF_MAXSTR)                :: inChar
      character(ESMF_MAXSTR), dimension(3)  :: inCharl, outCharl, &
                                               defaultCharl, dfltoutCharl
      logical                               :: inLog
      logical, dimension(3)                 :: inLogl
!EOC         

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      if (rc/=ESMF_SUCCESS) goto 10
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_Attribute Example"
        print *, "--------------------------------------- "
      endif

!BOE
! First construct some ESMF objects, such as the gridded Component, States, 
! FieldBundle, and Fields.
!EOE
!BOC
      if (petCount<4) then
        gridcomp = ESMF_GridCompCreate(name="gridded_component", &
          petList=(/0/), rc=rc)
      else 
        gridcomp = ESMF_GridCompCreate(name="gridded_component", &
          petList=(/0,1,2,3/), rc=rc)
      endif
      importState = ESMF_StateCreate("importState", ESMF_STATE_IMPORT, rc=rc)
      exportState = ESMF_StateCreate("exportState", ESMF_STATE_EXPORT, rc=rc)
        
      DPEDT = ESMF_FieldCreateEmpty(name='DPEDT', rc=rc)
      DTDT = ESMF_FieldCreateEmpty(name='DTDT', rc=rc)
      DUDT = ESMF_FieldCreateEmpty(name='DUDT', rc=rc)
      DVDT = ESMF_FieldCreateEmpty(name='DVDT', rc=rc)
      PHIS = ESMF_FieldCreateEmpty(name='PHIS', rc=rc)
      QTR = ESMF_FieldCreateEmpty(name='QTR', rc=rc)
      CNV = ESMF_FieldCreateEmpty(name='CNV', rc=rc)
      CONVCPT = ESMF_FieldCreateEmpty(name='CONVCPT', rc=rc)
      CONVKE = ESMF_FieldCreateEmpty(name='CONVKE', rc=rc)
      CONVPHI = ESMF_FieldCreateEmpty(name='CONVPHI', rc=rc)
      
      fbundle = ESMF_FieldBundleCreate(name="fbundle", rc=rc)
!EOC

!BOE
!    Add Attribute packages to all of the Fields and the Component.  First
!    we must choose a convention and purpose for the Attribute packages.
!EOE

!BOC 
      conv = 'ESG'
      purp = 'general'

      call ESMF_AttributeAdd(DPEDT, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(DTDT, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(DUDT, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(DVDT, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(PHIS, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(QTR, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(CNV, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(CONVCPT, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(CONVKE, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeAdd(CONVPHI, convention=conv, purpose=purp, rc=rc)
      
      call ESMF_AttributeAdd(gridcomp, convention=conv, purpose=purp, rc=rc)
!EOC  

!BOE
!     The standard Attribute package (currently) supplied by ESMF for 
!     Field contains 6 Attributes, 2 of which are set automatically.  
!     The remaining 4 Attributes in the standard Field Attribute
!     package must be set manually by the user.  The names of the 4 
!     Attributes that the user must set are currently in accordance 
!     with the CF convention, which is a subset of the ESG convention.
!EOE

!BOC
      name1 = 'name'
      name2 = 'standard_name'
      name3 = 'long_name'
      name4 = 'units'
      
      value1 = 'DPEDT'
      value2 = 'tendency_of_air_pressure'
      value3 = 'Edge pressure tendency'
      value4 = 'Pa s-1'
      call ESMF_AttributeSet(DPEDT, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DPEDT, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DPEDT, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DPEDT, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'DTDT'
      value2 = 'tendency_of_air_temperature'
      value3 = 'Delta-p weighted temperature tendency'
      value4 = 'Pa K s-1'
      call ESMF_AttributeSet(DTDT, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DTDT, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DTDT, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DTDT, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'DUDT'
      value2 = 'tendency_of_eastward_wind'
      value3 = 'Eastward wind tendency'
      value4 = 'm s-2'
      call ESMF_AttributeSet(DUDT, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DUDT, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DUDT, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DUDT, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'DVDT'
      value2 = 'tendency_of_northward_wind'
      value3 = 'Northward wind tendency'
      value4 = 'm s-2'
      call ESMF_AttributeSet(DVDT, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DVDT, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DVDT, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(DVDT, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'PHIS'
      value2 = 'surface_geopotential'
      value3 = 'Surface geopotential height'
      value4 = 'm2 s-2'
      call ESMF_AttributeSet(PHIS, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(PHIS, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(PHIS, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(PHIS, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'QTR'
      value2 = ''
      value3 = 'Advected quantities'
      value4 = 'unknown'
      call ESMF_AttributeSet(QTR, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(QTR, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(QTR, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(QTR, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'CNV'
      value2 = 'atmosphere_kinetic_energy_content'
      value3 = 'Generation of atmosphere kinetic energy content'
      value4 = 'W m-2'
      call ESMF_AttributeSet(CNV, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CNV, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CNV, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CNV, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'CONVCPT'
      value2 = ''
      value3 = 'Vertically integrated enthalpy convergence'
      value4 = 'W m-2'
      call ESMF_AttributeSet(CONVCPT, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVCPT, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVCPT, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVCPT, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'CONVKE'
      value2 = ''
      value3 = 'Vertically integrated kinetic energy convergence'
      value4 = 'W m-2'
      call ESMF_AttributeSet(CONVKE, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVKE, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVKE, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVKE, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
    
      value1 = 'CONVPHI'
      value2 = ''
      value3 = 'Vertically integrated geopotential convergence'
      value4 = 'W m-2'
      call ESMF_AttributeSet(CONVPHI, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVPHI, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVPHI, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(CONVPHI, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
!EOC  

!BOE
!     The standard Attribute package (currently) supplied by ESMF for 
!     Component contains 10 Attributes.  They must be set manually by
!     the user.
!EOE

!BOC
      name1 = 'discipline'
      name2 = 'physical_domain'
      name3 = 'agency'
      name4 = 'institution'
      name5 = 'author'
      name6 = 'coding_language'
      name7 = 'model_component_framework'
      name8 = 'name'
      name9 = 'full_name'
      name10 = 'version'
      value1 = 'Atmosphere'
      value2 = 'Earth system'
      value3 = 'NASA'
      value4 = 'Global Modeling and Assimilation Office (GMAO)'
      value5 = 'Max Suarez'
      value6 = 'Fortran 90'
      value7 = 'ESMF (Earth System Modeling Framework)'
      value8 = 'GEOS-5 FV dynamical core'
      value9 = 'Goddard Earth Observing System Version 5 Finite Volume Dynamical Core'
      value10 = 'GEOSagcm-EROS-beta7p12'
      
      call ESMF_AttributeSet(gridcomp, name1, value1, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name2, value2, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name3, value3, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name4, value4, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name5, value5, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name6, value6, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name7, value7, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name8, value8, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name9, value9, convention=conv, &
        purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name10, value10, convention=conv, &
        purpose=purp, rc=rc)
!EOC

!BOE
!     Adding the Fields to the FieldBundle will automatically ``link" the 
!     Attribute hierarchies.  The same type of link will be generated
!     when adding a FieldBundle to a State.
!EOE

!BOC
      call ESMF_FieldBundleAdd(fbundle, DPEDT, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, DTDT, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, DUDT, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, DVDT, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, PHIS, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, QTR, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, CNV, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, CONVCPT, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, CONVKE, rc=rc)
      call ESMF_FieldBundleAdd(fbundle, CONVPHI, rc=rc)
      
      call ESMF_StateAdd(exportState, fieldbundle=fbundle, rc=rc)
!EOC

!BOE
!     The link between a State and the Component of interest must be
!     set manually by the user.
!EOE

!BOC
      call ESMF_AttributeSet(gridcomp, exportState, rc=rc)
!EOC

!BOE
!     There are (currently) two different formats available for writing
!     the contents of the Attribute packages in an Attribute hierarchy.
!     There is an XML formatted write, which generates a .xml file in the
!     execution directory with the contents of the write.  There is also
!     a tab-delimited write which writes to standard out, a file generated
!     in the execution directory with the extension .stdout.  Either of 
!     the AttributeWrite formats can be called on any of the objects which
!     are capable of holding Attributes, but only from objects in an 
!     Attribute hierarchy which contains Attribute packages will any 
!     relevant information be written.  In the the tab-delimited case 
!     relevant information will only be written if the Attribute hierarchy
!     contains ESMF-supplied Attribute packages.  The AttributeWrite
!     capability is in a developmental stage, present work is focusing
!     on making it more robust in future releases.  A flag is used to 
!     specify which format to write, the default is tab-delimited.
!EOE


      if (localPet==0) then
!BOC
      call ESMF_AttributeWrite(gridcomp,conv,purp, &
        attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
!EOC
        print *, ""
        print *, "--------------------------------------- "
        print *, "Begin tab-delimited AttributeWrite format"
        print *, "--------------------------------------- "
!BOC
      call ESMF_AttributeWrite(gridcomp,conv,purp,rc=rc)
!EOC
        print *, "--------------------------------------- "
        print *, "End tab-delimited AttributeWrite format"
        print *, "--------------------------------------- "
        print *, ""
        if (rc/=ESMF_SUCCESS) goto 10
      endif

!BOE
!     It is possible to customize an Attribute package by adding Attributes.  
!     One could also specify a whole new convention and purpose, to make an
!     Attribute package completely composed of customized Attributes.  This is
!     the recommended practice, as it will allow the customized Attribute package
!     to be nested inside the standard Attribute package.  Accordingly, in 
!     this example we add one Attribute of each available ESMF Attribute value type 
!     to a nested Attribute package, inside the standard Attribute package on the 
!     Component.  We must first make a list of the Attribute names so that we can 
!     add them to the Attribute package.  Afterwards, we can go through and set 
!     their values individually.
!EOE

!BOC
      conv = "Custom"
      
      attpackListTNames(1) = "ESMF_I4name"
      attpackListTNames(2) = "ESMF_I4namelist"
      attpackListTNames(3) = "ESMF_I8name"
      attpackListTNames(4) = "ESMF_I8namelist"
      attpackListTNames(5) = "ESMF_R4name"
      attpackListTNames(6) = "ESMF_R4namelist"
      attpackListTNames(7) = "ESMF_R8name"
      attpackListTNames(8) = "ESMF_R8namelist"
      attpackListTNames(9) = "Logical_name"
      attpackListTNames(10) = "Logical_namelist"
      attpackListTNames(11) = "Character_name"
      attpackListTNames(12) = "Character_namelist"
      
      call ESMF_AttributeAdd(gridcomp, convention=conv, purpose=purp, &
        attrList=attpackListTNames, rc=rc)
      
      inI4 = 4
      inI4l = (/1,2,3/)
      inI8 = 4
      inI8l = (/1,2,3/)
      inR4 = 4
      inR4l = (/1,2,3/)
      inR8 = 4
      inR8l = (/1,2,3/)
      inChar = "Character string 4"
      inCharl = (/ "Character string 1", &
                   "Character string 2", &
                   "Character string 3" /)
      inLog = .true.
      inLogl = (/.true., .false., .true. /)
      
      call ESMF_AttributeSet(gridcomp, name="ESMF_I4name", value=inI4, &
        convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="ESMF_I4namelist", &
        valueList=inI4l, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="ESMF_I8name", value=inI8, &
        convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="ESMF_I8namelist", &
        valueList=inI4l, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="ESMF_R4name", value=inR4, &
        convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="ESMF_R4namelist", &
        valueList=inI4l, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="ESMF_R8name", value=inR8, &
        convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="ESMF_R8namelist", &
        valueList=inI4l, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="Character_name", value=inChar, &
        convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="Character_namelist", &
        valueList=inCharl, convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="Logical_name", value=inLog, &
        convention=conv, purpose=purp, rc=rc)
      call ESMF_AttributeSet(gridcomp, name="Logical_namelist", &
        valueList=inLogl, convention=conv, purpose=purp, rc=rc)

      conv = "ESG"
!EOC

!BOE
!     We can retrieve Attributes by issuing the AttributeGet command.  This
!     can also be done with an optional default value (or list) so that  if
!     the Attribute is not found a value is returned without an error code.
!     Removal of Attributes is also possible.  In order to use the default
!     return value capabilites, we must first set up a default parameter.
!EOE

!BOC
      defaultCharl = (/ "Character string 4", &
                        "Character string 5", &
                        "Character string 6" /)
      
      call ESMF_AttributeGet(gridcomp, name="Character_namelist", &
        valueList=outCharl, convention=conv, purpose=purp, rc=rc) 
                    
      call ESMF_AttributeRemove(gridcomp, name="Character_namelist", &
        convention=conv, purpose=purp, rc=rc)
      
      call ESMF_AttributeGet(gridcomp, name="Character_namelist", &
        valueList=dfltoutCharl, defaultvalueList=defaultCharl, &
        convention=conv, purpose=purp, rc=rc)
!EOC
      
      if (all (dfltoutCharl /= defaultCharl)) then
        print *, "Attribute character list IN did not match OUT"
      endif

!BOE
!     Entire Attribute hierarchies can be copied from State to State, 
!     this can come in handy in a coupling Component.  
!EOE

!BOC
      call ESMF_AttributeCopy(exportState, importState, rc=rc)
!EOC     

  if (localPet==0) then
      print *, "--------------------------------------- "
      print *, "End of ESMF_Attribute Example"
      print *, "--------------------------------------- "
  endif

10 continue
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_AttributeEx.F90"
  else
    print *, "FAIL: ESMF_AttributeEx.F90"
  endif
  
end program
