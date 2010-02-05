! $Id: ESMF_AttributePackageEx.F90,v 1.12.2.1 2010/02/05 20:03:28 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_AttributePackageEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Intermediate Attribute usage: Attribute Packages}
!
! This example is slightly more complex than the example presented in section 
! \ref{AttributeEx} and illustrates the use of the Attribute class to create 
! Attribute hierarchies using Attribute packages.  A gridded Component
! is used in conjunction with two States, a FieldBundle, and various realistic
! Fields to create an Attribute hierarchy and copy it from one State to another.  
! Attributes packages are created on the Component and Fields, and the 
! standard Attributes in each package are used in the Attribute hierarchy.
! The Attribute package nesting capability is demonstrated by nesting the standard
! ESMF supplied packages for the Fields inside a user specified Attribute package
! with a customized convention.
! The first thing we must do is declare variables and initialize ESMF.
!EOE


!  !PROGRAM: ESMF\_AttributePackageEx - Examples of Attribute Package usage.
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
      character(ESMF_MAXSTR)  :: name1,name2,name3,name4, &
                                 value1,value2,value3, value4, &
                                 convESMF,convCC,purpGen
      
      character(ESMF_MAXSTR),dimension(2)   :: attrList         

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      if (rc/=ESMF_SUCCESS) goto 10
!EOC
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_AttributePackage Example"
        print *, "--------------------------------------- "
      endif

!BOE
!    We must construct the ESMF objects that will be responsible for the
!    Attributes we will be manipulating.  These objects include the 
!    gridded Component, two States, a FieldBundle, and 10 Fields.  In this trivial 
!    example we are constructing empty Fields with no underlying Grid.
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
!    Now we can add Attribute packages to all of the appropriate objects.
!    We will use the ESMF supplied Attribute packages for the Fields and 
!    the Component.  On the Fields, we will first use 
!    {\tt ESMF\_AttributeAdd()} to create standard Attribute packages, then
!    we will nest customized Attribute packages around the ESMF standard
!    Attribute packages.  In this simple example the purpose for the Attribute packages will
!    be specified as "General" in all cases.
!EOE

!BOC 
      convESMF = 'ESMF'
      convCC = 'CustomConvention'
      purpGen = 'General'

      attrList(1) = 'Coordinates'
      attrList(2) = 'Mask'

      ! DPEDT
      call ESMF_AttributeAdd(DPEDT, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(DPEDT, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! DTDT
      call ESMF_AttributeAdd(DTDT, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(DTDT, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! DUDT
      call ESMF_AttributeAdd(DUDT, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(DUDT, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! DVDT
      call ESMF_AttributeAdd(DVDT, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(DVDT, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! PHIS
      call ESMF_AttributeAdd(PHIS, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(PHIS, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! QTR
      call ESMF_AttributeAdd(QTR, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(QTR, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! CNV
      call ESMF_AttributeAdd(CNV, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(CNV, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! CONVCPT
      call ESMF_AttributeAdd(CONVCPT, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(CONVCPT, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! CONVKE
      call ESMF_AttributeAdd(CONVKE, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(CONVKE, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)

      ! CONVPHI
      call ESMF_AttributeAdd(CONVPHI, convention=convESMF, purpose=purpGen, rc=rc)
      call ESMF_AttributeAdd(CONVPHI, convention=convCC, purpose=purpGen, &
        attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, rc=rc)
      
      call ESMF_AttributeAdd(gridcomp, convention=convESMF, &
        purpose=purpGen, rc=rc)
!EOC  

!BOE
!     The standard Attribute package currently supplied by ESMF for 
!     Field contains 6 Attributes, 2 of which are set automatically.  
!     The remaining 4 Attributes in the standard Field Attribute
!     package must be set manually by the user.   We must also
!     set the Attributes of our own custom Attribute package, which
!     is built around the ESMF standard Attribute package.
!EOE

!BOC
      name1 = 'Name'
      name2 = 'StandardName'
      name3 = 'LongName'
      name4 = 'Units'
      
      ! DPEDT
      value1 = 'DPEDT'
      value2 = 'tendency_of_air_pressure'
      value3 = 'Edge pressure tendency'
      value4 = 'Pa s-1'
      ! Custom Attributes
      call ESMF_AttributeSet(DPEDT, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DPEDT, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(DPEDT, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DPEDT, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DPEDT, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DPEDT, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! DTDT
      value1 = 'DTDT'
      value2 = 'tendency_of_air_temperature'
      value3 = 'Delta-p weighted temperature tendency'
      value4 = 'Pa K s-1'
      ! Custom Attributes
      call ESMF_AttributeSet(DTDT, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DTDT, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(DTDT, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DTDT, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DTDT, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DTDT, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! DUDT
      value1 = 'DUDT'
      value2 = 'tendency_of_eastward_wind'
      value3 = 'Eastward wind tendency'
      value4 = 'm s-2'
      ! Custom Attributes
      call ESMF_AttributeSet(DUDT, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DUDT, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(DUDT, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DUDT, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DUDT, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DUDT, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! DVDT
      value1 = 'DVDT'
      value2 = 'tendency_of_northward_wind'
      value3 = 'Northward wind tendency'
      value4 = 'm s-2'
      ! Custom Attributes
      call ESMF_AttributeSet(DVDT, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DVDT, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(DVDT, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DVDT, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DVDT, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(DVDT, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! PHIS
      value1 = 'PHIS'
      value2 = 'surface_geopotential'
      value3 = 'Surface geopotential height'
      value4 = 'm2 s-2'
      ! Custom Attributes
      call ESMF_AttributeSet(PHIS, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(PHIS, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(PHIS, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(PHIS, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(PHIS, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(PHIS, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! QTR
      value1 = 'QTR'
      value2 = ''
      value3 = 'Advected quantities'
      value4 = 'unknown'
      ! Custom Attributes
      call ESMF_AttributeSet(QTR, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(QTR, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(QTR, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(QTR, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(QTR, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(QTR, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! CNV
      value1 = 'CNV'
      value2 = 'atmosphere_kinetic_energy_content'
      value3 = 'Generation of atmosphere kinetic energy content'
      value4 = 'W m-2'
      ! Custom Attributes
      call ESMF_AttributeSet(CNV, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CNV, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(CNV, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CNV, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CNV, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CNV, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! CONVCPT
      value1 = 'CONVCPT'
      value2 = ''
      value3 = 'Vertically integrated enthalpy convergence'
      value4 = 'W m-2'
      ! Custom Attributes
      call ESMF_AttributeSet(CONVCPT, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVCPT, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(CONVCPT, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVCPT, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVCPT, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVCPT, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! CONVKE
      value1 = 'CONVKE'
      value2 = ''
      value3 = 'Vertically integrated kinetic energy convergence'
      value4 = 'W m-2'
      ! Custom Attributes
      call ESMF_AttributeSet(CONVKE, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVKE, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(CONVKE, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVKE, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVKE, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVKE, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
    
      ! CONVPHI
      value1 = 'CONVPHI'
      value2 = ''
      value3 = 'Vertically integrated geopotential convergence'
      value4 = 'W m-2'
      ! Custom Attributes
      call ESMF_AttributeSet(CONVPHI, name='Coordinates', value='latlon', &
        convention=convCC, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVPHI, name='Mask', value='yes', &
        convention=convCC, purpose=purpGen, rc=rc)
      ! ESMF Attributes
      call ESMF_AttributeSet(CONVPHI, name1, value1, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVPHI, name2, value2, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVPHI, name3, value3, convention=convESMF, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(CONVPHI, name4, value4, convention=convESMF, &
        purpose=purpGen, rc=rc)
!EOC  

!BOE
!     The standard Attribute package currently supplied by ESMF for 
!     Component contains 10 Attributes.  These Attributes conform to both
!     the ESG and CF conventions, and must be set manually.
!EOE

!BOC
    call ESMF_AttributeSet(gridcomp, 'Agency', 'NASA', &
      convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'Author', 'Max Suarez', &
      convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'CodingLanguage', &
      'Fortran 90', convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'Discipline', &
      'Atmosphere', convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'FullName', &
      'Goddard Earth Observing System Version 5 Finite Volume Dynamical Core', &
        convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'ModelComponentFramework', &
      'ESMF', convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'Name', 'GEOS-5 FV dynamical core', &
      convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'PhysicalDomain', &
      'Earth system', convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'Version', &
      'GEOSagcm-EROS-beta7p12', convention=convESMF, purpose=purpGen, rc=rc)
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
!     set manually.
!EOE

!BOC
      call ESMF_AttributeLink(gridcomp, exportState, rc=rc)
!EOC

!BOE
!     There are currently two different formats available for writing
!     the contents of the Attribute packages in an Attribute hierarchy.
!     There is an XML formatted write, which generates an .xml file in the
!     execution directory with the contents of the write.  There is also
!     a tab-delimited write which writes to standard out, a file generated
!     in the execution directory with the extension .stdout.  Either of 
!     the {\tt ESMF\_AttributeWrite()} formats can be called on any of the objects which
!     are capable of manipulating Attributes, but only from objects in an 
!     Attribute hierarchy which contain ESMF standard Attribute packages can it be confirmed that any 
!     relevant information be written.  The {\tt ESMF\_AttributeWrite()}
!     capability is only functional for single-item Attributes at this point, it
!     will be more robust in future releases.  A flag is used to 
!     specify which format to write, the default is tab-delimited.
!EOE


      if (localPet==0) then
!BOC
      call ESMF_AttributeWrite(gridcomp,convESMF,purpGen, &
        attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
      call ESMF_AttributeWrite(gridcomp,convESMF,purpGen,rc=rc)
!EOC
        if (rc/=ESMF_SUCCESS) goto 10
      endif

    ! Destroy
    call ESMF_FieldDestroy(field=DPEDT, rc=rc)
    call ESMF_FieldDestroy(field=DTDT, rc=rc)
    call ESMF_FieldDestroy(field=DUDT, rc=rc)
    call ESMF_FieldDestroy(field=DVDT, rc=rc)
    call ESMF_FieldDestroy(field=PHIS, rc=rc)
    call ESMF_FieldDestroy(field=QTR, rc=rc)
    call ESMF_FieldDestroy(field=CNV, rc=rc)
    call ESMF_FieldDestroy(field=CONVCPT, rc=rc)
    call ESMF_FieldDestroy(field=CONVKE, rc=rc)
    call ESMF_FieldDestroy(field=CONVPHI, rc=rc)
    call ESMF_FieldBundleDestroy(fbundle, rc=rc)
    call ESMF_GridCompDestroy(gridcomp, rc=rc)
    call ESMF_StateDestroy(importState, rc=rc)
    call ESMF_StateDestroy(exportState, rc=rc)

  if (localPet==0) then
      print *, "--------------------------------------- "
      print *, "End of ESMF_AttributePackage Example"
      print *, "--------------------------------------- "
  endif

    call ESMF_Finalize(rc=rc)

10 continue
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_AttributePackageEx.F90"
  else
    print *, "FAIL: ESMF_AttributePackageEx.F90"
  endif
  
end program
