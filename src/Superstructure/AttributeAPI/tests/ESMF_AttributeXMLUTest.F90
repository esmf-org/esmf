! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_AttributeXMLUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeXMLUTest - Attribute XML Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute unit tests of features supporing
! the creation of XML files.  These include: multi-child trees; XML element
! 'attributes'; and multiple attribute packages of the same type, created at
! the same tree node level, on component objects.
! The companion file ESMF\_Attribute.F90 contains the definitions for the
! Attribute methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF         ! the ESMF Framework
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name, attrValue

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! Local variables  
      type(ESMF_AttPack)    :: attpack   
      integer                 :: ordinal, rc 
      type(ESMF_GridComp)     :: gridcomp, gridcomp2, gridcomp3
      type(ESMF_Field)        :: field1
      type(ESMF_FieldBundle)  :: fieldBundle
      type(ESMF_State)        :: importState
      character(ESMF_MAXSTR)  :: conv, purp
      character(ESMF_MAXSTR)  :: convCIM, purpComp
      character(ESMF_MAXSTR)  :: purpField, purpPlatform
      character(ESMF_MAXSTR)  :: convISO, purpRP, purpCitation
      
      character(ESMF_MAXSTR),dimension(3)   :: nestConv, nestPurp
      character(ESMF_MAXSTR),dimension(3)   :: attrList         

      logical :: rc_logical
      character(ESMF_MAXSTR), dimension(4) :: exclusions

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

!-------------------------------------------------------------------------
!  GRIDCOMP
!-------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  !   Multi-child tree construction tests. Uses CIM XML responsibleParty node
  !     as a test case, building it up as a tree of custom attribute packages
  !     (not from a standard, built-in package):
  !
  !    <responsibleParty>
  !       <gmd:CI_ResponsibleParty>
  !         <gmd:individualName>
  !           <gco:CharacterString>John Doe</gco:CharacterString>
  !         </gmd:individualName>
  !         <gmd:contactInfo>
  !           <gmd:CI_Contact>
  !             <gmd:address>
  !               <gmd:CI_Address>
  !                 <gmd:deliveryPoint>
  !                   <gco:CharacterString>
  !                     Department of Meteorology, University of ABC
  !                   </gco:CharacterString>
  !                 </gmd:deliveryPoint>
  !                 <gmd:electronicMailAddress>
  !                   <gco:CharacterString>
  !                     john.doe@earthsys.org
  !                   </gco:CharacterString>
  !                 </gmd:electronicMailAddress>
  !               </gmd:CI_Address>
  !             </gmd:address>
  !             <gmd:onlineResource>
  !               <gmd:CI_OnlineResource>
  !                 <gmd:linkage>
  !                   <gmd:URL>
  !                     www.earthsys.org
  !                   </gmd:URL>
  !                 </gmd:linkage>
  !               </gmd:CI_OnlineResource>
  !             </gmd:onlineResource>
  !           </gmd:CI_Contact>
  !         </gmd:contactInfo>
  !         <gmd:role>
  !           <gmd:CI_RoleCode codeList="" codeListValue="author">
  !           </gmd:CI_RoleCode>
  !         </gmd:role>
  !       </gmd:CI_ResponsibleParty>
  !       <abbreviation>JD</abbreviation>
  !     </responsibleParty>
  !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Construct a gridded component ESMF object that will be decorated with
    ! the Attributes we will be manipulating
    gridcomp = ESMF_GridCompCreate(name="gridded_comp_cust_rp", petList=(/0/), &
                 rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a gridded component to decorate with Attributes test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    ! Setup the convention and purpose strings
    !-------------------------------------------------------------------------
    convCIM = 'CIM 1.5'
    convISO = 'ISO 19115'

    purpComp = 'ModelComp'
    purpField = 'Inputs'
    purpPlatform = 'Platform'
    purpRP = 'RespParty'
    purpCitation = 'Citation'

    !-------------------------------------------------------------------------
    ! Construct gmd:address sub-node from the bottom up
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Create 2 leaf attribute nodes on the gridded component; 1st leaf
    !  physical address
    attrList(1) = 'gco:CharacterString'
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:deliveryPoint', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 1st attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Create 2 leaf attribute nodes on the gridded component; 2nd leaf
    !  email address
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:electronicMailAddress', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 2nd attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Now create parent node to include the 2 children nodes created above
    nestConv(1) = convCIM
    nestPurp(1) = 'gmd:deliveryPoint'
    nestConv(2) = convCIM
    nestPurp(2) = 'gmd:electronicMailAddress'
    attrList(1) = 'description'   ! filler, since API requires at least 1
                                  ! TODO: allow 0-item attrList (no attrList,
                                  ! just nested attPacks (children))
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:CI_Address', &
                                     attrList=attrList, count=1, &
                                     nestConvention=nestConv, &
                                     nestPurpose=nestPurp, nestCount=2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 1st parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Now create 2nd parent node to include the 1 child node (1st parent)
    !   created above
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:address', &
                                     attrList=attrList, count=1, &
                                     nestConvention=convCIM, &
                                     nestPurpose='gmd:CI_Address', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 2nd parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Set the attribute values within the 2 child nodes in the tree; child 1
    !  physical address
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:deliveryPoint', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'gco:CharacterString', &
      'Department of Meteorology, University of ABC', &
        convention=convCIM, purpose='gmd:deliveryPoint', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Set the attribute values within the 2 child nodes in the tree; child 2
    !  email address
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:electronicMailAddress', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'gco:CharacterString', &
      'john.doe@earthsys.org', &
        convention=convCIM, purpose='gmd:electronicMailAddress', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Set the attribute values within the 2 child nodes in the tree; dummy 1
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:CI_Address', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment1', attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Set the attribute values within the 2 child nodes in the tree; dummy 2
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:address', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment2', &
        convention=convCIM, purpose='gmd:address', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

    !-------------------------------------------------------------------------
    ! Construct gmd:onlineResource sub-node from the bottom up
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create leaf attribute node on the gridded component
     attrList(1) = 'gmd:URL'
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:linkage', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 3rd attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Now create parent node to include the child node created above
    attrList(1) = 'description'   ! filler, since API requires at least 1 attr
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                       purpose='gmd:CI_OnlineResource', &
                                       attrList=attrList, count=1, &
                                       nestConvention=convCIM, &
                                       nestPurpose='gmd:linkage', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 3rd parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create 2nd parent node on top of parent created above
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:onlineResource', &
                                     attrList=attrList, count=1, &
                                     nestConvention=convCIM, &
                                     nestPurpose='gmd:CI_OnlineResource', &
                                     rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 4th parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within the child node in the tree: URL
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:linkage', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'gmd:URL', &
      'www.earthsys.org', &
      convention=convCIM, purpose='gmd:linkage', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within a child node in the tree: dummy3
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:CI_OnlineResource', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment3', &
      convention=convCIM, purpose='gmd:CI_OnlineResource', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within a child node in the tree: dummy4
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:onlineResource', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment4', &
      convention=convCIM, purpose='gmd:onlineResource', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 4th dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    ! Construct gmd:contactInfo, containing gmd:address & gmd:onlineResource
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Now create parent node to include the 2 child nodes created above
    nestConv(1) = convCIM
    nestPurp(1) = 'gmd:address'
    nestConv(2) = convCIM
    nestPurp(2) = 'gmd:onlineResource'
    attrList(1) = 'description'   ! filler, since API requires at least 1 attr
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:CI_Contact', &
                                     attrList=attrList, count=1, &
                                     nestConvention=nestConv, &
                                     nestPurpose=nestPurp, nestCount=2, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 5th parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within a child node in the tree: dummy5
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:CI_Contact', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'description', &
        'dummy comment5', &
        convention=convCIM, purpose='gmd:CI_Contact', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create 2nd parent node on top of parent created above
    attrList(1) = 'description'   ! filler, since API requires at least 1 attr
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:contactInfo', &
                                     attrList=attrList, count=1, &
                                     nestConvention=convCIM, &
                                     nestPurpose='gmd:CI_Contact', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 6th parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within a child node in the tree: dummy6
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:contactInfo', attpack=attpack, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'description', &
        'dummy comment6', &
        convention=convCIM, purpose='gmd:contactInfo', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 6th dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    ! Construct gmd:individualName sub-node
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create leaf attribute node on the gridded component
    attrList(1) = 'gco:CharacterString'
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:individualName', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 4th attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within the child node in the tree: individualName
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:individualName', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'gco:CharacterString', &
      'John Doe', &
      convention=convCIM, purpose='gmd:individualName', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 4th child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    ! Construct gmd:role sub-node
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create leaf attribute node on the gridded component
    attrList(1) = 'gmd:CI_RoleCode'
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:role', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 5th attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within the child node in the tree: role
    call ESMF_AttributeSet(gridcomp, 'gmd:CI_RoleCode', &
      'author', &
      convention=convCIM, purpose='gmd:role', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    ! Construct gmd:CI_ResponsibleParty, containing gmd:individualName,
    !   gmd:contactInfo, gmd:role
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Now create parent node to include the 3 child nodes created above
    nestConv(1) = convCIM
    nestPurp(1) = 'gmd:contactInfo'
    nestConv(2) = convCIM
    nestPurp(2) = 'gmd:individualName'
    nestConv(3) = convCIM
    nestPurp(3) = 'gmd:role'
    attrList(1) = 'description'   ! filler, since API requires at least 1 attr
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='gmd:CI_ResponsibleParty', &
                                     attrList=attrList, count=1, &
                                     nestConvention=nestConv, &
                                     nestPurpose=nestPurp, nestCount=3, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 7th parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within a child node in the tree: dummy7
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'gmd:CI_ResponsibleParty', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment7', &
      convention=convCIM, purpose='gmd:CI_ResponsibleParty', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 7th dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    ! Finally, construct the top-level gmd:responsibleParty, containing
    !   gmd:CI_ResponsibleParty and attribute 'abbreviation'
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Now create parent node to include the 1 child node created above, and
    ! add new attribute 'abbreviation'
    attrList(1) = 'description'  ! use to hold XML comment
    attrList(2) = 'abbreviation'
    call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
                                     purpose='responsibleParty', &
                                     attrList=attrList, count=2, &
                                     nestConvention=convCIM, &
                                     nestPurpose='gmd:CI_ResponsibleParty', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 8th parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within the child node in the tree: abbreviation
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'responsibleParty', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'abbreviation', 'JD', &
      convention=convCIM, purpose='responsibleParty', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 6th child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! set XML comment (not actually part of compter-readable file -- filtered
    ! out by parsers -- only for human readability)
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'responsibleParty', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'description', &
      'responsibleParty uri :: d8397bf0-223f-31df-b0dc-001ce029e16b', &
      convention=convCIM, purpose='responsibleParty', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 7th child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write out the attribute tree as an XML file
    call ESMF_AttributeWrite(gridcomp, convCIM, 'responsibleParty', &
      attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write out custom RP XML file test"
    call ESMF_Test((rc==ESMF_SUCCESS .or. rc==ESMF_RC_LIB_NOT_PRESENT), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! compare the output file to the baseline file
    exclusions(1) = "ESMF Version"
    rc_logical = ESMF_TestFileCompare('gridded_comp_cust_rp.xml', &
      'baseline_gridded_comp_cust_rp.xml', exclusionList=exclusions)
    write(failMsg, *) "Did not return True"
    write(name, *) "Compare the XML output file to the baseline file"
    call ESMF_Test(rc_logical.eqv..true., name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  !   Multiple Attribute Packages of the same (conv,purp) tests. Uses built-in,
  !   standard CIM XML responsibleParty node package as a test case
  !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct a gridded component ESMF object that will be decorated with
    ! the Attributes we will be manipulating
    gridcomp2 = ESMF_GridCompCreate(name="gridded_comp_cim_rp", petList=(/0/), &
                 rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a gridded component to decorate with Attributes test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! call ESMF_AttributeWrite(gridcomp2, convCIM, &
    !                          purpComp, &
    !                          attwriteflag=ESMF_ATTWRITE_XML,rc=rc)

    !-------------------------------------------------------------------------
    !EX__UTest
    ! TODO: try this instead of creating separate gridcomp2 below (another
    !       variation of the multiple-attpack-of-the-same-type test)
    ! To create a xml output file separate from the above tests, rename the
    ! gridded component ESMF object. It will be decorated with the standard
    ! built-in CIM Responsible Party attribute package
    !call ESMF_GridCompSet(gridcomp, name="gridded_comp_cim_rp", rc=rc)
    !write(failMsg, *) "Did not return ESMF_SUCCESS"
    !write(name, *) "Renaming a gridded component Attributes test"
    !call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create standard CIM attribute package on the gridded component
    call ESMF_AttributeAdd(gridcomp2, &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating std CIM responsibleParty attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! call ESMF_AttributeWrite(gridcomp2, convCIM, &
    !                          purpComp, &
    !                          attwriteflag=ESMF_ATTWRITE_XML,rc=rc)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create duplicate standard CIM attribute package on the gridded component
    call ESMF_AttributeAdd(gridcomp2, &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating duplicate std CIM responsibleParty attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! call ESMF_AttributeRemove(gridcomp2, &
    !                 convention=convCIM, &
    !                 purpose=purpComp, rc=rc)
    ! call ESMF_AttributeWrite(gridcomp2, convCIM, &
    !                          purpComp, &
    !                          attwriteflag=ESMF_ATTWRITE_XML,rc=rc)

    !-------------------------------------------------------------------------
#if 0
! TODO: test attPackInstanceName when implemented (replaces ordinal)
    !EX__UTest
    ! Set the 1st attribute value within the 1st CIM RP package
    ordinal = 1
    call ESMF_AttributeSet(gridcomp2, 'Name', 'Jane Doe', &
                           convention=convCIM, &
                           purpose=purpComp, &
    !       convention=convISO, purpose=purpRP, &
                           ordinal=ordinal, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st attribute value in 1st CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX__UTest
    ! Set the 1st attribute value within the 2nd CIM RP package
    ordinal = 2
    call ESMF_AttributeSet(gridcomp2, 'Name', 'Sally Doe', &
                           convention=convCIM, &
                           purpose=purpComp, &
    !       convention=convISO, purpose=purpRP, &
                           ordinal=ordinal, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st attribute value in 2nd CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX__UTest
    ! Get the 1st attribute value within the 2nd CIM RP package
    ordinal = 2
    call ESMF_AttributeGet(gridcomp2, 'Name', value=attrValue, &
                           convention=convCIM, &
                           purpose=purpComp, &
    !       convention=convISO, purpose=purpRP, &
                           ordinal=ordinal, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get 1st attribute value in 2nd CIM RP package test"
    call ESMF_Test((attrValue=='Pink Panther' .and. rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !print *, "attrValue = ", attrValue

    !-------------------------------------------------------------------------
    !EX__UTest
    ! Get the 1st attribute value within the 1st CIM RP package
    ordinal = 1
    call ESMF_AttributeGet(gridcomp2, 'Name', value=attrValue, &
                           convention=convCIM, &
                           purpose=purpComp, &
    !       convention=convISO, purpose=purpRP, &
                           ordinal=ordinal, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get 1st attribute value in 1st CIM RP package test"
    call ESMF_Test((attrValue=='Bugs Bunny' .and. rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !print *, "attrValue = ", attrValue

    !-------------------------------------------------------------------------
    !EX__UTest
    ! Get the 1st attribute value within the 2nd CIM RP package
    !  default ordinal=2 (last one)
    call ESMF_AttributeGet(gridcomp2, 'Name', value=attrValue, &
                           convention=convCIM, &
                           purpose=purpComp, &
    !       convention=convISO, purpose=purpRP, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get 1st attribute value in 2nd CIM RP package test (default ordinal=2)"
    call ESMF_Test((attrValue=='Pink Panther' .and. rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !print *, "attrValue = ", attrValue

#endif
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create triplicate standard CIM attribute package on the gridded component
    call ESMF_AttributeAdd(gridcomp2, &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating triplicate std CIM responsibleParty attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st attribute value within the last CIM RP package
    call ESMF_AttributeGetAttPack(gridcomp2, convCIM, &
      purpComp, attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp2, 'Name', 'John Doe', &
                           attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    call ESMF_AttributeGetAttPack(gridcomp2, convISO, &
      purpRP, attpack=attpack, rc=rc)
    ! Set the 2nd attribute value within the last CIM RP package
    call ESMF_AttributeSet(gridcomp2, 'PhysicalAddress', &
      'Department of Meteorology, University of ABC', &
      attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 3rd attribute value within the last CIM RP package
    call ESMF_AttributeSet(gridcomp2, 'EmailAddress', &
                                      'john.doe@earthsys.org', &
                                      attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 4th attribute value within the last CIM RP package
    call ESMF_AttributeSet(gridcomp2, 'URL', &
                                      'www.earthsys.org', &
                                      attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 4th attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 5th attribute value within the last CIM RP package
    ! This is set as an XML element attribute to ensure proper output format
    call ESMF_AttributeSet(gridcomp2, 'ResponsiblePartyRole', 'author', &
      attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! call ESMF_AttributeRemove(gridcomp2, &
    !                 convention=convCIM, &
    !                 purpose=purpComp, rc=rc)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write out the attribute tree as a CIM-formatted XML file
    call ESMF_AttributeWrite(gridcomp2, convCIM, &
                                 purpComp, &
    !call ESMF_AttributeWrite(gridcomp2, convISO, &
    !                            purpRP, &
      attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write out CIM RP XML file test"
    call ESMF_Test((rc==ESMF_SUCCESS .or. rc==ESMF_RC_LIB_NOT_PRESENT), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! compare the output file to the baseline file
    exclusions(1) = "ESMF Version"
    exclusions(2) = "documentID"
    exclusions(3) = "documentCreationDate"
    rc_logical = ESMF_TestFileCompare('gridded_comp_cim_rp.xml', &
      'baseline_gridded_comp_cim_rp.xml', exclusionList=exclusions)
    write(failMsg, *) "Did not return True"
    write(name, *) "Compare the XML output file to the baseline file"
    call ESMF_Test(rc_logical.eqv..true., name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  !   <CIMDocument> attribute representation and output test for
  !   <modelComponent>, <simulationRun>, including <input>s (fields) and
  !   <platform>. Uses built-in, standard CIM packages.
  !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct a gridded component ESMF object that will be decorated with
    ! Attributes to output <CIMDocument>s
    gridcomp3 = ESMF_GridCompCreate(name="gridded_comp_cim", petList=(/0/), &
                 rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a gridded component to decorate with Attributes test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create standard CIM attribute package on the gridded component
    call ESMF_AttributeAdd(gridcomp3, &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating std CIM responsibleParty attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    ! <modelComponent> attributes
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st <modelComponent> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'ShortName', 'EarthSys_Atmos', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st <modelComponent> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 2nd <modelComponent> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'LongName', 'Earth System High Resolution Global Model', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd <modelComponent> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 3rd <modelComponent> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'Description',  &
      'EarthSys brings together expertise from the global ' // &
      'community in a concerted effort to develop coupled ' // &
      'climate models with increased horizontal resolutions.  ' // &
      'Increasing the horizontal resolution of coupled climate ' // &
      'models will allow us to capture climate processes and ' // &
      'weather systems in much greater detail.', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd <modelComponent> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 4th <modelComponent> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'ReleaseDate', '2009-02-15T01:02:03Z', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th <modelComponent> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 5th <modelComponent> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'ModelType', 'aerosol', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 6th <modelComponent> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    ! <simulationRun> attributes
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st <simulationRun> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'SimulationShortName', '1.1_EarthSys_Sim', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st <simulationRun> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 2nd <simulationRun> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'SimulationLongName', 'EarthSys Simulation for Experiment 1.1', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd <simulationRun> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 3rd <simulationRun> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'SimulationRationale', &
     'EarthSys simulation run in repsect to CMIP5 core experiment 1.1 (Decadal)', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd <simulationRun> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 4th <simulationRun> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'SimulationStartDate', &
     '1960-01-01T00:00:00Z', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 4th <simulationRun> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 5th <simulationRun> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'SimulationDuration', 'P10Y', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th <simulationRun> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    ! <documentGenealogy>
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st <documentGenealogy> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'PreviousVersion', &
      'EarthSys1 Atmosphere', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st <documentGenealogy> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 2nd <documentGenealogy> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'PreviousVersionDescription', &
     'Horizontal resolution increased to 1.20 x 0.80 degrees; ' // &
     'Timestep reduced from 30 minutes to 15 minutes.', &
                           convention=convCIM, &
                           purpose=purpComp, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd <documentGenealogy> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#if 0
    ! TODO: uncomment and expand when we have better definition from CIM
    !-------------------------------------------------------------------------
    ! <componentProperty> Scientific Property Description package
    !-------------------------------------------------------------------------
    !EX__UTest
    ! Set the 1st <componentProperty> attribute value within the CIM component
    !   package
    call ESMF_AttributeGetAttPack(gridcomp, convCIM, &
      'Scientific Property Description', attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp3, 'ScientificPropertyShortName', &
      'TimeStep', &
                      convention=convCIM, &
                      purpose='Scientific Property Description', &
                      rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st <componentProperty> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX__UTest
    ! Set the 2nd <componentProperty> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'ScientificPropertyLongName', &
      'Clock time step', &
                      convention=convCIM, &
                      purpose='Scientific Property Description', &
                      rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd <componentProperty> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX__UTest
    ! Set the 3rd <componentProperty> attribute value within the CIM component
    !   package
    call ESMF_AttributeSet(gridcomp3, 'ScientificPropertyValue', &
      '20 mins', &
                      convention=convCIM, &
                      purpose='Scientific Property Description', &
                      rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd <componentProperty> attribute value in CIM component package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

    !-------------------------------------------------------------------------
    ! <platform> Platform Description package
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeGetAttPack(gridcomp3, convCIM, &
      purpPlatform, attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp3, 'MachineDescription', &
      'HECToR (Phase 2a) is currently an integrated system known ' // &
      'as Rainier, which includes a scalar MPP XT4 system, a vector ' // &
      'system known as BlackWidow, and storage systems.', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 2nd <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'MachineName', &
      'HECToR', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 3rd <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'MachineOperatingSystem', &
      'Unicos', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 4th <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'MachineMaximumProcessors', &
      '22656', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 4th <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 5th <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'MachineProcessorType', &
      'AMD X86_64', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 6th <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'MachineCoresPerProcessor', &
      '4', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 6th <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 7th <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'MachineVendor', &
      'Cray Inc', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 7th <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 8th <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'CompilerName', &
      'Pathscale', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 8th <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 9th <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'CompilerVersion', &
      '3.0', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 9th <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 10th <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'MachineInterconnectType', &
      'Cray Interconnect', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 10th <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 11th <platform> attribute value within the CIM platform
    !   package
    call ESMF_AttributeSet(gridcomp3, 'MachineSystem', &
      'Parallel', &
                           convention=convCIM, &
                           purpose=purpPlatform, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 11th <platform> attribute value in CIM platform package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    ! <citation> Citation Description package
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st <citation> attribute value within the CIM citation
    !   package
    call ESMF_AttributeGetAttPack(gridcomp3, convISO, &
      purpCitation, attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp3, 'ShortTitle', &
      'Doe_2009', &
                           convention=convISO, &
                           purpose=purpCitation, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st <citation> attribute value in CIM citation package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 2nd <citation> attribute value within the CIM citation
    !   package
    call ESMF_AttributeSet(gridcomp3, 'LongTitle', &
     'Doe, J.A.; Norton, A.B.; ' // &
     'Clark, G.H.; Davies, I.J.. 2009 EarthSys: ' // &
     'The Earth System High Resolution Global Atmosphere Model - Model ' // &
     'description and basic evaluation. Journal of Climate, 15 (2). ' // &
     '1261-1296.', &
                           convention=convISO, &
                           purpose=purpCitation, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd <citation> attribute value in CIM citation package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 3rd <citation> attribute value within the CIM citation
    !   package
    call ESMF_AttributeSet(gridcomp3, 'Date', &
      '2009-04-20', &
                           convention=convISO, &
                           purpose=purpCitation, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd <citation> attribute value in CIM citation package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 4th <citation> attribute value within the CIM citation
    !   package
    call ESMF_AttributeSet(gridcomp3, 'PresentationForm', &
      'Online Refereed', &
                           convention=convISO, &
                           purpose=purpCitation, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 4th <citation> attribute value in CIM citation package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 5th <citation> attribute value within the CIM citation
    !   package
    call ESMF_AttributeSet(gridcomp3, 'DOI', &
      'doi:17.1035/2009JCLI4508.1', &
                           convention=convISO, &
                           purpose=purpCitation, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th <citation> attribute value in CIM citation package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    ! <responsibleParty> Responsible Party package
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st attribute value within the CIM RP package
    ! This sets <gmd:individualName> and <gmd:role> codeListValue='author'
    !call ESMF_AttributeSet(gridcomp3, 'PrincipalInvestigator','John Doe', &
    call ESMF_AttributeGetAttPack(gridcomp3, convISO, &
      purpRP, attpack=attpack, rc=rc)
    call ESMF_AttributeSet(gridcomp3, 'Name','John Doe', &
      convention=convISO, purpose=purpRP, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 2nd attribute value within the CIM RP package
    call ESMF_AttributeSet(gridcomp3, 'PhysicalAddress', &
      'Department of Meteorology, University of ABC', &
      convention=convISO, purpose=purpRP, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 3rd attribute value within the CIM RP package
    call ESMF_AttributeSet(gridcomp3, 'EmailAddress', &
                                       'john.doe@earthsys.org', &
      convention=convISO, purpose=purpRP, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 5th attribute value within the CIM RP package
    call ESMF_AttributeSet(gridcomp3, 'ResponsiblePartyRole', 'author', &
      convention=convISO, purpose=purpRP, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th attribute value in CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    ! <input>
    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct a field ESMF object that will be decorated with
    ! Attributes to output within <input>s in a <simulationRun>
    field1 = ESMF_FieldEmptyCreate(name="DMS_emi", rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a field to decorate with Attributes test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create standard CIM attribute package on the field
    call ESMF_AttributeAdd(field1, &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating std CIM Inputs attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st <input> attribute value within the CIM field
    !   package
    call ESMF_AttributeSet(field1, 'ShortName', 'DMS_emi', &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st <input> attribute value in CIM field package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 2nd <input> attribute value within the CIM field
    !   package
    call ESMF_AttributeSet(field1, 'CouplingPurpose', 'Boundary', &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd <input> attribute value in CIM field package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 3rd <input> attribute value within the CIM field
    !   package
    call ESMF_AttributeSet(field1, 'CouplingSource', 'EarthSys_Atmos', &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd <input> attribute value in CIM field package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 4th <input> attribute value within the CIM field
    !   package
    call ESMF_AttributeSet(field1, 'CouplingTarget', 'EarthSys_AtmosDynCore', &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 4th <input> attribute value in CIM field package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 5th <input> attribute value within the CIM field
    !   package
    call ESMF_AttributeSet(field1, 'SpatialRegriddingMethod', &
     'Conservative-First-Order', &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th <input> attribute value in CIM field package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 6th <input> attribute value within the CIM field
    !   package
    call ESMF_AttributeSet(field1, 'SpatialRegriddingDimension', &
     '2D', &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 6th <input> attribute value in CIM field package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 7th <input> attribute value within the CIM field
    !   package
    call ESMF_AttributeSet(field1, 'Frequency', &
     '15 Minutes', &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 7th <input> attribute value in CIM field package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 8th <input> attribute value within the CIM field
    !   package
    call ESMF_AttributeSet(field1, 'TimeTransformationType', &
     'TimeAverage', &
                           convention=convCIM, &
                           purpose=purpField, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 8th <input> attribute value in CIM field package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct a fieldbundle ESMF object that will contain fields
    fieldBundle = ESMF_FieldBundleCreate(name="Field Bundle", rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a fieldbundle to contain fields test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Add the field to the field bundle (links attributes also)
    call ESMF_FieldBundleAdd(fieldBundle, (/ field1 /), rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Adding a field to a fieldbundle test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Construct an import state ESMF object that will contain a fieldbundle
    importState = ESMF_StateCreate(name="importState",  &
                                   stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating an import state test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Add a fieldbundle to the import state (links attributes also)
    call ESMF_StateAdd(importState, fieldbundleList=(/fieldBundle/), rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Adding a field bundle to an import state test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Link import state attributes to the gridded component
    call ESMF_AttributeLink(gridcomp3, importState, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Linking import state attributes to gridded component"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write out the attribute tree as a CIM-formatted XML file
    call ESMF_AttributeWrite(gridcomp3, convCIM, &
                                 purpComp, &
      attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write out CIM XML file test"
    call ESMF_Test((rc==ESMF_SUCCESS .or. rc==ESMF_RC_LIB_NOT_PRESENT), &
                    name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! compare the output file to the baseline file
    exclusions(1) = "ESMF Version"
    exclusions(2) = "documentID"
    exclusions(3) = "documentCreationDate"
    exclusions(4) = "<id>"
    rc_logical = ESMF_TestFileCompare('gridded_comp_cim.xml', &
      'baseline_gridded_comp_cim.xml', exclusionList=exclusions)
    write(failMsg, *) "Did not return True"
    write(name, *) "Compare the XML output file to the baseline file"
    call ESMF_Test(rc_logical.eqv..true., name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------


    !------------------------------------------------------------------------
    ! clean up
    call ESMF_FieldDestroy(field1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateDestroy(importState, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridcomp3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridcomp2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif
    call ESMF_GridCompDestroy(gridcomp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeXMLUTest
