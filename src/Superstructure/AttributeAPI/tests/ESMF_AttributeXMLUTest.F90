! $Id: ESMF_AttributeXMLUTest.F90,v 1.3 2010/07/15 16:45:21 eschwab Exp $
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
      use ESMF_Mod         ! the ESMF Framework
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_AttributeXMLUTest.F90,v 1.3 2010/07/15 16:45:21 eschwab Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name, attrValue

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! Local variables  
      integer                 :: ordinal, rc 
      type(ESMF_GridComp)     :: gridcomp, gridcomp2
      character(ESMF_MAXSTR)  :: conv, purp
      
      character(ESMF_MAXSTR),dimension(3)   :: nestConv, nestPurp
      character(ESMF_MAXSTR),dimension(3)   :: attrList         

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
  !           <gco:CharacterString>Gerard Devine</gco:CharacterString>
  !         </gmd:individualName>
  !         <gmd:contactInfo>
  !           <gmd:CI_Contact>
  !             <gmd:address>
  !               <gmd:CI_Address>
  !                 <gmd:deliveryPoint>
  !                   <gco:CharacterString>
  !                     Department of Meteorology University of Reading Earley Gate, Reading Devine
  !                   </gco:CharacterString>
  !                 </gmd:deliveryPoint>
  !                 <gmd:electronicMailAddress>
  !                   <gco:CharacterString>
  !                     g.m.devine@reading.ac.uk
  !                   </gco:CharacterString>
  !                 </gmd:electronicMailAddress>
  !               </gmd:CI_Address>
  !             </gmd:address>
  !             <gmd:onlineResource>
  !               <gmd:CI_OnlineResource>
  !                 <gmd:linkage>
  !                   <gmd:URL>
  !                     www.nerc.ac.uk
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
  !       <abbreviation>GD</abbreviation>
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
    ! Construct gmd:address sub-node from the bottom up
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Create 2 leaf attribute nodes on the gridded component; 1st leaf
    !  physical address
    attrList(1) = 'gco:CharacterString'
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                     purpose='gmd:deliveryPoint', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 1st attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Create 2 leaf attribute nodes on the gridded component; 2nd leaf
    !  email address
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                     purpose='gmd:electronicMailAddress', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 2nd attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Now create parent node to include the 2 children nodes created above
    nestConv(1) = 'CIM'
    nestPurp(1) = 'gmd:deliveryPoint'
    nestConv(2) = 'CIM'
    nestPurp(2) = 'gmd:electronicMailAddress'
    attrList(1) = 'description'   ! filler, since API requires at least 1
                                  ! TODO: allow 0-item attrList (no attrList,
                                  ! just nested attPacks (children))
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
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
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                     purpose='gmd:address', &
                                     attrList=attrList, count=1, &
                                     nestConvention='CIM', &
                                     nestPurpose='gmd:CI_Address', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 2nd parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Set the attribute values within the 2 child nodes in the tree; child 1
    !  physical address
    call ESMF_AttributeSet(gridcomp, 'gco:CharacterString', &
      'Department of Meteorology University of Reading Earley Gate, Reading', &
        convention='CIM', purpose='gmd:deliveryPoint', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Set the attribute values within the 2 child nodes in the tree; child 2
    !  email address
    call ESMF_AttributeSet(gridcomp, 'gco:CharacterString', &
      'g.m.devine@reading.ac.uk', &
        convention='CIM', purpose='gmd:electronicMailAddress', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Set the attribute values within the 2 child nodes in the tree; dummy 1
      call ESMF_AttributeSet(gridcomp, 'description', &
        'dummy comment1', &
          convention='CIM', purpose='gmd:CI_Address', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !NEX_UTest
    ! Set the attribute values within the 2 child nodes in the tree; dummy 2
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment2', &
        convention='CIM', purpose='gmd:address', rc=rc)
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
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                     purpose='gmd:linkage', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 3rd attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Now create parent node to include the child node created above
    attrList(1) = 'description'   ! filler, since API requires at least 1 attr
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                       purpose='gmd:CI_OnlineResource', &
                                       attrList=attrList, count=1, &
                                       nestConvention='CIM', &
                                       nestPurpose='gmd:linkage', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 3rd parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create 2nd parent node on top of parent created above
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                     purpose='gmd:onlineResource', &
                                     attrList=attrList, count=1, &
                                     nestConvention='CIM', &
                                     nestPurpose='gmd:CI_OnlineResource', &
                                     rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 4th parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within the child node in the tree: URL
    call ESMF_AttributeSet(gridcomp, 'gmd:URL', &
      'www.reading.ac.uk', &
      convention='CIM', purpose='gmd:linkage', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within a child node in the tree: dummy3
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment3', &
      convention='CIM', purpose='gmd:CI_OnlineResource', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within a child node in the tree: dummy4
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment4', &
      convention='CIM', purpose='gmd:onlineResource', rc=rc)
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
    nestConv(1) = 'CIM'
    nestPurp(1) = 'gmd:address'
    nestConv(2) = 'CIM'
    nestPurp(2) = 'gmd:onlineResource'
    attrList(1) = 'description'   ! filler, since API requires at least 1 attr
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
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
    call ESMF_AttributeSet(gridcomp, 'description', &
        'dummy comment5', &
        convention='CIM', purpose='gmd:CI_Contact', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th dummy attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create 2nd parent node on top of parent created above
    attrList(1) = 'description'   ! filler, since API requires at least 1 attr
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                     purpose='gmd:contactInfo', &
                                     attrList=attrList, count=1, &
                                     nestConvention='CIM', &
                                     nestPurpose='gmd:CI_Contact', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 6th parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within a child node in the tree: dummy6
      call ESMF_AttributeSet(gridcomp, 'description', &
        'dummy comment6', &
        convention='CIM', purpose='gmd:contactInfo', rc=rc)
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
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                     purpose='gmd:individualName', &
                                     attrList=attrList, count=1, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 4th attribute node leaf test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within the child node in the tree: individualName
    call ESMF_AttributeSet(gridcomp, 'gco:CharacterString', &
      'Gerard Devine', &
      convention='CIM', purpose='gmd:individualName', rc=rc)
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
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
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
      convention='CIM', purpose='gmd:role', rc=rc)
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
    nestConv(1) = 'CIM'
    nestPurp(1) = 'gmd:contactInfo'
    nestConv(2) = 'CIM'
    nestPurp(2) = 'gmd:individualName'
    nestConv(3) = 'CIM'
    nestPurp(3) = 'gmd:role'
    attrList(1) = 'description'   ! filler, since API requires at least 1 attr
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
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
    call ESMF_AttributeSet(gridcomp, 'description', &
      'dummy comment7', &
      convention='CIM', purpose='gmd:CI_ResponsibleParty', rc=rc)
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
    call ESMF_AttributeAdd(gridcomp, convention='CIM', &
                                     purpose='responsibleParty', &
                                     attrList=attrList, count=2, &
                                     nestConvention='CIM', &
                                     nestPurpose='gmd:CI_ResponsibleParty', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating 8th parent attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the attribute value within the child node in the tree: abbreviation
    call ESMF_AttributeSet(gridcomp, 'abbreviation', 'GD', &
      convention='CIM', purpose='responsibleParty', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 6th child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! set XML comment (not actually part of compter-readable file -- filtered
    ! out by parsers -- only for human readability)
    call ESMF_AttributeSet(gridcomp, 'description', &
      'responsibleParty uri :: c8594af0-283f-11df-a0dc-001de019e26d', &
      convention='CIM', purpose='responsibleParty', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 7th child attribute node test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write out the attribute tree as an XML file
    call ESMF_AttributeWrite(gridcomp, 'CIM', 'responsibleParty', &
      attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write out custom RP XML file test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

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

    ! call ESMF_AttributeWrite(gridcomp2, 'CIM 1.0', &
    !                          'Model Component Simulation Description', &
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
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating std CIM responsibleParty attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! call ESMF_AttributeWrite(gridcomp2, 'CIM 1.0', &
    !                          'Model Component Simulation Description', &
    !                          attwriteflag=ESMF_ATTWRITE_XML,rc=rc)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create duplicate standard CIM attribute package on the gridded component
    call ESMF_AttributeAdd(gridcomp2, &
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating duplicate std CIM responsibleParty attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! call ESMF_AttributeRemove(gridcomp2, &
    !                 convention='CIM 1.0', &
    !                 purpose='Model Component Simulation Description', rc=rc)
    ! call ESMF_AttributeWrite(gridcomp2, 'CIM 1.0', &
    !                          'Model Component Simulation Description', &
    !                          attwriteflag=ESMF_ATTWRITE_XML,rc=rc)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st attribute value within the 1st CIM RP package
    ordinal = 1
    call ESMF_AttributeSet(gridcomp2, 'IndividualName', 'Bugs Bunny', &
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
    !       convention='ISO 19115', purpose='Responsible Party Description', &
                           ordinal=ordinal, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st attribute value in 1st CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st attribute value within the 2nd CIM RP package
    ordinal = 2
    call ESMF_AttributeSet(gridcomp2, 'IndividualName', 'Pink Panther', &
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
    !       convention='ISO 19115', purpose='Responsible Party Description', &
                           ordinal=ordinal, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st attribute value in 2nd CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Get the 1st attribute value within the 2nd CIM RP package
    ordinal = 2
    call ESMF_AttributeGet(gridcomp2, 'IndividualName', value=attrValue, &
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
    !       convention='ISO 19115', purpose='Responsible Party Description', &
                           ordinal=ordinal, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get 1st attribute value in 2nd CIM RP package test"
    call ESMF_Test((attrValue=='Pink Panther' .and. rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !print *, "attrValue = ", attrValue

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Get the 1st attribute value within the 1st CIM RP package
    ordinal = 1
    call ESMF_AttributeGet(gridcomp2, 'IndividualName', value=attrValue, &
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
    !       convention='ISO 19115', purpose='Responsible Party Description', &
                           ordinal=ordinal, &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get 1st attribute value in 1st CIM RP package test"
    call ESMF_Test((attrValue=='Bugs Bunny' .and. rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !print *, "attrValue = ", attrValue

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Get the 1st attribute value within the 2nd CIM RP package
    !  default ordinal=2 (last one)
    call ESMF_AttributeGet(gridcomp2, 'IndividualName', value=attrValue, &
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
    !       convention='ISO 19115', purpose='Responsible Party Description', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get 1st attribute value in 2nd CIM RP package test (default ordinal=2"
    call ESMF_Test((attrValue=='Pink Panther' .and. rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !print *, "attrValue = ", attrValue

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Create triplicate standard CIM attribute package on the gridded component
    call ESMF_AttributeAdd(gridcomp2, &
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating triplicate std CIM responsibleParty attribute package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 1st attribute value within the last CIM RP package
    call ESMF_AttributeSet(gridcomp2, 'IndividualName', 'Gerard Devine', &
                           convention='CIM 1.0', &
                           purpose='Model Component Simulation Description', &
    !       convention='ISO 19115', purpose='Responsible Party Description', &
                           rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 1st attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 2nd attribute value within the last CIM RP package
    call ESMF_AttributeSet(gridcomp2, 'IndividualPhysicalAddress', &
      'Department of Meteorology University of Reading Earley Gate, Reading Devine', &
      convention='ISO 19115', purpose='Responsible Party Description', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 2nd attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 3rd attribute value within the last CIM RP package
      call ESMF_AttributeSet(gridcomp2, 'IndividualEmailAddress', &
                                       'g.m.devine@reading.ac.uk', &
      convention='ISO 19115', purpose='Responsible Party Description', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 3rd attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 4th attribute value within the last CIM RP package
    call ESMF_AttributeSet(gridcomp2, 'IndividualURL', &
                                     'www.nerc.ac.uk', &
      convention='ISO 19115', purpose='Responsible Party Description', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 4th attribute value in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !-------------------------------------------------------------------------
  !   XML element 'attribute' test. Uses built-in, standard CIM XML
  !     responsibleParty node package as a test case
  !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Set the 5th attribute value within the last CIM RP package
    ! This is set as an XML element attribute to ensure proper output format
    call ESMF_AttributeSet(gridcomp2, 'IndividualRole', 'author', &
                                      attrAttribute=.true., &
      convention='ISO 19115', purpose='Responsible Party Description', rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set 5th attribute value (XML element attribute) in last CIM RP package test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! call ESMF_AttributeRemove(gridcomp2, &
    !                 convention='CIM 1.0', &
    !                 purpose='Model Component Simulation Description', rc=rc)

    !-------------------------------------------------------------------------
    !EX_UTest
    ! Write out the attribute tree as a CIM-formatted XML file
    call ESMF_AttributeWrite(gridcomp2, 'CIM 1.0', &
                                 'Model Component Simulation Description', &
    !call ESMF_AttributeWrite(gridcomp2, 'ISO 19115', &
    !                            'Responsible Party Description', &
      attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Write out CIM RP XML file test"
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    ! clean up
    call ESMF_GridCompDestroy(gridcomp2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
#endif
    call ESMF_GridCompDestroy(gridcomp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    !------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeXMLUTest
