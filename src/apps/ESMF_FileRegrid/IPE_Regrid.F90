program IPE_Regrid

! !USES:
  use ESMF
  use ESMF_ArrayMod
  use ESMF_UtilSortMod
  use ESMF_IOGridspecMod
  use ESMF_RegridWeightGenMod

#ifdef ESMF_NETCDF
      use netcdf
#endif
  implicit none

#ifndef ESMF_MPIUNI
  include "mpif.h"
#endif

  integer, parameter :: MAXNAMELEN = 64
  character(len=MAXNAMELEN) :: srcfilename, dstfilename
  type(ESMF_MESH) :: srcMesh, dstMesh
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srclon(:,:,:), srclat(:,:,:), srchgt(:,:),srcdata(:,:,:)
  real(ESMF_KIND_R8), pointer :: dstlon(:,:), dstlat(:,:), dsthgt(:), dstdata(:)
  real(ESMF_KIND_R8), pointer :: databuf(:)
  integer(ESMF_KIND_I4), pointer :: maxlevs(:)
  integer(ESMF_KIND_I4), pointer :: numPerRow(:), shuffleOrder(:)
  integer :: nc1, nc2
  integer :: varid
  integer :: ndims, dimids(3)
  integer :: srcdims(3), dstdims(2)
  integer :: PetNo, PetCnt
  integer(ESMF_KIND_I4), pointer :: elementIds(:), elementTypes(:), elementConn(:)
  integer(ESMF_KIND_I4), pointer :: nodeIds(:), nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer(ESMF_KIND_I4), pointer :: southind(:), northind(:), totallats(:), gap(:)
  integer :: minheight, maxheight, halo, neighbor, remind
  integer(ESMF_KIND_I4), pointer :: totalheight(:)
  real(ESMF_KIND_R8) :: lon, lat, hgt, lon1, lat1, hgt1
  real(ESMF_KIND_R4) :: interval
  integer :: i,j, k, ii, jj, kk, count1, count3, count8, localcount, countup, save, base, base1
  logical :: even
  integer :: start, count, diff, lastlat, totalelements, totalnodes, localnodes, startid
  integer :: srctotalnodes
  integer :: elmtcount, increment
  integer :: startlevel, next, ind, ind1, totalnodes2d, totallevels, myrows, trigs
  integer, pointer :: rowinds(:), petTable(:), baseind(:)
  integer(ESMF_KIND_I4), pointer :: elementCnt(:), nodeCnt(:), sendbuf(:), recvbuf(:)
  real(ESMF_KIND_R8), pointer :: conntbl(:), globalCoords(:,:), fptr2d(:,:), fptr1d(:)
  type(ESMF_Arrayspec) :: arrayspec
  type(ESMF_Array) :: array, array1, array2
  type(ESMF_Field) :: srcField, dstField
  type(ESMF_RouteHandle) :: routehandle
  real(ESMF_KIND_R8) :: maxerror, minerror, totalerrors, deg2rad
  real(ESMF_KIND_R8) :: starttime, endtime
  real(ESMF_KIND_R8) :: differr
  real(ESMF_KIND_R8), pointer :: weights(:)
  integer(ESMF_KIND_I4), pointer :: indices(:,:)
  character(len=80) :: filename
  integer :: wgtcount(1)
  integer, pointer :: allCounts(:)
  real, parameter :: PI=3.1415927
  integer :: rc, status

  ! For output
  character(len=MAXNAMELEN) :: srcoutfile, dstoutfile
  real(ESMF_KIND_R8), pointer :: lontbl(:), lattbl(:), hgttbl(:)
  integer :: lonid, latid, hgtid, vertid, elmtid, nodeid, numid, connid
  integer :: data1id, data2id, wgtid, srcid, dstid
  integer :: globalTotal, globalTotalelmt, nodestartid, totalwgts
  type(ESMF_Distgrid) :: nodalDistgrid, distgrid
  
  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  !------------------------------------------------------------------------
  ! get global vm information
  !
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  ! set up local pet info
  call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

  !------------------------------------------------------------------------

  srcfilename = 'data/ipe3dgrid.nc'
  dstfilename = 'data/wam3dgrid.nc'
  srcoutfile = 'ipe3dmesh.nc'
  dstoutfile = 'wam3dmesh.nc'

  minheight = 90
  maxheight = 800
  deg2rad = PI/180.0
  ! Read in the source grid -- IPE
  status = nf90_open(path=srcfilename, mode=nf90_nowrite, ncid=nc1)
  call CheckNCError(status, srcfilename)
  status = nf90_inq_varid(nc1,'lons', varid)
  call CheckNCError(status, 'lons')
  status = nf90_inquire_variable(nc1, varid, ndims=ndims, dimids = dimids)
  call CheckNCError(status, 'lons')
  status = nf90_inquire_dimension(nc1,dimids(1), len=srcdims(1))
  call CheckNCError(status, 'lons 1st dimension')
  status = nf90_inquire_dimension(nc1,dimids(2), len=srcdims(2))
  call CheckNCError(status, 'lons 2nd dimension')
  status = nf90_inquire_dimension(nc1,dimids(3), len=srcdims(3))
  call CheckNCError(status, 'lons 3rd dimension')

  print *, 'srcfile dimension ', srcdims

  ! IPE dimension order:  nlevels, lats and lons (1115, 170, 80)
  allocate(srclon(srcdims(1), srcdims(2), srcdims(3)), &
  	   srclat(srcdims(1), srcdims(2), srcdims(3)), &
	   srchgt(srcdims(1), srcdims(2)))

  allocate(maxlevs(srcdims(2)))
  status = nf90_get_var(nc1, varid, srclon)
  call CheckNCError(status, 'lons')
  status = nf90_inq_varid(nc1,'lats', varid)
  call CheckNCError(status, 'lats')
  status = nf90_get_var(nc1, varid, srclat)
  call CheckNCError(status, 'lats')
  status = nf90_inq_varid(nc1,'height', varid)
  call CheckNCError(status, 'height')
  status = nf90_get_var(nc1, varid, srchgt)
  call CheckNCError(status, 'height')
  status = nf90_inq_varid(nc1,'maxLevels', varid)
  call CheckNCError(status, 'maxLevels')
  status = nf90_get_var(nc1, varid, maxlevs)
  call CheckNCError(status, 'maxLevels')
  status = nf90_close(nc1)
  call CheckNCError(status, srcFileName)

  ! Construct srcMesh based on block decomposition
  ! The decomposition is stored in a namelist called SMSnamelist, 
  ! the keyword is process_layout and it is a 2D decomposition of n,m
  ! where nxm has to be equal to the total number of processors and the
  ! latitude lines (170) will be decomposed to n blocks and longitude lines (80)
  ! to m blocks.  Usually we keep n=1 if total number of processors are < 80
  
  ! Let's hardcode the docomposition to 1xPetCnt if PetCnt < 80 for now
  ! 
  ! Also we only build the mesh with a subset of the height levels that
  ! intersect with the WAM grid.  Currently, we set it to 90KM to 600KM
  !
  ! Step 1: calculate number of longitude lines in the local processor
  if (srcdims(3) > PetCnt) then
    count = srcdims(3)/PetCnt
    remind = srcdims(3) - PetCnt*count
    if (PetNo < remind) then
       count = count+1
       start = count*PetNo+1
    else
       start = count*PetNo+remind+1
    endif
  else 
    print *, 'Total number of processor greater than ', srcdims(3)
    call ErrorMsgAndAbort(-1)
  endif
  print *, PetNo, ' start from ', start, ' with ', count, ' columns'


  !Find the total node count and cell count in the local PE
  ! hardcode the maxheight to 600km, find how many height levels are within the range
  ! for each latitute tube
  ! Need to separate it into south hemisphere and north hemisphere 
  allocate(totalheight(srcdims(2)),northind(srcdims(2)), southind(srcdims(2)))
  allocate(gap(srcdims(2)))
  do j=1,srcdims(2)
    do i=1,(maxlevs(j)-1)/2
      if (srchgt(i,j)> maxheight) then
          northind(j)=i-1
	  exit
      endif
    enddo
    if (i==(maxlevs(j)/2+1)) northind(j)=i-1 
    do i=maxlevs(j),(maxlevs(j)+1)/2+1,-1
      if (srchgt(i,j)> maxheight) then
          southind(j)=i+1
	  exit
      endif
    enddo
    if (i==(maxlevs(j)+1)/2) southind(j)=i+1 
    if (southind(j)==northind(j)) southind(j)=southind(j)+1
    totalheight(j) = northind(j)+maxlevs(j)-southind(j)+1
    gap(j) = southind(j)-northind(j)+1   
    !if (PetNo == 0) then
    !  print *, 'min/max levels for ', j, 'th field line are', northind(j), southind(j)
    !endif
  enddo

  ! The layers in between northind(1) and and southind(1) will not be included in 
  ! the mesh, will adjust the node index by taking out the gap layers to reduce the number
  ! of missing indices.
  maxheight=totalheight(1)
  ! Now construct the node arrays for Mesh creation
  ! First count how many nodes used in the local PE, each longitude has the same number of
  ! nodes, need to add one halo column
  totalnodes = 0
  localnodes = 0
  if (PetCnt > 1) then
    do j=1,srcdims(2)
      totalnodes = totalheight(j)*(count+1)+totalnodes  
      localnodes = totalheight(j)*count+localnodes
    enddo
  else 
    do j=1,srcdims(2)
      totalnodes = totalheight(j)*count+totalnodes  
    enddo
    localnodes=totalnodes
  endif
  if (PetNo == 0) then
     print *, ' Total nodes:', localnodes, totalnodes
  endif
  allocate(nodeIds(totalnodes), nodeCoords(totalnodes*3),nodeOwners(totalnodes))

  ! find out the starting global id of the local node
  allocate(nodeCnt(PetCnt*2),sendbuf(2))
  sendbuf(1)=localnodes
  sendbuf(2)=count
  call ESMF_VMAllGather(vm, sendbuf, nodeCnt, 2, rc=rc)
  ! find the starting globalNodeID
  ! and global total nodes
  globalTotal = 0
  startid=0
  do i=1,PetNo
    startid=startid+nodeCnt(i*2-1)
  enddo
  do i=1,PetCnt
    globalTotal=globalTotal+nodeCnt(i*2-1)
  enddo
  deallocate(sendbuf)
  srctotalnodes = globalTotal

  ! count number of latitudes and local elements at each height 
  totalelements=0
  allocate(totallats(northind(1)))
  do j=1,northind(1)
    do i=1,srcdims(2)
      if (northind(i)<j) exit
    enddo
    totallats(j)=i-1
    ! do not count the toppest level
    if (j<northind(1)) then
       totalelements=totalelements + count*(totallats(j)-1)
    endif
  enddo
  totalelements = totalelements*2

  ! Do the hallo longitude
  if (PetNo == PetCnt-1) then
      halo=1
      neighbor=0
      base=1
      increment=nodeCnt(2)
  else
      halo=start+count
      neighbor=PetNo+1
      base=startid+localnodes+1
      increment=nodeCnt(PetNo*2+2)
  endif
  deallocate(nodeCnt)
  print *, PetNo, ' halo, neighbor, base, increment ', halo, neighbor, base, increment  
  count1 = 1
  localcount = 1
  count3 = 1

  !----------------------------
  ! The local node will be arranged by longitude first, followed by latitude, height and hemisphere.  
  ! There will be fewer latitude columns at higher height. 
  ! North hemiphere first, then South hemiphere.  Two hemisphere should have the same number of nodes.
  ! The global ID is the sequential index of the 3D array coord(height,lat, lon).  But, the nodes at the
  ! South Hemisphere has adjusted heights by deducting the number of layers of the gap between southind(:)
  ! and northind(:)
  !-----------------------------
  ! North Hemisphere

  do i=1,northind(1)
    do j=1,totallats(i)
      do k=start,count+start-1
         !nodeIds(count1)=i+(j-1)*maxheight+(k-1)*j*maxheight
         nodeIds(count1)=localcount+startid
	 lon=srclon(i,j,k)
         lat=srclat(i,j,k)
         hgt=srchgt(i,j)
         call convert2Cart(lon,lat,hgt,nodeCoords(count3:count3+2))
         nodeOwners(count1)=PetNo
         count1=count1+1
         localcount=localcount+1
         count3=count3+3
       enddo
       if (PetCnt > 1) then
       ! Add hallo
       !nodeIds(count1)=i+(j-1)*maxheight+(halo-1)*j*maxheight
       ! Need to find out the globalID of the neightboring node
       nodeIds(count1)=base
       base=base+increment
       lon=srclon(i,j,halo)
       lat=srclat(i,j,halo)
       hgt=srchgt(i,j)
       call convert2Cart(lon,lat,hgt,nodeCoords(count3:count3+2))
       nodeOwners(count1)=neighbor
       count1=count1+1
       count3=count3+3
       endif
    enddo
  enddo
  ! South Hemisphere
  do ii=1,northind(1)
    do j=1,totallats(ii)
      !adjust the height index based on the maxlevel of the latitude
      i=maxlevs(j)-ii+1
      do k=start,count+start-1
         !nodeIds(count1)=(i-gap(j))+(j-1)*maxheight+(k-1)*j*maxheight
         nodeIds(count1)=localcount+startid
	 lon=srclon(i,j,k)
         lat=srclat(i,j,k)
         hgt=srchgt(i,j)
         call convert2Cart(lon,lat,hgt,nodeCoords(count3:count3+2))
         nodeOwners(count1)=PetNo
         count1=count1+1
         localcount=localcount+1
         count3=count3+3
       enddo
       ! Add hallo
       if (PetCnt > 1) then
       !nodeIds(count1)=(i-gap(j))+(j-1)*maxheight+(halo-1)*j*maxheight
       nodeIds(count1)=base
       base=base+increment
       lon=srclon(i,j,halo)
       lat=srclat(i,j,halo)
       hgt=srchgt(i,j)
       call convert2Cart(lon,lat,hgt,nodeCoords(count3:count3+2))
       nodeOwners(count1)=neighbor
       count1=count1+1
       count3=count3+3
       endif 
     enddo
  enddo
  nodestartid = startid+1
  print *, PetNo, 'totalnodes, localnodes ', count1-1, localcount-1
  if (count1-1 /= totalnodes) then
     print *, 'count mismatch:', count1-1, totalnodes
  endif


  ! Create mesh and add node
  srcMesh=ESMF_MeshCreate(3,3,coordSys=ESMF_COORDSYS_CART, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize()
  call ESMF_MeshAddNodes(srcMesh, nodeIds, nodeCoords, nodeOwners, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize()

  ! an Array with analytical data values
  !allocate(srcdata(count, srcdims(2), srcdims(3)))
  allocate(srcdata(srcdims(1),srcdims(2),count))
  do i=1,srcdims(1)
    do j=1,srcdims(2)
     do k=1,count
       kk=start+k-1
       srcdata(i,j,k)=1.0+srchgt(i,j)*0.01+(cos(srclat(i,j,kk)*deg2rad) &
       		 *cos(srclat(i,j,kk)*deg2rad)) &
		      *cos(2*srclon(i,j,kk)*deg2rad)   
     enddo
    enddo
  enddo
  ! Now construct Mesh elements
  ! Mesh are separated at the megnetic equator, so the mesh elements are two less than the 
  ! total latitude points at each layer
  ! First find out how many elements there are per processor
  ! Start from the bottom, calculate the elements in the north hemisphere, then double it
  ! The number of cells at each height is determined by how many grid points at that layer

  allocate(elementIds(totalelements), elementTypes(totalelements), &
  	   elementConn(totalelements*8))

  elementTypes(:)=ESMF_MESHELEMTYPE_HEX
  
  ! find out the starting global id of the local element
  allocate(elementCnt(PetCnt),sendbuf(1))
  sendbuf(1)=totalelements
  call ESMF_VMAllGather(vm, sendbuf, elementCnt, 1, rc=rc)
  ! find the starting elementID
  startid=0
  do i=1,PetNo
    startid=startid+elementCnt(i)
  enddo
  globalTotalelmt=0
  do i=1,PetCnt
    globalTotalelmt=globalTotalelmt+elementCnt(i)
  enddo
  deallocate(elementCnt, sendbuf)

  print *, PetNo, 'Total Elements: ', totalelements, 'starting at', startid

  ! Build the local elementConn table using local node indices
  count1=1
  count8=1
  ! base is the starting local index of the node at level i
  base=0
  ! Do the bottom rectangle of each element first
  ! North hemisphere first

  !!!!!!!!!!!!!!!!!!!!!!
  ! For sequential case, make count one less to make the code change less
  if (PetCnt == 1) count=count-1
  !!!!!!!!!!!!!!!!!!!!!!

  do i=1,northind(1)-1
    do j=1,totallats(i)-1
      do k=1,count
         elementIds(count1)=count1+startid
         elementConn(count8)=base+(count+1)*j+k
         elementConn(count8+1)=base+(count+1)*j+k+1
         elementConn(count8+2)=base+(count+1)*(j-1)+k+1
         elementConn(count8+3)=base+(count+1)*(j-1)+k
	 count1=count1+1
	 count8=count8+8
      enddo      
      if (PetCnt ==1) then
         elementIds(count1)=count1+startid
         elementConn(count8)=base+(count+1)*j+k
         elementConn(count8+1)=base+(count+1)*j+1
         elementConn(count8+2)=base+(count+1)*(j-1)+1
         elementConn(count8+3)=base+(count+1)*(j-1)+k
	 count1=count1+1
	 count8=count8+8
      endif
    enddo
    base=base+totallats(i)*(count+1)
  enddo
  ! add the top level to base
  base = base+totallats(i)*(count+1)
  ! South Hemisphere
  do i=1,northind(1)-1
    do j=1,totallats(i)-1
      do k=1,count
         elementIds(count1)=count1+startid
         elementConn(count8)=base+(count+1)*(j-1)+k
         elementConn(count8+1)=base+(count+1)*(j-1)+k+1
         elementConn(count8+2)=base+(count+1)*j+k+1
         elementConn(count8+3)=base+(count+1)*j+k
	 count1=count1+1
	 count8=count8+8
      enddo      
      if (PetCnt == 1) then
         elementIds(count1)=count1+startid
         elementConn(count8)=base+(count+1)*(j-1)+k
         elementConn(count8+1)=base+(count+1)*(j-1)+1
         elementConn(count8+2)=base+(count+1)*j+1
         elementConn(count8+3)=base+(count+1)*j+k
	 count1=count1+1
	 count8=count8+8
      endif
    enddo
    base=base+totallats(i)*(count+1)
  enddo

  ! Check the total number of elements match with totalelements
  if (count1-1 /= totalelements) then
     print *, PetNo, ' total elements mismatch ', count1, totalelements
  endif
  ! Now building the top face (quad) of the cube, note if the top level has fewer latitudes, we need 
  ! to collape some the the top into on single line.
  count8=1
  base = 0
  ! first time for North hemisphere, second time for South hemisphere, should be identical
  if (PetCnt==1) then 
     elmtcount=count+1
  else
     elmtcount=count
  endif
  do ii=1,2  
    do i=1, northind(1)-2
      ! base is the starting local index to the elementConn table for the level i+1
      base = base+count*(totallats(i)-1)*8
      ! countup is the sequential index to elementConn at level i+1 (starting at 1)
      countup=1
      if (totallats(i)==totallats(i+1)) then
        do j=1,totallats(i)-1
          do k=1,elmtcount
            elementConn(count8+4)=elementConn(base+countup)
            elementConn(count8+5)=elementConn(base+countup+1)
            elementConn(count8+6)=elementConn(base+countup+2)
            elementConn(count8+7)=elementConn(base+countup+3)
            count8=count8+8
  	    countup=countup+8
          enddo
        enddo
      else ! the top level has fewer rows than the bottom layer
        diff = totallats(i)-totallats(i+1)
        ! put the prism cells close the the last few latitudes (close to the equator)
        lastlat=totallats(i)-diff*2
        do j=1,lastlat-1
          do k=1,elmtcount
            elementConn(count8+4)=elementConn(base+countup)
            elementConn(count8+5)=elementConn(base+countup+1)
            elementConn(count8+6)=elementConn(base+countup+2)
            elementConn(count8+7)=elementConn(base+countup+3)
            count8=count8+8
	    countup=countup+8
          enddo
        enddo
        even=.TRUE.
        jj=lastlat
        do j=lastlat,totallats(i)-1
  	  if (even) then
            save = countup
            do k=1,elmtcount
              elementConn(count8+4)=elementConn(base+countup)
              elementConn(count8+5)=elementConn(base+countup+1)
              elementConn(count8+6)=elementConn(base+countup+1)
              elementConn(count8+7)=elementConn(base+countup)
              count8=count8+8
	      countup=countup+8
	    enddo
	    even = .FALSE.
	    countup = save            
          else
            do k=1,elmtcount
              elementConn(count8+4)=elementConn(base+countup)
              elementConn(count8+5)=elementConn(base+countup+1)
              elementConn(count8+6)=elementConn(base+countup+2)
              elementConn(count8+7)=elementConn(base+countup+3)
              count8=count8+8
 	      countup=countup+8
            enddo
            jj=jj+1
	    even = .TRUE.
	  endif
        enddo
        !print *, PetNo, 'Level ', i, 'Top level elements ', countup/8
      endif 	    
   enddo

  ! Now the top row of the cube i=northind(1)-1
  ! First find out the startind nodeind of the top level 
  ! base1 is the starting index of the local node id at the top level 
  if (ii==1) base1=0
  do j=1,northind(1)-1
     base1=base1+totallats(j)*(count+1)
  enddo
  print *, PetNo, 'base1 for node:', base1  
  ! If the top two levels have the same number of latitude points, i=northind(1)-1
  if (totallats(i)==totallats(i+1)) then
      if (ii==1) then 
        do j=1,totallats(i)-1
          do k=1,count
             elementConn(count8+4)=base1+(count+1)*j+k
             elementConn(count8+5)=base1+(count+1)*j+k+1
             elementConn(count8+6)=base1+(count+1)*(j-1)+k+1
             elementConn(count8+7)=base1+(count+1)*(j-1)+k
             count8=count8+8
           enddo
	   if (PetCnt==1) then
             elementConn(count8+4)=base1+(count+1)*j+k
             elementConn(count8+5)=base1+(count+1)*j+1
             elementConn(count8+6)=base1+(count+1)*(j-1)+1
             elementConn(count8+7)=base1+(count+1)*(j-1)+k
             count8=count8+8
           endif
        enddo
	base1 = base1+totallats(i+1)*(count+1)
     else ! (ii==2)
        do j=1,totallats(i)-1
          do k=1,count
             elementConn(count8+4)=base1+(count+1)*(j-1)+k
             elementConn(count8+5)=base1+(count+1)*(j-1)+k+1
             elementConn(count8+6)=base1+(count+1)*j+k+1
             elementConn(count8+7)=base1+(count+1)*j+k
             count8=count8+8
           enddo
           if (PetCnt==1) then
             elementConn(count8+4)=base1+(count+1)*(j-1)+k
             elementConn(count8+5)=base1+(count+1)*(j-1)+1
             elementConn(count8+6)=base1+(count+1)*j+1
             elementConn(count8+7)=base1+(count+1)*j+k
             count8=count8+8
           endif
        enddo
      endif ! ii
    else ! totallats(i) /= totallats(i+1)
      diff = totallats(i)-totallats(i+1)
      ! put the prism cells close the the last few latitudes (close to the equator)
      lastlat=totallats(i)-diff*2
      ! the first 1 to lastlat rows will be cubes
      if (ii==1) then 
        do j=1,lastlat-1
          do k=1,count
             elementConn(count8+4)=base1+(count+1)*j+k
             elementConn(count8+5)=base1+(count+1)*j+k+1
             elementConn(count8+6)=base1+(count+1)*(j-1)+k+1
             elementConn(count8+7)=base1+(count+1)*(j-1)+k
             count8=count8+8
           enddo
	   if (PetCnt==1) then
             elementConn(count8+4)=base1+(count+1)*j+k
             elementConn(count8+5)=base1+(count+1)*j+1
             elementConn(count8+6)=base1+(count+1)*(j-1)+1
             elementConn(count8+7)=base1+(count+1)*(j-1)+k
             count8=count8+8
	   endif
        enddo
      else ! (ii==2)
        do j=1,lastlat-1
          do k=1,count
             elementConn(count8+4)=base1+(count+1)*(j-1)+k
             elementConn(count8+5)=base1+(count+1)*(j-1)+k+1
             elementConn(count8+6)=base1+(count+1)*j+k+1
             elementConn(count8+7)=base1+(count+1)*j+k
             count8=count8+8
           enddo
	   if (PetCnt==1) then
             elementConn(count8+4)=base1+(count+1)*(j-1)+k
             elementConn(count8+5)=base1+(count+1)*(j-1)+1
             elementConn(count8+6)=base1+(count+1)*j+1
             elementConn(count8+7)=base1+(count+1)*j+k
             count8=count8+8
	   endif
        enddo
      endif ! ii
      even=.TRUE.
      jj=lastlat
      do j=lastlat,totallats(i)-1
	if (even) then ! collapse the top into a line
          do k=1,count
            elementConn(count8+4)=base1+(count+1)*(jj-1)+k
            elementConn(count8+5)=base1+(count+1)*(jj-1)+k+1
            elementConn(count8+6)=base1+(count+1)*(jj-1)+k+1
            elementConn(count8+7)=base1+(count+1)*(jj-1)+k
            count8=count8+8
	  enddo
          if (PetCnt==1) then
            elementConn(count8+4)=base1+(count+1)*(jj-1)+k
            elementConn(count8+5)=base1+(count+1)*(jj-1)+1
            elementConn(count8+6)=base1+(count+1)*(jj-1)+1
            elementConn(count8+7)=base1+(count+1)*(jj-1)+k
            count8=count8+8
	  endif
	  even = .FALSE.            
        else ! odd, the top is a quad
	   if (ii==1) then
             do k=1,count
               elementConn(count8+4)=base1+(count+1)*jj+k
               elementConn(count8+5)=base1+(count+1)*jj+k+1
               elementConn(count8+6)=base1+(count+1)*(jj-1)+k+1
               elementConn(count8+7)=base1+(count+1)*(jj-1)+k
               count8=count8+8
             enddo
	     if (PetCnt==1) then
               elementConn(count8+4)=base1+(count+1)*jj+k
               elementConn(count8+5)=base1+(count+1)*jj+1
               elementConn(count8+6)=base1+(count+1)*(jj-1)+1
               elementConn(count8+7)=base1+(count+1)*(jj-1)+k
               count8=count8+8
             endif	       
	     jj=jj+1
           else ! (ii==2)
             do k=1,count
               elementConn(count8+4)=base1+(count+1)*(jj-1)+k
               elementConn(count8+5)=base1+(count+1)*(jj-1)+k+1
               elementConn(count8+6)=base1+(count+1)*jj+k+1
               elementConn(count8+7)=base1+(count+1)*jj+k
               count8=count8+8
             enddo
             if (PetCnt==1) then
               elementConn(count8+4)=base1+(count+1)*(jj-1)+k
               elementConn(count8+5)=base1+(count+1)*(jj-1)+1
               elementConn(count8+6)=base1+(count+1)*jj+1
               elementConn(count8+7)=base1+(count+1)*jj+k
               count8=count8+8
	     endif
             jj=jj+1
	   endif ! ii
	   even = .TRUE.
         endif ! even
      enddo ! j=lastlat,totallats(i)-1
      base1 = base1+totallats(i+1)*(count+1)
    endif ! (totallats(i)==totallats(i+1)) 

    ! Need to add the top level to base
    base = base + (totallats(northind(1)-1)-1)*count*8
  enddo ! ii=1,2
  
  do i=1,totalelements*8
    if (elementConn(i) > totalnodes .OR. elementConn(i)==0) then
       print *, PetNo, ' node index out of range', i/8, i, elementConn(i)
    endif
  enddo
  call ESMF_VMBarrier(vm) 

  call ESMF_MeshAddElements(srcMesh, elementIds, elementTypes, elementConn,rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

   !! Write out the mesh information to a UGRID file.....
  deallocate(nodeCoords)
  allocate(nodeCoords(localnodes*3))
  call ESMF_MeshGet(srcMesh, nodalDistgrid=nodalDistgrid, & 
    ownedNodeCoords=nodeCoords, numOwnedNodes=totalnodes)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  if (totalnodes /= localnodes) then
     print *, PetNo, 'totalnode mismatch', totalnodes, localnodes
  endif

  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_NODE,rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  ! do the Regrid with some analytical data
  call ESMF_FieldGet(srcField, farrayPtr=fptr1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  ! set the field array values from srcdata using the local order
  ! North hemisphere first
!  write(filename, "(A8,I1)") 'srcdata.', PetNo
!  open(100, file=filename)
  count1=1
  do i=1,northind(1)
    do j=1,totallats(i)
      do k=1,count
        fptr1d(count1)=srcdata(i,j,k)
!        write(100, "(I8, 3I5, 3F7.2, F7.4)")  count1, i, j, k+start-1,srclon(i,j,k+start-1), srclat(i,j,k+start-1),srchgt(i,j), srcdata(i,j,k)
        count1=count1+1
      enddo
    enddo
  enddo
  ! South hemiphere next
  do ii=1,northind(1)
    do j=1,totallats(ii)
      i=maxlevs(j)-ii+1
      do k=1,count
        fptr1d(count1)=srcdata(i,j,k)
!        write(100, "(I8, 3I5, 3F7.2, F7.4)")  count1, i, j, k+start-1,srclon(i,j,k+start-1), srclat(i,j,k+start-1),srchgt(i,j), srcdata(i,j,k)
        count1=count1+1
      enddo
    enddo
  enddo
!  close(100)


  print *, PetNo, 'srcdata min/max', minval(fptr1d), maxval(fptr1d)

  ! convert it back to spherical degree coordinates and separate them into
  ! three arrays
  allocate(lontbl(totalnodes), lattbl(totalnodes), hgttbl(totalnodes))
  count3 = 1
  do i = 1, totalnodes
   call Convert2Sphdeg(nodeCoords(count3), nodeCoords(count3+1), nodeCoords(count3+2), &
       lontbl(i), lattbl(i), hgttbl(i))
   count3=count3+3
  enddo

#if 0
  ! Set the local data using the Owned coordinates
  do i=1,totalnodes
     fptr1d(i)=1.0+hgttbl(i)*0.01+(cos(lattbl(i)*deg2rad)*cos(lattbl(i)*deg2rad)) &
              * cos(2*lontbl(i)*deg2rad)   
  enddo
#endif
  ! Convert the localId to globalID in elementConn then write it out
  allocate(conntbl(8*totalelements))
  do i=1, totalelements*8
       conntbl(i)=nodeIds(elementConn(i))
  enddo
  
  ! Output IPE mesh to a file
#if 1  
  if (PetNo==0) then
    status = nf90_create(srcoutfile, nf90_clobber, nc1)
    call CheckNCError(status, srcoutfile)
    status = nf90_def_dim(nc1, 'numnodes', globaltotal, nodeid)
    status = nf90_def_dim(nc1, 'numcells', globaltotalelmt*8, elmtid)

    status = nf90_def_var(nc1, 'nodelon', NF90_DOUBLE, (/nodeid/), lonid)
    status = nf90_def_var(nc1, 'nodelat', NF90_DOUBLE, (/nodeid/), latid)
    status = nf90_def_var(nc1, 'heights', NF90_DOUBLE, (/nodeid/), hgtid)
    status = nf90_def_var(nc1, 'vertids', NF90_DOUBLE, (/elmtid/),  connid)
    status = nf90_def_var(nc1, 'testdata1', NF90_DOUBLE, (/nodeid/),  connid)
    status = nf90_def_var(nc1, 'testdata2', NF90_DOUBLE, (/nodeid/),  connid)
    status = nf90_enddef(nc1)
    status = nf90_close(nc1)
  endif
  call ESMF_VMBarrier(vm)
 
  ! Write the node coordinates to the file in the order of the PetNo
  do i=0,PetCnt-1
    if (PetNo == i) then
       print *, PetNo, 'write out data starting ', nodestartid
       status = nf90_open(srcoutfile, NF90_WRITE, nc1)
       call CheckNCError(status, srcoutfile)
       status = nf90_inq_varId(nc1, 'nodelon', lonid)
       call CheckNCError(status, 'nodelon')
       print *, PetNo, 'write nodes', nodestartid, totalnodes
       status = nf90_put_var(nc1, lonid, lontbl, start=(/nodestartid/), &
       	      	count=(/totalnodes/))
       call CheckNCError(status, 'nodelon')
       status = nf90_inq_varId(nc1, 'nodelat', latid)
       call CheckNCError(status, 'nodelat')
       status = nf90_put_var(nc1, latid, lattbl, start=(/nodestartid/), &
       	      	count=(/totalnodes/))
       call CheckNCError(status, 'nodelat')
       status = nf90_inq_varId(nc1, 'heights', hgtid)
       call CheckNCError(status, 'heights')
       status = nf90_put_var(nc1, hgtid, hgttbl, start=(/nodestartid/), &
       	      	count=(/totalnodes/))
       call CheckNCError(status, 'heights')
       status = nf90_inq_varId(nc1, 'testdata1', hgtid)
       call CheckNCError(status, 'testdata1')
       status = nf90_put_var(nc1, hgtid, fptr1d, start=(/nodestartid/), &
       	      	count=(/totalnodes/))
       call CheckNCError(status, 'testdata')
       deallocate(lontbl, lattbl, hgttbl)
       print *, PetNo, 'write element conn', startid+1, totalelements
       status = nf90_inq_varId(nc1, 'vertids', vertid)
       call CheckNCError(status, 'vertids')
       status = nf90_put_var(nc1, vertid, conntbl, &
       	      	start=(/startid*8+1/), count=(/8*totalelements/))
       call CheckNCError(status, 'vertids')
       status = nf90_close(nc1)
    endif
    call ESMF_VMBarrier(vm)
  enddo 
  
  ! Reshape the elementConn for output    
  deallocate(conntbl)
#endif

  deallocate(nodeIds, nodeOwners, nodeCoords)
  deallocate(elementIds, elementTypes, elementConn)
  deallocate(srclon, srclat, srchgt)

  !!-------------------------------------
  !! Create WAM mesh
  !!-------------------------------------
  ! Read in the destination grid (WAM)
  !!
  status = nf90_open(path=dstfilename, mode=nf90_nowrite, ncid=nc1)
  call CheckNCError(status, dstfilename)
  status = nf90_inq_varid(nc1,'lons', varid)
  call CheckNCError(status, 'lons')
  status = nf90_inquire_variable(nc1, varid, ndims=ndims, dimids = dimids)
  call CheckNCError(status, 'lons')
  status = nf90_inquire_dimension(nc1,dimids(1), len=dstdims(1))
  call CheckNCError(status, 'lons 1st dimension')
  status = nf90_inquire_dimension(nc1,dimids(2), len=dstdims(2))
  call CheckNCError(status, 'lons 2nd dimension')

  print *, PetNo, ' dstfile dimension ', dstdims

  ! WAM dimension order:  lons, lats (192, 94)
  allocate(dstlon(dstdims(1), dstdims(2)), &
  	   dstlat(dstdims(1), dstdims(2)))
  status = nf90_get_var(nc1, varid, dstlon)
  call CheckNCError(status, 'lons')
  status = nf90_inq_varid(nc1,'lats', varid)
  call CheckNCError(status, 'lats')
  status = nf90_get_var(nc1, varid, dstlat)
  call CheckNCError(status, 'lats')

  status = nf90_inq_varid(nc1,'height', varid)
  call CheckNCError(status, 'height')
  status = nf90_inquire_variable(nc1, varid, ndims=ndims, dimids = dimids)
  call CheckNCError(status, 'height')
  status = nf90_inquire_dimension(nc1,dimids(1), len=dstdims(3))
  call CheckNCError(status, 'height 1st dimension')
  allocate(dsthgt(dstdims(3)))
  status = nf90_get_var(nc1, varid, dsthgt)
  call CheckNCError(status, 'height')
  allocate(NumPerRow(dstdims(2)), ShuffleOrder(dstdims(2)))
  status = nf90_inq_varid(nc1,'NumPerRow', varid)
  call CheckNCError(status, 'NumPerRow')
  status = nf90_get_var(nc1, varid, NumPerRow)
  call CheckNCError(status, 'NumPerRow')
  status = nf90_inq_varid(nc1,'ShuffleOrder', varid)
  call CheckNCError(status, 'ShuffleOrder')
  status = nf90_get_var(nc1, varid, ShuffleOrder)
  call CheckNCError(status, 'ShuffleOrder')
  status= nf90_close(nc1)
  call CheckNCError(status, dstFileName)

  ! find the lower height level where height > minheight
  do i=1,dstdims(3)
     if (dsthgt(i) > minheight) then
    	startlevel = i-1
	exit
     endif
  enddo
  totallevels = dstdims(3)-startlevel+1

  ! create the node table, find the total number of nodes in each processor, including the not-owned node
  localnodes=0
  totalnodes=0
  totalelements=0 
  myrows = dstdims(2)/PetCnt
  if ((dstdims(2)-myrows*PetCnt) > PetNo) myrows = myrows+1
  allocate(rowinds(myrows), baseind(myrows))
  allocate(petTable(dstdims(2)))
  next = 0
  ind1 = 0
  do i=1,dstdims(2)
     ind=ShuffleOrder(i)
     petTable(ind)=next
     if (next == PetNo) then
       ind1=ind1+1
       rowinds(ind1)=ind
       localnodes=localnodes + numPerRow(ind)
     endif
     next=next+1
     if (next == PetCnt) next=0
  enddo
  ! sort rowinds
  call ESMF_UtilSort(rowinds, ESMF_SORTFLAG_ASCENDING, rc)
  do i=1,myrows
     ind=rowinds(i)
     baseind(i)=totalnodes
     ! Add the neighbor nodes
     ! If PetCnt==1, no need to add neighbor node
     ! If last row, no need to add neighbors
     ! If the neighbor is local, no need to add
     if (ind < dstdims(2)) then
       if (PetCnt>1) then
        if ((i < myrows .and. rowinds(i+1) /= ind+1) .or. i==myrows) then
          totalnodes=totalnodes+numPerRow(ind)+numPerRow(ind+1)
        else
          totalnodes=totalnodes+numPerRow(ind)
        endif
       endif
       if (numPerRow(ind) >= numPerRow(ind+1)) then
         totalelements = totalelements + numPerRow(ind)
       else
         totalelements = totalelements + numPerRow(ind+1)
       endif
     else
       totalnodes=totalnodes+numPerRow(ind)
       !Add extra elements at the top
       totalelements = totalelements+numPerRow(ind)/2
     endif
  enddo
  if (PetCnt == 1) then
     baseind(1)=0
     do i=2,dstdims(2)
       baseind(i)=baseind(i-1)+numPerRow(i-1)
     enddo
  endif

  totalnodes2d=totalnodes
  totalnodes = totalnodes * totallevels 
  localnodes = localnodes * totallevels
  totalelements = totalelements * (totallevels-1)
  allocate(nodeIds(totalnodes), nodeOwners(totalnodes), nodeCoords(totalnodes*3))

  print *, PetNo, 'totalelmts, totalnodes, localnodes', totalelements, totalnodes, localnodes
  print *, PetNo, 'myrows, totallevels', myrows, totallevels
  
  ! Fill nodeIds, nodeOwners, and nodeCoords arrays, longitude first, latitude, then height
  count1=1
  localcount=1
  count3=1
  if (PetCnt > 1) then
  do k=1, totallevels
    do i=1,myrows
       ind=rowinds(i)
       do j=1,numPerRow(ind)
          ! Global id based on the 3D indices
       	  nodeIds(count1)= j+dstdims(1)*(ind-1)+dstdims(1)*dstdims(2)*(k-1)
          nodeOwners(count1)=PetNo
	  lon = dstlon(j,ind)
          lat  = dstlat(j,ind)
          hgt = dsthgt(startlevel+k-1)
          call convert2Cart(lon, lat, hgt, nodeCoords(count3:count3+2))
          count1=count1+1
          localcount=localcount+1
          count3=count3+3
       enddo
       ! if not the last row, add the neighbor row's nodes and the neighbor
       ! is not local
       if (ind < dstdims(2)) then
         if (i==myrows .or. (i < myrows .and. rowinds(i+1)/= ind+1)) then
       	  do j=1, numPerRow(ind+1)
            ! Global id based on the 3D indices
            nodeIds(count1)= j+dstdims(1)*ind+dstdims(1)*dstdims(2)*(k-1)
	    if (PetTable(ind+1) == PetNo) then
	       print *, PetNo, 'wrong neighbor ', count1, PetTable(ind+1)
            endif
            nodeOwners(count1)=PetTable(ind+1)
	    lon = dstlon(j,ind+1)
            lat  = dstlat(j,ind+1)
            hgt = dsthgt(k+startlevel-1)
            call convert2Cart(lon, lat, hgt, nodeCoords(count3:count3+2))
            count1=count1+1
            count3=count3+3
          enddo
         endif
	endif
     enddo
  enddo
  else ! PetCnt==1
  ! For sequential case, store the rows in its order, do not shuffle
  do k=1, totallevels
    do ind=1,myrows
       do j=1,numPerRow(ind)
          ! Global id based on the 3D indices
       	  nodeIds(count1)= j+dstdims(1)*(ind-1)+dstdims(1)*dstdims(2)*(k-1)
          nodeOwners(count1)=PetNo
	  lon = dstlon(j,ind)
          lat  = dstlat(j,ind)
          hgt = dsthgt(k+startlevel-1)
          call convert2Cart(lon, lat, hgt, nodeCoords(count3:count3+2))
          count1=count1+1
          localcount=localcount+1
          count3=count3+3
       enddo
     enddo
  enddo
  endif ! PetCnt > 1

  if (count1-1 /= totalnodes .or. localcount-1 /= localnodes) then
     print *, 'totalcount mismatch ', count1-1, totalnodes, localcount-1, localnodes
  endif

  !create analytical destination data
  allocate(dstdata(localnodes))
  count1=1
  do k=startlevel, startlevel+totallevels-1
    do j=1, myrows
      ind=rowinds(j)
      do i= 1, numPerRow(ind)
        dstdata(count1)=1.0+dsthgt(k)*0.01+cos(dstlat(i,ind)*deg2rad)* &
			cos(dstlat(i,ind)*deg2rad)*cos(2*dstlon(i,ind)*deg2rad)		
        count1=count1+1
      enddo
    enddo
  enddo						
  if (count1-1 /= localnodes) then
     print *, 'localnodes mismatch in dstdata', count1-1, localnodes
  endif
  dstMesh = ESMF_MeshCreate(3,3,coordSys=ESMF_COORDSYS_CART, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize()
  call ESMF_MeshAddNodes(dstMesh, nodeIds, nodeCoords, nodeOwners, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize()
  
  !deallocate(dstlon, dstlat, dsthgt)
  !deallocate(nodeIds, nodeCoords, nodeOwners)

  allocate(elementIds(totalelements), elementTypes(totalelements), &
           elementConn(totalelements*8))

  elementTypes(:)=ESMF_MESHELEMTYPE_HEX

  ! find out the starting global id of the local element
  allocate(elementCnt(PetCnt),sendbuf(1))
  sendbuf(1)=totalelements
  call ESMF_VMAllGather(vm, sendbuf, elementCnt, 1, rc=rc)
  ! find the starting elementID
  startid=0
  do i=1,PetNo
    startid=startid+elementCnt(i)
  enddo
  globaltotalelmt = 0
  do i=1,PetCnt
    globaltotalelmt=globaltotalelmt+elementCnt(i)
  enddo
  deallocate(elementCnt, sendbuf)

  print *, PetNo, 'Total Elements: ', totalelements, 'starting at', startid

  ! Build the local elementConn table using local node indices
  count1=1
  count8=1
  do k=1, totallevels-1
    do i=1,myrows
       if (PetCnt > 1) then
           ind=rowinds(i)
       else
	   ind = i
       endif
       base = baseind(i)+totalnodes2d*(k-1)
       if (ind == dstdims(2)) then
         ! create dummy elements by connecting every other nodes to form a triangle
         do j=1, numPerRow(ind)-2, 2
       	   elementIds(count1)=startid+count1
	   elementConn(count8)= base+j
	   elementConn(count8+1)=base+j+1
	   elementConn(count8+2)=base+j+2
	   elementConn(count8+3)=base+j+2
	   elementConn(count8+4)=base+totalnodes2d+j
	   elementConn(count8+5)=base+totalnodes2d+j+1
	   elementConn(count8+6)=base+totalnodes2d+j+2
	   elementConn(count8+7)=base+totalnodes2d+j+2
	   count1=count1+1
	   count8=count8+8
         enddo
         ! Last one, connect it back to the first node 
     	 elementIds(count1)=startid+count1
	 elementConn(count8)= base+j
	 elementConn(count8+1)=base+j+1
	 elementConn(count8+2)=base+1
	 elementConn(count8+3)=base+1
	 elementConn(count8+4)=base+totalnodes2d+j
	 elementConn(count8+5)=base+totalnodes2d+j+1
	 elementConn(count8+6)=base+totalnodes2d+1
	 elementConn(count8+7)=base+totalnodes2d+1
	 count1=count1+1
	 count8=count8+8
         cycle       
       endif
       ! the two adjacent rows have the same number of points, elements are cubes
       if (numPerRow(ind+1) == numPerRow(ind)) then
         do j=1,numPerRow(ind)-1
       	   elementIds(count1)=startid+count1
	   elementConn(count8)= base+j
	   elementConn(count8+1)=base+j+1
	   elementConn(count8+2)=base+numPerRow(ind)+j+1
	   elementConn(count8+3)=base+numPerRow(ind)+j
	   elementConn(count8+4)=base+totalnodes2d+j
	   elementConn(count8+5)=base+totalnodes2d+j+1
	   elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+j+1
	   elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
	   count1=count1+1
	   count8=count8+8
         enddo
         ! last one in the row, wrap around
       	 elementIds(count1)=startid+count1
	 elementConn(count8)= base+j
	 elementConn(count8+1)=base+1
	 elementConn(count8+2)=base+numPerRow(ind)+1
	 elementConn(count8+3)=base+numPerRow(ind)+j
	 elementConn(count8+4)=base+totalnodes2d+j
	 elementConn(count8+5)=base+totalnodes2d+1
	 elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+1
	 elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
	 count1=count1+1
	 count8=count8+8
       else
         ! the number of nodes are different, make prism elements
	 diff=numPerRow(ind)-numPerRow(ind+1)
	 if (diff > 0) then 
          ! make triangles with base at lower row
          ! triangles will be evenly distributed
	  interval = real(numPerRow(ind))/(diff+1)
	  jj=1
          trigs=1
          do j=1,numPerRow(ind)-1
	     if (j > trigs*interval) then
!	     if (mod(j,increment)==0) then
               ! triangles - base at bottom
               trigs=trigs+1
               elementIds(count1)=startid+count1
	       elementConn(count8)= base+j
	       elementConn(count8+1)=base+j+1
	       elementConn(count8+2)=base+numPerRow(ind)+jj
	       elementConn(count8+3)=base+numPerRow(ind)+jj
	       elementConn(count8+4)=base+totalnodes2d+j
	       elementConn(count8+5)=base+totalnodes2d+j+1
	       elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+jj
	       elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+jj
             else
               elementIds(count1)=startid+count1
	       elementConn(count8)= base+j
	       elementConn(count8+1)=base+j+1
	       elementConn(count8+2)=base+numPerRow(ind)+jj+1
	       elementConn(count8+3)=base+numPerRow(ind)+jj
	       elementConn(count8+4)=base+totalnodes2d+j
	       elementConn(count8+5)=base+totalnodes2d+j+1
	       elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+jj+1
	       elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+jj
	       jj=jj+1
	     endif
	     count1=count1+1
	     count8=count8+8
           enddo
           ! last one in the row, wrap around
           elementIds(count1)=startid+count1
	   elementConn(count8)= base+j
	   elementConn(count8+1)=base+1
	   elementConn(count8+2)=base+numPerRow(ind)+1
	   elementConn(count8+3)=base+numPerRow(ind)+jj
	   elementConn(count8+4)=base+totalnodes2d+j
	   elementConn(count8+5)=base+totalnodes2d+1
	   elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+1
	   elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+jj
  	   count1=count1+1
	   count8=count8+8
	   if (k==1 .and. (jj /= numPerRow(ind+1))) then
	      print *, PetNo, 'Upper row index mismatch', ind, jj, numPerRow(ind+1)
           endif
        else  ! diff < 0 
          ! make triangles with base at upper row
          ! triangles will be evenly distributed
	  interval = real(numPerRow(ind+1))/(-1*diff+1)
	  jj=1
	  trigs=1
          do j=1,numPerRow(ind+1)-1
	     if (j > trigs*interval) then
	       trigs = trigs+1              
               ! triangles - base at bottom
               elementIds(count1)=startid+count1
	       elementConn(count8)= base+jj
	       elementConn(count8+1)=base+jj
	       elementConn(count8+2)=base+numPerRow(ind)+j+1
	       elementConn(count8+3)=base+numPerRow(ind)+j
	       elementConn(count8+4)=base+totalnodes2d+jj
	       elementConn(count8+5)=base+totalnodes2d+jj
	       elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+j+1
	       elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
             else
               elementIds(count1)=startid+count1
	       elementConn(count8)= base+jj
	       elementConn(count8+1)=base+jj+1
	       elementConn(count8+2)=base+numPerRow(ind)+j+1
	       elementConn(count8+3)=base+numPerRow(ind)+j
	       elementConn(count8+4)=base+totalnodes2d+jj
	       elementConn(count8+5)=base+totalnodes2d+jj+1
	       elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+j+1
	       elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
	       jj=jj+1
	     endif
	     count1=count1+1
	     count8=count8+8
           enddo
           ! last one in the row, wrap around
       	   elementIds(count1)=startid+count1
	   elementConn(count8)= base+jj
	   elementConn(count8+1)=base+1
	   elementConn(count8+2)=base+numPerRow(ind)+1
	   elementConn(count8+3)=base+numPerRow(ind)+j
	   elementConn(count8+4)=base+totalnodes2d+jj
	   elementConn(count8+5)=base+totalnodes2d+1
	   elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+1
	   elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
 	   count1=count1+1
	   count8=count8+8
	   if (k==1 .and. (jj /= numPerRow(ind))) then
	      print *, PetNo, 'Lower row index mismatch', ind, jj, numPerRow(ind)
           endif
        endif
       endif         	            
    enddo
!    print *, PetNo, 'Level and base', k, base
  enddo 

  if (count1-1 /= totalelements) then
     print *, 'total element mismatch ', count1-1, totalelements
  endif

  do i=1, totalelements*8
     if (elementConn(i) > totalnodes) then
          print *, PetNo, 'node id out of bound', i/8, elementConn(i)
     endif
  enddo  
  call ESMF_MeshAddElements(dstMesh, elementIds, elementTypes, elementConn,rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  !deallocate(elementIds, elementTypes, elementConn)
  print *, PetNo, 'Done MeshAddElements()'

  globaltotal = dstdims(1)*dstdims(2)*totallevels
  
  deallocate(nodeCoords)
  allocate(nodeCoords(localnodes*3))
  call ESMF_MeshGet(dstMesh, nodalDistgrid=nodaldistgrid, & 
       			      rc=rc)
  call ESMF_MeshGet(dstMesh, ownedNodeCoords=nodeCoords, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  ! create an array on the nodaldistgrid and gather the results at PetNo=0
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  array=ESMF_ArrayCreate(nodaldistgrid, arrayspec, &
   		        distgridToArrayMap=(/2/), &
   			undistLBound=(/1/), undistUBound=(/3/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
   call ESMF_ArrayGet(array, farrayPtr=fptr2d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  do i=1, localnodes
     count1=(i-1)*3
     call Convert2Sphdeg(nodeCoords(count1+1), nodeCoords(count1+2), &
     	  nodeCoords(count1+3),fptr2d(1,i), fptr2d(2,i), fptr2d(3,i))
     !fptr2d(1:3,i)=nodeCoords((i-1)*3+1:(i-1)*3+3)
  enddo
  ! Create the destination array at PET0 to do the redist
  distgrid = ESMF_DistgridCreate((/1/), (/globaltotal/),regDecomp=(/1/),rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  array1 = ESMF_ArrayCreate(distgrid, arrayspec, &
   		        distgridToArrayMap=(/2/), &
   			undistLBound=(/1/), undistUBound=(/3/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  if (PetNo == 0) then
       call ESMF_ArrayGet(array1, farrayptr=fptr2d, rc=rc)
       if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
       fptr2d(:,:)=-999.0
  endif
  call ESMF_ArrayRedistStore(array, array1, routehandle, &
       				   ignoreUnmatchedIndices=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArrayRedist(array, array1, routehandle, rc=rc)   
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArrayRedistRelease(routehandle, rc=rc)
  call ESMF_ArrayDestroy(array)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  call ESMF_MeshGet(dstMesh, nodalDistgrid=nodaldistgrid, rc=rc)
  ! create an array on the nodaldistgrid and gather the results at PetNo=0
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  array=ESMF_ArrayCreate(nodaldistgrid, dstdata, rc=rc)
  
  ! Create an array on PET0 and redist the data to PET0 to be written out to NC file 
  distgrid = ESMF_DistgridCreate((/1/), (/globaltotal/),regDecomp=(/1/),rc=rc)
 if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  array2 = ESMF_ArrayCreate(distgrid, arrayspec, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  if (PetNo == 0) then
     call ESMF_ArrayGet(array2, farrayptr=fptr1d, rc=rc)
     if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
     fptr1d(:)=-999.0
  endif
  call ESMF_ArrayRedistStore(array, array2, routehandle, &
       				   ignoreUnmatchedIndices=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArrayRedist(array, array2, routehandle, rc=rc)   
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArrayRedistRelease(routehandle, rc=rc)
  call ESMF_ArrayDestroy(array)

  if (PetNo==0) then
    status = nf90_create(dstoutfile, nf90_clobber, nc1)
    call CheckNCError(status, dstoutfile)
    status = nf90_def_dim(nc1, 'numnodes', globaltotal, nodeid)
    status = nf90_def_dim(nc1, 'numcells', globaltotalelmt*8, elmtid)

    status = nf90_def_var(nc1, 'nodelon', NF90_DOUBLE, (/nodeid/), lonid)
    status = nf90_def_var(nc1, 'nodelat', NF90_DOUBLE, (/nodeid/), latid)
    status = nf90_def_var(nc1, 'heights', NF90_DOUBLE, (/nodeid/), hgtid)
    status = nf90_def_var(nc1, 'vertids', NF90_INT, (/elmtid/),  connid)
    status = nf90_def_var(nc1, 'testdata1', NF90_DOUBLE, (/nodeid/),  data1id)
    status = nf90_def_var(nc1, 'testdata2', NF90_DOUBLE, (/nodeid/),  data2id)
    status = nf90_enddef(nc1)
#if 0
    allocate(lontbl(globaltotal), lattbl(globaltotal), hgttbl(globaltotal))
    ii = 1
    do k = 1, totallevels
     do j=1,dstdims(2)
       do i=1,dstdims(1)
         lontbl(ii)= dstlon(i,j)
	 lattbl(ii)=dstlat(i,j)
	 hgttbl(ii)=dsthgt(k+startlevel-1)
	 ii = ii + 1
       enddo
     enddo
    enddo
#endif 
    allocate(databuf(globaltotal))
    databuf(:)=fptr2d(1,:)
    status = nf90_put_var(nc1, lonid, databuf)
    call CheckNCError(status, 'nodelon')
    databuf(:)=fptr2d(2,:)
    status = nf90_put_var(nc1, latid, databuf)
    call CheckNCError(status, 'nodelat')
    databuf(:)=fptr2d(3,:)
    status = nf90_put_var(nc1, hgtid, databuf)
    call CheckNCError(status, 'heights')
    deallocate(databuf)
    status = nf90_put_var(nc1, data1id, fptr1d)
    call CheckNCError(status, 'testdata1')
    status = nf90_close(nc1)
  endif
  call ESMF_VMBarrier(vm)
  call ESMF_ArrayDestroy(array1)
  call ESMF_ArrayDestroy(array2)
  
  do i=1,totalelements*8
    elementConn(i)=nodeIds(elementConn(i))
  enddo 
 
  do i=0,PetCnt-1
    if (PetNo == i) then
       print *, PetNo, 'write out data starting ', nodestartid
       status = nf90_open(dstoutfile, NF90_WRITE, nc1)
       call CheckNCError(status, dstoutfile)
       print *, PetNo, 'write element conn', startid+1, totalelements
       status = nf90_inq_varId(nc1, 'vertids', vertid)
       call CheckNCError(status, 'vertids')
       status = nf90_put_var(nc1, vertid, elementConn, &
       	      	start=(/startid*8+1/), count=(/8*totalelements/))
       call CheckNCError(status, 'vertids')
       status = nf90_close(nc1)
    endif
    call ESMF_VMBarrier(vm)
  enddo 
  deallocate(dstlon, dstlat, dsthgt)
  deallocate(elementIds, elementTypes, elementConn)

  call ESMF_VMBarrier(vm)

  ! Create src and dst fields and run RegridStore()
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_NODE,rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

#if 1
  call ESMF_VMWTime(starttime)
  print *, PetNo, 'Before ESMF_FieldRegridStore'
  call ESMF_VMLogMemInfo('Before ESMF_FieldRegridStore',rc=rc)
  call ESMF_FieldRegridStore(srcField, dstField, &
       	 unmappedaction =ESMF_UNMAPPEDACTION_IGNORE, &
	 regridmethod = ESMF_REGRIDMETHOD_BILINEAR, &
	 polemethod = ESMF_POLEMETHOD_NONE, &
	 routehandle = routehandle, &
	 factorList=weights, factorIndexList=indices, rc=rc)
  call ESMF_VMLogMemInfo('After ESMF_FieldRegridStore',rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_VMWTime(endtime)

  write(*,*) "Completed weight generation in ", (endtime-starttime)*1000, "msecs"

  ! find out total number of weights
  wgtcount(1)=size(weights,1)
  allocate(allCounts(PetCnt))
  call ESMF_VMAllGather(vm,wgtcount,allCounts,1,rc=status)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  ! calculate the size of the global weight table
  startid = 1
  totalwgts = 0
  do i=1,PetNo
     startid = startid+allCounts(i)
  enddo
  do i=1,PetCnt
    totalwgts=allCounts(i)+totalwgts
  end do

   print *, PetNo, 'local count ', wgtcount(1),  startid, totalwgts

  ! Output the weights into a NetCDF file
  if (PetNo==0) then
    status = nf90_create('ipe2wamwgt.nc', nf90_clobber, nc1)
    call CheckNCError(status, dstoutfile)
    status = nf90_def_dim(nc1, 'numwgts', totalwgts, nodeid)

    status = nf90_def_var(nc1, 'weights', NF90_DOUBLE, (/nodeid/), wgtid)
    status = nf90_def_var(nc1, 'src', NF90_INT, (/nodeid/), srcid)
    status = nf90_def_var(nc1, 'dst', NF90_INT, (/nodeid/), dstid)
    status = nf90_enddef(nc1)
    status = nf90_close(nc1)
  endif

do i=0,PetCnt-1
   if (i==PetNo) then
       status = nf90_open('ipe2wamwgt.nc', NF90_WRITE, nc1)
       call CheckNCError(status, dstoutfile)
       print *, PetNo, 'write factorList and factorIndexList', startid, wgtcount(1)
       status = nf90_inq_varId(nc1, 'weights', wgtid)
       call CheckNCError(status, 'weights')
       status = nf90_put_var(nc1, wgtid, weights, &
       	      	start=(/startid/), count=(/wgtcount(1)/))
       call CheckNCError(status, 'weights')
       status = nf90_inq_varId(nc1, 'src', srcid)
       call CheckNCError(status, 'src')
       status = nf90_put_var(nc1, srcid, indices(1,:), &
       	      	start=(/startid/), count=(/wgtcount(1)/))
       call CheckNCError(status, 'dst')
       status = nf90_inq_varId(nc1, 'dst', dstid)
       call CheckNCError(status, 'dst')
       status = nf90_put_var(nc1, dstid, indices(2,:), &
       	      	start=(/startid/), count=(/wgtcount(1)/))
       call CheckNCError(status, 'dst')
       status = nf90_close(nc1)
    endif
    call ESMF_VMBarrier(vm)
enddo
  
  ! now check the destination values agaist it analytic values
  call ESMF_FieldGet(dstField, farrayptr=fptr1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  print *, PetNo, 'dstfield bound', lbound(fptr1d), ubound(fptr1d)
  fptr1d = -999.0
  deallocate(srcdata, maxlevs, northind, southind)
  call ESMF_FieldRegrid(srcField, dstField, routehandle, &
       zeroregion=ESMF_REGION_SELECT, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  
  ! now check the destination values agaist it analytic values
  call ESMF_FieldGet(dstField, farrayptr=fptr1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  print *, PetNo, 'dstdata min/max', minval(fptr1d), maxval(fptr1d)

  ! check the average relative errors
  totalerrors = 0
  minerror = 1000.0
  maxerror = -1000.0
  count1=0
  do i=1, localnodes
   if (fptr1d(i) > -990.0) then
     differr = abs(fptr1d(i)-dstdata(i))/dstdata(i)
     totalerrors = totalerrors + differr
     if (differr < minerror) minerror=differr
     if (differr > maxerror) maxerror=differr
     count1=count1+1
   endif
  enddo
  !deallocate(dstdata)

  print *, PetNo, 'count/mean/min/max errors:', count1, totalerrors/count1, minerror, maxerror

  call ESMF_VMBarrier(vm)

  ! Write out the destination array, also redist to PET0 before writing out
  call ESMF_FieldGet(dstField, array=array,rc=rc)
  ! Create the destination array at PET0 to do the redist
  distgrid = ESMF_DistgridCreate((/1/), (/globaltotal/),regDecomp=(/1/),rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  array1 = ESMF_ArrayCreate(distgrid, arrayspec, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  if (PetNo == 0) then
     call ESMF_ArrayGet(array1, farrayptr=fptr1d, rc=rc)
     if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
     fptr1d(:)=-999.0
  endif
  call ESMF_ArrayRedistStore(array, array1, routehandle, &
       				   ignoreUnmatchedIndices=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArrayRedist(array, array1, routehandle, rc=rc)   
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArrayRedistRelease(routehandle, rc=rc)

  deallocate(nodeIds, nodeCoords, nodeOwners)
  ! Write out the interpolated data
  if (PetNo==0) then
    status = nf90_open(dstoutfile, NF90_WRITE, nc1)
    call CheckNCError(status, dstoutfile)
    status = nf90_inq_varId(nc1, 'testdata2', data2id)
    call CheckNCError(status, 'testdata2')
    status = nf90_put_var(nc1, data2id, fptr1d)
    call CheckNCError(status, 'testdata2')
    status = nf90_close(nc1)
    call CheckNCError(status, 'testdata2')
  endif
  call ESMF_ArrayDestroy(array1)
!  call ESMF_FieldDestroy(srcField)
!  call ESMF_FieldDestroy(dstField)
  call ESMF_FieldRegridRelease(routehandle)
  deallocate(weights, indices)

#endif

  ! Do WAM to IPE regridding
  call ESMF_VMWTime(starttime)
  print *, PetNo, 'Before ESMF_FieldRegridStore'
  call ESMF_VMLogMemInfo('Before ESMF_FieldRegridStore',rc=rc)
  call ESMF_FieldRegridStore(dstField, srcField, &
       	 unmappedaction =ESMF_UNMAPPEDACTION_IGNORE, &
	 regridmethod = ESMF_REGRIDMETHOD_BILINEAR, &
	 polemethod = ESMF_POLEMETHOD_NONE, &
	 routehandle = routehandle, &
	 factorList=weights, factorIndexList=indices, rc=rc)
  call ESMF_VMLogMemInfo('After ESMF_FieldRegridStore',rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  call ESMF_VMWTime(endtime)
  write(*,*) "Completed weight generation WAM->IPE in ", (endtime-starttime)*1000, "msecs"

  ! find out total number of weights
  wgtcount(1)=size(weights,1)
  allocate(allCounts(PetCnt))
  call ESMF_VMAllGather(vm,wgtcount,allCounts,1,rc=status)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  ! calculate the size of the global weight table
  startid = 1
  totalwgts = 0
  do i=1,PetNo
     startid = startid+allCounts(i)
  enddo
  do i=1,PetCnt
    totalwgts=allCounts(i)+totalwgts
  end do

   print *, PetNo, 'local count ', wgtcount(1),  startid, totalwgts

  ! Output the weights into a NetCDF file
  if (PetNo==0) then
!    status = nf90_create('wam2ipewgt.nc', IOR(nf90_clobber, nf90_netcdf4), nc1)
    status = nf90_create('wam2ipewgt.nc', nf90_clobber, nc1)
    call CheckNCError(status, dstoutfile)
    status = nf90_def_dim(nc1, 'numwgts', totalwgts, nodeid)

    status = nf90_def_var(nc1, 'weights', NF90_DOUBLE, (/nodeid/), wgtid)
    status = nf90_def_var(nc1, 'src', NF90_INT, (/nodeid/), srcid)
    status = nf90_def_var(nc1, 'dst', NF90_INT, (/nodeid/), dstid)
    status = nf90_enddef(nc1)
    status = nf90_close(nc1)
  endif

do i=0,PetCnt-1
   if (i==PetNo) then
       status = nf90_open('wam2ipewgt.nc', NF90_WRITE, nc1)
       call CheckNCError(status, dstoutfile)
       print *, PetNo, 'write factorList and factorIndexList', startid, wgtcount(1)
       status = nf90_inq_varId(nc1, 'weights', wgtid)
       call CheckNCError(status, 'weights')
       status = nf90_put_var(nc1, wgtid, weights, &
       	      	start=(/startid/), count=(/wgtcount(1)/))
       call CheckNCError(status, 'weights')
       print *, 'before write out src', size(indices(1,:))
       status = nf90_inq_varId(nc1, 'src', srcid)
       call CheckNCError(status, 'src')
       status = nf90_put_var(nc1, srcid, indices(1,:), &
       	      	start=(/startid/), count=(/wgtcount(1)/))
       call CheckNCError(status, 'dst')
       print *, 'before write out dst'
       status = nf90_inq_varId(nc1, 'dst', dstid)
       call CheckNCError(status, 'dst')
       status = nf90_put_var(nc1, dstid, indices(2,:), &
       	      	start=(/startid/), count=(/wgtcount(1)/))
       call CheckNCError(status, 'dst')
       status = nf90_close(nc1)
    endif
    call ESMF_VMBarrier(vm)
enddo
  
  ! Assign values to the WAM grid before regrid
  ! now check the destination values agaist it analytic values
  call ESMF_FieldGet(dstField, farrayptr=fptr1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  fptr1d = dstdata ! analystical data
  deallocate(dstdata)
  ! Save the source data used as analytical data
  call ESMF_FieldGet(srcField, farrayptr=fptr1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  allocate(dstdata(size(fptr1d)))
  dstdata(:) = fptr1d(:)
  print *, PetNo, 'dstfield bound', lbound(fptr1d), ubound(fptr1d)
  fptr1d = -999.0
  call ESMF_FieldRegrid(dstField, srcField, routehandle, &
       zeroregion=ESMF_REGION_SELECT, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  
  ! now check the destination values agaist it analytic values
  call ESMF_FieldGet(srcField, farrayptr=fptr1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  print *, PetNo, 'dstdata min/max', minval(fptr1d), maxval(fptr1d)

  ! check the average relative errors
  totalerrors = 0
  minerror = 1000.0
  maxerror = -1000.0
  count1=0
  localnodes = size(dstdata)
  do i=1, localnodes
   if (fptr1d(i) > -990.0) then
     differr = abs(fptr1d(i)-dstdata(i))/dstdata(i)
     totalerrors = totalerrors + differr
     if (differr < minerror) minerror=differr
     if (differr > maxerror) maxerror=differr
     count1=count1+1
   endif
  enddo
  deallocate(dstdata)

  print *, PetNo, 'count/mean/min/max errors:', count1, totalerrors/count1, minerror, maxerror

  ! Write out the destination array, also redist to PET0 before writing out
  call ESMF_FieldGet(srcField, array=array,rc=rc)
  ! Create the destination array at PET0 to do the redist
  distgrid = ESMF_DistgridCreate((/1/), (/srctotalnodes/),regDecomp=(/1/),rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  array1 = ESMF_ArrayCreate(distgrid, arrayspec, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  if (PetNo == 0) then
     call ESMF_ArrayGet(array1, farrayptr=fptr1d, rc=rc)
     if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
     fptr1d(:)=-999.0
  endif
  call ESMF_ArrayRedistStore(array, array1, routehandle, &
       				   ignoreUnmatchedIndices=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArrayRedist(array, array1, routehandle, rc=rc)   
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  call ESMF_ArrayRedistRelease(routehandle, rc=rc)

  ! Write out the interpolated data
  if (PetNo==0) then
    status = nf90_open(srcoutfile, NF90_WRITE, nc1)
    call CheckNCError(status, srcoutfile)
    status = nf90_inq_varId(nc1, 'testdata2', data2id)
    call CheckNCError(status, 'testdata2')
    status = nf90_put_var(nc1, data2id, fptr1d)
    call CheckNCError(status, 'testdata2')
    status = nf90_close(nc1)
    call CheckNCError(status, 'testdata2')
  endif

  call ESMF_Finalize()

contains

  subroutine ErrorMsgAndAbort(localPet)
    integer ::  localPet
  
    if (localPet >= 0) then
      write(*,*) "ERROR: Problem on processor ",localPet,". Please see the PET*.RegridWeightGen.Log files for a traceback."
    else
      write(*,*) "ERROR: Please see the PET*.RegridWeightGen.Log files for a traceback."
    endif
  
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  end subroutine ErrorMsgAndAbort

!------------------------------------------------------------------------------
!
!  check CDF file error code
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CheckNCError"
subroutine CheckNCError (ncStatus, errmsg)

    integer,          intent(in)  :: ncStatus
    character(len=*), intent(in)  :: errmsg

    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    if ( ncStatus .ne. nf90_noerror) then
        print '("NetCDF Error: ", A, " : ", A)', &
    		trim(errmsg),trim(nf90_strerror(ncStatus))
        call ErrorMsgAndAbort(-1)
    end if
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine CheckNCError

!------------------------------------------------------------------------------
!
!  check CDF file error code
!
#undef  ESMF_METHOD
#define ESMF_METHOD "convet2Cart"
subroutine convert2Cart (lon, lat, hgt, coords, rc)
   real(ESMF_KIND_R8):: lon, lat, hgt
   real(ESMF_KIND_R8):: coords(3)
   integer, optional :: rc

   real(ESMF_KIND_R8) :: earthradius, nhgt
   integer :: localrc

   if (present(rc)) rc=ESMF_FAILURE
   earthradius = 6371.0
   nhgt = 1+hgt/earthradius

   call c_esmc_sphdeg_to_cart(lon, lat, &
               coords(1), coords(2), coords(3), &
               localrc)
   if (localrc /= ESMF_SUCCESS) return 

   coords(1)=nhgt*coords(1)
   coords(2)=nhgt*coords(2)
   coords(3)=nhgt*coords(3)

   if (present(rc)) rc=ESMF_SUCCESS

end subroutine convert2Cart

#undef  ESMF_METHOD
#define ESMF_METHOD "convet2Sphdeg"
subroutine convert2Sphdeg (coord1, coord2, coord3, lon, lat, hgt)
   real(ESMF_KIND_R8):: coord1, coord2, coord3
   real(ESMF_KIND_R8):: lon, lat, hgt

   real(ESMF_KIND_R8) :: earthradius, nhgt, rad2deg
   real, parameter :: PI=3.1415927
   integer :: localrc

   earthradius = 6371.0
   rad2deg = 180.0/PI
   nhgt = sqrt(coord1*coord1+coord2*coord2+coord3*coord3)
   hgt = (nhgt-1)*earthradius
   lon = atan(coord2/coord1)*rad2deg
   if (coord1 < 0) lon = lon + 180.0
   if (coord1 > 0 .and. coord2 < 0) lon = 360.0 + lon
   lat = 90-acos(coord3/nhgt)*rad2deg

end subroutine convert2Sphdeg

end program
  	    