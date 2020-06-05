!===============================================================================
! MODULE: HYCOM_COUPLE
!
! DESCRIPTION:
!   This module copies internal HYCOM variables to/from NUOPC fields.
!
! SUBROUTINES:
!   hycom_couple_init
!     Initialize decomposition blocks, coordinates, mask, and area.
!
!   set_hycom_import_flag
!     Determine field connections.
!
!   export_from_hycom_deb
!     Copy internal HYCOM variable to export fortran array.
!
!   import_to_hycom_deb
!     Copy import fortran array to internal HYCOM variable.
!
!   ocn_import_forcing
!     Process uncoupled variables.
!
!   hycom_couple_final
!     Deallocate memory.
!
!===============================================================================
module hycom_couple

!===============================================================================
! use modules
!===============================================================================
  use mod_xc ! HYCOM communication interface
  use mod_cb_arrays
  use hycom_read_latlon, only: get_coord

!===============================================================================
! settings
!===============================================================================
  implicit none

  private

!===============================================================================
! public
!===============================================================================
  public :: hycom_couple_init
  public :: set_hycom_import_flag
  public :: export_from_hycom_deb
  public :: import_to_hycom_deb
  public :: ocn_import_forcing
  public :: hycom_couple_final
  public :: cpldom_type
  public :: cpldom

!===============================================================================
! module variables
!===============================================================================
  type cpldom_type
    integer              :: idim_size
    integer              :: jdim_size
    integer, allocatable :: deBList(:,:,:)
    real, allocatable    :: lon_p(:,:)
    real, allocatable    :: lat_p(:,:)
    real, allocatable    :: mask_p(:,:)
    real, allocatable    :: area_p(:,:)
    real, allocatable    :: lon_q(:,:)
    real, allocatable    :: lat_q(:,:)
    real, allocatable    :: mask_q(:,:)
    real, allocatable    :: area_q(:,:)
  end type cpldom_type

  type(cpldom_type) :: cpldom

!===============================================================================
  contains
!===============================================================================
  subroutine hycom_couple_init(nPets, rc)
!   arguments
    integer, intent(in)  :: nPets
    integer, intent(out) :: rc
!   local variables
    character(*), parameter :: rname="hycom_couple_init"
    real, allocatable       :: tmx(:,:)
    real, allocatable       :: tmp_e(:,:)
    integer                 :: i, j

    rc = 0 ! success

    if (mnproc.eq.1) print *, rname//" start..."

!   grid size
    cpldom%idim_size=itdm
    cpldom%jdim_size=jtdm

#ifdef ESPC_COUPLE
!   deBlockList
!   directly from HYCOM
    if (.not.allocated(cpldom%deBList)) then
      allocate(cpldom%deBList(2,2,nPets))
    endif
    do i=1, nPets
      cpldom%deBList(1,1,i)=deBlockList(1,1,i)
      cpldom%deBList(2,1,i)=deBlockList(2,1,i)
      cpldom%deBList(1,2,i)=deBlockList(1,2,i)
      cpldom%deBList(2,2,i)=deBlockList(2,2,i)
    enddo
    if (mnproc.eq.1) then
      print *,'itdm,jtdm=',itdm,jtdm
      print *,'hycom,deBList BL11 BL21 BL12 BL22'
      do i=1, nPets
        write(*,"(I4,4I8,3x,2I8)") i,                    &
          cpldom%deBList(1,1,i),cpldom%deBList(2,1,i),   &
          cpldom%deBList(1,2,i),cpldom%deBList(2,2,i),   &
          cpldom%deBList(1,2,i)-cpldom%deBList(1,1,i)+1, &
          cpldom%deBList(2,2,i)-cpldom%deBList(2,1,i)+1
      enddo
    endif
#endif

!   allocate arrays
    if (mnproc.eq.1) then
      if (.not.allocated(cpldom%lon_p)) allocate(cpldom%lon_p(itdm,jtdm))
      if (.not.allocated(cpldom%lat_p)) allocate(cpldom%lat_p(itdm,jtdm))
      if (.not.allocated(cpldom%area_p)) allocate(cpldom%area_p(itdm,jtdm))
      if (.not.allocated(cpldom%mask_p)) allocate(cpldom%mask_p(itdm,jtdm))
      if (.not.allocated(cpldom%lon_q)) allocate(cpldom%lon_q(itdm,jtdm))
      if (.not.allocated(cpldom%lat_q)) allocate(cpldom%lat_q(itdm,jtdm))
      if (.not.allocated(cpldom%area_q)) allocate(cpldom%area_q(itdm,jtdm))
      if (.not.allocated(cpldom%mask_q)) allocate(cpldom%mask_q(itdm,jtdm))
!     read hycom regional.grid.a
      call get_coord(cpldom%lat_p, cpldom%lon_p, cpldom%area_p, cpldom%lat_q, &
        cpldom%lon_q, cpldom%area_q, itdm, jtdm, rc)
    else
      if (.not.allocated(cpldom%lon_p)) allocate(cpldom%lon_p(1,1))
      if (.not.allocated(cpldom%lat_p)) allocate(cpldom%lat_p(1,1))
      if (.not.allocated(cpldom%area_p)) allocate(cpldom%area_p(1,1))
      if (.not.allocated(cpldom%mask_p)) allocate(cpldom%mask_p(1,1))
      if (.not.allocated(cpldom%lon_q)) allocate(cpldom%lon_q(1,1))
      if (.not.allocated(cpldom%lat_q)) allocate(cpldom%lat_q(1,1))
      if (.not.allocated(cpldom%area_q)) allocate(cpldom%area_q(1,1))
      if (.not.allocated(cpldom%mask_q)) allocate(cpldom%mask_q(1,1))
      cpldom%lat_p(:,:)=0.0
      cpldom%lon_p(:,:)=0.0
      cpldom%area_p(:,:)=0.0
      cpldom%lat_q(:,:)=0.0
      cpldom%lon_q(:,:)=0.0
      cpldom%area_q(:,:)=0.0
    endif

!   mask information
    if (.not.allocated(tmx)) allocate(tmx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy))

!   mask_p information
    tmx(:,:)=0.0
    do j=1, jj
    do i=1, ii
      tmx(i,j)=ishlf(i,j)
    enddo
    enddo
    call xcaget(cpldom%mask_p, tmx, 1)

!   mask_q information
    tmx(:,:)=0.0
    do j=1, jj
    do i=1, ii
      tmx(i,j)=iq(i,j)
    enddo
    enddo
    call xcaget(cpldom%mask_q, tmx, 1)

    if (mnproc.eq.1) print *, rname//" end..."

  end subroutine hycom_couple_init

  !-----------------------------------------------------------------------------

  subroutine set_hycom_import_flag(k, fieldName, rc)
!   arguments
    integer, intent(in)           :: k
    character(len=30), intent(in) :: fieldName
    integer, intent(out)          :: rc
!   local variables
    character(*), parameter :: rname="set_hycom_import_flag"

    rc = 0 ! success

    if (mnproc.eq.1) print *, rname//" start...,k,name=", k, fieldName

!   initialize couple flags when field index is 1
    if (k.eq.1) then
      cpl_taux=.false.
      cpl_tauy=.false.
      cpl_u10=.false.
      cpl_v10=.false.
      cpl_wndspd=.false.
      cpl_ustara=.false.
      cpl_airtmp=.false.
      cpl_vapmix=.false.
      cpl_swflx_net=.false.
      cpl_lwflx_net=.false.
      cpl_swflx_net2down=.false.
      cpl_lwflx_net2down=.false.
      cpl_swflxd=.false.
      cpl_lwflxd=.false.
      cpl_mslprs=.false.
      cpl_precip=.false.
      cpl_surtmp=.false.
      cpl_seatmp=.false.
      cpl_sbhflx=.false.
      cpl_lthflx=.false.
      cpl_sic=.false.
      cpl_sitx=.false.
      cpl_sity=.false.
      cpl_siqs=.false.
      cpl_sifh=.false.
      cpl_sifs=.false.
      cpl_sifw=.false.
      cpl_sit=.false.
      cpl_sih=.false.
      cpl_siu=.false.
      cpl_siv=.false.
    endif

!   set couple flags based on fieldnames
    if (fieldName.eq.'taux10') then
      cpl_taux=.true.
    elseif (fieldName.eq.'tauy10') then
      cpl_tauy=.true.
      if (.not.cpl_taux) then
        if (mnproc.eq.1) print *,"error - tauy before taux"
        call xcstop('('//rname//')')
               stop '('//rname//')'
      endif !error
    elseif (fieldName.eq.'u10') then
      cpl_u10=.true.
    elseif (fieldName.eq.'v10') then
      cpl_v10=.true.
      if (.not.cpl_u10) then
        if (mnproc.eq.1) print *,"error - v10 before u10"
        call xcstop('('//rname//')')
               stop '('//rname//')'
      endif !error
    elseif (fieldName.eq.'wndspd10') then
      cpl_wndspd=.true.
    elseif (fieldName.eq.'ustara10') then
      cpl_ustara=.true.
    elseif (fieldName.eq.'airtmp') then
      cpl_airtmp=.true.
    elseif (fieldName.eq.'airhum') then
      cpl_vapmix=.true.
    elseif (fieldName.eq.'swflx_net') then
      cpl_swflx_net=.true.
    elseif (fieldName.eq.'lwflx_net') then
      cpl_lwflx_net=.true.
    elseif (fieldName.eq.'swflx_net2down') then
      cpl_swflx_net2down=.true.
    elseif (fieldName.eq.'lwflx_net2down') then
      cpl_lwflx_net2down=.true.
    elseif (fieldName.eq.'swflxd') then
      cpl_swflxd=.true.
    elseif (fieldName.eq.'lwflxd') then
      cpl_lwflxd=.true.
    elseif (fieldName.eq.'mslprs') then
      cpl_mslprs=.true.
    elseif (fieldName.eq.'prcp') then
      cpl_precip=.true.
    elseif (fieldName.eq.'gt') then
      cpl_surtmp=.true.
      cpl_seatmp=.true.
    elseif (fieldName.eq.'sbhflx') then
      cpl_sbhflx=.true.
    elseif (fieldName.eq.'lthflx') then
      cpl_lthflx=.true.
!   import ice concentration
    elseif (fieldName.eq.'sic') then
      cpl_sic=.true.
!   import ice x-stress
    elseif (fieldName.eq.'sitx') then
      cpl_sitx=.true.
!   import ice y-stress
    elseif (fieldName.eq.'sity') then
      cpl_sity=.true.
!   import solar thru grid cell ave.
    elseif (fieldName.eq.'siqs') then
      cpl_siqs=.true.
!   import freeze, melt, h. flux
    elseif (fieldName.eq.'sifh') then
      cpl_sifh=.true.
!   import salt flux
    elseif (fieldName.eq.'sifs') then
      cpl_sifs=.true.
!   import water flux
    elseif (fieldName.eq.'sifw') then
      cpl_sifw=.true.
!   import sea ice temperature
    elseif (fieldName.eq.'sit_sfc') then
      cpl_sit=.true.
!   import sea ice thickness
    elseif (fieldName.eq.'sih') then
      cpl_sih=.true.
!   import sea ice x-velocity
    elseif (fieldName.eq.'siu') then
      cpl_siu=.true.
!   import sea ice y-velocity
    elseif (fieldName.eq.'siv') then
      cpl_siv=.true.
    else ! error
      if (mnproc.eq.1) print *, "error - fieldName unknown: "//trim(fieldName)
      call xcstop('('//rname//')')
      stop '('//rname//')'
    endif !fieldName

    if (mnproc.eq.1) print *, rname//" end..."

  end subroutine set_hycom_import_flag

  !-----------------------------------------------------------------------------

  subroutine export_from_hycom_deb(tlb, tub, expData, fieldName, show_minmax, &
    rc)
!   arguments
    integer, intent(in)           :: tlb(2)
    integer, intent(in)           :: tub(2)
    real, intent(inout)           :: expData(tlb(1):tub(1),tlb(2):tub(2))
    character(len=30), intent(in) :: fieldName
    logical, intent(in)           :: show_minmax
    integer, intent(out)          :: rc
!   local variables
    character(*), parameter :: rname="export_from_hycom_deb"
    real, allocatable       :: ocn_msk(:,:)
    real, allocatable       :: field_tmp(:,:)
    real, allocatable       :: tmx(:,:)
    integer                 :: i, j, jja
!   integer                 :: k
!   real                    :: mgrid(ii,jj)

    rc = 0 ! success

    if (mnproc.eq.1) print *, rname//" start...,name=", fieldName

!   (1+i0,ii+i0) could be the subset of (tlb(1),tub(1))
!   (1+j0,jja+j0) == (tlb(2),tub(2))

!   print *,"idm,jdm,nbdy,ii,jj=",mnproc,idm,jdm,nbdy,ii,jj

    call export_from_hycom_tiled(util2, fieldName) !can't use util1

#if defined(ARCTIC)
!   arctic (tripole) domain, top row is replicated (ignore it)
    jja=min(jj,(jtdm-1-j0))
#else
    jja=jj
#endif

!   export unit conversions
    if (fieldName.eq.'sst') then
      do j=1, jj
      do i=1, ii
#ifndef ESPC_NOCANONICAL_CONVERT
!       canonical unit conversion: sst (C) -> (K)
        util2(i,j)=util2(i,j)+273.15d0
#endif
      enddo
      enddo
    endif

!   copy internal data to export array
    expData(:,:)=0.0
    do j=1, jja
    do i=1, ii
!     mgrid(i,j)=util2(i,j)
      expData(i+i0,j+j0)=util2(i,j)
    enddo
    enddo

!   diagnostic output
    if (show_minmax) then
!     allocate memory
      if (mnproc.eq.1) then
        allocate(ocn_msk(itdm,jtdm))
        allocate(field_tmp(itdm,jtdm))
      else
        allocate(ocn_msk(1,1))
        allocate(field_tmp(1,1))
      endif
      allocate(tmx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy))

!     ocn_msk: sea/land mask
      tmx(:,:)=0.0
      do j=1, jja
      do i=1, ii
        tmx(i,j)=ishlf(i,j)
!x      tmx(i,j)=ip(i,j)
      enddo
      enddo
      call xcaget(ocn_msk,tmx,1)
!     call xcsync(no_flush)

!     field_tmp: export field data
      tmx(:,:)=0.0
      do j=1, jja
      do i=1, ii
        tmx(i,j)=expData(i+i0,j+j0)
      enddo
      enddo
      call xcaget(field_tmp,tmx,1)
!     call xcsync(no_flush)

!     write minmax to stdout
      if (mnproc.eq.1) then
        write(*,992) trim(fieldName),          &
          maxval(field_tmp,mask=ocn_msk.eq.1), &
          minval(field_tmp,mask=ocn_msk.eq.1), &
          (sum(field_tmp,mask=ocn_msk.eq.1)/count(ocn_msk.eq.1))
 992    format('export_from_hycom_deb,max,min,mean=',A10,3E23.15)
      endif

!     test check pang
      call xcaget(ocn_msk, pang, 1)
      if (mnproc.eq.1) then
        print *,'export_from_hycom pang, min,max=', &
          minval(ocn_msk), maxval(ocn_msk)
      endif

!     deallocate memory
      if (allocated(ocn_msk)) deallocate(ocn_msk)
      if (allocated(field_tmp)) deallocate(field_tmp)
      if (allocated(tmx)) deallocate(tmx)
    endif !show_minmax

    if (mnproc.eq.1) print *, rname//" end..."

  end subroutine export_from_hycom_deb

  !-----------------------------------------------------------------------------

  subroutine import_to_hycom_deb(tlb, tub, impData, fieldName, show_minmax, &
    data_init_flag, rc)
!   arguments
    integer, intent(in)           :: tlb(2), tub(2)
    real, intent(inout)           :: impData(tlb(1):tub(1),tlb(2):tub(2))
    character(len=30), intent(in) :: fieldName
    logical, intent(in)           :: show_minmax
    logical, intent(in)           :: data_init_flag
    integer, intent(out)          :: rc
!   local variables
    character(*), parameter :: rname="import_to_hycom_deb"
    integer                 :: i, j, mcnt
    real                    :: uij, vij
    real, allocatable       :: ocn_msk(:,:)
    real, allocatable       :: field_tmp(:,:)
    real, allocatable       :: tmx(:,:)
    real, parameter         :: sstmin=-1.8d0
    real, parameter         :: sstmax=35.0d0
    integer                 :: jja
    real                    :: albw,degtorad
    integer                 :: ierr
!   integer                 :: k

    rc = 0 ! success

    if (mnproc.eq.1) print *, rname//" start,name=...", fieldName

!   (1+i0,ii+i0) could be the subset of (tlb(1),tub(1))
!   (1+j0,jja+j0) == (tlb(2),tub(2))

!   if ((k.eq.1).and.(mnproc.eq.1)) print *, "w0,w1..=", w0, w1, w2, w3

#if defined(ARCTIC)
!   arctic (tripole) domain, top row is replicated (ignore it)
    jja=min(jj,(jtdm-1-j0))
#else
    jja=jj
#endif

!   -----------------
!    import from atm
!   -----------------
!   import xstress: Pa
    if (fieldName.eq.'taux10') then
      do j=1, jja
      do i=1, ii
!       taux(i,j,l0)=mgrid(i,j)
        if (ishlf(i,j).eq.1) then
          taux(i,j,l0)=impData(i+i0,j+j0)
        else
          taux(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(taux(1-nbdy,1-nbdy,l0),1,1, halo_pv)
#endif
      call xctilr(taux(1-nbdy,1-nbdy,l0),1,1, nbdy,nbdy, halo_pv)
!   -----------------
!   import ystress: Pa
    elseif (fieldName.eq.'tauy10') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          tauy(i,j,l0)=impData(i+i0,j+j0)
        else
          tauy(i,j,l0)=0.0
        endif
          uij=taux(i,j,l0)
          vij=tauy(i,j,l0)
!         rotate to (x,y)ward
          taux(i,j,l0)=cos(pang(i,j))*uij + sin(pang(i,j))*vij
          tauy(i,j,l0)=cos(pang(i,j))*vij - sin(pang(i,j))*uij
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(taux(1-nbdy,1-nbdy,l0),1,1, halo_pv)
      call xctila(tauy(1-nbdy,1-nbdy,l0),1,1, halo_pv)
#endif
      call xctilr(taux(1-nbdy,1-nbdy,l0),1,1, nbdy,nbdy, halo_pv)
      call xctilr(tauy(1-nbdy,1-nbdy,l0),1,1, nbdy,nbdy, halo_pv)
!   -----------------
!   import u wind at 10m height: ms-1
    elseif (fieldName.eq.'u10') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          taux(i,j,l0)=impData(i+i0,j+j0)
        else
          taux(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(taux(1-nbdy,1-nbdy,l0),1,1, halo_pv)
#endif
      call xctilr(taux(1-nbdy,1-nbdy,l0),1,1, nbdy,nbdy, halo_pv)
!   -----------------
!   import v wind at 10m height: ms-1
    elseif (fieldName.eq.'v10') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          tauy(i,j,l0)=impData(i+i0,j+j0)
        else
          tauy(i,j,l0)=0.0
        endif
        uij=taux(i,j,l0)
        vij=tauy(i,j,l0)
!       rotate to (x,y)ward
        taux(i,j,l0)=cos(pang(i,j))*uij + sin(pang(i,j))*vij
        tauy(i,j,l0)=cos(pang(i,j))*vij - sin(pang(i,j))*uij
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(taux(1-nbdy,1-nbdy,l0),1,1, halo_pv)
      call xctila(tauy(1-nbdy,1-nbdy,l0),1,1, halo_pv)
#endif
      call xctilr(taux(1-nbdy,1-nbdy,l0),1,1, nbdy,nbdy, halo_pv)
      call xctilr(tauy(1-nbdy,1-nbdy,l0),1,1, nbdy,nbdy, halo_pv)
!   -----------------
!   import wind speed: m s-1
    elseif (fieldName.eq.'wndspd10') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          wndspd(i,j,l0)=impData(i+i0,j+j0)
        else
          wndspd(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(wndspd(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import friction speed: m s-1
    elseif (fieldName.eq.'ustara10') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          ustara(i,j,l0)=impData(i+i0,j+j0)
        else
          ustara(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(ustara(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import air temperature
    elseif (fieldName.eq.'airtmp') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
!         canonical unit conversion: airtmp (K) -> (C)
          airtmp(i,j,l0)=impData(i+i0,j+j0)-273.15
        else
          airtmp(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(airtmp(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import specific humidity: kg kg-1
    elseif (fieldName.eq.'airhum') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          if (flxflg.ne.5) then !Alex flxflg.eq.4 => mixing ratio
!           convert from specific humidity to mixing ratio
            vapmix(i,j,l0)=impData(i+i0,j+j0)/(1.-impData(i+i0,j+j0))
          else !Alex flxflg.eq.5 => specific humidity
            vapmix(i,j,l0)=impData(i+i0,j+j0)
          endif
        else
          vapmix(i,j,l0)=0.01
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(vapmix(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import sw flux: w m-2
    elseif (fieldName.eq.'swflx_net') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          swflx(i,j,l0)=impData(i+i0,j+j0)
        else
          swflx(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(swflx(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import downward sw flux: w m-2
    elseif ((fieldName.eq.'swflx_net2down').or. &
            (fieldName.eq.'swflxd')) then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          swflx(i,j,l0)=impData(i+i0,j+j0)
        else
          swflx(i,j,l0)=0.0
        endif
      enddo
      enddo
      if (albflg.ne.0) then !swflx is Qswdn
!       use the same method as on forfun.F
!       convert swflx to net shortwave into the ocean
!       shortwave through sea ice is handled separately
        if (albflg.eq.1) then
          do j=1, jja
          do i=1, ii
!           if (ishlf(i,j).eq.1) then
              swflx(i,j,l0)=swflx(i,j,l0)*(1.0-0.09) !NAVGEM albedo
!           else
!             swflx(i,j,l0)=0.0
!           endif
          enddo
          enddo
        else !albflg.eq.2
          degtorad=4.d0*atan(1.d0)/180.d0
          do j=1, jja
          do i=1, ii
!           latitudinally-varying ocean albedo (Large and Yeager, 2009)
!           5.8% at the equator and 8% at the poles
            albw=(0.069-0.011*cos(2.0*degtorad*plat(i,j)))
!           if (ishlf(i,j).eq.1) then
              swflx(i,j,l0)=swflx(i,j,l0)*(1.0-albw)
!           else
!             swflx(i,j,l0)=0.0
!           endif
          enddo
          enddo
        endif !albflg
      endif
#if defined(ARCTIC)
      call xctila(swflx(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import lw flux: w m-2
    elseif (fieldName.eq.'lwflx_net') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
!         canonical unit conversion: lwflx_net (upward) -> (downward)
          lwflx(i,j,l0)=impData(i+i0,j+j0)*(-1.)
        else
          lwflx(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(lwflx(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import downward lw flux: w m-2
!   +ve into ocean
    elseif ((fieldName.eq.'lwflx_net2down').or. &
            (fieldName.eq.'lwflxd')) then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          lwflx(i,j,l0)=impData(i+i0,j+j0)
        else
          lwflx(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(lwflx(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import precip: m s-1
    elseif (fieldName.eq.'prcp') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
!         canonical unit conversion: prcp (kg_m-2_s-1)-> m_s-1
          precip(i,j,l0)=impData(i+i0,j+j0)*(0.001)
        else
          precip(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(precip(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
!   -----------------
!   import surface temperature
    elseif (fieldName.eq.'gt') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
!         canonical unit conversion: gt (K) -> (C)
          surtmp(i,j,l0)=impData(i+i0,j+j0)-273.15
        else
          surtmp(i,j,l0)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(surtmp(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
      if (sstflg.ne.3) then !use atmos sst as "truth"
        do j=1, jja
        do i=1, ii
          seatmp(i,j,l0)=max(sstmin,min(surtmp(i,j,l0),sstmax))
        enddo
        enddo
#if defined(ARCTIC)
        call xctila(seatmp(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
      endif
!   -----------------
!   import sea level pressure anomaly: Pa
    elseif (fieldName.eq.'mslprs') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          mslprs(i,j,l0)=impData(i+i0,j+j0)
        else
          mslprs(i,j,l0)=1000.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(mslprs(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
      call xctilr(mslprs(1-nbdy,1-nbdy,l0),1,1, nbdy,nbdy, halo_ps)
!   ---------------------
!    import from sea ice
!   ---------------------
!   import ice concentration
    elseif (fieldName.eq.'sic') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          sic_import(i,j)=impData(i+i0,j+j0)
        else
          sic_import(i,j)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(sic_import(1-nbdy,1-nbdy),1,1,halo_ps)
#endif
!   ---------------------
!   import ice x-stress
    elseif (fieldName.eq.'sitx') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          sitx_import(i,j)=impData(i+i0,j+j0)
        else
          sitx_import(i,j)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(sitx_import(1-nbdy,1-nbdy),1,1,halo_pv)
#endif
!   ---------------------
!   import ice y-stress
    elseif (fieldName.eq.'sity') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          sity_import(i,j)=impData(i+i0,j+j0)
        else
          sity_import(i,j)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(sity_import(1-nbdy,1-nbdy),1,1,halo_pv)
#endif
!   ---------------------
!   import solar thru grid cell ave.
    elseif (fieldName.eq.'siqs') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          siqs_import(i,j)=impData(i+i0,j+j0)
        else
          siqs_import(i,j)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(siqs_import(1-nbdy,1-nbdy),1,1,halo_ps)
#endif
!   ---------------------
!   import freeze, melt, H. Flux
    elseif (fieldName.eq.'sifh') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          sifh_import(i,j)=impData(i+i0,j+j0)
        else
          sifh_import(i,j)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(sifh_import(1-nbdy,1-nbdy),1,1,halo_ps)
#endif
!   ---------------------
!   import salt flux
    elseif (fieldName.eq.'sifs') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          sifs_import(i,j)=impData(i+i0,j+j0)
        else
          sifs_import(i,j)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(sifs_import(1-nbdy,1-nbdy),1,1,halo_ps)
#endif
!   ---------------------
!   import water flux
    elseif (fieldName.eq.'sifw') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          sifw_import(i,j)=impData(i+i0,j+j0)
        else
          sifw_import(i,j)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(sifw_import(1-nbdy,1-nbdy),1,1,halo_ps)
#endif
!   ---------------------
!   import sea ice temperature
    elseif (fieldName.eq.'sit_sfc') then
#ifndef ESPC_NOCANONICAL_CONVERT
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
!         canonical unit conversion: sit_sfc (K) -> (C)
          sit_import(i,j)=impData(i+i0,j+j0)-273.15
        else
          sit_import(i,j)=0.0
        endif
      enddo
      enddo
#else
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          sit_import(i,j)=impData(i+i0,j+j0)
        else
          sit_import(i,j)=0.0
        endif
      enddo
      enddo
#endif
#if defined(ARCTIC)
      call xctila(sit_import(1-nbdy,1-nbdy),1,1,halo_ps)
#endif
!   ---------------------
!   import sea ice thickness
    elseif (fieldName.eq.'sih') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          sih_import(i,j)=impData(i+i0,j+j0)
        else
          sih_import(i,j)=0.0
        endif
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(sih_import(1-nbdy,1-nbdy),1,1,halo_ps)
#endif
!   ---------------------
!   import sea ice x-velocity
    elseif (fieldName.eq.'siu') then
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          siu_import(i,j)=impData(i+i0,j+j0)
        else
          siu_import(i,j)=0.0
        endif
      enddo
      enddo
!   ---------------------
!   import sea ice y-velocity
    elseif (fieldName.eq.'siv') then
#ifndef ESPC_NOCANONICAL_CONVERT
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          siv_import(i,j)=impData(i+i0,j+j0)
        else
          siv_import(i,j)=0.0
        endif
        uij=siu_import(i,j)
        vij=siv_import(i,j)
!       rotate to (x,y)ward
        siu_import(i,j)=cos(pang(i,j))*uij + sin(pang(i,j))*vij
        siv_import(i,j)=cos(pang(i,j))*vij - sin(pang(i,j))*uij
      enddo
      enddo
#else
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then
          siv_import(i,j)=impData(i+i0,j+j0)
        else
          siv_import(i,j)=0.0
        endif
#endif
#if defined(ARCTIC)
      call xctila(siu_import(1-nbdy,1-nbdy),1,1,halo_pv)
      call xctila(siv_import(1-nbdy,1-nbdy),1,1,halo_pv)
#endif
    else ! field unknown error
      if (mnproc.eq.1) print *, "error - fieldName unknown: "//trim(fieldName)
      call xcstop('('//rname//')')
      stop '('//rname//')'
    endif !fieldName

!   diagnostic output
    if (show_minmax) then
!     allocate memory
      if (mnproc.eq.1) then
        allocate(ocn_msk(itdm,jtdm))
        allocate(field_tmp(itdm,jtdm))
      else
        allocate(ocn_msk(1,1))
        allocate(field_tmp(1,1))
      endif
      allocate(tmx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy))

!     ocn_msk: sea/land mask
      tmx(:,:)=0.0
      do j=1, jja
      do i=1, ii
        tmx(i,j)=ishlf(i,j)
!x      tmx(i,j)=ip(i,j)
      enddo
      enddo
      call xcaget(ocn_msk,tmx,1)
!     call xcsync(no_flush)

!     field_tmp: import data
      tmx(:,:)=0.0
      do j=1, jja
      do i=1, ii
!       tmx(i,j)=mgrid(i,j)
        tmx(i,j)=impData(i+i0,j+j0)
      enddo
      enddo
      call xcaget(field_tmp,tmx,1)
!      call mpi_barrier(mpi_comm_hycom,ierr)
!      call xcsync(no_flush)

!     write minmax to stdout
      if (mnproc.eq.1) then
        write(*,992) trim(fieldName),          &
          maxval(field_tmp,mask=ocn_msk.eq.1), &
          minval(field_tmp,mask=ocn_msk.eq.1), &
          (sum(field_tmp,mask=ocn_msk.eq.1)/count(ocn_msk.eq.1))
 992    format('import_to_hycom_deb,max,min,mean=',A10,3E23.15)
      endif

!     deallocate memory
      if (allocated(ocn_msk)) deallocate(ocn_msk)
      if (allocated(field_tmp)) deallocate(field_tmp)
      if (allocated(tmx)) deallocate(tmx)
    endif !show_minmax

    if (mnproc.eq.1) print *, rname//" end..."

  end subroutine import_to_hycom_deb

  !-----------------------------------------------------------------------------

  subroutine ocn_import_forcing(rc)
!   arguments
    integer, intent(out) :: rc
!   local variables
    character(*), parameter :: rname="ocn_import_forcing"
    integer                 :: i, j, m, n, jja

    rc = 0 ! success

    if (mnproc.eq.1) print *, rname//" start..."

#if defined(ARCTIC)
!   arctic (tripole) domain, top row is replicated (ignore it)
    jja=min(jj,(jtdm-1-j0))
#else
    jja=jj
#endif

!   calculate radflx
    if (lwflag.eq.2) then
      do j=1, jja
      do i=1, ii
!       radflx is defined as net lwflx+swflx, +ve into ocean
        radflx(i,j,l0)=lwflx(i,j,l0)+swflx(i,j,l0)
      enddo
      enddo
#if defined(ARCTIC)
      call xctila(radflx(1-nbdy,1-nbdy,l0),1,1,halo_ps)
#endif
    else
      if (mnproc.eq.1) print *,"error - lwflag .ne. 2"
      call xcstop('('//rname//')')
             stop '('//rname//')'
    endif

!   check CICE feedback
    if ((.not.cpl_sic) .or.(.not.cpl_sitx).or.(.not.cpl_sity).or. &
        (.not.cpl_siqs).or.(.not.cpl_sifh).or.(.not.cpl_sifs).or. &
        (.not.cpl_sifw).or.(.not.cpl_sit) .or.(.not.cpl_sih) .or. &
        (.not.cpl_siu) .or.(.not.cpl_siv)) then
      if (mnproc.eq.1) print *, &
        'warning... no feedback from CICE to HYCOM('//rname//')'
    else
!     copy import data to sea ice
      do j=1, jja
      do i=1, ii
        if (ishlf(i,j).eq.1) then !standard ocean point
          if ((iceflg.ge.2).and.(icmflg.ne.3)) then
            covice(i,j)=sic_import(i,j) !Sea Ice Concentration
            si_c(i,j)=sic_import(i,j) !Sea Ice Concentration
            if (covice(i,j).gt.0.0) then
               si_tx(i,j) = -sitx_import(i,j) !Sea Ice X-Stress into ocean
               si_ty(i,j) = -sity_import(i,j) !Sea Ice Y-Stress into ocean
              fswice(i,j) =  siqs_import(i,j) !Solar Heat Flux thru Ice to Ocean
              flxice(i,j) =  fswice(i,j) + &
                             sifh_import(i,j) !Ice Freezing/Melting Heat Flux
              sflice(i,j) =  sifs_import(i,j)*1.e3 !Ice Salt Flux
              wflice(i,j) =  sifw_import(i,j) !Ice freshwater Flux
              temice(i,j) =   sit_import(i,j) !Sea Ice Temperature
                si_t(i,j) =   sit_import(i,j) !Sea Ice Temperature
              thkice(i,j) =   sih_import(i,j) !Sea Ice Thickness
                si_h(i,j) =   sih_import(i,j) !Sea Ice Thickness
                si_u(i,j) =   siu_import(i,j) !Sea Ice X-Velocity
                si_v(i,j) =   siv_import(i,j) !Sea Ice Y-Velocity
            else
               si_tx(i,j) = 0.0
               si_ty(i,j) = 0.0
              fswice(i,j) = 0.0
              flxice(i,j) = 0.0
              sflice(i,j) = 0.0
              wflice(i,j) = 0.0
              temice(i,j) = 0.0
                si_t(i,j) = 0.0
              thkice(i,j) = 0.0
                si_h(i,j) = 0.0
                si_u(i,j) = 0.0
                si_v(i,j) = 0.0
            endif !covice
          elseif ((iceflg.ge.2).and.(icmflg.eq.3)) then
            si_c(i,j)=sic_import(i,j) !Sea Ice Concentration
            if (si_c(i,j).gt.0.0) then
              si_tx(i,j) = -sitx_import(i,j) !Sea Ice X-Stress into ocean
              si_ty(i,j) = -sity_import(i,j) !Sea Ice Y-Stress into ocean
               si_h(i,j) =   sih_import(i,j) !Sea Ice Thickness
               si_t(i,j) =   sit_import(i,j) !Sea Ice Temperature
               si_u(i,j) =   siu_import(i,j) !Sea Ice X-Velocity
               si_v(i,j) =   siv_import(i,j) !Sea Ice Y-Velocity
            else
              si_tx(i,j) = 0.0
              si_ty(i,j) = 0.0
               si_h(i,j) = 0.0
               si_t(i,j) = 0.0
               si_u(i,j) = 0.0
               si_v(i,j) = 0.0
            endif !covice
          endif !iceflg>=2 (icmflg)
        endif !ishlf
      enddo
      enddo

#if defined(ARCTIC)
!     update last active row of array
!jcx  call xctila( sic_import,1,1,halo_ps) !Sea Ice Concentration
!jcx  call xctila(sitx_import,1,1,halo_pv) !Sea Ice X-Stress
!jcx  call xctila(sity_import,1,1,halo_pv) !Sea Ice Y-Stress
!jcx  call xctila(siqs_import,1,1,halo_ps) !Solar Heat Flux thru Ice to Ocean
!jcx  call xctila(sifh_import,1,1,halo_ps) !Ice Freezing/Melting Heat Flux
!jcx  call xctila(sifs_import,1,1,halo_ps) !Ice Freezing/Melting Salt Flux
!jcx  call xctila(sifw_import,1,1,halo_ps) !Ice Net Water Flux
!jcx  call xctila( sit_import,1,1,halo_ps) !Sea Ice Temperature
!jcx  call xctila( sih_import,1,1,halo_ps) !Sea Ice Thickness
!jcx  call xctila( siu_import,1,1,halo_pv) !Sea Ice X-Velocity
!jcx  call xctila( siv_import,1,1,halo_pv) !Sea Ice Y-Velocity
      if ((iceflg.ge.2).and.(icmflg.ne.3)) then
        call xctila(covice,1,1,halo_ps) !Sea Ice Concentration
        call xctila(  si_c,1,1,halo_ps) !Sea Ice Concentration
        call xctila( si_tx,1,1,halo_pv) !Sea Ice X-Stress into ocean
        call xctila( si_ty,1,1,halo_pv) !Sea Ice Y-Stress into ocean
        call xctila(fswice,1,1,halo_ps) !Solar Heat Flux thru Ice to Ocean
        call xctila(flxice,1,1,halo_ps) !Ice Freezing/Melting Heat Flux
        call xctila(sflice,1,1,halo_ps) !Ice Salt Flux
        call xctila(wflice,1,1,halo_ps) !Ice Freshwater Flux
        call xctila(temice,1,1,halo_ps) !Sea Ice Temperature
        call xctila(  si_t,1,1,halo_ps) !Sea Ice Temperature
        call xctila(thkice,1,1,halo_ps) !Sea Ice Thickness
        call xctila(  si_h,1,1,halo_ps) !Sea Ice Thickness
        call xctila(  si_u,1,1,halo_pv) !Sea Ice X-Velocity
        call xctila(  si_v,1,1,halo_pv) !Sea Ice Y-Velocity
      elseif ((iceflg.ge.2).and.(icmflg.eq.3)) then
        call xctila(  si_c,1,1,halo_ps) !Sea Ice Concentration
        call xctila( si_tx,1,1,halo_pv) !Sea Ice X-Stress into ocean
        call xctila( si_ty,1,1,halo_pv) !Sea Ice Y-Stress into ocean
        call xctila(  si_h,1,1,halo_ps) !Sea Ice Thickness
        call xctila(  si_t,1,1,halo_ps) !Sea Ice Temperature
        call xctila(  si_u,1,1,halo_pv) !Sea Ice X-Velocity
        call xctila(  si_v,1,1,halo_pv) !Sea Ice Y-Velocity
      endif
#endif

!     smooth sea ice velocity fields
      call psmooth(si_u,0,0,ishlf,util1)
      call psmooth(si_v,0,0,ishlf,util1)
#if defined(ARCTIC)
      call xctila(si_u,1,1,halo_pv)
      call xctila(si_v,1,1,halo_pv)
#endif
!     call xctilr(si_u,1,1, nbdy,nbdy, halo_pv)
!     call xctilr(si_v,1,1, nbdy,nbdy, halo_pv)

!     copy back from si_ to _import for archive_ice
      do j=1, jja
      do i=1, ii
        if (si_c(i,j).gt.0.0) then
          siu_import(i,j)=si_u(i,j) !Sea Ice X-Velocity
          siv_import(i,j)=si_v(i,j) !Sea Ice Y-Velocity
        endif !si_c
      enddo !i
      enddo !j
    endif !feedback from CICE to HYCOM

    if (mnproc.eq.1) print *, rname//" end..."

  end subroutine ocn_import_forcing

  !-----------------------------------------------------------------------------

  subroutine hycom_couple_final(rc)
!   arguments
    integer, intent(out) :: rc
!   local variables
    character(*), parameter :: rname="hycom_couple_final"

    rc = 0 ! success

    if (mnproc.eq.1) print *, rname//" start..."

!   deallocate memory
    if (allocated(cpldom%deBList)) deallocate(cpldom%deBList)
    if (allocated(cpldom%lon_p)) deallocate(cpldom%lon_p)
    if (allocated(cpldom%lat_p)) deallocate(cpldom%lat_p)
    if (allocated(cpldom%area_p)) deallocate(cpldom%area_p)
    if (allocated(cpldom%mask_p)) deallocate(cpldom%mask_p)
    if (allocated(cpldom%lon_q)) deallocate(cpldom%lon_q)
    if (allocated(cpldom%lat_q)) deallocate(cpldom%lat_q)
    if (allocated(cpldom%area_q)) deallocate(cpldom%area_q)
    if (allocated(cpldom%mask_q)) deallocate(cpldom%mask_q)

    if (mnproc.eq.1) print *, rname//" end..."

  end subroutine hycom_couple_final
!===============================================================================
end module hycom_couple
!===============================================================================

