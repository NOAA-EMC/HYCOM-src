!===============================================================================
! MODULE: read_impexp_config_mod
!
! DESCRIPTION:
!   This module creates HYCOM coupling field lists.
!
! SUBROUTINES:
!   read_impexp_config
!     Overloaded interface for configuring field list from file or using
!     defaults.
!
!===============================================================================
#include "HYCOM_NUOPC_Macros.h"
!===============================================================================
module read_impexp_config_mod

!===============================================================================
! use modules
!===============================================================================
  use ESMF
  use NUOPC
  use mod_hycom, only: mediator_type, atm_model_type

!===============================================================================
! settings
!===============================================================================
  implicit none

  private

!===============================================================================
! public
!===============================================================================
  public read_impexp_config
  public set_impexp_fields

!===============================================================================
! module variables
!===============================================================================
  type hycom_fld_type
    sequence
    character(len=30)    :: fieldName   = "dummy"
    character(len=60)    :: standName   = "dummy"
    character(len=30)    :: fieldUnit   = "-"
    real                 :: minValue    = -1.0e10
    real                 :: maxValue    =  1.0e10
    real                 :: fillValue   =  9.9e10
    logical              :: fieldEnable = .TRUE.
  end type hycom_fld_type

  type(hycom_fld_type), target, allocatable :: dfltFldsImp(:)
  type(hycom_fld_type), target, allocatable :: dfltFldsExp(:)

!==============================================================================
! interface blocks
!==============================================================================
  interface read_impexp_config
    module procedure read_impexp_config_dflt
    module procedure read_impexp_config_flnm
    module procedure read_impexp_config_list
  end interface

!===============================================================================
  contains
!===============================================================================
  subroutine set_impexp_fields()

    if (trim(mediator_type) == "cmeps") then
      ! Set import fields
      if (trim(atm_model_type) == "datm") then
        if (.not. allocated(dfltFldsImp)) allocate(dfltFldsImp(11))
        dfltFldsImp = (/ &
          hycom_fld_type("taux10   ","mean_zonal_moment_flx        ","N m-2     "), & !01 Faxa_taux
          hycom_fld_type("tauy10   ","mean_merid_moment_flx        ","N m-2     "), & !02 Faxa_tauy
          hycom_fld_type("airtmp   ","inst_temp_height_lowest      ","K         "), & !03 Sa_tbot
          hycom_fld_type("airhum   ","inst_spec_humid_height_lowest","kg kg-1   "), & !04 Sa_shum
          hycom_fld_type("prcp     ","mean_prec_rate               ","kg m-2 s-1"), & !05 Faxa_rain
          hycom_fld_type("mslprs   ","inst_pres_height_surface     ","Pa        "), & !06 Sa_pslv
          hycom_fld_type("swflx_net","mean_net_sw_flx              ","W m-2     "), & !07 Foxx_swnet
          hycom_fld_type("lwflx_net","mean_net_lw_flx              ","W m-2     "), & !08 Foxx_lwnet
          hycom_fld_type("wndspd10 ","inst_wind_speed_height_lowest","m s-1     "), & !09 Sa_wspd
          hycom_fld_type("gt       ","inst_temp_skin_temperature   ","K         "), & !10 Sa_tskn
          hycom_fld_type("sic      ","ice_fraction                 ","1         ")  & !11 Si_ifrac
        /)
      else
        if (.not. allocated(dfltFldsImp)) allocate(dfltFldsImp(13))
        dfltFldsImp = (/ &
          hycom_fld_type("u10    ","inst_zonal_wind_height_lowest","m s-1     "), & !01 Sa_u
          hycom_fld_type("v10    ","inst_merid_wind_height_lowest","m s-1     "), & !02 Sa_v
          hycom_fld_type("taux10 ","mean_zonal_moment_flx        ","N m-2     "), & !03 Faxa_taux
          hycom_fld_type("tauy10 ","mean_merid_moment_flx        ","N m-2     "), & !04 Faxa_tauy
          hycom_fld_type("airtmp ","inst_temp_height_lowest      ","K         "), & !05 Sa_tbot
          hycom_fld_type("airhum ","inst_spec_humid_height_lowest","kg kg-1   "), & !06 Sa_shum
          hycom_fld_type("prcp   ","mean_prec_rate               ","kg m-2 s-1"), & !07 Faxa_rain
          hycom_fld_type("swflxd ","mean_net_sw_flx              ","W m-2     "), & !08 Foxx_swnet
          hycom_fld_type("lwflxd ","mean_net_lw_flx              ","W m-2     "), & !09 Foxx_lwnet
          hycom_fld_type("mslprs ","inst_pres_height_surface     ","Pa        "), & !10 Sa_pslv
          hycom_fld_type("gt     ","inst_temp_skin_temperature   ","K         "), & !11 Sa_tskn
          hycom_fld_type("sensflx","mean_sensi_heat_flx          ","W m-2     "), & !12 Foxx_sen
          hycom_fld_type("latflx ","mean_laten_heat_flx          ","W m-2     ")  & !13 Foxx_lat
        /)
      end if
      ! Set export fields
      if (.not. allocated(dfltFldsExp)) allocate(dfltFldsExp(5))
      dfltFldsExp = (/ &
        hycom_fld_type("sst        ","sea_surface_temperature","K"), & !01 So_t
        hycom_fld_type("mask       ","ocean_mask             ","1"), & !02 So_omask
        hycom_fld_type("cpl_scalars","cpl_scalars            ","1"), & !03 cpl_scalars
        hycom_fld_type("ssu        ","ocn_current_zonal      ","1"), & !04 So_u
        hycom_fld_type("ssv        ","ocn_current_merid      ","1")  & !05 So_v
      /)
    else
      ! Set import fields
      if (.not. allocated(dfltFldsImp)) allocate(dfltFldsImp(13))
      dfltFldsImp = (/ &
        hycom_fld_type("u10    ","inst_zonal_wind_height10m","m s-1     "), & !01
        hycom_fld_type("v10    ","inst_merid_wind_height10m","m s-1     "), & !02
        hycom_fld_type("taux10 ","mean_zonal_moment_flx_atm","N m-2     "), & !03
        hycom_fld_type("tauy10 ","mean_merid_moment_flx_atm","N m-2     "), & !04
        hycom_fld_type("airtmp ","inst_temp_height2m       ","K         "), & !05
        hycom_fld_type("airhum ","inst_spec_humid_height2m ","kg kg-1   "), & !06
        hycom_fld_type("prcp   ","mean_prec_rate           ","kg m-2 s-1"), & !07
        hycom_fld_type("swflxd ","mean_net_sw_flx          ","W m-2     "), & !08
        hycom_fld_type("lwflxd ","mean_net_lw_flx          ","W m-2     "), & !09
        hycom_fld_type("mslprs ","inst_pres_height_surface ","Pa        "), & !10
        hycom_fld_type("gt     ","inst_temp_height_surface ","K         "), & !11
        hycom_fld_type("sensflx","mean_sensi_heat_flx      ","W m-2     "), & !12
        hycom_fld_type("latflx ","mean_laten_heat_flx      ","W m-2     ")  & !13
      /)
      ! Set export fields
      if (.not. allocated(dfltFldsExp)) allocate(dfltFldsExp(2))
      dfltFldsExp = (/ &
        hycom_fld_type("sst ","sea_surface_temperature","K"), & !01
        hycom_fld_type("mask","ocean_mask             ","1")  & !02
      /)
    end if
  end subroutine set_impexp_fields

  subroutine read_impexp_config_dflt(numExpFields,numImpFields, &
  expFieldName,impFieldName,expStandName,impStandName, &
  expFieldUnit,impFieldUnit,expFieldEnable,impFieldEnable,rc)
    integer,intent(out)                   :: numImpFields,numExpFields
    character(len=30),pointer,intent(out) :: impFieldName(:),expFieldName(:)
    character(len=60),pointer,intent(out) :: impStandName(:),expStandName(:)
    character(len=30),pointer,intent(out) :: impFieldUnit(:),expFieldUnit(:)
    logical,pointer,intent(out)           :: impFieldEnable(:),expFieldEnable(:)
    integer,intent(out)                   :: rc                ! return code

    rc = ESMF_SUCCESS

    call read_impexp_config_list(dfltFldsExp,dfltFldsImp,numExpFields,numImpFields, &
      expFieldName,impFieldName,expStandName,impStandName, &
      expFieldUnit,impFieldUnit,expFieldEnable,impFieldEnable,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="read_impexp_config failed", CONTEXT)) return

  end subroutine read_impexp_config_dflt

  !-----------------------------------------------------------------------------

  subroutine read_impexp_config_flnm(fname,numExpFields,numImpFields, &
  expFieldName,impFieldName,expStandName,impStandName, &
  expFieldUnit,impFieldUnit,expFieldEnable,impFieldEnable,rc)
    character(len=30),intent(in)          :: fname
    integer,intent(out)                   :: numImpFields,numExpFields
    character(len=30),pointer,intent(out) :: impFieldName(:),expFieldName(:)
    character(len=60),pointer,intent(out) :: impStandName(:),expStandName(:)
    character(len=30),pointer,intent(out) :: impFieldUnit(:),expFieldUnit(:)
    logical,pointer,intent(out)           :: impFieldEnable(:),expFieldEnable(:)
    integer,intent(out)                   :: rc                ! return code

    ! local variables
    type(ESMF_Config)                  :: fieldsConfig
    type(NUOPC_FreeFormat)             :: attrFF
    integer                            :: lineCount
    integer                            :: tokenCount
    type(hycom_fld_type),allocatable   :: fldsExp(:)
    type(hycom_fld_type),allocatable   :: fldsImp(:)
    character(len=NUOPC_FreeFormatLen),allocatable :: tokenList(:)
    integer                            :: i,j
    integer                            :: stat

    rc = ESMF_SUCCESS

!   load fname into fieldsConfig
    fieldsConfig = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
    call ESMF_ConfigLoadFile(fieldsConfig, fname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return

!   read export fields from config
    attrFF = NUOPC_FreeFormatCreate(fieldsConfig, &
      label="ocn_export_fields", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
    call NUOPC_FreeFormatGet(attrFF, lineCount=lineCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
    allocate(fldsExp(lineCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of fldsExp memory failed.", &
      line=__LINE__,file=__FILE__)) &
      return  ! bail out
    do i=1, lineCount
      call NUOPC_FreeFormatGetLine(attrFF, line=i, &
        tokenCount=tokenCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      if (.not.((tokenCount.eq.3).or.(tokenCount.eq.6))) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="Malformed ocn_export_fields item FORMAT="// &
            "'HYCOM_NAME' 'STANDARD_NAME' 'UNITS' "// &
!            "['MINVAL' 'MAXVAL' 'FILLVAL'] "// &
            "in file: "//trim(fname), &
          CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(tokenList(tokenCount))
      call NUOPC_FreeFormatGetLine(attrFF, line=i, tokenList=tokenList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      fldsExp(i)%fieldName=tokenList(1)
      fldsExp(i)%standName=tokenList(2)
      fldsExp(i)%fieldUnit=tokenList(3)
      if (tokenCount.eq.6) then
        fldsExp(i)%minValue = ESMF_UtilString2Real(tokenList(4), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
        fldsExp(i)%maxValue = ESMF_UtilString2Real(tokenList(5), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
        fldsExp(i)%fillValue = ESMF_UtilString2Real(tokenList(6), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      endif
      fldsExp(i)%fieldEnable=.true.
      deallocate(tokenList)
    enddo
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return

!   read import fields from config
    attrFF = NUOPC_FreeFormatCreate(fieldsConfig, &
      label="ocn_import_fields", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
    call NUOPC_FreeFormatGet(attrFF, lineCount=lineCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
    allocate(fldsImp(lineCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of fldsImp memory failed.", &
      line=__LINE__,file=__FILE__)) &
      return  ! bail out
    do i=1,lineCount
      call NUOPC_FreeFormatGetLine(attrFF, line=i, &
        tokenCount=tokenCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      if (.not.((tokenCount.eq.3).or.(tokenCount.eq.6))) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="Malformed ocn_import_fields FORMAT="// &
            "'HYCOM_NAME' 'STANDARD_NAME' 'UNITS' "// &
!            "['MINVAL' 'MAXVAL' 'FILLVAL'] "// &
            "in file: "//trim(fname), &
          CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(tokenList(tokenCount))
      call NUOPC_FreeFormatGetLine(attrFF, line=i, tokenList=tokenList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      fldsImp(i)%fieldName=tokenList(1)
      fldsImp(i)%standName=tokenList(2)
      fldsImp(i)%fieldUnit=tokenList(3)
      if (tokenCount.eq.6) then
        fldsImp(i)%minValue = ESMF_UtilString2Real(tokenList(4), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
        fldsImp(i)%maxValue = ESMF_UtilString2Real(tokenList(5), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
        fldsImp(i)%fillValue = ESMF_UtilString2Real(tokenList(6), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      endif
      fldsImp(i)%fieldEnable=.true.
      deallocate(tokenList)
    enddo
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return

!   create field lists
    call read_impexp_config_list(fldsExp,fldsImp,numExpFields,numImpFields, &
      expFieldName,impFieldName,expStandName,impStandName, &
      expFieldUnit,impFieldUnit,expFieldEnable,impFieldEnable,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return

!   cleanup
    deallocate(fldsExp, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of fldsExp memory failed.", &
      line=__LINE__,file=__FILE__)) &
      return  ! bail out
    deallocate(fldsImp, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of fldsImp memory failed.", &
      line=__LINE__,file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigDestroy(fieldsConfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return

  end subroutine read_impexp_config_flnm

  !-----------------------------------------------------------------------------

  subroutine read_impexp_config_list(fldsExp,fldsImp,numExpFields,&
  numImpFields,expFieldName,impFieldName,expStandName,impStandName, &
  expFieldUnit,impFieldUnit,expFieldEnable,impFieldEnable,rc)
    type(hycom_fld_type),intent(in)       :: fldsExp(:)
    type(hycom_fld_type),intent(in)       :: fldsImp(:)
    integer,intent(out)                   :: numImpFields,numExpFields
    character(len=30),pointer,intent(out) :: impFieldName(:),expFieldName(:)
    character(len=60),pointer,intent(out) :: impStandName(:),expStandName(:)
    character(len=30),pointer,intent(out) :: impFieldUnit(:),expFieldUnit(:)
    logical,pointer,intent(out)           :: impFieldEnable(:),expFieldEnable(:)
    integer,intent(out)                   :: rc                ! return code

    ! local variables
    character(len=30),parameter :: cname = "OCN"
    integer :: i
    integer :: stat
    integer :: enCnt
    character(ESMF_MAXSTR) :: logMsg

    rc = ESMF_SUCCESS

    numImpFields=size(fldsImp)
    allocate( &
      impFieldName(numImpFields), &
      impStandName(numImpFields), &
      impFieldUnit(numImpFields), &
      impFieldEnable(numImpFields), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of impField memory failed.", &
      line=__LINE__,file=__FILE__)) &
      return  ! bail out

    enCnt=0
    do i=1,numImpFields
      impFieldName(i)=fldsImp(i)%fieldName
      impStandName(i)=fldsImp(i)%standName
      impFieldUnit(i)=fldsImp(i)%fieldUnit
      impFieldEnable(i)=fldsImp(i)%fieldEnable
      if (impFieldEnable(i)) enCnt=enCnt+1
    enddo

    ! Report enabled import fields
    write(logMsg,'(a,a,i0,a)') TRIM(cname)//': ', &
      'List of enabled import fields(',enCnt,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(cname)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    enCnt = 0
    do i=1, numImpFields
      if (.NOT.impFieldEnable(i)) cycle
      enCnt = enCnt + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(cname)//': ', &
        enCnt,' ',TRIM(impFieldName(i)), &
        ' ',TRIM(impStandName(i))
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

    numExpFields=size(fldsExp)
    allocate( &
      expFieldName(numExpFields), &
      expStandName(numExpFields), &
      expFieldUnit(numExpFields), &
      expFieldEnable(numExpFields), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of expField memory failed.", &
      line=__LINE__,file=__FILE__)) &
      return  ! bail out

    enCnt=0
    do i=1,numExpFields
      expFieldName(i)=fldsExp(i)%fieldName
      expStandName(i)=fldsExp(i)%standName
      expFieldUnit(i)=fldsExp(i)%fieldUnit
      expFieldEnable(i)=fldsExp(i)%fieldEnable
      if (expFieldEnable(i)) enCnt=enCnt+1
    enddo

    ! Report enabled export fields
    write(logMsg,'(a,a,i0,a)') TRIM(cname)//': ', &
      'List of enabled export fields(',enCnt,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(cname)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    enCnt = 0
    do i=1, numExpFields
      if (.NOT.expFieldEnable(i)) cycle
      enCnt = enCnt + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(cname)//': ', &
        enCnt,' ',TRIM(expFieldName(i)), &
        ' ',TRIM(expStandName(i))
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine read_impexp_config_list

  !-----------------------------------------------------------------------------

!===============================================================================
end module read_impexp_config_mod
!===============================================================================
