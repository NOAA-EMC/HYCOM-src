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

!===============================================================================
! settings
!===============================================================================
  implicit none

  private

!===============================================================================
! public
!===============================================================================
  public read_impexp_config

!===============================================================================
! module variables
!===============================================================================
  type fieldRemapFlag
    sequence
    private
      integer :: remap
  end type

  type(fieldRemapFlag), parameter ::       &
    FLD_REMAP_ERROR  = fieldRemapFlag(-1), &
    FLD_REMAP_UNKOWN = fieldRemapFlag(0),  &
    FLD_REMAP_REDIST = fieldRemapFlag(1),  &
    FLD_REMAP_BILINR = fieldRemapFlag(2),  &
    FLD_REMAP_CONSRV = fieldRemapFlag(3)

  type fieldMaskFlag
    sequence
    private
      integer :: mask
  end type

  type(fieldMaskFlag), parameter ::   &
    FLD_MASK_ERR = fieldMaskFlag(-1), &
    FLD_MASK_UNK = fieldMaskFlag(0),  &
    FLD_MASK_NNE = fieldMaskFlag(1),  &
    FLD_MASK_LND = fieldMaskFlag(2),  &
    FLD_MASK_WTR = fieldMaskFlag(3)

  type hycom_fld_type
    sequence
    character(len=30)    :: fieldName   = "dummy"
    character(len=60)    :: standName   = "dummy"
    character(len=30)    :: fieldUnit   = "-"
    logical              :: fieldEnable = .FALSE.
    type(fieldRemapFlag) :: mapping     = FLD_REMAP_REDIST
    type(fieldMaskFlag)  :: mask        = FLD_MASK_NNE
    real                 :: fillValue   = 999999999.0
  end type hycom_fld_type

#ifdef CMEPS
  type(hycom_fld_type),target,dimension(8) :: dfltFldsImp = (/              &
    hycom_fld_type("u10","Sa_u",                                        & !01
                   "m s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),     &
    hycom_fld_type("v10","Sa_v",                                        & !02
                   "m s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),     &
    hycom_fld_type("airtmp","Sa_tbot",                                  & !03
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),         &
    hycom_fld_type("airhum","Sa_shum",                                  & !04
                   "kg kg-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),   &
    hycom_fld_type("swflxd","Faxa_swnet",                               & !05
                   "W m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),     &
    hycom_fld_type("lwflxd","Faxa_lwdn",                                & !06
                   "W m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),     &
    hycom_fld_type("prcp","Faxa_rainl",                                 & !07
                   "kg m-2 s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("mslprs","Sa_pbot",                                  & !08
                   "Pa",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)         &
  /)
#else
  type(hycom_fld_type),target,dimension(11) :: dfltFldsImp = (/&
    hycom_fld_type("u10","inst_zonal_wind_height10m",&                    !01
                   "m s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("v10","inst_merid_wind_height10m",&                    !02
                   "m s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("taux10","mean_zonal_moment_flx_atm",&                 !03
                   "N m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("tauy10","mean_merid_moment_flx_atm",&                 !04
                   "N m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("airtmp","inst_temp_height2m",&                        !05
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("airhum","inst_spec_humid_height2m",&                  !06
                   "kg kg-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("prcp","mean_prec_rate",&                              !07
                   "kg m-2 s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("swflxd","mean_net_sw_flx",&                           !08
                   "W m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("lwflxd","mean_net_lw_flx",&                           !09
                   "W m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("mslprs","inst_pres_height_surface",&                  !10
                   "Pa",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("gt","inst_temp_height_surface",&                      !11
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)/)
#endif

#ifdef CMEPS
  type(hycom_fld_type),target,dimension(1) :: dfltFldsExp = (/              &
    hycom_fld_type("sst","So_t",                                        & !01
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)          &
  /)
#else
  type(hycom_fld_type),target,dimension(1) :: dfltFldsExp = (/&
    hycom_fld_type("sst","sea_surface_temperature",&                         !01
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)/)
#endif

!==============================================================================
! interface blocks
!==============================================================================
  interface read_impexp_config
    module procedure read_impexp_config_dflt
    module procedure read_impexp_config_flnm
    module procedure read_impexp_config_list
  end interface

  interface operator (==)
    module procedure field_rfeq
    module procedure field_mfeq
  end interface

  interface assignment (=)
    module procedure field_rfas_string
    module procedure field_mfas_string
    module procedure field_stringas_rf
    module procedure field_stringas_mf
  end interface

!===============================================================================
  contains
!===============================================================================
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
    character(len=NUOPC_FreeFormatLen) :: tokenList(7)
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
      if (tokenCount.ne.7) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="Malformed field list item FORMAT="// &
            "'NAME' 'STANDARD_NAME' 'UNITS' 'ENABLED' "// &
            "'MAPPING_TYPE' 'MASKING_FLAG' 'FILL_VALUE' in "//trim(fname), &
          CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      call NUOPC_FreeFormatGetLine(attrFF, line=i, tokenList=tokenList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      fldsExp(i)%fieldName=tokenList(1)
      fldsExp(i)%standName=tokenList(2)
      fldsExp(i)%fieldUnit=tokenList(3)
      tokenList(4) = ESMF_UtilStringUpperCase(tokenList(4), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      fldsExp(i)%fieldEnable=(tokenList(4).eq.".TRUE.")
      fldsExp(i)%mapping=tokenList(5)
      fldsExp(i)%mask=tokenList(6)
      fldsExp(i)%fillValue = ESMF_UtilString2Real(tokenList(7), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
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
      if (tokenCount.ne.7) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="Malformed field list item FORMAT="// &
            "'NAME' 'STANDARD_NAME' 'UNITS' 'ENABLED' "// &
            "'MAPPING_TYPE' 'MASKING_FLAG' 'FILL_VALUE' in "//trim(fname), &
          CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      call NUOPC_FreeFormatGetLine(attrFF, line=i, tokenList=tokenList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      fldsImp(i)%fieldName=tokenList(1)
      fldsImp(i)%standName=tokenList(2)
      fldsImp(i)%fieldUnit=tokenList(3)
      tokenList(4) = ESMF_UtilStringUpperCase(tokenList(4), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
      fldsImp(i)%fieldEnable=(tokenList(4).eq.".TRUE.")
      fldsImp(i)%mapping=tokenList(5)
      fldsImp(i)%mask=tokenList(6)
      fldsImp(i)%fillValue = ESMF_UtilString2Real(tokenList(7), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, CONTEXT)) return
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

  function field_rfeq(rf1, rf2)
    logical field_rfeq
    type(fieldRemapFlag), intent(in) :: rf1, rf2
    field_rfeq = (rf1%remap == rf2%remap)
  end function field_rfeq

  !-----------------------------------------------------------------------------

  subroutine field_rfas_string(string, rfval)
    character(len=*), intent(out) :: string
    type(fieldRemapFlag), intent(in) :: rfval
    if (rfval == FLD_REMAP_UNKOWN) then
      write(string,'(a)') 'FLD_REMAP_UNKOWN'
    elseif (rfval == FLD_REMAP_REDIST) then
      write(string,'(a)') 'FLD_REMAP_REDIST'
    elseif (rfval == FLD_REMAP_BILINR) then
      write(string,'(a)') 'FLD_REMAP_BILINR'
    elseif (rfval == FLD_REMAP_CONSRV) then
      write(string,'(a)') 'FLD_REMAP_CONSRV'
    else
      write(string,'(a)') 'FLD_REMAP_ERROR'
    endif
  end subroutine field_rfas_string

  !-----------------------------------------------------------------------------

  subroutine field_stringas_rf(rfval, string)
    type(fieldRemapFlag), intent(out) :: rfval
    character(len=*), intent(in) :: string
    if (string .eq. 'FLD_REMAP_UNKOWN') then
      rfval = FLD_REMAP_UNKOWN
    elseif (string .eq. 'FLD_REMAP_REDIST') then
      rfval = FLD_REMAP_REDIST
    elseif (string .eq.'FLD_REMAP_BILINR') then
      rfval = FLD_REMAP_BILINR
    elseif (string .eq. 'FLD_REMAP_CONSRV') then
      rfval = FLD_REMAP_CONSRV
    else
      rfval = FLD_REMAP_ERROR
    endif
  end subroutine field_stringas_rf

  !-----------------------------------------------------------------------------

  function field_mfeq(mf1, mf2)
    logical field_mfeq
    type(fieldMaskFlag), intent(in) :: mf1, mf2
    field_mfeq = (mf1%mask == mf2%mask)
  end function field_mfeq

  !-----------------------------------------------------------------------------

  subroutine field_mfas_string(string, mfval)
    character(len=*), intent(out) :: string
    type(fieldMaskFlag), intent(in) :: mfval
    if (mfval == FLD_MASK_UNK) then
      write(string,'(a)') 'FLD_MASK_UNK'
    elseif (mfval == FLD_MASK_NNE) then
      write(string,'(a)') 'FLD_MASK_NNE'
    elseif (mfval == FLD_MASK_LND) then
      write(string,'(a)') 'FLD_MASK_LND'
    elseif (mfval == FLD_MASK_WTR) then
      write(string,'(a)') 'FLD_MASK_WTR'
    else
      write(string,'(a)') 'FLD_MASK_ERR'
    endif
  end subroutine field_mfas_string

  !-----------------------------------------------------------------------------

  subroutine field_stringas_mf(mfval,string)
    type(fieldMaskFlag), intent(out) :: mfval
    character(len=*), intent(in) :: string
    if (string .eq. 'FLD_MASK_UNK') then
      mfval = FLD_MASK_UNK
    elseif (string .eq. 'FLD_MASK_NNE') then
      mfval = FLD_MASK_NNE
    elseif (string .eq. 'FLD_MASK_LND') then
      mfval = FLD_MASK_LND
    elseif (string .eq. 'FLD_MASK_WTR') then
      mfval = FLD_MASK_WTR
    else
      mfval = FLD_MASK_ERR
    endif
  end subroutine field_stringas_mf

!===============================================================================
end module read_impexp_config_mod
!===============================================================================
