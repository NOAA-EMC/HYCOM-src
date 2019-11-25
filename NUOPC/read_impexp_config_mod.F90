module read_impexp_config_mod

  !-----------------------------------------------------------------------------
  ! Field Configuration Utilities
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none

  private

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
    real                 :: fillValue   = 999999999
  end type hycom_fld_type

  type(hycom_fld_type),target,dimension(21) :: fldsImp = (/&
    hycom_fld_type("u10","10m_u_wind",&                                      !01
                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("v10","10m_v_wind",&                                      !02
                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("wndspd10","10m_wind_speed",&                             !03
                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("airtmp","air_temperature_at_sea_level",&                 !04
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("airhum","air_humidity_at_sea_level",&                    !05
                   "kg_kg-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("swflxd","shortwave_radiative_flux_into_ocean_nonmed",&   !06
                   "w_m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("lwflxd","longwave_radiative_flux_into_ocean_nonmed",&    !07
                   "w_m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("prcp","precipitation_amount",&                           !08
                   "kg_m-2_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("gt","gt",&                                               !09
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("mslprs","mean_sea_level_pressure_anomaly",&              !10
                   "Pa",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sic","ice_concentration",&                               !11
                   "-",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sitx","downward_x_stress_at_sea_ice_base",&              !12
                   "Pa",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sity","downward_y_stress_at_sea_ice_base",&              !13
                   "Pa",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("siqs","downward_sea_ice_basal_solar_heat_flux",&         !14
                   "w_m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sifh","downward_sea_ice_basal_heat_flux",&               !15
                   "w_m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sifs","downward_sea_ice_basal_salt_flux",&               !16
                   "kg_m-2_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sifw","downward_sea_ice_basal_water_flux",&              !17
                   "kg_m-2_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sit_sfc","ice_surface_temperature",&                     !18
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("siu","sea_ice_x_velocity",&                              !19
                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("siv","sea_ice_y_velocity",&                              !20
                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sih","sea_ice_thickness",&                               !21
                   "m",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)/)

  type(hycom_fld_type),target,dimension(4) :: fldsExp = (/&
    hycom_fld_type("sst","sea_surface_temperature",&                         !01
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("sss","sea_surface_salinity",&                            !02
                   "ppt",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("ssu","sea_surface_x_velocity",&                          !03
                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    hycom_fld_type("ssv","sea_surface_y_velocity",&                          !04
                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)/)


  public read_impexp_config

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine read_impexp_config(fname,numExpFields,numImpFields, &
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

  end subroutine

  !-----------------------------------------------------------------------------

end module read_impexp_config_mod
