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

#ifdef CMEPS
  type(hycom_fld_type),target,dimension(8) :: fldsImp = (/              &
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
  type(hycom_fld_type),target,dimension(11) :: fldsImp = (/&
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
!    hycom_fld_type("sic","ice_fraction",&                                 !12
!                   "1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("sitx","downward_x_stress_at_sea_ice_base",&           !13
!                   "Pa",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("sity","downward_y_stress_at_sea_ice_base",&           !14
!                   "Pa",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("siqs","downward_sea_ice_basal_solar_heat_flux",&      !15
!                   "w_m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("sifh","downward_sea_ice_basal_heat_flux",&            !16
!                   "w_m-2",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("sifs","downward_sea_ice_basal_salt_flux",&            !17
!                   "kg_m-2_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("sifw","downward_sea_ice_basal_water_flux",&           !18
!                   "kg_m-2_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("sit_sfc","ice_surface_temperature",&                  !19
!                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("siu","sea_ice_x_velocity",&                           !20
!                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("siv","sea_ice_y_velocity",&                           !21
!                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("sih","sea_ice_thickness",&                            !22
!                   "m",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)/)
#endif

#ifdef CMEPS
  type(hycom_fld_type),target,dimension(1) :: fldsExp = (/              &
    hycom_fld_type("sst","So_t",                                        & !01
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)          &
  /)
#else
  type(hycom_fld_type),target,dimension(1) :: fldsExp = (/&
    hycom_fld_type("sst","sea_surface_temperature",&                         !01
                   "K",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)/)
!    hycom_fld_type("sss","sea_surface_salinity",&                            !02
!                   "ppt",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("ssu","sea_surface_x_velocity",&                          !03
!                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
!    hycom_fld_type("ssv","sea_surface_y_velocity",&                          !04
!                   "m_s-1",.TRUE.,FLD_REMAP_BILINR,FLD_MASK_NNE,1)/)
#endif

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
