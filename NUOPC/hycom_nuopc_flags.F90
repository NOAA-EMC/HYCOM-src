!===============================================================================
! MODULE: HYCOM NUOPC Flags Module
!
! DESCRIPTION:
!   This module provides flags for the HYCOM NUOPC cap.
!
! FLAGS:
!
!===============================================================================
#include "HYCOM_NUOPC_Macros.h"
!===============================================================================
module hycom_nuopc_flags
#define MODNAME "hycom_nuopc_flags"

  use ESMF, only: ESMF_UtilStringUpperCase, ESMF_SUCCESS

!===============================================================================
! settings
!===============================================================================
  implicit none
  private
  save

!===============================================================================
! flags
!===============================================================================
  type import_flag
    sequence
    private
      integer :: imp
  end type import_flag

  type(import_flag), parameter :: &
    IMPORT_ERROR     = import_flag(-1), &
    IMPORT_REQUIRED  = import_flag(0),  &
    IMPORT_UNCOUPLED = import_flag(1),  &
    IMPORT_FLEXIBLE  = import_flag(2)

  type model_flag
    sequence
    private
      integer :: mdl
  end type model_flag

  type(model_flag), parameter :: &
    MODEL_ERROR   = model_flag(-1), &
    MODEL_DEFAULT = model_flag(0),  &
    MODEL_DATM    = model_flag(1)

!===============================================================================
! public
!===============================================================================
  public import_flag
  public IMPORT_ERROR
  public IMPORT_REQUIRED
  public IMPORT_UNCOUPLED
  public IMPORT_FLEXIBLE
  public model_flag
  public MODEL_ERROR
  public MODEL_DEFAULT
  public MODEL_DATM

  public operator(==), assignment(=)

  interface operator (==)
    module procedure import_flag_eq
    module procedure model_flag_eq
  end interface

  interface assignment (=)
    module procedure import_flag_toString
    module procedure import_flag_frString
    module procedure model_flag_toString
    module procedure model_flag_frString
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function import_flag_eq(val1, val2)
    logical import_flag_eq
    type(import_flag), intent(in) :: val1, val2
    import_flag_eq = (val1%imp == val2%imp)
  end function import_flag_eq

  !-----------------------------------------------------------------------------

  subroutine import_flag_toString(string, val)
    character(len=*), intent(out) :: string
    type(import_flag), intent(in) :: val
    if (val == IMPORT_REQUIRED) then
      write(string,'(a)') 'REQUIRED'
    elseif (val == IMPORT_UNCOUPLED) then
      write(string,'(a)') 'UNCOUPLED'
    elseif (val == IMPORT_FLEXIBLE) then
      write(string,'(a)') 'FLEXIBLE'
    else
      write(string,'(a)') 'ERROR'
    endif
  end subroutine import_flag_toString

  !-----------------------------------------------------------------------------

  subroutine import_flag_frString(val, string)
    type(import_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=16) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = IMPORT_ERROR
    elseif (ustring .eq. 'REQUIRED') then
      val = IMPORT_REQUIRED
    elseif (ustring .eq. 'UNCOUPLED') then
      val = IMPORT_UNCOUPLED
    elseif (ustring .eq. 'FLEXIBLE') then
      val = IMPORT_FLEXIBLE
    else
      val = IMPORT_ERROR
    endif
  end subroutine import_flag_frString

  !-----------------------------------------------------------------------------

  function model_flag_eq(val1, val2)
    logical model_flag_eq
    type(model_flag), intent(in) :: val1, val2
    model_flag_eq = (val1%mdl == val2%mdl)
  end function model_flag_eq

  !-----------------------------------------------------------------------------

  subroutine model_flag_toString(string, val)
    character(len=*), intent(out) :: string
    type(model_flag), intent(in) :: val
    if (val == MODEL_DEFAULT) then
      write(string,'(a)') 'DEFAULT'
    elseif (val == MODEL_DATM) then
      write(string,'(a)') 'DATM'
    else
      write(string,'(a)') 'ERROR'
    endif
  end subroutine model_flag_toString

  !-----------------------------------------------------------------------------

  subroutine model_flag_frString(val, string)
    type(model_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=16) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = MODEL_ERROR
    elseif (ustring .eq. 'DEFAULT') then
      val = MODEL_DEFAULT
    elseif (ustring .eq. 'DATM') then
      val = MODEL_DATM
    else
      val = MODEL_ERROR
    endif
  end subroutine model_flag_frString

!===============================================================================
end module hycom_nuopc_flags
!===============================================================================
