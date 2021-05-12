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
    IMPORT_ADAPTABLE = import_flag(2)

!===============================================================================
! public
!===============================================================================
  public import_flag
  public IMPORT_ERROR
  public IMPORT_REQUIRED
  public IMPORT_UNCOUPLED
  public IMPORT_ADAPTABLE

  public operator(==), assignment(=)

  interface operator (==)
    module procedure field_imp_eq
  end interface

  interface assignment (=)
    module procedure field_imp_toString
    module procedure field_imp_frString
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function field_imp_eq(val1, val2)
    logical field_imp_eq
    type(import_flag), intent(in) :: val1, val2
    field_imp_eq = (val1%imp == val2%imp)
  end function field_imp_eq

  !-----------------------------------------------------------------------------

  subroutine field_imp_toString(string, val)
    character(len=*), intent(out) :: string
    type(import_flag), intent(in) :: val
    if (val == IMPORT_REQUIRED) then
      write(string,'(a)') 'REQUIRED'
    elseif (val == IMPORT_UNCOUPLED) then
      write(string,'(a)') 'UNCOUPLED'
    elseif (val == IMPORT_ADAPTABLE) then
      write(string,'(a)') 'ADAPTABLE'
    else
      write(string,'(a)') 'ERROR'
    endif
  end subroutine field_imp_toString

  !-----------------------------------------------------------------------------

  subroutine field_imp_frString(val, string)
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
    elseif (ustring .eq. 'ADAPTABLE') then
      val = IMPORT_ADAPTABLE
    else
      val = IMPORT_ERROR
    endif
  end subroutine field_imp_frString

!===============================================================================
end module hycom_nuopc_flags
!===============================================================================
