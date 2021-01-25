! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020, Science and Technology Facilities Council.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author: I. Kavcic, Met Office
!
! -----------------------------------------------------------------------------
! A kernel that assigns a value to a field on a continuous function space W0
! -----------------------------------------------------------------------------
module setval_field_w0_kernel_mod

  use argument_mod,      only: arg_type,          &
                               GH_FIELD, GH_REAL, &
                               GH_INC, GH_READ, CELLS
  use fs_continuity_mod, only: W0
  use constants_mod,     only: r_def, i_def
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! The type declaration for the kernel. Contains the metadata needed by
  ! the PSy layer.
  !-----------------------------------------------------------------------------
  type, public, extends(kernel_type) :: setval_field_w0_kernel_type
    private
    type(arg_type), dimension(2) :: meta_args = (/ &
         arg_type(GH_FIELD, GH_INC, W0),           &
         arg_type(GH_REAL,  GH_READ)               &
         /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: setval_field_w0_code
  end type setval_field_w0_kernel_type

  public setval_field_w0_code

  contains

  !> @brief Sets a field on W0 function space to a scalar value
  !> @param[in] nlayers Number of layers
  !> @param[in,out] field_1_w0 Field to update to a scalar
  !> @param[in] rscalar_2 Value to set the field to
  !> @param[in] ndf_w0 Number of degrees of freedom per cell for the
  !!                   updated field
  !> @param[in] undf_w0 Number of unique degrees of freedom for the
  !!                    updated field
  !> @param[in] map_w0 Dofmap for the cell at the base of the column for
  !!                   the updated field
  subroutine setval_field_w0_code(nlayers, field_1_w0, rscalar_2, &
                                  ndf_w0, undf_w0, map_w0)

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: undf_w0
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    real(kind=r_def), intent(in) :: rscalar_2
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field_1_w0

    ! Internal variables
    integer(kind=i_def) :: k, df

    ! Update field
    do k = 0, nlayers-1
      do df = 1, ndf_w0
        field_1_w0( map_w0(df) + k ) = rscalar_2
      end do
    end do

  end subroutine setval_field_w0_code

end module setval_field_w0_kernel_mod