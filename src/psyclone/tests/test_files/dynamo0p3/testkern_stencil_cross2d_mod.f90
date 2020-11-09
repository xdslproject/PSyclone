! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author A. Coughtrie, Met Office

module testkern_stencil_cross2d_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none
  ! Test kernel for 2D Cross stencil use
  type, extends(kernel_type) :: testkern_stencil_cross2d_type
     type(arg_type), dimension(4) :: meta_args =              &
          (/ arg_type(gh_field, gh_inc,  w1),                 &
             arg_type(gh_field, gh_read, w2, stencil(cross2d)), &
             arg_type(gh_field, gh_read, w2),                 &
             arg_type(gh_field, gh_read, w3)                  &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => testkern_stencil_cross2d_code
  end type testkern_stencil_cross2d_type

contains

  subroutine testkern_stencil_cross2d_code(nlayers, fld1,     &
                                   fld2, fld2_st_size,        &
                                   fld2_st_max_branch_length, &
                                   fld2_st_dofmap,            &
                                   fld3, fld4,                &
                                   ndf_w1, undf_w1, map_w1,   &
                                   ndf_w2, undf_w2, map_w2,   &
                                   ndf_w3, undf_w3, map_w3)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w3
    integer(kind=i_def), intent(in), dimension(4) :: fld2_st_size
    integer(kind=i_def), intent(in) :: fld2_st_max_branch_length
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    integer(kind=i_def), intent(in) :: fld2_st_dofmap(ndf_w2,fld2_st_max_branch_length,4)
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld1
    real(kind=r_def), intent(in), dimension(undf_w2)    :: fld2
    real(kind=r_def), intent(in), dimension(undf_w2)    :: fld3
    real(kind=r_def), intent(in), dimension(undf_w3)    :: fld4

  end subroutine testkern_stencil_cross2d_code

end module testkern_stencil_cross2d_mod