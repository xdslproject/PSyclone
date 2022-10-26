! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018, Science and Technology Facilities Council.
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

program simple

  use kind_params_mod, only: go_wp
  use grid_mod
  use field_mod
  use gocean_mod, only: gocean_init, gocean_finalise
  use compute_mod,   only: compute_u, compute_v, compute_w, compute_th
  implicit none

  type(grid_type), target :: model_grid

  type(r2d_field) :: u_fld, v_fld, w_fld, th_fld, su_fld, sv_fld, sw_fld, &
      sth_fld, tzc1_fld, tzc2_fld
  real(go_wp) :: tcx, tcy, cx, cy

  integer :: ncycle, ierr
  integer :: jpiglo, jpjglo

  ! Dimensions of our domain
  jpkglo = 4096
  jpiglo = 4096

  call gocean_init()

  ! Create our grid
  model_grid = grid_type(GO_ARAKAWA_C,                                 &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/), &
                         GO_OFFSET_SW)
  !> Generate a domain decomposition
  !call model_grid%init(jpkglo, jpiglo)

  ! Having specified the T points mask, we can set up mesh parameters
  call grid_init(model_grid, jpkglo, jpiglo, 1000.0_go_wp, 1000.0_go_wp)

  print *, "Initialised system for grid size ",jpkglo, " by ", jpiglo

  tcx=1.0
  tcy=1.0
  cx=1.0
  cy=1.0

  ! Create fields on this grid
  u_fld  = r2d_field(model_grid, GO_U_POINTS)
  v_fld  = r2d_field(model_grid, GO_U_POINTS)
  w_fld  = r2d_field(model_grid, GO_U_POINTS)
  th_fld  = r2d_field(model_grid, GO_U_POINTS)

  su_fld  = r2d_field(model_grid, GO_U_POINTS)
  sv_fld  = r2d_field(model_grid, GO_U_POINTS)
  sw_fld  = r2d_field(model_grid, GO_U_POINTS)
  sth_fld  = r2d_field(model_grid, GO_U_POINTS)

  tzc1_fld  = r2d_field(model_grid, GO_U_POINTS)
  tzc2_fld  = r2d_field(model_grid, GO_U_POINTS)

  call invoke(compute_u(su_fld, u_fld, v_fld, w_fld, tcx, tcy, tzc1_fld, tzc2_fld), &
              compute_v(sv_fld, u_fld, v_fld, w_fld, tcx, tcy, tzc1_fld, tzc2_fld), &
              compute_w(sw_fld, u_fld, v_fld, w_fld, tcx, tcy, tzc1_fld, tzc2_fld), &
              compute_th(sth_fld, u_fld, v_fld, w_fld, th_fld, cx, cy, tzc1_fld, tzc2_fld))

  call gocean_finalise()
end program simple

