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

module compute_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use field_mod
  use grid_mod
  implicit none

  private

  public compute_u, compute_u_code, compute_v, compute_v_code, &
          compute_w, compute_w_code, compute_th, compute_th_code

  type, extends(kernel_type) :: compute_u
     type(go_arg), dimension(8) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),        & 
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & 
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)        &
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS     
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_u_code
  end type compute_u

  type, extends(kernel_type) :: compute_v
     type(go_arg), dimension(8) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),        & 
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & 
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)        &
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS     
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_v_code
  end type compute_v

  type, extends(kernel_type) :: compute_w
     type(go_arg), dimension(8) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),        & 
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & 
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)        &
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS     
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_w_code
  end type compute_w

  type, extends(kernel_type) :: compute_th
     type(go_arg), dimension(9) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),        & 
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        & 
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READ,  GO_R_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE),        &
             go_arg(GO_READ,  GO_CU, GO_POINTWISE)        &
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS     
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_th_code
  end type compute_th

contains

  subroutine compute_u_code(k, i, su, u, v, w, tcx, tcy, tzc1, tzc2)
    integer,  intent(in) :: k, i
    real(go_wp), intent(out), dimension(:,:) :: su
    real(go_wp), intent(in),  dimension(:,:) :: u, v, w, tzc1, tzc2
    real(go_wp), intent(in) :: tcx, tcy

    real(go_wp) :: temp

    temp=tcx*(u(k,i-1) * (u(k,i) + u(k,i-1)) - u(k,i+1) * (u(k,i) + u(k,i+1)))
    temp=temp+tcy*(u(k,i) * (v(k,i) + v(k,i+1)) - u(k,i) * (v(k,i) + v(k,i+1)))
    temp=temp+(tzc1(k,1) * u(k-1,i) * (w(k-1,i) + w(k-1,i+1)) - tzc2(k,1) * &
            u(k+1,i) * (w(k,i) + w(k,i+1)))
    su(k,i)=temp
  end subroutine compute_u_code

  subroutine compute_v_code(k, i, sv, u, v, w, tcx, tcy, tzc1, tzc2)
    integer,  intent(in) :: k, i
    real(go_wp), intent(out), dimension(:,:) :: sv
    real(go_wp), intent(in),  dimension(:,:) :: u, v, w, tzc1, tzc2
    real(go_wp), intent(in) :: tcx, tcy

    real(go_wp) :: temp

    temp=tcy * (v(k,i) * (v(k,i) + v(k,i)) - v(k,i) * (v(k,i) + v(k,i)))
    temp=temp+tcx*(v(k,i-1) * (u(k,i-1) + u(k,i)) - v(k,i+1) * (u(k,i) + u(k,i)))
    temp=temp+(tzc1(k,1) * v(k-1,i) * (w(k-1,i) + w(k,i)) - tzc2(k,1) * &
            v(k+1,i) * (w(k,i) + w(k,i)))
    sv(k,i)=temp
  end subroutine compute_v_code

  subroutine compute_w_code(k, i, sw, u, v, w, tcx, tcy, tzd1, tzd2)
    integer,  intent(in) :: k, i
    real(go_wp), intent(out), dimension(:,:) :: sw
    real(go_wp), intent(in),  dimension(:,:) :: u, v, w, tzd1, tzd2
    real(go_wp), intent(in) :: tcx, tcy

    real(go_wp) :: temp

    temp=tzd1(k,1) * w(k-1,i) * (w(k,i) + w(k-1,i)) - tzd2(k,1) * &
            w(k+1,i) * (w(k,i) + w(k+1,i))
    temp=temp+tcx*(w(k,i-1)*(u(k,i) + u(k+1,i-1)) - w(k,i+1) * (u(k,i) + u(k+1,i)))
    temp=temp+tcy*(w(k,i) * (v(k,i) + v(k+1,i)) - w(k,i) * (v(k,i) + v(k+1,i)));
    sw(k,i)=temp
  end subroutine compute_w_code

  subroutine compute_th_code(k, i, sth, u, v, w, th, cx, cy, tzc1, tzc2)
    integer,  intent(in) :: k, i
    real(go_wp), intent(out), dimension(:,:) :: sth
    real(go_wp), intent(in),  dimension(:,:) :: u, v, w, th, tzc1, tzc2
    real(go_wp), intent(in) :: cx, cy

    real(go_wp) :: temp

    temp=cx*0.5*(u(k,i-1) * th(k,i-1) - u(k,i) * th(k,i+1))
    temp=temp+cy*0.5*(v(k,i) * th(k,i) - v(k,i) * th(k,i))
    temp=temp+2.0*(tzc1(k,1)*w(k-1,i)*th(k-1,i) - tzc2(k,1)*w(k,i)*th(k+1,i))
    sth(k,i)=temp
  end subroutine compute_th_code
end module compute_mod
