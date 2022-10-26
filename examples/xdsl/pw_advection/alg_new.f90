program simple
  use kind_params_mod, only : go_wp
  use grid_mod
  use field_mod
  use gocean_mod, only : gocean_finalise, gocean_init
  use compute_mod, only : compute_th, compute_u, compute_v, compute_w
  use psy_simple, only : invoke_0
  TYPE(grid_type), TARGET :: model_grid
  type(r2d_field) :: u_fld
  type(r2d_field) :: v_fld
  type(r2d_field) :: w_fld
  type(r2d_field) :: th_fld
  type(r2d_field) :: su_fld
  type(r2d_field) :: sv_fld
  type(r2d_field) :: sw_fld
  type(r2d_field) :: sth_fld
  type(r2d_field) :: tzc1_fld
  type(r2d_field) :: tzc2_fld
  real(kind=go_wp) :: tcx
  real(kind=go_wp) :: tcy
  real(kind=go_wp) :: cx
  real(kind=go_wp) :: cy
  integer :: ncycle
  integer :: ierr
  integer :: jpiglo
  integer :: jpjglo

  jpkglo = 4096
  jpiglo = 4096
  call gocean_init()
  model_grid = grid_type(GO_ARAKAWA_C,(/GO_BC_PERIODIC, GO_BC_PERIODIC, GO_BC_NONE/),GO_OFFSET_SW)
  call grid_init(model_grid, jpkglo, jpiglo, 1000.0_go_wp, 1000.0_go_wp)
  PRINT *, "Initialised system for grid size ", jpkglo, " by ", jpiglo
  tcx = 1.0
  tcy = 1.0
  cx = 1.0
  cy = 1.0
  u_fld = r2d_field(model_grid,GO_U_POINTS)
  v_fld = r2d_field(model_grid,GO_U_POINTS)
  w_fld = r2d_field(model_grid,GO_U_POINTS)
  th_fld = r2d_field(model_grid,GO_U_POINTS)
  su_fld = r2d_field(model_grid,GO_U_POINTS)
  sv_fld = r2d_field(model_grid,GO_U_POINTS)
  sw_fld = r2d_field(model_grid,GO_U_POINTS)
  sth_fld = r2d_field(model_grid,GO_U_POINTS)
  tzc1_fld = r2d_field(model_grid,GO_U_POINTS)
  tzc2_fld = r2d_field(model_grid,GO_U_POINTS)
  call invoke_0(su_fld, u_fld, v_fld, w_fld, tcx, tcy, tzc1_fld, tzc2_fld, sv_fld, sw_fld, sth_fld, th_fld, cx, cy)
  call gocean_finalise()

end program simple
