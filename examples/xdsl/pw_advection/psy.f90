  MODULE psy_simple
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0(su_fld, u_fld, v_fld, w_fld, tcx, tcy, tzc1_fld, tzc2_fld, sv_fld, sw_fld, sth_fld, th_fld, cx, cy)
      USE compute_mod, ONLY: compute_th_code, compute_u_code, compute_v_code, compute_w_code
      TYPE(r2d_field), intent(inout) :: su_fld
      TYPE(r2d_field), intent(inout) :: u_fld
      TYPE(r2d_field), intent(inout) :: v_fld
      TYPE(r2d_field), intent(inout) :: w_fld
      REAL(KIND=go_wp), intent(inout) :: tcx
      REAL(KIND=go_wp), intent(inout) :: tcy
      TYPE(r2d_field), intent(inout) :: tzc1_fld
      TYPE(r2d_field), intent(inout) :: tzc2_fld
      TYPE(r2d_field), intent(inout) :: sv_fld
      TYPE(r2d_field), intent(inout) :: sw_fld
      TYPE(r2d_field), intent(inout) :: sth_fld
      TYPE(r2d_field), intent(inout) :: th_fld
      REAL(KIND=go_wp), intent(inout) :: cx
      REAL(KIND=go_wp), intent(inout) :: cy
      INTEGER j
      INTEGER i

      DO j = su_fld%internal%ystart, su_fld%internal%ystop, 1
        DO i = su_fld%internal%xstart, su_fld%internal%xstop, 1
          CALL compute_u_code(i, j, su_fld%data, u_fld%data, v_fld%data, w_fld%data, tcx, tcy, tzc1_fld%data, tzc2_fld%data)
        END DO
      END DO
      DO j = sv_fld%internal%ystart, sv_fld%internal%ystop, 1
        DO i = sv_fld%internal%xstart, sv_fld%internal%xstop, 1
          CALL compute_v_code(i, j, sv_fld%data, u_fld%data, v_fld%data, w_fld%data, tcx, tcy, tzc1_fld%data, tzc2_fld%data)
        END DO
      END DO
      DO j = sw_fld%internal%ystart, sw_fld%internal%ystop, 1
        DO i = sw_fld%internal%xstart, sw_fld%internal%xstop, 1
          CALL compute_w_code(i, j, sw_fld%data, u_fld%data, v_fld%data, w_fld%data, tcx, tcy, tzc1_fld%data, tzc2_fld%data)
        END DO
      END DO
      DO j = sth_fld%internal%ystart, sth_fld%internal%ystop, 1
        DO i = sth_fld%internal%xstart, sth_fld%internal%xstop, 1
          CALL compute_th_code(i, j, sth_fld%data, u_fld%data, v_fld%data, w_fld%data, th_fld%data, cx, cy, tzc1_fld%data, tzc2_fld%data)
        END DO
      END DO

    END SUBROUTINE invoke_0
  END MODULE psy_simple