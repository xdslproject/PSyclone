   !!=====================================================================================
   !! ***  traadv kernel extracted from the NEMO software (http://www.nemo-ocean.eu ) ***
   !! ***          governed by the CeCILL licence (http://www.cecill.info)            ***
   !!
   !! ***                             IS-ENES2 - CMCC/STFC                            ***
   !!=====================================================================================
PROGRAM tra_adv

   REAL*8,  DIMENSION(:,:,:), ALLOCATABLE   :: mydomain, zwx, zwy, umask, vmask
   REAL*8,  DIMENSION(:), ALLOCATABLE       :: rnfmsk_z
   REAL*8                                        :: zice, zu, z0u, zzwx, zv, z0v, zzwy, ztra, zbtr, zdt, zalpha
   REAL*8                                        :: r, checksum
   REAL*8                                        :: zw, z0w
   INTEGER                                       :: jpi, jpj, jpk, ji, jj, jk, jt
   INTEGER*8                                     :: itn_count
   integer :: itimer0, itimer1


   jpi=256
   jpj=256
   jpk=256
   itn_count=100     
   
   allocate(mydomain(jpi, jpj, jpk))   
   allocate(zwx(jpi, jpj, jpk))
   allocate(zwy(jpi, jpj, jpk))
   allocate(umask(jpi, jpj, jpk))
   allocate(vmask(jpi, jpj, jpk))
  
   call timer_init()
   call timer_start(itimer0, label='Initialise')

! arrays initialization

   r = jpi*jpj*jpk

   ! the following three lines can be uncommented to randomize arrays initialization
   !call random_seed()
   !call random_number(r)
   r = r*jpi*jpj*jpk

   DO jk = 1, jpk
      DO jj = 1, jpj
          DO ji = 1, jpi
              umask(ji,jj,jk) = ji*jj*jk/r
   !           mydomain(ji,jj,jk) =ji*jj*jk/r              
   !           vmask(ji,jj,jk)= ji*jj*jk/r              
          END DO
      END DO
   END DO  
   
   call timer_stop(itimer0)
   call timer_start(itimer1, label='Compute')

!***********************
!* Start of the symphony
!***********************

   !DO jt = 1, itn_count
      
      !DO jk = 1, jpk
      !    DO jj = 1, jpj
      !       DO ji = 1, jpi
      !          zind(ji,jj,jk) = MAX (   &
      !             rnfmsk(ji,jj) * rnfmsk_z(jk),      &
      !             upsmsk(ji,jj)                      &
      !             &                  ) * tmask(ji,jj,jk)
      !          zind(ji,jj,jk) = 1.0 - zind(ji,jj,jk)
      !       END DO
      !    END DO
      ! END DO

      !DO jj = 1, jpj
      !   DO ji = 1, jpi
      !      zwx(ji,jj,jpk) = 0.e0
      !      zwy(ji,jj,jpk) = 0.e0
      !   END DO
      !END DO

       !DO jk = 1, jpk-1
       !   DO jj = 1, jpj-1
       !      DO ji = 1, jpi-1
       !          zwx(ji,jj,jk) = umask(ji,jj,jk) * ( mydomain(ji+1,jj,jk) - mydomain(ji,jj,jk) )
       !          zwy(ji,jj,jk) = vmask(ji,jj,jk) * ( mydomain(ji,jj+1,jk) - mydomain(ji,jj,jk) )
       !      END DO
       !   END DO
       !END DO

  !END DO
        
  call timer_stop(itimer1)

  call timer_report()
  
  deallocate(mydomain)
  deallocate(zwx)
  deallocate(zwy)
  deallocate(umask)
  deallocate(vmask)  
end program tra_adv
