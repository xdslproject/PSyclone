   !!=====================================================================================
   !! ***  traadv kernel extracted from the NEMO software (http://www.nemo-ocean.eu ) ***
   !! ***          governed by the CeCILL licence (http://www.cecill.info)            ***
   !!
   !! ***                             IS-ENES2 - CMCC/STFC                            ***
   !!=====================================================================================
PROGRAM tra_adv

   REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: tsn
   REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: pun, pvn, pwn
   REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: mydomain, zslpx, zslpy, zwx, zwy, umask, vmask, tmask, zind
   REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:)     :: ztfreez, rnfmsk, upsmsk
   REAL*8, ALLOCATABLE, SAVE, DIMENSION(:)       :: rnfmsk_z
   REAL*8                                        :: zice, zu, z0u, zzwx, zv, z0v, zzwy, ztra, zbtr, zdt, zalpha
   REAL*8                                        :: r, checksum
   REAL*8                                        :: zw, z0w
   INTEGER                                       :: jpi, jpj, jpk, ji, jj, jk, jt
   INTEGER*8                                     :: itn_count


   jpi=64
   jpj=64
   jpk=64
   itn_count=1000

   ALLOCATE( mydomain (jpi,jpj,jpk))
   ALLOCATE( zwx (jpi,jpj,jpk))
   ALLOCATE( zwy (jpi,jpj,jpk))
   ALLOCATE( zslpx (jpi,jpj,jpk))
   ALLOCATE( zslpy (jpi,jpj,jpk))
   ALLOCATE( pun (jpi,jpj,jpk))
   ALLOCATE( pvn (jpi,jpj,jpk))
   ALLOCATE( pwn (jpi,jpj,jpk))
   ALLOCATE( umask (jpi,jpj,jpk))
   ALLOCATE( vmask (jpi,jpj,jpk))
   ALLOCATE( tmask (jpi,jpj,jpk))
   ALLOCATE( zind (jpi,jpj,jpk))
   ALLOCATE( ztfreez (jpi,jpj))
   ALLOCATE( rnfmsk (jpi,jpj))
   ALLOCATE( upsmsk (jpi,jpj))
   ALLOCATE( rnfmsk_z (jpk))
   ALLOCATE( tsn(jpi,jpj,jpk))


! arrays initialization

   r = jpi*jpj*jpk

   ! the following three lines can be uncommented to randomize arrays initialization
   !call random_seed()
   !call random_number(r)
   !r = r*jpi*jpj*jpk

   DO jk = 1, jpk
      DO jj = 1, jpj
          DO ji = 1, jpi
              umask(ji,jj,jk) = ji*jj*jk/r
              mydomain(ji,jj,jk) =ji*jj*jk/r
              pun(ji,jj,jk) =ji*jj*jk/r
              pvn(ji,jj,jk) =ji*jj*jk/r
              pwn(ji,jj,jk) =ji*jj*jk/r
              vmask(ji,jj,jk)= ji*jj*jk/r
              tsn(ji,jj,jk)= ji*jj*jk/r
              tmask(ji,jj,jk)= ji*jj*jk/r
          END DO
      END DO
   END DO

   r = jpi*jpj
   DO jj=1, jpj
      DO ji=1, jpi
         ztfreez(ji,jj) = ji*jj/r
         upsmsk(ji,jj) = ji*jj/r
         rnfmsk(ji,jj) = ji*jj/r
      END DO
   END DO

   DO jk=1, jpk
      rnfmsk_z(jk)=jk/jpk
   END DO

!***********************
!* Start of the symphony
!***********************

   DO jt = 1, itn_count
       DO jk = 1, jpk
          DO jj = 1, jpj
             DO ji = 1, jpi
                IF( tsn(ji,jj,jk) <= ztfreez(ji,jj) + 0.1d0 ) THEN   ;   zice = 1.d0
                ELSE                                                 ;   zice = 0.d0
                ENDIF
                zind(ji,jj,jk) = MAX (   &
                   rnfmsk(ji,jj) * rnfmsk_z(jk),      &
                   upsmsk(ji,jj)               ,      &
                   zice                               &
                   &                  ) * tmask(ji,jj,jk)
                   zind(ji,jj,jk) = 1 - zind(ji,jj,jk)
             END DO
          END DO
       END DO


      DO jj = 1, jpj
         DO ji = 1, jpi
            zwx(ji,jj,jpk) = 0.e0
            zwy(ji,jj,jpk) = 0.e0
         END DO
      END DO

       DO jk = 1, jpk-1
          DO jj = 1, jpj-1
             DO ji = 1, jpi-1
                 zwx(ji,jj,jk) = umask(ji,jj,jk) * ( mydomain(ji+1,jj,jk) - mydomain(ji,jj,jk) )
                 zwy(ji,jj,jk) = vmask(ji,jj,jk) * ( mydomain(ji,jj+1,jk) - mydomain(ji,jj,jk) )
             END DO
          END DO
       END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
            zslpx(ji,jj,jpk) = 0.e0
            zslpy(ji,jj,jpk) = 0.e0
         END DO
      END DO

       DO jk = 1, jpk-1
         DO jj = 2, jpj
            DO ji = 2, jpi
               zslpx(ji,jj,jk) =                    ( zwx(ji,jj,jk) + zwx(ji-1,jj  ,jk) )   &
               &            * ( 0.25d0 + SIGN( 0.25d0, zwx(ji,jj,jk) * zwx(ji-1,jj  ,jk) ) )
               zslpy(ji,jj,jk) =                    ( zwy(ji,jj,jk) + zwy(ji  ,jj-1,jk) )   &
               &            * ( 0.25d0 + SIGN( 0.25d0, zwy(ji,jj,jk) * zwy(ji  ,jj-1,jk) ) )
            END DO
         END DO
      END DO

      DO jk = 1, jpk-1
         DO jj = 2, jpj
            DO ji = 2, jpi
               zslpx(ji,jj,jk) = SIGN( 1.d0, zslpx(ji,jj,jk) ) * MIN(    ABS( zslpx(ji  ,jj,jk) ),   &
               &                                                2.d0*ABS( zwx  (ji-1,jj,jk) ),   &
               &                                                2.d0*ABS( zwx  (ji  ,jj,jk) ) )
               zslpy(ji,jj,jk) = SIGN( 1.d0, zslpy(ji,jj,jk) ) * MIN(    ABS( zslpy(ji,jj  ,jk) ),   &
               &                                                2.d0*ABS( zwy  (ji,jj-1,jk) ),   &
               &                                                2.d0*ABS( zwy  (ji,jj  ,jk) ) )
            END DO
         END DO
      END DO

      DO jk = 1, jpk-1
         zdt  = 1
         DO jj = 2, jpj-1
            DO ji = 2, jpi-1
                z0u = SIGN( 0.5d0, pun(ji,jj,jk) )
                zalpha = 0.5d0 - z0u
                zu  = z0u - 0.5d0 * pun(ji,jj,jk) * zdt

                zzwx = mydomain(ji+1,jj,jk) + zind(ji,jj,jk) * (zu * zslpx(ji+1,jj,jk))
                zzwy = mydomain(ji  ,jj,jk) + zind(ji,jj,jk) * (zu * zslpx(ji  ,jj,jk))

                zwx(ji,jj,jk) = pun(ji,jj,jk) * ( zalpha * zzwx + (1.-zalpha) * zzwy )

                z0v = SIGN( 0.5d0, pvn(ji,jj,jk) )
                zalpha = 0.5d0 - z0v
                zv  = z0v - 0.5d0 * pvn(ji,jj,jk) * zdt

                zzwx = mydomain(ji,jj+1,jk) + zind(ji,jj,jk) * (zv * zslpy(ji,jj+1,jk))
                zzwy = mydomain(ji,jj  ,jk) + zind(ji,jj,jk) * (zv * zslpy(ji,jj  ,jk))

                zwy(ji,jj,jk) = pvn(ji,jj,jk) * ( zalpha * zzwx + (1.d0-zalpha) * zzwy )
             END DO
          END DO
      END DO

      DO jk = 1, jpk-1
         DO jj = 2, jpj-1
            DO ji = 2, jpi-1
               zbtr = 1.
               ztra = - zbtr * ( zwx(ji,jj,jk) - zwx(ji-1,jj  ,jk  )   &
               &               + zwy(ji,jj,jk) - zwy(ji  ,jj-1,jk  ) )
               mydomain(ji,jj,jk) = mydomain(ji,jj,jk) + ztra
            END DO
         END DO
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
            zwx (ji,jj, 1 ) = 0.e0
            zwx (ji,jj,jpk) = 0.e0
         END DO
      END DO

      DO jk = 2, jpk-1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zwx(ji,jj,jk) = tmask(ji,jj,jk) * ( mydomain(ji,jj,jk-1) - mydomain(ji,jj,jk) )
            END DO
         END DO
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
            zslpx(ji,jj,1) = 0.e0
         END DO
      END DO

      DO jk = 2, jpk-1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zslpx(ji,jj,jk) =                    ( zwx(ji,jj,jk) + zwx(ji,jj,jk+1) )   &
               &            * ( 0.25d0 + SIGN( 0.25d0, zwx(ji,jj,jk) * zwx(ji,jj,jk+1) ) )
            END DO
         END DO
      END DO

      DO jk = 2, jpk-1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zslpx(ji,jj,jk) = SIGN( 1.d0, zslpx(ji,jj,jk) ) * MIN( ABS( zslpx(ji,jj,jk  ) ), &
               &                                               2.d0*ABS( zwx  (ji,jj,jk+1) ),   &
               &                                               2.d0*ABS( zwx  (ji,jj,jk  ) )  )
            END DO
         END DO
      END DO

      DO jj = 1, jpj
         DO ji = 1, jpi
            zwx(ji,jj, 1 ) = pwn(ji,jj,1) * mydomain(ji,jj,1)
         END DO
      END DO

      zdt  = 1
      zbtr = 1.
      DO jk = 1, jpk-1
         DO jj = 2, jpj-1
            DO ji = 2, jpi-1
               z0w = SIGN( 0.5d0, pwn(ji,jj,jk+1) )
               zalpha = 0.5d0 + z0w
               zw  = z0w - 0.5d0 * pwn(ji,jj,jk+1) * zdt * zbtr

               zzwx = mydomain(ji,jj,jk+1) + zind(ji,jj,jk) * (zw * zslpx(ji,jj,jk+1))
               zzwy = mydomain(ji,jj,jk  ) + zind(ji,jj,jk) * (zw * zslpx(ji,jj,jk  ))

               zwx(ji,jj,jk+1) = pwn(ji,jj,jk+1) * ( zalpha * zzwx + (1.-zalpha) * zzwy )
            END DO
         END DO
      END DO

      zbtr = 1.
      DO jk = 1, jpk-1
         DO jj = 2, jpj-1
            DO ji = 2, jpi-1
               ztra = - zbtr * ( zwx(ji,jj,jk) - zwx(ji,jj,jk+1) )
               mydomain(ji,jj,jk) = ztra
            END DO
         END DO
      END DO
  END DO

  deallocate( mydomain )
  deallocate( zwx )
  deallocate( zwy )
  deallocate( zslpx )
  deallocate( zslpy )
  deallocate( pun )
  deallocate( pvn )
  deallocate( pwn )
  deallocate( umask)
  deallocate( vmask)
  deallocate( tmask)
  deallocate( zind )
  deallocate( ztfreez )
  deallocate( rnfmsk)
  deallocate( upsmsk)
  deallocate( rnfmsk_z)
  deallocate( tsn)

end program tra_adv
