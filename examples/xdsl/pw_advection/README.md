Build with Psyclone: psyclone -api "gocean1.0" src/alg.f90 -opsy psy.f90 -oalg alg_new.f90

Compile: gfortran -ffree-line-length-0 -o exec src/compute_mod.f90 psy.f90 alg_new.f90 -I/some location/nemolite2d_edata/api_v1.0 -L/some location/nemolite2d_edata/api_v1.0 -l:gocean_api.a

Build with xDSL: python3.8 /home/nbrown23/projects/xdsl/PSyclone/bin/psyclone -nodm  -api "gocean1.0" -s ./xdsl_backends_transform.py src/alg.f90 -oalg /dev/null -opsy /dev/null