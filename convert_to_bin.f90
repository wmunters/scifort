program convert_to_bin

    implicit none

    complex(kind=8),dimension(:,:,:),allocatable :: u,v,w
    integer,parameter:: Nfields = 3
    character(len=8):: string
    real(kind=8):: time, Lx_datfile, Ly_datfile
    integer:: Nx2_datfile, Ny_datfile, Nz_datfile

    ! open and read the ascii file
    print *, 'Reading from ascii file.'
    open(unit=1, file='BL_field.dat', status='old', form='formatted')
    read(1,"(a8,g11.4)") string, time
    read(1,"(a8,g24.17)") string, Lx_datfile	
    read(1,"(a8,g24.17)") string, Ly_datfile
    read(1,"(a8,i4)") string, Nx2_datfile 	
    read(1,"(a8,i4)") string, Ny_datfile 
    read(1,"(a8,i4)") string, Nz_datfile 
    read(1,*) 

    print *, '###############################'
    print *, 'BL_field data:'
    print *, ' time=', time
    print *, ' Lx  =', Lx_datfile
    print *, ' Ly  =', Ly_datfile
    print *, ' Nx2 =', Nx2_datfile
    print *, ' Ny  =', Ny_datfile
    print *, ' Nz  =', Nz_datfile
    print *, '###############################'

    allocate(u(Nx2_datfile/2+1, Ny_datfile, Nz_datfile))
    allocate(v(Nx2_datfile/2+1, Ny_datfile, Nz_datfile))
    allocate(w(Nx2_datfile/2+1, Ny_datfile, Nz_datfile-1))
    print *, 'Reading u component'
    read(1,"(2(ES25.17E3,1x))") u
    print *, 'Reading v component'
    read(1,"(2(ES25.17E3,1x))") v
    print *, 'Reading w component'
    read(1,"(2(ES25.17E3,1x))") w
    close(1)

    ! dump in the binary file
    print *, 'Writing to binary file.'
    open(unit=2, file='BL_field_new.dat', status='new', access='stream', form='unformatted')
    write(2) time, Lx_datfile, Ly_datfile, Nx2_datfile, Ny_datfile, Nz_datfile, 0.0d0
    write(2) u
    write(2) v
    write(2) w
    close(2)

    print *, 'Finished!'

end program
