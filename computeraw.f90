program computeraw

    implicit none

    integer, parameter:: nx = 256
    integer, parameter:: ny = 256
    integer, parameter:: nz = 128
    real(kind=8):: realdummy
    integer::      intdummy
    integer(kind=8):: rec_len
    real(kind=8),dimension(:,:,:),allocatable:: ufield
    real(kind=8),dimension(:,:,:),allocatable:: vfield
    real(kind=8),dimension(:,:,:),allocatable:: wfield

    allocate(ufield(nx, ny, nz))
    allocate(vfield(nx, ny, nz))
    allocate(wfield(nx, ny, nz-1))


    ! Read from file
    open(unit=1,file=trim(datfile),status='old',access='stream',form='unformatted')
    read(1) realdummy, realdummy, realdummy, intdummy, intdummy, intdummy, realdummy
    read(1) ufield
    read(1) vfield
    read(1) wfield
    close(1)

    ! Write to file
    inquire(IOLENGTH=rec_len) ufield
    open(2,file='u.raw', status='unknown', form='unformatted', access='direct', recl=rec_len)
    write(2,rec=1) ufield
    close(2)


end program computeraw
