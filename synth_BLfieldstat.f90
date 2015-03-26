PROGRAM synth_BLfield

    implicit none


    integer::px,py,startz,starty,stopz,stopy,Ny_loc,Nz_loc,no_procsx,no_procsy,rank,Nx,Ny,Nz,Nl,l,Nz_write
    character(len=80):: rankstring, filename, filename_write
    real(kind=8),dimension(:,:,:,:),allocatable:: field

    no_procsx = 4
    no_procsy = 4
    Nx    = 128 
    Ny    = 128 
    Nz    = 80
    Ny_loc = Ny/no_procsx
    Nz_loc = Nz/no_procsy
    Nl    = 11 

    print *, 'Program for synthesizing BL_fieldstat from parallel IO files'
    print *, 'procsx = ', no_procsx
    print *, 'procsy = ', no_procsy
    print *, 'Nx =     ', Nx
    print *, 'Ny =     ', Ny
    print *, 'Nz =     ', Nz
    print *, 'Nl =     ', Nl
    print *, ''
    print *, 'Ny_loc = ', Ny_loc
    print *, 'Nz_loc = ', Nz_loc


    allocate(field(Nx,Ny,Nz,Nl)) ! plus 1 because for some reason we write out one of the defunct modes 
    field = 0.0D0


    !
    ! BL_field is distributed in z pencils where the first dimension (x) is
    ! distributed among the xprocs and the second dimension (y) is distributed
    ! among the yprocs.
    !
    print *, 'Start reading from files'   

    do py=0,no_procsy-1
    do px=0,no_procsx-1

        rank = px + py*no_procsx

        starty = px*Ny_loc+1
        stopy  = starty+Ny_loc-1
        startz = py*Nz_loc+1
        stopz  = startz+Nz_loc-1

        print *, 'rank = ', rank, '  starty = ', starty, '  startz = ', startz

        write(unit=rankstring,fmt="(i4.4)") rank

        filename='rank_'//trim(rankstring)//'_BL_fieldstat.dat'
        
        open(unit=rank,file=trim(filename),status='old',form='formatted')
        read(rank,*) !Title
        read(rank,*) !procs
        read(rank,*) !amountofprocs
        read(rank,*) !
        read(rank,*) !Nx
        read(rank,*) ! 
        read(rank,*) !Ny
        read(rank,*) ! 
        read(rank,*) !Nz
        read(rank,*) ! 
        read(rank,*) !Nl
        read(rank,*) !
        read(rank,*) !
        do l=1,Nl
            read(rank,*) field(:,starty:stopy,startz:stopz,l)
        enddo
        close(rank)

    enddo
    enddo

    !
    ! Now write it
    !
    filename_write = 'BL_fieldstat_preliminary.dat'
    print *, 'Start writing to file'   

    open(unit=123,file=filename_write,status='replace',form='formatted')
    write(123,"(a21,a17,a14)") '%number of samples = ', ' time interval = ', ' in current = '
!    do l=1,Nl
!        write(123,"(2(ES25.17E3,1x))") field(:,:,1:Nz_write,l)
        write(123,"(4(ES25.17E3,1x))") field
!    enddo
    close(123)

    write(*,*) 'Succesfully finished'

END PROGRAM synth_BLfield
