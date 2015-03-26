PROGRAM synth_precursorBLfield

    implicit none


    integer::px,py,startx,starty,stopx,stopy,Nx_loc,Ny_loc,no_procsx,no_procsy,rank,Nx,Ny,Nz,Nl,l,Nz_read,Nz_write
    character(len=80):: rankstring, filename, filename_write
    complex(kind=8),dimension(:,:,:,:),allocatable:: field

    no_procsx = 16
    no_procsy = 8 
    Nx    = 768/2
    Ny    = 768
    Nx_loc = Nx/no_procsx
    Ny_loc = Ny/no_procsy
    Nz    = 192
    Nl    = 3

    print *, 'Program for synthesizing BL_field from parallel IO files'
    print *, 'procsx = ', no_procsx
    print *, 'procsy = ', no_procsy
    print *, 'Nx =     ', Nx
    print *, 'Ny =     ', Ny
    print *, 'Nz =     ', Nz
    print *, 'Nl =     ', Nl
    print *, ''
    print *, 'Nx_loc = ', Nx_loc
    print *, 'Ny_loc = ', Ny_loc


    allocate(field(Nx+1,Ny,Nz,Nl)) ! plus 1 because for some reason we write out one of the defunct modes 
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
        startx = px*Nx_loc+1
        stopx  = startx+Nx_loc-1
        starty = py*Ny_loc+1
        stopy  = starty+Ny_loc-1

        print *, 'rank = ', rank, '  startx = ', startx, '  starty = ', starty

        write(unit=rankstring,fmt="(i4.4)") rank

        filename='rank_'//trim(rankstring)//'_BL_field_precursor.dat'
        
        open(unit=rank,file=trim(filename),status='old',form='formatted')
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        read(rank,*)
        do l=1,Nl
            if(l.eq.3) then
                Nz_read = Nz - 1
            else
                Nz_read = Nz
            endif
            read(rank,"(2(ES25.17E3,1x))") field(startx:stopx,starty:stopy,1:Nz_read,l)
        enddo
        close(rank)

    enddo
    enddo

    !
    ! Now write it
    !
    filename_write = 'BL_field_preliminary_precursor.dat'
    print *, 'Start writing to file'   

    open(unit=123,file=filename_write,status='replace',form='formatted')
    write(123,"(a8)") '% time = '
    write(123,"(a8)") '% Lx   = '
    write(123,"(a8)") '% Ly   = '
    write(123,"(a8)") '% Nx2  = '
    write(123,"(a8)") '% Ny   = '
    write(123,"(a8)") '% Nz   = '
    write(123,"(a8)") '% '
    do l=1,Nl
        if(l.eq.3) then
            Nz_write = Nz - 1
        else
            Nz_write = Nz
        endif
        write(123,"(2(ES25.17E3,1x))") field(:,:,1:Nz_write,l)
    enddo
    close(123)

    write(*,*) 'Succesfully finished'

END PROGRAM synth_precursorBLfield
