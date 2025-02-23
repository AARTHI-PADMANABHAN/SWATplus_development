program main

  use mpi
  use time_module
  use hydrograph_module
  use maximum_data_module
  use calibration_data_module
  use hdf5_module
  use hdf5  ! Include HDF5 module for HDF5-related subroutines

  implicit none

  integer :: date_time(8)
  character*10 b(3)
  integer :: ierr, mpi_rank, mpi_size  ! MPI variables

  ! Initialize MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, mpi_rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_size, ierr)

  prog = " SWAT+ Jun 13 2023       MODULAR Rev 2023.60.5.7"

  ! Print program start information
  if (mpi_rank == 0) then
     write (*,1000)
     open (9003,file='simulation.out')
     write (9003,1000)
  end if
1000 format(1x,"                  SWAT+               ",/,             &
   &          "             Revision 60.5.7          ",/,             &
   &          "      Soil & Water Assessment Tool    ",/,             &
   &          "               PC Version             ",/,             &
   &          "    Program reading . . . executing",/)

  if (mpi_rank == 0) open (888,file="erosion.txt",recl = 1500)

  ! Proceed with the rest of the simulation
  call proc_bsn   
  call proc_date_time
  call proc_db
  call proc_read

  call hyd_connect
  call exco_db_read
  call dr_db_read
  
  call cli_lapse
  call object_read_output

  call om_water_init
  call pest_cha_res_read
  call path_cha_res_read
  call salt_cha_res_read

  call lsu_read_elements        !defining landscape units by hru

  call proc_hru
  call proc_cha
  call proc_aqu
  
  ! read decision table data for conditional management
  call dtbl_lum_read

  call hru_lte_read

  call proc_cond

  call res_read_weir !moved from proc_res Jaehak 2023
  call dtbl_res_read
  call dtbl_scen_read
  ! input scenarios used in simulation
  call cal_cond_read
        
  ! read manure allocation inputs
  call manure_allocation_read
  
  call dtbl_flocon_read
        
  ! read water treatment and water allocation files - before hru lum tables
  call treat_read_om
  call water_allocation_read
  
  call hru_dtbl_actions_init
  
  ! read reservoir and wetland data
  call proc_res
  call wet_read_hyd
  call wet_read
  if (db_mx%wet_dat > 0) call wet_all_initial
  if (bsn_cc%i_fpwet == 2) call wet_fp_init
  
  call proc_cal
  
  call proc_open
  
  ! Now, after the initialization, call the subroutine to create the HDF5 file, groups, and dataset
  call create_hdf5_output

  ! compute unit hydrograph parameters for subdaily runoff
  call unit_hyd_ru_hru

  call dr_ru
    
  call hyd_connect_out
  
  ! save initial time settings for soft calibration runs
  time_init = time

  ! simulate watershed processes
  if (time%step < 0) then
    ! export coefficient - average annual
    time%end_sim = 1
    call command
  else
    call time_control
  end if
  
  if (cal_soft == "y") call calsoft_control
  
  if (cal_hard == "y") then
    deallocate (cal_upd)
    call cal_parmchg_read
    call calhard_control
  end if
  
  ! write output for SWIFT input
  if (bsn_cc%swift_out == 1) call swift_output

  ! Call the subroutine to close the HDF5 resources
  call close_hdf5_resources
       
  ! write successful completion to screen and file
  if (mpi_rank == 0) then
     write (*,1001)
     write (9003,1001)
     open (107,file="success.fin")
     
     call DATE_AND_TIME (b(1), b(2), b(3), date_time)
     write (*,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7)
     write (9003,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7)
  end if
1234  format(/,"  Date of Sim", 2x,i2,"/",i2,"/",i4, " Time",2x,i2,":",i2,":",i2)
        
  if (mpi_rank == 0) write (107,1001)     
1001 format (/," Execution successfully completed ")

  if (mpi_rank == 0) then
   ! Print the total time to the screen using print *
   print *, "Total time to complete hru_output for all ihru: ", total_time, " seconds"
  end if

  ! Finalize MPI
  call MPI_FINALIZE(ierr)

  stop      
end program main

