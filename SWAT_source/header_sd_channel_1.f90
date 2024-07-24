subroutine header_sd_channel

  use sd_channel_module
  use basin_module
  use hydrograph_module
  use hdf5
  
  implicit none

  ! HDF5 variables
  integer :: file_id, dataset_id, dataspace_id, prop, status
  integer(hsize_t) :: dims(1)
  character(len=100) :: hdf5_file

  ! Initialize HDF5
  call h5open_f(status)

  ! Define the HDF5 file name
  hdf5_file = "channel_sd.h5"
  
  ! Create a new HDF5 file
  call h5fcreate_f(hdf5_file, H5F_ACC_TRUNC_F, file_id, status)

!!! SWAT-DEG CHANNEL - SUBDAILY OUTPUT
  if (sp_ob%chandeg > 0) then
    if (pco%sd_chan%d == "y") then
      if (time%step > 0.) then
        !!!!!!!! SD_CHANNEL
        ! Create dataspace and dataset for subdaily channel data
        dims = 1
        call h5screate_simple_f(1, dims, dataspace_id, status)
        call h5dcreate_f(file_id, "subdaily/channel_sd", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

        ! Write data to dataset
        ! Here, we assume sdch_hdr_subday and other variables are arrays that hold the data
        call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, sdch_hdr_subday, dims, status)

        ! Close dataset and dataspace
        call h5dclose_f(dataset_id, status)
        call h5sclose_f(dataspace_id, status)

        ! Write additional metadata or units if necessary
        ! ...
      end if
    end if

    ! Create dataspace and dataset for daily channel data
    dims = 1
    call h5screate_simple_f(1, dims, dataspace_id, status)
    call h5dcreate_f(file_id, "daily/channel_sd", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

    ! Write data to dataset
    call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, ch_wbod_hdr, dims, status)

    ! Close dataset and dataspace
    call h5dclose_f(dataset_id, status)
    call h5sclose_f(dataspace_id, status)

    ! Monthly channel data
    if (pco%sd_chan%m == "y") then
      dims = 1
      call h5screate_simple_f(1, dims, dataspace_id, status)
      call h5dcreate_f(file_id, "monthly/channel_sd", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

      call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, ch_wbod_hdr, dims, status)

      call h5dclose_f(dataset_id, status)
      call h5sclose_f(dataspace_id, status)
    end if

    ! Yearly channel data
    if (pco%sd_chan%y == "y") then
      dims = 1
      call h5screate_simple_f(1, dims, dataspace_id, status)
      call h5dcreate_f(file_id, "yearly/channel_sd", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

      call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, ch_wbod_hdr, dims, status)

      call h5dclose_f(dataset_id, status)
      call h5sclose_f(dataspace_id, status)
    end if

    ! All available years channel data
    if (pco%sd_chan%a == "y") then
      dims = 1
      call h5screate_simple_f(1, dims, dataspace_id, status)
      call h5dcreate_f(file_id, "all_years/channel_sd", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

      call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, ch_wbod_hdr, dims, status)

      call h5dclose_f(dataset_id, status)
      call h5sclose_f(dataspace_id, status)
    end if
  end if

!!!!!!!! SD_CHANMORPH
  if (sp_ob%chandeg > 0) then
    if (pco%sd_chan%d == "y") then
      dims = 1
      call h5screate_simple_f(1, dims, dataspace_id, status)
      call h5dcreate_f(file_id, "daily/channel_sdmorph", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

      call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, sdch_hdr, dims, status)

      call h5dclose_f(dataset_id, status)
      call h5sclose_f(dataspace_id, status)
    end if

    if (pco%sd_chan%m == "y") then
      dims = 1
      call h5screate_simple_f(1, dims, dataspace_id, status)
      call h5dcreate_f(file_id, "monthly/channel_sdmorph", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

      call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, sdch_hdr, dims, status)

      call h5dclose_f(dataset_id, status)
      call h5sclose_f(dataspace_id, status)
    end if

    if (pco%sd_chan%y == "y") then
      dims = 1
      call h5screate_simple_f(1, dims, dataspace_id, status)
      call h5dcreate_f(file_id, "yearly/channel_sdmorph", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

      call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, sdch_hdr, dims, status)

      call h5dclose_f(dataset_id, status)
      call h5sclose_f(dataspace_id, status)
    end if

    if (pco%sd_chan%a == "y") then
      dims = 1
      call h5screate_simple_f(1, dims, dataspace_id, status)
      call h5dcreate_f(file_id, "all_years/channel_sdmorph", H5T_NATIVE_REAL, dataspace_id, dataset_id, status)

      call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, sdch_hdr, dims, status)

      call h5dclose_f(dataset_id, status)
      call h5sclose_f(dataspace_id, status)
    end if
  end if

  ! Close HDF5 file
  call h5fclose_f(file_id, status)

  ! Close HDF5
  call h5close_f(status)

  return
end subroutine header_sd_channel
