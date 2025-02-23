      subroutine proc_read
      use input_file_module, only : in_snodas
      implicit none

      call cli_read_atmodep
      call cli_staread

      call constit_db_read
      call pest_metabolite_read     !! read pesticide metabolites
      call soil_plant_init
      call solt_db_read
      call pest_hru_aqu_read
      call path_hru_aqu_read
      call hmet_hru_aqu_read
      call salt_hru_aqu_read

      call topo_read
      call field_read
      call hydrol_read

      if (in_snodas%flag == 1) then
            write(9001,*) 'Reading snow data from snodas'
            call snowdb_read_snodas
      else
            write(9001,*) 'Reading snow data from defualt snow file'
            call snowdb_read
      end if
      
      call soil_db_read
      call soil_lte_db_read
      
	  return
      
      end subroutine proc_read