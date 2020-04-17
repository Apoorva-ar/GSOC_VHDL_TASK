
prj_project new -name "uart_spi" -impl "impl1" -dev LCMXO2-1200HC-5TG144C -synthesis "lse"
prj_src add "C:/Users/apoorva/Desktop/gsoc_task/GSOC_VHDL_TASK-master/bridge.vhd" "C:/Users/apoorva/Desktop/gsoc_task/GSOC_VHDL_TASK-master/sclk_gen.vhd" "C:/Users/apoorva/Desktop/gsoc_task/GSOC_VHDL_TASK-master/spi_data_path.vhd" "C:/Users/apoorva/Desktop/gsoc_task/GSOC_VHDL_TASK-master/spi_master.vhd" "C:/Users/apoorva/Desktop/gsoc_task/GSOC_VHDL_TASK-master/uart.vhd"
prj_project save
prj_run Synthesis -impl impl1
prj_project close