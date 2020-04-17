
prj_project new -name "uart_spi" -impl "impl1" -dev LCMXO2-1200HC-5TG144C -synthesis "lse"
prj_src add "/bridge.vhd" "/sclk_gen.vhd" "/spi_data_path.vhd" "/spi_master.vhd" "/uart.vhd"
prj_project save
prj_run Synthesis -impl impl1
prj_project close
