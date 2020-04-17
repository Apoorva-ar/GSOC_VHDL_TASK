----------------------------------------------------------------------------
-- Auther : Apoorva Arora
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version. 

----------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY spi_master IS
    GENERIC (
        DATA_SIZE : INTEGER := 8
    );
    PORT (
        system_clock     : IN std_logic; -- system clock
        system_reset_spi : IN std_logic; -- system reset
        ---- SPI Bus INterface
        SPI_CSN  : OUT std_logic_vector(3 DOWNTO 0);
        SPI_MOSI : OUT std_logic;
        SPI_MISO : IN std_logic;
        SPI_clk  : OUT std_logic;
        --- SPI Master User INterface
        master_chip_select : IN std_logic;                                -- chip select for SPI master
        input_data_reg     : IN std_logic_vector(DATA_SIZE - 1 DOWNTO 0); -- Input data
        data_write_en      : IN std_logic;
        data_read_en       : IN std_logic;
        output_data_reg    : OUT std_logic_vector(DATA_SIZE - 1 DOWNTO 0); -- output data
        --- Status Flags
        tx_ready         : OUT std_logic; -- Transmitter ready 
        rx_ready         : OUT std_logic; -- Receiver ready
        tx_error         : OUT std_logic; -- Transmitter error
        rx_error         : OUT std_logic; -- Receiver error
        system_interrupt : OUT std_logic;
        --- SPI master configuration registers
        SPI_slave_select  : IN std_logic_vector(1 DOWNTO 0); -- Slave select
        SPI_CPOL          : IN std_logic;                    -- CPOL value - 0 or 1
        SPI_CPHA          : IN std_logic;                    -- CPHA value - 0 or 1 
        LSB_first_flag    : IN std_logic;                    -- lsb first when '1' else MSB first
        SPI_master_start  : IN std_logic;                    -- START SPI Master Transactions
        clock_period      : IN std_logic_vector(7 DOWNTO 0); -- SPI clock divide w.r.t system clock
        setup_time_cycles : IN std_logic_vector(7 DOWNTO 0); -- SPI clock setup time w.r.t system clock
        hold_time_cycles  : IN std_logic_vector(7 DOWNTO 0); -- SPI clock hold time w.r.t system clock
        tx_wait_cycles    : IN std_logic_vector(7 DOWNTO 0)  -- Inter transmission wait time w.r.t system clock
    );
END spi_master;

ARCHITECTURE behavioral OF spi_master IS

    COMPONENT sclk_gen
        GENERIC (
            DATA_SIZE : INTEGER);
        PORT (
            ------- system inputs --------------
            sys_clk : IN std_logic;
            reset   : IN std_logic;
            ------ input settings --------------
            start_spi            : IN std_logic;
            CPOL_sclk            : IN std_logic;
            half_clk_count       : IN std_logic_vector(7 DOWNTO 0);
            setup_cycles         : IN std_logic_vector(7 DOWNTO 0);
            hold_cycles          : IN std_logic_vector(7 DOWNTO 0);
            transmit_wait_cycles : IN std_logic_vector(7 DOWNTO 0);
            ------ output ----------------------
            master_start : OUT std_logic;
            sclk_out     : OUT std_logic
        );
    END COMPONENT;

    COMPONENT spi_data_path
        GENERIC (
            DATA_SIZE : INTEGER);
        PORT (
            clk_sys        : IN std_logic;                                -- system clock
            reset          : IN std_logic;                                -- system reset
            csn            : IN std_logic;                                -- Master chip select active low
            data_input     : IN std_logic_vector(DATA_SIZE - 1 DOWNTO 0); -- Input data
            wr             : IN std_logic;                                -- Write data enable
            rd             : IN std_logic;                                -- Read data enable
            spi_start_flag : IN std_logic;
            data_output    : OUT std_logic_vector(DATA_SIZE - 1 DOWNTO 0); -- output data
            tx_ready_flag  : OUT std_logic;                                -- Transmitter ready 
            rx_ready_flag  : OUT std_logic;                                -- Receiver ready
            tx_error_flag  : OUT std_logic;                                -- Transmitter error
            rx_error_flag  : OUT std_logic;                                -- Receiver error
            CPOL           : IN std_logic;                                 -- SCLK Polarity
            CPHA           : IN std_logic;                                 -- SCLK Phase
            LSB_flag       : IN std_logic;
            Interrrupt     : OUT std_logic;
            MOSI           : OUT std_logic; -- SPI Master out Slave in
            MISO           : IN std_logic;  -- SPI Master in Slave out 
            ssn            : IN std_logic;  -- SPI Slave chip select 
            SCLK           : IN std_logic   -- SPI Clcok
        );

    END COMPONENT;

    SIGNAL sclk_signal       : std_logic;
    SIGNAL slave_chip_select : std_logic;
BEGIN

    SPI_clk <= sclk_signal;

    SPI_clock_gen : sclk_gen
    GENERIC MAP(
        DATA_SIZE => DATA_SIZE)
    PORT MAP(
        sys_clk              => system_clock,
        reset                => system_reset_spi,
        start_spi            => SPI_master_start,
        half_clk_count       => clock_period,
        setup_cycles         => setup_time_cycles,
        hold_cycles          => hold_time_cycles,
        transmit_wait_cycles => tx_wait_cycles,
        CPOL_sclk            => SPI_CPOL,
        master_start         => slave_chip_select,
        sclk_out             => sclk_signal
    );

    SPI_Data_transaction : spi_data_path
    GENERIC MAP(
        DATA_SIZE => DATA_SIZE)
    PORT MAP(
        clk_sys        => system_clock,
        reset          => system_reset_spi,
        csn            => master_chip_select,
        data_input     => input_data_reg,
        wr             => data_write_en,
        rd             => data_read_en,
        spi_start_flag => SPI_master_start,
        data_output    => output_data_reg,
        tx_ready_flag  => tx_ready,
        rx_ready_flag  => rx_ready,
        tx_error_flag  => tx_error,
        rx_error_flag  => rx_error,
        Interrrupt     => system_interrupt,
        CPOL           => SPI_CPOL,
        CPHA           => SPI_CPHA,
        LSB_flag       => LSB_first_flag,
        MOSI           => SPI_MOSI,
        MISO           => SPI_MISO,
        ssn            => slave_chip_select,
        SCLK           => sclk_signal
    );

    PROCESS (system_clock, system_reset_spi)
    BEGIN
        IF system_reset_spi = '1' THEN
            SPI_CSN <= (OTHERS => '1');
        ELSIF rising_edge(system_clock) THEN
            CASE (SPI_slave_select) IS
                WHEN "00" =>
                    SPI_CSN <= "111" & slave_chip_select;
                WHEN "01" =>
                    SPI_CSN <= "11" & slave_chip_select & '1';
                WHEN "10" =>
                    SPI_CSN <= '1' & slave_chip_select & "11";
                WHEN "11" =>
                    SPI_CSN <= slave_chip_select & "111";
                WHEN OTHERS =>
                    SPI_CSN <= "1111";
            END CASE;
        END IF;
    END PROCESS;
END behavioral;
