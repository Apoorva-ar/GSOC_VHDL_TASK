----------------------------------------------------------------------------
-- Auther : Apoorva Arora
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
--
--  This top module describes UART and SPI master Bridge
--  UART receives an Address packet which consists of information about the meassage:-
--  ADDRESS (7 downto 0) --> bits(7 and 6) = slave address
--                       --> bit 5         = read/write transaction
--                       --> bits (4 to 0) = number of bytes in the message
----------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY UART_SPI_Bridge IS
    GENERIC (
        DATA_SIZE : INTEGER := 16
    );
    PORT (
        clk_sys   : IN STD_LOGIC; --system clock
        reset_sys : IN STD_LOGIC; --ascynchronous reset

        SPI_CSN_port  : OUT std_logic_vector(3 DOWNTO 0);
        SPI_MOSI_port : OUT std_logic;
        SPI_MISO_port : IN std_logic;
        SPI_clk_port  : OUT std_logic;

        UART_tx_port : OUT STD_LOGIC; --UART tx
        UART_rx_port : IN STD_LOGIC); --UART rx

END UART_SPI_Bridge;

ARCHITECTURE behavioral OF UART_SPI_Bridge IS
    ----------------SPI Component -------------------------------------
    COMPONENT spi_master
        GENERIC (
            DATA_SIZE : INTEGER := 16
        );
        PORT (
            system_clock : IN std_logic; -- system clock
            system_reset : IN std_logic; -- system reset
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
    END COMPONENT;
    ----------------UART Component -------------------------------------
    COMPONENT uart
        PORT (
            clk     : IN STD_LOGIC;                    --system clock
            reset   : IN STD_LOGIC;                    --ascynchronous reset
            tx_en   : IN STD_LOGIC;                    --initiate transmission
            tx_data : IN STD_LOGIC_VECTOR(7 DOWNTO 0); --data to transmit
            tx_busy : OUT STD_LOGIC;

            rx       : IN STD_LOGIC;  --receive pin
            rx_busy  : OUT STD_LOGIC; --data reception in progress
            new_data : OUT STD_LOGIC; --FLAG FOR I2C TO START TRANSACTION

            rx_error : OUT STD_LOGIC;                    --start, parity, or stop bit error detected
            rx_data  : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); --data received
            tx       : OUT STD_LOGIC);                   --transmit pin
    END COMPONENT;
    ---------------------------------------------------------------------
    TYPE state_machine IS(address, data, read_SPI_state, write_SPI_state);
    SIGNAL state                : state_machine; -- state machine
    SIGNAL SPI_data_chip_select : std_logic;
    SIGNAL SPI_input_reg        : std_logic_vector(15 DOWNTO 0);
    SIGNAL SPI_data_valid       : std_logic;
    SIGNAL SPI_read_enable      : std_logic;
    SIGNAL SPI_output_reg       : std_logic_vector(15 DOWNTO 0);
    SIGNAL SPI_tx_ready         : std_logic;
    SIGNAL SPI_rx_ready         : std_logic;
    SIGNAL SPI_tx_error         : std_logic;
    SIGNAL SPI_rx_error         : std_logic;
    SIGNAL SPI_system_interrupt : std_logic;
    SIGNAL SPI_slave_address    : std_logic_vector(1 DOWNTO 0);
    SIGNAL SPI_CPOL             : std_logic := '1';
    SIGNAL SPI_CPHA             : std_logic := '0';
    SIGNAL SPI_LSB              : std_logic := '0';
    SIGNAL SPI_start            : std_logic;
    SIGNAL SPI_clk_cycles       : std_logic_vector(7 DOWNTO 0) := "00000001";
    SIGNAL SPI_setup_cycles     : std_logic_vector(7 DOWNTO 0) := "00000001";
    SIGNAL SPI_hold_cycles      : std_logic_vector(7 DOWNTO 0) := "00000001";
    SIGNAL new_SPI_wait_cycles  : std_logic_vector(7 DOWNTO 0) := "00000001";
    SIGNAL UART_new_data        : std_logic;
    SIGNAL UART_tx_en           : std_logic;
    SIGNAL UART_tx_data_Reg     : std_logic_vector(7 DOWNTO 0);
    SIGNAL UART_rx_data_reg     : std_logic_vector(7 DOWNTO 0);
    SIGNAL UART_tx_busy         : std_logic;
    SIGNAL UART_rx_busy         : std_logic;
    SIGNAL COMMAND              : std_logic;
    SIGNAL msg_len              : std_logic_vector(31 DOWNTO 0);
BEGIN

    ----------------------------------PORT MAPPING -------------------------
    C1 : spi_master
    GENERIC MAP(
        DATA_SIZE => DATA_SIZE)
    PORT MAP(
        system_clock => clk_sys,
        system_reset => reset_sys,
        ---- SPI Bus INterface
        SPI_CSN  => SPI_CSN_port,
        SPI_MOSI => SPI_MOSI_port,
        SPI_MISO => SPI_MISO_port,
        SPI_clk  => SPI_clk_port,
        --- SPI Master User INterface
        master_chip_select => SPI_data_chip_select, --
        input_data_reg     => SPI_input_reg,
        data_write_en      => SPI_data_valid,
        data_read_en       => SPI_read_enable,
        output_data_reg    => SPI_output_reg,
        --- Status Flags
        tx_ready         => SPI_tx_ready,
        rx_ready         => SPI_rx_ready,
        tx_error         => SPI_tx_error,
        rx_error         => SPI_rx_error,
        system_interrupt => SPI_system_interrupt,
        --- SPI master configuration registers
        SPI_slave_select  => SPI_slave_address,
        SPI_CPOL          => SPI_CPOL,
        SPI_CPHA          => SPI_CPHA,
        LSB_first_flag    => SPI_LSB,
        SPI_master_start  => SPI_start,
        clock_period      => SPI_clk_cycles,
        setup_time_cycles => SPI_setup_cycles,
        hold_time_cycles  => SPI_hold_cycles,
        tx_wait_cycles    => new_SPI_wait_cycles
    );
    C2 : uart PORT MAP
    (
        clk      => clk_sys,
        reset    => reset_sys,
        tx_en    => UART_tx_en,
        tx_data  => UART_tx_data_Reg,
        tx_busy  => UART_tx_busy,
        rx       => UART_rx_port,
        rx_busy  => UART_rx_busy,
        new_data => UART_new_data,
        rx_data  => UART_rx_data_reg,
        tx       => UART_tx_port
    );
    -----------------------------------------------------------------------
    ---------------- Bridge FSM -------------------------------------------
    -----------------------------------------------------------------------
    PROCESS (sys_clk, reset_sys)
        VARIABLE cntr : INTEGER := 0;
    BEGIN
        IF (reset_sys = '1') THEN
            state                <= address; --go so state address and check for UART enabled data
            COMMAND              <= '0';
            SPI_start            <= '0';
            SPI_data_chip_select <= '1';
            SPI_read_enable      <= '0';
            SPI_write_enable     <= '0';
            UART_tx_en           <= '0';
            UART_tx_data_Reg     <= (OTHERS => '0');
            UART_rx_en           <= '0';
            UART_rx_data_Reg     <= (OTHERS => '0');
            msg_len              <= (OTHERS => '0');
            cntr := 0;
        ELSE
            IF (rising_edge(sys_clk)) THEN
            BEGIN
                CASE state IS
                    WHEN address =>
                        IF (UART_new_data = '1') THEN                      -- New data arrived from UART
                            SPI_slave_address <= UART_rx_data_reg(7 DOWNTO 6); -- store address
                            COMMAND           <= UART_rx_data_reg(5);          -- read/write SPI command
                            msg_len           <= UART_rx_data_reg(4 DOWNTO 0); -- number of bytes in message
                            state             <= data;                         -- change state to data state
                            SPI_start         <= '1';                          -- start SPI transaction
                        END IF;
                        SPI_data_chip_select <= '1';
                    WHEN data =>
                        SPI_start <= '0';       -- Disable start SPI transaction
                        IF (COMMAND = '0') THEN --READ transaction
                            IF (UART_new_data = '1') THEN
                                SPI_data_chip_select <= '0'; -- master chip select LOW
                                SPI_read_enable      <= '1'; -- read_enable
                                state                <= read_SPI_state;
                            END IF;
                        ELSE -- WRITE transaction
                            IF (UART_new_data = '1') THEN
                                SPI_data_chip_select <= '0'; -- master chip select LOW
                                SPI_write_enable     <= '1'; -- read_enable
                                state                <= write_SPI_state;
                            END IF;
                        END IF;
                    WHEN read_SPI_state =>
                        IF UART_tx_busy = '0' THEN
                            UART_tx_en       <= '1';           -- valid data flag HIGH
                            UART_tx_data_Reg <= SPI_input_reg; -- Latch in data
                            IF cntr = msg_len THEN             -- if last byte transaction
                                state <= address;                  -- change state back to address state
                                cntr := 0;
                            ELSE
                                cntr := cntr + 1;
                            END IF;
                        END IF;
                    WHEN write_SPI_state =>
                        IF UART_rx_busy = '0' THEN
                            UART_rx_en       <= '1';            -- valid data flag HIGH
                            UART_rx_data_Reg <= SPI_output_reg; -- Latch in data
                            IF cntr = msg_len THEN
                                state <= address; -- change state back to address state
                                cntr := 0;
                            ELSE
                                cntr := cntr + 1;
                            END IF;
                        END IF;
                END CASE;
            END IF;
        END IF;
    END PROCESS;
END behavioral;