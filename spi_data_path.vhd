----------------------------------------------------------------------------

--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version. 

----------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY spi_data_path IS
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

END spi_data_path;

ARCHITECTURE behavioral OF spi_data_path IS
    SIGNAL data_in_reg_i             : std_logic_vector(DATA_SIZE - 1 DOWNTO 0);
    SIGNAL data_in                   : std_logic_vector(DATA_SIZE - 1 DOWNTO 0);
    SIGNAL rxdata_register           : std_logic_vector(DATA_SIZE - 1 DOWNTO 0);
    SIGNAL txdata_register           : std_logic_vector(DATA_SIZE - 1 DOWNTO 0);
    SIGNAL rx_data_rising_edge       : std_logic_vector(DATA_SIZE - 1 DOWNTO 0);
    SIGNAL rx_data_falling_edge      : std_logic_vector(DATA_SIZE - 1 DOWNTO 0);
    SIGNAL tx_error                  : std_logic;
    SIGNAL rx_error                  : std_logic;
    SIGNAL tx_ready                  : std_logic;
    SIGNAL rx_ready                  : std_logic;
    SIGNAL d1_TxReady_i              : std_logic;
    SIGNAL d2_TxReady_i              : std_logic;
    SIGNAL d1_RxReady_i              : std_logic;
    SIGNAL d2_RxReady_i              : std_logic;
    SIGNAL MOSI_00                   : std_logic;
    SIGNAL MOSI_01                   : std_logic;
    SIGNAL MOSI_10                   : std_logic;
    SIGNAL MOSI_11                   : std_logic;
    SIGNAL rx_cntr_done_rising_edge  : std_logic;
    SIGNAL rx_cntr_done_falling_edge : std_logic;
    SIGNAL rx_done_1                 : std_logic;
    SIGNAL rx_done_2                 : std_logic;
    SIGNAL rx_done_3                 : std_logic;
    SIGNAL tx_cntr_done_rising_edge  : std_logic;
    SIGNAL tx_cntr_done_falling_edge : std_logic;
    SIGNAL tx_done_1                 : std_logic;
    SIGNAL tx_done_2                 : std_logic;
    SIGNAL tx_done_3                 : std_logic;
    SIGNAL rx_data_cntr_rising_edge  : std_logic_vector(5 DOWNTO 0);
    SIGNAL rx_data_cntr_falling_edge : std_logic_vector(5 DOWNTO 0);
    SIGNAL tx_data_cntr_rising_edge  : std_logic_vector(5 DOWNTO 0);
    SIGNAL tx_data_cntr_falling_edge : std_logic_vector(5 DOWNTO 0);

BEGIN

    tx_ready_flag <= tx_ready;
    rx_ready_flag <= rx_ready;
    tx_error_flag <= tx_error;
    rx_error_flag <= rx_error;
    data_output   <= rxdata_register;

    -------------------------------------------------------------
    ---------------- Latch in new input data---------------------
    -------------------------------------------------------------

    PROCESS (clk_sys, reset)
    BEGIN
        IF (reset = '1') THEN
            data_in         <= (OTHERS => '0');
            txdata_register <= (OTHERS => '0');
        ELSIF rising_edge(clk_sys) THEN
            IF (wr = '1' AND csn = '0' AND tx_ready = '1') THEN
                data_in         <= data_input;
                txdata_register <= data_in;
            END IF;
        END IF;
    END PROCESS;
    -------------------------------------------------------------
    --------------- Latch in new output data---------------------
    -------------------------------------------------------------
    PROCESS (clk_sys, reset)
    BEGIN
        IF (reset = '1') THEN
            rxdata_register <= (OTHERS => '0');
        ELSIF rising_edge(clk_sys) THEN
            IF (rx_done_1 = '1' AND rx_done_2 = '0') THEN
                IF ((CPOL = '0' AND CPHA = '0') OR (CPOL = '1' AND CPHA = '1')) THEN
                    rxdata_register <= rx_data_rising_edge;
                ELSE
                    rxdata_register <= rx_data_falling_edge;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    -------------------------------------------------------------
    -------------- SPI Transmit Transaction ---------------------
    -------------------------------------------------------------

    -------------------------------------------------------------
    --------------- CPOL, CPHA = (0,0) --------------------------
    -------------------------------------------------------------

    PROCESS (txdata_register, tx_data_cntr_falling_edge, LSB_flag)
    BEGIN
        IF (LSB_flag = '1') THEN
            MOSI_00 <= txdata_register(conv_integer(tx_data_cntr_falling_edge));
        ELSE
            MOSI_00 <= txdata_register(conv_integer(DATA_SIZE - tx_data_cntr_falling_edge - 1));
        END IF;
    END PROCESS;
    -------------------------------------------------------------
    --------------- CPOL, CPHA = (1,0) --------------------------
    -------------------------------------------------------------

    PROCESS (txdata_register, tx_data_cntr_rising_edge, LSB_flag)
    BEGIN
        IF (LSB_flag = '1') THEN
            MOSI_10 <= txdata_register(conv_integer(tx_data_cntr_rising_edge));
        ELSE
            MOSI_10 <= txdata_register(conv_integer(DATA_SIZE - tx_data_cntr_rising_edge - 1));
        END IF;
    END PROCESS;

    -------------------------------------------------------------
    --------------- CPOL, CPHA = (0,1) --------------------------
    -------------------------------------------------------------
    PROCESS (SCLK, reset)
    BEGIN
        IF reset = '1' THEN
            MOSI_01 <= '1';
        ELSIF rising_edge(SCLK) THEN
            IF (LSB_flag = '1') THEN
                MOSI_01 <= txdata_register(conv_integer(tx_data_cntr_rising_edge));
            ELSE
                MOSI_01 <= txdata_register(conv_integer(DATA_SIZE - tx_data_cntr_rising_edge - 1));
            END IF;
        END IF;
    END PROCESS;

    -------------------------------------------------------------
    --------------- CPOL, CPHA = (1,1) --------------------------
    -------------------------------------------------------------

    PROCESS (SCLK, reset)
    BEGIN
        IF reset = '1' THEN
            MOSI_11 <= '1';
        ELSIF falling_edge(SCLK) THEN
            IF (LSB_flag = '1') THEN
                MOSI_11 <= txdata_register(conv_integer(tx_data_cntr_falling_edge));
            ELSE
                MOSI_11 <= txdata_register(conv_integer(DATA_SIZE - tx_data_cntr_falling_edge - 1));
            END IF;
        END IF;
    END PROCESS;
    --------------------------------------------------------------------------
    --- TX Data counter @falling edge of SCLK (CPOL,CPHA) = (0,0) or (1,1) ---
    --------------------------------------------------------------------------

    PROCESS (SCLK, reset)
    BEGIN
        IF (reset = '1') THEN
            tx_data_cntr_falling_edge <= (OTHERS => '0');
            tx_cntr_done_falling_edge <= '0';
        ELSIF falling_edge(SCLK) THEN
            IF (tx_data_cntr_falling_edge = DATA_SIZE - 1) THEN
                tx_data_cntr_falling_edge <= (OTHERS => '0');
                tx_cntr_done_falling_edge <= '1';
            ELSIF (ssn = '0') THEN
                tx_data_cntr_falling_edge <= tx_data_cntr_falling_edge + 1;
                tx_cntr_done_falling_edge <= '0';
            END IF;
        END IF;
    END PROCESS;
    --------------------------------------------------------------------------
    --- TX Data counter @falling edge of SCLK (CPOL,CPHA) = (1,0) or (0,1) ---
    --------------------------------------------------------------------------
    PROCESS (SCLK, reset)
    BEGIN
        IF (reset = '1') THEN
            tx_data_cntr_rising_edge <= (OTHERS => '0');
            tx_cntr_done_rising_edge <= '0';
        ELSIF rising_edge(SCLK) THEN
            IF (tx_data_cntr_rising_edge = DATA_SIZE - 1) THEN
                tx_data_cntr_rising_edge <= (OTHERS => '0');
                tx_cntr_done_rising_edge <= '1';
            ELSIF (ssn = '0') THEN
                tx_data_cntr_rising_edge <= tx_data_cntr_rising_edge + 1;
                tx_cntr_done_rising_edge <= '0';
            END IF;
        END IF;
    END PROCESS;
    --------------------------------------------------------------------------
    ----------------------------- MOSI update --------------------------------
    --------------------------------------------------------------------------
    PROCESS (ssn, CPOL, CPHA, MOSI_00, MOSI_01, MOSI_10, MOSI_11)
    BEGIN
        IF (ssn = '0') THEN
            IF (CPOL = '0' AND CPHA = '0') THEN
                MOSI <= MOSI_00;
            ELSIF (CPOL = '0' AND CPHA = '1') THEN
                MOSI <= MOSI_01;
            ELSIF (CPOL = '1' AND CPHA = '0') THEN
                MOSI <= MOSI_10;
            ELSE
                MOSI <= MOSI_11;
            END IF;
        ELSE
            MOSI <= '0';
        END IF;
    END PROCESS;

    --------------------------------------------------------------------------
    ----------------------------- tx_done Flag update ------------------------
    --------------------------------------------------------------------------
    PROCESS (clk_sys, reset)
    BEGIN
        IF (reset = '1') THEN
            tx_done_1 <= '0';
            tx_done_2 <= '0';
            tx_done_3 <= '0';
        ELSIF rising_edge(clk_sys) THEN
            IF (CPOL = '0' AND CPHA = '0') OR (CPOL = '1' AND CPHA = '1') THEN
                tx_done_1 <= tx_cntr_done_falling_edge;
            ELSE
                tx_done_1 <= tx_cntr_done_rising_edge;
            END IF;
            tx_done_2 <= tx_done_1;
            tx_done_3 <= tx_done_2;
        END IF;
    END PROCESS;

    --------------------------------------------------------------------------
    ---------------------------- tx_ready Flag update ------------------------
    --------------------------------------------------------------------------

    PROCESS (clk_sys, reset)
    BEGIN
        IF (reset = '1') THEN
            tx_ready <= '1';
        ELSIF rising_edge(clk_sys) THEN
            IF (spi_start_flag = '1') THEN
                tx_ready <= '0';
            ELSIF (tx_done_2 = '1' AND tx_done_3 = '0') THEN
                tx_ready <= '1';
            END IF;
        END IF;
    END PROCESS;

    --------------------------------------------------------------------------
    ----------------------------- tx_error Flag update -----------------------
    --------------------------------------------------------------------------
    PROCESS (clk_sys, reset)
    BEGIN
        IF (reset = '1') THEN
            tx_error <= '0';
        ELSIF rising_edge(clk_sys) THEN
            IF (tx_ready = '0' AND wr = '1' AND csn = '0') THEN
                tx_error <= '1';
            ELSIF (wr = '1' AND csn = '0') THEN
                tx_error <= '0';
            END IF;
        END IF;
    END PROCESS;

    -------------------------------------------------------------
    --------------- SPI Receive Transaction ---------------------
    -------------------------------------------------------------

    ------------------------------------------------------------------------
    --- MOSI Sampling @rising edge of SCLK (CPOL,CPHA) = (0,0) or (1,1) ----
    ------------------------------------------------------------------------

    PROCESS (SCLK, reset)
    BEGIN
        IF (reset = '1') THEN
            rx_data_rising_edge <= (OTHERS => '0');
        ELSIF rising_edge(SCLK) THEN
            IF (ssn = '0' AND ((CPOL = '0' AND CPHA = '0') OR (CPOL = '1' AND CPHA = '1'))) THEN
                IF (LSB_flag = '1') THEN
                    rx_data_rising_edge <= MISO & rx_data_rising_edge(DATA_SIZE - 1 DOWNTO 1);
                ELSE
                    rx_data_rising_edge <= rx_data_rising_edge(DATA_SIZE - 2 DOWNTO 0) & MISO;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (SCLK, reset)
    BEGIN
        IF (reset = '1') THEN
            rx_data_cntr_rising_edge <= (OTHERS => '0');
            rx_cntr_done_rising_edge <= '0';
        ELSIF rising_edge(SCLK) THEN
            IF (ssn = '0' AND ((CPOL = '0' AND CPHA = '0') OR (CPOL = '1' AND CPHA = '1'))) THEN
                IF (rx_data_cntr_rising_edge = DATA_SIZE - 1) THEN
                    rx_data_cntr_rising_edge <= (OTHERS => '0');
                    rx_cntr_done_rising_edge <= '1';
                ELSIF (ssn = '0') THEN
                    rx_data_cntr_rising_edge <= rx_data_cntr_rising_edge + 1;
                    rx_cntr_done_rising_edge <= '0';
                END IF;
            END IF;
        END IF;
    END PROCESS;

    ------------------------------------------------------------------------
    --- MOSI Sampling @falling edge of SCLK (CPOL,CPHA) = (1,0) or (0,1) ---
    ------------------------------------------------------------------------
    PROCESS (SCLK, reset)
    BEGIN
        IF (reset = '1') THEN
            rx_data_falling_edge <= (OTHERS => '0');
        ELSIF falling_edge(SCLK) THEN
            IF (ssn = '0' AND ((CPOL = '1' AND CPHA = '0') OR (CPOL = '0' AND CPHA = '1'))) THEN
                IF (LSB_flag = '1') THEN
                    rx_data_falling_edge <= MISO & rx_data_falling_edge(DATA_SIZE - 1 DOWNTO 1);
                ELSE
                    rx_data_falling_edge <= rx_data_falling_edge(DATA_SIZE - 2 DOWNTO 0) & MISO;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (SCLK, reset)
    BEGIN
        IF (reset = '1') THEN
            rx_data_cntr_falling_edge <= (OTHERS => '0');
            rx_cntr_done_falling_edge <= '0';
        ELSIF falling_edge(SCLK) THEN
            IF (rx_data_cntr_falling_edge = DATA_SIZE - 1) THEN
                rx_data_cntr_falling_edge <= (OTHERS => '0');
                rx_cntr_done_falling_edge <= '1';
            ELSIF (ssn = '0') THEN
                rx_data_cntr_falling_edge <= rx_data_cntr_falling_edge + 1;
                rx_cntr_done_falling_edge <= '0';
            END IF;
        END IF;
    END PROCESS;
    ----------------------------------------------------------------------------
    ---------------------- RX Done Flag update ---------------------------------
    ----------------------------------------------------------------------------
    PROCESS (clk_sys, reset)
    BEGIN
        IF (reset = '1') THEN
            rx_done_1 <= '0';
            rx_done_2 <= '0';
            rx_done_3 <= '0';
        ELSIF rising_edge(clk_sys) THEN
            IF (ssn = '0' AND ((CPOL = '0' AND CPHA = '0') OR (CPOL = '1' AND CPHA = '1'))) THEN
                rx_done_1 <= rx_cntr_done_rising_edge;
            ELSE
                rx_done_1 <= rx_cntr_done_falling_edge;
            END IF;
            rx_done_2 <= rx_done_1;
            rx_done_3 <= rx_done_2;
        END IF;
    END PROCESS;
    ----------------------------------------------------------------------------
    ---------------------- RX Ready Flag update --------------------------------
    ----------------------------------------------------------------------------

    PROCESS (clk_sys, reset)
    BEGIN
        IF (reset = '1') THEN
            rx_ready <= '0';
        ELSIF rising_edge(clk_sys) THEN
            IF (rx_done_2 = '1' AND rx_done_3 = '0') THEN
                rx_ready <= '1';
            ELSIF (rd = '1' AND csn = '0') THEN
                rx_ready <= '0';
            END IF;
        END IF;
    END PROCESS;
    ----------------------------------------------------------------------------
    ---------------------- RX Error Flag update --------------------------------
    ----------------------------------------------------------------------------
    PROCESS (clk_sys, reset)
    BEGIN
        IF (reset = '1') THEN
            rx_error <= '0';
        ELSIF rising_edge(clk_sys) THEN
            IF (rx_done_2 = '1' AND rx_done_3 = '0' AND rx_ready = '1') THEN
                rx_error <= '1';
            ELSIF (rd = '1' AND csn = '0') THEN
                rx_error <= '0';
            END IF;
        END IF;
    END PROCESS;
    ------------------------------------------------------------------------------------------------
    -- Interrupt generation logic
    ------------------------------------------------------------------------------------------------
    PROCESS (clk_sys, reset)
    BEGIN
        IF reset = '1' THEN
            Interrrupt <= '0';
        ELSIF rising_edge(clk_sys) THEN
            IF (tx_error = '1' OR rx_error = '1' OR tx_ready = '1' OR rx_ready = '1') THEN
                Interrrupt <= '1';
            END IF;
        END IF;
    END PROCESS;

END behavioral;