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

ENTITY sclk_gen IS
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
END sclk_gen;

ARCHITECTURE count_arch OF sclk_gen IS

    SIGNAL half_clk                 : std_logic_vector(7 DOWNTO 0);
    SIGNAL sclk_period_i            : std_logic_vector(7 DOWNTO 0);
    SIGNAL count_sclk               : std_logic_vector(7 DOWNTO 0);
    SIGNAL slk_pi_shift             : std_logic;
    SIGNAL slk_signal               : std_logic;
    SIGNAL falling_sclk             : std_logic;
    SIGNAL rising_sclk              : std_logic;
    SIGNAL delay_counter_start      : std_logic;
    SIGNAL transmit_wait_done       : std_logic;
    SIGNAL hold_done                : std_logic;
    SIGNAL setup_done               : std_logic;
    SIGNAL delay_counter            : std_logic_vector(7 DOWNTO 0);
    SIGNAL falling_edge_count_start : std_logic;
    SIGNAL data_count               : std_logic_vector(7 DOWNTO 0);
    SIGNAL start_spi_sig            : std_logic;
    SIGNAL sclk_gen_enable          : std_logic;

    TYPE sclk_state IS (idle_state, setup_time_state,
        data_transmit_state, hold_time_state, wait_state);
    SIGNAL present_state : sclk_state;
BEGIN

    ----------------------------------------------------------
    --------------- sclk gen ---------------------------------
    ----------------------------------------------------------
    PROCESS (sys_clk, reset)
    BEGIN
        IF reset = '1' THEN
            count_sclk <= "00000001";
            slk_signal <= '0';
        ELSIF rising_edge(sys_clk) THEN
            IF sclk_gen_enable = '1' THEN
                IF count_sclk < half_clk_count THEN
                    count_sclk <= count_sclk + 1;
                ELSE
                    count_sclk <= "00000001";
                END IF;
            ELSE
                count_sclk <= "00000010";
            END IF;
            IF count_sclk > half_clk THEN
                slk_signal <= '0';
            ELSE
                slk_signal <= '1';
            END IF;
        END IF;
    END PROCESS;

    half_clk <= '0' & half_clk_count(7 DOWNTO 1);

    ----------------------------------------------------------
    --------------- sclk edge detection ----------------------
    ----------------------------------------------------------
    PROCESS (sys_clk, reset)
    BEGIN
        IF reset = '1' THEN
            slk_pi_shift <= '0';
        ELSIF rising_edge(sys_clk) THEN
            slk_pi_shift <= slk_signal;
        END IF;
    END PROCESS;

    rising_sclk <= '1' WHEN slk_signal = '1' AND slk_pi_shift = '0' ELSE
        '0';
    falling_sclk <= '1' WHEN slk_signal = '0' AND slk_pi_shift = '1' ELSE
        '0';

    ----------------------------------------------------------
    --------------- output sclk ------------------------------
    ----------------------------------------------------------
    PROCESS (sys_clk, reset)
    BEGIN
        IF reset = '1' THEN
            sclk_out <= '0';
        ELSIF rising_edge(sys_clk) THEN
            IF present_state = data_transmit_state THEN
                IF (CPOL_sclk = '0') THEN
                    sclk_out <= slk_signal;
                ELSE
                    sclk_out <= NOT slk_signal;
                END IF;
            ELSE
                sclk_out <= CPOL_sclk;
            END IF;
        END IF;
    END PROCESS;

    ----------------------------------------------------------
    --------------- Core Process  ----------------------------
    ----------------------------------------------------------

    PROCESS (sys_clk, reset)
    BEGIN
        IF reset = '1' THEN
            start_spi_sig <= '0';
        ELSIF rising_edge(sys_clk) THEN
            start_spi_sig <= start_spi;
        END IF;
    END PROCESS;

    --------------- Data Bit counter  ------------------------
    ----------------------------------------------------------
    PROCESS (sys_clk, reset)
    BEGIN
        IF reset = '1' THEN
            data_count <= (OTHERS => '0');
        ELSIF rising_edge(sys_clk) THEN
            IF falling_edge_count_start = '0' THEN
                data_count <= (OTHERS => '0');
            ELSIF (falling_sclk = '1') THEN
                data_count <= data_count + 1;
            END IF;
        END IF;
    END PROCESS;
    ----------------------- Core FSM  ------------------------
    ----------------------------------------------------------
    PROCESS (sys_clk, reset)
    BEGIN
        IF reset = '1' THEN
            present_state            <= idle_state;
            delay_counter_start      <= '0';
            sclk_gen_enable          <= '0';
            master_start             <= '1';
            falling_edge_count_start <= '0';
        ELSIF (rising_edge(sys_clk)) THEN
            CASE (present_state) IS
                WHEN idle_state =>
                    IF (start_spi_sig = '1') THEN -- registered input
                        present_state       <= setup_time_state;
                        delay_counter_start <= '1';
                        master_start        <= '0';
                        sclk_gen_enable     <= '0';
                    ELSE
                        present_state            <= idle_state;
                        delay_counter_start      <= '0';
                        master_start             <= '1';
                        falling_edge_count_start <= '0';
                        sclk_gen_enable          <= '0';
                    END IF;
                WHEN setup_time_state =>
                    IF (setup_done = '1') THEN
                        delay_counter_start      <= '0';
                        present_state            <= data_transmit_state;
                        sclk_gen_enable          <= '1';
                        falling_edge_count_start <= '1';
                    ELSE
                        present_state       <= setup_time_state;
                        delay_counter_start <= '1';
                    END IF;
                WHEN data_transmit_state =>
                    IF (data_count = DATA_SIZE) THEN
                        present_state            <= hold_time_state;
                        delay_counter_start      <= '1';
                        falling_edge_count_start <= '0';
                    ELSE
                        present_state <= data_transmit_state;
                    END IF;
                WHEN hold_time_state =>
                    IF (hold_done = '1') THEN
                        delay_counter_start <= '0';
                        present_state       <= wait_state;
                        master_start        <= '1';
                        sclk_gen_enable     <= '0';
                    ELSE
                        present_state       <= hold_time_state;
                        delay_counter_start <= '1';
                    END IF;
                WHEN wait_state =>
                    IF (transmit_wait_done = '1') THEN
                        delay_counter_start <= '0';
                        present_state       <= idle_state;
                    ELSE
                        present_state       <= wait_state;
                        delay_counter_start <= '1';
                    END IF;
                WHEN OTHERS =>
                    present_state            <= idle_state;
                    delay_counter_start      <= '0';
                    sclk_gen_enable          <= '0';
                    master_start             <= '1';
                    falling_edge_count_start <= '0';
            END CASE;
        END IF;
    END PROCESS;

    ----------------------------------------------------------
    --------------- Timer Logic  -----------------------------
    ----------------------------------------------------------
    PROCESS (sys_clk, reset)
    BEGIN
        IF reset = '1' THEN
            delay_counter <= "00000001";
        ELSIF rising_edge(sys_clk) THEN
            IF delay_counter_start = '0' THEN
                delay_counter <= "00000001";
            ELSE
                delay_counter <= delay_counter + 1;
            END IF;
        END IF;
    END PROCESS;
    transmit_wait_done <= '1' WHEN delay_counter = transmit_wait_cycles ELSE
        '0';
    hold_done <= '1' WHEN delay_counter = hold_cycles ELSE
        '0';
    setup_done <= '1' WHEN delay_counter = setup_cycles ELSE
        '0';

END count_arch;