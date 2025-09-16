-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

entity corelogic_sim is
	generic(g_clock_period: time:= 5 ns);
end corelogic_sim;

architecture sim of corelogic_sim is
	component core_logic is
		generic (
			g_axi_port_width	: positive;
			g_input_elem_width	: positive;
			g_output_elem_width	: positive;
			g_ctrl_address_width: positive;
			g_ctrl_data_width	: positive
		);
		port (
			DATA_CLK, RST		: IN  std_logic := '0';
			DATAIN_VALID		: IN  std_logic := '0';
			DATAIN_READY		: OUT std_logic := '0';
			DATAIN_LAST			: IN  std_logic := '0';
			DATAIN				: IN  std_logic_vector(g_axi_port_width-1 downto 0)		:= (others=>'0');
			DATAOUT_VALID		: OUT std_logic := '0';
			DATAOUT_READY		: IN  std_logic := '0';
			DATAOUT_LAST		: OUT std_logic := '0';
			DATAOUT				: OUT std_logic_vector(g_axi_port_width-1 downto 0)		:= (others=>'0');
			CONTROL_START		: IN  std_logic := '0';
			CONTROL_READY		: OUT std_logic := '0';
			CONTROL_ADDR		: IN  std_logic_vector(g_ctrl_address_width-1 downto 0) := (others=>'0');
			CONTROL_DATA		: IN  std_logic_vector(g_ctrl_data_width-1 downto 0)	:= (others=>'0');
			CONTROL_RESP		: OUT std_logic_vector(1 downto 0) := (others=>'0')
		);
	end component;

	-- Signals
	signal clock, reset			: std_logic := '0';
	signal din_valid, dout_valid: std_logic := '0';
	signal din_ready, dout_ready: std_logic := '0';
	signal dout_last, din_last	: std_logic := '0';
	signal ctrl_start			: std_logic := '0';
	signal ctrl_ready			: std_logic := '0';

	signal data_in				: std_logic_vector(31 downto 0) 	:= (others=>'0');
	signal data_out				: std_logic_vector(31 downto 0) 	:= (others=>'0');
	signal ctrl_address_in		: std_logic_vector( 7 downto 0) 	:= (others=>'0');
	signal ctrl_data_in			: std_logic_vector(15 downto 0) 	:= (others=>'0');
	signal ctrl_resp			: std_logic_vector( 1 downto 0)		:= (others=>'0');

	type simulation_step_t is (init, init_config, vb_load1, vb_load2, mat_mult_simple, mat_mult_get, sim_ended);
	signal simulation_step: simulation_step_t;
	signal counter: integer := 0;
begin
	uut: component core_logic
	generic map(
		g_axi_port_width=>32,
		g_input_elem_width=>8,
		g_output_elem_width=>16,
		g_ctrl_address_width=>8,
		g_ctrl_data_width=>16
	)
	port map(
		DATA_CLK => clock,
		RST => reset,
		DATAIN_VALID => din_valid,
		DATAIN_READY => din_ready,
		DATAIN_LAST  => din_last,
		DATAIN => data_in,
		DATAOUT_VALID => dout_valid,
		DATAOUT_READY => dout_ready,
		DATAOUT_LAST  => dout_last,
		DATAOUT => data_out,
		CONTROL_START => ctrl_start,
		CONTROL_READY => ctrl_ready,
		CONTROL_ADDR => ctrl_address_in,
		CONTROL_DATA => ctrl_data_in,
		CONTROL_RESP => ctrl_resp
	);

	clock_proc:
	process begin
		clock <= '1';
		wait for g_clock_period/2;
		clock <= '0';
		wait for g_clock_period/2;
	end process;

	control:
	process begin
		-- Reset component
		simulation_step <= init;
		reset <= '1';
		din_valid <= '0';
		din_last <= '0';
		dout_ready <= '0';
		data_in <= (others=>'0');
		ctrl_start <= '0';
		ctrl_address_in <= (others=>'0');
		ctrl_data_in <= (others=>'0');
		wait for g_clock_period;
		reset <= '0';
		wait for g_clock_period;

		-- Configure device (send data)

		-- How to communicate with `core_logic`
		-- 1. Wait for `ctrl_ready` = '1'
		-- 2. Set `ctrl_start` high for one cycle
		-- 3. Set `ctrl_start` low
		-- 4. Check response (always comes)
		simulation_step <= init_config;

		wait for g_clock_period;
		ctrl_start <= '1';
		-- input vector length
		ctrl_address_in <= x"01";
		ctrl_data_in <= x"0080"; -- 128
		wait for g_clock_period;
		ctrl_start <= '0';

		wait for g_clock_period;
		ctrl_start <= '1';
		-- output vector length
		ctrl_address_in <= x"02";
		ctrl_data_in <= x"003C"; -- 60
		wait for g_clock_period;
		ctrl_start <= '0';
		wait for g_clock_period*2; -- wait a little more

		wait for g_clock_period;
		ctrl_start <= '1';
		-- Vector Buffer Select
		ctrl_address_in <= x"03";
		ctrl_data_in <= x"0000";
		wait for g_clock_period;
		ctrl_start <= '0';

		-- No more data
		ctrl_start <= '0';

		-- Load Vector Buffer Command
		simulation_step <= vb_load1;

		wait for g_clock_period;
		ctrl_start <= '1';
		ctrl_address_in <= x"00";
		ctrl_data_in <= x"0003";
		wait for g_clock_period;

		ctrl_start <= '0';
		ctrl_address_in <= x"01"; -- to check the component doesn't care about this change
		wait for g_clock_period;

		-- Wait until the component is ready to receive data
		data_in <= x"00000000";
		if din_ready = '0' then
			wait until din_ready = '1';
		end if;
		wait for g_clock_period;

		-- Send some data but hang up
		for i in 1 to 10 loop
			din_valid <= '1';
			data_in <= data_in + x"01010001";
			wait for g_clock_period;
			counter <= counter + 4;
		end loop;

		din_valid <= '0';
		data_in <= (others=>'0');
		wait for g_clock_period;

		-- Continue with data but hang up for longer
		data_in <= x"01000001";
		for i in 1 to 10 loop
			din_valid <= '1';
			data_in <= data_in + x"01000001";
			wait for g_clock_period;
			counter <= counter + 4;
		end loop;

		din_valid <= '0';
		wait for g_clock_period*4;

		-- Continue with the rest of our data
		data_in <= x"14140014";
		din_valid <= '1';
		for i in 21 to 32 loop
			data_in <= data_in + x"00000001";
			if i = 32 then din_last <= '1'; end if;
			wait for g_clock_period;
			counter <= counter + 4;
		end loop;

		-- Wait
		din_valid <= '0';
		din_last <= '0';

		-- Load buffer 1
		if ctrl_ready = '0' then
			wait until ctrl_ready = '1';
		end if;
		simulation_step <= vb_load2;
		-- Vector Buffer Select
		ctrl_start <= '1';
		ctrl_address_in <= x"03";
		ctrl_data_in <= x"0001";
		wait for g_clock_period;

		ctrl_start <= '0';
		wait for g_clock_period;

		-- Send `load_vbuffer` command for selected buffer 1
		ctrl_start <= '1';
		ctrl_address_in <= x"00";
		ctrl_data_in <= x"0003";
		wait for g_clock_period;
		ctrl_start <= '0';
		wait for g_clock_period;

		din_valid <= '1';
		if din_ready = '0' then
			wait until din_ready = '1';
		end if;

		for i in 1 to 32/2 loop
			data_in <= x"0000FF00";
			wait for g_clock_period;
			data_in <= x"01000002"; -- [1, 1, 0, 0]
			if i = 32/2 then din_last <= '1'; end if;
			wait for g_clock_period;
		end loop;
		din_last  <= '0';
		din_valid <= '0';

		-- Send an `mms` command
		counter <= 0;
		simulation_step <= mat_mult_simple;
		if ctrl_ready = '0' then
			wait until ctrl_ready = '1';
		end if;
		wait for g_clock_period;
		ctrl_start <= '1';
		-- Vector Buffer Select
		ctrl_address_in <= x"03";
		ctrl_data_in <= x"0001";
		wait for g_clock_period;
		ctrl_start <= '0';

		wait for g_clock_period;
		ctrl_start <= '1';
		ctrl_address_in <= x"00";
		ctrl_data_in <= x"0007";
		wait for g_clock_period;
		ctrl_start <= '0';

		data_in <= x"08000506";
		if din_ready = '0' then
			wait until din_ready;
		end if;
		wait for g_clock_period;

		din_valid <= '1';
		counter <= 0;
		-- Send an uninterrupted stream
		for i in 1 to 8 loop -- 8 cycles
			data_in <= data_in(23 downto 0) & data_in(31 downto 24);
			wait for g_clock_period;
			counter <= counter + 4;
		end loop;

		-- Hang up for one cycle
		din_valid <= '0';
		wait for g_clock_period;
		din_valid <= '1';
		for i in 1 to 2 loop -- 10 cycles
			data_in <= data_in(7 downto 0) & data_in(31 downto 8);
			wait for g_clock_period;
			counter <= counter + 4;
		end loop;

		-- Hang up for two cycles
		din_valid <= '0';
		wait for g_clock_period*2;
		din_valid <= '1';
		for i in 1 to 3 loop -- 13 cycles
			data_in <= data_in(7 downto 0) & data_in(31 downto 8);
			wait for g_clock_period;
			counter <= counter + 4;
		end loop;

		-- Transmit with intermitten breaks
		for i in 1 to 14 loop -- 27 cycles
			data_in <= data_in(31 downto 24) & not data_in(23 downto 0);
			din_valid <= '1';
			wait for g_clock_period;
			counter <= counter + 4;
			din_valid <= '0';
			wait for g_clock_period*4;
		end loop;

		-- No more break-ups; continue normally
		din_valid <= '1';
		for i in 1 to 1877 loop -- 1904 cycles
			data_in <= not data_in(31 downto 24) & data_in(7 downto 0) & not data_in(23 downto 8);
			wait for g_clock_period;
			counter <= counter + 4;
		end loop;

		-- Start intermitten break-ups again for the last few cycles
		for i in 1 to 15 loop
			data_in <= data_in(7 downto 0) & data_in(31 downto 8);
			din_valid <= '1';
			wait for g_clock_period;
			counter <= counter + 4;
			din_valid <= '0';
			wait for g_clock_period;
		end loop;

		-- We'll delay the last number
		wait for g_clock_period*5;
		din_valid <= '1';
		counter <= counter + 4;
		wait for g_clock_period;
		din_valid <= '0';

		-- Ready to receive results
		simulation_step <= mat_mult_get;
		din_valid <= '0';
		dout_ready <= '1';
		wait for g_clock_period;

		-- Get 2 cycles of data and hang up
		wait for g_clock_period*2;
		dout_ready <= '0';
		wait for g_clock_period;

		-- Continue
		dout_ready <= '1';
		wait until dout_valid = '0';

		-- Done:)
		simulation_step <= sim_ended;
		wait until false;

	end process;
end sim;
