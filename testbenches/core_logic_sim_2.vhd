-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

-- Simple Mode Test: This simulation doesn't focus on valid signals

entity corelogic_sim is
	generic(g_clock_period: time:= 5 ns);
end corelogic_sim;

architecture sim2 of corelogic_sim is
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
			DATAIN				: IN  std_logic_vector(31 downto 0)	:= (others=>'0');
			DATAOUT_VALID		: OUT std_logic := '0';
			DATAOUT_READY		: IN  std_logic := '0';
			DATAOUT_LAST		: OUT std_logic := '0';
			DATAOUT				: OUT std_logic_vector(31 downto 0)	:= (others=>'0');
			CONTROL_START		: IN  std_logic := '0';
			CONTROL_READY		: OUT std_logic := '0';
			CONTROL_ADDR		: IN  std_logic_vector(7 downto 0) := (others=>'0');
			CONTROL_DATA		: IN  std_logic_vector(15 downto 0):= (others=>'0');
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

	signal data_in				: std_logic_vector(31 downto 0)		:= (others=>'0');
	signal data_out				: std_logic_vector(31 downto 0)		:= (others=>'0');
	signal ctrl_address_in		: std_logic_vector( 7 downto 0)		:= (others=>'0');
	signal ctrl_data_in			: std_logic_vector(15 downto 0)		:= (others=>'0');
	signal ctrl_resp			: std_logic_vector( 1 downto 0)		:= (others=>'0');

	type simulation_step_t is (init, init_config, vb_load1, vb_load2, mat_mult_simple, mat_mult_get, sim_ended);
	signal simulation_step: simulation_step_t;
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
		ctrl_data_in <= x"0040"; -- 64
		wait for g_clock_period;
		ctrl_start <= '0';

		wait for g_clock_period;
		ctrl_start <= '1';
		-- output vector length
		ctrl_address_in <= x"02";
		ctrl_data_in <= x"0010"; -- 16
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
		wait for g_clock_period;

		-- Wait until the component is ready to receive data
		data_in <= x"0201FEFF";
		wait for g_clock_period;

		-- Load Vbuffer 1
		for i in 1 to 64/4 loop
			din_valid <= '1';
			data_in <= data_in(23 downto 0) & not data_in(31 downto 24);
			wait for g_clock_period;
		end loop;

		-- Wait
		din_valid <= '0';

		-- Load buffer 1
		wait until ctrl_ready = '1';
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

		for i in 1 to 64/4/2 loop
			din_valid <= '1';
			data_in <= x"0000FE00"; -- -2
			wait for g_clock_period;
			data_in <= x"01000000"; -- 1
			wait for g_clock_period;
		end loop;

		din_valid <= '1';
		wait for g_clock_period;
		din_valid <= '0';

		-- Send an `mms` command
		wait until ctrl_ready = '1';
		simulation_step <= mat_mult_simple;
		ctrl_start <= '1';
		-- Vector Buffer Select
		ctrl_address_in <= x"03";
		ctrl_data_in <= x"0000";
		wait for g_clock_period;
		ctrl_start <= '0';

		-- Send command
		wait for g_clock_period;
		ctrl_start <= '1';
		ctrl_address_in <= x"00";
		ctrl_data_in <= x"0007";
		wait for g_clock_period;
		ctrl_start <= '0';

		din_valid <= '1';
		data_in <= x"FF000001";
		wait until din_ready = '1';
		wait for g_clock_period;
		for i in 1 to 16*64/4 loop
			data_in <= data_in(15 downto 0) + "001" & not data_in(23 downto 16) - "010" & data_in(31 downto 24);
			wait for g_clock_period;
		end loop;
		din_valid <= '0';

		-- Ready to receive results
		simulation_step <= mat_mult_get;
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
end sim2;
