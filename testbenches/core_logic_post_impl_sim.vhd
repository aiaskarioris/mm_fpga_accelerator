-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;

-- Simple Mode Test: This simulation doesn't focus on valid signals

entity corelogic_sim is
	generic(g_clock_period: time:= 6 ns);
end corelogic_sim;

architecture sim3 of corelogic_sim is
	component core_logic is
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
			CONTROL_ADDR		: IN  std_logic_vector(3 downto 0) := (others=>'0');
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
	signal ctrl_address_in		: std_logic_vector( 3 downto 0)		:= (others=>'0');
	signal ctrl_data_in			: std_logic_vector(15 downto 0)		:= (others=>'0');
	signal ctrl_resp			: std_logic_vector( 1 downto 0)		:= (others=>'0');

	type simulation_step_t is (init, init_config, loadLUT0, vb_load1, vb_load2, mmai_0, mmai_1, mmai_bias, mmai_get, sim_ended);
	signal simulation_step: simulation_step_t;

begin
	uut: component core_logic
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

		if ctrl_ready = '0' then
			wait until ctrl_ready = '1';
		end if;
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
		ctrl_address_in <= x"1";
		ctrl_data_in <= x"0200"; -- 512
		wait for g_clock_period;
		ctrl_start <= '0';

		wait for g_clock_period*2;
		ctrl_start <= '1';
		-- output vector length
		ctrl_address_in <= x"2";
		ctrl_data_in <= x"0100"; -- 256
		wait for g_clock_period;
		ctrl_start <= '0';
		wait for g_clock_period*2; -- wait a little more

		-- Set LUT0
		simulation_step <= loadLUT0;
		ctrl_start <= '1';
		ctrl_address_in <= x"0";
		ctrl_data_in <= x"0005";
		wait for g_clock_period;
		ctrl_start <= '0';
		if din_ready = '0' then
			wait until din_ready = '1';
		end if;
		wait for g_clock_period;
		-- Start transmition; LUT expects 512x8-bits or 128 AXI_S Clocks (this number may change)
		wait for g_clock_period*12;
		din_valid <= '1';
		data_in <= x"80818181";
		wait for g_clock_period;
		din_valid <= '0';
		wait for g_clock_period*3;
		din_valid <= '1';
		for i in 1 to 127 loop
			if i < 16 then
				data_in <= x"82828282"; -- all -128
			elsif i < 64 then
				data_in <= to_stdlogicvector(i/8-128, 8) & to_stdlogicvector(i/8-128, 8) & to_stdlogicvector(i/8-128, 8) & to_stdlogicvector(i/8-128, 8);
			elsif i < 128-16 then
				data_in <= to_stdlogicvector(i/8, 8) & to_stdlogicvector(i/8, 8) & to_stdlogicvector(i/8, 8) & to_stdlogicvector(i/8, 8);
			else -- i > 992
				data_in <= x"7c7c7c7c"; -- all 127
			end if;
			wait for g_clock_period;
		end loop;
		data_in <= x"7E7F7F7F";
		din_last <= '1';
		wait for g_clock_period;
		din_last <= '0';
		din_valid <= '0';
		wait for g_clock_period;

		-- Done, wait until component is ready to receive next command
		if ctrl_ready = '0' then
			wait until ctrl_ready = '1';
		end if;
		wait for g_clock_period;
		ctrl_start <= '1';

		-- Both-Vector-Buffers Load
		ctrl_address_in <= x"0";
		ctrl_data_in <= x"0004";
		wait for g_clock_period;
		ctrl_start <= '0';

		simulation_step <= vb_load1;
		-- Wait until the component is ready to receive data
		data_in <= x"0201FEFF";
		if din_ready = '0' then
			wait until din_ready = '1';
		end if;

		-- Load Vbuffer 0
		for i in 1 to 512/4 loop
			din_valid <= '1';
			data_in <= data_in(23 downto 0) & not data_in(31 downto 24);
			if i = 512/4 then din_last <= '1'; end if;
			wait for g_clock_period;
		end loop;
		din_last <= '0';

		-- Load VBuffer 1
		simulation_step <= vb_load2;
		wait for g_clock_period;
		if din_ready = '0' then
			wait until din_ready = '1';
		end if;

		for i in 1 to 256/4/2 loop
			din_valid <= '1';
			data_in <= x"0000FE00"; -- -2
			wait for g_clock_period;
			data_in <= x"01000000"; -- 1
			if i = 256/4/2 then
				din_valid <= '1';
				din_last <= '1';
			end if;
			wait for g_clock_period;
		end loop;

		din_last <= '0';
		din_valid <= '0';

		wait for g_clock_period;

		-- Send an `mmai` command
		if ctrl_ready = '0' then
			wait until ctrl_ready = '1';
		end if;
		simulation_step <= mmai_0;
		ctrl_start <= '1';
		ctrl_address_in <= x"0";
		ctrl_data_in <= x"0008";
		wait for g_clock_period;
		ctrl_start <= '0';

		din_valid <= '1';
		data_in <= x"FF000001";
		if din_ready = '0' then
			wait until din_ready = '1';
		end if;
		wait for g_clock_period;
		-- Send first matrix
		for i in 1 to 512*256/4 loop
			data_in <= data_in(15 downto 0) + "001" & not data_in(23 downto 16) - "010" & data_in(31 downto 24);
			if i = 512*256/4-1 then
				din_last <= '1';
			end if;
			wait for g_clock_period;
		end loop;
		din_last <= '0';
		din_valid <= '0';

		-- Don't wait for the second one unless `core_logic` asks us too
		if din_ready = '0' then
			wait for g_clock_period;
			-- wait until din_ready = '1';
		end if;

		simulation_step <= mmai_1;
		din_valid <= '1';
		for i in 1 to 256*256/4 loop
			data_in <= not data_in(15 downto 0) + "001" & data_in(23 downto 16) + "010" & data_in(31 downto 24);
			if i = 256*256/4-1 then din_last <= '1'; end if;
			wait for g_clock_period;
		end loop;
		din_last <= '0';

		-- Now it's time for Bias
		if din_ready = '0' then
			wait for g_clock_period;
		end if;
		data_in <= x"0102FEFF";
		wait for g_clock_period;

		simulation_step <= mmai_bias;
		for i in 1 to 256/4 loop
			data_in <= data_in(7 downto 0) & not data_in(31 downto 8);
			if i = 256/4-1 then din_last <= '1'; end if;
			wait for g_clock_period;
		end loop;
		din_last <= '0';
		din_valid <= '0';

		-- Ready to receive results
		simulation_step <= mmai_get;
		dout_ready <= '1';
		wait for g_clock_period;

		wait until dout_valid = '1';
		wait for 256/4*g_clock_period;
		dout_ready <= '0';

		-- Done:)
		simulation_step <= sim_ended;
		wait until false;

	end process;
end sim3;
