-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

entity vbuffer_sim is
	generic(g_clock_period: time:= 5 ns);
end vbuffer_sim;

architecture sim of vbuffer_sim is
	component vector_buffer is
		generic (
			g_port_elem_width	: positive := 8;
			g_port_elem_count	: positive := 4;

			g_buffer_count		: positive := 2;
			g_max_buffer_length : positive := 1024 -- in `g_port_elem_count` bytes

			-- Note: For simplicity, the final, total length of one buffer will be equal to
			-- `g_port_elem_count` * `g_max_buffer_length` bytes.
		);
		port (
			CLK, RST	: IN  std_logic := '0';
			-- Data inputs
			PORTIN		: IN  std_logic_vector(g_port_elem_width*g_port_elem_count-1 downto 0)	:= (others=>'0');
			-- Data output
			PORTOUT		: OUT std_logic_vector(g_port_elem_count*g_port_elem_width-1 downto 0)	:= (others=>'0');

			-- Control Ports
			ADVANCE		: IN  std_logic := '0'; -- Outputs next adress when '1'
			STORE		: IN  std_logic := '0'; -- Set to '1' to write into the selected buffer
			CONTROLIN	: IN  std_logic_vector(12 downto 0)  := (others=>'0'); -- General Purpose Control Port
			VALID		: IN  std_logic := '0';

			-- Bypass output
			BYPASSEN	: IN  std_logic := '0'; -- Set to '1' to enable bypass
			VALIDOUT	: OUT std_logic := '0';
			BYPASSOUT	: OUT std_logic_vector(g_port_elem_count*g_port_elem_width-1 downto 0)	:= (others=>'0')
		);
	end component;

	-- Signals
	signal clock, reset	: std_logic := '0';
	signal advance_in	: std_logic := '0';
	signal store_in		: std_logic := '0';
	signal valid_in		: std_logic := '0';
	signal bypass_en	: std_logic := '0';
	signal valid_out	: std_logic := '0';

	signal port_in		: std_logic_vector(31 downto 0) := (others=>'0');
	signal port_out		: std_logic_vector(31 downto 0) := (others=>'0');
	signal control_in	: std_logic_vector(12 downto 0) := (others=>'0');
	signal bypass_out	: std_logic_vector(31 downto 0) := (others=>'0');

	type simulation_step_t is (init, load0, load1, do);
	signal simulation_step: simulation_step_t := init;
begin
	uut: component vector_buffer
	generic map(g_port_elem_width=>8, g_port_elem_count=>4, g_buffer_count=>2,
	  g_max_buffer_length=>64)
	port map(
		CLK => clock,
		RST => reset,
		PORTIN => port_in,
		PORTOUT => port_out,
		ADVANCE => advance_in,
		STORE => store_in,
		CONTROLIN => control_in,
		VALID => valid_in,
		BYPASSEN => bypass_en,
		VALIDOUT => valid_out,
		BYPASSOUT => bypass_out
	);

	clock_proc:
	process begin
		clock <= '1';
		wait for g_clock_period/2;
		clock <= '0';
		wait for g_clock_period/2;
	end process;

	data_generation:
	process begin
		wait for g_clock_period;
		port_in <= port_in + x"01000001";
	end process;

	control:
	process begin
		-- Reset and set buffers to 32 (*4=128B) length
		simulation_step <= init;
		reset <= '1';
		control_in <= '0' & x"020";
		valid_in <= '1';
		store_in <= '0';
		advance_in <= '0';
		bypass_en <= '0';
		wait for g_clock_period;
		reset <= '0';
		valid_in <= '0';
		wait for g_clock_period;

		-- Load something into buffer 1;
		-- To do this enable store, set control_in to buffer 0 and start
		-- providing data
		simulation_step <= load1;
		control_in <= (11 downto 0 => '0') & '1';
		valid_in <= '1';
		wait for g_clock_period;
		store_in <= '1'; -- writing starts now
		wait for g_clock_period*4;

		-- Hang up for one cycle
		valid_in <= '0';
		wait for g_clock_period;

		-- Continue
		valid_in <= '1';
		wait for g_clock_period;

		-- Hang up for longer
		valid_in <= '0';
		wait for g_clock_period*4;

		-- Continue
		valid_in <= '1';
		wait for g_clock_period*(32-5-4);

		-- Halve transmition speed
		for i in 0 to 3 loop
			valid_in <= '0';
			wait for g_clock_period;
			valid_in <= '1';
			wait for g_clock_period;
		end loop;
		-- Done
		store_in <= '0';
		valid_in <= '0';

		-- Stall
		control_in <= (others=>'0');
		wait for g_clock_period*4;

		-- Select buffer 0
		simulation_step <= load0;
		control_in <= (others=>'0');
		valid_in <= '1';
		wait for g_clock_period;

		-- Load something into buffer 0
		store_in <= '1';
		control_in <= (11 downto 0 => '0') & '0';
		valid_in <= '1';
		wait for g_clock_period*32;

		-- Try outputting buffer 1
		simulation_step <= do;
		store_in <= '0';
		valid_in <= '1';
		control_in <= (11 downto 0 => '0') & '1';
		wait for g_clock_period;
		valid_in <= '0'; -- we keep valid_in high only to select a buffer, it is not needed then
		advance_in <= '1';
		wait for g_clock_period*4;

		-- Simulate a hiccup
		advance_in <= '0';
		wait for g_clock_period;

		-- Continue normally
		advance_in <= '1';
		wait for g_clock_period*3;

		-- Simulate throughput reduction (for whatever reason)
		for i in 0 to 4 loop
			advance_in <= '0';
			wait for g_clock_period;
			advance_in <= '1';
			wait for g_clock_period;
		end loop;

		-- Continue
		wait for g_clock_period*32;
	end process;
end sim;
