-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity ab_sim is
	generic(g_clock_period: time:= 10 ns);
end ab_sim;

architecture sim of ab_sim is
	component accum_bank is
		generic (
			g_elem_count			: positive := 4;
			g_input_port_width		: positive := 4*16;
			g_output_port_width		: positive := 4*18;
			g_bram_word_width		: positive := 18;
			g_accumulator_count		: positive := 8;
			g_accum_counter_width	: positive := 3; -- =log2(g_accumulator_count)
			g_cycle_counter_width	: positive := 10; -- (to support matrices with columns of up to 4096 numbers)
			g_control_port_width	: positive := 10
		);
		port (
			-- Clock/Reset
			CLK, RST:	IN std_logic	:= '0';
			RSTOUT	:	OUT std_logic	:= '0'; -- Passes reset to the next BA
			-- Data I/O Ports
			DATAIN	:	IN
				std_logic_vector(g_input_port_width-1  downto 0) := (others => '0');
			DATAOUT	:	OUT std_logic_vector(g_output_port_width-1 downto 0) := (others => '0');
			VALIDIN	:	IN  std_logic	:= '0'; -- Accumulations only occur if VALIDIN is high
			COMPLETE:	OUT std_logic	:= '0';
			-- Control ports (to set number of usable lanes)
			CONTROLIN	:	IN  std_logic_vector(g_control_port_width-1 downto 0)	:= (others => '0');
			CONTROLOUT	:	OUT std_logic_vector(g_control_port_width-1 downto 0)	:= (others => '0')
		);
	end component;

	-- Signals
	signal clock, reset_in, reset_out	: std_logic	:= '0';
	signal valid_in : std_logic := '0';
	signal data_in	: std_logic_vector(63 downto 0)	:= (others=>'0');
	signal data_out : std_logic_vector(71 downto 0) := (others=>'0');

	signal complete_out	: std_logic := '0';
	signal control_in	: std_logic_vector(11 downto 0) := (others=>'0');
	signal control_out	: std_logic_vector(11 downto 0) := (others=>'0');

	-- For debugging
	signal indata_counter : integer := -1;

	type result_array_t is array (3 downto 0) of std_logic_vector(17 downto 0);
	signal result : result_array_t := (others=>(others=>'0'));

	type step_t is (settings, part1, part2, get);
	signal simulation_step: step_t;
begin
	uut: component accum_bank
	generic map (
		g_elem_count=>4, g_input_port_width=>64, g_output_port_width=>72, g_bram_word_width=>18,
		g_accumulator_count=>1024, g_accum_counter_width=>12, g_cycle_counter_width=>10,
		g_control_port_width=>12
	)
	port map (
		CLK => clock,
		RST => reset_in,
		RSTOUT  => reset_out,
		DATAIN  => data_in,
		DATAOUT => data_out,
		VALIDIN => valid_in,
		COMPLETE => complete_out,
		CONTROLIN  => control_in,
		CONTROLOUT => control_out
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
		data_in <= data_in + x"01010001" and x"7F7F7F7F7F7F7F7F";
		wait for g_clock_period;
	end process;

	datain_counter:
	process(clock) begin
		if rising_edge(clock) and valid_in = '1' then
			indata_counter <= indata_counter + 1;
		end if;
	end process;

	-- 18-bit signals concatenated are hard to read; we'll split
	-- data-out into different signals
	result(3) <= data_out(71 downto 54);
	result(2) <= data_out(53 downto 36);
	result(1) <= data_out(35 downto 18);
	result(0) <= data_out(17 downto 0);

	control:
	process begin
		simulation_step <= settings;
		valid_in <= '0';
		-- Initialize component: use 256 lanes and count 2 cycles
		reset_in <= '1';
		control_in <= x"100"; -- lanes
		wait for g_clock_period;
		control_in <= x"002"; -- cycles
		wait for g_clock_period;
		reset_in <= '0';
		control_in <= x"000";

		-- Wait a bit before supplying data
		valid_in <='0';
		wait for g_clock_period*2;

		-- Supply data continuously
		simulation_step <= part1;
		valid_in <= '1';
		wait for g_clock_period*10;


		-- Wait one cycle, then continue
		valid_in <= '0';
		wait for g_clock_period;

		-- Supply more data
		valid_in <= '1';
		wait for g_clock_period*4;

		-- Longer break this time
		valid_in <= '0';
		wait for g_clock_period*4;

		-- Continue until 6 numbers left for cycle loop
		for i in 14 to 250 loop
			valid_in <= '1';
			wait for g_clock_period;
		end loop;

		for i in 250 to 256 loop
			valid_in <= '1';
			wait for g_clock_period;
			valid_in <= '0';
			wait for g_clock_period;
		end loop;

		simulation_step <= part2;

		-- Continue
		valid_in <= '1';
		wait for g_clock_period*(256-6);

		control_in <= x"010";
		while complete_out = '0' loop
			valid_in <= '1';
			wait for g_clock_period;
			valid_in <= '0';
			wait for g_clock_period;
		end loop;

		valid_in <= '1';

		-- Should be ready; play with `control_in` to see what we got
		control_in <= x"000";
		wait for g_clock_period;
		simulation_step <= get;
		loop
			control_in <= control_in + x"02";
			wait for g_clock_period;
		end loop;
	end process;
end sim;
