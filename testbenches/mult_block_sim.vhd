-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity mb_sim is
	generic(g_clock_period: time:= 10 ns);
end mb_sim;

architecture sim of mb_sim is
	component multiplier_block is
		generic (
			-- Number of multiplication pairs
			g_mult_pairs	: positive := 4;
			-- Number of bits in input port
			g_vinput_elem_width		: positive := 8;
			g_hinput_elem_width		: positive := 8
		);
		port (
			CLK, RST	: IN  std_logic	:= '0';
			RSTOUT		: OUT std_logic := '0';
			-- Data inputs
			VERT_IN		: IN
				std_logic_vector(g_vinput_elem_width*g_mult_pairs-1 downto 0)	:= (others => '0');
			HOR_IN		: IN
				std_logic_vector(g_hinput_elem_width*g_mult_pairs-1 downto 0)	:= (others => '0');
			-- Data outputs
			MULTOUT 	: OUT
				std_logic_vector((g_hinput_elem_width+g_vinput_elem_width)*g_mult_pairs-1 downto 0)	:= (others => '0');
			VERT_OUT	: OUT
				std_logic_vector(g_hinput_elem_width*g_mult_pairs-1 downto 0)	:= (others => '0');
			-- Control I/O
			VALIDIN 	: IN  std_logic	:= '0'; -- from vertical data input (AXI Stream)
			VALIDOUT 	: OUT std_logic	:= '0'  -- to AB and next MB
		);
	end component;

	-- Signals
	signal clock, reset, reset_out, valid_in, valid_out: std_logic := '0';
	signal vertical_in		: std_logic_vector(31 downto 0) := (others=>'0');
	signal horizontal_in	: std_logic_vector(31 downto 0) := (others=>'0');
	signal to_ab			: std_logic_vector(63 downto 0) := (others=>'0');
	signal vertical_out		: std_logic_vector(31 downto 0) := (others=>'0');

begin
	uut: component multiplier_block
	generic map (
		g_mult_pairs=>4, g_vinput_elem_width=>8, g_hinput_elem_width=>8
	)
	port map (
		CLK => clock,
		RST => reset,
		RSTOUT => reset_out,
		VERT_IN => vertical_in,
		HOR_IN => horizontal_in,
		MULTOUT => to_ab,
		VERT_OUT => vertical_out,
		VALIDIN => valid_in,
		VALIDOUT => valid_out
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
		horizontal_in <= horizontal_in + x"01010001";
		vertical_in <= vertical_in + x"02fe0001";
		wait for g_clock_period;
	end process;

	control:
	process begin
		valid_in <= '0';

		-- Reset block
		reset <= '1';
		wait for g_clock_period;
		reset <= '0';
		wait for g_clock_period;

		-- Wait a while before supplying data
		valid_in <= '0';
		wait for g_clock_period*2;

		-- Supply data normally
		valid_in <= '1';
		wait for g_clock_period;
		valid_in <= '0';

		-- Wait 7 cycles before supplying data again
		wait for g_clock_period*7;

		-- Supply and keep running
		loop
			valid_in <= '1';
			wait for g_clock_period;
			valid_in <= '0';
			wait for g_clock_period*7;
		end loop;
	end process;
end sim;
