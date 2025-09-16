-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

entity lut_comp_sim is
	generic(g_clock_period: time:= 5 ns);
end lut_comp_sim;

architecture sim of lut_comp_sim is
	component lut_component is
		port (
			CLK, RST		: IN  std_logic;
			LUT_EN			: IN  std_logic;
			LOAD_START		: IN  std_logic;
			LOADIN_VALID	: IN  std_logic;
			LOADIN			: IN  std_logic_vector(31 downto 0);
			DATAIN			: IN  std_logic_vector(18*4-1 downto 0);
			DATAOUT			: OUT std_logic_vector(8*4-1 downto 0)
		);
	end component;

-- Signals
	signal clock, reset	: std_logic := '0';
	signal enable		: std_logic := '0';
	signal loadstart	: std_logic := '0';
	signal loadvalid	: std_logic := '0';
	signal load_in		: std_logic_vector(31 downto 0) := x"01020304";
	signal data_in		: std_logic_vector(18*4-1 downto 0) := (others=>'0');
	signal data_out		: std_logic_vector(31 downto 0) := (others=>'0');

	-- Split Input and Output (Data) Elements for readability
	type   din_split_t  is array (3 downto 0) of std_logic_vector(17 downto 0);
	type   dout_split_t is array (3 downto 0) of std_logic_vector( 7 downto 0);
	signal din_split	: din_split_t  := (others=>(others=>'0'));
	signal dout_split	: dout_split_t := (others=>(others=>'0'));

	type simulation_step_t is (init, disabled, bank0_load, compute);
	signal simulation_step: simulation_step_t := init;
	signal load_index	: integer := 0;
begin
	uut: component lut_component
	port map (
		CLK => clock,
		RST => reset,
		LUT_EN => enable,
		LOAD_START => loadstart,
		LOADIN_VALID => loadvalid,
		LOADIN => load_in,

		DATAIN => data_in,
		DATAOUT => data_out
	);

	clock_proc:
	process begin
		clock <= '1';
		wait for g_clock_period/2;
		clock <= '0';
		wait for g_clock_period/2;
	end process;

	load_data_proc:
	process begin
		load_in <= load_in(0) & (load_in(31 downto 1) + "01");
		wait for g_clock_period;
	end process;

	debug_indices:
	process begin
		wait for g_clock_period;
		if loadvalid = '1' and enable = '1' then
			load_index <= load_index + 1;
		end if;
	end process;

	data_in_proc:
	process begin
		-- We'll handle each element individually
		din_split(0) <= not din_split(0);
		din_split(1) <= (others=>'0');
		din_split(2) <= (others=>'0');
		din_split(3) <= din_split(3) + x"300";
		wait for g_clock_period*2;
	end process;

	-- Concat input
	-- data_in <= din_split(2) & din_split(1) & din_split(0) & din_split(3);
	data_in <= din_split(3) & din_split(2) & din_split(1) & din_split(0);

	-- Split Output
	out_split: for i in 3 downto 0 generate
		dout_split(i) <= data_out((i+1)*8-1 downto i*8);
	end generate out_split;

	control:
	process begin
		simulation_step <= init;
		reset <= '1';
		enable <= '0';
		wait for g_clock_period;
		reset <= '0';
		wait for g_clock_period*4;

		-- Disable LUT; nothing should happen
		simulation_step <= disabled;
		enable <= '0';
		loadstart <= '1';
		loadvalid <= '1';
		wait for g_clock_period*5;
		loadstart <= '0';
		loadvalid <= '0';
		wait for g_clock_period;

		simulation_step <= bank0_load;
		enable <= '1';
		loadstart <= '1';
		loadvalid <= '1';
		wait for g_clock_period;
		loadstart <= '0';
		wait for g_clock_period*9;
		loadvalid <= '0';
		wait for g_clock_period;
		loadvalid <= '1';
		wait for g_clock_period;
		loadvalid <= '0';
		wait for g_clock_period*4;
		loadvalid <= '1';
		wait for g_clock_period*(128-12);
		-- Hang up before the last cycle
		loadvalid <= '0';
		wait for g_clock_period;
		loadvalid <= '1';
		wait for g_clock_period;
		loadvalid <= '0';

		simulation_step <= compute;
		wait until false;


	end process;
end sim;
