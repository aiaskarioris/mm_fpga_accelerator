-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

entity bias_comp_sim is
	generic(g_clock_period: time:= 5 ns);
end bias_comp_sim;

architecture sim of bias_comp_sim is
	component bias_component is
		generic (
        g_axi_port_width	: positive;
        g_load_elem_width	: positive;
        g_data_port_width	: positive;
        g_elem_count		: positive;
        g_max_lane_count	: positive;
        g_control_port_width: positive
    );
    port (
        CLK, RST	:	IN  std_logic;
        CONTROLIN   :   IN  std_logic_vector(g_control_port_width-1 downto 0);
        LOADIN      :   IN  std_logic_vector(g_axi_port_width-1 downto 0);
        LOAD_EN     :   IN  std_logic;
        DATAIN		:	IN  std_logic_vector(g_data_port_width-1 downto 0);
        DATAOUT		:	OUT std_logic_vector(g_data_port_width-1 downto 0)
    );
	end component;

	-- Signals
	signal clock, reset : std_logic := '0';
	signal load_enable	: std_logic := '0';
	signal control_in	: std_logic_vector(12 downto 0) := (others=>'0');
	signal load_in		: std_logic_vector(31 downto 0) := (others=>'0');
	signal data_in		: std_logic_vector(71 downto 0) := (others=>'0');
	signal data_out		: std_logic_vector(71 downto 0) := (others=>'0');

	type simulation_step_t is (init, load, compute);
	signal simulation_step: simulation_step_t;

	-- Handling 72-bit signals is hard so we'll break it down a bit
	signal elem_signal	: std_logic_vector(17 downto 0) := (others=>'0');

begin
	uut: component bias_component
	generic map(
        g_axi_port_width  => 32,
        g_load_elem_width => 8,
        g_data_port_width => 18*4,
        g_elem_count => 4,
        g_max_lane_count => 4096,
        g_control_port_width=> 13
    )
    port map (
        CLK => clock,
        RST => reset,
        CONTROLIN => control_in,
        LOADIN => load_in,
        LOAD_EN => load_enable,
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

	data:
	process begin
		elem_signal <= (others=>'0');
		wait for g_clock_period;
		for i in 1024 downto 0 loop
			elem_signal <= elem_signal + "01";
			wait for g_clock_period;
		end loop;
	end process;

	data_in <= elem_signal & not elem_signal & elem_signal & not elem_signal;
	load_in <= ("000" & elem_signal(4 downto 0)) & ("000" & elem_signal(4 downto 0))
		& ("000" & elem_signal(4 downto 0))& ("000" & elem_signal(4 downto 0));

	control:
	process begin
		simulation_step <= init;
		reset <= '0';
		wait for g_clock_period;
		reset <= '1';
		control_in <= "00000" & x"40"; -- = 64 quads or 256 elements
		wait for g_clock_period;

		-- Start loading
		control_in <= (others=>'0');
		simulation_step <= load;
		reset <= '0';
		load_enable <= '1';
		wait for g_clock_period*10; -- 0 to 9

		-- Turn LOAD_EN down for a cycle
		load_enable <= '0';
		wait for g_clock_period;

		-- Continue a bit
		load_enable <= '1';
		wait for g_clock_period*4; -- 10 to 13

		-- Leave load_enable down for more cycles
		load_enable <= '0';
		wait for g_clock_period*3;

		load_enable <= '1';
		wait for g_clock_period*40;

		-- Finish of with bad signal
		for i in 0 to 10 loop
			load_enable <= '0';
			wait for g_clock_period;
			load_enable <= '1';
			wait for g_clock_period;
		end loop;

		load_enable <= '0';
		wait for g_clock_period*4;

		-- `control_in` can now be used to select quads in memory
		simulation_step <= compute;
		for i in 0 to 16 loop
			wait for g_clock_period;
			control_in <= control_in + "01";
		end loop;
		wait for g_clock_period;

		-- Slow down
		for i in 0 to 16 loop
			control_in <= control_in + "01";
			wait for g_clock_period*2;
		end loop;

		-- Don't wait anymore
		loop
			if control_in /= "111111" then
				control_in <= control_in + "01" ;
			else
				control_in <= (others=>'0');
			end if;
			wait for g_clock_period*2;
		end loop;

	end process;

end sim;
