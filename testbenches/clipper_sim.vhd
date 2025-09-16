-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity clipper_sim is
	generic(g_clock_period: time:= 10 ns);
end clipper_sim;

architecture sim of clipper_sim is
	component clipper_async is
		generic (g_input_width, g_output_width: positive);
		port(
			DIN :	IN  std_logic_vector(g_input_width-1 downto 0);
			DOUT:	OUT std_logic_vector(g_output_width-1 downto 0);
			CLIP:	OUT std_logic := '0'
		);
	end component;

	component clipper_sync is
		generic (g_input_width, g_output_width: positive);
		port(
			CLK :	IN  std_logic := '0';
			DIN :	IN  std_logic_vector(g_input_width-1 downto 0);
			DOUT:	OUT std_logic_vector(g_output_width-1 downto 0);
			CLIP:	OUT std_logic := '0'
		);
	end component;


-- Signals
	signal clock	: std_logic := '0';
	signal long_in_a, long_out_a	: std_logic_vector(15 downto 0):= (others=>'0');
	signal short_in_a, short_out_a	: std_logic_vector( 7 downto 0):= (others=>'0');
	signal clip_out_a, clip_sink_a	: std_logic := '0';

	signal long_in_s, long_out_s	: std_logic_vector(15 downto 0):= (others=>'0');
	signal short_in_s, short_out_s	: std_logic_vector( 7 downto 0):= (others=>'0');
	signal clip_out_s, clip_sink_s	: std_logic := '0';
begin

	clock_proc:
	process begin
		clock <= '1';
		wait for g_clock_period/2;
		clock <= '0';
		wait for g_clock_period/2;
	end process;

	clipperasync: component clipper_async
		generic map(g_input_width => 16, g_output_width => 8)
		port map(
			DIN  => long_in_a,
			DOUT => short_out_a,
			CLIP => clip_out_a
		);

	expanderasync: component clipper_async
		generic map(g_input_width => 8, g_output_width => 16)
		port map(
			DIN  => short_in_a,
			DOUT => long_out_a,
			CLIP => clip_sink_a
		);

	clippersync: component clipper_sync
		generic map(g_input_width => 16, g_output_width => 8)
		port map(
			CLK  => clock,
			DIN  => long_in_s,
			DOUT => short_out_s,
			CLIP => clip_out_s
		);

	expandersync: component clipper_sync
		generic map(g_input_width => 8, g_output_width => 16)
		port map(
			CLK  => clock,
			DIN  => short_in_s,
			DOUT => long_out_s,
			CLIP => clip_sink_s
		);

	short_in_a <= short_out_a;
	short_in_s <= short_out_s;

	data_generation:
	process begin
		long_in_s <= long_in_s + x"021";
		long_in_a <= long_in_a + x"022";
		wait for g_clock_period;
	end process;

end sim;

