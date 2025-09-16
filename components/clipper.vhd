-- Aias Karioris, 2025

-- Clipper
--
-- Receives an `std_logic_vector` of a certain numeric value and forwards
-- it to a narrower output, saturating the value if required.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity clipper_async is
	generic (
		g_input_width	: positive := 16;
		g_output_width	: positive := 18
	);
	port (
		DIN	:	IN   std_logic_vector(g_input_width-1 downto 0)  := (others=>'0');
		DOUT:	OUT  std_logic_vector(g_output_width-1 downto 0) := (others=>'0');
		CLIP:	OUT  std_logic := '0' -- Set high when a clipping occurs
	);
end clipper_async;

architecture behavioral of clipper_async is
-- Constants
	-- Number of bits by which the input port is wider than the output port
	-- Negative differences are supported
	constant c_port_difference	: integer := g_input_width - g_output_width;
begin
	ifgen:
	-- The input will be clipped
	if c_port_difference > 0 generate
		-- Note: as overhead we refer to all bits more significant than the MSB of the output, including the MSB

		-- Values used to determine if the input should be clipped
		-- If `overhead_part` is not equal to one of these two constants, clipping occurs
		constant c_overhead_pos	: std_logic_vector(g_input_width-1 downto g_output_width-1) := (others=>'0');
		constant c_overhead_neg	: std_logic_vector(g_input_width-1 downto g_output_width-1) := (others=>'1');

		signal sign_bit		: std_logic := '0';
		signal should_clip	: std_logic := '0';
		signal overhead_part: std_logic_vector(g_input_width-1 downto g_output_width-1);
		begin
			-- Get input's overhead
			overhead_part <= DIN(g_input_width-1 downto g_output_width-1);
			sign_bit <= DIN(g_input_width-1);

			-- Check if the input should be clipped
			should_clip <= '0' when (overhead_part = c_overhead_pos or overhead_part = c_overhead_neg)
							else '1';

			-- Select output
			with should_clip select DOUT <=
				DIN(g_output_width-1 downto 0) when '0',
				sign_bit & (g_output_width-2 downto 0 => not sign_bit) when '1',
				(others=>'-') when others; -- not sure why vivado wants this

			-- Output `CLIP` signal
			CLIP <= should_clip;

	-- Bits will be appended to the input
	elsif c_port_difference < 0 generate
		signal extension_bit: std_logic;
		begin
			extension_bit <= DIN(g_input_width-1);
			DOUT <= (g_output_width-1 downto g_input_width => extension_bit) & DIN;
			CLIP <= '0';

	-- Ports are the same; This component will probably not even get synthesized
	else generate
		begin
			assert true report "Input and Output port of clipper are equal in size." severity warning;
			DOUT <= DIN;
			CLIP <= '0';
	end generate ifgen;

end behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity clipper_sync is
	generic (
		g_input_width	: positive := 16;
		g_output_width	: positive := 18
	);
	port (
		CLK	:	IN   std_logic := '0';
		DIN	:	IN   std_logic_vector(g_input_width-1 downto 0)  := (others=>'0');
		DOUT:	OUT  std_logic_vector(g_output_width-1 downto 0) := (others=>'0');
		CLIP:	OUT  std_logic := '0' -- Set high when a clipping occurs
	);
end clipper_sync;

architecture behavioral of clipper_sync is
-- Constants
	-- Number of bits by which the input port is wider than the output port
	-- Negative differences are supported
	constant c_port_difference	: integer := g_input_width - g_output_width;
-- I/O Signals
	signal data_in	: std_logic_vector(g_input_width-1  downto 0) := (others=>'0');
	signal data_out	: std_logic_vector(g_output_width-1 downto 0) := (others=>'0');
	signal clip_out	: std_logic;
begin
	ifgen:
	-- The input will be clipped
	if c_port_difference > 0 generate
		-- Note: as overhead we refer to all bits more significant than the MSB of the output, including the MSB
		-- Values used to determine if the input should be clipped
		-- If `overhead_part` is not equal to one of these two constants, clipping occurs
		constant c_overhead_pos	: std_logic_vector(g_input_width-1 downto g_output_width-1) := (others=>'0');
		constant c_overhead_neg	: std_logic_vector(g_input_width-1 downto g_output_width-1) := (others=>'1');

		signal sign_bit		: std_logic := '0';
		signal should_clip	: std_logic := '0';
		signal overhead_part: std_logic_vector(g_input_width-1 downto g_output_width-1);
		begin
			data_in <= DIN;

			-- Get input's overhead
			overhead_part <= data_in(g_input_width-1 downto g_output_width-1);
			sign_bit <= data_in(g_input_width-1);

			-- Check if the input should be clipped
			should_clip <= '0' when (overhead_part = c_overhead_pos or overhead_part = c_overhead_neg)
							else '1';

			-- Select output
			process(CLK)
			begin
				if rising_edge(CLK) then
					with should_clip select data_out <=
						data_in(g_output_width-1 downto 0) when '0',
						sign_bit & (g_output_width-2 downto 0 => not sign_bit) when '1',
						(others=>'-') when others; -- not sure why vivado wants this

					clip_out <= should_clip;
				end if;
			end process;

			DOUT <= data_out;
			-- Output `CLIP` signal
			CLIP <= clip_out;

	-- Bits will be appended to the input
	elsif c_port_difference < 0 generate
		signal extension_bit: std_logic;
		begin
			data_in <= DIN;

			extension_bit <= data_in(g_input_width-1);
			process(CLK)
			begin
				if rising_edge(CLK) then
					data_out <= (g_output_width-1 downto g_input_width => extension_bit) & DIN;
				end if;
			end process;

			DOUT <= data_out;
			CLIP <= '0';

	-- Ports are the same; This component will probably not even get synthesized
	else generate
		begin
			assert true report "Input and Output port of synchronous clipper are equal in size." severity warning;
			data_in <= DIN;

			process(CLK)
			begin
				if rising_edge(CLK) then
					data_out <= data_in;
				end if;
			end process;

			DOUT <= data_out;
			CLIP <= '0';
	end generate ifgen;

end behavioral;
