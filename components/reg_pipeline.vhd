-- Aias Karioris, 2025

-- Pipeline with Clock Enable
--
-- Pipelined shift registers with configurable width and depth
-- A Clock Enable pin can optionally be used to disable
-- movement momentarily if required.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;

entity reg_pipeline is
	generic (
		g_width: positive := 16;
		g_depth: positive := 3
	);
	port (
		CLK		: IN  std_logic := '0';
		CLK_EN	: IN  std_logic := '0';
		DIN		: IN  std_logic_vector(g_width-1 downto 0) := (others=>'0');
		DOUT	: OUT std_logic_vector(g_width-1 downto 0) := (others=>'0')
	);
end reg_pipeline;

architecture behavioral of reg_pipeline is
	type sh_reg_t is array (g_depth-1 downto 0) of std_logic_vector(g_width-1 downto 0);
	signal sh_reg: sh_reg_t := (others=>(others=>'0'));

	signal clock_en		: std_logic := '0';
	signal data_in, data_out	: std_logic_vector(g_width-1 downto 0) := (others=>'0');
begin
	clock_en <= CLK_EN;
	data_in <= DIN;

	process(CLK)
	begin
		if rising_edge(CLK) then
			if clock_en = '1' then
				-- Get input
				sh_reg(g_depth-1) <= data_in;

				-- Shift everything else
				for i in g_depth-1 downto 1 loop
					sh_reg(i-1) <= sh_reg(i);
				end loop;
			end if;
		end if;
	end process;

	data_out <= sh_reg(0);
	DOUT <= data_out;

end behavioral;
