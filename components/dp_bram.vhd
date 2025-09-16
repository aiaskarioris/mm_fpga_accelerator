-- Aias Karioris, 2025

-- Dual-Port Block Ram Component
--
-- This component creates an interface for
-- a dual-port block ram. It is written
-- in such a way as to get implemented with
-- BRAM in the final Vivado implementation.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;


-- BRAMs in Zynq Ultrascale+ can be configured as 9x4K, 18x2K or 36x1K

entity dp_bram is
	generic (
		g_addresswidth	: positive := 11;
		g_wordwidth		: positive := 18;
		g_wordcount		: positive := 2048
	);
	port (
		CLK		: IN std_logic := '0';
		-- Port A
		PORTA_EN	: IN  std_logic := '0';
		PORTA_WE	: IN  std_logic := '0';
		PORTA_ADDR	: IN  std_logic_vector(g_addresswidth-1 downto 0)	:= (others => '0');
		PORTA_DIN	: IN  std_logic_vector(g_wordwidth-1 downto 0)		:= (others => '0');
		PORTA_DOUT	: OUT std_logic_vector(g_wordwidth-1 downto 0)		:= (others => '0');
		-- Port B
		PORTB_EN	: IN  std_logic := '0';
		PORTB_WE	: IN  std_logic := '0';
		PORTB_ADDR	: IN  std_logic_vector(g_addresswidth-1 downto 0)	:= (others => '0');
		PORTB_DIN	: IN  std_logic_vector(g_wordwidth-1 downto 0)		:= (others => '0');
		PORTB_DOUT	: OUT std_logic_vector(g_wordwidth-1 downto 0)		:= (others => '0')
	);
end dp_bram;

architecture behavioral of dp_bram is
-- Functions
-- Types/Constants
	type ram_t is array(0 to g_wordcount-1) of std_logic_vector(g_wordwidth-1 downto 0);

-- I/O Signals
	signal en_a, en_b, we_a, we_b	: std_logic := '0';
	signal addr_a, addr_b	: std_logic_vector(g_addresswidth-1 downto 0)	:= (others=>'0');
	signal din_a, din_b		: std_logic_vector(g_wordwidth-1 downto 0)		:= (others=>'0');
	signal dout_a, dout_b	: std_logic_vector(g_wordwidth-1 downto 0)		:= (others=>'0');

-- Registers
	attribute ram_style : string;
	shared  variable data	: ram_t;
	--attribute ram_style of data : variable is "block";
	--signal data: ram_t := (others=>(others=>'1'));

-- Internal Signals
	-- Addresses in Ports A and B converted to integers
	signal addr_a_int		: integer;
	signal addr_b_int		: integer;

begin
	-- Wire input ports to signals
	en_a <= PORTA_EN;
	en_b <= PORTB_EN;
	we_a <= PORTA_WE;
	we_b <= PORTB_WE;
	addr_a <= PORTA_ADDR;
	addr_b <= PORTB_ADDR;
	din_a <= PORTA_DIN;
	din_b <= PORTB_DIN;

	addr_a_int <= to_integer(addr_a);
	addr_b_int <= to_integer(addr_b);

	-- Port A
	process(CLK)
	begin
		if rising_edge(CLK) then
			if en_a = '1' then
				dout_a <= data(addr_a_int);
				if we_a = '1' then
					-- Writing to `data` with two processes causes simulation to fail
					data(addr_a_int) := din_a;
				end if;
			end if;
		end if;
	end process;

	process(CLK)
	begin
		if rising_edge(CLK) then
			if en_b = '1' then
				dout_b <= data(addr_b_int);
				if we_b = '1' then
					data(addr_b_int) := din_b;
				end if;
			end if;
		end if;
	end process;

	-- Old method
	-- process(CLK)
	-- begin
	-- 	if rising_edge(CLK) and en_a = '1' then
	-- 		-- Read
	-- 		if we_a = '0' then
	-- 			dout_a <= data(addr_a_int);
	-- 		-- Write
	-- 		else
	-- 			--data(addr_a_int) <= din_a;
	-- 			-- Simulation doesn't like dual port memories
	-- 		end if;
	-- 	end if;
	-- end process;
 --
	-- -- Port B (same as Port A)
	-- process(CLK)
	-- begin
	-- 	if rising_edge(CLK) and en_b = '1' then
	-- 		-- Read
	-- 		if we_b = '0' then
	-- 			dout_b <= data(addr_b_int);
	-- 		-- Write
	-- 		else
	-- 			data(addr_b_int) <=din_b;
	-- 		end if;
	-- 	end if;
	-- end process;

	-- Wire signals to output ports
	PORTA_DOUT <= dout_a;
	PORTB_DOUT <= dout_b;
end behavioral;
