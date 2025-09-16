-- Aias Karioris, 2025

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

entity bram_sim is
	generic(g_clock_period: time:= 5 ns);
end bram_sim;

architecture sim of bram_sim is
	component dp_bram is
		generic (g_addresswidth, g_wordwidth, g_wordcount: positive);
		port (
			CLK			: IN std_logic;
			-- Port A
			PORTA_EN	: IN  std_logic;
			PORTA_WE	: IN  std_logic;
			PORTA_ADDR	: IN  std_logic_vector(g_addresswidth-1 downto 0);
			PORTA_DIN	: IN  std_logic_vector(g_wordwidth-1 downto 0);
			PORTA_DOUT	: OUT std_logic_vector(g_wordwidth-1 downto 0);
			-- Port B
			PORTB_EN	: IN  std_logic;
			PORTB_WE	: IN  std_logic;
			PORTB_ADDR	: IN  std_logic_vector(g_addresswidth-1 downto 0);
			PORTB_DIN	: IN  std_logic_vector(g_wordwidth-1 downto 0);
			PORTB_DOUT	: OUT std_logic_vector(g_wordwidth-1 downto 0)
		);
	end component;

	-- Signals
	signal clock, en_a, en_b, we_a, we_b : std_logic := '0';
	signal address_a, address_b			 : std_logic_vector(10 downto 0) := (others=>'0');
	signal din_a, din_b, dout_a, dout_b	 : std_logic_vector(17 downto 0) := (others=>'0');

	type simulation_step_t is (init, portb_write, porta_read, doubletrouble);
	signal simulation_step: simulation_step_t;

begin
	uut: component dp_bram
	generic map(g_addresswidth=>11, g_wordwidth=>18, g_wordcount=>2048)
	port map(
		CLK => clock,
		PORTA_EN => en_a,
		PORTA_WE => we_a,
		PORTA_ADDR => address_a,
		PORTA_DIN => din_a,
		PORTA_DOUT => dout_a,

		PORTB_EN => en_b,
		PORTB_WE => we_b,
		PORTB_ADDR => address_b,
		PORTB_DIN => din_b,
		PORTB_DOUT => dout_b
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
		simulation_step <= init;
		en_a <= '1';
		en_b <= '1';
		we_a <= '0';
		we_b <= '0';

		address_a <= "000" & x"00";
		address_b <= "000" & x"00";
		din_a <= (others=>'0');

		wait for g_clock_period;

		-- Write into memory a 4 words
		simulation_step <= portb_write;
		we_b <= '1';
		for i in 0 to 4 loop
			din_b <= din_b + x"02";
			wait for g_clock_period;
			address_b <= address_b + x"1";

		end loop;
		we_b <= '0';

		-- Let's read something from port a
		simulation_step <= porta_read;
		address_a <= "000" & x"03";
		for i in 4 downto 0 loop
			wait for g_clock_period;
			address_a <= address_a - x"1";
		end loop;

		wait for g_clock_period;

		-- In a practical scenario, port a reads and port b writes
		simulation_step <= doubletrouble;
		address_a <= "000" & x"01";
		address_b <= "000" & x"00";
		for i in 0 to 100 loop
			din_b <= dout_a + x"2000";
			wait for g_clock_period;
			we_b <= '1';
			address_a <= address_a + x"01";
			address_b <= address_b + x"01";
		end loop;
	end process;


end sim;
