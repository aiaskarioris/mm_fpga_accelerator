-- Aias Karioris, 2025

-- Multiplier Block
--
-- This component receives two (vertical and one horizontal) vectors (elements of each vector are
--   received in a parallel bus) and calculates the hadamard product of the two vectors.
--   The vertical input is optionally passed to the next PM in the array, if any.
-- The vertical input is accompanied by a 'valid_in' signal that is related
--   to AXI Stream's `valid`. The two inputs are only evaluated when valid is set.
-- This block forwards its vertical input to the block below one cycle after receiving
--   a valid signal. The multiplication output becomes available after a 2 cycles and is
--   accompanied by a valid signal as well. Multiplcations results are fed to an Accumulation Bank (different component)

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL; -- std_logic_vector addition
use IEEE.STD_LOGIC_SIGNED."*";

entity multiplier_block is
	generic (
		-- Number of multiplication pairs
		g_mult_pairs	: positive := 4;
		-- Number of bits in input port
		g_mat_input_elem_width		: positive := 8;
		g_vec_input_elem_width		: positive := 8
	);
	port (
		CLK, RST	: IN  std_logic	:= '0';
		-- Data inputs
		MAT_IN		: IN
			std_logic_vector(g_mat_input_elem_width*g_mult_pairs-1 downto 0)	:= (others => '0');
		VEC_IN		: IN
			std_logic_vector(g_vec_input_elem_width*g_mult_pairs-1 downto 0)	:= (others => '0');
		-- Data outputs
		MULTOUT 	: OUT
			std_logic_vector((g_mat_input_elem_width+g_vec_input_elem_width)*g_mult_pairs-1 downto 0)	:= (others => '0');

		-- Control I/O
		VALIDIN 	: IN  std_logic	:= '0'; -- from matrix data input (AXI Stream)
		VALIDOUT	: OUT std_logic := '0'
	);
end multiplier_block;

architecture behavioral of multiplier_block is
-- Functions
-- Components

-- Types/Constants
	constant c_vdata_width			: positive := g_mat_input_elem_width*g_mult_pairs;
	constant c_hdata_width			: positive := g_vec_input_elem_width*g_mult_pairs;
	constant c_output_elem_width	: positive := g_mat_input_elem_width+g_vec_input_elem_width;
	constant c_output_width			: positive := c_output_elem_width*g_mult_pairs;

-- I/O Signals
	signal rst_in		: std_logic;
	signal rst_out		: std_logic;
	signal mat_data_in	: std_logic_vector(c_vdata_width-1 downto 0)	:= (others=>'0');
	signal vec_data_in	: std_logic_vector(c_hdata_width-1 downto 0)	:= (others=>'0');
	signal mult_out		: std_logic_vector(c_output_width-1 downto 0)	:= (others=>'0');
	signal valid_out	: std_logic := '0';
	signal valid_in		: std_logic := '0';

-- Registers
	signal m_reg		: std_logic_vector(c_vdata_width-1 downto 0)	:= (others=>'0');
	signal v_reg		: std_logic_vector(c_hdata_width-1 downto 0)	:= (others=>'0');
	--signal mult_out_reg	: std_logic_vector(c_output_width-1 downto 0)	:= (others=>'0');
	signal valid_reg	: std_logic := '0';

-- Internal Signals
	signal mult_async	: std_logic_vector(c_output_width-1 downto 0)	:= (others=>'0');

begin
	-- Wire input ports to their signals
	rst_in <= RST;
	mat_data_in <= MAT_IN;
	vec_data_in <= VEC_IN;
	valid_in <= VALIDIN;

	-- Update registers and outputs
	process(CLK)
	begin
		if rising_edge(CLK) then
			if rst_in = '1' then
				m_reg <= (others => '0');
				v_reg <= (others => '0');
				valid_reg <= '0';
				valid_out <= '0';
			else
				-- If `valid_in` is set, update registers
				-- Note: Here we are checking the INPUT, not the register
				if valid_in = '1' then
					v_reg <= vec_data_in;
					m_reg <= mat_data_in;
				else
					v_reg <= v_reg;
					m_reg <= m_reg;
				end if;

				-- Update `valid_reg`
				valid_reg <= valid_in;

				-- valid_out should be set once mult. results are ready
				valid_out <= valid_reg;
			end if;
		end if;
	end process;


	multiplications:
	for i in g_mult_pairs-1 downto 0 generate
		mult_async((i+1)*c_output_elem_width-1 downto i*c_output_elem_width) <=
		  m_reg((i+1)*g_mat_input_elem_width-1 downto i*g_mat_input_elem_width)
		  * v_reg((i+1)*g_vec_input_elem_width-1 downto i*g_vec_input_elem_width);
	end generate multiplications;

	-- Multiplication logic
	process(CLK)
	begin
		if rising_edge(CLK) then
			if rst_in = '1' then
				mult_out <= (others=>'0');

			-- Do multiplications if valid was received
			-- NOTE: will this make the multipliers run less frequently => less costly?
			elsif valid_reg = '1' then
				mult_out <= mult_async;
			else
				mult_out <= mult_out;
			end if;
		end if;
	end process;

	-- Reset propagation
	process(CLK)
	begin
		if rising_edge(CLK) then
			rst_out <= rst_in;
		end if;
	end process;

	-- Wire output signals to their ports
	MULTOUT <= mult_out;
	VALIDOUT <= valid_out;
end behavioral;
