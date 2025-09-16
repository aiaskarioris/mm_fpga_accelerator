-- Aias Karioris, 2025

-- Lookup-Table Function Component
--
-- Receives signed integers as input and applies a non-linear function to them
-- using a LUT stored in memory. Contains two LUT Banks which are selectable.
--
-- Data is loaded into the LUT component once; only during system instantiation.
-- The intended use of this component is to store functions that will be used
-- multiple times during calculations. This means that in practice, the only
-- relevant time of the LUT component is the time taken to process an input.

-- Each bank's length is fixed and must be filled in its entirety. As such,
-- during data loading, the component only needs to now if the data present
-- on its `LOADIN` port is valid and it will automatically handle its registers.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;

entity lut_component is
	generic (
		g_elem_count		: positive := 4;
		-- Port Widths
		g_axi_port_width	: positive := 32;
		g_input_port_width	: positive := 18*4;
		g_output_port_width	: positive := 8*4;
		-- Length of bank in 36-bit words
		g_bank_length		: positive := 128
	);
	port (
		CLK, RST		: IN  std_logic := '0';
		-- Selects wich bank will be affected by an operation
		LUT_EN			: IN  std_logic	:= '0';
		-- Initiates a load operation; The operation must be completed
		LOAD_START		: IN  std_logic := '0';
		-- Load Port: used to write to the memory
		LOADIN_VALID	: IN  std_logic := '0';
		LOADIN			: IN  std_logic_vector(g_axi_port_width-1 downto 0) := (others=>'0');
		-- Data Input
		--DATAIN_VALID	: IN  std_logic := '0'; -- prob. not needed NOTE
		DATAIN			: IN  std_logic_vector(g_input_port_width-1 downto 0) := (others=>'0');
		-- Data Output
		--DATAOUT_VALID	: OUT std_logic := '0'; -- prob. not needed NOTE
		DATAOUT			: OUT std_logic_vector(g_output_port_width-1 downto 0) := (others=>'0')
	);
end lut_component;

architecture behavioral of lut_component is
-- Functions
	function log2int(arg: positive) return integer is
		variable tmp: positive := 1;
		variable ret: integer  := 0;
	begin
		if arg = 1 then
			return 0;
		end if;

		while arg > tmp loop
			tmp := tmp * 2;
			ret := ret + 1;
		end loop;
		return ret;
	end function;

-- Constants/Types
	constant c_load_elem_width		: positive := g_axi_port_width / g_elem_count;
	constant c_input_elem_width		: positive := g_input_port_width / g_elem_count;
	constant c_output_elem_width	: positive := g_output_port_width / g_elem_count;
	-- RAM Constants
	constant c_ram_count			: positive := g_elem_count / 2;
	constant c_ram_addr_width		: positive := log2int(g_bank_length);
	constant c_ram_word_width		: positive := 36;
	-- LSBs in each `DATAIN` element that get discarded
	constant c_discard_width		: positive := c_input_elem_width - c_ram_addr_width - log2int(g_elem_count) + 1; -- = 2,
	-- Width of register that indexes words in RAM
	constant c_index_reg_width		: positive := c_ram_addr_width;

	type load_state_t is (idle, ongoing, complete);

	-- Constant inputs
	constant c_low					: std_logic := '0';
	constant c_high					: std_logic := '1';
	constant c_ram_din_zeros		: std_logic_vector(c_ram_word_width-1 downto 0) := (others=>'0');
	constant c_max_index			: std_logic_vector(c_index_reg_width-1 downto 0):= (others=>'1');

-- Components
	component dp_bram is
		generic(g_addresswidth, g_wordwidth, g_wordcount: positive);
		port(
			CLK			: IN std_logic;
			PORTA_EN	: IN  std_logic;
			PORTA_WE	: IN  std_logic;
			PORTA_ADDR	: IN  std_logic_vector(g_addresswidth-1 downto 0);
			PORTA_DIN	: IN  std_logic_vector(g_wordwidth-1 downto 0);
			PORTA_DOUT	: OUT std_logic_vector(g_wordwidth-1 downto 0);
			PORTB_EN	: IN  std_logic;
			PORTB_WE	: IN  std_logic;
			PORTB_ADDR	: IN  std_logic_vector(g_addresswidth-1 downto 0);
			PORTB_DIN	: IN  std_logic_vector(g_wordwidth-1 downto 0);
			PORTB_DOUT	: OUT std_logic_vector(g_wordwidth-1 downto 0)
		);
	end component;

	-- Synchronous clippers
	component clipper_sync is
        generic( g_input_width, g_output_width : positive );
        port(
			CLK : IN  std_logic;
            DIN : IN  std_logic_vector(g_input_width-1  downto 0);
            DOUT: OUT std_logic_vector(g_output_width-1 downto 0);
            CLIP: OUT std_logic
        );
    end component;

-- I/O Signals
	signal reset		: std_logic := '0';
	signal lut_enable	: std_logic := '0';
	signal ld_start		: std_logic := '0';
	signal ldin_valid	: std_logic := '0';
	signal din_valid, dout_valid : std_logic := '0';

	signal load_in		: std_logic_vector(g_axi_port_width-1 downto 0) := (others=>'0');
	signal data_in		: std_logic_vector(g_input_port_width-1 downto 0) := (others=>'0');
	signal data_out		: std_logic_vector(g_output_port_width-1 downto 0) := (others=>'0');

-- Registers
	signal axi_in_reg	: std_logic_vector(g_axi_port_width-1 downto 0)  := (others=>'0');
	signal index_reg	: std_logic_vector(c_index_reg_width-1 downto 0) := (others=>'0');
	signal load_state	: load_state_t := idle;

	signal ldin_valid_lat1 : std_logic := '0';

-- Signals
	signal ram_en			: std_logic := '0';
	signal ram_w			: std_logic := '0';
	signal ram_out_reg_en	: std_logic := '0';

	-- Address signals for RAMs; Driven by `index_reg` or data input
	type ram_addr_t is array (c_ram_count*2-1 downto 0) of std_logic_vector(c_ram_addr_width-1 downto 0);
	signal ram_addr: ram_addr_t := (others=>(others=>'0'));

	-- Data input of RAMs; All ports with write capabilites use the same signals
	signal ram_data_in	: std_logic_vector(c_ram_word_width-1 downto 0) := (others=>'0');

	-- Data output of RAMs
	type ram_data_out_t is array (c_ram_count*2-1 downto 0) of std_logic_vector(c_ram_word_width-1 downto 0);
	signal ram_data_out	: ram_data_out_t := (others=>(others=>'0'));

	-- Parts of elements of `DATAIN` that go to RAM's address and MUX selector respectively
	type din_to_addr_t is array (g_elem_count-1 downto 0) of std_logic_vector(c_ram_addr_width-1 downto 0);
	type din_to_sel_t  is array (g_elem_count-1 downto 0) of std_logic_vector(log2int(g_elem_count)-1 downto 0);
	signal din_to_addr	: din_to_addr_t := (others=>(others=>'0'));
	signal din_to_sel	: din_to_sel_t  := (others=>(others=>'0'));

	-- Array of selected elements from all RAM Ports; These are forwarded to `DATAOUT`
	type dout_mux_out_t is array (g_elem_count-1 downto 0) of std_logic_vector(c_output_elem_width-1 downto 0);
	signal dout_mux_out	: dout_mux_out_t := (others=>(others=>'0'));

begin
	-- Wire input ports to signals
	reset <= RST;
	lut_enable <= LUT_EN;
	ld_start    <= LOAD_START;
	ldin_valid  <= LOADIN_VALID;
	load_in     <= LOADIN;
	data_in     <= DATAIN;

	-- `LOADIN` contents should be expanded before creating the write word for RAM;
	-- These expanders are asynchronous so they will introduce a cycle of latency from the input to RAM
	load_in_expanders: for i in g_elem_count-1 downto 0 generate
		ldin_exp: clipper_sync
		generic map(g_input_width=>c_load_elem_width, g_output_width=>c_ram_word_width/g_elem_count)
		port map(
			CLK  => CLK,
			DIN  => load_in((i+1)*c_load_elem_width-1 downto i*c_load_elem_width),
			DOUT => ram_data_in((i+1)*(c_ram_word_width/g_elem_count)-1 downto i*c_ram_word_width/g_elem_count),
			CLIP => open
		);
	end generate load_in_expanders;

	-- Split `DATAIN` into distinct signals for each element
	data_in_split: for i in g_elem_count-1 downto 0 generate
	-- elem_left_bound	:= (i+1)*c_input_elem_width-1;
	-- elem_right_bound	:= i*c_input_elem_width+c_discard_width;
	-- din_to_addr <= data_in( elem_left_bound downto elem_left_bound-c_ram_addr_width )
	-- din_to_sel  <= data_in( elem_right_bound+log2int(g_elem_count)-1 downto elem_right_bound )

		din_to_addr(i) <= data_in((i+1)*c_input_elem_width-1 downto (i+1)*c_input_elem_width-c_ram_addr_width );
		din_to_sel(i)  <= data_in(i*c_input_elem_width+c_discard_width+log2int(g_elem_count)-1 downto i*c_input_elem_width+c_discard_width);
	end generate data_in_split;

	-- Instantiate RAM blocks for storing and reading LUTs;
	-- A Dual-Port BRAM is instantiated for every pair of output elements.
	-- Each RAM outputs a 36-bit quad with 4 elements each and the 2 LSB of the input
	-- select which element of the outputted quad to use
	ram_array: for i in c_ram_count-1 downto 0 generate
		type mux_sel_int_t is array (g_elem_count-1 downto 0) of integer;
		signal mux_sel_int : mux_sel_int_t;
	begin
		ram: dp_bram
		generic map (
			g_addresswidth => c_ram_addr_width,
			g_wordwidth    => c_ram_word_width,
			g_wordcount    => g_bank_length
		)
		port map (
			CLK => CLK,
			PORTA_EN => ram_en,
			PORTA_WE => c_low, -- port a is not used for writing
			PORTA_ADDR => ram_addr(2*i),
			PORTA_DIN  => c_ram_din_zeros,
			PORTA_DOUT => ram_data_out(2*i),

			PORTB_EN => ram_en,
			PORTB_WE => ram_w,
			PORTB_ADDR => ram_addr(2*i+1),
			PORTB_DIN  => ram_data_in, -- All RAM modules use the same input for writing (LOADIN)
			PORTB_DOUT => ram_data_out(2*i+1)
		);

		address_mux: for p in 1 downto 0 generate
			-- RAM Address Multiplexing
			with load_state select ram_addr(2*i+p) <=
				din_to_addr(2*i+p) when idle, -- DATA In is the address
				index_reg when ongoing | complete; -- use the internal register (requires padding)
		end generate address_mux;

		byte_sel_as_positives: for p in 1 downto 0 generate
			-- Cast mux-sel portion of inputs to integer
			mux_sel_int(i*2+p) <= to_integer(din_to_sel(i*2+p));
		end generate byte_sel_as_positives;

		-- Byte Multiplexing of RAM's data output
		byte_sel_mux: for p in 1 downto 0 generate
			signal select_byte : integer;
		begin
			select_byte <= mux_sel_int(i*2+p);
			dout_mux_out(2*i+p) <= ram_data_out(2*i+p)((select_byte+1)*c_output_elem_width-1 downto select_byte*c_output_elem_width);
		end generate byte_sel_mux;

		-- Wire Byte MUXs to `DATAOUT`
		data_out_wiring: for p in 1 downto 0 generate
			process(CLK)
			begin
				if rising_edge(CLK) then
					if reset = '1' then
						data_out((2*i+p+1)*c_output_elem_width-1 downto (2*i+p)*c_output_elem_width) <= (others=>'0');
					else
						data_out((2*i+p+1)*c_output_elem_width-1 downto (2*i+p)*c_output_elem_width) <= dout_mux_out(2*i+p);
					end if;
				end if;
			end process;
		end generate data_out_wiring;
	end generate ram_array;

	-- Mulitplex RAM Enable Signal, which is common for all ports;
	with load_state select ram_en <=
		lut_enable 			when idle,
		ldin_valid_lat1	when ongoing, -- Disable if `LOADIN_VALID` is low (with a cycle of latency)
		'0' 			when complete; -- Turn off ports during complete

	with load_state select ram_w <=
		'0'			when idle,
		'1'			when ongoing | complete;

	-- Load Valid In with latency
	process(CLK) begin
		if rising_edge(CLK) then ldin_valid_lat1 <= ldin_valid and lut_enable; end if;
	end process;

	-- State Control
	process(CLK)
	begin
		if rising_edge(CLK) then
			if reset = '1' then
				load_state <= idle;
				index_reg <= (others=>'0');
			else
				case load_state is
					when idle =>
						load_state <= ongoing when ld_start = '1' else idle;
						index_reg  <= (others=>'0');
					when ongoing =>
						index_reg  <= index_reg + "01" when ldin_valid = '1' else index_reg;
						load_state <= complete when (index_reg = c_max_index and ldin_valid_lat1='1') else ongoing;
					when complete =>
						load_state <= idle;
						-- index_reg will be 0 by now
				end case;
			end if;
		end if;
	end process;


	DATAOUT <= data_out;
end behavioral;
