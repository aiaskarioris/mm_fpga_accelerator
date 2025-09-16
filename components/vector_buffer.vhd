-- Aias Karioris, 2025

-- Vector Buffer
-- Buffer of controllable length that can store 2 vectors.
--
-- When resetting, the internal index of the buffer is set to 0.
-- The length of a buffer is set automatically by monitoring how
-- many valid cycles `STORE` is high.
--
-- A buffer can be selected by specifying its number with `CONTROLIN`
-- on the same cycle `VALID` is set.
--
-- To write to a buffer enable `VALID` and `STORE`. Allow one idle
-- cycle if a buffer change should occur. After the last cycle after writing
-- `STORE` should be set low immediately.
--
-- Once writing has completed the component will index itself to the first
-- element of the buffer. During this time it is possible to select a different
-- buffer as described above. The index will be kept at the start of the buffer
-- until `ADVANCE` is set high. To output data keep `ADVANCE` high. For every
-- cycle `ADVANCE` is high the interanl index will keep incrementing. Once the
-- index reaches the length of the buffer it will reset automatically.
--
-- Setting `BYPASSEN` high will disable the component's logic and `PORTOUT` will
-- output `PORTIN`'s content with one cycle latency. `VALIDIN` will be forwarded
-- to `VALIDOUT` with one cycle latency as well.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;

entity vector_buffer is
	generic (
		g_port_elem_width	: positive := 8;
		g_port_elem_count	: positive := 4;

		g_buffer_count		: positive := 2;
		g_max_buffer_length : positive := 1024 -- in `g_port_elem_count` bytes

		-- Note: For simplicity, the final, total length of one buffer will be equal to
		-- `g_port_elem_count` * `g_max_buffer_length` bytes.
	);
	port (
		CLK, RST	: IN  std_logic := '0';
		-- Data inputs
		PORTIN		: IN  std_logic_vector(g_port_elem_width*g_port_elem_count-1 downto 0)	:= (others=>'0');
		-- Data output
		PORTOUT		: OUT std_logic_vector(g_port_elem_count*g_port_elem_width-1 downto 0)	:= (others=>'0');

		-- Control Ports
		ADVANCE		: IN  std_logic := '0'; -- Outputs next adress when '1'
		STORE		: IN  std_logic := '0'; -- Set to '1' to write into the selected buffer
		CONTROLIN	: IN  std_logic_vector(12 downto 0)  := (others=>'0'); -- General Purpose Control Port
		VALID		: IN  std_logic := '0';

		-- Bypass output
		BYPASSEN	: IN  std_logic := '0'; -- Set to '1' to enable bypass
		VALIDOUT	: OUT std_logic := '0';
		BYPASSOUT	: OUT std_logic_vector(g_port_elem_count*g_port_elem_width-1 downto 0)	:= (others=>'0')
	);
end vector_buffer;

architecture behavioral of vector_buffer is
-- Components
	component reg_pipeline is
		generic (g_width, g_depth: positive);
		port (
			CLK, CLK_EN	: IN  std_logic;
			DIN			: IN  std_logic_vector(g_width-1 downto 0);
			DOUT		: OUT std_logic_vector(g_width-1 downto 0)
		);
	end component;

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

-- Type/Constants
	constant c_control_port_width	: positive	:= 13;
	constant c_port_width			: positive	:= g_port_elem_count*g_port_elem_width;
	constant c_length_reg_width		: positive	:= log2int(g_max_buffer_length);
	constant c_buffer_sel_reg_width : positive	:= log2int(g_buffer_count);

	type buffer_t is array (0 to g_max_buffer_length*g_buffer_count-1) of std_logic_vector(c_port_width-1 downto 0);
	-- Attribute of buffer_t
	attribute ram_style	: string;

-- I/O Signals
	signal clock		: std_logic := '0';
	signal reset_in		: std_logic := '0';
	signal valid_in		: std_logic := '0';
	signal advance_in	: std_logic := '0';
	signal store_in		: std_logic := '0';
	signal bypass_en	: std_logic := '0';
	signal valid_out	: std_logic := '0';

	signal data_in		: std_logic_vector(c_port_width-1 downto  0)		:= (others=>'0');
	signal control_in	: std_logic_vector(c_control_port_width-1 downto 0)	:= (others=>'0');
	signal data_out		: std_logic_vector(c_port_width-1 downto 0)			:= (others=>'0');
	signal bypass_out	: std_logic_vector(c_port_width-1 downto 0)			:= (others=>'0');

-- Registers
	-- Register with the last `store_in` value
	signal store_reg		: std_logic		:= '0';

	-- Write Pipepline Registers
	signal data_in_preg		: std_logic_vector(c_port_width-1 downto 0) := (others=>'0');

	-- The buffers themselves
	signal 		data		: buffer_t ;

	-- Currently selected buffer
	signal buffer_sel_reg	: std_logic_vector(c_buffer_sel_reg_width-1 downto 0)	:= (others=>'0');
	signal buffer_sel		: positive;

	-- Register of read/write index within selected buffer
	signal data_idx_reg		: std_logic_vector(c_length_reg_width-1 downto 0):= (others=>'0');
	signal data_idx			: positive;

	-- Buffer length
	type buffer_len_reg_t is array(g_buffer_count-1 downto 0) of std_logic_vector(c_length_reg_width-1 downto 0);
	signal buffer_len_reg	: buffer_len_reg_t := (others=>(others=>'0'));

-- Internal Signals
	-- Register for addressing memory; combines `buffer_sel` and `data_idx`
	signal mem_idx_sig		: std_logic_vector(c_buffer_sel_reg_width+c_length_reg_width-1 downto 0) := (others=>'0');
	signal mem_idx			: positive;

	-- Active buffer's length
	signal active_buffer_len: std_logic_vector(c_length_reg_width-1 downto 0) := (others=>'0');

	signal pipeline_clock_en: std_logic := '0';

begin
	-- Wire input ports to their signals
	clock <= CLK;
	reset_in <= RST;
	valid_in <= VALID;
	advance_in <= ADVANCE;
	store_in <= STORE;
	bypass_en <= BYPASSEN;

	data_in <= PORTIN;
	control_in <= CONTROLIN;

	-- Update `store_reg`
	process(CLK)
	begin
		if rising_edge(CLK) then
			store_reg <= store_in;
		end if;
	end process;

	-- Buffer Select Register
	process(CLK)
	begin
		if rising_edge(CLK) then
			if reset_in = '1'  then
				buffer_sel_reg <= (others=>'0');
			-- elsif valid_in = '1' then
			-- 	buffer_sel_reg <= control_in(c_buffer_sel_reg_width-1 downto 0);
			-- else
			-- 	buffer_sel_reg <= buffer_sel_reg;
			else
				buffer_sel_reg <= control_in(c_buffer_sel_reg_width-1 downto 0);
			end if;
		end if;
	end process;

	active_buffer_len <= buffer_len_reg(to_integer(buffer_sel_reg));

	pipeline_clock_en <= '1' when (valid_in = '1' or data_idx_reg = (active_buffer_len - "01")) else '0';
	-- Once all data has been received, `valid_in` will go low but we'll still need to keep
	-- the pipelines going for the last write to complete.

	data_in_pipeline: component reg_pipeline
	generic map(g_width=>g_port_elem_count*g_port_elem_width, g_depth=>1)
	port map (
		CLK => clock,
		CLK_EN => pipeline_clock_en,
		DIN  => data_in,
		DOUT => data_in_preg
	);


	-- Keep an integer signal of `buffer_sel_reg` for indexing
	buffer_sel <= to_integer(buffer_sel_reg);

	-- Keep an integer signal of the read/write index
	data_idx <= to_integer(data_idx_reg);

	-- Combine index registers into one
	mem_idx_sig <= buffer_sel_reg & data_idx_reg;

	-- Keep an integer signal of `mem_idx_sig`
	mem_idx <= to_integer(mem_idx_sig);


	process(CLK)
	begin
		if rising_edge(CLK) then
			if reset_in = '1' then
				-- Clear registers
				-- Clear outputs
				data_out <= (others=>'0');

				-- Lengths are cleared only if `valid_in` is asserted;
				-- This way the length registers can retain their values even when the component
				-- is reset and getting in the proper state to output data.
				if valid_in = '1' then
					len_reg_clear: for i in 0 to g_buffer_count-1 loop
						buffer_len_reg(i) <= (others=>'0');
					end loop len_reg_clear;
				end if;
			else
				-- This process is "de-activated" if bypass is enabled
				if bypass_en='0' then
					-- A data store begins
					if valid_in = '1' and store_in='1' and store_reg='0' then
						data(mem_idx) <= data_in_preg;
						data_idx_reg <= (others=>'0');
						data_out <= (others=>'0');
						-- buffer_len_reg(to_integer(buffer_sel_reg)) <= (others=>'0');

					-- A data store is ongoing
					elsif store_in='1' and store_reg='1' and valid_in = '1' then
						data(mem_idx) <= data_in_preg;
						data_idx_reg <= data_idx_reg + '1';
						data_out <= (others=>'0');
						buffer_len_reg(to_integer(buffer_sel_reg)) <= buffer_len_reg(to_integer(buffer_sel_reg)) + "01";

					-- A data store has been completed
					elsif store_in='0' and store_reg='1' then
						data(mem_idx) <= data_in_preg;
						data_idx_reg <= (others=>'0');
						data_out <= (others=>'0');
						buffer_len_reg(to_integer(buffer_sel_reg)) <= buffer_len_reg(to_integer(buffer_sel_reg)) + "01"; -- last one

					-- Data should be outputted
					elsif store_in='0' and store_reg='0' then
						data_out <= data(mem_idx);

						-- `data_idx_reg` increases only when `advance_in` is high
						-- Increment `data_idx_reg` or reset it if required
						if advance_in = '1' and data_idx_reg < active_buffer_len - "01" then
							data_idx_reg <= data_idx_reg + "01";
						elsif advance_in = '1' and data_idx_reg >= active_buffer_len - "01" then
							data_idx_reg <= (others=>'0');
						else
							-- Don't update
							data_idx_reg <= data_idx_reg;
						end if;

					-- Nothing should happen
					else
						data_out <= data(mem_idx);
						data_idx_reg <= data_idx_reg;
					end if;
				end if; -- valid_in/bypass_en

			end if; -- reset_in
		end if;
	end process;


	-- Bypass functionality
	process(CLK)
	begin
		if rising_edge(CLK) then
			if bypass_en = '1' then
				bypass_out <= data_in;
				valid_out  <= valid_in;
			else
				bypass_out <= (others=>'0');
				valid_out <= '0';
			end if;
		end if;
	end process;

	-- Wire output signals to their ports
	VALIDOUT  <= valid_out;
	PORTOUT   <= data_out;
	BYPASSOUT <= bypass_out;
end behavioral;
