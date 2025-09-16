-- Aias Karioris, 2025

-- Accumulator Bank
--
-- Component which contains a number of registers for storing multiple accumulation results.
-- If reset is high for two consecutive cycles, `CONTROLIN` sets the number of active lanes on the first cycle and the
-- number of rows expected on the second clock. Setting these numbers for Simple Mode is fairly straightforward.
-- In AI Mode, AB essentially expects an `output_vec_len + input_vec_len` long vector (it doesn't care that VB switches its buffers)
-- an a matrix of dimensions `(output_vec_len + input_vec_len) * input_vec_len`.
--
-- Once all expected additions have been made `COMPLETE` will be set high,
-- and `CONTROLIN` can be used to address 4 elements, which will be outputted on `DATAOUT`.
-- During this stage, the elements in the output are positioned as [ (MSB) 3 2 1 0 (LSB) ]

-- After `COMPLETE` becomes high, allow two cycles for `DATAOUT` to respond to `CONTROLIN`
-- In practice, this component is implemented with two dual port BRAM, as to allow a 4-element
-- output per clock cycle.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL; -- std_logic_vector addition
-- The "numeric_std_unsigned vs std_logic_arith" debate is long and tedious;
-- we'll use "NUMERIC_STD_UNSIGNGED" to be formal but they are (?) roughly the same

entity accum_bank is
	generic (
		g_elem_count		: positive := 4;
		-- Number of bits in input port
		g_input_port_width	: positive := 4*16;
		-- Number of bits in output port
		g_output_port_width	: positive := 4*18;
		-- Word size of BRAM / Width of one output element
		g_bram_word_width		: positive := 18;
		-- Number of (synthesized) accumulation lanes
		g_accumulator_count		: positive := 8;
		g_accum_counter_width	: positive := 3; -- =log2(g_accumulator_count)
		-- Size of cycle counter; should be big enough to count all the way up to 2*MATRIX_COL_SIZE/4
		g_cycle_counter_width	: positive := 12;
		-- Size in bits of control ports, should be MAX(g_accumulator_count, g_cycle_counter_width)
		g_control_port_width	: positive := 12

		-- Note some generics are defined for readability and don't change the component's
		-- functionality. Creating an automatically configurable addition network tree
		-- requires unreasonable effort for the scope of this project.
	);
	port (
		-- Clock/Reset
		CLK, RST	:	IN  std_logic	:= '0';
		RSTOUT		:	OUT std_logic	:= '0'; -- Passes reset to the next BA
		-- Data I/O Ports
		DATAIN		:	IN  std_logic_vector(g_input_port_width-1  downto 0)	:= (others => '0');
		DATAOUT		:	OUT std_logic_vector(g_output_port_width-1 downto 0)	:= (others => '0');
		VALIDIN		:	IN  std_logic	:= '0'; -- Accumulations only occur if VALIDIN is high
		COMPLETE	:	OUT std_logic	:= '0'; -- Will become high once the component is ready to transmit results
		-- Control ports (to set number of usable lanes)
		CONTROLIN	:	IN  std_logic_vector(g_control_port_width-1 downto 0)	:= (others => '0');
		CONTROLOUT	:	OUT std_logic_vector(g_control_port_width-1 downto 0)	:= (others => '0')
	);
end accum_bank;

architecture behavioral of accum_bank is
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

	function pow2int(arg: integer) return integer is
		variable tmp:  integer := 1;
		variable ret:  integer := 2;
	begin
		if arg = 0 then
			return 1;
		end if;

		while tmp < arg loop
			ret  := ret * 2;
			tmp  := tmp + 1;
		end loop;
		return ret;
	end function;

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

	component reg_pipeline is
		generic(g_depth, g_width: positive);
		port(
			CLK		: IN  std_logic;
			CLK_EN	: IN  std_logic;
			DIN		: IN  std_logic_vector(g_width-1 downto 0);
			DOUT	: OUT std_logic_vector(g_width-1 downto 0)
		);
	end component;

	component clipper_async is
		generic(g_input_width, g_output_width: positive);
		port(DIN: IN std_logic_vector(g_input_width-1 downto 0); DOUT: OUT std_logic_vector(g_output_width-1 downto 0); CLIP: OUT std_logic);
	end component;

-- Types/Constants
	-- Element widths of input & output ports
	constant c_input_elem_width	: integer := g_input_port_width / g_elem_count;
	constant c_output_elem_width: integer := g_output_port_width / g_elem_count;

	-- Number of RAM Blocks
	constant c_ram_count		: integer := g_elem_count / 2;

	-- Constants and signal types for the Addition Network Tree
	constant c_tree_depth		: integer := log2int(g_elem_count); -- log2(4) = 2 addition layers
	-- Signals for addition network; for each level in the tree a bit is added in the signals
	type add_net_input_signal_t  is array (g_elem_count-1 downto 0)   of std_logic_vector(c_input_elem_width-1 downto 0);
	type add_net_depth1_signal_t is array (g_elem_count/2-1 downto 0) of std_logic_vector(c_input_elem_width   downto 0);
	-- type add_net_depth2_signal_t is array (g_elem_count/4-1 downto 0) of std_logic_vector(c_input_elem_width+1 downto 0); --

	-- Types for BRAM data and address I/O
	type bram_dataio_t is array (c_ram_count-1 downto 0) of std_logic_vector(g_bram_word_width-1 downto 0);
	-- Address widths in ports are one bit shorter than `g_accum_counter_width`
	type bram_addr_t is array (c_ram_count-1 downto 0) of std_logic_vector(g_accum_counter_width-2 downto 0);


	-- Accumulator Type
	-- Size of an accumulator in bits, before possible clipping for storing in BRAM
	constant c_accumulator_width:	integer	:= c_input_elem_width+c_tree_depth+1;
	-- Size of stored accumulation result in memory (9/18/36)
	constant c_bram_din_void	:	std_logic_vector(g_bram_word_width-1 downto 0) := (others=>'0'); -- don't really care what ends up on Port A's DIN

	-- Size of shift-register for `valid_in`; This pipeline should be as long as
	-- to allow a '1' in `valid_in` to reach the Accumulator Gate Register by the time
	-- the result of the addition network's result reaches the accumulator's input
	constant c_valid_in_shreg_depth: positive := c_tree_depth + 3;

	-- Constant input signals
	constant c_high			: std_logic := '1';
	constant c_low			: std_logic := '0';

-- I/O Signals
	signal clock			:  std_logic;
	signal rst_in, rst_out	:  std_logic;
	signal data_in			:  std_logic_vector(g_input_port_width-1  downto 0)	:= (others =>'0');
	signal data_out			:  std_logic_vector(g_output_port_width-1 downto 0)	:= (others =>'0');
	signal valid_in			:  std_logic := '0';
	signal control_in		:  std_logic_vector(g_control_port_width-1 downto 0)	:= (others => '0');
	signal control_out		:  std_logic_vector(g_control_port_width-1 downto 0)	:= (others => '0');

-- Registers
	-- Register for selecting accumulation lane; Used to MUX/DEMUX lanes
	signal lane_sel_reg		:	std_logic_vector(g_accum_counter_width-1 downto 0) := (others=>'0');
	-- Register where a bit is set for every lane being used; used to reset `lane_sel_reg`
	signal lanes_used_reg	:	std_logic_vector(g_accum_counter_width-1 downto 0) := (others=>'1');

	-- Registers counting how many accumulation cycles have occurred and how many are expected
	signal cycle_counter	:	std_logic_vector(g_cycle_counter_width-1 downto 0) := (others=>'1');
	signal expected_cycles	:	std_logic_vector(g_cycle_counter_width-1 downto 0) := (others=>'1');
	signal process_complete	:	std_logic	:= '0'; -- set when cycle_counter = expected_cycles

	-- Registers (or signals if tree is not synchronous) of addition network tree
	signal add_net_input_reg :	add_net_input_signal_t	:= (others=>(others=>'1'));
	signal add_net_depth1_reg:	add_net_depth1_signal_t	:= (others=>(others=>'1'));
	-- Addition Network Tree output
	signal add_net_out_reg	:	std_logic_vector(c_accumulator_width-2 downto 0) := (others=>'1');

	-- Register for sum of `add_net_out_reg` and accumulated value (from Port A)
	-- Contents of this register are loaded into Port B
	signal final_accum_reg	:	std_logic_vector(g_bram_word_width-1 downto 0) := (others=>'0');

	-- Registers for BRAM I/O
	signal bram_douta_reg	:	bram_dataio_t	:= (others=>(others=>'0')); -- Output of Port A, added to `add_net_out_reg`
	signal bram_doutb_reg	:	bram_dataio_t	:= (others=>(others=>'0')); -- Output of Port B; Only used after process' complete

-- Pipelines
	-- Shift register (pipeline) of `valid_in`; Each subcomponent in the AB needs different latencies of this signal
	signal valid_in_shreg	:	std_logic_vector(c_valid_in_shreg_depth-1 downto 0)	:= (others=>'1');
	signal proc_compl_shreg	:	std_logic_vector(c_valid_in_shreg_depth-1 downto 0)	:= (others=>'0');
	signal lane_sel_lat1	:	std_logic_vector(g_accum_counter_width-1 downto 0)	:= (others=>'0');
	signal lane_sel_lat3	:	std_logic_vector(g_accum_counter_width-1 downto 0)	:= (others=>'0');

-- Internal Signals
	-- Register for storing control_in on the first RESET clock; Used only if RESET is held for two clocks
	signal ctrl_in_1_reg	:	std_logic_vector(g_control_port_width-1 downto 0)	:= (others=>'1');
	-- Set to '1' if reset is held high for more than 1 cycle
	signal reset_dur_reg	:	std_logic := '0';

	-- Integer signal for easily selecting a DOUT a Register
	signal douta_reg_sel	: integer := 0;

	-- Output of cycle counter shift register
	signal cc_shreg_out_1	:	std_logic_vector(g_cycle_counter_width-1 downto 0);
	signal cc_shreg_out_2	:	std_logic_vector(g_cycle_counter_width-1 downto 0);
	signal add_net_shreg_out:	std_logic_vector(c_accumulator_width-2 downto 0)	:= (others=>'0');
	signal lane_sel_shreg_out:	std_logic_vector(g_accum_counter_width-1 downto 0)	:= (others=>'0');

	signal bram_douta_rst	:	std_logic := '0';

	-- Signals for BRAM ports; a signal is created for each BRAM instance
	--signal bram_dinb_sig	:	bram_dataio_t	:= (others=>(others=>'0'));
	signal bram_douta_sig	:	bram_dataio_t	:= (others=>(others=>'0'));
	signal bram_doutb_sig	:	bram_dataio_t	:= (others=>(others=>'0'));
	signal bram_porta_addr	:	bram_addr_t		:= (others=>(others=>'0'));
	signal bram_portb_addr	:	bram_addr_t		:= (others=>(others=>'0'));

	signal bram_portb_we	:	std_logic_vector(c_ram_count-1 downto 0) := (others=>'0');
	signal bram_porta_en	:	std_logic_vector(c_ram_count-1 downto 0) := (others=>'0');
	signal bram_portb_en	:	std_logic_vector(c_ram_count-1 downto 0) := (others=>'0');

	-- Signal that drives `bram_portb_en` during computations; declaring this signal
	-- independently somewhat simplifies the code.
	signal portb_dyn_en		:	std_logic_vector(c_ram_count-1 downto 0) := (others=>'0');

	-- Signals for clipping data before storing in BRAM
	signal final_accum_sig_i:	std_logic_vector(c_accumulator_width-1 downto 0)	:= (others=>'0');
	signal final_accum_sig_o:	std_logic_vector(g_bram_word_width-1 downto 0)		:= (others=>'0');

	-- Signal for synchronously feeding `DATAOUT` register
	signal data_out_sig		:	std_logic_vector(g_output_port_width-1 downto 0)	:= (others=>'0');

-- Debug
	type dbg_dout_t is array (g_elem_count-1 downto 0) of std_logic_vector(c_output_elem_width-1 downto 0);
	signal dbg_dout : dbg_dout_t := (others=>(others=>'0'));

begin
	-- Wire input ports to their signals
	clock      <= CLK;
	rst_in     <= RST;
	data_in    <= DATAIN;
	valid_in   <= VALIDIN;
	control_in <= CONTROLIN;

	-- Pipelines
	-- `valid_in` pipeline
	process(CLK)
	begin
		if rising_edge(CLK) then
			if rst_in = '1' then
				valid_in_shreg <= (others=>'0');
			else
				valid_in_shreg(c_valid_in_shreg_depth-1) <= valid_in;
				for d in c_valid_in_shreg_depth-2 downto 0 loop
					valid_in_shreg(d) <= valid_in_shreg(d+1);
				end loop;
			end if;
		end if;
	end process;

	-- `process_complete` pipeline
	process(CLK)
	begin
		if rising_edge(CLK) then
			if rst_in = '1' then
				proc_compl_shreg <= (others=>'0');
			else
				proc_compl_shreg(c_valid_in_shreg_depth-1) <= process_complete;
				for d in c_valid_in_shreg_depth-2 downto 0 loop
					proc_compl_shreg(d) <= proc_compl_shreg(d+1);
				end loop;
			end if;
		end if;
	end process;

	-- Lane Select Pipeline; this pipeline uses as input the `lane_sel_reg`
	lane_sel_shreg: component reg_pipeline
		generic map(
			g_width => g_accum_counter_width,
			g_depth => 3
			--g_depth => 4
		)
		port map(
			CLK => clock,
			CLK_EN => c_high,
			DIN => lane_sel_lat1,
			DOUT => lane_sel_shreg_out
		);

	lane_sel_for_douta: component reg_pipeline
		generic map(
			g_width => g_accum_counter_width,
			g_depth => 2
			--g_depth => 3
		)
		port map(
			CLK => clock,
			CLK_EN => c_high,
			DIN => lane_sel_reg,
			DOUT => lane_sel_lat3
		);

	-- Cycle Counter Pipeline;
	-- We need two outputs from this pipeline so we chain two together
	cycle_counter_shreg_1: component reg_pipeline
		generic map(
			g_width => g_cycle_counter_width,
			g_depth => 2
			--g_depth => 3
		)
		port map(
			CLK => clock,
			CLK_EN => c_high,
			DIN => cycle_counter,
			DOUT => cc_shreg_out_1
		);

	cycle_counter_shreg_2: component reg_pipeline
		generic map(
			g_width => g_cycle_counter_width,
			g_depth => 2
			-- g_depth => 3
		)
		port map(
			CLK => clock,
			CLK_EN => c_high,
			DIN => cc_shreg_out_1,
			DOUT => cc_shreg_out_2
		);


	-- To keep a consistent output bandwidth, two Dual-Port RAMs are used;
	-- During accumulation, each BRAM is used once per two cycles. After completion,
	-- both BRAMs output one pair of data each on every cycle, thus outputting 4 elements in total.
	-- Each BRAM stores half of the accumulation results.
	ram_signals: for i in c_ram_count-1 downto 0 generate
		ram: component dp_bram
			generic map(
				g_addresswidth=>g_accum_counter_width-1,
				g_wordwidth=>g_bram_word_width,
				g_wordcount=>g_accumulator_count
			)
			port map(
				CLK => clock,
				-- Port A is used for reading
				PORTA_EN => bram_porta_en(i),
				PORTA_WE => c_low, -- Port A is never used for writing
				PORTA_ADDR => bram_porta_addr(i),
				PORTA_DIN => c_bram_din_void,
				PORTA_DOUT => bram_douta_sig(i),
				-- Port B is used for writing
				PORTB_EN => bram_portb_en(i),
				PORTB_WE => bram_portb_we(i),
				PORTB_ADDR => bram_portb_addr(i),
				PORTB_DIN => final_accum_reg, -- All Write Ports can share this
				PORTB_DOUT => bram_doutb_sig(i)
			);

		-- Enable signals for BRAM Ports; before a process is completed each port is enabled by
		-- valid_in with a particular latency. After completion the memories can remain enabled.
		bram_porta_en(i) <= valid_in_shreg(c_valid_in_shreg_depth-1) when proc_compl_shreg(0) = '0' else '1';

		portb_dyn_en(i) <= '1' when	to_integer(lane_sel_shreg_out(log2int(c_ram_count)-1 downto 0)) = i and
			valid_in_shreg(c_valid_in_shreg_depth-4) = '1' else '0';
		bram_portb_en(i) <= portb_dyn_en(i) when proc_compl_shreg(0) = '0' else '1';
		-- Each port gets enabled only if the selected lane (during this point in time) is stored in the port's RAM
		-- and only if a high valid signal has been received at the same time.

		-- Address Select signals for BRAM Ports
		-- While a process is ongoing ports are addressed by different stages of the lane select pipeline
		-- Once the process is complete, `CONTROL_IN` is used to select pairs of accumulation results within the memory
		with proc_compl_shreg(0) select bram_porta_addr(i) <=
			'0' & lane_sel_lat1(g_accum_counter_width-2 downto log2int(c_ram_count))		when '0',
			control_in(g_accum_counter_width-3 downto 0) & '0'		when '1', -- TODO NOT SURE ABOUT THIS -3
			(others=>'0') when others;

		with proc_compl_shreg(0) select bram_portb_addr(i) <=
			'0' & lane_sel_shreg_out(g_accum_counter_width-2 downto log2int(c_ram_count))	when '0',
			control_in(g_accum_counter_width-3 downto 0) & '1'			when '1',
			(others=>'0') when others;

		-- During computations, `lane_sel` pass through all values and depending on its LSB, one RAM is enabled for writing. This means that
		-- RAM0 is written only with LSB=0 and RAM1 only for LSB=1. Since we want data in each RAM to be continuous, the address in each port
		-- should also be continuous; thus the LSB in `lane_sel` isn't present in address signals `bram_porta/b_addr` and `lane_sel` is shifted right.

		-- During outputting, `CONTROLIN` will address quads in AB's memory by increments of 1. Because of the writing scheme used in the computation stage,
		-- RAM0 will have stored all even elements of a quad and RAM1 will have all the odd ones. For this reason, both ports use `CONTROLIN` as a base address
		-- (instead of trimming bits or adding 1). Since a single value of `CONTROLIN` addresses 4 elements and we essentially have 2 address spaces in the two RAMs,
		-- we need to supply two different addresses to each one of the two Ports in each RAM (CONTROLIN and CONTROLIN+1). Thus, after process completion,
		-- each address in each port is `CONTROLIN` with either 0 or 1 apppended to the end.


		-- Port B of BRAM is used for writing only before process completion of outside of resets
		bram_portb_we(i) <= '0' when rst_in = '1' or proc_compl_shreg(1) = '1' else '1';

		-- BRAM Port A Output Register
		process(CLK)
		begin
			if rising_edge(CLK) then
				-- This register should be 0 during the first accumulation cycle (cycle_counter = 0)
				if bram_douta_rst = '1' or rst_in = '1' then
					bram_douta_reg(i) <= (g_bram_word_width-1 downto 0 => '0');
				elsif valid_in = '1' or proc_compl_shreg(0) = '1' then
					bram_douta_reg(i) <= bram_douta_sig(i);
				else
					bram_douta_reg(i) <= bram_douta_reg(i);
				end if;
			end if;
		end process;

		-- BRAM Port B Output Register
		process(CLK)
		begin
			if rising_edge(CLK) then
				if valid_in = '1' then
					bram_doutb_reg(i) <= bram_doutb_sig(i);
				else
					bram_doutb_reg(i) <= bram_doutb_sig(i);
				end if;
			end if;
		end process;

	end generate ram_signals;
	-- End for-generate ends here -- End for-generate ends here -- End for-generate ends here --

	-- Reset BRAM Port A Output Register during the first cycle; Common for all RAMs
	bram_douta_rst <= '1' when cc_shreg_out_1 = (g_cycle_counter_width-1 downto 0 => '0')
		else '0';

	-- Final addition register, its contents are clipped and stored in BRAM
	process(CLK)
	begin
		douta_reg_sel <= to_integer(lane_sel_lat3(log2int(c_ram_count)-1 downto 0));

		if rising_edge(CLK) then
			if valid_in_shreg(c_valid_in_shreg_depth-3) = '1' then
				final_accum_reg <= add_net_out_reg + bram_douta_reg(douta_reg_sel);
				-- Important note: IEEE.NUMERIC_STD_UNSIGNED addition always returns an `std_logic_vector` with
				-- length equal to the widest operand, in this case `bram_douta_reg` (18 bits). This is why
				-- `final_accum_reg` is as wide as BRAM, even though for a 3-stage addition tree,
				-- only 11 bits are utilized.
			else
				final_accum_reg <= final_accum_reg;
			end if;
		end if;
	end process;


	-- Lane Select Latency Register
	process(CLK)
	begin
		if rising_edge(CLK) then
			if valid_in = '1' then
				lane_sel_lat1 <= lane_sel_reg;
			else
				lane_sel_lat1 <= lane_sel_lat1;
			end if;
		end if;
	end process;

	-- Control I/O; Configuration registers are set up and handled here
	process(CLK)
	begin
		if rising_edge(CLK) then
			-- When a reset occurs...
			if rst_in = '1' then
				-- Store control_in
				ctrl_in_1_reg <= control_in;
				-- Reset cycle counter
				cycle_counter <= (others => '0');
				process_complete <= '0';
				-- If `reset_dur_reg` is high this is the second reset cycle
				if reset_dur_reg = '1' then
					lanes_used_reg  <= ctrl_in_1_reg(g_accum_counter_width-1 downto 0);
					expected_cycles <= control_in(g_cycle_counter_width-1 downto 0);
				else -- Set duration register for next cycle; it will be cleared if reset drops
					reset_dur_reg <= '1';
				end if;
				-- Reset end
			else
				reset_dur_reg <= '0';
				if valid_in = '1' then
					-- Check if `cycle_counter` needs updating
					if (lane_sel_reg = lanes_used_reg-"1") and process_complete='0' then
						cycle_counter <= cycle_counter + "1";
					end if;

					-- Check if all cycles completed; Don't wait for `cycle_counter` to update
					if (cycle_counter = expected_cycles-"1") and (lane_sel_reg=lanes_used_reg-"1") then
						process_complete <= '1';

					elsif process_complete = '1' then
						-- If `process_complete` gets set only a reset can clear it
						process_complete <= '1';
					else
						process_complete <= '0';
					end if;
				end if;
			end if;

			-- Pass reset and control to the next AB component
			rst_out   <= rst_in;
			control_out <= control_in;
		end if;
	end process;

	-- Accumulator and lane select logic
	process(CLK)
	begin
		if rising_edge(CLK) then
			if rst_in = '1' then
				-- Reset certain registers
				lane_sel_reg  <= (others=>'0');

			-- Only open gate if a '1' has reached the `valid_in_pipeline`'s end
			elsif valid_in = '1' then
				-- `lane_sel_reg` controls which lane is used; if the process is completed
				-- it is short-wired with `control_in`. If the process is ongoing it is either
				-- increased everytime a `valid_in` signal is received or it is reset once a cycle completes
				if proc_compl_shreg(c_valid_in_shreg_depth-1) = '1' then
					-- `lane_sel_reg` is controlled by `control_in`
					lane_sel_reg <= control_in(g_accum_counter_width-1 downto 0);
				elsif lane_sel_reg = lanes_used_reg-"1" then
					-- a cycle completed, reset register
					lane_sel_reg <= (others => '0');
				else
					-- an addition completed, continue
					lane_sel_reg <= lane_sel_reg + "1";
				end if;
			else
				lane_sel_reg <= lane_sel_reg;
			end if;
		end if;
	end process;


	-- Addition Network Tree - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	-- Pass inputs to addition network
	process(CLK)
	begin
		if rising_edge(CLK) then
			for i in g_elem_count-1 downto 0 loop
				if valid_in = '1' then
					add_net_input_reg(i) <= data_in((i+1)*c_input_elem_width-1 downto i*c_input_elem_width);
				else
					-- Sum 0s if `valid_in` is not set
					add_net_input_reg(i) <= add_net_input_reg(i);
				end if;
			end loop;
		end if;
	end process;

	-- Synthesize the addition tree
	process(CLK)
	begin
		if rising_edge(CLK) then
			-- Level 1
			for i in 0 to 1 loop
				-- Extend the first bit so that no over/underflow can occur
				add_net_depth1_reg(i) <= (add_net_input_reg(i*2)(c_input_elem_width-1) & add_net_input_reg(i*2))
				+ (add_net_input_reg(i*2+1)(c_input_elem_width-1) & add_net_input_reg(i*2+1));
			end loop;
			-- Output
			add_net_out_reg <= (add_net_depth1_reg(0)(c_input_elem_width) & add_net_depth1_reg(0))
			+ (add_net_depth1_reg(1)(c_input_elem_width) & add_net_depth1_reg(1));
		end if;
	end process;
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	-- Pass BRAM Data-out registers to `DATAOUT`.
	dout_clipper_generate: for i in c_ram_count-1 downto 0 generate
		constant index_a	: integer := i;
		constant index_b	: integer := i+c_ram_count;
		-- see note
	begin
		data_out_sig((index_a+1)*c_output_elem_width-1 downto index_a*c_output_elem_width) <= bram_douta_sig(i);
		data_out_sig((index_b+1)*c_output_elem_width-1 downto index_b*c_output_elem_width) <= bram_doutb_sig(i);

		-- for debugging
		dbg_dout(index_a) <= data_out_sig((index_a+1)*c_output_elem_width-1 downto index_a*c_output_elem_width);
		dbg_dout(index_b) <= data_out_sig((index_b+1)*c_output_elem_width-1 downto index_b*c_output_elem_width);
	end generate dout_clipper_generate;
	-- Note: Since each RAM stores either even or odd lanes, the outputs of the RAMs must be interwhined;
	-- In the case of 2 RAMs, RAM0 has even and RAM1 has odd. Then for a given index of an output quad,
	-- RAM0 has lanes 0 and 2 while RAM1 has lanes 1 and 3. Thus, Ports A of both RAMs go next to each other,
	-- then followed by Ports B. This logic (probably) scales for larger `c_ram_count`.

	-- Data out control; If accumulation results are stored in a wider variable than the output of
	-- the accumulation bank component, clipping will be applied here.
	process(CLK)
	begin
		if rising_edge(CLK) then
			if proc_compl_shreg(0) = '1' then -- Check end of pipeline; we want BRAM to be done writing
				data_out <= data_out_sig;
			else
				-- data_out is inactive
				data_out <= (others=>'0');
			end if;
		end if;
	end process;

	-- Wire output signals to output ports
	RSTOUT     <= rst_out;
	DATAOUT    <= data_out;
	COMPLETE   <= proc_compl_shreg(0);
	CONTROLOUT <= control_out;

end behavioral;
