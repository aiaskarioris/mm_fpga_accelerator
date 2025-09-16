-- Aias Karioris, 2025

-- Core Logic of Matrix Multiplication Component
--
-- Core component for Matrix Multiplication Component.
-- All sub-components are connected and controlled here via a State Machine.
--
-- This component receives commands via the `CONTROL_DATA` and `CONTROL_ADDR` ports when
-- it has asserted its `CONTROL_READY` port and `CONTROL_START` is set high for a single clock.
--
-- While its `DATAIN` and `DATAOUT` ports can be connected directly to AXI Stream ports,
-- the `CONTROL` port cannot since it lacks handshake functionality and expects command arguments
-- to arrive on the same clock cycle.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;

entity core_logic is
	generic (
		g_axi_port_width	: positive := 32;
		g_input_elem_width	: positive := 8;
		g_output_elem_width	: positive := 16;

		g_ctrl_address_width: positive := 4;
		g_ctrl_data_width	: positive := 16
	);
	port (
		DATA_CLK		: IN  std_logic := '0';
		RST				: IN  std_logic := '0';
		-- Parallel Data in (AXI Stream Slave)
		DATAIN_VALID	: IN  std_logic := '0';
		DATAIN_READY	: OUT std_logic := '0';
		DATAIN_LAST		: IN  std_logic := '0';
		DATAIN			: IN  std_logic_vector(g_axi_port_width-1 downto 0)	:= (others=>'0');
		-- Parallel Data out (AXI Stream Master)
		DATAOUT_VALID	: OUT std_logic := '0';
		DATAOUT_READY	: IN  std_logic := '0';
		DATAOUT_LAST	: OUT std_logic := '0';
		DATAOUT			: OUT std_logic_vector(g_axi_port_width-1 downto 0)	:= (others=>'0');

		-- AXI Lite Interface for configuration
		CONTROL_START	: IN  std_logic := '0';
		CONTROL_READY	: OUT std_logic := '0';
		CONTROL_ADDR	: IN  std_logic_vector(g_ctrl_address_width-1 downto 0) := (others=>'0');
		CONTROL_DATA	: IN  std_logic_vector(g_ctrl_data_width-1 downto 0) := (others=>'0');
		CONTROL_RESP	: OUT std_logic_vector(1 downto 0) := (others=>'0')

	);
end core_logic;

architecture behavioral of core_logic is
-- Functions (1)
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

-- Types/Constants
	constant c_elements_in_input	: positive := g_axi_port_width / g_input_elem_width;
	constant c_elements_in_output	: positive := g_axi_port_width / g_output_elem_width; -- simple mode only
	constant c_command_width		: positive := 4; -- up to 15 commands
	constant c_vec_len_reg_width	: positive := 13;

	constant c_max_out_vec_len		: positive := 4096;
	constant c_mb_out_width			: positive := g_input_elem_width * 2 * c_elements_in_input;
	constant c_ab_bram_width		: positive := 18;
	constant c_ab_out_width			: positive := 18*4;
	constant c_bc_control_width		: positive := log2int(c_max_out_vec_len / c_elements_in_input) + 1;

	-- Length in quads
	constant c_lut_length			: positive := 128;

	-- The general purpose register is used by the state machine mainly
	-- to count how many cycles of data are left for a state change.
	-- It should be wide enough to store `in_vec_len`/4 * `out_vec_len` with max values,
	-- i.e. 4096/4 * 4096, for when the maximum dimensions have been configured
	constant c_gp_reg_width			: positive := c_vec_len_reg_width*2-2;
	constant c_gp_reg_zero			: std_logic_vector(c_gp_reg_width-1 downto 0) := (others=>'0');

	-- Number of latency cycles from a change in `gp_reg` until indexed packet reaches `data_out`
	constant c_mmsimple_output_latency	: positive := 2;
	constant c_mmai_output_latency		: positive := 6; --this was 5 but I changed LUT and added one cycle of lat.

	-- Constants for commands
	constant c_cmd_no_command		: std_logic_vector(c_command_width-1 downto 0) := x"0";
	constant c_cmd_reset_all		: std_logic_vector(c_command_width-1 downto 0) := x"1";
	constant c_cmd_reset_vbuffer	: std_logic_vector(c_command_width-1 downto 0) := x"2";

	constant c_cmd_load_vbuffer		: std_logic_vector(c_command_width-1 downto 0) := x"3";
	constant c_cmd_load_all_vbuffers: std_logic_vector(c_command_width-1 downto 0) := x"4";
	constant c_cmd_load_lut0		: std_logic_vector(c_command_width-1 downto 0) := x"5";
	constant c_cmd_load_lut1		: std_logic_vector(c_command_width-1 downto 0) := x"6";
	constant c_cmd_mm_simple		: std_logic_vector(c_command_width-1 downto 0) := x"7";
	constant c_cmd_mm_ai			: std_logic_vector(c_command_width-1 downto 0) := x"8";

	-- Helper constants
	constant c_high	: std_logic := '1';
	constant c_low	: std_logic := '0';
	constant c_lut_len_binary		: std_logic_vector(c_gp_reg_width-1 downto 0) := to_stdlogicvector(c_lut_length, c_gp_reg_width);

	constant c_version	: string := "1613-1007";

	-- States for command execution
	type runtime_state_t is (
		-- GP States
		st_self_reset, st_idle, st_reset_all, st_ab_config,
		-- Load Vector Buffer States
		st_vb_reset, st_ldvb_selbuf, st_ldvb_store, st_ldvb_endstore,
		-- For Both Buffers States
		st_bvb_init, st_bvb_store0, st_bvb_next, st_bvb_store1, st_bvb_endstore,
		-- Load LUT data
		st_ldlut_init, st_ldlut_do,
		-- Matrix Multiplication (Simple Mode) States
		st_mms_init, st_mms_do, st_mms_done, st_mms_get,
		-- Matrix Multiplication (AI Mode) States
		st_mmai_init, st_mmai_mm1, st_mmai_nextmm, st_mmai_mm2,
		st_mmai_prep_bias, st_mmai_bias, st_mmai_done, st_mmai_get,

		-- We want some cycles of preparations before resetting to idle
		st_command_complete,

		st_bad_command
	);


-- Functions (2)
	function command_to_state(cmd: std_logic_vector(c_command_width-1 downto 0)) return runtime_state_t is
	begin
		case cmd is
			when c_cmd_no_command		=> return st_idle;
			-- Reset Commands
			when c_cmd_reset_all		=> return st_reset_all; -- reset_all
			when c_cmd_reset_vbuffer	=> return st_vb_reset;	-- reset_vbuffer
			-- Load Commands
			when c_cmd_load_vbuffer		=> return st_vb_reset;		-- load_vbuffer
			when c_cmd_load_all_vbuffers=> return st_bvb_init;		-- Load both Vector Buffers with one stream
			when c_cmd_load_lut0		=> return st_ldlut_init;	-- load_lut (bank 0)
			when c_cmd_load_lut1		=> return st_ldlut_init;	-- load_lut (bank 1)
			-- Matrix Multiplication
			when c_cmd_mm_simple		=> return st_mms_init;	-- matrix_multiplication_simple
			when c_cmd_mm_ai			=> return st_mmai_init;	-- matrix_multiplication_ai

			when others => return st_bad_command;
		end case;
	end function;

-- Components
	component vector_buffer is
		generic(
			g_port_elem_width, g_port_elem_count,
			g_buffer_count, g_max_buffer_length : positive
		);
		port (
			CLK, RST	: IN  std_logic;
			PORTIN		: IN  std_logic_vector(g_port_elem_width*g_port_elem_count-1 downto 0);
			PORTOUT		: OUT std_logic_vector(g_port_elem_count*g_port_elem_width-1 downto 0);
			ADVANCE		: IN  std_logic;
			STORE		: IN  std_logic;
			CONTROLIN	: IN  std_logic_vector(12 downto 0);
			VALID		: IN  std_logic;
			BYPASSEN	: IN  std_logic;
			VALIDOUT	: OUT std_logic;
			BYPASSOUT	: OUT std_logic_vector(g_port_elem_count*g_port_elem_width-1 downto 0)
		);
	end component;

	component multiplier_block is
		generic(
			g_mult_pairs, g_vec_input_elem_width,
			g_mat_input_elem_width : positive
		);
		port(
			CLK, RST	: IN  std_logic;
			MAT_IN		: IN  std_logic_vector(g_mat_input_elem_width*g_mult_pairs-1 downto 0);
			VEC_IN		: IN  std_logic_vector(g_vec_input_elem_width*g_mult_pairs-1 downto 0);
			MULTOUT 	: OUT std_logic_vector((g_mat_input_elem_width+g_vec_input_elem_width)*g_mult_pairs-1 downto 0);
			VALIDIN		: IN  std_logic;
			VALIDOUT	: OUT std_logic
		);
	end component;

	component accum_bank is
		generic(
			g_elem_count, g_input_port_width, g_output_port_width, g_bram_word_width,
			g_accumulator_count, g_accum_counter_width, g_cycle_counter_width,
			g_control_port_width: positive
		);
		port(
			CLK, RST	: IN  std_logic;
			RSTOUT		: OUT std_logic;
			DATAIN		: IN  std_logic_vector(g_input_port_width-1 downto 0);
			DATAOUT		: OUT std_logic_vector(g_output_port_width-1 downto 0);
			VALIDIN		: IN  std_logic;
			COMPLETE	: OUT std_logic;
			CONTROLIN	: IN  std_logic_vector(g_control_port_width-1 downto 0);
			CONTROLOUT	: OUT std_logic_vector(g_control_port_width-1 downto 0)
		);
	end component;

	component bias_component is
		generic(
			g_axi_port_width	: positive;
			g_load_elem_width	: positive;
			g_data_port_width	: positive;
			g_elem_count		: positive;
			g_max_lane_count	: positive;
			g_control_port_width: positive
		);
		port(
			CLK			: IN  std_logic;
			RST			: IN  std_logic;
			CONTROLIN	: IN  std_logic_vector(g_control_port_width-1 downto 0);
			LOADIN		: IN  std_logic_vector(g_axi_port_width-1 downto 0);
			LOAD_EN		: IN  std_logic;
			DATAIN		: IN  std_logic_vector(g_data_port_width-1 downto 0);
			DATAOUT		: OUT std_logic_vector(g_data_port_width-1 downto 0)
		);
	end component;

	component lut_component is
		generic(
			g_axi_port_width	: positive;
			g_elem_count		: positive;
			g_input_port_width	: positive;
			g_output_port_width	: positive;
			g_bank_length		: positive
		);
		port(
			CLK			: IN  std_logic;
			RST			: IN  std_logic;
			LUT_EN		: IN  std_logic;
			LOAD_START	: IN  std_logic;
			LOADIN_VALID: IN  std_logic;
			LOADIN		: IN  std_logic_vector(g_axi_port_width-1 downto 0);
			DATAIN		: IN  std_logic_vector(g_input_port_width-1 downto 0);
			DATAOUT		: OUT std_logic_vector(g_output_port_width-1 downto 0)
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

-- I/O Signals
	signal clock, ctrl_clock	: std_logic := '0';
	signal reset_in				: std_logic := '0';
	signal din_valid, dout_valid: std_logic := '0';
	signal din_ready, dout_ready: std_logic := '0';
	signal dout_last, din_last	: std_logic := '0';
	signal ctrl_start			: std_logic := '0';
	signal ctrl_ready			: std_logic := '0';

	signal data_in				: std_logic_vector(g_axi_port_width-1  downto 0) 	:= (others=>'0');
	signal data_out				: std_logic_vector(g_axi_port_width-1  downto 0) 	:= (others=>'0');
	signal ctrl_address_in		: std_logic_vector(g_ctrl_address_width-1 downto 0) := (others=>'0');
	signal ctrl_data_in			: std_logic_vector(g_ctrl_data_width-1 downto 0) 	:= (others=>'0');
	signal ctrl_response		: std_logic_vector(1 downto 0) := (others=>'0');

-- Internal Signals
-- Signals to/from components
	-- Async. set to '1' when gp_reg is zero
	signal gp_is_zero			: std_logic := '0';

	-- Vector Buffer
	signal vbuffer_reset		: std_logic := '0';
	signal vbuffer_advance		: std_logic := '0';
	signal vbuffer_store		: std_logic := '0';
	signal vbuffer_valid		: std_logic := '0';
	signal vbuffer_bypass_en	: std_logic := '0';
	signal vbuffer_control		: std_logic_vector(12 downto 0) := (others=>'0');

	-- Vector Buffer
	signal vbuffer_in			: std_logic_vector(g_axi_port_width-1 downto 0)	:= (others=>'0');
	signal vbuffer_out			: std_logic_vector(g_axi_port_width-1 downto 0)	:= (others=>'0'); -- theoretically, vbuffer's output could use different widths for elements

	-- Μultiplication Block
	signal mb_reset, mb_in_valid: std_logic := '0';
	signal mb_in				: std_logic_vector(g_axi_port_width-1 downto 0)	:= (others=>'0');
	signal mb_to_ab				: std_logic_vector(c_mb_out_width-1 downto 0)	:= (others=>'0');
	signal mb_out_valid			: std_logic := '0';

	-- Accumulation Bank
	signal ab_reset				: std_logic	:= '0';
	signal ab_out				: std_logic_vector(c_ab_out_width-1 downto 0)	:= (others=>'0');
	signal ab_control			: std_logic_vector(12 downto 0)	:= (others=>'0');
	signal ab_expected_cycles	: std_logic_vector(12 downto 0)	:= (others=>'0');
	signal ab_complete_out		: std_logic_vector( 0 downto 0)	:= "0";

	-- Bias Component
	signal bias_reset			: std_logic := '0';
	signal bias_load_en			: std_logic := '0';
	signal bias_control_in		: std_logic_vector(c_bc_control_width-1 downto 0) := (others=>'0');
	signal bc_ctrl_sel_reg		: std_logic_vector(c_bc_control_width-1 downto 0) := (others=>'0');
	signal bias_in				: std_logic_vector(c_ab_out_width-1 downto 0) := (others=>'0');
	signal bias_to_lut			: std_logic_vector(c_ab_out_width-1 downto 0) := (others=>'0');

	-- LUT Component
	signal lut_reset			: std_logic := '0';
	signal lut_load_start		: std_logic := '0';
	signal lut_ldin_valid		: std_logic := '0';
	signal lut_out_0			: std_logic_vector(g_axi_port_width-1 downto 0);
	signal lut_out_1			: std_logic_vector(g_axi_port_width-1 downto 0);
	signal lut_out				: std_logic_vector(g_axi_port_width-1 downto 0);

	-- Depending on the operation mode, `DATAOUT` will either have 2 16-bit elements (Simple Mode)
	-- or 4 8-bit elements (AI Mode). We use one signal for each mode and decide which one drives
	-- `data_out` based on `state`.
	signal dout_simple_mode		: std_logic_vector(g_axi_port_width-1 downto 0)	:= (others=>'0');
	signal dout_ai_mode			: std_logic_vector(g_axi_port_width-1 downto 0)	:= (others=>'0');

	-- For proper outputting `gp_reg` must be propagated with some latency
	signal gp_reg_shreg1		: std_logic_vector(c_gp_reg_width-1 downto 0)	:= (others=>'0');
	signal gp_reg_shreg2		: std_logic_vector(c_gp_reg_width-1 downto 0)	:= (others=>'0');
	signal gp_shreg1_en			: std_logic := '0';
	signal gp_shreg2_en			: std_logic := '0';

	-- If `in_vec_len_reg` and `out_vec_len_reg` receive their maximum values (1024, 4096 resp.) in AI Mode,
	-- even though `gp_reg` will become 1024*4096*2-1, which fits in 21 bits, the product itself will require
	-- 22 bits, which will cause an error; We'll use a wider signal for this Multiplication
	signal wide_product_sig		: std_logic_vector(c_gp_reg_width downto 0); -- 1 bit wider than `gp_reg`

	-- When AB outputs a `COMPLETE` signal the state machine goes to the next state and
	-- starts probing for data. However, this components `DATAOUT_VALID` will become high
	-- once AB start outputting the correct data, one cycle after receiving `CONTROL_IN`
	signal ab_valid_out			: std_logic_vector(0 downto 0) := "0";
	signal ab_valid_out_ai		: std_logic_vector(0 downto 0) := "0";

-- Registers
	-- State Machine
	signal state				: runtime_state_t	:= st_self_reset;
	-- Set after receiving a command
	signal start_command		: std_logic := '0';
	-- General Purpose register; states can use this for various purposes
	signal gp_reg				: std_logic_vector(c_gp_reg_width-1 downto 0) := (others=>'0');
	-- Register for counting how many parts of a row have been received
	signal row_part_reg			: std_logic_vector(c_vec_len_reg_width-1 downto 0) := (others=>'0');
	-- Set when registers affecting the accumulator bank are updated
	signal ab_needs_update		: std_logic := '0';
	-- Set once `gp_reg` starts controlling AB's Output in AI Mode
	signal mmai_output_started_reg	: std_logic_vector(0 downto 0) := (others=>'0');

	signal data_in_reg			: std_logic_vector(g_axi_port_width-1 downto 0) := (others=>'0');
	signal din_valid_reg		: std_logic := '0';

	-- Registers for storing command arguments
	signal ctrl_data_in_reg		: std_logic_vector(g_ctrl_data_width-1 downto 0) 	:= (others=>'0');
	signal ctrl_address_in_reg	: std_logic_vector(g_ctrl_address_width-1 downto 0)	:= (others=>'0');
	signal ctrl_start_lat1		: std_logic := '0';

	-- Register for `ctrl_ready` output
	signal ctrl_ready_reg		: std_logic := '0';

-- AXI Lite Registers
	signal command				: std_logic_vector( c_command_width-1 downto 0)		:= (others=>'0');
	signal last_command			: std_logic_vector( c_command_width-1 downto 0)		:= (others=>'0');
	signal in_vec_len_reg		: std_logic_vector(c_vec_len_reg_width-1 downto 0)	:= (others=>'0');
	signal out_vec_len_reg		: std_logic_vector(c_vec_len_reg_width-1 downto 0)	:= (others=>'0');
	signal vbuffer_sel_reg		: std_logic						:= '0';
	signal lut_sel_reg			: std_logic_vector( 1 downto 0)	:= (others=>'0');
	signal operation_mode_reg	: std_logic_vector( 1 downto 0)	:= (others=>'0');

begin
	-- Wire input ports to signals
	clock <= DATA_CLK;
	reset_in <= RST;
	din_valid <= DATAIN_VALID;
	din_last <= DATAIN_LAST;
	ctrl_start <= CONTROL_START;

	data_in <= DATAIN;
	ctrl_address_in <= CONTROL_ADDR;
	ctrl_data_in <= CONTROL_DATA;

	dout_ready <= DATAOUT_READY;

	-- Data In and DIN Valid registers
	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			if reset_in = '1' then
				data_in_reg <= (others=>'0');
				din_valid_reg <= '0';
			else
				data_in_reg <= data_in;
				din_valid_reg <= din_valid and din_ready;
				-- Unless we also sample when our side is ready
				-- to receive data (due to state) components using
				-- `din_valid_reg` will be controlled only by whatever
				-- state the Master is and we don't want that
			end if;
		end if;
	end process;

	-- Connect components
	vbuffer: component vector_buffer
	generic map(
		g_port_elem_width=>g_input_elem_width,
		g_port_elem_count=>c_elements_in_input,
		g_buffer_count=>2,
		g_max_buffer_length=>1024
	)
	port map(
		CLK => clock,
		RST => vbuffer_reset,
		PORTIN => data_in,
		PORTOUT => vbuffer_out,
		ADVANCE => vbuffer_advance,
		STORE => vbuffer_store,
		CONTROLIN => vbuffer_control,
		VALID => vbuffer_valid,

		BYPASSEN => c_low,
		VALIDOUT => open,
		BYPASSOUT => open
	);

	mb: component multiplier_block
	generic map(
		g_mult_pairs=>c_elements_in_input, g_mat_input_elem_width=>g_input_elem_width,
		g_vec_input_elem_width=>g_input_elem_width
	)
	port map(
		CLK => clock,
		RST => mb_reset,
		VEC_IN => vbuffer_out,
		MAT_IN => data_in_reg,
		MULTOUT => mb_to_ab,
		VALIDIN => mb_in_valid,
		VALIDOUT => mb_out_valid
	);

	ab: component accum_bank
	generic map(
		g_elem_count=>c_elements_in_input,
		g_input_port_width=>c_mb_out_width,
		g_output_port_width=>c_ab_out_width,
		g_bram_word_width=>c_ab_bram_width, -- Word length of BRAM
		g_accumulator_count=>4096, g_accum_counter_width=>12,
		g_cycle_counter_width=>10, g_control_port_width=>13
		-- Accum. Count = maximum length of output vector (4096, 12 bits),
		-- Cycle Counter = maxium length of input vector (2976) / elementcount (4) =>  10 bits
	)
	port map(
		CLK => clock,
		RST => ab_reset,
		RSTOUT => open,
		DATAIN => mb_to_ab,
		DATAOUT => ab_out,
		VALIDIN => mb_out_valid,
		COMPLETE => ab_complete_out(0),
		CONTROLIN  => ab_control,
		CONTROLOUT => open
	);

	bias: component bias_component
	generic map(
		g_axi_port_width => g_axi_port_width,
		g_load_elem_width => 8,
		g_data_port_width => c_ab_out_width,
		g_elem_count => c_elements_in_input,
		g_max_lane_count => 4096, -- number of lanes, not quads
		g_control_port_width => c_bc_control_width -- enough to specify number of quads (and address them all)
	)
	port map(
		CLK => clock,
		RST => bias_reset,
		CONTROLIN => bias_control_in,
		LOADIN  => data_in, -- Test VBUFFER's BYPASSOUT too
		LOAD_EN => bias_load_en,
		DATAIN => bias_in,
		DATAOUT => bias_to_lut
	);

	-- A latency stage is added between AB and BIAS to reduce `gp_reg`'s fan-out
	-- and possibly achieve higher clock frequencies
	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			if reset_in = '1' then
				bc_ctrl_sel_reg <= (others=>'0');
				bias_in <= (others=>'0');
			else -- possibly add an Enable signal here
				bc_ctrl_sel_reg <= gp_reg(12 downto 2);
				bias_in <= ab_out;
			end if;
		end if;
	end process;

	lut: component lut_component
	generic map(
		g_axi_port_width => g_axi_port_width,
		g_elem_count => c_elements_in_input,
		g_input_port_width => c_ab_out_width,
		g_output_port_width => g_axi_port_width,
		g_bank_length => c_lut_length
	)
	port map(
		CLK => clock,
		RST => lut_reset,
		LUT_EN => lut_sel_reg(0),
		LOAD_START => lut_load_start,
		LOADIN_VALID => lut_ldin_valid,
		LOADIN => data_in,
		DATAIN => bias_to_lut,
		DATAOUT => lut_out_0
	);

	lut1: component lut_component
	generic map(
		g_axi_port_width => g_axi_port_width,
		g_elem_count => c_elements_in_input,
		g_input_port_width => c_ab_out_width,
		g_output_port_width => g_axi_port_width,
		g_bank_length => c_lut_length
	)
	port map(
		CLK => clock,
		RST => lut_reset,
		LUT_EN => lut_sel_reg(1),
		LOAD_START => lut_load_start,
		LOADIN_VALID => lut_ldin_valid,
		LOADIN => data_in,
		DATAIN => bias_to_lut,
		DATAOUT => lut_out_1
	);

	-- Some time is needed once the AB is ready to output data and before
	-- this data is present at `DATAOUT` but during this time invalid data will
	-- be outputted. For this reason, `ab_complete` which becomes `ab_valid_out`
	-- needs some latency before reaching `DATAOUT_VALID`.
	ab_valid_out_shreg: component reg_pipeline
	generic map(g_width=>1, g_depth=>3) -- NOTE Change `g_depth` if ab/dp_bram become slower
	port map(
		CLK => clock,
		CLK_EN => c_high,
		DIN => ab_complete_out,
		DOUT => ab_valid_out
	);

	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			-- Reset register after command completition
			if reset_in = '1' or state = st_idle then
				mmai_output_started_reg <= "0";
			-- The Bias Vector must have been received and AB should have asserted `complete`
			-- in order to start outputting.
			-- Normally, `complete` will be asserted way before the bias vector has been received but this
			-- isn't true for smaller output vector lengths (<~64)
			elsif (state = st_mmai_done or state = st_mmai_get or state = st_command_complete) then
				mmai_output_started_reg <= ab_complete_out;
			end if;
		end if;
	end process;

	-- In AI Mode, the first valid data packet will reach `DATAOUT` a few cycles after
	-- entering `st_mmai_done` after (receiving the bias vector)
	abai_valid_out_shreg: component reg_pipeline
	generic map(g_width=>1, g_depth=>5)	-- NOTE Change `g_depth` if bias/lut become slower
	port map(
		CLK => clock,
		CLK_EN => c_high,
		DIN => mmai_output_started_reg,
		DOUT => ab_valid_out_ai
	);

	-- This signal is useful for controling std_logic values that need to
	-- change their behaviour on the last cycle of a state
	gp_is_zero <= '1' when gp_reg = c_gp_reg_zero else '0';

	-- Before `st_mmai_mm1`, this multiplication may overflow so it is cast to a wider signal
	-- Before `st_mmai_mm2` the general purpose register will be `in_len * in_len / 4` so this signal must be updated
	-- NUMERIC_STD_UNSIGNED multiplication will silently overflow if we aren't careful;
	-- Padding `in_vec_len` will cause the output to
	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			if reset_in = '1' then
				wide_product_sig <= (others=> '0');

			-- Prepare for `st_mmai_mm2`; During `st_mmai_nextmm` this signal must already be set to its desired value
			elsif (state = st_mmai_mm1 or state = st_mmai_nextmm) then
				wide_product_sig <= (out_vec_len_reg(c_vec_len_reg_width-1 downto 2) * ('0'&out_vec_len_reg)) - "01";

			-- Prepare for `st_mmai_mm1`; This signal must already be set during `st_mmai_init`. If `in/out_vec_len` change
			-- a cycle will be required for `wide_product_sig` to update; However, if `st_mmai_init` is to be entered after a
			-- a size change, a Both-Vector-Buffer command will always be executed in between, giving this signal
			-- enough time to update.
			else
				wide_product_sig <= (in_vec_len_reg(c_vec_len_reg_width-1 downto 2) * ('0'&out_vec_len_reg)) - "01";
			end if;
		end if;
	end process;
	-- with state select wide_product_sig <=
	-- 	(in_vec_len_reg(c_vec_len_reg_width-1 downto 2) * ('0'&out_vec_len_reg)) - "01" when st_mmai_init,
	-- 	(in_vec_len_reg(c_vec_len_reg_width-1 downto 2) * ('0'&in_vec_len_reg)) - "01"  when st_mmai_mm1 | st_mmai_nextmm,
	-- 	(others=> '0') when others;


	-- State Machine and Command Handling
	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			if reset_in = '1' then
				gp_reg <= (others=>'0');
				state <= st_self_reset;
				din_ready <= '0';
				dout_last <= '0';
			else
				case state is
					when st_self_reset =>
						state <= st_idle;
						din_ready <= '0';
						dout_last <= '0';

					when st_idle =>
						din_ready <= '0';
						dout_last <= '0';
						-- Check if a new command has arrived
						if start_command = '1' then -- A high `start_command` always falls on the next cycle
							state <= command_to_state(command);
						else
							state <= st_idle;
						end if;

					-- TODO
					when st_reset_all =>
						state <= st_idle;
						din_ready <= '0';

					-- Vector Buffer Loading (Single) ------
					when st_vb_reset => -- both ld_vbuffer and reset_vbuffer start here
						state <= st_command_complete when command = c_cmd_reset_vbuffer else st_ldvb_selbuf;
						din_ready <= '0';

					when st_ldvb_selbuf =>
						gp_reg <= (c_gp_reg_width-1 downto c_vec_len_reg_width-2=>'0') & in_vec_len_reg(c_vec_len_reg_width-1 downto 2) - "01"; -- = in_vec_len_reg / 4 - 1
						state <= st_ldvb_store;
						din_ready <= '1'; -- will be set on the next cycle

					when st_ldvb_store =>
						gp_reg <= (gp_reg - "1") when din_valid = '1' else gp_reg;
						state <= st_ldvb_endstore when (gp_is_zero = '1' and din_valid = '1') or (din_last = '1')  else st_ldvb_store;
						din_ready <= not gp_is_zero;

					when st_ldvb_endstore =>
						state <= st_idle;
						din_ready <= '0';

					-- Vector Buffer Loading (Both) --------
					when st_bvb_init =>
						state <= st_bvb_store0;
						gp_reg <= (c_gp_reg_width-1 downto c_vec_len_reg_width-2=>'0') & in_vec_len_reg(c_vec_len_reg_width-1 downto 2) - "01";
						din_ready <= '1'; -- will be set on the next cycle

					when st_bvb_store0 =>
						gp_reg <= gp_reg - "01" when din_valid = '1' else gp_reg;
						state <= st_bvb_next when (??gp_is_zero and din_valid = '1') else st_bvb_store0;
						din_ready <= not gp_is_zero;

					when st_bvb_next =>
						-- The second vector buffer is as long as `out_vec_len`!
						gp_reg <= (c_gp_reg_width-1 downto c_vec_len_reg_width-2=>'0') & out_vec_len_reg(c_vec_len_reg_width-1 downto 2) - "01";
						state <= st_bvb_store1;
						din_ready <= '1';

					when st_bvb_store1 =>
						gp_reg <= gp_reg - "01" when din_valid = '1' else gp_reg;
						state <= st_bvb_endstore when (??gp_is_zero and din_valid = '1') else st_bvb_store1;
						din_ready <= not gp_is_zero;

					when st_bvb_endstore =>
						state <= st_idle;
						din_ready <= '0';

					-- This state might be entered after a matrix multiplication command if the accumulator
					-- bank's settings have to be updated. The next state depends on the ongoing command.
					when st_ab_config =>
						state <= st_mms_do when command = c_cmd_mm_simple else st_mmai_mm1;
						din_ready <= '1';

					when st_bad_command =>
						state <= st_idle;
						din_ready <= '0';

				-- LUT Loading -----------------------------
					when st_ldlut_init =>
						-- `gp_reg` will count how many quads we are expecting
						gp_reg <= c_lut_len_binary(c_gp_reg_width-1 downto 2) & "00" - "01";
						state <= st_ldlut_do;
						din_ready <= '1';

					when st_ldlut_do =>
						gp_reg <= gp_reg - "01" when din_valid = '1' else gp_reg;
						state <= st_idle when (??gp_is_zero and din_valid = '1') else st_ldlut_do;
						din_ready <= not gp_is_zero;

				-- Matrix Multiplication (simple mode) -----
					when st_mms_init =>
						-- `gp_reg` will count the number of input packets we are expecting
						gp_reg <= in_vec_len_reg(c_vec_len_reg_width-1 downto 2) * out_vec_len_reg - "01"; -- in_vec_len / 4 * out_vec_len -1
						-- If the accumulator bank needs its settings updated `st_mms_do` will have to be preceeded by `st_ab_config`
						state <= st_ab_config when ab_needs_update = '1' else st_mms_do;
						din_ready <= '0' when ab_needs_update = '1' else '1'; -- Should be set on the next cycle if going to `st_mms_do`

					when st_mms_do =>
						gp_reg <= gp_reg - "01" when din_valid = '1' else gp_reg;
						state <= st_mms_done when (??gp_is_zero and din_valid = '1') else st_mms_do;
						din_ready <= '1';

					when st_mms_done =>
						-- `gp_reg` will count the number of packets we are expecting for the output
						gp_reg <= (c_gp_reg_width-1 downto c_vec_len_reg_width=>'0') & (out_vec_len_reg - "010");
						-- The next state will be reached once the accumulation bank is ready to output results
						state <= st_mms_get when ab_complete_out(0) = '1' else st_mms_done;
						din_ready <= '0'; -- we are not expecting anything in this state

					when st_mms_get =>
						-- `gp_reg` decreases by 2 every cycle; its 2nd LSB toggles which pair of AB output will reach `DATAOUT`
						-- and the bits above, increasing every two cycles, control wich quad AB outputs.
						if gp_reg /= c_gp_reg_zero and dout_ready = '1' then
							gp_reg <= gp_reg - x"02";
						else
							gp_reg <= gp_reg;
						end if;

						state <= st_command_complete when (??gp_is_zero and dout_ready = '1') else st_mms_get;
						din_ready <= '0';
						dout_last <= '0';

				-- Matrix Multiplicaion (AI Mode) ----------
					-- `st_mmai_init` lasts only one cycle and initializes the component for a total AI Operation (2 matrix mult., bias and lut)
					when st_mmai_init =>
						-- Prepare to receive `out_vec_len` * `in_vec_len` numbers
						gp_reg <= wide_product_sig(c_gp_reg_width-1 downto 0);
						state <= st_ab_config when ab_needs_update = '1' else st_mmai_mm1;
						din_ready <= '0' when ab_needs_update = '1' else '1'; -- Should be set on the next cycle if going to `st_mmai_do`
						-- At this point, `buffer_sel_reg` should be set to 0; However, this is done in another process

					when st_mmai_mm1 =>
						gp_reg <= gp_reg - "01" when din_valid = '1' else gp_reg;
						state <= st_mmai_nextmm when (??gp_is_zero and din_valid = '1') else st_mmai_mm1;
						din_ready <= '0' when (??gp_is_zero and din_valid = '1') else '1'; -- We won't be ready the cycle after gp_reg = 0

					when st_mmai_nextmm =>
						-- Prepare to receive `out_vec_len`*`in_vec_len` numbers again
						gp_reg <= wide_product_sig(c_gp_reg_width-1 downto 0);
						state <= st_mmai_mm2;
						din_ready <= '1';
						-- At this point, `buffer_sel_reg` should be set to 0; However, this is done in another process

					when st_mmai_mm2 =>
						gp_reg <= gp_reg - "01" when din_valid = '1' else gp_reg;
						state <= st_mmai_prep_bias when (??gp_is_zero and din_valid = '1') else st_mmai_mm2;
						din_ready <= '0' when (??gp_is_zero and din_valid = '1') else '1'; -- We won't be ready the cycle after gp_reg = 0

					when st_mmai_prep_bias =>
						-- Prepare to receive `out_vec_len` numbers
						gp_reg <= (c_gp_reg_width-1 downto c_vec_len_reg_width-2 => '0') & out_vec_len_reg(c_vec_len_reg_width-1 downto 2) - "01";
						state <= st_mmai_bias;
						din_ready <= '1';

					when st_mmai_bias =>
						gp_reg <= gp_reg - "01" when din_valid = '1' else gp_reg;
						state <= st_mmai_done when (??gp_is_zero and din_valid = '1') else st_mmai_bias;
						din_ready <= not gp_is_zero;  -- We won't be ready the next cycle after gp_reg = 0

					when st_mmai_done =>
						-- Prepare to transmit `out_vec_len` numbers; gp_reg counts elements now, not quads
						gp_reg <= (c_gp_reg_width-1 downto c_vec_len_reg_width=>'0') & out_vec_len_reg - "100";
						state <= st_mmai_get when ab_complete_out(0) = '1' else state;
						din_ready <= '0';

					when st_mmai_get =>
						-- `gp_reg` has all bits above the first two LSBs connected to AB's control;
						-- Since the AI pipeline can output 4 elements in each cycle, the connected bits
						-- should increase by 1 on every cycle, or `gp_reg` should increase by 4 every cycle.
						if gp_reg /= c_gp_reg_zero and dout_ready = '1' then
							gp_reg <= gp_reg - x"04";
						else
							gp_reg <= gp_reg;
						end if;

						state <= st_command_complete when ??gp_is_zero else st_mmai_get;
						din_ready <= '0';

					-- Once `gp_reg` reaches zero the output module will begin to output the last data packet;
					-- However, this packet will need some time to reach `data_out`; The clock cycles needed for that to happen
					-- are stored in `gp_reg` from the previous state.
					when st_command_complete =>
						din_ready <= '0';

						-- Depending on the ongoing command, a different signal controls when this state is over
						if command = c_cmd_mm_simple then
							state <= st_idle when gp_reg_shreg1 = c_gp_reg_zero else st_command_complete;
							dout_last <= '1' when gp_reg_shreg1 - x"02" = c_gp_reg_zero else '0';
							-- Correct; Don't touch
						elsif command = c_cmd_mm_ai then
							state <= st_idle when gp_reg_shreg2 = c_gp_reg_zero else st_command_complete;
							dout_last <= '1' when gp_reg_shreg2 - x"04" = c_gp_reg_zero else '0';
						else
							state <= st_idle;
						end if;

				end case;
			end if;
		end if;
	end process;

	-- During outputting, `gp_reg` is used to address AB's RAM but the outputs
	-- reach `data_out` with some latency. When elements reach the output we
	-- need to know what their address was and we do that by passing `gp_reg` through a pipeline
	simplemode_gp_reg_shreg: component reg_pipeline
		generic map(g_width=>c_gp_reg_width, g_depth=>c_mmsimple_output_latency)
		port map(
			CLK => clock,
			CLK_EN => gp_shreg1_en, -- No need to keep this register active on all states
			DIN  => gp_reg,
			DOUT => gp_reg_shreg1
		);

	aimode_gp_reg_shreg: component reg_pipeline
		generic map(g_width=>c_gp_reg_width, g_depth=>c_mmai_output_latency-c_mmsimple_output_latency)
		port map(
			CLK => clock,
			CLK_EN => gp_shreg2_en,
			DIN => gp_reg_shreg1,
			DOUT => gp_reg_shreg2
		);

	-- Signals for enabling the two pipelines; the second pipeline needs the first one
	-- to be enabled in order for it to work
	with state select gp_shreg1_en <=
		'1' when st_mms_get  | st_command_complete, -- Simple Mode
		'1' when st_mmai_get, --st_command_complete, -- AI Mode
		'0' when others;

	with state select gp_shreg2_en <=
		'1' when st_mmai_get | st_command_complete,
		'0' when others;

	-- Vector Buffer Control ----------------------------------------
	with state select vbuffer_reset <=
		'1' when st_self_reset | st_reset_all | st_vb_reset | st_bvb_init,
		'0' when others;


	-- `vbuffer_advance` should be asserted after all packets of a matrix row have been received;
	-- A register is needed for that
	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			if reset_in = '1' then
				row_part_reg <= (others=>'0');
			-- Increase register
			elsif (state = st_mms_do) or (state = st_mmai_mm1) or (state = st_mmai_mm2) then
				if ??din_valid then
					row_part_reg <= row_part_reg + "01" when row_part_reg /= (out_vec_len_reg - "01")
						else (others=>'0');
				end if;
			-- Keep register cleared
			else
				row_part_reg <= (others=>'0');
			end if;
		end if;
	end process;

	-- `vbuffer_advance` is a synchronous signal within vbuffer and as such has a cycle of latency
	-- in its effect. As such, if `st_ab_config` will be entered it should remain low while if `st_mm*_init`
	-- will be succeeded by a `do` state, `vbuffer_advance` must be set high during init.
	vbuffer_advance <= '1' when (row_part_reg = (out_vec_len_reg - "01") and ??din_valid)
		else '0';

	with state select vbuffer_valid <=
		'1' when st_reset_all | st_vb_reset | st_ldvb_selbuf | st_ldvb_endstore
							  | st_bvb_init | st_bvb_next    | st_bvb_endstore
							  | st_mms_init | st_mmai_init   | st_mmai_nextmm, -- This is done during init to set select a buffer
		din_valid when st_ldvb_store | st_bvb_store0 | st_bvb_store1,
		'0' when others;

	with state select vbuffer_store <=
		'1' when st_ldvb_store | st_bvb_store0 | st_bvb_store1,
		'0' when others;

	with state select vbuffer_control <=
		-- During a vb reset, `in_vec_len` >> 2 is passed (2 = log2int(elements in input))
		(12 downto c_vec_len_reg_width-2 => '0') & in_vec_len_reg(c_vec_len_reg_width-1 downto 2)	when st_reset_all | st_vb_reset | st_bvb_init,
		-- During all other states, the buffer select register is passed; `vbuffer` will update only if its valid_in signal is high too
		(12 downto 1 => '0') & vbuffer_sel_reg				when others;

	-- Multiplication Block Control ---------------------------------
	with state select mb_reset <=
		'1' when st_self_reset | st_mms_init | st_mmai_init,
		'0' when others;

	with state select mb_in_valid <=
		-- MB has 2 cycles latency so it needs to continue functioning after
		-- `st_mms_do` has completed as well as between `st_mmai_mm1/2`
		din_valid_reg when 	st_mms_do      | st_mms_done | st_mmai_mm1 |
							st_mmai_nextmm | st_mmai_mm2 | st_mmai_prep_bias,
		'0' when others;

	-- Accumulator Bank Control -------------------------------------
	-- AB undergoes a short reset every time a multiplication starts (st_mm*_init). If `ab_needs_update` was set
	-- for whatever reason, after the aforementioned states, `st_ab_config` will trigger, thus keeping `ab_reset`
	-- high for two cycles.
	with state select ab_reset <=
		'1' when st_self_reset | st_reset_all | st_ab_config | st_mms_init | st_mmai_init,
		'0' when others;

	-- Depending on the ongoing command, `st_ab_config` should supply a different value to `ab_control` in the MUX below
	-- (in_vec_len/4 in simple mode, (in_vec_len+out_vec_len)/4 that in AI mode)
	with command select ab_expected_cycles <=
		(12 downto c_vec_len_reg_width-2=>'0') & in_vec_len_reg(c_vec_len_reg_width-1 downto 2)		when c_cmd_mm_simple,
		((12 downto c_vec_len_reg_width-2=>'0') & in_vec_len_reg(c_vec_len_reg_width-1 downto 2))
			+ out_vec_len_reg(c_vec_len_reg_width-1 downto 2)										when c_cmd_mm_ai,
		(others=>'0') when others;

	-- The number of active lanes is set on the first reset cycle and expected cycle count is set on the second.
	-- Since this signal doesn't know if the next cycle will also be an ab reset and since `accum_bank`
	-- ignores `CONTROLIN` if a reset doesn't last two cycles, we feed `ab_control` the number of lanes anyway.
	with state select ab_control <= -- 13 bits
		-- Active Lanes (first cycle, valid only if `st_ab_config` is triggered)
		out_vec_len_reg when st_mms_init | st_mmai_init,
		-- Expected Cycles
		ab_expected_cycles when st_ab_config,
		-- `gp_reg` is used to get the results out of AB; the 2 LSBs are not wired to AB
		gp_reg(14 downto 2) when st_mms_get | st_mmai_get,
		(others=>'0') when others;

	-- Bias Component -----------------------------------------------
	with state select bias_reset <=
		'1' when st_self_reset | st_reset_all | st_mmai_init,
		'0' when others;

	with state select bias_load_en <=
		'1' when st_mmai_bias,
		'0' when others;

	with state select bias_control_in <=
		out_vec_len_reg(c_vec_len_reg_width-1 downto 2) when st_self_reset | st_reset_all | st_mmai_init,
		bc_ctrl_sel_reg when st_mmai_get | st_command_complete,
		(others=>'0') when others;

	-- LUT Component ------------------------------------------------
	with state select lut_reset <=
		'1' when st_self_reset | st_reset_all,
		'0' when others;

	-- Start is high only for the same cycle the first data packet arrives
	-- lut_load_start <= '1' when (state = st_ldlut_do and gp_reg = c_lut_len_binary and din_valid = '1') else '0';
	lut_load_start <= '1' when (state = st_ldlut_do and gp_reg = c_lut_len_binary - "01" and din_valid = '1') else '0';

	with state select lut_ldin_valid <=
		din_valid when st_ldlut_init | st_ldlut_do,
		'0' when others;

	-- LUT Output multiplexing
	with lut_sel_reg select lut_out <=
		lut_out_0 when "01",
		lut_out_1 when others;


	-- AXI Controller's registers
	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			if reset_in = '1' then
				ctrl_address_in_reg <= (others=>'0');
				ctrl_data_in_reg <= (others=>'0');
				ctrl_start_lat1 <= '0';
			else
				ctrl_address_in_reg <= ctrl_address_in;
				ctrl_data_in_reg <= ctrl_data_in;
				ctrl_start_lat1 <= ctrl_start;
			end if;
		end if;
	end process;

	-- Command Decoder
	-- (responses are handled here)
	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			if reset_in = '1' then
				command <= (others=>'0');
				last_command <= (others=>'0');
				in_vec_len_reg  <= (others=>'0');
				out_vec_len_reg <= (others=>'0');
				vbuffer_sel_reg <= '0';
				lut_sel_reg <= "01";
				operation_mode_reg <= (others=>'0');
				ab_needs_update <= '1';

			elsif ctrl_start_lat1 = '1' and state = st_idle then
			-- elsif ctrl_start = '1' and state = st_idle then
				case to_integer(ctrl_address_in_reg) is
					when 0 =>
						-- Get new command and store last one; If the last command was a matrix multiplication of a different mode
						-- `ab_needs_update` will have to be set!
						command <= ctrl_data_in_reg(3 downto 0);
						last_command <= command;

						start_command <= '1'; -- start handling the command on the next cycle
						ctrl_response <= "00";

						-- LoadLUT commands cause `lut_sel_reg` to change
						if command = c_cmd_load_lut0 then
							lut_sel_reg <= "01";
						elsif command = c_cmd_load_lut1 then
							lut_sel_reg <= "10";
						end if;

					-- Input Vector Length
					when 1 =>
						in_vec_len_reg <= ctrl_data_in_reg(c_vec_len_reg_width-1 downto 0);
						ab_needs_update <= '1';
						ctrl_response <= "00";
					-- Output Vector Length
					when 2 =>
						out_vec_len_reg <= ctrl_data_in_reg(c_vec_len_reg_width-1 downto 0);
						ab_needs_update <= '1';
						ctrl_response <= "00";
					-- Vector Buffer Select
					when 3 =>
						vbuffer_sel_reg <= ctrl_data_in_reg(0);
						ab_needs_update <= ab_needs_update;
						ctrl_response <= "00";
					-- LUT Function Select
					when 4 =>
						if ctrl_data_in_reg(0) = '0' then
							lut_sel_reg <= "01";
						else
							lut_sel_reg <= "10";
						end if;

						ab_needs_update <= ab_needs_update;
						ctrl_response <= "00";
					-- Op. Mode: TODO UNUSED
					when 5 =>
						operation_mode_reg <= ctrl_data_in_reg(1 downto 0);
						ab_needs_update <= ab_needs_update;
						ctrl_response <= "00";
					-- Invalid Address
					when others =>
						ctrl_response <= "10"; -- Error:(
						ab_needs_update <= ab_needs_update;
				end case;

			-- The `command` should be set to 0 when entering the 'command_complete' state
			-- This has to be done here since this process is responsible for `command`
			elsif state = st_command_complete then
				-- command <= (others=>'0');
				start_command <= '0';

			-- In Both-Vector Buffers Loading Mode, `vbuffer_sel_reg` should update on each on
			elsif state = st_bvb_init then
				vbuffer_sel_reg <= '0';

			elsif state = st_bvb_next then
				vbuffer_sel_reg <= '1';


			-- We want vector buffers to be auto-selected during AI Mode
			elsif state = st_mmai_init then
				vbuffer_sel_reg <= '0';
				-- Also check if `ab_needs_update` will be set; Use previous value in case
				-- register was set for another reason
				ab_needs_update <= '1' when last_command = c_cmd_mm_simple else ab_needs_update;

			-- Changing `vbuffer_sel_reg` on `st_mmai_nextmm` isn't fast enough;
			-- We have to change it on the last cycle of `st_mmai_mm1`
			elsif state = st_mmai_mm1 and ??gp_is_zero and din_valid = '1' then -- TODO Not 99% sure about din_valid
				vbuffer_sel_reg <= '1';

			-- Check `ab_needs_update` for simple mode too
			elsif state = st_mms_init then
				ab_needs_update <= '1' when last_command = c_cmd_mm_ai else ab_needs_update;

			else
				start_command <= '0'; -- clear this flag
			end if;

		end if;
	end process;

	-- -- If we aren't in the idle state we are busy; We are also busy right after
	-- -- receiving a `ctrl_start` (TODO or maybe not?)
	-- with state select ctrl_ready <=
	-- 	not ctrl_start_lat1 and not start_command when st_idle,
	-- 	'0' when others;

	process(DATA_CLK)
	begin
		if rising_edge(DATA_CLK) then
			if reset_in = '1' then
				ctrl_ready_reg <= '0';
			elsif state = st_idle then
				ctrl_ready_reg <= not ctrl_start_lat1 and not start_command;
			else
				ctrl_ready_reg <= '0';
			end if;
		end if;
	end process;

	ctrl_ready <= ctrl_ready_reg;

	-- Stream Output Control ----------------------------------------
	with command select data_out <=
		dout_simple_mode 	when c_cmd_mm_simple,
		dout_ai_mode		when c_cmd_mm_ai,
		(others=>'0') when others;

	-- Simple Mode Output: The LSBs are cut from the output
	output_clippers: for i in c_elements_in_output downto 1 generate
		signal sel : integer;
	begin
		-- Cast MUX selector bits of pipelined `gp_reg` as integer for readability.
		sel <= 2 when gp_reg_shreg1(1) = '1' else 0;
		-- Pass to `dout_simple` only the elements that are currently selected by `gp_reg[1:0]`
		dout_simple_mode(i*g_output_elem_width-1 downto (i-1)*g_output_elem_width) <=
			ab_out((i+sel)*c_ab_bram_width-1 downto (i+sel)*c_ab_bram_width-g_output_elem_width);
	-- Note: Counting from `c_... downto 1` instead of `c_...-1 downto 0` simplifies the expression
	end generate output_clippers;

	-- AI Mode Output
	dout_ai_mode <= lut_out;

	-- Our output will become valid a few cycles after entering a `st_*_get` state;
	-- Once this happens it will never become invalid.
	with state select dout_valid <=
		ab_valid_out(0) when st_mms_get,
		ab_valid_out_ai(0) when st_mmai_get,
		'1' when st_command_complete,
		'0' when others;
	---------------------------------------------------------------

	-- Wire output signals to their ports
	CONTROL_READY <= ctrl_ready;
	CONTROL_RESP <= ctrl_response;
	DATAIN_READY <= din_ready;
	DATAOUT <= data_out;
	DATAOUT_LAST <= dout_last;
	DATAOUT_VALID <= dout_valid;

end behavioral;
