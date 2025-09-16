-- Aias Karioris, 2025

-- Bias Component
--
-- Component for storing and applying bias. It is meant to augment the
-- functionality of an Accumulation Bank.
--
-- The Bias Component has two parallel inputs; `LOAD IN` and `DATA IN`. The load
-- input is used to store the initial bias of each column; It is connected to the input
-- AXI Stream of the accelerator and has a limited throughput due to AXI Stream.
-- The data input comes from the Accumulation Bank
--
-- During a reset, `CONTROLIN` specifies the number of lanes used. While `LOAD_EN` is low
-- the component receives data in `DATAIN` and adds them to the contents of its RAM, before
-- passing the results to `DATAOUT`.
--
-- `CONTROLIN` always refers to the number of `g_elem_count` elements. This means that if
-- `g_elem_count` is 4, `CONTROLIN` during a reset specifies the number of quads that should be used.
-- Likewise, during computations, `CONTROLIN` selects a quad of lanes. For this reason, `g_control_port_width`
-- doesn't need to be wide enough to address all values from 0 to `g_max_lane_count` (e.g. 13 bits for 0 to 4096)

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL; -- std_logic_vector addition

entity bias_component is
    generic (
        g_axi_port_width        : positive  := 32;
        g_load_elem_width       : positive  :=  8;
        -- Width of elements in `DATAIN`
        g_data_port_width       : positive  := 18*4;
        g_elem_count            : positive  := 4;
        -- Amount of right bitshifting applied to bias before addition with `DATAIN` elements
        -- (enough so that the first bits of int. part of both operands match)
        g_pad_bit_count         : positive  := 7;
        -- Maximum number of lanes (max output vector length)
        g_max_lane_count        : positive  := 4096; -- number of lanes, not quads
        g_control_port_width    : positive  := 10 -- = log2int(4096/4), not log2int(4096), see Description
    );
    port (
        CLK, RST	:	IN  std_logic   := '0';
        CONTROLIN   :   IN  std_logic_vector(g_control_port_width-1 downto 0)   := (others=>'0');

        LOADIN      :   IN  std_logic_vector(g_axi_port_width-1 downto 0)       := (others=>'0');
        LOAD_EN     :   IN  std_logic   := '0';

        DATAIN		:	IN  std_logic_vector(g_data_port_width-1 downto 0);
        DATAOUT		:	OUT std_logic_vector(g_data_port_width-1 downto 0)
    );
end bias_component;

architecture behavioral of bias_component is
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

-- Components
    component clipper_async is
        generic( g_input_width, g_output_width : positive );
        port(
            DIN : IN  std_logic_vector(g_input_width-1  downto 0);
            DOUT: OUT std_logic_vector(g_output_width-1 downto 0);
            CLIP: OUT std_logic
        );
    end component;

-- Types/Constants
    constant c_data_elem_width  : positive := g_data_port_width / g_elem_count;
    constant c_elements_in_ldin : positive := g_axi_port_width / g_load_elem_width;
    constant c_lane_reg_width   : positive := log2int(g_max_lane_count / c_elements_in_ldin); -- counts quad
    constant c_elem_in_mem_width: positive := 9;
    constant c_mem_word_width   : positive := c_elem_in_mem_width * c_elements_in_ldin; -- = 36 bits
    constant c_mem_word_count   : positive := g_max_lane_count / c_elements_in_ldin;
    constant c_mem_addr_width   : positive := log2int(c_mem_word_count);

-- Registers
    signal lane_sel_reg         : std_logic_vector(c_lane_reg_width-1 downto 0) := (others=>'0');
    signal lane_count_reg       : std_logic_vector(c_lane_reg_width downto 0) := (others=>'0');
    -- Output of RAM
    signal ram_out_reg          : std_logic_vector(c_mem_word_width-1 downto 0) := (others=>'0');
    -- Output result buffer
    signal dataout_reg          : std_logic_vector(g_data_port_width-1 downto 0) := (others=>'0');
    -- Set after completing bias loading
    signal load_complete        : std_logic := '0';

    -- (B)RAM
    type bias_ram_t is array (0 to c_mem_word_count-1) of std_logic_vector(c_mem_word_width-1 downto 0);
    signal ram : bias_ram_t := (others=>(others=>'0'));

-- Internal signals
    signal input_expander_out   : std_logic_vector(c_mem_word_width-1 downto 0)     := (others=>'0');
    signal load_signals         : std_logic_vector(1 downto 0) := (others=>'0');

-- I/O Signals
    signal reset                : std_logic := '0';
    signal load_enable          : std_logic := '0';
    signal control_in           : std_logic_vector(g_control_port_width-1 downto 0) := (others=>'0');
    signal load_in              : std_logic_vector(g_axi_port_width-1 downto 0)     := (others=>'0');
    signal data_in              : std_logic_vector(g_data_port_width-1 downto 0);

    type dbg_t is array (g_elem_count-1 downto 0) of std_logic_vector(c_data_elem_width-1 downto 0);
    signal dbg_din  : dbg_t     := (others=>(others=>'0'));
    signal dbg_bias : dbg_t     := (others=>(others=>'0'));
    signal dbg_dout : dbg_t     := (others=>(others=>'0'));

begin
    reset <= RST;
    control_in <= CONTROLIN;
    load_in <= LOADIN;
    load_enable <= LOAD_EN;
    data_in <= DATAIN;

    -- Synthesize expanders for load inputs (8 -> 9) so that they match the RAM's size
    din_expanders: for i in g_elem_count-1 downto 0 generate
        e: clipper_async
            generic map(g_input_width=>g_load_elem_width, g_output_width=>9)
            port map(
                DIN  => load_in((i+1)*g_load_elem_width-1 downto i*g_load_elem_width),
                DOUT => input_expander_out((i+1)*c_elem_in_mem_width-1 downto i*c_elem_in_mem_width),
                CLIP => open
            );
    end generate din_expanders;

    -- Addition
    adders: for i in g_elem_count-1 downto 0 generate
        process(CLK)
        begin
            if rising_edge(CLK) then
                -- Load the accumulation of each input packet into a `dataout_reg` register
                dataout_reg((i+1)*c_data_elem_width-1 downto i*c_data_elem_width) <=
                    data_in((i+1)*c_data_elem_width-1 downto i*c_data_elem_width) +
                    -- Elements in the data in port are indexed by `c_data_elem_width`
                    (ram_out_reg((i+1)*c_elem_in_mem_width-1 downto i*c_elem_in_mem_width) & -- don't forget to add pad bits
                     (g_pad_bit_count-1 downto 0 => '0'));
                    -- Elements in RAM are indexed by `c_elem_in_mem_width`
            end if;
        end process;
    end generate adders;

    process(CLK)
    begin
        if rising_edge(CLK) then
            if reset = '1' then
                -- Get lane count from CONTROLIN
                lane_count_reg <= control_in;
                -- Reset Register
                lane_sel_reg <= (others=>'0');
                ram_out_reg  <= (others=>'0');
                -- Get ready to receive the bias
                load_complete <= '0';

            -- Once `LOAD_IN` gets high loading to memory begins; This assumes a reset has already occurred
            elsif load_enable = '1' and load_complete = '0' then
                ram(to_integer(lane_sel_reg(c_mem_addr_width-1 downto 0))) <= input_expander_out;
                -- Update the lane select register; Clear it back to 0 when the payload has been received
                lane_sel_reg <= lane_sel_reg + "1" when (lane_sel_reg /= lane_count_reg-"1") else (others=>'0');
                load_complete <= '0' when lane_sel_reg /= lane_count_reg-"1" else '1';

            -- Normal operation; note that `LOAD_EN` serves as the valid flag as well
            -- so the component needs to remember if loading has ended
            elsif load_complete = '1' then
                -- In this state `CONTROLIN` is the address of the RAM
                    ram_out_reg <= ram(to_integer(control_in(c_mem_addr_width-1 downto 0)));
                    load_complete <= '1';
            end if;
        end if;
    end process;

    -- Debug Signals
    dbg: for i in g_elem_count-1 downto 0 generate
        dbg_din(i)  <= data_in((i+1)*c_data_elem_width-1 downto i*c_data_elem_width);
        dbg_bias(i) <= (1 downto 0 => ram_out_reg((i+1)*c_elem_in_mem_width-1)) & (ram_out_reg((i+1)*c_elem_in_mem_width-1 downto i*c_elem_in_mem_width) & (g_pad_bit_count-1 downto 0 => '0'));
        dbg_dout(i) <= dataout_reg((i+1)*c_data_elem_width-1 downto i*c_data_elem_width);
    end generate dbg;

    -- Data Out Register
    DATAOUT <= dataout_reg;
end behavioral;

