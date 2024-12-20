-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Martin Navrátil <xnavram00 AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;


-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
type cpu_state is (prepare_st, ready_st, run_st,reset_st, done_st, fetch_st,decode_st,
dec_ptr_inst, inc_ptr_inst,-- these instructions are used to modify data ptr
dec_ptr_inst_w, inc_ptr_inst_w,-- these instructions are used to modify data ptr
inc_val_inst_p,dec_val_inc_p,-- will be used to prepare the instruction to be executed
inc_val_inst_m,dec_val_inc_m,-- will be in middle of instruction execution
set_to_tmp_p,set_to_tmp_e,set_to_tmp_w,--will set the value of acc to be equal of current cell
get_from_tmp_p,get_from_tmp_e,get_from_tmp_w,-- will take the value from acc and put it into current cell
put_char_p, put_char_e, put_char_w,
get_char_p, get_char_e, get_char_w,
jump_right_p, jump_right_e,jump_right_w,-- it is equivalent to [ instruction
jump_left_p, jump_left_e,jump_left_w,--it is equivalent to ] instruction
inc_val_inst_w,dec_val_inc_w, nop_inst);
  signal end_of_code_ptr : std_logic_vector(12 downto 0):=(others => '0');
  signal data_ptr: std_logic_vector(12 downto 0):=(others => '0');
  signal instruction_ptr : std_logic_vector(12 downto 0):=(others => '0');
  signal setup_state : std_logic:='1';
  signal state : cpu_state:=prepare_st;
  signal acc_reg: std_logic_vector(7 downto 0):=(others => '0');
  signal fetch_time_ctr : std_logic_vector(2 downto 0):=(others=>'0');--will count the time spent fetching instructions
  signal cycle_counter : std_logic_vector(24 downto 0):=(others => '0');--counting number of cycles from start
  signal left_bracked_ptr: std_logic_vector(12 downto 0):=(others => '0');-- this one is pointing torwards the last seen [
  signal right_bracked_ptr: std_logic_vector(12 downto 0):=(others => '0');-- this one is pointing to the last seen ]
begin

 -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
 --   - nelze z vice procesu ovladat stejny signal,
 --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
 --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
 --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly. 
 -- this process will set the end of code ptr
  setup: process(CLK,state,RESET)
  begin
    if rising_edge(CLK) then -- may need to change the setup_state to cpu_state
          cycle_counter<=unsigned(cycle_counter)+1;
      if state=prepare_st and EN='1' then
        
        if setup_state='1' then
          cycle_counter<=(others => '0');
          
      if DATA_RDATA=X"40" and setup_state='1' then
        setup_state<='0';
        else
      setup_state<='1';
        end_of_code_ptr<=unsigned(end_of_code_ptr)+1;
      end if;
        end if;
    else
      if state=reset_st then
        setup_state<='1';
        end_of_code_ptr<=(others => '0');
      end if;
      
      end if;
      if state=done_st then
          cycle_counter<=(others => '0');
      end if;
    end if;
  end process setup;

  state_manager: process(CLK,RESET,state)
  begin
    if rising_edge(CLK) then
      READY<='1';
      DONE<='0';
      if EN='1' then
        
      if setup_state='1' then
        if (DATA_RDATA=X"5B") and (left_bracked_ptr="000000000000") then
          left_bracked_ptr<=unsigned(end_of_code_ptr)-2;
        end if;
        if (DATA_RDATA=X"5D") and (right_bracked_ptr="000000000000") then
          right_bracked_ptr<=unsigned(end_of_code_ptr)-2;
        end if;
        state<=prepare_st;
        READY<='0';
      else
        --when we are in prepare state and there is nothing else to do it should be set to fetch instructions
        if state=prepare_st then
          state<=fetch_st;-- this indicates we are trying to fetch instruction
        end if;
        --if DATA_RDWR='1' then
          
          fetch_time_ctr<=(others => '0');-- allways it will be converted to zero when not in use
        if state=fetch_st then
          if fetch_time_ctr="10" then
            
        case DATA_RDATA is
          when X"3E" =>--that is > instruction
            state<=inc_ptr_inst;
          when X"3C" =>--that is < instruction
            state<=dec_ptr_inst;
          when X"2B" =>--that is + instruction prefatch
            state<=inc_val_inst_p;
          when X"2D" =>--that is - instruction prefatch
            state<=dec_val_inc_p;
          when X"24"=>-- set value of acc ($)
            state<=set_to_tmp_p;
          when X"21"=>
            state<=get_from_tmp_p;
          when X"2E"=>
            state<=put_char_p;
          when X"2C"=>
            state<=get_char_p;
          when X"40" =>
            state<=done_st;
          when X"5B"=>--this is [ so we will jump to the next ]
            left_bracked_ptr<=instruction_ptr;
            state<=jump_right_p;
          when X"5D"=>-- this is ] so we will need to jump to the closest [
            right_bracked_ptr<=instruction_ptr;
            state<=jump_left_p;
          --must implement execution in next stages of this function
          when others =>
            -- TODO: fix and make it wait exacly 3 cycles
              state<=nop_inst;
        end case;
        else
          fetch_time_ctr<=unsigned(fetch_time_ctr)+1;
          end if;
      end if;
      -- this will be executed after end of every instruction
      case state is 
        when inc_ptr_inst=>state<=inc_ptr_inst_w;
        when dec_ptr_inst=>state<=dec_ptr_inst_w;
        when inc_ptr_inst_w=>state<=fetch_st;
        when dec_ptr_inst_w=>state<=fetch_st;
        when inc_val_inst_p=>state<=inc_val_inst_m;
        when inc_val_inst_m=>state<=inc_val_inst_w;
        when inc_val_inst_w=>state<=fetch_st;
        when dec_val_inc_p=>state<=dec_val_inc_m;
        when dec_val_inc_m=>state<=dec_val_inc_w;
        when dec_val_inc_w=>state<=fetch_st;
        when set_to_tmp_p=>state<=set_to_tmp_e;
        when set_to_tmp_e=>state<=set_to_tmp_w;
        when set_to_tmp_w=>state<=fetch_st;
        when get_from_tmp_p=>state<=get_from_tmp_e;
        when get_from_tmp_e=>state<=get_from_tmp_w;
        when get_from_tmp_w=>state<=fetch_st;
        when put_char_p=>
        if OUT_BUSY='0' then
          state<=put_char_e;
          else
          state<=put_char_p;
        end if;
        when put_char_e=>state<=put_char_w;
        when put_char_w=>state<=fetch_st;
        when get_char_p=>
          if IN_VLD='1'then
            state<=get_char_e;
          else
          state<=get_char_p;
          end if;
        when get_char_e=>state<=get_char_w;
        when get_char_w=>state<=fetch_st;
        when done_st=>DONE<='1';
        when nop_inst=>state<=fetch_st;
        when jump_right_p=>state<=jump_right_e;
        when jump_right_e=>state<=jump_right_w;
        when jump_right_w=>state<=fetch_st;
        when jump_left_p=>state<=jump_left_e;
        when jump_left_e=>state<=jump_left_w;
        when jump_left_w=>state<=fetch_st;
        when others =>
          

      end case;
        --end if;
      end if;
      end if;
      if RESET='1' then
        state<=reset_st;
        READY<='0';
        left_bracked_ptr<=(others => '0');
        right_bracked_ptr<=(others => '0');
      -- this is very temporary (WARNING)
        OUT_INV<='0';
      -- end of temporary section
      end if;
      
    end if;
    
  end process state_manager;

  INSTRUCT_EXEC: process(CLK,state)
  begin
    if rising_edge(CLK) then
      case state is
        when prepare_st=> --this will prepare on case when it is being set up
          if end_of_code_ptr/="000000000000" then
          data_ptr<=unsigned(end_of_code_ptr)-1;--this is hacky solution but it should work therefore there is no reason to change it
          else
            data_ptr<=(others => '0');
          end if;
          --data_ptr<=end_of_code_ptr;
        when inc_ptr_inst=>
          data_ptr<=unsigned(data_ptr)+1;
        when dec_ptr_inst=>
          data_ptr<=unsigned(data_ptr)-1;
        --when inc_val_inst_p=>-- will do nothing and wait for mem
          when inc_val_inst_w=>
            DATA_WDATA<=unsigned(DATA_RDATA)+1;
          when dec_val_inc_w=>
            DATA_WDATA<=unsigned(DATA_RDATA)-1;
        when get_from_tmp_p=>
          DATA_WDATA<=acc_reg;
        when get_from_tmp_e=>
          DATA_WDATA<=acc_reg;
        when set_to_tmp_e=>
          acc_reg<=DATA_RDATA;
        when set_to_tmp_w=>
          acc_reg<=DATA_RDATA;
        when get_char_e=>
          DATA_WDATA<=IN_DATA;
        when reset_st=>
          acc_reg<=(others => '0');
          data_ptr<=(others => '0');
          DATA_WDATA<=(others => '0');
        when others =>
      end case;
    end if;
  end process INSTRUCT_EXEC;

  MEMORY_MANAGER: process(CLK,state)
  begin
    if rising_edge(CLK) then
     -- DATA_RDWR<='1';
      OUT_WE<='0';
      case state is
        when prepare_st=>
          DATA_ADDR<=end_of_code_ptr;
          DATA_EN<='1';
        when dec_val_inc_p=>--prepare
          DATA_RDWR<='1';
          DATA_ADDR<=data_ptr;
          DATA_EN<='1';
        when dec_val_inc_m=>--middle of execution
          DATA_RDWR<='0';
          DATA_ADDR<=data_ptr;
          DATA_EN<='1';
          instruction_ptr<=unsigned(instruction_ptr)+1;
        when dec_val_inc_w=>--write
          DATA_RDWR<='0';
          DATA_ADDR<=data_ptr;
          DATA_EN<='1';
         when inc_val_inst_p=>--prepare
          DATA_RDWR<='1';
          DATA_ADDR<=data_ptr;
          DATA_EN<='1';
        when inc_val_inst_m=>--middle of execution
          DATA_RDWR<='0';
          DATA_ADDR<=data_ptr;
          DATA_EN<='1';
          instruction_ptr<=unsigned(instruction_ptr)+1;
        when inc_val_inst_w=>--write
          DATA_RDWR<='0';
          DATA_ADDR<=data_ptr;
          DATA_EN<='1';
        when fetch_st=>
          OUT_WE<='0';
      DATA_RDWR<='1';
          DATA_ADDR<=instruction_ptr;
          DATA_EN<='1';
        when reset_st=>
          OUT_WE<='0';
          IN_REQ<='0';
        OUT_DATA<=(others => '0');
      DATA_RDWR<='1';
          DATA_ADDR<=(others => '0');
          DATA_EN<='0';
          instruction_ptr<=(others => '0');
        when nop_inst=>
          instruction_ptr<=unsigned(instruction_ptr)+1;
        when done_st=>
          instruction_ptr<=instruction_ptr;
          DATA_EN<='0';
        when inc_ptr_inst=>
          DATA_ADDR<=unsigned(instruction_ptr)+1;
          instruction_ptr<=unsigned(instruction_ptr)+1;
          DATA_EN<='1';
        when inc_ptr_inst_w=>
          DATA_ADDR<=instruction_ptr;
          DATA_EN<='1';
        when set_to_tmp_p=>
          DATA_ADDR<=data_ptr;
          DATA_EN<='1';
          DATA_RDWR<='1';
          instruction_ptr<=unsigned(instruction_ptr)+1;--will start incrementin the instruction_ptr in the preparing phase
          -- will take more cycles to guarantie that the values will be correct

        when get_from_tmp_p=>
          DATA_ADDR<=data_ptr;
          DATA_EN<='1';
          instruction_ptr<=unsigned(instruction_ptr )+1;
        when get_from_tmp_e=>
          DATA_RDWR<='0';
        when get_from_tmp_w=>
          DATA_RDWR<='1';--this may still be 0 just to make sure it will be written but I think one cycle is enought
        when get_char_p=>
          IN_REQ<='1';
          DATA_ADDR<=data_ptr;
        when get_char_e=>
          instruction_ptr<=unsigned(instruction_ptr )+1;
          IN_REQ<='0';
        when get_char_w=>
          DATA_RDWR<='0';

        when put_char_p=>
          DATA_ADDR<=data_ptr;
        when put_char_e=>
          OUT_DATA<=DATA_RDATA;
          instruction_ptr<=unsigned(instruction_ptr)+1;
        when put_char_w=>
          OUT_DATA<=DATA_RDATA;
          OUT_WE<='1';
        when jump_left_p=>
          DATA_ADDR<=data_ptr;
        when jump_left_e=>
            instruction_ptr<=unsigned(instruction_ptr)+1;
        when jump_left_w=>
          if DATA_RDATA/="00000000" then
            instruction_ptr<=unsigned(left_bracked_ptr)+1;
          end if;
        when jump_right_p=>
          DATA_ADDR<=data_ptr;
        when jump_right_e=>
          instruction_ptr<=unsigned(instruction_ptr)+1;
        when jump_right_w=>
          if DATA_RDATA="00000000" then
            instruction_ptr<=unsigned(right_bracked_ptr)+1;
          end if;
        when dec_ptr_inst=>
          instruction_ptr<=unsigned(instruction_ptr)+1;
        when dec_ptr_inst_w=>
          DATA_ADDR<=instruction_ptr;



        when others =>
      DATA_RDWR<='1';
          DATA_ADDR<=(others => '0');
          -- if it is comment in instructions we have to go over it
          instruction_ptr<=unsigned(instruction_ptr)+1;

      end case;
    end if;
  end process MEMORY_MANAGER;
end behavioral;

