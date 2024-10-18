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
type cpu_state is (prepare_st, ready_st, run_st, done_st, decode_st,
dec_ptr_inst, inc_ptr_inst,-- these instructions are used to modify data ptr
inc_val_inst,vec_val_inc);
  signal end_of_code_ptr : std_logic_vector(12 downto 0):=(others => '0');
  signal data_ptr: std_logic_vector(12 downto 0):=(others => '0');
  signal instruction_ptr : std_logic_vector(12 downto 0):=(others => '0');
  signal setup_state : std_logic:='1';
  signal state : cpu_state:=prepare_st;
begin

 -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
 --   - nelze z vice procesu ovladat stejny signal,
 --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
 --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
 --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly. 
 -- this process will set the end of code ptr
  setup: process(CLK)
  begin
    if rising_edge(CLK) and state=prepare_st then -- may need to change the setup_state to cpu_state
      setup_state<='1';
      if DATA_RDATA=X"40" then
        setup_state<='0';
        else
        end_of_code_ptr<=unsigned(end_of_code_ptr)+1;
      end if;
      
    end if;
  end process setup;

  state_manager: process(CLK)
  begin
    if rising_edge(CLK) then
      if setup_state='1' then
        state<=prepare_st;
      else
        if DATA_RDWR='1' then
          
        case DATA_RDATA is
          when X"3E" =>--that is > instruction
            state<=inc_ptr_inst;
          when X"3C" =>--that is < instruction
            state<=dec_ptr_inst;

          when others =>
            

        end case;
        end if;
      end if;
      
    end if;
    
  end process state_manager;

  INSTRUCT_EXEC: process(CLK,state)
  begin
    if rising_edge(CLK) then
      case state is
        when prepare_st=> --this will prepare on case when it is being set up
        data_ptr<=end_of_code_ptr;
        when inc_ptr_inst=>
          data_ptr<=unsigned(data_ptr)+1;
        when dec_ptr_inst=>
          data_ptr<=unsigned(data_ptr)-1;
          

        when others =>
          

      end case;
      
    end if;
    
  end process INSTRUCT_EXEC;





end behavioral;

