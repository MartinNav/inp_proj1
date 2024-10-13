-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Martin Navrátil <xnavram00 AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

type cpu_state is (prepare, ready, run, done);

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
  signal end_of_code_ptr : std_logic_vector(12 downto 0):=(others => '0');
  signal state : cpu_state:=prepare;
begin

 -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
 --   - nelze z vice procesu ovladat stejny signal,
 --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
 --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
 --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly. 

end behavioral;

entity find_end_of_code is
  port (
    --when this element is being activated DATA_RDWR should be set to 1
    CLK : in std_logic;
    EN : in std_logic;
    RESET : in std_logic;
    DATA_RDATA: in std_logic_vector(7 downto 0);--The actual data
    DATA_ADDR :out std_logic_vector(12 downto 0);--this is the pointer to currently read data
    END_OF_CODE_ADDR : out std_logic_vector(12 downto 0);
    DONE : out std_logic--if it is found it will be 1 otherwise it will be 0
  );
end entity find_end_of_code;
architecture behavioral of find_end_of_code is
  signal CURR_ADDR : std_logic_vector(12 downto 0) := (others => '0');
  
begin
  
  process(CLK)
  begin
    if rising_edge(CLK) and EN='1' then
      END_OF_CODE_ADDR<=(others => '0');
      if DATA_RDATA /= X"40" then
        CURR_ADDR<=std_logic_vector(unsigned(CURR_ADDR)+1);
      else
        DONE<='1';
        END_OF_CODE_ADDR<=CURR_ADDR;
      end if;
      if RESET='1' then
        CURR_ADDR<=(others => '0');
        DONE<='0';
      end if;
      DATA_ADDR<=CURR_ADDR;
      
    end if;
    
  end process ;
  
  
end architecture behavioral;
