-- luisa
-- CS 232 Spring 2013
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The alu circuit implements the specified operation on srcA and srcB, putting
-- the result in dest and setting the appropriate condition flags.

-- The opcode meanings are shown in the case statement below

-- condition outputs
-- cr(0) <= '1' if the result of the operation is 0
-- cr(1) <= '1' if there is a 2's complement overflow
-- cr(2) <= '1' if the result of the operation is negative
-- cr(3) <= '1' if the operation generated a carry of '1'

-- Note that the and/or/xor operations are defined on std_logic_vectors, so you
-- may have to convert the srcA and srcB signals to std_logic_vectors, execute
-- the operation, and then convert the result back to an unsigned.  You can do
-- this all within a single expression.


entity alu is
  
  port (
    srcA : in  unsigned(15 downto 0);         -- input A
    srcB : in  unsigned(15 downto 0);         -- input B
    op   : in  std_logic_vector(2 downto 0);  -- operation
    cr   : out std_logic_vector(3 downto 0);  -- condition outputs
    dest : out unsigned(15 downto 0));      -- output value

end alu;

architecture test of alu is

  -- The signal tdest is an intermediate signal to hold the result and
  -- catch the carry bit in location 16.
  signal tdest : unsigned(16 downto 0);  
  
  -- Note that you should always put the carry bit into index 16, even if the
  -- carry is shifted out the right side of the number (into position -1) in
  -- the case of a shift or rotate operation.  This makes it easy to set the
  -- condition flag in the case of a carry out.
 begin
  process (srcA, srcB, op) 
  begin  
    case op is
      when "000" => tdest <= (srca(15) & srcA) +  (srcb(15) & srcB) ;     -- addition     dest = srcA + srcB
      when "001" => tdest <=  (srca(15) & srcA) -  (srcb(15)  & srcB);      -- subtraction  dest = srcA - srcB
      when "010" => 
		tdest(16)<='0';
		tdest <=  srca(15)& unsigned(std_logic_vector(srcA) and  std_logic_vector(srcB));      -- and          dest = srcA and srcB
      when "011" =>
		tdest(16)<='0';
		tdest <=  srca(15) & unsigned(std_logic_vector(srcA )or  std_logic_vector(srcB));     -- or           tdest = srcA or srcB
      when "100" => 
		tdest(16)<='0';
		tdest <=  srca(15)& unsigned(std_logic_vector(srcA )xor  std_logic_vector(srcB)); 
      -- xor          dest = srcA xor srcB
      when "101" => 
		if srcb(0)='0' then 
			tdest <= shift_left(srca,1) & '0'; 
		else tdest<= srca(15) & shift_right(srca,1);
		end if;		-- shift        dest = srca shifted left arithmetic by one if srcB(0) is 0, otherwise right
      when "110" =>  
		if srcb(0)='0' then 
			tdest <= rotate_left(srca,1)& '0'; 
		else tdest<=  srca(15)&rotate_right(srca,1);    
		end if;		-- rotate       dest = srca rotated left by one if srcB(0) is 0, otherwise right
      when "111" =>    tdest<= '0' & srca;     -- pass         dest = srcA
      when others => 
        null;
    end case;
  end process;

  -- connect the low 16 bits of tdest to dest here

  -- set the four CR output bits here
  
 

  dest<= tdest(15 downto 0);
  

    cr(0)<= '1' when tdest(15 downto 0) = 0 else '0';
	 cr(1)<= '1' when srca(15) = srcb(15) and tdest(15) /= srca(15) and op = "000" and op="001"else '0';
	 cr(2)<= tdest(15);
	 cr(3)<= tdest(16);
     

	
end test;