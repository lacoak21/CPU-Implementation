-- Quartus II VHDL Template
-- Four-State Moore State Machine

-- A Moore machine's outputs are dependent only on the current state.
-- The output is written only when the state changes.  (State
-- transitions are synchronous.)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cpu is

	   port (
    clk   : in  std_logic;                       -- main clock
    reset : in  std_logic;                       -- reset button

    PCview : out std_logic_vector( 7 downto 0);  -- debugging outputs
    IRview : out std_logic_vector(15 downto 0);
    RAview : out std_logic_vector(15 downto 0);
    RBview : out std_logic_vector(15 downto 0);
    RCview : out std_logic_vector(15 downto 0);
    RDview : out std_logic_vector(15 downto 0);
    REview : out std_logic_vector(15 downto 0);

    iport : in  std_logic_vector(7 downto 0);    -- input port
    oport : out std_logic_vector(15 downto 0)  -- output port
        );

		  
end entity;

architecture rtl of cpu  is 
	
	
		

		Signal RAM_output : std_logic_vector(15 downto 0);
		Signal RAM_we : std_logic;
		Signal ROM_output : std_logic_vector(15 downto 0);
		Signal sp : std_logic_vector(15 downto 0);
		Signal mbr : std_logic_vector(15 downto 0);
		Signal mar :std_logic_vector(7 downto 0);
		Signal IR : std_logic_vector(15 downto 0);
		signal CR:std_logic_vector(3 downto 0);
		signal RA: std_logic_vector(15 downto 0);
		signal RB: std_logic_vector(15 downto 0);
	   signal RC: std_logic_vector(15 downto 0);
		signal RD: std_logic_vector(15 downto 0);
		signal RE:std_logic_vector(15 downto 0);
		signal PC: std_logic_vector(7 downto 0);
		signal outreg: std_logic_vector(15 downto 0);
		signal counter: unsigned(2 downto 0);
		signal ALUIn1 : unsigned(15 downto 0);
		signal ALUIn2 : unsigned(15 downto 0);
		signal ALUout : unsigned(15 downto 0);
		Signal aluop : std_LOGIC_VECTOR(2 downto 0);
		Signal aluc : std_LOGIC_VECTOR(3 downto 0);
--		Signal aluc : std_LOGIC_VECTOR();
		
--
		
	type state_type is (start, fetch,executes, executealu, executew, executewr, executerp1, executerp2,halt);

	-- Register to hold the current state
	signal state   : state_type;
	
	
	
		
		
	COMPONENT memram
		PORT
		(
			address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		data		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		wren		: IN STD_LOGIC ;
		q		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
		END COMPONENT;
		
		
		COMPONENT fib
		PORT
		(
			address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		q		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
		END COMPONENT;
		
		component alu
		PORT
		    (srcA : in  unsigned(15 downto 0);        -- input A
    srcB : in  unsigned(15 downto 0);         -- input B
    op   : in  std_logic_vector(2 downto 0);  -- operation
    cr   : out std_logic_vector(3 downto 0);  -- condition outputs
    dest : out unsigned(15 downto 0)
	 );
		END COMPONENT;

	begin
	
		pcview<= pc;
		IRview<= IR;
		RAview<= RA;
		RBview<= RB;
		RCview<= RC;
		RDview<= RD;
		REview<= RE;
		oport<=outreg;
		
		
		
	
	memram1 : memram

	PORT MAP (
	   address=>mar,
		clock=>clk,
		data=>mbr,
		wren=>RAM_we,
		q =>RAM_output
	);
	
		fib1 : fib

	PORT MAP (
	   address=>pc,
		clock=>clk,
		q =>RoM_output
	);
	
	alu1 : alu
	
	PORT MAP (
	  srcA =>aluIn1,        -- input A
    srcB =>aluIn2,         -- input B
    op   => aluop,  -- operation
    cr   =>aluc,  -- condition outputs
    dest =>aluout
	);
	
	

	-- Logic to advance to the next state
	process (clk, reset) is
	begin
		if reset = '0' then
			pc<="00000000";
			Ir<="0000000000000000";
			outreg<="0000000000000000";
			mar<="00000000";
			mbr<="0000000000000000";
			ra<="0000000000000000";
			rb<="0000000000000000";
			rc<="0000000000000000";
			rd<="0000000000000000";
			re<="0000000000000000";
			sp<="0000000000000000";
			cr<="0000";
			counter<="000";
			state<=start;
		elsif (rising_edge(clk)) then
			case state is
				when start =>
					counter<=counter+1;
					if counter="111" then 
						counter<="000";
						state<=fetch ;
					end if;
				when fetch =>
					IR<=ROM_output;
					pc<=std_LOGIC_VECTOR(unsigned(pc)+1);
					state <= executes;
				when executes =>
					if IR(15 downto 12)= "0000" then 
						if IR(11) = '1' then
							mar<=std_LOGIC_VECTOR(unsigned(IR(7 downto 0))+ unsigned(RE(7 downto 0)));
						else 
							Mar<=IR(7 downto 0);
						end if;
						
		  
					elsif IR(15 downto 12)= "0001" then 
						if IR(11) = '1' then
							mar<=std_LOGIC_VECTOR(unsigned(IR(7 downto 0))+ unsigned(RE(7 downto 0)));
						else 
							Mar<=IR(7 downto 0);
						end if;
						
							if IR(10 downto 8)= "000" then 
							mbr<=RA;
						elsif IR(10 downto 8)= "001" then 
							mbr<=Rb;
						elsif IR(10 downto 8)= "010" then
							mbr<=Rc;
						elsif IR(10 downto 8)= "011" then 
							mbr<=Rd;
						elsif IR(10 downto 8)= "100" then 
							mbr<=Re;
						elsif IR(10 downto 8)= "101" then 
							mbr<=Std_LOGIC_VECTOR(sp);
						end if;
					elsif IR(15 downto 12)= "0010" then
						pc<=IR(7 downto 0);
					elsif IR(15 downto 12)= "0011" then
			
							if IR(11 downto 10)="00" then
								if IR(9 downto 8)= "00" and cr(0)='1' then   
										pc<=IR(7 downto 0);
								elsif IR(9 downto 8)= "01" and cr(1)='1' then 
										pc<=IR(7 downto 0);
							   elsif IR(9 downto 8)= "10" and CR(2)='1' then 
										pc<=IR(7 downto 0);
					
							   elsif IR(9 downto 8)= "11" and CR(3)='1' then
										pc<=IR(7 downto 0);
								end if;
							
							elsif IR(11 downto 10)="01" then
								pc<=IR(7 downto 0);
								mar<=std_LOGIC_VECTOR(sp(7 downto 0));
								mbr<="0000" & CR & PC;
								sp<=std_logic_vector(unsigned(sp)+1);
							elsif IR(11 downto 10)="10" then
								mar<=std_logic_vector(unsigned(sp(7 downto 0))-1);
								Sp<=std_logic_vector(unsigned(sp)-1);
							elsif IR(11 downto 10)="11" then
								state<=halt;
							end if;
			
					elsif IR(15 downto 12)= "0100" then
						mar<=std_LOGIC_VECTOR(sp(7 downto 0));
						sp<=std_logic_vector(unsigned(sp)+1);
						if IR(11 downto 9)= "000" then 
							mbr<=ra;
						elsif IR(11 downto 9)= "001" then 
							mbr<=rb;
						elsif IR(11 downto 9)= "010" then
							mbr<=RC;
						elsif IR(11 downto 9)= "011" then 
							mbr<=RD;
						elsif IR(11 downto 9)= "100" then 
							mbr<=RE;
						elsif IR(11 downto 9)= "101" then 
							mbr<=std_LOGIC_VECTOR(sp);
						elsif IR(11 downto 9)= "110" then 
							mbr<="00000000" & pc(7 downto 0);
						elsif IR(11 downto 9)= "111" then
							mbr<="000000000000"&cr;
						end if;
					elsif IR(15 downto 12)= "0101" then
						mar<=std_LOGIC_VECTOR(unsigned(sp(7 downto 0))-1);
						sp<=std_logic_vector(unsigned(sp)-1);
					elsif IR(15 downto 12)= "0110" then
						null;
					elsif IR(15 downto 12)= "0111" then
						null;
					elsif IR(15 downto 12)= "1000" or IR(15 downto 12)= "1001" or IR(15 downto 12)= "1010" or IR(15 downto 12)= "1011" or IR(15 downto 12)= "1100" then
						ALUop <= IR(14 downto 12);
						
						if IR(11 downto 9)= "000" then 
							aluin1<=unsigned(RA);
						elsif IR(11 downto 9)= "001" then 
							aluin1<=unsigned(Rb);
						elsif IR(11 downto 9)= "010" then
							aluin1<=unsigned(Rc);
						elsif IR(11 downto 9)= "011" then 
							aluin1<=unsigned(Rd);
						elsif IR(11 downto 9)= "100" then 
							aluin1<=unsigned(Re);
						elsif IR(11 downto 9)= "101" then 
							aluin1<=unsigned(sp);
						elsif IR(11 downto 9)= "110" then 
							aluin1<="0000000000000000";
						elsif IR(11 downto 9)= "111" then
							aluin1<="1111111111111111";
						end if;
						
							if IR(8 downto 6)= "000" then 
							aluIn2<=unsigned(RA);
						elsif IR(8 downto 6)= "001" then 
							aluIn2<=unsigned(Rb);
						elsif IR(8 downto 6)= "010" then
							aluIn2<=unsigned(Rc);
						elsif IR(8 downto 6)= "011" then 
							aluIn2<=unsigned(Rd);
						elsif IR(8 downto 6)= "100" then 
							aluIn2<=unsigned(Re);
						elsif IR(8 downto 6)= "101" then 
							aluIn2<=unsigned(sp);
						elsif IR(8 downto 6)= "110" then 
							aluIn2<="0000000000000000";
						elsif IR(8 downto 6)= "111" then
							aluIn2<="1111111111111111";
						end if;
						
					elsif IR(15 downto 12)= "1101" or IR(15 downto 12)= "1110" then
						ALUop <= IR(14 downto 12);
						
						if IR(10 downto 8)= "000" then 
							aluIn1<=unsigned(RA);
						elsif IR(10 downto 8)= "001" then 
							aluIn1<=unsigned(Rb);
						elsif IR(10 downto 8)= "010" then
							aluIn1<=unsigned(Rc);
						elsif IR(10 downto 8)= "011" then 
							aluIn1<=unsigned(Rd);
						elsif IR(10 downto 8)= "100" then 
							aluIn1<=unsigned(Re);
						elsif IR(10 downto 8)= "101" then 
							aluIn1<=unsigned(sp);
						elsif IR(10 downto 8)= "110" then 
							aluIn1<="0000000000000000";
						elsif IR(10 downto 8)= "111" then
							aluIn1<="1111111111111111";
						end if;
						aluIn2(0)<=IR(11);
						
				elsif IR(15 downto 12)= "1111" then	
				ALUop <= IR(14 downto 12);
					if IR(11)= '1' then
						aluIn1<= IR(10)&IR(10)&IR(10)&IR(10)&Ir(10)& ir(10)&IR(10)&IR(10)&unsigned(IR(10 downto 3));
					else
						
						if IR(10 downto 8)= "000" then 
							aluIn1<=unsigned(RA);
						elsif IR(10 downto 8)= "001" then 
							aluIn1<=unsigned(Rb);
						elsif IR(10 downto 8)= "010" then
							aluIn1<=unsigned(Rc);
						elsif IR(10 downto 8)= "011" then 
							aluIn1<=unsigned(Rd);
						elsif IR(10 downto 8)= "100" then 
							aluIn1<=unsigned(Re);
						elsif IR(10 downto 8)= "101" then 
							aluIn1<=unsigned(sp);
						elsif IR(10 downto 8)= "110" then 
							aluIn1<="00000000"&unsigned(pc);
						elsif IR(10 downto 8)= "111" then
							aluIn1<=unsigned(ir);
						end if;
					
					end if;
				
			 end if;
		
						
					state <= executealu;
				when executealu =>
					if (IR(15 downto 12)= "0001") or (IR(15 downto 12)= "0100") or (IR(15 downto 12)="0011")then
						RAM_WE<='1';
		
					
					end if;
					state <= executew;
				when executew =>
					state <= executewr;
				when executewr =>
					RAM_WE<='0';
					if IR(15 downto 12)= "0000" then
						if IR(10 downto 8)= "000" then 
							RA<=RAm_output;
						elsif IR(10 downto 8)= "001" then 
							RB<=RAm_output;
						elsif IR(10 downto 8)= "010" then
							RC<=RAm_output;
						elsif IR(10 downto 8)= "011" then 
							RD<=RAm_output;
						elsif IR(10 downto 8)= "100" then 
							RE<=RAm_output;
						elsif IR(10 downto 8)= "101" then 
							SP<=(RAm_output);

						end if;
		  
					elsif IR(15 downto 12)= "0001" then
						null;
					elsif IR(15 downto 12)= "0010" then
						null;
					elsif IR(15 downto 12)= "0011" then
					
							if IR(11 downto 10)="10" then
								PC <=  RAM_output(7 downto 0);
								CR <= ram_output(11 downto 8);
								
								state<=executerp1;
								
				
								
							else 
							null;
							
							end if;
					
					elsif IR(15 downto 12)= "0100" then
						null;
					elsif IR(15 downto 12)= "0101" then
							if IR(11 downto 9)= "000" then 
							RA<=RAm_output;
						elsif IR(11 downto 9)= "001" then 
							RB<=RAm_output;
						elsif IR(11 downto 9)= "010" then
							RC<=RAm_output;
						elsif IR(11 downto 9)= "011" then 
							RD<=RAm_output;
						elsif IR(11 downto 9)= "100" then 
							RE<=RAm_output;
						elsif IR(11 downto 9)= "101" then 
							SP<=(RAm_output);
						elsif IR(11 downto 9)= "110" then 
							PC<=RAm_output(7 downto 0);
						elsif IR(11 downto 9)= "111" then
							CR<=RAm_output(3 downto 0);
						end if;
						
					elsif IR(15 downto 12)= "0110" then
							if IR(11 downto 9)= "000" then 
							outreg<=RA;
						elsif IR(11 downto 9)= "001" then 
							outreg<=RB;
						elsif IR(11 downto 9)= "010" then
							outreg<=RC;
						elsif IR(11 downto 9)= "011" then 
							outreg<=Rd;
						elsif IR(11 downto 9)= "100" then 
							outreg<=re;
						elsif IR(11 downto 9)= "101" then 
							outreg<=std_LOGIC_VECTOR(sp);
						elsif IR(11 downto 9)= "110" then 
							outreg<="00000000"&std_LOGIC_VECTOR(PC);
						elsif IR(11 downto 9)= "111" then
						outreg<=std_LOGIC_VECTOR(IR);
						end if;
					elsif IR(15 downto 12)= "0111" then
						if IR(11 downto 9)= "000" then 
							RA<="00000000"&iport;
						elsif IR(11 downto 9)= "001" then 
							RB<="00000000"&iport;
						elsif IR(11 downto 9)= "010" then
							RC<="00000000"&iport;
						elsif IR(11 downto 9)= "011" then 
							RD<="00000000"&iport;
						elsif IR(11 downto 9)= "100" then 
							RE<="00000000"&iport;
						elsif IR(11 downto 9)= "101" then 
							SP<="00000000"&iport;
						end if;

					elsif IR(15 downto 12)= "1000" or IR(15 downto 12)= "1001" or IR(15 downto 12)= "1010" or IR(15 downto 12)= "1011" or IR(15 downto 12)= "1100" or IR(15 downto 12)= "1101" or IR(15 downto 12)= "1000" then
						
							if IR(2 downto 0)= "000" then 
							RA<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "001" then 
							RB<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "010" then
							RC<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "011" then 
							RD<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "100" then 
							RE<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "101" then 
							SP<=std_LOGIC_VECTOR(aluout);
						end if;
						
						CR<=aluc;
						
			
			
						
				elsif IR(15 downto 12)= "1111" then	
				
				CR<=aluc;
					
							if IR(2 downto 0)= "000" then 
							RA<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "001" then 
							RB<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "010" then
							RC<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "011" then 
							RD<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "100" then 
							RE<=std_LOGIC_VECTOR(aluout);
						elsif IR(2 downto 0)= "101" then 
							SP<=std_LOGIC_VECTOR(aluout);
						end if;
					end if;
					state<=fetch;
				when executerp1 =>
					state <= executerp2;
				when executerp2 =>
					state<= fetch;
			   when halt =>
				  state<=halt;
			end case;
		end if;
	end process;



end rtl;