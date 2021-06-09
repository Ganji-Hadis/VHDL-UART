# VHDL-UART
Implement UART communication

#Top Madule

	library IEEE;
 	use IEEE.STD_LOGIC_1164.ALL;
	-----------------------------------------------------------------
	entity Main is
		port( dataInTransmiter: in std_logic_vector(6 downto 0);
				clk : in std_logic;
				dataOutReciver: out std_logic_vector(6 downto 0));
	end Main;
	-----------------------------------------------------------------
	architecture Behavioral of Main is

		component Transmitter is
			port( clk : in std_logic;
					rst: in std_logic;
					readyToSend : in std_logic;
					dataIn : in std_logic_vector (6 downto 0);
					busy : out std_logic;
					frame: out std_logic_vector(9 downto 0));
		end component;
	
		component Receiver is
			port( clk :in std_logic;
					rst:in std_logic;
					readyToRecive : in std_logic;
					dataIn:in std_logic_vector(9 downto 0);
					dataOut : out std_logic_vector (6 downto 0);
					valid : out std_logic);
		end component;
	
		signal valid : STD_LOGIC:= '0'; 
		signal busy : STD_LOGIC:= '0'; 
		signal frame : std_logic_vector(9 downto 0);
	begin
	tx: Transmitter port map(clk, '0', '1', dataInTransmiter, busy, frame);
	rx: Receiver port map(clk, '0', '1', frame, dataOutReciver , valid);

	end Behavioral;

*********************************************************************************************
#Sub Madule: Transmitter

	library IEEE;
	use IEEE.STD_LOGIC_1164.ALL;
	use IEEE.STD_LOGIC_UNSIGNED.ALL;
	-------------------------------------------------------------------------------
	entity Transmitter is
		port( clk : in std_logic;
				rst: in std_logic;
				readyToSend : in std_logic;                 -- handshake sig
				dataIn : in std_logic_vector (6 downto 0);  -- data for send
				busy : out std_logic;                      
				frame: out std_logic_vector(9 downto 0));   -- sent frame
	end Transmitter;
	--------------------------------------------------------------------------------
	architecture Behavioral of Transmitter is

		type StateType is (idle, frame0, frame1, frame2, frame3, frame4, frame5,
						frame6, frame7, frame8, frame9, stop);
		--signal pr_state , nx_state: state ;
		signal state: StateType := idle;
		signal tx_regdata : std_logic_vector(6 downto 0);   -- register for svaing data
		signal tx_reg : std_logic_vector(9 downto 0);       -- register for make frame
		signal prity : std_logic;
	begin

		process(rst, clk)
		begin
			if (rst= '1') then                              --FSM is off
				tx_regdata <= "0000000";
				tx_reg <= "0000000000";
				state <= idle;
				busy <= '0';
			
			elsif rising_edge (clk) then                   -- FSM is on. wait for clk
			--	pr_state <= nx_state;                       -- go to next state
		
			--end if;
		--end process;
	
		--process(pr_state)
		--begin
		
				prity <= tx_regdata(0) xor tx_regdata(1) xor tx_regdata(2) xor tx_regdata(3)    -- make parity bit
						xor tx_regdata(4) xor tx_regdata(5) xor tx_regdata(6) ;
		
				case state is                     -- FSM begin
			
				when idle =>                      -- idle state
					busy <= '0';
					if(readyToSend = '1') then
						tx_regdata <= dataIn;
						state <= frame0;
					else
						state <= idle;
					end if;
			
				when  frame0 =>                  -- put stop bit in frame
					busy <= '1';
					tx_reg(0) <= '0';
					state <=  frame1;

				when  frame1 =>                  -- put parity in frame
					busy <= '1';
					tx_reg(1) <= prity ;
					state <=  frame2;
					
				when  frame2 =>                  -- put data(0) in frame
					busy <= '1';
					tx_reg(2) <= tx_regdata(0);
					state <=  frame3;
				
				when  frame3 =>                 -- put data(1) in frame
					busy <= '1';
					tx_reg(3) <= tx_regdata(1);
					state <=  frame4;
				
				when  frame4 =>                 -- put data(2) in frame
					busy <= '1';
					tx_reg(4) <= tx_regdata(2);
					state <=  frame5;
					
				when  frame5 =>                 -- put data(3) in frame
					busy <= '1';
					tx_reg(5) <= tx_regdata(3);
					state <=  frame6;
					
				when  frame6 =>                 -- put data(4) in frame
					busy <= '1';
					tx_reg(6) <= tx_regdata(4);
					state <=  frame7;
					
				when  frame7 =>                 -- put data(5) in frame
					busy <= '1';
					tx_reg(7) <= tx_regdata(5);
					state <=  frame8;
					
				when  frame8 =>                 -- put data(6) in frame. put all of data bit
					busy <= '1';
					tx_reg(8) <= tx_regdata(6);
					state <=  frame9;
			
				when  frame9 =>                 -- put start bit in frame.
					busy <= '1';
					tx_reg(9) <= '1' ;
					state <=  stop;
					
				when stop =>                    -- send frame. back to idle
					busy <= '0';
					state <= idle;
					frame <= tx_reg;
			end case;
		end if;
	
		end process;

	end Behavioral;

****************************************************************************************************
#Sub Madule: Receiver

	library IEEE;
	use IEEE.STD_LOGIC_1164.ALL;
	-----------------------------------------------------------
	entity Receiver is
		port( clk : in std_logic;
				rst : in std_logic;
				readyToRecive : in std_logic;                  -- handshake sig
				dataIn : in std_logic_vector(9 downto 0);      -- input frame
				dataOut : out std_logic_vector (6 downto 0);   -- show receive data
				valid : out std_logic);                        -- check error
			
	end Receiver;
	-----------------------------------------------------------
	architecture Behavioral of Receiver is

		type StateType is (idle, frame0, frame1, frame2, frame3, frame4, frame5,
						frame6, frame7, frame8, frame9, stop);
		--signal pr_state , nx_state: state ;
		signal state: StateType := idle;
		signal prity : std_logic;
		signal framerx : std_logic_vector(9 downto 0);   -- reg for receive input frame
		--signal busy : std_logic;
		--signal rx_state : state;
	begin

		process(rst, clk)
		begin
	
			if (rst='1') then              -- FSM is off
				dataOut <= "0000000";
				framerx <= "0000000000";  
				state <= idle;
				valid <= '0';
			
			elsif rising_edge(clk) then        -- FSM is on. wait for clk
			--	pr_state <= nx_state;           -- go to next state
		
			--end if;
		--end process;

		--process(pr_state )
		--begin
	
				prity <= dataIn(2) xor dataIn(3) xor dataIn(4) xor dataIn(5)      -- make parity bit
						xor dataIn(6) xor dataIn(7) xor dataIn(8);
	
				if(prity = dataIn(1)) then             -- check error
					valid <= '1';                       -- no error --> data is valid.
			
		 			case state is        				-- FSM begin
					when idle =>                         -- idle state
						if (readyToRecive = '1') then  
							state <= frame0;
						else
							state <= idle;
						end if;
					
					when  frame0 =>                     -- receive frame(0) --> stop bit
						framerx(0) <= dataIn(0);
						state <=  frame1;
					
					when  frame1 =>                     -- receive frame(1) --> parity bit
						framerx(1) <= dataIn(1);
						state <=  frame2;
				
					when  frame2 =>                     -- receive frame(2) --> data(0)
						framerx(2) <= dataIn(2);
						state <=  frame3;
				
					when  frame3 =>                     -- receive frame(3) --> data(1)
						framerx(3) <= dataIn(3);
						state <=  frame4;
				
					when  frame4 =>							-- receive frame(4) --> data(2)
						framerx(4) <= dataIn(4);         
						state <=  frame5;
					
					when  frame5 =>						   -- receive frame(5) --> data(3)
						framerx(5) <= dataIn(5);        
						state <=  frame6;
				
					when  frame6 =>							-- receive frame(6) --> data(4)
						framerx(6) <= dataIn(6);
						state <=  frame7;
			
					when  frame7 =>						   -- receive frame(7) --> data(5)
						framerx(7) <= dataIn(7);
						state <=  frame8;
				
					when  frame8 =>                     -- receive frame(8) --> data(6)
						framerx(8) <= dataIn(8); 
						state <=  frame9;
				
					when  frame9 =>                     -- receive frame(9) --> start bit
						framerx(9) <= dataIn(9);
						state <=  stop;
				
					when stop =>                        -- show data part of receive frame. back to idle
						dataOut <= framerx(8 downto 2);
						state<= idle;

					end case;
		
				else
					valid <= '0' ;                        -- error detect --> data isn't valid.
					dataOut <= "0000000" ;
		
					end if;
			end if;
			
		end process;

	end Behavioral;

**************************************************************************************************
#TestBench

	LIBRARY ieee;
	USE ieee.std_logic_1164.ALL;
 
	-- Uncomment the following library declaration if using
	-- arithmetic functions with Signed or Unsigned values
	--USE ieee.numeric_std.ALL;
 
	ENTITY tb IS
	END tb;
 
	ARCHITECTURE behavior OF tb IS 
 
    	-- Component Declaration for the Unit Under Test (UUT)
 
    	COMPONENT Main
   	 PORT(
       	  dataInTransmiter: in std_logic_vector(6 downto 0);
				clk : in std_logic;
				dataOutReciver: out std_logic_vector(6 downto 0)
       	 );
  	  END COMPONENT;
    

 	  --Inputs
  	 signal dataInTransmiter : std_logic_vector(6 downto 0) := (others => '0');
  	 signal clk : std_logic := '0';
	
 	--Outputs
 	  signal dataOutReciver : std_logic_vector(6 downto 0);

  	 -- Clock period definitions
  	 constant clk_period : time := 20 ns;
 
	BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
 	  uut: Main PORT MAP (
   	       dataInTransmiter => dataInTransmiter,
    	      clk => clk,
   	       dataOutReciver => dataOutReciver
   	     );

 	  -- Clock process definitions
	   clk_process :process
 	  begin
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
   	end process;
 

  	 -- Stimulus process
  	 stim_proc: process
  	 begin		
    	  -- hold reset state for 100 ns.
   	   --wait for 100 ns;	
	
   	   wait for clk_period*1/2;
   	   dataInTransmiter <=	 "1101001";
		wait for 100 ns;
		
		 wait for clk_period*1/2;
      		dataInTransmiter	<=	 "0101010";
      	-- insert stimulus here 

      wait;
 	  end process;

	END;
