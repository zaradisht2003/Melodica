import StmtFSM :: *;
import Cur_Cycle :: *;
import FIFOF        :: *;
import FIFO        :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import FloatingPoint :: *;
import LFSR         :: *;

// Project imports
import PositCore_Types :: *;
import PositCore_Coproc :: *;
import PositCore :: *;
import Extracter :: *;
import Normalizer :: *;
import Extracter_Types :: *;
import Normalizer_Types :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import FMA_PNE_Quire_PC :: *;					
import FDA_PNE_Quire_PC :: *;
import FtoP_PNE_PC :: *;
import PtoF_PNE_PC :: *;
import PositToQuire_PNE_PC :: *;
import QuireToPosit_PNE_PC :: *;
`ifdef BASIC_OPS
import Add_PNE_PC :: *;
import Mul_PNE_PC :: *;
import Div_PNE_PC :: *;
`endif
import FloatingPoint :: *;
import Utils  :: *;

`ifdef QUILLS
import FPU_Types :: *;
`else

//type definitions
typedef enum {RESET, START, FTP, FMA, QTP, OUT} State deriving (Bits,Eq, FShow);

typedef 255 Num_Tests;

(*synthesize*)
//Create an FSM specification (an expression of type Stmt)
module mkTb (Empty) ;
	Reg #(State)	state   <- mkReg(RESET);
	Reg #(FSingle)  reg1a   <- mkReg(0);
	Reg #(FSingle)  reg1b   <- mkReg(0);
	Reg #(FSingle)  reg2a   <- mkReg(0);
	Reg #(FSingle)  reg2b   <- mkReg(0);
	Reg #(FSingle)  reg_dummy <- mkReg(0);
//	Reg #(PositCmds) reg_round <- mkReg(0);
	FloatU reg1a_f = tagged S reg1a;
	FloatU reg1b_f = tagged S reg1b;
	FloatU reg2a_f = tagged S reg2a;
	FloatU reg2b_f = tagged S reg2b;
	FloatU reg_d = tagged S reg_dummy;
	FloatingPoint::RoundMode round_mode = Rnd_Nearest_Even;
	//module mkPositCore #(Bit #(4) verbosity) (PositCore_IFC);
	PositCore_IFC positCore_ftop1 <- mkPositCore(4'b0);
	PositCore_IFC positCore_ftop2 <- mkPositCore(4'b0);
	PositCore_IFC positCore_fma <- mkPositCore(4'b0);
	PositCore_IFC positCore_ptof <- mkPositCore(4'b0);
	PositCore_IFC_accel positCore_accel <- mkPositCore_accel(4'b0);

	LFSR  #(Bit#(32))            lfsr1          <- mkLFSR_32;
	LFSR  #(Bit#(32))            lfsr2           <- mkLFSR_32;
	Reg   #(Bool)                 rgSetup        <- mkReg (False);
	Reg   #(Bool)                 rgGenComplete  <- mkReg (False);
	Reg   #(Bit#(32))             rg_count_fsm  <- mkReg (0);
	Reg   #(Bit#(32))             rg_match  <- mkReg (0);
	Reg   #(Bit#(32))             rg_no_match  <- mkReg (0);
	Reg   #(Bit#(32))             rg_check  <- mkReg (0);

	Integer fifo_depth = 1024;
	FIFO #(Fpu_Rsp) ffO_PositCore <- mkSizedFIFO(fifo_depth);
	FIFO #(FloatE) ffO_PositCore_accel <- mkSizedFIFO(fifo_depth);
//	FIFO#(FloatE) inbound1 <- mkSizedFIFO(fifo_depth);
//	Reg #(FloatE) rg_PositCore_accel <- mkRegU;
	Reg #(Bool) doneSet <-mkReg(False);


	rule lfsrGenerate(!doneSet);
		lfsr1.seed('h10);// to create different random series
		lfsr2.seed('h10);
		doneSet<= True;
		rg_count_fsm <= 256;
	endrule
	//--------------------------------------------------------------------------------------------------------------	
// FSM using rules

(* mutually_exclusive = "rl_reset, rl_dispatch, rl_ftop, rl_fma, rl_qtp, rl_out" *)
	//--------------------------------------------------------------------------------------------------------------	
	//rule quire resetting
	rule rl_reset (state == RESET);
		positCore_fma.server_core.request.put(tuple4(reg_d, reg_d, round_mode, FCVT_R_P)); //core quire reset 
		positCore_accel.server_core.request.put(tuple4(reg_d,reg_d,RST_Q,round_mode));	//accel quire reset
		state <= START;
		$display ("%0d: State: ", cur_cycle, fshow (state));
	endrule
	//--------------------------------------------------------------------------------------------------------------	
	//rule initiating ftop for both
	rule rl_dispatch ( state == START && doneSet );
		let a <- positCore_fma.server_core.response.get();
		let b <- positCore_accel.server_core.response.get();	
		FSingle v_1a = unpack(lfsr1.value());
//		FSingle v_1a = 25;
//		reg1a <= v_1a;
		FSingle v_2a = unpack(lfsr2.value());
//		FSingle v_2a =50;
//		reg2a <= v_2a;
		lfsr1.next ();
   		lfsr2.next ();
		rg_count_fsm <= rg_count_fsm - 1;
		positCore_ftop1.server_core.request.put(tuple4(tagged S v_1a,reg_d,round_mode,FCVT_P_S));
		positCore_ftop2.server_core.request.put(tuple4(tagged S v_2a,reg_d,round_mode,FCVT_P_S)); 
		positCore_accel.server_core.request.put(tuple4(tagged S v_1a,tagged S v_2a,FMS_P,round_mode));
		$display ("%0d: State: ", cur_cycle, fshow (state));
		state <= FTP;
	endrule
	//--------------------------------------------------------------------------------------------------------------	
	//rule initiating fma
	rule rl_ftop (state == FTP);
		let ftop_a <- positCore_ftop1.server_core.response.get();
		let ftop_b <- positCore_ftop2.server_core.response.get();
		positCore_fma.server_core.request.put(tuple4(tpl_1(ftop_a),tpl_1(ftop_b),round_mode,FMS_P));
//		positCore_accel.server_core.request.put(tuple4(reg1a_f,reg2a_f,FMA_P,round_mode));
		state <= FMA;
		$display ("%0d: State: ", cur_cycle,fshow(state)," ",fshow(ftop_a), " ", fshow(ftop_b));
	endrule
	//--------------------------------------------------------------------------------------------------------------	
	//rule initiating quire to posit
	rule rl_fma (state == FMA);
		let accel_a <- positCore_accel.server_core.response.get();
		let a <- positCore_fma.server_core.response.get();
		positCore_fma.server_core.request.put(tuple4(reg_d, reg_d, round_mode, FCVT_P_R));
		positCore_accel.server_core.request.put(tuple4(reg_d, reg_d,RD_Q,round_mode));
		state <= QTP;
		$display ("%0d: State: ", cur_cycle, fshow (state));
	endrule
	//--------------------------------------------------------------------------------------------------------------	
	// rule initiation posit to float
	rule rl_qtp (state == QTP);
		let posit_val <- positCore_fma.server_core.response.get();
		positCore_ptof.server_core.request.put(tuple4(tpl_1(posit_val), reg_d, round_mode, FCVT_S_P));
		state <= OUT;

		let float_out <- positCore_accel.server_core.response.get();
		let e_out = tpl_1(float_out);
		if (isValid(e_out)) 
			begin
			FloatE final_out = fromMaybe(?,e_out);
			ffO_PositCore_accel.enq(final_out);
			//rg_PositCore_accel <= final_out;
			$display(fshow(final_out));
			end
		$display ("%0d: State: ", cur_cycle, fshow (state));
	endrule
	//--------------------------------------------------------------------------------------------------------------	
	//rule for final output
	rule rl_out (state == OUT && !rgGenComplete);
		let float_out <- positCore_ptof.server_core.response.get();
		ffO_PositCore.enq(float_out);
		$display ("%0d: State: ",cur_cycle, fshow(state),"OUT_PositCore", fshow(float_out));


		if (rg_count_fsm != 0)
			state <= RESET;
		else rgGenComplete <= True;		
	endrule
	//--------------------------------------------------------------------------------------------------------------	

/*	rule rl_out_accel;
		let float_out <- positCore_accel.server_core.response.get();
		match{.var1, .var2} = float_out;
		if (isValid (var1))
			begin			
			ffO_PositCore_accel.enq(var1.Valid);
			$display ("%0d: State: ",cur_cycle,"OUT_accel", fshow(var1.Valid));
			end
	endrule
*/
	//--------------------------------------------------------------------------------------------------------------	

	rule rl_check(rgGenComplete && rg_check < 257);
		let a = ffO_PositCore.first();
		let b = ffO_PositCore_accel.first();
		if (a==b)
			begin
			//$display("Outputs match", a, " ", b);
			rg_match <= rg_match +1;
			//$display(fshow(rg_match));
			end
		else
			begin
			$display("No match", a, " ", b); 
			rg_no_match <= rg_no_match + 1;
			end
		ffO_PositCore.deq();
		ffO_PositCore_accel.deq();
		rg_check <= rg_check + 1;
		//$display(fshow(rg_check));
		if (rg_check == 32'hff)
			begin
			$display("Match new: ",fshow(rg_match));
			$display("No Match: ",fshow(rg_no_match));
			end		
	endrule

		

endmodule		








