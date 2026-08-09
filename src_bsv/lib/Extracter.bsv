package Extracter;

import FIFO                :: *;
import GetPut              :: *;
import ClientServer        :: *;
import FShow               :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import Utils               :: *;

typedef struct {
   PositType               ziflag;  // REGULAR, INF, ZERO
   Bit #(1)                sign;    // The sign bit
   Int #(ScaleWidthPlus1)  scale;   // scale value
   Bit #(FracWidth)        frac;    // fraction value
} Posit_Extract deriving(Bits, FShow);

Integer es_int = valueOf(ExpWidth); // eS = 2 for B-Posit
Integer n_int = valueOf(PositWidth);

function PositType fv_special_case(Bit#(PositWidth) x);
   Bit #(1) a = ((x<<1) ==  0 ? 1'b0 : 1'b1);
   if (a == 1'b0 && msb(x) == 1'b0) return ZERO;
   else if(a == 1'b0 && msb(x) == 1'b1) return INF;
   else return REGULAR;
endfunction

(* synthesize *)
module mkExtracter #(Bit #(2) verbosity) (Server #(Posit, Posit_Extract));
   FIFO #(Posit_Extract) ff_extract_out <- mkFIFO;

   interface Put request;
      method Action put (Posit p);
         let zi = fv_special_case(p);
         let sign = msb(p);

         // B-Posit Decode Logic (rS = 6, eS = 2)
         Bit#(5) chck_bits = p[n_int-3 : n_int-7];
         Bit#(1) reg_msb = p[n_int-2];
         Bit#(5) xor_chck = chck_bits ^ signExtend(reg_msb);

         // One-hot selection: bit is 1 when terminating bit (different from reg_msb) is reached
         Bit#(5) one_hot_sel;
         one_hot_sel[4] = xor_chck[4]; // size 2 (p[n_int-3] != reg_msb)
         one_hot_sel[3] = ~xor_chck[4] & xor_chck[3]; // size 3
         one_hot_sel[2] = ~xor_chck[4] & ~xor_chck[3] & xor_chck[2]; // size 4
         one_hot_sel[1] = ~xor_chck[4] & ~xor_chck[3] & ~xor_chck[2] & xor_chck[1]; // size 5
         one_hot_sel[0] = ~xor_chck[4] & ~xor_chck[3] & ~xor_chck[2] & ~xor_chck[1]; // size 6 (max rS)

         // k >= 0 when (reg_msb ^ sign) == 1
         Bool k_pos = (reg_msb ^ sign) == 1'b1;

         // Priority Encoder for Regime Value k
         Int#(RegimeWidth) regime;
         if (one_hot_sel[4] == 1) regime = k_pos ? 0 : -1;
         else if (one_hot_sel[3] == 1) regime = k_pos ? 1 : -2;
         else if (one_hot_sel[2] == 1) regime = k_pos ? 2 : -3;
         else if (one_hot_sel[1] == 1) regime = k_pos ? 3 : -4;
         else regime = k_pos ? 4 : -5; // size 6

         // EXP_SIG MUX (N-3 bits wide output)
         Bit#(PositWidthMinus3) exp_sig_out = 0;
         if (one_hot_sel[4] == 1) exp_sig_out = p[n_int-4 : 0];
         else if (one_hot_sel[3] == 1) exp_sig_out = {p[n_int-5 : 0], 1'b0};
         else if (one_hot_sel[2] == 1) exp_sig_out = {p[n_int-6 : 0], 2'b00};
         else if (one_hot_sel[1] == 1) exp_sig_out = {p[n_int-7 : 0], 3'b000};
         else if (one_hot_sel[0] == 1) exp_sig_out = {p[n_int-8 : 0], 4'b0000};

         // Extract Exponent (5 bits for eS=5) and Fraction (24 bits for N=32, eS=5)
         Bit#(5) raw_exp = exp_sig_out[n_int-4 : n_int-8];
         Bit#(FracWidth) raw_frac = exp_sig_out[n_int-9 : 0];

         // Process Exponent
         Bit#(5) exp = raw_exp ^ signExtend(sign);
         Bit#(1) exp_cin = (sign == 1 && raw_frac == 0) ? 1 : 0;

         let output_regf = Posit_Extract {
            ziflag : zi,
            sign : sign,
            scale : (zi == ZERO) ? 0 : ((extend(regime) << es_int) + unpack(extend(exp)) + unpack(extend(exp_cin))),
            frac : (zi == ZERO) ? 0 : raw_frac
         };

         ff_extract_out.enq(output_regf);

         if (verbosity > 1) begin
            $display ("%m.request: 0x%08x", p);
         end
      endmethod
   endinterface
   interface Get response = toGet (ff_extract_out);
endmodule

endpackage: Extracter
