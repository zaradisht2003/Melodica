package Normalizer;

import FIFO         :: *;
import GetPut       :: *;
import ClientServer :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;

typedef struct {
   Bit #(1)                sign;
   PositType               zi;
   Bool                    nan;
   Bit #(ScaleWidthPlus1)  scale;
   Bit #(FracWidth)        frac;
   Bit #(1)                frac_msb;
   Bit #(1)                frac_zero;
} Prenorm_Posit deriving(Bits,FShow);

typedef struct {
   Bit #(PositWidth) posit;
   PositType         zi;
   Bool              nan;
   Bool              rounding;
} Norm_Posit deriving(Bits,FShow);

function Bit#(PositWidth) fv_outp_z_i(PositType z_i);
   Bit#(PositWidth) one = extend(1'b1);
   if(z_i == INF)
       return one<<(valueOf(PositWidthMinus1));
   else return '0;
endfunction

(* synthesize *)
module mkNormalizer #(Bit #(2) verbosity) (Server #(Prenorm_Posit, Norm_Posit));
   FIFO #(Norm_Posit) fifo_output_reg <- mkFIFO;

   interface Put request;
      method Action put (Prenorm_Posit p);
         // B-Posit Encoder Logic (rS = 3, eS = 6)
         Int#(ScaleWidthPlus1) s_val = unpack(p.scale);
         Int#(4) k_int = truncate(s_val >> valueOf(ExpWidth));
         Bit#(4) k_val = pack(k_int);
         Bit#(3) raw_reg = 0;
         Bit#(2) reg_sz = 0;

         case (k_val)
             4'b0000: begin raw_reg = 3'b010; reg_sz = 2; end // k=0
             4'b0001: begin raw_reg = 3'b110; reg_sz = 3; end // k=1
             4'b1111: begin raw_reg = 3'b001; reg_sz = 2; end // k=-1
             4'b1110: begin raw_reg = 3'b000; reg_sz = 3; end // k=-2
             default: begin raw_reg = (msb(k_val) == 0) ? 3'b110 : 3'b000; reg_sz = 3; end
         endcase

         // Apply sign XOR to regime
         Bit#(3) final_reg = raw_reg ^ signExtend(p.sign);

         // Exponent is XORed with sign (6 bits for eS=6)
         Bit#(6) raw_exp = p.scale[5:0] ^ signExtend(p.sign);

         // Significand (raw_frac) is 23 bits for eS=6, N=32
         Bit#(23) sig_bits = p.frac[22:0];

         // MUX for packing (31 bits)
         Bit#(31) packed_val = 0;
         if (reg_sz == 2) packed_val = {final_reg[1:0], raw_exp, sig_bits[22:0]};
         else packed_val = {final_reg[2:0], raw_exp, sig_bits[22:1]};

         Bit#(32) final_posit = {p.sign, packed_val};

         let output_regf = Norm_Posit {
             nan    : p.nan,
             posit  : (p.zi == REGULAR) ? final_posit : fv_outp_z_i(p.zi),
             zi     : p.zi,
             rounding : False
          };
         fifo_output_reg.enq(output_regf);

         if (verbosity > 1) begin
             $display("output_norm %b", output_regf);
         end
      endmethod
   endinterface
   interface Get response = toGet (fifo_output_reg);
endmodule

endpackage: Normalizer
