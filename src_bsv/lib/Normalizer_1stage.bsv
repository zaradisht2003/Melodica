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
         // B-Posit Encoder Logic (rS = 6, eS = 5)
         Bit#(4) k_val = p.scale[8:5];
         Bit#(6) raw_reg = 0;
         Bit#(3) reg_sz = 0;
         
         case (k_val)
             4'b0000: begin raw_reg = 6'b000010; reg_sz = 2; end
             4'b0001: begin raw_reg = 6'b000110; reg_sz = 3; end
             4'b0010: begin raw_reg = 6'b001110; reg_sz = 4; end
             4'b0011: begin raw_reg = 6'b011110; reg_sz = 5; end
             4'b0100: begin raw_reg = 6'b111110; reg_sz = 6; end
             4'b0101: begin raw_reg = 6'b111111; reg_sz = 6; end
             4'b1111: begin raw_reg = 6'b000001; reg_sz = 2; end // -1
             4'b1110: begin raw_reg = 6'b000001; reg_sz = 3; end // -2
             4'b1101: begin raw_reg = 6'b000001; reg_sz = 4; end // -3
             4'b1100: begin raw_reg = 6'b000001; reg_sz = 5; end // -4
             4'b1011: begin raw_reg = 6'b000001; reg_sz = 6; end // -5
             4'b1010: begin raw_reg = 6'b000000; reg_sz = 6; end // -6
             default: begin raw_reg = 6'b000000; reg_sz = 6; end
         endcase
         
         // Apply sign XOR to regime
         Bit#(6) final_reg = raw_reg ^ signExtend(p.sign);
         
         // Exponent is XORed with sign
         Bit#(5) raw_exp = p.scale[4:0] ^ signExtend(p.sign);
         
         // Significand (raw_frac) is 24 bits for eS=5, N=32. Take top 24 bits of FracWidth (27)
         Bit#(24) sig_bits = p.frac[26:3];
         
         // MUX for packing
         Bit#(31) packed_val = 0;
         if (reg_sz == 2) packed_val = {final_reg[1:0], raw_exp, sig_bits};
         else if (reg_sz == 3) packed_val = {final_reg[2:0], raw_exp, sig_bits[23:1]};
         else if (reg_sz == 4) packed_val = {final_reg[3:0], raw_exp, sig_bits[23:2]};
         else if (reg_sz == 5) packed_val = {final_reg[4:0], raw_exp, sig_bits[23:3]};
         else packed_val = {final_reg[5:0], raw_exp, sig_bits[23:4]};
         
         Bit#(32) final_posit = {p.sign, packed_val};
         
         let output_regf = Norm_Posit {
             nan    : p.nan,
             posit  : (p.zi == REGULAR) ? final_posit : fv_outp_z_i(p.zi),
             zi     : p.zi,
             rounding : False // B-Posit decoder/encoder from paper doesn't explicitly round here or it's handled in ALU
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
