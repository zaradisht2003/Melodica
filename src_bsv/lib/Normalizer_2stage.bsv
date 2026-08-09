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
         // B-Posit Encoder Logic (rS = 6, eS = 4)
         Int#(ScaleWidthPlus1) s_val = unpack(p.scale);
         Int#(4) k_int = truncate(s_val >> valueOf(ExpWidth));
         Bit#(4) k_val = pack(k_int);
         Bit#(6) raw_reg = 0;
         Bit#(3) reg_sz = 0;

         case (k_val)
             4'b0000: begin raw_reg = 6'b000010; reg_sz = 2; end // k=0
             4'b0001: begin raw_reg = 6'b000110; reg_sz = 3; end // k=1
             4'b0010: begin raw_reg = 6'b001110; reg_sz = 4; end // k=2
             4'b0011: begin raw_reg = 6'b011110; reg_sz = 5; end // k=3
             4'b0100: begin raw_reg = 6'b111110; reg_sz = 6; end // k=4
             4'b0101: begin raw_reg = 6'b111111; reg_sz = 6; end // k=5
             4'b1111: begin raw_reg = 6'b000001; reg_sz = 2; end // k=-1
             4'b1110: begin raw_reg = 6'b000001; reg_sz = 3; end // k=-2
             4'b1101: begin raw_reg = 6'b000001; reg_sz = 4; end // k=-3
             4'b1100: begin raw_reg = 6'b000001; reg_sz = 5; end // k=-4
             4'b1011: begin raw_reg = 6'b000001; reg_sz = 6; end // k=-5
             4'b1010: begin raw_reg = 6'b000000; reg_sz = 6; end // k=-6
             default: begin raw_reg = 6'b000000; reg_sz = 6; end
         endcase

         // Apply sign XOR to regime
         Bit#(6) final_reg = raw_reg ^ signExtend(p.sign);

         // Exponent is XORed with sign (4 bits for eS=4)
         Bit#(4) raw_exp = p.scale[3:0] ^ signExtend(p.sign);

         // Significand (raw_frac) is 25 bits for eS=4, N=32
         Bit#(25) sig_bits = p.frac[24:0];

         // MUX for packing (31 bits)
         Bit#(31) packed_val = 0;
         if (reg_sz == 2) packed_val = {final_reg[1:0], raw_exp, sig_bits[24:0]};
         else if (reg_sz == 3) packed_val = {final_reg[2:0], raw_exp, sig_bits[24:1]};
         else if (reg_sz == 4) packed_val = {final_reg[3:0], raw_exp, sig_bits[24:2]};
         else if (reg_sz == 5) packed_val = {final_reg[4:0], raw_exp, sig_bits[24:3]};
         else packed_val = {final_reg[5:0], raw_exp, sig_bits[24:4]};

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
