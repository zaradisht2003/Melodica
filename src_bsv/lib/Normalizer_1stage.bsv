// Copyright (c) HPC Lab, Department of Electrical Engineering, IIT Bombay
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

package Normalizer;

// --------------------------------------------------------------
// This package defines:
//
//    mkNormalizer: 1 stage normalizer which composes the
//    different posit fields into a posit word
// --------------------------------------------------------------

// Library imports
import FIFO         :: *;
import GetPut       :: *;
import ClientServer :: *;

// Project imports
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;

// Prenorm_Posit is the input to the normalizer as received from a compute
// pipeline. Value consists of:
//    sign of posit
//    zero and infinity flag
//    NaN flag,
//    scale bits
//    fraction bits that were truncated due to size reduction from quire
//    truncated_frac_msb is the MSB of the truncated bits and
//    truncated_frac_zero is 1 if all the other bits (other than msb) of the
//    trucated fraction are zero

typedef struct {
   Bit #(1)                sign;
   PositType               zi;
   Bool                    nan;
   Bit #(ScaleWidthPlus1)  scale;
   Bit #(FracWidth)        frac;
   Bit #(1)                frac_msb;
   Bit #(1)                frac_zero;
} Prenorm_Posit deriving(Bits,FShow);

// Norm_Posit is the output of the normalizer
typedef struct {
   Bit #(PositWidth) posit;
   PositType         zi;
   Bool              nan;
   Bool              rounding;
} Norm_Posit deriving(Bits,FShow);

(* synthesize *)
module mkNormalizer #(Bit #(2) verbosity) (Server #(Prenorm_Posit, Norm_Posit));

   // make a FIFO to store data at the end of each stage of the pipeline, and also for input and outputs
   FIFO #(Norm_Posit) fifo_output_reg <- mkFIFO;

   // Posit# (N, ES)
   Integer es_int = valueOf(ExpWidth);
   Integer n_int = valueOf(PositWidth);
   Integer n_1_int = valueOf(PositWidthMinus1);
   UInt#(BitsPerPositWidth) n_2_int = fromInteger(n_int-2);

   // this function is used to construct the regime field
   // depending on the value you add those many number of zeros
   // or one with a bit flip at the end
   function Tuple2 #(
        Bit #(PositWidthMinus1)
      , UInt#(BitsPerPositWidth)) fv_calculate_regime (Bit #(ScaleWidthMinusExpWidthPlus1) a);

      Bit #(PositWidthMinus1) one = extend(1'b1);//000000000...001
      Bit #(PositWidthMinus1) k;
      UInt#(BitsPerPositWidth) n1;

      // first bit is 1 means the number is negative so the value has to negated
      // and will be denoted by leading number of zeros            
      if (a [valueOf(ScaleWidthMinusExpWidth)] == 1'b1) begin
         n1 = truncate (unpack (twos_complement(a)));
         k = one<<(n_2_int-n1);//0000..010...0
      end

      // first bit is 0 that means the value denotes the leading number of ones        
      else begin
         n1 = boundedPlus(truncate(unpack(a)),1);
         k = (~('1>>n1));//111...11000..00
      end
      return tuple2(k,n1);
   endfunction

   // this function is used to give output for special case i.e.
   // give all zeros for zero_infinity flag = 10(zero) and give
   // first bit 1 and others zeros for zero_infinity flag =
   // 01(infinity) 
   function Bit#(PositWidth) fv_outp_z_i(PositType z_i);
   Bit#(PositWidth) one = extend(1'b1);
   if(z_i == INF)
       return one<<n_1_int;
   else return '0;
   endfunction

   // This function is used to give final output by taking twos
   // complement of the whole number if the sign bit is 1 
   function Bit#(PositWidth) fv_outp_sign(Bit#(PositWidthMinus1) a, Bit#(1) s);
   if(s == 1)
       return {1'b1,twos_complement(a)};
   else return {1'b0,a};
   endfunction
   
   // This function determines the mask that will be used for the
   // exponent depending on the number of bits available for
   // exponent input: n_2_k: number of bits left for exponent and
   // fraction, exponent value
   // output: shift: the bits the exponent has to shift to be
   // placed just after regime, shift_new :shift in fraction bits
   // to accomodate any overflow of exponent , mask : to see whoch
   // exponent bits are to be used
   function Tuple3#(UInt#(BitsPerPositWidth),Bit#(ExpWidthPlus1), Bit#(ExpWidth)) fv_expo_window_mask (UInt#(BitsPerPositWidth) n_2_k,Bit#(ExpWidth) expo);
       let es_bit = fromInteger(es_int);
       Bit #(ExpWidthPlus1) one = extend(1'b1);
       Bit #(ExpWidth) expo_new = expo;
       UInt#(BitsPerPositWidth) shift;
       Bit #(ExpWidthPlus1) shift_new;

       // the number of bits available less than the maximum number of bits the exponent can use 
       if (n_2_k < es_bit) begin
          // dont shift the exponent, it will be placed at the last 
          shift = 0;

          // which bits of exponent are overlapping with regime field due to less
          // number of bits available for exponent 
          Bit #(ExpWidth) mask_e = '1>>(es_bit - n_2_k);

          // the overlap bits are 0 i.e. dont hold any information
          if ((expo & mask_e) == 0) begin
             shift_new = 0;//n_2_k se shift expo
             expo_new = expo >> n_2_k;// use all exponent bits
          end

          // use the complement of the value of exponent since we are increasing
          // or decreasing the regime by 1 as required, as round off of exponent
          // as can be seen in the mask  
          else begin
             shift_new = extend (twos_complement (expo));
             expo_new = truncate(one<<n_2_k) & expo;//any of the overlap bits is 1 ????????????(es)>1???
          end
       end

       else begin
          shift = n_2_k-es_bit;//shift the es bits to place them just after the regime field
          shift_new = 0;//no change in fraction bits
          expo_new = expo;//use all exponent bits
       end
       return tuple3(shift,shift_new,expo_new);
   endfunction

   interface Put request;
      method Action put (Prenorm_Posit p);
         // B-Posit implementation
         Int#(ScaleWidthMinusExpWidthPlus1) k_val_signed = unpack(p.scale [valueOf(ScaleWidth):es_int]);
         Bit#(ExpWidth) expo = (es_int == 0) ? 0 : p.scale[es_int-1:0];
         Bit#(FracWidth) frac = p.frac;
         Bit#(1) frac_msb = p.frac_msb;
         Bit#(1) frac_zero = p.frac_zero;

         // Saturation for B-Posit regime limit [-6, 5]
         if (k_val_signed > 5) begin
             k_val_signed = 5;
             expo = '1;
             frac = '1;
             frac_msb = 0;
             frac_zero = 1;
         end else if (k_val_signed < -6) begin
             k_val_signed = -6;
             expo = '0;
             frac = '0;
             frac_msb = 0;
             frac_zero = 1;
         end
         
         Int#(4) k_val = truncate(k_val_signed);
         
         Bit#(6) regime_bits;
         UInt#(3) regime_len; // 2 to 6
         
         case (k_val)
             5:  begin regime_bits = 6'b111111; regime_len = 6; end
             4:  begin regime_bits = 6'b111110; regime_len = 6; end
             3:  begin regime_bits = 6'b111100; regime_len = 5; end
             2:  begin regime_bits = 6'b111000; regime_len = 4; end
             1:  begin regime_bits = 6'b110000; regime_len = 3; end
             0:  begin regime_bits = 6'b100000; regime_len = 2; end
             -1: begin regime_bits = 6'b010000; regime_len = 2; end
             -2: begin regime_bits = 6'b001000; regime_len = 3; end
             -3: begin regime_bits = 6'b000100; regime_len = 4; end
             -4: begin regime_bits = 6'b000010; regime_len = 5; end
             -5: begin regime_bits = 6'b000001; regime_len = 6; end
             -6: begin regime_bits = 6'b000000; regime_len = 6; end
         endcase

         // Calculate shift for fraction (S = regime_len - 2)
         UInt#(3) S = regime_len - 2; // 0 to 4
         
         // Calculate the shifted fraction and rounding bits
         Bit#(FracWidth) shifted_frac = frac >> S;
         
         Bit#(1) flag_prev_truncate;
         Bit#(1) truncated_frac_zero;
         Bit#(1) truncated_frac_msb;
         
         if (S == 0) begin
             flag_prev_truncate = frac_msb;
             truncated_frac_zero = frac_zero;
             truncated_frac_msb = frac_msb;
         end else if (S == 1) begin
             flag_prev_truncate = frac[0];
             truncated_frac_zero = frac_zero & (~frac_msb);
             truncated_frac_msb = frac_msb;
         end else if (S == 2) begin
             flag_prev_truncate = frac[1];
             truncated_frac_zero = frac_zero & (~frac_msb) & (~frac[0]);
             truncated_frac_msb = frac[0];
         end else if (S == 3) begin
             flag_prev_truncate = frac[2];
             truncated_frac_zero = frac_zero & (~frac_msb) & (~frac[0]) & (~frac[1]);
             truncated_frac_msb = frac[1];
         end else begin // S == 4
             flag_prev_truncate = frac[3];
             truncated_frac_zero = frac_zero & (~frac_msb) & (~frac[0]) & (~frac[1]) & (~frac[2]);
             truncated_frac_msb = frac[2];
         end

         // Combine regime, exponent, and shifted fraction
         // Total bits for magnitude = PositWidth - 1
         // Magnitude = {regime_bits[5:6-regime_len], expo, shifted_frac[FracWidth-1 : S]}
         Bit#(PositWidthMinus1) magnitude = 0;
         
         // Build a PositWidthMinus1 bit string
         Bit#(PositWidthMinus1) padded_regime = extend(regime_bits);
         padded_regime = padded_regime << (valueOf(PositWidthMinus1) - 6);
         
         // Shift the regime to the very top
         Bit#(PositWidthMinus1) padded_expo = extend(expo);
         padded_expo = padded_expo << (valueOf(PositWidthMinus1) - 6 - es_int);
         
         Bit#(PositWidthMinus1) padded_frac = extend(shifted_frac);
         
         // Assemble using OR and shifts
         // Note: regime is left-aligned. Exponent starts after regime_len bits.
         // Fraction starts after regime_len + es_int bits.
         
         // Create masks or shifts dynamically (BSV barrel shifter)
         // Wait, B-Posit limits it to a MUX!
         Bit#(PositWidthMinus1) assembled_mag = 0;
         if (S == 0) begin // regime_len == 2
             assembled_mag = {regime_bits[5:4], expo, shifted_frac[valueOf(FracWidth)-1:0]};
         end else if (S == 1) begin // regime_len == 3
             assembled_mag = {regime_bits[5:3], expo, shifted_frac[valueOf(FracWidth)-1:1]};
         end else if (S == 2) begin // regime_len == 4
             assembled_mag = {regime_bits[5:2], expo, shifted_frac[valueOf(FracWidth)-1:2]};
         end else if (S == 3) begin // regime_len == 5
             assembled_mag = {regime_bits[5:1], expo, shifted_frac[valueOf(FracWidth)-1:3]};
         end else begin // S == 4, regime_len == 6
             assembled_mag = {regime_bits[5:0], expo, shifted_frac[valueOf(FracWidth)-1:4]};
         end
         
         // Rounding logic
         Bit#(1) expo_even = (es_int == 0) ? ~regime_bits[6-regime_len] : ~expo[0];
         Bit#(1) last_bit = assembled_mag[0];
         Bit#(1) flag_equidistant = 0;
         
         if (flag_prev_truncate == 1'b1 && truncated_frac_zero == 1'b1 && last_bit == 1'b0) begin
             flag_equidistant = 1'b1;
         end
         
         UInt#(PositWidthMinus1) uint_mag = unpack(assembled_mag);
         UInt#(PositWidthMinus1) rounded_mag = boundedPlus(uint_mag, extend(flag_prev_truncate));
         rounded_mag = rounded_mag - extend(flag_equidistant);
         
         Bool rounding = (flag_prev_truncate - flag_equidistant == 1'b1 || rounded_mag == 0 && flag_equidistant == 0);

         let output_regf = Norm_Posit {
             nan    : p.nan,
             posit  : (p.zi == REGULAR) ? fv_outp_sign (pack (rounded_mag), p.sign)
                                        : fv_outp_z_i (p.zi),
             zi     : p.zi,
             rounding : rounding
          };
         fifo_output_reg.enq(output_regf);
         `ifdef RANDOM_PRINT
         $display("output_norm %b",output_regf);
         `endif
      endmethod
   endinterface
   interface Get response = toGet (fifo_output_reg);
endmodule

endpackage: Normalizer
