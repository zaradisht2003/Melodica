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

package Extracter;

// --------------------------------------------------------------
// This package defines:
//
//    mkExtracter: 1-stage extracter which extracts the different
//    posit fields
// --------------------------------------------------------------


import FIFO                :: *;
import GetPut              :: *;
import ClientServer        :: *;
import FShow               :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import Utils               :: *;
import Cur_Cycle           :: *;

// --------
// Local type definitions
// Posit_Extract is the output of the extracter. 
typedef struct {
   PositType               ziflag;  // REGULAR, INF, ZERO
   Bit #(1)                sign;    // The sign bit
   Int #(ScaleWidthPlus1)  scale;   // scale value
   Bit #(FracWidth)        frac;    // fraction value
} Posit_Extract deriving(Bits, FShow);

// --------
// Helper functions
//
Integer es_int = valueOf(ExpWidth);
Integer n_int = valueOf(PositWidth);

// special case function is used to find the zero and infinity flag this
// function gives output 10 for zero, 01 for infinity, 00 if none Input is zero
// if all bits are 0 and infinity if the MSB is 1 and all other bits are 0
function PositType fv_special_case(Bit#(PositWidth) x);
   // a checks if all bits other than MSB are 0, if thery are a = 0 else a = 1
   Bit #(1) a = ((x<<1) ==  0 ? 1'b0 : 1'b1);

   // return 10 if all bits are 0
   if (a == 1'b0 && msb(x) == 1'b0) 
      return ZERO;

   // return 01 if MSB is 1 and other 0
   else if(a == 1'b0 && msb(x) == 1'b1) 
      return INF;

   // return 00 for all other cases
   else return REGULAR;
endfunction

// frac shift function is used to output the number of bits I need to shift the
// frac so that the the starting of the fraction is the first bit in output then
// it is appended with zeros
function Bit#(FracWidth) fv_frac_shift(UInt#(Iteration) iter0);
   Bit#(FracWidth) mask = '0;

   for (Integer k = 0; k<=valueOf(FracWidth); k=k+1)
      if (k == 0 && iter0 <= fromInteger(es_int))
         mask =fromInteger(valueOf(FracWidth)-k);
      else if(iter0 == fromInteger(es_int + k))
         mask =fromInteger(valueOf(FracWidth)-k);
   return mask;
endfunction


// --------
//
(* synthesize *)
module mkExtracter #(Bit #(2) verbosity) (Server #(Posit, Posit_Extract));
   // make a FIFO to store data at the end of each stage of the pipeline, and also
   // for input and outputs
   FIFO #(Posit_Extract) ff_extract_out <- mkFIFO;

   interface Put request;
      method Action put (Posit p);
         // check for zero and infinity special cases
         let zi = fv_special_case(p);

         // sign bit is 0 when posit is positive else 1 when posit is negative
         let sign = msb (p);

         // new input stage0 is got after removing the sign bit and finding its two's complement if posit is negative from input posit
         Bit#(PositWidthMinus1) new_inp1 = truncate(p);
         Bit#(PositWidthMinus1) new_inp = (sign == 0) ? new_inp1
                                                      : twos_complement(new_inp1);

         // B-Posit modifications:
         // The regime is limited to a maximum length of 6 bits.
         // Thus, we only compare new_inp[n_int-2] with the next 5 bits.
         Bit#(1) rc = new_inp[n_int-2];
         Bit#(5) regime_check_bits = new_inp[n_int-3 : n_int-7];
         Bit#(5) xor_bits = regime_check_bits ^ signExtend(rc);

         // Priority encoder to find the first differing bit (which marks the end of the regime)
         // pe_out is the number of matching bits after the first regime bit (0 to 5)
         Bit#(3) pe_out;
         if (xor_bits[4] == 1'b1) pe_out = 0;
         else if (xor_bits[3] == 1'b1) pe_out = 1;
         else if (xor_bits[2] == 1'b1) pe_out = 2;
         else if (xor_bits[1] == 1'b1) pe_out = 3;
         else if (xor_bits[0] == 1'b1) pe_out = 4;
         else pe_out = 5; // Regime is max 6 bits, so no terminating bit is checked beyond this

         // k gives the value of regime field
         Int#(RegimeWidth) k = (rc == 1'b1) ? unpack(extend(pe_out)) 
                                            : unpack(twos_complement(extend(pe_out + 1)));

         // MUX to extract exponent and fraction bits together
         // Since regime length is 1 + pe_out, and there is an opposite bit (except when pe_out == 5 where there might not be),
         // the exponent starts at n_int - 2 - regime_length, wait:
         // For pe_out == 0 (length 1), exp starts at n_int - 4
         // For pe_out == 1 (length 2), exp starts at n_int - 5
         // For pe_out == 2 (length 3), exp starts at n_int - 6
         // For pe_out == 3 (length 4), exp starts at n_int - 7
         // For pe_out == 4 (length 5), exp starts at n_int - 8
         // For pe_out == 5 (length 6), exp starts at n_int - 8 (Because there is no opposite bit! Wait!)
         
         // Let's verify B-Posit regime termination:
         // "the b-posit restricts the regime field to a 6-bit limit"
         // If pe_out == 5, the 6 bits are used for regime. The VERY NEXT bit is the exponent.
         // So if pe_out == 5, regime is at [n_int-2 : n_int-7]. Exponent starts at n_int-8!
         // Wait, if pe_out == 4, regime is at [n_int-2 : n_int-6], opposite bit at n_int-7. Exponent starts at n_int-8!
         // This means for both pe_out == 4 and pe_out == 5, the exponent starts at n_int-8.
         
         // Let's create the unshifted remaining bits
         Bit#(PositWidthMinus3) remaining_bits_0 = truncate(new_inp); // bits [n_int-4 : 0]
         Bit#(PositWidthMinus3) remaining_bits_1 = {new_inp[n_int-5:0], 1'b0};
         Bit#(PositWidthMinus3) remaining_bits_2 = {new_inp[n_int-6:0], 2'b0};
         Bit#(PositWidthMinus3) remaining_bits_3 = {new_inp[n_int-7:0], 3'b0};
         Bit#(PositWidthMinus3) remaining_bits_4 = {new_inp[n_int-8:0], 4'b0};
         Bit#(PositWidthMinus3) remaining_bits_5 = {new_inp[n_int-8:0], 4'b0}; // same shift as pe_out==4
         
         Bit#(PositWidthMinus3) remaining_bits_shifted = 0;
         case (pe_out)
             0: remaining_bits_shifted = remaining_bits_0;
             1: remaining_bits_shifted = remaining_bits_1;
             2: remaining_bits_shifted = remaining_bits_2;
             3: remaining_bits_shifted = remaining_bits_3;
             4: remaining_bits_shifted = remaining_bits_4;
             5: remaining_bits_shifted = remaining_bits_5;
         endcase

         // Exponent is the first es_int bits of remaining_bits_shifted
         Bit#(ExpWidth) expo = remaining_bits_shifted[valueOf(PositWidthMinus3)-1 : valueOf(PositWidthMinus3)-es_int];
         
         // Fraction is the rest
         Bit#(FracWidth) frac = truncate(remaining_bits_shifted << es_int);

         let output_regf = Posit_Extract {
            ziflag : zi,
            sign : sign ,
            // k_scale = 2^(Es)*k
            // scale = k_scale + exponent field (base 2)
            scale : (zi == ZERO) ? 0
                                 : ((extend(k) << es_int) + unpack(extend(expo))),
            // carrying fraction bits fordward
            frac : (zi == ZERO) ? unpack(fromInteger(0)) : frac
         };

         ff_extract_out.enq(output_regf);

         if (verbosity > 1) begin
            $display ("%0d: %m.request: 0x%08x", cur_cycle, p);
         end
      endmethod
   endinterface
   interface Get response = toGet (ff_extract_out);
endmodule

endpackage: Extracter


