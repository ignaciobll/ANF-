{-# LANGUAGE QuasiQuotes #-}
module CLSat where

import           Language.C.Quote.OpenCL        ( cfun )
import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( ppr )


kernelAnfAnd :: String
kernelAnfAnd = pretty 100 . ppr $ [cfun|
  /* Scalar Product - a(b + c) = ab + ac */
  __kernel void anfAND(int anf_1_size,   const __global uint *anf_1,
                       int anf_2_size,   const __global uint *anf_2,
                       int out_anf_size, __global uint *out_anf){

    int gid = get_global_id(0);

    if (gid < out_anf_size) {
      int anf_1_i = gid % anf_1_size;
      int anf_2_i = gid / anf_1_size;
      // It's an and operation, but merging variables is an or
      out_anf[gid] = anf_1[anf_1_i] | anf_2[anf_2_i];
    }
  }
|]
