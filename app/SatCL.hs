{-# LANGUAGE QuasiQuotes #-}

module SatCL where


import           Control.Parallel.OpenCL        ( CLDeviceType(..)
                                                , CLMemFlag(..)
                                                , CLError
                                                , clCreateBuffer
                                                , clCreateKernel
                                                -- Context
                                                , CLContext
                                                , clCreateContextFromType
                                                , clGetContextDevices
                                                , clReleaseContext
                                                -- Program
                                                , clCreateProgramWithSource
                                                , clBuildProgram
                                                , clGetProgramBuildLog -- For exception handling
                                                , clSetKernelArgSto
                                                -- Command Queue
                                                , clCreateCommandQueue
                                                , clEnqueueNDRangeKernel
                                                )


import           Data.Vector.Storable           ( Vector )
import qualified Data.Vector.Storable          as V
import           Foreign                        ( nullPtr
                                                , sizeOf
                                                )
import           Foreign.C.Types                ( CFloat
                                                , CInt
                                                , CUInt
                                                , CULong
                                                )

import           Language.C.Quote.OpenCL        ( cfun )

import           CLUtil.VectorBuffers           ( writeVectorToBuffer
                                                , bufferToVector
                                                )
import           CLUtil                         ( OpenCLState(..)
                                                , clContext
                                                , clDevice
                                                , clQueue
                                                )
{- General imports -}

import           Control.Exception              ( try )

import           Data.Foldable                  ( toList )

import           Data.Text.Lazy                 ( unpack )

import           Text.Pretty.Simple             ( pShow )
import           Text.PrettyPrint.Mainland      ( pretty
                                                , prettyCompact
                                                )
import           Text.PrettyPrint.Mainland.Class
                                                ( ppr )

{- Local Imports -}
import           Data.Formula.ANF.Base          ( canonical
                                                , fromProp
                                                )
import           Data.Formula.ANF.CanonicalBase ( fromBase )
import           Data.Formula.ANF.VectorRepr    ( fromCanonicalBase )
import           Data.Formula.Prop              ( fromSmt )

import           Data.SAT.Smt                   ( parseSmtFile )
import           Data.SAT.DIMACS                ( printParseError )

import qualified Data.List                     as L
import qualified Data.Formula.ANF.Base         as B

import           Utils.VarsTable                ( VarsTable(..) )
import qualified Utils.VarsTable               as VT

import           Utils.OpenCL                   ( autoKernel
                                                , ProgramSource(..)
                                                )
import           Utils.OpenCL.Bitonic           ( bitonicSortBuffer
                                                , BufferSize(..)
                                                )

kernelSource :: String
kernelSource = prettyCompact . ppr $ [cfun|
    /* This example kernel just does `map (*2)` */
    kernel void doubleArray(
        global int *in,
        global int *out
    ) {
        int i = get_global_id(0);
        out[i] = 2 * in[i];
    }
|]

kernelAnfAnd :: String
kernelAnfAnd = pretty 100 . ppr $ [cfun|
  /* Scalar Product - a(b + c) = ab + ac */
  __kernel void anfAND(int anf_1_size,   const __global uint *anf_1,
                       int anf_2_size,   const __global uint *anf_2,
                       int out_anf_size, __global uint *out_anf){

    int gid = get_global_id(0);

    if (gid < out_anf_size) {
      int anf_1_i = gid / anf_1_size;
      int anf_2_i = gid % anf_2_size;
      out_anf[gid] = anf_1[anf_1_i] & anf_2[anf_2_i];
    }
  }
|]

main :: IO ()
main = do
  context <- clCreateContextFromType [] [CL_DEVICE_TYPE_GPU] print
  device  <- head <$> clGetContextDevices context
  queue   <- clCreateCommandQueue context device []

  let state = OpenCLState { clDevice = device, clContext = context, clQueue = queue }

  smt <- loadSmt "test/files/test2.smt"
  case smt of
    Left  msg     -> putStrLn msg >> clReleaseContext context >> putStrLn "#fail"
    Right (v, vt) -> satanf state v vt >> clReleaseContext context >> putStrLn "#success"

satanf :: OpenCLState -> [Int] -> VarsTable a -> IO ()
satanf state v vt = do

  let (OpenCLState device context queue) = state
  let inputData = (V.fromList $ fmap fromIntegral v) :: Vector CUInt
  let nElem                              = V.length inputData
  let nBytes                             = nElem * sizeOf (undefined :: CUInt)
  let outNBytes                          = nBytes * nBytes
  putStrLn $ "Input data: " ++ show inputData

  program            <- clCreateProgramWithSource context kernelAnfAnd
  buildProgramStatus <- (try $ clBuildProgram program [device] "") :: IO (Either CLError ())

  case buildProgramStatus of
    Left clErr -> do
      putStrLn . show $ clErr
      clGetProgramBuildLog program device >>= putStrLn
    Right () -> putStrLn "Build OK"

  kernel <- clCreateKernel program "anfAND"

  -- Buffers for input and output data.
  -- We request OpenCL to create a buffer on the host (CL_MEM_ALLOC_HOST_PTR)
  -- since we're using CPU. The performance here may not be ideal, because
  -- we're copying the buffer. However, it's safe
  bufIn1 <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR] (nBytes, nullPtr)
  bufIn2 <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR] (nBytes, nullPtr)
  bufOut <- clCreateBuffer context [CL_MEM_WRITE_ONLY, CL_MEM_ALLOC_HOST_PTR] (outNBytes, nullPtr)

  writeVectorToBuffer state bufIn1 inputData
  writeVectorToBuffer state bufIn2 inputData

  clSetKernelArgSto kernel 0 ((fromIntegral nElem) :: CInt)
  clSetKernelArgSto kernel 1 bufIn1
  clSetKernelArgSto kernel 2 ((fromIntegral nElem) :: CInt)
  clSetKernelArgSto kernel 3 bufIn2
  clSetKernelArgSto kernel 4 ((fromIntegral (nElem * nElem)) :: CInt)
  clSetKernelArgSto kernel 5 bufOut
  execEvent  <- clEnqueueNDRangeKernel queue kernel [outNBytes] [] []

  outputData <- bufferToVector queue bufOut (nElem * nElem) [execEvent] :: IO (Vector CUInt)

  putStrLn $ "Output: " ++ show outputData
  mapM_ print $ [ V.slice (nElem * i) nElem outputData | i <- [0 .. nElem - 1] ]

  putStrLn $ "Args: " ++ "size " ++ show (nElem * nElem)
  -- Bitonic
  newVector <- bitonicSortBuffer state bufOut (BufferSize (fromIntegral $ nElem * nElem))
  putStrLn $ "Bitonic: " ++ show newVector

loadSmt :: String -> IO (Either String ([Int], VarsTable String))
loadSmt filename = do
  eSmt <- parseSmtFile filename

  case eSmt of
    Left  err -> return . Left $ unpack . pShow $ err
    Right smt -> do
      let mCanonical =
            fromBase . canonical . foldr B.And B.T . map (canonical . fromProp) . fromSmt $ smt
      case mCanonical of
        Nothing    -> return $ Left "Cannot convert to Canonical data"
        Just cBase -> do
          let vars = L.nub . toList $ cBase
          let vt@(VarsTable (nVars, a2i, i2a)) = VT.varsToInt vars  -- a2i = Map from variables ('a') to integers ('i')
          let v = fromCanonicalBase . fmap (VT.substituteVars a2i) $ cBase
          return $ Right (v, vt)
