{-# LANGUAGE QuasiQuotes #-}

module CLVectorSample where

import           Control.Parallel.OpenCL        ( CLDeviceType(..)
                                                , CLMemFlag(..)
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
import           Foreign.C.Types                ( CFloat )

import           Language.C.Quote.OpenCL        ( cfun )

import           CLUtil.VectorBuffers           ( writeVectorToBuffer
                                                , bufferToVector
                                                )
import           CLUtil                         ( OpenCLState(OpenCLState)
                                                , clContext
                                                , clDevice
                                                , clQueue
                                                )

import           Text.PrettyPrint.Mainland      ( prettyCompact )
import           Text.PrettyPrint.Mainland.Class
                                                ( ppr )

kernelSource :: String
kernelSource = prettyCompact . ppr $ [cfun|
    /* This example kernel just does `map (*2)` */
    kernel void doubleArray(
        global float *in,
        global float *out
    ) {
        int i = get_global_id(0);
        out[i] = 2 * in[i];
    }
|]

main :: IO ()
main = do
  context <- clCreateContextFromType [] [CL_DEVICE_TYPE_GPU] print
  device  <- head <$> clGetContextDevices context
  queue   <- clCreateCommandQueue context device []

  let state = OpenCLState { clDevice = device, clContext = context, clQueue = queue }

  program <- clCreateProgramWithSource context kernelSource
  clBuildProgram program [device] ""
  kernel <- clCreateKernel program "doubleArray"

  let inputData = V.fromList [(-4) .. 4] :: Vector CFloat
  let nElem     = V.length inputData
  let nBytes    = nElem * sizeOf (undefined :: CFloat)

  -- Buffers for input and output data.
  -- We request OpenCL to create a buffer on the host (CL_MEM_ALLOC_HOST_PTR)
  -- since we're using CPU. The performance here may not be ideal, because
  -- we're copying the buffer. However, it's safe
  bufIn  <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR] (nBytes, nullPtr)
  bufOut <- clCreateBuffer context [CL_MEM_WRITE_ONLY, CL_MEM_ALLOC_HOST_PTR] (nBytes, nullPtr)

  writeVectorToBuffer state bufIn inputData

  clSetKernelArgSto kernel 0 bufIn
  clSetKernelArgSto kernel 1 bufOut
  execEvent  <- clEnqueueNDRangeKernel queue kernel [nElem] [] []

  outputData <- bufferToVector queue bufOut nElem [execEvent] :: IO (Vector CFloat)

  release    <- clReleaseContext context

  print outputData
