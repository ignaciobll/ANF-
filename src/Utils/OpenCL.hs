{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Utils.OpenCL where

import           Control.Parallel.OpenCL        ( CLDeviceType(..)
                                                , CLMemFlag(..)
                                                , CLError
                                                , CLProgram
                                                , CLDeviceID
                                                , CLMem
                                                , CLKernel
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
import           Foreign.Storable               ( Storable )

import           Foreign.C.Types                ( CUInt )


import           CLUtil                         ( OpenCLState(..)
                                                , clContext
                                                , clDevice
                                                , clQueue
                                                )
import           Control.Exception              ( throw
                                                , try
                                                )

newtype ProgramSource = ProgramSource { getProgramSource :: String }


setupCL :: IO OpenCLState
setupCL = do
  context <- clCreateContextFromType [] [CL_DEVICE_TYPE_GPU] print
  device  <- head <$> clGetContextDevices context
  queue   <- clCreateCommandQueue context device []
  pure $ OpenCLState { clDevice = device, clContext = context, clQueue = queue }

loadProgram :: String -> IO ProgramSource
loadProgram filename = ProgramSource <$> readFile filename

createProgram :: CLContext -> ProgramSource -> IO CLProgram
createProgram context (ProgramSource source) = clCreateProgramWithSource context source

createKernel :: String -> CLProgram -> IO CLKernel
createKernel name program = clCreateKernel program name

autoKernel :: OpenCLState -> ProgramSource -> String -> IO CLKernel
autoKernel state source kname = do
  result <-
    (try $ createProgram (clContext state) source >>= createKernel kname) :: IO
      (Either CLError CLKernel)
  case result of
    Left clErr -> do
      program <- createProgram (clContext state) source
      log     <- clGetProgramBuildLog program (clDevice state)
      putStrLn "--- BEGIN LOG ---"
      putStrLn log
      putStrLn "---  END LOG  ---"
      throw clErr
    Right kernel -> pure kernel

prepareKernel :: OpenCLState -> String -> String -> IO (CLKernel, CLProgram)
prepareKernel state source kernelName = do
  program            <- clCreateProgramWithSource (clContext state) source
  buildProgramStatus <-
    (try $ clBuildProgram program [(clDevice state)] "") :: IO (Either CLError ())

  case buildProgramStatus of
    Left clErr -> do
      putStrLn . show $ clErr
      clGetProgramBuildLog program (clDevice state) >>= putStrLn
      throw clErr
    Right () -> do
      kernel <- clCreateKernel program kernelName
      pure $ (kernel, program)
