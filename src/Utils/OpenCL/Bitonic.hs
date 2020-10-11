{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.OpenCL.Bitonic where

import           Data.Bits

import           Control.Parallel.OpenCL        ( CLDeviceType(..)
                                                , CLMemFlag(..)
                                                , CLError
                                                , CLProgram
                                                , CLMem
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

import           Language.C.Quote.OpenCL        ( cfun )
import           Text.PrettyPrint.Mainland      ( pretty )
import           Text.PrettyPrint.Mainland.Class
                                                ( ppr )
import           Utils.OpenCL                   ( ProgramSource(..) )
import           CLUtil.State                   ( OpenCLState(..) )
import           Control.Parallel.OpenCL.Program
                                                ( CLKernel )
import           Foreign.C.Types                ( CInt )
import           Foreign.C                      ( CUInt )
import           Control.Parallel.OpenCL.Event  ( CLEvent )
import           Data.Vector.Storable           ( Vector )
import           CLUtil.VectorBuffers           ( bufferToVector )

kernelSrcBitonic :: ProgramSource
kernelSrcBitonic = ProgramSource . pretty 100 . ppr $ [cfun|
    kernel void bitonic_sort_kernel(__global int *input_ptr, int stage, int passOfStage) {

    uint threadId = get_global_id(0);
    uint pairDistance = 1 << (stage - passOfStage);
    uint blockWidth = 2 * pairDistance;
    uint temp;
    bool compareResult;
    uint leftId = (threadId & (pairDistance - 1)) + (threadId >> (stage - passOfStage)) * blockWidth;
    uint rightId = leftId + pairDistance;

    int leftElement, rightElement;
    int greater, lesser;
    leftElement = input_ptr[leftId];
    rightElement = input_ptr[rightId];

    uint sameDirectionBlockWidth = threadId >> stage;
    uint sameDirection = sameDirectionBlockWidth & 0x1;

    temp    = sameDirection ? rightId : temp;
    rightId = sameDirection ? leftId : rightId;
    leftId  = sameDirection ? temp : leftId;

    compareResult = (leftElement < rightElement);

    greater = compareResult ? rightElement : leftElement;
    lesser  = compareResult ? leftElement : rightElement;

    input_ptr[leftId] = lesser;
    input_ptr[rightId] = greater;
  }
|]

newtype Stage = Stage { getStage :: Int}
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype PassOfStage = PassOfStage { getPassOfStage :: Int }
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype BufferSize = BufferSize { getBufferSize :: CUInt }
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
{- |
  Recieves a Buffer that we can write in
-}
bitonicSortBuffer :: OpenCLState -> CLMem -> BufferSize -> IO (Vector CUInt)
bitonicSortBuffer state buf (BufferSize size) = do
  let OpenCLState { clDevice = device, clContext = context, clQueue = queue } = state

  program <- clCreateProgramWithSource context (getProgramSource kernelSrcBitonic)
  clBuildProgram program [device] ""
  kernel <- clCreateKernel program "bitonic_sort_kernel"

  let stage      = 0
  let nStages    = numStages size
  let globalSize = [size `shiftR` 1]
  let localSize  = [] :: [CUInt]

  clSetKernelArgSto kernel 0 buf
  events <- mainLoop (Stage 0)
                     (fromIntegral nStages)
                     state
                     []
                     kernel
                     globalSize
                     localSize
                     buf
                     (BufferSize size)
  bufferToVector queue buf (fromIntegral size) events :: IO (Vector CUInt)

mainLoop
  :: Stage       -- Stage step
  -> Int         -- Number of Stages
  -> OpenCLState -- State (queue, context, device)
  -> [CLEvent]   -- List of dependen events
  -> CLKernel    -- Kernel
  -> [CUInt]     -- Global Size
  -> [CUInt]     -- Local Size
  -> CLMem       -- Buffer
  -> BufferSize  -- Buffer's size
  -> IO [CLEvent]
mainLoop (Stage stage) nStages state events kernel globalSize localSize buf bufferSize
  | stage >= nStages = pure events
  | otherwise = do
    clSetKernelArgSto kernel 1 (fromIntegral stage :: CInt)
    newEvents <- innerLoop (Stage stage)
                           (PassOfStage 0)
                           state
                           events
                           kernel
                           globalSize
                           localSize
                           buf
                           bufferSize
    mainLoop (Stage stage + 1)
             nStages
             state
             (events ++ newEvents)
             kernel
             globalSize
             localSize
             buf
             bufferSize

innerLoop
  :: Stage       -- Stage step
  -> PassOfStage -- Passofstage step
  -> OpenCLState -- State (queue, context, device)
  -> [CLEvent]   -- List of dependen events
  -> CLKernel    -- Kernel
  -> [CUInt]     -- Global Size
  -> [CUInt]     -- Local Size
  -> CLMem       -- Buffer
  -> BufferSize  -- Buffer's size
  -> IO [CLEvent]
innerLoop (Stage stage) (PassOfStage passOfStage) state events kernel globalSize localSize buf (BufferSize size)
  | passOfStage > stage
  = pure events
  | otherwise
  = do
    putStrLn $ "Stage " ++ show stage ++ " Pass: " ++ show passOfStage
    clSetKernelArgSto kernel 2 (fromIntegral passOfStage :: CInt)
    event <- clEnqueueNDRangeKernel (clQueue state) kernel globalSize localSize events
    innerLoop (Stage stage)
              (PassOfStage passOfStage + 1)
              state
              (event : events)
              kernel
              globalSize
              localSize
              buf
              (BufferSize size)

numStages :: (Bits a, Integral a) => a -> a
numStages x | x > 1     = 1 + numStages (shiftR x 1)
            | otherwise = 0

bitonicSort' :: OpenCLState -> CLKernel -> CLMem -> BufferSize -> Stage -> PassOfStage -> IO CLMem
bitonicSort' state kernel buf size stage passOfStage = do
  clSetKernelArgSto kernel 0 buf
  clSetKernelArgSto kernel 1 (fromIntegral stage :: CInt)
  clSetKernelArgSto kernel 2 (fromIntegral passOfStage :: CInt)

  execEvent <- clEnqueueNDRangeKernel (clQueue state) kernel [size] [] []

  pure buf
