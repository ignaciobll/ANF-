{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.OpenCL.Bitonic where

import           Data.Bits

import           Control.Parallel.OpenCL        ( CLDeviceType(..)
                                                , CLMemFlag(..)
                                                , CLError
                                                , CLProgram
                                                , CLMem
                                                , CLEvent
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

import           CLUtil                         ( OpenCLState(..)
                                                , getCLMem
                                                )
import           CLUtil.VectorBuffers           ( writeVectorToBuffer
                                                , bufferToVector
                                                , vectorToBuffer
                                                )

import           Control.Parallel.OpenCL.Program
                                                ( CLKernel )

import           Foreign                        ( sizeOf
                                                , nullPtr
                                                )
import           Foreign.C.Types                ( CInt
                                                , CUInt
                                                )
import           Foreign.C                      ( CUInt )

{- General -}

import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON )

import qualified Data.Vector.Storable          as V

{- Locals -}
import           Utils.OpenCL                   ( ProgramSource(..)
                                                , prepareKernel
                                                )
import           Data.Vector.Storable           ( Vector )

import qualified Utils.Log                     as Log
import           Control.Monad                  ( (>=>) )

kernelSrcBitonic :: ProgramSource
kernelSrcBitonic = ProgramSource . pretty 100 . ppr $ [cfun|
    kernel void bitonic_sort_kernel(__global uint *input_ptr, uint stage, uint passOfStage) {

    uint threadId = get_global_id(0);
    uint pairDistance = 1 << (stage - passOfStage);
    uint blockWidth = 2 * pairDistance;
    uint temp;
    bool compareResult;
    uint leftId = (threadId & (pairDistance - 1)) + (threadId >> (stage - passOfStage)) * blockWidth;
    uint rightId = leftId + pairDistance;

    uint leftElement, rightElement;
    uint greater, lesser;
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
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral, ToJSON)

newtype PassOfStage = PassOfStage { getPassOfStage :: Int }
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral, ToJSON)

newtype BufferElems = BufferElems { getBufferElems :: Int }
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral, ToJSON)

data BitonicLog = BitonicLog {
  stage :: Stage,
  passOfStage :: Maybe PassOfStage,
  bufferSize :: BufferElems
  } deriving (Show, Eq, Generic, ToJSON)

bitonicSortVector :: OpenCLState -> CLKernel -> Vector CUInt -> IO (Vector CUInt)
bitonicSortVector state kernel vector =
  let
    logBuffer :: CLMem -> Int -> CLEvent -> IO (CLEvent)
    logBuffer buff nElems evt =
      bufferToVector (clQueue state) buff nElems [evt]
        >>= (print :: Vector CUInt -> IO ())
        >>  pure evt

    nearestPow2 :: (Bits a, Integral a) => a -> Int
    nearestPow2 n = fromIntegral $ if 2 ^ (numStages n) == n then numStages n else 1 + numStages n

    nearestPow2Buff :: Int -> IO CLMem
    nearestPow2Buff nElems = do
      let nBytes = (fromIntegral $ nearestPow2 nElems) * sizeOf (undefined :: CUInt)
      clCreateBuffer (clContext state) [CL_MEM_ALLOC_HOST_PTR] (nBytes, nullPtr)
  in
    do
      let nElems = V.length vector
      buff <- nearestPow2Buff nElems
      writeVectorToBuffer state buff vector
      evts <- bitonicSortBuffer' state kernel buff (BufferElems nElems)
      bufferToVector (clQueue state) buff (nearestPow2 nElems) [last evts] :: IO (Vector CUInt)

{- Buffer must have size 2^n -}
bitonicSortBuffer' :: OpenCLState -> CLKernel -> CLMem -> BufferElems -> IO [CLEvent]
bitonicSortBuffer' state kernel buff (BufferElems nElems) =
  mapM (bitonicSortCore' state kernel buff nElems) (stagePairs nElems)

bitonicSortCore' :: OpenCLState -> CLKernel -> CLMem -> Int -> (Stage, PassOfStage) -> IO (CLEvent)
bitonicSortCore' state kernel buff nElems (Stage stage, PassOfStage pos) = do
  clSetKernelArgSto kernel 0 buff
  clSetKernelArgSto kernel 1 (fromIntegral stage :: CInt)
  clSetKernelArgSto kernel 2 (fromIntegral pos :: CInt)
  clEnqueueNDRangeKernel (clQueue state) kernel [div nElems 2] [] []

prepareBitonic :: OpenCLState -> IO (CLKernel, CLProgram)
prepareBitonic state = prepareKernel state (getProgramSource kernelSrcBitonic) "bitonic_sort"

{- Auxiliary -}

numStages :: (Bits a, Integral a) => a -> a
numStages x | x > 1     = 1 + numStages (shiftR x 1)
            | otherwise = 0

stagePairs :: Int -> [(Stage, PassOfStage)]
stagePairs nElems = passOfStages stages >>= id -- (flatten)
 where
  stages       = [0 .. (numStages nElems) - 1] :: [Int]
  passOfStages = fmap (\stage -> fmap (\pos -> (Stage stage, PassOfStage pos)) [0 .. stage])
