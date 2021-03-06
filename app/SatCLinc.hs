{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module SatCLinc where

import           Test.Hspec                     ( hspec
                                                , shouldBe
                                                , describe
                                                , it
                                                )

import           Control.Parallel.OpenCL        ( CLDeviceType(..)
                                                , CLMemFlag(..)
                                                , CLError
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
                                                -- Memory
                                                , clReleaseMemObject
                                                , clCreateBuffer
                                                -- Event
                                                , clWaitForEvents
                                                , clReleaseEvent
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

import           CLUtil.VectorBuffers           ( writeVectorToBuffer
                                                , bufferToVector
                                                , vectorToBuffer
                                                )
import           CLUtil                         ( OpenCLState(..)
                                                , clContext
                                                , clDevice
                                                , clQueue
                                                , getCLMem
                                                )
{- General imports -}

import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitSuccess
                                                , exitFailure
                                                )

import           GHC.Generics
import           Data.Aeson                     ( ToJSON(..)
                                                , encode
                                                , object
                                                , (.=)
                                                )
import           Control.Monad                  ( (>=>) )
import           Control.Exception              ( try )

import           Data.Foldable                  ( toList )

import           Data.Text.Lazy                 ( unpack )


import           Text.Pretty.Simple             ( pShow
                                                , pPrint
                                                )

import           Data.Bits                      ( (.|.) )
import qualified Data.Bits                     as Bits

import qualified Data.ByteString.Lazy.Char8    as BL

{- Local Imports -}
import           Data.Formula.ANF.Base          ( canonical
                                                , fromProp
                                                )
import           Data.Formula.ANF.CanonicalBase ( fromBase
                                                , fromBase'
                                                )
import           Data.Formula.ANF.VectorRepr    ( toCanonicalBase
                                                , fromCanonicalBase
                                                )
import           Data.Formula.Prop              ( fromSmt )

import           Data.SAT.Smt                   ( parseSmtFile )
import           Data.SAT.DIMACS                ( printParseError )

import qualified Data.List                     as L
import qualified Data.Formula.ANF.Base         as B

import           Utils.VarsTable                ( restoreVars'
                                                , VarsTable(..)
                                                )
import qualified Utils.VarsTable               as VT

import           Utils.OpenCL                   ( autoKernel
                                                , prepareKernel
                                                , ProgramSource(..)
                                                )

import           Utils.Export.Spreadsheet       ( exportStringVarsTable )

import           Smtlib.Syntax.Syntax           ( Source )
import           Control.Parallel.OpenCL.Program
                                                ( CLKernel )

import           CLSat                          ( kernelAnfAnd )
import           Data.Aeson.Types               ( pairs )
import           Data.Time.Clock.POSIX          ( getPOSIXTime
                                                , POSIXTime
                                                )
import           Data.Formula.ANF.VectorRepr    ( toExpandedVars )
import qualified Utils.Log                     as Log


instance ToJSON CUInt where
  toJSON cuint = toJSON (fromIntegral cuint :: Integer)

instance ToJSON CInt where
  toJSON cint = toJSON (fromIntegral cint :: Integer)

data LogType = Step | CopyOpenCLMem | OpenCLBench | SequentialBench | OpenCLBitonic
  deriving (Eq, Show, Generic, ToJSON)

data LogBenchTick = Start | End deriving (Show, Eq, Ord, Generic, ToJSON)

data LogBench = LogBench {
  stepBench :: Int,
  tick :: LogBenchTick
  } deriving (Show, Eq, Generic, ToJSON)

data LogStep = LogStep {
  stepN :: Int,
  logNElemsBuff1 :: Int,  -- ^ logNElemsBuff1
  logNElemsBuff2 :: Int,  -- ^ logNElemsBuff2
  logNElemsBuffOut :: Int,  -- ^ logNElemsBuffOut
  logNElemsReduced :: Int,  -- ^ logNElemsReduced
  logNBytesBuff1 :: Int,  -- ^ logNBytesBuff1
  logNBytesBuff2 :: Int,  -- ^ logNBytesBuff2
  logNBytesBuffOut :: Int,  -- ^ logNBytesBuffOut
  logNBytesBuffReduced :: Int,  -- ^ logNBytesBuffReduced
  logBuff1 :: Maybe (Vector CUInt),  -- ^ logBuff1
  logBuff2 :: Maybe (Vector CUInt),  -- ^ logBuff2
  logBuffOut :: Maybe (Vector CUInt),  -- ^ logBuffOut
  logBuffReduced :: Maybe (Vector CUInt),  -- ^ logBuffReduced
  logUsedVars :: Int
  } deriving (Eq, Show, Generic)

instance Show OpenCLState
instance ToJSON LogStep

logger :: ToJSON a => a -> IO ()
logger = BL.putStrLn . encode

main :: IO ()
main = do
  [filename]              <- getArgs
  (state, handler', args) <- prepareContextAndFile filename
  result                  <- satanf handler' args

  case V.length result of
    0         -> putStrLn "UNSAT"
    otherwise -> putStrLn $ "Formula with " ++ (show . V.length) result ++ " elems."

  clReleaseContext (clContext state)
  exitSuccess

prepareContextAndFile :: String -> IO (OpenCLState, Handler SatanfArgs, SatanfArgs)
prepareContextAndFile filename = do
  context <- clCreateContextFromType [] [CL_DEVICE_TYPE_GPU] print
  device  <- head <$> clGetContextDevices context
  queue   <- clCreateCommandQueue context device []

  let state = OpenCLState { clDevice = device, clContext = context, clQueue = queue }

  (kernel, program) <- prepareKernel state kernelAnfAnd "anfAND"

  eSource           <- parseSmtFile filename
  case eSource of
    Left err -> hPutStrLn stderr (unpack . pShow $ err) >> clReleaseContext context >> exitFailure
    Right source -> do
      let vectorsAndTable = loadSmt source
      let v               = map fst vectorsAndTable
      let vt              = head . map snd $ vectorsAndTable
      let handler' = handleReduceResult >=> handleWithLog >=> nextSatanfArgs
      let step            = SatanfStep 0
      let args = (SatanfArgs step state kernel (V.fromList [0 :: CUInt]) v vt V.empty)
      pure $ (state, handler', args)

type Handler a = a -> IO a

newtype SatanfStep = SatanfStep { getSatanfStep :: Int }
  deriving (Show, Eq)
  deriving newtype (Enum, ToJSON)

data SatanfArgs = SatanfArgs {
  stepSatanf :: SatanfStep,
  getState :: OpenCLState,
  getKernel :: CLKernel,
  getAnf :: Vector CUInt,
  getNextAnfs :: [Vector CUInt],
  getVarsTable :: VarsTable String,
  getReduced :: Vector CUInt
  } deriving (Show)


-- viewFormula :: Vector CUInt

handleWithLog :: Handler SatanfArgs
handleWithLog args@(SatanfArgs step state kernel anf (v : vs) vt result) = do
  let nElemAcc                     = V.length anf
  let nElemAnd                     = V.length v
  let nElemOut                     = nElemAcc * nElemAnd
  let nBytesAcc = nElemAcc * sizeOf (undefined :: CUInt)
  let nBytesAnd = nElemAnd * sizeOf (undefined :: CUInt)
  let nBytesOut = nElemOut * sizeOf (undefined :: CUInt)
  let nElemRed                     = V.length result
  let nBytesRed = nElemRed * sizeOf (undefined :: CUInt)

  let (VarsTable (usedVars, _, _)) = vt

  posixTime <- (round . (* 1000)) <$> getPOSIXTime
  Log.log Step $ LogStep (getSatanfStep step)
                         nElemAcc
                         nElemAnd
                         nElemOut
                         nElemRed
                         nBytesAcc
                         nBytesAnd
                         nBytesOut
                         nBytesRed
                         Nothing -- (Just anf)
                         Nothing -- (Just v)
                         Nothing -- (Just result)
                         Nothing -- (Just $ reduceVector result)
                         usedVars

  pure args

handleReduceResult :: Handler SatanfArgs
handleReduceResult (SatanfArgs step state kernel anf vvs vt result) = do
  Log.log SequentialBench (LogBench (getSatanfStep step) Start)
  let reduced = (reduceVector result) == V.fromList []
  case reduced of
    False -> do
      Log.log SequentialBench (LogBench (getSatanfStep step) End)
      pure (SatanfArgs step state kernel anf vvs vt (reduceVector result))
    otherwise -> do --UNSAT
      Log.log SequentialBench (LogBench (getSatanfStep step) End)
      print $ head vvs
      pure (SatanfArgs step state kernel anf vvs vt (V.fromList []))

handleWithCheck :: Handler SatanfArgs
handleWithCheck args@(SatanfArgs _ state kernel anf (v : vs) vt result)
  | reduceVector (sequentialAnd anf v) == V.fromList [] && result == V.fromList [0] = pure args
  | reduceVector (sequentialAnd anf v) == result = pure args
  | otherwise = do
    putStrLn $ "Debug: " ++ show result
    hspec $ describe "Handler Check:" $ do
      it "result should be the same sequentially and in OpenCL" $ checkSequential anf v result
    pure args

nextSatanfArgs :: SatanfArgs -> IO SatanfArgs
nextSatanfArgs (SatanfArgs step state kernel anf (v : vs) vt result)
  | result == V.fromList [] = pure (SatanfArgs (succ step) state kernel result [] vt result)
  | otherwise               = pure (SatanfArgs (succ step) state kernel result vs vt result)

satanf :: Handler SatanfArgs -> SatanfArgs -> IO (Vector CUInt)
satanf handler (SatanfArgs step _     kernel _   []       _  result) = pure result
satanf handler (SatanfArgs step state kernel anf (v : vs) vt _     ) = do

  let (OpenCLState device context queue) = state
  let nElemAcc                           = V.length anf
  let nElemAnd                           = V.length v
  let nElemOut                           = nElemAcc * nElemAnd
  let nBytesAcc = nElemAcc * sizeOf (undefined :: CUInt)
  let nBytesAnd = nElemAnd * sizeOf (undefined :: CUInt)
  let nBytesOut = nElemOut * sizeOf (undefined :: CUInt)

  -- Buffers for input and output data.
  -- We request OpenCL to create a buffer on the host (CL_MEM_ALLOC_HOST_PTR)
  -- since we're using CPU. The performance here may not be ideal, because
  -- we're copying the buffer. However, it's safe
  bufIn1 <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR] (nBytesAcc, nullPtr)
  bufIn2 <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR] (nBytesAnd, nullPtr)
  bufOut <- clCreateBuffer context [CL_MEM_WRITE_ONLY, CL_MEM_ALLOC_HOST_PTR] (nBytesOut, nullPtr)

  writeVectorToBuffer state bufIn1 anf -- Base ANF
  writeVectorToBuffer state bufIn2 v   -- And ANF

  -- bufIn1 <- getCLMem <$> vectorToBuffer context anf
  -- bufIn2 <- getCLMem <$> vectorToBuffer context v
  -- bufOut <- clCreateBuffer context [CL_MEM_WRITE_ONLY, CL_MEM_ALLOC_HOST_PTR] (nBytesOut, nullPtr)

  clSetKernelArgSto kernel 0 ((fromIntegral nElemAcc) :: CInt)
  clSetKernelArgSto kernel 1 bufIn1
  clSetKernelArgSto kernel 2 ((fromIntegral nElemAnd) :: CInt)
  clSetKernelArgSto kernel 3 bufIn2
  clSetKernelArgSto kernel 4 ((fromIntegral nElemOut) :: CInt)
  clSetKernelArgSto kernel 5 bufOut

  Log.log OpenCLBench (LogBench (getSatanfStep step) Start)
  execEvent <- clEnqueueNDRangeKernel queue kernel [nBytesOut] [] []
  _         <- clWaitForEvents [execEvent]
  Log.log OpenCLBench (LogBench (getSatanfStep step) End)

  Log.log CopyOpenCLMem (LogBench (getSatanfStep step) Start)
  andVector <- bufferToVector queue bufOut nElemOut [execEvent] :: IO (Vector CUInt)
  Log.log CopyOpenCLMem (LogBench (getSatanfStep step) End)

  eventReleasebufIn1 <- clReleaseMemObject bufIn1
  eventReleasebufIn2 <- clReleaseMemObject bufIn2
  eventReleasebufOut <- clReleaseMemObject bufOut

  let result = andVector -- V.concat [andVector, anf, v]

  args <- handler (SatanfArgs step state kernel anf (v : vs) vt result)
  satanf handler args

loadSmt' :: VarsTable String -> Source -> [(Vector CUInt, VarsTable String)]
loadSmt' vt smt =
  foldr (\smt acc@((_, vt) : _) -> (smt2AnfVec vt smt) : acc) [(V.fromList [0], vt)]
    $ fmap (: []) smt

loadSmt :: Source -> [(Vector CUInt, VarsTable String)]
loadSmt = loadSmt' VT.emptyVarsTable

smt2AnfVec :: VarsTable String -> Source -> (Vector CUInt, VarsTable String)
smt2AnfVec vt smt =
  let cANF = fromBase' . canonical . foldr B.And B.T . map (canonical . fromProp) . fromSmt $ smt
      vt'  = VT.varsToIntWith vt cANF
      vector =
          V.fromList
            . fromCanonicalBase
            . fmap (fromIntegral :: Integral a => a -> CUInt)
            . VT.replaceVars vt'
            $ cANF
  in  (vector, vt')

reduceVector :: Vector CUInt -> Vector CUInt
reduceVector = V.fromList . map head . L.filter (odd . length) . L.group . L.sort . V.toList

sequentialAnd :: Vector CUInt -> Vector CUInt -> Vector CUInt
sequentialAnd acc and =
  let lacc = V.toList acc
      land = V.toList and
  in  V.fromList [ (a .|. b) | a <- land, b <- lacc ]

checkSequential :: Vector CUInt -> Vector CUInt -> Vector CUInt -> IO ()
checkSequential anf add result = result `shouldBe` reduceVector (sequentialAnd anf add)
