{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Data.Formula.ANF.VectorRepr    ( fromCanonicalBase )
import           Data.Formula.Prop              ( fromSmt )

import           Data.SAT.Smt                   ( parseSmtFile )
import           Data.SAT.DIMACS                ( printParseError )

import qualified Data.List                     as L
import qualified Data.Formula.ANF.Base         as B

import           Utils.VarsTable                ( VarsTable(..) )
import qualified Utils.VarsTable               as VT

import           Utils.OpenCL                   ( autoKernel
                                                , prepareKernel
                                                , ProgramSource(..)
                                                )
import           Utils.OpenCL.Bitonic           ( bitonicSortBuffer
                                                , BufferSize(..)
                                                )

import           Smtlib.Syntax.Syntax           ( Source )
import           Control.Parallel.OpenCL.Program
                                                ( CLKernel )

import           CLSat                          ( kernelAnfAnd )
import           Data.Aeson.Types               ( pairs )


instance ToJSON CUInt where
  toJSON cuint = toJSON (fromIntegral cuint :: Integer)

instance ToJSON CInt where
  toJSON cint = toJSON (fromIntegral cint :: Integer)

data LogStep = LogStep {
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
instance ToJSON LogStep where
  toJSON LogStep {..} = object
    [ "logUsedVars" .= logUsedVars
    , "logNElemsBuff1" .= logNElemsBuff1
    , "logNElemsBuff2" .= logNElemsBuff2
    , "logNElemsBuffOut" .= logNElemsBuffOut
    , "logNElemsReduced" .= logNElemsReduced
    , "logNBytesBuff1" .= logNBytesBuff1
    , "logNBytesBuff2" .= logNBytesBuff2
    , "logNBytesBuffOut" .= logNBytesBuffOut
    , "logNBytesBuffReduced" .= logNBytesBuffReduced
    , "logBuff1" .= logBuff1
    , "logBuff2" .= logBuff2
    , "logBuffOut" .= logBuffOut
    , "logBuffReduced" .= logBuffReduced
    ]

  toEncoding LogStep {..} =
    pairs
      $  "logUsedVars"
      .= logUsedVars
      <> "logNElemsBuff1"
      .= logNElemsBuff1
      <> "logNElemsBuff2"
      .= logNElemsBuff2
      <> "logNElemsBuffOut"
      .= logNElemsBuffOut
      <> "logNElemsReduced"
      .= logNElemsReduced
      <> "logNBytesBuff1"
      .= logNBytesBuff1
      <> "logNBytesBuff2"
      .= logNBytesBuff2
      <> "logNBytesBuffOut"
      .= logNBytesBuffOut
      <> "logNBytesBuffReduced"
      .= logNBytesBuffReduced
      <> "logBuff1"
      .= logBuff1
      <> "logBuff2"
      .= logBuff2
      <> "logBuffOut"
      .= logBuffOut
      <> "logBuffReduced"
      .= logBuffReduced

logger :: ToJSON a => a -> IO ()
logger = BL.putStrLn . encode

main :: IO ()
main = do
  [filename] <- getArgs

  context    <- clCreateContextFromType [] [CL_DEVICE_TYPE_GPU] print
  device     <- head <$> clGetContextDevices context
  queue      <- clCreateCommandQueue context device []

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
      print vt
      result <- satanf handler' (SatanfArgs state kernel (V.fromList [0 :: CUInt]) v vt V.empty)
      print result
      clReleaseContext context
      exitSuccess

type Handler a = a -> IO a

data SatanfArgs = SatanfArgs {
  getState :: OpenCLState,
  getKernel :: CLKernel,
  getAnf :: Vector CUInt,
  getNextAnfs :: [Vector CUInt],
  getVarsTable :: VarsTable String,
  getReduced :: Vector CUInt
  } deriving (Show)


-- viewFormula :: Vector CUInt

handleWithLog :: Handler SatanfArgs
handleWithLog args@(SatanfArgs state kernel anf (v : vs) vt result) = do
  let nElemAcc                     = V.length anf
  let nElemAnd                     = V.length v
  let nElemOut                     = nElemAcc * nElemAnd
  let nBytesAcc = nElemAcc * sizeOf (undefined :: CUInt)
  let nBytesAnd = nElemAnd * sizeOf (undefined :: CUInt)
  let nBytesOut                    = nBytesAcc * nBytesAnd
  let nElemRed                     = V.length result
  let nBytesRed = nElemRed * sizeOf (undefined :: CUInt)

  let (VarsTable (usedVars, _, _)) = vt

  logger $ LogStep nElemAcc
                   nElemAnd
                   nElemOut
                   nElemRed
                   nBytesAcc
                   nBytesAnd
                   nBytesOut
                   nBytesRed
                   (Just anf)
                   (Just v)
                   (Just result)
                   (Just $ reduceVector result)
                   usedVars

  pure args

handleReduceResult :: Handler SatanfArgs
handleReduceResult (SatanfArgs state kernel anf vvs vt result)
  | (reduceVector result) == V.fromList [] = pure
    (SatanfArgs state kernel anf vvs vt (V.fromList [0]))
  | otherwise = pure (SatanfArgs state kernel anf vvs vt (reduceVector result))


handleWithCheck :: Handler SatanfArgs
handleWithCheck args@(SatanfArgs state kernel anf (v : vs) vt result)
  | reduceVector (sequentialAnd anf v) == V.fromList [] && result == V.fromList [0] = pure args
  | reduceVector (sequentialAnd anf v) == result = pure args
  | otherwise = do
    putStrLn $ "Debug: " ++ show result
    hspec $ describe "Handler Check:" $ do
      it "result should be the same sequentially and in OpenCL" $ checkSequential anf v result
    pure args

nextSatanfArgs :: SatanfArgs -> IO SatanfArgs
nextSatanfArgs (SatanfArgs state kernel anf (v : vs) vt result) =
  pure (SatanfArgs state kernel result vs vt result)

satanf :: Handler SatanfArgs -> SatanfArgs -> IO (Vector CUInt)
satanf handler (SatanfArgs _     kernel _   []       _  result) = pure result
satanf handler (SatanfArgs state kernel anf (v : vs) vt _     ) = do

  let (OpenCLState device context queue) = state
  let nElemAcc                           = V.length anf
  let nElemAnd                           = V.length v
  let nElemOut                           = nElemAcc * nElemAnd
  let nBytesAcc = nElemAcc * sizeOf (undefined :: CUInt)
  let nBytesAnd = nElemAnd * sizeOf (undefined :: CUInt)
  let nBytesOut                          = nBytesAcc * nBytesAnd

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
  execEvent          <- clEnqueueNDRangeKernel queue kernel [nBytesOut] [] []

  andVector          <- bufferToVector queue bufOut nElemOut [execEvent] :: IO (Vector CUInt)

  eventReleasebufIn1 <- clReleaseMemObject bufIn1
  eventReleasebufIn2 <- clReleaseMemObject bufIn2
  eventReleasebufOut <- clReleaseMemObject bufOut

  let result = andVector -- V.concat [andVector, anf, v]

  args <- handler (SatanfArgs state kernel anf (v : vs) vt result)
  satanf handler args

loadSmt :: Source -> [(Vector CUInt, VarsTable String)]
loadSmt smt =
  foldr (\smt acc@((_, vt) : _) -> (smt2AnfVec vt smt) : acc) [(V.fromList [0], VT.emptyVarsTable)]
    $ fmap (: []) smt

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
