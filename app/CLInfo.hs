module CLInfo where

import           Control.Parallel.OpenCL.Query  ( clGetPlatformIDs
                                                , clGetDeviceIDs
                                                , clGetPlatformInfo
                                                , clGetDeviceExecutionCapabilities
                                                , clGetDeviceAddressBits
                                                , clGetDeviceAvailable
                                                , clGetDeviceCompilerAvailable
                                                , clGetDeviceEndianLittle
                                                , clGetDeviceErrorCorrectionSupport
                                                , clGetDeviceExtensions
                                                , clGetDeviceGlobalMemCacheSize
                                                , clGetDeviceGlobalMemCachelineSize
                                                , clGetDeviceGlobalMemSize
                                                , clGetDeviceImageSupport
                                                , clGetDeviceImage2DMaxHeight
                                                , clGetDeviceImage2DMaxWidth
                                                , clGetDeviceImage3DMaxDepth
                                                , clGetDeviceImage3DMaxHeight
                                                , clGetDeviceImage3DMaxWidth
                                                , clGetDeviceLocalMemSize
                                                , clGetDeviceMaxClockFrequency
                                                , clGetDeviceMaxComputeUnits
                                                , clGetDeviceMaxConstantArgs
                                                , clGetDeviceMaxConstantBufferSize
                                                , clGetDeviceMaxMemAllocSize
                                                , clGetDeviceMaxParameterSize
                                                , clGetDeviceMaxReadImageArgs
                                                , clGetDeviceMaxSamplers
                                                , clGetDeviceMaxWorkGroupSize
                                                , clGetDeviceMaxWorkItemDimensions
                                                , clGetDeviceMaxWorkItemSizes
                                                , clGetDeviceMaxWriteImageArgs
                                                , clGetDeviceMemBaseAddrAlign
                                                , clGetDeviceMinDataTypeAlignSize
                                                , clGetDeviceName
                                                , clGetDevicePlatform
                                                , clGetDevicePreferredVectorWidthChar
                                                , clGetDevicePreferredVectorWidthShort
                                                , clGetDevicePreferredVectorWidthInt
                                                , clGetDevicePreferredVectorWidthLong
                                                , clGetDevicePreferredVectorWidthFloat
                                                , clGetDevicePreferredVectorWidthDouble
                                                , clGetDeviceProfile
                                                , clGetDeviceProfilingTimerResolution
                                                , clGetDeviceVendor
                                                , clGetDeviceVendorID
                                                , clGetDeviceVersion
                                                , clGetDeviceDriverVersion
                                                , clGetDeviceSingleFPConfig
                                                , clGetDeviceDoubleFPConfig
                                                , clGetDeviceHalfFPConfig
                                                , clGetDeviceLocalMemType
                                                , clGetDeviceGlobalMemCacheType
                                                , clGetDeviceQueueProperties
                                                , clGetDeviceType
                                                )

import           Foreign.C.Types                ( CSize )

import           Control.Parallel.OpenCL        ( CLPlatformID
                                                , CLDeviceID
                                                , CLPlatformInfo(..)
                                                , CLDeviceExecCapability
                                                , CLuint
                                                , CLulong
                                                , CLPlatformID
                                                , CLDeviceFPConfig
                                                , CLDeviceLocalMemType
                                                , CLDeviceMemCacheType
                                                , CLCommandQueueProperty
                                                , CLDeviceType(..)
                                                )

import           Control.Monad                  ( forM )

data PlatformInfo = PlatformInfo
  { cl_platform_profile   :: String
  , cl_platform_version   :: String
  , cl_platform_name      :: String
  , cl_platform_extension :: String
  , cl_devices            :: [DeviceInfo]
  } deriving Show


main :: IO ()
main = do
  putStrLn "SatANF - clinfo"
  platformIDs <- clGetPlatformIDs :: IO [CLPlatformID]
  fullInfo    <- forM platformIDs $ \platformId -> do
    platformInfo <- getPlatformInfo platformId :: IO PlatformInfo
    deviceIDs    <- clGetDeviceIDs platformId CL_DEVICE_TYPE_ALL :: IO [CLDeviceID]
    devicesInfo  <- forM deviceIDs $ \deviceId -> getDeviceInfo deviceId
    let (PlatformInfo profile version name extensions _) = platformInfo
    return $ (PlatformInfo profile version name extensions devicesInfo)
  print fullInfo

getPlatformInfo :: CLPlatformID -> IO PlatformInfo
getPlatformInfo platform =
  PlatformInfo
    <$> clGetPlatformInfo platform CL_PLATFORM_PROFILE
    <*> clGetPlatformInfo platform CL_PLATFORM_VERSION
    <*> clGetPlatformInfo platform CL_PLATFORM_NAME
    <*> clGetPlatformInfo platform CL_PLATFORM_EXTENSIONS
    <*> pure []

data DeviceInfo = DeviceInfo
  { cl_device_ExecutionCapabilities :: [CLDeviceExecCapability]
  , cl_device_AddressBits :: CLuint
  , cl_device_Available :: Bool
  , cl_device_CompilerAvailable :: Bool
  , cl_device_EndianLittle :: Bool
  , cl_device_ErrorCorrectionSupport :: Bool
  , cl_device_Extensions :: String
  , cl_device_GlobalMemCacheSize :: CLulong
  , cl_device_GlobalMemCachelineSize :: CLuint
  , cl_device_GlobalMemSize :: CLulong
  , cl_device_ImageSupport :: Bool
  , cl_device_Image2DMaxHeight :: CSize
  , cl_device_Image2DMaxWidth :: CSize
  , cl_device_Image3DMaxDepth :: CSize
  , cl_device_Image3DMaxHeight :: CSize
  , cl_device_Image3DMaxWidth :: CSize
  , cl_device_LocalMemSize :: CLulong
  , cl_device_MaxClockFrequency :: CLuint
  , cl_device_MaxComputeUnits :: CLuint
  , cl_device_MaxConstantArgs :: CLuint
  , cl_device_MaxConstantBufferSize :: CLulong
  , cl_device_MaxMemAllocSize :: CLulong
  , cl_device_MaxParameterSize :: CSize
  , cl_device_MaxReadImageArgs :: CLuint
  , cl_device_MaxSamplers :: CLuint
  , cl_device_MaxWorkGroupSize :: CSize
  , cl_device_MaxWorkItemDimensions :: CLuint
  , cl_device_MaxWorkItemSizes :: [CSize]
  , cl_device_MaxWriteImageArgs :: CLuint
  , cl_device_MemBaseAddrAlign :: CLuint
  , cl_device_MinDataTypeAlignSize :: CLuint
  , cl_device_Name :: String
  , cl_device_Platform :: CLPlatformID
  , cl_device_PreferredVectorWidthChar :: CLuint
  , cl_device_PreferredVectorWidthShort :: CLuint
  , cl_device_PreferredVectorWidthInt :: CLuint
  , cl_device_PreferredVectorWidthLong :: CLuint
  , cl_device_PreferredVectorWidthFloat :: CLuint
  , cl_device_PreferredVectorWidthDouble :: CLuint
  , cl_device_Profile :: String
  , cl_device_ProfilingTimerResolution :: CSize
  , cl_device_Vendor :: String
  , cl_device_VendorID :: CLuint
  , cl_device_Version :: String
  , cl_device_DriverVersion :: String
  , cl_device_SingleFPConfig :: [CLDeviceFPConfig]
  , cl_device_DoubleFPConfig :: [CLDeviceFPConfig]
  , cl_device_HalfFPConfig :: [CLDeviceFPConfig]
  , cl_device_LocalMemType :: CLDeviceLocalMemType
  , cl_device_GlobalMemCacheType :: CLDeviceMemCacheType
  , cl_device_QueueProperties :: [CLCommandQueueProperty]
  , cl_device_Type :: [CLDeviceType]
  } deriving Show

getDeviceInfo :: CLDeviceID -> IO DeviceInfo
getDeviceInfo device =
  DeviceInfo
    <$> clGetDeviceExecutionCapabilities device
    <*> clGetDeviceAddressBits device
    <*> clGetDeviceAvailable device
    <*> clGetDeviceCompilerAvailable device
    <*> clGetDeviceEndianLittle device
    <*> clGetDeviceErrorCorrectionSupport device
    <*> clGetDeviceExtensions device
    <*> clGetDeviceGlobalMemCacheSize device
    <*> clGetDeviceGlobalMemCachelineSize device
    <*> clGetDeviceGlobalMemSize device
    <*> clGetDeviceImageSupport device
    <*> clGetDeviceImage2DMaxHeight device
    <*> clGetDeviceImage2DMaxWidth device
    <*> clGetDeviceImage3DMaxDepth device
    <*> clGetDeviceImage3DMaxHeight device
    <*> clGetDeviceImage3DMaxWidth device
    <*> clGetDeviceLocalMemSize device
    <*> clGetDeviceMaxClockFrequency device
    <*> clGetDeviceMaxComputeUnits device
    <*> clGetDeviceMaxConstantArgs device
    <*> clGetDeviceMaxConstantBufferSize device
    <*> clGetDeviceMaxMemAllocSize device
    <*> clGetDeviceMaxParameterSize device
    <*> clGetDeviceMaxReadImageArgs device
    <*> clGetDeviceMaxSamplers device
    <*> clGetDeviceMaxWorkGroupSize device
    <*> clGetDeviceMaxWorkItemDimensions device
    <*> clGetDeviceMaxWorkItemSizes device
    <*> clGetDeviceMaxWriteImageArgs device
    <*> clGetDeviceMemBaseAddrAlign device
    <*> clGetDeviceMinDataTypeAlignSize device
    <*> clGetDeviceName device
    <*> clGetDevicePlatform device
    <*> clGetDevicePreferredVectorWidthChar device
    <*> clGetDevicePreferredVectorWidthShort device
    <*> clGetDevicePreferredVectorWidthInt device
    <*> clGetDevicePreferredVectorWidthLong device
    <*> clGetDevicePreferredVectorWidthFloat device
    <*> clGetDevicePreferredVectorWidthDouble device
    <*> clGetDeviceProfile device
    <*> clGetDeviceProfilingTimerResolution device
    <*> clGetDeviceVendor device
    <*> clGetDeviceVendorID device
    <*> clGetDeviceVersion device
    <*> clGetDeviceDriverVersion device
    <*> clGetDeviceSingleFPConfig device
    <*> clGetDeviceDoubleFPConfig device
    <*> clGetDeviceHalfFPConfig device
    <*> clGetDeviceLocalMemType device
    <*> clGetDeviceGlobalMemCacheType device
    <*> clGetDeviceQueueProperties device
    <*> clGetDeviceType device
