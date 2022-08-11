
{-# LANGUAGE OverloadedStrings   #-}
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB

import           Ledger                     (datumHash, scriptHashAddress)


import           PubSale

import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Api
import           Data.String                         (IsString (..))
import           Data.Aeson
import           GHC.Num (encodeDoubleInteger)

whitelist :: [AssetClass]
whitelist = [AssetClass ("29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6", "4d494e74"), AssetClass ("29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6", "4d494e")]

daSale :: CurrencySymbol -> Sale
daSale cur =
  Sale {
    saleTokenRef = AssetClass ("126b8676446c84a5cd6e3259223b16a2314c5676b88ae1c1f8579a8f", "744d454c44"),
    validToken = cur,
    salePrice = 50000,
    maxTokens = 250000
  }

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 0 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "script-dump"
  else 
    do 
      let
        scriptnum = 42
        validatorname = "validator.plutus"
        appliedSTScript = policy whitelist
        appliedValidatorScript = saleValidatorScript (daSale $ curSymbol whitelist)

        validatorAsCbor = serialise appliedValidatorScript
        validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
        validatorScript = PlutusScriptSerialised validatorShortBs

        stCbor = serialise appliedSTScript
        stShortBs = SBS.toShort . LB.toStrict $ stCbor
        stScript = PlutusScriptSerialised stShortBs

      putStrLn $ "Writing output to: " ++ validatorname
      writePlutusScript' scriptnum validatorname validatorScript validatorShortBs

      writePlutusScript' scriptnum "st.plutus" stScript stShortBs

writePlutusScript' :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript' scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
