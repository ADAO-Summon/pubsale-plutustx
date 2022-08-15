
{-# LANGUAGE OverloadedStrings   #-}
import           Prelude
import           System.Environment

import           Cardano.Api hiding (Address)
import           Cardano.Api.Shelley hiding (Address)
import           Codec.Serialise hiding (encode)

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB

import           Ledger                     (datumHash, scriptHashAddress)


import           PubSale

import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Api
import           Data.String                         (IsString (..))
import           Data.Aeson (encode)
import           GHC.Num (encodeDoubleInteger)

whiteListCur1 :: CurrencySymbol
whiteListCur1 = fromString "5b01968867e13432afaa2f814e1d15e332d6cd0aa77e350972b0967d"
whiteListCur2 :: CurrencySymbol
whiteListCur2 = fromString "29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6"
whiteListCur3 :: CurrencySymbol
whiteListCur3 = fromString "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24"

whiteListName1 :: TokenName
whiteListName1 = fromString "ADAOGovernanceToken"
whiteListName2 :: TokenName
whiteListName2 = fromString "MIN"
whiteListName3 :: TokenName
whiteListName3 = fromString "LQ"

whitelist :: [AssetClass]
whitelist = [(assetClass whiteListCur1 whiteListName1), (assetClass whiteListCur2 whiteListName2), (assetClass whiteListCur3 whiteListName3)]

saleRefToken :: CurrencySymbol
saleRefToken = fromString "9ffd99dd2354da45cf46ccb2097098dcfeaade7eb9fdbfe5aa9a52d2"

saleRefName :: TokenName
saleRefName = fromString "tSummon"

daSale :: CurrencySymbol -> Sale
daSale cur =
  Sale {
    saleTokenRef = (assetClass saleRefToken saleRefName),
    validToken = cur,
    salePrice = LovelacePerToken 50000,
    maxTokens = 250000000000
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

address :: PubSaleDatum
address = RequestDatum (Address (PubKeyCredential "deadbeef") (Just $ (StakingHash (PubKeyCredential "beefbeef"))))

writePlutusScript' :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript' scriptnum filename scriptSerial scriptSBS =
  do 
    print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData address))
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
