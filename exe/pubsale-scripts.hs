{-# LANGUAGE TemplateHaskell #-}

{- | Module     : Main
     Maintainer : riley@summonplatform.io
     Description: Export scripts given configuration.

     Export scripts given configuration.
-}
module Main (main) where

--import Agora.AuthorityToken (AuthorityToken, authorityTokenPolicy)
--import Agora.Governor (Governor (Governor))
--import Agora.Governor qualified as Governor
--import Agora.Governor.Scripts (authorityTokenFromGovernor, authorityTokenSymbolFromGovernor, governorPolicy, governorValidator, proposalFromGovernor, stakeFromGovernor)
--import Agora.Proposal (Proposal)
--import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
--import Agora.SafeMoney (GTTag)
--import Agora.Stake (Stake)
--import Agora.Stake.Scripts (stakePolicy, stakeValidator)
--import Agora.Treasury (treasuryValidator)
import PublicSale.Sale (..)
import PublicSale.Sale.Scripts (..)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Function ((&))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Development.GitRev (gitBranch, gitHash)
import GHC.Generics qualified as GHC
import Plutarch.Api.V1 (mintingPolicySymbol, mkMintingPolicy)
import PlutusLedgerApi.V1 (TxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol)
import PlutusLedgerApi.V1.Value qualified as Value
import ScriptExport.API (runServer)
import ScriptExport.Options (parseOptions)
import ScriptExport.ScriptInfo (ScriptInfo, mkPolicyInfo, mkValidatorInfo)
import ScriptExport.Types (Builders, insertBuilder)

main :: IO ()
main =
  parseOptions >>= runServer revision builders
  where
    -- This encodes the git revision of the server. It's useful for the caller
    -- to be able to ensure they are compatible with it.
    revision :: Text
    revision = $(gitBranch) <> "@" <> $(gitHash)

{- | Builder for Sale script.
-}
builders :: Builders
builders =
  def
    -- Agora scripts
    & insertBuilder "saleValidator" ((.saleValidatorInfo) . saleScripts)
    & insertBuilder "saleStateThread" ((.saleStateThreadInfo) . saleScripts)
    -- Trivial scripts. These are useful for testing, but they likely aren't useful
    -- to you if you are actually interested in deploying to mainnet.
    & insertBuilder
      "alwaysSucceedsPolicy"
      (\() -> mkPolicyInfo $ plam $ \_ _ -> popaque (pconstant ()))
    & insertBuilder
      "alwaysSucceedsValidator"
      (\() -> mkValidatorInfo $ plam $ \_ _ _ -> popaque (pconstant ()))
    & insertBuilder
      "neverSucceedsPolicy"
      (\() -> mkPolicyInfo $ plam $ \_ _ -> perror)
    & insertBuilder
      "neverSucceedsValidator"
      (\() -> mkValidatorInfo $ plam $ \_ _ _ -> perror)

saleScripts :: ScriptParams -> SaleScripts
saleScripts params =
  SaleScripts
    { saleStateThreadInfo = mkPolicyInfo (requestPolicy params.allowList)
    , saleValidatorInfo = mkValidatorInfo (sale)
    }
  where
    stPolicy :: CurrencySymbol
    stPolicy = mintingPolicySymbol $ mkMintingPolicy $ requestPolicy params.allowList

    sale :: Sale
    sale = Sale
      { Sale.saleTokenRef = params.saleTokenRef
      , Sale.validToken = stPolicy
      , Sale.salePrice = params.salePrice
      , Sale.maxTokens = params.maxTokens
      }

data ScriptParams where
  ScriptParams ::
    { saleTokenRef :: AssetClass,
      salePrice    :: Integer,
      maxTokens    :: Integer, -- These aren't even used..
      allowList    :: [AssetClass]
    } ->
    ScriptParams
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, GHC.Generic, Ord)

data SaleScripts = SaleScripts
  { saleStateThreadInfo :: ScriptInfo
  , saleValidatorInfo   :: ScriptInfo
  }
  deriving anyclass
    ( -- | @since 0.2.0
      Aeson.ToJSON
    , -- | @since 0.2.0
      Aeson.FromJSON
    )
  deriving stock
    ( -- | @since 0.2.0
      Show
    , -- | @since 0.2.0
      Eq
    , -- | @since 0.2.0
      GHC.Generic
    )
