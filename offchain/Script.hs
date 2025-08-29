module Script where

import Contract.Bounty
import Contract.Bridge
import Contract.MintingPolicy
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types

skyMintingPolicy' :: SkyMintingParams -> GYScript 'PlutusV3
skyMintingPolicy' smp = mintingPolicyFromPlutus $ skyMintingPolicyScript smp

skyMintingAddress :: (HasCallStack, GYTxQueryMonad m) => SkyMintingParams -> m GYAddress
skyMintingAddress smp = scriptAddress $ skyMintingPolicy' smp

bridgeValidator' :: BridgeParams -> GYScript 'PlutusV3
bridgeValidator' bp = scriptFromPlutus $ bridgeValidatorScript bp

bridgeValidatorAddress :: (HasCallStack, GYTxQueryMonad m) => BridgeParams -> m GYAddress
bridgeValidatorAddress bp = scriptAddress $ bridgeValidator' bp

bountyValidator' :: BountyParams -> GYScript 'PlutusV3
bountyValidator' cp = scriptFromPlutus $ clientValidatorScript cp

bountyValidatorAddress :: (HasCallStack, GYTxQueryMonad m) => BountyParams -> m GYAddress
bountyValidatorAddress cp = scriptAddress $ bountyValidator' cp
