module Hledger.Utils.TH
( makeHledgerClassyLenses
) where

import Data.Char (toLower)
import Data.List.Extra (uncons, unsnoc)
import qualified Data.Set as Set
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)
import Lens.Micro ((&), (.~))
import Lens.Micro.TH (DefName(TopName), lensClass, lensField, makeLensesWith, classyRules)

-- | Make classy lenses for Hledger options fields.
-- This is intended to be used with BalancingOpts, InputOpt, ReportOpts,
-- ReportSpec, and CliOpts.
-- When run on X, it will create a typeclass named HasX (except for ReportOpts,
-- which will be named HasReportOptsNoUpdate) containing all the lenses for that type.
-- If the field name starts with an underscore, the lens name will be created
-- by stripping the underscore from the front on the name. If the field name ends with
-- an underscore, the field name ends with an underscore, the lens name will be
-- mostly created by stripping the underscore, but a few names for which this
-- would create too many conflicts instead have a second underscore appended.
-- ReportOpts fields for which updating them requires updating the query in
-- ReportSpec are instead names by dropping the trailing underscore and
-- appending NoUpdate to the name, e.g. querystring_ -> querystringNoUpdate.
makeHledgerClassyLenses :: Name -> DecsQ
makeHledgerClassyLenses x = flip makeLensesWith x $ classyRules
    & lensField .~ (\_ _ n -> fieldName $ nameBase n)
    & lensClass .~ (className . nameBase)
  where
    fieldName n | Just ('_', name) <- uncons n   = [TopName (mkName name)]
                | Just (name, '_') <- unsnoc n,
                  name `Set.member` queryFields  = [TopName (mkName $ name ++ "NoUpdate")]
                | Just (name, '_') <- unsnoc n,
                  name `Set.member` commonFields = [TopName (mkName $ name ++ "__")]
                | Just (name, '_') <- unsnoc n   = [TopName (mkName name)]
                | otherwise                      = []

    -- Fields which would cause too many conflicts if we exposed lenses with these names.
    commonFields = Set.fromList
        [ "empty", "drop", "color", "transpose"  -- ReportOpts
        , "anon", "new", "auto"                  -- InputOpts
        , "rawopts", "file", "debug", "width"    -- CliOpts
        ]

    -- When updating some fields of ReportOpts within a ReportSpec, we need to
    -- update the rsQuery term as well. To do this we implement a special
    -- HasReportOpts class with some special behaviour. We therefore give the
    -- basic lenses a special NoUpdate name to avoid conflicts.
    className "ReportOpts" = Just (mkName "HasReportOptsNoUpdate", mkName "reportOptsNoUpdate")
    className (x:xs)       = Just (mkName ("Has" ++ x:xs), mkName (toLower x : xs))
    className []           = Nothing

    -- Fields of ReportOpts which need to update the Query when they are updated.
    queryFields = Set.fromList ["period", "statuses", "depth", "date2", "real", "querystring"]
