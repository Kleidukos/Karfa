module Karfa.Load where

withGhc :: [Flag] -> Ghc a -> IO a
withGhc flags action = do
  libDir <- fmap (fromMaybe (error "No GhcDir found") . snd) (getGhcDirs flags)

  -- Catches all GHC source errors, then prints and re-throws them.
  let handleSrcErrors action' = flip handleSourceError action' $ \err -> do
        printException err
        liftIO exitFailure
      needHieFiles = Flag_HyperlinkedSource `elem` flags

  withGhc' libDir needHieFiles (ghcFlags flags) (\_ -> handleSrcErrors action)




-- | Start a GHC session with the -haddock flag set. Also turn off
-- compilation and linking. Then run the given 'Ghc' action.
withGhc' :: String -> Bool -> [String] -> (DynFlags -> Ghc a) -> IO a
withGhc' libDir needHieFiles flags ghcActs = runGhc (Just libDir) $ do
  logger <- getLogger
  dynflags' <- parseGhcFlags logger =<< getSessionDynFlags

  -- We disable pattern match warnings because than can be very
  -- expensive to check
  let dynflags'' = unsetPatternMatchWarnings $
        updOptLevel 0 dynflags'
  -- ignore the following return-value, which is a list of packages
  -- that may need to be re-linked: Haddock doesn't do any
  -- dynamic or static linking at all!
  _ <- setSessionDynFlags dynflags''
  ghcActs dynflags''
  where

    -- ignore sublists of flags that start with "+RTS" and end in "-RTS"
    --
    -- See https://github.com/haskell/haddock/issues/666
    filterRtsFlags :: [String] -> [String]
    filterRtsFlags flgs = foldr go (const []) flgs True
      where go "-RTS" func _ = func True
            go "+RTS" func _ = func False
            go _      func False = func False
            go arg    func True = arg : func True


    parseGhcFlags :: MonadIO m => Logger -> DynFlags -> m DynFlags
    parseGhcFlags logger dynflags = do
      -- TODO: handle warnings?

      let extra_opts | needHieFiles = [Opt_WriteHie, Opt_Haddock]
                     | otherwise = [Opt_Haddock]
          dynflags' = (foldl' gopt_set dynflags extra_opts)
                        { backend = NoBackend
                        , ghcMode = CompManager
                        , ghcLink = NoLink
                        }
          flags' = filterRtsFlags flags

      (dynflags'', rest, _) <- parseDynamicFlags logger dynflags' (map noLoc flags')
      if not (null rest)
        then throwE ("Couldn't parse GHC options: " ++ unwords flags')
        else return dynflags''

