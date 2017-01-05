{-# LANGUAGE DeriveGeneric #-}
module Vgrep.Command where

import GHC.Generics

data Command
    = DisplayPagerOnly     -- ^ Display the pager full-screen
    | DisplayResultsOnly   -- ^ Display the results list full-screen
    | SplitFocusPager      -- ^ Split screen, focus on pager
    | SplitFocusResults    -- ^ Split screen, focus on results list

    | PagerUp              -- ^ Scroll one line up in pager
    | PagerDown            -- ^ Scroll one line down in pager
    | PagerPgUp            -- ^ Scroll one page up in pager
    | PagerPgDown          -- ^ Scroll one page down in pager
    | PagerScrollLeft      -- ^ Scroll eight characters left in pager
    | PagerScrollRight     -- ^ Scroll eight characters right in pager

    | ResultsUp            -- ^ Move to previous result
    | ResultsDown          -- ^ Move to next result
    | ResultsPgUp          -- ^ Move one page up in results list
    | ResultsPgDown        -- ^ Move one page down in results list

    | PrevResult           -- ^ Move to previous result and update pager
    | NextResult           -- ^ Move to next result and update pager
    | PagerGotoResult      -- ^ Update pager with currently selected result

    | OpenFileInEditor     -- ^ Open file in external editor and jump to
                           -- currently selected result

    | Exit                 -- ^ Exit the application

    | None                 -- ^ Do nothing (used to override a config)

    deriving (Eq, Show, Generic)
