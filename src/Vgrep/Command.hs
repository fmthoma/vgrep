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
    | PagerPageUp          -- ^ Scroll one page up in pager
    | PagerPageDown        -- ^ Scroll one page down in pager
    | PagerHalfPageUp      -- ^ Scroll half a page up in pager
    | PagerHalfPageDown    -- ^ Scroll half a page down in pager
    | PagerScrollLeft      -- ^ Scroll eight characters left in pager
    | PagerScrollRight     -- ^ Scroll eight characters right in pager

    | ResultsUp            -- ^ Move to previous result
    | ResultsDown          -- ^ Move to next result
    | ResultsPageUp        -- ^ Move one page up in results list
    | ResultsPageDown      -- ^ Move one page down in results list

    | PrevResult           -- ^ Move to previous result and update pager
    | NextResult           -- ^ Move to next result and update pager
    | PagerGotoResult      -- ^ Update pager with currently selected result

    | EdlineEnterSearch    -- ^
    | EdlineLeave

    | OpenFileInEditor     -- ^ Open file in external editor and jump to
                           -- currently selected result

    | Exit                 -- ^ Exit the application

    | Unset                -- ^ Treat keybinding as if not present, fall back to
                           -- alternative binding (used to override keybindings)

    deriving (Eq, Show, Generic)

instance Monoid Command where
    mempty = Unset
    mappend Unset a = a
    mappend a     _ = a
