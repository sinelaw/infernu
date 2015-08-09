{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.DOM
       where

import           Infernu.Builtins.Util
import           Infernu.Prelude
import           Infernu.Types

data PropType t = R t | RW t
-- [Constructor(DOMString type, optional EventInit eventInitDict),
--  Exposed=(Window,Worker,System)]
-- interface Event {
eventProps eventTarget =
--   [Pure]
--   readonly attribute DOMString type;
    [ ("type", R $ ty string)
--   [Pure]
--   readonly attribute EventTarget? target;
    , ("target", R $ ty eventTarget) -- TODO: "nullable"
--   [Pure]
--   readonly attribute EventTarget? currentTarget;
    , ("currentTarget", R $ ty eventTarget)

--   const unsigned short NONE = 0;
    , ("NONE", R $ ty number)
--   const unsigned short CAPTURING_PHASE = 1;
    , ("CAPTURING_PHASE", R $ ty number)
--   const unsigned short AT_TARGET = 2;
    , ("AT_TARGET", R $ ty number)
--   const unsigned short BUBBLING_PHASE = 3;
    , ("BUBBLING_PHASE", R $ ty number)
--   [Pure]
--   readonly attribute unsigned short eventPhase;
    , ("eventPhase", R $ ty number)

--   void stopPropagation();
    , ("stopPropagation", RW $ ts [0] $ funcN [tvar 0])
--   void stopImmediatePropagation();
    , ("stopImmediatePropagation", RW $ ts [0] $ funcN [tvar 0])

--   [Pure]
--   readonly attribute boolean bubbles;
--   [Pure]
--   readonly attribute boolean cancelable;
--   void preventDefault();
--   [Pure]
--   readonly attribute boolean defaultPrevented;

--   [Unforgeable, Pure]
--   readonly attribute boolean isTrusted;
--   [Pure]
--   readonly attribute DOMHighResTimeStamp timeStamp;

--   [Throws]
--   void initEvent(DOMString type, boolean bubbles, boolean cancelable);
    ]
-- };

--eventRowType = foldM addProp (TRowEnd Nothing) eventProps

--interface EventTarget {
eventTargetProps eventListener event =
  -- /* Passing null for wantsUntrusted means "default behavior", which
  --    differs in content and chrome.  In content that default boolean
  --    value is true, while in chrome the default boolean value is
  --    false. */
  -- [Throws]
  -- void addEventListener(DOMString type,
  --                       EventListener? listener,
  --                       optional boolean capture = false,
  --                       optional boolean? wantsUntrusted = null);
    [ ("addEventListener", ts [0] $ funcN [tvar 0, string, eventListener]) -- TODO optional: , boolean, boolean
  -- [Throws]
  -- void removeEventListener(DOMString type,
  --                          EventListener? listener,
  --                          optional boolean capture = false);
    , ("removeEventListener", ts [0] $ funcN [tvar 0, string, eventListener]) -- boolean
  -- [Throws]
  -- boolean dispatchEvent(Event event);
    , ("dispatchEvent", ts [0] $ funcN [tvar 0, event])
    ]
-- };

elementGlobalAttributes :: [(String, TypeScheme)]
elementGlobalAttributes =
    -- TODO add more
  [ ("accesskey",  ty string)
  , ("class",      ty string)
  , ("id",         ty string)
  , ("style",      ty string)
  , ("tabindex",   ty number)
  , ("title",      ty string)
  ]

