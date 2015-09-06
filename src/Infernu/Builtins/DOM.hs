{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.DOM
       where

import           Infernu.Builtins.Util
import           Infernu.Prelude
import           Infernu.Types

-- DOMHighResTimeStamp
timestamp = number -- TODO

-- [Constructor(DOMString type, optional EventInit eventInitDict),
--  Exposed=(Window,Worker,System)]
-- interface Event {
eventProps eventTarget =
--   [Pure]
--   readonly attribute DOMString type;
    [ ("type", ty string)
--   [Pure]
--   readonly attribute EventTarget? target;
    , ("target", ty eventTarget) -- TODO: "nullable"
--   [Pure]
--   readonly attribute EventTarget? currentTarget;
    , ("currentTarget", ty eventTarget)

--   const unsigned short NONE = 0;
    , ("NONE", ty number)
--   const unsigned short CAPTURING_PHASE = 1;
    , ("CAPTURING_PHASE", ty number)
--   const unsigned short AT_TARGET = 2;
    , ("AT_TARGET", ty number)
--   const unsigned short BUBBLING_PHASE = 3;
    , ("BUBBLING_PHASE", ty number)
--   [Pure]
--   readonly attribute unsigned short eventPhase;
    , ("eventPhase", ty number)

--   void stopPropagation();
    , ("stopPropagation", ts [0] $ funcN [tvar 0] undef)
--   void stopImmediatePropagation();
    , ("stopImmediatePropagation", ts [0] $ funcN [tvar 0] undef)

--   [Pure]
--   readonly attribute boolean bubbles;
    , ("bubbles", ty boolean)
--   [Pure]
--   readonly attribute boolean cancelable;
    , ("cancelable", ty boolean)
--   void preventDefault();
    , ("preventDefault", ts [0] $ funcN [tvar 0] undef)
--   [Pure]
--   readonly attribute boolean defaultPrevented;
    , ("defaultPrevented", ty boolean)

--   [Unforgeable, Pure]
--   readonly attribute boolean isTrusted;
    , ("isTrusted", ty boolean)
--   [Pure]
--   readonly attribute DOMHighResTimeStamp timeStamp;
    , ("timeStamp", ty timestamp)

--   [Throws]
--   void initEvent(DOMString type, boolean bubbles, boolean cancelable);
    , ("initEvent", ts [0] $ funcN [tvar 0, string, boolean, boolean] undef)
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
    [ ("addEventListener", ts [0] $ funcN [tvar 0, string, eventListener] undef) -- TODO optional: , boolean, boolean
  -- [Throws]
  -- void removeEventListener(DOMString type,
  --                          EventListener? listener,
  --                          optional boolean capture = false);
    , ("removeEventListener", ts [0] $ funcN [tvar 0, string, eventListener] undef) -- boolean
  -- [Throws]
  -- boolean dispatchEvent(Event event);
    , ("dispatchEvent", ts [0] $ funcN [tvar 0, event] boolean)
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

