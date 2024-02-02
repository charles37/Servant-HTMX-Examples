{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module HTMXHeaders where
import Servant (URI)


import GHC.TypeLits
import Data.Proxy
import Data.Kind (Type)
import Servant

import Network.Wai (Response)



-- Response Headers
-- 
-- htmx supports some htmx-specific response headers:
-- 
--     HX-Location - allows you to do a client-side redirect that does not do a full page reload
--     HX-Push-Url - pushes a new url into the history stack
--     HX-Redirect - can be used to do a client-side redirect to a new location
--     HX-Refresh - if set to “true” the client-side will do a full refresh of the page
--     HX-Replace-Url - replaces the current URL in the location bar
--     HX-Reswap - allows you to specify how the response will be swapped. See hx-swap for possible values
--     HX-Retarget - a CSS selector that updates the target of the content update to a different element on the page
--     HX-Reselect - a CSS selector that allows you to choose which part of the response is used to be swapped in. Overrides an existing hx-select on the triggering element
--     HX-Trigger - allows you to trigger client-side events
--     HX-Trigger-After-Settle - allows you to trigger client-side events after the settle step
--     HX-Trigger-After-Swap - allows you to trigger client-side events after the swap step
-- 
-- For more on the HX-Trigger headers, see HX-Trigger Response Headers.
-- 
-- Submitting a form via htmx has the benefit of no longer needing the Post/Redirect/Get Pattern. After successfully processing a POST request on the server, you don’t need to return a HTTP 302 (Redirect). You can directly return the new HTML fragment.
-- 
data HTMXHeader = HXLocation | HXPushUrl | HXRedirect | HXRefresh
                | HXReplaceUrl | HXReswap | HXRetarget | HXReselect
                | HXTrigger | HXTriggerAfterSettle | HXTriggerAfterSwap


data HTMXHeaderType :: HTMXHeader -> Type where
    HXLocationType :: String -> HTMXHeaderType 'HXLocation
    HXPushUrlType :: String-> HTMXHeaderType 'HXPushUrl
    HXRedirectType :: String -> HTMXHeaderType 'HXRedirect
    HXRefreshType :: Bool -> HTMXHeaderType 'HXRefresh
    HXReplaceUrlType :: String -> HTMXHeaderType 'HXReplaceUrl
    HXReswapType :: String -> HTMXHeaderType 'HXReswap
    HXRetagetType :: String -> HTMXHeaderType 'HXRetarget
    HXReselectType :: String -> HTMXHeaderType 'HXReselect
    HXTriggerType :: String -> HTMXHeaderType 'HXTrigger
    HXTriggerAfterSettleType :: String -> HTMXHeaderType 'HXTriggerAfterSettle
    HXTriggerAfterSwapType :: String -> HTMXHeaderType 'HXTriggerAfterSwap

type family HTMXHeaders (headers :: [HTMXHeader]) :: Type where
    HTMXHeaders '[]       = ()
    HTMXHeaders (h ': hs) = (HTMXHeaderType h, HTMXHeaders hs)

addHeaderToResponse :: HTMXHeaderType h -> 
addHeaderToResponse (HXLocationType url) resp = addHeader "HX-Location" url resp
addHeaderToResponse (HXPushUrlType url) resp = addHeader "HX-Push-Url" url resp
addHeaderToResponse (HXRedirectType url) resp = addHeader "HX-Redirect" url resp

-- addHTMXHeaders :: HTMXHeaders hs -> Response -> Response
-- addHTMXHeaders () resp = resp
-- addHTMXHeaders (header : rest) resp = addHTMXHeaders rest (addHeaderToResponse header resp)
-- 

