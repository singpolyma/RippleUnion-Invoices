module Records where

import Prelude ()
import BasicPrelude
import qualified Data.Text as T

import Text.Blaze.Html (Html)
import Network.URI (URI)
import Text.Email.Validate (EmailAddress)
import Data.Base58Address (RippleAddress)
import Currency (Currency(..))

import Data.Text.Buildable
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)
import SimpleForm (DefaultWidget(..), text)
import SimpleForm.Validation (DefaultValidation(..))
import qualified SimpleForm.Validation as SFV

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

instance Buildable RippleAddress where
	build = build . show

instance Buildable Currency where
	build = build . showC
		where
		showC (ISO4217Currency cur) = show cur
		showC (NonStandardCurrency str) = T.pack str

instance DefaultWidget Currency where
	wdef = text . fmap showC
		where
		showC (ISO4217Currency cur) = show cur
		showC (NonStandardCurrency str) = T.pack str

instance DefaultValidation Currency where
	vdef = fmap ((\t -> go (t,reads t)) . textToString) SFV.text
		where
		go (_, [(x, "")]) = x
		go (t, _) = NonStandardCurrency t

data Home = Home {
		renderedInvoiceForm :: Html,
		invoiceFormAction :: URI
	}

data Invoice = Invoice {
		from :: EmailAddress,
		to :: EmailAddress,
		ripple :: RippleAddress,
		amount :: Double,
		currency :: Currency,
		message :: Text
	}
