{-# LANGUAGE CPP #-}
module Application (rippleInvoiceForm, sendRippleInvoice, classicInvoiceForm, sendClassicInvoice) where

import Prelude ()
import BasicPrelude
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import qualified Data.Text as T

import Network.Wai (Application, Request(..))
import Network.HTTP.Types (ok200, seeOther303)
import Network.Wai.Util (stringHeaders, textBuilder, redirect', responseToMailPart)
import Network.Mail.Mime (Address(..), Mail(..), renderSendMail)

import Network.Wai.Digestive (queryFormEnv, bodyFormEnv_)
import SimpleForm.Combined (label, Label(..), wdef, vdef, ShowRead(..), unShowRead, required)
import SimpleForm.Render (errors)
import SimpleForm.Render.XHTML5 (render)
import SimpleForm.Digestive.Combined (SimpleForm', input, input_, getSimpleForm, postSimpleForm)
import SimpleForm (textarea)

import Network.URI (URI(..), isUnescapedInURIComponent, escapeURIString)
import Network.URI.Partial (relativeTo)

import Records
import MustacheTemplates
#include "PathHelpers.hs"

s :: (IsString s) => String -> s
s = fromString

Just [htmlCT] = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

lbl :: String -> Maybe Label
lbl = Just . Label . s

invoiceForm :: (Functor m, MonadIO m) => Bool -> SimpleForm' m Invoice
invoiceForm classic = do
	from'     <- input_ (s"from") (Just . from)
	to'       <- input_ (s"to") (Just . to)
	ripple' <- uncurry (input (s"ripple") (Just . fmap ShowRead . ripple)) $
			if classic then
					((wdef . (>>= leftToMaybe), fmap Left vdef),
					mempty {label = lbl"Your username"})
			else
					((wdef . (>>= rightToMaybe), fmap Right vdef),
					mempty {label = lbl"Your Ripple address"})
	amount'   <- input_ (s"amount") (Just . amount)
	currency' <- input_ (s"currency") (Just . currency)
	message'   <- input  (s"message") (Just . message) (textarea,vdef)
		(mempty {required = False})

	return $ Invoice <$> from' <*> to' <*> (fmap (fmap unShowRead) ripple') <*> amount' <*> currency' <*> message'

showInvoiceForm :: Bool -> URI -> Application
showInvoiceForm classic root (Request {queryString = qs}) = do
	(form,_) <- postSimpleForm hideErr (return $ queryFormEnv qs) (invoiceForm classic)
	textBuilder ok200 [htmlCT] $ viewHome htmlEscape (Home form path classic)
	where
	path
		| classic = sendClassicInvoicePath `relativeTo` root
		| otherwise = sendRippleInvoicePath `relativeTo` root
	hideErr opt = render (opt {errors = []})

rippleInvoiceForm :: URI -> Application
rippleInvoiceForm = showInvoiceForm False

classicInvoiceForm :: URI -> Application
classicInvoiceForm = showInvoiceForm True

sendInvoice :: Bool -> URI -> Application
sendInvoice classic root req = do
	(form,minv) <- postSimpleForm render (bodyFormEnv_ req) (invoiceForm classic)
	case minv of
		Just invoice -> do
			mailBody <- responseToMailPart True =<< textBuilder ok200 []
				(template (escapeURIString isUnescapedInURIComponent) invoice)
			liftIO $ renderSendMail Mail {
				mailFrom    = emailToAddress $ from invoice,
				mailTo      = [emailToAddress $ to invoice],
				mailCc      = [], mailBcc  = [],
				mailHeaders = [(s"Subject", formatSubject $ message invoice)],
				mailParts   = [[mailBody]]
			}
			redirect' seeOther303 [] home
		Nothing ->
			textBuilder ok200 [htmlCT] $ viewHome htmlEscape (Home form path classic)
	where
	template
		| classic = viewClassicEmail
		| otherwise = viewEmail
	emailToAddress = Address Nothing . show
	formatSubject msg
		| T.null msg = s"Ripple Invoice"
		| otherwise = s"Ripple Invoice \"" ++ T.take 30 msg ++ s"\""
	path
		| classic = sendClassicInvoicePath `relativeTo` root
		| otherwise = sendRippleInvoicePath `relativeTo` root
	home
		| classic = classicInvoiceFormPath `relativeTo` root
		| otherwise = rippleInvoiceFormPath `relativeTo` root

sendRippleInvoice :: URI -> Application
sendRippleInvoice = sendInvoice False

sendClassicInvoice :: URI -> Application
sendClassicInvoice = sendInvoice True
