{-# LANGUAGE CPP #-}
module Application (rippleInvoiceForm, sendRippleInvoice) where

import Prelude ()
import BasicPrelude
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

invoiceForm :: (Functor m, MonadIO m) => SimpleForm' m Invoice
invoiceForm = do
	from'     <- input_ (s"from") (Just . from)
	to'       <- input_ (s"to") (Just . to)
	ripple'   <- input  (s"ripple") (Just . ShowRead . ripple) (wdef,vdef)
		(mempty {label = lbl"Your Ripple address"})
	amount'   <- input_ (s"amount") (Just . amount)
	currency' <- input_ (s"currency") (Just . currency)
	message'   <- input  (s"message") (Just . message) (textarea,vdef)
		(mempty {required = False})

	return $ Invoice <$> from' <*> to' <*> fmap unShowRead ripple' <*> amount' <*> currency' <*> message'

rippleInvoiceForm :: URI -> Application
rippleInvoiceForm root (Request {queryString = qs}) = do
	(form,_) <- postSimpleForm hideErr (return $ queryFormEnv qs) invoiceForm
	textBuilder ok200 [htmlCT] $ viewHome htmlEscape (Home form path)
	where
	path = sendRippleInvoicePath `relativeTo` root
	hideErr opt = render (opt {errors = []})

sendRippleInvoice :: URI -> Application
sendRippleInvoice root req = do
	(form,minvoice) <- postSimpleForm render (bodyFormEnv_ req) invoiceForm
	case minvoice of
		Just invoice -> do
			mailBody <- responseToMailPart True =<< textBuilder ok200 []
				(viewEmail (escapeURIString isUnescapedInURIComponent) invoice)
			liftIO $ renderSendMail Mail {
				mailFrom    = emailToAddress $ from invoice,
				mailTo      = [emailToAddress $ to invoice],
				mailCc      = [], mailBcc  = [],
				mailHeaders = [(s"Subject", formatSubject $ message invoice)],
				mailParts   = [[mailBody]]
			}
			redirect' seeOther303 [] (rippleInvoiceFormPath `relativeTo` root)
		Nothing ->
			textBuilder ok200 [htmlCT] $ viewHome htmlEscape (Home form path)
	where
	emailToAddress = Address Nothing . show
	formatSubject msg
		| T.null msg = s"Ripple Invoice"
		| otherwise = s"Ripple Invoice \"" ++ T.take 30 msg ++ s"\""
	path = sendRippleInvoicePath `relativeTo` root
