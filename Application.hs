{-# LANGUAGE CPP #-}
module Application (rippleInvoiceForm, sendRippleInvoice) where

import Prelude ()
import BasicPrelude

import Network.Wai (Application, Request(..))
import Network.HTTP.Types (ok200)
import Network.Wai.Util (stringHeaders, textBuilder)

import Network.Wai.Digestive (queryFormEnv, bodyFormEnv_)
import SimpleForm.Combined (label, Label(..), wdef, vdef, ShowRead(..), unShowRead)
import SimpleForm.Render (errors)
import SimpleForm.Render.XHTML5 (render)
import SimpleForm.Digestive.Combined (SimpleForm', input, input_, getSimpleForm, postSimpleForm)
import SimpleForm (textarea)

import Network.URI (URI(..))
import Network.URI.Partial (relativeTo)

import Records
import MustacheTemplates
#include "PathHelpers.hs"

s :: (IsString s) => String -> s
s = fromString

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
	message'   <- input  (s"message") (Just . message) (textarea,vdef) mempty

	return $ Invoice <$> from' <*> to' <*> fmap unShowRead ripple' <*> amount' <*> currency' <*> message'

rippleInvoiceForm :: URI -> Application
rippleInvoiceForm root (Request {queryString = qs}) = do
	(form,_) <- postSimpleForm hideErr (return $ queryFormEnv qs) invoiceForm
	textBuilder ok200 headers $ viewHome htmlEscape (Home form path)
	where
	path = sendRippleInvoicePath `relativeTo` root
	hideErr opt = render (opt {errors = []})
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

sendRippleInvoice :: URI -> Application
sendRippleInvoice = undefined
