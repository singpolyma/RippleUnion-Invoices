Main: Main.hs Application.hs Routes.hs MustacheTemplates.hs PathHelpers.hs
	ghc -threaded -O2 -Wall -fno-warn-name-shadowing Main.hs

Routes.hs: routes
	routeGenerator -r -m Application -n 1 $< > $@

PathHelpers.hs: routes
	routeGenerator -p -n 1 $< > $@

MustacheTemplates.hs: Records.hs view/home.mustache view/meta.mustache view/header.mustache view/email.mustache view/classicEmail.mustache
	mustache2hs -m Records.hs view/home.mustache Home view/meta.mustache Home view/header.mustache Home view/email.mustache Invoice view/classicEmail.mustache Invoice > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main Routes.hs PathHelpers.hs MustacheTemplates.hs
