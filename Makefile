example:
	nix-build example.nix -A ghcjs.reflex-jexcel -o result-reflex-jexcel
	$(BROWSER) ./result-reflex-jexcel/bin/reflex-jexcel-exe.jsexe/index.html
