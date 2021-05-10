doc: 
	dune b @doc 
	cp -f -R  _build/default/_doc/_html/* docs/ 

.PHONY: doc

