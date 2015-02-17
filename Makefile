
test : simp
	lua runtests.lua

simp : *.pas
	fpc -gl -B simp.pas

