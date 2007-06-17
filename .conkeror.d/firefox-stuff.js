function startRepl()
{
    Components
	.classes['@hyperstruct.net/mozlab/mozrepl;1']
	.getService(Components.interfaces.nsIMozRepl)
	.start(4242);
}
interactive("repl", startRepl, []);
