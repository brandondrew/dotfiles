default_show_numbered_links = false;
default_show_numbered_images = false;
global_numbered_links_mode = false;

// Reload .conkeror RC file
function reinit()
{
 load_rc_file("~/.conkeror.js");
}
add_command("reinit", reinit, []);

function startRepl()
{
    Components
	.classes['@hyperstruct.net/mozlab/mozrepl;1']
	.getService(Components.interfaces.nsIMozRepl)
	.start(4242);
}
add_command("repl", startRepl, []);


function miniBufferCompleteKeyPress(event)
{
    try {
	var field = document.getElementById("input-field");
	if (event.keyCode == KeyEvent.DOM_VK_RETURN) {
	    try{
		var val = removeWhiteSpace (field.value);
		if (val.length == 0 && gDefaultMatch != null)
		    val = gDefaultMatch;
		var match = findCompleteMatch(gMiniBufferCompletions, val);
		addHistory(val);
		var callback = gReadFromMinibufferCallBack;
		gReadFromMinibufferCallBack = null;
		gReadFromMinibufferAbortCallBack = null;
		closeInput(true);
		if (callback) {
		    if (gAllowNonMatches)
			callback(match, val);
		    else if (match)
			callback(match);
		}
	    event.preventDefault();
	    event.preventBubble();
	    } catch (e) {window.alert(e);}
	} else if (handle_history(event, field)) {
	    event.preventDefault();
	    event.preventBubble();
	} else if (event.keyCode == KeyEvent.DOM_VK_TAB) {
	    var str = field.value;
	    var idx;
	    var initialSelection = field.value.substring(0, field.selectionStart);

	    // deselect in unambiguous part
	    while (gCurrentCompletions.length == miniBufferCompleteStr(field.value.substring(0, field.selectionStart+1), gMiniBufferCompletions).length &&
		   field.selectionStart != field.value.length) {
		field.setSelectionRange(field.selectionStart + 1, field.value.length);
	    } 
		  
	    // cycle through options (only if this press of tab didn't result in changing the selection)
	    if (initialSelection == field.value.substring(0, field.selectionStart)) {
		if (gCurrentCompletion != null) {
		    idx = gCurrentCompletion + (event.shiftKey ? -1:1);
		    if (idx >= gCurrentCompletions.length)
			idx = 0;
		    else if (idx < 0)
			idx = gCurrentCompletions.length - 1;
		} else {
		    idx = 0;
		    // Build our completion list
		    gCurrentCompletions = miniBufferCompleteStr(str, gMiniBufferCompletions);
		    if (gCurrentCompletions.length == 0)
			idx = null;
		}
		if (idx != null && gCurrentCompletions[idx]) {
		    gCurrentCompletion = idx;
		    field.value = gCurrentCompletions[idx][0];
		    // When we allow non-matches it generally means the
		    // completion takes an argument. So add a space.
 		    if (gAllowNonMatches)
 			field.value += " ";
		    field.setSelectionRange(initialSelection.length, field.value.length);

		}
	    }
	    event.preventDefault();
	    event.preventBubble();
	} else if (event.keyCode == KeyEvent.DOM_VK_ESCAPE
		   || (event.ctrlKey && (event.charCode == 103))) {
	    // Call the abort callback
	    if (gReadFromMinibufferAbortCallBack)
		gReadFromMinibufferAbortCallBack();
	    gReadFromMinibufferAbortCallBack = null;
	    gReadFromMinibufferCallBack = null;
	    closeInput(true);
	    event.preventDefault();
	    event.preventBubble();
	} else if (event.charCode && !event.ctrlKey && !metaPressed(event)) {
	    var initialSelectionStart = field.selectionStart;
	    gCurrentCompletion = gCurrentCompletion || 0;
	    gCurrentCompletions = miniBufferCompleteStr(field.value.substr(0, field.selectionStart) + String.fromCharCode(event.charCode), 
							gMiniBufferCompletions);

	    if (gCurrentCompletions[gCurrentCompletion]) {
		field.value = gCurrentCompletions[gCurrentCompletion][0];
		field.setSelectionRange(initialSelectionStart + 1, field.value.length);
		event.preventDefault();
	    }
	} else if (handle_basic_input(event)) {
	    // they did something so reset the completion cycle
	    gCurrentCompletion = null;
	    event.preventDefault();
	    event.preventBubble();
	}	    
    } catch(e) {alert(e);}
}

// altered version changes the prompt
function switchToBuffer()
{
    var bufs = getBrowser().getBrowserNames();
    var defBrowser = getBrowser().lastBrowser().webNavigation.currentURI.spec;
    var matches = zip2(bufs,getBrowser().mBrowsers);
    miniBufferCompleteImproved("Switch to buffer: ", defBrowser, "buffer", matches, false, go_to_buffer, null, defBrowser);
    document.getElementById("input-field").select();
}

function miniBufferCompleteImproved(prompt, initVal, history, completions, nonMatches, callBack, abortCallback, def)
{
    gReadFromMinibufferCallBack = callBack;
    gReadFromMinibufferAbortCallBack = abortCallback;
    gMiniBufferCompletions = completions;
    gCurrentCompletions = miniBufferCompleteStr("", gMiniBufferCompletions);
    for (i=0;i < gCurrentCompletions.length; i++)
	{ if (gCurrentCompletions[i][0] == initVal) gCurrentCompletion = i; }

    gDefaultMatch = def;
    gAllowNonMatches = nonMatches;
    initHistory(history);
    readInput(prompt, function () { setInputValue(initVal); }, "miniBufferCompleteKeyPress(event);");    
}

add_command("switch-to-buffer", switchToBuffer, []);

function killBuffer()
{
    var defBrowser = getBrowser().webNavigation.currentURI.spec;
    var bufs = getBrowser().getBrowserNames();
    var matches = zip2(bufs,getBrowser().mBrowsers);
    miniBufferCompleteImproved("Kill buffer: ", defBrowser, "buffer", matches, true,
		       function(m,b) {if (b=="") {getBrowser().killCurrentBrowser();} else {getBrowser().killBrowser(m);}});
    document.getElementById("input-field").select();
}

add_command("kill-buffer", killBuffer, []);

// function openUrl(args, fillInput)
// {
//     var prefix = args[0];
//     var templs = [];
//      for (var x in gWebJumpLocations)
//  	templs.push([x,x]);
//     var input = fillInput ? getWebNavigation().currentURI.spec : null;
//     var initVal = gHistory && gHistory.url[gHistory.url.length];
//     var completions = gHistory ? gHistory.url.reverse() : []

//     miniBufferCompleteImproved(open_url_in_prompt(prefix), initVal, "url", completions, true, 
// 		       function(match,url) {open_url_in(prefix, get_url_or_webjump(url));});
//     document.getElementById("input-field").select();
// }

// add_command("open-url", openUrl, []);