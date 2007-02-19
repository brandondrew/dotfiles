// buffer-switcher

function readFromMiniBuffer(prompt, initVal, history, completions, allowNonMatches, 
			    defaultMatch, callBack, abortCallback)
{
    var field = document.getElementById("input-field");
    gReadFromMinibufferCallBack = callBack;
    gReadFromMinibufferAbortCallBack = abortCallback;
    gMiniBufferCompletions = completions;
    gCurrentCompletion = null;
    gDefaultMatch = defaultMatch;
    gAllowNonMatches = allowNonMatches;
    initHistory(history);
    read_from_minibuffer_internal (prompt);
    if (initVal) {
        setInputValue(initVal);
	field.setSelectionRange(0, field.value.length);
    }
}

function switch_to_buffer()
{
    var bufs = getBrowser().getBrowserNames();
    var defBrowser = getBrowser().lastBrowser().webNavigation.currentURI.spec;
    var matches = zip2(bufs,getBrowser().mBrowsers);
    readFromMiniBuffer("Switch to buffer: ", defBrowser, 
		       "buffer", matches, false, defBrowser, go_to_buffer, null);
}

function kill_browser()
{
    var defBrowser = getBrowser().webNavigation.currentURI.spec;
    var bufs = getBrowser().getBrowserNames();
    var matches = zip2(bufs,getBrowser().mBrowsers);
    readFromMiniBuffer("Kill buffer: ", defBrowser, "buffer", matches, true, null,
		       function(m,b) {if (b=="") {getBrowser().killCurrentBrowser();} 
			   else {getBrowser().killBrowser(m);}});
}

function minibuffer_complete(direction)
{
    var field = document.getElementById("input-field");
    var str = field.value;
    var enteredText = str.substring(0, field.selectionStart);
    var initialSelectionStart = field.selectionStart;
    //    if (typeof(direction) == 'undefined') 
	direction = 1;

    gCurrentCompletions = miniBufferCompleteStr(enteredText, gMiniBufferCompletions);
    gCurrentCompletion = gCurrentCompletion || 0; // TODO: set this based on contents of field?

    // deselect unambiguous part
    while (gCurrentCompletions.length == 
	   miniBufferCompleteStr(str.substring(0, field.selectionStart + 1), 
				 gMiniBufferCompletions).length &&
	   field.selectionStart != field.value.length) {
	field.setSelectionRange(field.selectionStart + 1, field.value.length);
    }

    // if the above had no effect, cycle through options
    if (initialSelectionStart == field.selectionStart) {
	gCurrentCompletion = wrap(gCurrentCompletion + direction, gCurrentCompletions.length - 1);
	//gCurrentCompletion = gCurrentCompletion + direction;
	if(!gCurrentCompletions[gCurrentCompletion]) return;
	field.value = gCurrentCompletions[gCurrentCompletion][0];
	// When we allow non-matches it generally means the
	// completion takes an argument. So add a space.
	if (gAllowNonMatches)
	    field.value += " ";
	field.setSelectionRange(enteredText.length, field.value.length);
    }
}

function minibuffer_complete_reverse ()
{
    minibuffer_complete(-1);
}

function wrap(val, max)
{
    if (val < 0)
	return max;
    if (val > max)
	return 0;
    return val;
}

function minibuffer_change(args)
{
    var event = args[0];
    var field = document.getElementById("input-field");
    var guessedText = field.value.substring(field.selectionStart, field.value.length);
    var enteredText = field.value.substring(0, field.selectionStart);

    // if you type the next letter of the selected text, don't erase it all, just shrink the selection
    if(String.fromCharCode(event.charCode) == guessedText[0]) {
	field.setSelectionRange(field.selectionStart + 1, field.value.length);
    } else {
	field.value = enteredText + String.fromCharCode(event.charCode);
	
	// are there other viable options?
	gCurrentCompletions = miniBufferCompleteStr(field.value, gMiniBufferCompletions);
	if (gCurrentCompletions.length != 0)
	    {
		// if so, use them
		field.value = gCurrentCompletions[0][0];
		// and select the unentered part
		field.setSelectionRange(enteredText.length + 1, field.value.length);
	    }
    }

    gCurrentCompletion = null;
}

add_command("minibuffer-complete", minibuffer_complete, []);
add_command("minibuffer-complete-reverse", minibuffer_complete_reverse, []);
add_command("switch-to-buffer", switch_to_buffer, []);
add_command("kill-buffer", kill_browser, []);
add_command("minibuffer-change", minibuffer_change, [["E"]]);
define_key (minibuffer_kmap, make_key (match_any_unmodified_key), "minibuffer-change");
