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

function openTestBuffers(){
    open_url_in(2, 'http://technomancy.us');
    open_url_in(2, 'http://dev.technomancy.us/phil');
    open_url_in(2, 'http://dev.technomancy.us');
}
add_command("test-buffers", openTestBuffers, []);

add_webjump("conkerorwiki","http://dev.technomancy.us/conkeror/search?q=%s&wiki=on&changeset=on&ticket=on");

// modeline style
var conkeror_css;

function conkeror_css_add_rule (rule)
{
    function find_conkeror_css () {
        for (var i = 0; i < document.styleSheets.length; i++) {
            if (document.styleSheets[i].href == "chrome://conkeror/content/conkeror.css")
                return document.styleSheets[i];
        }
    }
    if (! conkeror_css)
        conkeror_css = find_conkeror_css();
    conkeror_css.insertRule (rule, conkeror_css.cssRules.length);
}

conkeror_css_add_rule (".mode-line {   border-left: 1px solid rgb(230,230,230);  border-top: 1px solid rgb(230,230,230);  border-right: 1px solid rgb(100,100,100);  border-bottom: 1px solid rgb(100,100,100); } ");


// alter user preferences
function set_pref(k, v) {
    if (!gPrefService) { message("EEP: no gPrefService"); return 0; }
    gPrefService.setIntPref(k, v);
    message(k + " set to " + v);
}

function get_pref(k) {
    if (!gPrefService) { message("EEP: no gPrefService"); return 0; }
    if (!gPrefService.prefHasUserValue(k)) return null;
    else return gPrefService.getIntPref(k);
}

// toggle proxy
function toggle_http_proxy()
{
    var proxytype = get_pref("network.proxy.type");
    set_pref("network.proxy.type", proxytype == 1 ? 0 : 1);
 }
add_command("toggle-http-proxy", toggle_http_proxy, []);
//define_key(ctrlx_kmap, make_key("p", 0), "toggle-http-proxy");


// fill domain
define_key(minibuffer_kmap, make_key("/", MOD_CTRL), "fill_domain");

function fillDomain() {
    var field = document.getElementById("input-field");
    var paths = String(getWebNavigation().currentURI.spec).split('/');
    var domain = paths[0] + "/" + paths[1] + "/" + paths[2] + "/";
    field.value = domain;
}
add_command("fill_domain", fillDomain, []);



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
    // set up onchange to reset the current completion
    if (gMiniBufferCompletions != null) {
        var field = document.getElementById("input-field"); 
        field.onchange = "gCurrentCompletion = null;"
    }
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

function minibuffer_complete(direction)
{
    var field = document.getElementById("input-field");
    var str = field.value;
    var enteredText = str.substring(0, field.selectionStart);
    var initialSelectionStart = field.selectionStart;
    direction = 1;

    gCurrentCompletions = miniBufferCompleteStr(enteredText, gMiniBufferCompletions);
    gCurrentCompletion = gCurrentCompletion || 0;

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

function wrap(val, max)
{
    if (val < 0)
	return max;
    if (val > max)
	return 0;
    return val;
}

add_command("minibuffer-complete", minibuffer_complete, []);
add_command("minibuffer-complete-reverse", minibuffer_complete, [-1]);
add_command("switch-to-buffer", switch_to_buffer, []);

