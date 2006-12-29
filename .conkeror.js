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
bind_key(minibuffer_kmap, make_key("/", MOD_CTRL), "fill_domain");

function fillDomain() {
    var field = document.getElementById("input-field");
    var paths = String(getWebNavigation().currentURI.spec).split('/');
    var domain = paths[0] + "/" + paths[1] + "/" + paths[2] + "/";
    field.value = domain;
}
add_command("fill_domain", fillDomain, []);