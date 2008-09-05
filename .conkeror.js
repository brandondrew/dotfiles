// TODO:
// - js2-mode
// - monospace font

interactive("fill_domain", function (I) {
	      var field = I.minibuffer.input_element;
	      var paths = String(I.window.content.location).split('/');
	      var domain = paths[0] + "/" + paths[1] + "/" + paths[2] + "/";
	      field.value = domain;});

interactive("tinyurl", function(I) {
    I.window.content.location.href = 'http://tinyurl.com/create.php?url=' + 
        encodeURIComponent(I.window.content.location.href);
});

define_key(minibuffer_keymap, kbd("/", MOD_CTRL), "fill_domain");

add_webjump("hub", "http://github.com/search?q=%s");
add_webjump("wikipedia", "http://www.google.com/search?q=wikipedia+%s&btnI=I'm Feeling Lucky");
add_delicious_webjumps ("technomancy");

url_remoting_fn = load_url_in_new_buffer;
url_completion_use_history = true;
can_kill_last_buffer = false;


register_user_stylesheet( 
    "data:text/css,"+
        escape("#minibuffer, tree.completions, .mode-line { font-family: Terminus; font-size: 12pt; }"));

/* Sets up the reddit-mode for the given buffer. */
function twitter_mode_setup(buffer) {
    var blacklist = /iphone|twitteriffic/i;
    var document = buffer.document;
    if(document.twitter) return;
    else document.twitter_mode_loaded = true;
    if(!String(window.content.location).substr(0, 23) == "http://twitter.com/home") return;

    var trs = document.getElementsByTagName("tr");
    for each(let tr in trs) {
        if(tr.innerHTML.match(blacklist)) {
            tr.style.display = "none";
        };
    };
}

/* Setting up and tearing down the mode */

function enable_twitter_mode(buffer) {
  var doc = buffer.document;
  if(doc.twitterCurrent != null)
    twitter_highlight(doc.twitterLinkDivs[doc.twitterCurrent]);
  buffer.local_variables.content_buffer_normal_keymap = twitter_keymap;
  add_hook.call(buffer, "content_buffer_finished_loading_hook", twitter_mode_setup);
}

function disable_twitter_mode(buffer) {
  var doc = buffer.document;

  if(doc.twitterCurrent != null)
    twitter_dehighlight(doc.twitterLinkDivs[doc.twitterCurrent]);
  remove_hook.call(buffer, "content_buffer_finished_loading_hook", twitter_mode_setup);
}

define_page_mode("twitter_mode", "twitter",
                 $enable = enable_twitter_mode,
                 $disable = disable_twitter_mode,
                 $doc = "twitter page-mode: blacklisting for twitter.");

var twitter_re = build_url_regex($domain = /([a-zA-Z0-9\-]*\.)*twitter/);
auto_mode_list.push([twitter_re, twitter_mode]);
