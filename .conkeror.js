// TODO:
// - js2-mode
// - monospace font

interactive("fill_domain", function (I) {
	      var field = I.minibuffer.input_element;
	      var paths = String(I.window.content.location).split('/');
	      var domain = paths[0] + "/" + paths[1] + "/" + paths[2] + "/";
	      field.value = domain;});


define_key(minibuffer_keymap, kbd("/", MOD_CTRL), "fill_domain");

add_webjump("hub", "http://github.com/search?q=%s");
add_webjump("wikipedia", "http://www.google.com/search?q=wikipedia+%s&btnI=I'm Feeling Lucky");

url_remoting_fn = load_url_in_new_buffer;
url_completion_use_history = true;
can_kill_last_buffer = false;