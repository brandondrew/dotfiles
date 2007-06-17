function openTestBuffers(){
    open_url_in(2, 'http://technomancy.us');
    open_url_in(2, 'http://dev.technomancy.us/phil');
    open_url_in(2, 'http://dev.technomancy.us');
}
interactive("test-buffers", openTestBuffers, []);

// fill domain
function fillDomain() {
    var field = document.getElementById("input-field");
    var paths = String(getWebNavigation().currentURI.spec).split('/');
    var domain = paths[0] + "/" + paths[1] + "/" + paths[2] + "/";
    field.value = domain;
}
interactive("fill_domain", fillDomain, []);
define_key(minibuffer_kmap, kbd("/", MOD_CTRL), "fill_domain");

// follow link in new buffer
interactive ("follow-link-in-new-buffer", open_url_in, [['value', 4], 'focused_link_url_s']);
define_key(top_kmap, kbd(KeyEvent.DOM_VK_RETURN, MOD_CTRL), "follow-link-in-new-buffer");