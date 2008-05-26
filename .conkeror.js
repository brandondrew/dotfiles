// TODO:
// - js2-mode
// - monospace font

url_completion_use_history = true;

// check definition of paste_x_primary_selection
function fillDomain (I) {
  var field = document.getElementById("input-field");
  var paths = String(getWebNavigation().currentURI.spec).split('/');
  var domain = paths[0] + "/" + paths[1] + "/" + paths[2] + "/";
  field.value = domain;
};

interactive("fill_domain", fillDomain, []);
define_key(minibuffer_kmap, kbd("/", MOD_CTRL), "fill_domain");

function foo() {
  var hi;


}