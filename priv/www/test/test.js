var session_id = '';
var resources = [
    "users",
    "groups",
    "users_groups",
    "folders",
    "documents",
    "folders_groups",
    "dfserver"
  ];
$(document).onReady(function() {
    resources.each(function(el, i) {
      $('resource').append($E('option').html(el));
      });
    $('login_button').onClick(function() {
      login = $('login').value();
      password = $('password').value();
      new Xhr("/login", {
        params: {username: login, password: password },
        onSuccess: function(request) {
          var json = request.json;
          if (json.error) {
            $('flash').html(json.error);
          } else {
            $('flash').html(json.message);
            $('login_box').slide();
            $('tester').slide();
            session_id = json.session_id;
          }
        }
      }).send();
    });
    $('tester_button').onClick(function() {
      var resource = $('resource').value();
      var id = $('resource_id').value();
      var method = $('method').value();
      var params = $('params').value().trim()
        .split("\n")
        .map(function(elem, i) {
          var name = elem.split("=", 1);
          var eqS = elem.indexOf("=");
          var data = encodeURIComponent(elem.substr(eqS+1));
          return [name,data].join('=');
        })
        .merge(['session_id='+session_id])
        .join('&');
      var url = '/' + [resource, id].join('/');
      //console.log("sending request to " + url + " with params: " + params);
      var starttime = new Date().getTime();
      new Xhr(url, {
        method: method,
        params: params,
        onSuccess: function(req) {
          $('time').html((new Date().getTime() - starttime) + "ms");
          if (req.json) {
            json = req.json;
            if (json.error) {
              $('answer').html("Error! " + json.error);
            } else if (json.response) {
              objectString = Object.deepToString(json)
                .replace(/\n/g,'<br/>').replace(/ /g, '&nbsp;');
              $('answer').html(objectString);
              //console.log(json.response);
            }
          } else if (this.getHeader('Content-Disposition')) {
            $('answer').html('received file: <a href="'+this.url+'?session_id='+session_id+'">download</a>');
          } else {
            $('answer').html(this.text.split('<').join('&lt;').split('>').join("&gt;\n"));
          }
        }
      }).send();
    });
});

$ext(Object, {
  deepToString: function(object, indent) {
    indent = typeof(indent) == 'undefined' ? "" : indent;
    var tokens = [], key, value;
    for (key in object) {
      var value = object[key];
      if (isArray(value)) {
        arr = [];
        for (var i = 0; i < value.length; i++) {
          arr.push(Object.deepToString(value[i], indent+"    "));
        }
        value = '[\n'+indent+arr.join(',\n'+indent)+'\n'+indent+']';
      } else if (isHash(value)) {
        value = Object.deepToString(value, indent+"    ");
      }
      tokens.push(key + ': ' + value);
    }
    return "{\n" + indent + tokens.join(",\n" + indent) + "\n"+indent+"}";
  }
}, true);

