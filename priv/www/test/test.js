var session_id = '';
var resources = [
    "users",
    "groups",
    "users_groups",
    "folders",
    "documents",
    "folders_groups",
    "digsig",
    "crypto",
    "dfserver",
    "run_test",
    "test_results"
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
          if (elem.blank()) {
            return null;
          }
          var name = elem.split("=", 1);
          var eqS = elem.indexOf("=");
          var data = encodeURIComponent(elem.substr(eqS+1));
          return name+'='+data;
        })
        .merge(['session_id='+session_id])
        .compact().join('&');
      var url = '/' + [resource, id].join('/');
      //console.log("sending request to " + url + " with params: " + params);
      var starttime = new Date().getTime();
      if (resource != 'test_results') {
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
          },
          onFailure: function(req) {
            $('answer').html("Request failed");
          }
        }).send();
      } else {
        new Xhr(url, {
          method: method,
          params: params,
          onSuccess: function(req) {
            var json = req.json;
            if (json.error) {
              $('answer').html("Error! " + json.error);
            } else if (json.response) {
              var results = {};
              var maxtime = 0;
              var mintime = null;
              for (i in json.response) {
                var l = json.response[i].log_entry;
                if (typeof l != 'undefined') {
                  if (typeof results[l.num] == 'undefined') {
                    results[l.num] = {};
                  }
                  results[l.num][l.action] = l.time;
                  if (mintime == null) {
                    mintime = l.time;
                  }
                  if (l.time < mintime) {
                    mintime = l.time;
                  }
                  if (l.time > maxtime) {
                    maxtime = l.time;
                  }
                }
              }
              maxtime = maxtime - mintime;
              $('time').html(maxtime/1000 + "ms");
              var div = $E('div', {style: {position: 'relative'}});
              var rownum = 0;
              var colors = ['red', 'gray', 'blue', 'gray', 'green', 'gray', 'purple'].reverse();
              Object.each(results, function(key, result) {
                var started_at = (result.decrypt_started-mintime)*800/maxtime;
                [result.decrypted, result.verify_started, result.verified, result.sign_started, result.signed, result.encrypt_started, result.encrypted]
                .reverse()
                .each(function(t, i) {
                  var time = (t-mintime)*800/maxtime - started_at;
                  div.append(
                    $E('div', {style: {
                      position: 'absolute',
                      'background-color': colors[i],
                      height: '5px',
                      'top': (rownum*6)+'px',
                      left: started_at + 'px',
                      width: time + 'px'
                    }})
                    );
                });
                rownum++;
              });
              $('answer').update(div);
            }
          },
          onFailure: function(req) {
            $('answer').html("Request failed");
          }
        }).send();
      }
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

