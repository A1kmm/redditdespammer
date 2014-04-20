window.util = window.util || {};
util.authenticatedRequest = function(path, data) {
    data.sessionId = window.sessionId;
    data = JSON.stringify(data);
    model = {};
    return new Ember.RSVP.Promise(function(resolve) {
        $.post(path, data).done(function(output) {
          resolve(_.extend(model, JSON.parse(output)));
        }).fail(function() {
          resolve(_.extend(model, {type: "CommandFailed", msg: "Cannot load data"}));
        });
    });
};

util.sendCommand = function(data) {
  return util.authenticatedRequest(api + "command", data);
};

util.sendQuery = function(data) {
  return util.authenticatedRequest(api + "query", data);
};

util.setupController = function(controller, model) {
    model.set("pageError", false);
    if (model.get('type') == "CommandFailed") {
        if (model.get('msg') == "Your session is invalid") {
          // Ember seems to only allow one nested transitionToRoute (e.g. front page -> user -> login doesn't work),
          // so we work around this with a setTimeout.
          window.setTimeout(function() {
            controller.transitionToRoute("login");
          }, 0);
          return;
        } else {
          model.set("error", model.get('msg'));
          model.set("pageError", true);
        }
    }
    controller.set("model", model);
};

util.randomVersion = function() {
  return Math.floor(Math.random()*10000);
};
