window.App = Ember.Application.create();
window.api = '/api/';

if (localStorage != null) {
    window.sessionId = localStorage.getItem("sessionId");
}

App.Router.map(function() {
  this.resource('login', { path: '/login' });
  this.resource('user', { path: '/user/:user/:version' });
});

function addValidityChecks(obj, needValidation, groups) {
    if (groups === undefined) {
        groups = {};
    }
    var newProperties = {};
    var propertyNames = [];
    _.map(needValidation, function(validators, propertyName) {
        newProperties[propertyName + "Valid"] = function() {
            var value = this.get(propertyName);
            return _.every(_.map(validators, function(validator) { return validator(value) === true; }));
        }.property(propertyName);

        newProperties[propertyName + "Invalid"] = function() {
            return !this.get(propertyName + "Valid");
        }.property(propertyName + "Valid");

        newProperties[propertyName + "Error"] = function() {
            var value = this.get(propertyName);
            return _.find(_.map(validators, function(validator) { return validator(value); }), function(v) {
                return v !== true;
            });
        }.property(propertyName);

        propertyNames.push(propertyName + "Valid");
    });
    var validFn = (function() {
        var that = this;
        return _.all(_.map(propertyNames,
            function(propertyName) {
                return that.get(propertyName);
            }));
        });
    newProperties.valid = validFn.property.apply(validFn, propertyNames);
    newProperties.invalid = function() { return !this.get('valid'); }.property('valid');

    for (var groupName in groups) {
        validFn = (function() {
            var that = this;
            return _.all(_.map(groups[groupName],
                function(propertyName) {
                    return that.get(propertyName);
                }));
        });
        newProperties[groupName + 'Valid'] = validFn.property.apply(validFn, groups[groupName]);
        newProperties[groupName + 'Invalid'] = function() { return !this.get(groupName + 'Valid'); }
           .property(groupName + 'Valid');
    }

    return _.extend(obj, newProperties);
}

function validateNotEmpty(value) {
    if (value.length > 0)
        return true;
    return "This field is required";
}

function authenticatedRequest(path, data) {
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
}
function sendCommand(data) {
  return authenticatedRequest(api + "command", data);
}
function sendQuery(data) {
  return authenticatedRequest(api + "query", data);
}

function setupController(controller, model) {
    if (model.get('type') == "CommandFailed") {
        if (model.get('msg') == "Your session is invalid") {
          // Ember seems to only allow one nested transitionToRoute (e.g. front page -> user -> login doesn't work),
          // so we work around this with a setTimeout.
          window.setTimeout(function() {
            controller.transitionToRoute("login");
          }, 0);
          return;
        } else
          model.set("error", model.get('msg'));
    }
    controller.set("model", model);
}

function randomVersion() {
  return Math.floor(Math.random()*10000);
}

App.IndexRoute = Ember.Route.extend({
    setupController: function(controller) {
        if (window.sessionId == null) {
            controller.transitionToRoute('login');
        } else {
            controller.transitionToRoute('user', 'me', 1);
        }
    }
});

App.RdFormComponent = Ember.Component.extend({
    actions: {
        submit: function() {
            this.sendAction('submit');
        }
    }
});

App.RdFormIComponent = App.RdFormComponent.extend({});

App.Login = Ember.Object.extend(addValidityChecks({ username: "", password: "" },
    {'username': [validateNotEmpty], 'password': [validateNotEmpty]}));

App.LoginRoute = Ember.Route.extend({
    model: function() {
        return App.Login.create();
    }
});

App.LoginController = Ember.ObjectController.extend({
    actions: {
        attemptLogin: function() {
            var controller = this;
            $.post(api + "login", JSON.stringify({username: this.get('username'), password: this.get('password')}))
                .done(function(result) {
                    var result = JSON.parse(result);
                    if (result.type == "CommandFailed")
                      controller.set("error", result.msg);
                    else if (result.type == "SessionStarted") {
                      window.sessionId = result.sessionId;
                      if (localStorage != null)
                        localStorage.setItem('sessionId', result.sessionId);
                      controller.transitionToRoute('user', 'me', randomVersion());
                    }
                })
                .fail(function() {
                    this.set("error", "Cannot load data");
                });
        }
    }
});

App.User = Ember.Object.extend(addValidityChecks({ newUsername: "", newPassword: "" },
      {'newUsername': [validateNotEmpty], 'newPassword': [validateNotEmpty]},
      {'createUser': ['newUsername', 'newPassword']}
    ));
App.UserRoute = Ember.Route.extend({
    model: function(params) {
        return sendQuery({type: "ShowAccessForUser", user: params.user}).then(function(resp) {
            var model = App.User.create(resp);
            model.set("userparam", params.user);
            return model;
        });
    },
    setupController: setupController
});
App.UserController = Ember.ObjectController.extend({
    actions: {
        createUser: function() {
            $('#createUserDialog').modal();
        },
        confirmCreateUser: function() {
            var controller = this;
            sendCommand({type: "AddUser", password: this.get('newPassword'), username: this.get('newUsername')})
              .then(function(resp) {
                if (resp.success) {
                    $('#createUserDialog').modal('hide');
                    controller.transitionToRoute('user', controller.get('newUsername'), randomVersion());
                } else {
                    controller.set('createError', resp.message);
                }
              });
        },
        viewUser: function() {
            this.transitionToRoute('user', this.get('newUsername'), randomVersion());
        }
    }
});