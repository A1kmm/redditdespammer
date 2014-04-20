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
        var dependProperties = _.flatten(_.filter(_.pluck(validators, 'extraProperties')));
        dependProperties.push(propertyName);
        var valFn = function() {
            var value = this.get(propertyName);
            var model = this;
            return _.every(_.map(validators, function(validator) { return validator(value, model) === true; }));
        };
        newProperties[propertyName + "Valid"] = valFn.property.apply(valFn, dependProperties);

        newProperties[propertyName + "Invalid"] = function() {
            return !this.get(propertyName + "Valid");
        }.property(propertyName + "Valid");

        var errFn = function() {
            var model = this;
            var value = this.get(propertyName);
            return _.find(_.map(validators, function(validator) { return validator(value, model); }), function(v) {
                return v !== true;
            });
        }

        newProperties[propertyName + "Error"] = errFn.property.apply(errFn, dependProperties);

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

    _.map(groups, function(groupValues, groupName) {
        var groupValues = groups[groupName];
        var validFn = (function() {
            var that = this;
            return _.all(_.map(groupValues,
                function(propertyName) {
                    return that.get(propertyName + "Valid");
                }));
        });
        newProperties[groupName + 'Valid'] = validFn.property.apply(validFn, groupValues);
        newProperties[groupName + 'Invalid'] = function() { return !this.get(groupName + 'Valid'); }
           .property(groupName + 'Valid');
    });

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

function validatePasswordsDifferent(value, model) {
    var otherValue = model.get('newPassword');
    if (otherValue != value)
      return "Passwords don't match";
    return true;
}
validatePasswordsDifferent.extraProperties = ["newPassword"];

App.User = Ember.Object.extend(addValidityChecks({
    newUsername: "", newPassword: "", newPassword2: "",
    bot: "", subreddit: "",
    canBlock: function() {
        return this.get('status') == 'Active';
    }.property('status'),
    canUnblock: function() {
        return this.get('status') == 'Blocked';
    }.property('status')
                },
      {'newUsername': [validateNotEmpty], 'newPassword': [validateNotEmpty],
       'newPassword2': [validateNotEmpty, validatePasswordsDifferent],
       'bot': [validateNotEmpty], 'subreddit': [validateNotEmpty]
      },
      {'createUser': ['newUsername', 'newPassword'], 'changePassword': ['newPassword', 'newPassword2'],
       'addPermission': ['bot', 'subreddit']}
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
        },
        changePassword: function() {
            var controller = this;
            sendCommand({type: "SetPassword", password: this.get('newPassword'), username: this.get('userparam')})
              .then(function(resp) {
                if (resp.success) {
                    controller.set('error', '');
                    controller.set('success', 'Password changed');
                } else {
                    controller.set('success', '');
                    controller.set('error', resp.message);
                }
              });
        },
        addPermission: function() {
            var controller = this;
            sendCommand({type: "GrantAccess", username: this.get('username'),
                         bot: this.get('bot'), subreddit: this.get('subreddit'),
                         permission: {type: this.get('permission')}})
                .then(function(resp) {
                    if (resp.success) {
                       controller.transitionToRoute('user', controller.get('userparam'), randomVersion());
                    } else {
                        controller.set('success', '');
                        controller.set('error', resp.message);
                    }
                });
        }
    }
});