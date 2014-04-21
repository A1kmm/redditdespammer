function validatePasswordsDifferent(value, model) {
    var otherValue = model.get('newPassword');
    if (otherValue != value)
      return "Passwords don't match";
    return true;
}
validatePasswordsDifferent.extraProperties = ["newPassword"];

App.User = Ember.Object.extend(util.addValidityChecks({
    newUsername: "", newPassword: "", newPassword2: "",
    bot: "", subreddit: "",
    canBlock: function() {
        return this.get('status') == 'Active';
    }.property('status'),
    canUnblock: function() {
        return this.get('status') == 'Blocked';
    }.property('status')
                },
      {'newUsername': [util.validateNotEmpty], 'newPassword': [util.validateNotEmpty],
       'newPassword2': [util.validateNotEmpty, validatePasswordsDifferent],
       'bot': [util.validateNotEmpty], 'subreddit': [util.validateNotEmpty]
      },
      {'createUser': ['newUsername', 'newPassword'], 'changePassword': ['newPassword', 'newPassword2'],
       'addPermission': ['bot', 'subreddit']}
    ));
App.UserRoute = Ember.Route.extend({
    model: function(params) {
        return util.sendQuery({type: "ShowAccessForUser", user: params.user}).then(function(resp) {
            var model = App.User.create(resp);
            model.set("userparam", params.user);
            return model;
        });
    },
    setupController: util.setupController
});

App.UserController = Ember.ObjectController.extend({
    actions: {
        createUser: function() {
            $('#createUserDialog').modal();
        },
        confirmCreateUser: function() {
            var controller = this;
            util.sendCommand({type: "AddUser", password: this.get('newPassword'), username: this.get('newUsername')})
              .then(function(resp) {
                if (resp.success) {
                    $('#createUserDialog').modal('hide');
                    setTimeout(function() {
                        controller.transitionToRoute('user', controller.get('newUsername'), util.randomVersion());
                    }, 0);
                } else {
                    controller.set('createError', resp.message);
                }
              });
        },
        viewUser: function() {
            this.transitionToRoute('user', this.get('newUsername'), util.randomVersion());
        },
        changePassword: function() {
            var controller = this;
            util.sendCommand({type: "SetPassword", password: this.get('newPassword'), username: this.get('userparam')})
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
            util.sendCommand({type: "GrantAccess", username: this.get('userparam'),
                         bot: this.get('bot'), subreddit: this.get('subreddit'),
                         permission: {type: this.get('permission')}})
                .then(function(resp) {
                    if (resp.success) {
                       controller.transitionToRoute('user', controller.get('userparam'), util.randomVersion());
                    } else {
                        controller.set('success', '');
                        controller.set('error', resp.message);
                    }
                });
        },

        blockUser: function() {
            var controller = this;
            util.sendCommand({type: "BlockUser", username: this.get('userparam')})
                .then(function(resp) {
                    if (resp.success) {
                       controller.transitionToRoute('user', controller.get('userparam'), util.randomVersion());
                    } else {
                        controller.set('success', '');
                        controller.set('error', resp.message);
                    }
                });
        },

        unblockUser: function() {
            var controller = this;
            util.sendCommand({type: "UnblockUser", username: this.get('userparam')})
                .then(function(resp) {
                    if (resp.success) {
                       controller.transitionToRoute('user', controller.get('userparam'), util.randomVersion());
                    } else {
                        controller.set('success', '');
                        controller.set('error', resp.message);
                    }
                });
        },

        revokePermission: function(permission) {
            var controller = this;
            util.sendCommand({type: "RevokeAccess", username: this.get('userparam'),
                         bot: permission.bot, subreddit: permission.subreddit,
                         permission: {type: permission.permission}})
                .then(function(resp) {
                    if (resp.success) {
                       controller.transitionToRoute('user', controller.get('userparam'), util.randomVersion());
                    } else {
                        controller.set('success', '');
                        controller.set('error', resp.message);
                    }
                });
        }
    }
});
