App.Subreddit = Ember.Object.extend(util.addValidityChecks(
    { newBannedPhrase: "", newExemptUsername: "", grantUsername: "" },
    { newBannedPhrase: [util.validateNotEmpty], newExemptUsername: [util.validateNotEmpty],
      grantUsername: [util.validateNotEmpty] }));
App.Subreddit.UsersAndAccess = Ember.Object.extend({});
App.Subreddit.ExemptUsers = Ember.Object.extend({});
App.Subreddit.BannedPhrases = Ember.Object.extend({});

App.SubredditRoute = Ember.Route.extend({
    model: function(params) {
        return new Ember.RSVP.Promise(function(resolve) {
            async.parallel({
                usersAndAccess: function(callback) {
                    util.sendQuery({type: "ShowUsersAndAccess", bot: params.bot, subreddit: params.subreddit}).
                        then(function(result) { callback(null, result); });
                },
                exemptUsers: function(callback) {
                    util.sendQuery({type: "ListExemptUsers", bot: params.bot, subreddit: params.subreddit}).
                        then(function(result) { callback(null, result); });
                },
                bannedPhrases: function(callback) {
                    util.sendQuery({type: "ListBannedPhrases", bot: params.bot, subreddit: params.subreddit}).
                        then(function(result) { callback(null, result); });
                }
            }, function(err, results) {
                resolve(App.Subreddit.create({
                    botParam: params.bot,
                    subredditParam: params.subreddit,
                    usersAndAccess: App.Subreddit.UsersAndAccess.create(results.usersAndAccess),
                    exemptUsers: App.Subreddit.ExemptUsers.create(results.exemptUsers),
                    bannedPhrases: App.Subreddit.BannedPhrases.create(results.bannedPhrases)
                }));
            });
        });
    },
    setupController: function(controller, model) {
       model.set("usersAndAccess.panelError", false);
       model.set("exemptUsers.panelError", false);
       model.set("bannedPhrases.panelError", false);
       if (model.get('usersAndAccess.type') == "CommandFailed") {
           if (model.get('usersAndAccess.msg') == "Your session is invalid") {
             // Ember seems to only allow one nested transitionToRoute (e.g. front page -> user -> login doesn't work),
             // so we work around this with a setTimeout.
             window.setTimeout(function() {
               controller.transitionToRoute("login");
             }, 0);
             return;
           } else {
             model.set("usersAndAccess.panelError", true);
           }
       }
       if (model.get('exemptUsers.type') == "CommandFailed") {
         model.set("exemptUsers.panelError", true);
       }
       if (model.get('bannedPhrases.type') == "CommandFailed") {
         model.set("bannedPhrases.panelError", true);
       }
       controller.set("model", model);
    }
});
App.SubredditController = Ember.ObjectController.extend({
    actions: {
        addBannedPhrase: function() {
            var controller = this;
            util.sendCommand({type: "AddBannedPhrase", bot: this.get("botParam"), subreddit: this.get("subredditParam"),
                              phrase: this.get('newBannedPhrase') }).
                then(function(result) {
                    if (result.success) {
                        controller.transitionToRoute('subreddit', controller.get('botParam'),
                            controller.get('subredditParam'), util.randomVersion());
                    } else {
                        controller.set('bannedPhrases.msg', result.message);
                    }
                });
        },
        deleteBannedPhrase: function(phrase) {
            var controller = this;
            util.sendCommand({type: "DeleteBannedPhrase", bot: this.get("botParam"),
                              subreddit: this.get("subredditParam"), phrase: phrase }).
                then(function(result) {
                    if (result.success) {
                        controller.transitionToRoute('subreddit', controller.get('botParam'),
                            controller.get('subredditParam'), util.randomVersion());
                    } else {
                        controller.set('bannedPhrases.msg', result.message);
                    }
                });
        },
        addExemptUser: function() {
            var controller = this;
            util.sendCommand({type: "AddExemptUser", bot: this.get("botParam"), subreddit: this.get("subredditParam"),
                              username: this.get('newExemptUsername') }).
                then(function(result) {
                    if (result.success) {
                        controller.transitionToRoute('subreddit', controller.get('botParam'),
                            controller.get('subredditParam'), util.randomVersion());
                    } else {
                        controller.set('exemptUsers.msg', result.message);
                    }
                });
        },
        deleteExemptUser: function(user) {
            var controller = this;
            util.sendCommand({type: "DeleteExemptUser", bot: this.get("botParam"),
                              subreddit: this.get("subredditParam"), username: user }).
                then(function(result) {
                    if (result.success) {
                        controller.transitionToRoute('subreddit', controller.get('botParam'),
                            controller.get('subredditParam'), util.randomVersion());
                    } else {
                        controller.set('exemptUsers.msg', result.message);
                    }
                });
        },
        addPermission: function() {
            var controller = this;
            util.sendCommand({type: "GrantAccess", bot: this.get("botParam"), subreddit: this.get("subredditParam"),
                              username: this.get('grantUsername'), permission: {type: this.get('permission')} }).
                then(function(result) {
                    if (result.success) {
                        controller.transitionToRoute('subreddit', controller.get('botParam'),
                            controller.get('subredditParam'), util.randomVersion());
                    } else {
                        controller.set('usersAndAccess.msg', result.message);
                    }
                });
        },
        revokePermission: function(permission) {
            var controller = this;
            util.sendCommand({type: "RevokeAccess", username: permission.username,
                         bot: this.get("botParam"), subreddit: this.get("subredditParam"),
                         permission: {type: permission.permission}})
                .then(function(resp) {
                    if (resp.success) {
                        controller.transitionToRoute('subreddit', controller.get('botParam'),
                            controller.get('subredditParam'), util.randomVersion());
                    } else {
                        controller.set('usersAndAccess.msg', resp.message);
                    }
                });
        }
    }
});
