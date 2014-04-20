App.Login = Ember.Object.extend(util.addValidityChecks({ username: "", password: "" },
    {'username': [util.validateNotEmpty], 'password': [util.validateNotEmpty]}));

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
                      controller.transitionToRoute('user', 'me', util.randomVersion());
                    }
                })
                .fail(function() {
                    this.set("error", "Cannot load data");
                });
        }
    }
});
