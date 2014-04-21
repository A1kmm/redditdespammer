App.Bots = Ember.Object.extend(util.addValidityChecks({botName: "", redditUsername: "", redditPassword: ""},
    {"botName": [util.validateNotEmpty], "redditUsername": [util.validateNotEmpty],
     "redditPassword": [util.validateNotEmpty]}));
App.BotsRoute = Ember.Route.extend({
    model: function() {
        return util.sendQuery({type: "ListBots"}).then(function(resp) { return App.Bots.create(resp); });
    },
    setupController: util.setupController
});
App.BotsController = Ember.ObjectController.extend({
    actions: {
        createBot: function() {
            var controller = this;
            util.sendCommand({type: "AddBot", bot: this.get('botName'),
                redditUsername: this.get('redditUsername'), redditPassword: this.get('redditPassword') })
              .then(function(resp) {
                if (resp.success) {
                    controller.transitionToRoute('bot', controller.get('botName'), util.randomVersion());
                } else {
                    controller.set('error', resp.message);
                }
              });
        }
    }
});

App.Bot = Ember.Object.extend(util.addValidityChecks({subreddit: ""}, {subreddit: [util.validateNotEmpty]}));
App.BotRoute = Ember.Route.extend({
    model: function(params) {
        if (params.bot == "all")
            return App.Bot.create({botParam: params.bot, subreddits: ["all"], canAddSubreddit: false});
        else
            return util.sendQuery({type: "ListSubreddits", bot: params.bot})
                .then(function(resp) {
                        return App.Bot.create(_.extend({botParam: params.bot, canAddSubreddit: true}, resp));
                      });
    },
    setupController: util.setupController
});
App.BotController = Ember.ObjectController.extend({
    actions: {
        createSubreddit: function() {
            var controller = this;
            util.sendCommand({type: "AddSubreddit", bot: this.get('botParam'), subreddit: this.get('subreddit') })
              .then(function(resp) {
                if (resp.success) {
                    controller.transitionToRoute('subreddit', controller.get('botParam'), controller.get('subreddit'),
                        util.randomVersion());
                } else {
                    controller.set('error', resp.message);
                }
              });
        }
    }
});
