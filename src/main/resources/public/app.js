window.App = Ember.Application.create();
window.api = '/api/';

if (localStorage != null) {
    window.sessionId = localStorage.getItem("sessionId");
}

App.Router.map(function() {
  this.resource('login', { path: '/login' });
  this.resource('user', { path: '/user/:user/:version' });
  this.resource('bots', { path: '/botlist/:version' });
  this.resource('bot', { path: '/bot/:bot/:version' });
  this.resource('subreddit', { path: '/subreddit/:bot/:subreddit/:version' });
});

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
