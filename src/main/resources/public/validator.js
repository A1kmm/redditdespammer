window.util = window.util || {};
util.addValidityChecks = function(obj, needValidation, groups) {
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
};

util.validateNotEmpty = function(value) {
    if (value.length > 0)
        return true;
    return "This field is required";
};
