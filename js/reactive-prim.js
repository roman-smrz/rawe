
function r_prim_add_attr() {
        this.compute_ = function(params) {
                var name, value;
                var attr = params[1]
                if (attr.AttrVal) {
                        name = attr.AttrVal[0];
                        value = attr.AttrVal[1];
                }
                if (attr.AttrBool) {
                        name = attr.AttrBool[0];
                        value = true;
                }
                return params[0].clone().attr(name, value);
        }
}

function r_prim_bhv_to_html_inner(out) {
        var b = this;
        this.depend[out.id] = true;

        this.compute_ = function(x) {
                $('*[bhv-id='+out.id+']').each(function() {
                        var gen = $(this).attr('bhv-gen');
                        if (!gen) return;

                        b.gen = gen;
                        b.depend[gen] = true;
                        r_behaviours[gen].rdepend[b.id] = true;
                        // TODO: clear old dependency
                });
                if (!b.gen) return { Nothing: null, NotYet: null };
                return r_behaviours[b.gen].compute(x);
        }
}

// BehaviourFun a b -> BehaviourFun b c -> BehaviourFun a c
function r_prim_compose(f, g) {
        this.depend[f.id] = true;
        this.depend[g.id] = true;

        this.compute_ = function(x) {
                return g.compute(f.compute(x));
        };
}

// a -> BehaviourFun b a
function r_prim_const(value) {
        this.compute_ = function(x) { return value; };
}

function r_prim_debug() {
        this.compute_ = function(x) {
                alert(x[0] + ': ' + x[1]);
                return x[1];
        };
}

function r_prim_eq_string() {
        this.compute_ = function(params) {
                return params[0] == params[1];
        };
}

function r_prim_error() {
        this.compute_ = function(msg) { /*alert(msg);*/ };
}

/*
function r_prim_fst() {}
*/

function r_prim_gen() {
        this.compute_ = function() {
                return this.value;
        };

        this.change = function(value) {
                this.value = value;
                r_current_time++;
                this.invalidate();
                r_update_html();
        };

}

function r_prim_hask_to_bhv(inner, func) {
        this.depend[func.id] = true;
        this.compute_ = function(x) {
                inner.value = x;
                return func.compute([]);
        }
}

function r_prim_hask_to_bhv_inner() {
        this.compute_ = function() { return this.value; };
}

function r_prim_length() {
        this.compute_ = function(list) {
                return list.length;
        }
}

function r_prim_lookup() {
        this.compute_ = function(param) {
                var key = param[0];
                var list = param[1];
                for (i in list) {
                        if (list[i][0] == key)
                                return { Just: list[i][1] };
                }
                return { Nothing: null };
        };
}

function r_prim_lt_int() {
        this.compute_ = function(params) {
                return params[0] < params[1];
        }
}

function r_prim_plus() {
        this.compute_ = function(x) { return x[0]+x[1]; }
}

function r_prim_plus_mb() {
        this.compute_ = function(x) {
                if (typeof x[0].Just != 'undefined' && typeof x[1].Just != 'undefined')
                        return { Just: x[0].Just + x[1].Just };
                return { Nothing: null };
        }
}

function r_prim_product(f, g) {
        this.depend[f.id] = true;
        this.depend[g.id] = true;
        this.compute_ = function(x) {
                return [f.compute(x), g.compute(x)];
        };
}

function r_prim_sget(name) {
        this.values = [];
        var b = this;

        this.compute_ = function(x) {
                for (i in this.values) {
                        if (this.values[i][0] == x) {
                                return { Just: this.values[i][1] };
                        }
                }

                $.get(document.location.href, { q: name }, function(json) {
                        b.values.push([x, jQuery.parseJSON(json)]);
                        r_current_time++;
                        b.invalidate();
                        r_update_html();
                });

                return { Nothing: null };
        };
}

function r_prim_spost(name) {
        this.results = {};
        var b = this;

        this.compute_ = function(x) {
                if (typeof x.Timed == 'undefined')
                        return { Nothing: null };

                var time = x.Timed[0];
                var pairs = x.Timed[1];

                if (typeof this.results[time] != 'undefined')
                        return this.results[time]
                this.results[time] = { Nothing: null };

                var params = {};
                for (i in pairs)
                        params[pairs[i][0]] = pairs[i][1];

                $.post(document.location.href+'?q='+name, params, function(json) {
                        b.results[time] = { Just: jQuery.parseJSON(json) };
                        r_current_time++;
                        b.invalidate();
                        r_update_html();
                });

                return { Nothing: null };
        };
}

/*
function r_prim_snd() {}
*/

function r_prim_to_html_int() {
        this.compute_ = function(x) {
                return $('<span>'+x+'</span>');
        };
}

function r_prim_to_html_string() {
        this.compute_ = function(x) {
                // TODO: escape the string
                return $('<span>'+x+'</span>');
        };
}

function r_prim_until() {
        this.compute_ = function(params) {
                var def = params[0];
                var unt = params[1];

                if (typeof unt.Just != 'undefined')
                        return unt.Just;
                return def;
        };
}



/* Bool constructors and destructor */

function r_prim_true() {
        this.compute_ = function() { return true; };
}

function r_prim_false() {
        this.compute_ = function() { return false; };
}

function r_prim_bool() {
        this.compute_ = function(params) {
                var t = params[0];
                var f = params[1][0];
                var c = params[1][1];
                if (c) return t;
                return f;
        };
}


/* Maybe constructors and destructor */

function r_prim_nothing() {
        this.compute_ = function() { return { Nothing: null }; };
}

function r_prim_just() {
        this.compute_ = function(x) { return { Just: x }; };
}

function r_prim_maybe() {
        this.compute_ = function(params) {
                var def = params[0];
                var func = params[1][0];
                var mb = params[1][1];

                if (typeof mb.Just == 'undefined')
                        return def;
                return func.compute(mb.Just);
        };
}


/* Timed constructors and destructor */

function r_prim_not_yet() {
        this.compute_ = function() { return { NotYet: null }; };
}

function r_prim_on_time() {
        this.compute_ = function(x) { return { Timed: [ ++r_current_time, x ] }; };
}

function r_prim_timed() {
        this.compute_ = function(params) {
                var not_yet = params[0];
                var on_time = params[1][0];
                var value = params[1][1];

                if (typeof value.Timed == 'undefined')
                        return not_yet;
                return on_time.compute(value.Timed[1]);
        };
}
