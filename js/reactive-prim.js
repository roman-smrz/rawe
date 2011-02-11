
function r_prim_add_attr() {
        this.compute_ = function(params) {
                var name, value;
                var attr = params[1].get();
                if (attr.AttrVal) {
                        name = attr.AttrVal[0];
                        value = attr.AttrVal[1];
                }
                if (attr.AttrBool) {
                        name = attr.AttrBool[0];
                        value = true;
                }
                return params[0].get().clone().attr(name, value);
        }
}

function r_prim_bhv_to_html_inner(out) {
        var b = this;
        this.depend[out.get().id] = true;

        this.compute = function(x) { return new Thunk(function() {
                $('*[bhv-id='+out.get().id+']').each(function() {
                        var gen = $(this).attr('bhv-gen');
                        // TODO: some error message
                        if (!gen) return;

                        b.gen = gen;
                        b.depend[gen] = true;
                        r_behaviours[gen].rdepend[b.id] = true;
                        // TODO: clear old dependency
                });
                if (!b.gen) return cthunk( { Nothing: null, NotYet: null } );
                return r_behaviours[b.gen].compute(x).get();
        }); }
}

// BehaviourFun a b -> BehaviourFun b c -> BehaviourFun a c
function r_prim_compose(f, g) {
        this.depend[f.get().id] = true;
        this.depend[g.get().id] = true;

        this.compute = function(x) {
                return g.get().compute(f.get().compute(x));
        };
}

// a -> BehaviourFun b a
function r_prim_const(value) {
        this.compute = function() { return value; };
}

function r_prim_debug() {
        this.compute_ = function(x) {
                alert(x[0].get() + ': ' + x[1].get());
                return x[1].get();
        };
}

function r_prim_eq_string() {
        this.compute_ = function(params) {
                return params[0].get() == params[1].get();
        };
}

function r_prim_error() {
        this.compute_ = function(msg) { alert(msg); };
}

/*
function r_prim_fst() {}
*/

function r_prim_gen() {
        this.compute = function() {
                return this.value;
        };

        this.change = function(value) {
                this.value = cthunk(value);
                r_current_time++;
                this.invalidate();
                r_update_html();
        };

}

function r_prim_hask_to_bhv(inner, func) {
        this.depend[func.get().id] = true;
        this.compute = function(x) { return new Thunk(function() {
                // TODO: the value of inner behaviour may be change in the meantime
                inner.get().value = x;
                return func.get().compute(cthunk([])).get();
        }); };
}

function r_prim_hask_to_bhv_inner() {
        this.compute = function() { return this.value; };
}

function r_prim_length() {
        this.compute_ = function(list) {
                return list.length;
        }
}

function r_prim_lookup() {
        this.compute_ = function(param) {
                var key = param[0].get();
                var list = param[1].get();
                for (i in list) {
                        if (list[i].get()[0].get() == key)
                                return { Just: list[i].get()[1] };
                }
                return { Nothing: null };
        };
}

function r_prim_lt_int() {
        this.compute_ = function(params) {
                return params[0].get() < params[1].get();
        }
}

function r_prim_plus() {
        this.compute_ = function(x) { return x[0].get()+x[1].get(); }
}

function r_prim_plus_mb() {
        this.compute_ = function(x) {
                if (typeof x[0].get().Just != 'undefined' && typeof x[1].get().Just != 'undefined')
                        return { Just: cthunk (x[0].get().Just.get() + x[1].get().Just.get()) };
                return { Nothing: null };
        }
}

function r_prim_product(f, g) {
        this.depend[f.get().id] = true;
        this.depend[g.get().id] = true;
        this.compute = function(x) { return new Thunk(function() {
                return [f.get().compute(x), g.get().compute(x)];
        }); };
}

function r_prim_sget(name) {
        this.values = [];
        var b = this;

        this.compute_ = function(x) {
                for (i in this.values) {
                        if (this.values[i][0] == x) {
                                return { Just: cthunk( this.values[i][1] ) };
                        }
                }

                $.get(document.location.href, { q: name.get() }, function(json) {
                        b.values.push([x, jQuery.parseJSON(json)]);
                        r_current_time++;
                        b.invalidate();
                        r_update_html();
                });

                return { Nothing: null };
        };
}

function r_prim_spost(name) {
        var results = {};
        var b = this;

        this.compute = function(x) { return new Thunk(function() {
                if (typeof x.get().Timed == 'undefined')
                        return { Nothing: null };

                var time = x.get().Timed[0];
                var pairs = x.get().Timed[1].get();

                if (typeof results[time] != 'undefined')
                        return results[time]
                results[time] = { Nothing: null };

                var params = {};
                for (i in pairs)
                        params[pairs[i].get()[0].get()] = pairs[i].get()[1].get();

                $.post(document.location.href+'?q='+name.get(), params, function(json) {
                        results[time] = { Just: cthunk( jQuery.parseJSON(json) ) };
                        r_current_time++;
                        b.invalidate();
                        r_update_html();
                });

                return { Nothing: null };
        }); };
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
        this.compute = function(params) { return new Thunk(function() {
                var def = params.get()[0];
                var unt = params.get()[1];

                if (typeof unt.get().Just != 'undefined')
                        return unt.get().Just.get();
                return def.get();
        }); };
}



/* Bool constructors and destructor */

function r_prim_true() {
        this.compute_ = function() { return true; };
}

function r_prim_false() {
        this.compute_ = function() { return false; };
}

function r_prim_bool() {
        this.compute = function(params) { return new Thunk(function() {
                var t = params.get()[0];
                var f = params.get()[1].get()[0];
                var c = params.get()[1].get()[1];
                if (c.get()) return t.get();
                return f.get();
        }); };
}


/* Maybe constructors and destructor */

function r_prim_nothing() {
        this.compute = function() { return cthunk({ Nothing: null }); };
}

function r_prim_just() {
        this.compute = function(x) { return cthunk( { Just: x } ); };
}

function r_prim_maybe() {
        this.compute = function(params) { return new Thunk(function() {
                var def = params.get()[0];
                var func = params.get()[1].get()[0];
                var mb = params.get()[1].get()[1];

                if (typeof mb.get().Just == 'undefined')
                        return def.get();
                return func.get().compute(mb.get().Just).get();
        }); };
}


/* Timed constructors and destructor */

function r_prim_not_yet() {
        this.compute = function() { return cthunk({ NotYet: null }); };
}

function r_prim_on_time() {
        this.compute = function(x) { return cthunk( { Timed: [ ++r_current_time, x ] } ); };
}

function r_prim_timed() {
        this.compute = function(params) { return new Thunk(function() {
                var not_yet = params.get()[0];
                var on_time = params.get()[1].get()[0];
                var value = params.get()[1].get()[1];

                if (typeof value.get().Timed == 'undefined')
                        return not_yet.get();
                return on_time.get().compute(value.get().Timed[1]).get();
        }); };
}
