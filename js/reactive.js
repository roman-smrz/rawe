var r_bhv_func = {};
var r_srv_deps = {};
var r_srv_val = {};
var r_srv_expr = {};
var r_event_deps = {};
var r_cur_bhv = null;


var r_behaviours = {};
var r_invalid_html = [];
var r_current_time = 0;
var r_init_html_funs = [];


function Behaviour(id) {
        this.id = id;
        this.depend = {};
        this.rdepend = {};
        this.valid = true;
        this.last_change = 0;

        /*
        this.change = function(value) {
                if (this.value == value)
                        return;

                this.invalidate();
                this.value = value;
                this.valid = true;

                for (i in r_invalid)
                        r_behaviours[r_invalid[i]].recompute();
                r_invalid = [];
        }
        */

        this.compute = function(x) {
                if (typeof x != 'undefined')
                        return this.compute_(x);
        }

        this.invalidate = function() {
                if (this.last_change == r_current_time)
                        return;

                this.valid = false;
                this.last_change = r_current_time;

                if (this.html) {
                        r_invalid_html.push(this);
                }

                for (i in this.rdepend)
                        r_behaviours[i].invalidate();
        }

        /*
        this.recompute = function() {
                if (this.valid) return;

                if (i in this.depend)
                        r_behaviours[i].recompute();
                this.value = this.compute();
                this.valid = true;
        }
        */
}


function r_init() {
        $(document).ready(function() {
                r_init_html_funs.push(function(elems) {
                        elems.find('*[bhv-id]').each(function() {
                                r_invalid_html.push(r_behaviours[$(this).attr('bhv-id')]);
                        });
                        elems.add(elems.find('*[bhv-gen]')).filter('*[bhv-gen]').each(function() {
                                r_init_gen(r_behaviours[$(this).attr('bhv-gen')], $(this));
                        });
                });

                for (id in r_behaviours) {
                        var b = r_behaviours[id];
                        for (did in b.depend) {
                                r_behaviours[did].rdepend[id] = true;
                        }
                }

                for (i in r_init_html_funs) r_init_html_funs[i]($(document));
                for (i in r_behaviours) {
                        var b = r_behaviours[i];
                        if (b.html) r_invalid_html.push(b);
                }
                r_update_html();

                /*
                for (bhv in r_bhv_func)
                        r_bhv_update(bhv);

                for (srv in r_srv_expr) {
                        (function(srv, ids) {
                                $.get(document.location.href, { q: srv },
                                        function(json) {
                                                r_srv_val[srv] = jQuery.parseJSON(json)
                                                for (bhv in ids) r_bhv_update(bhv);
                                        });
                        })(srv, r_srv_deps[srv]);
                }
                */
        });
}

function r_update_html() {
        while (b = r_invalid_html.shift()) {
                var value = b.compute(null);
                if (typeof value == 'undefined')
                        value = $('<div></div>');

                var first = true;
                $('*[bhv-id="'+b.id+'"]').each(function() {
                        if (first) {
                                var newNode = value.clone();
                                newNode.each(function() { $(this).attr('bhv-id', ''+b.id); });
                                newNode.find('input:text').each(function() {
                                        var gen = $(this).attr('bhv-gen');
                                        if (gen) $(this).val(r_behaviours[gen].value);
                                });

                                for (i in r_init_html_funs) r_init_html_funs[i](newNode);
                                $(this).replaceWith(newNode);
                                first = false;
                        } else {
                                $(this).remove();
                        }
                });
        }
}




function r_init_gen(b, elem) {
        if (elem.is('form')) {
                if (typeof b.value == 'undefined')
                        b.value = { NotYet: null };
                elem.submit(function(e) {
                        e.preventDefault();
                        var result = [];
                        elem.find('input, select, textarea').each(function() {
                                result.push([$(this).attr('name'), $(this).val()]);
                        });
                        b.change({ Timed: [++r_current_time, result] });
                });
        }

        if (elem.is('input:text')) {
                b.value = elem.val();
                elem.change(function(e) {
                        b.change(elem.val());
                });
                elem.keyup(function(e) {
                        b.change(elem.val());
                });
        }
}


/*

function r_compose(f, g) {
        var result = function(param) {
                if (typeof param == 'undefined') return undefined;
                param = g(param);
                if (typeof param == 'undefined') return undefined;
                return f(param);
        }
        return result;
}

function r_product(f, g) {
        return (function(param) {
                if (typeof param == 'undefined')
                        return undefined;
                x = f(param); y = g(param);
                if (typeof x == 'undefined' || typeof y == 'undefined')
                        return undefined;
                return [x, y];
        });
}

function r_bhv_update(bhv) {
        r_cur_bhv = bhv;
        var value = r_bhv_func[bhv](null);
        if (typeof value == 'undefined')
                return;

        var first = true
        $('*[bhv-id="'+bhv+'"]').each(function() {
                if (first) {
                        var newNode = value.clone();
                        newNode.each(function() { $(this).attr('bhv-id', ''+bhv); });
                        $(this).replaceWith(newNode);
                        first = false;
                } else {
                        $(this).remove();
                }
        });
}

function r_srv_call(name) {
        if (r_srv_deps[name] == undefined)
                r_srv_deps[name] = {};
        r_srv_deps[name][r_cur_bhv] = true;
        r_srv_expr[name] = name;

        return r_srv_val[name];
}


function r_bhv_init(id, rdep) {
        if (r_bhv_rdeps[id] == undefined)
                r_bhv_rdeps[id] = new Array();
        r_bhv_rdeps[id].push(rdep);

        if (r_bhv_time[id] != undefined)
                return;
        r_bhv_time[id] = 0;
        
        for (i in r_bhv_deps[id]) {
                dep = r_bhv_deps[id][i];
                r_bhv_init(dep, id);
        }
}


function call_event(id, value) {

}

function r_prim_toHtmlHtmlList(param) {
        var result = $('<div></div>');
        for (i in param) result.append(param[i].clone());
        return result.children();
}

function r_prim_enumFromTo(param) {
        var result = new Array();
        for (var i = param[0]; i <= param[1]; i++)
                result.push(i);
        return result;
}
*/
