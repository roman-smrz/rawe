var r_bhv_func = {};
var r_srv_deps = {};
var r_srv_val = {};
var r_srv_expr = {};
var r_event_deps = {};
var r_cur_bhv = null;

function r_init() {
        $(document).ready(function() {
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
        });
}


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
