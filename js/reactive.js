var r_bhv_deps = {};
var r_bhv_rdeps = {};
var r_bhv_func = {};
var r_bhv_srv = {};
var r_bhv_val = {};
var r_bhv_time = {};
var r_srv_bhv = {};

function r_init() {
        $(document).ready(function() {
                for (id in r_bhv_func) r_bhv_init(id);
                for (var id in r_bhv_srv) {
                        (function(id) {
                                $.get(document.location.href, { q: r_bhv_srv[id] },
                                        function(json) { r_bhv_set(id, jQuery.parseJSON(json)); });
                        })(id);
                        r_srv_bhv[r_bhv_srv[id]] = id;
                }
                for (id in r_bhv_val) r_bhv_set(id, r_bhv_val[id]);
        });
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

function r_bhv_update(id) {
        for (i in r_bhv_deps[id]) {
                if (r_bhv_time[r_bhv_deps[id][i]] > r_bhv_time[id]) {
                        r_bhv_set(id, r_bhv_func[id]());
                        return;
                }
        }
}

function r_bhv_set(id, value) {
        r_bhv_val[id] = value;
        r_bhv_time[id] = (new Date()).getTime();

        var first = true
        $('*[bhv-id="'+id+'"]').each(function() {
                if (first) {
                        var newNode = value.clone();
                        newNode.each(function() { $(this).attr('bhv-id', ''+id); });
                        $(this).replaceWith(newNode);
                        first = false;
                } else {
                        $(this).remove();
                }
        });

        for (i in r_bhv_rdeps[id])
                r_bhv_update(r_bhv_rdeps[id][i]);
}

function r_srv_val(name) {

}

function call_event(id, value) {

}

function r_toHtmlHtmlList(param) {
        var result = $('<div></div>');
        for (i in param) result.append(param[i].clone());
        return result.children();
}
