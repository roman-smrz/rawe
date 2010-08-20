var r_bhv_deps = {};
var r_bhv_rdeps = {};
var r_bhv_func = {};
var r_bhv_val = {};

function r_init() {
        $(document).ready(function() {
                for (id in r_bhv_func)
                        r_bhv_init(id);

                $('div.bhv-placeholder').each(function() {
                        var id = $(this).attr('bhv-id');
                        var newNode = r_bhv_val[id].clone();
                        newNode.each(function() { $(this).attr('bhv-id', ''+id); });
                        $(this).replaceWith(newNode);
                });
        });
}

function r_bhv_init(id, rdep) {
        if (r_bhv_rdeps[id] == undefined)
                r_bhv_rdeps[id] = new Array();
        r_bhv_rdeps[id].push(rdep);
        if (r_bhv_val[id] != undefined)
                return;
        
        for (i in r_bhv_deps[id]) {
                dep = r_bhv_deps[id][i];
                if (r_bhv_val[dep] == undefined)
                        r_bhv_init(dep, id);
        }
        r_bhv_val[id] = r_bhv_func[id]();
}

function call_event(id, value) {

}

function r_toHtmlHtmlList(param) {
        var result = $('<div></div>');
        for (i in param) result.append(param[i].clone());
        return result.children();
}
