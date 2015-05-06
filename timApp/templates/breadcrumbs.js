function splitPath(path) {
    var result = [];
    var relstart = 0;
    for (i = 0; i < path.length; i++) {
        if (path[i] == '/') {
            result.push({name: path.substr(relstart, i - relstart), fullname: path.substr(0, i)});
            relstart = i + 1;
        }
    }
    if (relstart < i)
        result.push({name: path.substr(relstart, i), fullname: path.substr(0, i)});
    return result;
}

function getBreadcrumbs(folder) {
   var crumbs = splitPath(folder);
   crumbs.unshift({name: '[root]', fullname: ''});
   return crumbs;
}
