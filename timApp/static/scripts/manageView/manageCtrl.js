var PermApp = angular.module('permApp', ['ngSanitize', 'angularFileUpload']);

// from https://stackoverflow.com/questions/14833326
PermApp.directive('focusMe', function ($timeout, $parse) {
    return {
        link: function (scope, element, attrs) {
            var model = $parse(attrs.focusMe);
            scope.$watch(model, function (value) {
                if (value === true) {
                    $timeout(function () {
                        element[0].focus();
                    });
                }
            });
            element.bind('blur', function () {
                scope.$apply(model.assign(scope, false));
            });
        }
    };
});

PermApp.controller("PermCtrl", [
    '$scope',
    '$http',
    '$upload',
    '$window',
    function (sc, $http, $upload, $window) {
        sc.wikiRoot = "https://trac.cc.jyu.fi/projects/ohj2/wiki/"; // Todo: replace something remembers users last choice
        $http.defaults.headers.common.Version = function() {
            if ('versions' in sc.doc && sc.doc.versions.length > 0 && 'hash' in sc.doc.versions[0]) {
                return sc.doc.versions[0];
            }
            return ""; 
        };

        sc.getJustDocName = function(fullName) {
            i = fullName.lastIndexOf('/');
            return i < 0 ? fullName : fullName.substr(i + 1);
        };

        sc.getFolderName = function(fullName) {
            i = fullName.lastIndexOf('/');
            return i < 0 ? '' : fullName.substring(0, fullName.lastIndexOf('/'));
        };

        sc.getAliases = function() {
            $http.get('/alias/' + sc.doc.id, {
            }).success(function (data, status, headers, config) {
                if (sc.aliases.length > 0 &&
                    (data.length == 0 || data[0].fullname != sc.aliases[0].fullname))
                        // The first name has changed, reload to update the links
                        location.reload();
                else
                    sc.aliases = data;

            }).error(function (data, status, headers, config) {
                alert(data.message);
            });

            return [];
        };

        sc.showAddRightFn = function (type) {
            sc.accessType = type;
            sc.showAddRight = true;
            sc.focusEditor = true;
        };

        sc.changeOwner = function() {
            sc.ownerUpdating = true;
            $http.put('/changeOwner/' + sc.doc.id + '/' + sc.doc.owner.id).success(
                function (data, status, headers, config) {

                }).error(function (data, status, headers, config) {
                    alert(data.error);
                }).then(function () {
                    sc.ownerUpdating = false;
                });
        };

        sc.removeConfirm = function (group, type) {
            if (confirm("Are you sure you want to remove this usergroup?")) {
                sc.removePermission(group, type);
            }
        };

        sc.getPermissions = function () {
            $http.get('/getPermissions/' + sc.doc.id).success(function (data, status, headers, config) {
                sc.grouprights = data.grouprights;
            }).error(function (data, status, headers, config) {
                alert("Could not fetch permissions.");
            });
        };

        sc.removePermission = function (right, type) {
            $http.put('/removePermission/' + sc.doc.id + '/' + right.gid + '/' + type).success(
                function (data, status, headers, config) {
                    sc.getPermissions();
                }).error(function (data, status, headers, config) {
                    alert(data.error);
                });
        };

        sc.addPermission = function (groupname, type) {
            console.log("/addPermission/" + sc.doc.id + "/" + groupname + "/" + type.name);
            $http.put('/addPermission/' + sc.doc.id + '/' + groupname + '/' + type.name).success(
                function (data, status, headers, config) {
                    sc.getPermissions();
                    sc.showAddRight = false;
                }).error(function (data, status, headers, config) {
                    alert(data.error);
                });
        };

        sc.renameFolder = function (newName) {
            $http.put('/rename/' + sc.doc.id, {
                'new_name': sc.oldFolderName + '/' + newName
            }).success(function (data, status, headers, config) {
                // This is needed to update the breadcrumbs
                location.reload();
                //sc.doc.fullname = sc.oldFolderName + '/' + newName;
                //sc.doc.name = newName;
                //sc.oldName = newName;
            }).error(function (data, status, headers, config) {
                alert(data.message);
            });
        };

        sc.moveFolder = function (newLocation) {
            $http.put('/rename/' + sc.doc.id, {
                'new_name': newLocation + '/' + sc.oldName
            }).success(function (data, status, headers, config) {
                // This is needed to update the breadcrumbs
                location.reload();
                //sc.doc.fullname = newLocation + '/' + sc.oldName;
                //sc.doc.location = newLocation;
                //sc.oldFolderName = newLocation;
            }).error(function (data, status, headers, config) {
                alert(data.message);
            });
        };

        sc.combine = function(folder, name) {
            return (folder + '/' + name).replace(/(^\/+)|(\/+$)/, '')
        };

        sc.aliasPublicClicked = function(alias) {
            alias.public = !alias.public;
            alias.publicChanged = !alias.publicChanged;
        };

        sc.aliasChanged = function(alias) {
            return alias.publicChanged || alias.fullname != sc.combine(alias.location, alias.name);
        };

        sc.addAlias = function(newAlias) {
            $http.put('/alias/' + sc.doc.id + '/' + sc.combine(newAlias.location, newAlias.name), {
                'public': Boolean(newAlias.public)
            }).success(function (data, status, headers, config) {
                sc.getAliases();
            }).error(function (data, status, headers, config) {
                alert(data.message);
            });

            sc.newAlias = {location: sc.oldFolderName};
        };

        sc.removeAlias = function(alias) {
            $http.delete('/alias/' + sc.doc.id + '/' + alias.fullname, {
            }).success(function (data, status, headers, config) {
                sc.getAliases();
            }).error(function (data, status, headers, config) {
                alert(data.message);
            });
        };

        sc.updateAlias = function(alias) {
            $http.post('/alias/' + sc.doc.id + '/' + alias.fullname, {
                'public': Boolean(alias.public),
                'new_name': sc.combine(alias.location, alias.name)
            }).success(function (data, status, headers, config) {
                sc.getAliases();
            }).error(function (data, status, headers, config) {
                alert(data.message);
            });
        };

        sc.deleteDocument = function (doc) {
            if (confirm('Are you sure you want to delete this document?')) {
                $http.delete('/documents/' + doc)
                    .success(function (data, status, headers, config) {
                        location.replace('/view/');
                    }).error(function (data, status, headers, config) {
                        alert(data.message);
                    });
            }
        };

        sc.deleteFolder = function (folder) {
            if (confirm('Are you sure you want to delete this folder?')) {
                $http.delete('/folders/' + folder)
                    .success(function (data, status, headers, config) {
                        location.replace('/view/');
                    }).error(function (data, status, headers, config) {
                        alert(data.message);
                    });
            }
        };

        sc.onFileSelect = function (url, $files) {
            // $files: an array of files selected, each file has name, size,
            // and type.
            sc.progress = 'Uploading... ';
            sc.uploadInProgress = true;
            for (var i = 0; i < $files.length; i++) {
                var file = $files[i];
                sc.upload = $upload.upload({
                    url: url,
                    method: 'POST',
                    file: file
                }).progress(function (evt) {
                    sc.progress = 'Uploading... ' + parseInt(100.0 * evt.loaded / evt.total) + '%';
                }).success(function (data, status, headers, config) {
                    sc.doc.versions = data;
                    $http.get('/download/' + sc.doc.id).success(function (data) {
                        sc.doc.fulltext = data;
                        sc.fulltext = data;
                        sc.progress = 'Uploading... Done!';
                    })
                }).error(function (data, status, headers, config) {
                    sc.progress = 'Error occurred: ' + data.error;
                }).then(function () {
                    sc.uploadInProgress = false;
                });
            }
        };

        sc.updateDocument = function (doc, $files) {
            sc.onFileSelect('/update/' + doc.id + '/' + doc.versions[0], $files);
        };

        sc.convertDocument = function (doc) {
            var text = sc.tracWikiText;
            wikiSource = sc.wikiRoot.replace("wiki","browser");
            //var text = sc.fulltext;
            text = text.replace(/\[\[PageOutline\]\].*\n/g,"");  // remove contents
            text = text.replace(/ !([a-zA-Z])/g," $1");                   // remove cat !IsBig
            text = text.replace(/\r\n/g,"\n");                   // change windows nl's
            text = text.replace(/{{{(.*?)}}}/g, "`$1`");         // change verbatim 
            // text = text.replace(/{{{\n(.*?)\n}}}/, "```\n$1\n```"); // change code blocks
            text = text.replace(/\n{{{#!comment\n((.|\s|\S)*?)\n}}}\n/g, "\n#- {.comment}\n$1\n#-\n"); // comments
            text = text.replace(/\n{{{\n/g, "\n#-\n```\n"); // change code blocks
            text = text.replace(/\n}}}\n/g, "\n```\n"); // change code blocks
            text = text.replace(/\n====\s+(.*?)\s+====.*\n/g, '\n#-\n#### $1\n'); // headings
            text = text.replace(/\n===\s+(.*?)\s+===.*\n/g, '\n#-\n### $1\n');
            text = text.replace(/\n==\s+(.*?)\s+==.*\n/g, '\n#-\n## $1\n');
            text = text.replace(/\n=\s+(.*?)\s+=.*\n/g, '\n#-\n# $1\n');
            // text = text.replace(/^ \d+. ', r'1.'); 
            lines = text.split("\n");
            for (var i=0; i<lines.length; i++) {
                var line = lines[i];
                if ( true || line.lastIndexOf('    ',0) !== 0 ) {
                    line = line.replace(/\[((https?|mms):\/\/[^\s\[\]]+)\s+([^\[\]]+)\]/g, '[$3]($1)'); // ordinary links
                    line = line.replace(/\[((https?|mms):\/\/[^\s\[\]]+)\s+(\[.*?\])\]/g, '[$3]($1)');   // links like [url [text]]
                    line = line.replace(/\[wiki:([^\s\[\]]+)\]/g, '[$1]('+(sc.wikiRoot || "")+'$1)'); // [wiki:local] -> [local](URL)
                    line = line.replace(/\[wiki:([^\s\[\]]+)\s+([^\[\]]+)\]/g, '[$2]('+(sc.wikiRoot || "")+'$1)'); // [wiki:local text] -> [text](URL)
                    line = line.replace(/\[source:([^\s\[\]]+)\s+([^\[\]]+)\]/g, '[$2]('+(wikiSource || "")+'$1)');
                    line = line.replace(/\!(([A-Z][a-z0-9]+){2,})/, '$1');
                    line = line.replace(/\'\'\'(.*?)\'\'\'/, '**$1**');  // bold
                    line = line.replace(/\'\'(.*?)\'\'/, '*$1*');   // italics
                }
                lines[i] = line;
            }
            text = lines.join("\n");

/*            

text = re.sub(r'(?m)^====\s+(.*?)\s+====.*$', r'#-\n#### \1', text)
text = re.sub(r'(?m)^===\s+(.*?)\s+===.*$', r'#-\n### \1', text)
text = re.sub(r'(?m)^==\s+(.*?)\s+==.*$', r'#-\n## \1', text)
text = re.sub(r'(?m)^=\s+(.*?)\s+=.*$', r'#-\n# \1', text)
#text = re.sub(r'^       * ', r'****', text)
#text = re.sub(r'^     * ', r'***', text)
#text = re.sub(r'^   * ', r'**', text)
#text = re.sub(r'^ * ', r'*', text)
text = re.sub(r'^ \d+. ', r'1.', text)

a = []
for line in text.split('\n'):
    if True or not line.startswith('    '):
        line = re.sub(r'\[(https?://[^\s\[\]]+)\s([^\[\]]+)\]', r'[\2](\1)', line)
        line = re.sub(r'\[(wiki:[^\s\[\]]+)\s([^\[\]]+)\]', r'[\2](/\1/)', line)
        line = re.sub(r'\!(([A-Z][a-z0-9]+){2,})', r'\1', line)
        line = re.sub(r'\'\'\'(.*?)\'\'\'', r'*\1*', line)
        line = re.sub(r'\'\'(.*?)\'\'', r'_\1_', line)
        line = re.sub(r'\(/wiki:', r'(https://trac.cc.jyu.fi/projects/ohj2/wiki/', line)
    a.append(line)
text = '\n'.join(a)
*/
            //sc.tracWikiText = text;
            sc.fulltext = text;
        }

        
        sc.saveDocument = function (doc) {
            sc.saving = true;
            $http.post('/update/' + doc.id + '/' + doc.versions[0],
                {
                    'fulltext': sc.fulltext,
                    'original': sc.doc.fulltext
                }).success(
                function (data, status, headers, config) {
                    sc.fulltext = data.fulltext;
                    sc.doc.fulltext = sc.fulltext;
                    sc.doc.versions = data.versions;
                    sc.saving = false;
                }).error(function (data, status, headers, config) {
                    sc.saving = false;
                    console.log(data);
                    if ('is_warning' in data && data.is_warning) {
                        if ( confirm(data.error + "\n\nDo you still wish to save the document?") ) {
                            sc.saveDocumentWithWarnings(doc);
                        }
                    }
                    else {
                        alert(data.error);
                    }
                });
        };

        sc.saveDocumentWithWarnings = function (doc) {
            sc.saving = true;
            $http.post('/update/' + doc.id + '/' + doc.versions[0],
                {
                    'fulltext': sc.fulltext,
                    'original': sc.doc.fulltext,
                    'ignore_warnings': true
                }).success(
                function (data, status, headers, config) {
                    sc.fulltext = data.fulltext;
                    sc.doc.fulltext = sc.fulltext;
                    sc.doc.versions = data.versions;
                }).error(function (data, status, headers, config) {
                    alert(data.error);
                }).finally(function () {
                    sc.saving = false;
                });
        };

        sc.markAllAsRead = function() {
            sc.readUpdating = true;
            $http.put('/read/' + sc.doc.id + '?_=' + Date.now())
                .success(function (data, status, headers, config) {

                }).error(function (data, status, headers, config) {
                    $window.alert('Could not mark the document as read.');
                }).finally(function (data, status, headers, config) {
                    sc.readUpdating = false;
                });
        };

        sc.toggleDiv = function(divName) {
            if (sc.showCreateDiv == divName)
                sc.showCreateDiv = "";
            else
                sc.showCreateDiv = divName;
        };

        sc.createTranslation = function() {
            $http.put('/translate/' + sc.doc.id + "/" + sc.translationName + '?_=' + Date.now())
                .success(function (data, status, headers, config) {
                    location.assign("/view/" + data.name);
                }).error(function (data, status, headers, config) {
                    $window.alert('Could not create a translation. Error message is: ' + data.error);
                }).finally(function (data, status, headers, config) {
                });
        };

        sc.createCitation = function() {
            $http.put('/cite/' + sc.doc.id + "/" + sc.citationName + '?_=' + Date.now())
                .success(function (data, status, headers, config) {
                    location.assign("/view/" + data.name);
                }).error(function (data, status, headers, config) {
                    $window.alert('Could not create a translation. Error message is: ' + data.error);
                }).finally(function (data, status, headers, config) {
                });
        };

        sc.grouprights = grouprights;
        sc.userGroups = groups;
        sc.accessTypes = accessTypes;
        sc.accessType = sc.accessTypes[0];
        sc.doc = doc;
        sc.crumbs = crumbs;
        sc.isFolder = isFolder;
        //sc.newName = sc.getJustDocName(doc.name);
        //sc.newFolderName = sc.getFolderName(doc.name);
        //sc.oldName = sc.newName;
        //sc.oldFolderName = sc.newFolderName;
        sc.newAlias = {location: sc.newFolderName};
        doc.fulltext = doc.fulltext.trim();
        sc.fulltext = doc.fulltext;
        sc.aliases = sc.getAliases();
        sc.showCreateDiv = "";

        if (isFolder) {
            sc.newName = doc.name;
            sc.newFolderName = doc.location;
            sc.oldName = sc.newName;
            sc.oldFolderName = sc.newFolderName;
        }
    }]);
