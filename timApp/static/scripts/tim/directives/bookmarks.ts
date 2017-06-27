import angular from "angular";
import {timApp} from "tim/app";
import * as focusMe from "tim/directives/focusMe";
import {markAsUsed} from "tim/utils";
import {$http, $timeout, $uibModal, $window} from "../ngimport";

markAsUsed(focusMe);

class BookmarksController {
    private data: {name, isOpen, items: {}[]}[];
    private deleting: boolean;
    private userId: number;

    constructor() {
        if ($window.bookmarks && !this.data) {
            this.data = angular.copy($window.bookmarks);
        }
        this.deleting = false;

        if (this.userId && !this.data) {
            (async () => {
                const response = await $http.get("/bookmarks/get/" + this.userId);
                this.getFromServer(response);
            })();
        }
    }

    getFromServer(response, groupToKeepOpen?) {
        this.data = response.data;
        this.keepGroupOpen(groupToKeepOpen);
    }

    keepGroupOpen(groupToKeepOpen) {
        if (!groupToKeepOpen) {
            return;
        }
        for (let i = 0; i < this.data.length; ++i) {
            if (this.data[i].name === groupToKeepOpen.name) {
                this.data[i].isOpen = true;
                return;
            }
        }
    }

    getTopLevelBookmarks() {
        if (!this.data) {
            return [];
        }
        for (let i = 0; i < this.data.length; ++i) {
            if (this.data[i].name === "") {
                return this.data[i].items;
            }
        }
        return [];
    }

    isSaveablePage() {
        return true;
    }

    newBookmark(group, e) {
        e.preventDefault();
        const suggestedName = ($window.item || {}).title || document.title;
        const modalInstance = $uibModal.open({
            animation: false,
            ariaLabelledBy: "modal-title",
            ariaDescribedBy: "modal-body",
            templateUrl: "createBookmark.html",
            controller: "CreateBookmarkCtrl",
            controllerAs: "$ctrl",
            size: "md",
            resolve: {
                bookmark() {
                    return {
                        group: group || "",
                        name: suggestedName,
                        link: "",
                    };
                },
            },
        });

        modalInstance.result.then((bookmark) => {
            if (!bookmark.name) {
                return;
            }
            $http.post("/bookmarks/add", bookmark)
                .then(this.getFromServer, (response) => {
                    $window.alert("Could not add bookmark.");
                });
        }, () => {
        });
    }

    editItem(group, item, e) {
        e.stopPropagation();
        e.preventDefault();
        const modalInstance = $uibModal.open({
            animation: false,
            ariaLabelledBy: "modal-title",
            ariaDescribedBy: "modal-body",
            templateUrl: "createBookmark.html",
            controller: "CreateBookmarkCtrl",
            controllerAs: "$ctrl",
            size: "md",
            resolve: {
                bookmark() {
                    return {
                        group: group.name,
                        name: item.name,
                        link: item.path,
                    };
                },
            },
        });

        modalInstance.result.then(function(bookmark) {
            if (!bookmark.name) {
                return;
            }
            $http.post("/bookmarks/edit", {
                old: {
                    group: group.name,
                    name: item.name,
                    link: item.path,
                }, new: bookmark,
            })
                .then((response) => {
                    this.getFromServer(response, group);
                }, response => {
                    $window.alert("Could not edit bookmark.");
                });
        }, () => {
            $timeout(() => {
                this.keepGroupOpen(group);
            }, 0);
        });
    }

    deleteItem(group, item, e) {
        e.stopPropagation();
        e.preventDefault();
        return $http.post("/bookmarks/delete", {
            group: group.name,
            name: item.name,
        })
            .then((response) => {
                this.getFromServer(response, group);
            }, (response) => {
                $window.alert("Could not delete bookmark.");
            });
    }

    deleteGroup(group, e) {
        e.stopPropagation();
        e.preventDefault();
        if ($window.confirm("Are you sure you want to delete this bookmark group?")) {
            $http.post("/bookmarks/deleteGroup", {group: group.name})
                .then(this.getFromServer, response => {
                    $window.alert("Could not delete bookmark group.");
                });
        }
    }

    toggleDelete(e) {
        e.stopPropagation();
        e.preventDefault();
        this.deleting = !this.deleting;
    }
}

timApp.component("bookmarks", {
    bindings: {
        data: "=?",
        userId: "=?",
    },
    controller: BookmarksController,
    templateUrl: "/static/templates/bookmarks.html",
});

class CreateBookmarkCtrl {
    private static $inject = ["bookmark", "$uibModalInstance"];

    private bookmarkForm: {};
    private focusName: boolean;
    private focusGroup: boolean;
    private showParamsCheckbox: boolean;
    private showHashCheckbox: boolean;
    private bookmark: {link: string};
    private includeParams: boolean;
    private includeHash: boolean;
    private uibModalInstance: angular.ui.bootstrap.IModalServiceInstance;

    constructor(bookmark, uibModalInstance: angular.ui.bootstrap.IModalServiceInstance) {
        this.uibModalInstance = uibModalInstance;
        this.bookmarkForm = {};
        this.bookmark = bookmark;
        if (bookmark.group === "Last edited" || bookmark.group === "Last read") {
            bookmark.group = "";
        }
        this.focusGroup = !bookmark.group;
        this.focusName = !this.focusGroup;
        this.showParamsCheckbox = $window.location.search.length > 1;
        this.showHashCheckbox = $window.location.hash.length > 1;
    }

    public ok() {
        if (!this.bookmark.link) {
            this.bookmark.link = $window.location.pathname;
            if (this.includeParams) {
                this.bookmark.link += $window.location.search;
            }
            if (this.includeHash) {
                this.bookmark.link += $window.location.hash;
            }
        }

        this.uibModalInstance.close(this.bookmark);
    }

    public cancel() {
        this.uibModalInstance.dismiss("cancel");
    }
}

timApp.controller("CreateBookmarkCtrl", CreateBookmarkCtrl);
