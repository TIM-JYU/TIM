import {IController, IScope} from "angular";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {timApp} from "../app";
import {IFolder, IItem} from "../item/IItem";
import {$http} from "../util/ngimport";
import {Binding, to} from "../util/utils";

type PreviewList = Array<{from: string; to: string}>;

interface PreviewResponse {
    preview: PreviewList;
    dest_exists: boolean;
}

class CopyFolderCtrl implements IController {
    static $inject = ["$scope"];
    private copyingFolder: "notcopying" | "copying" | "finished";
    private item!: Binding<IItem, "<">;
    private copyPreviewList: PreviewList | undefined;
    private scope: IScope;
    private destExists?: boolean;
    private copyFolderPath: string | undefined;
    private copyFolderExclude: string;
    private newFolder?: IFolder;

    constructor(scope: IScope) {
        this.scope = scope;
        this.copyingFolder = "notcopying";
        this.copyFolderExclude = "";
    }

    $onInit() {
        this.copyFolderPath = this.item.path;
    }

    async copyFolderPreview(path: string, exclude: string) {
        this.copyingFolder = "notcopying";
        const r = await to(
            $http.post<PreviewResponse>(`/copy/${this.item.id}/preview`, {
                destination: path,
                exclude: exclude,
            })
        );
        if (r.ok) {
            this.copyPreviewList = r.result.data.preview;
            this.destExists = r.result.data.dest_exists;
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    async copyFolder(path: string, exclude: string) {
        this.copyingFolder = "copying";
        const r = await to(
            $http.post<IFolder>(`/copy/${this.item.id}`, {
                destination: path,
                exclude: exclude,
            })
        );
        if (r.ok) {
            this.copyingFolder = "finished";
            this.copyPreviewList = undefined;
            this.destExists = undefined;
            this.newFolder = r.result.data;
        } else {
            this.copyingFolder = "notcopying";
            await showMessageDialog(r.result.data.error);
        }
    }

    copyParamChanged() {
        this.copyPreviewList = undefined;
        this.destExists = undefined;
    }
}

timApp.component("timCopyFolder", {
    bindings: {
        item: "<",
    },
    controller: CopyFolderCtrl,
    template: `
<form name="copyForm">
    <p>You can copy all documents and folders in this folder to another folder.</p>
    <div class="form-group" tim-error-state>
        <label for="destination" class="control-label">Destination:</label>
        <input name="copyPath" class="form-control" tim-location id="destination" type="text" autocomplete="off"
               ng-model="$ctrl.copyFolderPath" ng-change="$ctrl.copyParamChanged()">
        <tim-error-message></tim-error-message>
    </div>
    <p>You can optionally exclude some documents/folders from being copied.</p>
    <div class="form-group" tim-error-state>
        <label for="exclude" class="control-label">Exclude documents/folders that match:</label>
        <input name="exclude" class="form-control" id="exclude" type="text" autocomplete="off"
               ng-model="$ctrl.copyFolderExclude" ng-change="$ctrl.copyParamChanged()">
        <tim-error-message></tim-error-message>
    </div>
    <button ng-click="$ctrl.copyFolderPreview($ctrl.copyFolderPath, $ctrl.copyFolderExclude)" class="timButton"
            ng-disabled="$ctrl.copyFolderPath === $ctrl.item.path ||
            copyForm.copyPath.$invalid"
            ng-show="$ctrl.copyingFolder === 'notcopying'">Copy preview...
    </button>
    <ul ng-show="$ctrl.copyPreviewList.length > 0">
        <li ng-repeat="p in $ctrl.copyPreviewList"><span ng-bind="p.from"></span> <i
                class="glyphicon glyphicon-arrow-right"></i> <span ng-bind="p.to"></span></li>
    </ul>
    <p ng-show="$ctrl.copyPreviewList.length === 0">Nothing would be copied.</p>
    <tim-alert severity="warning" ng-show="$ctrl.destExists">
        The destination folder already exists. Make sure this is intended before copying.
    </tim-alert>
    <button ng-click="$ctrl.copyFolder($ctrl.copyFolderPath, $ctrl.copyFolderExclude)" class="timButton"
            ng-show="$ctrl.copyFolderPath !== item.path &&
                     $ctrl.copyPreviewList.length > 0 &&
                     $ctrl.copyingFolder === 'notcopying'">Copy
    </button>
    <span ng-show="$ctrl.copyingFolder === 'copying'">Copying...</span>
    <span ng-show="$ctrl.copyingFolder === 'finished'">
    Folder {{ $ctrl.item.name }} copied to
    <a href="/manage/{{ $ctrl.newFolder.path }}" ng-bind="$ctrl.newFolder.path"></a>.
    </span>
</form>
    `,
});
