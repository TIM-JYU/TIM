import {SearchBoxComponent} from "tim/search/search-box.component";
import {to2} from "tim/util/utils";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showSearchResultDialog(r: SearchBoxComponent) {
    const {SearchResultsDialogComponent} = await import(
        "./search-results-dialog.component"
    );
    await to2(
        (await angularDialog.open(SearchResultsDialogComponent, r)).result
    );
}
