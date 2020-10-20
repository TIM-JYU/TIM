import {IItem} from "tim/item/IItem";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {ICourseListParams} from "tim/document/course/course-list-dialog.component";

export async function showCourseDialog(d: IItem) {
    const {CourseDialogComponent} = await import("./course-dialog.component");
    return await (await angularDialog.open(CourseDialogComponent, d)).result;
}

export async function showCourseListDialog(d: ICourseListParams) {
    const {CourseListDialogComponent} = await import(
        "./course-list-dialog.component"
    );
    return await (await angularDialog.open(CourseListDialogComponent, d))
        .result;
}
