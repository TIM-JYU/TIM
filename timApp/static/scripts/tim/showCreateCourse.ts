import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showCreateCourse() {
    const {CreateCourseDialogComponent} = await import(
        "./create-course-dialog.component"
    );
    return (await angularDialog.open(CreateCourseDialogComponent, undefined))
        .result;
}
