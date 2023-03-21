import {isRootFolder} from "tim/item/IItem";
import {genericglobals} from "tim/util/globals";

export interface IItemLink {
    route: string;
    title: string;
}

const availableViews: IItemLink[] = [];

enum Views {
    view,
    manage,
    teacher,
    answers,
    lecture,
    velp,
    slide,
    review,
}

const viewNames: Record<Views, string> = {
    [Views.view]: $localize`View`,
    [Views.manage]: $localize`Manage`,
    [Views.teacher]: $localize`Teacher`,
    [Views.answers]: $localize`Answers`,
    [Views.lecture]: $localize`Lecture`,
    [Views.velp]: $localize`Velp`,
    [Views.slide]: $localize`Slide`,
    [Views.review]: $localize`Review`,
};

export function getAvailableViews() {
    if (availableViews.length > 0) {
        return availableViews;
    }
    const item = genericglobals().curr_item;
    if (!item) {
        return availableViews;
    }

    const allowedRoutes = [Views.view];
    if (!isRootFolder(item)) {
        allowedRoutes.push(Views.manage);
    }
    if (!item.isFolder) {
        if (item.rights.teacher) {
            allowedRoutes.push(Views.teacher);
        }
        if (item.rights.see_answers) {
            allowedRoutes.push(Views.answers);
        }
        allowedRoutes.push(Views.lecture, Views.velp, Views.slide);
    }
    availableViews.push(
        ...allowedRoutes.map((r) => ({
            route: Views[r],
            title: viewNames[r],
        }))
    );
    return availableViews;
}
