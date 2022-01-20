import {Pipe, PipeTransform} from "@angular/core";

const COMMON_BOOKMARK_GROUPS: Record<string, string> = {
    "My courses": $localize`My courses`,
    "Hidden courses": $localize`Hidden courses`,
    "Last edited": $localize`Last edited`,
    "Last read": $localize`Last read`,
};

@Pipe({
    name: "bookmarkName",
})
export class BookmarkNamePipe implements PipeTransform {
    transform(value: string): string {
        if (COMMON_BOOKMARK_GROUPS[value]) {
            return COMMON_BOOKMARK_GROUPS[value];
        }
        return value;
    }
}
