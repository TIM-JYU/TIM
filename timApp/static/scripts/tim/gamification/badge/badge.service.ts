import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {Subject} from "rxjs";
import {toPromise} from "tim/util/utils";
import type {
    IBadgeAward,
    IBadgeTemplate,
    IErrorAlert,
    IGroupBadges,
} from "tim/gamification/badge/badge.interface";
import {sortLang} from "tim/user/IUser";
import type {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";

interface IBadgeData {
    context_group?: string;
    group_id: number;
    badge_id?: number;
    message: string;
}

@Injectable({
    providedIn: "root",
})
export class BadgeService {
    private updateBadgeSubject = new Subject<void>();
    private groupNameUpdated = new Subject<{id: number; newName: string}>();
    updateBadgeList$ = this.updateBadgeSubject.asObservable();
    groupNameUpdated$ = this.groupNameUpdated.asObservable();
    public activeDialogRef:
        | AngularDialogComponent<{message: string; modal: boolean}, unknown>
        | undefined;
    alerts: Array<IErrorAlert> = [];

    notifyBadgeViewerUpdate() {
        this.updateBadgeSubject.next();
    }

    constructor(private http: HttpClient) {}

    /**
     * Joins each award returned by `/badges/group_badges` with its template.
     *
     * Awards whose template is missing from the payload are dropped; the route only
     * omits a template when the award should not be shown in this context anyway.
     * @param payload The two lists as the route returns them.
     * @return IBadgeAward[]
     */
    joinAwards(payload: IGroupBadges): IBadgeAward[] {
        const templates = new Map(payload.templates.map((t) => [t.id, t]));
        const awards: IBadgeAward[] = [];
        for (const badge of payload.badges) {
            const template = templates.get(badge.badge_id);
            if (template) {
                awards.push({...badge, template});
            }
        }
        return awards;
    }

    /**
     * Fetches badges for user or group.
     * ID can be user's personal group ID with one member or group ID with multiple.
     * @param id defines user/group.
     * @param contextGroup Course context for badges.
     * @return IBadgeAward[] newest first
     */
    async getBadges(id: number, contextGroup: string) {
        const resp = await toPromise(
            this.http.get<IGroupBadges>(
                `/badges/group_badges/${id}/${contextGroup}`
            )
        );
        if (resp.ok) {
            // The route returns oldest first.
            return this.joinAwards(resp.result).reverse();
        }
        return [];
    }

    /**
     * Withdraws selected badge from user.
     * @param badgeGivenID Used to define user who has selected badge.
     * @param contextGroup Course context for badges.
     */
    async withdrawBadge(badgeGivenID: number, contextGroup: string) {
        const result = await toPromise(
            this.http.post<{ok: boolean}>("/badges/withdraw_badge", {
                badge_given_id: badgeGivenID,
                context_group: contextGroup,
            })
        );
        if (!result.ok) {
            return {ok: false, data: result};
        }
        this.triggerUpdateBadgeList();
        return {ok: true};
    }

    /**
     * Sends http post request, that gives badge to a user or group.
     * User/group is defined by group_id & badge_id.
     * If request returns an error, showError method is called.
     * notifyBadgeViewerUpdate is called to handle live updates.
     * @param data Contains all information for http post request.
     */
    async assignBadges(data: IBadgeData) {
        const result = await toPromise(
            this.http.post<{ok: boolean}>("/badges/give_badge", {
                context_group: data.context_group,
                group_id: data.group_id,
                badge_id: data.badge_id,
                message: data.message,
            })
        );
        if (!result.ok) {
            this.showError(
                this.alerts,
                {data: {error: result.result.error.error}},
                "danger"
            );
            return;
        }
        if (data.group_id) {
            this.notifyBadgeViewerUpdate();
        }
    }

    /**
     *     Send a request to update viewer, when a new badge is created
     */
    triggerUpdateBadgeList() {
        this.updateBadgeSubject.next();
    }

    /**
     * if a dialog window is already open, it will be closed.
     */
    closeActiveDialog(): void {
        if (this.activeDialogRef) {
            this.activeDialogRef.close({});
            // this.activeDialogRef = null;
        }
    }

    /**
     * Displays an error message in the alerts list, avoiding duplicates.
     *
     * @param alerts - The current list of alert messages.
     * @param response - The response object containing the error message.
     * @param type - The type of the alert to display (e.g., warning or danger).
     */
    showError(
        alerts: IErrorAlert[],
        response: {data: {error: string}},
        type: "warning" | "danger"
    ) {
        const msg = `Error: ${response.data.error ?? response.data}`;
        if (alerts.some((a: IErrorAlert) => a.msg === msg && a.type === type)) {
            return;
        }
        alerts.push({msg, type});
    }

    /**
     * Removes an alert message from the alerts list at the specified index.
     *
     * @param alerts - The list of alert messages.
     * @param index - The index of the alert to remove.
     */
    closeAlert(alerts: IErrorAlert[], index: number) {
        alerts.splice(index, 1);
    }

    private defaultBadgeValues = {
        title: "",
        color: "gray",
        image: 0,
        description: "",
        shape: "hexagon",
    };

    getDefaultBadgeValues() {
        return this.defaultBadgeValues;
    }

    // Available icons for badges
    private availableImages = [
        {id: 1, name: "Trophy"},
        {id: 2, name: "Winner"},
        {id: 3, name: "Teamwork"},
        {id: 4, name: "Code"},
        {id: 5, name: "Debug"},
        {id: 6, name: "On Fire"},
        {id: 7, name: "Rocket"},
        {id: 8, name: "Smile"},
        {id: 9, name: "Terminal"},
        {id: 10, name: "Deployed Code"},
        {id: 11, name: "Loop"},
        {id: 12, name: "100 Points"},
        {id: 13, name: "Communication"},
    ];

    // Available shapes for badges
    private availableShapes = [
        {id: "hexagon", value: "Hexagon"},
        {id: "flower", value: "Flower"},
        {id: "round", value: "Round"},
        {id: "square", value: "Square"},
    ];

    // Color list for badges
    private availableColors = [
        {id: "brown", forCreatorList: "Brown"},
        {id: "coral", forCreatorList: "Coral"},
        {id: "dark-blue", forCreatorList: "Dark Blue"},
        {id: "dark-green", forCreatorList: "Dark Green"},
        {id: "green", forCreatorList: "Green"},
        {id: "mint", forCreatorList: "Mint"},
        {id: "olive", forCreatorList: "Olive"},
        {id: "orange", forCreatorList: "Orange"},
        {id: "pink", forCreatorList: "Pink"},
        {id: "purple", forCreatorList: "Purple"},
        {id: "red", forCreatorList: "Red"},
        {id: "skyblue", forCreatorList: "Skyblue"},
        {id: "turquoise", forCreatorList: "Turquoise"},
        {id: "violet", forCreatorList: "Violet"},
        {id: "wine", forCreatorList: "Wine"},
        {id: "yellow", forCreatorList: "Yellow"},
        {id: "black-vibrant", forCreatorList: "Black Vibrant"},
        {id: "green-vibrant", forCreatorList: "Green Vibrant"},
        {id: "navy-vibrant", forCreatorList: "Navy Vibrant"},
        {id: "orange-vibrant", forCreatorList: "Orange Vibrant"},
        {id: "purple-vibrant", forCreatorList: "Purple Vibrant"},
        {id: "red-vibrant", forCreatorList: "Red Vibrant"},
        {id: "yellow-vibrant", forCreatorList: "Yellow Vibrant"},
    ];

    /**
     * Returns a sorted list of available icons
     * Sorts the icons alphabetically based on their name property
     *
     * @returns Array - A sorted array of available icons
     */
    getAvailableImages() {
        return this.availableImages.sort((a, b) =>
            a.name.localeCompare(b.name)
        );
    }

    /**
     * Returns a sorted list of available shapes.
     * Sorts the shapes alphabetically based on their `value` property.
     *
     * @returns Array - A sorted array of available shapes.
     */
    getAvailableShapes() {
        return this.availableShapes.sort((a, b) =>
            a.value.localeCompare(b.value)
        );
    }

    /**
     * Returns a sorted list of available colors.
     *
     * - Sorts colors based on whether their name contains the word "Vibrant".
     * - If both colors are either "Vibrant" or not, they are sorted alphabetically.
     * - If one color is "Vibrant" and the other is not, the non-"Vibrant" color is placed first.
     *
     * @returns Array - A sorted array of available colors.
     */
    getAvailableColors() {
        return this.availableColors.sort((a, b) => {
            const isVibrantA: boolean = a.forCreatorList
                .toLowerCase()
                .includes("vibrant");
            const isVibrantB: boolean = b.forCreatorList
                .toLowerCase()
                .includes("vibrant");

            if (isVibrantA === isVibrantB) {
                return a.forCreatorList.localeCompare(b.forCreatorList);
            }
            return isVibrantA ? 1 : -1;
        });
    }

    /**
     * Function is triggered when a wheel scroll event occurs.
     * Checks if the element can actually scroll vertically.
     * Apply custom scroll logic if the element can scroll.
     * @param event Browser's scroll-wheel event
     */
    onScrollList(event: WheelEvent) {
        const element = event.currentTarget as HTMLElement;
        const scrollable = element.scrollHeight > element.clientHeight;
        if (scrollable) {
            const targetElement = event.currentTarget as HTMLElement;
            const scrollAmount = event.deltaY * 0.5;
            targetElement.scrollTop += scrollAmount;
            event.preventDefault();
        }
    }

    /**
     * Sorts badge templates by title or by when they were created.
     *
     * @param templates The templates to sort
     * @param sortType "az", "za", "newest", "oldest"; anything else returns a copy
     * @returns A new sorted array
     */
    sortBadgeTemplates(
        templates: IBadgeTemplate[],
        sortType: string
    ): IBadgeTemplate[] {
        return this.sortByTitleOrTime(
            templates,
            sortType,
            (template) => template.title,
            (template) => template.created
        );
    }

    /**
     * Sorts awarded badges by their template's title or by when they were given.
     *
     * @param awards The awards to sort
     * @param sortType "az", "za", "newest", "oldest"; anything else returns a copy
     * @returns A new sorted array
     */
    sortBadgeAwards(awards: IBadgeAward[], sortType: string): IBadgeAward[] {
        return this.sortByTitleOrTime(
            awards,
            sortType,
            (award) => award.template.title,
            (award) => award.given
        );
    }

    /**
     * Shared ordering for both sorts; the callers say how to read a title and a
     * timestamp out of their own element type. Alphabetical sorting is locale-aware
     * and uses the configured `sortLang` locale.
     */
    private sortByTitleOrTime<T>(
        items: T[],
        sortType: string,
        title: (item: T) => string,
        time: (item: T) => string
    ): T[] {
        const sorted = [...items];
        const ms = (item: T) => new Date(time(item)).getTime();

        switch (sortType) {
            case "az":
                return sorted.sort((a, b) =>
                    title(a).localeCompare(title(b), sortLang, {
                        sensitivity: "base",
                    })
                );
            case "za":
                return sorted.sort((a, b) =>
                    title(b).localeCompare(title(a), sortLang, {
                        sensitivity: "base",
                    })
                );
            case "newest":
                return sorted.sort((a, b) => ms(b) - ms(a));
            case "oldest":
                return sorted.sort((a, b) => ms(a) - ms(b));
            default:
                return sorted;
        }
    }

    /**
     * Tests connection with check_connection route.
     * If there is error with result, calls showError method via badge-service and returns true.
     * If no errors, returns false.
     */
    async checkConnectionError(alerts: IErrorAlert[]) {
        // No trailing slash: the route is registered as "/badges/check_connection",
        // and Flask answers 404 for the same path with one, which made every caller
        // report a connection error and give up.
        const result = await toPromise(
            this.http.get("/badges/check_connection")
        );
        if (!result.ok) {
            this.showError(
                alerts,
                {
                    data: {
                        error: "Unexpected error. Check your internet connection.",
                    },
                },
                "danger"
            );
            return true;
        }
        return false;
    }
}
