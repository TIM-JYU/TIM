import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {Subject} from "rxjs";
import {toPromise} from "tim/util/utils";
import type {IBadge} from "tim/gamification/badge/badge.interface";

interface IData {
    context_group?: string;
    group_id: number;
    badge_id?: number;
    message: string;
}

@Injectable({
    providedIn: "root",
})
export class BadgeService {
    // Subject, joka laukaisee updatesignaalin
    private updateBadgeSubject = new Subject<void>();

    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

    // Observable updatetapahtuman kuunteluun
    updateBadgeList$ = this.updateBadgeSubject.asObservable();

    private groupNameUpdated = new Subject<{id: number; newName: string}>();
    groupNameUpdated$ = this.groupNameUpdated.asObservable();

    notifyBadgeViewerUpdate() {
        this.updateBadgeSubject.next();
    }

    constructor(private http: HttpClient) {}

    /**
     * Fetches badges for user or group.
     * ID can be user's personal group ID with one member or group ID with multiple.
     * @param id defines user/group.
     * @param contextGroup Course context for badges.
     * @return returns IBadge[]
     */
    async getBadges(id: number, contextGroup: string) {
        const result = await toPromise(
            this.http.get<[]>(`/groups_badges/${id}/${contextGroup}`)
        );
        const userBadges: IBadge[] = [];
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    userBadges.push(alkio);
                }
            }
        }
        return userBadges.reverse();
    }

    /**
     * Withdraws selected badge from user.
     * @param badgeGivenID Used to define user who has selected badge.
     * @param context_group Course context for badges.
     */
    async withdrawBadge(badgeGivenID: number, context_group: string) {
        const result = await toPromise(
            this.http.post<{ok: boolean}>("/withdraw_badge", {
                badge_given_id: badgeGivenID,
                context_group: context_group,
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
    async assignBadges(data: IData) {
        const result = await toPromise(
            this.http.post<{ok: boolean}>("/give_badge", {
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

    private dialogOpen = false;

    // Checks, if dialog-window is open
    isDialogOpen(): boolean {
        return this.dialogOpen;
    }
    // Sets the dialog-window as "open"
    setDialogOpen(isOpen: boolean): void {
        this.dialogOpen = isOpen;
    }

    // Send a request to update viewer, when a new badge is created
    triggerUpdateBadgeList() {
        this.updateBadgeSubject.next();
    }

    public activeDialogRef: any = null; // Store the active dialog reference

    // Close the active dialog if it exists
    closeActiveDialog(): void {
        if (this.activeDialogRef) {
            this.activeDialogRef.close({}); // Close the current dialog
            this.activeDialogRef = null; // Reset the reference
        }
    }

    // Show errors
    showError(
        alerts: any,
        response: {data: {error: string}},
        type: "warning" | "danger"
    ) {
        const msg = `Error: ${response.data.error ?? response.data}`;
        if (alerts.some((a: any) => a.msg === msg && a.type === type)) {
            return;
        }
        alerts.push({msg, type});
    }

    // Removes an error-message from alerts-list
    closeAlert(alerts: any, index: number) {
        alerts.splice(index, 1);
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
            // Check if the name contains "Vibrant"
            const isVibrantA = a.forCreatorList
                .toLowerCase()
                .includes("vibrant");
            const isVibrantB = b.forCreatorList
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
}
