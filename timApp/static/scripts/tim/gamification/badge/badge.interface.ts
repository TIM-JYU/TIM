import type {IGroup} from "tim/user/IUser";

/**
 * A badge template: the definition of a badge, owned by a context group.
 *
 * Mirrors `BadgeTemplate.to_json()`. Returned by `/badges/all_badges/<context_group>`.
 *
 * Timestamps arrive as ISO strings rather than Date objects, because Angular's
 * HttpClient does not revive dates; wrap them in `new Date(...)` before doing any date
 * arithmetic on them.
 */
export interface IBadgeTemplate {
    id: number;
    title: string;
    description: string;
    color: string;
    shape: string;
    image: number;
    /**
     * ID of the context group the template belongs to, not its name. Badge routes that
     * take a `context_group` parameter expect the *name*, so pass the component's
     * context group name to those rather than this field.
     */
    context_group: number;
    active: boolean;
    created_by: number;
    created: string;
    modified: string | null;
    deleted: string | null;
}

/**
 * A badge: one award of a template to a group, with an optional message.
 *
 * Mirrors `Badge.to_json()`. Returned by `/badges/give_badge`. It carries only what is
 * specific to the award; everything about how the badge looks and reads belongs to the
 * template it references through {@link IBadge.badge_id}.
 */
export interface IBadge {
    id: number;
    /** ID of the {@link IBadgeTemplate} this is an award of. */
    badge_id: number;
    /** ID of the user group that received the badge. */
    group_id: number;
    message: string;
    /** False once the badge has been withdrawn. */
    active: boolean;
    given_by: number;
    given: string;
    withdrawn: string | null;
}

/**
 * An award as `/badges/group_badges/<group_id>/<context_group>` returns it: an
 * {@link IBadge} plus the display name of the user who gave it, resolved server-side.
 */
export interface IGivenBadge extends IBadge {
    /** Null if the user that gave the badge no longer exists. */
    given_by_name: string | null;
}

/**
 * A template as `/badges/group_badges/<group_id>/<context_group>` returns it: an
 * {@link IBadgeTemplate} plus the display name of the user who created it.
 */
export interface IBadgeTemplateInfo extends IBadgeTemplate {
    /** Null if the user that created the template no longer exists. */
    created_by_name: string | null;
}

/**
 * The payload of `/badges/group_badges/<group_id>/<context_group>`.
 *
 * Awards and the templates they refer to are returned side by side rather than merged,
 * so nothing is duplicated: every badge in `badges` has its template in `templates`,
 * found by `badge_id`. {@link BadgeService.joinAwards} does that join.
 */
export interface IGroupBadges {
    badges: IGivenBadge[];
    templates: IBadgeTemplateInfo[];
}

/**
 * An award together with the template it is an award of.
 *
 * This is the shape the badge UI renders: the award's own fields at the top level, and
 * everything about how the badge looks and reads under {@link IBadgeAward.template}.
 */
export interface IBadgeAward extends IGivenBadge {
    template: IBadgeTemplateInfo;
}

export interface IErrorAlert {
    msg: string;
    type: "warning" | "danger";
    id?: string;
}

/**
 * Interface that extends IGroup contents and adds description field for a pretty group name.
 */
export interface IBadgeGroup extends IGroup {
    description: string;
}
