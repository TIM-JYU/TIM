export interface ListOptions {
    // VIESTIM Keep this updated with ListOptions class (at the Python side of things)
    name: string;
    domain?: string;
    archive: ArchiveType;
    notify_owners_on_list_change?: boolean;
    list_description?: string;
    list_info?: string;
    only_text?: boolean;
    default_reply_type?: ReplyToListChanges;
    email_admin_url?: string;
    tim_users_can_join?: boolean;
    members_can_unsubscribe?: boolean;
    default_send_right?: boolean;
    default_delivery_right?: boolean;
    list_subject_prefix?: string;
}

export enum ReplyToListChanges {
    // See ReplyToListChanges Python class.
    NOCHANGES,
    ADDLIST,
}

export enum ArchiveType {
    // See ArchiveType class on Python side of things for explanations.
    NONE,
    SECRET,
    GROUPONLY,
    UNLISTED,
    PUBLIC,
}

// For proper setting of archive options on UI.
export interface ArchivePolicyNames {
    archiveType: ArchiveType;
    policyName: string;
}

// Mapping of archive policy enum to explanations given in UI.
export const archivePolicyNames: ArchivePolicyNames[] = [
    {archiveType: ArchiveType.NONE, policyName: "No archiving."},
    {
        archiveType: ArchiveType.SECRET,
        policyName: "Secret archive, only for owner.",
    },
    {
        archiveType: ArchiveType.GROUPONLY,
        policyName:
            "Members only archive. Only members of this list can access.",
    },
    {
        archiveType: ArchiveType.UNLISTED,
        policyName: "Unlisted archive. Everyone with link can access.",
    },
    {
        archiveType: ArchiveType.PUBLIC,
        policyName:
            "Public archive. Everyone with link can access and the archive is advertised.",
    },
];

export interface MemberInfo {
    // VIESTIM Keep this members' db models' json representation.
    name: string;
    sendRight: boolean;
    deliveryRight: boolean;
    email: string;
}
