export interface ListOptions {
    // VIESTIM Keep this updated with ListOptions class (at the Python side of things)
    listname: string;
    domain?: string;
    archive: ArchiveType;
    notifyOwnerOnListChange?: boolean;
    listDescription?: string;
    listInfo?: string;
    onlyText?: boolean;
    defaultReplyType?: ReplyToListChanges;
    emailAdminURL?: string;
    timUsersCanJoin?: boolean;
    canUnsubscribe?: boolean;
    defaultSendRight?: boolean;
    defaultDeliveryRight?: boolean;
    listSubjectPrefix?: string;
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
