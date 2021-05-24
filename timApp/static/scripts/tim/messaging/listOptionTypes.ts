import {Moment} from "moment";

export interface ListOptions {
    // Keep this updated with ListOptions Python class. That class also provides further information about these
    // variables.
    name: string;
    domain?: string;
    archive?: ArchiveType;
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
    non_member_message_pass?: boolean;
    allow_attachments?: boolean;
    // distribution?: Channel[];
    distribution?: Distribution;
    removed?: Moment;
}

// Interface to express what message channels the message list has in use.
export interface Distribution {
    tim_message: boolean;
    email_list: boolean;
}

// See class Channel on listoptions.py for explanation.
export enum Channel {
    TIM_MESSAGE = "tim_message",
    EMAIL_LIST = "email_list",
}

// See ReplyToListChanges Python class.
export enum ReplyToListChanges {
    NOCHANGES = "no_munging",
    ADDLIST = "point_to_list",
}

// See ArchiveType class on Python side of things for explanations.
export enum ArchiveType {
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
].reverse();

// See MemberInfo Python class for further details.
export interface MemberInfo {
    name: string;
    sendRight: boolean;
    deliveryRight: boolean;
    email: string;
    removed?: Moment;
}
