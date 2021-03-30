import {showMessageDialog} from "tim/ui/showMessageDialog";
import {genericglobals, Locale} from "../util/globals";
import {$http, $httpParamSerializer} from "../util/ngimport";
import {to, ToReturn} from "../util/utils";
import {ADMIN_GROUPNAME, IFullUser, IUser, TEACHERS_GROUPNAME} from "./IUser";

export interface ILoginResponse {
    other_users: IUser[];
    current_user: IFullUser;
}

export class UserService {
    private current: IFullUser; // currently logged in user
    private group: IUser[]; // any additional users that have been added in the session - this does not include the main user

    constructor(current: IFullUser, group: IUser[]) {
        this.current = current;
        this.group = group;
    }

    public getCurrent(): IFullUser {
        return this.current;
    }

    public getCurrentPersonalFolderPath() {
        return this.getCurrent().folder?.path;
    }

    public getSessionUsers() {
        return this.group;
    }

    public async logout(user: IUser) {
        const r = await to(
            $http.post<ILoginResponse>("/logout", {user_id: user.id})
        );
        if (!r.ok) {
            void showMessageDialog(r.result.data.error);
            return;
        }
        const response = r.result;
        this.group = response.data.other_users;
        this.current = response.data.current_user;
        if (!this.isLoggedIn()) {
            window.location.reload();
        }
    }

    public isLoggedIn() {
        return this.current.id > 0; // TODO: maybe !== 0
    }

    public getCurrentLocale(): Locale {
        return genericglobals().locale;
    }

    public getCurrentLanguage(): "fi" | "en" {
        const loc = this.getCurrentLocale();
        if (loc === "fi") {
            return loc;
        }
        return "en";
    }

    /**
     * Checks whether the user belongs to a group.
     */
    public belongsToGroup(groupName: string) {
        for (const group of this.current.groups) {
            if (group.name === groupName) {
                return true;
            }
        }
        return false;
    }

    public isGroupAdmin() {
        return userBelongsToGroupOrIsAdmin("Group admins");
    }

    public canScheduleFunctions() {
        return (
            isAdmin() ||
            Users.belongsToGroup("Function schedulers") ||
            Users.belongsToGroup(TEACHERS_GROUPNAME)
        );
    }

    public async loginWithEmail(
        email: string,
        password: string,
        addUser: boolean
    ): ToReturn<ILoginResponse> {
        const r = await to(
            $http<ILoginResponse>({
                method: "POST",
                url: "/altlogin",
                headers: {
                    "Content-Type": "application/x-www-form-urlencoded",
                    "X-Requested-With": "XMLHttpRequest",
                },
                data: $httpParamSerializer({
                    email,
                    password,
                    add_user: addUser,
                }),
            })
        );
        if (r.ok) {
            this.group = r.result.data.other_users;
            this.current = r.result.data.current_user;
        }
        return r;
    }
}

export const Users = new UserService(
    genericglobals().current_user,
    genericglobals().other_users
);

export function isAdmin() {
    return Users.belongsToGroup(ADMIN_GROUPNAME);
}

/**
 * Checks whether user belongs to a certain group or admins group.
 * @returns {boolean}
 */
export function userBelongsToGroupOrIsAdmin(group: string) {
    return isAdmin() || Users.belongsToGroup(group);
}
