import {saveCurrentScreenPar} from "../document/parhelpers";
import {showMessageDialog} from "../ui/dialog";
import {genericglobals} from "../util/globals";
import {$http, $httpParamSerializer} from "../util/ngimport";
import {to, ToReturn} from "../util/utils";
import {ADMIN_GROUPNAME, IFullUser, IUser} from "./IUser";

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

    public getCurrentPersonalFolderPath(): string {
        return this.getCurrent().folder.path;
    }

    public getSessionUsers() {
        return this.group;
    }

    public async logout(user: IUser, logoutFromKorppi = false) {
        const r = await to($http.post<ILoginResponse>("/logout", {user_id: user.id}));
        if (!r.ok) {
            void showMessageDialog(r.result.data.error);
            return;
        }
        const response = r.result;
        this.group = response.data.other_users;
        this.current = response.data.current_user;
        if (!this.isLoggedIn()) {
            if (logoutFromKorppi) {
                this.korppiLogout(() => {
                    window.location.reload();
                });
            } else {
                window.location.reload();
            }
        }
    }

    public isLoggedIn() {
        return this.current.id > 0; // TODO: maybe !== 0
    }

    public korppiLogin(addUser: boolean) {
        saveCurrentScreenPar();
        const targetUrl = "/openIDLogin?provider=korppi";
        const separator = targetUrl.indexOf("?") >= 0 ? "&" : "?";
        const cameFromRaw = "";
        const anchorRaw = window.location.hash.replace("#", "");
        const redirectFn = () => {
            window.location.replace(targetUrl + separator + $httpParamSerializer({
                came_from: cameFromRaw,
                anchor: anchorRaw,
                add_user: addUser,
            }));
        };
        if (addUser) {
            this.korppiLogout(redirectFn);
        } else {
            redirectFn();
        }
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

    public korppiLogout(redirectFn: () => void) {
        $http(
            {
                withCredentials: true,
                method: "POST",
                url: "https://korppi.jyu.fi/openid/manage/manage",
                headers: {
                    "Content-Type": "application/x-www-form-urlencoded",
                },
                data: $httpParamSerializer({logout: "Logout"}),
            }).finally(redirectFn);
    }

    public async loginWithEmail(email: string, password: string, addUser: boolean): ToReturn<ILoginResponse> {
        const r = await to($http<ILoginResponse>(
            {
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
            }));
        if (r.ok) {
            this.group = r.result.data.other_users;
            this.current = r.result.data.current_user;
        }
        return r;
    }
}

export const Users = new UserService(genericglobals().current_user, genericglobals().other_users);

/**
 * Checks whether user belongs to a certain group or admins group.
 * @returns {boolean}
 */
export function userBelongsToGroupOrIsAdmin(group: string) {
    return Users.belongsToGroup(ADMIN_GROUPNAME) || Users.belongsToGroup(group);
}
