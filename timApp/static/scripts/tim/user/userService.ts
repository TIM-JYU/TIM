import {$http, $httpParamSerializer, $window} from "../util/ngimport";
import {to, ToReturn} from "../util/utils";
import {IFullUser, IUser} from "./IUser";

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

    public getCurrent(): IUser {
        return this.current;
    }

    public getSessionUsers() {
        return this.group;
    }

    public isKorppi(): boolean {
        return this.current.name.indexOf("@") < 0;
    }

    public logout(user: IUser, logoutFromKorppi = false) {
        $http.post<ILoginResponse>("/logout", {user_id: user.id}).then((response) => {
            this.group = response.data.other_users;
            this.current = response.data.current_user;
            if (!this.isLoggedIn()) {
                if (logoutFromKorppi) {
                    this.korppiLogout(function() {
                        $window.location.reload();
                    });
                } else {
                    $window.location.reload();
                }
            }
        });
    }

    public isLoggedIn() {
        return this.current.id > 0; // TODO: maybe !== 0
    }

    public korppiLogin(addUser: boolean) {
        const targetUrl = "/openIDLogin?provider=korppi";
        const separator = targetUrl.indexOf("?") >= 0 ? "&" : "?";
        const cameFromRaw = $window.came_from || "";
        const cameFrom = encodeURIComponent(cameFromRaw.replace("#", "%23"));
        const anchorRaw = $window.anchor || window.location.hash.replace("#", "");
        const anchor = encodeURIComponent(anchorRaw);
        const redirectFn = function() {
            $window.location.replace(targetUrl + separator + $httpParamSerializer({
                came_from: cameFrom,
                anchor,
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

    public korppiLogout(redirectFn: () => any) {
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
        const [err, response] = await to($http<ILoginResponse>(
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
        if (response) {
            this.group = response.data.other_users;
            this.current = response.data.current_user;
        }
        if (err && !response) {
            return [err, response];
        } else if (response && !err) {
            return [err, response];
        } else {
            throw new Error("unreachable");
        }
    }
}

export let Users: UserService = null as any;

export function initUserService() {
    if (Users != null) {
        throw new Error("UserService already initialized");
    }
    Users = new UserService($window.current_user, $window.other_users);
}
