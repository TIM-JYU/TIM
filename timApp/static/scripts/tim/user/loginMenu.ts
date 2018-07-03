import {IController} from "angular";
import {timApp} from "tim/app";
import {IUser} from "./IUser";
import {Users} from "./userService";

class LoginMenuController implements IController {
    private loggingout: boolean;
    private form: {email: string, password: string};
    private addingToSession: boolean;
    private korppiLoading: boolean = false;

    constructor() {
        this.form = {email: "", password: ""};
        this.loggingout = false;
        this.addingToSession = false;
    }

    $onInit() {

    }

    getCurrentUser = () => Users.getCurrent();
    getSessionUsers = () => Users.getSessionUsers();

    addUser($event: Event) {
        $event.stopPropagation();
        this.addingToSession = !this.addingToSession;
    }

    logout = (user: IUser, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);
    isLoggedIn = () => Users.isLoggedIn();

    korppiLogin(addingToSession: boolean) {
        this.korppiLoading = true;
        Users.korppiLogin(addingToSession);
    }

    isKorppi = () => Users.isKorppi();

    stopClick($event: Event) {
        $event.stopPropagation();
    }

    toggled(open: boolean) {
        if (!open) {
            this.addingToSession = false;
        }
    }

    loginWithEmail() {
        Users.loginWithEmail(this.form.email, this.form.password, this.addingToSession,
            () => {
                this.addingToSession = false;
            });
    }

    beginLogout($event: Event) {
        if (Users.isKorppi()) {
            this.loggingout = true;
            $event.stopPropagation();
        } else {
            this.logout(this.getCurrentUser());
        }
    }
}

timApp.component("loginMenu", {
    bindings: {},
    controller: LoginMenuController,
    templateUrl: "/static/templates/loginMenu.html",
});
