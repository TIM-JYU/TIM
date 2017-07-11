import {IController} from "angular";
import {timApp} from "tim/app";
import {Users} from "../services/userService";

class LoginMenuController implements IController {
    private loggingout: boolean;
    private form: {email: string, password: string};
    private addingToSession: boolean;
    private korppiLoading: boolean;

    constructor() {
        this.form = {email: "", password: ""};
        this.loggingout = false;
        this.addingToSession = false;
    }

    getCurrentUser = () => Users.getCurrent();
    getSessionUsers = () => Users.getSessionUsers();

    addUser($event) {
        $event.stopPropagation();
        this.addingToSession = !this.addingToSession;
    }

    logout = (user, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);
    isLoggedIn = () => Users.isLoggedIn();

    korppiLogin(addingToSession) {
        this.korppiLoading = true;
        Users.korppiLogin(addingToSession);
    }

    isKorppi = () => Users.isKorppi();

    stopClick($event) {
        $event.stopPropagation();
    }

    toggled(open) {
        if (!open) {
            this.addingToSession = false;
        }
    }

    addTestUser($event) {
        Users.addUser();
    }

    loginWithEmail() {
        Users.loginWithEmail(this.form.email, this.form.password, this.addingToSession,
            () => {
                this.addingToSession = false;
            });
    }

    beginLogout($event) {
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
