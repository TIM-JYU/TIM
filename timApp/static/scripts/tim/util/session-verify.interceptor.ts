import {Injectable} from "@angular/core";
import type {
    HttpErrorResponse,
    HttpEvent,
    HttpHandler,
    HttpInterceptor,
    HttpRequest,
} from "@angular/common/http";
import {HTTP_INTERCEPTORS} from "@angular/common/http";
import type {Observable} from "rxjs";
import {retry} from "rxjs";
import {openSessionCheckDialog} from "tim/ui/session-check/openSessionCheckDialog";

export const SESSION_VERIFICATION_NEEDED_CODE = 490;

let globalHandleExpirationPromise: Promise<void> | undefined;
export async function handleExpiredSession() {
    if (!globalHandleExpirationPromise) {
        globalHandleExpirationPromise = (async () => {
            await openSessionCheckDialog();
            globalHandleExpirationPromise = undefined;
        })();
    }

    await globalHandleExpirationPromise;
}

@Injectable()
export class SessionVerifyInterceptor implements HttpInterceptor {
    constructor() {}

    intercept(
        request: HttpRequest<unknown>,
        next: HttpHandler
    ): Observable<HttpEvent<unknown>> {
        return next.handle(request).pipe(
            retry({
                delay: async (error: HttpErrorResponse) => {
                    if (error.status === SESSION_VERIFICATION_NEEDED_CODE) {
                        await handleExpiredSession();
                        return true;
                    }
                    throw error;
                },
            })
        );
    }
}

export const SessionVerify = {
    provide: HTTP_INTERCEPTORS,
    useClass: SessionVerifyInterceptor,
    multi: true,
};
