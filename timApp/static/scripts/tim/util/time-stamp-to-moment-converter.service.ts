import type {
    HttpEvent,
    HttpHandler,
    HttpInterceptor,
    HttpRequest,
} from "@angular/common/http";
import {HttpErrorResponse} from "@angular/common/http";
import {HttpResponse} from "@angular/common/http";
import type {Observable} from "rxjs/internal/Observable";
import {Injectable} from "@angular/core";
import {catchError, map} from "rxjs/operators";
import {convertDateStringsToMoments} from "tim/util/utils";
import {throwError} from "rxjs";

@Injectable()
export class TimeStampToMomentConverter implements HttpInterceptor {
    intercept(
        req: HttpRequest<unknown>,
        next: HttpHandler
    ): Observable<HttpEvent<unknown>> {
        return next
            .handle(req)
            .pipe(
                map((event: HttpEvent<unknown>) => {
                    if (
                        event instanceof HttpResponse &&
                        event.headers.get("No-Date-Conversion") !== "true"
                    ) {
                        event = event.clone({
                            body: convertDateStringsToMoments(event.body),
                        });
                    }
                    return event;
                })
            )
            .pipe(
                catchError((error: unknown) => {
                    if (
                        error instanceof HttpErrorResponse &&
                        error.headers.get("No-Date-Conversion") !== "true"
                    ) {
                        error = new HttpErrorResponse({
                            headers: error.headers,
                            status: error.status,
                            statusText: error.statusText,
                            url: error.url ?? undefined,
                            error: convertDateStringsToMoments(error.error),
                        });
                    }
                    return throwError(() => error);
                })
            );
    }
}
