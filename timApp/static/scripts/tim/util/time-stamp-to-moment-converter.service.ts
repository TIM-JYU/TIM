import {
    HttpEvent,
    HttpHandler,
    HttpInterceptor,
    HttpRequest,
    HttpResponse,
} from "@angular/common/http";
import {Observable} from "rxjs/internal/Observable";
import {Injectable} from "@angular/core";
import {map} from "rxjs/operators";
import {convertDateStringsToMoments} from "./utils";

@Injectable()
export class TimeStampToMomentConverter implements HttpInterceptor {
    intercept(
        req: HttpRequest<unknown>,
        next: HttpHandler
    ): Observable<HttpEvent<unknown>> {
        return next.handle(req).pipe(
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
        );
    }
}
