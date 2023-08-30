// https://vasily-ivanov.medium.com/instanceof-in-angular-html-templates-63f23d497242
import type {PipeTransform} from "@angular/core";
import {Pipe} from "@angular/core";

type AbstractType<T> = abstract new (...args: never[]) => T;

/**
 * Check that type is of correct type.
 */
@Pipe({
    name: "instanceof",
    pure: true,
})
export class InstanceofPipe implements PipeTransform {
    public transform<V, R>(value: V, type: AbstractType<R>): R | undefined {
        return value instanceof type ? value : undefined;
    }
}
