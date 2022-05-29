import {Component, EventEmitter, Output} from "@angular/core";

@Component({
    selector: "tim-t9-keyboard",
    template: `
    <table class="t9kbd">
        <tr>
            <td>
                <button (click)="applyT9.emit('1')">1<br>&nbsp;</button>
            </td>
            <td>
                <button (click)="applyT9.emit('2')">2<br>ABC</button>
            <td>
                <button (click)="applyT9.emit('3')">3<br>DEF</button>
            </td>
        </tr>
        <tr>
            <td>
                <button (click)="applyT9.emit('4')">4<br>GHI</button>
            </td>
            <td>
                <button (click)="applyT9.emit('5')">5<br>JKL</button>
            <td>
                <button (click)="applyT9.emit('6')">6<br>MNO</button>
            </td>
        </tr>
        <tr>
            <td>
                <button (click)="applyT9.emit('7')">7<br>PQRS</button>
            </td>
            <td>
                <button (click)="applyT9.emit('8')">8<br>TUV</button>
            <td>
                <button (click)="applyT9.emit('9')">9<br>WXYZ</button>
            </td>
        </tr>
        <tr>
            <td>
                <button (click)="applyT9.emit('clr')">clr<br>&nbsp;</button>
            </td>
            <td>
                <button (click)="applyT9.emit('0')">0<br>space</button>
            <td>
                <button (click)="applyT9.emit('<=')"><=<br>&nbsp;</button>
            </td>
        </tr>
    </table>
  `,
    styleUrls: ["./t9-keyboard.component.scss"],
})
export class T9KeyboardComponent {
    @Output() applyT9 = new EventEmitter<string>();
}
