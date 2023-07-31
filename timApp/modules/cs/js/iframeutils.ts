export const communicationJS = `
<script>
    window.addEventListener('message', function (e) {
        if (e.data.msg === "init") {
            window.port2 = e.ports[0];
            window.port2.onmessage = onMessage;
            if (window.onInit) {
                window.onInit();
            }
            window.port2.postMessage({msg: "Inited"});
        }
    });

    function onMessage(event) {
        // console.log(event.data);
        // console.log(event.origin);
        if (event.data.msg === "setData") {
            if (window.setData) {
                const msg = {msg: "Got data"};
                const ret = setData(event.data.data);
                if (ret) {
                    msg.ret = ret;
                }
                window.port2.postMessage(msg);
            } else {
                console.log("iframe got setData message but iframe has no setData function");
            }
        }
        if (event.data.msg === "getData") {
            window.port2.postMessage({msg: "data", data: getData()});
        }
        if (event.data.msg === "getDataSave") {
            window.port2.postMessage({msg: "datasave", data: getData()});
        }
        if (event.data.msg === "close") {
            close();
        }
        if (event.data.msg === "start"){
            edit(event.data.fullscreen);
        }
        if (event.data.msg === "toggleEditorOptions"){
            toggleOptions();
        }
    }

    // INITDATA
</script>
    `;

export function getIFrameDataUrl(html: string, initdata?: string) {
    let s = html
        .replace("</body>", communicationJS + "\n</body>")
        .replace("// INITDATA", initdata ?? "");
    s = encodeURIComponent(s).replace(
        /%([0-9A-F]{2})/g,
        function toSolidBytes(match, p1) {
            return String.fromCharCode(parseInt("0x" + p1, 16));
        }
    );
    return "data:text/html;base64," + btoa(s);
}
