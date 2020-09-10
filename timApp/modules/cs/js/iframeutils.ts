export const communicationJS = `
<script>
    window.addEventListener('message', function (e) {
        if (e.data.msg === "init") {
            window.port2 = e.ports[0];
            window.port2.onmessage = onMessage;
            window.port2.postMessage({msg: "Inited"});
        }
    });

    function onMessage(event) {
        // console.log(event.data);
        // console.log(event.origin);
        if (event.data.msg === "setData") {
            if (window.setData) {
                setData(event.data.data);
                window.port2.postMessage({msg: "Got data"});
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
    }

    // INITDATA
</script>
    `;

export function getIFrameDataUrl(html: string, initdata?: string) {
    return (
        "data:text/html;base64," +
        btoa(
            html
                .replace("</body>", communicationJS + "\n</body>")
                .replace("// INITDATA", initdata ?? "")
        )
    );
}
