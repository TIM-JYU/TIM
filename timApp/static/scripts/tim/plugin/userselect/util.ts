export const MediaDevicesSupported =
    typeof navigator !== "undefined" && !!navigator.mediaDevices;

const supportedCameraConstraints: Record<
    string,
    boolean
> = MediaDevicesSupported
    ? (navigator.mediaDevices.getSupportedConstraints() as Record<
          string,
          boolean
      >)
    : {};

export function cameraConstraintSupported(constraint: string): boolean {
    return MediaDevicesSupported && supportedCameraConstraints[constraint];
}

export async function hasVideoDevices() {
    if (!MediaDevicesSupported) {
        return false;
    }
    // Getting video device will fail if there are no video devices attached => return false
    try {
        const stream = await navigator.mediaDevices.getUserMedia({
            video: true,
        });
        // Reset the camera so that it can be used
        stream.getVideoTracks().forEach((track) => track.stop());
        return true;
    } catch (e) {
        console.error(e);
        return false;
    }
}

export async function setStreamConstraints(
    stream: MediaStream,
    constraints: Record<string, unknown>
) {
    const cleanConstraints: Record<string, unknown> = {};
    for (const k in constraints) {
        if (
            Object.hasOwnProperty.call(constraints, k) &&
            cameraConstraintSupported(k)
        ) {
            cleanConstraints[k] = constraints[k];
        }
    }
    if (!cleanConstraints) {
        return;
    }
    // Browser *should* ignore unknown constraints, but on some browsers it appears to throw nonetheless
    try {
        await stream.getVideoTracks()[0].applyConstraints({
            advanced: [cleanConstraints],
        });
    } catch (e) {
        // Swallow the error; the torch is just not supported
    }
}
