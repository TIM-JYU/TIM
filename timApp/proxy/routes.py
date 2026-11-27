import os
import re
import html
from io import BytesIO
from os.path import basename
from urllib.parse import urlparse, unquote

import requests
from collections.abc import Iterator
from flask import Response, send_file, stream_with_context
from requests.exceptions import MissingSchema, InvalidURL

from timApp.auth.accesshelper import verify_logged_in
from timApp.tim_app import app
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import add_csp_header, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
import ipaddress
import socket

proxy = TypedBlueprint("proxy", __name__, url_prefix="")


@proxy.get("/getproxy")
def getproxy(
    url: str,
    auth_token: str | None = None,
    raw: bool = False,
    mimetype: str | None = None,
    file: bool = False,
) -> Response:
    parsed = urlparse(url)
    if not parsed.scheme:
        raise RouteException("Unknown URL scheme")
    if parsed.scheme not in ("http", "https"):
        raise RouteException(f"URL scheme not allowed: {parsed.scheme}")
    if parsed.netloc not in app.config["PROXY_WHITELIST"]:
        raise RouteException(f"URL domain not whitelisted: {parsed.netloc}")
    if parsed.netloc not in app.config["PROXY_WHITELIST_NO_LOGIN"]:
        verify_logged_in()
    headers = {}
    if auth_token:
        headers["Authorization"] = f"Token {auth_token}"
    try:
        r = requests.get(url, headers=headers)
    except (MissingSchema, InvalidURL):
        raise RouteException("Invalid URL")
    except requests.exceptions.ConnectionError as e:
        req_url = e.request.url if e.request else url
        raise RouteException(f"Connection error to {req_url}: {e}")
    if raw:
        mimetype = mimetype or r.headers.get("Content-Type", None)
        resp = Response(
            r.content,
            status=r.status_code,
            mimetype=mimetype,
        )
        add_csp_header(resp, "sandbox allow-scripts")
        resp.headers["Access-Control-Allow-Origin"] = "*"
        return resp
    if file and r.status_code == 200:
        filename = basename(parsed.path) or "download"
        mimetype = r.headers.get("Content-Type", "application/octet-stream")
        return send_file(
            BytesIO(r.content),
            as_attachment=True,
            download_name=filename,
            mimetype=mimetype,
        )

    return json_response({"data": r.text, "status_code": r.status_code})


def is_safe_public_url(hostname: str) -> bool:
    """Verify that the hostname resolves to a public IP address (SSRF protection).

    :param hostname: Hostname to validate.
    :return: True if the hostname resolves to a public IP address, False otherwise.
    """
    try:
        if app.debug:
            return True
        ip_str = socket.gethostbyname(hostname)
        ip = ipaddress.ip_address(ip_str)
        return not (
            ip.is_private
            or ip.is_loopback
            or ip.is_link_local
            or ip.is_multicast
            or ip.is_reserved
        )
    except (socket.gaierror, ValueError):
        return False


def pdf_error_response(message: str, status: int = 400) -> Response:
    """Return a lightweight HTML error message for rendering inside an iframe without TIM page chrome.

    :param message: Error message describing why the PDF failed to load.
    :param status: HTTP status code for the error response.
    :return: Flask Response containing the formatted HTML error notice.
    """
    escaped_message = html.escape(message)
    body = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <style>
    body {{
      font-family: sans-serif;
      color: #db0404;
      border: 1px solid #f5c6cb;
      border-radius: 4px;
      padding: 16px;
      margin: 16px;
    }}
  </style>
</head>
<body>
  <strong>Failed to load PDF file:</strong><br>{escaped_message}
</body>
</html>"""
    return Response(body, status=status, mimetype="text/html; charset=utf-8")


@proxy.get("/get_mimetype")
def get_mimetype(url: str) -> Response:
    """Inspect an external URL and return its detected MIME type.

    :param url: External URL to inspect.
    :return: JSON response containing the detected 'mimetype' (or None if unknown/disallowed).
    """
    verify_logged_in()

    parsed = urlparse(url)
    if parsed.scheme not in ("http", "https"):
        return json_response({"mimetype": None})

    hostname = parsed.hostname
    if not hostname or not is_safe_public_url(hostname):
        return json_response({"mimetype": None})

    try:
        # Try a lightweight HEAD request first
        r = requests.head(url, timeout=5, allow_redirects=True)
        raw_ct = r.headers.get("Content-Type", "").lower()
        # Strip parameters like '; charset=utf-8'
        clean_ct = raw_ct.split(";")[0].strip() if raw_ct else ""

        if clean_ct and clean_ct != "application/octet-stream" and r.status_code == 200:
            return json_response({"mimetype": clean_ct})

        # Fallback: If HEAD is blocked (403/405) or returns generic octet-stream, peek at magic bytes
        if r.status_code in (403, 405) or clean_ct in ("", "application/octet-stream"):
            r_get = requests.get(url, timeout=5, stream=True)
            chunk = next(r_get.iter_content(chunk_size=16), b"")
            get_ct = r_get.headers.get("Content-Type", "").lower().split(";")[0].strip()
            r_get.close()

            if chunk.startswith(b"%PDF-"):
                return json_response({"mimetype": "application/pdf"})
            if get_ct and get_ct != "application/octet-stream":
                return json_response({"mimetype": get_ct})

    except requests.exceptions.RequestException:
        pass

    return json_response({"mimetype": None})


@proxy.get("/getpdfproxy")
def getpdfproxy(url: str) -> Response:
    """Fetch an external PDF file, validate its header, and stream it safely for inline viewing.

    :param url: URL of the external PDF file to fetch.
    :return: Flask Response streaming the validated PDF document or an HTML error response.
    """
    verify_logged_in()

    parsed = urlparse(url)
    hostname = parsed.hostname
    if parsed.scheme not in ("http", "https"):
        return pdf_error_response(f"URL scheme not allowed: {parsed.scheme}")
    if not hostname or not is_safe_public_url(hostname):
        return pdf_error_response(
            "URL host is not allowed (private or invalid address)."
        )

    try:
        r = requests.get(url, timeout=15, stream=True)
    except (MissingSchema, InvalidURL):
        return pdf_error_response("Invalid URL")
    except requests.exceptions.RequestException as e:
        return pdf_error_response(f"Error fetching file: {e}")

    if r.status_code != 200:
        r.close()
        return pdf_error_response(
            f"External server returned status code {r.status_code}"
        )

    # Check incoming Content-Type (allow octet-stream as a generic binary fallback)
    incoming_ct = r.headers.get("Content-Type", "").lower()
    if not (
        "application/pdf" in incoming_ct or "application/octet-stream" in incoming_ct
    ):
        r.close()
        return pdf_error_response(
            f"Invalid content type ({incoming_ct}). Expected a PDF file."
        )

    # Check Content-Length upfront if provided by the external server
    max_size = 50 * 1024 * 1024
    content_length = r.headers.get("Content-Length")
    if content_length and content_length.isdigit() and int(content_length) > max_size:
        r.close()
        return pdf_error_response("PDF file is too large (maximum 50 MB).")

    # Peek at the first chunk to verify magic bytes (%PDF-) before streaming
    chunk_iter = r.iter_content(chunk_size=8192)
    try:
        first_chunk = next(chunk_iter, b"")
    except requests.exceptions.RequestException as e:
        r.close()
        return pdf_error_response(f"Error reading file stream: {e}")

    if not first_chunk.startswith(b"%PDF-"):
        r.close()
        return pdf_error_response("Downloaded file is not a valid PDF document.")

    def generate() -> Iterator[bytes]:
        """Yield validated PDF chunks up to max_size and ensure the upstream connection closes."""
        bytes_sent = len(first_chunk)
        try:
            yield first_chunk
            for chunk in chunk_iter:
                if not chunk:
                    continue
                bytes_sent += len(chunk)
                if bytes_sent > max_size:
                    break
                yield chunk
        finally:
            r.close()

    # Extract a clean filename for Chrome's PDF viewer title and download dialog
    filename = "document.pdf"
    orig_cd = r.headers.get("Content-Disposition", "")
    cd_match = re.search(
        r'filename\*?=(?:UTF-8\'\')?"?([^";]+)"?', orig_cd, re.IGNORECASE
    )
    if cd_match:
        filename = os.path.basename(unquote(cd_match.group(1)))
    else:
        url_basename = os.path.basename(unquote(parsed.path))
        if url_basename:
            filename = url_basename

    # Sanitize filename to prevent header injection and ensure .pdf extension
    filename = re.sub(r'["\\\r\n]', "", filename).strip() or "document.pdf"
    # if not filename.lower().endswith(".pdf"):
    #    filename += ".pdf"

    resp = Response(
        stream_with_context(generate()),
        status=200,
        mimetype="application/pdf",
    )
    # Prevent browser from MIME-sniffing the content as HTML
    resp.headers["X-Content-Type-Options"] = "nosniff"
    # Render inline instead of forcing a download
    resp.headers["Content-Disposition"] = f'inline; filename="{filename}"'
    # Strict CSP without 'sandbox' directive so Chrome's built-in PDF viewer works
    add_csp_header(
        resp, "default-src 'none'; object-src 'self'; style-src 'unsafe-inline'"
    )
    resp.headers["Access-Control-Allow-Origin"] = "*"
    if content_length and content_length.isdigit():
        resp.headers["Content-Length"] = content_length

    return resp
