from lxml import html

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.util.utils import decode_csplugin

diamond_tex = r"""
\begin{tikzpicture}
\node[shape=diamond] {diamond};
\end{tikzpicture}
""".strip()

diamond_svg = f"""
<span class="mathp display"><img style="width:6.93474em; vertical-align:-0.00000em" src="data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnPz4KPCEtLSBUaGlzIGZpbGUgd2FzIGdlbmVyYXRlZCBieSBkdmlzdmdtIDIuMTIgLS0+CjxzdmcgdmVyc2lvbj0nMS4xJyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHhtbG5zOnhsaW5rPSdodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rJyB3aWR0aD0nNTcuNzg5NTE5cHQnIGhlaWdodD0nNTcuNzg5NTE5cHQnIHZpZXdCb3g9Jy03MiAtNzIgNTcuNzg5NTE5IDU3Ljc4OTUxOSc+CjxkZWZzPgo8cGF0aCBpZD0nZzAtMjgnIGQ9J000LjgxMTk1NS0uODg2Njc1Vi0xLjQ0NDU4M0g0LjU2Mjg4OVYtLjg4NjY3NUM0LjU2Mjg4OS0uMzA4ODQyIDQuMzEzODIzLS4yNDkwNjYgNC4yMDQyMzQtLjI0OTA2NkMzLjg3NTQ2Ny0uMjQ5MDY2IDMuODM1NjE2LS42OTczODUgMy44MzU2MTYtLjc0NzE5OFYtMi43Mzk3MjZDMy44MzU2MTYtMy4xNTgxNTcgMy44MzU2MTYtMy41NDY3IDMuNDc2OTYxLTMuOTE1MzE4QzMuMDg4NDE4LTQuMzAzODYxIDIuNTkwMjg2LTQuNDYzMjYzIDIuMTEyMDgtNC40NjMyNjNDMS4yOTUxNDMtNC40NjMyNjMgLjYwNzcyMS0zLjk5NTAxOSAuNjA3NzIxLTMuMzM3NDg0Qy42MDc3MjEtMy4wMzg2MDUgLjgwNjk3NC0yLjg2OTI0IDEuMDY2MDAyLTIuODY5MjRDMS4zNDQ5NTYtMi44NjkyNCAxLjUyNDI4NC0zLjA2ODQ5MyAxLjUyNDI4NC0zLjMyNzUyMkMxLjUyNDI4NC0zLjQ0NzA3MyAxLjQ3NDQ3MS0zLjc3NTg0MSAxLjAxNjE4OS0zLjc4NTgwM0MxLjI4NTE4MS00LjEzNDQ5NiAxLjc3MzM1LTQuMjQ0MDg1IDIuMDkyMTU0LTQuMjQ0MDg1QzIuNTgwMzI0LTQuMjQ0MDg1IDMuMTQ4MTk0LTMuODU1NTQyIDMuMTQ4MTk0LTIuOTY4ODY3Vi0yLjYwMDI0OUMyLjY0MDEtMi41NzAzNjEgMS45NDI3MTUtMi41NDA0NzMgMS4zMTUwNjgtMi4yNDE1OTRDLjU2Nzg3LTEuOTAyODY0IC4zMTg4MDQtMS4zODQ4MDcgLjMxODgwNC0uOTQ2NDUxQy4zMTg4MDQtLjEzOTQ3NyAxLjI4NTE4MSAuMTA5NTg5IDEuOTEyODI3IC4xMDk1ODlDMi41NzAzNjEgLjEwOTU4OSAzLjAyODY0My0uMjg4OTE3IDMuMjE3OTMzLS43NTcxNjFDMy4yNTc3ODMtLjM1ODY1NSAzLjUyNjc3NSAuMDU5Nzc2IDMuOTk1MDE5IC4wNTk3NzZDNC4yMDQyMzQgLjA1OTc3NiA0LjgxMTk1NS0uMDc5NzAxIDQuODExOTU1LS44ODY2NzVaTTMuMTQ4MTk0LTEuMzk0NzdDMy4xNDgxOTQtLjQ0ODMxOSAyLjQzMDg4NC0uMTA5NTg5IDEuOTgyNTY1LS4xMDk1ODlDMS40OTQzOTYtLjEwOTU4OSAxLjA4NTkyOC0uNDU4MjgxIDEuMDg1OTI4LS45NTY0MTNDMS4wODU5MjgtMS41MDQzNTkgMS41MDQzNTktMi4zMzEyNTggMy4xNDgxOTQtMi4zOTEwMzRWLTEuMzk0NzdaJy8+CjxwYXRoIGlkPSdnMC00NycgZD0nTTUuMjUwMzExIDBWLS4zMDg4NDJDNC41NTI5MjctLjMwODg0MiA0LjQ3MzIyNS0uMzc4NTggNC40NzMyMjUtLjg2Njc1Vi02LjkxNDA3MkwzLjAzODYwNS02LjgwNDQ4M1YtNi40OTU2NDFDMy43MzU5OS02LjQ5NTY0MSAzLjgxNTY5MS02LjQyNTkwMyAzLjgxNTY5MS01LjkzNzczM1YtMy43ODU4MDNDMy41MjY3NzUtNC4xNDQ0NTggMy4wOTgzODEtNC40MDM0ODcgMi41NjAzOTktNC40MDM0ODdDMS4zODQ4MDctNC40MDM0ODcgLjMzODczLTMuNDI3MTQ4IC4zMzg3My0yLjE0MTk2OEMuMzM4NzMtLjg3NjcxMiAxLjMxNTA2OCAuMTA5NTg5IDIuNDUwODA5IC4xMDk1ODlDMy4wODg0MTggLjEwOTU4OSAzLjUzNjczNy0uMjI5MTQxIDMuNzg1ODAzLS41NDc5NDVWLjEwOTU4OUw1LjI1MDMxMSAwWk0zLjc4NTgwMy0xLjE3NTU5MkMzLjc4NTgwMy0uOTk2MjY0IDMuNzg1ODAzLS45NzYzMzkgMy42NzYyMTQtLjgwNjk3NEMzLjM3NzMzNS0uMzI4NzY3IDIuOTI5MDE2LS4xMDk1ODkgMi41MDA2MjMtLjEwOTU4OUMyLjA1MjMwNC0uMTA5NTg5IDEuNjkzNjQ5LS4zNjg2MTggMS40NTQ1NDUtLjc0NzE5OEMxLjE5NTUxNy0xLjE1NTY2NiAxLjE2NTYyOS0xLjcyMzUzNyAxLjE2NTYyOS0yLjEzMjAwNUMxLjE2NTYyOS0yLjUwMDYyMyAxLjE4NTU1NC0zLjA5ODM4MSAxLjQ3NDQ3MS0zLjU0NjdDMS42ODM2ODYtMy44NTU1NDIgMi4wNjIyNjctNC4xODQzMDkgMi42MDAyNDktNC4xODQzMDlDMi45NDg5NDEtNC4xODQzMDkgMy4zNjczNzItNC4wMzQ4NjkgMy42NzYyMTQtMy41ODY1NUMzLjc4NTgwMy0zLjQxNzE4NiAzLjc4NTgwMy0zLjM5NzI2IDMuNzg1ODAzLTMuMjE3OTMzVi0xLjE3NTU5MlonLz4KPHBhdGggaWQ9J2cwLTY2JyBkPSdNMi40NjA3NzIgMFYtLjMwODg0MkMxLjgwMzIzOC0uMzA4ODQyIDEuNzYzMzg3LS4zNTg2NTUgMS43NjMzODctLjc0NzE5OFYtNC40MDM0ODdMLjM2ODYxOC00LjI5Mzg5OFYtMy45ODUwNTZDMS4wMTYxODktMy45ODUwNTYgMS4xMDU4NTMtMy45MjUyOCAxLjEwNTg1My0zLjQzNzExMVYtLjc1NzE2MUMxLjEwNTg1My0uMzA4ODQyIC45OTYyNjQtLjMwODg0MiAuMzI4NzY3LS4zMDg4NDJWMEwxLjQyNDY1OC0uMDI5ODg4QzEuNzczMzUtLjAyOTg4OCAyLjEyMjA0Mi0uMDA5OTYzIDIuNDYwNzcyIDBaTTEuOTEyODI3LTYuMDE3NDM1QzEuOTEyODI3LTYuMjg2NDI2IDEuNjgzNjg2LTYuNTQ1NDU1IDEuMzg0ODA3LTYuNTQ1NDU1QzEuMDQ2MDc3LTYuNTQ1NDU1IC44NDY4MjQtNi4yNjY1MDEgLjg0NjgyNC02LjAxNzQzNUMuODQ2ODI0LTUuNzQ4NDQzIDEuMDc1OTY1LTUuNDg5NDE1IDEuMzc0ODQ0LTUuNDg5NDE1QzEuNzEzNTc0LTUuNDg5NDE1IDEuOTEyODI3LTUuNzY4MzY5IDEuOTEyODI3LTYuMDE3NDM1WicvPgo8cGF0aCBpZD0nZzAtNzUnIGQ9J004LjA5OTYyNiAwVi0uMzA4ODQyQzcuNTgxNTY5LS4zMDg4NDIgNy4zMzI1MDMtLjMwODg0MiA3LjMyMjU0LS42MDc3MjFWLTIuNTEwNTg1QzcuMzIyNTQtMy4zNjczNzIgNy4zMjI1NC0zLjY3NjIxNCA3LjAxMzY5OS00LjAzNDg2OUM2Ljg3NDIyMi00LjIwNDIzNCA2LjU0NTQ1NS00LjQwMzQ4NyA1Ljk2NzYyMS00LjQwMzQ4N0M1LjEzMDc2LTQuNDAzNDg3IDQuNjkyNDAzLTMuODA1NzI5IDQuNTIzMDM5LTMuNDI3MTQ4QzQuMzgzNTYyLTQuMjkzODk4IDMuNjQ2MzI2LTQuNDAzNDg3IDMuMTk4MDA3LTQuNDAzNDg3QzIuNDcwNzM1LTQuNDAzNDg3IDIuMDAyNDkxLTMuOTc1MDkzIDEuNzIzNTM3LTMuMzU3NDFWLTQuNDAzNDg3TC4zMTg4MDQtNC4yOTM4OThWLTMuOTg1MDU2QzEuMDE2MTg5LTMuOTg1MDU2IDEuMDk1ODktMy45MTUzMTggMS4wOTU4OS0zLjQyNzE0OFYtLjc1NzE2MUMxLjA5NTg5LS4zMDg4NDIgLjk4NjMwMS0uMzA4ODQyIC4zMTg4MDQtLjMwODg0MlYwTDEuNDQ0NTgzLS4wMjk4ODhMMi41NjAzOTkgMFYtLjMwODg0MkMxLjg5MjkwMi0uMzA4ODQyIDEuNzgzMzEzLS4zMDg4NDIgMS43ODMzMTMtLjc1NzE2MVYtMi41OTAyODZDMS43ODMzMTMtMy42MjY0MDEgMi40OTA2Ni00LjE4NDMwOSAzLjEyODI2OS00LjE4NDMwOUMzLjc1NTkxNS00LjE4NDMwOSAzLjg2NTUwNC0zLjY0NjMyNiAzLjg2NTUwNC0zLjA3ODQ1NlYtLjc1NzE2MUMzLjg2NTUwNC0uMzA4ODQyIDMuNzU1OTE1LS4zMDg4NDIgMy4wODg0MTgtLjMwODg0MlYwTDQuMjE0MTk3LS4wMjk4ODhMNS4zMzAwMTIgMFYtLjMwODg0MkM0LjY2MjUxNi0uMzA4ODQyIDQuNTUyOTI3LS4zMDg4NDIgNC41NTI5MjctLjc1NzE2MVYtMi41OTAyODZDNC41NTI5MjctMy42MjY0MDEgNS4yNjAyNzQtNC4xODQzMDkgNS44OTc4ODMtNC4xODQzMDlDNi41MjU1MjktNC4xODQzMDkgNi42MzUxMTgtMy42NDYzMjYgNi42MzUxMTgtMy4wNzg0NTZWLS43NTcxNjFDNi42MzUxMTgtLjMwODg0MiA2LjUyNTUyOS0uMzA4ODQyIDUuODU4MDMyLS4zMDg4NDJWMEw2Ljk4MzgxMS0uMDI5ODg4TDguMDk5NjI2IDBaJy8+CjxwYXRoIGlkPSdnMC03NycgZD0nTTUuMzMwMDEyIDBWLS4zMDg4NDJDNC44MTE5NTUtLjMwODg0MiA0LjU2Mjg4OS0uMzA4ODQyIDQuNTUyOTI3LS42MDc3MjFWLTIuNTEwNTg1QzQuNTUyOTI3LTMuMzY3MzcyIDQuNTUyOTI3LTMuNjc2MjE0IDQuMjQ0MDg1LTQuMDM0ODY5QzQuMTA0NjA4LTQuMjA0MjM0IDMuNzc1ODQxLTQuNDAzNDg3IDMuMTk4MDA3LTQuNDAzNDg3QzIuNDcwNzM1LTQuNDAzNDg3IDIuMDAyNDkxLTMuOTc1MDkzIDEuNzIzNTM3LTMuMzU3NDFWLTQuNDAzNDg3TC4zMTg4MDQtNC4yOTM4OThWLTMuOTg1MDU2QzEuMDE2MTg5LTMuOTg1MDU2IDEuMDk1ODktMy45MTUzMTggMS4wOTU4OS0zLjQyNzE0OFYtLjc1NzE2MUMxLjA5NTg5LS4zMDg4NDIgLjk4NjMwMS0uMzA4ODQyIC4zMTg4MDQtLjMwODg0MlYwTDEuNDQ0NTgzLS4wMjk4ODhMMi41NjAzOTkgMFYtLjMwODg0MkMxLjg5MjkwMi0uMzA4ODQyIDEuNzgzMzEzLS4zMDg4NDIgMS43ODMzMTMtLjc1NzE2MVYtMi41OTAyODZDMS43ODMzMTMtMy42MjY0MDEgMi40OTA2Ni00LjE4NDMwOSAzLjEyODI2OS00LjE4NDMwOUMzLjc1NTkxNS00LjE4NDMwOSAzLjg2NTUwNC0zLjY0NjMyNiAzLjg2NTUwNC0zLjA3ODQ1NlYtLjc1NzE2MUMzLjg2NTUwNC0uMzA4ODQyIDMuNzU1OTE1LS4zMDg4NDIgMy4wODg0MTgtLjMwODg0MlYwTDQuMjE0MTk3LS4wMjk4ODhMNS4zMzAwMTIgMFonLz4KPHBhdGggaWQ9J2cwLTgxJyBkPSdNNC42OTI0MDMtMi4xMzIwMDVDNC42OTI0MDMtMy40MDcyMjMgMy42OTYxMzktNC40NjMyNjMgMi40OTA2Ni00LjQ2MzI2M0MxLjI0NTMzLTQuNDYzMjYzIC4yNzg5NTQtMy4zNzczMzUgLjI3ODk1NC0yLjEzMjAwNUMuMjc4OTU0LS44NDY4MjQgMS4zMTUwNjggLjEwOTU4OSAyLjQ4MDY5NyAuMTA5NTg5QzMuNjg2MTc3IC4xMDk1ODkgNC42OTI0MDMtLjg2Njc1IDQuNjkyNDAzLTIuMTMyMDA1Wk0zLjg2NTUwNC0yLjIxMTcwNkMzLjg2NTUwNC0xLjg1MzA1MSAzLjg2NTUwNC0xLjMxNTA2OCAzLjY0NjMyNi0uODc2NzEyQzMuNDI3MTQ4LS40MjgzOTQgMi45ODg3OTItLjEzOTQ3NyAyLjQ5MDY2LS4xMzk0NzdDMi4wNjIyNjctLjEzOTQ3NyAxLjYyMzkxLS4zNDg2OTIgMS4zNTQ5MTktLjgwNjk3NEMxLjEwNTg1My0xLjI0NTMzIDEuMTA1ODUzLTEuODUzMDUxIDEuMTA1ODUzLTIuMjExNzA2QzEuMTA1ODUzLTIuNjAwMjQ5IDEuMTA1ODUzLTMuMTM4MjMyIDEuMzQ0OTU2LTMuNTc2NTg4QzEuNjEzOTQ4LTQuMDM0ODY5IDIuMDgyMTkyLTQuMjQ0MDg1IDIuNDgwNjk3LTQuMjQ0MDg1QzIuOTE5MDU0LTQuMjQ0MDg1IDMuMzQ3NDQ3LTQuMDI0OTA3IDMuNjA2NDc2LTMuNTk2NTEzUzMuODY1NTA0LTIuNTkwMjg2IDMuODY1NTA0LTIuMjExNzA2WicvPgo8L2RlZnM+CjxnIGlkPSdwYWdlMSc+CjxnIHN0cm9rZS1taXRlcmxpbWl0PScxMCcgdHJhbnNmb3JtPSd0cmFuc2xhdGUoLTQzLjEwNTI0NywtNDMuMTA1MjQ3KXNjYWxlKDAuOTk2MjY0LC0wLjk5NjI2NCknPgo8ZyBmaWxsPScjMDAwJyBzdHJva2U9JyMwMDAnPgo8ZyBzdHJva2Utd2lkdGg9JzAuNCc+CjxnIHRyYW5zZm9ybT0ndHJhbnNsYXRlKC0xOC44OTUsLTMuNDE1MDEpJz4KPGcgc3Ryb2tlPSdub25lJyB0cmFuc2Zvcm09J3NjYWxlKC0xLjAwMzc1LDEuMDAzNzUpdHJhbnNsYXRlKC00My4xMDUyNDcsLTQzLjEwNTI0NylzY2FsZSgtMSwtMSknPgo8ZyBmaWxsPScjMDAwJz4KPGcgc3Ryb2tlPSdub25lJz4KPHVzZSB4PSctNDMuMTA1MjQ3JyB5PSctNDMuMTA1MjQ3JyB4bGluazpocmVmPScjZzAtNDcnLz4KPHVzZSB4PSctMzcuNTY2MDIxJyB5PSctNDMuMTA1MjQ3JyB4bGluazpocmVmPScjZzAtNjYnLz4KPHVzZSB4PSctMzQuNzk2NDA5JyB5PSctNDMuMTA1MjQ3JyB4bGluazpocmVmPScjZzAtMjgnLz4KPHVzZSB4PSctMjkuODE1MDg5JyB5PSctNDMuMTA1MjQ3JyB4bGluazpocmVmPScjZzAtNzUnLz4KPHVzZSB4PSctMjEuNTE2MjA4JyB5PSctNDMuMTA1MjQ3JyB4bGluazpocmVmPScjZzAtODEnLz4KPHVzZSB4PSctMTYuNTM0ODg3JyB5PSctNDMuMTA1MjQ3JyB4bGluazpocmVmPScjZzAtNzcnLz4KPHVzZSB4PSctMTAuOTk1NjYyJyB5PSctNDMuMTA1MjQ3JyB4bGluazpocmVmPScjZzAtNDcnLz4KPC9nPgo8L2c+CjwvZz4KPC9nPgo8L2c+CjwvZz4KPC9nPgo8L2c+Cjwvc3ZnPg==" title="{diamond_tex}"></span>
"""

a_plus_b_mathjax = r"""<span class="math inline">\(a+b\)</span>"""
a_plus_b_svg = """<span class="mathp inline"><img style="width:2.72619em; vertical-align:-0.15963em" src="data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnPz4KPCEtLSBUaGlzIGZpbGUgd2FzIGdlbmVyYXRlZCBieSBkdmlzdmdtIDIuMTIgLS0+CjxzdmcgdmVyc2lvbj0nMS4xJyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHhtbG5zOnhsaW5rPSdodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rJyB3aWR0aD0nMjIuNzE4MjQzcHQnIGhlaWdodD0nOC43NDg3MTdwdCcgdmlld0JveD0nLS41MDAwMDIgLTcuNDE4NSAyMi43MTgyNDMgOC43NDg3MTcnPgo8ZGVmcz4KPHN0eWxlIHR5cGU9J3RleHQvY3NzJz4KPCFbQ0RBVEFbcGF0aCB7c3Ryb2tlOiBjdXJyZW50Q29sb3I7c3Ryb2tlLXdpZHRoOiAwLjA1cHQ7fV1dPgo8L3N0eWxlPgo8cGF0aCBpZD0nZzEtNDMnIGQ9J000LjA3NDcyLTIuMjkxNDA3SDYuODU0Mjk2QzYuOTkzNzczLTIuMjkxNDA3IDcuMTgzMDY0LTIuMjkxNDA3IDcuMTgzMDY0LTIuNDkwNjZTNi45OTM3NzMtMi42ODk5MTMgNi44NTQyOTYtMi42ODk5MTNINC4wNzQ3MlYtNS40Nzk0NTJDNC4wNzQ3Mi01LjYxODkyOSA0LjA3NDcyLTUuODA4MjE5IDMuODc1NDY3LTUuODA4MjE5UzMuNjc2MjE0LTUuNjE4OTI5IDMuNjc2MjE0LTUuNDc5NDUyVi0yLjY4OTkxM0guODg2Njc1Qy43NDcxOTgtMi42ODk5MTMgLjU1NzkwOC0yLjY4OTkxMyAuNTU3OTA4LTIuNDkwNjZTLjc0NzE5OC0yLjI5MTQwNyAuODg2Njc1LTIuMjkxNDA3SDMuNjc2MjE0Vi40OTgxMzJDMy42NzYyMTQgLjYzNzYwOSAzLjY3NjIxNCAuODI2ODk5IDMuODc1NDY3IC44MjY4OTlTNC4wNzQ3MiAuNjM3NjA5IDQuMDc0NzIgLjQ5ODEzMlYtMi4yOTE0MDdaJy8+CjxwYXRoIGlkPSdnMC05NycgZD0nTTMuNzE2MDY1LTMuNzY1ODc4QzMuNTM2NzM3LTQuMTM0NDk2IDMuMjQ3ODIxLTQuNDAzNDg3IDIuNzk5NTAyLTQuNDAzNDg3QzEuNjMzODczLTQuNDAzNDg3IC4zOTg1MDYtMi45Mzg5NzkgLjM5ODUwNi0xLjQ4NDQzM0MuMzk4NTA2LS41NDc5NDUgLjk0NjQ1MSAuMTA5NTg5IDEuNzIzNTM3IC4xMDk1ODlDMS45MjI3OSAuMTA5NTg5IDIuNDIwOTIyIC4wNjk3MzggMy4wMTg2OC0uNjM3NjA5QzMuMDk4MzgxLS4yMTkxNzggMy40NDcwNzMgLjEwOTU4OSAzLjkyNTI4IC4xMDk1ODlDNC4yNzM5NzMgLjEwOTU4OSA0LjUwMzExMy0uMTE5NTUyIDQuNjYyNTE2LS40MzgzNTZDNC44MzE4OC0uNzk3MDExIDQuOTYxMzk1LTEuNDA0NzMyIDQuOTYxMzk1LTEuNDI0NjU4QzQuOTYxMzk1LTEuNTI0Mjg0IDQuODcxNzMxLTEuNTI0Mjg0IDQuODQxODQzLTEuNTI0Mjg0QzQuNzQyMjE3LTEuNTI0Mjg0IDQuNzMyMjU0LTEuNDg0NDMzIDQuNzAyMzY2LTEuMzQ0OTU2QzQuNTMzMDAxLS42OTczODUgNC4zNTM2NzQtLjEwOTU4OSAzLjk0NTIwNS0uMTA5NTg5QzMuNjc2MjE0LS4xMDk1ODkgMy42NDYzMjYtLjM2ODYxOCAzLjY0NjMyNi0uNTY3ODdDMy42NDYzMjYtLjc4NzA0OSAzLjY2NjI1Mi0uODY2NzUgMy43NzU4NDEtMS4zMDUxMDZDMy44ODU0My0xLjcyMzUzNyAzLjkwNTM1NS0xLjgyMzE2MyAzLjk5NTAxOS0yLjIwMTc0M0w0LjM1MzY3NC0zLjU5NjUxM0M0LjQyMzQxMi0zLjg3NTQ2NyA0LjQyMzQxMi0zLjg5NTM5MiA0LjQyMzQxMi0zLjkzNTI0M0M0LjQyMzQxMi00LjEwNDYwOCA0LjMwMzg2MS00LjIwNDIzNCA0LjEzNDQ5Ni00LjIwNDIzNEMzLjg5NTM5Mi00LjIwNDIzNCAzLjc0NTk1My0zLjk4NTA1NiAzLjcxNjA2NS0zLjc2NTg3OFpNMy4wNjg0OTMtMS4xODU1NTRDMy4wMTg2OC0xLjAwNjIyNyAzLjAxODY4LS45ODYzMDEgMi44NjkyNC0uODE2OTM2QzIuNDMwODg0LS4yNjg5OTEgMi4wMjI0MTYtLjEwOTU4OSAxLjc0MzQ2Mi0uMTA5NTg5QzEuMjQ1MzMtLjEwOTU4OSAxLjEwNTg1My0uNjU3NTM0IDEuMTA1ODUzLTEuMDQ2MDc3QzEuMTA1ODUzLTEuNTQ0MjA5IDEuNDI0NjU4LTIuNzY5NjE0IDEuNjUzNzk4LTMuMjI3ODk1QzEuOTYyNjQtMy44MTU2OTEgMi40MTA5NTktNC4xODQzMDkgMi44MDk0NjUtNC4xODQzMDlDMy40NTcwMzYtNC4xODQzMDkgMy41OTY1MTMtMy4zNjczNzIgMy41OTY1MTMtMy4zMDc1OTdTMy41NzY1ODgtMy4xODgwNDUgMy41NjY2MjUtMy4xMzgyMzJMMy4wNjg0OTMtMS4xODU1NTRaJy8+CjxwYXRoIGlkPSdnMC05OCcgZD0nTTIuMzgxMDcxLTYuODA0NDgzQzIuMzgxMDcxLTYuODE0NDQ2IDIuMzgxMDcxLTYuOTE0MDcyIDIuMjUxNTU3LTYuOTE0MDcyQzIuMDIyNDE2LTYuOTE0MDcyIDEuMjk1MTQzLTYuODM0MzcxIDEuMDM2MTE1LTYuODE0NDQ2Qy45NTY0MTMtNi44MDQ0ODMgLjg0NjgyNC02Ljc5NDUyMSAuODQ2ODI0LTYuNjE1MTkzQy44NDY4MjQtNi40OTU2NDEgLjkzNjQ4OC02LjQ5NTY0MSAxLjA4NTkyOC02LjQ5NTY0MUMxLjU2NDEzNC02LjQ5NTY0MSAxLjU4NDA2LTYuNDI1OTAzIDEuNTg0MDYtNi4zMjYyNzZDMS41ODQwNi02LjI1NjUzOCAxLjQ5NDM5Ni01LjkxNzgwOCAxLjQ0NDU4My01LjcwODU5M0wuNjI3NjQ2LTIuNDYwNzcyQy41MDgwOTUtMS45NjI2NCAuNDY4MjQ0LTEuODAzMjM4IC40NjgyNDQtMS40NTQ1NDVDLjQ2ODI0NC0uNTA4MDk1IC45OTYyNjQgLjEwOTU4OSAxLjczMzQ5OSAuMTA5NTg5QzIuOTA5MDkxIC4xMDk1ODkgNC4xMzQ0OTYtMS4zNzQ4NDQgNC4xMzQ0OTYtMi44MDk0NjVDNC4xMzQ0OTYtMy43MTYwNjUgMy42MDY0NzYtNC40MDM0ODcgMi44MDk0NjUtNC40MDM0ODdDMi4zNTExODMtNC40MDM0ODcgMS45NDI3MTUtNC4xMTQ1NyAxLjY0MzgzNi0zLjgwNTcyOUwyLjM4MTA3MS02LjgwNDQ4M1pNMS40NDQ1ODMtMy4wMzg2MDVDMS41MDQzNTktMy4yNTc3ODMgMS41MDQzNTktMy4yNzc3MDkgMS41OTQwMjItMy4zODcyOThDMi4wODIxOTItNC4wMzQ4NjkgMi41MzA1MTEtNC4xODQzMDkgMi43ODk1MzktNC4xODQzMDlDMy4xNDgxOTQtNC4xODQzMDkgMy40MTcxODYtMy44ODU0MyAzLjQxNzE4Ni0zLjI0NzgyMUMzLjQxNzE4Ni0yLjY2MDAyNSAzLjA4ODQxOC0xLjUxNDMyMSAyLjkwOTA5MS0xLjEzNTc0MUMyLjU4MDMyNC0uNDY4MjQ0IDIuMTIyMDQyLS4xMDk1ODkgMS43MzM0OTktLjEwOTU4OUMxLjM5NDc3LS4xMDk1ODkgMS4wNjYwMDItLjM3ODU4IDEuMDY2MDAyLTEuMTE1ODE2QzEuMDY2MDAyLTEuMzA1MTA2IDEuMDY2MDAyLTEuNDk0Mzk2IDEuMjI1NDA1LTIuMTIyMDQyTDEuNDQ0NTgzLTMuMDM4NjA1WicvPgo8L2RlZnM+CjxnIGlkPSdwYWdlMSc+Cjx1c2UgeD0nMCcgeT0nMCcgeGxpbms6aHJlZj0nI2cwLTk3Jy8+Cjx1c2UgeD0nNy40ODAwMicgeT0nMCcgeGxpbms6aHJlZj0nI2cxLTQzJy8+Cjx1c2UgeD0nMTcuNDQyNjMzJyB5PScwJyB4bGluazpocmVmPScjZzAtOTgnLz4KPC9nPgo8L3N2Zz4=" title="a+b"></span>"""

mathjax_html = f"""
<div ng-non-bindable tabindex="0" class="parContent">
    <p>{a_plus_b_mathjax}
    </p>
</div>
"""

svg_html = f"""
<div ng-non-bindable tabindex="0" class="parContent">
    <p>{a_plus_b_svg}
    </p>
</div>
"""


class MathTest(TimRouteTest):
    def test_svg_math(self):
        self.login_test1()
        d = self.create_doc(initial_par="$a+b$")
        d.document.set_settings({"math_type": "svg"})
        self.assert_same_html(
            self.get(d.url, as_tree=True).cssselect(".parContent")[1],
            f"""
<div ng-non-bindable tabindex="0" class="parContent">
    <p>{a_plus_b_svg}
    </p>
</div>
""",
        )
        return d

    def test_mathjax_math(self):
        self.login_test1()
        d = self.create_doc(initial_par="$a+b$")
        d.document.set_settings({"math_type": "mathjax"})
        self.assert_same_html(
            self.get(d.url, as_tree=True).cssselect(".parContent")[1], mathjax_html
        )
        d.document.set_settings({"math_type": "xxx"})
        self.assert_same_html(
            self.get(d.url, as_tree=True).cssselect(".parContent")[1], mathjax_html
        )
        d.document.set_settings({"math_type": None})
        self.assert_same_html(
            self.get(d.url, as_tree=True).cssselect(".parContent")[1], mathjax_html
        )

    def test_mathtype_change(self):
        d = self.test_svg_math()
        d.document.set_settings({"math_type": "mathjax"})
        self.assert_same_html(
            self.get(d.url, as_tree=True).cssselect(".parContent")[1], mathjax_html
        )

    def test_math_preamble(self):
        self.login_test1()
        d = self.create_doc(
            initial_par=rf"""
$${diamond_tex}$$"""
        )
        d.document.set_settings(
            {
                "math_type": "svg",
                "math_preamble": r"""
\usetikzlibrary{shapes}
        """,
            }
        )
        self.assert_same_html(
            self.get(d.url, as_tree=True).cssselect(".parContent > p > span")[0],
            diamond_svg,
        )

    def test_math_preamble_single_par(self):
        self.login_test1()
        d = self.create_doc(
            initial_par=r"""
#- {math_type=svg math_preamble="\\usetikzlibrary{shapes}"}
"""
            f"""{diamond_tex}"""
        )
        t = self.get(d.url, as_tree=True)
        self.assert_same_html(t.cssselect(".parContent > span")[0], diamond_svg)

    def test_math_plugin(self):
        self.login_test1()
        d = self.create_doc(
            initial_par=r"""
#- {math_type=svg plugin=csPlugin}
stem: 'md: $a+b$'

#- {plugin=csPlugin}
stem: 'md: $a+b$'
"""
        )
        t = self.get(d.url, as_tree=True)
        plugins = t.cssselect("cs-runner")
        for plugin, e in zip(plugins, [a_plus_b_svg, a_plus_b_mathjax]):
            stem = decode_csplugin(plugin)["stem"]
            self.assert_same_html(html.fromstring(stem), e)

    def test_mixed_settings(self):
        self.login_test1()
        d = self.create_doc(
            initial_par=r"""
#- {math_preamble="\\newcommand{\\nothing}{}"}
$a+b$"""
        )
        d.document.set_settings({"math_type": "svg"})
        t = self.get(d.url, as_tree=True)
        self.assert_same_html(t.cssselect(".parContent")[1], svg_html)
