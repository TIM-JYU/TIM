from lxml import html

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.util.utils import decode_csplugin

diamond_tex = r"""
\begin{tikzpicture}
\node[shape=diamond] {diamond};
\end{tikzpicture}
""".strip()

diamond_svg = f"""
<span class="mathp display"><img style="width:6.93474em; vertical-align:-0.00000em" src="data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnPz4KPCEtLSBUaGlzIGZpbGUgd2FzIGdlbmVyYXRlZCBieSBkdmlzdmdtIDMuMi4xIC0tPgo8c3ZnIHZlcnNpb249JzEuMScgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzU3Ljc4OTUxOXB0JyBoZWlnaHQ9JzU3Ljc4OTUxOXB0JyB2aWV3Qm94PSctNzIgLTcyIDU3Ljc4OTUxOSA1Ny43ODk1MTknPgo8ZGVmcz4KPHBhdGggaWQ9J2cwLTI4JyBkPSdNNC44MTE5NTUtLjg4NjY3NVYtMS40NDQ1ODNINC41NjI4ODlWLS44ODY2NzVDNC41NjI4ODktLjMwODg0MiA0LjMxMzgyMy0uMjQ5MDY2IDQuMjA0MjM0LS4yNDkwNjZDMy44NzU0NjctLjI0OTA2NiAzLjgzNTYxNi0uNjk3Mzg1IDMuODM1NjE2LS43NDcxOThWLTIuNzM5NzI2QzMuODM1NjE2LTMuMTU4MTU3IDMuODM1NjE2LTMuNTQ2NyAzLjQ3Njk2MS0zLjkxNTMxOEMzLjA4ODQxOC00LjMwMzg2MSAyLjU5MDI4Ni00LjQ2MzI2MyAyLjExMjA4LTQuNDYzMjYzQzEuMjk1MTQzLTQuNDYzMjYzIC42MDc3MjEtMy45OTUwMTkgLjYwNzcyMS0zLjMzNzQ4NEMuNjA3NzIxLTMuMDM4NjA1IC44MDY5NzQtMi44NjkyNCAxLjA2NjAwMi0yLjg2OTI0QzEuMzQ0OTU2LTIuODY5MjQgMS41MjQyODQtMy4wNjg0OTMgMS41MjQyODQtMy4zMjc1MjJDMS41MjQyODQtMy40NDcwNzMgMS40NzQ0NzEtMy43NzU4NDEgMS4wMTYxODktMy43ODU4MDNDMS4yODUxODEtNC4xMzQ0OTYgMS43NzMzNS00LjI0NDA4NSAyLjA5MjE1NC00LjI0NDA4NUMyLjU4MDMyNC00LjI0NDA4NSAzLjE0ODE5NC0zLjg1NTU0MiAzLjE0ODE5NC0yLjk2ODg2N1YtMi42MDAyNDlDMi42NDAxLTIuNTcwMzYxIDEuOTQyNzE1LTIuNTQwNDczIDEuMzE1MDY4LTIuMjQxNTk0Qy41Njc4Ny0xLjkwMjg2NCAuMzE4ODA0LTEuMzg0ODA3IC4zMTg4MDQtLjk0NjQ1MUMuMzE4ODA0LS4xMzk0NzcgMS4yODUxODEgLjEwOTU4OSAxLjkxMjgyNyAuMTA5NTg5QzIuNTcwMzYxIC4xMDk1ODkgMy4wMjg2NDMtLjI4ODkxNyAzLjIxNzkzMy0uNzU3MTYxQzMuMjU3NzgzLS4zNTg2NTUgMy41MjY3NzUgLjA1OTc3NiAzLjk5NTAxOSAuMDU5Nzc2QzQuMjA0MjM0IC4wNTk3NzYgNC44MTE5NTUtLjA3OTcwMSA0LjgxMTk1NS0uODg2Njc1Wk0zLjE0ODE5NC0xLjM5NDc3QzMuMTQ4MTk0LS40NDgzMTkgMi40MzA4ODQtLjEwOTU4OSAxLjk4MjU2NS0uMTA5NTg5QzEuNDk0Mzk2LS4xMDk1ODkgMS4wODU5MjgtLjQ1ODI4MSAxLjA4NTkyOC0uOTU2NDEzQzEuMDg1OTI4LTEuNTA0MzU5IDEuNTA0MzU5LTIuMzMxMjU4IDMuMTQ4MTk0LTIuMzkxMDM0Vi0xLjM5NDc3WicvPgo8cGF0aCBpZD0nZzAtNDcnIGQ9J001LjI1MDMxMSAwVi0uMzA4ODQyQzQuNTUyOTI3LS4zMDg4NDIgNC40NzMyMjUtLjM3ODU4IDQuNDczMjI1LS44NjY3NVYtNi45MTQwNzJMMy4wMzg2MDUtNi44MDQ0ODNWLTYuNDk1NjQxQzMuNzM1OTktNi40OTU2NDEgMy44MTU2OTEtNi40MjU5MDMgMy44MTU2OTEtNS45Mzc3MzNWLTMuNzg1ODAzQzMuNTI2Nzc1LTQuMTQ0NDU4IDMuMDk4MzgxLTQuNDAzNDg3IDIuNTYwMzk5LTQuNDAzNDg3QzEuMzg0ODA3LTQuNDAzNDg3IC4zMzg3My0zLjQyNzE0OCAuMzM4NzMtMi4xNDE5NjhDLjMzODczLS44NzY3MTIgMS4zMTUwNjggLjEwOTU4OSAyLjQ1MDgwOSAuMTA5NTg5QzMuMDg4NDE4IC4xMDk1ODkgMy41MzY3MzctLjIyOTE0MSAzLjc4NTgwMy0uNTQ3OTQ1Vi4xMDk1ODlMNS4yNTAzMTEgMFpNMy43ODU4MDMtMS4xNzU1OTJDMy43ODU4MDMtLjk5NjI2NCAzLjc4NTgwMy0uOTc2MzM5IDMuNjc2MjE0LS44MDY5NzRDMy4zNzczMzUtLjMyODc2NyAyLjkyOTAxNi0uMTA5NTg5IDIuNTAwNjIzLS4xMDk1ODlDMi4wNTIzMDQtLjEwOTU4OSAxLjY5MzY0OS0uMzY4NjE4IDEuNDU0NTQ1LS43NDcxOThDMS4xOTU1MTctMS4xNTU2NjYgMS4xNjU2MjktMS43MjM1MzcgMS4xNjU2MjktMi4xMzIwMDVDMS4xNjU2MjktMi41MDA2MjMgMS4xODU1NTQtMy4wOTgzODEgMS40NzQ0NzEtMy41NDY3QzEuNjgzNjg2LTMuODU1NTQyIDIuMDYyMjY3LTQuMTg0MzA5IDIuNjAwMjQ5LTQuMTg0MzA5QzIuOTQ4OTQxLTQuMTg0MzA5IDMuMzY3MzcyLTQuMDM0ODY5IDMuNjc2MjE0LTMuNTg2NTVDMy43ODU4MDMtMy40MTcxODYgMy43ODU4MDMtMy4zOTcyNiAzLjc4NTgwMy0zLjIxNzkzM1YtMS4xNzU1OTJaJy8+CjxwYXRoIGlkPSdnMC02NicgZD0nTTIuNDYwNzcyIDBWLS4zMDg4NDJDMS44MDMyMzgtLjMwODg0MiAxLjc2MzM4Ny0uMzU4NjU1IDEuNzYzMzg3LS43NDcxOThWLTQuNDAzNDg3TC4zNjg2MTgtNC4yOTM4OThWLTMuOTg1MDU2QzEuMDE2MTg5LTMuOTg1MDU2IDEuMTA1ODUzLTMuOTI1MjggMS4xMDU4NTMtMy40MzcxMTFWLS43NTcxNjFDMS4xMDU4NTMtLjMwODg0MiAuOTk2MjY0LS4zMDg4NDIgLjMyODc2Ny0uMzA4ODQyVjBMMS40MjQ2NTgtLjAyOTg4OEMxLjc3MzM1LS4wMjk4ODggMi4xMjIwNDItLjAwOTk2MyAyLjQ2MDc3MiAwWk0xLjkxMjgyNy02LjAxNzQzNUMxLjkxMjgyNy02LjI4NjQyNiAxLjY4MzY4Ni02LjU0NTQ1NSAxLjM4NDgwNy02LjU0NTQ1NUMxLjA0NjA3Ny02LjU0NTQ1NSAuODQ2ODI0LTYuMjY2NTAxIC44NDY4MjQtNi4wMTc0MzVDLjg0NjgyNC01Ljc0ODQ0MyAxLjA3NTk2NS01LjQ4OTQxNSAxLjM3NDg0NC01LjQ4OTQxNUMxLjcxMzU3NC01LjQ4OTQxNSAxLjkxMjgyNy01Ljc2ODM2OSAxLjkxMjgyNy02LjAxNzQzNVonLz4KPHBhdGggaWQ9J2cwLTc1JyBkPSdNOC4wOTk2MjYgMFYtLjMwODg0MkM3LjU4MTU2OS0uMzA4ODQyIDcuMzMyNTAzLS4zMDg4NDIgNy4zMjI1NC0uNjA3NzIxVi0yLjUxMDU4NUM3LjMyMjU0LTMuMzY3MzcyIDcuMzIyNTQtMy42NzYyMTQgNy4wMTM2OTktNC4wMzQ4NjlDNi44NzQyMjItNC4yMDQyMzQgNi41NDU0NTUtNC40MDM0ODcgNS45Njc2MjEtNC40MDM0ODdDNS4xMzA3Ni00LjQwMzQ4NyA0LjY5MjQwMy0zLjgwNTcyOSA0LjUyMzAzOS0zLjQyNzE0OEM0LjM4MzU2Mi00LjI5Mzg5OCAzLjY0NjMyNi00LjQwMzQ4NyAzLjE5ODAwNy00LjQwMzQ4N0MyLjQ3MDczNS00LjQwMzQ4NyAyLjAwMjQ5MS0zLjk3NTA5MyAxLjcyMzUzNy0zLjM1NzQxVi00LjQwMzQ4N0wuMzE4ODA0LTQuMjkzODk4Vi0zLjk4NTA1NkMxLjAxNjE4OS0zLjk4NTA1NiAxLjA5NTg5LTMuOTE1MzE4IDEuMDk1ODktMy40MjcxNDhWLS43NTcxNjFDMS4wOTU4OS0uMzA4ODQyIC45ODYzMDEtLjMwODg0MiAuMzE4ODA0LS4zMDg4NDJWMEwxLjQ0NDU4My0uMDI5ODg4TDIuNTYwMzk5IDBWLS4zMDg4NDJDMS44OTI5MDItLjMwODg0MiAxLjc4MzMxMy0uMzA4ODQyIDEuNzgzMzEzLS43NTcxNjFWLTIuNTkwMjg2QzEuNzgzMzEzLTMuNjI2NDAxIDIuNDkwNjYtNC4xODQzMDkgMy4xMjgyNjktNC4xODQzMDlDMy43NTU5MTUtNC4xODQzMDkgMy44NjU1MDQtMy42NDYzMjYgMy44NjU1MDQtMy4wNzg0NTZWLS43NTcxNjFDMy44NjU1MDQtLjMwODg0MiAzLjc1NTkxNS0uMzA4ODQyIDMuMDg4NDE4LS4zMDg4NDJWMEw0LjIxNDE5Ny0uMDI5ODg4TDUuMzMwMDEyIDBWLS4zMDg4NDJDNC42NjI1MTYtLjMwODg0MiA0LjU1MjkyNy0uMzA4ODQyIDQuNTUyOTI3LS43NTcxNjFWLTIuNTkwMjg2QzQuNTUyOTI3LTMuNjI2NDAxIDUuMjYwMjc0LTQuMTg0MzA5IDUuODk3ODgzLTQuMTg0MzA5QzYuNTI1NTI5LTQuMTg0MzA5IDYuNjM1MTE4LTMuNjQ2MzI2IDYuNjM1MTE4LTMuMDc4NDU2Vi0uNzU3MTYxQzYuNjM1MTE4LS4zMDg4NDIgNi41MjU1MjktLjMwODg0MiA1Ljg1ODAzMi0uMzA4ODQyVjBMNi45ODM4MTEtLjAyOTg4OEw4LjA5OTYyNiAwWicvPgo8cGF0aCBpZD0nZzAtNzcnIGQ9J001LjMzMDAxMiAwVi0uMzA4ODQyQzQuODExOTU1LS4zMDg4NDIgNC41NjI4ODktLjMwODg0MiA0LjU1MjkyNy0uNjA3NzIxVi0yLjUxMDU4NUM0LjU1MjkyNy0zLjM2NzM3MiA0LjU1MjkyNy0zLjY3NjIxNCA0LjI0NDA4NS00LjAzNDg2OUM0LjEwNDYwOC00LjIwNDIzNCAzLjc3NTg0MS00LjQwMzQ4NyAzLjE5ODAwNy00LjQwMzQ4N0MyLjQ3MDczNS00LjQwMzQ4NyAyLjAwMjQ5MS0zLjk3NTA5MyAxLjcyMzUzNy0zLjM1NzQxVi00LjQwMzQ4N0wuMzE4ODA0LTQuMjkzODk4Vi0zLjk4NTA1NkMxLjAxNjE4OS0zLjk4NTA1NiAxLjA5NTg5LTMuOTE1MzE4IDEuMDk1ODktMy40MjcxNDhWLS43NTcxNjFDMS4wOTU4OS0uMzA4ODQyIC45ODYzMDEtLjMwODg0MiAuMzE4ODA0LS4zMDg4NDJWMEwxLjQ0NDU4My0uMDI5ODg4TDIuNTYwMzk5IDBWLS4zMDg4NDJDMS44OTI5MDItLjMwODg0MiAxLjc4MzMxMy0uMzA4ODQyIDEuNzgzMzEzLS43NTcxNjFWLTIuNTkwMjg2QzEuNzgzMzEzLTMuNjI2NDAxIDIuNDkwNjYtNC4xODQzMDkgMy4xMjgyNjktNC4xODQzMDlDMy43NTU5MTUtNC4xODQzMDkgMy44NjU1MDQtMy42NDYzMjYgMy44NjU1MDQtMy4wNzg0NTZWLS43NTcxNjFDMy44NjU1MDQtLjMwODg0MiAzLjc1NTkxNS0uMzA4ODQyIDMuMDg4NDE4LS4zMDg4NDJWMEw0LjIxNDE5Ny0uMDI5ODg4TDUuMzMwMDEyIDBaJy8+CjxwYXRoIGlkPSdnMC04MScgZD0nTTQuNjkyNDAzLTIuMTMyMDA1QzQuNjkyNDAzLTMuNDA3MjIzIDMuNjk2MTM5LTQuNDYzMjYzIDIuNDkwNjYtNC40NjMyNjNDMS4yNDUzMy00LjQ2MzI2MyAuMjc4OTU0LTMuMzc3MzM1IC4yNzg5NTQtMi4xMzIwMDVDLjI3ODk1NC0uODQ2ODI0IDEuMzE1MDY4IC4xMDk1ODkgMi40ODA2OTcgLjEwOTU4OUMzLjY4NjE3NyAuMTA5NTg5IDQuNjkyNDAzLS44NjY3NSA0LjY5MjQwMy0yLjEzMjAwNVpNMy44NjU1MDQtMi4yMTE3MDZDMy44NjU1MDQtMS44NTMwNTEgMy44NjU1MDQtMS4zMTUwNjggMy42NDYzMjYtLjg3NjcxMkMzLjQyNzE0OC0uNDI4Mzk0IDIuOTg4NzkyLS4xMzk0NzcgMi40OTA2Ni0uMTM5NDc3QzIuMDYyMjY3LS4xMzk0NzcgMS42MjM5MS0uMzQ4NjkyIDEuMzU0OTE5LS44MDY5NzRDMS4xMDU4NTMtMS4yNDUzMyAxLjEwNTg1My0xLjg1MzA1MSAxLjEwNTg1My0yLjIxMTcwNkMxLjEwNTg1My0yLjYwMDI0OSAxLjEwNTg1My0zLjEzODIzMiAxLjM0NDk1Ni0zLjU3NjU4OEMxLjYxMzk0OC00LjAzNDg2OSAyLjA4MjE5Mi00LjI0NDA4NSAyLjQ4MDY5Ny00LjI0NDA4NUMyLjkxOTA1NC00LjI0NDA4NSAzLjM0NzQ0Ny00LjAyNDkwNyAzLjYwNjQ3Ni0zLjU5NjUxM1MzLjg2NTUwNC0yLjU5MDI4NiAzLjg2NTUwNC0yLjIxMTcwNlonLz4KPC9kZWZzPgo8ZyBpZD0ncGFnZTEnPgo8ZyBzdHJva2UtbWl0ZXJsaW1pdD0nMTAnIHRyYW5zZm9ybT0ndHJhbnNsYXRlKC00My4xMDUyNDcsLTQzLjEwNTI0NylzY2FsZSgwLjk5NjI2NCwtMC45OTYyNjQpJz4KPGcgZmlsbD0nIzAwMCcgc3Ryb2tlPScjMDAwJz4KPGcgc3Ryb2tlLXdpZHRoPScwLjQnPgo8ZyB0cmFuc2Zvcm09J3RyYW5zbGF0ZSgtMTguODk1LC0zLjQxNTAxKSc+CjxnIHN0cm9rZT0nbm9uZScgdHJhbnNmb3JtPSdzY2FsZSgtMS4wMDM3NSwxLjAwMzc1KXRyYW5zbGF0ZSgtNDMuMTA1MjQ3LC00My4xMDUyNDcpc2NhbGUoLTEsLTEpJz4KPGcgZmlsbD0nIzAwMCc+CjxnIHN0cm9rZT0nbm9uZSc+Cjx1c2UgeD0nLTQzLjEwNTI0NycgeT0nLTQzLjEwNTI0NycgeGxpbms6aHJlZj0nI2cwLTQ3Jy8+Cjx1c2UgeD0nLTM3LjU2NjAyMScgeT0nLTQzLjEwNTI0NycgeGxpbms6aHJlZj0nI2cwLTY2Jy8+Cjx1c2UgeD0nLTM0Ljc5NjQwOScgeT0nLTQzLjEwNTI0NycgeGxpbms6aHJlZj0nI2cwLTI4Jy8+Cjx1c2UgeD0nLTI5LjgxNTA4OScgeT0nLTQzLjEwNTI0NycgeGxpbms6aHJlZj0nI2cwLTc1Jy8+Cjx1c2UgeD0nLTIxLjUxNjIwOCcgeT0nLTQzLjEwNTI0NycgeGxpbms6aHJlZj0nI2cwLTgxJy8+Cjx1c2UgeD0nLTE2LjUzNDg4NycgeT0nLTQzLjEwNTI0NycgeGxpbms6aHJlZj0nI2cwLTc3Jy8+Cjx1c2UgeD0nLTEwLjk5NTY2MicgeT0nLTQzLjEwNTI0NycgeGxpbms6aHJlZj0nI2cwLTQ3Jy8+CjwvZz4KPC9nPgo8L2c+CjwvZz4KPC9nPgo8L2c+CjwvZz4KPC9nPgo8L3N2Zz4=" title="{diamond_tex}"></span>
"""

a_plus_b_mathjax = r"""<span class="math inline">\(a+b\)</span>"""
a_plus_b_svg = """<span class="mathp inline"><img style="width:2.72619em; vertical-align:-0.15963em" src="data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnPz4KPCEtLSBUaGlzIGZpbGUgd2FzIGdlbmVyYXRlZCBieSBkdmlzdmdtIDMuMi4xIC0tPgo8c3ZnIHZlcnNpb249JzEuMScgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzIyLjcxODI0M3B0JyBoZWlnaHQ9JzguNzQ4NzE3cHQnIHZpZXdCb3g9Jy0uNTAwMDAyIC03LjQxODUgMjIuNzE4MjQzIDguNzQ4NzE3Jz4KPGRlZnM+CjxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+CjwhW0NEQVRBW3BhdGgge3N0cm9rZTogY3VycmVudENvbG9yO3N0cm9rZS13aWR0aDogMC4wNXB0O31dXT4KPC9zdHlsZT4KPHBhdGggaWQ9J2cxLTQzJyBkPSdNNC4wNzQ3Mi0yLjI5MTQwN0g2Ljg1NDI5NkM2Ljk5Mzc3My0yLjI5MTQwNyA3LjE4MzA2NC0yLjI5MTQwNyA3LjE4MzA2NC0yLjQ5MDY2UzYuOTkzNzczLTIuNjg5OTEzIDYuODU0Mjk2LTIuNjg5OTEzSDQuMDc0NzJWLTUuNDc5NDUyQzQuMDc0NzItNS42MTg5MjkgNC4wNzQ3Mi01LjgwODIxOSAzLjg3NTQ2Ny01LjgwODIxOVMzLjY3NjIxNC01LjYxODkyOSAzLjY3NjIxNC01LjQ3OTQ1MlYtMi42ODk5MTNILjg4NjY3NUMuNzQ3MTk4LTIuNjg5OTEzIC41NTc5MDgtMi42ODk5MTMgLjU1NzkwOC0yLjQ5MDY2Uy43NDcxOTgtMi4yOTE0MDcgLjg4NjY3NS0yLjI5MTQwN0gzLjY3NjIxNFYuNDk4MTMyQzMuNjc2MjE0IC42Mzc2MDkgMy42NzYyMTQgLjgyNjg5OSAzLjg3NTQ2NyAuODI2ODk5UzQuMDc0NzIgLjYzNzYwOSA0LjA3NDcyIC40OTgxMzJWLTIuMjkxNDA3WicvPgo8cGF0aCBpZD0nZzAtOTcnIGQ9J00zLjcxNjA2NS0zLjc2NTg3OEMzLjUzNjczNy00LjEzNDQ5NiAzLjI0NzgyMS00LjQwMzQ4NyAyLjc5OTUwMi00LjQwMzQ4N0MxLjYzMzg3My00LjQwMzQ4NyAuMzk4NTA2LTIuOTM4OTc5IC4zOTg1MDYtMS40ODQ0MzNDLjM5ODUwNi0uNTQ3OTQ1IC45NDY0NTEgLjEwOTU4OSAxLjcyMzUzNyAuMTA5NTg5QzEuOTIyNzkgLjEwOTU4OSAyLjQyMDkyMiAuMDY5NzM4IDMuMDE4NjgtLjYzNzYwOUMzLjA5ODM4MS0uMjE5MTc4IDMuNDQ3MDczIC4xMDk1ODkgMy45MjUyOCAuMTA5NTg5QzQuMjczOTczIC4xMDk1ODkgNC41MDMxMTMtLjExOTU1MiA0LjY2MjUxNi0uNDM4MzU2QzQuODMxODgtLjc5NzAxMSA0Ljk2MTM5NS0xLjQwNDczMiA0Ljk2MTM5NS0xLjQyNDY1OEM0Ljk2MTM5NS0xLjUyNDI4NCA0Ljg3MTczMS0xLjUyNDI4NCA0Ljg0MTg0My0xLjUyNDI4NEM0Ljc0MjIxNy0xLjUyNDI4NCA0LjczMjI1NC0xLjQ4NDQzMyA0LjcwMjM2Ni0xLjM0NDk1NkM0LjUzMzAwMS0uNjk3Mzg1IDQuMzUzNjc0LS4xMDk1ODkgMy45NDUyMDUtLjEwOTU4OUMzLjY3NjIxNC0uMTA5NTg5IDMuNjQ2MzI2LS4zNjg2MTggMy42NDYzMjYtLjU2Nzg3QzMuNjQ2MzI2LS43ODcwNDkgMy42NjYyNTItLjg2Njc1IDMuNzc1ODQxLTEuMzA1MTA2QzMuODg1NDMtMS43MjM1MzcgMy45MDUzNTUtMS44MjMxNjMgMy45OTUwMTktMi4yMDE3NDNMNC4zNTM2NzQtMy41OTY1MTNDNC40MjM0MTItMy44NzU0NjcgNC40MjM0MTItMy44OTUzOTIgNC40MjM0MTItMy45MzUyNDNDNC40MjM0MTItNC4xMDQ2MDggNC4zMDM4NjEtNC4yMDQyMzQgNC4xMzQ0OTYtNC4yMDQyMzRDMy44OTUzOTItNC4yMDQyMzQgMy43NDU5NTMtMy45ODUwNTYgMy43MTYwNjUtMy43NjU4NzhaTTMuMDY4NDkzLTEuMTg1NTU0QzMuMDE4NjgtMS4wMDYyMjcgMy4wMTg2OC0uOTg2MzAxIDIuODY5MjQtLjgxNjkzNkMyLjQzMDg4NC0uMjY4OTkxIDIuMDIyNDE2LS4xMDk1ODkgMS43NDM0NjItLjEwOTU4OUMxLjI0NTMzLS4xMDk1ODkgMS4xMDU4NTMtLjY1NzUzNCAxLjEwNTg1My0xLjA0NjA3N0MxLjEwNTg1My0xLjU0NDIwOSAxLjQyNDY1OC0yLjc2OTYxNCAxLjY1Mzc5OC0zLjIyNzg5NUMxLjk2MjY0LTMuODE1NjkxIDIuNDEwOTU5LTQuMTg0MzA5IDIuODA5NDY1LTQuMTg0MzA5QzMuNDU3MDM2LTQuMTg0MzA5IDMuNTk2NTEzLTMuMzY3MzcyIDMuNTk2NTEzLTMuMzA3NTk3UzMuNTc2NTg4LTMuMTg4MDQ1IDMuNTY2NjI1LTMuMTM4MjMyTDMuMDY4NDkzLTEuMTg1NTU0WicvPgo8cGF0aCBpZD0nZzAtOTgnIGQ9J00yLjM4MTA3MS02LjgwNDQ4M0MyLjM4MTA3MS02LjgxNDQ0NiAyLjM4MTA3MS02LjkxNDA3MiAyLjI1MTU1Ny02LjkxNDA3MkMyLjAyMjQxNi02LjkxNDA3MiAxLjI5NTE0My02LjgzNDM3MSAxLjAzNjExNS02LjgxNDQ0NkMuOTU2NDEzLTYuODA0NDgzIC44NDY4MjQtNi43OTQ1MjEgLjg0NjgyNC02LjYxNTE5M0MuODQ2ODI0LTYuNDk1NjQxIC45MzY0ODgtNi40OTU2NDEgMS4wODU5MjgtNi40OTU2NDFDMS41NjQxMzQtNi40OTU2NDEgMS41ODQwNi02LjQyNTkwMyAxLjU4NDA2LTYuMzI2Mjc2QzEuNTg0MDYtNi4yNTY1MzggMS40OTQzOTYtNS45MTc4MDggMS40NDQ1ODMtNS43MDg1OTNMLjYyNzY0Ni0yLjQ2MDc3MkMuNTA4MDk1LTEuOTYyNjQgLjQ2ODI0NC0xLjgwMzIzOCAuNDY4MjQ0LTEuNDU0NTQ1Qy40NjgyNDQtLjUwODA5NSAuOTk2MjY0IC4xMDk1ODkgMS43MzM0OTkgLjEwOTU4OUMyLjkwOTA5MSAuMTA5NTg5IDQuMTM0NDk2LTEuMzc0ODQ0IDQuMTM0NDk2LTIuODA5NDY1QzQuMTM0NDk2LTMuNzE2MDY1IDMuNjA2NDc2LTQuNDAzNDg3IDIuODA5NDY1LTQuNDAzNDg3QzIuMzUxMTgzLTQuNDAzNDg3IDEuOTQyNzE1LTQuMTE0NTcgMS42NDM4MzYtMy44MDU3MjlMMi4zODEwNzEtNi44MDQ0ODNaTTEuNDQ0NTgzLTMuMDM4NjA1QzEuNTA0MzU5LTMuMjU3NzgzIDEuNTA0MzU5LTMuMjc3NzA5IDEuNTk0MDIyLTMuMzg3Mjk4QzIuMDgyMTkyLTQuMDM0ODY5IDIuNTMwNTExLTQuMTg0MzA5IDIuNzg5NTM5LTQuMTg0MzA5QzMuMTQ4MTk0LTQuMTg0MzA5IDMuNDE3MTg2LTMuODg1NDMgMy40MTcxODYtMy4yNDc4MjFDMy40MTcxODYtMi42NjAwMjUgMy4wODg0MTgtMS41MTQzMjEgMi45MDkwOTEtMS4xMzU3NDFDMi41ODAzMjQtLjQ2ODI0NCAyLjEyMjA0Mi0uMTA5NTg5IDEuNzMzNDk5LS4xMDk1ODlDMS4zOTQ3Ny0uMTA5NTg5IDEuMDY2MDAyLS4zNzg1OCAxLjA2NjAwMi0xLjExNTgxNkMxLjA2NjAwMi0xLjMwNTEwNiAxLjA2NjAwMi0xLjQ5NDM5NiAxLjIyNTQwNS0yLjEyMjA0MkwxLjQ0NDU4My0zLjAzODYwNVonLz4KPC9kZWZzPgo8ZyBpZD0ncGFnZTEnPgo8dXNlIHg9JzAnIHk9JzAnIHhsaW5rOmhyZWY9JyNnMC05NycvPgo8dXNlIHg9JzcuNDgwMDInIHk9JzAnIHhsaW5rOmhyZWY9JyNnMS00MycvPgo8dXNlIHg9JzE3LjQ0MjYzMycgeT0nMCcgeGxpbms6aHJlZj0nI2cwLTk4Jy8+CjwvZz4KPC9zdmc+" title="a+b"></span>"""

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
        self.impl_test_svg_math()

    def impl_test_svg_math(self):
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
        d = self.impl_test_svg_math()
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
        el = self.get(d.url, as_tree=True).cssselect(".parContent > p > span")[0]
        self.assert_same_html(
            el,
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
