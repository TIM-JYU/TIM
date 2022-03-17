#!/bin/python3
import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import TypedDict


class DotNetPackage(TypedDict, total=False):
    dependencies: dict[str, str]
    runtime: dict[str, str]


class DotNetLibrary(TypedDict, total=False):
    sha512: str
    hashPath: str


class DotNetDeps(TypedDict):
    runtimeTarget: dict
    compilationOptions: dict
    targets: dict[str, dict[str, DotNetPackage]]
    libraries: dict[str, DotNetLibrary]


def gen_deps(base_dir: Path, csproj_file: str, deps_dir: str):
    print(f"Generating deps for {csproj_file}")
    name, _ = os.path.splitext(csproj_file)
    with tempfile.TemporaryDirectory(dir=base_dir.as_posix()) as tmp_folder:
        with open(os.path.join(tmp_folder, "main.cs"), "w", encoding="utf-8") as f:
            f.write("System.Console.ReadKey();")
        shutil.copy(os.path.join(deps_dir, csproj_file), tmp_folder)
        shutil.copy(os.path.join(deps_dir, "Directory.Build.props"), tmp_folder)
        shutil.copy(os.path.join(deps_dir, "NuGet.Config"), tmp_folder)
        subprocess.run(["dotnet", "build", "-c", "Release", tmp_folder])
        with open(
            os.path.join(tmp_folder, "bin", "Release", f"{name}.deps.json"),
            encoding="utf-8",
        ) as f:
            deps_obj: DotNetDeps | None = json.load(f)

    if not deps_obj:
        return

    print("Cleaning up generated deps.json")
    print("Cleaning up signature")
    del deps_obj["runtimeTarget"]["signature"]

    print("Cleaning up targets and collecting compile dependencies")
    build_deps = []
    for packages in deps_obj["targets"].values():
        for package_name, package_info in list(packages.items()):
            package_info: DotNetPackage
            package_name: str

            if package_name == f"{name}/1.0.0":
                print(f"Removing {package_name} from targets list")
                del packages[package_name]
                continue

            package_base_dir = package_name.lower()
            if "runtime" in package_info:
                build_deps.extend(
                    [
                        os.path.join(package_base_dir, dep)
                        for dep in package_info["runtime"].keys()
                    ]
                )

    print("Cleaning up libraries")
    for library_name, lib in list(deps_obj["libraries"].items()):
        library_name: str
        lib: DotNetLibrary

        if library_name == f"{name}/1.0.0":
            print(f"Removing {library_name} from libraries list")
            del deps_obj["libraries"][library_name]
            continue

        lib["sha512"] = ""
        del lib["hashPath"]

    config_path = base_dir / "configs"

    with (config_path / f"{name}.deps.json").open("w", encoding="utf-8") as f:
        json.dump(deps_obj, f, indent=2)
    with (config_path / f"{name}.build.deps").open("w", encoding="utf-8") as f:
        f.writelines([f"{b}\n" for b in build_deps])


def should_run(base_path: Path) -> bool:
    return (
        not (base_path / "configs").exists()
        or not (base_path / "nuget_cache").exists()
        or (base_path / "refresh").exists()
    )


def remove(p: Path) -> None:
    if p.is_file() or p.is_symlink():
        p.unlink(missing_ok=True)
    elif p.is_dir():
        shutil.rmtree(p, ignore_errors=True)


def main():
    # Get base directory from first argument
    if len(sys.argv) < 2:
        print("Provide base directory as first argument")
        return
    base_dir = Path(os.path.abspath(sys.argv[1]))
    base_dir.mkdir(exist_ok=True)
    if not should_run(base_dir):
        print(
            "Skipping dotnet run generation, all necessary folders exist. If you want to regenerate, create file "
            "named `refresh`"
        )
        return
    remove(base_dir / "configs")
    remove(base_dir / "nuget_cache")
    print("Generating dependency lists for dotnet")
    os.makedirs(base_dir / "configs", exist_ok=True)
    os.makedirs(base_dir / "nuget_cache", exist_ok=True)
    for dirpath, dirnames, filenames in os.walk("deps"):
        for filename in filenames:
            name, ext = os.path.splitext(filename)
            if ext == ".csproj":
                gen_deps(base_dir, filename, dirpath)
    remove(base_dir / "refresh")


if __name__ == "__main__":
    main()
