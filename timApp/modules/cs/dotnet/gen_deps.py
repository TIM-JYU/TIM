#!/bin/python3
import json
import os
import shutil
import subprocess
import tempfile
from typing import TypedDict, Dict, Optional


class DotNetPackage(TypedDict, total=False):
    dependencies: Dict[str, str]
    runtime: Dict[str, str]


class DotNetLibrary(TypedDict, total=False):
    sha512: str
    hashPath: str


class DotNetDeps(TypedDict):
    runtimeTarget: Dict
    compilationOptions: Dict
    targets: Dict[str, Dict[str, DotNetPackage]]
    libraries: Dict[str, DotNetLibrary]


def gen_deps(csproj_file: str, base_dir: str):
    print(f'Generating deps for {csproj_file}')
    name, _ = os.path.splitext(csproj_file)
    deps_obj: Optional[DotNetDeps] = None
    with tempfile.TemporaryDirectory(dir=base_dir) as tmp_folder:
        with open(os.path.join(tmp_folder, 'main.cs'), 'w', encoding='utf-8') as f:
            f.write('System.Console.ReadKey();')
        shutil.copy(os.path.join(base_dir, csproj_file), tmp_folder)
        shutil.copy(os.path.join(base_dir, "NuGet.Config"), tmp_folder)
        subprocess.run(['dotnet', 'build', '-c', 'Release', tmp_folder])
        with open(os.path.join(tmp_folder, 'bin', 'Release', f'{name}.deps.json'), 'r', encoding='utf-8') as f:
            deps_obj = json.load(f)

    if not deps_obj:
        return

    print('Cleaning up generated deps.json')
    print('Cleaning up signature')
    del deps_obj['runtimeTarget']['signature']

    print('Cleaning up targets and collecting compile dependencies')
    build_deps = []
    for packages in deps_obj['targets'].values():
        for package_name, package_info in list(packages.items()):
            package_info: DotNetPackage
            package_name: str

            if package_name == f'{name}/1.0.0':
                print(f'Removing {package_name} from targets list')
                del packages[package_name]
                continue

            package_base_dir = package_name.lower()
            if 'runtime' in package_info:
                build_deps.extend([os.path.join(package_base_dir, dep) for dep in package_info['runtime'].keys()])

    print('Cleaning up libraries')
    for library_name, lib in list(deps_obj['libraries'].items()):
        library_name: str
        lib: DotNetLibrary

        if library_name == f'{name}/1.0.0':
            print(f'Removing {library_name} from libraries list')
            del deps_obj['libraries'][library_name]
            continue

        lib['sha512'] = ''
        del lib['hashPath']

    with open(os.path.join('configs', f'{name}.deps.json'), 'w', encoding='utf-8') as f:
        json.dump(deps_obj, f, indent=2)
    with open(os.path.join('configs', f'{name}.build.deps'), 'w', encoding='utf-8') as f:
        f.writelines([f'{b}\n' for b in build_deps])


def should_run():
    return not os.path.exists('configs') or not os.path.exists('nuget_cache') or os.path.exists('refresh')


def remove(path):
    if os.path.isfile(path) or os.path.islink(path):
        os.remove(path)
    elif os.path.isdir(path):
        shutil.rmtree(path, ignore_errors=True)


def main():
    if not should_run():
        print('Skipping dotnet run generation, all necessary folders exist. If you want to regenerate, create file '
              'named `refresh`')
        return
    remove('configs')
    remove('nuget_cache')
    remove('refresh')
    print('Generating dependency lists for dotnet')
    os.makedirs('configs', exist_ok=True)
    os.makedirs('nuget_cache', exist_ok=True)
    for dirpath, dirnames, filenames in os.walk('deps'):
        for filename in filenames:
            name, ext = os.path.splitext(filename)
            if ext == '.csproj':
                gen_deps(filename, dirpath)


if __name__ == '__main__':
    main()
