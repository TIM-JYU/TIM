# dotnet support scripts and tooling for TIM

This folder contains support tooling needed to run `dotnet` command and possible supporting libraries in csplugin.

## Quick guide: refreshing library versions

To refresh library versions from NuGet, do the following:

0. CD to this folder. If you are in TIM root, do `cd timApp/modules/cs/dotnet`
1. Edit relevant `.csproj` files in `deps` folder. Update package versions if needed.
2. Run `touch refresh`
3. Restart csplugin container

## Compiling and running .NET code

.NET code can be compiled and run in csplugin in two ways: using *project templates* and using `dotnet run`. Notes on
using both is outlined next.

### Using predefined projects (Jypeli, NUnit, C# console app, ...)

The fast way is intended for compiling specific pre-made project types (C# console applications, Jypeli games, NUnit
tests). In this way, `csc` script is invoked directly to compile the code and then `dotnet exec` to run the code.  
The run times are low but project types are more limited (and you need to predefine project template).

For this approach, you need to define a project type as a csplugin language.

#### Adding or updating project type

1. Create a new `.csproj` file or open one you want to edit in `deps` folder. You can use `jypeli.csproj` as a base for
   a new project type.
2. Add one or multiple `PackageReference`s for all NuGet packages that are needed to build the project.

   You can reference any packages available on [NuGet](https://www.nuget.org/).  
   Try to lock versions as precisely as possible and avoid using wildcards.

3. Run `touch refresh` and restart csplugin. Follow csplugin logs (`./dc logs -f csplugin`) to make sure the
   configuration files are generated correctly.
4. Make sure dependency files are generated in `configs` folder for your `.csproj` template

   Two files should be generated for your `<project_type>.csproj`:
    1. `<project_type>.build.deps` - contains a list of DLL paths relative to `nuget_cache` folder. You will need to
       pass these paths to `csc`.
    2. `<project_type>.deps.json` - contains runtime settings for .NET runtime. You will have to pass the path to this
       JSON file to `dotnet exec`.

5. If you are creating a new project type, define it as a new language in `languages.py` and invoke `/cs/dotnet/csc` to
   compile and `dotnet exec` to run the code.

   Use `Jypeli` language type as an example.  
   Specifically, you will need to pass `<project_type>.build.deps` to `csc` as a reference list
   and `<project_type>.deps.json` to `dotnet exec` via `--depsfile` argument.

### Compiling entire projects (`dotnet run`)

The slow way is the most basic but at the moment not directly supported by csplugin. For it, you use can `shell` run
type.

When using `shell` run type, define `.csproj` file and all necessary source files.  
Finally, run `dotnet run` to build and run the project.

csplugin is set up so that you can install any NuGet packages you want. Note that doing so increases run times
dramatically.

NuGet packages are not cached in this approach to save space.

 