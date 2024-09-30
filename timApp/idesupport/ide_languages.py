import os
import re
import textwrap

from timApp.idesupport.files import SupplementaryFile, is_in_filename
from tim_common.cs_utils import populated

DOTNET_VERSION = "net8.0"


class Language:
    ttype = "_language"

    def __init__(self, plugin_json):
        self.fileext = ""
        self.comment_syntax_lookup = "//"
        self.plugin_json = plugin_json
        self.ide_task_id = ""
        self.filename = self.init_filename()

    def find_comment_line_characters(self) -> str:
        return self.comment_syntax_lookup

    def init_filename(self):
        return self.plugin_json["markup"].get("filename")

    def get_filename(self):
        return self.filename

    @staticmethod
    def get_classname(s: str) -> str | None:
        class_pattern = r"\bclass\s+(\w+)"
        match = re.search(class_pattern, s)
        if not match:
            return None
        return match.group(1)

    def try_to_get_classname(self) -> str:
        clsname = Language.get_classname(self.plugin_json.get("program"))
        if clsname is None:
            clsname = Language.get_classname(self.plugin_json.get("by"))
        if clsname is None:
            clsname = Language.get_classname(self.plugin_json.get("byCode"))
        return clsname

    def generate_supplementary_files(self, extrafiles) -> list[SupplementaryFile]:
        return []

    @staticmethod
    def make_language(ttype, plugin_json, ide_task_id):
        cls = languages.get(ttype)
        if cls is None:
            cls = Language
        obj = cls(plugin_json)
        obj.ide_task_id = ide_task_id
        return obj

    @classmethod
    def all_subclasses(cls):
        subclasses = cls.__subclasses__()
        return subclasses + [i for sc in subclasses for i in sc.all_subclasses()]


class CS(Language):
    ttype = ["cs", "c#", "csharp"]

    def __init__(self, plugin_json):
        super().__init__(plugin_json)
        self.fileext = "cs"
        clsname = self.try_to_get_classname()
        if not clsname:
            clsname, _ = os.path.splitext(self.filename)
        self.classname = clsname

    def init_filename(self):
        filename = super().init_filename()
        if filename is None:
            filename = self.try_to_get_classname()
        if filename is None:
            filename = "Main.cs"  # TODO
        return filename

    def generate_supplementary_files(self, extrafiles) -> list[SupplementaryFile]:
        if is_in_filename(extrafiles, r".*\.csproj"):
            return []
        proj_file = SupplementaryFile(
            filename=f"{self.ide_task_id}.csproj",
            content=textwrap.dedent(
                f"""<Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <OutputType>Exe</OutputType>
                    <TargetFramework>{DOTNET_VERSION}</TargetFramework>
                  </PropertyGroup>
                </Project>
                """
            ),
        )
        return [proj_file]


class Jypeli(CS):
    ttype = "jypeli"

    def __init__(self, plugin_json):
        super().__init__(plugin_json)

    def generate_supplementary_files(self, extrafiles) -> list[SupplementaryFile]:
        files = []
        if not is_in_filename(extrafiles, r".*\.csproj"):
            proj_file = SupplementaryFile(
                filename=f"{self.ide_task_id}.csproj",
                content=textwrap.dedent(
                    f"""<Project Sdk="Microsoft.NET.Sdk">
                        <PropertyGroup>
                            <OutputType>WinExe</OutputType>
                            <TargetFramework>net8.0</TargetFramework>
                        </PropertyGroup>
                        <ItemGroup>
                            <PackageReference Include="Jypeli.NET" Version="11.*"/>
                            <PackageReference Include="Jypeli.FarseerPhysics.NET" Version="2.*"/>
                        </ItemGroup>
                        <ItemGroup>
                            <PackageReference Include="Jypeli.NET" Version="11.*"/>
                            <PackageReference Include="Jypeli.FarseerPhysics.NET" Version="2.*"/>
                        </ItemGroup>
                        <ItemGroup>
                          <None Update="Content\\*.*">
                            <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
                          </None>
                        </ItemGroup>      
                    </Project>
                    """
                ),
            )
            files.append(proj_file)
        if not is_in_filename(extrafiles, r"Ohjelma\.cs"):
            main_file = SupplementaryFile(
                filename="Ohjelma.cs",
                content=textwrap.dedent(
                    f"""using System;
                    namespace {self.classname};
                    public static class Program
                    {{
                        [STAThread]
                        static void Main()
                        {{
                            using var game = new {self.classname}();
                            game.Run();
                        }}
                    }}
                """
                ),
            )
            files.append(main_file)
        return files


class PY3(Language):
    ttype = ["py", "py3", "python", "python3"]

    def __init__(self, plugin_json):
        super().__init__(plugin_json)
        self.comment_syntax_lookup = "#"
        self.fileext = "py"


class CC(Language):
    ttype = "cc"

    def __init__(self, plugin_json):
        super().__init__(plugin_json)
        self.fileext = "c"


class CPP(CC):
    ttype = ["c++", "cpp"]

    def __init__(self, plugin_json):
        super().__init__(plugin_json)
        self.fileext = "cpp"


class Java(Language):
    ttype = "java"

    def __init__(self, plugin_json):
        super().__init__(plugin_json)
        self.fileext = "java"


languages = populated(Language)
