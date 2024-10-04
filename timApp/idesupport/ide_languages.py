"""
Custom IDE support handlers for different language types.
"""

import os
import re
import textwrap
from typing import Any

from timApp.idesupport.files import SupplementaryFile, is_in_filename
from tim_common.cs_utils import populated

DOTNET_VERSION = "net$(NETCoreAppMaximumVersion)"  # "net8.0"
# see: https://www.meziantou.net/how-to-use-the-latest-target-framework-available-for-a-dotnet-sdk.htm


class Language:
    """
    A language type handler for the IDE support.
    Classes derived from this base type can define custom behavior for TIM language types.
    If any language-class is written to separate file, it must be imported here so that populated-function
    will work.
    """

    ttype: str | list[str] = "_language"
    """
    Language types that the class handles. 
    Should be equivalent to csPlugin language types.
    """

    def __init__(self, plugin_json: dict):
        self.fileext = ""
        """
        File extension to use for the source code files.
        """
        self.comment_syntax_lookup = "//"
        """
        The start of a comment line in the language.
        """

        self.plugin_json = plugin_json
        self.ide_task_id = ""
        self.filename = self.init_filename()

    def find_comment_line_characters(self) -> str:
        return self.comment_syntax_lookup

    def init_filename(self) -> str:
        """
        Generate the file name to use for the main file of the task.

        :return: The file name to use for the main file of the task.
        """
        return self.plugin_json["markup"].get("filename")

    def get_filename(self) -> str:
        """
        :return: Current file name to use for the main file of the task.
        """
        return self.filename

    @staticmethod
    def get_classname(s: str | None) -> str | None:
        if s is None:
            return None

        class_pattern = r"\bclass\s+(\w+)"
        match = re.search(class_pattern, s)
        if not match:
            return None
        return match.group(1)

    def try_to_get_classname(self) -> str | None:
        clsname = Language.get_classname(self.plugin_json.get("program"))
        if clsname is None:
            clsname = Language.get_classname(self.plugin_json.get("by"))
        if clsname is None:
            clsname = Language.get_classname(self.plugin_json.get("byCode"))
        return clsname

    def generate_supplementary_files(
        self, extrafiles: list[dict[str, str]]
    ) -> list[SupplementaryFile]:
        """
        Generate the supplementary files needed for the task.

        :param extrafiles: List of extra files provided in the task markup

        :return: List of SupplementaryFiles needed to run the task
        """
        return []

    @staticmethod
    def make_language(ttype: str, plugin_json: Any, ide_task_id: str) -> "Language":
        """
        Initialize the language handler for a specific language type.

        :param ttype: language type
        :param plugin_json: plugin markup as a dictionary
        :param ide_task_id: task id of the IDE task

        :return: Language handler object if found, otherwise the base Language class
        """
        cls = languages.get(ttype)
        if cls is None:
            cls = Language
        obj = cls(plugin_json)
        obj.ide_task_id = ide_task_id
        return obj

    @classmethod
    def all_subclasses(cls) -> Any:
        subclasses = cls.__subclasses__()
        return subclasses + [i for sc in subclasses for i in sc.all_subclasses()]


class CS(Language):
    ttype: str | list[str] = ["cs", "c#", "csharp"]

    def __init__(self, plugin_json: Any):
        super().__init__(plugin_json)
        self.fileext = "cs"
        clsname = self.try_to_get_classname()
        if not clsname:
            clsname, _ = os.path.splitext(self.filename)
        self.classname = clsname

    def init_filename(self) -> str:
        filename = super().init_filename()
        if filename is None:
            filename = self.try_to_get_classname()
        if filename is None:
            filename = "Main.cs"  # TODO
        return filename

    def generate_supplementary_files(
        self, extrafiles: list[dict[str, str]]
    ) -> list[SupplementaryFile]:
        if is_in_filename(extrafiles, r".*\.csproj"):
            return []
        proj_file = SupplementaryFile(
            filename=f"{self.ide_task_id}.csproj",
            content=textwrap.dedent(
                f"""\
                <Project Sdk="Microsoft.NET.Sdk">
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

    def __init__(self, plugin_json: Any):
        super().__init__(plugin_json)

    def generate_supplementary_files(
        self, extrafiles: list[dict[str, str]]
    ) -> list[SupplementaryFile]:
        files = []
        if not is_in_filename(extrafiles, r".*\.csproj"):
            proj_file = SupplementaryFile(
                filename=f"{self.ide_task_id}.csproj",
                content=textwrap.dedent(
                    f"""\
                    <Project Sdk="Microsoft.NET.Sdk">
                        <PropertyGroup>
                            <OutputType>WinExe</OutputType>
                            <TargetFramework>{DOTNET_VERSION}</TargetFramework>
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
                    f"""\
                    using System;
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

    def __init__(self, plugin_json: Any):
        super().__init__(plugin_json)
        self.comment_syntax_lookup = "#"
        self.fileext = "py"


class CC(Language):
    ttype: str | list[str] = "cc"

    def __init__(self, plugin_json: Any):
        super().__init__(plugin_json)
        self.fileext = "c"


class CPP(CC):
    ttype = ["c++", "cpp"]

    def __init__(self, plugin_json: Any):
        super().__init__(plugin_json)
        self.fileext = "cpp"


class Java(Language):
    ttype = "java"

    def __init__(self, plugin_json: Any):
        super().__init__(plugin_json)
        self.fileext = "java"


languages = populated(Language)
