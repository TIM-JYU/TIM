from pathlib import Path
from typing import List

from timApp.user.settings.theme import Theme


class ThemeNotFoundException(Exception):
    pass


def generate_theme_scss(themes: List[Theme], gen_dir: Path) -> None:
    """Generates an SCSS file based on the given theme names.

    .. note::
       This function sorts the given theme list.

    The structure of the generated SCSS file is as follows:

    1. Charset declaration (always UTF-8)
    2. Import default values for variables (from variables.scss)
    3. For each theme:

      1. Declare an empty mixin whose name is the same as the theme
      2. Import the theme's SCSS file. This may contain a mixin of the same name which will override
         the empty one.

    4. Import all.scss that contains all generic SCSS files
    5. For each theme:

      1. Include the theme mixin in root scope. This trick allows the theme file to override any
         generic CSS.

    :param themes: The list of themes.
    :param gen_dir: The directory where the SCSS file should be generated.

    """
    for t in themes:
        if not t.exists():
            raise ThemeNotFoundException(t.filename)
    combined = get_combined_css_filename(themes)
    file_path = gen_dir / f'{combined}.scss'
    if file_path.exists():
        return
    gen_dir.mkdir(exist_ok=True)
    with file_path.open(encoding='utf-8', mode='w') as f:
        f.write('@charset "UTF-8";\n')
        f.write('@import "stylesheets/variables";\n')
        for t in themes:
            f.write(f'@mixin {t.filename} {{}}\n')
            f.write(f'@import "stylesheets/themes/{t.filename}";\n')
        f.write('@import "stylesheets/all.scss";\n')  # "all" conflicts with a jQuery CSS file, so we must add the .scss extension
        for t in themes:
            f.write(f'@include {t.filename};\n')


def get_combined_css_filename(themes: List[Theme]):
    """Returns the combined file name based on the given list of theme names.

    :param themes: The list of themes.
    :return: The combined file name based on the themes. If the list is empty, 'default' is returned.

    """
    return '-'.join(t.filename for t in themes) or 'default'
