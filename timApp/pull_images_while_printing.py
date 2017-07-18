"""
    Pulls the images in the documens that are part of the document that is being
    printed. Needs two arguments, the image path/URL and the folder that the
    image should be put into.
    
    The program checks whether the image resides on the TIM server or on another
    host machine. The image files that are found on the local machine are just copied
    to the temporary images folder supplied as a parameter.
     
    Needs to be called in the LaTeX document that is being printed. An example of the usage below.
    
    '
        ...
        \write18{python3 <PATH-TO-THIS-FILE>/pull_images_while_printing.py <IMAGE_PATH/URL> <TEMP-IMAGEFOLDER-PATH>}
        \includegraphics{<TEMP-IMAGEFOLDER-PATH>/<IMAGE_NAME>}
        ...
    '
    
"""
import sys
from urllib.parse import urlparse


def pull_image(image_path_or_url: str, output_folder: str):
    url_object = urlparse(image_path_or_url)
    if url_object.scheme is None or url_object.scheme == "":
        print("not a URL")
    #TODO: rest of this
    return


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Not enough arguments supplied. Please supply run the command as" +
              "'python3 <PATH-TO-THIS-FILE>/pull_images_while_printing.py <IMAGE_PATH/URL> <TEMP-IMAGEFOLDER-PATH>'")

    image_path_or_url = sys.argv[1]
    output_folder = sys.argv[2]
    # TODO: add checks

    pull_image(image_path_or_url, output_folder)

