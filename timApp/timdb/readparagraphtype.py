import enum


class ReadParagraphType(enum.Enum):
    on_screen = 1
    hover_par = 2
    click_par = 3
    click_red = 4

    def class_str(self):
        return class_map[self]


class_map = {
    ReadParagraphType.on_screen: 'screen',
    ReadParagraphType.hover_par: 'hover',
    ReadParagraphType.click_par: 'click',
    ReadParagraphType.click_red: 'read'
}
