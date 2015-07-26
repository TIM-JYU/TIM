import sys
sys.path.insert(0, '/py') # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon
from http_params import *

all_temps = get_all_templates('templates')
print(json.dumps(all_temps))

for i in range(0,len(all_temps["text"])):
    temps = all_temps["templates"][i]
    tab = all_temps["text"][i]
    print("Tab: " + tab)
    for templ in temps:
        t = get_template('templates',str(i),templ["file"])
        print(t)

t = get_template('templates', '0', "pali_7")
print(t)


a = {"a" : 1}
b = {"b" : 2}
c = {"c" : 3}
d = join_dict(a,b)
print(d)