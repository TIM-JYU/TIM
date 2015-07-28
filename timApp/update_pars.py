import os
import timdb.documents
import documentmodel.document

def main():
    if not os.path.exists("tim_files"):
        print("Path does not exist: tim_files")
        return

    doc_ids = {}
    for doc_id in os.listdir("tim_files/docs"):
        for doc_major in os.listdir("tim_files/docs/{}".format(doc_id)):
            abs_major = "tim_files/docs/{}/{}".format(doc_id, doc_major)
            if os.path.isdir(abs_major):
                for doc_minor in os.listdir(abs_major):
                    abs_minor = "tim_files/docs/{}/{}/{}".format(doc_id, doc_major, doc_minor)
                    if os.path.isfile(abs_minor):
                        with open(abs_minor, 'r') as f:
                            while True:
                                line = f.readline()
                                if line == '':
                                    break
                                doc_ids[line.rstrip('\n')] = doc_id

    for par_id in os.listdir("tim_files/pars"):
        if len(par_id) != 12 or par_id not in doc_ids:
            continue
        doc_id = doc_ids[par_id]
        root_dir = "tim_files/pars/{}".format(doc_id)
        if not os.path.exists(root_dir):
            os.mkdir(root_dir)

        if not os.path.exists('{}/{}'.format(root_dir, par_id)):
            link_name = "tim_files/pars/{}/{}".format(doc_id, par_id)
            link_src = "../{}".format(par_id)
            os.symlink(link_src, link_name, True)

if __name__ == "__main__":
    main()