import shutil
import os
import timdb.documents
import documentmodel.document
from tempfile import mkstemp


def get_latest_hash(doc_id, par_id):
    return os.readlink('tim_files/pars/{}/{}/current'.format(doc_id, par_id))

def get_largest_file_number(path, default=None):
        if not os.path.exists(path):
            return default

        largest = -1
        for name in os.listdir(path):
            try:
                largest = max(largest, int(name))
            except ValueError:
                pass
        return largest if largest > -1 else default

def main():
    if not os.path.exists("tim_files"):
        print("Path does not exist: tim_files")
        return

    # doc_ids = {}
    # for doc_id in os.listdir("tim_files/docs"):
    #     for doc_major in os.listdir("tim_files/docs/{}".format(doc_id)):
    #         abs_major = "tim_files/docs/{}/{}".format(doc_id, doc_major)
    #         if os.path.isdir(abs_major):
    #             for doc_minor in os.listdir(abs_major):
    #                 abs_minor = "tim_files/docs/{}/{}/{}".format(doc_id, doc_major, doc_minor)
    #                 if os.path.isfile(abs_minor):
    #                     _, tmpfile = mkstemp()
    #                     is_changed = False
    #                     with open(tmpfile, 'w') as f_tmp:
    #                         with open(abs_minor, 'r') as f:
    #                             while True:
    #                                 line = f.readline()
    #                                 if len(line) < 12:
    #                                     break
    #                                 doc_ids[line[:12]] = doc_id
    #
    # for par_id in os.listdir("tim_files/pars"):
    #     if len(par_id) != 12 or par_id not in doc_ids:
    #         continue
    #     doc_id = doc_ids[par_id]
    #     root_dir = "tim_files/pars/{}".format(doc_id)
    #     if not os.path.exists(root_dir):
    #         os.mkdir(root_dir)
    #
    #     if not os.path.exists('{}/{}'.format(root_dir, par_id)):
    #         link_name = "tim_files/pars/{}/{}".format(doc_id, par_id)
    #         link_src = "../{}".format(par_id)
    #         os.symlink(link_src, link_name, True)

    for doc_id in os.listdir("tim_files/docs"):
        doc_major = get_largest_file_number('tim_files/docs/' + doc_id)
        if doc_major is None:
            continue

        doc_minor = get_largest_file_number('tim_files/docs/{}/{}'.format(doc_id, doc_major))
        if doc_minor is None:
            continue

        abs_minor = "tim_files/docs/{}/{}/{}".format(doc_id, doc_major, doc_minor)
        if not os.path.isfile(abs_minor):
            continue

        _, tmpfile = mkstemp()
        is_changed = False
        with open(tmpfile, 'w') as f_tmp:
            with open(abs_minor, 'r') as f:
                while True:
                    line = f.readline()
                    if len(line) < 12:
                        break
                    par_id = line[:12]
                    if len(line) < 15:
                        f_tmp.write('{}/{}\n'.format(par_id, get_latest_hash(doc_id, par_id)))
                        is_changed = True
                    else:
                        f_tmp.write(line)
        if is_changed:
            shutil.copy(tmpfile, abs_minor)
        os.unlink(tmpfile)


if __name__ == "__main__":
    main()