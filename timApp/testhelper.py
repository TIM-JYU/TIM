import random
import string
from sys import stdout


def random_str(min_length, max_length):
    chars = string.ascii_lowercase
    length = random.randint(min_length, max_length)
    return ''.join(random.choice(chars) for _ in range(length))


def random_word():
    return random_str(2, 8)


def random_clause():
    words = random.randint(3, 20)
    return ' '.join(random_word() for _ in range(words))


def random_sentence():
    parts = random.randint(1, 4)
    return ', '.join(random_clause() for _ in range(parts))


def random_paragraph():
    sentences = random.randint(3, 6)
    return '. '.join(random_sentence() for _ in range(sentences)) + '.'


def print_times(times):
    t_sum = sum(times)
    t_avg = t_sum / len(times)
    t_avgdev = sum([abs(t - t_avg) for t in times]) / len(times)
    print(f"Sum of all times was {t_sum:.0f} seconds.")
    print(
        f"Average execution took {t_avg:.3f} +/- {t_avgdev:.3f} seconds (min: {min(times):.3f}, max: {max(times):.3f})\n")


def progress_print(line):
    stdout.write("\r")
    stdout.write(line)
    stdout.flush()
