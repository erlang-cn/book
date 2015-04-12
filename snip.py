#!/usr/bin/env python2

import argparse
import functools
import errno
import os
import os.path
import sqlite3
import glob
import hashlib
import shutil
import StringIO
import itertools

COMMAND="COMMAND"
parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers(dest=COMMAND)
commands = {}
conn = None
REFLINE = r"""\begin{{SingleSpacing}}\lstinputlisting[numbers=left,caption={{{efile}}},firstnumber={start},firstline={start},lastline={end}]{{\currfiledir {file}}}\end{{SingleSpacing}}
"""

def argument(*args, **kwargs):
    return lambda parser: parser.add_argument(*args, **kwargs)


def command(*arguments):

    def decorator(func):
        name = func.__name__.replace("_", "-")
        subparser = subparsers.add_parser(name, help=func.__doc__)
        dests = [arg(subparser).dest for arg in arguments]

        @functools.wraps(func)
        def wrapper(args):
            return func(**{d:args.__dict__[d] for d in dests if d in args})

        commands[name]=wrapper
        return wrapper

    return decorator


def list_files():
    for filename in (
            glob.glob("src/*/*.erl") +
            glob.glob("src/*/*.tex")):
        yield filename[4:], os.stat(filename).st_mtime


def get_snippet_hashes(filename):
    snippets = {}
    current_name = None
    current_text = None
    line_start = None
    text_start = None
    expect_name = None
    lno = 1
    tno = 0

    with open('src/' + filename, 'r') as f:
        for line in f:
            tno += 1
            if line.startswith("% SNIP BEGIN "):
                assert current_name is None, "%s:%d: nested SNIP BEGIN" % (filename, tno)
                current_name = line[13:].rstrip()
                if expect_name is not None:
                    assert current_name == expect_name, "%s:%d: EXPECT %s, GOT %s" % (filename, tno, expect_name, current_name)
                current_text = ""
                line_start = lno
                text_start = tno
                expect_name = None
            elif line.startswith("% SNIP END") or line.startswith("%- SNIP END"):
                assert current_name is not None, "%s:%d: SNIP END BEFORE SNIP BEGIN" % (filename, tno)
                yield (
                    current_name,
                    hashlib.sha512(current_text).hexdigest(),
                    line_start,
                    lno - 1,
                    text_start+1,
                    tno)
                current_name = None
                current_text = None
                line_start = None
                text_start = None
                if line[:2] == "%-":
                    lno -= 1
            elif line.startswith("% SNIP REFERENCE "):
                [_, expect_name] = line[17:].rstrip().split()
                lno += 1
                if current_name is None:
                    continue
                current_text += line
            else:
                lno += 1
                if current_name is None:
                    continue
                current_text += line


def get_snippet_text(filename, text_start, text_end):
    with open('src/' + filename, 'r') as f:
        lines = f.readlines()
    return "".join(lines[text_start-1:text_end-1])


def replace_snippet_text(filename, name, text):
    buf = StringIO.StringIO()
    begin_found = False

    with open('src/' + filename, 'r') as f:
        for line in f:
            if line.startswith("% SNIP BEGIN "):
                buf.write(line)
                if line[13:].rstrip() == name:
                    begin_found = True
                    buf.write(text)
            elif not begin_found:
                buf.write(line)
            elif line.startswith("% SNIP END") or line.startswith("%- SNIP END"):
                begin_found = False
                buf.write(line)
            else:
                continue

    with open('src/' + filename, 'w') as f:
        f.write(buf.getvalue())


def refresh():
    global conn
    try:
        os.mkdir(".snip")
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

    conn = sqlite3.connect('.snip/state.sqlite')
    conn.execute("""PRAGMA foreign_keys = ON""")
    conn.executescript(
"""
CREATE TABLE IF NOT EXISTS files (
  id INTEGER PRIMARY KEY,
  filename TEXT UNIQUE,
  timestamp REAL);

CREATE TABLE IF NOT EXISTS snippets (
  id INTEGER PRIMARY KEY,
  file_id INTEGER REFERENCES files(id) ON DELETE CASCADE,
  name TEXT,
  hash TEXT,
  line_start INTEGER,
  line_end INTEGER,
  text_start INTEGER,
  text_end INTEGER);
""")

    files = list(list_files())

    with conn:
        conn.executemany(
            """DELETE FROM files WHERE filename = ? AND timestamp < ?""",
            files)

        for filename, timestamp in files:
            cur = conn.execute(
                """INSERT OR IGNORE INTO files(filename, timestamp) VALUES (?, ?)""",
                (filename, timestamp))

            if cur.rowcount != 1:
                continue

            file_id = cur.lastrowid
            snippets = get_snippet_hashes(filename)
            conn.executemany(
                """
INSERT INTO snippets(file_id, name, hash, line_start, line_end, text_start, text_end)
VALUES (?, ?, ?, ?, ?, ?, ?)
""", [(file_id,n,h,s1,e1,s2,e2) for (n,h,s1,e1,s2,e2) in snippets])


def generate_file(filename):
    buf = StringIO.StringIO()
    last_line = None
    dirpath = os.path.dirname(filename)

    with open("src/"+filename, "r") as f:
        for line in f:
            if line.startswith("% SNIP BEGIN "):
                continue
            elif line.startswith("% SNIP END"):
                continue
            elif line.startswith("%- SNIP END"):
                last_line = last_line.rstrip()
            elif line.startswith("% SNIP REFERENCE "):
                [fname, name] = line[17:].rstrip().split()
                cur = conn.execute(
"""
SELECT snippets.line_start, snippets.line_end FROM snippets
JOIN files ON snippets.file_id = files.id
WHERE files.filename = ? AND snippets.name = ?
""", (os.path.join(dirpath, fname), name))
                (s, e) = cur.fetchone()
                buf.write(REFLINE.format(
                    file=fname,
                    efile=os.path.join(dirpath, fname).replace("_", r"\_"),
                    start=s,
                    end=e))
            else:
                if last_line is not None:
                    buf.write(last_line)
                last_line = line

        if last_line is not None:
            buf.write(last_line)

    try:
        os.makedirs("build/"+dirpath)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

    with open("build/"+filename, "w") as f:
        f.write(buf.getvalue())


@command()
def clear():
    """clear"""
    try:
        shutil.rmtree(".snip")
    except OSError as e:
        if e.errno != errno.ENOENT:
            raise


@command()
def status():
    """status"""
    refresh()

    cur = conn.execute(
        """SELECT name FROM snippets GROUP BY name HAVING count(DISTINCT hash) > 1""")
    names = cur.fetchall()
    if len(names):
        print "CONFLICTS:"
        for (name,) in names:
            print ' ', name
        exit(1)


@command()
def build():
    """build"""
    refresh()

    cur = conn.execute("""SELECT name FROM snippets GROUP BY name HAVING count(DISTINCT hash) > 1""")
    names = cur.fetchall()
    assert len(names) == 0, "conflict exists"

    cur = conn.execute("""SELECT filename FROM files""")
    for (filename,) in cur.fetchall():
        generate_file(filename)


@command(argument("name", help="snippet name"))
def sync(name):
    """sync"""
    refresh()

    cur = conn.execute("""SELECT count(DISTINCT hash) FROM snippets WHERE name = ?""", (name,))
    (count,) = cur.fetchone()
    if count < 2:
        return

    cur = conn.execute(
"""
SELECT files.filename, snippets.text_start, snippets.text_end, snippets.hash FROM snippets
JOIN files ON snippets.file_id = files.id
WHERE name = ? ORDER BY snippets.hash, files.filename
""", (name,))

    choices = [[(f,s,e) for (f,s,e,_) in g]
               for k, g in itertools.groupby(cur.fetchall(), lambda x: x[3])]

    for i, choice in enumerate(choices):
        print "%d) " % (i+1,) + ", ".join(["%s(%d-%d)"%(f,s,e) for f,s,e in choice])

    result = raw_input("Your choice(leave empty to skip): ")
    if result == "":
        return

    num = int(result)
    assert 0 < num <= len(choices), "number out of range"
    num -= 1

    text = get_snippet_text(*choices[num][0])

    filenames = set(f for (i,choice) in enumerate(choices) if i != num for (f,_,_) in choice)
    for filename in filenames:
        replace_snippet_text(filename, name, text)


@command()
def help():
    """Print help message"""
    parser.print_help()


if __name__ == '__main__':
    args = parser.parse_args()
    commands[args.__dict__[COMMAND]](args)
