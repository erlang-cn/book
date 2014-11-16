#!/usr/bin/env python2

import os.path
import sys
from dulwich.repo import Repo
from dulwich.objects import Blob, Tree, Commit
import glob


repo = Repo(".")

import sys
if len(sys.argv) > 1:
    commit_sha = repo.get_refs()["refs/heads/"+sys.argv[1]]
else:
    commit_sha = repo.head()

commit = repo.get_object(commit_sha)

index = repo.open_index()
assert not list(index.changes_from_tree(repo.object_store, commit.tree)), "uncommited changes"


def add_blob(store, path):
    with open(path, "rb") as f:
        blob = Blob.from_string(f.read())
    store.add_object(blob)
    return blob.id

def add_files(store, tree, files):
    for a, b in files:
        tree.add(a, 0100644, add_blob(store, b))

def make_preview(store):
    tree = Tree()
    files = [
        ("index.html", "preview.html"),
        ("pages.html", "pages.html")]
    files.extend([(n,n) for n in glob.glob("*.svg")])
    add_files(store, tree, files)
    store.add_object(tree)
    return tree.id

def make_release_tree(store, sha):
    tree = Tree()
    tree.add(sha, 040000, make_preview(store))

    names = [
        ("code","zip"),
        ("exercise","zip"),
        ("src","zip"),
        ("screen","pdf"),
        ("print","pdf")]

    files = [("%s.%s.%s"%(a,sha,b), "%s.%s"%(a,b)) for a,b in names]
    add_files(store, tree, files)

    with open("index.html", "rb") as f:
        html = f.read().format(sha=sha)

    blob = Blob.from_string(html)
    store.add_object(blob)
    tree.add("index.html", 0100644, blob.id)
    store.add_object(tree)
    return tree.id

store = repo.object_store

new_commit = Commit()
new_commit.author = commit.author
new_commit.committer = commit.committer
new_commit.author_time = commit.author_time
new_commit.commit_time = commit.commit_time
new_commit.author_timezone = commit.author_timezone
new_commit.commit_timezone = commit.commit_timezone
new_commit.encoding = commit.encoding
new_commit.message = commit.message
new_commit.tree = make_release_tree(store, commit_sha)

store.add_object(new_commit)

repo['refs/heads/release'] = new_commit.id
