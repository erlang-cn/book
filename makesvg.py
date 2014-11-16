#!/usr/bin/env python2

import sys
import os.path

import poppler
import cairo


def convert_to_svg(page, filename):
    w,h = page.get_size()
    surface = cairo.SVGSurface(filename, w, h)
    ctx = cairo.Context(surface)
    page.render(ctx)


if __name__ == '__main__':
    filename = sys.argv[1]

    doc = poppler.document_new_from_file('file://'+os.path.abspath(filename), None)
    page_nums = map(int, sys.argv[2:]) or range(doc.get_n_pages())
    
    for pn in page_nums:
        page = doc.get_page(pn)
        convert_to_svg(page, '%d.svg'%(pn+1,))

    html = ''.join('<a href="{0}.svg" target="page">Page {0}</a><br />\n'.format(pn+1) for pn in page_nums)
    with open("pages.html", "w") as f:
        f.write(html)

