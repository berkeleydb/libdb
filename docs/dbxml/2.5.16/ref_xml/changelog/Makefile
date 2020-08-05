# $Id$

.SUFFIXES: .html .so
.so.html:
	@f=$<; echo "$${f##*..} -> ${BUILDDIR}/$@"
	@rm -f "$@"
	@m4 -DM4API=${API}						\
	    -DM4MAJOR=${DB_VERSION_MAJOR}				\
	    -DM4MINOR=${DB_VERSION_MINOR}				\
	    -DM4PAGEPATH=${BUILDDIR}/$@					\
	    -DM4PATCH=${DB_VERSION_PATCH}				\
	    -DM4PRODUCT=${PRODUCT}					\
	    -DM4RELDIR=${RELDIR}					\
	    -DM4XMLMAJOR=${DBXML_VERSION_MAJOR}				\
	    -DM4XMLMINOR=${DBXML_VERSION_MINOR}				\
	    -DM4XMLPATCH=${DBXML_VERSION_PATCH}				\
	    -I${DOCS_SRC}						\
	    -I${BUILD_UTILS}						\
	    ${BUILD_UTILS}/m4/m4.dbxml $< |				\
	    sed -f ${BUILD_UTILS}/html.sed > $@

# Avoid global make variables stepping on our name space.
C=
COMMON=
CXX=

VPATH=$(DOCS_SRC)/ref_xml/changelog


BUILD=	toc.html
COPY=   1.2.0.html 1.2.1.html 2.0.9.html 2.1.8.html 2.2.13.html \
	2.2.8.html 2.3.8.html 2.3.10.html 2.4.html

all: ${BUILD}
all: $(COMMON) $(CXX)
